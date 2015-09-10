/*
 * Copyright 2011-2014 Formal Methods and Tools, University of Twente
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <assert.h>
#include <inttypes.h>
#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <atomics.h>
#include <avl.h>
#include <barrier.h>
#include <cache.h>
#include <lace.h>
#include <llmsset.h>
#include <refs.h>
#include <sha2.h>
#include <sylvan.h>
#include <tls.h>

#if USE_NUMA
#include <numa.h>
#endif

#ifndef SYLVAN_CACHE_STATS
#define SYLVAN_CACHE_STATS 0
#endif

#ifndef SYLVAN_OPERATION_STATS
#define SYLVAN_OPERATION_STATS 0
#endif

#define SYLVAN_STATS SYLVAN_CACHE_STATS || SYLVAN_OPERATION_STATS

/**
 * Complement handling macros
 */
#define BDD_HASMARK(s)              (s&sylvan_complement?1:0)
#define BDD_TOGGLEMARK(s)           (s^sylvan_complement)
#define BDD_STRIPMARK(s)            (s&~sylvan_complement)
#define BDD_TRANSFERMARK(from, to)  (to ^ (from & sylvan_complement))
// Equal under mark
#define BDD_EQUALM(a, b)            ((((a)^(b))&(~sylvan_complement))==0)

static inline BDD BDD_SETDATA(BDD s, uint32_t data)
{
    return (s & 0x800000ffffffffff) | (((uint64_t)data & 0x7fffff)<<40);
}

typedef struct __attribute__((packed)) bddnode {
    uint64_t high     : 40;
    uint32_t level    : 24;
    uint64_t low      : 40;
    unsigned int data : 23;
    uint8_t comp      : 1;
} *bddnode_t; // 16 bytes

#define GETNODE(bdd) ((bddnode_t)llmsset_index_to_ptr(nodes, BDD_STRIPMARK(bdd)))

/**
 * Static global variables
 */

static int workers;

static llmsset_t nodes;

llmsset_t
__sylvan_get_internal_data()
{
    return nodes;
}

TASK_2(size_t, sylvan_table_usage_par, size_t, start, size_t, end)
{
    if (end - start <= 128) {
        return llmsset_get_filled_partial(nodes, start, end);
    } else {
        size_t part = (end-start)/2;
        if (part < 128) part = 128;
        SPAWN(sylvan_table_usage_par, start, start+part);
        size_t end2 = start+2*part;
        if (end2 > end) end2 = end;
        size_t res = CALL(sylvan_table_usage_par, start+part, end2);;
        res += SYNC(sylvan_table_usage_par);
        return res;
    }
}

VOID_TASK_IMPL_2(sylvan_table_usage, size_t*, filled, size_t*, total)
{
    size_t tot = llmsset_get_size(nodes);
    if (filled != NULL) *filled = CALL(sylvan_table_usage_par, 0, tot);
    if (total != NULL) *total = tot;
}

/**
 * Macros for statistics
 */
#if SYLVAN_STATS
typedef enum {
#if SYLVAN_CACHE_STATS
    C_cache_new,
    C_cache_exists,
    C_cache_reuse,
#endif
    C_gc_user,
    C_gc_hashtable_full,
#if SYLVAN_OPERATION_STATS
    C_ite,
    C_exists,
    C_relprod_paired,
    C_relprod_paired_prev,
    C_constrain,
    C_restrict,
    C_compose,
#endif
    C_MAX
} Counters;

#define N_CNT_THREAD 128 /* Maximum number of threads for counting */
#define SYLVAN_PAD(x,b) ( (b) - ( (x) & ((b)-1) ) ) /* b must be power of 2 */
struct {
    uint64_t count[C_MAX];
    char pad[SYLVAN_PAD(sizeof(uint64_t)*C_MAX, 64)];
} sylvan_stats[N_CNT_THREAD];
#endif

#if SYLVAN_STATS
#define SV_CNT(s) {(sylvan_stats[LACE_WORKER_ID].count[s]+=1);}
#else
#define SV_CNT(s) ; /* Empty */
#endif

#if SYLVAN_CACHE_STATS
#define SV_CNT_CACHE(s) SV_CNT(s)
#else
#define SV_CNT_CACHE(s) /* Empty */
#endif

#if SYLVAN_OPERATION_STATS
#define SV_CNT_OP(s) SV_CNT(s)
#else
#define SV_CNT_OP(s) /* Empty */
#endif

/**
 * Thread-local insert index for LLMSset
 */
static DECLARE_THREAD_LOCAL(insert_index, uint64_t*);

static uint64_t*
initialize_insert_index()
{
    LOCALIZE_THREAD_LOCAL(insert_index, uint64_t*);
    insert_index = (uint64_t*)malloc(LINE_SIZE);
    LACE_ME;
    size_t my_id = LACE_WORKER_ID;
    *insert_index = llmsset_get_insertindex_multi(nodes, my_id, workers);
    SET_THREAD_LOCAL(insert_index, insert_index);
    return insert_index;
}

/**
 * External references
 */

BDD
sylvan_ref(BDD a)
{
    if (a == sylvan_false || a == sylvan_true) return a;
    refs_up(BDD_STRIPMARK(a));
    return a;
}

void
sylvan_deref(BDD a)
{
    if (a == sylvan_false || a == sylvan_true) return;
    refs_down(BDD_STRIPMARK(a));
}

size_t
sylvan_count_refs()
{
    return refs_count();
}

/**
 * Internal references (during operations)
 */
typedef struct ref_internal
{
    size_t r_size, r_count;
    size_t s_size, s_count;
    BDD *results;
    Task **spawns;
} *ref_internal_t;

static DECLARE_THREAD_LOCAL(ref_key, ref_internal_t);

static __attribute__((noinline)) ref_internal_t
ref_init()
{
    ref_internal_t s = (ref_internal_t)malloc(sizeof(struct ref_internal));
    s->r_size = 128;
    s->r_count = 0;
    s->s_size = 128;
    s->s_count = 0;
    s->results = (BDD*)malloc(sizeof(BDD) * 128);
    s->spawns = (Task**)malloc(sizeof(Task*) * 128);
    SET_THREAD_LOCAL(ref_key, s);
    return s;
}

static __attribute__((noinline)) void
ref_resize_results()
{
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);
    ref_key->results = (BDD*)realloc(ref_key->results, sizeof(BDD) * (ref_key->r_size*=2));
}

static __attribute__((noinline)) void
ref_resize_spawns()
{
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);
    ref_key->spawns = (Task**)realloc(ref_key->spawns, sizeof(Task*) * (ref_key->s_size*=2));
}

#define REFS_INIT                                                                       \
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);                                     \
    if (!ref_key) { ref_key=ref_init(); }                                               \
    size_t mark_old_count = ref_key->r_count;
 
#define REFS_PUSH(a)                                                                    \
{                                                                                       \
    BDD result_to_mark = a;                                                             \
    if (result_to_mark != sylvan_false && result_to_mark != sylvan_true) {              \
        LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);                                 \
        size_t count = ref_key->r_count;                                                \
        if (count >= ref_key->r_size) { ref_resize_results(); }                         \
        ref_key->results[count] = result_to_mark;                                       \
        ref_key->r_count = count+1;                                                     \
    }                                                                                   \
}

#define REFS_SPAWN(s)                                                                   \
{                                                                                       \
    Task *task_to_mark = s;                                                             \
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);                                     \
    size_t count = ref_key->s_count;                                                    \
    if (count >= ref_key->s_size) { ref_resize_spawns(); }                              \
    ref_key->spawns[count] = task_to_mark;                                              \
    ref_key->s_count = count+1;                                                         \
}

#define REFS_DESPAWN                                                                    \
{                                                                                       \
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);                                     \
    ref_key->s_count--;                                                                 \
}

#define REFS_EXIT                                                                       \
{                                                                                       \
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);                                     \
    ref_key->r_count = mark_old_count;                                                  \
}

/**
 * Implementation of garbage collection
 */

/* Recursively mark BDD nodes as 'in use' */
static void
sylvan_gc_mark_rec(BDD bdd)
{
    if (bdd == sylvan_false || bdd == sylvan_true) return;

    if (llmsset_mark_unsafe(nodes, bdd&0x000000ffffffffff)) {
        bddnode_t n = GETNODE(bdd);
        sylvan_gc_mark_rec(n->low);
        sylvan_gc_mark_rec(n->high);
    }
}

/* Mark external references */
static void
sylvan_gc_mark_refs(int my_id, int workers)
{
    // part of the refs hash table per worker
    size_t per_worker = (refs_size + workers - 1)/ workers;
    if (per_worker < 8) per_worker = 8;

    // which part of the refs hash table we start
    size_t first = per_worker * my_id;
    if (first >= refs_size) return;

    // which part of the refs hash table we end
    size_t end = per_worker * (my_id + 1);
    if (end >= refs_size) end = refs_size;

    // iterate through refs hash table, mark all found
    size_t *it = refs_iter(first, end);
    while (it != NULL) sylvan_gc_mark_rec(refs_next(&it, end));
}

static int gc_enabled = 1;

void
sylvan_gc_enable()
{
    gc_enabled = 1;
}

void
sylvan_gc_disable()
{
    gc_enabled = 0;
}

static barrier_t gcbar;
static volatile int gc;

static
void sylvan_gc_go(int master)
{
    if (master && !cas(&gc, 0, 1)) master=0;

    // phase 1: clear cache and hash array
    barrier_wait(&gcbar);

    if (master) cache_clear();

    LACE_ME;
    int my_id = LACE_WORKER_ID;
    llmsset_clear_multi(nodes, my_id, workers);

    // phase 2: mark nodes to keep
    barrier_wait(&gcbar);

    sylvan_gc_mark_refs(my_id, workers);

    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);
    if (ref_key) {
        size_t i = ref_key->r_count;
        while (i--) sylvan_gc_mark_rec(ref_key->results[i]);
        for (i=0; i<ref_key->s_count; i++) {
            Task *t = ref_key->spawns[i];
            if (!TASK_IS_STOLEN(t)) break;
            if (TASK_IS_COMPLETED(t)) sylvan_gc_mark_rec(*(BDD*)TASK_RESULT(t));
        }
    }

    // phase 3: rehash
    barrier_wait(&gcbar);

    LOCALIZE_THREAD_LOCAL(insert_index, uint64_t*);
    if (insert_index == NULL) insert_index = initialize_insert_index();
    *insert_index = llmsset_get_insertindex_multi(nodes, my_id, workers);

    llmsset_rehash_multi(nodes, my_id, workers);

    // phase 4: done
    compiler_barrier();
    if (master) gc = 0;
    barrier_wait(&gcbar);
}

/* Callback for Lace */
TASK_0(void*, sylvan_lace_test_gc)
{
    if (gc) sylvan_gc_go(0);
    return 0;
}

/* Test called from every BDD operation */
static inline void
sylvan_gc_test()
{
    while (gc) sylvan_gc_go(0);
}

/* Manually perform garbage collection */
void
sylvan_gc()
{
#if SYLVAN_STATS
    LACE_ME;
    SV_CNT(C_gc_user);
#endif
    sylvan_gc_go(1);
}

/** init and quit functions */

static int granularity = 1; // default

void
sylvan_init(size_t tablesize, size_t cachesize, int _granularity)
{
    lace_set_callback(TASK(sylvan_lace_test_gc));
    workers = lace_workers();

    INIT_THREAD_LOCAL(ref_key);
    INIT_THREAD_LOCAL(insert_index);

#if USE_NUMA
    if (numa_available() != -1) {
        numa_set_interleave_mask(numa_all_nodes_ptr);
    }
#endif

    sylvan_reset_counters();

    granularity = _granularity;

    gc = 0;
    barrier_init(&gcbar, lace_workers());

    // Sanity check
    if (sizeof(struct bddnode) != 16) {
        fprintf(stderr, "Invalid size of bdd nodes: %ld\n", sizeof(struct bddnode));
        exit(1);
    }

    if (tablesize > 40) {
        fprintf(stderr, "sylvan_init error: tablesize must be <= 40!\n");
        exit(1);
    }

    if (cachesize > 40) {
        fprintf(stderr, "sylvan_init error: cachesize must be <= 40!\n");
        exit(1);
    }

    nodes = llmsset_create(1LL<<tablesize);
    cache_create(1LL<<cachesize);
    refs_create(1024);

    // Another sanity check
    llmsset_test_multi(nodes, workers);
}

void
sylvan_quit()
{
    // TODO: remove lace callback

    cache_free();
    llmsset_free(nodes);
    refs_free();
    barrier_destroy(&gcbar);
}

#ifndef SYLVAN_REPORT_COLORED
#define SYLVAN_REPORT_COLORED 1
#endif

#if SYLVAN_REPORT_COLORED
#define BLACK "\33[22;30m"
#define GRAY "\33[01;30m"
#define RED "\33[22;31m"
#define LRED "\33[01;31m"
#define GREEN "\33[22;32m"
#define LGREEN "\33[01;32m"
#define BLUE "\33[22;34m"
#define LBLUE "\33[01;34m"
#define BROWN "\33[22;33m"
#define YELLOW "\33[01;33m"
#define CYAN "\33[22;36m"
#define LCYAN "\33[22;36m"
#define MAGENTA "\33[22;35m"
#define LMAGENTA "\33[01;35m"
#define NC "\33[0m"
#define BOLD "\33[1m"
#define ULINE "\33[4m" //underline
#define BLINK "\33[5m"
#define INVERT "\33[7m"
#else
#define LRED
#define NC
#define BOLD
#define ULINE
#define BLUE
#define RED
#endif

void
sylvan_reset_counters()
{
#if SYLVAN_STATS
    int i,j;
    for (i=0;i<N_CNT_THREAD;i++) {
        for (j=0;j<C_MAX;j++) {
            sylvan_stats[i].count[j] = 0;
        }
    }
#endif
}

void
sylvan_report_stats()
{
#if SYLVAN_STATS
    int i,j;

    printf(LRED  "****************\n");
    printf(     "* ");
    printf(NC BOLD"SYLVAN STATS");
    printf(NC LRED             " *\n");
    printf(     "****************\n");
    printf(NC ULINE "Memory usage\n" NC LBLUE);
    printf("BDD table:          ");
    llmsset_print_size(nodes, stdout);
    printf("\n");
    //printf("Cache:              ");
    //llci_print_size(_bdd.cache, stdout);
    //printf("\n");

    uint64_t totals[C_MAX];
    for (i=0;i<C_MAX;i++) totals[i] = 0;
    for (i=0;i<N_CNT_THREAD;i++) {
        for (j=0;j<C_MAX;j++) totals[j] += sylvan_stats[i].count[j];
    }

#if SYLVAN_CACHE_STATS
    printf(NC ULINE "Cache\n" NC LBLUE);

    uint64_t total_cache = totals[C_cache_new] + totals[C_cache_exists] + totals[C_cache_reuse];
    printf("New results:         %" PRIu64 "\n", totals[C_cache_new]);
    printf("Existing results:    %" PRIu64 "\n", totals[C_cache_exists]);
    printf("Reused results:      %" PRIu64 "\n", totals[C_cache_reuse]);
    printf("Total results:       %" PRIu64 "\n", total_cache);
#endif

    printf(NC ULINE "GC\n" NC LBLUE);
    printf("GC user-request:     %" PRIu64 "\n", totals[C_gc_user]);
    printf("GC full table:       %" PRIu64 "\n", totals[C_gc_hashtable_full]);

#if SYLVAN_OPERATION_STATS
    printf(NC ULINE "Call counters (ITE, exists, relprod_paired, relprod_paired_prev, constrain)\n" NC LBLUE);
    for (i=0;i<workers;i++) {
        printf("Worker %02d:           %" PRIu64 ", %" PRIu64 ", %" PRIu64 ", %" PRIu64 ", %" PRIu64 "\n", i,
            sylvan_stats[i].count[C_ite], sylvan_stats[i].count[C_exists], 
            sylvan_stats[i].count[C_relprod_paired], sylvan_stats[i].count[C_relprod_paired_prev],
            sylvan_stats[i].count[C_constrain]);
    }
    printf("Totals:              %" PRIu64 ", %" PRIu64 ", %" PRIu64 ", %" PRIu64 ", %" PRIu64 "\n",
        totals[C_ite], totals[C_exists], 
        totals[C_relprod_paired], totals[C_relprod_paired_prev],
        totals[C_constrain]);
#endif

    printf(LRED  "****************" NC " \n");

    printf("BDD Unique table: %zu of %zu buckets filled.\n", llmsset_get_filled(nodes), llmsset_get_size(nodes));
#endif
}

/**
 * Core BDD operations
 */

BDD
sylvan_makenode(BDDVAR level, BDD low, BDD high)
{
    if (low == high) return low;

    // Normalization to keep canonicity
    // low will have no mark

    struct bddnode n;
    int mark;

    LOCALIZE_THREAD_LOCAL(insert_index, uint64_t*);
    if (insert_index == NULL) insert_index = initialize_insert_index();

    if (BDD_HASMARK(low)) {
        mark = 1;
        n = (struct bddnode){high, level, low, 0, (uint8_t)(BDD_HASMARK(high) ? 0 : 1)};
    } else {
        mark = 0;
        n = (struct bddnode){high, level, low, 0, (uint8_t)(BDD_HASMARK(high) ? 1 : 0)};
    }

    BDD result;
    uint64_t index;
    int created;
    if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
#if SYLVAN_STATS
        LACE_ME;
        SV_CNT(C_gc_hashtable_full);
#endif

        if (gc_enabled) {
            //size_t before_gc = llmsset_get_filled(nodes);
            sylvan_gc_go(1);
            //size_t after_gc = llmsset_get_filled(nodes);
            //size_t total = llmsset_get_size(nodes);
            //fprintf(stderr, "GC: %.01f%% to %.01f%%\n", 100.0*(double)before_gc/total, 100.0*(double)after_gc/total);
        }

        if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
            fprintf(stderr, "BDD Unique table full, %zu of %zu buckets filled!\n", llmsset_get_filled(nodes), llmsset_get_size(nodes));
            exit(1);
        }
    }

    result = index;
    return mark ? result | sylvan_complement : result;
}

BDD
sylvan_ithvar(BDDVAR level)
{
    return sylvan_makenode(level, sylvan_false, sylvan_true);
}

BDDVAR
sylvan_var(BDD bdd)
{
    assert(!sylvan_isconst(bdd));
    return GETNODE(bdd)->level;
}

static inline BDD
node_lowedge(bddnode_t node)
{
    return node->low;
}

static inline BDD
node_highedge(bddnode_t node)
{
    return node->high | (node->comp ? sylvan_complement : 0LL);
}

static inline BDD
node_low(BDD bdd, bddnode_t node)
{
    return BDD_TRANSFERMARK(bdd, node_lowedge(node));
}

static inline BDD
node_high(BDD bdd, bddnode_t node)
{
    return BDD_TRANSFERMARK(bdd, node_highedge(node));
}

BDD
sylvan_low(BDD bdd)
{
    if (sylvan_isconst(bdd)) return bdd;
    return node_low(bdd, GETNODE(bdd));
}

BDD
sylvan_high(BDD bdd)
{
    if (sylvan_isconst(bdd)) return bdd;
    return node_high(bdd, GETNODE(bdd));
}

/**
 * Debugging functionality that converts a BDD to one without complemented edges.
 */

BDD
sylvan_makenode_nocomp(BDDVAR level, BDD low, BDD high)
{
    if (low == high) return low;

    LOCALIZE_THREAD_LOCAL(insert_index, uint64_t*);
    if (insert_index == NULL) insert_index = initialize_insert_index();

    struct bddnode n = (struct bddnode){high, level, low, 0, 0};

    uint64_t index;
    int created;
    if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
#if SYLVAN_STATS
        LACE_ME;
        SV_CNT(C_gc_hashtable_full);
#endif
        if (gc_enabled) sylvan_gc_go(1);
        if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
            fprintf(stderr, "BDD Unique table full, %zu of %zu buckets filled!\n", llmsset_get_filled(nodes), llmsset_get_size(nodes));
            exit(1);
        }
    }

    return (BDD)index;
}

BDD
sylvan_bdd_to_nocomp(BDD bdd)
{
    if (bdd == sylvan_true) return sylvan_true_nc;
    if (bdd == sylvan_false) return sylvan_false;

    bddnode_t n = GETNODE(bdd);
    return sylvan_makenode_nocomp(n->level, sylvan_bdd_to_nocomp(node_low(bdd, n)), sylvan_bdd_to_nocomp(node_high(bdd, n)));
}

/**
 * BDD operations
 */
#define CACHE_ITE 0
#define CACHE_RELPROD_PAIRED 1
#define CACHE_RELPROD_PAIRED_PREV 2
#define CACHE_COUNT 3
#define CACHE_EXISTS 4
#define CACHE_SATCOUNT 5
#define CACHE_COMPOSE 6
#define CACHE_RESTRICT 7
#define CACHE_CONSTRAIN 8

/**
 * Implementation of unary, binary and if-then-else operators.
 */

TASK_IMPL_4(BDD, sylvan_ite, BDD, a, BDD, b, BDD, c, BDDVAR, prev_level)
{
    /* Terminal cases */
    if (a == sylvan_true) return b;
    if (a == sylvan_false) return c;
    if (a == b) b = sylvan_true;
    if (a == sylvan_not(b)) b = sylvan_false;
    if (a == c) c = sylvan_false;
    if (a == sylvan_not(c)) c = sylvan_true;
    if (b == c) return b;
    if (b == sylvan_true && c == sylvan_false) return a;
    if (b == sylvan_false && c == sylvan_true) return sylvan_not(a);

    /* End terminal cases. Apply rewrite rules to optimize cache use */

    /* At this point, A is not a constant true/false. */

    if (sylvan_isconst(b) && BDD_STRIPMARK(c) < BDD_STRIPMARK(a)) {
        if (b == sylvan_false) {
            // ITE(A,F,C) = ITE(~C,F,~A)
            //            = (A and F) or (~A and C)
            //            = F or (~A and C)
            //            = (~C and F) or (C and ~A)
            //            = ITE(~C,F,~A)
            BDD t = a;
            a = sylvan_not(c);
            c = sylvan_not(t);
        } else {
            // ITE(A,T,C) = ITE(C,T,A)
            //            = (A and T) or (~A and C)
            //            = A or (~A and C)
            //            = C or (~C and A)
            //            = (C and T) or (~C and A)
            //            = ITE(C,T,A)
            BDD t = a;
            a = c;
            c = t;
        }
    }

    if (sylvan_isconst(c) && BDD_STRIPMARK(b) < BDD_STRIPMARK(a)) {
        if (c == sylvan_false) {
            // ITE(A,B,F) = ITE(B,A,F)
            //            = (A and B) or (~A and F)
            //            = (A and B) or F
            //            = (B and A) or (~B and F)
            BDD t = a;
            a = b;
            b = t;
        } else {
            // ITE(A,B,T) = ITE(~B,~A,T)
            //            = (A and B) or (~A and T)
            //            = (A and B) or ~A
            //            = (~B and ~A) or B
            //            = (~B and ~A) or (B and T)
            //            = ITE(~B,~A,T)
            BDD t = a;
            a = sylvan_not(b);
            b = sylvan_not(t);
        }
    }

    if (BDD_STRIPMARK(b) == BDD_STRIPMARK(c)) {
        // At this point, B and C are not constants because that is a terminal case
        // 1. if A then B else ~B = if B then A else ~A
        // 2. if A then ~B else B = if ~B then A else ~A
        if (BDD_STRIPMARK(a) > BDD_STRIPMARK(b)) {
            // a > b, exchange:
            b = a;
            a = sylvan_not(c);
            c = sylvan_not(b);
        }
    }

    // ITE(~A,B,C) = ITE(A,C,B)
    if (BDD_HASMARK(a)) {
        a = BDD_STRIPMARK(a);
        BDD t = c;
        c = b;
        b = t;
    }

    /**
     * Apply De Morgan: ITE(A,B,C) = ~ITE(A,~B,~C)
     *
     * Proof:
     *   ITE(A,B,C) = (A and B) or (~A and C)
     *              = (A or C) and (~A or B)
     *              = ~(~(A or C) or ~(~A or B))
     *              = ~((~A and ~C) or (A and ~B))
     *              = ~((A and ~B) or (~A and ~C))
     *              = ~ITE(A,~B,~C)
     */
    int mark = 0;
    if (BDD_HASMARK(b)) {
        b = sylvan_not(b);
        c = sylvan_not(c);
        mark = 1;
    }

    sylvan_gc_test();

    // The value of a,b,c may be changed, but the reference counters are not changed at this point.

    SV_CNT_OP(C_ite);

    bddnode_t na = sylvan_isconst(a) ? 0 : GETNODE(a);
    bddnode_t nb = sylvan_isconst(b) ? 0 : GETNODE(b);
    bddnode_t nc = sylvan_isconst(c) ? 0 : GETNODE(c);

    // Get lowest level
    BDDVAR level = 0xffffffff;
    if (na) level = na->level;
    if (nb && level > nb->level) level = nb->level;
    if (nc && level > nc->level) level = nc->level;

    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_ITE), b, c, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return mark ? sylvan_not(result) : result;
        }
    }

    // Get cofactors
    BDD aLow = a, aHigh = a;
    BDD bLow = b, bHigh = b;
    BDD cLow = c, cHigh = c;
    if (na && level == na->level) {
        aLow = node_low(a, na);
        aHigh = node_high(a, na);
    }
    if (nb && level == nb->level) {
        bLow = node_low(b, nb);
        bHigh = node_high(b, nb);
    }
    if (nc && level == nc->level) {
        cLow = node_low(c, nc);
        cHigh = node_high(c, nc);
    }

    // Recursive computation
    BDD low=sylvan_invalid, high=sylvan_invalid;
    REFS_INIT;
    if (sylvan_isconst(aHigh)) {
        if (aHigh == sylvan_true) high = bHigh;
        else high = cHigh;
        low = CALL(sylvan_ite, aLow, bLow, cLow, level);
        REFS_PUSH(low);
    } else {
        REFS_SPAWN(SPAWN(sylvan_ite, aHigh, bHigh, cHigh, level));
        low = CALL(sylvan_ite, aLow, bLow, cLow, level);
        REFS_PUSH(low);
        high = SYNC(sylvan_ite);
        REFS_DESPAWN;
        REFS_PUSH(high);
    }

    BDD result = sylvan_makenode(level, low, high);
    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_ITE), b, c, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return mark ? sylvan_not(result) : result;
}

/**
 * Calculate constrain a @ c
 */
TASK_IMPL_3(BDD, sylvan_constrain, BDD, a, BDD, b, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (b == sylvan_true) return a;
    if (b == sylvan_false) return sylvan_false;
    if (sylvan_isconst(a)) return a;
    if (a == b) return sylvan_true;
    if (a == sylvan_not(b)) return sylvan_false;

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count operation */
    SV_CNT_OP(C_constrain);

    // a != constant and b != constant
    bddnode_t na = GETNODE(a);
    bddnode_t nb = GETNODE(b);

    BDDVAR level = na->level < nb->level ? na->level : nb->level;

    // CONSULT CACHE

    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_CONSTRAIN), b, 0, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return result;
        }
    }

    // DETERMINE TOP BDDVAR AND COFACTORS

    BDD aLow, aHigh, bLow, bHigh;

    if (na->level == level) {
        aLow = node_low(a, na);
        aHigh = node_high(a, na);
    } else {
        aLow = aHigh = a;
    }

    if (nb->level == level) {
        bLow = node_low(b, nb);
        bHigh = node_high(b, nb);
    } else {
        bLow = bHigh = b;
    }

    BDD result;
    REFS_INIT;

    BDD low=sylvan_invalid, high=sylvan_invalid;
    if (bLow == sylvan_true) low = aLow;
    else if (bLow == sylvan_false) return CALL(sylvan_constrain, aHigh, bHigh, level);
    else REFS_SPAWN(SPAWN(sylvan_constrain, aLow, bLow, level));
    if (bHigh == sylvan_true) high = bHigh;
    else if (bHigh == sylvan_false) {
        if (bLow != sylvan_true) low = SYNC(sylvan_constrain);
        return low;
    } else {
        high = CALL(sylvan_constrain, aHigh, bHigh, level);
        REFS_PUSH(high);
    }
    if (bLow != sylvan_true) {
        low = SYNC(sylvan_constrain);
        REFS_DESPAWN;
        REFS_PUSH(low);
    }
    result = sylvan_makenode(level, low, high);

    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_CONSTRAIN), b, 0, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result;
}

/**
 * Calculate restrict a @ b
 */
TASK_IMPL_3(BDD, sylvan_restrict, BDD, a, BDD, b, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (b == sylvan_true) return a;
    if (b == sylvan_false) return sylvan_false;
    if (sylvan_isconst(a)) return a;
    if (a == b) return sylvan_true;
    if (a == sylvan_not(b)) return sylvan_false;

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count operation */
    SV_CNT_OP(C_restrict);

    // a != constant and b != constant
    bddnode_t na = GETNODE(a);
    bddnode_t nb = GETNODE(b);

    BDDVAR level = na->level < nb->level ? na->level : nb->level;

    /* Consult cache */
    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_RESTRICT), b, 0, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return result;
        }
    }

    BDD result;
    REFS_INIT;

    if (nb->level < na->level) {
        BDD c = CALL(sylvan_ite, node_low(b,nb), sylvan_true, node_high(b,nb), 0);
        REFS_PUSH(c);
        result = CALL(sylvan_restrict, a, c, level);
    } else {
        BDD aLow=node_low(a,na),aHigh=node_high(a,na),bLow=b,bHigh=b;
        if (na->level == nb->level) {
            bLow = node_low(b,nb);
            bHigh = node_high(b,nb);
        }
        if (bLow == sylvan_false) {
            result = CALL(sylvan_restrict, aHigh, bHigh, level);
        } else if (bHigh == sylvan_false) {
            result = CALL(sylvan_restrict, aLow, bLow, level);
        } else {
            REFS_SPAWN(SPAWN(sylvan_restrict, aLow, bLow, level));
            BDD high = CALL(sylvan_restrict, aHigh, bHigh, level);
            REFS_PUSH(high);
            BDD low = SYNC(sylvan_restrict);
            REFS_DESPAWN;
            REFS_PUSH(low);
            result = sylvan_makenode(level, low, high);
        }
    }

    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_RESTRICT), b, 0, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result;
}

/**
 * Calculates \exists variables . a
 */
TASK_IMPL_3(BDD, sylvan_exists, BDD, a, BDD, variables, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (sylvan_isconst(a)) return a;
    if (sylvan_set_isempty(variables)) return a;

    sylvan_gc_test();

    SV_CNT_OP(C_exists);

    // a != constant
    bddnode_t na = GETNODE(a);
    BDDVAR level = na->level;

    // Get cofactors
    BDD aLow = node_low(a, na);
    BDD aHigh = node_high(a, na);

    while (!sylvan_set_isempty(variables) && sylvan_var(variables) < level) {
        // Skip variables before x
        variables = sylvan_set_next(variables);
    }

    if (sylvan_set_isempty(variables)) return a; // again, trivial case

    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_EXISTS), variables, 0, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return result;
        }
    }

    BDD result;
    REFS_INIT;

    if (sylvan_set_var(variables) == level) {
        // level is in variable set, perform abstraction
        BDD low = CALL(sylvan_exists, aLow, sylvan_set_next(variables), level);
        if (low == sylvan_true) {
            result = sylvan_true;
        } else {
            REFS_PUSH(low);
            BDD high = CALL(sylvan_exists, aHigh, sylvan_set_next(variables), level);
            if (high == sylvan_true) {
                result = sylvan_true;
            }
            else if (low == sylvan_false && high == sylvan_false) {
                result = sylvan_false;
            }
            else {
                REFS_PUSH(high);
                result = CALL(sylvan_ite, low, sylvan_true, high, 0); // or
            }
        }
    } else {
        // level is not in variable set
        BDD low, high;
        REFS_SPAWN(SPAWN(sylvan_exists, aHigh, variables, level));
        low = CALL(sylvan_exists, aLow, variables, level);
        REFS_PUSH(low);
        high = SYNC(sylvan_exists);
        REFS_DESPAWN;
        REFS_PUSH(high);
        result = sylvan_makenode(level, low, high);
    }

    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_EXISTS), variables, 0, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result;
}

TASK_IMPL_4(BDD, sylvan_relprod_paired, BDD, a, BDD, b, BDDSET, vars, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (a == sylvan_true && b == sylvan_true) return sylvan_true;
    if (a == sylvan_false || b == sylvan_false) return sylvan_false;
    if (a == b) b = sylvan_true;
    else if (a == sylvan_not(b)) return sylvan_false;

    if (sylvan_set_isempty(vars)) {
        assert(b == sylvan_true);
        return a;
    }

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count operation */
    SV_CNT_OP(C_relprod_paired);

    /* Determine top level */
    bddnode_t na = sylvan_isconst(a) ? 0 : GETNODE(a);
    bddnode_t nb = sylvan_isconst(b) ? 0 : GETNODE(b);

    BDDVAR level;
    if (na && nb) level = na->level < nb->level ? na->level : nb->level;
    else if (na) level = na->level;
    else level = nb->level;

    /* Skip vars */
    bddnode_t vars_node = GETNODE(vars);
    while (vars_node->level < level) {
        vars = node_low(vars, vars_node);
        if (sylvan_set_isempty(vars)) {
            assert(b == sylvan_true); // if no more vars in trans, should be done now
            return a;
        }
        vars_node = GETNODE(vars);
    }

    /* Consult cache */
    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_RELPROD_PAIRED), b, vars, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return result;
        }
    }

    /* Determine cofactors */
    BDD aLow=a, aHigh=a, bLow=b, bHigh=b;
    if (na && na->level == level) {
        aLow = node_low(a, na);
        aHigh = node_high(a, na);
    }
    if (nb && nb->level == level) {
        bLow = node_low(b, nb);
        bHigh = node_high(b, nb);
    }

    /* Recursively calculate low and high */
    BDD low=sylvan_invalid, high=sylvan_invalid, result;

    REFS_INIT;

    if (vars_node->level == level) {
        BDD _vars = node_low(vars, vars_node);
        if ((level & 1) == 0) {
            // quantify
            low = CALL(sylvan_relprod_paired, aLow, bLow, _vars, level);
            if (low == sylvan_true) {
                result = sylvan_true;
            }
            else {
                REFS_PUSH(low);
                high = CALL(sylvan_relprod_paired, aHigh, bHigh, _vars, level);
                if (high == sylvan_true) { result = sylvan_true; }
                else if (low == sylvan_false && high == sylvan_false) { result = sylvan_false; }
                else {
                    REFS_PUSH(high);
                    result = CALL(sylvan_ite, low, sylvan_true, high, 0);
                }
            }
        } else {
            // substitute
            if (aLow == sylvan_true && bLow == sylvan_true) low = sylvan_true;
            else if (aLow == sylvan_false || bLow == sylvan_false) low = sylvan_false;
            else if (aLow == bLow) low = sylvan_true;
            else if (BDD_EQUALM(aLow, bLow)) low = sylvan_false;
            else { REFS_SPAWN(SPAWN(sylvan_relprod_paired, aLow, bLow, _vars, level)); }
            high = CALL(sylvan_relprod_paired, aHigh, bHigh, _vars, level);
            REFS_PUSH(high);
            if (low == sylvan_invalid) {
                low = SYNC(sylvan_relprod_paired);
                REFS_DESPAWN;
                REFS_PUSH(low);
            }
            result = sylvan_makenode(level-1, low, high);
        }
    } else {
        // no quantify or substitute, just "and"
        if (aLow == sylvan_true && bLow == sylvan_true) low = sylvan_true;
        else if (aLow == sylvan_false || bLow == sylvan_false) low = sylvan_false;
        else if (aLow == bLow) low = sylvan_true;
        else if (BDD_EQUALM(aLow, bLow)) low = sylvan_false;
        else { REFS_SPAWN(SPAWN(sylvan_relprod_paired, aLow, bLow, vars, level)); }
        high = CALL(sylvan_relprod_paired, aHigh, bHigh, vars, level);
        REFS_PUSH(high);
        if (low == sylvan_invalid) {
            low = SYNC(sylvan_relprod_paired);
            REFS_DESPAWN;
            REFS_PUSH(low);
        }
        result = sylvan_makenode(level, low, high);
    }

    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_RELPROD_PAIRED), b, vars, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result;
}

TASK_IMPL_4(BDD, sylvan_relprod_paired_prev, BDD, a, BDD, b, BDD, vars, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (a == sylvan_true && b == sylvan_true) return sylvan_true;
    if (a == sylvan_false || b == sylvan_false) return sylvan_false;

    if (sylvan_set_isempty(vars)) {
        assert(b == sylvan_true);
        return a;
    }    

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count operation */
    SV_CNT_OP(C_relprod_paired_prev);

    /* Determine top level */
    bddnode_t na = sylvan_isconst(a) ? 0 : GETNODE(a);
    bddnode_t nb = sylvan_isconst(b) ? 0 : GETNODE(b);

    BDDVAR level;
    if (na && nb) level = na->level < nb->level ? na->level : nb->level;
    else if (na) level = na->level;
    else level = nb->level;

    /* Skip vars */
    bddnode_t vars_node = GETNODE(vars);
    while (vars_node->level < level) {
        vars = node_low(vars, vars_node);
        if (sylvan_set_isempty(vars)) {
            assert(b == sylvan_true); // if no more vars in trans, should be done now
            return a;
        }
        vars_node = GETNODE(vars);
    }

    /* Consult cache */
    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_RELPROD_PAIRED_PREV), b, vars, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return result;
        }
    }

    /* Determine cofactors */
    BDD aLow = a, aHigh = a;
    BDD bLow = b, bHigh = b;
    if (na && na->level == level) {
        aLow = node_low(a, na);
        aHigh = node_high(a, na);
    }
    if (nb && nb->level == level) {
        bLow = node_low(b, nb);
        bHigh = node_high(b, nb);
    }

    BDD low=sylvan_invalid, high=sylvan_invalid, result;
    REFS_INIT;

    /* Determine cases */
    if (level != vars_node->level) {
        // not in transition relation
        REFS_SPAWN(SPAWN(sylvan_relprod_paired_prev, aLow, bLow, vars, level));
        high = CALL(sylvan_relprod_paired_prev, aHigh, bHigh, vars, level);
        REFS_PUSH(high);
        low = SYNC(sylvan_relprod_paired_prev);
        REFS_DESPAWN;
        REFS_PUSH(low);
        result = sylvan_makenode(level, low, high);
    } else if ((level & 1) == 1) {
        // in transition relation, but primed variable!
        // i.e. a does not match, but b does
        BDD _vars = node_low(vars, vars_node);
        REFS_SPAWN(SPAWN(sylvan_relprod_paired_prev, aLow, bLow, _vars, level));
        high = CALL(sylvan_relprod_paired_prev, aHigh, bHigh, _vars, level);
        REFS_PUSH(high);
        low = SYNC(sylvan_relprod_paired_prev);
        REFS_DESPAWN;
        REFS_PUSH(low);
        result = CALL(sylvan_ite, low, sylvan_true, high, 0);
    } else {
        // in transition relation, and a or b is normal variable
        if (nb) {
            if (nb->level == level) {
                // consume nb, then run same level to match a and b
                REFS_SPAWN(SPAWN(sylvan_relprod_paired_prev, a, bLow, vars, level));
                high = CALL(sylvan_relprod_paired_prev, a, bHigh, vars, level);
                REFS_PUSH(high);
                low = SYNC(sylvan_relprod_paired_prev);
                REFS_DESPAWN;
                REFS_PUSH(low);
                result = sylvan_makenode(level, low, high);
            } else if (nb->level == level+1) {
                // match a and b
                // transition from 'any' to 'both'...
                // return 'quantify' // OR
                BDD _vars = node_low(vars, vars_node);
                bLow = node_low(b, nb);
                bHigh = node_high(b, nb);
                REFS_SPAWN(SPAWN(sylvan_relprod_paired_prev, aLow, bLow, _vars, level));
                high = CALL(sylvan_relprod_paired_prev, aHigh, bHigh, _vars, level);
                REFS_PUSH(high);
                low = SYNC(sylvan_relprod_paired_prev);
                REFS_DESPAWN;
                REFS_PUSH(low);
                result = CALL(sylvan_ite, low, sylvan_true, high, 0);
            } else {
                // match for a but not for b
                BDD _vars = node_low(vars, vars_node);
                REFS_SPAWN(SPAWN(sylvan_relprod_paired_prev, aLow, bLow, _vars, level));
                high = CALL(sylvan_relprod_paired_prev, aHigh, bHigh, _vars, level);
                REFS_PUSH(high);
                low = SYNC(sylvan_relprod_paired_prev);
                REFS_DESPAWN;
                REFS_PUSH(low);
                result = CALL(sylvan_ite, low, sylvan_true, high, 0);
            }
        } else {
            // match for a but not for b
            BDD _vars = node_low(vars, vars_node);
            REFS_SPAWN(SPAWN(sylvan_relprod_paired_prev, aLow, bLow, _vars, level));
            high = CALL(sylvan_relprod_paired_prev, aHigh, bHigh, _vars, level);
            REFS_PUSH(high);
            low = SYNC(sylvan_relprod_paired_prev);
            REFS_DESPAWN;
            REFS_PUSH(low);
            result = CALL(sylvan_ite, low, sylvan_true, high, 0);
        }
    }

    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_RELPROD_PAIRED_PREV), b, vars, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result;
}

/**
 * Function composition
 */
TASK_IMPL_3(BDD, sylvan_compose, BDD, a, BDDMAP, map, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (a == sylvan_false || a == sylvan_true) return a;
    if (sylvan_map_isempty(map)) return a;

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count operation */
    SV_CNT_OP(C_compose);

    /* Determine top level */
    bddnode_t n = GETNODE(a);
    BDDVAR level = n->level;

    /* Skip map */
    bddnode_t map_node = GETNODE(map);
    while (map_node->level < level) {
        map = node_low(map, map_node);
        if (sylvan_map_isempty(map)) return a;
        map_node = GETNODE(map);
    }

    /* Consult cache */
    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != level / granularity;
    if (cachenow) {
        BDD result;
        if (cache_get(BDD_SETDATA(a, CACHE_COMPOSE), map, 0, &result)) {
            SV_CNT_CACHE(C_cache_reuse);
            return result;
        }
    }

    REFS_INIT;

    /* Recursively calculate low and high */
    REFS_SPAWN(SPAWN(sylvan_compose, node_low(a, n), map, level));
    BDD high = CALL(sylvan_compose, node_high(a, n), map, level);
    REFS_PUSH(high);
    BDD low = SYNC(sylvan_compose);
    REFS_DESPAWN;
    REFS_PUSH(low);

    /* Calculate result */
    BDD root = map_node->level == level ? node_high(map, map_node) : sylvan_ithvar(level);
    REFS_PUSH(root);
    BDD result = CALL(sylvan_ite, root, high, low, 0);

    REFS_EXIT;

    if (cachenow) {
        if (cache_put(BDD_SETDATA(a, CACHE_COMPOSE), map, 0, result)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result;
}


/**
 * Count number of nodes for each level
 */

// TODO: use AVL

void sylvan_nodecount_levels_do_1(BDD bdd, uint32_t *variables)
{
    if (!sylvan_isnode(bdd)) return;

    bddnode_t na = GETNODE(bdd);
    if (na->data & 1) return;
    variables[na->level]++;
    na->data |= 1; // mark
    sylvan_nodecount_levels_do_1(na->low, variables);
    sylvan_nodecount_levels_do_1(na->high, variables);
}

void sylvan_nodecount_levels_do_2(BDD bdd)
{
    if (!sylvan_isnode(bdd)) return;

    bddnode_t na = GETNODE(bdd);
    if (!(na->data & 1)) return;
    na->data &= ~1; // unmark
    sylvan_nodecount_levels_do_2(na->low);
    sylvan_nodecount_levels_do_2(na->high);
}

void sylvan_nodecount_levels(BDD bdd, uint32_t *variables)
{
    sylvan_nodecount_levels_do_1(bdd, variables);
    sylvan_nodecount_levels_do_2(bdd);
}

/**
 * Count number of nodes in BDD
 */

uint64_t sylvan_nodecount_do_1(BDD a)
{
    if (sylvan_isconst(a)) return 0;
    bddnode_t na = GETNODE(a);
    if (na->data & 1) return 0;
    na->data |= 1; // mark
    uint64_t result = 1;
    result += sylvan_nodecount_do_1(na->low);
    result += sylvan_nodecount_do_1(na->high);
    return result;
}

void sylvan_nodecount_do_2(BDD a)
{
    if (sylvan_isconst(a)) return;
    bddnode_t na = GETNODE(a);
    if (!(na->data & 1)) return;
    na->data &= ~1; // unmark
    sylvan_nodecount_do_2(na->low);
    sylvan_nodecount_do_2(na->high);
}

size_t sylvan_nodecount(BDD a)
{
    uint32_t result = sylvan_nodecount_do_1(a);
    sylvan_nodecount_do_2(a);
    return result;
}

/**
 * CALCULATE NUMBER OF DISTINCT PATHS TO TRUE
 */

TASK_IMPL_1(long double, sylvan_pathcount, BDD, bdd)
{
    if (bdd == sylvan_false) return 0.0;
    if (bdd == sylvan_true) return 1.0;
    REFS_SPAWN(SPAWN(sylvan_pathcount, sylvan_low(bdd)));
    REFS_SPAWN(SPAWN(sylvan_pathcount, sylvan_high(bdd)));
    long double res1 = SYNC(sylvan_pathcount);
    REFS_DESPAWN
    res1 += SYNC(sylvan_pathcount);
    REFS_DESPAWN
    return res1;
}

/**
 * CALCULATE NUMBER OF VAR ASSIGNMENTS THAT YIELD TRUE
 */

TASK_IMPL_3(sylvan_satcount_double_t, sylvan_satcount_cached, BDD, bdd, BDDSET, variables, BDDVAR, prev_level)
{
    /* Trivial cases */
    if (bdd == sylvan_false) return 0.0;
    if (bdd == sylvan_true) return powl(2.0L, sylvan_set_count(variables));

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count variables before var(bdd) */
    size_t skipped = 0;
    BDDVAR var = sylvan_var(bdd);
    bddnode_t set_node = GETNODE(variables);
    while (var != set_node->level) {
        skipped++;
        variables = node_low(variables, set_node);
        // if this assertion fails, then variables is not the support of <bdd>
        assert(!sylvan_set_isempty(variables));
        set_node = GETNODE(variables);
    }

    /* Count operation */
    // SV_CNT_OP(C_satcount);
    
    union {
        sylvan_satcount_double_t d;
        uint64_t s;
    } hack;

    /* Consult cache */
    int cachenow = granularity < 2 || prev_level == 0 ? 1 : prev_level / granularity != var / granularity;
    if (cachenow) {
        if (cache_get(BDD_SETDATA(bdd, CACHE_SATCOUNT), variables, 0, &hack.s)) {
            SV_CNT_CACHE(C_cache_reuse);
            return hack.d * powl(2.0L, skipped);
        }
    }

    REFS_SPAWN(SPAWN(sylvan_satcount_cached, sylvan_high(bdd), node_low(variables, set_node), var));
    sylvan_satcount_double_t low = CALL(sylvan_satcount_cached, sylvan_low(bdd), node_low(variables, set_node), var);
    sylvan_satcount_double_t result = (low + SYNC(sylvan_satcount_cached));
    REFS_DESPAWN

    if (cachenow) {
        hack.d = result;
        if (cache_put(BDD_SETDATA(bdd, CACHE_SATCOUNT), variables, 0, hack.s)) {
            SV_CNT_CACHE(C_cache_new);
        } else {
            SV_CNT_CACHE(C_cache_exists);
        }
    }

    return result * powl(2.0L, skipped);
}

TASK_IMPL_2(long double, sylvan_satcount, BDD, bdd, BDD, variables)
{
    /* Trivial cases */
    if (bdd == sylvan_false) return 0.0;
    if (bdd == sylvan_true) return powl(2.0L, sylvan_set_count(variables));

    /* Perhaps execute garbage collection */
    sylvan_gc_test();

    /* Count variables before var(bdd) */
    size_t skipped = 0;
    BDDVAR var = sylvan_var(bdd);
    bddnode_t set_node = GETNODE(variables);
    BDDVAR var_var = set_node->level;
    while (var != var_var) {
        if (var < var_var) {
            fprintf(stderr, "sylvan_satcount: var %d is not in variables!\n", var);
            assert(0);
        }
        skipped++;
        variables = node_low(variables, set_node);
        if (sylvan_set_isempty(variables)) {
            fprintf(stderr, "sylvan_satcount: var %d is not in variables!\n", var);
            assert(0);
        }
        set_node = GETNODE(variables);
        if (var_var >= set_node->level) {
            fprintf(stderr, "sylvan_satcount: bad order in variables! (%d >= %d)\n", var_var, set_node->level);
            assert(0);
        }
        var_var = set_node->level;
    }

    /* Count operation */
    // SV_CNT_OP(C_satcount);

    REFS_SPAWN(SPAWN(sylvan_satcount, sylvan_high(bdd), node_low(variables, set_node)));
    long double low = CALL(sylvan_satcount, sylvan_low(bdd), node_low(variables, set_node));
    long double result = (low + SYNC(sylvan_satcount)) * powl(2.0L, skipped);
    REFS_DESPAWN
    return result;
}

static void
gnomesort_bddvars(BDDVAR* arr, size_t size)
{
    size_t i=0;
    while (i<size) {
        if (i == 0 || arr[i-1] <= arr[i]) i++;
        else { BDDVAR tmp = arr[i]; arr[i] = arr[i-1]; arr[--i] = tmp; }
    }
}

int
sylvan_sat_one(BDD bdd, BDDVAR *vars, size_t cnt, char* str)
{
    if (bdd == sylvan_false) return 0;
    if (str == NULL) return 0;

    BDDVAR *sorted_vars = (BDDVAR*)alloca(sizeof(BDDVAR)*cnt);
    memcpy(sorted_vars, vars, sizeof(BDDVAR)*cnt);
    gnomesort_bddvars(sorted_vars, cnt);

    size_t i = 0;
    for (i=0; i<cnt; i++) {
        BDDVAR var = sorted_vars[i];

        size_t idx=0;
        for (idx=0; vars[idx]!=var; idx++) {}

        if (bdd != sylvan_true) {
            bddnode_t node = GETNODE(bdd);
            if (node->level == var) {
                BDD lowedge = node_low(bdd, node);
                BDD highedge = node_high(bdd, node);
                if (highedge == sylvan_false) {
                    // take low edge
                    bdd = lowedge;
                    str[idx++] = 0;
                } else if (lowedge == sylvan_false) {
                    // take high edge
                    bdd = highedge;
                    str[idx++] = 1;
                } else {
                    // take random edge
                    if (rand() & 0x2000) {
                        bdd = lowedge;
                        str[idx++] = 0;
                    } else {
                        bdd = highedge;
                        str[idx++] = 1;
                    }
                }
                continue;
            }
        }
        str[idx++] = 2;
    }

    return 1;
}

BDD
sylvan_sat_one_bdd(BDD bdd)
{
    if (bdd == sylvan_false) return sylvan_false;
    if (bdd == sylvan_true) return sylvan_true;

    bddnode_t node = GETNODE(bdd);
    BDD low = node_low(bdd, node);
    BDD high = node_high(bdd, node);

    REFS_INIT;
    BDD m;

    BDD result;
    if (low == sylvan_false) {
        m = sylvan_sat_one_bdd(high);
        REFS_PUSH(m);
        result = sylvan_makenode(node->level, sylvan_false, m);
    } else if (high == sylvan_false) {
        m = sylvan_sat_one_bdd(low);
        REFS_PUSH(m);
        result = sylvan_makenode(node->level, m, sylvan_false);
    } else {
        if (rand() & 0x2000) {
            m = sylvan_sat_one_bdd(low);
            REFS_PUSH(m);
            result = sylvan_makenode(node->level, m, sylvan_false);
        } else {
            m = sylvan_sat_one_bdd(high);
            REFS_PUSH(m);
            result = sylvan_makenode(node->level, sylvan_false, m);
        }
    }

    REFS_EXIT;

    return result;
}

BDD
sylvan_cube(BDDSET vars, char *cube)
{
    if (vars == sylvan_false) return sylvan_true;

    bddnode_t n = GETNODE(vars);
    BDDVAR v = n->level;
    vars = node_low(vars, n);

    BDD result = sylvan_cube(vars, cube+1);
    if (*cube == 0) {
        REFS_INIT;
        REFS_PUSH(result);
        result = sylvan_makenode(v, result, sylvan_false);
        REFS_EXIT;
    } else if (*cube == 1) {
        REFS_INIT;
        REFS_PUSH(result);
        result = sylvan_makenode(v, sylvan_false, result);
        REFS_EXIT;
    }

    return result;
}

TASK_IMPL_3(BDD, sylvan_union_cube, BDD, bdd, BDDSET, vars, char*, cube)
{
    /* Terminal cases */
    if (bdd == sylvan_true) return sylvan_true;
    if (bdd == sylvan_false) return sylvan_cube(vars, cube);
    if (vars == sylvan_false) return sylvan_true;

    bddnode_t nv = GETNODE(vars);

    for (;;) {
        if (*cube == 0 || *cube == 1) break;
        // *cube should be 2
        cube++;
        vars = node_low(vars, nv);
        if (vars == sylvan_false) return sylvan_true;
        nv = GETNODE(vars);
    }

    sylvan_gc_test();

    // missing: SV_CNT_OP
    
    bddnode_t n = GETNODE(bdd);
    BDD result = bdd;
    BDDVAR v = nv->level;

    REFS_INIT;
    if (v < n->level) {
        vars = node_low(vars, nv);
        if (*cube == 0) {
            result = sylvan_union_cube(bdd, vars, cube+1);
            REFS_PUSH(result);
            result = sylvan_makenode(v, result, bdd);
        } else /* *cube == 1 */ {
            result = sylvan_union_cube(bdd, vars, cube+1);
            REFS_PUSH(result);
            result = sylvan_makenode(v, bdd, result);
        }
    } else if (v > n->level) {
        BDD high = node_high(bdd, n);
        BDD low = node_low(bdd, n);
        SPAWN(sylvan_union_cube, high, vars, cube);
        BDD new_low = sylvan_union_cube(low, vars, cube);
        REFS_PUSH(new_low);
        BDD new_high = SYNC(sylvan_union_cube);
        if (new_low != low || new_high != high) {
            REFS_PUSH(new_high);
            result = sylvan_makenode(n->level, new_low, new_high);
        }
    } else /* v == n->level */ {
        vars = node_low(vars, nv);
        BDD high = node_high(bdd, n);
        BDD low = node_low(bdd, n);
        if (*cube == 0) {
            BDD new_low = sylvan_union_cube(low, vars, cube+1);
            if (new_low != low) {
                REFS_PUSH(new_low);
                result = sylvan_makenode(n->level, new_low, high);
            }
        } else /* *cube == 1 */ {
            BDD new_high = sylvan_union_cube(high, vars, cube+1);
            if (new_high != high) {
                REFS_PUSH(new_high);
                result = sylvan_makenode(n->level, low, new_high);
            }
        }
    }
    REFS_EXIT;

    return result;
}

/**
 * IMPLEMENTATION OF BDDSET
 */

int
sylvan_set_in(BDDSET set, BDDVAR level)
{
    while (!sylvan_set_isempty(set)) {
        bddnode_t n = GETNODE(set);
        if (n->level == level) return 1;
        if (n->level > level) return 0; // BDDs are ordered
        set = node_low(set, n);
    }

    return 0;
}

size_t
sylvan_set_count(BDDSET set)
{
    size_t result = 0;
    for (;!sylvan_set_isempty(set);set = sylvan_set_next(set)) result++;
    return result;
}

void
sylvan_set_toarray(BDDSET set, BDDVAR *arr)
{
    size_t i = 0;
    while (!sylvan_set_isempty(set)) {
        bddnode_t n = GETNODE(set);
        arr[i++] = n->level;
        set = node_low(set, n);
    }
}

TASK_IMPL_2(BDDSET, sylvan_set_fromarray, BDDVAR*, arr, size_t, length)
{
    if (length == 0) return sylvan_set_empty();
    REFS_INIT;
    BDDSET sub = sylvan_set_fromarray(arr+1, length-1);
    REFS_PUSH(sub);
    BDDSET result = sylvan_set_add(sub, *arr);
    REFS_EXIT;
    return result;
}

void
sylvan_test_isset(BDDSET set)
{
    while (set != sylvan_false) {
        assert(set != sylvan_true);
        assert(llmsset_is_marked(nodes, set));
        bddnode_t n = GETNODE(set);
        assert(node_high(set, n) == sylvan_true);
        set = node_low(set, n);
    }
}

/**
 * IMPLEMENTATION OF BDDMAP
 */

BDDMAP
sylvan_map_add(BDDMAP map, BDDVAR key, BDD value)
{
    if (sylvan_map_isempty(map)) return sylvan_makenode(key, sylvan_map_empty(), value);

    bddnode_t n = GETNODE(map);
    BDDVAR key_m = n->level;

    if (key_m < key) {
        // add recursively and rebuild tree
        REFS_INIT;
        BDDMAP low = sylvan_map_add(node_low(map, n), key, value);
        REFS_PUSH(low);
        BDDMAP result = sylvan_makenode(key_m, low, node_high(map, n));
        REFS_EXIT;
        return result;
    } else if (key_m > key) {
        return sylvan_makenode(key, map, value);
    } else {
        // replace old
        return sylvan_makenode(key, node_low(map, n), value);
    }
}

BDDMAP
sylvan_map_addall(BDDMAP map_1, BDDMAP map_2)
{
    // one of the maps is empty
    if (sylvan_map_isempty(map_1)) return map_2;
    if (sylvan_map_isempty(map_2)) return map_1;

    bddnode_t n_1 = GETNODE(map_1);
    BDDVAR key_1 = n_1->level;

    bddnode_t n_2 = GETNODE(map_2);
    BDDVAR key_2 = n_2->level;

    REFS_INIT;
    BDDMAP result;
    if (key_1 < key_2) {
        // key_1, recurse on n_1->low, map_2
        BDDMAP low = sylvan_map_addall(node_low(map_1, n_1), map_2);
        REFS_PUSH(low);
        result = sylvan_makenode(key_1, low, node_high(map_1, n_1));
    } else if (key_1 > key_2) {
        // key_2, recurse on map_1, n_2->low
        BDDMAP low = sylvan_map_addall(map_1, node_low(map_2, n_2));
        REFS_PUSH(low);
        result = sylvan_makenode(key_2, low, node_high(map_2, n_2));
    } else {
        // equal: key_2, recurse on n_1->low, n_2->low
        BDDMAP low = sylvan_map_addall(node_low(map_1, n_1), node_low(map_2, n_2));
        REFS_PUSH(low);
        result = sylvan_makenode(key_2, low, node_high(map_2, n_2));
    }
    REFS_EXIT;
    return result;
}

BDDMAP
sylvan_map_remove(BDDMAP map, BDDVAR key)
{
    if (sylvan_map_isempty(map)) return map;

    bddnode_t n = GETNODE(map);
    BDDVAR key_m = n->level;

    if (key_m < key) {
        REFS_INIT;
        BDDMAP low = sylvan_map_remove(node_low(map, n), key);
        REFS_PUSH(low);
        BDDMAP result = sylvan_makenode(key_m, low, node_high(map, n));
        REFS_EXIT;
        return result;
    } else if (key_m > key) {
        return map;
    } else {
        return node_low(map, n);
    }
}

BDDMAP
sylvan_map_removeall(BDDMAP map, BDDMAP toremove)
{
    if (sylvan_map_isempty(map)) return map;
    if (sylvan_map_isempty(toremove)) return map;

    bddnode_t n_1 = GETNODE(map);
    BDDVAR key_1 = n_1->level;

    bddnode_t n_2 = GETNODE(toremove);
    BDDVAR key_2 = n_2->level;

    if (key_1 < key_2) {
        REFS_INIT;
        BDDMAP low = sylvan_map_removeall(node_low(map, n_1), toremove);
        REFS_PUSH(low);
        BDDMAP result = sylvan_makenode(key_1, low, node_high(map, n_1));
        REFS_EXIT;
        return result;
    } else if (key_1 > key_2) {
        return sylvan_map_removeall(map, node_low(toremove, n_2));
    } else {
        return sylvan_map_removeall(node_low(map, n_1), node_low(toremove, n_2));
    }
}

int
sylvan_map_in(BDDMAP map, BDDVAR key)
{
    while (!sylvan_map_isempty(map)) {
        bddnode_t n = GETNODE(map);
        if (n->level == key) return 1;
        if (n->level > key) return 0; // BDDs are ordered
        map = node_low(map, n);
    }

    return 0;
}

size_t
sylvan_map_count(BDDMAP map)
{
    size_t r=0;
    while (!sylvan_map_isempty(map)) { r++; map=sylvan_map_next(map); }
    return r;
}

BDDMAP
sylvan_set_to_map(BDDSET set, BDD value)
{
    if (sylvan_set_isempty(set)) return sylvan_map_empty();
    REFS_INIT;
    bddnode_t set_n = GETNODE(set);
    BDD sub = sylvan_set_to_map(node_low(set, set_n), value); 
    REFS_PUSH(sub);
    BDD result = sylvan_makenode(sub, set_n->level, value);
    REFS_EXIT;
    return result;
}

/**
 * Determine the support of a BDD (all variables used in the BDD)
 */

TASK_IMPL_1(BDD, sylvan_support, BDD, bdd)
{
    if (!sylvan_isnode(bdd)) return sylvan_false;

    bddnode_t n = GETNODE(bdd);
    BDD high, low, set, result;

    REFS_INIT;
    REFS_SPAWN(SPAWN(sylvan_support, n->low));
    high = CALL(sylvan_support, n->high);
    REFS_PUSH(high);
    low = SYNC(sylvan_support);
    REFS_DESPAWN;
    REFS_PUSH(low);
    set = CALL(sylvan_ite, high, sylvan_true, low, 0);
    REFS_PUSH(set);
    result = CALL(sylvan_ite, sylvan_ithvar(n->level), sylvan_true, set, 0);
    REFS_EXIT;

    return result;
}

/**
 * GENERIC MARK/UNMARK (DATA FIELD IN BDD NODE) METHODS
 */

static inline int
sylvan_mark(bddnode_t node, unsigned int mark)
{
    if (node->data & mark) return 0;
    node->data |= mark;
    return 1;
}

static inline int
sylvan_unmark(bddnode_t node, unsigned int mark)
{
    if (node->data & mark) {
        node->data &= ~mark;
        return 1;
    } else {
        return 0;
    }
}

static __attribute__((unused)) void
sylvan_mark_rec(bddnode_t node, unsigned int mark)
{
    if (sylvan_mark(node, mark)) {
        if (!sylvan_isconst(node->low)) sylvan_mark_rec(GETNODE(node->low), mark);
        if (!sylvan_isconst(node->high)) sylvan_mark_rec(GETNODE(node->high), mark);
    }
}

static __attribute__((unused)) void
sylvan_unmark_rec(bddnode_t node, unsigned int mark)
{
    if (sylvan_unmark(node, mark)) {
        if (!sylvan_isconst(node->low)) sylvan_unmark_rec(GETNODE(node->low), mark);
        if (!sylvan_isconst(node->high)) sylvan_unmark_rec(GETNODE(node->high), mark);
    }
}

/**
 * fprint, print
 */
void
sylvan_fprint(FILE *f, BDD bdd)
{
    sylvan_serialize_reset();
    size_t v = sylvan_serialize_add(bdd);
    fprintf(f, "%s%zu,", bdd&sylvan_complement?"!":"", v);
    sylvan_serialize_totext(f);
}

void
sylvan_print(BDD bdd)
{
    sylvan_fprint(stdout, bdd);
}



/**
 * Output to .DOT files
 */

/***
 * We keep a set [level -> [node]] using AVLset
 */
struct level_to_nodeset {
    BDDVAR level;
    avl_node_t *set;
};

AVL(level_to_nodeset, struct level_to_nodeset)
{
    return left->level - right->level;
}

AVL(nodeset, BDD)
{
    return *left - *right;
}

static void __attribute__((noinline))
sylvan_dothelper_register(avl_node_t **set, BDD bdd)
{
    struct level_to_nodeset s, *ss;
    bddnode_t node = GETNODE(bdd);
    s.level = node->level;
    ss = level_to_nodeset_search(*set, &s);
    if (ss == NULL) {
        s.set = NULL;
        ss = level_to_nodeset_put(set, &s, NULL);
    }
    assert(ss != NULL);
    bdd = BDD_STRIPMARK(bdd);
    nodeset_insert(&ss->set, &bdd);
}

static void
sylvan_fprintdot_rec(FILE *out, BDD bdd, avl_node_t **levels)
{
    if (!sylvan_isnode(bdd)) return;

    bdd = BDD_STRIPMARK(bdd);
    bddnode_t n = GETNODE(bdd);
    if (!sylvan_mark(n, 1)) return;

    sylvan_dothelper_register(levels, bdd);

    fprintf(out, "%" PRIu64 " [label=\"%d\"];\n", bdd, n->level);

    sylvan_fprintdot_rec(out, n->low, levels);
    sylvan_fprintdot_rec(out, n->high, levels);

    fprintf(out, "%" PRIu64 " -> %" PRIu64 " [style=dashed];\n", bdd, (BDD)n->low);
    fprintf(out, "%" PRIu64 " -> %" PRIu64 " [style=solid dir=both arrowtail=%s];\n", bdd, (BDD)n->high, n->comp ? "dot" : "none");
}

void
sylvan_fprintdot(FILE *out, BDD bdd)
{
    fprintf(out, "digraph \"DD\" {\n");
    fprintf(out, "graph [dpi = 300];\n");
    fprintf(out, "center = true;\n");
    fprintf(out, "edge [dir = forward];\n");
    fprintf(out, "0 [shape=box, label=\"0\", style=filled, shape=box, height=0.3, width=0.3];\n");
    fprintf(out, "root [style=invis];\n");
    fprintf(out, "root -> %" PRIu64 " [style=solid dir=both arrowtail=%s];\n", BDD_STRIPMARK(bdd), BDD_HASMARK(bdd) ? "dot" : "none");

    avl_node_t *levels = NULL;
    sylvan_fprintdot_rec(out, bdd, &levels);

    if (levels != NULL) {
        size_t levels_count = avl_count(levels);
        struct level_to_nodeset *arr = level_to_nodeset_toarray(levels);
        size_t i;
        for (i=0;i<levels_count;i++) {
            fprintf(out, "{ rank=same; ");
            size_t node_count = avl_count(arr[i].set);
            size_t j;
            BDD *arr_j = nodeset_toarray(arr[i].set);
            for (j=0;j<node_count;j++) {
                fprintf(out, "%" PRIu64 "; ", arr_j[j]);
            }
            fprintf(out, "}\n");
        }
        level_to_nodeset_free(&levels);
    }

    fprintf(out, "}\n");
    if (!sylvan_isconst(bdd)) sylvan_unmark_rec(GETNODE(bdd), 1);
}

void
sylvan_printdot(BDD bdd)
{
    sylvan_fprintdot(stdout, bdd);
}

static void __attribute__((noinline))
sylvan_dothelper_nocomp_register(avl_node_t **set, BDD bdd)
{
    struct level_to_nodeset s, *ss;
    bddnode_t node = GETNODE(bdd);
    s.level = node->level;
    ss = level_to_nodeset_search(*set, &s);
    if (ss == NULL) {
        s.set = NULL;
        ss = level_to_nodeset_put(set, &s, NULL);
    }
    assert(ss != NULL);
    nodeset_insert(&ss->set, &bdd);
}

static void
sylvan_fprintdot_nocomp_rec(FILE *out, BDD bdd, avl_node_t **levels)
{
    if (!sylvan_isnode(bdd)) return;

    bddnode_t n = GETNODE(bdd);
    if (!sylvan_mark(n, 1)) return;

    sylvan_dothelper_nocomp_register(levels, bdd);

    fprintf(out, "%" PRIu64 " [label=\"%d\"];\n", bdd, n->level);

    sylvan_fprintdot_nocomp_rec(out, n->low, levels);
    sylvan_fprintdot_nocomp_rec(out, n->high, levels);

    fprintf(out, "%" PRIu64 " -> %" PRIu64 " [style=dashed];\n", bdd, (BDD)n->low);
    fprintf(out, "%" PRIu64 " -> %" PRIu64 " [style=solid];\n", bdd, (BDD)n->high);
}

void
sylvan_fprintdot_nocomp(FILE *out, BDD bdd)
{
    // Bye comp
    bdd = sylvan_bdd_to_nocomp(bdd);

    fprintf(out, "digraph \"DD\" {\n");
    fprintf(out, "graph [dpi = 300];\n");
    fprintf(out, "center = true;\n");
    fprintf(out, "edge [dir = forward];\n");
    if (bdd != sylvan_true_nc) fprintf(out, "0 [shape=box, label=\"0\", style=filled, shape=box, height=0.3, width=0.3];\n");
    if (bdd != sylvan_false) fprintf(out, "%" PRIu64 " [shape=box, label=\"1\", style=filled, shape=box, height=0.3, width=0.3];\n", sylvan_true_nc);
    fprintf(out, "root [style=invis];\n");
    fprintf(out, "root -> %" PRIu64 " [style=solid];\n", bdd);

    avl_node_t *levels = NULL;
    sylvan_fprintdot_nocomp_rec(out, bdd, &levels);

    if (levels != NULL) {
        size_t levels_count = avl_count(levels);
        struct level_to_nodeset *arr = level_to_nodeset_toarray(levels);
        size_t i;
        for (i=0;i<levels_count;i++) {
            fprintf(out, "{ rank=same; ");
            size_t node_count = avl_count(arr[i].set);
            size_t j;
            BDD *arr_j = nodeset_toarray(arr[i].set);
            for (j=0;j<node_count;j++) {
                fprintf(out, "%" PRIu64 "; ", arr_j[j]);
            }
            fprintf(out, "}\n");
        }
        level_to_nodeset_free(&levels);
    }

    if (!sylvan_isconst(bdd)) fprintf(out, "{ rank=same; 0; %" PRIu64 "; }\n", sylvan_true_nc);

    fprintf(out, "}\n");
    if (!sylvan_isconst(bdd)) sylvan_unmark_rec(GETNODE(bdd), 1);
}

void
sylvan_printdot_nocomp(BDD bdd)
{
    sylvan_fprintdot_nocomp(stdout, bdd);
}

/**
 * SERIALIZATION
 */

struct sylvan_ser {
    BDD bdd;
    size_t assigned;
};

// Define a AVL tree type with prefix 'sylvan_ser' holding
// nodes of struct sylvan_ser with the following compare() function...
AVL(sylvan_ser, struct sylvan_ser)
{
    return left->bdd - right->bdd;
}

// Define a AVL tree type with prefix 'sylvan_ser_reversed' holding 
// nodes of struct sylvan_ser with the following compare() function...
AVL(sylvan_ser_reversed, struct sylvan_ser)
{
    return left->assigned - right->assigned;
}

// Initially, both sets are empty
static avl_node_t *sylvan_ser_set = NULL;
static avl_node_t *sylvan_ser_reversed_set = NULL;

// Start counting (assigning numbers to BDDs) at 1
static size_t sylvan_ser_counter = 1;
static size_t sylvan_ser_done = 0;

// Given a BDD, assign unique numbers to all nodes
static size_t
sylvan_serialize_assign_rec(BDD bdd)
{
    if (sylvan_isnode(bdd)) {
        bddnode_t n = GETNODE(bdd);

        struct sylvan_ser s, *ss;
        s.bdd = BDD_STRIPMARK(bdd);
        ss = sylvan_ser_search(sylvan_ser_set, &s);
        if (ss == NULL) {
            // assign dummy value
            s.assigned = 0;
            ss = sylvan_ser_put(&sylvan_ser_set, &s, NULL);

            // first assign recursively
            sylvan_serialize_assign_rec(n->low);
            sylvan_serialize_assign_rec(n->high);

            // assign real value
            ss->assigned = sylvan_ser_counter++;

            // put a copy in the reversed table
            sylvan_ser_reversed_insert(&sylvan_ser_reversed_set, ss);
        }

        return ss->assigned;
    }

    return BDD_STRIPMARK(bdd);
}

size_t
sylvan_serialize_add(BDD bdd)
{
    return BDD_TRANSFERMARK(bdd, sylvan_serialize_assign_rec(bdd));
}

void
sylvan_serialize_reset()
{
    sylvan_ser_free(&sylvan_ser_set);
    sylvan_ser_free(&sylvan_ser_reversed_set);
    sylvan_ser_counter = 1;
    sylvan_ser_done = 0;
}

size_t
sylvan_serialize_get(BDD bdd)
{
    if (!sylvan_isnode(bdd)) return bdd;
    struct sylvan_ser s, *ss;
    s.bdd = BDD_STRIPMARK(bdd);
    ss = sylvan_ser_search(sylvan_ser_set, &s);
    assert(ss != NULL);
    return BDD_TRANSFERMARK(bdd, ss->assigned);
}

BDD
sylvan_serialize_get_reversed(size_t value)
{
    if (!sylvan_isnode(value)) return value;
    struct sylvan_ser s, *ss;
    s.assigned = BDD_STRIPMARK(value);
    ss = sylvan_ser_reversed_search(sylvan_ser_reversed_set, &s);
    assert(ss != NULL);
    return BDD_TRANSFERMARK(value, ss->bdd);
}

void
sylvan_serialize_totext(FILE *out)
{
    fprintf(out, "[");
    avl_iter_t *it = sylvan_ser_reversed_iter(sylvan_ser_reversed_set);
    struct sylvan_ser *s;

    while ((s=sylvan_ser_reversed_iter_next(it))) {
        BDD bdd = s->bdd;
        bddnode_t n = GETNODE(bdd);
        fprintf(out, "(%zu,%u,%zu,%zu,%u),", s->assigned,
                                             n->level,
                                             sylvan_serialize_get(n->low),
                                             sylvan_serialize_get(n->high),
                                             n->comp);
    }

    sylvan_ser_reversed_iter_free(it);
    fprintf(out, "]");
}

void
sylvan_serialize_tofile(FILE *out)
{
    size_t count = avl_count(sylvan_ser_reversed_set);
    assert(count >= sylvan_ser_done);
    assert(count == sylvan_ser_counter-1);
    count -= sylvan_ser_done;
    fwrite(&count, sizeof(size_t), 1, out);

    struct sylvan_ser *s;
    avl_iter_t *it = sylvan_ser_reversed_iter(sylvan_ser_reversed_set);

    /* Skip already written entries */
    size_t index = 0;
    while (index < sylvan_ser_done && (s=sylvan_ser_reversed_iter_next(it))) {
        index++;
        assert(s->assigned == index);
    }

    while ((s=sylvan_ser_reversed_iter_next(it))) {
        index++;
        assert(s->assigned == index);

        bddnode_t n = GETNODE(s->bdd);

        struct bddnode node;
        node.high = sylvan_serialize_get(n->high);
        node.low = sylvan_serialize_get(n->low);
        node.level = n->level;
        node.data = 0;
        node.comp = n->comp;

        assert(node.high < index);
        assert(node.low < index);

        fwrite(&node, sizeof(struct bddnode), 1, out);
    }

    sylvan_ser_done = sylvan_ser_counter-1;
    sylvan_ser_reversed_iter_free(it);
}

void
sylvan_serialize_fromfile(FILE *in)
{
    size_t count, i;
    assert(fread(&count, sizeof(size_t), 1, in) == 1);

    for (i=1; i<=count; i++) {
        struct bddnode node;
        assert(fread(&node, sizeof(struct bddnode), 1, in) == 1);

        assert(node.low <= sylvan_ser_done);
        assert(node.high <= sylvan_ser_done);

        BDD low = sylvan_serialize_get_reversed(node.low);
        BDD high = sylvan_serialize_get_reversed(node.high);
        if (node.comp) high |= sylvan_complement;

        struct sylvan_ser s;
        s.bdd = sylvan_makenode(node.level, low, high);
        s.assigned = ++sylvan_ser_done; // starts at 0 but we want 1-based...

        sylvan_ser_insert(&sylvan_ser_set, &s);
        sylvan_ser_reversed_insert(&sylvan_ser_reversed_set, &s);
    }
}

/**
 * Generate SHA2 structural hashes.
 * Hashes are independent of location.
 * Mainly useful for debugging purposes.
 */
static void
sylvan_sha2_rec(BDD bdd, SHA256_CTX *ctx)
{
    if (bdd == sylvan_true || bdd == sylvan_false) {
        SHA256_Update(ctx, (void*)&bdd, sizeof(BDD));
        return;
    }

    bddnode_t node = GETNODE(bdd);
    if (sylvan_mark(node, 1)) {
        uint32_t level = node->level;
        if (node->comp) level |= 0x80000000;
        SHA256_Update(ctx, (void*)&level, sizeof(uint32_t));
        sylvan_sha2_rec(node->high, ctx);
        sylvan_sha2_rec(node->low, ctx);
    }
}

void
sylvan_printsha(BDD bdd)
{
    sylvan_fprintsha(stdout, bdd);
}

void
sylvan_fprintsha(FILE *f, BDD bdd)
{
    char buf[80];
    sylvan_getsha(bdd, buf);
    fprintf(f, "%s", buf);
}

void
sylvan_getsha(BDD bdd, char *target)
{
    SHA256_CTX ctx;
    SHA256_Init(&ctx);
    sylvan_sha2_rec(bdd, &ctx);
    if (bdd != sylvan_true && bdd != sylvan_false) sylvan_unmark_rec(GETNODE(bdd), 1);
    SHA256_End(&ctx, target);
}

/**
 * Debug tool to check that a BDD is properly ordered.
 * Also that every BDD node is marked 'in-use' in the hash table.
 */
static void
sylvan_test_isbdd_rec(BDD bdd, BDDVAR parent)
{
    if (bdd == sylvan_true) return;
    if (bdd == sylvan_false) return;
    assert(llmsset_is_marked(nodes, BDD_STRIPMARK(bdd)));
    bddnode_t n = GETNODE(bdd);
    assert(parent < n->level);
    sylvan_test_isbdd_rec(node_low(bdd, n), n->level);
    sylvan_test_isbdd_rec(node_high(bdd, n), n->level);
}

void
sylvan_test_isbdd(BDD bdd)
{
    if (bdd == sylvan_true) return;
    if (bdd == sylvan_false) return;
    assert(llmsset_is_marked(nodes, BDD_STRIPMARK(bdd)));
    bddnode_t n = GETNODE(bdd);
    sylvan_test_isbdd_rec(node_low(bdd, n), n->level);
    sylvan_test_isbdd_rec(node_high(bdd, n), n->level);
}

