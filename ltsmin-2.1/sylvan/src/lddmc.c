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
#include <lddmc.h>
#include <refs.h>
#include <sha2.h>
#include <tls.h>

#if USE_NUMA
#include <numa.h>
#endif

/**
 * MDD node structure
 */
typedef struct __attribute__((packed)) mddnode {
    uint64_t a, b;
} * mddnode_t; // 16 bytes

// RmRR RRRR RRRR VVVV | VVVV DcDD DDDD DDDD (little endian - in memory)
// VVVV RRRR RRRR RRRm | DDDD DDDD DDDc VVVV (big endian)

// Ensure our mddnode is 16 bytes
typedef char __lddmc_check_mddnode_t_is_16_bytes[(sizeof(struct mddnode)==16) ? 1 : -1];

inline uint32_t
mddnode_getvalue(mddnode_t n)
{
    return *(uint32_t*)((uint8_t*)n+6);
}

inline uint8_t
mddnode_getmark(mddnode_t n)
{
    return n->a & 1;
}

inline uint8_t
mddnode_getcopy(mddnode_t n)
{
    return n->b & 0x10000 ? 1 : 0;
}

inline uint64_t
mddnode_getright(mddnode_t n)
{
    return (n->a & 0x0000ffffffffffff) >> 1;
}

inline uint64_t
mddnode_getdown(mddnode_t n)
{
    return n->b >> 17;
}

inline void
mddnode_setvalue(mddnode_t n, uint32_t value)
{
    *(uint32_t*)((uint8_t*)n+6) = value;
}

inline void
mddnode_setmark(mddnode_t n, uint8_t mark)
{
    n->a = (n->a & 0xfffffffffffffffe) | (mark ? 1 : 0);
}

inline void
mddnode_setright(mddnode_t n, uint64_t right)
{
    n->a = (n->a & 0xffff000000000001) | (right << 1);
}

inline void
mddnode_setdown(mddnode_t n, uint64_t down)
{
    n->b = (n->b & 0x000000000001ffff) | (down << 16);
}

inline void
mddnode_make(mddnode_t n, uint32_t value, uint64_t right, uint64_t down)
{
    n->a = right << 1;
    n->b = down << 17;
    *(uint32_t*)((uint8_t*)n+6) = value;
}

inline void
mddnode_makecopy(mddnode_t n, uint64_t right, uint64_t down)
{
    n->a = right << 1;
    n->b = ((down << 1) | 1) << 16;
}

static inline MDD MDD_SETDATA(MDD s, uint32_t data)
{
    return s | (((uint64_t)data) << 42);
}

#define GETNODE(mdd) ((mddnode_t)llmsset_index_to_ptr(nodes, mdd))

/**
 * Static global variables
 */

static int workers;

static llmsset_t nodes;

llmsset_t
__lddmc_get_internal_data()
{
    return nodes;
}

/**
 * Thread-local insert index for LLMSset
 */
DECLARE_THREAD_LOCAL(insert_index, uint64_t*);

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

MDD
lddmc_ref(MDD a)
{
    if (a == lddmc_true || a == lddmc_false) return a;
    refs_up(a);
    return a;
}

void
lddmc_deref(MDD a)
{
    if (a == lddmc_true || a == lddmc_false) return;
    refs_down(a);
}

size_t
lddmc_count_refs()
{
    return refs_count();
}

/**
* Internal references
*/
typedef struct ref_internal
{
    size_t r_size, r_count;
    size_t s_size, s_count;
    MDD *results;
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
    s->results = (MDD*)malloc(sizeof(MDD) * 128);
    s->spawns = (Task**)malloc(sizeof(Task*) * 128);
    SET_THREAD_LOCAL(ref_key, s);
    return s;
}

static __attribute__((noinline)) void
ref_resize_results()
{
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);
    ref_key->results = (MDD*)realloc(ref_key->results, sizeof(MDD) * (ref_key->r_size*=2));
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
    MDD result_to_mark = a;                                                             \
    if (result_to_mark > lddmc_true) {                                                  \
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

#define REFS_RESET                                                                      \
{                                                                                       \
    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);                                     \
    ref_key->r_count = mark_old_count;                                                  \
}

#define SSPAWN(...) REFS_SPAWN(SPAWN(__VA_ARGS__))
#define SSYNC(...) SYNC(__VA_ARGS__); REFS_DESPAWN

/**
 * Garbage collection
 */

static int gc_enabled = 1;
static volatile int gc = 0; // which gc phase
static barrier_t gcbar; // barrier

void
lddmc_gc_enable()
{
    gc_enabled = 1;
}

void
lddmc_gc_disable()
{
    gc_enabled = 0;
}

/* Recursively mark MDD nodes as 'in use' */
static void
lddmc_gc_mark_rec(MDD mdd)
{
    if (mdd <= lddmc_true) return;

    if (llmsset_mark_unsafe(nodes, mdd)) {
        mddnode_t n = GETNODE(mdd);
        lddmc_gc_mark_rec(mddnode_getright(n));
        lddmc_gc_mark_rec(mddnode_getdown(n));
    }
}

/* Mark external references */
static void
lddmc_gc_mark_external_refs(int my_id, int workers)
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
    while (it != NULL) lddmc_gc_mark_rec(refs_next(&it, end));
}

static void
lddmc_gc_go(int master)
{
    if (master && !cas(&gc, 0, 1)) master = 0;

    // phase 1: clear cache and hash array
    barrier_wait(&gcbar);
    if (master) cache_clear();

    LACE_ME;
    int my_id = LACE_WORKER_ID;
    llmsset_clear_multi(nodes, my_id, workers);

    // phase 2: mark nodes to keep
    barrier_wait(&gcbar);

    lddmc_gc_mark_external_refs(my_id, workers);

    LOCALIZE_THREAD_LOCAL(ref_key, ref_internal_t);
    if (ref_key) {
        size_t i = ref_key->r_count;
        for (i=0;i<ref_key->r_count;i++) lddmc_gc_mark_rec(ref_key->results[i]);
        for (i=0;i<ref_key->s_count;i++) {
            Task *t = ref_key->spawns[i];
            if (!TASK_IS_STOLEN(t)) break;
            if (TASK_IS_COMPLETED(t)) lddmc_gc_mark_rec(*(MDD*)TASK_RESULT(t));
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
TASK_0(void*, lddmc_lace_test_gc)
{
    if (gc) lddmc_gc_go(0);
    return 0;
}

/* Test called from every MDD operation */
static inline void
lddmc_gc_test()
{
    while (gc) lddmc_gc_go(0);
}

/* Manually perform garbage collection */
void
lddmc_gc()
{
    lddmc_gc_go(1);
}

/**
 * Primitives
 */

MDD
lddmc_makenode(uint32_t value, MDD ifeq, MDD ifneq)
{
    if (ifeq == lddmc_false) return ifneq;
 
    // check if correct (should be false, or next in value)
    assert(ifneq != lddmc_true);
    if (ifneq != lddmc_false) assert(value < mddnode_getvalue(GETNODE(ifneq)));

    LOCALIZE_THREAD_LOCAL(insert_index, uint64_t*);
    if (insert_index == NULL) insert_index = initialize_insert_index();

    struct mddnode n;
    mddnode_make(&n, value, ifneq, ifeq);

    uint64_t index;
    int created;
    if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
        if (gc_enabled) {
            //size_t before_gc = llmsset_get_filled(nodes);
            REFS_INIT;
            REFS_PUSH(ifeq);
            REFS_PUSH(ifneq);
            lddmc_gc_go(1);
            REFS_RESET;
            //size_t after_gc = llmsset_get_filled(nodes);
            //fprintf(stderr, "GC: %.01f%% to %.01f%%\n", 100.0*(double)before_gc/total, 100.0*(double)after_gc/total);
        }

        if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
            fprintf(stderr, "MDD Unique table full, %zu of %zu buckets filled!\n", llmsset_get_filled(nodes), llmsset_get_size(nodes));
            exit(1);
        }
    }

    return (MDD)index;
}

MDD
lddmc_make_copynode(MDD ifeq, MDD ifneq)
{
    LOCALIZE_THREAD_LOCAL(insert_index, uint64_t*);
    if (insert_index == NULL) insert_index = initialize_insert_index();

    struct mddnode n;
    mddnode_makecopy(&n, ifneq, ifeq);

    uint64_t index;
    int created;
    if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
        if (gc_enabled) {
            //size_t before_gc = llmsset_get_filled(nodes);
            REFS_INIT;
            REFS_PUSH(ifeq);
            REFS_PUSH(ifneq);
            lddmc_gc_go(1);
            REFS_RESET;
            //size_t after_gc = llmsset_get_filled(nodes);
            //fprintf(stderr, "GC: %.01f%% to %.01f%%\n", 100.0*(double)before_gc/total, 100.0*(double)after_gc/total);
        }

        if (llmsset_lookup(nodes, &n, insert_index, &created, &index) == 0) {
            fprintf(stderr, "MDD Unique table full, %zu of %zu buckets filled!\n", llmsset_get_filled(nodes), llmsset_get_size(nodes));
            exit(1);
        }
    }

    return (MDD)index;
}

MDD
lddmc_extendnode(MDD mdd, uint32_t value, MDD ifeq)
{
    if (mdd <= lddmc_true) return lddmc_makenode(value, ifeq, mdd);

    mddnode_t n = GETNODE(mdd);
    if (mddnode_getcopy(n)) return lddmc_make_copynode(mddnode_getdown(n), lddmc_extendnode(mddnode_getright(n), value, ifeq));
    uint32_t n_value = mddnode_getvalue(n);
    if (n_value < value) return lddmc_makenode(n_value, mddnode_getdown(n), lddmc_extendnode(mddnode_getright(n), value, ifeq));
    if (n_value == value) return lddmc_makenode(value, ifeq, mddnode_getright(n));
    /* (n_value > value) */ return lddmc_makenode(value, ifeq, mdd);
}

uint32_t
lddmc_value(MDD mdd)
{
    return mddnode_getvalue(GETNODE(mdd));
}

MDD
lddmc_follow(MDD mdd, uint32_t value)
{
    for (;;) {
        if (mdd <= lddmc_true) return mdd;
        const mddnode_t n = GETNODE(mdd);
        if (!mddnode_getcopy(n)) {
            const uint32_t v = mddnode_getvalue(n);
            if (v == value) return mddnode_getdown(n);
            if (v > value) return lddmc_false;
        }
        mdd = mddnode_getright(n);
    }
}

int
lddmc_iscopy(MDD mdd)
{
    if (mdd <= lddmc_true) return 0;

    mddnode_t n = GETNODE(mdd);
    return mddnode_getcopy(n) ? 1 : 0;
}

MDD
lddmc_followcopy(MDD mdd)
{
    if (mdd <= lddmc_true) return lddmc_false;

    mddnode_t n = GETNODE(mdd);
    if (mddnode_getcopy(n)) return mddnode_getdown(n);
    else return lddmc_false;
}

/**
 * Initialize and Quit functions
 */

/*static char* to_h(double size, char *buf)
{
    const char* units[] = {"B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"};
    int i = 0;
    for (;size>1024;size/=1024) i++;
    sprintf(buf, "%.*f %s", i, size, units[i]);
    return buf;
}*/

void
lddmc_init(size_t tablesize, size_t cachesize)
{
    lace_set_callback(TASK(lddmc_lace_test_gc));
    workers = lace_workers();

    INIT_THREAD_LOCAL(ref_key);
    INIT_THREAD_LOCAL(insert_index);

#if USE_NUMA
    if (numa_available() != -1) {
        numa_set_interleave_mask(numa_all_nodes_ptr);
    }
#endif

    gc = 0;
    barrier_init(&gcbar, lace_workers());

    // Sanity check
    if (sizeof(struct mddnode) != 16) {
        fprintf(stderr, "Invalid size of mdd nodes: %ld\n", sizeof(struct mddnode));
        exit(1);
    }

    if (tablesize > 42) {
        fprintf(stderr, "lddmc_init error: tablesize must be <= 47!\n");
        exit(1);
    }

    if (cachesize > 40) {
        fprintf(stderr, "lddmc_init error: cachesize must be <= 40!\n");
        exit(1);
    }

    nodes = llmsset_create(1LL<<tablesize);
    cache_create(1LL<<cachesize);
    refs_create(1024);

    /*
    size_t nodes_mem = (1LL<<tablesize) * (sizeof(struct mddnode) + 8); // 8 bytes overhead per node
    size_t cache_mem = (1LL<<cachesize) * sizeof(struct cache_entry);

    char buf1[32], buf2[32];
    fprintf(stderr, "LDDmc initialized, nodes table: %s, operations cache: %s\n", to_h(nodes_mem, buf1), to_h(cache_mem, buf2));
    */
}

void
lddmc_quit()
{
    // TODO: remove lace callback

    cache_free();
    llmsset_free(nodes);
    refs_free();
    barrier_destroy(&gcbar);
}

/**
 * MDD operations
 */
#define CACHE_RELPROD   0
#define CACHE_MINUS     1
#define CACHE_UNION     2
#define CACHE_INTERSECT 3
#define CACHE_SATCOUNT  4
#define CACHE_PROJECT   5
#define CACHE_JOIN      6
#define CACHE_MATCH     7
#define CACHE_RELPREV   8

static inline int
match_ldds(MDD *one, MDD *two)
{
    MDD m1 = *one, m2 = *two;
    if (m1 == lddmc_false || m2 == lddmc_false) return 0;
    mddnode_t n1 = GETNODE(m1), n2 = GETNODE(m2);
    uint32_t v1 = mddnode_getvalue(n1), v2 = mddnode_getvalue(n2);
    while (v1 != v2) {
        if (v1 < v2) {
            m1 = mddnode_getright(n1);
            if (m1 == lddmc_false) return 0;
            n1 = GETNODE(m1);
            v1 = mddnode_getvalue(n1);
        } else if (v1 > v2) {
            m2 = mddnode_getright(n2);
            if (m2 == lddmc_false) return 0;
            n2 = GETNODE(m2);
            v2 = mddnode_getvalue(n2);
        }
    }
    *one = m1;
    *two = m2;
    return 1;
}

TASK_IMPL_2(MDD, lddmc_union, MDD, a, MDD, b)
{
    /* Terminal cases */
    if (a == b) return a;
    if (a == lddmc_false) return b;
    if (b == lddmc_false) return a;
    assert(a != lddmc_true && b != lddmc_true); // expecting same length

    /* Test gc */
    lddmc_gc_test();

    /* Improve cache behavior */
    if (a < b) { MDD tmp=b; b=a; a=tmp; }

    /* Access cache */
    MDD result;
    if (cache_get(MDD_SETDATA(a, CACHE_UNION), b, 0, &result)) return result;

    /* Get nodes */
    mddnode_t na = GETNODE(a);
    mddnode_t nb = GETNODE(b);

    const int na_copy = mddnode_getcopy(na) ? 1 : 0;
    const int nb_copy = mddnode_getcopy(nb) ? 1 : 0;
    const uint32_t na_value = mddnode_getvalue(na);
    const uint32_t nb_value = mddnode_getvalue(nb);

    /* Perform recursive calculation */
    if (na_copy && nb_copy) {
        REFS_INIT;
        SSPAWN(lddmc_union, mddnode_getdown(na), mddnode_getdown(nb));
        MDD right = CALL(lddmc_union, mddnode_getright(na), mddnode_getright(nb));
        REFS_PUSH(right);
        MDD down = SSYNC(lddmc_union);
        REFS_RESET;
        result = lddmc_make_copynode(down, right);
    } else if (na_copy) {
        MDD right = CALL(lddmc_union, mddnode_getright(na), b);
        result = lddmc_make_copynode(mddnode_getdown(na), right);
    } else if (nb_copy) {
        MDD right = CALL(lddmc_union, a, mddnode_getright(nb));
        result = lddmc_make_copynode(mddnode_getdown(nb), right);
    } else if (na_value < nb_value) {
        MDD right = CALL(lddmc_union, mddnode_getright(na), b);
        result = lddmc_makenode(na_value, mddnode_getdown(na), right);
    } else if (na_value == nb_value) {
        REFS_INIT;
        SSPAWN(lddmc_union, mddnode_getdown(na), mddnode_getdown(nb));
        MDD right = CALL(lddmc_union, mddnode_getright(na), mddnode_getright(nb));
        REFS_PUSH(right);
        MDD down = SSYNC(lddmc_union);
        REFS_RESET;
        result = lddmc_makenode(na_value, down, right);
    } else /* na_value > nb_value */ {
        MDD right = CALL(lddmc_union, a, mddnode_getright(nb));
        result = lddmc_makenode(nb_value, mddnode_getdown(nb), right);
    }

    /* Write to cache */
    cache_put(MDD_SETDATA(a, CACHE_UNION), b, 0, result);

    return result;
}

TASK_IMPL_2(MDD, lddmc_minus, MDD, a, MDD, b)
{
    /* Terminal cases */
    if (a == b) return lddmc_false;
    if (a == lddmc_false) return lddmc_false;
    if (b == lddmc_false) return a;
    assert(b != lddmc_true);
    assert(a != lddmc_true); // Universe is unknown!! // Possibly depth issue?

    /* Test gc */
    lddmc_gc_test();

    /* Access cache */
    MDD result;
    if (cache_get(MDD_SETDATA(a, CACHE_MINUS), b, 0, &result)) return result;

    /* Get nodes */
    mddnode_t na = GETNODE(a);
    mddnode_t nb = GETNODE(b);
    uint32_t na_value = mddnode_getvalue(na);
    uint32_t nb_value = mddnode_getvalue(nb);

    /* Perform recursive calculation */
    if (na_value < nb_value) {
        MDD right = CALL(lddmc_minus, mddnode_getright(na), b);
        result = lddmc_makenode(na_value, mddnode_getdown(na), right);
    } else if (na_value == nb_value) {
        REFS_INIT;
        SSPAWN(lddmc_minus, mddnode_getright(na), mddnode_getright(nb));
        MDD down = CALL(lddmc_minus, mddnode_getdown(na), mddnode_getdown(nb));
        REFS_PUSH(down);
        MDD right = SSYNC(lddmc_minus);
        REFS_RESET;
        result = lddmc_makenode(na_value, down, right);
    } else /* na_value > nb_value */ {
        result = CALL(lddmc_minus, a, mddnode_getright(nb));
    }

    /* Write to cache */
    cache_put(MDD_SETDATA(a, CACHE_MINUS), b, 0, result);

    return result;
}

/* result: a plus b; res2: b minus a */
TASK_IMPL_3(MDD, lddmc_zip, MDD, a, MDD, b, MDD*, res2)
{
    /* Terminal cases */
    if (a == b) {
        *res2 = lddmc_false;
        return a;
    }
    if (a == lddmc_false) {
        *res2 = b;
        return b;
    }
    if (b == lddmc_false) {
        *res2 = lddmc_false;
        return a;
    }

    assert(a != lddmc_true && b != lddmc_true); // expecting same length

    /* Test gc */
    lddmc_gc_test();

    /* Access cache */
    MDD result;
    if (cache_get(MDD_SETDATA(a, CACHE_UNION), b, 0, &result) &&
        cache_get(MDD_SETDATA(b, CACHE_MINUS), a, 0, res2)) return result;

    /* Get nodes */
    mddnode_t na = GETNODE(a);
    mddnode_t nb = GETNODE(b);
    uint32_t na_value = mddnode_getvalue(na);
    uint32_t nb_value = mddnode_getvalue(nb);

    /* Perform recursive calculation */
    if (na_value < nb_value) {
        MDD right = CALL(lddmc_zip, mddnode_getright(na), b, res2);
        result = lddmc_makenode(na_value, mddnode_getdown(na), right);
    } else if (na_value == nb_value) {
        MDD down2, right2;
        REFS_INIT;
        SSPAWN(lddmc_zip, mddnode_getdown(na), mddnode_getdown(nb), &down2);
        MDD right = CALL(lddmc_zip, mddnode_getright(na), mddnode_getright(nb), &right2);
        REFS_PUSH(right);
        REFS_PUSH(right2);
        MDD down = SSYNC(lddmc_zip);
        REFS_RESET;
        result = lddmc_makenode(na_value, down, right);
        *res2 = lddmc_makenode(na_value, down2, right2);
    } else /* na_value > nb_value */ {
        MDD right2;
        MDD right = CALL(lddmc_zip, a, mddnode_getright(nb), &right2);
        result = lddmc_makenode(nb_value, mddnode_getdown(nb), right);
        *res2 = lddmc_makenode(nb_value, mddnode_getdown(nb), right2);
    }

    /* Write to cache */
    cache_put(MDD_SETDATA(a, CACHE_UNION), b, 0, result);
    cache_put(MDD_SETDATA(b, CACHE_MINUS), a, 0, *res2);

    return result;
}

TASK_IMPL_2(MDD, lddmc_intersect, MDD, a, MDD, b)
{
    /* Terminal cases */
    if (a == b) return a;
    if (a == lddmc_false || b == lddmc_false) return lddmc_false;
    assert(a != lddmc_true && b != lddmc_true);

    /* Test gc */
    lddmc_gc_test();

    /* Get nodes */
    mddnode_t na = GETNODE(a);
    mddnode_t nb = GETNODE(b);
    uint32_t na_value = mddnode_getvalue(na);
    uint32_t nb_value = mddnode_getvalue(nb);

    /* Skip nodes if possible */
    while (na_value != nb_value) {
        if (na_value < nb_value) {
            a = mddnode_getright(na);
            if (a == lddmc_false) return lddmc_false;
            na = GETNODE(a);
            na_value = mddnode_getvalue(na);
        }
        if (nb_value < na_value) {
            b = mddnode_getright(nb);
            if (b == lddmc_false) return lddmc_false;
            nb = GETNODE(b);
            nb_value = mddnode_getvalue(nb);
        }
    }

    /* Access cache */
    MDD result;
    if (cache_get(MDD_SETDATA(a, CACHE_INTERSECT), b, 0, &result)) return result;

    /* Perform recursive calculation */
    REFS_INIT;
    SSPAWN(lddmc_intersect, mddnode_getright(na), mddnode_getright(nb));
    MDD down = CALL(lddmc_intersect, mddnode_getdown(na), mddnode_getdown(nb));
    REFS_PUSH(down);
    MDD right = SSYNC(lddmc_intersect);
    REFS_RESET;
    result = lddmc_makenode(na_value, down, right);

    /* Write to cache */
    cache_put(MDD_SETDATA(a, CACHE_INTERSECT), b, 0, result);

    return result;
}

// proj: -1 (rest 0), 0 (no match), 1 (match)
TASK_IMPL_3(MDD, lddmc_match, MDD, a, MDD, b, MDD, proj)
{
    if (a == b) return a;
    if (a == lddmc_false || b == lddmc_false) return lddmc_false;

    mddnode_t p_node = GETNODE(proj);
    uint32_t p_val = mddnode_getvalue(p_node);
    if (p_val == (uint32_t)-1) return a;

    assert(a != lddmc_true);
    if (p_val == 1) assert(b != lddmc_true);

    /* Test gc */
    lddmc_gc_test();

    /* Skip nodes if possible */
    if (p_val == 1) {
        if (!match_ldds(&a, &b)) return lddmc_false;
    }

    /* Access cache */
    MDD result;
    if (cache_get(MDD_SETDATA(a, CACHE_MATCH), b, proj, &result)) return result;

    /* Perform recursive calculation */
    REFS_INIT;
    mddnode_t na = GETNODE(a);
    MDD down;
    if (p_val == 1) {
        mddnode_t nb = GETNODE(b);
        /* right = */ SSPAWN(lddmc_match, mddnode_getright(na), mddnode_getright(nb), proj);
        down = CALL(lddmc_match, mddnode_getdown(na), mddnode_getdown(nb), mddnode_getdown(p_node));
    } else {
        /* right = */ SSPAWN(lddmc_match, mddnode_getright(na), b, proj);
        down = CALL(lddmc_match, mddnode_getdown(na), b, mddnode_getdown(p_node));
    }
    REFS_PUSH(down);
    MDD right = SSYNC(lddmc_match);
    REFS_RESET;
    result = lddmc_makenode(mddnode_getvalue(na), down, right);

    /* Write to cache */
    cache_put(MDD_SETDATA(a, CACHE_MATCH), b, proj, result);

    return result;
}

TASK_4(MDD, lddmc_relprod_help, uint32_t, val, MDD, set, MDD, rel, MDD, proj)
{
    return lddmc_makenode(val, CALL(lddmc_relprod, set, rel, proj), lddmc_false);
}

// meta: -1 (end; rest not in rel), 0 (not in rel), 1 (read), 2 (write), 3 (only-read), 4 (only-write)
TASK_IMPL_3(MDD, lddmc_relprod, MDD, set, MDD, rel, MDD, meta)
{
    if (set == lddmc_false) return lddmc_false;
    if (rel == lddmc_false) return lddmc_false;

    mddnode_t n_meta = GETNODE(meta);
    uint32_t m_val = mddnode_getvalue(n_meta);
    if (m_val == (uint32_t)-1) return set;
    if (m_val != 0) assert(set != lddmc_true && rel != lddmc_true);

    /* Skip nodes if possible */
    if (!mddnode_getcopy(GETNODE(rel))) {
        if (m_val == 1 || m_val == 3) {
            if (!match_ldds(&set, &rel)) return lddmc_false;
        }
    }

    /* Access cache */
    MDD result;
    if (cache_get(MDD_SETDATA(set, CACHE_RELPROD), rel, meta, &result)) return result;

    mddnode_t n_set = GETNODE(set);
    mddnode_t n_rel = GETNODE(rel);

    /* Recursive operations */
    REFS_INIT;
    if (m_val == 0) { // not in rel
        SSPAWN(lddmc_relprod, mddnode_getright(n_set), rel, meta);
        MDD down = CALL(lddmc_relprod, mddnode_getdown(n_set), rel, mddnode_getdown(n_meta));
        REFS_PUSH(down);
        MDD right = SSYNC(lddmc_relprod);
        REFS_RESET;
        result = lddmc_makenode(mddnode_getvalue(n_set), down, right);
    } else if (m_val == 1) { // read
        // read layer: if not copy, then set&rel are already matched
        SSPAWN(lddmc_relprod, set, mddnode_getright(n_rel), meta); // spawn next read in list

        // for this read, either it is copy ('for all') or it is normal match
        if (mddnode_getcopy(n_rel)) {
            // spawn for every value to copy (set)
            int count = 0;
            for (;;) {
                // stay same level of set (for write)
                SSPAWN(lddmc_relprod, set, mddnode_getdown(n_rel), mddnode_getdown(n_meta));
                count++;
                set = mddnode_getright(n_set);
                if (set == lddmc_false) break;
                n_set = GETNODE(set);
            }

            // sync+union (one by one)
            result = lddmc_false;
            while (count--) {
                REFS_PUSH(result);
                MDD result2 = SSYNC(lddmc_relprod);
                REFS_PUSH(result2);
                result = CALL(lddmc_union, result, result2);
                REFS_RESET;
            }
        } else {
            // stay same level of set (for write)
            result = CALL(lddmc_relprod, set, mddnode_getdown(n_rel), mddnode_getdown(n_meta));
        }

        REFS_PUSH(result);
        MDD result2 = SSYNC(lddmc_relprod); // sync next read in list
        REFS_PUSH(result2);
        result = CALL(lddmc_union, result, result2);
        REFS_RESET;
    } else if (m_val == 3) { // only-read
        if (mddnode_getcopy(n_rel)) {
            // copy on read ('for any value')
            // result = union(result_with_copy, result_without_copy)
            SSPAWN(lddmc_relprod, set, mddnode_getright(n_rel), meta); // spawn without_copy

            // spawn for every value to copy (set)
            int count = 0;
            for (;;) {
                SSPAWN(lddmc_relprod_help, mddnode_getvalue(n_set), mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta));
                count++;
                set = mddnode_getright(n_set);
                if (set == lddmc_false) break;
                n_set = GETNODE(set);
            }

            // sync+union (one by one)
            result = lddmc_false;
            while (count--) {
                REFS_PUSH(result);
                MDD result2 = SSYNC(lddmc_relprod_help);
                REFS_PUSH(result2);
                result = CALL(lddmc_union, result, result2);
                REFS_RESET;
            }

            // add result from without_copy
            REFS_PUSH(result);
            MDD result2 = SSYNC(lddmc_relprod);
            REFS_PUSH(result2);
            result = CALL(lddmc_union, result, result2);
            REFS_RESET;
        } else {
            // only-read, without copy
            SSPAWN(lddmc_relprod, mddnode_getright(n_set), mddnode_getright(n_rel), meta);
            MDD down = CALL(lddmc_relprod, mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta));
            REFS_PUSH(down);
            MDD right = SSYNC(lddmc_relprod);
            REFS_RESET;
            result = lddmc_makenode(mddnode_getvalue(n_set), down, right);
        }
    } else if (m_val == 2 || m_val == 4) { // write, only-write
        if (m_val == 4) {
            // only-write, so we need to include 'for all variables'
            SSPAWN(lddmc_relprod, mddnode_getright(n_set), rel, meta); // next in set
        }

        // spawn for every value to write (rel)
        int count = 0;
        for (;;) {
            uint32_t value;
            if (mddnode_getcopy(n_rel)) value = mddnode_getvalue(n_set);
            else value = mddnode_getvalue(n_rel);
            SSPAWN(lddmc_relprod_help, value, mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta));
            count++;
            rel = mddnode_getright(n_rel);
            if (rel == lddmc_false) break;
            n_rel = GETNODE(rel);
        }

        // sync+union (one by one)
        result = lddmc_false;
        while (count--) {
            REFS_PUSH(result);
            MDD result2 = SSYNC(lddmc_relprod_help);
            REFS_PUSH(result2);
            result = CALL(lddmc_union, result, result2);
            REFS_RESET;
        }

        if (m_val == 4) {
            // sync+union with other variables
            REFS_PUSH(result);
            MDD result2 = SSYNC(lddmc_relprod);
            REFS_PUSH(result2);
            result = CALL(lddmc_union, result, result2);
            REFS_RESET;
        }
    } 

    /* Write to cache */
    cache_put(MDD_SETDATA(set, CACHE_RELPROD), rel, meta, result);

    return result;
}

TASK_5(MDD, lddmc_relprev_help, uint32_t, val, MDD, set, MDD, rel, MDD, proj, MDD, uni)
{
    return lddmc_makenode(val, CALL(lddmc_relprev, set, rel, proj, uni), lddmc_false);
}

/**
 * Calculate all predecessors to a in uni according to rel[meta]
 * <meta> follows the same semantics as relprod
 * i.e. 0 (not in rel), 1 (read), 2 (write), 3 (only-read), 4 (only-write), -1 (end; rest=0)
 */
TASK_IMPL_4(MDD, lddmc_relprev, MDD, set, MDD, rel, MDD, meta, MDD, uni)
{
    if (set == lddmc_false) return lddmc_false;
    if (rel == lddmc_false) return lddmc_false;
    if (uni == lddmc_false) return lddmc_false;

    mddnode_t n_meta = GETNODE(meta);
    uint32_t m_val = mddnode_getvalue(n_meta);
    if (m_val == (uint32_t)-1) {
        if (set == uni) return set;
        else return lddmc_intersect(set, uni);
    }

    if (m_val != 0) assert(set != lddmc_true && rel != lddmc_true && uni != lddmc_true);

    /* Skip nodes if possible */
    if (m_val == 0) {
        // not in rel: match set and uni ('intersect')
        if (!match_ldds(&set, &uni)) return lddmc_false;
    } else if (mddnode_getcopy(GETNODE(rel))) {
        // read+copy: no matching (pre is everything in uni)
        // write+copy: no matching (match after split: set and uni)
        // only-read+copy: match set and uni
        // only-write+copy: no matching (match after split: set and uni)
        if (m_val == 3) {
            if (!match_ldds(&set, &uni)) return lddmc_false;
        }
    } else if (m_val == 1) {
        // read: match uni and rel
        if (!match_ldds(&uni, &rel)) return lddmc_false;
    } else if (m_val == 2) {
        // write: match set and rel
        if (!match_ldds(&set, &rel)) return lddmc_false;
    } else if (m_val == 3) {
        // only-read: match uni and set and rel
        mddnode_t n_set = GETNODE(set);
        mddnode_t n_rel = GETNODE(rel);
        mddnode_t n_uni = GETNODE(uni);
        uint32_t n_set_value = mddnode_getvalue(n_set);
        uint32_t n_rel_value = mddnode_getvalue(n_rel);
        uint32_t n_uni_value = mddnode_getvalue(n_uni);
        while (n_uni_value != n_rel_value || n_rel_value != n_set_value) {
            if (n_uni_value < n_rel_value || n_uni_value < n_set_value) {
                uni = mddnode_getright(n_uni);
                if (uni == lddmc_false) return lddmc_false;
                n_uni = GETNODE(uni);
                n_uni_value = mddnode_getvalue(n_uni);
            }
            if (n_set_value < n_rel_value || n_set_value < n_uni_value) {
                set = mddnode_getright(n_set);
                if (set == lddmc_false) return lddmc_false;
                n_set = GETNODE(set);
                n_set_value = mddnode_getvalue(n_set);
            }
            if (n_rel_value < n_set_value || n_rel_value < n_uni_value) {
                rel = mddnode_getright(n_rel);
                if (rel == lddmc_false) return lddmc_false;
                n_rel = GETNODE(rel);
                n_rel_value = mddnode_getvalue(n_rel);
            }
        }
    } else if (m_val == 4) {
        // only-write: match set and rel (then use whole universe)
        if (!match_ldds(&set, &rel)) return lddmc_false;
    }

    /* Access cache */
    MDD result;
    const MDD c_a = MDD_SETDATA(set, CACHE_RELPREV);
    const MDD c_b = MDD_SETDATA(rel, (uint32_t)uni); // store lower 22 bits in c_b
    const MDD c_c = MDD_SETDATA(meta, (uint32_t)(uni>>22)); // store higher 20 bits in c_c
    if (cache_get(c_a, c_b, c_c, &result)) return result;

    mddnode_t n_set = GETNODE(set);
    mddnode_t n_rel = GETNODE(rel);
    mddnode_t n_uni = GETNODE(uni);

    REFS_INIT;
    /* Recursive operations */
    if (m_val == 0) { // not in rel
        // m_val == 0 : not in rel (intersection set and universe)
        SSPAWN(lddmc_relprev, mddnode_getright(n_set), rel, meta, mddnode_getright(n_uni));
        MDD down = CALL(lddmc_relprev, mddnode_getdown(n_set), rel, mddnode_getdown(n_meta), mddnode_getdown(n_uni));
        REFS_PUSH(down);
        MDD right = SSYNC(lddmc_relprev);
        REFS_RESET;
        result = lddmc_makenode(mddnode_getvalue(n_set), down, right);
    } else if (m_val == 1) { // read level
        // result value is in case of copy: everything in uni!
        // result value is in case of not-copy: match uni and rel!
        SSPAWN(lddmc_relprev, set, mddnode_getright(n_rel), meta, uni); // next in rel
        if (mddnode_getcopy(n_rel)) {
            // result is everything in uni
            // spawn for every value to have been read (uni)
            int count = 0;
            for (;;) {
                SSPAWN(lddmc_relprev_help, mddnode_getvalue(n_uni), set, mddnode_getdown(n_rel), mddnode_getdown(n_meta), uni);
                count++;
                uni = mddnode_getright(n_uni);
                if (uni == lddmc_false) break;
                n_uni = GETNODE(uni);
            }

            // sync+union (one by one)
            result = lddmc_false;
            while (count--) {
                REFS_PUSH(result);
                MDD result2 = SSYNC(lddmc_relprev_help);
                REFS_PUSH(result2);
                result = CALL(lddmc_union, result, result2);
                REFS_RESET;
            }
        } else {
            // already matched
            MDD down = CALL(lddmc_relprev, set, mddnode_getdown(n_rel), mddnode_getdown(n_meta), uni);
            result = lddmc_makenode(mddnode_getvalue(n_uni), down, lddmc_false);
        }
        REFS_PUSH(result);
        MDD result2 = SSYNC(lddmc_relprev);
        REFS_PUSH(result2);
        result = CALL(lddmc_union, result, result2);
        REFS_RESET;
    } else if (m_val == 3) { // only-read level
        // result value is in case of copy: match set and uni! (already done first match)
        // result value is in case of not-copy: match set and uni and rel!
        SSPAWN(lddmc_relprev, set, mddnode_getright(n_rel), meta, uni); // next in rel
        if (mddnode_getcopy(n_rel)) {
            // spawn for every matching set+uni
            int count = 0;
            for (;;) {
                SSPAWN(lddmc_relprev_help, mddnode_getvalue(n_uni), mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta), mddnode_getdown(n_uni));
                count++;
                uni = mddnode_getright(n_uni);
                if (!match_ldds(&set, &uni)) break;
                n_set = GETNODE(set);
                n_uni = GETNODE(uni);
            }

            // sync+union (one by one)
            result = lddmc_false;
            while (count--) {
                REFS_PUSH(result);
                MDD result2 = SSYNC(lddmc_relprev_help);
                REFS_PUSH(result2);
                result = CALL(lddmc_union, result, result2);
                REFS_RESET;
            }
        } else {
            // already matched
            MDD down = CALL(lddmc_relprev, mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta), mddnode_getdown(n_uni));
            result = lddmc_makenode(mddnode_getvalue(n_uni), down, lddmc_false);
        }
        REFS_PUSH(result);
        MDD result2 = SSYNC(lddmc_relprev);
        REFS_PUSH(result2);
        result = CALL(lddmc_union, result, result2);
        REFS_RESET;
    } else if (m_val == 2) { // write level
        // note: the read level has already matched the uni that was read.
        // write+copy: only for the one set equal to uni...
        // write: match set and rel (already done)
        SSPAWN(lddmc_relprev, set, mddnode_getright(n_rel), meta, uni);
        if (mddnode_getcopy(n_rel)) {
            MDD down = lddmc_follow(set, mddnode_getvalue(n_uni));
            if (down != lddmc_false) {
                result = CALL(lddmc_relprev, down, mddnode_getdown(n_rel), mddnode_getdown(n_meta), mddnode_getdown(n_uni));
            } else {
                result = lddmc_false;
            }
        } else {
            result = CALL(lddmc_relprev, mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta), mddnode_getdown(n_uni));
        }
        REFS_PUSH(result);
        MDD result2 = SSYNC(lddmc_relprev);
        REFS_PUSH(result2);
        result = CALL(lddmc_union, result, result2);
        REFS_RESET;
    } else if (m_val == 4) { // only-write level
        // only-write+copy: match set and uni after spawn
        // only-write: match set and rel (already done)
        SSPAWN(lddmc_relprev, set, mddnode_getright(n_rel), meta, uni);
        if (mddnode_getcopy(n_rel)) {
            // spawn for every matching set+uni
            int count = 0;
            for (;;) {
                if (!match_ldds(&set, &uni)) break;
                n_set = GETNODE(set);
                n_uni = GETNODE(uni);
                SSPAWN(lddmc_relprev_help, mddnode_getvalue(n_uni), mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta), mddnode_getdown(n_uni));
                count++;
                uni = mddnode_getright(n_uni);
            }

            // sync+union (one by one)
            result = lddmc_false;
            while (count--) {
                REFS_PUSH(result);
                MDD result2 = SSYNC(lddmc_relprev_help);
                REFS_PUSH(result2);
                result = CALL(lddmc_union, result, result2);
                REFS_RESET;
            }
        } else {
            // spawn for every value in universe!!
            int count = 0;
            for (;;) {
                SSPAWN(lddmc_relprev_help, mddnode_getvalue(n_uni), mddnode_getdown(n_set), mddnode_getdown(n_rel), mddnode_getdown(n_meta), mddnode_getdown(n_uni));
                count++;
                uni = mddnode_getright(n_uni);
                if (uni == lddmc_false) break;
                n_uni = GETNODE(uni);
            }

            // sync+union (one by one)
            result = lddmc_false;
            while (count--) {
                REFS_PUSH(result);
                MDD result2 = SSYNC(lddmc_relprev_help);
                REFS_PUSH(result2);
                result = CALL(lddmc_union, result, result2);
                REFS_RESET;
            }
        }
        REFS_PUSH(result);
        MDD result2 = SSYNC(lddmc_relprev);
        REFS_PUSH(result2);
        result = CALL(lddmc_union, result, result2);
        REFS_RESET;
    }

    /* Write to cache */
    cache_put(c_a, c_b, c_c, result);

    return result;
}

// Same 'proj' as project. So: proj: -2 (end; quantify rest), -1 (end; keep rest), 0 (quantify), 1 (keep)
TASK_IMPL_4(MDD, lddmc_join, MDD, a, MDD, b, MDD, a_proj, MDD, b_proj)
{
    if (a == lddmc_false || b == lddmc_false) return lddmc_false;

    /* Test gc */
    lddmc_gc_test();

    mddnode_t n_a_proj = GETNODE(a_proj);
    mddnode_t n_b_proj = GETNODE(b_proj);
    uint32_t a_proj_val = mddnode_getvalue(n_a_proj);
    uint32_t b_proj_val = mddnode_getvalue(n_b_proj);

    while (a_proj_val == 0 && b_proj_val == 0) {
        a_proj = mddnode_getdown(n_a_proj);
        b_proj = mddnode_getdown(n_b_proj);
        n_a_proj = GETNODE(a_proj);
        n_b_proj = GETNODE(b_proj);
        a_proj_val = mddnode_getvalue(n_a_proj);
        b_proj_val = mddnode_getvalue(n_b_proj);
    }

    if (a_proj_val == (uint32_t)-2) return b; // no a left
    if (b_proj_val == (uint32_t)-2) return a; // no b left
    if (a_proj_val == (uint32_t)-1 && b_proj_val == (uint32_t)-1) return CALL(lddmc_intersect, a, b);
    
    // At this point, only proj_val {-1, 0, 1}; max one with -1; max one with 0.
    const int keep_a = a_proj_val != 0;
    const int keep_b = b_proj_val != 0;

    if (keep_a && keep_b) {
        // If both 'keep', then match values
        if (!match_ldds(&a, &b)) return lddmc_false;
    }

    /* Access cache */
    MDD result;
    const MDD c_a = MDD_SETDATA(a, CACHE_JOIN);
    const MDD c_b = MDD_SETDATA(b, (uint32_t)a_proj); // store lower 22 bits in c_b
    const MDD c_c = MDD_SETDATA(b_proj, (uint32_t)(a_proj>>22)); // store higher 20 bits in c_c
    if (cache_get(c_a, c_b, c_c, &result)) return result;

    /* Perform recursive calculation */
    const mddnode_t na = GETNODE(a);
    const mddnode_t nb = GETNODE(b);
    uint32_t val;
    MDD down;

    REFS_INIT;
    if (keep_a) {
        if (keep_b) {
            val = mddnode_getvalue(nb);
            SSPAWN(lddmc_join, mddnode_getright(na), mddnode_getright(nb), a_proj, b_proj);
            if (a_proj_val != (uint32_t)-1) a_proj = mddnode_getdown(n_a_proj);
            if (b_proj_val != (uint32_t)-1) b_proj = mddnode_getdown(n_b_proj);
            down = CALL(lddmc_join, mddnode_getdown(na), mddnode_getdown(nb), a_proj, b_proj);
        } else {
            val = mddnode_getvalue(na);
            SSPAWN(lddmc_join, mddnode_getright(na), b, a_proj, b_proj);
            if (a_proj_val != (uint32_t)-1) a_proj = mddnode_getdown(n_a_proj);
            if (b_proj_val != (uint32_t)-1) b_proj = mddnode_getdown(n_b_proj);
            down = CALL(lddmc_join, mddnode_getdown(na), b, a_proj, b_proj);
        }
    } else {
        val = mddnode_getvalue(nb);
        SSPAWN(lddmc_join, a, mddnode_getright(nb), a_proj, b_proj);
        if (a_proj_val != (uint32_t)-1) a_proj = mddnode_getdown(n_a_proj);
        if (b_proj_val != (uint32_t)-1) b_proj = mddnode_getdown(n_b_proj);
        down = CALL(lddmc_join, a, mddnode_getdown(nb), a_proj, b_proj);
    }

    REFS_PUSH(down);
    MDD right = SSYNC(lddmc_join);
    REFS_RESET;
    result = lddmc_makenode(val, down, right);

    /* Write to cache */
    cache_put(c_a, c_b, c_c, result);

    return result;
}

// so: proj: -2 (end; quantify rest), -1 (end; keep rest), 0 (quantify), 1 (keep)
TASK_IMPL_2(MDD, lddmc_project, const MDD, mdd, const MDD, proj)
{
    if (mdd == lddmc_false) return lddmc_false; // projection of empty is empty
    if (mdd == lddmc_true) return lddmc_true; // projection of universe is universe...

    mddnode_t p_node = GETNODE(proj);
    uint32_t p_val = mddnode_getvalue(p_node);
    if (p_val == (uint32_t)-1) return mdd;
    if (p_val == (uint32_t)-2) return lddmc_true; // because we always end with true.

    lddmc_gc_test();

    MDD result;
    if (cache_get(MDD_SETDATA(mdd, CACHE_PROJECT), proj, 0, &result)) return result;

    mddnode_t n = GETNODE(mdd);

    REFS_INIT;
    if (p_val == 1) { // keep
        SSPAWN(lddmc_project, mddnode_getright(n), proj);
        MDD down = CALL(lddmc_project, mddnode_getdown(n), mddnode_getdown(p_node));
        REFS_PUSH(down);
        MDD right = SSYNC(lddmc_project);
        REFS_RESET;
        result = lddmc_makenode(mddnode_getvalue(n), down, right);
    } else { // quantify
        if (mddnode_getdown(n) == lddmc_true) { // assume lowest level
            result = lddmc_true;
        } else {
            int count = 0;
            MDD p_down = mddnode_getdown(p_node), _mdd=mdd;
            while (1) {
                SSPAWN(lddmc_project, mddnode_getdown(n), p_down);
                count++;
                _mdd = mddnode_getright(n);
                assert(_mdd != lddmc_true);
                if (_mdd == lddmc_false) break;
                n = GETNODE(_mdd);
            }
            result = lddmc_false;
            while (count) {
                REFS_PUSH(result);
                MDD down = SSYNC(lddmc_project);
                count--;
                REFS_PUSH(down);
                result = CALL(lddmc_union, result, down);
                REFS_RESET;
            }
        }
    }

    cache_put(MDD_SETDATA(mdd, CACHE_PROJECT), proj, 0, result);

    return result;
}

// so: proj: -2 (end; quantify rest), -1 (end; keep rest), 0 (quantify), 1 (keep)
TASK_IMPL_3(MDD, lddmc_project_minus, const MDD, mdd, const MDD, proj, MDD, avoid)
{
    // This implementation assumed "avoid" has correct depth
    if (avoid == lddmc_true) return lddmc_false;
    if (mdd == avoid) return lddmc_false;
    if (mdd == lddmc_false) return lddmc_false; // projection of empty is empty
    if (mdd == lddmc_true) return lddmc_true; // avoid != lddmc_true

    mddnode_t p_node = GETNODE(proj);
    uint32_t p_val = mddnode_getvalue(p_node);
    if (p_val == (uint32_t)-1) return lddmc_minus(mdd, avoid);
    if (p_val == (uint32_t)-2) return lddmc_true;

    lddmc_gc_test();

    MDD result;
    if (cache_get(MDD_SETDATA(mdd, CACHE_PROJECT), proj, avoid, &result)) return result;

    mddnode_t n = GETNODE(mdd);

    REFS_INIT;
    if (p_val == 1) { // keep
        // move 'avoid' until it matches
        uint32_t val = mddnode_getvalue(n);
        MDD a_down = lddmc_false;
        while (avoid != lddmc_false) {
            mddnode_t a_node = GETNODE(avoid);
            uint32_t a_val = mddnode_getvalue(a_node);
            if (a_val > val) {
                break;
            } else if (a_val == val) {
                a_down = mddnode_getdown(a_node);
                break;
            }
            avoid = mddnode_getright(a_node);
        }
        SSPAWN(lddmc_project_minus, mddnode_getright(n), proj, avoid);
        MDD down = CALL(lddmc_project_minus, mddnode_getdown(n), mddnode_getdown(p_node), a_down);
        REFS_PUSH(down);
        MDD right = SSYNC(lddmc_project_minus);
        REFS_RESET;
        result = lddmc_makenode(val, down, right);
    } else { // quantify
        if (mddnode_getdown(n) == lddmc_true) { // assume lowest level
            result = lddmc_true;
        } else {
            int count = 0;
            MDD p_down = mddnode_getdown(p_node), _mdd=mdd;
            while (1) {
                SSPAWN(lddmc_project_minus, mddnode_getdown(n), p_down, avoid);
                count++;
                _mdd = mddnode_getright(n);
                assert(_mdd != lddmc_true);
                if (_mdd == lddmc_false) break;
                n = GETNODE(_mdd);
            }
            result = lddmc_false;
            while (count) {
                REFS_PUSH(result);
                MDD down = SSYNC(lddmc_project_minus);
                count--;
                REFS_PUSH(down);
                result = CALL(lddmc_union, result, down);
                REFS_RESET;
            }
        }
    }

    cache_put(MDD_SETDATA(mdd, CACHE_PROJECT), proj, avoid, result);

    return result;
}

MDD
lddmc_union_cube(MDD a, uint32_t* values, size_t count)
{
    if (a == lddmc_false) return lddmc_cube(values, count);
    if (a == lddmc_true) {
        assert(count == 0);
        return lddmc_true;
    }
    assert(count != 0);

    mddnode_t na = GETNODE(a);
    uint32_t na_value = mddnode_getvalue(na);

    /* Only create a new node if something actually changed */

    if (na_value < *values) {
        MDD right = lddmc_union_cube(mddnode_getright(na), values, count);
        if (right == mddnode_getright(na)) return a; // no actual change
        return lddmc_makenode(na_value, mddnode_getdown(na), right);
    } else if (na_value == *values) {
        MDD down = lddmc_union_cube(mddnode_getdown(na), values+1, count-1);
        if (down == mddnode_getdown(na)) return a; // no actual change
        return lddmc_makenode(na_value, down, mddnode_getright(na));
    } else /* na_value > *values */ {
        return lddmc_makenode(*values, lddmc_cube(values+1, count-1), a);
    }
}

MDD
lddmc_union_cube_copy(MDD a, uint32_t* values, int* copy, size_t count)
{
    if (a == lddmc_false) return lddmc_cube_copy(values, copy, count);
    if (a == lddmc_true) {
        assert(count == 0);
        return lddmc_true;
    }
    assert(count != 0);

    mddnode_t na = GETNODE(a);

    /* Only create a new node if something actually changed */

    int na_copy = mddnode_getcopy(na);
    if (na_copy && *copy) {
        MDD down = lddmc_union_cube_copy(mddnode_getdown(na), values+1, copy+1, count-1);
        if (down == mddnode_getdown(na)) return a; // no actual change
        return lddmc_make_copynode(down, mddnode_getright(na));
    } else if (na_copy) {
        MDD right = lddmc_union_cube_copy(mddnode_getright(na), values, copy, count);
        if (right == mddnode_getright(na)) return a; // no actual change
        return lddmc_make_copynode(mddnode_getdown(na), right);
    } else if (*copy) {
        return lddmc_make_copynode(lddmc_cube_copy(values+1, copy+1, count-1), a);
    }

    uint32_t na_value = mddnode_getvalue(na);
    if (na_value < *values) {
        MDD right = lddmc_union_cube_copy(mddnode_getright(na), values, copy, count);
        if (right == mddnode_getright(na)) return a; // no actual change
        return lddmc_makenode(na_value, mddnode_getdown(na), right);
    } else if (na_value == *values) {
        MDD down = lddmc_union_cube_copy(mddnode_getdown(na), values+1, copy+1, count-1);
        if (down == mddnode_getdown(na)) return a; // no actual change
        return lddmc_makenode(na_value, down, mddnode_getright(na));
    } else /* na_value > *values */ {
        return lddmc_makenode(*values, lddmc_cube_copy(values+1, copy+1, count-1), a);
    }
}

int
lddmc_member_cube(MDD a, uint32_t* values, size_t count)
{
    while (1) {
        if (a == lddmc_false) return 0;
        if (a == lddmc_true) return 1;
        assert(count > 0); // size mismatch

        a = lddmc_follow(a, *values);
        values++;
        count--;
    }
}

int
lddmc_member_cube_copy(MDD a, uint32_t* values, int* copy, size_t count)
{
    while (1) {
        if (a == lddmc_false) return 0;
        if (a == lddmc_true) return 1;
        assert(count > 0); // size mismatch

        if (*copy) a = lddmc_followcopy(a);
        else a = lddmc_follow(a, *values);
        values++;
        count--;
    }
}

MDD
lddmc_cube(uint32_t* values, size_t count)
{
    if (count == 0) return lddmc_true;
    return lddmc_makenode(*values, lddmc_cube(values+1, count-1), lddmc_false);
}

MDD
lddmc_cube_copy(uint32_t* values, int* copy, size_t count)
{
    if (count == 0) return lddmc_true;
    if (*copy) return lddmc_make_copynode(lddmc_cube_copy(values+1, copy+1, count-1), lddmc_false);
    else return lddmc_makenode(*values, lddmc_cube_copy(values+1, copy+1, count-1), lddmc_false);
}

/**
 * Count number of nodes for each level
 */

static void
lddmc_nodecount_levels_mark(MDD mdd, size_t *variables)
{
    if (mdd <= lddmc_true) return;
    mddnode_t n = GETNODE(mdd);
    if (!mddnode_getmark(n)) {
        mddnode_setmark(n, 1);
        (*variables) += 1;
        lddmc_nodecount_levels_mark(mddnode_getright(n), variables);
        lddmc_nodecount_levels_mark(mddnode_getdown(n), variables+1);
    }
}

static void
lddmc_nodecount_levels_unmark(MDD mdd)
{
    if (mdd <= lddmc_true) return;
    mddnode_t n = GETNODE(mdd);
    if (mddnode_getmark(n)) {
        mddnode_setmark(n, 0);
        lddmc_nodecount_levels_unmark(mddnode_getright(n));
        lddmc_nodecount_levels_unmark(mddnode_getdown(n));
    }
}

void
lddmc_nodecount_levels(MDD mdd, size_t *variables)
{
    lddmc_nodecount_levels_mark(mdd, variables);
    lddmc_nodecount_levels_unmark(mdd);
}

/**
 * Count number of nodes in MDD
 */

static size_t
lddmc_nodecount_mark(MDD mdd)
{
    if (mdd <= lddmc_true) return 0;
    mddnode_t n = GETNODE(mdd);
    if (mddnode_getmark(n)) return 0;
    mddnode_setmark(n, 1);
    return 1 + lddmc_nodecount_mark(mddnode_getdown(n)) + lddmc_nodecount_mark(mddnode_getright(n));
}

static void
lddmc_nodecount_unmark(MDD mdd)
{
    if (mdd <= lddmc_true) return;
    mddnode_t n = GETNODE(mdd);
    if (mddnode_getmark(n)) {
        mddnode_setmark(n, 0);
        lddmc_nodecount_unmark(mddnode_getright(n));
        lddmc_nodecount_unmark(mddnode_getdown(n));
    }
}

size_t
lddmc_nodecount(MDD mdd)
{
    size_t result = lddmc_nodecount_mark(mdd);
    lddmc_nodecount_unmark(mdd);
    return result;
}

/**
 * CALCULATE NUMBER OF VAR ASSIGNMENTS THAT YIELD TRUE
 */

TASK_IMPL_1(lddmc_satcount_double_t, lddmc_satcount_cached, MDD, mdd)
{
    if (mdd == lddmc_false) return 0.0;
    if (mdd == lddmc_true) return 1.0;

    /* Perhaps execute garbage collection */
    lddmc_gc_test();

    union {
        lddmc_satcount_double_t d;
        uint64_t s;
    } hack;

    if (cache_get(MDD_SETDATA(mdd, CACHE_SATCOUNT), 0, 0, &hack.s)) return hack.d;
    mddnode_t n = GETNODE(mdd);

    SPAWN(lddmc_satcount_cached, mddnode_getdown(n));
    lddmc_satcount_double_t right = CALL(lddmc_satcount_cached, mddnode_getright(n));
    hack.d = right + SYNC(lddmc_satcount_cached);

    cache_put(MDD_SETDATA(mdd, CACHE_SATCOUNT), 0, 0, hack.s);

    return hack.d;
}

TASK_IMPL_1(long double, lddmc_satcount, MDD, mdd)
{
    if (mdd == lddmc_false) return 0.0;
    if (mdd == lddmc_true) return 1.0;

    /* Perhaps execute garbage collection */
    lddmc_gc_test();

    mddnode_t n = GETNODE(mdd);

    SPAWN(lddmc_satcount, mddnode_getdown(n));
    long double right = CALL(lddmc_satcount, mddnode_getright(n));
    return right + SYNC(lddmc_satcount);
}

TASK_IMPL_5(MDD, lddmc_collect, MDD, mdd, lddmc_sat_cb, cb, void*, context, uint32_t*, values, size_t, count)
{
    if (mdd == lddmc_false) return lddmc_false;
    if (mdd == lddmc_true) {
        return (MDD)WRAP(cb, values, count, context);
    }

    mddnode_t n = GETNODE(mdd);

    REFS_INIT;
    SSPAWN(lddmc_collect, mddnode_getright(n), cb, context, values, count);

    uint32_t newvalues[count+1];
    if (count > 0) memcpy(newvalues, values, sizeof(uint32_t)*count);
    newvalues[count] = mddnode_getvalue(n);
    MDD down = CALL(lddmc_collect, mddnode_getdown(n), cb, context, newvalues, count+1);

    if (down == lddmc_false) {
        MDD result = SSYNC(lddmc_collect);
        return result;
    }

    REFS_PUSH(down);
    MDD right = SSYNC(lddmc_collect);

    if (right == lddmc_false) {
        REFS_RESET;
        return down;
    }

    REFS_PUSH(right);
    MDD result = CALL(lddmc_union, down, right);

    REFS_RESET;
    return result;
}

VOID_TASK_5(_lddmc_sat_all_nopar, MDD, mdd, lddmc_sat_cb, cb, void*, context, uint32_t*, values, size_t, count)
{
    if (mdd == lddmc_false) return;
    if (mdd == lddmc_true) {
        WRAP(cb, values, count, context);
        return;
    }

    mddnode_t n = GETNODE(mdd);
    values[count] = mddnode_getvalue(n);
    CALL(_lddmc_sat_all_nopar, mddnode_getdown(n), cb, context, values, count+1);
    CALL(_lddmc_sat_all_nopar, mddnode_getright(n), cb, context, values, count);
}

VOID_TASK_IMPL_3(lddmc_sat_all_nopar, MDD, mdd, lddmc_sat_cb, cb, void*, context)
{
    // determine depth
    size_t count=0;
    MDD _mdd = mdd;
    while (_mdd > lddmc_true) {
        _mdd = mddnode_getdown(GETNODE(_mdd));
        assert(_mdd != lddmc_false);
        count++;
    }

    uint32_t values[count];
    CALL(_lddmc_sat_all_nopar, mdd, cb, context, values, 0);
}

VOID_TASK_IMPL_5(lddmc_sat_all_par, MDD, mdd, lddmc_sat_cb, cb, void*, context, uint32_t*, values, size_t, count)
{
    if (mdd == lddmc_false) return;
    if (mdd == lddmc_true) {
        WRAP(cb, values, count, context);
        return;
    }

    mddnode_t n = GETNODE(mdd);

    SPAWN(lddmc_sat_all_par, mddnode_getright(n), cb, context, values, count);

    uint32_t newvalues[count+1];
    if (count > 0) memcpy(newvalues, values, sizeof(uint32_t)*count);
    newvalues[count] = mddnode_getvalue(n);
    CALL(lddmc_sat_all_par, mddnode_getdown(n), cb, context, newvalues, count+1);

    SYNC(lddmc_sat_all_par);
}

struct lddmc_match_sat_info
{
    MDD mdd;
    MDD match;
    MDD proj;
    size_t count;
    uint32_t values[0];
};

// proj: -1 (rest 0), 0 (no match), 1 (match)
VOID_TASK_3(lddmc_match_sat, struct lddmc_match_sat_info *, info, lddmc_sat_cb, cb, void*, context)
{
    MDD a = info->mdd, b = info->match, proj = info->proj;

    if (a == lddmc_false || b == lddmc_false) return;

    if (a == lddmc_true) {
        assert(b == lddmc_true);
        WRAP(cb, info->values, info->count, context);
        return;
    }

    mddnode_t p_node = GETNODE(proj);
    uint32_t p_val = mddnode_getvalue(p_node);
    if (p_val == (uint32_t)-1) {
        assert(b == lddmc_true);
        CALL(lddmc_sat_all_par, a, cb, context, info->values, info->count);
        return;
    }

    /* Get nodes */
    mddnode_t na = GETNODE(a);
    mddnode_t nb = GETNODE(b);
    uint32_t na_value = mddnode_getvalue(na);
    uint32_t nb_value = mddnode_getvalue(nb);

    /* Skip nodes if possible */
    if (p_val == 1) {
        while (na_value != nb_value) {
            if (na_value < nb_value) {
                a = mddnode_getright(na);
                if (a == lddmc_false) return;
                na = GETNODE(a);
                na_value = mddnode_getvalue(na);
            }
            if (nb_value < na_value) {
                b = mddnode_getright(nb);
                if (b == lddmc_false) return;
                nb = GETNODE(b);
                nb_value = mddnode_getvalue(nb);
            }
        }
    }

    struct lddmc_match_sat_info *ri = (struct lddmc_match_sat_info*)alloca(sizeof(struct lddmc_match_sat_info)+sizeof(uint32_t[info->count]));
    struct lddmc_match_sat_info *di = (struct lddmc_match_sat_info*)alloca(sizeof(struct lddmc_match_sat_info)+sizeof(uint32_t[info->count+1]));

    ri->mdd = mddnode_getright(na);
    di->mdd = mddnode_getdown(na);
    ri->match = b;
    di->match = mddnode_getdown(nb);
    ri->proj = proj;
    di->proj = mddnode_getdown(p_node);
    ri->count = info->count;
    di->count = info->count+1;
    if (ri->count > 0) memcpy(ri->values, info->values, sizeof(uint32_t[info->count]));
    if (di->count > 0) memcpy(di->values, info->values, sizeof(uint32_t[info->count]));
    di->values[info->count] = na_value;

    SPAWN(lddmc_match_sat, ri, cb, context);
    CALL(lddmc_match_sat, di, cb, context);
    SYNC(lddmc_match_sat);
}

VOID_TASK_IMPL_5(lddmc_match_sat_par, MDD, mdd, MDD, match, MDD, proj, lddmc_sat_cb, cb, void*, context)
{
    struct lddmc_match_sat_info i;
    i.mdd = mdd;
    i.match = match;
    i.proj = proj;
    i.count = 0;
    CALL(lddmc_match_sat, &i, cb, context);
}

int
lddmc_sat_one(MDD mdd, uint32_t* values, size_t count)
{
    if (mdd == lddmc_false) return 0;
    if (mdd == lddmc_true) return 1;
    assert(count != 0);
    mddnode_t n = GETNODE(mdd);
    *values = mddnode_getvalue(n);
    return lddmc_sat_one(mddnode_getdown(n), values+1, count-1);
}

MDD
lddmc_sat_one_mdd(MDD mdd)
{
    if (mdd == lddmc_false) return lddmc_false;
    if (mdd == lddmc_true) return lddmc_true;
    mddnode_t n = GETNODE(mdd);
    MDD down = lddmc_sat_one_mdd(mddnode_getdown(n));
    return lddmc_makenode(mddnode_getvalue(n), down, lddmc_false);
}

TASK_IMPL_4(MDD, lddmc_compose, MDD, mdd, lddmc_compose_cb, cb, void*, context, int, depth)
{
    if (depth == 0 || mdd == lddmc_false || mdd == lddmc_true) {
        return (MDD)WRAP(cb, mdd, context);
    } else {
        mddnode_t n = GETNODE(mdd);
        REFS_INIT;
        SSPAWN(lddmc_compose, mddnode_getright(n), cb, context, depth);
        MDD down = lddmc_compose(mddnode_getdown(n), cb, context, depth-1);
        REFS_PUSH(down);
        MDD right = SSYNC(lddmc_compose);
        REFS_RESET;
        return lddmc_makenode(mddnode_getvalue(n), down, right);
    }
}

/**
 * GENERIC MARK/UNMARK METHODS
 */

static inline int
lddmc_mark(mddnode_t node)
{
    if (mddnode_getmark(node)) return 0;
    mddnode_setmark(node, 1);
    return 1;
}

static inline int
lddmc_unmark(mddnode_t node)
{
    if (mddnode_getmark(node)) {
        mddnode_setmark(node, 0);
        return 1;
    } else {
        return 0;
    }
}

static void
lddmc_unmark_rec(mddnode_t node)
{
    if (lddmc_unmark(node)) {
        MDD node_right = mddnode_getright(node);
        if (node_right > lddmc_true) lddmc_unmark_rec(GETNODE(node_right));
        MDD node_down = mddnode_getdown(node);
        if (node_down > lddmc_true) lddmc_unmark_rec(GETNODE(node_down));
    }
}

/*************
 * DOT OUTPUT
*************/

static void
lddmc_fprintdot_rec(FILE* out, MDD mdd)
{
    // assert(mdd > lddmc_true);

    // check mark
    mddnode_t n = GETNODE(mdd);
    if (mddnode_getmark(n)) return;
    mddnode_setmark(n, 1);

    // print the node
    uint32_t val = mddnode_getvalue(n);
    fprintf(out, "%" PRIu64 " [shape=record, label=\"", mdd);
    if (mddnode_getcopy(n)) fprintf(out, "<c> *");
    else fprintf(out, "<%u> %u", val, val);
    MDD right = mddnode_getright(n);
    while (right != lddmc_false) {
        mddnode_t n2 = GETNODE(right);
        uint32_t val2 = mddnode_getvalue(n2);
        fprintf(out, "|<%u> %u", val2, val2);
        right = mddnode_getright(n2);
        // assert(right != lddmc_true);
    }
    fprintf(out, "\"];\n");

    // recurse and print the edges
    for (;;) {
        MDD down = mddnode_getdown(n);
        // assert(down != lddmc_false);
        if (down > lddmc_true) {
            lddmc_fprintdot_rec(out, down);
            if (mddnode_getcopy(n)) {
                fprintf(out, "%" PRIu64 ":c -> ", mdd);
            } else {
                fprintf(out, "%" PRIu64 ":%u -> ", mdd, mddnode_getvalue(n));
            }
            if (mddnode_getcopy(GETNODE(down))) {
                fprintf(out, "%" PRIu64 ":c [style=solid];\n", down);
            } else {
                fprintf(out, "%" PRIu64 ":%u [style=solid];\n", down, mddnode_getvalue(GETNODE(down)));
            }
        }
        MDD right = mddnode_getright(n);
        if (right == lddmc_false) break;
        n = GETNODE(right);
    }
}

static void
lddmc_fprintdot_unmark(MDD mdd)
{
    if (mdd <= lddmc_true) return;
    mddnode_t n = GETNODE(mdd);
    if (mddnode_getmark(n)) {
        mddnode_setmark(n, 0);
        for (;;) {
            lddmc_fprintdot_unmark(mddnode_getdown(n));
            mdd = mddnode_getright(n);
            if (mdd == lddmc_false) return;
            n = GETNODE(mdd);
        }
    }
}

void
lddmc_fprintdot(FILE *out, MDD mdd)
{
    fprintf(out, "digraph \"DD\" {\n");
    fprintf(out, "graph [dpi = 300];\n");
    fprintf(out, "center = true;\n");
    fprintf(out, "edge [dir = forward];\n");

    // Special case: false
    if (mdd == lddmc_false) {
        fprintf(out, "0 [shape=record, label=\"False\"];\n");
        fprintf(out, "}\n");
        return;
    }

    // Special case: true
    if (mdd == lddmc_true) {
        fprintf(out, "1 [shape=record, label=\"True\"];\n");
        fprintf(out, "}\n");
        return;
    }

    lddmc_fprintdot_rec(out, mdd);
    lddmc_fprintdot_unmark(mdd);

    fprintf(out, "}\n");
}

void
lddmc_printdot(MDD mdd)
{
    lddmc_fprintdot(stdout, mdd);
}

/**
 * Some debug stuff
 */
void
lddmc_fprint(FILE *f, MDD mdd)
{
    lddmc_serialize_reset();
    size_t v = lddmc_serialize_add(mdd);
    fprintf(f, "%zu,", v);
    lddmc_serialize_totext(f);
}

void
lddmc_print(MDD mdd)
{
    lddmc_fprint(stdout, mdd);
}

/**
 * SERIALIZATION
 */

struct lddmc_ser {
    MDD mdd;
    size_t assigned;
};

// Define a AVL tree type with prefix 'lddmc_ser' holding
// nodes of struct lddmc_ser with the following compare() function...
AVL(lddmc_ser, struct lddmc_ser)
{
    return left->mdd - right->mdd;
}

// Define a AVL tree type with prefix 'lddmc_ser_reversed' holding
// nodes of struct lddmc_ser with the following compare() function...
AVL(lddmc_ser_reversed, struct lddmc_ser)
{
    return left->assigned - right->assigned;
}

// Initially, both sets are empty
static avl_node_t *lddmc_ser_set = NULL;
static avl_node_t *lddmc_ser_reversed_set = NULL;

// Start counting (assigning numbers to MDDs) at 2
static volatile size_t lddmc_ser_counter = 2;
static size_t lddmc_ser_done = 0;

// Given a MDD, assign unique numbers to all nodes
static size_t
lddmc_serialize_assign_rec(MDD mdd)
{
    if (mdd <= lddmc_true) return mdd;

    mddnode_t n = GETNODE(mdd);

    struct lddmc_ser s, *ss;
    s.mdd = mdd;
    ss = lddmc_ser_search(lddmc_ser_set, &s);
    if (ss == NULL) {
        // assign dummy value
        s.assigned = 0;
        ss = lddmc_ser_put(&lddmc_ser_set, &s, NULL);

        // first assign recursively
        lddmc_serialize_assign_rec(mddnode_getright(n));
        lddmc_serialize_assign_rec(mddnode_getdown(n));

        compiler_barrier();

        // assign real value
        ss->assigned = lddmc_ser_counter++;

        // put a copy in the reversed table
        lddmc_ser_reversed_insert(&lddmc_ser_reversed_set, ss);
    }

    return ss->assigned;
}

size_t
lddmc_serialize_add(MDD mdd)
{
    return lddmc_serialize_assign_rec(mdd);
}

void
lddmc_serialize_reset()
{
    lddmc_ser_free(&lddmc_ser_set);
    lddmc_ser_free(&lddmc_ser_reversed_set);
    lddmc_ser_counter = 2;
    lddmc_ser_done = 0;
}

size_t
lddmc_serialize_get(MDD mdd)
{
    if (mdd <= lddmc_true) return mdd;
    struct lddmc_ser s, *ss;
    s.mdd = mdd;
    ss = lddmc_ser_search(lddmc_ser_set, &s);
    assert(ss != NULL);
    return ss->assigned;
}

MDD
lddmc_serialize_get_reversed(size_t value)
{
    if ((MDD)value <= lddmc_true) return (MDD)value;
    struct lddmc_ser s, *ss;
    s.assigned = value;
    ss = lddmc_ser_reversed_search(lddmc_ser_reversed_set, &s);
    assert(ss != NULL);
    return ss->mdd;
}

void
lddmc_serialize_totext(FILE *out)
{
    avl_iter_t *it = lddmc_ser_reversed_iter(lddmc_ser_reversed_set);
    struct lddmc_ser *s;

    fprintf(out, "[");
    while ((s=lddmc_ser_reversed_iter_next(it))) {
        MDD mdd = s->mdd;
        mddnode_t n = GETNODE(mdd);
        fprintf(out, "(%zu,v=%u,d=%zu,r=%zu),", s->assigned,
                                                mddnode_getvalue(n),
                                                lddmc_serialize_get(mddnode_getdown(n)),
                                                lddmc_serialize_get(mddnode_getright(n)));
    }
    fprintf(out, "]");

    lddmc_ser_reversed_iter_free(it);
}

void
lddmc_serialize_tofile(FILE *out)
{
    size_t count = avl_count(lddmc_ser_reversed_set);
    assert(count >= lddmc_ser_done);
    assert(count == lddmc_ser_counter-2);
    count -= lddmc_ser_done;
    fwrite(&count, sizeof(size_t), 1, out);

    struct lddmc_ser *s;
    avl_iter_t *it = lddmc_ser_reversed_iter(lddmc_ser_reversed_set);

    /* Skip already written entries */
    size_t index = 0;
    while (index < lddmc_ser_done && (s=lddmc_ser_reversed_iter_next(it))) {
        assert(s->assigned == index+2);
        index++;
    }

    while ((s=lddmc_ser_reversed_iter_next(it))) {
        assert(s->assigned == index+2);
        index++;

        mddnode_t n = GETNODE(s->mdd);

        struct mddnode node;
        uint64_t right = lddmc_serialize_get(mddnode_getright(n));
        uint64_t down = lddmc_serialize_get(mddnode_getdown(n));
        if (mddnode_getcopy(n)) mddnode_makecopy(&node, right, down);
        else mddnode_make(&node, mddnode_getvalue(n), right, down);

        assert(right <= index);
        assert(down <= index);

        fwrite(&node, sizeof(struct mddnode), 1, out);
    }

    lddmc_ser_done = lddmc_ser_counter-2;
    lddmc_ser_reversed_iter_free(it);
}

void
lddmc_serialize_fromfile(FILE *in)
{
    size_t count, i;
    assert(fread(&count, sizeof(size_t), 1, in) == 1);

    for (i=1; i<=count; i++) {
        struct mddnode node;
        assert(fread(&node, sizeof(struct mddnode), 1, in) == 1);

        assert(mddnode_getright(&node) <= lddmc_ser_done+1);
        assert(mddnode_getdown(&node) <= lddmc_ser_done+1);

        MDD right = lddmc_serialize_get_reversed(mddnode_getright(&node));
        MDD down = lddmc_serialize_get_reversed(mddnode_getdown(&node));

        struct lddmc_ser s;
        if (mddnode_getcopy(&node)) s.mdd = lddmc_make_copynode(down, right);
        else s.mdd = lddmc_makenode(mddnode_getvalue(&node), down, right);
        s.assigned = lddmc_ser_done+2; // starts at 0 but we want 2-based...
        lddmc_ser_done++;

        lddmc_ser_insert(&lddmc_ser_set, &s);
        lddmc_ser_reversed_insert(&lddmc_ser_reversed_set, &s);
    }
}

static void
lddmc_sha2_rec(MDD mdd, SHA256_CTX *ctx)
{
    if (mdd <= lddmc_true) {
        SHA256_Update(ctx, (void*)&mdd, sizeof(uint64_t));
        return;
    }

    mddnode_t node = GETNODE(mdd);
    if (lddmc_mark(node)) {
        uint32_t val = mddnode_getvalue(node);
        SHA256_Update(ctx, (void*)&val, sizeof(uint32_t));
        lddmc_sha2_rec(mddnode_getdown(node), ctx);
        lddmc_sha2_rec(mddnode_getright(node), ctx);
    }
}

void
lddmc_printsha(MDD mdd)
{
    lddmc_fprintsha(stdout, mdd);
}

void
lddmc_fprintsha(FILE *out, MDD mdd)
{
    char buf[80];
    lddmc_getsha(mdd, buf);
    fprintf(out, "%s", buf);
}

void
lddmc_getsha(MDD mdd, char *target)
{
    SHA256_CTX ctx;
    SHA256_Init(&ctx);
    lddmc_sha2_rec(mdd, &ctx);
    if (mdd > lddmc_true) lddmc_unmark_rec(GETNODE(mdd));
    SHA256_End(&ctx, target);
}

size_t
lddmc_test_ismdd(MDD mdd)
{
    if (mdd == lddmc_true) return 1;
    if (mdd == lddmc_false) return 0;

    int first = 1;
    uint32_t value = 0;
    size_t depth = 0;

    if (mdd != lddmc_false) {
        mddnode_t n = GETNODE(mdd);
        if (mddnode_getcopy(n)) {
            mdd = mddnode_getright(n);
            depth = lddmc_test_ismdd(mddnode_getdown(n));
            assert(depth >= 1);
        }
    }

    while (mdd != lddmc_false) {
        assert(llmsset_is_marked(nodes, mdd));

        mddnode_t n = GETNODE(mdd);
        uint32_t next_value = mddnode_getvalue(n);
        assert(mddnode_getcopy(n) == 0);
        if (first) {
            first = 0;
            depth = lddmc_test_ismdd(mddnode_getdown(n));
            assert(depth >= 1);
        } else {
            assert(value < next_value);
            assert(depth == lddmc_test_ismdd(mddnode_getdown(n)));
        }

        value = next_value;
        mdd = mddnode_getright(n);
    }

    return 1 + depth;
}

TASK_2(size_t, lddmc_table_usage_par, size_t, start, size_t, end)
{
    if (end - start <= 128) {
        return llmsset_get_filled_partial(nodes, start, end);
    } else {
        size_t part = (end-start)/2;
        if (part < 128) part = 128;
        SPAWN(lddmc_table_usage_par, start, start+part);
        size_t end2 = start+2*part;
        if (end2 > end) end2 = end;
        size_t res = CALL(lddmc_table_usage_par, start+part, end2);;
        res += SYNC(lddmc_table_usage_par);
        return res;
    }
}

VOID_TASK_IMPL_2(lddmc_table_usage, size_t*, filled, size_t*, total)
{
    size_t tot = llmsset_get_size(nodes);
    if (filled != NULL) *filled = CALL(lddmc_table_usage_par, 0, tot);
    if (total != NULL) *total = tot;
}


