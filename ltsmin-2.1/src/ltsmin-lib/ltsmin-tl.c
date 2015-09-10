#include <hre/config.h>

#include <hre/user.h>
#include <ltsmin-lib/ltsmin-grammar.h>
#include <ltsmin-lib/ltsmin-parse-env.h> // required for ltsmin-lexer.h!
#include <ltsmin-lib/ltsmin-lexer.h>
#include <ltsmin-lib/ltsmin-tl.h>
#include <util-lib/chunk_support.h>
#include <util-lib/dynamic-array.h>

const char *
PRED_NAME(Pred pred)
{
    switch (pred) {
    case PRED_TRUE:             return "true";
    case PRED_FALSE:            return "false";
    case PRED_NOT:              return "!";
    case PRED_OR:               return "||";
    case PRED_AND:              return "&&";
    case PRED_EQ:               return "==";
    case PRED_EQUIV:            return "<->";
    case PRED_IMPLY:            return "->";
    default:        Abort ("Not a keyword/operator, token id: %d", pred);
    }
}

const char *
LTL_NAME(LTL ltl)
{
    switch (ltl) {
    case LTL_FUTURE:            return "<>";
    case LTL_GLOBALLY:          return "[]";
    case LTL_RELEASE:           return "R";
    case LTL_WEAK_UNTIL:        return "W";
    case LTL_STRONG_RELEASE:    Abort ("Strong release isn't implemented!");
    case LTL_NEXT:              return "X";
    case LTL_UNTIL:             return "U";
    default:                    return PRED_NAME((Pred)ltl);
    }
}


const char *
CTL_NAME(CTL ctl)
{
    switch (ctl) {
    case CTL_NEXT:          return "X";
    case CTL_UNTIL:         return "U";
    case CTL_FUTURE:        return "<>";
    case CTL_GLOBALLY:      return "[]";
    case CTL_EXIST:         return "E";
    case CTL_ALL:           return "A";
    default:                return PRED_NAME((Pred)ctl);
    }
}

const char *
MU_NAME(MU mu)
{
    switch (mu) {
    case MU_AND:                return "&";
    case MU_OR:                 return "|";

    case MU_EDGE_EXIST:         Abort ("Unimplemented: MU_EDGE_EXIST");
    case MU_EDGE_ALL:           Abort ("Unimplemented: MU_EDGE_ALL");
    case MU_EDGE_EXIST_LEFT:    return "<";
    case MU_EDGE_EXIST_RIGHT:   return ">";
    case MU_EDGE_ALL_LEFT:      return "[";
    case MU_EDGE_ALL_RIGHT:     return "]";
    case MU_MU:                 return "mu";
    case MU_NU:                 return "nu";
    case MU_NEXT:               return "X";
    case MU_EXIST:              return "E";
    case MU_ALL:                return "A";
    default:                    return PRED_NAME((Pred)mu);
    }
}

static stream_t
read_formula (const char *file)
{
    FILE *in=fopen( file, "r" );
    size_t *used = RTmalloc(sizeof(size_t));
    if (in) {
        return stream_input(in);
    } else {
        return stream_read_mem((void*)file, strlen(file), used);
    }
}

static void
fill_env (ltsmin_parse_env_t env, lts_type_t ltstype)
{
    int N = lts_type_get_state_length(ltstype);
    for (int i = 0; i < N; i++) {
        char*name=lts_type_get_state_name(ltstype,i);
        HREassert (name);
        int idx = LTSminStateVarIndex(env,name);
        HREassert (i == idx, "Model has equally named state variables ('%s') at index %d and %d", name, i, idx);
    }

    int L = lts_type_get_state_label_count(ltstype);
    for (int i = 0; i < L; i++) {
        char*name=lts_type_get_state_label_name(ltstype,i);
        HREassert (name);
        // consider state label an state variable with idx >= N
        int idx = LTSminStateVarIndex(env,name);
        HREassert (N + i == idx, "Model has equally named state labels ('%s') at index %d and %d", name, i - N, idx);
    }

    int E=lts_type_get_edge_label_count(ltstype);
    for (int i = 0; i < E; i++) {
        char*name=lts_type_get_edge_label_name(ltstype,i);
        HREassert (name);
        int idx = LTSminEdgeVarIndex(env,name);
        HREassert (i == idx);
    }
}

ltsmin_expr_t
pred_parse_file(const char *file, ltsmin_parse_env_t env, lts_type_t ltstype)
{
    stream_t stream = read_formula (file);

    fill_env (env, ltstype);

    LTSminConstant      (env, PRED_FALSE,  PRED_NAME(PRED_FALSE));
    LTSminConstant      (env, PRED_TRUE,   PRED_NAME(PRED_TRUE));

    LTSminBinaryOperator(env, PRED_EQ,     PRED_NAME(PRED_EQ), 1);
    LTSminPrefixOperator(env, PRED_NOT,    PRED_NAME(PRED_NOT),  2);

    LTSminBinaryOperator(env, PRED_AND,    PRED_NAME(PRED_AND),  4);
    LTSminBinaryOperator(env, PRED_OR,     PRED_NAME(PRED_OR),  5);

    LTSminBinaryOperator(env, PRED_EQUIV,  PRED_NAME(PRED_EQUIV),6);
    LTSminBinaryOperator(env, PRED_IMPLY,  PRED_NAME(PRED_IMPLY), 7);

    ltsmin_parse_stream(TOKEN_EXPR,env,stream);
    ltsmin_expr_t expr=env->expr;

    return expr;
}

/******************************************************************
 * Note: some of these functions leak memory of type ltsmin_expr_t
 *****************************************************************/

/* LTL:
 *     E. M. Clarke, O. Grumberg and D. A. Peled,
 *     Model Checking,
 *     MIT Press,1999
 *
 * Nonstandard connectives:
 * (Until)      p U q = F q | (p W q)
 * (Release)    p R q = q W (p & q)
 * (Release)    p R q = ! (!p U !q) (dual of until)
 * (Weak Until) p W q = q R (q | p)
 * (Weak Until) p W q = (p U q) | G p
 * (Strong Release) p M q = ! (!p W !q) (dual of weak until)
 *
 * Operator percendence
 *  HIGH
 *     ==, <=, >=, != (state label expression)
 *     !
 *     G, F, X
 *     &
 *     |
 *     ^ (XOR)
 *     <-> (EQUIV)
 *     -> (IMPLY)
 *     U, R
 *  LOW
 */

/* Convert weak untils to until or generally */
static ltsmin_expr_t
ltl_tree_walker(ltsmin_expr_t in)
{
    ltsmin_expr_t arg1, arg2, u, g;
    // handle sub-expressions
    switch (in->node_type) {
        case UNARY_OP:
            arg1 = ltl_tree_walker(in->arg1);
            in->arg1 = arg1;
            LTSminExprRehash(in);
            break;
        case BINARY_OP:
            arg1 = ltl_tree_walker(in->arg1);
            arg2 = ltl_tree_walker(in->arg2);
            switch (in->token) {
                case LTL_WEAK_UNTIL:
                    u = LTSminExpr(BINARY_OP, LTL_UNTIL, 0, arg1, arg2);
                    g = LTSminExpr(UNARY_OP, LTL_GLOBALLY, 0, arg1, NULL);
                    RTfree (in);
                    in = LTSminExpr(BINARY_OP, LTL_OR, 0, u, g);
                    break;
                default:
                    in->arg1 = arg1;
                    in->arg2 = arg2;
                    LTSminExprRehash(in);
                    break;
            }
            break;
        default:
            break;
    }
    return in;
}

/* Parser Priorities:
 * HIGH
 *     binary priority 1          : ==, !=, etc
 *     prefix priority 2          : !
 *     prefix priority 3          : G,F,X
 *     binary priority 4          : &
 *     binary priority 5          : |
 *     binary priority 6          : <->
 *     binary priority 7          : <->
 *     binary priority 8          : U,R
 * LOW
 */
ltsmin_expr_t
ltl_parse_file(const char *file, ltsmin_parse_env_t env, lts_type_t ltstype)
{
    stream_t stream = read_formula (file);

    fill_env (env, ltstype);

    LTSminConstant      (env, LTL_FALSE,        LTL_NAME(LTL_FALSE));
    LTSminConstant      (env, LTL_TRUE,         LTL_NAME(LTL_TRUE));

    LTSminBinaryOperator(env, LTL_EQ,           LTL_NAME(LTL_EQ), 1);
    LTSminPrefixOperator(env, LTL_NOT,          LTL_NAME(LTL_NOT),  2);

    LTSminPrefixOperator(env, LTL_GLOBALLY,     LTL_NAME(LTL_GLOBALLY), 3);
    LTSminPrefixOperator(env, LTL_FUTURE,       LTL_NAME(LTL_FUTURE), 3);
    LTSminPrefixOperator(env, LTL_NEXT,         LTL_NAME(LTL_NEXT),  3);

    LTSminBinaryOperator(env, LTL_AND,          LTL_NAME(LTL_AND),  4);
    LTSminBinaryOperator(env, LTL_OR,           LTL_NAME(LTL_OR),  5);

    LTSminBinaryOperator(env, LTL_EQUIV,        LTL_NAME(LTL_EQUIV),6);
    LTSminBinaryOperator(env, LTL_IMPLY,        LTL_NAME(LTL_IMPLY), 7);

    LTSminBinaryOperator(env, LTL_UNTIL,        LTL_NAME(LTL_UNTIL),  8);
    LTSminBinaryOperator(env, LTL_WEAK_UNTIL,   LTL_NAME(LTL_WEAK_UNTIL),  8); // translated to U \/ []
    LTSminBinaryOperator(env, LTL_RELEASE,      LTL_NAME(LTL_RELEASE),  8);

    ltsmin_parse_stream(TOKEN_EXPR,env,stream);
    env->expr = ltl_tree_walker (env->expr);
    ltsmin_expr_t expr = env->expr;

    return expr;
}


/* CTL:
 *     E. M. Clarke, E. A. Emerson and A. P. Sistla,
 *     Automatic Verification of Finite-State Concurrent Systems Using Temporal Logic Specifications,
 *     ACM Transactions on Programming Languages and Systems,
 *     vol 8-2, pages 244-263, April, 1986
 *
 * Operator percendence
 *  HIGH
 *     =, <=, >=, != (state label expression)
 *     !
 *     AG, AF, AX, EG, EF, EX
 *     &
 *     |
 *     ^ (XOR)
 *     <-> (EQUIV)
 *     -> (IMPLY)
 *     U (?AU, EU?)
 *  LOW
 */
ltsmin_expr_t
ctl_parse_file(const char *file, ltsmin_parse_env_t env, lts_type_t ltstype)
{
    stream_t stream = read_formula (file);

    fill_env (env, ltstype);

    LTSminConstant      (env, CTL_FALSE,        CTL_NAME(CTL_FALSE));
    LTSminConstant      (env, CTL_TRUE,         CTL_NAME(CTL_TRUE));

    LTSminBinaryOperator(env, CTL_EQ,           CTL_NAME(CTL_EQ), 1);
    LTSminPrefixOperator(env, CTL_NOT,          CTL_NAME(CTL_NOT),  2);

    LTSminPrefixOperator(env, CTL_EXIST,        CTL_NAME(CTL_EXIST),  3);
    LTSminPrefixOperator(env, CTL_ALL,          CTL_NAME(CTL_ALL),  3);

    LTSminPrefixOperator(env, CTL_GLOBALLY,     CTL_NAME(CTL_GLOBALLY), 3);
    LTSminPrefixOperator(env, CTL_FUTURE,       CTL_NAME(CTL_FUTURE), 3);
    LTSminPrefixOperator(env, CTL_NEXT,         CTL_NAME(CTL_NEXT),  3);

    LTSminBinaryOperator(env, CTL_AND,          CTL_NAME(CTL_AND),  4);
    LTSminBinaryOperator(env, CTL_OR,           CTL_NAME(CTL_OR),  5);

    LTSminBinaryOperator(env, CTL_EQUIV,        CTL_NAME(CTL_EQUIV),6);
    LTSminBinaryOperator(env, CTL_IMPLY,        CTL_NAME(CTL_IMPLY), 7);

    LTSminBinaryOperator(env, CTL_UNTIL,        CTL_NAME(CTL_UNTIL),  8);

    ltsmin_parse_stream(TOKEN_EXPR,env,stream);
    ltsmin_expr_t expr=env->expr;

    return expr;
}


/*
 * From: Modal mu-calculi, Julian Bradfield and Colin Stirling
 * For the concrete syntax, we shall assume that modal operators have higher
 * precedence than boolean, and that fixpoint operators have lowest precedence,
 * so that the scope of a fixpoint extends as far to the right as possible.
 *
 * note: when checking priorities, use ltsmin-grammer.lemon
 * for priorities, notice that prefix 1 has a higher priority than bin 1
 */
ltsmin_expr_t
mu_parse_file(const char *file, ltsmin_parse_env_t env, lts_type_t ltstype)
{
    stream_t stream = read_formula (file);

    fill_env (env, ltstype);

    LTSminConstant      (env, MU_FALSE,        "false");
    LTSminConstant      (env, MU_TRUE,         "true");

    LTSminKeyword(env, MU_MU, "mu");
    LTSminKeyword(env, MU_NU, "nu");
    LTSminKeyword(env, MU_EDGE_EXIST_LEFT,  "<");
    LTSminKeyword(env, MU_EDGE_EXIST_RIGHT, ">");
    LTSminKeyword(env, MU_EDGE_ALL_LEFT,  "[");
    LTSminKeyword(env, MU_EDGE_ALL_RIGHT, "]");

    LTSminBinaryOperator(env, MU_AND, "&",2);
    LTSminBinaryOperator(env, MU_AND, "/\\",2);
    LTSminBinaryOperator(env, MU_OR, "\\/",3);
    LTSminBinaryOperator(env, MU_OR, "|",3);
    LTSminBinaryOperator(env, MU_EQ, "==",1);
    LTSminPrefixOperator(env, MU_NOT, "!",1);

    LTSminPrefixOperator(env, MU_NEXT, "X",4);
    LTSminPrefixOperator(env, MU_EXIST, "E",4);
    LTSminPrefixOperator(env, MU_ALL, "A",4);

    ltsmin_parse_stream(TOKEN_EXPR,env,stream);
    ltsmin_expr_t expr=env->expr;

    env->expr=NULL;
    return expr;
}


/* Translate LTL token into semantically equivalent CTL token */
ltsmin_expr_t ltl_to_ctl_star_1(ltsmin_expr_t in)
{
    ltsmin_expr_t res = RT_NEW(struct ltsmin_expr_s);
    memcpy(res, in, sizeof(struct ltsmin_expr_s));
    switch (in->token) {
        case LTL_SVAR:      res->token = CTL_SVAR;      break;
        case LTL_EVAR:      res->token = CTL_EVAR;      break;
        case LTL_NUM:       res->token = CTL_NUM;       break;
        case LTL_CHUNK:     res->token = CTL_CHUNK;     break;
        case LTL_VAR:       res->token = CTL_VAR;       break;
        case LTL_EQ:        res->token = CTL_EQ;        break;
        case LTL_TRUE:      res->token = CTL_TRUE;      break;
        case LTL_FALSE:     res->token = CTL_FALSE;     break;
        case LTL_OR:        res->token = CTL_OR;        break;
        case LTL_AND:       res->token = CTL_AND;       break;
        case LTL_NOT:       res->token = CTL_NOT;       break;
        case LTL_NEXT:      res->token = CTL_NEXT;      break;
        case LTL_UNTIL:     res->token = CTL_UNTIL;     break;
        case LTL_IMPLY:     res->token = CTL_IMPLY;     break;
        case LTL_EQUIV:     res->token = CTL_EQUIV;     break;
        case LTL_FUTURE:    res->token = CTL_FUTURE;    break;
        case LTL_GLOBALLY:  res->token = CTL_GLOBALLY;  break;
        //case LTL_RELEASE:   // convert..
        //case LTL_WEAK_UNTIL // convert..
        default:
            // unhandled?
            Abort("unhandled case in ltl_to_ctl_star");
    }
    // handle sub-expressions
    switch (in->node_type) {
        // UNARY
        case UNARY_OP:
            res->arg1 = ltl_to_ctl_star_1(res->arg1);
            break;
        // BINARY
        case BINARY_OP:
            res->arg1 = ltl_to_ctl_star_1(res->arg1);
            res->arg2 = ltl_to_ctl_star_1(res->arg2);
            break;
        default:
            break;
    }
    LTSminExprRehash(res);
    return res;
}

/* Translate LTL formula phi into CTL* formula A (phi) */
ltsmin_expr_t ltl_to_ctl_star(ltsmin_expr_t in)
{
    ltsmin_expr_t res = LTSminExpr(UNARY_OP, CTL_ALL, 0, ltl_to_ctl_star_1(in), NULL);
    return res;
}

ltsmin_expr_t ltl_normalize(ltsmin_expr_t in)
{
    return in;
}

/* Any CTL formula is also a CTL* formula */
ltsmin_expr_t ctl_to_ctl_star(ltsmin_expr_t in)
{
    // just return the ctl furmula
    return in;
}

ltsmin_expr_t ctl_normalize(ltsmin_expr_t in)
{
    /* AP is the set of atomic propositions. CTL is the
       smalles set of all state formulas such that
       * p \in AP is a state formula
       * if f and g are state formulas then !f and f & g are state formulas.
       * if f and g are state formulas then Xf, Ff, Gf, fUg and fRg are path
         formulas.
       * If f is a path formula, then Af and Ef are state formulas.
       *
       * The then compound operators can all be expressed in terms of
       * three operators, EX, EG and EU.
       *
       * AX f = !EX(!f)
       * EF f = E(true U f)
       * AG f = !EF(!f) = !E(true U !f)
       * AF f = !EG(!f)
       * A[f U g] = !E[!gU(!f & !g)] & !EG(!g)
       * A[f R g] = !E[!f U !g]
       * E[f R g] = !A[!f U !g]
       */
    return in;
}

ltsmin_expr_t ctl_to_mu(ltsmin_expr_t in)
{
    /* Source: formalising the translation of CTL into L_mu
     * T is the translation from CTL to mu
     * T(p \in AP) = p
     * T(!f) = !T(f)
     * T(f & g) = T(g) & T(g)
     * T(EX f) = <.> T(f)
     * T(EG f) = nu Q. f & <.> Q
     * T(E[f U g]) = mu Q. g | (f & <.> Q)
     */
     return in;
}

int tableaux_node_eq(tableaux_node_t *n1, tableaux_node_t *n2)
{
    if (!n1 && !n2) return 1;
    if (!n1 || !n2) return 0;
    if (n1 == n2) return 1;
    if (n1->hash != n2->hash) return 0;
    // check quantifier
    if (n1->quantifier != n2->quantifier) return 0;
    // check expression list
    tableaux_expr_list_t *n1l = n1->expr_list;
    tableaux_expr_list_t *n2l = n2->expr_list;
    while(n1l && n2l) {
        // expressions are supposed to be unique, ie
        // there is at most 1 pointer to one expression
        if (n1l->expr != n2l->expr) return 0;
        n1l = n1l->next; n2l = n2l->next;
    }
    return (!n1l && !n2l);
}

/* create a structure that holds the tableaux to convert the ctl star to mu calculus */
tableaux_t* tableaux_create()
{
    tableaux_t *t = RTmallocZero(sizeof(tableaux_t));
    t->expressions.size = 1 << 5;
    t->expressions.table = RTmallocZero(sizeof(tableaux_table_item_t)*t->expressions.size);
    t->expressions.fn_eq = (int (*)(void*,void*))LTSminExprEq;

    t->nodes.size = 1 << 5;
    t->nodes.table = RTmallocZero(sizeof(tableaux_table_item_t)*t->nodes.size);
    t->nodes.fn_eq = (int (*)(void*,void*))tableaux_node_eq;
    return t;
}

/* destroy the tableaux */
void tableaux_destroy(tableaux_t *t)
{
    RTfree(t->expressions.table);
    RTfree(t->nodes.table);
    RTfree(t);
}

/* grow the simple hash table, size = size * 2 */
void tableaux_table_grow(tableaux_table_t *t)
{
    tableaux_table_t new_t;
    new_t.size = t->size << 1;
    new_t.count = 0;
    new_t.table = RTmallocZero(sizeof(tableaux_table_item_t)*new_t.size);
    if (new_t.table) {
        // move all elements
        for(int el=0; el < t->size; el++) {
            if (t->table[el].data != NULL)
                tableaux_table_add(&new_t, t->table[el].hash, t->table[el].data);
        }
        // swap internal tables
        RTfree(t->table);
        t->table = new_t.table;
        t->size = new_t.size;
        return;
    }
    Abort("tl tableaux realloc failed");
}

/* add an element to the simple hash table
 * data is added by linear probing, if bucket already used, try bucket+1, +2,...
 * Requires:
 * (equal) element not already in the table
 * data != NULL
 */
void tableaux_table_add(tableaux_table_t *t, uint32_t hash, void* data)
{
    //HREassert(hash != NULL, "no hash");
    uint32_t key = hash & (t->size-1);
    while(1) {
        // empty bucket?
        if (t->table[key].data == NULL) {
            t->table[key].hash = hash; t->table[key].data = data;
            break;
        } else {
            // linear probing
            key = (key+1) % t->size;
        }
    }
    t->count++;

    // grow table if load factor > 0.7
    if (t->count > t->size * 0.7)
        tableaux_table_grow(t);
}

/* lookup a hash in the table, use fn_eq set in struct to compare data */
void* tableaux_table_lookup(tableaux_table_t *t, uint32_t hash, void* data)
{
    uint32_t key = hash & (t->size-1);
    while(1) {
        // found?
        if (t->table[key].hash == hash) {
            if (t->fn_eq(t->table[key].data, data)) {
                return t->table[key].data;
            }

        // empty bucket?
        } else if (t->table[key].data == NULL) {
            return NULL;
        }

        // linear probing
        key = (key+1) % t->size;
    }
}

/* for debuggin only */
char *ltsmin_expr_print_ltl(ltsmin_expr_t ltl,char* buf)
{
    // no equation
    HREassert (ltl, "Empty LTL expression");

    // left eq
    switch(ltl->node_type) {
        case BINARY_OP:
            *buf++='(';
            buf = ltsmin_expr_print_ltl(ltl->arg1, buf);
        default:;
    }
    // middle
    switch(ltl->token) {
        case LTL_SVAR: sprintf(buf, "@S%d", ltl->idx); break;
        case LTL_EVAR: sprintf(buf, "@E%d", ltl->idx); break;
        case LTL_NUM: sprintf(buf, "%d", ltl->idx); break;
        case LTL_VAR:
            if (-1 == ltl->num)
                sprintf(buf, "@V%d", ltl->idx);
            else
                sprintf(buf, "@C%d", ltl->num);
            break;
        case LTL_CHUNK: sprintf(buf, "@H%d", ltl->idx); break;
        case LTL_EQ: sprintf(buf, " == "); break;
        case LTL_TRUE: sprintf(buf, "true"); break;
        case LTL_OR: sprintf(buf, " or "); break;
        case LTL_NOT: sprintf(buf, "!"); break;
        case LTL_NEXT: sprintf(buf, "X "); break;
        case LTL_UNTIL: sprintf(buf, " U "); break;
        case LTL_FALSE: sprintf(buf, "false"); break;
        case LTL_AND: sprintf(buf, " and "); break;
        case LTL_EQUIV: sprintf(buf, " <-> "); break;
        case LTL_IMPLY: sprintf(buf, " -> "); break;
        case LTL_FUTURE: sprintf(buf, "F "); break;
        case LTL_GLOBALLY: sprintf(buf, "G "); break;
        default:
            Abort("unknown LTL token");
    }
    buf += strlen(buf);
    // right eq
    switch(ltl->node_type) {
        case UNARY_OP:
            buf = ltsmin_expr_print_ltl(ltl->arg1, buf);
            break;
        case BINARY_OP:
            buf = ltsmin_expr_print_ltl(ltl->arg2, buf);
            *buf++=')';
            break;
        default:;
    }
    *buf='\0';
    return buf;
}

/* print ctl/ctl* expression in a buffer
 * returns the buffer + size taken for the expression
 * assumes buffer is large enough */
char* ltsmin_expr_print_ctl(ltsmin_expr_t ctl, char* buf)
{
    // no equation
    if (!ctl) return buf;

    // left eq
    switch(ctl->node_type) {
        case BINARY_OP:
            *buf++='(';
            buf = ltsmin_expr_print_ctl(ctl->arg1, buf);
        default:;
    }
    // middle
    switch(ctl->token) {
        case CTL_SVAR: sprintf(buf, "@S%d", ctl->idx); break;
        case CTL_EVAR: sprintf(buf, "@E%d", ctl->idx); break;
        case CTL_NUM: sprintf(buf, "%d", ctl->idx); break;
        case CTL_VAR:
            if (-1 == ctl->num)
                sprintf(buf, "@V%d", ctl->idx);
            else
                sprintf(buf, "@C%d", ctl->num);
            break;
        case CTL_CHUNK: sprintf(buf, "@H%d", ctl->idx); break;
        case CTL_EQ: sprintf(buf, " == "); break;
        case CTL_TRUE: sprintf(buf, "true"); break;
        case CTL_OR: sprintf(buf, " or "); break;
        case CTL_NOT: sprintf(buf, "!"); break;
        case CTL_NEXT: sprintf(buf, "X "); break;
        case CTL_UNTIL: sprintf(buf, " U "); break;
        case CTL_FALSE: sprintf(buf, "false"); break;
        case CTL_AND: sprintf(buf, " and "); break;
        case CTL_EQUIV: sprintf(buf, " <-> "); break;
        case CTL_IMPLY: sprintf(buf, " -> "); break;
        case CTL_FUTURE: sprintf(buf, "F "); break;
        case CTL_GLOBALLY: sprintf(buf, "G "); break;
        case CTL_EXIST: sprintf(buf, "E "); break;
        case CTL_ALL: sprintf(buf, "A "); break;
        default:
            Abort("unknown CTL token");
    }
    buf += strlen(buf);
    // right eq
    switch(ctl->node_type) {
        case UNARY_OP:
            buf = ltsmin_expr_print_ctl(ctl->arg1, buf);
            break;
        case BINARY_OP:
            buf = ltsmin_expr_print_ctl(ctl->arg2, buf);
            *buf++=')';
            *buf='\0';
            break;
        default:;
    }
    *buf='\0';
    return buf;
}

/* print a node in a buffer */
void tableaux_node_print(tableaux_node_t *n, char* buf)
{
    if (n == NULL) {
        sprintf(buf, "NULL"); return;
    }
    char Q[] = {'N', 'A', 'E'};
    sprintf(buf, "%c: [", Q[n->quantifier]);
    buf += strlen(buf);
    tableaux_expr_list_t *l = n->expr_list;
    while(l != NULL) {
        if (l != n->expr_list) { sprintf(buf, ","); buf++; }
        buf = ltsmin_expr_print_ctl(l->expr, buf);
        /* the following lines also print the generating expressions
        if (l->generating_expr[0]) {
        sprintf(buf, "<~");
        buf += strlen(buf);
        buf = ltsmin_expr_print_ctl(l->generating_expr[0]->expr, buf);
        }
        if (l->generating_expr[1]) {
        sprintf(buf, "<~");
        buf += strlen(buf);
        buf = ltsmin_expr_print_ctl(l->generating_expr[1]->expr, buf);
        } */
        l = l->next;
    }
    sprintf(buf, "]");
}

/* Add an expression to a node
 * If no node (n == NULL), create a new node structure
 * otherwise, add the expression in the expression list, in increasing order of
 * the hash
 */
tableaux_node_t* tableaux_node_add_expr(tableaux_node_quantifier_t q, tableaux_node_t *n, ltsmin_expr_t e, tableaux_expr_list_t *generating_e)
{
    tableaux_node_t *res = n;
    // add expression to tableaux node
    if (res == NULL) {
        res = RTmallocZero(sizeof(tableaux_node_t));
        res->quantifier = q;
        res->hash = (0x9AB724D1 * q) + e->hash;
        // add first expression
        res->expr_list = RTmalloc(sizeof(tableaux_expr_list_t));
        res->expr_list->expr = e;
        res->expr_list->next = NULL;
        res->expr_list->generating_expr[0] = generating_e;
        res->expr_list->generating_expr[1] = NULL;
        return res;
    } else {
        if (res->quantifier != q) Abort("Tableaux node quantifiers don't match")
    }
    // add to tableaux list
    // note: list is sorted in increasing order of the hash values of the expressions
    // new expression
    tableaux_expr_list_t *ne = RTmalloc(sizeof(tableaux_expr_list_t));
    ne->expr = e;
    ne->generating_expr[0] = generating_e;
    ne->generating_expr[1] = NULL;
    // list pointer for updating
    tableaux_expr_list_t **l = &n->expr_list;
    while (*l != NULL) {
        if ( (*l)->expr->hash < e->hash) break;
        // assume unique expressions are added
        if ( (*l)->expr == e) {
            RTfree(ne);
            // add generating expression
            if ((*l)->generating_expr[0] != generating_e && (*l)->generating_expr[1] != generating_e) {
                if ((*l)->generating_expr[0] == NULL) {
                    (*l)->generating_expr[0] = generating_e;
                } else {
                    if ((*l)->generating_expr[1] != NULL) Abort("invalid assumption: generating_expr[<2]");
                    (*l)->generating_expr[1] = generating_e;
                }
            }
            return res;
        }
        l =  &(*l)->next;
    }
    // add new item to list
    ne->next = *l;
    *l = ne;

    // update hash
    // hash = sum of hashes of the expressions + (0x9AB724D1 * quantifier);
    res->hash += e->hash;
    return res;
}

/* for debuggin, build a test node instead of a parsed ctl* formula */
tableaux_node_t* build_test_node(tableaux_t *t)
{
    ltsmin_expr_t n = LTSminExpr(INT, CTL_NUM, 1, NULL, NULL);
    ltsmin_expr_t v = LTSminExpr(VAR, CTL_VAR, 0, NULL, NULL);
    ltsmin_expr_t eq = LTSminExpr(BINARY_OP, CTL_EQ, 0, v, n);
    ltsmin_expr_t g = LTSminExpr(UNARY_OP, CTL_GLOBALLY, 0, eq, NULL);
    ltsmin_expr_t xg = LTSminExpr(UNARY_OP, CTL_NEXT, 0, g, NULL);
    ltsmin_expr_t fxg = LTSminExpr(UNARY_OP, CTL_FUTURE, 0, xg, NULL);
    ltsmin_expr_t xfxg = LTSminExpr(UNARY_OP, CTL_NEXT, 0, fxg, NULL);

    // add expressions
    tableaux_table_add(&t->expressions, xg->hash, xg);
    tableaux_table_add(&t->expressions, xfxg->hash, xfxg);

    // create node
    tableaux_node_t *res = tableaux_node_add_expr(NODE_ALL, NULL, xg, NULL);
    res = tableaux_node_add_expr(NODE_ALL, res, xfxg, NULL);

    // add node
    tableaux_table_add(&t->nodes, res->hash, res);

    return res;
}

/* Extend a syntax tree node on branch b with a label and a node
 * Some care must be taken to extend TABLEAUX rules
 * If a NULL node is added to the left side of a tableaux expression
 * The tableaux rule is set to TABLEAUX_IDENTITY instead
 * 1) if a NULL node is added, this tableaux is not extended and the sytax
 *    tree is labelled TABLEAUX_IDENTITY instead
 * 2) if the right side is added, the left side is used instead
 *
 * 3) If both the nodes are NULL, this is handled by apply rule
 */
syntax_tree_t* tableaux_extend_syntax_tree(tableaux_t *t, syntax_tree_t *s, syntax_tree_branch_t b, syntax_tree_label_t label, tableaux_node_t *n)
{
    // whoops, no expressions in node
    if (n == NULL) {
        s->label = TABLEAUX_IDENTITY;
        return NULL;
    }

    // allocate syntax tree ndoe
    syntax_tree_t *st = RTmallocZero(sizeof(syntax_tree_t));

    // lookup n, already in tableaux?
    tableaux_node_t *n_old;
    if ((n_old = tableaux_table_lookup(&t->nodes, n->hash, n))) {
        // is this a preterminal?
        // is there a node n' strictly above n that has an equal labelling?
        {
            syntax_tree_t *p = s->parent;
            while(p) {
                if (tableaux_node_eq(p->node, n_old)) {
                    st->companion = p;
                    p->is_companion = 1;

                    st->label = TABLEAUX_PRETERMINAL;
                    break;
                }
                p = p->parent;
            }
        }
    } else {
        // add node to tableaux table
        tableaux_table_add(&t->nodes, n->hash, n);
    }
    st->node = n;
    st->parent = s;
    if (s->label != TABLEAUX_IDENTITY) {
        s->label = label;
        s->branch[b] = st;
    } else {
        s->branch[ST_LEFT] = st;
    }
    return st;
}

/* Adds all unused expressions (not in rule application) to a new node */
tableaux_node_t* tableaux_node_delta_phi(tableaux_node_quantifier_t q, tableaux_node_t *n_origin, tableaux_expr_list_t *generating_e)
{
    tableaux_node_t *n = NULL;
    tableaux_expr_list_t *l = n_origin->expr_list;
    while(l) {
        if (l != generating_e)
            n = tableaux_node_add_expr(q, n, l->expr, l);
        l = l->next;
    }
    return n;
}

/* returns the equivalent expression already in the tableaux table, or
 * adds the expression to the tableaux table */
ltsmin_expr_t tableaux_expr(tableaux_t *t, ltsmin_expr_t e)
{
    // convert expression to positive normal form
    e = ctl_star_to_pnf(e);
    ltsmin_expr_t result = NULL;
    // assume arg1 is set
    if (!(result = tableaux_table_lookup(&t->expressions, e->hash, e))) {
        result = e;
        tableaux_table_add(&t->expressions, e->hash, e);
    } else {
        LTSminExprDestroy(e);
    }
    return result;
}

/* return the tail of a unary expression: X phi returns phi */
ltsmin_expr_t tableaux_expr_unary_tail(tableaux_t *t, ltsmin_expr_t e)
{
    // return tail of a unary expression UNARY_OP phi thus phi
    // the function also looks up the expression in the expression
    // table, such that no duplicates can exist
    if (e->node_type != UNARY_OP) Abort("tableaux_expr_unary_tail on non-unary operator");
    return tableaux_expr(t, e->arg1);
}

/* prepend a unary_op to an expression */
ltsmin_expr_t tableaux_expr_unary_cons(tableaux_t *t, CTL op, ltsmin_expr_t e)
{
    // convert node to positive normal form
    ltsmin_expr_t ee = LTSminExpr(UNARY_OP, op, 0, e, NULL);
    ltsmin_expr_t res = tableaux_expr(t, ee);
    RTfree(ee);
    return res;
}

/* Apply a rule in the tableaux */
void tableaux_apply_rule(tableaux_t *t, syntax_tree_t *s)
{
    // need to fill label, and all nodes below the line in the syntax tree
    switch(s->node->quantifier)
    {
        case NODE_EXIST:
        case NODE_ALL: {
            // transition rule:
            //  A ( X phi_1, ... , X phi_n )
            // ----------------------------
            //  A ( phi_1, ... , phi_n )

            // applicable?
            // do all expressions start with CTL_NEXT?
            tableaux_expr_list_t *el = s->node->expr_list;
            while(el) {
                if (el->expr->token == CTL_NEXT) {
                    el = el->next;
                } else break;
            }
            // apply transition rule if at end of list
            if (!el) {
                tableaux_node_t *n = NULL;
                // insert all expressions in table
                el = s->node->expr_list;
                while(el) {
                    ltsmin_expr_t e = tableaux_expr_unary_tail(t, el->expr);
                    n = tableaux_node_add_expr(s->node->quantifier, n, e, el);
                    el = el->next;
                }

                tableaux_extend_syntax_tree(t, s, ST_LEFT, s->node->quantifier == NODE_ALL?TABLEAUX_ALL_NEXT:TABLEAUX_EXIST_NEXT, n);
            } else {
                // note: el->expr is the first expression in the list for which a rule is applicable
                tableaux_node_t *n_left = NULL;
                tableaux_node_t *n_right = NULL;
                switch(el->expr->token) {
                    case CTL_FUTURE:
                    case CTL_GLOBALLY: {

                        /* The four cases
                         *           A(PHI, F phi)                    A(PHI, G phi)
                         *  I  ----------------------   /\  ----------------------------------
                         *      A(PHI, phi, O F phi))        A(PHI, phi)  A(PSI, phi, O G phi)
                         *
                         *           E(PHI, G phi)                    E(PHI, F phi)
                         *  I  ----------------------   \/  ----------------------------------
                         *      E(PHI, phi, O G phi))        E(PHI, phi)  E(PSI, phi, O F phi)
                         */

                        int split = ((s->node->quantifier == NODE_ALL && el->expr->token == CTL_GLOBALLY ) ||
                                     (s->node->quantifier == NODE_EXIST && el->expr->token == CTL_FUTURE) );
                        syntax_tree_label_t l = el->expr->token == CTL_GLOBALLY ? TABLEAUX_AND : TABLEAUX_OR;
                        if (split) {
                            n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                            n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_tail(t, el->expr), el);
                            tableaux_extend_syntax_tree(t, s, ST_LEFT, l, n_left);

                            n_right = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                            n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NEXT, el->expr), el);
                            tableaux_extend_syntax_tree(t, s, ST_RIGHT, l, n_right);
                        } else {
                            n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                            n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_tail(t, el->expr), el);
                            n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_cons(t, CTL_NEXT, el->expr), el);
                            tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_IDENTITY, n_left);
                        }

                        } break;
                    case CTL_TRUE:
                    case CTL_FALSE:
                    case CTL_EQ: {
                        /* The two cases
                         *       A(PHI, Y)         E(PHI, Y)
                         *  \/  -----------   /\  -----------
                         *       A(PHI)  Y        E(PHI)   Y
                         */
                        syntax_tree_label_t l = s->node->quantifier == NODE_ALL ? TABLEAUX_OR : TABLEAUX_AND;

                        n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                        tableaux_extend_syntax_tree(t, s, ST_LEFT, l, n_left);

                        n_right = tableaux_node_add_expr(NODE_NONE, n_right, el->expr, el);
                        tableaux_extend_syntax_tree(t, s, ST_RIGHT, l, n_right);
                        } break;
                    case CTL_UNTIL: {
                        /* The two cases
                         *                  A(PHI, phi U psi)
                         *  /\  ---------------------------------------------
                         *       A(PHI, psi, phi)  A(PHI, psi, X (phi U psi) )
                         *
                         *                  E(PHI, phi U psi)
                         *  \/  ---------------------------------------------
                         *       E(PHI, psi)       E(PHI, phi, X (phi U psi) )
                         */
                        switch (s->node->quantifier) {
                            case NODE_ALL: {
                                n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t, el->expr->arg2), el);
                                n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t, el->expr->arg1), el);
                                tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_AND, n_left);

                                n_right = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr(t, el->expr->arg2), el);
                                n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NEXT, el->expr), el);
                                tableaux_extend_syntax_tree(t, s, ST_RIGHT, TABLEAUX_AND, n_right);
                                } break;
                            case NODE_EXIST: {
                                n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t, el->expr->arg2), el);
                                tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_OR, n_left);

                                n_right = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr(t, el->expr->arg1), el);
                                n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NEXT, el->expr), el);
                                tableaux_extend_syntax_tree(t, s, ST_RIGHT, TABLEAUX_OR, n_right);
                                } break;
                            case NODE_NONE:;
                        }
                        } break;
                    case CTL_NOT: {
                        // for not, check two tokens
                        switch(el->expr->arg1->token)
                        {
                            case CTL_NOT: {
                                /* The two cases
                                 *       A(PHI, !!Y)       E(PHI, !!Y)
                                 *   I  -------------   I -------------
                                 *       A(PHI)    Y        E(PHI)   Y
                                 */
                                n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_tail(t, el->expr->arg1), el);

                                tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_IDENTITY, n_left);
                                } break;

                            case CTL_FALSE:
                            case CTL_TRUE:
                            case CTL_EQ: {
                                // * same as CTL_EQ */

                                /* The two cases
                                 *       A(PHI, !Y)         E(PHI, !Y)
                                 *  \/  ------------   /\  ------------
                                 *       A(PHI)  !Y         E(PHI)  !Y
                                 */
                                syntax_tree_label_t l = s->node->quantifier == NODE_ALL ? TABLEAUX_OR : TABLEAUX_AND;

                                n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                tableaux_extend_syntax_tree(t, s, ST_LEFT, l, n_left);

                                n_right = tableaux_node_add_expr(NODE_NONE, n_right, el->expr, el);
                                tableaux_extend_syntax_tree(t, s, ST_RIGHT, l, n_right);
                                } break;
                            case CTL_UNTIL: {
                                /* Dual of CTL_UNTIL */

                                /* The two cases
                                 *                  A(PHI, !(phi U psi))
                                 *  /\  -------------------------------------------------
                                 *       A(PHI, !psi)      A(PHI, !phi, X (!(phi U psi)) )
                                 *
                                 *                  E(PHI, !(phi U psi))
                                 *  \/  ---------------------------------------------------
                                 *       E(PHI, !psi, !phi)  E(PHI, !psi, X(!(phi U psi)) )
                                 */
                                switch (s->node->quantifier) {
                                    case NODE_ALL: {
                                        n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                        n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_cons(t, CTL_NOT, el->expr->arg1->arg2), el);
                                        tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_AND, n_left);

                                        n_right = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                        n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NOT, el->expr->arg1->arg1), el);
                                        n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NEXT, el->expr), el);
                                        tableaux_extend_syntax_tree(t, s, ST_RIGHT, TABLEAUX_AND, n_right);
                                        } break;
                                    case NODE_EXIST: {
                                        n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                        n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_cons(t, CTL_NOT, el->expr->arg1->arg2), el);
                                        n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr_unary_cons(t, CTL_NOT, el->expr->arg1->arg1), el);
                                        tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_OR, n_left);

                                        n_right = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                                        n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NOT, el->expr->arg1->arg2), el);
                                        n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr_unary_cons(t, CTL_NEXT, el->expr), el);
                                        tableaux_extend_syntax_tree(t, s, ST_RIGHT, TABLEAUX_OR, n_right);
                                        } break;
                                    case NODE_NONE:;
                                }
                                } break;
                            default:
                                // what about ! A (..) and ! E( ..) -> should possibly use tableaux_not?
                                Abort("unhandled in conversion not expr");
                        }

                        } break;
                    case CTL_OR:
                    case CTL_AND: {
                        /* The four cases
                         *        A(PHI, phi /\ psi)           A(PHI, phi \/ psi)
                         * /\  -----------------------     I  --------------------
                         *     A(PHI, phi) A(PHI, psi)          A(PHI, phi, psy)
                         *
                         *        E(PHI, phi \/ psi)           E(PHI, phi /\ psi)
                         * \/  -----------------------     I  --------------------
                         *     E(PHI, phi) E(PHI, psi)          E(PHI, phi, psy)
                         *
                         * Thus:
                         *           split                         identity
                         *  ^ label  =    ctl ^ label
                         */
                        int split = ((s->node->quantifier == NODE_ALL && el->expr->token == CTL_AND ) ||
                                     (s->node->quantifier == NODE_EXIST && el->expr->token == CTL_OR) );
                        syntax_tree_label_t l = el->expr->token == CTL_AND ? TABLEAUX_AND : TABLEAUX_OR;

                        if (split) {
                            n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                            n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t, el->expr->arg1), el);
                            tableaux_extend_syntax_tree(t, s, ST_LEFT, l, n_left);

                            n_right = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                            n_right = tableaux_node_add_expr(s->node->quantifier, n_right, tableaux_expr(t, el->expr->arg2), el);
                            tableaux_extend_syntax_tree(t, s, ST_RIGHT, l, n_right);
                        } else {
                            n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                            n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t, el->expr->arg1), el);
                            n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t, el->expr->arg2), el);

                            tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_IDENTITY, n_left);
                        }
                        } break;
                    case CTL_EXIST:
                    case CTL_ALL: {
                        /* The four cases
                         *      A(PHI, A(PSI))         A(PHI, E(PSI))
                         * \/  ---------------   \/  ----------------
                         *      A(PHI) A(PSI)          A(PHI) E(PSI)
                         *
                         *      E(PHI, A(PSI))         E(PHI, E(PSI))
                         * /\  ---------------   /\  ----------------
                         *      E(PHI) A(PSI)          E(PHI) E(PSI)
                         */
                        tableaux_node_quantifier_t expr_quantifier = el->expr->token == CTL_ALL ? NODE_ALL : NODE_EXIST;
                        syntax_tree_label_t l = s->node->quantifier == NODE_ALL ? TABLEAUX_OR : TABLEAUX_AND;

                        n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                        tableaux_extend_syntax_tree(t, s, ST_LEFT, l, n_left);
                        n_right = tableaux_node_add_expr(expr_quantifier, n_right, tableaux_expr_unary_tail(t, el->expr), el);
                        tableaux_extend_syntax_tree(t, s, ST_RIGHT, l, n_right);
                        } break;
                    case CTL_IMPLY:
                        /* convert phi -> psi into !phi | psi */
                        n_left = tableaux_node_delta_phi(s->node->quantifier, s->node, el);
                        // split imply expression
                        ltsmin_expr_t e = LTSminExpr(BINARY_OP, CTL_OR, 0,
                            LTSminExpr(UNARY_OP, CTL_NOT, 0, el->expr->arg1, NULL),
                            el->expr->arg2);
                        n_left = tableaux_node_add_expr(s->node->quantifier, n_left, tableaux_expr(t,e), el);
                        tableaux_extend_syntax_tree(t, s, ST_LEFT, TABLEAUX_IDENTITY, n_left);
                        // free created expression
                        RTfree(e->arg1);
                        RTfree(e);

                        break;
                    default:
                        Abort("unhandled case in tableaux apply_rule");
                }
                return;
            }

            } break;
        case NODE_NONE:
            s->label = TABLEAUX_TERMINAL;
            break;
    }
}

/* for debugging only: print the syntax tree */

static array_manager_t line_man=NULL;
static char** tableaux_lines;
static int max_level = 0;

#define LMRLINE 4096
static char* tableaux_label[] = {"  ", "  ", "  ", " !", "/\\","\\/","AX","EX"};
// pstart initially specifies minimal left position of the parent
// after function call it is the start position of the drawn child
#ifndef MAX
#define MAX(a,b) ((a) < (b) ? (b) : (a))
#endif
void tableaux_print_compressed(syntax_tree_t *s, int level, int* pstart, int* pstop)
{
    // reserve two line buffers
    ensure_access(line_man, level+1);
    if (max_level<level) max_level = level+1;
    if (tableaux_lines[level] == NULL)
        tableaux_lines[level] = RTmallocZero(LMRLINE);
    if (tableaux_lines[level+1] == NULL)
        tableaux_lines[level+1] = RTmallocZero(LMRLINE);

    // plot node
    char* line = tableaux_lines[level];
    int line_len = strlen(line);

    // print node
    char node_str[4096];
    tableaux_node_print(s->node, node_str);
    int  node_len  = strlen(node_str);

    // copy start stop
    int  start = *pstart;
    int  between = *pstart+node_len;
    int  stop = between;

    // move start/stop
    if (line_len > (start - 6)) {
        line_len += (line_len-(start-6)<6)?line_len-(start-6):6; // min(6, line_len-(start-6)); // correct?
        stop += (line_len - start);
        start = line_len; // += (line_len - start);
    }

    // print left branch
    if (s->branch[ST_LEFT] != NULL) {
        tableaux_print_compressed(s->branch[ST_LEFT], level+2, &start, &between);

        // assumption right branch is only set when left branch != NULL
        // fix truncated stop value
        stop = MAX(start + node_len, between);

        // print right branch
        if (s->branch[ST_RIGHT] != NULL) {
            // add some extra spacing between the nodes for readability
            between+=6; stop +=6;
            // print the node
            tableaux_print_compressed(s->branch[ST_RIGHT], level+2, &between, &stop);
        }
        //Warning(info, "after left/right start %d between %d stop %d", start, between, stop);
    }

    // at this point stop - start should be >= node_len
    // furthermore, start is fixed after this

    //Warning(info, "after node start %d stop %d", start, stop);
    if (stop - start < node_len) {
        //Abort("failed node_len assumption");
        // hmmz, the right child should be aligned to right instead..
        stop = start + node_len;
    }

    // need more padding
    if (line_len <= start) {
        sprintf(line, "%-*s", start, line);
        line_len = start;
    }

    // print node, centered
    int padding_left = 0;
    int padding_right = 0;
    int padding_len = stop - start;
    if (node_len < padding_len) {
        int padding = padding_len - node_len;
        padding_left  = padding / 2;
        padding_right = padding - padding_left;
    }
    sprintf(line+line_len, "%*s%s", padding_left, "", node_str);

    // print vertical line (start -- stop)
    // except below a terminal
    if (s->branch[ST_LEFT] != NULL) {
        sprintf(tableaux_lines[level+1], "%-*s", start, tableaux_lines[level+1]);
        sprintf(tableaux_lines[level+1]+start-3, "%s ", tableaux_label[s->label]);
        memset(tableaux_lines[level+1]+start, '-', stop-start);
    }

    // fix parents start/stop
    *pstart = start + padding_left;
    *pstop =  stop - padding_right;
}

/* print the tableux */
void tableaux_print(tableaux_t *t)
{
    max_level = 0;
    line_man = create_manager(65536);
    ADD_ARRAY(line_man, tableaux_lines, char*);
    int start = 6;
    int stop = start;
    tableaux_print_compressed(&t->root, 0, &start, &stop);
    for(int i=0; i <= max_level; i++) {
        Warning(info, "%s", tableaux_lines[i]);
    }
    destroy_manager(line_man);
}

/* count the variables in some expression in the tableaux */
/* counting is used to prevent clashes between variables assigned to mu/nu operators */
void tableaux_count_vars_e(tableaux_t *t, ltsmin_expr_t e)
{
    // find expression terminals
    if (e->arg1 != NULL) tableaux_count_vars_e(t, e->arg1);
    if (e->arg2 != NULL) tableaux_count_vars_e(t, e->arg2);
    if (e->arg1 == NULL && e->arg2 == NULL && e->token == CTL_VAR) {
        t->used_var = MAX(t->used_var, e->idx + 1);
    }
}
/* count the variables in some syntax tree */
/* counting is used to prevent clashes between variables assigned to mu/nu operators */
void tableaux_count_vars_s(tableaux_t *t, syntax_tree_t *s)
{
    // find tableaux terminals
    if (s->branch[ST_LEFT]) tableaux_count_vars_s(t,s->branch[ST_LEFT]);
    if (s->branch[ST_RIGHT]) tableaux_count_vars_s(t,s->branch[ST_RIGHT]);
    // look for use of token = MU_VAR in equation
    if (!s->branch[ST_LEFT] && !s->branch[ST_RIGHT]) {
        tableaux_expr_list_t *el = s->node->expr_list;
        while(el) {
            tableaux_count_vars_e(t, el->expr);
            el = el->next;
        }

    }
}

/* apply rule to the root node, continue until terminal node is encountered */
void tableaux_build_syntax_tree(tableaux_t *t, syntax_tree_t* s)
{
    if (s->label == TABLEAUX_PRETERMINAL) return;
    if (s->label == TABLEAUX_IDENTITY && s->branch[ST_LEFT] == NULL && s->branch[ST_RIGHT] == NULL) {
        // this should destroy the syntax_tree node, and set the TABLEAUX_IDENTITY flag if the parent
        // this can probably only happen if MU_TRUE and MU_FALSE are converted to NULL nodes for the
        // tableaux. For simplicity, just let them in the formula, and simplify afterwards.
        Abort("TABLAUX_IDENTITY empty on both sides!");
    } else {
        tableaux_apply_rule(t, s);
        if (s->branch[ST_LEFT])  tableaux_build_syntax_tree(t, s->branch[ST_LEFT]);
        if (s->branch[ST_RIGHT]) tableaux_build_syntax_tree(t, s->branch[ST_RIGHT]);
    }

    // count the variables already used, set var to maximum
    if (!s->parent) tableaux_count_vars_s(t, s);
}

/* return the dual of a ctl token */
int ctl_star_dual(int token) {
    switch(token) {
        case CTL_ALL: return CTL_EXIST;
        case CTL_EXIST: return CTL_ALL;
        case CTL_FUTURE: return CTL_GLOBALLY;
        case CTL_GLOBALLY: return CTL_FUTURE;
        case CTL_AND: return CTL_OR;
        case CTL_OR: return CTL_AND;
        case CTL_NEXT: return CTL_NEXT;
        // case CTL_UNTIL: return CTL_RELEASE;
        // case CTL_WEAK_UNTIL: return CTL_STRONG_RELEASE;
    }
    Abort("invalid dual");
    return token;
}

/* convert ctl* to positive normal form */
/* De Morgan:
 * ! (p | q ) = !p & !q
 * ! (q & q ) = !p | !q
 * ! A p = E ! p
 * ! E p = A ! p
 * ! X p = X ! p
 * ! F p = G ! p
 * ! G p = F ! p
 */
ltsmin_expr_t ctl_star_to_pnf_1(ltsmin_expr_t e, int negated)
{
    if (e == NULL) return NULL;
    ltsmin_expr_t res = NULL;
    switch (e->token) {
        case CTL_UNTIL:
            // since the tableaux rules don't yet have a rule for release,
            // encode release using until rule:, thus, don't touch this negation
            // p V q = ! (!p U !q) (dual of until)
            // ! (p U q) = (!p V !q)
            // ! (p V q) = (!p U !q)
            // thus
            // ! (p U q) = (!p V !q), = ! (!!p U !!q) = !(p U q) :)
            if (negated) {
                res = LTSminExpr(e->node_type, e->token, e->idx,
                        ctl_star_to_pnf_1(e->arg1, !negated), ctl_star_to_pnf_1(e->arg2, !negated));
                res = LTSminExpr(UNARY_OP, CTL_NOT, 0, res, NULL);
            } else {
                res = LTSminExpr(e->node_type, e->token, e->idx,
                        ctl_star_to_pnf_1(e->arg1, negated), ctl_star_to_pnf_1(e->arg2, negated));
            }
            break;
        case CTL_NOT:
            res = ctl_star_to_pnf_1(e->arg1, !negated);
            break;
        case CTL_FALSE:
            if (negated) {
                res = LTSminExpr(CONSTANT, CTL_TRUE, 0, NULL, NULL);
            } else {
                res = LTSminExpr(CONSTANT, CTL_FALSE, 0, NULL, NULL);
            }
            break;
        case CTL_TRUE:
            if (negated) {
                res = LTSminExpr(CONSTANT, CTL_FALSE, 0, NULL, NULL);
            } else {
                res = LTSminExpr(CONSTANT, CTL_TRUE, 0, NULL, NULL);
            }
            break;
        case CTL_EVAR:
        case CTL_SVAR: case CTL_EQ: case CTL_VAR:
            // copy, if (negated), add CTL_NOT
            res = LTSminExprClone(e);
            if (negated)
                res = LTSminExpr(UNARY_OP, CTL_NOT, 0, res, NULL);
            break;
        default:
            res = LTSminExpr(e->node_type, negated? ctl_star_dual(e->token) : e->token, e->idx,
                        ctl_star_to_pnf_1(e->arg1, negated), ctl_star_to_pnf_1(e->arg2, negated));
    }
    return res;
}

/* convert ctl* formula to positive normal form */
ltsmin_expr_t ctl_star_to_pnf(ltsmin_expr_t e)
{
    return ctl_star_to_pnf_1(e, 0);
}

/* build the initial tableaux node (root node) */
tableaux_node_t* build_initial_node(tableaux_t *t, ltsmin_expr_t e)
{
    ltsmin_expr_t e_pnf = ctl_star_to_pnf(e);

    tableaux_node_quantifier_t q = NODE_NONE;

    if (e_pnf->token == CTL_ALL || e_pnf->token == CTL_EXIST) {
        // get tail
        q = (e_pnf->token == CTL_ALL) ? NODE_ALL : NODE_EXIST;
        e = tableaux_expr_unary_tail(t, e_pnf);
    } else {
        // add expression
        e = tableaux_expr(t, e_pnf);
    }
    // build node
    tableaux_node_t *n = tableaux_node_add_expr(q, NULL, e, NULL);
    // add node
    tableaux_table_add(&t->nodes, n->hash, n);
    // free pnf expression
    LTSminExprDestroy(e_pnf);
    // return node
    return n;
}

/* print a mu expression */
char* ltsmin_expr_print_mu(ltsmin_expr_t mu, char* buf)
{
    // no equation
    if (!mu) return buf;

    // left eq
    switch(mu->node_type) {
        case BINARY_OP:
            *buf++ = '(';
            buf = ltsmin_expr_print_mu(mu->arg1, buf);
        default:;
    }
    // middle
    switch(mu->token) {
        case MU_SVAR: sprintf(buf, "@S%d", mu->idx); break;
        case MU_EVAR: sprintf(buf, "@E%d", mu->idx); break;
        case MU_NUM: sprintf(buf, "%d", mu->idx); break;
        case MU_VAR:
            if (-1 == mu->num)
                sprintf(buf, "@V%d", mu->idx);
            else
                sprintf(buf, "@C%d", mu->num);
            break;
        case MU_CHUNK: sprintf(buf, "@H%d", mu->idx); break;
        case MU_EQ: sprintf(buf, " == "); break;
        case MU_TRUE: sprintf(buf, "true"); break;
        case MU_OR: sprintf(buf, " \\/ "); break;
        case MU_NOT: sprintf(buf, "!"); break;
        case MU_NEXT: sprintf(buf, "X "); break;
        case MU_FALSE: sprintf(buf, "false"); break;
        case MU_AND: sprintf(buf, " /\\ "); break;
        //case MU_EQUIV: sprintf(buf, " <-> "); break;
        //case MU_IMPLY: sprintf(buf, " -> "); break;
        case MU_EXIST: sprintf(buf, "E "); break;
        case MU_ALL: sprintf(buf, "A "); break;
        case MU_MU:
            sprintf(buf, "(mu @V%d", mu->idx); buf += strlen(buf);
            sprintf(buf, ". "); buf += strlen(buf);
            buf = ltsmin_expr_print_mu(mu->arg1, buf);
            sprintf(buf, ")");
            break;
        case MU_NU:
            sprintf(buf, "(nu @V%d", mu->idx); buf += strlen(buf);
            sprintf(buf, ". "); buf += strlen(buf);
            buf = ltsmin_expr_print_mu(mu->arg1, buf);
            sprintf(buf, ")");
            break;
        case MU_EDGE_EXIST:
            sprintf(buf, "<%s> ", "?"); break; // TODO
        case MU_EDGE_ALL:
            sprintf(buf, "[%s] ", "?"); break; // TODO
        default:
            Abort("unknown MU token");
    }
    buf += strlen(buf);
    // right eq
    switch(mu->node_type) {
        case UNARY_OP:
            buf = ltsmin_expr_print_mu(mu->arg1, buf);
            break;
        case BINARY_OP:
            buf = ltsmin_expr_print_mu(mu->arg2, buf);
            *buf++ = ')';
            *buf = 0;
            break;
        default:;
    }
    *buf='\0';
    return buf;
}


/* convert CTL tokens to MU tokens */
ltsmin_expr_t ctl_star_to_mu_1(ltsmin_expr_t in)
{
    ltsmin_expr_t res = RT_NEW(struct ltsmin_expr_s);
    memcpy(res, in, sizeof(struct ltsmin_expr_s));
    switch (in->token) {
        case CTL_SVAR:      res->token = MU_SVAR;      break;
        case CTL_EVAR:      res->token = MU_EVAR;      break;
        case CTL_NUM:       res->token = MU_NUM;       break;
        case CTL_CHUNK:     res->token = MU_CHUNK;     break;
        case CTL_VAR:       res->token = MU_VAR;       break;
        case CTL_EQ:        res->token = MU_EQ;        break;
        case CTL_TRUE:      res->token = MU_TRUE;      break;
        case CTL_FALSE:     res->token = MU_FALSE;     break;
        case CTL_OR:        res->token = MU_OR;        break;
        case CTL_AND:       res->token = MU_AND;       break;
        case CTL_NOT:       res->token = MU_NOT;       break;
        case CTL_NEXT:      res->token = MU_NEXT;      break;
        case CTL_ALL:       res->token = MU_ALL;       break;
        case CTL_EXIST:     res->token = MU_EXIST;     break;
        default:
            // unhandled?
            Abort("unhandled case in ctl_star_to_mu_1");
    }
    // handle sub-expressions
    switch (in->node_type) {
        case UNARY_OP:
            res->arg1 = ctl_star_to_mu_1(res->arg1);
            break;
        case BINARY_OP:
            res->arg1 = ctl_star_to_mu_1(res->arg1);
            res->arg2 = ctl_star_to_mu_1(res->arg2);
            break;
        default:
            break;
    }
    LTSminExprRehash(res);
    return res;
}

/* decide whether an expression is a mu/nu path-expression */
int is_path_expr(tableaux_path_t t, ltsmin_expr_t e)
{
    // path is in the form L(phi_1, phi2) or XL(phi_1, phi2)
    // with L = U for mu paths and L=!U for nu-paths
    if (e->token == CTL_NEXT) e = e->arg1;

    switch(t) {
        case MU_PATH:
            switch(e->token) {
                case CTL_FUTURE:
                case CTL_UNTIL:
                    return 1;
                case CTL_NOT:
                    // !G
                    return (e->arg1->token == CTL_GLOBALLY);
            }
            break;
        case NU_PATH:
            switch(e->token) {
                case CTL_GLOBALLY:
                   return 1;
                case CTL_NOT:
                    // !U or !F
                    return (e->arg1->token == CTL_UNTIL ||
                            e->arg1->token == CTL_FUTURE);
            }
            break;
    }
    return 0;
}

/* find a path to the companion (via parent relation)
 * then, decide whether there exist a mu/nu path from the companion to
 * to node on the way back
 * this is done by setting the in_path flag in the expression_list
 */
int find_path(tableaux_path_t t, syntax_tree_t* s, syntax_tree_t* companion)
{
    // clear is_path flag in nodes
    tableaux_expr_list_t* clear = s->node->expr_list;
    while(clear) {
        clear->in_path = 0;
        clear = clear->next;
    }

    if (s == companion) {
        // find the path
        companion->S_phi->in_path = is_path_expr(t, companion->S_phi->expr);
        return companion->S_phi->in_path;
    } else {
        int res = 0;
        if ((res = find_path(t, s->parent, companion))) {
            res = 0;
            tableaux_expr_list_t *el = s->node->expr_list;
            while(el) {
                el->in_path = res =
                    ((el->generating_expr[0] && el->generating_expr[0]->in_path) ||
                     (el->generating_expr[1] && el->generating_expr[1]->in_path) );
                el = el->next;
            }
        }
        return res;
    }
}

/* translate the syntax tree into a mu formula */
ltsmin_expr_t tableaux_translate_syntax_tree(tableaux_t* t, syntax_tree_t *s)
{
    ltsmin_expr_t res = NULL;
    int connect_op = 0;
    int fixpoint_op1 = 0;
    int fixpoint_op2 = 0;

    switch(s->node->quantifier)
    {
        case NODE_EXIST:
            connect_op = MU_AND;
            fixpoint_op1 = MU_NU;
            fixpoint_op2 = MU_MU;
            break;
        case NODE_ALL:
            connect_op = MU_OR;
            fixpoint_op1 = MU_MU;
            fixpoint_op2 = MU_NU;
            break;

        case NODE_NONE:
            return ctl_star_to_mu_1(s->node->expr_list->expr);
    }
    // for NODE_ALL/NODE_EXIST
    ltsmin_expr_t mu_left = NULL;
    ltsmin_expr_t mu_right = NULL;


    // handle companion/non-terminal nodes
    if (s->is_companion) {
        ltsmin_expr_t mu_nu_list = NULL;

        tableaux_expr_list_t *el = s->node->expr_list;

        // fixed variables
        s->path_var[0] = t->used_var++;

        while(el) {
            ltsmin_expr_t el_expr = NULL;

            // new path variables
            s->path_var[1] = t->used_var++;
            s->S_phi = el;

            // translate everything below
            if (s->branch[ST_LEFT]) {
                mu_left = tableaux_translate_syntax_tree(t, s->branch[ST_LEFT]);
            }

            if (s->branch[ST_RIGHT]) {
                mu_right = tableaux_translate_syntax_tree(t, s->branch[ST_RIGHT]);
            }
            // end nu
            switch(s->label) {
                case TABLEAUX_TERMINAL:
                    // error:
                    // it isn't a terminal node if is_companion is set here
                    Abort("encountered terminal with is_companion set");
                    break;
                case TABLEAUX_PRETERMINAL:
                    // error:
                    // it isn't a terminal node if is_companion is set here
                    Abort("encountered preterminal with is_companion set");
                    break;
                case TABLEAUX_IDENTITY:
                    el_expr = mu_left;
                    break;
                case TABLEAUX_NOT: break; // UNUSED
                case TABLEAUX_AND:
                    el_expr = LTSminExpr(BINARY_OP, MU_AND, 0, mu_left, mu_right);
                    break;
                case TABLEAUX_OR:
                    el_expr = LTSminExpr(BINARY_OP, MU_OR, 0, mu_left, mu_right);
                    break;
                case TABLEAUX_ALL_NEXT:
                    el_expr = LTSminExpr(UNARY_OP, MU_ALL, 0, LTSminExpr(UNARY_OP, MU_NEXT, 0, mu_left, mu_right), NULL);
                    break;
                case TABLEAUX_EXIST_NEXT:
                    el_expr = LTSminExpr(UNARY_OP, MU_EXIST, 0, LTSminExpr(UNARY_OP, MU_NEXT, 0, mu_left, mu_right), NULL);
                    break;
            }

            // build nu
            ltsmin_expr_t nu = LTSminExpr(fixpoint_op2, fixpoint_op2, s->path_var[1], el_expr, NULL);

            // build nu list
            if (s->node->expr_list != el) {
                mu_nu_list = LTSminExpr(BINARY_OP, connect_op, 0, mu_nu_list, nu);
            } else {
                mu_nu_list = nu;
            }
            el = el->next;
        }

        res = LTSminExpr(fixpoint_op1, fixpoint_op1, s->path_var[0], mu_nu_list, NULL);
    } else {
        // translate everything below
        if (s->branch[ST_LEFT]) {
            mu_left = tableaux_translate_syntax_tree(t, s->branch[ST_LEFT]);
        }

        if (s->branch[ST_RIGHT]) {
            mu_right = tableaux_translate_syntax_tree(t, s->branch[ST_RIGHT]);
        }

        // end nu
        switch(s->label) {
            case TABLEAUX_TERMINAL:
                // is this correct?
                res = ctl_star_to_mu_1(s->node->expr_list->expr); // 1 expression max?
                break;
            case TABLEAUX_PRETERMINAL:
                {
                    int path_var_idx = 0;

                    char buf[4096];
                    char buf1[4096];
                    tableaux_node_print(s->node, buf);
                    tableaux_node_print(s->companion->node, buf1);
                    Warning(info, "encountered preterminal %s with companion %s", buf, buf1);
                    ltsmin_expr_print_ctl(s->companion->S_phi->expr, buf);
                    if (s->node->quantifier == NODE_ALL) {
                        path_var_idx = find_path(NU_PATH, s, s->companion);
                        Warning(info, " nu-path exist from %s to %s? %s", buf, buf, path_var_idx ? "yes" : "no");
                    } else {
                        path_var_idx = find_path(MU_PATH, s, s->companion);
                        Warning(info, " mu-path exist from %s to %s? %s", buf, buf, path_var_idx ? "yes" : "no");
                    }

                    res = LTSminExpr((ltsmin_expr_case)MU_VAR, MU_VAR, s->companion->path_var[path_var_idx], mu_left, mu_right);
                }
                break;
            case TABLEAUX_IDENTITY:
                res = mu_left;
                break;
            case TABLEAUX_NOT: break; // UNUSED
            case TABLEAUX_AND:
                res = LTSminExpr(BINARY_OP, MU_AND, 0, mu_left, mu_right);
                break;
            case TABLEAUX_OR:
                res = LTSminExpr(BINARY_OP, MU_OR, 0, mu_left, mu_right);
                break;
            case TABLEAUX_ALL_NEXT:
                res = LTSminExpr(UNARY_OP, MU_ALL, 0, LTSminExpr(UNARY_OP, MU_NEXT, 0, mu_left, mu_right), NULL);
                break;
            case TABLEAUX_EXIST_NEXT:
                res = LTSminExpr(UNARY_OP, MU_EXIST, 0, LTSminExpr(UNARY_OP, MU_NEXT, 0, mu_left, mu_right), NULL);
                break;
        }
    }
    return res;
}

/* convert ctl* formula to mu calculus */
ltsmin_expr_t ctl_star_to_mu(ltsmin_expr_t in)
{
    char buf[4096];
    tableaux_t *t = tableaux_create();

    // step 1: convert ctl_star formula to tableaux start node
    Warning(info, "-- conversion --");
    //tableaux_node_t *n = build_test_node(t);
    tableaux_node_t *n = build_initial_node(t,in);
    t->root.node = n;

    // step 2: calculate the tableaux
    tableaux_build_syntax_tree(t, &t->root);

    // debug print this
    tableaux_print(t);

    // step 3: translate the tableaux
    ltsmin_expr_t mu = tableaux_translate_syntax_tree(t, &t->root);
    ltsmin_expr_print_mu(mu, buf);
    Warning(info, "** %s **", buf);

    tableaux_destroy(t);
    Warning(info, "-- end conversion --");

    return mu;
}

/* for debugging */
ltsmin_expr_t print_expr_1(ltsmin_expr_t in, int indent)
{
    for(int i=0; i < indent; i++) {
        printf("\t");
    }
    printf("[%d/%d/%x]\n", in->token, in->idx, in->hash);
    if (in->arg1) print_expr_1(in->arg1, indent+1);
    if (in->arg2) print_expr_1(in->arg2, indent+1);
    printf("\n");
    return in;
}

ltsmin_expr_t print_expr(ltsmin_expr_t in)
{
    print_expr_1(in, 0);
    return in;
}

/* Comments:
 * De-Morgan for mu-/nu-formula:
 * ! (mu Y . p(Y) ) = (nu Y . !p(!Y) )
 * ! (nu Y . p(Y) ) = (mu Y . !p(!Y) )
 */
