#ifndef __LIB_SCOOP_H
#define __LIB_SCOOP_H

typedef struct scoop_model *scoop_model_t;

extern void scoop_put_constant(const char*var,const char*val);

extern void scoop_init(int argc, char *argv[]);

extern void scoop_fini();

extern char* prcrl_par_name(scoop_model_t model,int idx);

extern char* prcrl_par_type(scoop_model_t model,int idx);

void* scoop_load_mapa(const char* name);

void* scoop_load_prcrl(const char* name);

extern void prcrl_get_state_reward(scoop_model_t model, uint32_t* state, uint32_t* reward);

extern uint32_t prcrl_rewards(scoop_model_t model);

#define HsStablePtr void*
#define HsPtr void*
#define HsInt32 int32_t

extern HsStablePtr get_confluent_summands(HsStablePtr a1);
extern HsStablePtr get_diamond_summands(HsStablePtr a1);
extern HsInt32 empty_conf(HsStablePtr a1);
extern HsInt32 head_conf(HsStablePtr a1);
extern HsStablePtr tail_conf(HsStablePtr a1);
extern HsStablePtr const_empty(void);
extern HsStablePtr const_put(HsStablePtr a1, HsPtr a2, HsPtr a3);
extern HsStablePtr load_prcrl(HsPtr a1, HsStablePtr a2);
extern HsStablePtr load_mapa(HsPtr a1, HsStablePtr a2);
extern HsPtr prcrl_get_action(HsStablePtr a1, HsInt32 a2);
extern void print_prcrl(HsStablePtr a1);
extern HsInt32 prcrl_pars(HsStablePtr a1);
extern HsInt32 prcrl_summands(HsStablePtr a1);
extern HsInt32 prcrl_is_used(HsStablePtr a1, HsInt32 a2, HsInt32 a3);
// extern HsPtr prcrl_par_type(HsStablePtr a1, HsInt32 a2);
extern void prcrl_get_init(HsStablePtr a1, HsPtr a2);
extern HsInt32 prcrl_explore(HsStablePtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
extern HsInt32 prcrl_explore_long(HsStablePtr a1, HsInt32 a2, HsPtr a3, HsPtr a4, HsPtr a5);


#endif

