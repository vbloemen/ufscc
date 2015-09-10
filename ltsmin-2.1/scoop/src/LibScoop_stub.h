#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern void prcrl_get_state_reward(HsStablePtr a1, HsPtr a2, HsPtr a3);
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
extern HsInt32 prcrl_rewards(HsStablePtr a1);
extern HsInt32 prcrl_summands(HsStablePtr a1);
extern HsInt32 prcrl_is_used(HsStablePtr a1, HsInt32 a2, HsInt32 a3);
extern HsPtr prcrl_par_name(HsStablePtr a1, HsInt32 a2);
extern HsPtr prcrl_par_type(HsStablePtr a1, HsInt32 a2);
extern void prcrl_get_init(HsStablePtr a1, HsPtr a2);
extern HsInt32 prcrl_explore(HsStablePtr a1, HsPtr a2, HsPtr a3, HsPtr a4);
extern HsInt32 prcrl_explore_long(HsStablePtr a1, HsInt32 a2, HsPtr a3, HsPtr a4, HsPtr a5);
#ifdef __cplusplus
}
#endif

