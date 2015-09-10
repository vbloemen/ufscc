// -*- tab-width:4 ; indent-tabs-mode:nil -*-
#include <HsFFI.h>
#include <LibScoop_stub.h>
//FIXME: #include <scoop.h>

#ifdef __GLASGOW_HASKELL__
extern void __stginit_LibScoop(void);
#endif
#include <stdio.h>

static HsStablePtr constants;

void scoop_put_constant(const char*var,const char*val){
    constants=const_put(constants,(char*)var,(char*)val);
}

void scoop_init(int argc, char *argv[])
{
    hs_init(&argc, &argv);
#ifdef __GLASGOW_HASKELL__
    hs_add_root(__stginit_LibScoop);
#endif
    constants=const_empty();
}


void scoop_fini(){
    hs_exit();
}

void* scoop_load_mapa(const char* name){
    return load_mapa((char*)name,constants);
}
void* scoop_load_prcrl(const char* name){
    return load_prcrl((char*)name,constants);
}

static int seq_no=0;

int get_sequence_no(){
//    Warning(info,"sequence number %d",seq_no);
    return ++seq_no;
}

