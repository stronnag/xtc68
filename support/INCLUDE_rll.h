#ifndef _RLL_H
#define _RLL_H
#ifndef _SYS_TYPES
#include <sys/types.h>
#endif
#ifndef _THINGS_H
#include <things.h>
#endif
typedef struct {
char bss_iden[8];
long bss_xdef;
long bss_rlib;
long bss_xref;
long bss_reloc;
long bss_udata;
long bss_usize;
} BSS_HEADER;
#define BSSFLAG"<<BSS>>"
typedef struct {
char xdef_iden[8];
struct {
long offset;
char symlen;
char symname[1];
} xdef[1];
} BSS_XDEF;
#define BSS_XDEFFLAG"<<XDEF>>"
#define BSS_XDEF_END 0x0
typedef struct {
char rlib_iden[8];
short rlib_count;
struct {
char version[4];
char name[16];
} rlib[1];
} BSS_RLIB;
#define BSS_RLIBFLAG"<<RLIB>>"
typedef struct {
char xref_iden[8];
struct {
short offset;
char status;
char symlen;
char symname[1];
} xref[1];
} BSS_XREF;
#define BSS_XREF_FLAG"<<XREF>>"
#define BSS_XREF_SKIP 0xfffe
#define BSS_XREF_END_ENTRY 0x0000
#define BSS_XREF_END 0x00000000
typedef struct {
char reloc_iden[10];
long reloc_init;
unsigned char reloc_next[1];
} BSS_RELOC;
#define BSS_RELOCFLAG"<<RELOC>>"
#define BSS_RELOC_END 0x00
#define BSS_RELOC_SKIP 0xff
typedef struct {
long reloc_init;
unsigned char reloc_next[1];
} BSS_RELOC_LD1;
typedef struct {
long reloc_offset[1];
} BSS_RELOC_GST;
#define BSS_RELOC_GST_END 0x00
typedef struct {
THING_LINKAGE linkage;
char th_filler[15];
THING_HEADER header;
char rll_iden[8];
BSS_HEADER * rll_bss;
int (*rll_loadinit)();
int (*rll_linkinit)();
long rll_fwdl;
long rll_revl;
short rll_time;
short rll_cnt;
long rll_jbst;
} RLL_HEADER;
#define RLLFLAG"THG%"
#define RLLTYPE 0
#define RLLIDEN"<<RLL>>"
#define RLM_FLAG"%RLM"
typedef struct {
long flag;
long type;
long base;
long call;
} RLM_HEADER;
typedef struct {
unsigned char dbgdir[34];
unsigned char libdir[34];
short timeout;
short debug;
short mode;
} RLM_READVALUES;
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
int RLM_LinkCode _P_((char *,BSS_HEADER *,jobid_t,chanid_t));
int RLM_LinkRLL _P_((char *,char *,BSS_XREF *,jobid_t,chanid_t));
int RLM_UnlinkRLL _P_((char *,char *,BSS_XREF *,chanid_t));
int RLM_LoadLib _P_((char *,timeout_t,chanid_t));
int RLM_SetTimeout _P_((short));
int RLM_SetLoadMode _P_((short));
int RLM_SetLoadDir _P_((char *));
int RLM_SetDebugDir _P_((char *));
int RLM_SetDebugMode _P_((short));
int RLM_ReadSettings _P_((RLM_READVALUES *));
int RLM_RelocLD _P_((char *,BSS_HEADER *));
int RLM_RelocGST _P_((char *,long *));
int RLM_RelocLD_Old _P_((char *,void *));
#undef _P_
#endif
