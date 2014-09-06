#ifndef _THINGS_H
#define _THINGS_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
typedef struct _thing_linkage {
struct _thing_linkage * th_nxtth;
long th_usage;
char * th_frfre;
char * th_frzap;
char * th_thing;
char * th_use;
char * th_free;
char * th_ffree;
char * th_remov;
char th_nshar;
char th_check;
long th_verid;
short th_name;
char th_name_text[1];
};
typedef struct _thing_linkage THING_LINKAGE;
typedef struct {
char thh_flag[4];
long thh_type;
} THING_HEADER;
#define THH_FLAG"THG%"
#define THT_LST 0x01000000
#define THT_UTIL 0x00000000
#define THT_EXEC 0x00000001
#define THT_DATA 0x00000002
#define THT_EXTN 0x00000003
#define THT_EXTS 0x00000004
typedef struct {
THING_HEADER thh;
char * thh_entr;
char * thh_exec;
} THING_HEADER_SELF;
typedef struct {
THING_HEADER thh;
long thh_next;
long thh_exld;
} THING_HEADER_LIST;
typedef struct {
THING_HEADER thh;
long thh_hdrs;
long thh_hdrl;
long thh_data;
long thh_strt;
} THING_HEADER_EXEC;
typedef struct {
THING_HEADER thh;
long thh_next;
long thh_exld;
long thh_pdef;
long thh_pdes;
char thh_code[1];
} THING_HEADER_EXT;
typedef struct {
short id;
short type;
void *ptr;
short str_len;
} HK_HEAD,*PHK_HEAD;
typedef struct
{
HK_HEAD hd;
char name[1];
} HK_ITEM,*PHK_ITEM;
#define HK_ID 0x6869
#define HKI_LLRC -8
#define HKI_STPR -6
#define HKI_STBF -4
#define HKI_STUF -2
#define HKI_CMD 0
#define HKI_NOP 2
#define HKI_XTHG 4
#define HKI_XFIL 6
#define HKI_PICK 8
#define HKI_WAKE 10
#define HKI_WKXF 12
#define HKI__TRN 0
#define HKI_TRN 1
typedef struct
{
short jsrl;
long gard;
short xo;
short yo;
short xs;
short ys;
short brdr;
short gmem;
short jma6;
} HKH_GUARD,*PHKH_GUARD;
#define HKH_JSRL 0x4eb9
#define HKH_JMPA6 0x4ed6
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
int sms_fthg _P_((char *,jobid_t,long *,long,char *,char **));
int sms_lthg _P_((THING_LINKAGE *));
int sms_nthg _P_((char *,THING_LINKAGE **));
int sms_nthu _P_((char *,THING_LINKAGE **,jobid_t *));
int sms_rthg _P_((char *));
char * sms_uthg _P_((char *,jobid_t,timeout_t,long *,char *,long *,THING_LINKAGE **));
int sms_zthg _P_((char *));
int hk_cjob _P_((void));
int hk_do _P_((HK_ITEM *));
int hk_fitem _P_((char *key_name,HK_ITEM **,short *,short *));
int hk_gtbuf _P_((int,char **));
int hk_kjob _P_((void));
int hk_on _P_((char *));
int hk_off _P_((char *));
int hk_rset _P_((int,char *));
int hk_remov _P_((char *));
int hk_set _P_((int,HK_ITEM *,HKH_GUARD *));
int hk_stbuf _P_((char *));
#ifdef __LIBRARY__
#endif
#endif
