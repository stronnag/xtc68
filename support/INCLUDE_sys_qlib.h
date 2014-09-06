#ifndef _QLIB_H
#define _QLIB_H
#ifndef _QDOS_H
#ifndef _SMS_H
#ifndef _QPTR_H
#error This file should only be included indirectly via
#error the <qdos.h>,<sms.h> or <qptr.h> header files.
#endif
#endif
#endif
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#ifndef _LIMITS_H
#include <limits.h>
#endif
#ifndef NULL
#define NULL ((void *)0)
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define TIMEOUT_FOREVER (timeout_t)-1
typedef struct REGS {
datareg_t D0,D1,D2,D3;
addreg_t A0,A1,A2,A3;
} REGS_t;
extern int _oserr;
typedef struct QLSTR {
short qs_strlen;
char qs_str[1];
} QLSTR_t;
#define QLSTR_DEF(n,l) \
struct { \
short qs_strlen; \
char qs_str[l+1]; \
} n
#define QLSTR_INIT(n,s) QLSTR_DEF(n,sizeof(s)) = {sizeof(s)-1,s}
typedef struct QLFLOAT {
short qfp_exp;
long qfp_mant;
} QLFLOAT_t;
typedef struct QLHEAP {
long qh_size;
long qh_freeaddr;
jobid_t qh_job;
long qh_zero;
} QLHEAP_t;
typedef struct MELT {
long mp_size;
struct MELT *mp_next;
} MELT_t;
#define MELTSIZE sizeof(struct MELT)
typedef struct QLVECTABLE {
long (*qv_adderr)_P_((void));
long (*qv_illegal)_P_((void));
long (*qv_divzero)_P_((void));
long (*qv_CHK)_P_((void));
long (*qv_TRAPV)_P_((void));
long (*qv_priviol)_P_((void));
long (*qv_tracexpt)_P_((void));
long (*qv_intlev7)_P_((void));
long (*qv_5trap)_P_((void));
long (*qv_6trap)_P_((void));
long (*qv_7trap)_P_((void));
long (*qv_8trap)_P_((void));
long (*qv_9trap)_P_((void));
long (*qv_10trap)_P_((void));
long (*qv_11trap)_P_((void));
long (*qv_12trap)_P_((void));
long (*qv_13trap)_P_((void));
long (*qv_14trap)_P_((void));
long (*qv_15trap)_P_((void));
} QLVECTABLE_t;
typedef struct FULLREGS {
datareg_t jb_D0,jb_D1,jb_D2,jb_D3,jb_D4,jb_D5,jb_D6,jb_D7;
addreg_t jb_A0,jb_A1,jb_A2,jb_A3,jb_A4,jb_A5,jb_A6,jb_A7;
short jb_SR;
addreg_t jb_PC;
} FULLREGS_t;
typedef struct JOBHEADER {
long jb_len;
long jb_start;
jobid_t jb_owner;
long jb_hold;
unsigned short jb_tag;
unsigned char jb_prior;
unsigned char jb_princ;
short jb_stat;
char jb_rela6;
char jb_wflag;
jobid_t jb_wjob;
QLVECTABLE_t * jb_trapv;
struct FULLREGS jb_regs;
short jb_resvd;
} JOBHEADER_t;
typedef struct QL_LINK {
struct QL_LINK *l_next;
void (*l_rtn)_P_((void));
} QL_LINK_t;
typedef struct QLD_LINK {
struct QLD_LINK *ld_next;
long (*ld_io)_P_((void));
long (*ld_open)_P_((void));
long (*ld_close)_P_((void));
} QLD_LINK_t;
typedef struct QLDDEV_LINK {
struct QLDDEV_LINK *ldd_next;
long (*ldd_io)_P_((void));
long (*ldd_open)_P_((void));
long (*ldd_close)_P_((void));
long (*ldd_slave)_P_((void));
long (*ldd_rename)_P_((void));
long (*ldd_trunc)_P_((void));
long (*ldd_format)_P_((void));
long ldd_plen;
QLSTR_t ldd_dname;
} QLDDEV_LINK_t;
#ifndef _QRAM_H
typedef struct QLRECT {
unsigned short q_width;
unsigned short q_height;
unsigned short q_x;
unsigned short q_y;
} QLRECT_t;
#endif
typedef struct WINDOWDEF {
unsigned char border_colour;
unsigned char border_width;
unsigned char paper;
unsigned char ink;
unsigned short width;
unsigned short height;
unsigned short x_origin;
unsigned short y_origin;
} WINDOWDEF_t;
#define TIME_QL_UNIX(t) ((t) - 283996800)
#define TIME_UNIX_QL(t) ((t) + 283996800)
#define BLACK_M8 0
#define BLUE_M8 1
#define RED_M8 2
#define MAGENTA_M8 3
#define GREEN_M8 4
#define CYAN_M8 5
#define YELLOW_M8 6
#define WHITE_M8 7
#define BLACK_M4 0
#define RED_M4 2
#define GREEN_M4 4
#define WHITE_M4 7
#define DM_XOR -1
#define DM_OVER 0
#define DM_OR 1
#define CWIDTH_6 0
#define CWIDTH_8 1
#define CWIDTH_12 2
#define CWIDTH_16 3
#define CHEIGHT_10 0
#define CHEIGHT_20 1
typedef struct qdirect {
unsigned long d_length;
unsigned char d_access;
unsigned char d_type;
unsigned long d_datalen;
long d_reserved;
unsigned short d_szname;
char d_name[36];
long d_update;
union {
long d_refdate;
struct {
unsigned short d_version;
unsigned short d_fileno;
} v2;
} u;
long d_backup;
} qdirect_t;
typedef struct DIR_LIST {
struct DIR_LIST *dl_next;
struct qdirect dl_dir;
char dl_cname[1];
} DIR_LIST_t;
#define DREADSIZE sizeof( struct qdirect )
#define QF_DATA_TYPE 0
#define QF_PROG_TYPE 1
#define QF_RELOC_TYPE 2
#define THOR_DIR_TYPE 3
#define CST_DIR_TYPE 4
#define QF_DIR_TYPE 255
typedef struct ext_mdinf {
union {
char m_size[22];
QLSTR_t m_name;
} xm_name;
union {
char m_dsize[6];
QLSTR_t m_dname;
} xm_dname;
unsigned char xm_dnum;
char xm_rdonly;
unsigned short xm_alloc;
unsigned long xm_total;
unsigned long xm_free;
unsigned long xm_hdrlen;
char xm_spare[20];
char xm_spare2[36];
} ext_mdinf_t;
#define QDR_ALL 0
#define QDR_DATA 1
#define QDR_PROG 2
#define QDR_DIR 4
#define OLD_EXCL 0
#define OLD_SHARE 1
#define NEW_EXCL 2
#define NEW_OVER 3
#define DIROPEN 4
#define NETDEV 0x1
#define DIRDEV 0x2
extern WINDOWDEF_t _condetails;
extern char _copyright[];
extern long _def_priority;
extern char * _endmsg;
extern timeout_t _endtimeout;
extern long _Jobid;
extern long _memincr;
extern long _memmax;
extern long _memqdos;
extern long _mneed;
extern long _pipesize;
extern char _prog_name[];
extern char _SLASH;
extern char * _SPorig;
extern long _stack;
extern long _stackmargin;
extern char * _sys_var;
extern char _tmpdir_[];
extern char _tmpnam_[];
extern char _version[];
extern int os_nerr;
extern char *os_errlist[];
extern char _CPU;
extern char _FPU;
extern long (*_cmdchannels) _P_((long));
extern int (*_cmdparams) _P_((const char *,char ***,int *,int (*)_P_((char *,char ***,int *)) ));
extern int (*_cmdwildcard) _P_((char *,char ***,int *));
extern void (*_consetup) _P_((chanid_t,WINDOWDEF_t *));
extern void _initcon _P_((void));
extern long (*_stackchannels) _P_((long));
extern int (*_readkbd) _P_((chanid_t,timeout_t,char *));
int cmdexpand _P_((char *,char ***,int*));
void consetup_qpac _P_((chanid_t,WINDOWDEF_t *));
void consetup_title _P_((chanid_t,WINDOWDEF_t *));
int readkbd_move _P_((chanid_t,timeout_t,char *));
int ioppick _P_((jobid_t));
#define cstr_to_ql _cstr_to_ql
#define d_to_qlfp _d_to_qlfp
QLSTR_t * cstr_to_ql _P_((QLSTR_t *,char *));
QLFLOAT_t * d_to_qlfp _P_((QLFLOAT_t *,double));
char * qlstr_to_c _P_((char *,QLSTR_t *));
QLFLOAT_t * i_to_qlfp _P_((QLFLOAT_t *,int));
QLFLOAT_t * l_to_qlfp _P_((QLFLOAT_t *,long));
QLFLOAT_t * w_to_qlfp _P_((QLFLOAT_t *,int));
double qlfp_to_d _P_((QLFLOAT_t *));
long qlfp_to_f _P_((QLFLOAT_t *));
QLSTR_t * qstrcat _P_((QLSTR_t *,const QLSTR_t *));
char * qstrchr _P_((const QLSTR_t *,int));
int qstrcmp _P_((const QLSTR_t *,const QLSTR_t *));
QLSTR_t * qstrcpy _P_((QLSTR_t *,const QLSTR_t *));
int qstricmp _P_((QLSTR_t *,QLSTR_t *));
size_t qstrlen _P_((const QLSTR_t *));
QLSTR_t * qstrncat _P_((QLSTR_t *,const QLSTR_t *,short));
int qstrncmp _P_((const QLSTR_t *,const QLSTR_t *,short));
QLSTR_t * qstrncpy _P_((QLSTR_t *,const QLSTR_t *,short));
int qstrnicmp _P_((QLSTR_t *,QLSTR_t *,short));
#define argfree _argfree
#define argpack _argpack
#define argunpack _argunpack
#ifndef BEEP
#define beep _beep
#endif
#define chpdir _chpdir
#define do_sound _do_sound
void _super _P_((void));
void _user _P_((void));
void argfree _P_((char ***));
char * argpack _P_((char * const *,int));
int argunpack _P_((const char *,char ***,int *,int (*)_P_((char *,char ***,int*)) ));
#ifndef _BEEP
void beep _P_((unsigned short,unsigned char));
#define _BEEP
#endif
int chddir _P_((char *));
int chpdir _P_((char *));
void do_sound _P_((unsigned short,unsigned char,unsigned char,unsigned char,\
unsigned short,unsigned char,unsigned char,unsigned char));
int fnmatch _P_((char *,char *));
int fqstat _P_((int,struct qdirect *));
char * getcdd _P_((char *,int));
char * getcpd _P_((char *,int));
long getchid _P_((int));
char * getcname _P_((chanid_t,char *));
int iscon _P_((chanid_t,timeout_t));
int isdevice _P_((const char *,int *));
struct QLDDEV_LINK * isdirchid _P_((chanid_t));
int isdirdev _P_((const char *));
int isnoclose _P_((int));
int keyrow _P_((char));
chanid_t open_qdir _P_((const char *));
int poserr _P_((const char *));
void qdir_delete _P_((DIR_LIST_t *));
DIR_LIST_t * qdir_read _P_((char *,char *,int));
DIR_LIST_t * qdir_sort _P_((DIR_LIST_t *,char *,\
int (*)(DIR_LIST_t *,DIR_LIST_t *,char *)));
long qdos1 _P_((struct REGS *,struct REGS *));
long qdos2 _P_((struct REGS *,struct REGS *));
long qdos3 _P_((struct REGS *,struct REGS *));
pid_t qforkl _P_((const jobid_t,const char *,int *,const char *,...));
pid_t qforklp _P_((const jobid_t,const char *,int *,const char *,...));
pid_t qforkv _P_((const jobid_t,const char *,int *,char * const *));
pid_t qforkvp _P_((const jobid_t,const char *,int *,char * const *));
int qinstrn _P_((char *,int));
int read_qdir _P_((chanid_t,char *,char *,struct qdirect *,int));
int qstat _P_((char *,struct qdirect *));
long stackcheck _P_((void));
long stackreport _P_((void));
char * str_oserr _P_((int));
int usechid _P_((chanid_t));
int waitfor _P_((jobid_t,int *));
#ifdef __LIBRARY__
#define DIR_MAGIC 0x4AFB
#define ENV_MAGIC 0x4AFC
extern unsigned char _CCX_option;
extern unsigned char _C_esc_a[];
extern unsigned char _C_esc[];
extern unsigned char _C_hex_a[];
extern unsigned char _C_hex[];
extern unsigned char _C_oct_a[];
extern unsigned char _C_oct[];
extern char _data_use[];
extern char _prog_use[];
extern char _spl_use[];
extern int _argc;
extern char **_VARG;
extern char *_VENV;
extern char **_VENVP;
extern char _iname[];
extern char _oname[];
extern char _ename[];
extern char *_Envp;
extern long _nsems;
extern char _conname[];
extern chanid_t _conchan;
extern chanid_t _endchanid;
extern int _fmode;
extern int _iomode;
extern long _stackmax;
extern int _ydays_per_month[];
extern void (*_tmpdel)(void);
int _cd _P_((char *,char *));
int _ch_chown _P_((chanid_t,jobid_t));
int _chksmpl _P_((QLSTR_t *));
long _cmdchans _P_((long));
void _cmdparse _P_((char *));
chanid_t _conget _P_((void));
void _consetup_default _P_((chanid_t,WINDOWDEF_t *));
void _envsetup _P_((char *));
long _forkexec _P_((const char *,int,int *,char * const *,jobid_t));
int _fqstat _P_((chanid_t,struct qdirect *));
char * _getcd _P_((char *,char *,int));
jobid_t _get_chown _P_((chanid_t));
void _main _P_((char *));
char * _mkname _P_((char *,const char *));
int _read_qdir_action _P_((chanid_t,char *,char *,struct qdirect *,\
int,char *,char *,int,char *,int));
int _read_qdir_prep _P_((char *,char *,char *,int *,char *,\
char *,char **,int *));
int _qlmknam _P_((char *,const char *,const char *,int));
void _setcd _P_((char *,char *));
void _stdchans _P_((void));
long _stkchans _P_((long));
void _stack_error _P_((long));
void _stack_newmax _P_((void));
int _waitforjob _P_((jobid_t));
#endif
#undef _P_
#endif
