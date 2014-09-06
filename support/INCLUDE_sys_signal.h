#ifndef _SYS_SIGNAL_H
#define _SYS_SIGNAL_H
#ifndef _SIGNAL_H
#include <signal.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
#ifndef _SIGSET_T
typedef unsigned long sigset_t;
#define _SIGSET_T
#endif
struct SIG_PRIOR_R {
unsigned norm :3;
unsigned susp :3;
unsigned wfio :3;
unsigned wfjob:3;
unsigned req_sigstack:1;
unsigned reserved : 19;
};
struct SIG_PRIOR_S {
unsigned norm : 3;
unsigned susp : 3;
unsigned wfio : 3;
unsigned wfjob: 3;
unsigned df_susp : 1;
unsigned df_wfio : 1;
unsigned df_wfjob : 1;
unsigned retry_sigsend :1;
unsigned reserved : 16;
};
struct SIG_INFO {
unsigned long vers;
unsigned long gtick;
long (*sigvec)(int,chanid_t,...);
};
#define SIG_MAGIC 0x254d5347
struct SIG_MSG {
unsigned long magic;
unsigned long len;
unsigned short type;
unsigned short txi;
jobid_t jobid;
unsigned long signr;
struct SIG_PRIOR_S prio;
unsigned long uval;
};
struct TMR_MSG {
unsigned long magic;
unsigned long len;
unsigned short type;
unsigned short txi;
jobid_t jobid;
unsigned long signr;
struct SIG_PRIOR_S prio;
unsigned long uval;
unsigned long t_evid;
unsigned long t_ticks;
unsigned long t_int;
};
#define M_SIG 0
#define M_TIMER 1
#define M_HINIT 2
#define MT_TICKS 0
#define MT_SECS 1
#define EV_ALARM 1
#define HI_UVAL 0x1
#define HI_APRIO 0x2
#define HI_SYSC 0x4
struct SIG_HIMSG {
unsigned long magic;
unsigned long len;
unsigned short type;
unsigned short txi;
jobid_t jobid;
unsigned long hi_nsigs;
unsigned long hi_stack;
};
struct QDOS_SIGH {
unsigned short m1;
unsigned long m2;
void (*sighandler)(void);
char * stackmin;
struct SIG_PRIOR_R priority;
};
struct QDOS_CSIGH {
unsigned short m1;
unsigned long m2;
unsigned nsig;
void (*** curr) (int) ;
void (*** def) (int) ;
sigset_t **samask;
unsigned short prot_id;
union {unsigned short levl;
unsigned short **plevl;} hprotlevel;
unsigned short stack_id;
char * stackmin;
unsigned short pri_id;
union {struct SIG_PRIOR_R prio;
struct SIG_PRIOR_R **pprio; } priority;
void *siginfo;
int (*sigvec)(int,...);
unsigned short cprotlevel;
unsigned short defer;
unsigned short activ;
long **uval;
sigset_t block;
sigset_t pending;
sigset_t resethandler;
};
struct SREGS {
datareg_t d0,d1,d2;
addreg_t a0,a1;
};
#define SGN_NORM 0
#define SGN_WFIO 1
#define SGN_WFJB 2
#define SGN_SUSP 3
#define SGN_RAISE 4
struct SIGSVB {
void (*sig_clup)(void);
short signature;
long sf1,sf2;
datareg_t d4,d5,d6;
addreg_t a4,a5,a7;
short sr;
long ret_addr;
};
struct ESVB {
struct SIGSVB sigsvb;
sigset_t oblock;
long ohand;
long signr;
struct QDOS_CSIG *hptr;
long uval;
datareg_t d0,d1,d2;
addreg_t a0,a1;
};
#define sigmask(i) ((sigset_t)(1 << (i-1)))
#ifdef VERY_OLD
struct C_HANDLER {
unsigned nsig;
sigset_t sigpend;
sigset_t sigblock;
SigProtState_t sigprotect;
sigset_t sigreset;
void (*(* sigcurrent)[_NSIG+1]) (int) ;
void (*(* sigdefault)[_NSIG+1]) (int) ;
sigset_t (*sigsamask)[_NSIG+1] ;
};
extern struct C_HANDLER _sigcontrol;
extern void (* __sigcurrent[_NSIG+1])(int);
extern void (* __sigdefault[_NSIG+1])(int);
extern sigset_t __sigsamask[_NSIG+1];
#endif
extern chanid_t _sigch;
typedef long (*_sigvec_t) _P_((int,...));
extern _sigvec_t _sigvec;
#ifdef DEBUG
extern chanid_t _sigdebugch;
extern int _sigdebug;
extern int sigprintf(char *,...);
#endif
extern long _spbase;
extern long _stackmargin;
extern struct SIG_PRIOR_R _defsigrp;
extern struct SIG_PRIOR_S _defsigsp;
extern struct SIG_PRIOR_S _defsigskp;
extern unsigned short _hinit_flags;
#ifdef VERY_OLD
#define SIGF_BLK 0x1
#define SIGF_RES 0x2
extern long _defsigblock;
void _sigwrap(void);
void checksig(void);
void _call_sighandler(int);
#endif
int sendsig(chanid_t,jobid_t,long,struct SIG_PRIOR_S,long );
int set_timer_event(struct TMR_MSG *);
struct SYSCTL {
sigset_t pending;
long rval;
};
#define SYSCALL0(_flags_,_sctl_,_sysfunc_) \
((*_sigvec)(_sigch,8,_flags_,_sctl_,_sysfunc_,0))
#define SYSCALL1(_flags_,_sctl_,_sysfunc_,_arg1_) \
((*_sigvec)(_sigch,8,_flags_,_sctl_,_sysfunc_,\
1,_arg1_))
#define SYSCALL2(_flags_,_sctl_,_sysfunc_,_arg1_,_arg2_) \
((*_sigvec)(_sigch,8,_flags_,_sctl_,_sysfunc_,\
2,_arg1_,_arg2_))
#define SYSCALL3(_flags_,_sctl_,_sysfunc_,_arg1_,_arg2_,_arg3_) \
((*_sigvec)(_sigch,8,_flags_,_sctl_,_sysfunc_,\
3,_arg1_,_arg2_,_arg3_))
#define SYS_PENDING(_sctlp_) (((long)_sctlp_)>0 ? *_sctlp_ : (sigset_t)0)
#define SYS_ISPENDING(_sctlp_) (SYS_PENDING(_sctlp_) != (sigset_t)0)
#define SYSCTL(_flags_) ((*_sigvec)(_sigch,9,_flags_))
#define SCTL_EXP 0x1
#define _SIG_FTX (sigmask(SIGBUS) | sigmask(SIGILL) | sigmask(SIGSEGV))
#if 0
extern struct _sigcontrol {
int count;
sigset_t mask;
sigset_t pend;
sigset_t block;
long alarm;
void (*handler[_NSIG+1])_P_((int));
} _sigcurrent;
extern void (*_sigdefault [_NSIG+1])_P_((int));
#endif
#if 0
extern char _SigSend;
extern pid_t _SigSendPid;
extern long _SigSendPriority;
extern char * _SigSendAddress;
extern int _SigSendNum;
extern char _SigSendEnd;
void _chksig _P_((void));
void _sigdie _P_((int));
void _sigignore _P_((int));
void _sigreport _P_((int));
void (*_signal _P_((int,void (*)_P_((int)))))_P_((int));
#endif
extern void _onsigkill _P_((int));
#define _SIG_TERMINATE 0
#define _SIG_ABNORMAL 1
#define _SIG_IGNORE 2
#define _SIG_STOP 3
#define _SIG_ALARM 4
#define _SIG_TERMINATE 0
#define _SIG_ABNORMAL 1
#define _SIG_IGNORE 2
#define _SIG_STOP 3
#define _SIG_ALARM 4
#define _SIG_UNBLOCKABLE sigmask( SIGKILL )
extern char * __SigNoMsg;
extern short __SigNoCnt;
#if OLD
extern sigset_t * __SigSaMask[_NSIG+1];
extern void (* __SigCurrent[_NSIG+1]) (int);
extern void (* __SigDefault[_NSIG+1]) (int);
extern long * __SigUval[_NSIG+1];
#endif
extern int _SigNoImp(int,...);
#endif
