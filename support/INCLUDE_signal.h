#ifndef _SIGNAL_H
#define _SIGNAL_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#ifndef NULL
#define NULL ((void *)0)
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
typedef int sig_atomic_t;
#ifndef _SIGSET_T
#define _SIGSET_T
typedef unsigned long sigset_t;
#endif
#ifdef _OLDSIGNALDEF
#define SIGABRT 1
#define SIGALRM 2
#define SIGFPE 3
#define SIGHUP 4
#define SIGILL 5
#define SIGINT 6
#define SIGKILL 7
#define SIGPIPE 8
#define SIGQUIT 9
#define SIGSEGV 10
#define SIGTERM 11
#define SIGUSR1 12
#define SIGUSR2 13
#define _NSIG 13
#endif
#ifndef _OLDSIGNALDEF
#define SIGHUP 1
#define SIGINT 2
#define SIGQUIT 3
#define SIGILL 4
#define SIGPIPE 5
#define SIGSEGV 6
#define SIGBUS 7
#define SIGFPE 8
#define SIGKILL 9
#define SIGALRM 10
#define SIGABRT 11
#define SIGTRACE 12
#define SIGWINCHD 13
#define SIGWMREQ 14
#define SIGTERM 15
#define SIGUSR1 16
#define SIGUSR2 17
#define _NSIG 17
#endif
#define NSIG _NSIG
#define SIGCHLD 0
#define SIGCONT 0
#define SIGSTOP 0
#define SIGTSTP 0
#define SIGTTIN 0
#define SIGTTOU 0
#ifndef _POSIX_SOURCE
#define SIGIOT 1
#define SIGUNUSED 0
#define SIGSTKFLT 0
#define SIGTRAP SIGTRACE
#define SIGEMT 0
#endif
#ifdef _POSIX_SOURCE
#define SA_NOCLDSTOP 1
#endif
#define SIG_BLOCK 0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2
#define SIG_IGN ((void (*)(int))0)
#define SIG_DFL ((void (*)(int))1)
#define SIG_HOLD ((void (*)(int))2)
#define SIG_ERR ((void (*)(int))-1)
struct sigaction {
void (*sa_handler)_P_((int));
sigset_t sa_mask;
int sa_flags;
};
#define SA_RESETHAND 0x1
typedef char * caddr_t;
typedef struct {
int si_signo;
int si_errno;
int si_code;
pid_t si_pid;
uid_t si_uid;
int si_status;
long si_band;
caddr_t si_addr;
} siginfo_t;
int kill _P_((pid_t,int));
int raise _P_((int));
int sigaction _P_((int,struct sigaction *,struct sigaction *));
int sigaddset _P_((sigset_t *,int));
int sigdelset _P_((sigset_t *,int));
int sigemptyset _P_((sigset_t *));
int sigfillset _P_((sigset_t *));
int sigismember _P_((sigset_t *,int));
void (*signal _P_((int,void (*)_P_((int)))))_P_((int));
int sigpending _P_((sigset_t *));
int sigprocmask _P_((int,sigset_t *,sigset_t *));
int sigsuspend _P_((sigset_t *));
void psignal _P_((int,const char *));
void psiginfo _P_((siginfo_t *,const char *));
int sighold _P_((int));
int sigrelse _P_((int));
int sigignore _P_((int));
int sigpause _P_((int));
void (*sigset _P_((int,void (*)_P_((int)))))_P_((int));
int fraise _P_((int,int));
int raiseu _P_((int,int));
int sigcleanup _P_((void));
#undef _P_
#endif
