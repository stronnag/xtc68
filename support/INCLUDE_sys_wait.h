#ifndef _SYS_WAIT_H_
#define _SYS_WAIT_H_
#ifndef _TYPES_H
#include <sys/types.h>
#endif
#define WNOHANG 0x01
#define WUNTRACED 0x02
#define WIFEXITED(stat_val) (0)
#define WEXITSTATUS(stat_val) (0)
#define WIFSIGNALED(stat_val) (0)
#define WTERMSIG(stat_val) (0)
#define WIFSTOPPED(stat_val) (0)
#define WSTOPSIG(stat_val) (0)
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
pid_t wait _P_((int *));
pid_t waitpid _P_((pid_t,int *,int));
#undef _P_
#endif
