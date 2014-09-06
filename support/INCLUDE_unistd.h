#ifndef _UNISTD_H
#define _UNISTD_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
#define _POSIX_VERSION 198808L
#define STDIN_FILENO 0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
#ifndef NULL
#define NULL ((void *)0)
#endif
#define _SC_ARG_MAX 1
#define _SC_CHILD_MAX 2
#define _SC_CLOCKS_PER_SEC 3
#define _SC_NGROUPS_MAX 4
#define _SC_OPEN_MAX 5
#define _SC_JOB_CONTROL 6
#define _SC_SAVED_IDS 7
#define _SC_VERSION 8
#define _PC_LINK_MAX 1
#define _PC_MAX_CANON 2
#define _PC_MAX_INPUT 3
#define _PC_NAME_MAX 4
#define _PC_PATH_MAX 5
#define _PC_PIPE_BUF 6
#define _PC_NO_TRUNC 7
#define _PC_VDISABLE 8
#define _PC_CHOWN_RESTRICTED 9
#define _POSIX_CHOWN_RESTRICTED
#define _POSIX_VDISABLE'\t'
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
#define environ _environ
extern char **environ;
#define chdir _Chdir
#define close _Close
#define dup2 _Dup2
#define read _Read
#define unlink _Unlink
#define write _Write
void _exit _P_((int));
int access _P_((char *,int));
int alarm _P_((unsigned int));
int chdir _P_((char *));
int chown _P_((const char *,uid_t,gid_t));
int close _P_((int));
char * ctermid _P_((char *));
char * cuserid _P_((char *));
int dup _P_((int));
int dup2 _P_((int,int));
int execl _P_((const char *,int *,const char *,...));
int execlp _P_((const char *,int *,const char *,...));
int execv _P_((const char *,int *,char * const * ));
int execvp _P_((const char *,int *,char * const * ));
pid_t forkl _P_((const char *,int *,const char *,...));
pid_t forklp _P_((const char *,int *,const char *,...));
pid_t forkv _P_((const char *,int *,char * const *));
pid_t forkvp _P_((const char *,int *,char * const *));
long fpathconf _P_((int,int));
int fsync _P_((int));
int ftruncate _P_((int,off_t));
char * getcwd _P_((char *,int));
gid_t getegid _P_((void));
uid_t geteuid _P_((void));
gid_t getgid _P_((void));
int getgroups _P_((int,gid_t));
char * getlogin _P_((void));
pid_t getpgrp _P_((void));
pid_t getpid _P_((void));
pid_t getppid _P_((void));
uid_t getuid _P_((void));
int isatty _P_((int));
int link _P_((const char *,const char *));
off_t lseek _P_((int,off_t,int));
long pathconf _P_((const char *,int));
int pause _P_((void));
int pipe _P_((int *));
int read _P_((int,void *,unsigned int));
int rmdir _P_((const char *));
int seteuid _P_((int));
int setgid _P_((int));
int setpgid _P_((pid_t,pid_t));
pid_t setsid _P_((void));
int setuid _P_((int));
unsigned sleep _P_((unsigned int));
int stime _P_((const time_t *));
void sync _P_((void));
long sysconf _P_((int));
int truncate _P_((char *,off_t));
char * ttyname _P_((int));
int unlink _P_((const char *));
int write _P_((int,void *,unsigned int));
void * sbrk _P_((int));
void * lsbrk _P_((long));
char * brk _P_((char *));
char * mktemp _P_((char *));
pid_t tcgetpgrp _P_((int));
int tcsetpgrp _P_((int,pid_t));
extern int (*_f_onexit) _P_((int));
#ifdef __LIBRARY__
#endif
#undef _P_
#endif
