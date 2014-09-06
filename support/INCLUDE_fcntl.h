#ifndef _FCNTL_H_
#define _FCNTL_H_
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#define O_RDONLY 0x0000
#define O_WRONLY 0x0001
#define O_RDWR 0x0002
#define O_ACCMODE (O_RDONLY|O_WRONLY|O_RDWR)
#define O_BINARY 0x0008
#define O_CREAT 0x0010
#define O_EXCL 0x0020
#define O_NOCTTY 0x0040
#define O_TRUNC 0x0080
#define O_APPEND 0x0100
#define O_NDELAY 0x0200
#define O_NONBLOCK 0x0400
#define O_SYNC 0x0800
#define O_RAW 0x1000
#define O_PDIR 0x1000
#define O_DDIR 0x2000
#define O_DIR 0x4000
#define F_DUPFD 1
#define F_GETFD 2
#define F_SETFD 3
#define F_GETFL 4
#define F_SETFL 5
#define F_GETLK 6
#define F_SETLK 7
#define F_SETLKW 8
struct flock {
short l_type;
short l_whence;
off_t l_start;
off_t l_len;
long l_sysid;
pid_t l_pid;
};
#define F_RDLCK 1
#define F_UNLCK 2
#define F_WRLCK 3
#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
#define SEEK_START SEEK_SET
#define SEEK_REL SEEK_CUR
#define tell( fd ) lseek( fd,0L,SEEK_REL)
#ifndef _TYPES_H
#include <sys/types.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define creat _Creat
#define open _open
extern int fdmode _P_((int,int));
extern int iomode _P_((int,int));
extern int fcntl _P_((int,int,...));
extern int opene _P_((const char *,mode_t,int));
extern int creat _P_((const char *,mode_t));
extern int open _P_((const char *,int,...));
typedef struct UFB UFB_t;
#define _Open _OpenVector
extern int (*_OpenVector) _P_((const char *,int,...));
extern int (*_conread) _P_((struct UFB *,void *,unsigned int));
extern int (*_conwrite) _P_((struct UFB *,void *,unsigned int));
extern int qopen _P_((const char *,int,...));
extern char _Qopen_in[];
extern char _Qopen_out[];
extern timeout_t _Timeout;
#ifdef __LIBRARY__
#ifndef _TERMIOS_H
#include <termios.h>
#endif
struct UFB {
char ufbtyp;
char ufbst;
short ufbflg;
chanid_t ufbfh;
chanid_t ufbfh1;
jobid_t ufbjob;
char * ufbnam;
struct termios ufbterm;
};
#define D_UNKNOWN 0
#define D_DISK 1
#define D_CON 2
#define D_PRN 3
#define D_AUX 4
#define D_NULL 5
#define D_PIPE 6
#define D_SOCKET 7
#define D_NET 8
#define UFB_EOF 0x80
#define UFB_OP 0x01
#define UFB_RA 0x02
#define UFB_WA 0x04
#define UFB_AP 0x08
#define UFB_NC 0x10
#define UFB_DP 0x20
#define UFB_NT 0x40
#define UFB_ND 0x80
#define UFB_TF 0x100
#define UFB_SY 0x200
#define UFB_NB 0x400
extern struct _MODE_TABLE {
short ufbflag;
short fileflag;
short modemask;
} _ModeTable[];
extern UFB_t * _ufbs;
extern long _nufbs;
int __Open _P_((const char *,int,...));
long _do_opene _P_((char *,long,int,long (*)_P_((char *,long,...))));
UFB_t * _Chkufb _P_((int));
timeout_t _GetTimeout _P_((UFB_t *));
int _ModeFd _P_((short));
short _ModeUfb _P_((int));
UFB_t * _Newufb _P_((int));
int _Openufb _P_((void));
void _Initcon _P_((void));
#endif
#undef _P_
#endif
