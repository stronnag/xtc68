
#ifndef _STD_H
#define _STD_H
#define BYTE char
#define BOOLEAN int
#define WORD short int
#define UWORD unsigned short int
#define DOUBLE double
#define LONG long
#define ULONG unsigned long
#define REG register
#define FAST register
#define IMPORT extern
#define LOCAL static
#define EXTERN extern
#define MLOCAL static
#define GLOBAL
#define VOID void
#ifdef UCHARA
#define UBYTE char
#else
#define UBYTE unsigned char
#endif
typedef UBYTE TEXT;
typedef WORD INT16;
typedef UWORD UINT16;
typedef int INT32;
typedef unsigned int UINT32;
typedef UBYTE UTINY,TBOOL;
typedef unsigned int UCOUNT,ARGINT,BITS;
typedef BYTE TINY;
typedef int BOOL,COUNT,METACH,HANDLE;
#define FAILURE (-1)
#define SUCCESS (0)
#define YES 1
#define NO 0
#define FOREVER for(;;)
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#define NULLPTR (char *) 0L
#ifndef FALSE
#define FALSE (0)
#endif
#ifndef TRUE
#define TRUE (!FALSE)
#endif
#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_NDELAY 4
#define O_APPEND 8
#define O_CREAT 0x0100
#define O_TRUNC 0x200
#define O_EXCL 0x400
#define O_RAW 0x8000
#define F_DUPFD 0
#define F_GETFD 1
#define F_SETFD 2
#define F_GETFL 3
#define F_SETFL 4
#endif
#define BYTMASK 255
#define BYTEMASK 255
#ifndef max
#define max(x,y) ((x) < (y) ? (y) : (x))
#endif
#ifndef min
#define min(x,y) ((x) > (y) ? (y) : (x))
#endif
#define abs(x) ((x) < 0 ? -(x) : (x))
#define BOXPARM(x) (x).g_x,(x).g_y,(x).g_w,(x).g_h
#define ABOXPARM(x) &(x).g_x,&(x).g_y,&(x).g_w,&(x).g_h
#define Newblk(t) ((t *)malloc(sizeof(t)))
#define LtoP(x) ((TEXT *)(x))
#define PtoL(x) ((LONG)(x))
#endif
