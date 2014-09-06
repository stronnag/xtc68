#ifndef _SYS_TYPES_H
#define _SYS_TYPES_H
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif
#ifndef _TIME_T
#define _TIME_T
typedef long time_t;
#endif
#ifndef _CLOCK_T
#define _CLOCK_T
typedef long clock_t;
#endif
#ifndef _WCHAR_T
#define _WCHAR_T
typedef char wchar_t;
#endif
typedef unsigned long dev_t;
typedef unsigned char gid_t;
typedef unsigned int ino_t;
typedef int mode_t;
typedef unsigned char nlkink_t;
typedef long off_t;
typedef long pid_t;
typedef unsigned short uid_t;
typedef long ptrdiff_t;
typedef char * addreg_t;
typedef long datareg_t;
typedef long chanid_t;
typedef unsigned char event_t;
typedef long jobid_t;
typedef short timeout_t;
typedef unsigned char colour_t;
#ifndef INT16
#define INT16 short
#endif
#ifndef INT32
#define INT32 long
#endif
#if 0
#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#endif
#ifdef _LIBRARY_SOURCE
#ifndef __LIBRARY__
#define __LIBRARY__
#endif
#endif
#ifdef __LIBRARY__
#ifdef __STDC__
#define _LIB_F0_(t1) (t1)
#define _LIB_F1_(t1,n1) \
(t1 n1)
#define _LIB_F2_(t1,n1,t2,n2) \
(t1 n1,t2 n2)
#define _LIB_F3_(t1,n1,t2,n2,t3,n3) \
(t1 n1,t2 n2,t3 n3)
#define _LIB_F4_(t1,n1,t2,n2,t3,n3,t4,n4) \
(t1 n1,t2 n2,t3 n3,t4 n4)
#define _LIB_F5_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5) \
(t1 n1,t2 n2,t3 n3,t4 n4,t5 n5)
#define _LIB_F6_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6) \
(t1 n1,t2 n2,t3 n3,t4 n4,t5 n5,t6 n6)
#define _LIB_F7_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7) \
(t1 n1,t2 n2,t3 n3,t4 n4,t5 n5,t6 n6,t7 n7)
#define _LIB_F8_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7,t8,n8) \
(t1 n1,t2 n2,t3 n3,t4 n4,t5 n5,t6 n6,t7 n7,t8 n8)
#define _LIB_F9_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7,t8,n8,t9,n9) \
(t1 n1,t2 n2,t3 n3,t4 n4,t5 n5,t6 n6,t7 n7,t8 n8,t9 n9)
#define _LIB_F10_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7,t8,n8,t9,n9,t10,n10) \
(t1 n1,t2 n2,t3 n3,t4 n4,t5 n5,t6 n6,t7 n7,t8 n8,t9 n9,t10 n10)
#else
#define _LIB_F0(t1) ()
#define _LIB_F1_(t1,n1) \
(n1) t1 n1;
#define _LIB_F2_(t1,n1,t2,n2) \
(n1,n2) t1 n1; t2 n2;
#define _LIB_F3_(t1,n1,t2,n2,t3,n3) \
(n1,n2,n3) t1 n1; t2 n2; t3 n3;
#define _LIB_F4_(t1,n1,t2,n2,t3,n3,t4,n4) \
(n1,n2,n3,n4) t1 n1; t2 n2; t3 n3; t4 n4;
#define _LIB_F5_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5) \
(n1,n2,n3,n4,n5) t1 n1; t2 n2; t3 n3; t4 n4; t5 n5;
#define _LIB_F6_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6) \
(n1,n2,n3,n4,n5,n6) \
t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6;
#define _LIB_F7_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7) \
(n1,n2,n3,n4,n5,n6,n7) \
t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7;
#define _LIB_F8_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7,t8,n8) \
(n1,n2,n3,n4,n5,n6,n7,n8) \
t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8;
#define _LIB_F9_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7,t8,n8,t9,n9) \
(n1,n2,n3,n4,n5,n6,n7,n8,n9) \
t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9;
#define _LIB_F10_(t1,n1,t2,n2,t3,n3,t4,n4,t5,n5,t6,n6,t7,n7,t8,n8,t9,n9,t10,n10) \
(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10) \
t1 n1; t2 n2; t3 n3; t4 n4; t5 n5; t6 n6; t7 n7; t8 n8; t9 n9; t10 n10;
#endif
#endif
#endif
