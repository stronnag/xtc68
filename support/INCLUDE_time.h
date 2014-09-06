#ifndef _TIME_H
#define _TIME_H
#ifndef NULL
#define NULL ((void *)0)
#endif
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#define CLOCKS_PER_SEC 50
#ifndef _TM_STRUC
#define _TM_STRUCT
struct tm
{
int tm_sec;
int tm_min;
int tm_hour;
int tm_mday;
int tm_mon;
int tm_year;
int tm_wday;
int tm_yday;
int tm_isdst;
};
#endif
extern char *tzname[];
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define asctime _Asctime
#define time _Time
char * asctime _P_((const struct tm *));
clock_t clock _P_((void));
char * ctime _P_((const time_t *));
double difftime _P_((time_t,time_t));
time_t mktime _P_((struct tm *));
size_t strftime _P_((char *,size_t,const char *,const struct tm *));
time_t time _P_((time_t *));
void tzset _P_((void));
struct tm * gmtime _P_((const time_t *));
struct tm * localtime _P_((const time_t *));
#ifdef _LIBRARY_SOURCE
#undef __LIBRARY__
#define __LIBRARY__
#endif
#ifdef __LIBRARY__
extern char * __week_day[];
extern char * __month[];
extern int __days_per_month[];
extern int __local_clock;
extern time_t __lt_offset;
extern time_t __dst_offset;
extern time_t __dst_switch;
extern time_t __back_switch;
extern int __n_hemi;
#endif
#undef _P_
#endif
