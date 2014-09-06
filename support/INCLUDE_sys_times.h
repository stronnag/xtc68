
#ifndef _TIMES_H
#define _TIMES_H
#ifndef _CLOCK_T
#define _CLOCK_T
typedef long clock_t;
#endif
#define CLK_TCK 50
struct tms {
clock_t tms_utime;
clock_t tms_stime;
clock_t tms_cutime;
clock_t tms_cstime;
};
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
clock_t times _P_((struct tms *__buffer));
#ifdef __LIBRARY__
#endif
#undef _P_
#endif
