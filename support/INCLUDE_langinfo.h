
#ifndef _LANGINFO_H
#define _LANGINFO_H
#define DAY_1 1
#define DAY_2 2
#define DAY_3 3
#define DAY_4 4
#define DAY_5 5
#define DAY_6 6
#define DAY_7 7
#define ABDAY_1 8
#define ABDAY_2 9
#define ABDAY_3 10
#define ABDAY_4 11
#define ABDAY_5 12
#define ABDAY_6 13
#define ABDAY_7 14
#define MON_1 15
#define MON_2 16
#define MON_3 17
#define MON_4 18
#define MON_5 19
#define MON_6 20
#define MON_7 21
#define MON_8 22
#define MON_9 23
#define MON_10 24
#define MON_11 25
#define MON_12 26
#define ABMON_1 27
#define ABMON_2 28
#define ABMON_3 29
#define ABMON_4 30
#define ABMON_5 31
#define ABMON_6 32
#define ABMON_7 33
#define ABMON_8 34
#define ABMON_9 35
#define ABMON_10 36
#define ABMON_11 37
#define ABMON_12 38
#define RADIXCHAR 39
#define THOUSEP 40
#define YESSTR 41
#define NOSTR 42
#define CRNCYSTR 43
#define D_T_FMT 44
#define D_FMT 45
#define T_FMT 46
#define AM_STR 47
#define PM_STR 48
#define MAXSTRMSG 48
#ifndef _NL_TYPES_H
#include <nl_types.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
char * nl_langinfo _P_((nl_item));
#undef _P_
#endif
