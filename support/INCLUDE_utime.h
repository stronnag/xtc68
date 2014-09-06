#ifndef _UTIME_H
#define _UTIME_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
struct utimbuf {
time_t actime;
time_t modtime;
};
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
int utime _P_((char *,struct utimbuf *));
#undef _P_
#endif
