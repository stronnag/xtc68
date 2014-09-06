
#ifndef _GRP_H
#define _GRP_H
#ifndef _TYPES_H
#include <sys/types.h>
#endif
struct group {
char * gr_name;
char * gr_passwd;
gid_t gr_gid;
char ** gr_mem;
};
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
struct group * getgrgid _P_((gid_t _gid));
struct group * getgrnam _P_((const char *_name));
void endgrent _P_((void));
struct group * getgrent _P_((void));
void setgrent _P_((void));
#ifdef _LIBRARY_
#endif
#undef _P_
#endif
