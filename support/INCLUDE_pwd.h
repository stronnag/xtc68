
#ifndef _PWD_H
#define _PWD_H
#ifndef _TYPES_H
#include <sys/types.h>
#endif
struct passwd {
char * pw_name;
uid_t pw_uid;
gid_t pw_gid;
char * pw_dir;
char * pw_shell;
char *pw_passwd;
char *pw_gecos;
};
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
struct passwd * getpwnam _P_((const char *));
struct passwd * getpwuid _P_((int));
void endpwent _P_((void));
struct passwd * getpwent _P_((void));
void setpwent _P_((void));
#ifdef _LIBRARY_
#endif
#undef _P_
#endif
