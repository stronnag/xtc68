#ifndef _DIRENT_H
#define _DIRENT_H
#ifndef _SYS_TYPES_H_
#include <sys/types.h>
#endif
#ifndef _LIMITS_H
#include <limits.h>
#endif
#define d_ino d_fileno
struct dirent {
off_t d_off;
ino_t d_ino;
unsigned short d_reclen;
unsigned short d_namlen;
char d_name[MAXNAMELEN + 1];
};
#define d_namelen d_namlen
typedef struct _dirdesc {
long dd_loc;
short dd_namelen;
char dd_name[MAXNAMELEN+1];
struct dirent dd_buf;
} DIR;
#define DIRSIZ(dp) \
(((sizeof(struct dirent) - (sizeof((dp)->d_name)) + ((dp)->d_namelen))+1)&~1U)
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
int getdents _P_((int,struct dirent *,size_t));
int closedir _P_((DIR *));
DIR * opendir _P_((char *));
void rewinddir _P_((DIR *));
void seekdir _P_((DIR *,long));
long telldir _P_((DIR *));
struct dirent *readdir _P_((DIR *));
#undef _P_
#endif
