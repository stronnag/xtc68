#ifndef _MEMORY_H
#define _MEMORY_H
#ifndef _STRING_H
#include <string.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define bcmp _Bcmp
#define bcopy _Bcopy
#define bzero _Bzero
int bcmp _P_((char *,char *,int));
char * bcopy _P_((char *,char *,int));
void bzero _P_((char *,int));
#undef _P_
#endif
