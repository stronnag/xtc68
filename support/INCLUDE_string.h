#ifndef _STRING_H
#define _STRING_H
#ifndef _TYPES_H
#include <sys/types.h>
#endif
#ifndef NULL
#define NULL ((void *)0)
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#define memcmp _MemCmp
#define memcpy _MemCpy
#define memmove _MemMove
#define memset _MemSet
#define strcat _StrCat
#define strcpy _StrCpy
#define strlen _StrLen
void * memchr _P_((const void *,int,size_t));
int memcmp _P_((const void *,const void *,size_t));
void * memcpy _P_((void *,const void *,size_t));
void * memmove _P_((void *,const void *,size_t));
void * memset _P_((void *,int,size_t));
char * strcat _P_((char *,const char *));
char * strchr _P_((const char *,int));
int strcmp _P_((const char *,const char *));
int strcoll _P_((const char *,const char *));
char * strcpy _P_((char *,const char *));
size_t strcspn _P_((const char *,const char *));
char * strerror _P_((int));
size_t strlen _P_((const char *));
char * strncat _P_((char *,const char *,size_t));
int strncmp _P_((const char *,const char *,size_t));
char * strncpy _P_((char *,const char *,size_t));
char * strpbrk _P_((const char *,const char *));
char * strrchr _P_((const char *,int));
size_t strspn _P_((const char *,const char *));
char * strstr _P_((const char *,const char *));
char * strtok _P_((char *,const char *));
size_t strxfrm _P_((char *,const char *,size_t));
char * index _P_((const char *,int));
char * memccpy _P_((char *,char *,int,size_t));
char * rindex _P_((const char *,int));
int stricmp _P_((const char *,const char *));
char * strdup _P_((char *));
char * strlwr _P_((char *));
void strmfe _P_((char *,const char *,const char *));
void strmfn _P_((char *,const char *,const char *,const char *,const char *));
void strmfp _P_((char *,const char *,const char *));
int strnicmp _P_((const char *,const char *,size_t));
char * strnset _P_((char *,int,int));
int strpos _P_((const char *,int));
char * strrev _P_((char *));
int strrpos _P_((const char *,int));
char * strrstr _P_((const char *,const char *));
char * strset _P_((char *,int));
void strsfn _P_((char *,char *,char *,char *,char *));
char * strupr _P_((char *));
#define movmem(source,target,length) (void *)memmove((void *)(target),(void *)(source),(size_t)(length))
#define setmem(target,length,value) (void)memset((void *)(target),(int)(value),(size_t)(length))
int stccpy _P_((char *,char *,int));
char * stpblk _P_((char *));
#define stpbrk strpbrk
#define stpchrn strrchr
#define stpchr strchr
char * stpcpy _P_((char *,const char *));
int strbpl _P_((char **,int,char *));
#define strcmpi stricmp
#define strsrt tqsort
void repmem _P_((char *,char *,int,int));
void swmem _P_((char *,char *,unsigned));
int stcarg _P_((char *,char *));
int stcgfe _P_((char *,char *));
int stcgfn _P_((char *,char *));
int stcis _P_((char *,char *));
int stcisn _P_((char *,char *));
int stclen _P_((char *));
int stcd_i _P_((char *,int *));
int stcd_l _P_((char *,long *));
int stch_i _P_((char *,int *));
int stch_l _P_((char *,long *));
int stci_d _P_((char *,int));
int stci_h _P_((char *,int));
int stci_o _P_((char *,int));
int stcl_d _P_((char *,long));
int stcl_h _P_((char *,long));
int stcl_o _P_((char *,long));
int stco_i _P_((char *,int *));
int stco_l _P_((char *,long *));
int stcpm _P_((char *,char *,char **));
int stcpma _P_((char *,char *));
int stcu_d _P_((char *,unsigned));
int stcul_d _P_((char *,unsigned long));
char * stpdate _P_((char *,int,char *));
char * stpsym _P_((char *,char *,int));
char * stptime _P_((char *,int,char *));
char * stptok _P_((char *,char *,int,char *));
int stscmp _P_((char *,char *));
int stspfp _P_((char *,int *));
char * strrpbrk _P_((const char *,const char *));
int strfnd _P_((const char *,const char *));
void strins _P_((char *,char *));
char * strrstrip _P_((char *,int));
char * strstrip _P_((char *,int));
#undef _P_
#endif
