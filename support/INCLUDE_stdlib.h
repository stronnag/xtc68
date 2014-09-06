#ifndef _STDLIB_H_
#define _STDLIB_H_
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#ifndef NULL
#define NULL ((void *)0)
#endif
#define EXIT_SUCCESS 0
#define EXIT_FAILURE -1
#define MB_CUR_MAX sizeof(char)
#ifndef RAND_MAX
#define RAND_MAX 2147483647
#endif
extern char * optarg;
extern int optind,opterr,optopt;
typedef struct { int quot; int rem; } div_t;
typedef struct {long quot; long rem; } ldiv_t;
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
#define calloc _Calloc
#define realloc _Realloc
void * malloc _P_((size_t));
void * calloc _P_((size_t,size_t));
void * realloc _P_((void *,size_t));
void free _P_((void *));
char * alloca _P_((size_t));
char * getmem _P_((int));
char * getml _P_((long));
int rlsmem _P_((char *,int));
int rlsml _P_((char *,long));
#define qsort _Qsort
void *bsearch _P_((const void *,const void *,size_t,size_t,int (*)(const void *,const void *)));
void qsort _P_((void *,size_t,size_t,int (*)(const void *,const void *)));
void dqsort _P_((double *,int));
void fqsort _P_((float *,int));
void lqsort _P_((long *,int));
void sqsort _P_((short *,int));
void tqsort _P_((char **,int));
int rand _P_((void));
void srand _P_((unsigned));
double drand48 _P_((void));
double erand48 _P_((short *));
long jrand48 _P_((short *));
void lcong48 _P_((short *));
long lrand48 _P_((void));
long mrand48 _P_((void));
long nrand48 _P_((short *));
short *seed48 _P_((short *));
void srand48 _P_((long));
#define abort _Abort
#define putenv _Putenv
#define realloc _Realloc
#define strtod _Strtod
#define strtoul _Strtoul
int abs _P_((int));
void abort _P_((void));
char * argopt _P_((int,char**,char *,int *,char *));
int atexit _P_((void (*)(void)));
double atof _P_((const char *));
int atoi _P_((const char *));
long atol _P_((const char *));
div_t div _P_((int,int));
void exit _P_((int));
char * getenv _P_((const char *));
gid_t getegid _P_((void));
uid_t geteuid _P_((void));
int getfnl _P_((char *,char *,unsigned,int));
gid_t getgid _P_((void));
int getopt _P_((int,char **,char *));
int isatty _P_((int));
long labs _P_((long));
ldiv_t ldiv _P_((long,long));
char * mktemp _P_((char *));
int onexit _P_((int(*)(void)));
int putenv _P_((char *));
double strtod _P_((const char *,char **));
long strtol _P_((const char *,char **,int));
unsigned long strtoul _P_((const char *,char**,int));
int system _P_((const char *));
char * ttyname _P_((int));
long utpack _P_((char *));
void utunpk _P_((long,char *));
#define atof(s) strtod(s,0)
#define atoi(s) (int)strtoul(s,NULL,10)
#define atol(s) (long)strtoul(s,NULL,10)
int mblen _P_((const char * s,size_t n));
int mbtowc _P_((wchar_t * pwc,const char *s,size_t n));
size_t mbstowcs _P_((wchar_t * pwc,const char *s,size_t n));
int wctomb _P_((char * s,wchar_t pwc));
size_t wcstombs _P_((char * s,const wchar_t * pwc,size_t n));
char * getpass _P_((char * prompt));
#define iabs(value) abs(value)
int envunpk _P_((char *));
char * itoa _P_((int,char *));
#ifdef __LIBRARY__
int _mul10add _P_((double *valuep,int digit));
double _adjust _P_((double *valuep,int decexp,int negflag));
#endif
#undef _P_
#endif
