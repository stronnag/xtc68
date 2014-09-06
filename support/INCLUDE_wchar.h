#ifndef _WCHAR_H
#define _WCHAR_H
#ifndef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
#ifndef NULL
#define NULL ((void *)0)
#endif
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif
#ifndef _WCHAR_T
#define _WCHAR_T
typedef char wchar_t;
#endif
#ifndef _WCTYPE_T
#define _WCTYPE_T
typedef short wctype_t;
#endif
#ifndef _WINT_T
#define _WINT_T
typedef int wint_t;
#endif
#define WEOF EOF
#define WCHAR_MIN -32767
#define QCHAR_MAX +32767
#ifndef _TM_STRUCT
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
wchar_t * wcscpy _P_((wchar_t *,const wchar_t *));
wchar_t * wcsncpy _P_((wchar_t *,const wchar_t *,size_t));
wchar_t * wcscat _P_((wchar_t,const wchar_t *));
wchar_t * wcsncat _P_((wchar_t *,const wchar_t *,size_t));
int wcscoll _P_((const wchar_t *,const wchar_t *));
int wcscmp _P_((const wchar_t *,const wchar_t *));
int wcsncmp _P_((const wchar_t *,const wchar_t *,size_t));
size_t wcsxfrm _P_((wchar_t *,const wchar_t *,size_t));
wchar_t * wcschr _P_((const wchar_t *,wint_t));
size_t wcscspn _P_((const wchar_t *,const wchar_t *));
size_t wcslen _P_((const wchar_t *));
wchar_t wcspbrk _P_((const wchar_t *,const wchar_t *));
wchar_t * wcsrchr _P_((const wchar_t *,wint_t));
size_t wcsspn _P_((const wchar_t *,const wchar_t *));
wchar_t wcstok _P_((wchar_t *,const wchar_t *));
wchar_t wcswcs _P_((const wchar_t *,const wchar_t *));
double wcstod _P_((const wchar_t *,wchar_t **));
long wcstol _P_((const wchar_t *,wchar_t **,int));
unsigned long wcstoul _P_((const wchar_t *,wchar_t **,int));
wchar_t * wmemchr _P_((const wchar_t,wchar_t,szie_t));
int wmemcmp _P_((const wchar_t,const wchar_t,size_t));
wchar_t * wmemcpy _P_((wchar_t *,const wchar_t *,size_t));
wchar_t * wmemmove _P_((wchar_t *,const wchar_t *,size_t));
wchar_t * wmemset _P_((wchar_t *,int,size_t));
wint_t fgetwc _P_((FILE *));
wchar_t * fgetws _P_((wchar_t *,int,FILE *));
wint_t fputwc _P_((wint_t,FILE *));
int fputws _P_((const wchar_t *,FILE *));
int fwide _P_((FILE * stream,int orient));
wint_t getwc _P_((FILE *));
wint_t getwchar _P_((void));
wint_t putwc _P_((wint_t,FILE *));
wint_t putwchar _P_((wint_t));
wint_t ungetwc _P_((wint_t,FILE *));
int fwprintf _P_((FILE *,const wchar_t *,...));
int fwscanf _P_((FILE *,const wchar_t *,...));
int wprintf _P_((const wchar_t *,...));
int wscanf _P_((const wchar_t,...));
int swprintf _P_((wchar_t *,size_t,const wchar_t *,...));
int swscanf _P_((const wchar_t *,const wchar_t *,...));
int vfwprintf _P_((FILE *,const wchar_t *,va_list));
int vwprintf _P_((const wchar_t *,va_list));
int vswprintf _P_((wchar_t *,size_t,const wchar_t *,va_list));
#if 0
int wcswidth _P_((const wchar_t *,size_t));
int wcwidth _P_((wint_t));
#endif
size_t wcsftime _P_((wchar_t *,size_t,const wchar_t *,const struct tm *));
#undef _P_
#endif
