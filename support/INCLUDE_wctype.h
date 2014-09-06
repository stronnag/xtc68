#ifndef _WCTYPE_H
#define _WCTYPE_H
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
#ifndef _WCTRANS_T
#define _WCTRANS_T
typedef short wctrans_t;
#endif
int iswalnum _P_((wint_t));
int iswalpha _P_((wint_t));
int iswcntrl _P_((wint_t));
int iswdigit _P_((wint_t));
int iswgraph _P_((wint_t));
int iswlower _P_((wint_t));
int iswprint _P_((wint_t));
int iswpunct _P_((wint_t));
int iswspace _P_((wint_t));
int iswupper _P_((wint_t));
int iswxdigit _P_((wint_t));
wint_t towlower _P_((wint_t));
wint_t towupper _P_((wint_t));
wctype_t wctype _P_((const char *));
int iswctype _P_((wint_t,wctype_t));
wctrans_t wctrans _P_((const char *));
wint_t towctrans _P_((wint_t,wctrans_t);
#undef _P_
#endif
