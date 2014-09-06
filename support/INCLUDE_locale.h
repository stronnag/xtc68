
#ifndef _LOCALE_H
#define _LOCALE_H
struct lconv {
char *decimal_point;
char *thousands_sep;
char *grouping;
char *int_curr_symbol;
char *currency_symbol;
char *mon_decimal_point;
char *mon_thousands_sep;
char *mon_grouping;
char *positive_sign;
char *negative_sign;
char int_frac_digits;
char frac_digits;
char p_cs_precedes;
char p_sep_by_space;
char n_cs_precedes;
char n_sep_by_space;
char p_sign_posn;
char n_sign_posn;
};
#define NULL ((void *)0)
#define LC_ALL 1
#define LC_COLLATE 2
#define LC_CTYPE 3
#define LC_MONETARY 4
#define LC_NUMERIC 5
#define LC_TIME 6
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
char *setlocale _P_((int _category,const char *_locale));
struct lconv *localeconv _P_((void));
#undef _P_
#endif
