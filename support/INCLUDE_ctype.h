#ifndef _CTYPE_H
#define _CTYPE_H
#ifndef _TYPES_H
#include <sys/types.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
int isalpha _P_((int));
int isalnum _P_((int));
int isascii _P_((int));
int iscntrl _P_((int));
int iscsym _P_((int));
int iscsymf _P_((int));
int isdigit _P_((int));
int isgraph _P_((int));
int islower _P_((int));
int isprint _P_((int));
int ispunct _P_((int));
int isspace _P_((int));
int isupper _P_((int));
int isxdigit _P_((int));
int toascii _P_((int));
int tolower _P_((int));
int toupper _P_((int));
#define _U 1
#define _L 2
#define _N 4
#define _S 8
#define _P 16
#define _C 32
#define _B 64
#define _X 128
extern unsigned char * _ctype;
extern int _ctypetemp;
#define isalpha(c) (_ctype[(int)(c)] & (unsigned char)(_U|_L))
#define isupper(c) (_ctype[(int)(c)] & (unsigned char)_U)
#define islower(c) (_ctype[(int)(c)] & (unsigned char)_L)
#define isdigit(c) (_ctype[(int)(c)] & (unsigned char)_N)
#define isxdigit(c) (_ctype[(int)(c)] & (unsigned char)_X)
#define isspace(c) (_ctype[(int)(c)] & (unsigned char)_S)
#define ispunct(c) (_ctype[(int)(c)] & (unsigned char)_P)
#define isalnum(c) (_ctype[(int)(c)] & (unsigned char)(_U|_L|_N))
#define isprint(c) (_ctype[(int)(c)] & (unsigned char)(_P|_U|_L|_N|_B))
#define isgraph(c) (_ctype[(int)(c)] & (unsigned char)(_P|_U|_L|_N))
#define iscntrl(c) (_ctype[(int)(c)] & (unsigned char)_C)
#define isascii(c) ((unsigned)(c)<=127)
#define toascii(c) ((int)(c)&127)
#define _toupper(c) (_ctypetemp=(int)(c),islower(_ctypetemp)?((_ctypetemp)-('a'-'A')):(_ctypetemp))
#define _tolower(c) (_ctypetemp=(int)(c),isupper(_ctypetemp)?((_ctypetemp)+('a'-'A')):(_ctypetemp))
#define iscsym(c) (_ctypetemp=(int)(c),isalnum(_ctypetemp)||(((_ctypetemp)&127)==0x5f))
#define iscsymf(c) (_ctypetemp=(int)(c),isalpha(_ctypetemp)||(((_ctypetemp)&127)==0x5f))
#define toupper(c) _toupper(c)
#define tolower(c) _tolower(c)
#undef _P_
#endif
