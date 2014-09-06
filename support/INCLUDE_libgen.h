
#ifndef _LIBGEN_H
#define _LIBGEN_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#ifndef _STDIO_H
#include <stdio.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
#define basename _BaseName
#define bgets _Bgets
#define bufsplit _Bufsplit
#define copylist _Copylist
#define dirname _Dirname
#define gmatch _Gmatch
#define pathfind _Pathfind
#define strccpy _Strccpy
#define strcadd _Strcadd
#define strecpy _Strecpy
#define streadd _Streadd
#define stresize _Stresize
#define strfind _Strfind
#define strrspn _Strrspn
#define strtrns _Strtrns
char * basename _P_((char *));
char * bgets _P_((char *,size_t,FILE *,const char *));
size_t bufsplit _P_((char *,size_t,char **));
char * copylist _P_((const char *,size_t *));
char * dirname _P_((char *));
int gmatch _P_((const char *,const char *));
char * pathfind _P_((const char *,const char *,const char *));
char * strccpy _P_((char *,const char *));
char * strcadd _P_((char *,const char *));
char * strecpy _P_((char *,const char *,const char *));
char * streadd _P_((char *,const char *,const char *));
int stresize _P_((const char *,const char *));
int strfind _P_((const char *,const char *));
char * strrspn _P_((const char *,const char *));
char * strtrns _P_((const char *,const char *,const char *,char *));
#if 0
#define isencrypt _Isencrypt
int isencrypt _P_((const char *,size_t));
#define mkdirp _Mkdirp
int mkdirp _P_((const char *,mode_t));
#define rmdirp _Rmdirp
int rmdirp _P_((char *,char *));
#define p2open _P2open
int p2open _P_((const char *,FILE **));
#define p2close _P2close
int p2close _P_((FILE **));
#define regcmp _Regcmp
char * regcmp _P_((const char *,char *,...));
#define regex _Regex
char * regex _P_((const char *,const char *,...));
#endif
#ifdef __LIBRARY__
int _CescInt _P_((int));
int _HexInt _P_((int));
int _OctInt _P_((int));
int _IntCesc _P_((int));
char * __Streadd _P_((char *,const char *,const char *,const char *));
int __Stresize _P_((const char *,const char *,const char * ));
#endif
#undef _P_
#endif
