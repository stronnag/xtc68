
#ifndef _DEBUG_H
#define _DEBUG_H
#ifdef LIBDEBUG
#define DBG_INIT() dbg_init()
#define DBG(args) dbg args
#define DBG_PRINT(args) dbg_print args
#define DBG_ENTER(args) dbg args
#define DBG_RETURN(args) dbg args
#else
#define DBG_INIT()
#define DBG(args)
#define DBG_PRINT(args)
#define DBG_ENTER(args)
#define DBG_RETURN(args)
#endif
#ifndef _STDARG_H
#include <stdarg.h>
#endif
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
void dbg_init _P_((void));
void dbg _P_((char *,int,char *,...));
void dbg_print _P_((char *,...));
void dbg_vfprint _P_((char *,va_list));
void dbg_write _P_((char *,int));
void dbg_snap _P_((int));
void dbg_snap1 _P_((void));
void dbg_snap2 _P_((void));
void dbg_snap3 _P_((void));
int malloc_opt _P_((int,...));
#undef _P_
#endif
