
#ifndef _STDARG_H
#define _STDARG_H
#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long size_t;
#endif
typedef char *va_list;
#define __vasz(x) ((sizeof(x)+sizeof(int)-(size_t)1) & ~(sizeof(int) -(size_t)1))
#define va_start(ap,parmN) ((ap) = (va_list)&parmN + __vasz(parmN))
#define va_arg(ap,type) \
(*((type *)((va_list)((ap) = (void *)((va_list)(ap) + __vasz(type))) \
- __vasz(type))))
#define va_end(ap)
#endif
