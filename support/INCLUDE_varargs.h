
#ifndef _VARARGS_H
#define _VARARGS_H
typedef char *va_list;
#define __vasz(x) ((sizeof(x)+sizeof(int)-1) & ~(sizeof(int) -1))
#define va_dcl int va_alist;
#define va_start(ap) ((ap) = (va_list) &va_alist)
#define va_arg(ap,type) \
(*((type *)((va_list)((ap) = (void *)((va_list)(ap) + __vasz(type))) \
- __vasz(type))))
#define va_end(ap)
#endif
