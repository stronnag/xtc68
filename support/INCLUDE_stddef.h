#ifndef _STDDEF_H
#define _STDDEF_H
#ifndef _SYS_TYPES_H
#include <SYS/types.h>
#endif
#ifndef NULL
#define NULL ((void *)0)
#endif
#define offsetof(type,identifier) ((size_t)&((type *)0)->identifier)
#endif
