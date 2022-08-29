/*
 * C compiler
 * ==========
 *
 * Copyright 1989, 1990, 1991 Christoph van Wuellen.
 * Credits to Matthew Brandt.
 * All commercial rights reserved.
 *
 * This compiler may be redistributed as long there is no
 * commercial interest. The compiler must not be redistributed
 * without its full sources. This notice must stay intact.
 *
 * History:
 *
 * 1989   starting an 68000 C compiler, starting with material
 *        originally by M. Brandt
 * 1990   68000 C compiler further bug fixes
 *        started i386 port (December)
 * 1991   i386 port finished (January)
 *        further corrections in the front end and in the 68000
 *        code generator.
 *        The next port will be a SPARC port
 */

#ifndef _GEN86_H
#define _GEN86_H

#include "genx86.h"

typedef enum { target_8086, target_80186, target_80286 } TARGET;

/* support routines */
#define SUP_LDIV (const CHAR *)".ldiv"
#define SUP_LMOD (const CHAR *)".lmod"
#define SUP_LMUL (const CHAR *)".lmul"
#define SUP_LSHL (const CHAR *)".lshl"
#define SUP_LSHR (const CHAR *)".lshr"
#define SUP_ULDIV (const CHAR *)".uldiv"
#define SUP_ULMOD (const CHAR *)".ilmod"
#define SUP_ULMUL (const CHAR *)".ulmul"
#define SUP_ULSHL (const CHAR *)".ulshl"
#define SUP_ULSHR (const CHAR *)".ulshr"
#define SUP_ASLDIV (const CHAR *)".asldiv"
#define SUP_ASLMOD (const CHAR *)".aslmod"
#define SUP_ASLMUL (const CHAR *)".aslmul"
#define SUP_ASLSHL (const CHAR *)".aslshl"
#define SUP_ASLSHR (const CHAR *)".aslshr"
#define SUP_ASOPL (const CHAR *)".asopl"
#define SUP_ASULDIV (const CHAR *)".asuldiv"
#define SUP_ASULMOD (const CHAR *)".asilmod"
#define SUP_ASULMUL (const CHAR *)".asulmul"
#define SUP_ASULSHL (const CHAR *)".asulshl"
#define SUP_ASULSHR (const CHAR *)".asulshr"
#define SUP_STACKCHECK (const CHAR *)".stackcheck"

#endif /* _GEN86_H */
