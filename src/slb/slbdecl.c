/*      s l b d e c l _ c
 *
 * (c) Copyright 1991 David J. Walker
 *     Email:  d.j.walker.lon4905@oasis.icl.co.uk
 *
 * This module contains the declarations of all initialised data areas
 * used within the SLB librarian program.
 *
 *     Permission to copy and/or distribute granted under the
 *     following conditions:
 *
 *     1). This notice must remain intact.
 *     2). The author is not responsible for the consequences of use
 *         this software, no matter how awful, even if they
 *         arise from defects in it.
 *     3). Altered version must not be represented as being the
 *         original software.
 */

/*
 *  Set up the following macros so that the definitions are actually created
 *  from the entries in the header file
 */
#define EXTERN
#define INIT(param) = param

/*
 *  Now call in the header file to actually create them
 */
#include "slb.h"

/*
 *  Set various environment values to be different to defaults
 */
#ifdef QDOS
long _stack = 8L * 1024L;
long _stackmargin = 512L;
struct WINDOWDEF _condetails = {2, 1, 0, 7, 484, 216, 14, 14};
void (*_consetup)() = consetup_title;
#endif
/*
 *  List of possible error messages (used by error(n) routine);
 */
char *errmsg[22] = {
    /*  0 */ "Unknown error",
    /*  1 */ "file to read from not open",
    /*  2 */ "failure reading character\n(File Position = %ld)",
    /*  3 */ "failure  writing to file\n(File Position = %ld)",
    /*  4 */ "Failed to open file %s",
    /*  5 */ "failure reading module list",
    /*  6 */ "Cannot open module list",
    /*  7 */ "Abandoned due to parameter error",
    /*  8 */ "Cannot create library '%s'\n(it already exists)",
    /*  9 */ "Failed to create library '%s'",
    /* 10 */ "Failed to open library '%'",
    /* 11 */ "Failed to create workfile '%s'",
    /* 12 */ "Cannot create listfile %s",
    /* 13 */ "*** Failure closing workfile ***",
    /* 14 */ "*** Delete failed ***\nWorkfile '%s' remains",
    /* 15 */ "Old library still in '%s'\nNew library in '%s'\n*** Delete of old library failed ***",
    /* 16 */ "New library is in '%s'\n*** Restore to original library name failed ***",
    /* 17 */ "File '%s' not an SROFF file",
    /* 18 */ "sychronisation error - unexpected start module directive - file position=%ld",
    /* 19 */ "Unexpected End-of-File while copying module",
    /* 20 */ "Invalid SROFF format found",
};

int errmax = sizeof(errmsg) / sizeof(char *);
