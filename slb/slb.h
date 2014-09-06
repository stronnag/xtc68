/*
 *     s l b _ h
 *
 * This is the main header file for the slb SROFF librarian
 *
 * (c) Copyright 1991 David J. Walker
 *     Email:  d.j.walker.lon4905@oasis.icl.co.uk
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
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  01 Sep 92   DJW   - The -k flag added.
 *    (v2.8) 
 *  13 Oct 92   DJW   - The -U flag added.
 *                    - The -o flag added.
 *    (v2.9)          - Vectors added for symbol string comparison
 */
#ifndef EXTERN
#define EXTERN extern
#endif
#ifndef INIT
#define INIT(param)
#endif

EXTERN char    _prog_name[]  INIT("slb");
EXTERN char    _version[]    INIT("v2.9a");
EXTERN char    _copyright[]  INIT("SROFF librarian");
EXTERN char    copyright[]   INIT("(c) 1991 David J Walker");

#ifndef __GNUC__
#include <ansi.h>
#endif

#ifdef _MINIX
#define chanid_t  long
#define jobid_t  long
#define datareg_t long
#define addreg_t  char *
#define timeout_t long
#endif /* _MINIX */
#ifdef QDOS
#include <qdos.h>
#endif /* QDOS */

//#include <debug.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

#define PUBLIC  extern
#define PRIVATE static
#ifdef QDOS
#define DEFAULT_EXT "_o"
#else
#define DEFAULT_EXT ".o"
#endif
#define WORKNAME    "slb_workfile"

/*
 *  The default for SLB is to ignore the case of all names as this is
 *  the style of QDOS linkers.   However, case can be taken into account
 *  simply by redefining the stricmp() calls to be equivalent to strcmp().
 */
#ifdef _MINIX
#define stricmp strcmp
#endif /* _MINIX */

/*
 *  SROFF directives
 */
#define SROFF_FLAG    0xFB
#define SROFF_SOURCE  0x01
#define SROFF_COMMENT 0x02
#define SROFF_ORG     0x03
#define SROFF_SECTION 0x04
#define SROFF_OFFSET  0x05
#define SROFF_XDEF    0x06
#define SROFF_XREF    0x07
#define SROFF_DEFINE  0x10
#define SROFF_COMMON  0x12
#define SROFF_END     0x13

/*--------------------------------------------------------------------------
 *
 *  Structures that define the lists and binary trees that
 *  are used by this program.
 *
 *--------------------------------------------------------------------------/

/*
 *  The generalised common part of a binary tree
 *  (or list - but then not all fields used)
 */
struct NODE {
    struct NODE *owner, *prev, *next;      /* Links to other levels */
    char  *name;                            /* name of this node */
    };
typedef struct NODE   NODESTRUCT;
typedef struct NODE * NODEPTR;
typedef NODEPTR * NODEBASE;

/*
 *  A list of file/module names
 */
struct FILELIST {                   /* Base of list of module names */
        struct NODE node;
        char  * module;
        };
typedef struct FILELIST   LISTSTRUCT;
typedef struct FILELIST * LISTPTR;
EXTERN  LISTPTR filelist    INIT((LISTPTR)NULL);    /* list of files to process */
EXTERN  LISTPTR listend     INIT((LISTPTR)NULL);    /* Pointer to last entry in list */
EXTERN  LISTPTR modlist     INIT((LISTPTR)NULL);    /* Modules processed */

/*
 *  The tree used to maintain an (ordered) list of files/modules processed
 */
struct FILE_NODE {
    struct  NODE  node;
    struct  NODE  modules;   
    };
typedef struct FILE_NODE    FILESTRUCT;
typedef struct FILE_NODE *  FILEPTR;
EXTERN  FILEPTR filesdone
                    INIT((FILEPTR)NULL);
/*
 *  The tree used to maintain string values
 */
struct STRING_NODE {
    struct  NODE  node;
    short   count;                  /* number of references to this string */
    };
typedef struct STRING_NODE * STRINGPTR;
EXTERN  STRINGPTR   StringSpace     INIT((STRINGPTR)NULL);
/*
 *  The tree used to maintain global definitions.
 *  For the -W and -L options it will be in XDEF order, and list of files
 *  which reference this XDEF will be maintained.
 *  For the -Y option ti will be in file/module order and the lists will
 *  be of the XDEF's and XREF's in the current module..
 */
struct XDEF_NODE {
    struct  NODE  node;
    char    *file;              /* for multiple module files */
    LISTPTR xdef;               /* The file/module(s) containing the reference
                                   OR a list of xdef's in the current module */
    LISTPTR xref;               /* The list of files which refer to the reference
                                   OR a list of xref's in the current module */
    char *section;              /* SECTION name containing the reference */
    };
typedef struct XDEF_NODE * XDEFPTR;
EXTERN  XDEFPTR XdefTree        INIT((XDEFPTR)NULL);
/*
 *  The tree used to maintain the ID reference.  It is used when 
 *  building up the local symbol table for a module.
 *  NOTE.  To make this conform to the standard format the
 *          ID's are converted to 4 character Hex strings
 */
struct ID_NODE {
    struct  NODE  node;
    char *name;                     /* The text name associated with ID */
    };
typedef struct ID_NODE   IDSTRUCT;
typedef struct ID_NODE * IDPTR;
EXTERN  IDPTR   IdTree;

/*----------------------------------------------------------------------------
 *
 *              General work areas.
 *
 *      (The actual area for these variables is defined in 'slbdecl_c')
 *
 *----------------------------------------------------------------------------*/

EXTERN short   Id;                     /* last id read */
EXTERN char    *IdText;                /* ... as text */
EXTERN unsigned long  LongWord;        /* last long word read */
EXTERN char    LongText[10];           /* ... as text */
EXTERN char    *String;                /* Last string read */
EXTERN char    Op;                     /* Last truncation operator */
EXTERN char    TruncRule[50];          /* Last truncation rule */
EXTERN char    TruncOps[100];          /* Last trunc rule op-codes */

EXTERN char    workbuf[100];
EXTERN char    fullworkname[99] INIT("");
EXTERN char   *File      INIT(NULL);    /* Name of Current File */
EXTERN char   *Module    INIT(NULL);    /* Name of Current Module */
EXTERN char   *Section   INIT(NULL);    /* Name of Current Section */
EXTERN short   IdSection INIT(0);       /* Id of current Section */
EXTERN NODEPTR OwnerNode INIT((NODEPTR)NULL);  /* Owning node set by 'Find_Node' */

EXTERN  short   printpass INIT(1);
EXTERN  short   Aflag   INIT(0);        /* Assembler analysis */
EXTERN  short   cflag   INIT(0);
EXTERN  short   dflag   INIT(0);
EXTERN  short   eflag   INIT(0);
EXTERN  short   fflag   INIT(0);
EXTERN  short   kflag   INIT(0);
EXTERN  short   Lflag   INIT(0);        /* Library order */
EXTERN  short   mflag   INIT(0);
EXTERN  short   nflag   INIT(0);
EXTERN  short   oflag   INIT(0);
EXTERN  short   rflag   INIT(0);
EXTERN  short   Sflag   INIT(0);        /* SROFF analysis */
EXTERN  short   tflag   INIT(0);
EXTERN  short   Uflag   INIT(1);        /* Treat all symbols as upper case */
EXTERN  short   vflag   INIT(0);
EXTERN  short   Wflag   INIT(0);        /* Croos References by XDEF */
EXTERN  short   xflag   INIT(0);
EXTERN  short   Yflag   INIT(0);        /* Cross references by FILE */
EXTERN  short   debug   INIT(0);
EXTERN  short   maintflag INIT(0);
EXTERN  short   analflag  INIT(0);
EXTERN  short   libflag   INIT(0);

EXTERN  short   actions;            /* Count of actions requested */
EXTERN  int     ch;                 /* last character read from library file */
EXTERN  int     incount;            /* Count of read (if vflag) since reset */
EXTERN  int     my_argc;            /* Saved value of original argc */
EXTERN  char ** my_argv;            /* Saved value of original argv */

EXTERN  char *  libname     INIT(NULL);     /* Name of library file */
EXTERN  char *  listname    INIT(NULL);     /* Name of file for list */
EXTERN  char *  modulename  INIT(NULL);     /* Name of file containing module list */
EXTERN  FILE *  libfp       INIT((FILE *)NULL);
EXTERN  FILE *  newlibfp    INIT((FILE *)NULL);
EXTERN  FILE *  listfp      INIT((FILE *)NULL);
EXTERN  FILE *  modulefp    INIT((FILE *)NULL);

EXTERN  int     PageNo      INIT (0);
EXTERN  char *  Heading     INIT ("");
EXTERN  char *  HeadTime    INIT ("");
EXTERN  int     LineNo      INIT (01);
EXTERN  int     linepos     INIT(0);        /* Current position on line */

EXTERN  int  (*string_cmp)();

#define STRCMP  (*string_cmp)
#define MAXLINE          256
#define LINES_PER_PAGE   60        /* Not including heading */
#define INDENT_SPACES    5

EXTERN  char    filemodname[100];   /* Current file/Modulename */

extern  char * errmsg[];
extern  int    errmax;

#ifdef _MINIX
#ifdef LIBDEBUG
extern  long    _oserr;
#else
EXTERN  long    _oserr      INIT(0);
#endif /* LIBDEBUG */
#endif /* _MINIX */
#ifdef _MINIX
#define poserr(param)
#endif /* _MINIX */

#include "slbproto.h"

#define DBG(p1)
#define stricmp strcasecmp
