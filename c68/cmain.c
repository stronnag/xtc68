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

/*****************************************************************************/

#include "chdr.h"
#include "expr.h"
#include "cglbdec.h"
#include "proto.h"
#include "version.h"

#ifdef SIGNAL
#include <signal.h>
#ifndef _NSIG
#ifdef NSIG
#define _NSIG NSIG
#endif /* NSIG */
#endif /* _NSIG */
#endif /* SIGNAL */

#ifdef __MWERKS__
#include <console.h>
#include <sioux.h>
#endif /* __MWERKS__ */

/*********************************************** Static Function Definitions */

#ifdef _NSIG
static void exception P_((int));

#endif /* _NSIG */
static void help_option P_((BOOL, OPTION *, const char *));
static void uniq_option P_((BOOL, OPTION *, const char *));

/********************************************************** Static Variables */

#ifdef DEBUG
static OPTSET debugopts[] = {{(const char *)"global", MEMBER(DEBUG_GLOBAL)},
                             {(const char *)"peep", MEMBER(DEBUG_PEEP)},
                             {(const char *)"expr", MEMBER(DEBUG_EXPR)},
                             {(const char *)"code", MEMBER(DEBUG_CODE)},
                             {(const char *)"register", MEMBER(DEBUG_REGISTER)},
                             {(const char *)"symbol", MEMBER(DEBUG_SYMBOL)},
                             {(const char *)"flow", MEMBER(DEBUG_FLOW)},
                             {(const char *)"all", (SETVAL)~0},
                             {(const char *)NULL, (SETVAL)0}};

#endif /* DEBUG */

static OPTENUM intopts[] = {{(const char *)"16", 1}, {(const char *)"32", 0}, {(const char *)NULL, 0}};

static OPTENUM yesnoopts[] = {{(const char *)"yes", 1}, {(const char *)"no", 0}, {(const char *)NULL, 0}};

static OPTION opts[] = {
    /*
     *       Options which affect the front-end
     */
    {(const char *)"?", help_option, {NULL}, {NULL}},
    {(const char *)"v", uniq_option, {&verbose_option}, {NULL}},
#ifdef DEBUG
    {(const char *)"debug=", set_option, {&internal_option}, {&debugopts[0]}},
#endif /* DEBUG */
    {(const char *)"error=", numeric_option, {&error_option}, {NULL}},
    {(const char *)"warn=", numeric_option, {&warn_option}, {NULL}},
    {(const char *)"maxerr=", numeric_option, {&max_error_count}, {NULL}},
    {(const char *)"align=", enumeration_option, {&align_option}, {&yesnoopts[0]}},
#ifdef ASM
    {(const char *)"asm=", enumeration_option, {&asm_option}, {&yesnoopts[0]}},
#endif /* ASM */
#ifdef EXTENSION
    {(const char *)"extension=", enumeration_option, {&extension_option}, {&yesnoopts[0]}},
#endif /* EXTENSION */
#ifdef EXTERNAL
    {(const char *)"extern=", enumeration_option, {&extern_option}, {&yesnoopts[0]}},
#ifdef FLOAT_CHECK
    {(const char *)"fcheck=", enumeration_option, {&fcheck_option}, {&yesnoopts[0]}},
#endif /* FLOAT_CHECK */
#endif /* EXTERNAL */
#ifdef FORMAT_CHECK
    {(const char *)"format=", enumeration_option, {&format_option}, {&yesnoopts[0]}},
#endif /* FORMAT_CHECK */
#ifdef ICODE
    {(const char *)"icode", uniq_option, {&icode_option}, {NULL}},
#endif /* ICODE */
    {(const char *)"int=", enumeration_option, {&short_option}, {&intopts[0]}},
    {(const char *)"lattice=", enumeration_option, {&lattice_option}, {&yesnoopts[0]}},
#ifdef LIST
    {(const char *)"list=", enumeration_option, {&listing_option}, {&yesnoopts[0]}},
#endif /* LIST */
    {(const char *)"obsolete=", enumeration_option, {&obsolete_option}, {&yesnoopts[0]}},
#ifdef PACKENUM
    {(const char *)"packenum=", enumeration_option, {&packenum_option}, {&yesnoopts[0]}},
#endif /* PACKENUM */
    {(const char *)"revbit=", enumeration_option, {&bitfield_option}, {&yesnoopts[0]}},
#ifdef TOPSPEED
    {(const char *)"topspeed=", enumeration_option, {&topspeed_option}, {&yesnoopts[0]}},
#endif /* TOPSPEED */
    {(const char *)"trad=", enumeration_option, {&trad_option}, {&yesnoopts[0]}},
    {(const char *)"uchar=", enumeration_option, {&uchar_option}, {&yesnoopts[0]}},

/*
 *       Options which affect the code generation
 */
#ifdef DEBUGOPT
    {(const char *)"g", uniq_option, {&debug_option}, {NULL}},
#endif /*DEBUGOPT */
    {(const char *)"O", uniq_option, {&optimize_option}, {NULL}},
    {(const char *)"code=", enumeration_option, {&code_option}, {&yesnoopts[0]}},
    {(const char *)"longdouble=", enumeration_option, {&longdouble_option}, {&yesnoopts[0]}},
    {(const char *)"opt=", enumeration_option, {&opt_option}, {&yesnoopts[0]}},
    {(const char *)"pointer=", enumeration_option, {&small_option}, {&intopts[0]}},
    {(const char *)"separate=", enumeration_option, {&IandD_option}, {&yesnoopts[0]}},
#ifdef TRACE
    {(const char *)"trace=", enumeration_option, {&trace_option}, {&yesnoopts[0]}},
#endif /* TRACE */
    {NULL, NULL, {NULL}, {NULL}}};

static OPTIONS optsstd = {"Common ", opts};

static OPTIONS *optsarray[] = {&optsstd,
#ifdef MC680X0
                               &opts68k,
#endif /* MC680X0 */
#ifdef INTEL_386
                               &opts386,
#endif /* INTEL_386 */
#ifdef INTEL_86
                               &opts86,
#endif /* INTEL_86 */
#ifdef TMS320C30
                               &optsc30,
#endif /* TMS320C30 */
#ifdef ARM
                               &optsarm,
#endif /* ARM */
                               (OPTIONS *)NULL};

/*****************************************************************************/

/*ARGSUSED2 */
static void uniq_option P3(BOOL, set, OPTION *, optptr, __attribute__((unused)) const char *, arg) {
  if (set) {
    *(optptr->u1.ip) = 1;
  } else {
    if (*(optptr->u1.ip)) {
      eprintf("-%s", optptr->text);
    } else {
      eprintf("\t");
    }
    eprintf("\t\t-%s%s", optptr->text, newline);
  }
}

void numeric_option P3(BOOL, set, OPTION *, optptr, const char *, arg) {
  if (set) {
    switch (*arg) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      *(optptr->u1.ip) = atoi(arg);
      break;
    default:
      break;
    }
  } else {
    eprintf("-%s%d\t\t-%sn%s", optptr->text, *(optptr->u1.ip), optptr->text, newline);
  }
}

void string_option P3(BOOL, set, OPTION *, optptr, const char *, arg) {
  if (set) {
    *(optptr->u1.sp) = arg;
  } else {
    eprintf("-%s%s\t-%sstr%s", optptr->text, *(optptr->u1.sp), optptr->text, newline);
  }
}

void enumeration_option P3(BOOL, set, OPTION *, optptr, const char *, arg) {
  OPTENUM *optenums;
  const char *seperator;
  int value;
  size_t len;

  if (set) {
    len = strlen(arg);
    for (optenums = optptr->u2.ep; optenums->text; optenums++) {
      if (memcmp(optenums->text, arg, len) == 0) {
        *(optptr->u1.ip) = optenums->val;
        return;
      }
    }
#if 0
	message (MSG_ENUMOPT, arg, optptr->text);
#endif
  } else {
    seperator = "";
    eprintf("-%s", optptr->text);
    for (value = 0, optenums = optptr->u2.ep; optenums->text; value++, optenums++) {
      if (optenums->val == *(optptr->u1.ip)) {
        eprintf("%s", (optptr->u2.ep)[value].text);
      }
    }
    eprintf("\t\t-%s", optptr->text);
    for (optenums = optptr->u2.ep; optenums->text; optenums++) {
      eprintf("%s%s", seperator, optenums->text);
      seperator = "|";
    }
    eprintf("%s", newline);
  }
}

/*ARGSUSED2 */
void set_option P3(BOOL, set, OPTION *, optptr, const char *, arg) {
  OPTSET *optset;
  const char *seperator;
  SETVAL value;
  size_t len;

  if (set) {
    value = (SETVAL)0;
    while (*arg) {
      for (optset = optptr->u2.sp; optset->text; optset++) {
        len = strlen(optset->text);
        if (memcmp(optset->text, arg, len) == 0) {
          value |= optset->val;
          if (arg[len] == ',') {
            arg++;
          }
          arg += len;
          break;
        }
      }
      if (optset->text == NULL) {
        break;
      }
    }
    *(optptr->u1.ulp) = value;
  } else {
    eprintf("-%s", optptr->text);
    seperator = "";
    for (value = (unsigned long)0, optset = optptr->u2.sp; optset->text; value++, optset++) {
      if ((pwrof2((IVAL)(optset->val & *(optptr->u1.ulp))) != -1) && (pwrof2((IVAL)optset->val) != -1)) {
        eprintf("%s%s", seperator, (optptr->u2.sp)[value].text);
        seperator = ",";
      }
    }
    seperator = "";
    eprintf("\t\t-%s{", optptr->text);
    for (optset = optptr->u2.sp; optset->text; optset++) {
      eprintf("%s%s", seperator, optset->text);
      seperator = "|";
    }
    eprintf("}%s", newline);
  }
}

/*ARGSUSED2 */
void list_option P3(BOOL, set, OPTION *, optptr, __attribute__((unused)) const char *, arg) {
  if (!set) {
    eprintf("-%s", optptr->text);
    eprintf("%s", newline);
  }
}

void chip_option P3(BOOL, set, OPTION *, optptr, __attribute__((unused)) const char *, arg) {
  if (set) {
#ifdef MULTIPLE_ASSEMBLERS
    Funcs = (struct funcs *)optptr->u1.value;
#endif /* MULTIPLE_ASSEMBLERS */
#ifdef MULTIPLE_PROCESSORS
    GFuncs = (struct genfuncs *)optptr->u2.value2;
#endif /* MULTIPLE_PROCESSORS */
  } else {
    eprintf("\t\t\t-%s%s", optptr->text, newline);
  }
}

/*ARGSUSED2 */
static void help_option P3(BOOL, set, OPTION *, optptr, __attribute__((unused)) const char *, arg) {
  OPTIONS **optpptr;

  if (set) {
    message(MSG_USAGE, PROGNAME);
    for (optpptr = optsarray; *optpptr; optpptr++) {
      message(MSG_OPTPHASE, (*optpptr)->text);
      for (optptr = (*optpptr)->opts; optptr->text; optptr++) {
        optptr->cmd(FALSE, optptr, (const char *)NULL);
      }
    }
    eprintf("%s", newline);
    exit(EXIT_FAILURE);
  }
}

/*
 *  Process a parameter option
 *
 *  For convenience with front-end CC programs, any of the
 *  parameters can optionally be preceded by 'Q'.
 */
void options P2(const char *, arg, BOOL, is_pragma) {
  OPTIONS **optpptr;
  OPTION *optptr;
  const char *s = arg;
  BOOL done = FALSE;

  s++; /* forget '-' */
  if (*s == (char)'Q') {
    s++; /* forget 'Q' */
  }
  /*
   * Search the table to try and find a match
   */
  for (optpptr = optsarray; *optpptr; optpptr++) {
    for (optptr = (*optpptr)->opts; optptr->text; optptr++) {
      size_t len = strlen(optptr->text);

      if ((memcmp(optptr->text, s, len) == 0) && ((s[len - 1] == '=') || (s[len] == 0))) {
        optptr->cmd(TRUE, optptr, &s[(int)len]);
        done = TRUE;
        break;
      }
    }
  }
  if (!done && !is_pragma) {
    message(MSG_UNKNOWNOPT, arg);
  }
}

/*
 *   Parse the command line
 */
static void commandline P2(int, argc, char **, argv) {
  int i;
  char **pptr;

#ifdef ENVVAR
  char *ptr, *p;

#endif

#ifdef EPOC
  CommandLineParameters(&argc, &argv);
#else
  /* set up default files */
  input = stdin;
  output = stdout;
  errfile = stderr;
#endif /* EPOC */

#ifdef ENVVAR
  /* Accept parameters from Environment */
  if ((ptr = getenv(ENVNAME)) != NULL) {
    for (p = ptr; (ptr = strtok(p, " \t")) != NULL; p = NULL) {
      options(ptr, FALSE);
    }
  }
#endif /* ENVAR */

  argc--;
  argv++;
  /* Accept parameters from the command line */
  for (i = 0, pptr = argv; i < argc;) {
    if (**pptr == (char)'-') {
      char **pp;
      int count;

      options(*pptr, FALSE);
      argc--;
      for (pp = pptr, count = argc - i; count > 0; count--, pp++) {
        pp[0] = pp[1];
      }
    } else {
      i++;
      pptr++;
    }
  }
  openfiles(argc, argv);
#ifdef LIST
  listfile = errfile;
#endif /* LIST */
#ifdef DEBUG
  debugfile = errfile;
#endif /* DEBUG */
}

#ifdef _NSIG
static void exception P1(int, sig) {
  message(MSG_SIGNAL, sig);
  exit(EXIT_FAILURE);
}

#endif /* _NSIG */

int main P2(int, argc, char **, argv) {
#ifdef _NSIG
  int i;

  for (i = 1; i < _NSIG; ++i)
    if (signal(i, SIG_IGN) != SIG_IGN) {
      VOIDCAST signal(i, exception);
    }
#endif /* _NSIG */

  openerror();
#ifdef __MWERKS__
  argc = ccommand(&argv);
#endif /* __MWERKS__ */
  commandline(argc, argv);

  if (verbose_option) {
    eprintf("%s v%s (%s)%s", PROGNAME, VERSION, LAST_CHANGE_DATE, newline);
  }
  /*
   * set and check some global options
   */
  if (trad_option) {
    obsolete_option = 0;
  }
#ifdef FLOAT_SUPPORT
#ifndef FLOAT_BOOTSTRAP
  {
    /* set up floating point constants used within the compiler */
    int j;

    j = 0;
    ITOF(F_zero, j);
    j = 1;
    ITOF(F_one, j);
    j = 2;
    ITOF(F_two, j);
    j = 10;
    ITOF(F_ten, j);
    FASSIGN(F_half, F_one);
    FDIV(F_half, F_two);
  }
#endif /* FLOAT_BOOTSTRAP */
#endif /* FLOAT_SUPPORT */

  initialize_types();
  initsym();
  getsym();
#ifdef CPU_DEFINED
  g_initialize();
#endif /* CPU_DEFINED */
  translation_unit();
#ifdef CPU_DEFINED
  g_terminate();
#endif /* CPU_DEFINED */
  rel_global();
#ifdef VERBOSE
  if (verbose_option) {
    message(MSG_TIMES, decl_time, parse_time, opt_time, gen_time);
    message(MSG_ERRORCNT, total_errors);
  }
#endif
  return (total_errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS);
}
