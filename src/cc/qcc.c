/*
 *						cc_c
 *						~~~~
 *
 *	 Command line front end for the C68 Compilation system
 *
 *
 * (c) Copyright 1991-1996 David J. Walker
 *	   Email:  d.j.walker@x400.icl.co.uk
 *
 *	   The purpose of this module is to act as a front end
 *	   to the C development system:
 *		  a) CPP, C68, AS68/GWAA/QMAC and LD programs for C68 for QDOS.
 *		  b) CPP, C86, AS86 and LD86 for CPOC
 *
 *	   Permission to copy and/or distribute granted under the
 *	   following conditions:
 *
 *	   1). This notice must remain intact.
 *	   2). The author is not responsible for the consequences of use
 *		   this software, no matter how awful, even if they
 *		   arise from defects in it.
 *	   3). Altered version must not be represented as being the
 *		   original software.
 *
 *	This software is based on a program originally written by Jeremy Allison.
 *	It has since been heavily modified.
 */

#include "cc.h"
#ifdef WIN32
#include <process.h>
#endif /* WIN32 */

char _prog_name[] = CC;
char _version[] = VERSION;
char _copyright[] = "Compiler driver";
char copyright[] = COPYRIGHT;

#ifdef QDOS
void (*_consetup)() = consetup_title;         /* Fancy titled screen */
int (*_cmdwildcard)() = cmdexpand;            /* Wild card expansion */
int (*_Open)(const char *, int, ...) = qopen; /* Accept names with . */
void (*_SigStart)() = NULL;
#endif /* QDOS */

#undef _P_

int pass;        /* Current Pass */
CC_PASSES_t *cp; /* Current Pass details */

int unprotoflag = 0; /* Set if UNPROTO needed */
#ifdef QL
int rllflag = 0; /* Set if RLL need to be (potentially) linked in */
int rlllib = 0;  /* Set if building RLL library */
#endif           /* QL */

struct FILE_LIST file_list = {NULL, NULL};

/*
 *	N.B.  This array must correspond to the pass_order
 *		  enum statement in the cc.h header file.
 */
CC_PASSES_t passes[] = {

    {CC, NULL, {0, 0, NULL}},     {CPP, "i", {0, 0, NULL}}, {UNPROTO, "k", {0, 0, NULL}}, {COMPILER, "s", {0, 0, NULL}},
    {ASM, "o", {0, 0, NULL}}, /* Standard (default) assembler */
#ifdef QL
#ifdef QDOS
    {"qmac", "o", {0, 0, NULL}},  /* Quanta QMAC assembler */
    {"gwass", "o", {0, 0, NULL}}, /* George GWILT special GWASS assembler */
#endif                            /* QDOS */
#endif                            /* QL */
    {LD, NULL, {0, 0, NULL}},     {LD, NULL, {0, 0, NULL}}, {LD, NULL, {0, 0, NULL}},     {"rm", NULL, {0, 0, NULL}},
#ifdef QL
#ifdef QDOS
    {"cp", NULL, {0, 0, NULL}},
#endif /* QDOS */
#endif /* QL */
    {0, NULL, {0, 0, NULL}}};

/*
 *	Options list for CC program
 *
 *	This is the complete list of options recognised by the CC program.
 *	For each option, the pass to which it relates is given, plus the
 *	action to be taken on encountering this option.  The table is
 *	searched sequentially, so if options might conflict they must be
 *	put in the table in such an order that this conflict is resolved.
 */
struct OPTIONS {
  char *identifier;
  int pass;
  int action;
} options[] = {
#ifdef QL
    {"%", CC_PASS, ACTION_ADD_STKMEM},
#endif /* QL */
#ifdef CPOC
    {"0", ASM_PASS, ACTION_ADD},
    {"1", ASM_PASS, ACTION_ADD},
#endif /* CPOC */
#ifdef QL
    {"=", CC_PASS, ACTION_ADD_STKMEM},
    {"A", ASM_PASS, ACTION_ADD},
    {"BUFL", LDPRE_PASS, ACTION_ADD},
    {"BUFP", LDPRE_PASS, ACTION_ADD},
    {"bufl", LDPRE_PASS, ACTION_ADD},
    {"bufp", LDPRE_PASS, ACTION_ADD},
#endif /* QL */
    {"CODEMOD", C68_PASS, ACTION_ADD},
    {"CRF", LDPRE_PASS, ACTION_ADD},
    {"C", CPP_PASS, ACTION_ADD},
    {"codemod", C68_PASS, ACTION_ADD},
    {"crf", LDPRE_PASS, ACTION_ADD},
    {"c", CC_PASS, ACTION_ASM_LAST},
    {"DATAMOD", C68_PASS, ACTION_ADD},
    {"DEBUG", LDPRE_PASS, ACTION_ADD_OPTIONAL},
#ifdef QL
    {"DRLL_LIBS", CPP_PASS, ACTION_RLL_DEF},
#endif /* QL */
    {"D", CPP_PASS, ACTION_ADD_MORE},
    {"datamod", C68_PASS, ACTION_ADD},
    {"debug", LDPRE_PASS, ACTION_ADD_OPTIONAL},
    {"d", CPP_PASS, ACTION_ADD},
    {"error", C68_PASS, ACTION_ADD},
    {"extens", C68_PASS, ACTION_ADD},
    {"extern", C68_PASS, ACTION_ADD},
    {"E", CPP_PASS, ACTION_CPP_LAST},
#ifdef QL
    {"fpu", CC_PASS, ACTION_FPU},
#endif /* QL */
#ifdef CPOC
    {"fp", CC_PASS, ACTION_FPU},
#endif /* CPOC */
    {"frame", C68_PASS, ACTION_ADD},
    {"f", LD_PASS, ACTION_ADD},
#ifdef QL
#ifdef QDOS
    {"GWASS", CC_PASS, ACTION_GWASS},
    {"gwass", CC_PASS, ACTION_GWASS},
#endif /* QDOS */
#endif /* QL */
    {"g", C68_PASS, ACTION_ADD},
    {"H", CC_PASS, ACTION_IGNORE},
    {"h", CC_PASS, ACTION_IGNORE},
    {"I", CPP_PASS, ACTION_ADD_MORE},
    {"icode", C68_PASS, ACTION_ADD},
    {"i", CPP_PASS, ACTION_ADD_MORE},
#ifdef CPOC
    {"j", ASM_PASS, ACTION_ADD},
#endif
    {"L", LDPRE_PASS, ACTION_ADD_MORE},
    {"i", CPP_PASS, ACTION_ADD},
    {"lattice", C68_PASS, ACTION_ADD},
    {"list", C68_PASS, ACTION_ADD},
    {"l", LDPOST_PASS, ACTION_ADD_FILES},
    {"MM", CPP_PASS, ACTION_ADD},
    {"M", CPP_PASS, ACTION_ADD},
    {"maxerr", C68_PASS, ACTION_ADD},
    {"m", LDPRE_PASS, ACTION_ADD},
#ifdef QL
    {"N", ASM_PASS, ACTION_ADD},
#endif /* QL */
#ifdef OBSOLETE
    {"noformat", C68_PASS, ACTION_ADD},
    {"noopt", C68_PASS, ACTION_ADD},
    {"nopeep", C68_PASS, ACTION_ADD},
    {"noreg", C68_PASS, ACTION_ADD},
    {"nostdinc", CPP_PASS, ACTION_ADD},
    {"nostackopt", C68_PASS, ACTION_IGNORE},
    {"nowarn", C68_PASS, ACTION_ADD},
#endif /* OBSOLETE */
    {"O", C68_PASS, ACTION_ADD},
    {"o", LDPRE_PASS, ACTION_ADD_MORE},
    {"o", AS68_PASS, ACTION_ADD_MORE},
    {"P", CPP_PASS, ACTION_ADD},
    {"pedantic", CPP_PASS, ACTION_ADD},
    {"peep", C68_PASS, ACTION_ADD},
    {"p", CC_PASS, ACTION_CPP_LAST},
    {"opt", C68_PASS, ACTION_ADD},
#ifdef QL
#ifdef QDOS
    {"QMAC", CC_PASS, ACTION_QMAC},
    {"QTARGET", CC_PASS, ACTION_TARGET},
    {"Qtarget", CC_PASS, ACTION_TARGET},
#endif /* QDOS */
#endif /* QL */
    {"Qtrad", C68_PASS, ACTION_TRAD},
#ifdef QL
    {"Quchar", C68_PASS, ACTION_UCHAR},
#endif /* QL */
    {"Q", C68_PASS, ACTION_ADD_REST},
#ifdef QL
#ifdef QDOS
    {"qmac", CC_PASS, ACTION_QMAC},
#endif /* QDOS */
#endif /* QL */
    {"reg", C68_PASS, ACTION_ADD},
#ifdef QL
    {"R", LDPRE_PASS, ACTION_RLL_LIB},
    {"r", CC_PASS, ACTION_RLL},
#endif /* QL */
    {"SYM", LDPRE_PASS, ACTION_ADD},
    {"S", CC_PASS, ACTION_C68_LAST},
    {"separate", C68_PASS, ACTION_ADD},
    {"short", C68_PASS, ACTION_ADD},
    {"small", C68_PASS, ACTION_ADD},
    {"stackcheck", C68_PASS, ACTION_ADD},
    {"stackopt", C68_PASS, ACTION_ADD},
    {"sym", LDPRE_PASS, ACTION_ADD},
    {"s", LDPRE_PASS, ACTION_ADD_MORE},
#ifdef QL
#ifdef QDOS
    {"TARGET", CC_PASS, ACTION_TARGET},
#endif /* QDOS */
#endif /* QL */
    {"TMP", CC_PASS, ACTION_TMP_ALL},
    {"TRAD", C68_PASS, ACTION_TRAD},
#ifdef QL
#ifdef QDOS
    {"target", CC_PASS, ACTION_TARGET},
#endif /* QDOS */
#endif /* QL */
    {"tmp", CC_PASS, ACTION_TMP},
    {"trace", C68_PASS, ACTION_ADD},
    {"trad", C68_PASS, ACTION_TRAD},
    {"trigraphs", CPP_PASS, ACTION_ADD},
#ifdef QL
    {"UCHAR", C68_PASS, ACTION_UCHAR},
#endif /* QL */
    {"UNPROTO", CC_PASS, ACTION_UNPROTO},
    {"U", CPP_PASS, ACTION_ADD},
#ifdef QL
    {"uchar", C68_PASS, ACTION_UCHAR},
#endif /* QL */
    {"undef", CPP_PASS, ACTION_ADD},
    {"unproto", CC_PASS, ACTION_UNPROTO},
    {"u", CC_PASS, ACTION_IGNORE},
    {"v", CC_PASS, ACTION_VERBOSE_ALL},
    {"V", CC_PASS, ACTION_VERBOSE},
    {"warn", C68_PASS, ACTION_ADD},
#ifdef QL
    {"Xa", CC_PASS, ACTION_XA},
    {"Xc", CC_PASS, ACTION_XC},
    {"Xt", CC_PASS, ACTION_XT},
#endif /* QL */
    {"Y", CC_PASS, ACTION_PATH},
    {"y", CC_PASS, ACTION_PATH},
    {NULL, 0, 0}};

char *tmp_dir = NULL;

int isname = 0; /* set as soon as filename found */
int last_pass = ASM_PASS;
int asmpass = ASM_PASS; /* Set to identify the assembler to be used */
int dolink = 1;

int verbose = 0;
int alltemp = 0;
int no_c68 = 0;         /* set to skip C68 pass */
#define ASMCPP_OPT "-P" /* Option to CPP for assembler files */
int fpflag = 0;         /* Floating Point support required */

char *ccpath = NULL; /* Path for finding CC phases */
char *ccinc = NULL;  /* Path for automatic include path parameter */
char *cclib = NULL;  /* Path for automatic library path parameter */

/*============================================================ WHAT_OPT */
struct OPTIONS *what_opt(opt)
/*				 ~~~~~~~~
 *	Routine to decide if an option is allowable.
 *
 *	Returns NULL on failure, and pointer into options array otherwise.
 *----------------------------------------------------------------------*/
char *opt;
{
  struct OPTIONS *ptr;
  int i;

  DBG(("WHAT_OPT", 0x18, "Enter: opt='%s'", opt));
  for (i = 0, ptr = &options[0]; ptr->identifier != NULL; ptr++, i++) {
    DBG(("WHAT_OPT", 0x18, "comparing '%s' with '%s'", opt, ptr->identifier));
    if (!strncmp(opt, ptr->identifier, strlen(ptr->identifier))) {
      DBG(("WHAT_OPT", 0x18, "Exit %p", ptr));
      return (ptr);
    }
  }
  DBG(("WHAT_OPT", 0x18, "Exit NULL: Option not recognised"));
  return (NULL);
}

#ifdef QDOS
/*============================================================== NO_CONTINUE */
int no_continue()
/*		~~~~~~~~~~~
 *
 * Routine to check if this compilation set should be aborted.
 *---------------------------------------------------------------------------*/
{
  extern long _endchanid;
  chanid_t chan = getchid(STDIN_FILENO);
  int errnosav = errno;
  int oserrsav = _oserr;
  char c;
  int err;

  /*
   *	If we are running under the control of another
   *	program, then simply exit quietly with error.
   */
  if (_endchanid == 0) {
    return 1;
  }
  printstr("Compilation failed\nPress <Enter> to exit, any key to continue\n", NULL);
  (void)sd_cure(chan, (timeout_t)(-1)); /* Ensure a cursor is enabled */
  if ((err = io_fbyte(chan, (timeout_t)(-1), &c)) != 0) {
    errno = errnosav;
    exit(err);
  }
  errno = errnosav;
  _oserr = oserrsav;
  return (c == '\n');
}
#endif /* QDOS */

/*================================================================= ADD_TMP */
void add_tmp(char *outname)
/*		~~~~~~~
 *
 *	Routine to add the temporary directory to
 *	a filename when it is needed.
 *
 *	N.B. The assumption is made that there is always
 *		enough space in the 'outname' parameter to
 *		hold the resulting filename.
 *---------------------------------------------------------------------------*/
{
  DBG(("ADD_TMP", 0x11, "enter: tmp_dir='%s', outname='%s'", tmp_dir, outname));

  if (tmp_dir && *tmp_dir) {
    char tmpname[MAX_FNAME];
#ifdef QDOS
    int extra;
#endif /* QDOS */

    DBG(("ADD_TMP", 0x14, "setting temp directory to '%s'", tmp_dir));
    (void)strcpy(tmpname, tmp_dir);
#ifdef QDOS
    if (isdevice(outname, &extra) && strchr(outname, '_')) {
      DBG(("ADD_TMP", 0x14, "removing device name from '%s'", outname));
      (void)strcat(tmpname, (char *)((long)strchr(outname, '_') + 1));
    } else {
      (void)strcat(tmpname, outname);
    }
    DBG(("ADD_TMP", 0x14, " ...giving '%s'", tmpname));
#else
    {
      char *p = (char *)FilePart(outname);
      AddPart(tmpname, p, MAX_FNAME);
    }
    DBG(("ADD_TMP", 0x14, " ...giving '%s'", tmpname));
#endif /* QDOS */
    (void)strcpy(outname, tmpname);
  }
  DBG(("ADD_TMP", 0x11, "exit: outname($%p)='%s'", outname, outname));
  return;
}

/*================================================== GET_EXTENSION */
char *get_extension(char *filename)
/*		~~~~~~~~~~~~~
 *	Find the extension for the filename.
 *
 *	For QDOS system we also allow for the '.' seperator
 *	as well as the normal QDOS one to identify an
 *	extension.	This makes it easier when porting
 *	software to other systems as you can use the more
 *	common '.' form in makefiles, etc, and CC will sort
 *	it all out correctly.
 *
 *	Return pointer to first character of extension
 *	(after the separator character).
 *-----------------------------------------------------------------*/
{
  char *p;
  int x;

#ifdef QDOS
  if ((p = strrchr(filename, '.')) == NULL)
#endif
    p = strrchr(filename, DOT);

  x = strlen(filename) - (p - filename);

  if (p == NULL || x < 2 || x > 4) {
    printstr("Filename '", filename, "' has no extension\n", NULL);
    exit(ERR_BP);
  }
#ifdef QDOS
  if (*p == '.') /* Is seperator a fullstop ? */
  {
    *p = '_'; /* ... yes then change to underscore */
  }
#endif /* QDOS */
  return ++p;
}

/*==================================================== SET_EXTENSION */
void set_extension(char *outname, char *suffix)
/*		~~~~~~~~~~~~~
 *	Ensure that the extension is set to the
 *	value required.
 *-------------------------------------------------------------------*/
{
  (void)strcpy(get_extension(outname), suffix);
  return;
}

/*=========================================================== FREE_ARGS */
void free_passargs(PASS_OPTS_t *opts)
/*		~~~~~~~~~
 *	Free all the resources associated with the given PASS_OPTS
 *	structure, and reset it as an empty one.
 *----------------------------------------------------------------------*/
{
  char **argv;

  DBG(("FREE_ARGS", 0x14, "ENTER: opts = $%x", opts));
  if (opts->argv != NULL) {
    for (argv = opts->argv; *argv; argv++) {
      DBG(("FREE_ARGS", 0x18, "Freeing argument at $%x", *argv));
      free(*argv);
    }
    DBG(("FREE_ARGS", 0x18, "Freeing argv array at $%x", opts->argv));
    free(opts->argv);
    opts->argv = NULL;
    opts->argcmax = 0;
    opts->argc = 0;
  }
  DBG(("FREE_ARGS", 0x14, "EXIT"));
  return;
}

/*================================================================= ADD_ARGS */
void add_args(PASS_OPTS_t *optptr, char *p)
/*		~~~~~~~~
 *
 *	Add an new argument to the given array of pass options.
 *	The buffers will be expanded if necessary to make enough room.
 *
 *	The GWASS program is special in that options are all concatenated
 *	into a "single" option.  All other programs use a standard argv[]
 *	style array to store the options.
 *-------------------------------------------------------------------------*/
{
  char **argv;
  size_t oldsize; /* Size of current command line */

  DBG(("ADD_ARGS", 0x14, "Enter: optptr = $%x, argv=$%x, argc=%d; p='%s'", optptr, optptr->argv, optptr->argc, p));
#ifdef LIBDEBUG
  {
    int x;

    for (x = 0; x < optptr->argc; x++) {
      DBG(("ADD_ARGS", 0x18, "argv[%d] at $%x = %s", x, optptr->argv[x], optptr->argv[x]));
    }
  }
#endif /* LIBDEBUG */
  /*
   *	See if argv array needs expanding.	If so do it.
   */
  if (optptr->argc >= optptr->argcmax) {
    DBG(("ADD_ARGS", 0x14, "expanding argv by %d entries (argc=%d, argcmax=%d)", PASS_OPTS_INCR, optptr->argc,
         optptr->argcmax));
    optptr->argcmax += PASS_OPTS_INCR;
    optptr->argv = realloc(optptr->argv, (optptr->argcmax + 1) * sizeof(char *));
    if (optptr->argv == NULL) {
      printstr("Failure extending option buffers\n", NULL);
      exit(ERR_OM);
    }
    DBG(("ADD_ARGS", 0x14, "new expanded argv array at $%x", optptr->argv));
  }

  /*
   *	Ensure current option and next one are NULL before proceeding
   */
  argv = &optptr->argv[optptr->argc];
  DBG(("ADD_ARGS", 0x18, "next argument at $%x", argv));
  optptr->argc += 1; /* Increment count used */
  argv[0] = NULL;    /* Ensure current entry is NULL */
  argv[1] = NULL;    /* ... and next one in argv style */

  oldsize = 0;
#if defined(QL) && defined(QDOS)

  /*
   *	For the GWASS assembler,
   *	we only build up a single parameter string.
   */
  DBG(("ADD_ARGS", 0x18, "&(passes[GWASS_PASS].p_optbuf)=$%x", &(passes[GWASS_PASS].p_optbuf)));
  if (pass == GWASS_PASS) {
    DBG(("ADD_ARGS", 0x14, "GWASS pass, argc=%d", optptr->argc));
    if (optptr->argc > 1) {
      argv = &optptr->argv[1];
      oldsize = strlen(*argv);
      optptr->argc = 2;
      DBG(("ADD_ARGS", 0x14, "GWASS pass, argc=%d, argv[1]='%s'", optptr->argc, argv));
    }
  }
#endif /* QL */
  /*
   *	Extend string to new required length
   */
  DBG(("ADD_ARGS", 0x14, "old string of length %d at $%x", oldsize, argv[0]));
  *argv = realloc(*argv, oldsize + strlen(p) + 2);
  DBG(("ADD_ARGS", 0x14, "new string of length %d at $%x", oldsize + strlen(p), argv[0]));
  if (argv[0] == NULL) {
    printstr("Failure extending option buffers\n", NULL);
    exit(ERR_OM);
  }
  /*
   *	Ensure old size cleanly terminated (also allows for
   *	zero length strings), and then add new string
   */
  (*argv)[oldsize] = '\0'; /* Ensure NULL terminated */
  (void)strcat(*argv, p);  /* Add new value */

  DBG(("ADD_ARGS", 0x14, "Exit:  argv=$%x, argc=%d, new option buffer at $%x='%s'", optptr->argv, optptr->argc, *argv, *argv));
  return;
}

/*============================================================ COPY_ARGS */
void copy_args(PASS_OPTS_t *opts_to, PASS_OPTS_t *opts_from)
/*		~~~~~~~~~
 *	Copy all the arguments from one array to another one.
 *	This is typeically used to build up the command line
 *	array that is required.
 *-----------------------------------------------------------------------*/
{
  char **argv;

  assert(opts_to != NULL);
  assert(opts_from != NULL);

  DBG(("COPY_ARGS", 0x11, "Enter: opts_to=$%x, opts_from=$%x", opts_to, opts_from));
  for (argv = opts_from->argv; argv && *argv; argv++) {
    add_args(opts_to, *argv);
  }
  DBG(("COPY_ARGS", 0x11, "Exit"));
  return;
}

/*=========================================================== COMMAND_LINE */
int command_line(CC_PASSES_t *preopts, int thispass, CC_PASSES_t *postopts, char *inname, char *outname)
/*
 *	This option is used to work out a command line from its various
 *	components and then call the underlying program with the
 *	correct arguments.
 *
 *	Used to print a command line if verbose specified.
 *	The strings supplied or printed space separated.
 *-------------------------------------------------------------------------*/
{
  CC_PASSES_t *passopts;
  PASS_OPTS_t current_pass;
  int ret;
  char *cmdbuf = NULL;
  char progname[256];

  DBG(("COMMAND_LINE", 0x14, "preopts=$%x, thispass=%d, postopts=$%x", preopts, thispass, postopts));
  DBG(("COMMAND_LINE", 0x14, "inname='%s'", inname));
  DBG(("COMMAND_LINE", 0x14, "outname='%s'", outname));

  passopts = &passes[thispass];
  DBG(("COMMAND_LINE", 0x14, "p_name='%s'", passopts->p_name, pass));
  DBG(("COMMAND_LINE", 0x18, "passopts->argv[0]=$%x, value='%s'", passopts->p_optbuf.argv[0], passopts->p_optbuf.argv[0]));

  assert(pass >= CC_PASS && pass < MAX_PASS);
  assert(preopts == NULL || strcmp(passopts->p_name, preopts->p_name) == 0);
  assert(postopts == NULL || strcmp(passopts->p_name, postopts->p_name) == 0);

  current_pass.argc = 0;
  current_pass.argcmax = 0;
  current_pass.argv = NULL;
  (void)strcpy(progname, (ccpath != NULL && *ccpath) ? ccpath : "");
  (void)strcat(progname, passopts->p_name);
#ifdef EPOC
  (void)strcat(progname, ".IMG");

  {
    int fd;

    if ((fd = open(progname, O_RDONLY)) < 0) {
      printstr("Unable to locate program ", progname, "\n", NULL);
      return -1;
    }
    close(fd);
  }
#endif /* EPOC */

  DBG(("COMMAND_LINE", 0x14, "add program name to arguments"));
  add_args(&current_pass, progname);
  if (preopts != NULL) {
    DBG(("COMMAND_LINE", 0x14, "add preopt args to arguments"));
    copy_args(&current_pass, &preopts->p_optbuf);
  }
  if (passopts->p_optbuf.argv && *(passopts->p_optbuf.argv[0])) {
    DBG(("COMMAND_LINE", 0x14, "add passopts args to arguments"));
    copy_args(&current_pass, &passopts->p_optbuf);
  }
  /*
   *	See if input name supplied, and
   *	if so add to arguments
   */
  if (inname != NULL) {
    DBG(("COMMAND_LINE", 0x14, "add inname to arguments"));
    add_args(&current_pass, inname);
  }
  /*
   *	See if output name supplied
   */
  if (outname != NULL) {
#if defined(QL) && defined(QDOS)
    /*
     *	For the GWASS assembler,
     *	no output name should have been specified.
     */
    if (thispass == GWASS_PASS) {
      DBG(("COMMAND_LINE", 0x14, "GWASS pass:  outname = '%s'", outname));
      (void)thispass;
    } else {
#endif /* QL */
      /*
       *	Certain passes need a -o preceeding the name.
       */
      switch (thispass) {
#ifndef QL
#if defined(__unix__) || defined(__APPLE__)
      case CPP_PASS:
#endif /* __unix__ */
      case ASM_PASS:
        add_args(&current_pass, "-o");
        break;
#endif /* QL */
      default:
        break;
      }
      DBG(("COMMAND_LINE", 0x14, "add outname to arguments"));
      add_args(&current_pass, outname);
#if defined(QL) && defined(QDOS)
    }
#endif /* QL */
  }

  if (postopts != NULL) {
    DBG(("COMMAND_LINE", 0x14, "add postopts args to arguments"));
    copy_args(&current_pass, &postopts->p_optbuf);
  }

  DBG(("COMMAND_LINE", 0x14, "progname=%s", progname));
  DBG(("COMMAND_LINE", 0x14, "argv[0]=$%x (%s)", current_pass.argv[0], current_pass.argv[0]));

#ifndef WIN32
#ifndef QDOS
  cmdbuf = argpack(current_pass.argv, 1);
  if (cmdbuf == NULL) {
    DBG(("COMMAND_LINE", 0x11, "Exit: out of memory for command buffer"));
    ret = errno;
  }
#endif /* QDOS */
#endif /* WIN32 */
  /*
   *	If verbose mode required, then print
   *	out command line.  Any escape characters,
   *	etc that would be needed must be added.
   */
  if (verbose) {
#ifdef WIN32
    cmdbuf = argpack(current_pass.argv, 0);
#endif
#ifdef QDOS
    cmdbuf = argpack(current_pass.argv, 1);
#endif
    printstr(" ", cmdbuf, "\n", NULL);
  }

#ifdef WIN32
  /*
   *	For some reason, the WIN32 environment does not seem to
   *	pass back the result of running the program if the system()
   *	call is used - the result is always 0!
   */
  ret = _spawnvp(_P_WAIT, current_pass.argv[0], current_pass.argv);
#elif QDOS
  /*
   *	On QDOS we can use the execv() family of routines as these
   *	effectively act like a system call, but without the need
   *	to build up the command line to pass to it.
   */
  ret = execvp(current_pass.argv[0], (int *)-1L, current_pass.argv);
#else
  /*
   *	The most portable form is the system() call.
   */
  ret = system(cmdbuf);
#endif

  free(cmdbuf);
  free_passargs(&current_pass);
  DBG(("COMMAND_LINE", 0x11, "Exit: ret=%d", ret));
  return (ret);
}

/*================================================================== USEAGE */
void useage()
/*		~~~~~~
 *	Simply print a usage message.
/*--------------------------------------------------------------------------*/
{
  (void)printstr("useage: ", CC, " [options] filename[s]\n", NULL);
  exit(-1);
}

/*================================================================= ADD_OPT */
void add_option(pass_opt, p)
    /*		~~~~~~~~~~
     *
     *	Add an new option to a passes options.
     *	The buffers will be expanded if necessary to make enough room.
     *
     *	The GWASS program is special in that options are all concatenated
     *	into a "single" option.  All other programs use a standard argv[]
     *	style array to store the options.
     *-------------------------------------------------------------------------*/
    int pass_opt;
char *p;
{
  DBG(("ADD_OPT", 0x14, "pass = %d; p='%s'", pass_opt, p));

  /*
   *	Get parameter area for this pass
   */
  add_args(&passes[pass_opt].p_optbuf, p);
  return;
}

/*============================================================== PARAMETERS */
void Parameters(int argc, char *argv[])
/*		~~~~~~~~~~
 *	Process a set of parameters.  These may have been passed via the
 *	command line, or via an environment variable - we do not care.
 *--------------------------------------------------------------------------*/
{
  char workbuf[100];
  char *argvsf;
  int sf;

  DBG(("PARAMETERS", 0x11, "Enter: argc=%d", argc));
  for (sf = 1; sf < argc; sf++) {
    struct OPTIONS *optptr;

    DBG(("PARAMETERS", 0x12, "parameter parsing 'sf' loop - argv[sf]='%s'", argv[sf]));
    if (*argv[sf] != '-') {
      struct FILE_LIST *ptr;
      DBG(("PARAMETERS", 0x12, "filename - adding to list"));
      isname++;
    ADD_FILELIST:
      for (ptr = &file_list; ptr->next; ptr = ptr->next)
        ;
      DBG(("PARAMETERS", 0x24, "file_list starts at %p, finishes at %p", &file_list, ptr));
      ptr->next = malloc(sizeof(struct FILE_LIST));
      DBG(("PARAMETERS", 0x24, "Added new FILE_LIST structure at address %p", ptr->next));
      if ((ptr = ptr->next) == NULL) {
        DBG(("PARAMETERS", 0x21, "Failed to allocate FILE_LIST structure"));
        exit(ERR_OM);
      }
      ptr->next = NULL;
      ptr->name = argv[sf];
      continue; /* Skip filenames */
    }

    /* Stop after getting options */
    argvsf = argv[sf];

    /*
     *	Identify the option.
     *	The value of returned as an index into
     *	the option array.
     */
    DBG(("PARAMETERS", 0x28, "last_pass = %d", last_pass));
    if ((optptr = what_opt(&argvsf[1])) == NULL) {
      DBG(("PARAMETERS", 0x12, "   option not recognised - so ignored"));
      printstr("option '", argvsf, "' not recognised - ignored\n", NULL);
      continue;
    }
    DBG(("PARAMETERS", 0x12, " action=%d, pass=%d", optptr->action, optptr->pass));
    switch (optptr->action) {

#ifdef QL
    case ACTION_ADD_STKMEM:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD_STKMEM)"));
      argvsf++; /* Skip over - character */
      add_option(CPP_PASS, argvsf);
      add_option(C68_PASS, argvsf);
      add_option(ASM_PASS, argvsf);
      add_option(LD_PASS, argvsf);
      break;
#endif /* QL */

    case ACTION_ADD_ALL:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD_ALL)"));
      add_option(CPP_PASS, argvsf);
      add_option(C68_PASS, argvsf);
      add_option(ASM_PASS, argvsf);
      add_option(LD_PASS, argvsf);
      break;

    case ACTION_ADD:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD)"));
      add_option(optptr->pass, argvsf);
      break;

    case ACTION_ADD_FILES:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD_FILEIST)"));
      goto ADD_FILELIST;

    case ACTION_ADD_REST:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD_REST)"));
      (void)strcpy(workbuf, "-");
      add_option(optptr->pass, strcat(workbuf, argvsf + 2));
      break;

    case ACTION_ADD_MORE:
#ifdef QL
    ADD_MORE:
#endif /* QL */
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD_MORE)"));
      /*
       *	There must be more (typically a filename).
       *	If necessary get the next parameter
       */
      if (argvsf[2] == '\0') {
        (void)strcpy(workbuf, argvsf);
        argvsf = argv[++sf];
#ifdef CPOC
        add_option(optptr->pass, workbuf);
        workbuf[0] = '\0';
#endif /* CPOC */
        (void)strcat(workbuf, argvsf);
        add_option(optptr->pass, workbuf);
        break;
      }
      add_option(optptr->pass, argvsf);
      break;

    case ACTION_ADD_OPTIONAL:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ADD_OPTIONAL)"));
      add_option(optptr->pass, argvsf);
      if (*(argv[sf + 1]) != '-') {
        add_option(optptr->pass, argv[++sf]);
      }
      break;

    case ACTION_IGNORE:
      DBG(("PARAMETERS", 0x12, "   (ACTION_IGNORE)"));
      break;

    case ACTION_TMP_ALL:
      DBG(("PARAMETERS", 0x12, "   (ACTION_TMP_ALL)"));
      alltemp = 1;
      /* FALLTHRU */
    case ACTION_TMP: /* Define temp directory */
      DBG(("PARAMETERS", 0x12, "   (ACTION_TMP)"));
      /* Ensure directory ends in '_' */
      if (argvsf[strlen(argvsf) - 1] != '_')
#ifdef QDOS
      {
        printstr("Temporary directory (", argvsf, ") must end in _\n", NULL);
        exit(ERR_BP);
      }
#endif /* QDOS */
      tmp_dir = &argvsf[4];
      DBG(("PARAMETERS", 0x12, "   tmp_dir=%s", tmp_dir));
      break;

    case ACTION_VERBOSE_ALL:
      DBG(("PARAMETERS", 0x12, "   (ACTION_VERBOSE_ALL)"));
      verbose++;
#ifndef CPOC
      add_option(CPP_PASS, argvsf);
#endif /* CPOC */
      add_option(C68_PASS, argvsf);
#ifndef CPOC
      add_option(ASM_PASS, argvsf);
      add_option(LDPRE_PASS, argvsf);
#endif /* CPOC */
      break;

    case ACTION_VERBOSE:
      DBG(("PARAMETERS", 0x12, "   (ACTION_VERBOSE)"));
      verbose++;
      break;

    case ACTION_CPP_LAST:
      DBG(("PARAMETERS", 0x12, "   (ACTION_CPP_LAST)"));
#if 0
				add_option (CPP_PASS, argvsf);
#endif
      last_pass = CPP_PASS;
      dolink = 0;
      break;

    case ACTION_C68_LAST:
      DBG(("PARAMETERS", 0x12, "   (ACTION_C68_LAST)"));
      last_pass = C68_PASS;
      dolink = 0;
      break;

    case ACTION_ASM_LAST:
      DBG(("PARAMETERS", 0x12, "   (ACTION_ASM_LAST)"));
      last_pass = ASM_PASS;
      dolink = 0;
      break;

    case ACTION_TRAD:
      DBG(("PARAMETERS", 0x12, "   (ACTION_TRAD)"));
      add_option(CPP_PASS, "-U__STDC__");
      add_option(optptr->pass, argvsf);
      break;

    case ACTION_FPU:
      DBG(("PARAMETERS", 0x12, "   (ACTION_FPU)"));
      fpflag++;
#ifdef CPOC
      break;
#endif /* CPOC */

#ifdef QL
#ifdef QDOS
      if (CheckCPU() < 0x20) {
        printstr("The '-fpu' option needs the GWASS assembler\n", NULL);
        printstr("The GWASS assembler requires a 68020 or better\n", NULL);
        exit(-1);
      }
      /* FALLTHRU */

    case ACTION_GWASS:
      DBG(("PARAMETERS", 0x12, "   (ACTION_GWASS)"));
      if (CheckCPU() < 0x20) {
        printstr("The GWASS assembler requires a 68020 or better\n", NULL);
        exit(-1);
      }
      add_option(C68_PASS, "-qmc68k");
      add_option(GWASS_PASS, "D/197/Q//A/1/");
      asmpass = GWASS_PASS;
      passes[C68_PASS].p_suffix = "asm";
      break;

    case ACTION_QMAC:
      DBG(("PARAMETERS", 0x12, "   (ACTION_QMAC)"));
      asmpass = QMAC_PASS;
      passes[C68_PASS].p_suffix = "asm";
      break;

    case ACTION_TARGET:
      DBG(("PARAMETERS", 0x12, "    (ACTION_TARGET)"));
      (void)strcpy(workbuf, "-");
      add_option(C68_PASS, strcat(workbuf, argvsf[1] == 'Q' ? &argvsf[2] : &argvsf[1]));
      break;

#endif /* QDOS */
#endif /* QL */

    case ACTION_PATH:
      DBG(("PARAMETERS", 0x12, "   (ACTION_PATH)"));
      ccpath = &argvsf[2];
      break;
#ifdef QL
    case ACTION_RLL_DEF:
      DBG(("PARAMETERS", 0x12, "    (ACTION_RLL_DEF)"));
      rllflag++;
      goto ADD_MORE;

    case ACTION_RLL_LIB:
      DBG(("PARAMETERS", 0x12, "    (ACTION_RLL_LIB)"));
      rlllib++;
      goto ADD_MORE;

    case ACTION_RLL:
      DBG(("PARAMETERS", 0x12, "    (ACTION_RLL)"));
      add_option(CPP_PASS, "-DRLL_LIBS");
      rllflag++;
      goto ADD_FILELIST;
    case ACTION_UCHAR:
      DBG(("PARAMETERS", 0x12, "   (ACTION_UCHAR)"));
      add_option(CPP_PASS, "-D__CHAR_UNSIGNED__");
      add_option(optptr->pass, argvsf);
      break;

    case ACTION_UNPROTO:
      DBG(("PARAMETERS", 0x12, "   (ACTION_UNPROTO)"));
      unprotoflag++;
      break;

    case ACTION_XA:
      add_option(LDPRE_PASS, "-lxa");
      add_option(CPP_PASS, "-D__STDC__=0");
      add_option(CPP_PASS, "-trigraphs");
      break;
    case ACTION_XC:
      add_option(C68_PASS, "-obsolete");
      add_option(CPP_PASS, "-pedantic");
      add_option(CPP_PASS, "-D__STRICT_ANSI__= ");
      add_option(LDPRE_PASS, "-lxc");
      add_option(CPP_PASS, "-D__STDC__=1");
      add_option(CPP_PASS, "-trigraphs");
      break;
    case ACTION_XT:
      add_option(LDPRE_PASS, "-lxt");
      add_option(CPP_PASS, "-D__STDC__=0");
      break;
#endif /* QL */
    default:
      DBG(("PARAMETERS", 0x12, "   (default)"));
      useage();
      break;
    } /* end of 'index' switch */
    /*
     *	Option has been eaten - mark space unused
     */
    argv[sf] = NULL;
  } /* end of 'options' for loop */
  return;
}

/*================================================== ENVIRONMENT_PARAMETERS */
static void Environment_Parameters(char *argv[])
/*		~~~~~~~~~~~~~~~~~~~~~~
 *	This routine is responsible for getting the CC_OPTS environment
 *	variable, and then using it to set up data structures that look
 *	like the 'argc' and 'argv' ones passed to main(), before calling
 *	the shared parameter analysis routine.
 *-------------------------------------------------------------------------*/
{
#ifdef ENVVAR
  char *cc_opts;
  int envc;
  char **envv;

#ifdef QL
  cc_opts = getenv("CC_OPTS");
#endif /* QL */
#ifdef CPOC
  cc_opts = getenv("CPOC_OPTS");
#endif /* CPOC */
  if (cc_opts == NULL || *cc_opts == '\0') {
    return;
  }
  /*
   *	Initialise vector and count
   */
  envc = 1;
  envv = (char **)malloc(2 * sizeof(char *));
  envv[0] = argv[0];
  envv[1] = NULL;

  /*
   *	Now parse the Environment Variable, splitting it
   *	up into NULL terminated strings and building the
   *	vector array.
   */
  for (;;) {
    char sc;
    /*
     *	If we have any leading whitespace characters
     *	then we need to skip over them.
     */
    while (*cc_opts == ' ') {
      cc_opts++;
    }
    if (*cc_opts == '\0') {
      break;
    }
    sc = ' ';
    /*
     *	Look for any of the characters which are
     *	stored, but treated specially.
     */
    switch (*cc_opts) {
    case '\'':
    case '"':
      /* Next argument is string - store terminator */
      sc = *cc_opts++;
      break;
    case '\\':
      cc_opts++;
      break;
    }
    /*
     *	There is at least one more element, so add this
     *	new element to the vector array.
     */
    envv = (char **)realloc(envv, (size_t)((envc + 2) * sizeof(char *)));
    envv[envc++] = cc_opts;
    envv[envc] = NULL;
    /*
     * Parse until terminating character reached
     * The terminating character is changed to a NULL byte.
     * Allow for premature termination by NULL byte.
     */
    for (;; cc_opts++) {
      if (*cc_opts == '\0') {
        break;
      }
      if (*cc_opts == sc) {
        *cc_opts++ = '\0';
        break;
      }
    }
  } /* End of main for loop */

  Parameters(envc, envv);
#endif /* ENVVAR */
  return;
}

/*==================================================== ENVIRONMENT_VARIABLES */
void Environment_Variables(void)
/*		~~~~~~~~~~~~~~~~~~~~~
 *	This routine is used to read various environment variables and set a
 *	global variable appropriately depending on whether the environment
 *	variable was set or not.
 *
 *	N.B.  It is important that the value of an environment variable is
 *		  copied to a new string as changing any environment variable
 *		  can cause the others to be moved.
 *--------------------------------------------------------------------------*/
{
  static struct {
    char *name;
    char **result;
    char *deflt;
  } * ptr, envtable[] = {{"TMP", &tmp_dir, ""},
                         {"TEMP", &tmp_dir, ""},
#ifdef QL
                         {"CC_PATH", &ccpath, ""},
                         {"CC_CPP", &passes[CPP_PASS].p_name, CPP},
                         {"CC_C68", &passes[C68_PASS].p_name, "c68"},
                         {"CC_ASM", &passes[ASM_PASS].p_name, "as68"},
                         {"CC_LD", &passes[LD_PASS].p_name, LD},
                         {"CC_INC", &ccinc, NULL},
                         {"CC_LIB", &cclib, NULL},
#endif /* QL */
#ifdef CPOC
                         {"CPOC_PATH", &ccpath, ""},
                         {"CPOC_CPP", &passes[CPP_PASS].p_name, CPP},
                         {"CPOC_C86", &passes[C68_PASS].p_name, "c86"},
                         {"CPOC_ASM", &passes[ASM_PASS].p_name, "as86"},
                         {"CPOC_LD", &passes[LD_PASS].p_name, LD},
                         {"CPOC_INC", &ccinc, ""},
                         {"CPOC_LIB", &cclib, ""},
#endif /* CPOC */
                         {NULL, NULL, NULL}};

  DBG(("ENVIRONMENT_VARIABLES", 0x21, "Enter"));
  for (ptr = &envtable[0]; ptr->name != NULL; ptr++) {
#ifdef ENVVAR
    char *result;

    DBG(("ENVIRONMENT_VARIABLES", 0x24, "looking for %s", ptr->name));
    if ((result = getenv(ptr->name)) != NULL && strlen(result)) {
      DBG(("ENVIRONMENT_VARIABLES", 0x24, "...found, value='%s' stored at 0x%p", result, ptr->result));
      *(ptr->result) = strdup(result);
    } else {
#endif /* ENVVAR */
      DBG(("ENVIRONMENT_VARIABLES", 0x24, "... not found, default='%s' stored at 0x%p", ptr->deflt, ptr->result));
      *(ptr->result) = ptr->deflt;
#ifdef ENVVAR
    }
#endif /* ENVVAR */
  }
  DBG(("ENVIRONMENT_VARIABLES", 0x21, "Exit"));
  return;
}

/*==================================================================== MAIN */
int main(argc, argv)
/*		~~~~
 *
 * Main routine to do the compilations, managing all arguments.
 **
 *--------------------------------------------------------------------------*/
int argc;
char *argv[];
{
  struct FILE_LIST *nameptr;
  char inname[MAX_FNAME], outname[MAX_FNAME], workbuf[MAX_FNAME];
  char *p, **ptrptr;
  int ret;

  DBG_INIT();
#ifdef QDOS
  if (iscon(getchid(1), 0) && !isnoclose(1)) {
    verbose++;
  }
#endif /* QDOS */

  Environment_Variables();
  Environment_Parameters(argv);
  Parameters(argc, argv);
  DBG(("MAIN", 0x21, "Finished parsing parameters"));

  if (0 != verbose) {
    DBG(("MAIN", 0x14, "CC running in VERBOSE mode"));
    printstr(" ", _prog_name, _version, " (", _copyright, ") ", copyright, "\n", NULL);
  }

#ifdef CPOC
  /*
   *	Set up mandatory parameters
   */
#ifndef QDOS
  add_option(CPP_PASS, "-D__STDC__");    /* CPP: ANSI compatible */
#endif                                   /* QDOS */
  add_option(C68_PASS, "-int=16");       /* C86: 16 bit ints */
  add_option(C68_PASS, "-pointer=16");   /* C86: 16 bit pointers */
  add_option(C68_PASS, "-separate=yes"); /* C86: seperate Instruction and Data spaces */
  add_option(C68_PASS, "-topspeed=yes"); /* C86: accept TopSpeed syntax */
  add_option(ASM_PASS, "-1");            /* AS86: Use 186 instruction set */
  add_option(ASM_PASS, "-j");            /* AS86: Long branches by default */
  add_option(ASM_PASS, "-j");            /* AS86: ... but optimise if can */
  add_option(LDPRE_PASS, "-1");          /* LD86:  use 186 instruction set */
  add_option(LDPRE_PASS, "-i");          /* LD86: ? */
#endif                                   /* CPOC */

  /*
   *	Set up parameter options controlled
   *	by other options.
   */
  if ((NULL != ccinc) && ('\0' != *ccinc)) {
    (void)strcpy(workbuf, "-I");
    (void)strcat(workbuf, ccinc);
    add_option(CPP_PASS, workbuf);
  }

  if (0 != fpflag) {
    add_option(C68_PASS, "-fpu=yes");
  }

  if ((NULL != cclib) && ('\0' != *cclib)) {
#ifdef CPOC
    add_option(LD_PASS, "-L");
    workbuf[0] = '\0';
#else
    (void)strcpy(workbuf, "-L");
#endif /* CPOC */
    (void)strcat(workbuf, cclib);
    add_option(LD_PASS, workbuf);
  }
  /*
   *	Add any automatic libraries
   */
#if QL
  if (0 != fpflag) {
    add_option(LDPOST_PASS, "-lfpu");
  }
#endif /* QL */
#if CPOC
  if (0 != fpflag) {
    add_option(LD_PASS, "fpinit.o");
  } else {
    add_option(LD_PASS, "nofpinit.o");
  }
  add_option(LDPOST_PASS, "-lplib");
#endif /* CPOC */

  /*
   *	Now do some consistency checks
   */
  DBG(("MAIN", 0x18, "Checking that isname set (=%d)", isname));
  if (0 == isname) {
    DBG(("MAIN", 0x14, "No filenames supplied to compile"));
    printstr("No filenames to compile\n", NULL);
    exit(ERR_BP);
  }

  DBG(("MAIN", 0x18, "Checking tmp_dir - %p (%s)", tmp_dir, tmp_dir));
  if ((NULL != tmp_dir) && ('\0' != *tmp_dir)) {
    DBG(("MAIN", 0x18, "tmp_dir set - checking ends in directory seperator"));
    if (DSEP != tmp_dir[strlen(tmp_dir) - 1]) {
      DBG(("MAIN", 0x18, "... NO, so adding it"));
      tmp_dir = realloc(tmp_dir, strlen(tmp_dir) + strlen(DSEPS) + 2);
      (void)strcat(tmp_dir, DSEPS);
    }
  }

  DBG(("MAIN", 0x18, "Checking ccpath - %p (%s)", ccpath, ccpath));
  if ((NULL != ccpath) && ('\0' != *ccpath)) {
    DBG(("MAIN", 0x18, "ccpath set - checking ends in directory seperator character"));
    if (DSEP != ccpath[strlen(ccpath) - 1]) {
      DBG(("MAIN", 0x18, "... NO, so adding it"));
      ccpath = realloc(ccpath, strlen(ccpath) + strlen(DSEPS) + 1);
      (void)strcat(ccpath, DSEPS);
    }
#ifdef QDOS
    DBG(("MAIN", 0x18, "ccpath set - need to set as program directory"));
    if (chpdir(ccpath)) {
      DBG(("MAIN", 0x18, "... failed"));
      printstr("Failed to set Directory Path for C68 phases to ", ccpath, "\n", NULL);
      exit(ERR_BP);
    }
#endif /* QDOS */
  }

  if (ASM_PASS == last_pass) {
    DBG(("MAIN", 0x18, "assembler is the last pass"));
    DBG(("MAIN", 0x18, "... so ensure correct assembler used"));
    last_pass = asmpass;
  }
#ifdef QL
  if (rllflag) {
    add_option(LDPOST_PASS, "-rc");
  }
#endif /* QL */
  /*
   *	We have now finished handling all the parameters,
   *	so it is time to start compiling programs
   */
  DBG(("MAIN", 0x14, "Start compilation phase"));
  DBG(("MAIN", 0x18, "... first FILE_LIST structure at %p", file_list.next));
  for (nameptr = file_list.next; nameptr; nameptr = nameptr->next) {
    int start_pass;
    DBG(("MAIN", 0x14, "Next file to process is '%s'", nameptr->name));
    if ('-' == nameptr->name[0]) {
      DBG(("MAIN", 0x14, "... library file, so simply add to list link files '%s'"));
      add_option(LD_PASS, nameptr->name);
      continue;
    }
    (void)strcpy(inname, nameptr->name); /* set initial input filename */

    /*
     *	Use the file extension to work out
     *	what to do in this run of CC.
     */
    p = get_extension(inname);
    switch (toupper((unsigned char)*p)) {
    case 'C':
      /*
       *	For standard C files start with the pre-processor
       */
      DBG(("MAIN", 0x18, "File is a standard C file"));
      start_pass = CPP_PASS;
      break;
    case 'I':
      /*
       *	For pre-processed files, start with either
       *	the compiler or  unproto as appropriate
       */
      DBG(("MAIN", 0x18, "File is a  pre-processed file"));
      start_pass = unprotoflag == 0 ? C68_PASS : UNPROTO_PASS;
      break;
    case 'K':
      /*
       *	This is always a file that need unproto
       */
      DBG(("MAIN", 0x18, "File is preprocessed but needs unproto"));
      start_pass = UNPROTO_PASS;
      break;
    case 'S':
      /*
       *	This is a standard assembler file
       */
      DBG(("MAIN", 0x18, "File is a _S file"));
    ASM_PASS:
      start_pass = asmpass;

      if (ASM_PASS == asmpass) {
        int fd;
        char ch;

        DBG(("MAIN", 0x18, "...using AS68/AS86 assembler"));
        /*
         *	This is to allow for the special case of an
         *	assembler file that starts with a # character
         *	as this means it needs to be pre-processed
         */
        if (0 < (fd = open(inname, O_RDONLY))) {
          (void)read(fd, &ch, 1);
          (void)close(fd);
          if ('#' == ch) {
            goto X_FILE;
          }
        }
      }
      break;
    case 'X':
      /*
       *	We use the 'X' extension for the special case
       *	of an assembler file that must first be put
       *	through the preprocessor before the assembler.
       */
      DBG(("MAIN", 0x18, "File is a _X file"));
    X_FILE:
      no_c68 = 1;
      start_pass = CPP_PASS;
      add_option(CPP_PASS, ASMCPP_OPT);
#ifdef QL
#ifdef QDOS
      passes[CPP_PASS].p_suffix = (asmpass == ASM_PASS) ? "i" : "asm";
#else
      passes[CPP_PASS].p_suffix = "i";
#endif /* QDOS */
#endif /* QL */
      break;
    case 'O':
      /*
       *	These files only need the linker running
       */
      DBG(("MAIN", 0x18, "File is a _O file"));
      start_pass = LD_PASS;
      add_option(LD_PASS, inname);
      break;
    case 'A':
      /*
       *	We need to allow for ASM files here as
       *	is sometimes used for assemblers.
       */
      DBG(("MAIN", 0x18, "File might be an _ASM file"));
      if (!stricmp(p, "asm")) {
        goto ASM_PASS;
      }
      /* FALL THRU */
    default:
      printstr("Unknown extension '", DOTS, *p, "'\n", NULL);
      exit(ERR_BP);
    }

    /*
     *	Run the phases that we have deemed appropriate
     *	to be used on this file.
     */
    DBG(("MAIN", 0x18, "start_pass=%d, last_pass=%d", start_pass, last_pass));
    for (pass = start_pass; (pass <= last_pass) && (pass < LDPRE_PASS); pass++) {
      /*
       *	We only run the 'unproto' pass if we
       *	are explicitly told to do so (which is
       *	only required if the compiler phase is
       *	only K&R compatible).
       */
      if ((UNPROTO_PASS == pass) && (0 == unprotoflag)) {
        pass++;
      }
      /*
       *	If we are not running C68, then skip to
       *	the assembler phase
       */
      if ((C68_PASS == pass) && no_c68) {
        pass = ASM_PASS;
      }
      /*
       *	Make sure that we run the right
       *	assembler for this run.
       *	put through the pre-processor first
       */
      if (ASM_PASS == pass) {
        pass = asmpass;
      }

      /*
       *	We need to work out the correct output
       *	filename to use (if any)
       */
      cp = &passes[pass];
      if (pass != last_pass) {
        (void)strcpy(outname, basename(inname)); /* Get its basename */
      } else {
        (void)strcpy(outname, nameptr->name); /* get full original name */
      }

      if ((0 != alltemp) || (cp->p_suffix != passes[last_pass].p_suffix)) {
        add_tmp(outname);
      }

      set_extension(outname, cp->p_suffix);
      /*
       *	Run the command for this phase
       */
      ret = command_line(NULL, pass, NULL, inname, outname);
      /*
       *	If error return then we need
       *	to tidy up, and abort trying to
       *	process this file.
       */
      if ((0 != ret)) {
#ifdef QL
#ifdef QDOS
        if (GWASS_PASS == pass) {
          char errbuf[10];
          (void)itoa(ret, errbuf);
          printstr("GWASS returned error code ", errbuf, "\n", NULL);
        }
#endif /* QDOS */
#endif /* QL */
        DBG(("MAIN", 0x18, "command failed (%d) - tidying up", ret));
        (void)unlink(outname); /* remove failed output file */
#ifdef QDOS
        if (no_continue()) {
          exit(ret);
        }
#endif              /* QDOS */
        dolink = 0; /* A pass failed - forget linking */
        break;
      }
#ifdef QL
#ifdef QDOS
      /*
       *	For the GWASS assembler (which only runs on QDOS)
       *	we need to allow for the fact that there are
       *	various files generated we are not interested in!
       */
      if (GWASS_PASS == pass) {
        char gwname[MAX_FNAME];

        (void)strcpy(gwname, inname);
        pass = COPY_PASS;
        set_extension(gwname, "o");
        if (0 != strcmp(gwname, outname)) {
          DBG(("MAIN", 0x18, "Copying GWASS object file from '%s' to '%s'", gwname, outname));
          (void)command_line(NULL, COPY_PASS, NULL, gwname, outname);
          DBG(("MAIN", 0x18, "Deleting GWASS object file '%s'", gwname));
          if (0 != unlink(gwname)) {
            printstr("GWASS object file missing!\n", NULL);
            exit(-1);
          }
        }
        set_extension(gwname, "LST");
        DBG(("MAIN", 0x18, "Deleting GWASS list file '%s'", gwname));
        (void)unlink(gwname);
        set_extension(gwname, "SYM");
        DBG(("MAIN", 0x18, "Deleting GWASS symbol file '%s'", gwname));
        (void)unlink(gwname);
        pass = GWASS_PASS;
      }
#endif /* QDOS */
#endif /* QL */
      /*
       *	Delete any intermediate files
       *	- but not _o's
       */
      if (pass != start_pass) {
        DBG(("MAIN", 0x18, "Deleting intermediate file '%s'", inname));
        (void)unlink(inname);
      }
      (void)strcpy(inname, outname); /* Output from previous pass goes into
                                                      next pass */
    }

    if (dolink && (LD_PASS != start_pass)) {
      DBG(("MAIN", 0x18, "adding file %s to list of link files", outname));
      add_option(LD_PASS, outname);
      add_option(DEL_PASS, outname);
    }
  }

  /*
   *	We have now finished the compilation phase
   *	Finally do the link if neccessary.
   */
  DBG(("MAIN", 0x18, "Is link required?  dolink=%d", dolink));
  if (dolink == 0) {
    exit(ret);
  }

  DBG(("MAIN", 0x14, "Start link phase"));
  if (!(ret = command_line(&passes[LDPRE_PASS], LD_PASS, &passes[LDPOST_PASS], NULL, NULL))) {
    /*
     *	delete any intermediate object
     *	files at this point
     */
    for (ptrptr = passes[DEL_PASS].p_optbuf.argv; ptrptr != NULL && *ptrptr != NULL != 0; ptrptr++) {
      (void)unlink(*ptrptr);
    }
  } else {
    printstr("Link failed\n", NULL);
    exit(ret);
  }
  DBG(("MAIN", 0x11, "CC finished - return code = %d", ret));
  return ret;
}
