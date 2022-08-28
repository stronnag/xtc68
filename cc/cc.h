 #define _GNU_SOURCE

/*
 *						cc.h
 *						~~~~
 *
 *	 Header file for the Command line front end for the
 *	"C68 Compilation system for QDOS" and "CPOC for the Psion 3a"
 *
 * (c) Copyright 1991-1998 David J. Walker
 *	   Email:  itimpi@msn.com
 *
 *	   The purpose of this program is to act as a front end
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
 *
 *	AMENDMENT HISTORY
 *	~~~~~~~~~~~~~~~~~
 *
 *	Oct 90		JA	v1.0	Original version
 *	31/03/91	DJW v1.1	Made CC print out executed commands if
 *									verbose option set.
 *							Added -Q option for C68 v3.x
 *	26/05/91	DJW v1.2	Remove output file if a phase fails
 *	27/06/91	DJW v1.3  - Fixed bug whereby incorrect file name given to
 *							LD if starting with _o files.
 *						  - If -TMP specified, then put _o files on this
 *							device, and otherwise only _i and _s files.
 *						  - Proceeding to link assembled files in same run,
 *							then treat _o files as intermediate files
 *	20/06/91	DJW v1.4  - Allowed CPP to be provided its parameters in
 *							both lower case as well as upper case.	 This
 *							provides better compatibility with LATTICE
 *							which uses lower case options -i and -d
 *	26/05/91	DJW v1.5  - Added support for -g option to allow programs
 *							called by cc to be off the program path
 *	27/08/91	DJW v2.0  - Amended -tmp and -TMP options to first remove any
 *							existing device name before adding new prefix.
 *						  - The -g option now does a change of the PROG_USE
 *							directory and it is passed through to the daughter
 *							jobs.  This means it affects all references to the
 *							default program directory.
 *						  - A new phase is added to (optionally) be included between
 *							CPP and C68.  This is activated by the -UNPROTO runtime
 *							parameter to CC.
 *						  - The option to use the QMAC assembler (Quanta GST comaptible
 *							version) as an alternative to the AS68 one supplied with
 *							C68 can be activated using the -QMAC parameter to CC.
 *						 -	Explicit support for -h option removed as no longer
 *							required.  -h will be ignored for backwards compatibility.
 *
 *	19/01/92	DJW v2.01 - The 'no_continue' routine changed to only give the
 *							user a choice if channels have not been passed to CC.
 *						  - Environment variables checked for default values:
 *								TEMP orTMP		location for temporary files.
 *								C68PATH 		path for C68 components.
 *
 *	12/03/92   DJW v2.02 -	Added some checks on directory names for temporary
 *							files and C68 path ending in underscore.
 *						 -	Error routine upgraded to use variable number of
 *							parameters.
 *
 *	23/05/92	DJW v3.00	New version for C68 Release 3.
 *						 -	The -UNPROTO and -U options removed as C68 now
 *							understands ANSI style function prototypes and
 *							declarations.
 *						 -	The -trad option added to stop ANSI extensions
 *							being used.
 *						 -	The environment variables CC_CPP, CC_C68, CC_ASM
 *							and CC_LD can now be used to override the default
 *							program names for each path.
 *
 *	30/06/92   DJW	v3.01 - The C68PATH variable renamed to be CC_PATH.
 *						  - The -v option now passed to C68 and AS68 phases.
 *
 *	20/07/92   DJW	v3.02 - Removed the internal wild-card expansion routine
 *							by switching to generic C68 library option.
 *
 *	24/08/92   DJW	v3.03 - Added implicit '-v' option when cc is invoked
 *							directly without its channels being passed or
 *							redirected.
 *
 *	15/09/92   DJW	v3.04 - Completely rewrote the section on parsing the
 *							command line for options.
 *						  - Added option to automatically run CPP pass on
 *							assembler files whose first character is #
 *							(this is a Unix convention).
 *						  - The -g option replace by -Y option.
 *
 *	21/09/92   DJW	v3.05 - Removed -d option as this is now only likely to
 *							be relevant if running CPP in isolation  (it used
 *							to be treated as a synonym for -D).
 *
 *	22/08/93   DJW	v3.06 - Now pass the -O, -g, -U, -uchar, -error, -frame
 *							and -lattice options through to the C68 program
 *
 *	26 Sep 93  DJW	v4.00 - A full overhaul of the options done to try and
 *							provide the complete list supported by the C68
 *							Release 4 programs.
 *
 *	31 Dec 93  DJW	v4.12 - Added special support for -uchar option.
 *						  - Forced __STDC__ to be undefined when the -trad
 *							option is used.
 *						  - Added support for passing preset options via the
 *							CC_OPTS environment variable.  This involved
 *							re-working the way the command line was handled.
 *	15 Jan 94	DJW v4.20 - Added support for -nostackopt option for C68.
 *						  - Added support for -stackcheck option for C68
 *						  - Added support for -extern option for C68
 *						  - Added support for -trace option for C68
 *						  - Added support for -s option for LD (was same as -S)
 *						  - Added support for -r and -R options for LD RLL use.
 *						  - Added support for -crf option for LD cross-reference
 *						  - Added support for -map option for LD load map
 *						  - Added support for -sym option for LD symbol list
 *						  - Added support for -debug option for LD debug listing
 *
 *	01 Sep 94	DJW v4.21 - Made -nostackopt into dummy as no longer used.
 *						  - Added check that filename follow parameters that
 *							demand this.  Problem report from Franz Herrman.
 *
 *	11 Nov 94	DJW v4.22 - CC_LD environment variable now used.
 *
 *	29 Nov 94	DJW v4.23 - Corrected problem where an environment variable
 *							set to a NULL value was not being treated as if
 *							it was absent.
 *
 *	02 Jan 95	DJW v4.24 - Added use of qopen() routine.
 *						  - Allowed for both . and _ as seperator characters.
 *
 *	12 Mar 95	DJW v4.25 - Added support for -Xa, -Xt and -Xc parameter
 *							options
 *
 *	01 Feb 96	DJW v5.00 - Added support for the George Gwilt assembler GWASS
 *						  - Added checks that GWASS only allowed if 68020+
 *						  - Added checks that -fpu only allowed if FPU present
 *
 *	16 Feb 96	DJW v5.01 - Enhanced support for -r option.
 *						  - Enhanced support for -Xa, -Xt and -Xc options.
 *
 *	03 Mar 95	DJW v5.02 - Extra changes for GWASS support.
 *						  - Removed all calls to STDIO, and replaced with
 *							Unix style I/O to reduce memory footprint.
 *
 *	24 Mar 96	DJW v5.03 - Merged in Jonathan Hudsons XTC68 changes
 *
 *	28 Apr 96	DJW v5.04 - Added support for -f options to be passed to LD
 *
 *	28 May 96	DJW v5.05 - Merged in JH XTC68 changes.
 *
 *	28 Jun 96	DJW v5.06 - Added support for -datamodel and -codemode options.
 *
 *	23 Aug 96	DJW v5.10 - Bought into line with new parameter options for the
 *							main compiler phases.
 *						  - Redefined cross-compilation options.
 *						  - Added support for CPOC mode of working.
 *						  - Changes made to run in CPOC mode under
 *							Windows 95/NT and EPOC (TopSpeed).
 *
 *	05 Dec 96	DJW v5.11 - Changed to use 'spawnvp()' call to launch programs
 *							when run under WIN32.  This gets around problems with
 *							the return value from system() call in this environment.
 *
 *	03 Mar 97	DJW v5.12 - Changed storing of parameter options to use argv
 *							style arrays.  This will help with better support
 *							for spawnvp() on windows, and allow exec() to be
 *							used on QDOS, and system() on Unix style systems.
 *						  - Changed to handle latest GWASS correctly.
 *
 *	15 Aug 97	DJW v5.13 - Fixed memory corruption problem.
 *							This could cause random system lockups.
 *
 *	14 Mar 98	DJW v5.14 - Aligned QDOS implementation with CPOC one.
 *
 *	15 May 98	DJW v5.15 - Fixed problem with handling of GWASS assembler on QDOS
 */

#define VERSION " v5.15 "
#define COPYRIGHT "(c)1991-1998  D.J.Walker"

/********************************* CONFIGURATION *********************************/
/*
 *	 The exact facilities to be included in a particular version
 *	 of the front-en program can be controlled by #defines.  Ideally
 *	 these are set through the makefile used to build the project, but
 *	 if your development environment does not support this they can
 *	 instead be set explicitly through #define statements in this
 *	 file.	The options are:
 *
 *	 Set one of the following to specify the desired target environment:
 *	 QL 	 This is for versions of C68 for QDOS
 *			 (whatever the host system)
 *	 CPOC	 This is for versions of C86 for the Psion 3a
 *			 (whatever the host system)
 *
 *
 *	 The following are used to give information about the operating
 *	 environment in which the system is to be hosted:
 *	 QDOS		 This is for versions of C68 that are hosted on QDOS or a
 *				 compatible system such as Minerva or SMSQ(E)
 *	 __unix__	 The enviroment us a Unix-like system
 *	 DOS_LIKE	 MSDOS based system
 *	 WIN32		 Windows 95 or Windows NT based system.
 *	 EPOC		 Psion 3a based system
 *
 *
 *	 The following allow you to control optional features of the program
 *	 ENVVAR 	 Support for environment variables is to be included.
 *				 Note that this requires the libraries to have getenv().
 *	 OBSOLETE	 Builds in support for parameter options that are no
 *				 longer used, but were used in past versions.
 *
 *
 *	 The following options will only ever be set when trying to develop
 *	 or debug the CC front-end program itself:
 *	 LIBDEBUG	 This can be set IF you have the "libdebug" library.
 *	 JDBG		 This is an alternative that does an in-line print
 *
 *-----------------------------------------------------------------------*/

#define QL 1
/* #define CPOC */

/* #define QDOS */
/* #define WIN32 */
/* #define __unix__ */
/* #define EPOC */

/* #define ENVVAR */
/* #define OBSOLETE */

/* #define LIBDEBUG */
/* #define JDBG */

/***************************** END OF CONFIGURATION ******************************/

/*
 *	Do some cross-checks on the options
 */

#ifndef QL
#ifndef CPOC
#error "Target environment does not appear to be specified"
#endif /* CPOC */
#else
#ifdef CPOC
# error "Both CPOC and XTC68 targets specified"
#endif /* CPOC */
#endif /* QL */

#ifdef WIN32
#undef DOS_LIKE
#define DOS_LIKE
#endif /* WIN32 */

/*************************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifdef DOS_LIKE
#include <io.h>
#include <malloc.h>
#endif /* DOS_LIKE */

#ifdef QDOS
#include <unistd.h>
#define __LIBRARY__
#include <qdos.h>
#endif /* QDOS */

#ifdef EPOC
#include <unistd.h>
#include <cpoclib.h>
#endif /* EPOC */

#ifdef LIBDEBUG
#include <debug.h>
#else
#define DBG_INIT()
#ifndef JDBG
#define DBG(params)
#else
#define DBG(params) jdbg params
#endif /* JDBG*/
#endif /* LIBDEBUG*/

/*
 *	Set seperator characters according to environment
 */
#ifdef QDOS
# define DOT '_'
# define DOTS "_"
# define DSEP '_'
# define DSEPS "_"
#endif /* QDOS */

#if defined(__unix__) || defined(__APPLE__)
# define DOT '.'
# define DOTS "."
# define DSEP '/'
# define DSEPS "/"
# define ERR_OM 1
# define ERR_BP 1
#endif /* defined(__unix__) || defined(__APPLE__) */

#ifdef DOS_LIKE
# define DOT '.'
# define DOTS "."
# define DSEP '\\'
# define DSEPS "\\"
# define ERR_OM 1
# define ERR_BP 1
#endif /* DOS_LIKE */

#ifdef EPOC
# define DOT '.'
# define DOTS "."
# define DSEP '\\'
# define DSEPS "\\"
# define ERR_OM 1
# define ERR_BP 1
#endif /* EPOC */

/*
 *	Set names for some of the programs
 */
#ifdef QL
#ifdef QDOS
# define CC "cc"
# define CPP "cpp"
# define UNPROTO "unproto"
# define COMPILER "c68"
# define ASM "as68"
# define LD  "ld"
#else
# define CC "qcc"
# define CPP "qcpp"
# define UNPROTO "unproto"
# define COMPILER "c68"
# define ASM "as68"
# define LD  "qld"
#endif /* QDOS */
#endif /* QL */

#ifdef CPOC
# define CC "cpoc_cc"
# define CPP "cpoc_cpp"
# define UNPROTO "unproto"
# define COMPILER "c86"
# define ASM "as86"
# define LD  "ld86"
#endif /* CPOC */

#define STDIN_FILENO	0
#define STDOUT_FILENO	1
#define STDERR_FILENO	2

#define MAX_FNAME 60

/*
 *	The following structure is used when it
 *	is necessary to maintain a list of files.
 */
typedef struct FILE_LIST
	{
		struct FILE_LIST *	next;
		char *	name;
	}
	FILE_LIST_t;

/*
 *	These structures are used to build up a list
 *	of arguments for a particular phase.
 */
typedef struct PASS_OPTS
	{
		int 	argcmax;			/* Entries currently allocated */
		int 	argc;				/* Entries currently used */
		char ** argv;				/* Pointer to array of arg pointers */
	}
	PASS_OPTS_t;

typedef struct CC_PASSES
	{
	char *p_name;				/* Program name to call for this pass */
	char *p_suffix; 			/* Character(s) output file name must end with */
	struct PASS_OPTS p_optbuf;	/* Pointer to args control for this pass */
	}
	CC_PASSES_t;

#define PASS_OPTS_INCR	4		/* Amount by which pass options incremented */

/*
 *	The order in which pass information is
 *	stored in the structure in cc.c file that
 *	defines data for each pass.  It is important
 *	that the orders are kept in step.
 */
enum pass_order {
	CC_PASS,
	CPP_PASS,
	UNPROTO_PASS,
	C68_PASS,
	ASM_PASS,
#ifdef QL
#ifdef QDOS
	QMAC_PASS,
	GWASS_PASS,
#endif /* QDOS */
#endif /* QL */
	LDPRE_PASS,
	LD_PASS,
	LDPOST_PASS,
	DEL_PASS,
#ifdef QL
#ifdef QDOS
	COPY_PASS,
#endif /* QDOS */
#endif /* QL */
	MAX_PASS
	};

/*
 *	This is a list of the possible actions that
 *	we can assign to the processing of any
 *	particular parameter.	Each of the possible
 *	actions has an entry in the switch statement
 *	that is used to process parameter entries.
 */
enum actions {
		ACTION_IGNORE,					/* Option is not used */
		ACTION_ADD, 					/* Pass option to specified pass */
		ACTION_ADD_FILES,				/* Add option to filelist */
		ACTION_ADD_REST,				/* Pass option remainder to pass */
		ACTION_ADD_ALL, 				/* Pass option to all passes */
		ACTION_ADD_STKMEM,				/* Stack/Memory option add to all passes */
		ACTION_ADD_MORE,				/* More data follows, allow for space */
		ACTION_ADD_OPTIONAL,			/* More data may follow */
		ACTION_CPP_LAST,				/* Stop after the pre-processor phase */
		ACTION_C68_LAST,				/* Stop after the compiler phase */
		ACTION_ASM_LAST,				/* Stop after the assembler phase */
		ACTION_GWASS,					/* Use GWASS assembler */
		ACTION_QMAC,					/* Use QMAC as assembler phase */
		ACTION_FPU, 					/* Target requires hardware FPU */
		ACTION_VERBOSE, 				/* Run in verbose mode */
		ACTION_VERBOSE_ALL, 			/* Run all components in verbose mode */
		ACTION_TMP, 					/* Set directory for temporary files */
		ACTION_TMP_ALL, 				/* ... temporary and target files */
		ACTION_TRAD,					/* Set -trad option for C68 */
		ACTION_PATH 					/* Set program search path */
#ifdef QL
		,ACTION_RLL 					/* Set for RLL library to be used */
		,ACTION_RLL_LIB 				/* Set for RLL library being built */
		,ACTION_RLL_DEF 				/* Set for RLL defines used */
		,ACTION_TARGET					/* Handle cases where specific targets set for C68 */
		,ACTION_UCHAR					/* Set -uchar options for C68 */
#endif /* QL */
		,ACTION_UNPROTO 				/* Run UNPROTO before compiler */
#ifdef QL
		,ACTION_XA						/* Set -Xa options */
		,ACTION_XC						/* Set -Xc options */
		,ACTION_XT						/* Set -Xt options */
#endif /* QL */
                ,ACTION_OUT
};


/******************************************************************
 *
 *	Function prototypes.
 */

#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif

void	add_option				_P_((int, char *));
void	add_tmp 				_P_((char *));
#ifndef HAVE_BASENAME
char   *basename				_P_((char *));
#endif /* ! HAVE_BASENAME */
int 	command_line			_P_((CC_PASSES_t *, int, CC_PASSES_t *, char *, char *));
void	Environment_Variables	_P_((void));
int 	no_continue 			_P_((void));
void	printstr				_P_((char *,...));
void	useage					_P_((void));
struct OPTIONS *what_opt		_P_((char *));

void	argfree 				_P_((char ***));
int 	argunpack				_P_((const char *, char ***, int *, int (*)(char *, char ***, int *)));
char *	argpack 				_P_((char * const * argv, int flag));
#ifndef HAVE_LIBGEN
char *	strccpy 				_P_((char * output, const char * input));
char *	strcadd 				_P_((char * output, const char * input));
char *	strecpy 				_P_((char *, const char *,const char *));
char *	streadd 				_P_((char *, const char *, const char *));
#endif /* HAVE_LIBGEN */
void	my_error				_P_((int, char *));
char *	FilePart				_P_((char *));
void	AddPart 				_P_((char *, char *, int));
int 	CheckCPU				_P_((void));
#ifndef QDOS
# ifndef WIN32
#  ifdef EPOC
#	define stricmp	p_scmpi
#  else
#	define stricmp strcasecmp
#  endif /* EPOC */
# endif /* WIN32 */
#endif /* QDOS */

#ifdef JDBG
void	jdbg					_P_((char *, x,*ctl,...));
#endif /* JDBG */

#undef _P_
