/*
 *		c o n f i g . h
 *
 *	You will need to edit this file to set up the options that
 *	you want when building your version of c68/c386.  Most of
 *	the choices are obvious from the comments, and in the more
 *	obscure cases detailed guidance is given.
 *
 *	In principle those options that you do not want activated
 *	are changed to comments by inserting the 'start comment'
 *	symbol at the start of the line - and removing it to activate
 *	an option.
 */

#ifndef _CONFIG_H
#define _CONFIG_H

/*
 *	GENERAL OPTIONS
 *	~~~~~~~~~~~~~~~
 */
/*
 *	Set the following options to suit your system/requirements
 */
#define ERRORLEVEL_DEFAULT 0 /* Default error level			     */
#define WARNLEVEL_DEFAULT 3  /* Default warning level		     */

#undef ASM           /* ASM statement support                     */
#undef DEBUG         /* Include internal debug code               */
#define DEBUGOPT     /* Include -g command line option	     */
#define ENVVAR       /* Compiler options in environment variable  */
                     /* requires the getopt() call		     */
#define EXTENSION    /* Include proposed extensions to ANSI C     */
#define FACIST       /* Allow 'facist' checking of code	     */
#define FLOAT_CHECK  /* Support warning for FP operations	     */
#define FORMAT_CHECK /* Check fprintf and fscanf format strings   */
#undef HAS_NLS       /* System supports message catalogues	     */
#define HAS_STDARG   /* System supports stdarg.h		     */
#undef LIST          /* Output symbol tables                      */
#define PACKENUM     /* Allow enumerations to be any integer type */
#undef PEEPFLOW      /* Include peephole flow optimisations	     */
#undef PROBES        /* Support stack probes                      */
#define SEQUENCE     /* Check sequence points		     */
#undef SIGNAL        /* Catch internal signal to exit graceful    */
                     /* requires the signal() call		     */
#define STACK_CHECK  /* Library routine to check stack size       */
#undef TOPSPEED      /* Enable Topspeed compiler extensions	     */
#undef TRANSLATE     /* Include output names translation option   */
/*
 *	These options tend not to be changed except when developing
 */
#undef VERBOSE /* Support verbose mode */
#undef ICODE   /* Support icode analysis code */
/*
 *	This option is still experimental and under development, so it
 *	should not normally be activated.
 */
#undef TRACE    /* Calls for trace/debugger support */
#undef EXTERNAL /* Lists external definitions */

#undef SYNTAX_CORRECT
/*
 *	FLOATING POINT OPTIONS
 *	======================
 */
/*
 *	This option is used to allow a version of c68/c386 to be built
 *	that does not have built in floating point support, but allows
 *	the float and double keywords to be recognised at the syntax
 *	checking stage.  This is quite likely to be required to avoid
 *	compilation errors if header files which use these keywords are
 *	included by programs.
 */
#define FLOAT_KEYWORDS /* Floating point keywords recognised */
/*
 *	The following are used to determine what type of floating point
 *	support is required (the MFFP option only applies to c68).
 *	If neither option is defined, then the generated compiler is
 *	not capable of handling floating point.
 */
#define FLOAT_IEEE /* Treat FP numbers as IEEE */
#undef FLOAT_MFFP  /* Treat FP numbers as MFFP */

/*	This option is only used while bootstrapping a floating point compiler.
 *	It provides just enough floating point support to continue working
 *	towards the a version that has full floating point support.   At the
 *	end of the bootstrap process you unset this option and build the
 *	final version of c68/c386 which has full floating point support.
 */
#undef FLOAT_BOOTSTRAP /* Bootstrapping floating point version */

/*
 *	PROCESSOR Selection
 *	~~~~~~~~~~~~~~~~~~~
 *
 * These options determine which code generators will be built into the
 * compiler.
 */
#define MC68K    /* Motorola 68000 compiler */
#undef INTEL_86  /* Intel 8086 compiler */
#undef INTEL_386 /* Intel 80386 compiler */
#undef ARM       /* ARM compiler */
#undef TMS320C30 /* TI TMS320C30 compiler */

/*
 * Only one of the options below must be defined (and is only relevant
 * if more than one processor type has been selected).
 */
#define MC68000_DEFAULT  /* Default processor is MC68000 */
#undef INTEL_86_DEFAULT  /* Default processor is Intel 8086 */
#undef INTEL_386_DEFAULT /* Default processor is Intel 80386 */
#undef ARM_DEFAULT       /* Default processor is ARM */
#undef TMS320C30_DEFAULT /* Default processor is TI TMS320C30 */

/*
 *	Assembler Selection
 *	~~~~~~~~~~~~~~~~~~~
 *
 * These options determine which assembler syntax are supported by the
 * compiler - this may be dependent on the code generators selected.
 */

#define TARGET_ACK   /* Assembler is ACK     Motorola */
#undef TARGET_CPM    /* Assembler is DRI     Motorola */
#define TARGET_QMAC  /* Assembler is QMAC    Motorola */
#undef TARGET_GAS    /* Assembler is GAS     Motorola/Intel */
#undef TARGET_SUN    /* Assembler is Sun     Intel */
#undef TARGET_BAS    /* Assembler is BCC     Intel */
#undef TARGET_SYSV   /* Assembler is SVR4.0  Intel */
#undef TARGET_MASM   /* Assembler is Microsoft Intel */
#undef TARGET_OBJ    /* Assembler is ObjAsm  ARM */
#undef TARGET_ROSSIN /* Assembler is Rossin? */

#define TARGET_ACK_DEFAULT   /* Default Assembler is ACK     Motorola */
#undef TARGET_CPM_DEFAULT    /* Default Assembler is DRI 	Motorola */
#undef TARGET_QMAC_DEFAULT   /* Default Assembler is QMAC 	Motorola */
#undef TARGET_GAS_DEFAULT    /* Default Assembler is GAS 	Motorola/Intel */
#undef TARGET_SUN_DEFAULT    /* Default Assembler is Sun 	Intel */
#undef TARGET_BAS_DEFAULT    /* Default Assembler is BCC     Intel */
#undef TARGET_SYSV_DEFAULT   /* Default Assembler is SVR4.0	Intel */
#undef TARGET_MASM_DEFAULT   /* Default Assembler is Microsoft Intel */
#undef TARGET_OBJ_DEFAULT    /* Default Assembler is ObjAsm	ARM */
#undef TARGET_ROSSIN_DEFAULT /* Default Assembler is Rossin */

/*
 *	COMPILATION SYSTEM
 *	~~~~~~~~~~~~~~~~~~
 *	The following definition is used to work around a bug in the
 *	ACK assembler issued with MINIX 1.5 and earlier.  If your version
 *	of ACK has this bug fixed, then in some cases slightly more
 *	efficient code is generated if you comment out this option
 */
#undef MOVEMBUG  /* Assembler has problems in MOVEM (Motorola)*/
#undef CMP_BUG   /* Assembler has problems in CMP (Motorola)*/
#undef RELOC_BUG /* Assembler has problems with Label1-Label2 */

/*
 *	PORTING DEFINTIONS
 *	~~~~~~~~~~~~~~~~~~~
 *	The defines in this section will only need changing if you are
 *	trying to build c68/c386 with a compiler that does not support
 *	all the facilities of c68/c386.  It is likely that you may still
 *	have to make some changes to the code, but these changing these
 *	#defines appropriately will help keep it to a minimum.
 */
#undef void             /* Set active if 'void' not supported */
#define VOIDSTAR void * /* Set to char * if 'void' not supported */
#define VOIDCAST (void) /* Set to space if 'void' not supported */

/*
 *	Many K&R compiler have assumed that the result of the sizeof operator
 *	is an int.  However ANSI assumes that the result is the type size_t.
 *	Ensure that the setting is compatible with the value defined in your
 *	library and header files.
 */
#define STP_SIZE stp_ulong /* size_t type; either stp_uint or stp_ulong */
/*
 *	Many K&R compilers have assumed that the result of the pointer
 *	difference operation is an int.  However ANSI assumes that the
 *	result is the type ptrdiff_t.  Ensure that the setting is compatible
 *	with the value defined in your library and header files.
 *	 NOTE: This must be a signed value.
 */
#define STP_PTRDIFF stp_long /* ptrdiff_t type; either stp_int or stp_long */

/*
 *	The type of a wide character is implementation defined and has an
 *	integral type.
 */
#define STP_WIDE stp_char
/*#define STP_WIDE        stp_long*/

/*
 *	The compiler does its own memory allocation.  AL_HOST should be
 *	set to the worst case alignment requirements of the host system.
 *	(Must be a power of 2.)
 *	If left unset then the compiler will make a "best guess" at the
 *	alignment requirements.
 */
#define AL_HOST ((SIZE)2) /* Align memory on AL_HOST bytes */

/*
 *	If the host compiler doesn't support bitfields then define this
 *	definition to do nothing.
 *	This allows structures within the compiler to take less space at
 *	the cost of extra run-time code.
 */
/*#define COLON(x)	: x*/
#define COLON(x)

/*
 *	If your compiler requires all files to contain at least one
 *	declaration then define the following item.   This will declare
 *	a dummy static item in all files.
 */
/*#define	NO_EMPTY_FILES*/ /* Declare dummy static in all files */

#include "check.h"

#endif /* _CONFIG_H */
