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


#ifndef _CHDR_H
#define	_CHDR_H

#include "config.h"

#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

#ifdef VERBOSE
#include <time.h>
#define	STARTTIME(t)	clock_t t = clock()
#define	DOTIME(t1, t2)	t1 += clock()-t2; t2 = clock()
#else
#define	STARTTIME(t)
#define	DOTIME(t1, t2)
#endif /* VERBOSE */


/*
 *   Definitions used for I/O portability
 */
#ifdef EPOC
#include <plib.h>
#define	FHANDLE		void *
#undef	EXIT_FAILURE
#define	EXIT_FAILURE	1
#else /* not EPOC */
#include <stdio.h>
#define	FHANDLE		FILE *
#endif /* EPOC */

#ifdef HAS_STDARG
#include <stdarg.h>
#else
#include <varargs.h>
#endif /* HAS_STDARG */

#include <stdint.h>
/*
 *   For NON-ANSI systems.....
 */
#ifndef EXIT_SUCCESS
#define	EXIT_SUCCESS	0
#endif /* EXIT_SUCCESS */

#ifndef EXIT_FAILURE
#define	EXIT_FAILURE	1
#endif /* EXIT_FAILURE */

#ifndef FILENAME_MAX
#define	FILENAME_MAX	127
#endif /* FILENAME_MAX */

#ifndef __STDC__
#define	const			/* nothing */
#endif /* __STDC__ */

/*
 *   Definition to allow ANSI and K&R function declaration to exist
 *   on the same declaration.
 */
#if defined(__STDC__) || defined(__cplusplus)
#define P_(s) s
#else
#define P_(s) ()
#endif


/*
 * documents paths which it should be impossible to reach
 */

#define	CANNOT_REACH_HERE()	assert(0)

/*
 * the tokens returned from lexical analysis
 */

enum e_sym {
    /* Constants */
    tk_cconst,			/* Character Constant */
    tk_fconst,			/* Float Constant */
    tk_iconst,			/* Integer Constant */
    tk_lconst,			/* Long Integer Constant */
    tk_lrconst,			/* Long Double Constant */
    tk_rconst,			/* Double Constant */
    tk_wconst,			/* Wide Character Constant */
    tk_sconst,			/* String Constant */
    tk_wsconst,			/* Wide String Constant */
    tk_uconst,			/* Unsigned Integer Constant */
    tk_ulconst,			/* Unsigned Long Constant */

    /* Arithmetic Operators */
    tk_plus,			/* + symbol */
    tk_minus,			/* - symbol */
    tk_divide,			/* / symbol */
    tk_mod,			/* % symbol */

    /* Shift Operators */
    tk_lshift,			/* << symbol */
    tk_rshift,			/* >> symbol */

    /* Equality Operators */
    tk_eq,			/* == symbol */
    tk_neq,			/* != symbol */
    tk_lt,			/* < symbol */
    tk_leq,			/* <= symbol */
    tk_gt,			/* > symbol */
    tk_geq,			/* >= symbol */

    /* Assignment Operators */
    tk_assign,			/* = symbol */
    tk_asplus,			/* += symbol */
    tk_asminus,			/* -= symbol */
    tk_astimes,			/* *= symbol */
    tk_asdivide,		/* /= symbol */
    tk_asmod,			/* %= symbol */
    tk_asuparrow,		/* ^= symbol */
    tk_aslshift,		/* <<= symbol */
    tk_asrshift,		/* >>= symbol */
    tk_asand,			/* &= symbol */
    tk_asor,			/* |= symbol */

    /* Unary Operators */
    tk_autoinc,			/* ++ symbol */
    tk_autodec,			/* -- symbol */
    tk_not,			/* ! symbol */
    tk_compl,			/* ~ symbol */

    tk_hook,			/* ? symbol */
    tk_uparrow,			/* ^ symbol */
    tk_pointsto,		/* -> symbol */
    tk_dot,			/* . symbol */
    tk_lor,			/* || symbol */
    tk_land,			/* && symbol */
    tk_or,			/* | symbol */
    tk_and,			/* & symbol */

    tk_semicolon,		/* ; symbol */
    tk_closebr,			/* ] symbol */
    tk_end,			/* } symbol */
    tk_closepa,			/* ) symbol */

    tk_openbr,			/* [ symbol */
    tk_begin,			/* { symbol */
    tk_comma,			/* , symbol */
    tk_openpa,			/* ( */
    tk_star,			/* * symbol */
    tk_colon,			/* : symbol */
    tk_ellipsis,		/* ... */

    tk_id,			/* identifier */

    kw_char,			/* char */
    kw_double,			/* double */
    kw_enum,			/* enum */
    kw_float,			/* float */
    kw_int,			/* int */
    kw_long,			/* long */
    kw_short,			/* short */
    kw_signed,			/* signed */
    kw_struct,			/* struct */
    kw_union,			/* union */
    kw_unsigned,		/* unsigned */
    kw_void,			/* void */

    kw_auto,			/* auto */
    kw_extern,			/* extern */
    kw_register,		/* register */
    kw_static,			/* static */
    kw_typedef,			/* typedef */

    kw_const,			/* const */
    kw_volatile,		/* volatile */

#ifdef ASM
    kw_asm,			/* asm */
#endif				/* ASM */
    kw_break,			/* break */
    kw_case,			/* case */
    kw_continue,		/* continue */
    kw_default,			/* default */
    kw_do,			/* do */
    kw_else,			/* else */
    kw_for,			/* for */
    kw_goto,			/* goto */
    kw_if,			/* if */
    kw_return,			/* return */
    kw_sizeof,			/* sizeof */
    kw_switch,			/* switch */
    kw_while,			/* while */

#ifdef TOPSPEED
    kw_cdecl,			/* cdecl */
#endif				/* TOPSPEED */

#ifdef EXTENSION
    /* proposed extensions to ANSI C */
    kw_complex,			/* complex */
    kw_imaginary,		/* imaginary */
    kw_inline,			/* inline */
    kw_restrict,		/* restrict */
#endif				/* EXTENSION */

#ifdef FACIST
    kw_id,			/* c++ symbol */
#endif				/* FACIST */

    tk_eof			/* End-Of-File */
};

/*
 * storage classes
 */

enum e_sc {
    /* Storage classes defined in the ANSI C Specification */
    sc_static,			/* Symbol is static */
    sc_auto,			/* Symbol is automatic */
    sc_global,			/* Symbol is global */
    sc_external,		/* Symbol is external */
    sc_typedef,			/* Symbol is a typedef */
    sc_register,		/* Symbol is a register */

    /* Pseudo storage classes - these classes exist in the symbol table */
    sc_parms,			/* Symbol is a parameter to a function */
    sc_tag,			/* Symbol is a struct/union/enum tag */
    sc_const,			/* Symbol is an enumeration constant */
    sc_member,			/* Symbol is a member of a struct/union */
    sc_label			/* Symbol is a label */
};

/*
 * basic data types
 */

enum e_bt {
    bt_void,			/* void type */
    bt_char,			/* char type (signed) */
    bt_charu,			/* char type (unsigned) */
    bt_uchar,			/* unsigned char type */
    bt_schar,			/* signed char type */
    bt_short,			/* short type */
    bt_ushort,			/* unsigned short type */
    bt_int16,			/* int type (16 bits) */
    bt_uint16,			/* unsigned int type (16 bits) */
    bt_int32,			/* int type (32 bits) */
    bt_uint32,			/* unsigned int type (32 bits) */
    bt_long,			/* long type */
    bt_ulong,			/* unsigned long type */
    bt_float,			/* float type */
    bt_double,			/* double type */
    bt_longdouble,		/* long double type */
    bt_pointer16,		/* pointer type (16 bits) */
    bt_pointer32,		/* pointer type (32 bits) */
    bt_struct,			/* struct type */
    bt_union,			/* union type */
    bt_func,			/* function type */
    bt_bitfield,		/* Pseudo type - bitfield type */
    bt_ubitfield,		/* Pseudo type - unsigned bitfield type */
    bt_ellipsis			/* Pseudo type - parameter list ... */
};

typedef int BOOL;		/* true/false value */
typedef unsigned char BITSIZE;	/* size of a bitfield */
typedef struct sblock BLOCK;	/* scope table */
typedef enum e_bt BTYPE;	/* basic type */
typedef unsigned char CHAR;	/* input character */
typedef int32_t IVAL;		/* signed integral value */
typedef unsigned int LABEL;	/* internal label */
typedef unsigned int LEVEL;	/* scope level */
typedef unsigned char QUALIFIER;	/* type qualifiers */
typedef struct range RANGE;	/* value range */
typedef double RVAL;		/* floating point value */
typedef long SIZE;		/* size of object */
typedef unsigned char STATE;	/* type state */
typedef unsigned char STATUS;	/* status of symbol */
typedef enum e_sc STORAGE;	/* symbol storage class */
typedef struct slit STRING;	/* string literal */
typedef struct swtab SWITCH;	/* switch table */
typedef struct sym SYM;		/* symbol entry */
typedef struct stab TABLE;	/* symbol table */
typedef enum e_sym TOKEN;	/* scanner token */
typedef struct typ TYP;		/* type entry */
typedef uint32_t UVAL;	/* unsigned integral value */
typedef struct func FUNC;	/* function entry */
typedef struct structure STRUCT;	/* struct/union entry */
typedef struct reg_use REGUSAGE;	/* register usage on function calls */

#ifdef SEQUENCE
typedef unsigned int SEQNUM;	/* sequence number */

#endif

#ifndef TRUE
#define	TRUE		((BOOL)1)
#define	FALSE		((BOOL)0)
#endif /* TRUE */

#define	GLOBAL_SCOPE	((LEVEL)1)
#define	NIL_BLOCK	((BLOCK *)0)
#define	NIL_CHAR	((CHAR *)0)
#define	NIL_RANGE	((RANGE *)0)
#define	NIL_REGUSAGE	((REGUSAGE *)0)
#define	NIL_STRING	((STRING *)0)
#define	NIL_SWITCH	((SWITCH *)0)
#define	NIL_SYM		((SYM *)0)
#define	NIL_TABLE	((TABLE *)0)
#define	NIL_TYP		((TYP *)0)
#define	UNDEF_LABEL	((LABEL)0)


/*
 * these form the string literal pool
 */

struct slit {
    STRING *next;		/* next literal */
    const CHAR *str;		/* pointer to the literal string */
    LABEL   label;		/* label to use when referencing a literal */
    size_t  len;		/* length of the literal */
};

/*
 *   Symbol Table Entry
 */
struct stab {
    SYM    *head;		/* pointer to symbol at start of table */
    SYM    *tail;		/* pointer to symbol at end of table */
};

/*
 *   Scope Table
 */
struct sblock {
    TABLE   symbols;		/* variable/functions etc */
    TABLE   tags;		/* struct/union/enum tags */
    BLOCK  *prev;		/* previous scope block */
    SIZE    offset;		/* stack offset for temporary variables */
};

#define	symbolsof(b)		((b)->symbols.head)

/*
 *   Symbol Table
 */
struct scopetbl {
    BLOCK  *global;		/* pointer to global scope table */
    BLOCK  *local;		/* pointer to current scope table */
};

/*
 *   Range of a basic integral type
 */
struct range {
    IVAL    low;
    IVAL    high;
};

struct func {
    BLOCK  *lst;		/* parameter list */
    REGUSAGE *regusage;		/* usage of registers on function call */
};

struct structure {
    BLOCK  *lst;		/* members */
};

/*
 *   Type Entry
 */
struct typ {
    BTYPE type COLON (6);	/* type */
    QUALIFIER qual COLON (2);	/* type qualifier (const and/or volatile) */
    STATE state COLON (5);
    SIZE    size;		/* size in bytes of the type */
    TYP    *btp;		/* derived type */
    TYP    *next;		/* next type in type table */
    const CHAR *sname;		/* type name */
    union {
	RANGE  *range;		/* integral type range */
	FUNC   *func;		/* function parameters */
	STRUCT *structure;	/* struct/union members */
	struct {
	    BITSIZE width;	/* bit-field width */
	    BITSIZE offset;	/* bit-field offset */
	} bitfield;
	SIZE    index;		/* array size */
    } t;
};

#define	array_index(tp)		(tp)->t.index
#define	bitfield_offset(tp)	(tp)->t.bitfield.offset
#define	bitfield_width(tp)	(tp)->t.bitfield.width
#define	enumtype(tp)		(tp)->btp	/* type of the enumeration */
#define	is_ansi(tp)		((tp)->state & STATE_ANSI)
#define	is_array_assignment(tp)	((tp)->state & STATE_ARRAYASSIGN)
#define	is_bitfield_type(tp)	((tp)->type == bt_bitfield || (tp)->type == bt_ubitfield)
#define	is_char(tp)		((tp)->type == bt_char || (tp)->type == bt_charu)
#define	is_class(tp)		((tp)->type == bt_class)
#define	is_const_qualified(tp)	((tp)->qual & QUAL_CONST)
#define	is_derived_type(tp)	((tp)->state & STATE_DERIVED)
#define	is_ellipsis(tp)		((tp)->type == bt_ellipsis)
#define	is_enum(tp)		((tp)->state & STATE_ENUM)
#define	is_float(tp)		((tp)->type == bt_float)
#define	is_func(tp)		((tp)->type == bt_func)
#define	is_qualified_type(tp)	((tp)->qual != (QUALIFIER)0)
#define	is_restrict_qualified(tp)	((tp)->qual & QUAL_RESTRICT)
#define	is_same_size(tp1,tp2)	((tp1)->size == (tp2)->size)
#define	is_same_type(tp1,tp2)	((tp1)->type == (tp2)->type)
#define	is_unknown_size(tp)	((tp)->size == UNKNOWN_SIZE)
#define	is_void(tp)		((tp)->type == bt_void)
#define	is_volatile_qualified(tp)	((tp)->qual & QUAL_VOLATILE)
#define	members(tp)		(tp)->t.structure->lst	/* struct/union members */
#define	membersof(tp)		(members(tp)->symbols.head)
#define	nameoftype(tp)		((tp)->sname)
#define	nexttyp(tp)		(tp)->next
#define	parameters(tp)		(tp)->t.func->lst	/* function parameters */
#define	register_usage(tp)	(tp)->t.func->regusage
#define	parametersof(tp)	(parameters(tp)->symbols.head)
#define	rangeof(tp)		(tp)->t.range	/* integral type range */
#define	referenced_type(tp)	(tp)->btp	/* object pointer points at */
#define	returned_type(tp)	(tp)->btp	/* function return type */

#define	set_ansi(tp)		set_state(tp, STATE_ANSI)
#define	set_array_assignment(tp)	set_state(tp, STATE_ARRAYASSIGN)
#define	set_array_index(tp,i)	((tp)->t.index = (i))
#define	set_bit_offset(tp,o)	((tp)->t.bitfield.offset = (o))
#define	set_bit_width(tp,w)	((tp)->t.bitfield.width = (w))
#define	set_derived(tp)		set_state(tp, STATE_DERIVED)
#define	set_enum(tp)		set_state(tp, STATE_ENUM)
#define	set_enumtype(tp,t)	((tp)->btp = (t))
#define	set_members(tp,b)	((tp)->t.structure->lst = (b))
#define	set_nameoftype(tp,n)	((tp)->sname = (n))
#define	set_nexttyp(tp,n)	((tp)->next = (n))
#define	set_parameters(tp,b)	((tp)->t.func->lst = (b))
#define	set_register_usage(tp,u)	((tp)->t.func->regusage = (u))
#define	set_range(tp,r)		((tp)->t.range = (r))
#define	set_referenced_type(tp,t)	((tp)->btp = (t))
#define	set_returned_type(tp,t)	((tp)->btp = (t))
#define	set_state(tp,s)		((tp)->state |= (s))
#define	set_typedef(tp)		set_state(tp, STATE_TYPEDEF)

/*
 * values for "qual" field
 */
#define	QUAL_NONE	((QUALIFIER) 000)	/* no qualifiers */
#define	QUAL_CONST	((QUALIFIER) 001)	/* constant qualifier */
#define	QUAL_VOLATILE	((QUALIFIER) 002)	/* volatile qualifier */
#define	QUAL_RESTRICT	((QUALIFIER) 004)	/* restrict qualifier */

/*
 * values for "state" field
 */
#define	STATE_NONE	((STATE) 000)	/* none */
#define	STATE_DERIVED	((STATE) 001)	/* array or function definition */
#define	STATE_TYPEDEF	((STATE) 002)	/* type defined by a typedef */
#define	STATE_ANSI	((STATE) 004)	/* parameters are ANSI */
#define	STATE_ENUM	((STATE) 010)	/* type derived from an enumeration */
#define	STATE_ARRAYASSIGN	((STATE) 020)	/* array assignment */

/*
 * value for "size" field
 */
#define UNKNOWN_SIZE	-1L	/* value of 'size' if not known */

/*
 *   Symbol Table Entry
 */
struct sym {
    SYM    *next;		/* next symbol in the table */
    SYM    *hnext;		/* next symbol in the hash chain */
    const CHAR *name;		/* name of the symbol */
    TYP    *tp;			/* type of the symbol */
    union {			/* symbol value: */
	IVAL    i;		/*   signed */
	UVAL    u;		/*   unsigned */
	const CHAR *s;		/*   string */
	LABEL   l;		/*   internal label */
    } value;
    LEVEL level COLON (7);	/* scope level of this symbol */
    STORAGE storage_class COLON (4);	/* symbol storage class */
    STATUS status COLON (5);	/* various flags specific to the symbol */
#ifdef SEQUENCE
    SEQNUM  sequence;		/* sequence point symbol last modified */
#endif
};

/*
 * value for the 'status' field
 */
#define	SYM_USED	((STATUS) 001)	/* Symbol has been used */
#define	SYM_DEFINED	((STATUS) 002)	/* Symbol has been defined */
#define	SYM_SET		((STATUS) 004)	/* Symbol contents have been set */
#define	SYM_OUTSCOPE	((STATUS) 010)	/* Symbol is out of scope */
#define	SYM_OUTPUT	((STATUS) 020)	/* Symbol has been output to assembler file */

#define	is_symbol_defined(s)	((s)->status & SYM_DEFINED)
#define	is_symbol_used(s)	((s)->status & SYM_USED)
#define	is_symbol_set(s)	((s)->status & SYM_SET)
#define	is_symbol_output(s)	((s)->status & SYM_OUTPUT)
#define	is_symbol_outscope(s)	((s)->status & SYM_OUTSCOPE)
#define	is_global_scope(sp)	((sp)->level == GLOBAL_SCOPE)

#define	symbol_defined(s)	((s)->status |= SYM_DEFINED)
#define	symbol_used(s)		((s)->status |= SYM_USED)
#define	symbol_set(s)		((s)->status |= SYM_SET)
#define	symbol_output(s)	((s)->status |= SYM_OUTPUT)
#define	symbol_outscope(s)	((s)->status |= SYM_OUTSCOPE)

#define	is_auto(s)		(storageof(s) == sc_auto)
#define	is_const(s)		(storageof(s) == sc_const)
#define	is_extern(s)		(storageof(s) == sc_external)
#define	is_global(s)		(storageof(s) == sc_global)
#define	is_member(s)		(storageof(s) == sc_member)
#define	is_parms(s)		(storageof(s) == sc_parms)
#define	is_register(s)		(storageof(s) == sc_register)
#define	is_static(s)		(storageof(s) == sc_static)
#define	is_tag(s)		(storageof(s) == sc_tag)
#define	is_typedef(s)		(storageof(s) == sc_typedef)

#define	levelof(s)		((s)->level)
#define	nameof(s)		((s)->name)
#define	nextsym(s)		((s)->next)
#define	storageof(s)		((s)->storage_class)
#define	typeof(s)		((s)->tp)
#define	visibilityof(s)		((s)->visibility)

#define	set_level(s,l)		((s)->level = (l))
#define	set_name(s,n)		((s)->name = (n))
#define	set_nextsym(s,n)	((s)->next = (n))
#define	set_storage(s,st)	((s)->storage_class = (st))
#define	set_type(s,t)		((s)->tp = (t))

#define MAX_ERROR_COUNT 200	/* compilation halts when MAX_ERROR_COUNT reached */
#define MAX_ID_LEN	50	/* initial default length of an identifier */

/*
 * size of tables used in the global optimizer phase
 */
#define REG_LIST	20	/* maximum number of register variable tracked */
#define AUTO_LIST	100	/* maximum number of local variables tracked */

/*
 * switch label table
 */
struct swtab {
    LABEL   tablab;		/* label tag value */
    LABEL   deflab;		/* default label */
    LABEL   beglab;		/* label at start of switch */
    LABEL   numlabs;		/* number of labels */
    LABEL  *labels;		/* array of label tags */
    SWITCH *next;		/* next table */
};

#define	ERROR_RESYNC	3	/* number of tokens to successfully parse
				   after an error before reporting another
				   error */

/*
 *    List of symbolic constants for message numbers.
 */

#ifdef EPOC
#include "c86.rsg"
typedef unsigned int MSGNUM;

#else
enum err_msgs {
#define	MSG(x,y,z)	x,
#include "message.h"
    MSG_DUMMY
};
typedef enum err_msgs MSGNUM;

#endif /* EPOC */
#define	WARN_LEVEL1	WARN_COUNTPARAM
#define	WARN_LEVEL2	WARN_FORMATEND
#define	WARN_LEVEL3	WARN_DUBIOUS
#define	WARN_LEVEL4	WARN_ADDFUNC
#define WARN_LEVEL5	WARN_ACCESS
#define WARN_LEVEL6	WARN_CHAR
#define WARN_LEVEL7	WARN_BITWISE
#define	WARN_LEVEL8	WARN_DUPDECL
#define	MSG_BASE	MSG_ENUMOPT

/*
 * command line processing data structures
 */

typedef struct option OPTION;
typedef struct options OPTIONS;
typedef struct optenum OPTENUM;
typedef struct optset OPTSET;
typedef struct optlist OPTLIST;
typedef unsigned long SETVAL;
typedef unsigned int LISTVAL;

#define	EMPTY_SET	(Ox0UL)
struct optenum {
    const char *text;		/* enumeration name */
    int     val;		/* enumeration value */
};

#define	MEMBER(x)	(((unsigned long)1) << (x))
struct optset {
    const char *text;		/* set name */
    SETVAL  val;		/* set value */
};

struct optlist {
    const char *text;		/* list name */
    int     size;		/* size of the list */
    LISTVAL *val;		/* list value */
};

struct option {
    const char *text;		/* Text for option */
    void    (*cmd) P_ ((BOOL, OPTION *, const char *));		/* option action */
    union {
	void   *value;		/* Address of flag to set */
	int    *ip;
	unsigned long *ulp;
	const char **sp;
    } u1;
    union {
	void   *value2;
	OPTENUM *ep;
	OPTSET *sp;
	OPTLIST *lp;
    } u2;
};

struct options {
    const char *text;
    OPTION *opts;
};

#ifdef DEBUG
/*
 *   The following options are used to control internal debugging
 *   options within the compiler.
 */
#define	DEBUG_GLOBAL	0	/* debugging the global optimiser */
#define	DEBUG_PEEP	1	/* debugging the peephole optimiser */
#define	DEBUG_EXPR	2	/* debugging the expression analysis */
#define	DEBUG_CODE	3	/* debugging the code generator */
#define	DEBUG_REGISTER	4	/* debugging the register handling */
#define	DEBUG_SYMBOL	5	/* debugging the symbol table handling */
#define	DEBUG_FLOW	6	/* debugging the data flow analysis */

#define	is_debugging(x)	(internal_option & (1<<x))
#define	DPRINTF(x)	dprintf x
#else /* DEBUG */
#define	DPRINTF(x)
#endif /* DEBUG */

/*
 *   Some constants which cannot be easily represented by K&R compilers
 *   as there is no such thing as an unsigned constant.   We instead
 *   start it with a capital 'O' ... which makes it an identifier which
 *   we can #define to the appropriate value depending on the compiler.
 */
#ifdef __STDC__
#define Ox0UL		(0x0)
#define Ox1UL		(0x1)
#define	OxffUL		(0xff)
#define	OxffffUL	(0xffff)
#define	Ox8000UL	(0x8000)
#define	Ox00010000UL	(0x00010000)
#define	Ox001fffffUL	(0x001fffff)
#define	Ox007fffffUL	(0x007fffff)
#define	Ox00ffffffUL	(0x00ffffff)
#define	Ox01ffffffUL	(0x01ffffff)
#define	Ox80000000UL	(0x80000000)
#define	Ox7fffffffUL	(0x7fffffff)
#define	OxffffffffUL	(0xffffffff)
#define	OxffefffffUL	(0xffefffff)
#else
#define Ox0UL		((unsigned long)0x0)
#define Ox1UL		((unsigned long)0x1)
#define	OxffUL		((unsigned long)0xff)
#define	OxffffUL	((unsigned long)0xffff)
#define	Ox8000UL	((unsigned long)0x8000)
#define	OxffefffffUL	((unsigned long)0xffefffff)
#define	Ox00010000UL	((unsigned long)0x00010000)
#define	Ox001fffffUL	((unsigned long)0x001fffff)
#define	Ox007fffffUL	((unsigned long)0x007fffff)
#define	Ox00ffffffUL	((unsigned long)0x00ffffff)
#define	Ox01ffffffUL	((unsigned long)0x01ffffff)
#define	Ox80000000UL	((unsigned long)0x80000000)
#define	Ox7fffffffUL	((unsigned long)0x7fffffff)
#define	OxffffffffUL	((unsigned long)0xffffffff)
#define	OxffefffffUL	((unsigned long)0xffefffff)
#endif /* __STDC__ */

/*
 *   The following definitions are used to define the floating point
 *   operations that are used within the compiler.   This allows the
 *   operations to be defined in the most efficient means for the
 *   host system.
 */
#ifdef EPOC
#define	FTST(d)		(p_fcmp(&d, &F_zero) == 0)
#define	FNEG(d1)	p_fneg(&d1)
#define	FASSIGN(d1,d2)	p_fld(&d1, &d2)
#define	FEQ(d1,d2)	(p_fcmp(&d1, &d2) == 0)
#define	FNE(d1,d2)	(p_fcmp(&d1, &d2) != 0)
#define	FLT(d1,d2)	(p_fcmp(&d1, &d2) < 0)
#define	FGT(d1,d2)	(p_fcmp(&d1, &d2) > 0)
#define	FGE(d1,d2)	(p_fcmp(&d1, &d2) >= 0)
#define	FLE(d1,d2)	(p_fcmp(&d1, &d2) <= 0)
#define	FADD(d1,d2)	p_fadd(&d1,&d2)
#define	FSUB(d1,d2)	p_fsub(&d1,&d2)
#define	FMUL(d1,d2)	p_fmul(&d1,&d2)
#define	FDIV(d1,d2)	p_fdiv(&d1,&d2)
#define	FIADD(d1,i)	do {RVAL d2; ITOF(d2,i); FADD(d1,d2); } while(0)
#define	FUMINUS(d1,d2)	FASSIGN(d1,d2); FNEG(d1)
#define	FADD3(d1,d2,d3)	FASSIGN(d1,d2); FADD(d1,d3)
#define	FSUB3(d1,d2,d3)	FASSIGN(d1,d2); FSUB(d1,d3)
#define	FMUL3(d1,d2,d3)	FASSIGN(d1,d2); FMUL(d1,d3)
#define	FDIV3(d1,d2,d3)	FASSIGN(d1,d2); FDIV(d1,d3)
#define	ITOF(d,i)	p_itof(&d,(WORD*)&i)
#define	LTOF(d,l)	p_longtof(&d,(LONG*)&l)
#define	UTOF(d,u)	p_longtof(&d,(LONG*)&u)
#define	FTOL(l,d)	p_intl((LONG*)&l,&d)
#else
#define	FTST(d1)	(d1)
#define	FNEG(d1)	(d1 = -d1)
#define	FASSIGN(d1,d2)	(d1 = d2)
#define	FEQ(d1,d2)	(d1 == d2)
#define	FNE(d1,d2)	(d1 != d2)
#define	FLT(d1,d2)	(d1 < d2)
#define	FGT(d1,d2)	(d1 > d2)
#define	FLE(d1,d2)	(d1 <= d2)
#define	FGE(d1,d2)	(d1 >= d2)
#define	FADD(d1,d2)	(d1 = d1 + d2)
#define	FIADD(d1,i)	(d1 += (RVAL)i)
#define	FSUB(d1,d2)	(d1 -= d2)
#define	FMUL(d1,d2)	(d1 *= d2)
#define	FDIV(d1,d2)	(d1 /= d2)
#define	FUMINUS(d1,d2)	(d1 = -d2)
#define	FADD3(d1,d2,d3)	(d1 = d2 + d3)
#define	FSUB3(d1,d2,d3)	(d1 = d2 - d3)
#define	FMUL3(d1,d2,d3)	(d1 = d2 * d3)
#define	FDIV3(d1,d2,d3)	(d1 = d2 / d3)
#define	ITOF(d,i)	(d = (RVAL)i)
#define	LTOF(d,l)	(d = (RVAL)l)
#define	UTOF(d,u)	(d = (RVAL)u)
#define	FTOL(l,d)	(l = (IVAL)d)
#endif /* EPOC */

#endif /* _CHDR_H */
