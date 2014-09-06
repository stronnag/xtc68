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
#include "outproto.h"

/********************************************************** Static Variables */

#ifdef CPU_DEFINED
static BOOL was_bitfield = FALSE;

#endif /* CPU_DEFINED */

static UVAL bit_value = (UVAL) 0;
static int level = 0;


/*********************************************** Static Function Definitions */

static EXPR *constexpr P_ ((TYP *));
static SIZE initarray P_ ((TYP *, BOOL));
static SIZE initbitfield P_ ((TYP *));
static SIZE initchar P_ ((TYP *));
static SIZE initlong P_ ((TYP *));
static SIZE initpointer P_ ((TYP *));
static SIZE initshort P_ ((TYP *));
static SIZE initstruct P_ ((TYP *, BOOL));
static SIZE inittype P_ ((TYP *));
static SIZE initunion P_ ((TYP *, BOOL));
static void check_brace P_ ((BOOL));

#ifdef CPU_DEFINED
static SIZE alignfield P_ ((SIZE, SIZE));

#endif /* CPU_DEFINED */

#ifdef TOPSPEED
static SIZE initfunc P_ ((BOOL));

#endif /* TOPSPEED */

#ifdef FLOAT_SUPPORT
#ifndef FLOAT_BOOTSTRAP
static SIZE initfloat P_ ((TYP *));
static SIZE initdouble P_ ((TYP *));
static SIZE initlongdouble P_ ((TYP *));

#endif /* FLOAT_BOOTSTRAP */
#endif /* FLOAT_SUPPORT */

/*****************************************************************************/

static void check_brace P1 (BOOL, brace_seen)
{
    if (!brace_seen) {
	if (level == 1) {
	    message (ERR_BRACE);
	} else {
	    message (WARN_BRACE);
	}
    }
}


SIZE align P2 (const TYP *, tp, SIZE, offset)
{
    SIZE    size = 0L;

#ifdef CPU_DEFINED
    SIZE    al = alignment_of_type (tp);

#else
    SIZE    al = tp->size;

#endif
    if (al) {
	while ((offset + size) % al)
	    size++;
    }
    return size;
}

#ifdef CPU_DEFINED
/*
 * cast an argument back which has been widened on the caller's side.
 * append the resulting assignment expression to init_node
 */
static void castback P3 (SIZE, offset, TYP *, tp1, TYP *, tp2)
{
    EXPR   *ep1, *ep2;

    if (is_same_type (tp1, tp2)) {
	return;
    }
    ep2 = mk_autocon (offset);
    ep1 = copynode (ep2);
    ep2 = mk_ref (ep2, tp1);
    ep2 = mk_node (en_cast, ep2, NIL_EXPR, tp2);
    ep1 = mk_ref (ep1, tp2);
    ep1 = mk_node (en_assign, ep1, ep2, tp2);

    if (init_node == NIL_EXPR) {
	init_node = ep1;
    } else {
	init_node = mk_node (en_comma, init_node, ep1, tp2);
    }
}


/*
 *   Round 'size' up to a multiple of 'algn'
 */
static SIZE roundup P2 (SIZE, size, SIZE, algn)
{
    if (size % algn) {
	size = ((size / algn) + 1) * algn;
    }
    return size;
}

SIZE calculate_offset P4 (SYM *, sp, SIZE, offset, STORAGE, def_sc, BOOL, promoted)
{
    TYP    *tp = typeof (sp);
    SIZE    size;

    switch (storageof (sp)) {
    case sc_register:
	if (def_sc == sc_parms) {
	    goto parms;
	}
	/*FALLTHRU */
    case sc_auto:
	size = tp->size;
	switch (tp->type) {
	case bt_struct:
	case bt_union:
	    size = roundup (size, alignment_of_type (tp));
	    break;
	default:
	    break;
	}
	if (g_is_ascending_stack ()) {
	    sp->value.i = offset + align (tp, offset) + 1;
	} else {
	    sp->value.i = -(offset + align (tp, offset) + size);
	}
	offset += align (tp, offset) + size;
	break;
    case sc_static:
	if (sp->value.l == UNDEF_LABEL) {
	    sp->value.l = nextlabel++;
	}
	break;
    case sc_parms:
      parms:
	/*
	 * If we don't have a prototype in scope (ie. promoted is true)
	 * then parameters may have been widened when passed.  By knowing
	 * whether this is a BIGendian or LOWendian machine we can adjust
	 * the stack offset in order to perform the implicit narrowing cast.
	 */
	switch (tp->type) {
	case bt_char:
	case bt_charu:
	case bt_uchar:
	case bt_schar:
	    if (g_is_ascending_stack ()) {
		sp->value.i = -(offset + tp->size) + 1;
	    } else {
		if (g_is_bigendian ()) {
		    tp = promoted ? promote_type (tp) : tp_short;
		    sp->value.i = offset + tp->size - 1L;
		} else {
		    tp = tp_int;
		    sp->value.i = offset;
		}
	    }
	    break;
	case bt_short:
	case bt_ushort:
	case bt_int16:
	case bt_uint16:
	    if (g_is_ascending_stack ()) {
		sp->value.i = -(offset + tp->size) + 1;
	    } else {
		if (g_is_bigendian ()) {
		    tp = promoted ? promote_type (tp) : tp_short;
		    sp->value.i = offset + tp->size - 2L;
		} else {
		    tp = tp_int;
		    sp->value.i = offset;
		}
	    }
	    break;
	case bt_float:
	    if (g_is_ascending_stack ()) {
		sp->value.i = -(offset + tp->size) + 1L;
	    } else {
		if (promoted) {
		    castback (offset, tp_double, tp);
		    tp = tp_double;
		}
		sp->value.i = offset;
	    }
	    break;
	case bt_pointer16:
	case bt_pointer32:
	    tp = tp_pointer;
	    goto lab;		/* common code with function */
	case bt_func:
	    tp = tp_func;
	    /*
	     * arrays and functions are never passed. They are really
	     * Pointers
	     */
	  lab:if (is_derived_type (typeof (sp))) {
		global_flag++;
		set_type (sp, copy_type (typeof (sp)));
		global_flag--;
		typeof (sp)->state &= (STATE) ~(unsigned) STATE_DERIVED;
		typeof (sp)->size = tp->size;
	    }
	    /*FALLTHRU */
	default:
	    if (g_is_ascending_stack ()) {
		sp->value.i = -(offset + tp->size) + 1L;
	    } else {
		sp->value.i = offset;
	    }
	    break;
	}
	offset += tp->size;
	break;
    default:
	break;
    }
    return offset;
}
#endif /* CPU_DEFINED */

SIZE doinit P2 (SYM *, sp, SIZE, offset)
{
    TYP    *tp = typeof (sp);

#ifdef CPU_DEFINED
    LABEL   label;
    EXPR   *ep1;

#endif /* CPU_DEFINED */
    EXPR   *ep2;
    int     brace_level = 0;

    switch (lastst) {
    case tk_assign:
	/* Initializer */
	getsym ();
	symbol_set (sp);
	switch (storageof (sp)) {
	case sc_auto:
	case sc_register:
	    /* AUTO Initialisation */
	    switch (tp->type) {
	    case bt_pointer16:
	    case bt_pointer32:
		if (!is_array_type (tp)) {
		    goto common;
		}
		/* This must be an array.  However it is possible that the
		 * size of the array isn't known until we have parsed the
		 * initializer.  We must therefore delay calculating the
		 * offset of the item on the stack until we know it's actual
		 * size.
		 */
		/*FALLTHRU */
	    case bt_struct:
	    case bt_union:
#ifndef SYNTAX_CORRECT
		if (trad_option) {
		    message (ERR_ILLINIT);
		}
#endif /* SYNTAX_CORRECT */
		if (lastst == tk_id || lastst == tk_star || lastst == tk_openpa) {
		    goto common;
		}
#ifdef CPU_DEFINED
		label = nextlabel++;
		put_kseg (alignment_of_type (tp));
		put_label (label);
#endif /* CPU_DEFINED */
		VOIDCAST inittype (tp);

		if (is_array_type (tp)) {
		    tp = copy_type (tp);
		    set_array_assignment (tp);
		}
#ifdef CPU_DEFINED
		ep2 = mk_lcon (label);
		ep2 = mk_ref (ep2, tp);
		uses_structassign = TRUE;
#endif /* CPU_DEFINED */
		break;
	    default:
	      common:
		while (lastst == tk_begin) {
		    brace_level++;
		    getsym ();
		}
		ep2 = exprnc ();
#ifndef SYNTAX_CORRECT
		if (ep2 == NIL_EXPR) {
		    message (ERR_ILLINIT);
		    break;
		}
#endif /* SYNTAX_CORRECT */
		ep2 = implicit_castop (ep2, tp);
	    }
#ifdef CPU_DEFINED
	    offset = calculate_offset (sp, offset, sc_auto, FALSE);
	    ep1 = mk_symnode (sp);
	    ep1 = mk_ref (ep1, tp);
	    ep1 = mk_node (en_assign, ep1, ep2, tp);

	    if (init_node == NIL_EXPR) {
		init_node = ep1;
	    } else {
		init_node = mk_node (en_comma, init_node, ep1, tp);
	    }
#endif /* CPU_DEFINED */

	    while (brace_level--)
		needpunc (tk_end);
	    break;

	    /* Normal initializers */
	case sc_static:
#ifdef CPU_DEFINED
	    offset = calculate_offset (sp, offset, sc_auto, FALSE);
	    put_dseg (alignment_of_type (tp));
	    put_label (sp->value.l);
#endif /* CPU_DEFINED */
	    VOIDCAST inittype (tp);

	    break;
	case sc_external:
#ifndef SYNTAX_CORRECT
	    if (!is_global_scope (sp)) {
		message (ERR_ILLINIT);
	    }
#endif /* SYNTAX_CORRECT */
	    set_storage (sp, sc_global);
	    /*FALLTHRU */
	case sc_global:
#ifdef CPU_DEFINED
	    put_dseg (alignment_of_type (tp));
	    put_name (sp);
#endif /* CPU_DEFINED */
	    VOIDCAST inittype (tp);

	    break;
	case sc_parms:
	    message (ERR_ILLINIT);
	    break;
	default:
	    CANNOT_REACH_HERE ();
	    break;
	}
#ifndef SYNTAX_CORRECT
	if (is_symbol_defined (sp)) {
	    message (ERR_REDECL, nameof (sp));
	}
#endif /* SYNTAX_CORRECT */
	symbol_defined (sp);
	break;
    default:
#ifndef SYNTAX_CORRECT
	switch (storageof (sp)) {
	case sc_auto:
	case sc_global:
	case sc_static:
	    if (is_const_qualified (typeof (sp))) {
		message (WARN_CONSTINIT, nameof (sp));
	    }
	    break;
	default:
	    break;
	}
#endif /* SYNTAX_CORRECT */
#ifdef CPU_DEFINED
	offset = calculate_offset (sp, offset, sc_auto, FALSE);
#endif /* CPU_DEFINED */
	break;
    }
    sequence_point ();
    return offset;
}


static SIZE inittype P1 (TYP *, tp)
{
    BOOL    brace_seen = FALSE;
    SIZE    nbytes;

    level++;
    if (lastst == tk_begin) {
	brace_seen = TRUE;
	getsym ();
    }
    switch (tp->type) {
    case bt_char:
    case bt_charu:
    case bt_uchar:
    case bt_schar:
	nbytes = initchar (tp);
	break;
    case bt_short:
    case bt_ushort:
    case bt_int16:
    case bt_uint16:
	nbytes = initshort (tp);
	break;
    case bt_pointer16:
    case bt_pointer32:
	if (is_array_type (tp)) {
	    nbytes = initarray (tp, brace_seen);
	} else {
	    nbytes = initpointer (tp);
	}
	break;
    case bt_bitfield:
    case bt_ubitfield:
	nbytes = initbitfield (tp);
	break;
    case bt_int32:
    case bt_uint32:
    case bt_ulong:
    case bt_long:
	nbytes = initlong (tp);
	break;
    case bt_struct:
	nbytes = initstruct (tp, brace_seen);
	break;
    case bt_union:
	nbytes = initunion (tp, brace_seen);
	break;
#ifdef FLOAT_SUPPORT
#ifndef FLOAT_BOOTSTRAP
    case bt_float:
	nbytes = initfloat (tp);
	break;
    case bt_double:
	nbytes = initdouble (tp);
	break;
    case bt_longdouble:
	nbytes = initlongdouble (tp);
	break;
#endif /* FLOAT_BOOTSTRAP */
#endif /* FLOAT_SUPPORT */
#ifdef TOPSPEED
    case bt_func:
	if (topspeed_option) {
	    nbytes = initfunc (brace_seen);
	    break;
	}
	/*FALLTHRU */
#endif /* TOPSPEED */
    default:
#ifndef SYNTAX_CORRECT
	message (ERR_NOINIT);
	nbytes = 0L;
#endif /* SYNTAX_CORRECT */
	break;
    }
    if (brace_seen) {
	if (lastst == tk_comma) {
	    getsym ();
	}
	needpunc (tk_end);
    }
    level--;
    return nbytes;
}

#ifdef TOPSPEED
/*
 *   The TopSpeed C Compiler allows function to be "inlined" as
 *   a series of initialized bytes.
 */
static SIZE initfunc P1 (BOOL, brace_seen)
{
    SIZE    nbytes;

    check_brace (brace_seen);
    for (nbytes = 0L;;) {
	VOIDCAST arithexpr (tp_uchar);

	nbytes += tp_uchar->size;
	if (lastst != tk_comma) {
	    break;
	}
	getsym ();		/* comma */
	if (lastst == tk_end) {
	    break;
	}
    }
    return nbytes;
}
#endif /* TOPSPEED */

static SIZE initarray P2 (TYP *, tp, BOOL, brace_seen)
{
    SIZE    nbytes;
    TYP    *rtp = referenced_type (tp);

    if (((lastst == tk_sconst) &&
       (is_char (rtp) || rtp->type == bt_uchar || rtp->type == bt_schar)) ||
	((lastst == tk_wsconst && is_same_type (rtp, tp_wchar)))) {
#ifdef CPU_DEFINED
	SIZE    len;
	const CHAR *p;

#endif /* CPU_DEFINED */

	nbytes = (SIZE) lastsymlen;
#ifdef CPU_DEFINED
	for (len = nbytes, p = lastsym; len--;)
	    put_byte ((UVAL) *p++);
#endif /* CPU_DEFINED */
	if (is_unknown_size (tp)) {
	    tp->size = nbytes + 1L;
	}
	getsym ();		/* skip sconst/wsconst */
    } else {
	check_brace (brace_seen);
	for (nbytes = 0L;;) {
	    nbytes += inittype (rtp);
	    if (lastst != tk_comma || nbytes == tp->size) {
		break;
	    }
	    getsym ();		/* comma */
	    if (lastst == tk_end) {
		break;
	    }
	}
#ifndef SYNTAX_CORRECT
	if (tp->size > nbytes) {
	    message (WARN_INCOMPLETE);
	}
#endif /* SYNTAX_CORRECT */
    }

    if (is_unknown_size (tp)) {
	tp->size = nbytes;
#ifndef SYNTAX_CORRECT
    } else if (tp->size < nbytes) {
	message (ERR_INITSIZE);
#endif /* SYNTAX_CORRECT */
    }
#ifdef CPU_DEFINED
    for (; tp->size > nbytes; nbytes++)
	put_byte (Ox0UL);
#endif /* CPU_DEFINED */
    return nbytes;
}

static SIZE initunion P2 (TYP *, tp, BOOL, brace_seen)
{
    SYM    *sp = membersof (tp);
    SIZE    nbytes;

/*
 * Initialize the first branch
 */
    if (sp == NIL_SYM) {
	return 0L;
    }
    check_brace (brace_seen);
    nbytes = inittype (typeof (sp));
#ifdef CPU_DEFINED
    for (; nbytes < tp->size; nbytes++)
	put_byte (Ox0UL);
#endif /* CPU_DEFINED */
    return nbytes;
}

#ifdef CPU_DEFINED
static SIZE alignfield P2 (SIZE, nbytes, SIZE, offset)
{
    if (was_bitfield && nbytes < offset) {
	if (tp_int->size == 2L) {
	    put_word (bit_value);
	} else {
	    put_dword (bit_value);
	}
	bit_value = (UVAL) 0;
	was_bitfield = FALSE;
	nbytes += tp_int->size;
    }
    for (; nbytes < offset; nbytes++)
	put_byte (Ox0UL);
    return nbytes;
}
#endif /* CPU_DEFINED */

static SIZE initstruct P2 (TYP *, tp, BOOL, brace_seen)
{
    SYM    *sp;

#ifdef CPU_DEFINED
    SIZE    nbytes = 0L;

#endif /* CPU_DEFINED */
    check_brace (brace_seen);
    for (sp = membersof (tp); sp != NIL_SYM;) {
	if (is_const (sp)) {
	    sp = nextsym (sp);
	    continue;
	}
#ifdef CPU_DEFINED
	nbytes = alignfield (nbytes, sp->value.i);
	nbytes += inittype (typeof (sp));
#endif /* CPU_DEFINED */
	sp = nextsym (sp);
	if (lastst != tk_comma || sp == NIL_SYM) {
	    break;
	}
	getsym ();		/* comma */
	if (lastst == tk_end) {
	    break;
	}
    }
#ifndef SYNTAX_CORRECT
    if (sp != NIL_SYM) {
	message (WARN_INCOMPLETE);
    }
#endif /* SYNTAX_CORRECT */
#ifdef CPU_DEFINED
    nbytes = alignfield (nbytes, tp->size);
#endif /* CPU_DEFINED */
    return tp->size;
}

static SIZE initbitfield P1 (TYP *, tp)
{
    EXPR   *ep;
    UVAL    val;

#ifdef CPU_DEFINED
    was_bitfield = TRUE;
#endif /* CPU_DEFINED */
    if ((ep = constexpr (tp)) == NIL_EXPR) {
	return 0L;
    }
    val = ep->v.u & bitmask (bitfield_width (tp));
    bit_value |= (val << (int) bitfield_offset (tp));
    return 0L;
}

static SIZE initchar P1 (TYP *, tp)
{
    EXPR   *ep;

    if ((ep = constexpr (tp)) == NIL_EXPR) {
	return 0L;
    }
#ifdef CPU_DEFINED
    put_char (unsymbolexpr (ep));
#endif /* CPU_DEFINED */
    return tp->size;
}

static SIZE initshort P1 (TYP *, tp)
{
    EXPR   *ep;

    if ((ep = constexpr (tp)) == NIL_EXPR) {
	return 0L;
    }
#ifdef CPU_DEFINED
    put_short (unsymbolexpr (ep));
#endif /* CPU_DEFINED */
    return tp->size;
}

static SIZE initlong P1 (TYP *, tp)
{
    EXPR   *ep;

    if ((ep = constexpr (tp)) == NIL_EXPR) {
	return 0L;
    }
#ifdef CPU_DEFINED
    put_long (unsymbolexpr (ep));
#endif /* CPU_DEFINED */
    return tp->size;
}

#ifdef FLOAT_SUPPORT
#ifndef FLOAT_BOOTSTRAP
static SIZE initfloat P1 (TYP *, tp)
{
    RVAL    val;

    floatexpr (tp, &val);
#ifdef CPU_DEFINED
    put_float (&val);
#endif /* CPU_DEFINED */
    return tp->size;
}

static SIZE initdouble P1 (TYP *, tp)
{
    RVAL    val;

    floatexpr (tp, &val);
#ifdef CPU_DEFINED
    put_double (&val);
#endif /* CPU_DEFINE */
    return tp->size;
}

static SIZE initlongdouble P1 (TYP *, tp)
{
    RVAL    val;

    floatexpr (tp, &val);
#ifdef CPU_DEFINED
    put_longdouble (&val);
#endif /* CPU_DEFINED */
    return tp->size;
}
#endif /* FLOAT_BOOTSTRAP */
#endif /* FLOAT_SUPPORT */

static SIZE initpointer P1 (TYP *, tp)
{
    EXPR   *ep;

    if ((ep = constexpr (tp)) == NIL_EXPR) {
	return 0L;
    }
#ifdef CPU_DEFINED
    put_pointer (unsymbolexpr (ep));
#endif /* CPU_DEFINED */
    return tp->size;
}

static EXPR *constexpr P1 (TYP *, tp)
{
    EXPR   *ep = exprnc ();

#ifndef SYNTAX_CORRECT
    if (ep == NIL_EXPR) {
	message (ERR_CONSTEXPR);
	getsym ();
	return NIL_EXPR;
    }
#endif /* SYNTAX_CORRECT */
    ep = implicit_castop (ep, tp);
    ep = constantopt (ep);
    /* ep may still contain casts between 32-bit integers and pointers */
    while ((ep->nodetype == en_cast)
	   && (ep->etp->type == bt_long ||
	       ep->etp->type == bt_ulong ||
	       ep->etp->type == bt_int32 ||
	       ep->etp->type == bt_uint32 ||
	       ep->etp->type == bt_pointer32)
	   && (ep->v.p[0]->etp->type == bt_long ||
	       ep->v.p[0]->etp->type == bt_ulong ||
	       ep->v.p[0]->etp->type == bt_int32 ||
	       ep->v.p[0]->etp->type == bt_uint32 ||
	       ep->v.p[0]->etp->type == bt_pointer32)) {
	ep->v.p[0]->etp = ep->etp;
	ep = ep->v.p[0];
    }

#ifndef SYNTAX_CORRECT
    if (!tst_const (ep)) {
	message (ERR_CONSTEXPR);
	getsym ();
	return NIL_EXPR;
    }
#endif /* SYNTAX_CORRECT */
    return ep;
}
