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
 */

/*****************************************************************************/

#include "chdr.h"

#ifdef FLOAT_IEEE
#ifndef FLOAT_BOOTSTRAP

#include "expr.h"
#include "cglbdec.h"
#include "proto.h"

/********************************************************* Macro Definitions */

#define F_BIAS		0x7f - 1	/* allow for implicit one bit */
#define F_MAX_EXP	0xff
#define F_MANTDIG	24

/*****************************************************************************/

/*
 *   Convert the floating point value into the binary representation
 *   of an IEEE 32 bit floating point number.
 */
void ieee_single P2 (const RVAL *, dp, unsigned long *, p)
{
    unsigned long mantissa;
    unsigned long sign;
    int     exponent = F_BIAS;
    int     i;
    RVAL    d;

    FASSIGN (d, *dp);

    if (FLT (d, F_zero)) {
	sign = Ox80000000UL;
	FNEG (d);
    } else {
	sign = (unsigned long) 0;
    }
    while (FLT (d, F_half)) {
	FADD (d, d);
	--exponent;
	if (exponent == 0) {
	    *p = (unsigned long) 0;	/* zero fp number */
	    return;
	}
    }

    while (FGE (d, F_one)) {
	FMUL (d, F_half);
	++exponent;
	if (exponent >= F_MAX_EXP) {
	    *p = sign | Ox7fffffffUL;
	    return;
	}
    }

    /*
     *       d in [0.5,1.0[. Now: construct the mantissa
     */

    mantissa = (unsigned long) 0;
    for (i = 0; i < F_MANTDIG; i++) {
	FADD (d, d);
	mantissa <<= 1;
	if (FGE (d, F_one)) {
	    mantissa |= (unsigned long) 0x1L;
	    FSUB (d, F_one);
	}
    }

    /*
     *       round up, if the next bit would be 1
     */

    if (FGE (d, F_half)) {
	mantissa++;
	/* check on mantissa overflow */
	if (mantissa > Ox01ffffffUL) {
	    ++exponent;
	    /* exponent overflow? */
	    if (exponent >= F_MAX_EXP) {
		*p = sign | Ox7fffffffUL;
		return;
	    }
	    mantissa >>= 1;
	}
    }
    /*
     *       put the parts together and return the value
     */
    mantissa &= Ox007fffffUL;	/* hidden bit */
    mantissa |= sign | ((unsigned long) exponent << (F_MANTDIG - 1));
    *p = mantissa;
}


#define D_BIAS		0x3ff-1	/* allow for implicit one bit as well */
#define D_MAX_EXP	0x7ff
#define D_MANTDIG	53

/*
 *   Convert the floating point value into the binary representation
 *   of an IEEE 64 bit floating point number.
 */
void ieee_double P3 (const RVAL *, dp, unsigned long *, p, BOOL, bigendian)
{
    unsigned long mantissa1, mantissa2;
    unsigned long sign;
    int     exponent = D_BIAS;
    BOOL    flag;
    int     i;
    RVAL    d;

    FASSIGN (d, *dp);
    if (FLT (d, F_zero)) {
	sign = Ox80000000UL;
	d = -d;
    } else {
	sign = (unsigned long) 0;
    }

    while (FLT (d, F_half)) {
	FADD (d, d);
	--exponent;
	if (exponent == 0) {
	    p[0] = (unsigned long) 0;	/* zero fp number */
	    p[1] = (unsigned long) 0;
	    return;
	}
    }

    while (FGE (d, F_one)) {
	FMUL (d, F_half);
	++exponent;
	if (exponent >= D_MAX_EXP) {
	    p[!bigendian] = sign | Ox7fffffffUL;
	    p[bigendian] = OxffffffffUL;
	    return;
	}
    }

    /*
     *       d in [0.5,1.0].
     *       Now construct the mantissa
     */

    mantissa1 = (unsigned long) 0;
    mantissa2 = (unsigned long) 0;
    for (i = 0; i < D_MANTDIG; i++) {
	FADD (d, d);
	flag = ((mantissa2 & Ox80000000UL) != (unsigned long) 0L);
	if (flag) {
	    mantissa2 &= Ox7fffffffUL;
	}
	mantissa2 <<= 1;
	mantissa1 <<= 1;
	if (flag) {
	    mantissa1 |= (unsigned long) 0x1L;
	}
	if (FGT (d, F_one)) {
	    mantissa2 |= (unsigned long) 0x1L;
	    FSUB (d, F_one);
	}
    }

    /*
     *       round up, if the next bit would be 1
     */
    if (FGE (d, F_half)) {
	if (mantissa2 == OxffffffffUL) {
	    mantissa2 = (unsigned long) 0;
	    mantissa1++;
	    /* check on mantissa overflow */
	    if (mantissa1 > Ox001fffffUL) {
		++exponent;
		/* exponent overflow? */
		if (exponent >= D_MAX_EXP) {
		    p[!bigendian] = sign | Ox7fffffffUL;
		    p[bigendian] = OxffffffffUL;
		    return;
		}
		flag = ((mantissa1 & (unsigned long) 0x1) != (unsigned long) 0);
		mantissa1 >>= 1;
		mantissa2 >>= 1;
		if (flag) {
		    mantissa2 |= Ox80000000UL;
		}
	    }
	} else {
	    mantissa2++;
	}
    }
    /*
     *       put the parts together and return the value
     */

    mantissa1 &= OxffefffffUL;	/* hidden bit */
    mantissa1 |= sign | ((unsigned long) exponent << (D_MANTDIG - 32 - 1));

    p[!bigendian] = mantissa1;
    p[bigendian] = mantissa2;
}

#define LD_BIAS		0x3fff-1	/* allow for implicit one bit as well */
#define LD_MAX_EXP	0x7fff
#define LD_MANTDIG	64

/*
 *   Convert the floating point value into the binary representation
 *   of an IEEE 64 bit floating point number.
 */
void ieee_longdouble P3 (const RVAL *, dp, unsigned long *, p, BOOL, bigendian)
{
/*KDW --- needs more work */
    unsigned long mantissa1, mantissa2, mantissa3 = 0;
    unsigned long sign;
    int     exponent = LD_BIAS;
    BOOL    flag;
    int     i;
    RVAL    d;

    FASSIGN (d, *dp);
    if (FLT (d, F_zero)) {
	sign = Ox80000000UL;
	d = -d;
    } else {
	sign = (unsigned long) 0;
    }

    while (FLT (d, F_half)) {
	FADD (d, d);
	--exponent;
	if (exponent == 0) {
	    p[0] = (unsigned long) 0;	/* zero fp number */
	    p[1] = (unsigned long) 0;
	    return;
	}
    }

    while (FGE (d, F_one)) {
	FMUL (d, F_half);
	++exponent;
	if (exponent >= D_MAX_EXP) {
	    p[!bigendian] = sign | Ox7fffffffUL;
	    p[bigendian] = OxffffffffUL;
	    return;
	}
    }

    /*
     *       d in [0.5,1.0].
     *       Now construct the mantissa
     */

    mantissa1 = (unsigned long) 0;
    mantissa2 = (unsigned long) 0;
    for (i = 0; i < D_MANTDIG; i++) {
	FADD (d, d);
	flag = ((mantissa2 & Ox80000000UL) != (unsigned long) 0L);
	if (flag) {
	    mantissa2 &= Ox7fffffffUL;
	}
	mantissa2 <<= 1;
	mantissa1 <<= 1;
	if (flag) {
	    mantissa1 |= (unsigned long) 0x1L;
	}
	if (FGT (d, F_one)) {
	    mantissa2 |= (unsigned long) 0x1L;
	    FSUB (d, F_one);
	}
    }

    /*
     *       round up, if the next bit would be 1
     */
    if (FGE (d, F_half)) {
	if (mantissa2 == OxffffffffUL) {
	    mantissa2 = (unsigned long) 0;
	    mantissa1++;
	    /* check on mantissa overflow */
	    if (mantissa1 > Ox001fffffUL) {
		++exponent;
		/* exponent overflow? */
		if (exponent >= D_MAX_EXP) {
		    p[!bigendian] = sign | Ox7fffffffUL;
		    p[bigendian] = OxffffffffUL;
		    return;
		}
		flag = ((mantissa1 & (unsigned long) 0x1) != (unsigned long) 0);
		mantissa1 >>= 1;
		mantissa2 >>= 1;
		if (flag) {
		    mantissa2 |= Ox80000000UL;
		}
	    }
	} else {
	    mantissa2++;
	}
    }
    /*
     *       put the parts together and return the value
     */

    mantissa1 &= OxffefffffUL;	/* hidden bit */
    mantissa1 |= sign | ((unsigned long) exponent << (D_MANTDIG - 32 - 1));

    p[(!bigendian) * 2] = mantissa1;
    p[1] = mantissa2;
    p[bigendian * 2] = mantissa3;
}

#endif /* FLOAT_BOOTSTRAP */
#endif /* FLOAT_IEEE */
