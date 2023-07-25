
/*
 * Copyright (c) 1988,1991 by Sozobon, Limited.  Author: Joseph M Treat
 *
 * Permission is granted to anyone to use this software for any purpose
 * on any computer system, and to redistribute it freely, with the
 * following restrictions:
 * 1) No charge may be made other than reasonable charges for reproduction.
 * 2) Modified versions must be clearly marked as such.
 * 3) The authors are not responsible for any harmful consequences
 *    of using this software, even if they result from defects in it.
 */

#include "jas.h"
#include "proto.h"

#define R_ABS 0
#define R_DAT 1
#define R_TXT 2
#define R_BSS 3
#define R_EXT 4
#define R_UPPER 5
#define R_FIRST 7

#define RBLEN 256

typedef struct _reloc {
  struct _reloc *next;
  unsigned short cnt;
  unsigned short reloc[RBLEN];
} RELOC;

RELOC *relptr = (RELOC *)NULL;
RELOC *curptr;

extern SYM dot;
extern long newdot;

#define SEG(p) ((p)->flags & SEGMT)

#if defined(QDOS) || defined(XTC68)
static void output_with_xref(unsigned short rval, CBUF *code) {
  unsigned short val, segval;
#ifdef XTC68
  unsigned
#endif
      char c;

  switch (rval) {
  case R_TXT:
    segval = 0xFFFF;
    goto DO_XREF;
  case R_DAT:
    segval = 0xFFFE;
    goto DO_XREF;
  case R_BSS:
    segval = 0xFFFD;
    goto DO_XREF;
  case R_EXT:
    segval = (code->value.psym->index + 1);

  DO_XREF:
#ifndef XTC68
    val = 0xFB07;
#else
    val = 0x07FB;
#endif
    output(((char *)&val), 2, 1);
#if !defined(UNIXHOST) & !defined(XTC68)
    output((char *)&code->value.value, sizeof(char), 4);
#else
    output(mklowlong(code->value.value), sizeof(char), 4);
#endif
    /*
     * Now calculate the truncation rule
     */
#if 0
	/* Is is signed or unsigned */
	c = (code->nbits) | ( (rval == R_PCREL) ? 0x28 : 0x50 ); /* Signed PC-rel or
						 Unsigned - run time reloc */
#else
    c = (code->nbits / 8) | 0x50;

#endif
    output((char *)&c, 1, 1);
    c = 0x2B;
    output((char *)&c, 1, 1);
#ifndef XTC68
    output((char *)&segval, 2, 1);
#else
    output(swapw((char *)&segval, 1), 2, 1);
#endif
    c = 0xFB;
    output((char *)&c, 1, 1);
    break;
  default: /* Just write out the absolute value */
#if !defined(UNIXHOST) && !defined(XTC68)
    code->value.value <<= (32 - code->nbits);
    output2fb((unsigned char *)&code->value.value, sizeof(char), (int)code->nbits / 8);
#else
    switch (code->nbits) {
    case 8:
      output2fb((unsigned char *)mklowbyte(code->value.value), sizeof(char), 1);
      break;
    case 16:
      output2fb((unsigned char *)mklowshort(code->value.value), sizeof(char), 2);
      break;
    case 32:
      output2fb((unsigned char *)mklowlong(code->value.value), sizeof(char), 4);
      break;
    }
#endif
    break;
  }
}
#endif /* QDOS */

VOID addrel(unsigned short rval) {
  if (curptr->cnt == RBLEN) {
    curptr->next = ALLO(RELOC);
    curptr = curptr->next;
  }
  curptr->reloc[curptr->cnt++] = rval;
}

VOID dumprel(void) {
  register RELOC *rp;

  for (rp = relptr; rp; rp = rp->next) {
#if defined(UNIXHOST) || defined(XTC68)
    swapw((char *)rp->reloc, rp->cnt);
#endif
#if !defined(QDOS) && !defined(XTC68)
    output((char *)rp->reloc, sizeof(unsigned short), (int)rp->cnt);
#else
    output2fb((unsigned char *)rp->reloc, sizeof(unsigned short), (int)rp->cnt);
#endif
  }
  return;
}

VOID translate(unsigned short seg, int null) {
  register unsigned short stype;
  register CBUF *code;
  register unsigned short rval, orval;
  int havebyte = 0;
#ifdef USE_LINE
  int cline = 0;
#endif
#ifdef LABELDIFF
  LDIFF *lp;
#endif /* LABELDIFF */

  if (relptr == (RELOC *)NULL) {
    relptr = ALLO(RELOC);
    curptr = relptr;
  }

  rval = R_ABS;
#ifdef GENERIC
  for (; code = cget(seg); newdot += code->nbits / 8) {
#else
  for (; (code = cget(seg)) != NULL; newdot += code->nbits / 8) {
#endif
#ifdef USE_LINE
    cline = code->line;
#endif
    orval = rval;
    rval = R_ABS;
    if (code->action == GENSTMT)
      dot.value = newdot;

#ifdef LABELDIFF
    if (code->action == GENLDIFF) {

      code->action = GENVALUE;

      lp = (LDIFF *)code->value.value;

      code->value.value = lp->plus->value - lp->minus->value + lp->constant;
      free(lp);
      code->value.psym = (SYM *)NULL;
    }

#endif /* LABELDIFF */
    stype = UNK;
    if (code->value.psym) {
      stype = SEG(code->value.psym);
      switch (stype) {
      case DAT:
        rval = R_DAT;
        break;
      case TXT:
        rval = R_TXT;
        break;
      case BSS:
        rval = R_BSS;
        break;
      default:
        rval = R_EXT;
        break;
      }
      if (code->value.psym->flags & EQUATED) {
        stype = EQUATED;
        rval = R_ABS;
      }
    }
    if (code->action == GENPCREL) {
      if (code->value.psym && stype != TXT)
#ifdef USE_LINE
        warn(cline, "illegal pc-relative reference");
#else
        warn(0, "illegal pc-relative reference");
#endif
      rval = R_ABS;
      stype = UNK;
      code->value.value -= (dot.value + 2);
    }

    if (code->value.psym) {
      if (code->value.psym->flags & (SEGMT | EQUATED))
        code->value.value += code->value.psym->value;
    }

    /* don't check anything right now ...
            chkvalue( code );
    ... */

#if defined(QDOS) || defined(XTC68)
    output_with_xref((int)rval, code);
#else
#ifndef UNIXHOST
    code->value.value <<= (32 - code->nbits);
    output((char *)&code->value.value, sizeof(char), (int)code->nbits / 8);
#else
    switch (code->nbits) {
    case 8:
      output(mklowbyte(code->value.value), sizeof(char), 1);
      break;
    case 16:
      output(mklowshort(code->value.value), sizeof(char), 2);
      break;
    case 32:
      output(mklowlong(code->value.value), sizeof(char), 4);
      break;
    default:
#ifdef USE_LINE
      error(cline, "internal size error");
#else
      error(0, "internal size error");
#endif
    }
#endif
#endif
    if (rval == R_EXT)
      rval |= (code->value.psym->index << 3);

    if (havebyte == 1 && code->nbits != 8)
#ifdef USE_LINE
      error(cline, "relocation alignment error");
#else
      error(0, "relocation alignment error");
#endif
    if (havebyte == 1 && rval == R_ABS && orval == R_FIRST)
      rval = R_FIRST;
    if (havebyte == 1 && rval != orval)
#ifdef USE_LINE
      error(cline, "bytes not separately relocatable");
#else
      error(0, "bytes not separately relocatable");
#endif

    if (code->action == GENSTMT && rval != R_ABS)
#ifdef USE_LINE
      error(cline, "relocatable operation word");
#else
      error(0, "relocatable operation word");
#endif
    if (code->action == GENSTMT)
      rval = R_FIRST;
    if (code->nbits == 8 && havebyte == 0) {
      if (rval != R_ABS && rval != R_FIRST)
#ifdef USE_LINE
        error(cline, "bytes not separately relocatable");
#else
        error(0, "bytes not separately relocatable");
#endif
      havebyte = 1;
      continue;
    }
    havebyte = 0;

    if (code->nbits == 32) {
      addrel(R_UPPER);
    }
    addrel(rval);
  }

  dot.value = newdot += null;
  while (null--) {
    char zip = 0;

#if !defined(QDOS) && !defined(XTC68)
    output((char *)&zip, sizeof(char), 1);
#else
    output2fb((unsigned char *)&zip, sizeof(char), 1);
#endif
    if (havebyte && rval != R_ABS)
#ifdef USE_LINE
      error(cline, "bytes not separately relocatable");
#else
      error(0, "bytes not separately relocatable");
#endif
    if (havebyte == 0) {
      havebyte = 1;
      rval = R_ABS;
      continue;
    }
    havebyte = 0;
    addrel(rval);
  }
  if (havebyte)
#ifdef USE_LINE
    error(cline, "internal relocation alignment error");
#else
    error(0, "internal relocation alignment error");
#endif
}

VOID chkvalue(CBUF *code)
{
  long value;

  value = code->value.value;

  if (code->nbits != 32) {
    value <<= (32 - code->nbits);
    value >>= (32 - code->nbits);
    if (value != code->value.value)
#ifdef USE_LINE
      warn(code->line, "value overflows byte/word");
#else
      warn(0, "value overflows byte/word");
#endif
  }
}
