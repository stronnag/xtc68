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
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  October 94        - Modified by Thorsten Roskowetz
 *                      Fixes problem with branches greater than 32K
 *
 *  11 Dec 94   DJW   - Fixes merged with QDOS version
 */

#include "jas.h"
#include "proto.h"

BRANCH *brlist = (BRANCH *)NULL;

#ifndef GENERIC
static void moveup(BRANCH *, int);
#endif

VOID add_brnch(cptr, where)
CBUF *cptr;
long where;
{
  register BRANCH *bp;

  bp = ALLO(BRANCH);

  bp->where = where;
#ifndef GENERIC
  /*
   *  This is a check against the fact that one of the exit paths
   *  from generate() can return NULL, and possibly end up here.
   *  It is not clear without exhaustive logic flow analysis
   */
  if (cptr == NULL) {
    fprintf(stderr, "AS68: OPT/add_brnch:  cptr == NULL\n");
  }
#endif
  bp->cptr = cptr;
  bp->link = brlist;
  brlist = bp;
}

#ifndef GENERIC
static void moveup(bp, cnt) register BRANCH *bp;
register int cnt;
{
  register BRANCH *xbp;

  dottxt += cnt;
  /*
   * move all text symbols after this point up 'cnt' bytes
   */
  fixsymval(bp->where, (long)cnt, TXT);
  /*
   * move the location of all branches after this point up
   */
  for (xbp = brlist; xbp != bp; xbp = xbp->link)
    xbp->where += cnt;
}
#endif

void do_opt() {
#ifdef GENERIC
  BRANCH *bp, **lbp;
#else
  BRANCH *bq, **lbp;
  register BRANCH *bp;
  int changed = 0;
  long val;
#endif
  register CBUF *cp;
  SYM *sp;
#if defined(QDOS) || defined(XTC68)
  extern int Optimize;
#ifdef QDOS
  BRANCH *fbp = 0;
#endif
#endif
  /*
   * first take care of jsr's to external routines
   */
  lbp = &brlist;
#if defined(QDOS) || defined(XTC68)
  if (Optimize) {
#endif
    for (bp = brlist; bp != (BRANCH *)NULL; bp = bp->link) {
#ifdef QDOS
      if (fbp) {
        free(fbp);
        fbp = 0;
      }
#endif
      bq = bp->link;
      cp = bp->cptr;
      if (cp->value.value != 0x61 /* BSR */) {
        lbp = &bp->link;
        continue;
      }
      sp = cp[1].value.psym;
      if (sp->flags & SEGMT) {
        lbp = &bp->link;
        continue;
      }
      /*
       * change it to a jsr
       */
      cp[0].nbits = 16;
      cp[0].value.value = 0x4eb9; /* jsr abs.l */
      cp[1].nbits = 32;
      cp[1].action = GENRELOC;
#ifdef GENERIC
      dottxt += 4;
      /*
       * move all text symbols after this point up four bytes
       */
      fixsymval(bp->where, 4L, TXT);
      /*
       * move the location of all branches after this point up
       */
      {
        register BRANCH *xbp;

        for (xbp = brlist; xbp != bp; xbp = xbp->link)
          xbp->where += 4;
      }
#else
    moveup(bp, 4);
#endif /* GENERIC */
      /*
       * remove this entry from the branch list
       */
      *lbp = bp->link;
#ifndef QDOS
      /*
       * don't bother to free it, it will just make later ALLO's
       * slower
       */
#else
    /*
     * do bother. maybe this will make jas/as68 less memory-hungry
     * Erling Jacobsen, 27 Jun '92
     */
    fbp = bp; /* cannot free just now, as referenced in for(;;) */
              /* see above (and below) */
#endif
    }
#ifdef QDOS
    if (fbp) {
      free(fbp);
      fbp = 0;
    }
#endif
  }

  /*
   * now do normal branches and remaining bsr's
   */
  do {
    changed = 0;
    lbp = &brlist;
#ifdef GENERIC
    for (bp = brlist; bp != (BRANCH *)NULL; bp = bp->link) {
      register long val;
#else
  for (bp = brlist; bp != (BRANCH *)NULL; bp = bq) {
    bq = bp->link;
#endif
      cp = bp->cptr;
      sp = cp[1].value.psym;

      val = cp[1].value.value + sp->value - bp->where;
#ifdef GENERIC
      if (val >= -128 && val <= 127 && val != 0) {
#else
    if (val >= -128 && val < 128 && val != 0) {
#endif
        lbp = &bp->link;
        continue;
      }
#ifdef GENERIC
      /*
       * change it to a 16-bit displacement
       */
      cp[0].nbits = 16;
      cp[0].value.value <<= 8;
      cp[1].nbits = 16;
      cp[1].action = GENPCREL;
      dottxt += 2;
      /*
       * move all text symbols after this point up two bytes
       */
      fixsymval(bp->where, 2L, TXT);
      /*
       * move the location of all branches after this point up
       */
      {
        register BRANCH *xbp;

        for (xbp = brlist; xbp != bp; xbp = xbp->link)
          xbp->where += 2;
      }
      /*
       * remove this entry from the branch list
       */
      *lbp = bp->link;
#else
    /*
     * change it to a 16-bit displacement if possible
     */
    if (val >= -32768 && val < 32768) {
      if (cp[1].nbits != 16) {
        cp[0].nbits = 16;
        cp[0].value.value <<= 8;
        cp[1].nbits = 16;
        cp[1].action = GENPCREL;
        moveup(bp, 2);
        changed = 1;
      }
      lbp = &bp->link;
      continue;
    }
    /*
     * change it to jsr or jmp
     */
    cp[0].nbits = 16;
    switch ((int)cp[0].value.value) {
    case 0x61:
    case 0x6100:                  /* bsr */
      cp[0].value.value = 0x4eb9; /* jsr */
      break;
    case 0x60:
    case 0x6000:                  /* bra */
      cp[0].value.value = 0x4ef9; /* jmp */
      break;
    default: {
      extern int line;
      error(line, "conditional branch out of range");
    }
    }
    moveup(bp, cp[1].nbits == 16 ? 2 : 4);
    cp[1].nbits = 32;
    cp[1].action = GENRELOC;
    /*
     * remove this entry from the branch list
     */
    *lbp = bq;
    free(bp);
    bp = 0;
#endif /* ! GENERIC */
      changed = 1;
    }

  } while (changed);
}
