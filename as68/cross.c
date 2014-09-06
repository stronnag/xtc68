
/*
 * Copyright (c) 1991 by Sozobon, Limited.
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

/*
 * handle possible alignment and byte-order problems in cross environment
 */


void pcrossw(w, cp)
short w;
char *cp;
{
#ifdef HOST_LITTLE_ENDIAN
       *(short *)cp = (unsigned short) (w << 8) + (w >> 8);
#else
       *(short *)cp = w;
#endif

}

void pcrossl(l, cp)
long l;
char *cp;
{
       union {
               long l;
               char c[4];
       } u;

       u.l = l;
#ifdef HOST_LITTLE_ENDIAN
       cp[0] = u.c[3];
       cp[1] = u.c[2];
       cp[2] = u.c[1];
       cp[3] = u.c[0];
#else
       *(long *)cp = l;
#endif

}

static char mklowbuf[4];

char * mklowbyte(l)
long l;
{
       mklowbuf[0] = l;
       return mklowbuf;
}

char *mklowshort(l)
long l;
{
       mklowbuf[0] = l>>8;
       mklowbuf[1] = l;
       return mklowbuf;
}

char *
mklowlong(l)
long l;
{
       pcrossl(l, mklowbuf);
       return mklowbuf;
}

char * swapw(cp, n)
char *cp;
long n;
{
    char *p = cp;
#ifdef HOST_LITTLE_ENDIAN
       char t;

       while (n--) {
               t = cp[1];
               cp[1] = cp[0];
               cp[0] = t;
               cp += 2;
       }
#endif
    return p;
}

char *swapl(cp, n)
char *cp;
long n;
{
    char *p = cp;
#ifdef HOST_LITTLE_ENDIAN
       char t;

       while (n--) {
               t = cp[3];
               cp[3] = cp[0];
               cp[0] = t;

               t = cp[2];
               cp[2] = cp[1];
               cp[1] = t;
               cp += 4;
       }
#endif
    return p;
}

struct fsym {
       char name[8];
       short flags;
       short value[2];
} tmpsym;

struct sym {
       char name[8];
       short flags;
       long value;
};

char * fixsym(void *xsp)
{
       struct sym *sp = (struct sym *) xsp;
       struct fsym *fp;
       int i;

       fp = &tmpsym;
       for (i=0; i<8; i++)
               fp->name[i] = sp->name[i];
       pcrossw(sp->flags, (char *) &fp->flags);
       pcrossl(sp->value, (char *) fp->value);

       return (char *)&tmpsym;
}
