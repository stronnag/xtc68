
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

/*
 * Version 1.1 contains fixes for referencing the usc = calld ccr registers
 * and not exiting if the -V switch is specified. format specifications for
 * move sr, ?? and usp moves were incorrect -- fixed. added aliasing for
 * immediate operations to sr and ccr.
 *
 * Version 1.2 contains changes for minix: input compatibility with ack,
 * including short absolute addressing mode and explicit pc-relative addressing
 * mode involving text labels, and smarter use of memory.
 */

#include "jas.h"

#include "proto.h"
#include <unistd.h>

#ifdef VMS
#define unlink delete
#endif

#ifdef DOS_LIKE
#include <io.h>
#endif

#define VERSION 2
#define RELEASE 0

jmp_buf err_buf;

extern int sawerror;

char *ofile = NULL;
char *ifile = NULL;

int Optimize = 1;
int Lflag = 0;
int flag8 = 0;
extern FILE *yyin;

#ifdef QDOS
#include <qdos.h>
long _stack = 20 * 1024L;
long _stackmargin = 1024L;
char _prog_name[] = "as68";
char _version[] = "v2.4";
void (*_consetup)() = consetup_title;
long (*_writetrans)() = NULL;
#endif /* QDOS */

void warn(int n, char *s)
{
#ifndef QDOS
  fprintf(stderr, "AS68: ");
#else
  fprintf(stderr, "jas: ");
#endif
  if (n)
    fprintf(stderr, "line %d: ", n);
  fprintf(stderr, "( %s )\n", s);
  sawerror = 1;
}

void error(int n, char *s)
{
  warn(n, s);  longjmp(err_buf, 1);
}

int main(int argc, char *argv[]) {
  if (setjmp(err_buf)) {
    unlink(ofile);
    exit(1);
  }

  setflags(argc, argv);

  if (freopen(ifile, "r", stdin) == (FILE *)NULL)
    error(0, "can't open source file for reading");

  if (freopen(ofile, "wb", stdout) == (FILE *)NULL)
    error(0, "can't open object file for writing");
#ifdef QDOS
  /*
   *  Set file type as relocatable type for QDOS
   */
  {
    struct qdirect header;
    if (fs_headr(fgetchid(stdout), -1, &header, 14) == 14) {
      header.d_type = QF_RELOC_TYPE;
      fs_heads(fgetchid(stdout), -1, &header, 14);
    }
  }
#endif /* QDOS */

  yyin = stdin;

  aspass1();

  if (sawerror)
    unlink(ofile);

  return (sawerror);
}

char *allocate(unsigned long size) {
  char *loc;

  loc = calloc(size, 1);
#ifdef MEM_DEBUG
  fprintf(stderr, "alloc(%u bytes) => %lx\n", size, loc);
#endif
  if (loc) {
    return loc;
  }
  error(0, "out of memory");
  return (char *)NULL;
}

char *myreallocate(char *ptr, unsigned long size)
{
  register char *loc;

  loc = realloc(ptr, size);
  if (loc) {
    return loc;
  }
  error(0, "out of memory");
  return (char *)NULL;
}

void setflags(int ac, char **av)
{
  int errflag = 0, i;
  int Vflag = 0;
  for (i = 1; i < ac; i++) {
    if (*av[i] == '-') {
      switch (av[i][1]) {
      case 'o':
        ofile = av[++i];
        break;
      case 'N':
      case 'n':
        Optimize = 0;
        break;
      case 'V':
      case 'v':
        Vflag = 1;
        break;
      case 'L':
        Lflag = 1;
        if (av[i][2])
          Lflag = av[i][2] - '0';
        break;
      case 's':
        i++;
      case 'l':
      case 'u':
        break;
      case '8':
        flag8 = 1;
        break;
      default:
        errflag = 1;
        break;
      }
    } else if (!ifile) {
      ifile = av[i];
    } else if (!ofile) {
      ofile = av[i];
    } else {
      errflag = 1;
    }
  }

  if (Vflag) {
    fprintf(stderr, "Sozobon Assembler, Version %d.%d\n", VERSION, RELEASE);
    fprintf(stderr, "Copyright (c) 1988,1991 by Sozobon, Limited\n");
  }

  if (!ifile)
    errflag = 1;

  if (errflag) {
    fprintf(stderr, "usage: as68 [-N] source [-o object]\n");
    exit(1);
  }

  if (!ofile) {
    char buf[32];
    char *ip, *op;

    for (op = (char *)buf, ip = ifile; (*op++ = *ip) != 0; ip++) {
      if (*ip == '/'
#ifdef DOS_LIKE
          || *ip == '\\' || *ip == ':'
#endif
#ifdef VMS
          || *ip == ']' || *ip == ':'
#endif
      )
        op = buf;
    }

    if (op[-2] == 's' && op[-3] == '.') {
      op[-2] = 'o';
    } else {
      fprintf(stderr, "usage: as68 [-N] source [-o object]\n");
      exit(1);
    }
    ofile = strdup(buf);
  }
}

void output(char *buffer, size_t size, size_t nitems) {
  if (fwrite(buffer, size, nitems, stdout) != nitems)
    error(0, "trouble writing object file");
}

#if defined(QDOS) || defined(XTC68)
/*
 * This is the bit that looks after the fact that 0xFB is used as
 *  a directive in SROFF, so in many cases it needs to be output as
 *  0xFBFB instead.
 */
void output2fb(unsigned char *buffer, int size, int nitems) {

  register unsigned s;

  while (nitems--) {
    for (s = size; s--;) {
      if (*buffer == 0xfb) {
        fputc(*buffer, stdout);
      }
      /** 6/1/90 jcg */
      fputc(*buffer++, stdout);
      if (ferror(stdout)) {
#ifdef QDOS
        extern int errno, _oserr;
        fprintf(stderr, "errno=%d, _oserr=%d\n", errno, _oserr);
#endif
        error(0, "trouble writing object file for 2fb");
      }
    }
  }
  return;
}
#endif

#ifdef MEM_DEBUG

#ifdef free
#undef free
#endif

my_free(x) char *x;
{
  fprintf(stderr, "free( %lx )\n", x);
  free(x);
}
#endif /* MEM_DEBUG */
