
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

/* for megamax */
#define entry _entry

#if !defined (QDOS) && !defined(XTC68)
typedef struct {
	short magic;
	long tsize;
	long dsize;
	long bsize;
	long ssize;
	long stksize;
	long entry;
	short rlbflg;
} HEADER;

#define MAGIC	0x601a
#define SYMSIZE	14

void headers(void)
{
	HEADER header;
	extern long txtsize, datsize, bsssize, nsyms;

	header.magic = MAGIC;
	header.tsize = txtsize;
	header.dsize = datsize;
	header.bsize = bsssize;
	header.ssize = nsyms * SYMSIZE;
	header.stksize = 0L;
	header.entry = 0L;
	header.rlbflg = 0;

#ifndef UNIXHOST
	output( (char *) &header, sizeof header, 1 );
#else
	swapw(&header.magic, 1);
	output( (char *) &header.magic, sizeof(short), 1 );
	swapl(&header.tsize, 6);
	output( (char *) &header.tsize, sizeof(long), 6);
	swapw(&header.rlbflg, 1);
	output( (char *) &header.rlbflg, sizeof(short), 1 );
#endif
}

#else   /* QDOS */

static unsigned char txt_hdr[] = { 0xFB, 0x10, 0xFF, 0xFF, 4, 'T', 'E', 'X', 'T' };
static unsigned char data_hdr[] = { 0xFB, 0x10, 0xFF, 0xFE, 4, 'D', 'A', 'T', 'A' };
static unsigned char bss_hdr[] = { 0xFB, 0x10, 0xFF, 0xFD, 5, 'U', 'D', 'A', 'T','A' };
extern long txtsize, datsize, bsssize;

/* Output headers in GST object file format */
void headers(void)
{
  extern char *ofile;
  char c;
  long l;
  /* Ouput 0xFB 01 <len> <filename> */

#ifdef XTC68
  l = 0x01FB;
#else
  l = 0xFB010000;
#endif
  output((char *)&l, 2, 1);	/* uses long OK */
  c = strlen(ofile);
  output2fb((unsigned char *) &c,1,1);
  output2fb((unsigned char *)ofile, strlen(ofile), 1);

  /* Define the standard symbols for TEXT (-1), DATA (-2), and BSS (-3) */
  output( (char *)txt_hdr, 9, 1);
  output( (char *)data_hdr, 9, 1);
  output( (char *)bss_hdr, 10, 1);
}
#endif /* QDOS */
