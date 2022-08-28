/*      s l b d i s _ c
 *
 * This module handles the dis-assembly of an SROFF module.  It is
 * based on the MINIX mdb disassembler (see notice below), although
 * significant changes have been made from the original.
 *
 * The system has been modified to be a 2 pass system:
 *     Pass 1 is used to build up symbol tables.
 *     Pass 2 actually prints the disassembly.
 *
 * (c) Copyright 1991 David J. Walker
 *     Email:  d.j.walker@oasis.icl.co.uk
 *
 *     Permission to copy and/or distribute granted under the
 *     following conditions:
 *
 *     1). This notice must remain intact.
 *     2). The author is not responsible for the consequences of use
 *         this software, no matter how awful, even if they
 *         arise from defects in it.
 *     3). Altered version must not be represented as being the
 *         original software.
 *
 *  The original header from the MINIX source file now follows:
 *--------------------------------------------------------------------------
 * mdbdis.c - MINIX program disassembler                                    |
 *                                                                          |
 * Written by Bruce D. Szablak                                              |
 *                                                                          |
 * This free software is provided for non-commerical use. No warrantee      |
 * of fitness for any use is implied. You get what you pay for. Anyone      |
 * may make modifications and distribute them, but please keep this header  |
 * in the distribution.                                                     |
 *--------------------------------------------------------------------------
 */

#include "slb.h"
#include <stdarg.h>
#include <stdint.h>

#if defined(WIN32) || defined(__clang__)
static uint16_t bswap_16(uint16_t val) {
  return (uint16_t)(val << 8) + (val >> 8);
}
static uint32_t bswap_32(uint32_t val) {
  return (uint32_t)(((uint32_t)bswap_16(val & 0xffff) << 16) |
                    (uint32_t)bswap_16(val >> 16));
}
#else
#include <byteswap.h>
#endif

PRIVATE short gword         _PROTOTYPE((void));
PRIVATE long  glong         _PROTOTYPE((void));
PRIVATE void  reladdr       _PROTOTYPE((char));
PRIVATE void  movem         _PROTOTYPE((int, int, int));
PRIVATE void  opmode        _PROTOTYPE((char *, int, int, int, int));
PRIVATE int   op1           _PROTOTYPE((int, int));
PRIVATE void  op2           _PROTOTYPE((int, int, int, int, int));
PRIVATE void  symbolic      _PROTOTYPE((long, char));
PRIVATE void  Long_Label    _PROTOTYPE((char));

#define BYTE 0
#define WORD 1
#define LONG 2

struct label {
    struct label * next;       /* Linkage fields */
    short    IdSection;        /* Section Id to which this belongs */
    long     Offset;           /* Offset within section */
    char    *label;            /* Text name for label */
    short    type;             /* Set to indicate our guess at area type */
    };                         /*    0=unknown, 1=data, 2=code  */
typedef struct label LABEL;
typedef LABEL * LABELPTR;
#define TYPE_UNKNOWN 0
#define TYPE_DATA    1
#define TYPE_CODE    2

#define MAX_SECTIONS 10
LABELPTR labellist[MAX_SECTIONS] = {NULL};

#define BFIELD(w,b,l) (((w) >> ((b)-(l)+1)) & (int)((1L<<(l))-1))
#define BTST(w,b) ((w) & (1 << (b)))
PRIVATE char opwfmt[] = "%s.%c\t";
#define OPI(s,w) lprintf(opwfmt, s, w)

PRIVATE char size[] = "bwl";
PRIVATE char *cc[] = {"ra", "f", "hi", "ls", "cc", "cs", "ne", "eq", "vc", "vs",
              "pl", "mi", "ge", "lt", "gt", "le" };

PRIVATE char *bitop[] = { "btst", "bchg", "bclr", "bset" };
PRIVATE char *imedop[] = { "ori", "andi", "subi", "addi", "?", "eori", "cmpi" };
PRIVATE char *misc1[] = { "negx", "clr", "neg", "not" };
PRIVATE char *misc2[] = { "reset", "nop", "stop", "rte", "??", "rts", "trapv", "rtr" };
PRIVATE char *shf[] = { "as", "ls", "rox", "ro" };

PRIVATE char *fmts[] = { "d%d", "a%d", "(a%d)", "(a%d)+", "-(a%d)" };

PRIVATE char databuf[20];
PRIVATE long gaddr;
PRIVATE int gisize;
PRIVATE long modfstart;	        /* File position of start of module */
PRIVATE int  endflag;


/*============================================================ CHECK_LABEL */
PRIVATE
void    Check_Label()
/*      ~~~~~~~~~~~
 *  This routine is only active for the second pass.
 *  It sees if it is necessary to print a label at the current
 *  address.   It then prints the indent necessary for the instruction.
 *--------------------------------------------------------------------------*/
{
    lprintf ("\t");
    return;
}


/*============================================================= GET_DATA */
PRIVATE
long    get_data (size)
/*      ~~~~~~~~
 *  This routine is used to get the next bit of data from the file
 *  Either 2 or 4 bytes is always requested.
 *
 *  SROFF directives that are not directly relevant to the process
 *  of producing the dis-assembly will be either acted on directly
 *  or ignored as appropriate.
 *-----------------------------------------------------------------------*/
int     size;
{
    char    buffer[6];
    long    reply = 0;

    DBG (("GET_DATA",0x81,"Enter: size=%d",size));
    while (size && (inchar(libfp) != EOF)) {
        switch (ch) {
        case SROFF_FLAG:
                switch (inchar(libfp)) {
                case SROFF_FLAG:
                        DBG (("GET_DATA",0x88,"$FB character"));
                        reply = (reply << 8) + 0xFB;
                        strcat (databuf,"FB");
                        size--;
                        break;
                case SROFF_SOURCE:  /* SOURCE */
                        DBG (("GET_DATA",0x88,"SOURCE Directive"));
                        Get_String();
                        lprintf ("!   Module %s\n", String);
                        DBG (("GET_DATA",0x88,"... name = %s", String));
                        break;
                case SROFF_COMMENT:  /* COMMENT */
                        DBG (("GET_DATA",0x88,"COMMENT Directive"));
                        Get_String();
                        lprintf ("!   COMMENT: %s\n", String);
                        DBG (("GET_DATA",0x88,"... text = %s", String));
                        break;
                case SROFF_ORG:  /* ORG */
                        DBG (("GET_DATA",0x88,"ORG Directive"));
                        Get_LongWord();
                        lprintf (".org %d\n",LongWord);
                        gaddr = LongWord;
                        DBG (("GET_DATA",0x88,"... New value=%ld",LongWord));
                        break;
                case SROFF_SECTION:  /* SECTION */
                        DBG (("GET_DATA",0x88,"SECTION Directive"));
                        Get_Id();
			if (Id != IdSection) {
                            Section = Make_String (IdText);
			    IdSection = Id;
                        }
                        if (! stricmp(IdText,"DATA")) {
                            lprintf (".data\n");
                        } else if (! stricmp(IdText,"TEXT")) {
                            lprintf (".text\n");
                        } else if ((! stricmp(IdText,"UDATA"))
                               || (! stricmp(IdText,"BSS"))) {
                            lprintf (".bss\n");
                        } else if (! stricmp(IdText,"NLSI")) {
                            lprintf (".nlsi\n");
                        } else {
                            lprintf ("!  *** unrecognized section %s\n",IdText);
                        }
                        DBG (("GET_DATA",0x88,"... Id=%04,4x, name=%s",Id,IdText));
                        break;
                case SROFF_OFFSET:  /* OFFSET */
                        DBG (("GET_DATA",0x88,"OFFSET Directive"));
                        Get_LongWord();
                        lprintf ("! *** unexpected OFFSET %d\n",LongText);
                        DBG (("GET_DATA",0x88,"... Offset=%ld",LongWord));
                        break;
                case SROFF_XDEF:  /* XDEF */
                        DBG (("GET_DATA",0x88,"XDEF Directive"));
                        Get_String();
                        Get_LongWord();
                        Get_Id();
                        lprintf (".globl %s", String);
                        Print_Spaces (20-strlen(String));
                        lprintf ("! at %s+%s\n", IdText, LongText);
                        DBG (("GET_DATA",0x88,"... Name=%s, Offset=%ld, Id=%04.4x",String,LongWord,Id));
                        break;
                case SROFF_XREF:  /* XREF */
                        DBG (("GET_DATA",0x88,"XREF Directive"));
                        Get_LongWord();
                        Get_TruncRule();
                        DBG (("GET_DATA",0x88,"... Offset=%ld",LongWord));
                        break;
                case SROFF_DEFINE:  /* DEFINE */
                        DBG (("GET_DATA",0x88,"DEFINE Directive"));
                        Get_Id();
                        Get_String();
                        Add_Id (Id, String);
                        DBG (("GET_DATA",0x88,"... ID=$%04.4x, Name=%s",Id, String));
                        break;
                case SROFF_COMMON:  /* COMMON */
                        DBG (("GET_DATA",0x88,"COMMON Directive"));
                        Get_Id();
                        lprintf (".comm %s\n", IdText);
                        DBG (("GET_DATA",0x88,"... Id=%04.4x",Id));
                        break;
                case SROFF_END:  /* END */
                        DBG (("GET_DATA",0x88,"END Directive"));
                        if (printpass == 0) {
                            /* Reset to module start for second pass */
                            fseek (libfp, modfstart, 0);
                            gaddr = 0;
                        }
                        printpass++;
                        return (0);
                default:
                        eprintf ("ILLEGAL DIRECTIVE: $%02.2x at %ld\n", ch, ftell(libfp));
                        error (20);
                } /* end of FB switch */
                break;
        default:
            DBG (("GET_DATA",0x88,"$%02.2x character",ch));
            reply = (reply << 8) + (ch & 0xFF);
            sprintf (buffer,"%02x",ch & 0xFF);
            strcat (databuf,buffer);
            size--;
            break;
        }
    }   /* while 'size' */
    DBG (("GET_DATA",0x81,"Exit: reply=$%lx",reply));
    return reply;
}


/*================================================================ GWORD */
PRIVATE
short   gword()
/*      ~~~~~
 *
 *  Obtains the next word of the program code
 *-----------------------------------------------------------------------*/
{
    short   reply;

    DBG(("GWORD",0x1001,"enter:"));
    reply = (short)get_data(2);
    reply = bswap_16(reply);
    gaddr += 2;
    DBG(("GWORD",0x1001,"Exit: reply=%04.4x",reply));
    return (reply);
}

/*================================================================== GLONG */
PRIVATE
long    glong()
/*      ~~~~~
 *
 *  Obtains the next long word of the program code
 *-------------------------------------------------------------------------*/
{
    long    reply;

    DBG(("GLONG",0x1001,"Enter:"));
    reply = get_data(4);
    reply = bswap_32(reply);
    gaddr += 4;
    DBG(("GLONG",0x1001,"Exit: reply=%08.8c",reply));
    return (reply);
}

/*================================================================ RELADDR */
PRIVATE
void    reladdr (__attribute__((unused))char sep)
/*      ~~~~~~~
 *  Called when a relative address is expected.
 *  Converts a relative address into a name.
 *-------------------------------------------------------------------------*/
{
    int d = gword();
    lprintf ("RELADDR=%d",d);
    return;
}

/*=================================================================== MOVEM */
PRIVATE
void    movem (from, predec, rmsk)
/*      ~~~~~
 *  Analyses the operands of a 'movem' instruction.
 *-------------------------------------------------------------------------*/
int from, predec, rmsk;
{
    int b, f = 0;

    DBG(("MOVEM",0x801,"Enter"));
    if (!from)  lprintf(",");
    for (b = 16; b--; rmsk >>= 1)   {
        if (rmsk & 1)   {
            if (f)  {
                lprintf("/");
            } else  {
                f = 1;
            }
            if (predec)  {
                lprintf("%c%d", b>7 ? 'a' : 'd', b % 8);
            } else {
                lprintf("%c%d", b>7 ? 'd' : 'a', 7 - b % 8);
            }
        }
    }
    if (from)  lprintf(",");
    DBG(("MOVEM",0x801,"Exit"));
    return;
}

/*=================================================================== OP1 */
PRIVATE
int     op1 (mode,reg)
/*      ~~~
 *
 *------------------------------------------------------------------------*/
int mode, reg;
{
    int   d;
    long  l;

    DBG (("OP1",1001,"Enter"));
    /* if (mode < 5)   { */
    switch (mode) {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
        lprintf(fmts[mode], reg);
        break;
    case 5:
        lprintf("%d(a%d)", gword(), reg);
        break;
    case 6:
        d = gword();
        lprintf("%d(a%d,%c%d.%c)",
        BFIELD(d,7,8) | (BTST(d,7) ? 0xFF00 : 0), reg,
        BTST(d,15) ? 'a' : 'd', BFIELD(d,14,3),
        BTST(d,11) ? 'l' : 'w');
        break;
    default:
	switch (reg)    {
	case 0:
		lprintf("%d.w", gword());
		break;
	case 1:
		Long_Label((char)'\0');
		break;
	case 2:
		l = gaddr;
		lprintf("%d(pc)", d = gword());
		symbolic((long)(l+d), (char)'}');
		break;
	case 3:
		d = gword();
		lprintf("%d(pc,%c%d.%c)",
			BFIELD(d,7,8) | (BTST(d,7) ? 0xFF00 : 0),
			BTST(d,15) ? 'a' : 'd', BFIELD(d,14,3),
			BTST(d,11) ? 'l' : 'w');
		break;
	case 4:
		lprintf("#");
                switch (gisize) {
		case LONG:
			lprintf("%ld",(long)glong());
                        break;
		case BYTE:
			lprintf("%d", gword() & 0xFF);
                        break;
		default:
			lprintf("%d", gword());
                        break;
                }
		break;
	case 5:
		lprintf("sr"); break;
                break;
	} /* end of 'reg' switch */
        break;
    }
    DBG (("OP1",1001,"Exit: 0"));
    return 0;
}

/*================================================================== OP2 */
PRIVATE
void    op2 (f,m1,r1,m2,r2)
/*      ~~~
 * Print two operands
 * f set means order passed, clear reverses order
 *----------------------------------------------------------------------*/
int f, m1, r1, m2, r2;
{
    DBG (("OP2",1001,"Enter"));
    f ? op1(m1,r1) : op1(m2,r2);
    lprintf(",");
    f ? op1(m2,r2) : op1(m1,r1);
    DBG (("OP2",1001,"Exit"));
}

/*================================================================ OPMODE */
PRIVATE
void     opmode (op, opm, reg, m, r)
/*      ~~~~~~~
 *
 *------------------------------------------------------------------------*/
char *op;
int opm, reg, m, r;
{
    DBG (("OPMODE",1001,"Enter: opm=%d, reg=%d, m=%d, r=%d",opm, reg, m, r));
    OPI(op, size[gisize=BFIELD(opm,1,2)]);
    op2(BTST(opm,2),0,reg,m,r);
    DBG (("OPMODE",1001,"Exit"));
}


/*============================================================ SYMBOLIC */
PRIVATE
void    symbolic (long addr, char sep)
/*      ~~~~~~~~
 *  This routine takes an address and outputs the symbolic name,
 *  followed by the seperator (if any)
 *-----------------------------------------------------------------------*/
{
    if (printpass) {
        DBG (("SYMBOLIC", 0x101, "Enter: addr=%ld, sep=%c",addr,sep));
#if 0
        extern long saddr, eaddr;
        if (addr < saddr || addr > eaddr) {
            lprintf("0x%lx%c", addr, sep);
            return;
        }
        fputs(addr_to_name(addr - saddr, &off), stdout);
        if (off)
            lprintf("+0x%lx", off);
#endif
        lprintf ("SYMBOLIC%ld",addr);
        lprintf("%c", sep);
        DBG (("SYMBOLIC", 0x101, "Exit"));
    }
    return;
}

/*========================================================== LONG_LABEL */
PRIVATE
void    Long_Label(char sep)
/*      ~~~~~~~~~~
 *  Called when a long address expected.  This could be an
 *  external reference.
 *----------------------------------------------------------------------*/
{
     long    value;
     char    buffer[10];

     DBG(("LONG_LABEL",0x401,"Enter:"));
     /*
      * Start by reading the next two characters from the file:
      */
     value = ((inchar(libfp) << 8) + inchar(libfp)) & 0xFFFF;
     DBG(("LONG_LABEL",0x408,"... value of 2 bytes = %04.4x",value));
     /*
      * The possible characters at this stage are:
      *    0xFBFB      First character of value when it is FB
      *    0xFB??      Start of an XREF sequence
      *    other       absolute value
      */
      if ((value == 0xFBFB) || (value != 0xFB07)) {
          if (value == 0xFBF) {
              value = 0xFB00 + inchar(libfp);
              DBG(("LONG_LABEL",0x408,"... FB, so read byte"));
          }
          value = (value << 16)
                   + (inchar(libfp) << 8)
                   + (inchar(libfp));
          sprintf (buffer,"%08lx",value);
          DBG(("LONG_LABEL",0x408,"... data=%s",buffer));
          strcat(databuf,buffer);
          symbolic (value, sep);
      } else {
             DBG(("LONG_LABEL",0x408,"... XREF"));
             strcat(databuf,"<-XREF->");
             Get_LongWord();
             Get_TruncRule();
             lprintf ("LONG_LABEL%c",sep);
      }
     DBG(("LONG_LABEL",0x401,"Exit"));
}


/*====================================================== DISASM_MODULE */
PUBLIC
long    disasm_module (FILE *fp, __attribute__((unused))char *name)
/*      ~~~~~~~~~~~~~
 *  This is the main control for the disassembly process
 *----------------------------------------------------------------------*/
{
    unsigned int w, m1, m2, r1, r2, op;
    char ds;

    DBG (("DISASM_MODULE",0x21,"Enter: name=%s",name));
    gaddr = printpass = endflag = 0;
    modfstart = ftell (fp);
    strcpy (databuf,"");

    while (printpass < 2)   {
        Check_Label();
        w = gword();
        if (printpass > endflag) {
            endflag = printpass;
            continue;
        }
        m1 = BFIELD(w,8,3); m2 = BFIELD(w,5,3);
        r1 = BFIELD(w,11,3); r2 = BFIELD(w,2,3);
        op = BFIELD(w,15,4);
        DBG (("DISASM_MODULE",0x28,"op=%d",op));
        switch (op) {
        case 0x0:
                if (m2 == 1)    {
                    OPI("movep", BTST(w,6) ? 'l' : 'w');
                    op2((int)BTST(w,7),0,(int)r1,5,(int)r2);
                    break;
                }
                if (BTST(w,8))  {
                    OPI(bitop[BFIELD(w,7,2)], m2 ? 'b' : 'l');
                    op2(1,0,(int)r1,(int)m2,(int)r2);
                    break;
                }
                if (r1 == 4)    {
                    OPI(bitop[BFIELD(w,7,2)], m2 ? 'b' : 'l');
                    gisize = WORD;
                    op2(1,7,4,(int)m2,(int)r2);
                    break;
                }
                OPI(imedop[r1],size[gisize = m1]);
                op2(1,7,4,(int)m2,(int)r2);
                break;
        case 0x1:
                gisize = BYTE;
                goto domove;
        case 0x2:
                gisize = LONG;
                goto domove;
        case 0x3:
                gisize = WORD;

                domove:
                    OPI("move", size[gisize]);
                    op2(1,(int)m2,(int)r2,(int)m1,(int)r1);
                    break;
        case 0x4:
                if (BTST(w,8))  {
                    if (BTST(w,6))  {
                        lprintf("lea\t");
                        op1((int)m2,(int)r2);
                        lprintf(",a%d", r1);
                        break;
                    }
                    lprintf("chk\t");
                    op1((int)m2,(int)r2);
                    lprintf(",d%d", r1);
                    break;
                }
                if (r1 < 4) {
                    if (m1 == 3)    {
                        lprintf("move\t");
                        gisize = WORD;
                        if (r1 == 0)
                            lprintf("sr,");
                        op1((int)m2,(int)r2);
                        if (r1 == 2)
                            lprintf(",ccr");
                        if (r1 == 3)
                            lprintf(",sr");
                        break;
                    }
                    OPI(misc1[r1], size[m1]);
                    op1((int)m2,(int)r2);
                    break;
                } else if (r1 == 4) {
                    switch(m1)  {
                    case 0:
                            lprintf("nbcd\t");
                            break;
                    case 1:
                            lprintf(m2 ? "pea\t" : "swap\t");
                            break;
                    case 2:
                    case 3:
                            OPI(m2 ? "movem" : "ext", BTST(w,6) ? 'l' : 'w');
                            if (m2)
                                movem(1,m2==4, gword());
                            break;
                    }
                    op1((int)m2,(int)r2);
                    break;
                }
                if (r1 == 5)    {
                    if (m1 == 3)
                        lprintf("tas\t");
                    else
                        OPI("tst", size[m1]);
                    op1((int)m2,(int)r2);
                    break;
                }
                if (r1 == 6)    {
                    OPI("movem", BTST(w,6) ? 'l' : 'w');
                    op = gword();
                    op1((int)m2,(int)r2);
                    movem(0,m2==4,(int)op);
                    break;
                }
                if (BTST(w,7))  {
                    lprintf(BTST(w,6) ? "jmp\t" : "jsr\t");
                    op1((int)m2,(int)r2);
                    break;
                }
                switch (m2) {
                case 0:
                case 1:
                        lprintf("trap\t#%d", BFIELD(w,3,4));
                        break;
                case 2:
                        lprintf("link\ta%d,#%d", r2, gword());
                        break;
                case 3:
                        lprintf("unlk\ta%d", r2);
                        break;
                case 4:
                        lprintf("move.l a%d,usp", r2);
                        break;
                case 5:
                        lprintf("move.l usp,a%d", r2);
                        break;
                case 6:
                        lprintf(misc2[r2]);
                        break;
                }
                break;
        case 0x5:
                if (BFIELD(w,7,2) == 3) {
                    op = BFIELD(w,11,4);
                    if (m2 == 1)  {
                        lprintf("db%s\td%d,",cc[op], r2);
                        reladdr('\0');
                    } else  {
                        lprintf("s%s\t",cc[op]);
                        op1((int)m2,(int)r2);
                    }
                } else {
                    lprintf("%sq.%c\t#%d,",BTST(w,8)?"sub":"add",
                    size[BFIELD(w,7,2)],
                    ((r1 - 1) & 7) + 1);
                    op1((int)m2,(int)r2);
                }
                break;
        case 0x6:
                ds = BFIELD(w,7,8);
#ifdef GENERIC
                lprintf("b%s.%c\t", cc[BFIELD(w,11,4)], ds ? 's' : 'w');
#else
                lprintf("b%s\t", cc[BFIELD(w,11,4)]);
#endif
                if (ds)
                    symbolic(gaddr+ds,'\0');
                else
                    reladdr('\0');
                break;
        case 0x7:
                lprintf("moveq\t#%d,d%d",BFIELD(w,7,8),r1);
                break;
        case 0x8:
                if (m1 == 3 || m1 == 7) {
                    lprintf("div%c\t", BTST(w,8) ? 's' : 'u');
                    op2(0,0,(int)r1,(int)m2,(int)r2);
                } else if (m1 == 4 && (m2 == 1 || m2 == 0)) {
                    lprintf(m2 ? "sbcd\t-(a%d),-(a%d)" : "sbcd\td%d,d%d", r2, r1);
                } else {
                    opmode("or",(int)m1,(int)r1,(int)m2,(int)r2);
                }
                break;
        case 0x9:
        case 0xD:
                if ((m2 == 0 || m2 == 1) && m1 > 3 && m1 < 7)  {
                    OPI(op == 9 ? "subx" : "addx",
                    size[BFIELD(w,7,2)]);
                    m2 <<= 2;
                    op2(1,(int)m2,(int)r2,(int)m2,(int)r1);
                } else if (m1 == 3 || m1 == 7) {
                    gisize = m1 == 3 ? WORD : LONG;
                    OPI(op==9 ? "sub" : "add", size[gisize]);
                    op2(1,(int)m2,(int)r2,1,(int)r1);
                } else {
                    opmode(op==9 ? "sub" : "add",(int)m1,(int)r1,(int)m2,(int)r2);
                }
                break;
        case 0xB:
                if (BTST(w,8)) {
                    if (m2 == 1) {
                        OPI("cmpm", size[BFIELD(w,7,2)]);
                        lprintf("(a%d)+,(a%d)+",r2,r1);
                    } else {
                        opmode("eor",(int)m1,(int)r1,(int)m2,(int)r2);
                    }
                } else {
                    opmode("cmp",(int)m1,(int)r1,(int)m2,(int)r2);
                }
                break;
        case 0xC:
                if (m1 == 3 || m1 == 7) {
                    lprintf("mul%c\t", m1==7 ? 's' : 'u');
                    op2(0,0,(int)r1,(int)m2,(int)r2);
                } else if (m1 == 4 && (m2 == 1 || m2 == 0)) {
                    lprintf(m2 ? "abcd\t-(a%d),-(a%d)" : "abcd\td%d,d%d", r2, r1);
                } else if (m1 == 5) {
                    op = BTST(w,3) ? 'a' : 'd';
                    lprintf("exg\t%c%d,%c%d",op,r1,op,r2);
                } else if (m1 == 6) {
                    lprintf("exg\td%d,a%d",r1,r2);
                } else {
                    opmode("and",(int)m1,(int)r1,(int)m2,(int)r2);
                }
                break;
        case 0xE:
                if (BFIELD(w,7,2) == 3) {
                    lprintf("%s%c.w\t",shf[BFIELD(w,10,2)],
                                        BTST(w,8) ? 'l' : 'r');
                    op1((int)m2,(int)r2);
                } else {
                    lprintf("%s%c.%c\t",shf[BFIELD(w,4,2)],
                                        BTST(w,8) ? 'l' : 'r',
                    size[BFIELD(w,7,2)]);
                    if (BTST(w,5)) {
                        op2(1,0,(int)r1,0,(int)r2);
                    } else {
                        lprintf("#%d,",r1);
                        op1(0,(int)r2);
                    }
                }
                break;
        case 0xA:
        case 0xF:
                lprintf("%x", w);
                break;
        }
        if (strlen(databuf)) {
            lprintf("\t\t! %s\n",databuf);
            strcpy (databuf,"");
        }
    }
    DBG (("DISASM_MODULE",0x21,"Exit"));
    return gaddr;
}
