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

#include "config.h"

#ifdef INTEL

#ifdef TARGET_SYSV

#define OUT_MODULE
#include "chdr.h"
#include "expr.h"
#include "cglbdec.h"
#include "proto.h"
#include "genx86.h"
#include "outproto.h"
#include "version.h"

/********************************************************** Type Definitions */

enum e_gt { bytegen, wordgen, longgen, floatgen, nogen };
enum e_sg { noseg, codeseg, dataseg };

/*********************************************** Static Function Definitions */

static void putop P_((OPCODE));
static void putconst P_((const EXPR *));
static void putlen P_((ILEN));
static void putamode P_((const ADDRESS *, ILEN));
static void put_header P_((enum e_gt, SIZE));
static void seg P_((enum e_sg, const char *, SIZE));
static void put_bseg P_((SIZE));
static void nl P_((void));
static void put_align P_((SIZE));

/*********************************************** Global Function Definitions */

PRIVATE void put_dword P_((UVAL));
PRIVATE void put_cseg P_((SIZE));
PRIVATE void put_dseg P_((SIZE));
PRIVATE void put_kseg P_((SIZE));
PRIVATE void put_rseg P_((SIZE));
PRIVATE void put_reference P_((SYM *));

/********************************************************** Static Variables */

static enum e_gt gentype = nogen;
static enum e_sg curseg = noseg;
static int outcol = 0;
static SIZE align_type = 0L;
static const char *prefix = ".L";
static const char *comment = "|";

static const char *opl[] = {
    "movsbl", /* op_movsbl */
    "movzbl", /* op_movzbl */
    "movswl", /* op_movswl */
    "movzwl", /* op_movzwl */
    "movsbw", /* op_movsbw */
    "movzbw", /* op_movzbw */
    "cltd",   /* op_cdq */
    "cwtl",   /* op_cwd */
    "cbtw",   /* op_cbw */
    "mov",    /* op_mov */
    "xchg",   /* op_xchg */
    "lea",    /* op_lea */
    "not",    /* op_not */
    "neg",    /* op_neg */
    "add",    /* op_add */
    "sub",    /* op_sub */
    "adc",    /* op_adc */
    "sbb",    /* op_sbb */
    "imul",   /* op_imul */
    "idiv",   /* op_idiv */
    "div",    /* op_div */
    "and",    /* op_and */
    "or",     /* op_or */
    "xor",    /* op_xor */
    "inc",    /* op_inc */
    "dec",    /* op_dec */
    "cmp",    /* op_cmp */
    "push",   /* op_push */
    "pop",    /* op_pop */
    "jmp",    /* op_jmp */
    "loop",   /* op_loop */
    "call",   /* op_call */
    "leave",  /* op_leave */
    "enter",  /* op_enter */
    "ret",    /* op_ret */
    "test",   /* op_test */
    "jmp",    /* op_bra */
    "je",     /* op_je */
    "jne",    /* op_jne */
    "jl",     /* op_jl */
    "jle",    /* op_jle */
    "jg",     /* op_jg */
    "jge",    /* op_jge */
    "ja",     /* op_ja */
    "jae",    /* op_jae */
    "jb",     /* op_jb */
    "jbe",    /* op_jbe */
    "rep",    /* op_rep */
    "smov",   /* op_smov */
    "shl",    /* op_shl */
    "shr",    /* op_shr */
    "sal",    /* op_asl */
    "sar",    /* op_asr */
    "rol",    /* op_rol */
    "ror",    /* op_ror */
    "sahf",   /* op_sahf */
    "sete",   /* op_sete */
    "setne",  /* op_setne */
    "setb",   /* op_setb */
    "setbe",  /* op_setbe */
    "seta",   /* op_seta */
    "setae",  /* op_setae */
    "setl",   /* op_setl */
    "setle",  /* op_setle */
    "setg",   /* op_setg */
    "setge",  /* op_setge */
    "nop",    /* op_nop */
#ifdef FLOAT_IEEE
    "fadd",        /* op_fadd */
    "faddp",       /* op_faddp */
    "fsub",        /* op_fsub */
    "fsubp",       /* op_fsubp */
    "fdiv",        /* op_fdiv */
    "fdivp",       /* op_fdivp */
    "fmul",        /* op_fmul */
    "fmulp",       /* op_fmulp */
    "fsubr",       /* op_fsubr */
    "fsubrp",      /* op_fsubrp */
    "fdivr",       /* op_fdivr */
    "fdivrp",      /* op_fdivrp */
    "fld",         /* op_fld */
    "fldz",        /* op_fldz */
    "fst",         /* op_fst */
    "fstp",        /* op_fstp */
    "fstp %st(0)", /* op_fpop */
    "fild",        /* op_fild */
    "fildl",       /* op_fildl */
    "fistpl",      /* op_fistpl */
    "ftst",        /* op_ftst */
    "fchs",        /* op_fchs */
    "fcomp",       /* op_fcomp */
    "fcompp",      /* op_fcompp */
    "fnstsw",      /* op_fnstsw */
    "fwait",       /* op_fwait */
#endif             /* FLOAT_IEEE */
#ifdef ASM
    "",           /* op_asm */
#endif            /* ASM */
    ".line",      /* op_line */
    (char *)NULL, /* op_label */
};

/*****************************************************************************/

static void putop P1(OPCODE, op) {
  if (op >= OP_MIN && op <= OP_MAX && opl[op] != (char *)0) {
    oprintf("\t%s", opl[op]);
    return;
  }
  FATAL((__FILE__, "putop", "Illegal opcode %d", op));
}

/*
 * put a constant to the output file.
 */
static void putconst P1(const EXPR *, ep) {

  if (ep == NIL_EXPR) {
    FATAL((__FILE__, "putconst", "ep == 0"));
  }
  switch (ep->nodetype) {
  case en_autocon:
  case en_icon:
    oprintf("%ld", ep->v.i);
    break;
  case en_labcon:
    oprintf("%s%u", prefix, (unsigned)ep->v.l);
    break;
  case en_nacon:
    oprintf("%s", outlate(ep->v.str));
    break;
  case en_sym:
    oprintf("%s", outlate(nameof(ep->v.sp)));
    break;
  case en_add:
    putconst(ep->v.p[0]);
    oprintf("+");
    putconst(ep->v.p[1]);
    break;
  case en_sub:
    putconst(ep->v.p[0]);
    oprintf("-");
    putconst(ep->v.p[1]);
    break;
  case en_uminus:
    oprintf("-");
    putconst(ep->v.p[0]);
    break;
  case en_cast:
    putconst(ep->v.p[0]);
    break;
  case en_str:
    oprintf("%s", ep->v.str);
    break;
  default:
    FATAL((__FILE__, "putconst", "illegal constant node %d", ep->nodetype));
    break;
  }
}

/*
 * append the length field to an instruction.
 */
static void putlen P1(ILEN, l) {
  switch (l) {
  case IL0:
    break;
  case IL1:
    oprintf("b");
    break;
  case IL2:
    oprintf("w");
    break;
  case IL4:
    oprintf("l");
    break;
  case IL4 + 1:
    /* special value for single precision float */
    oprintf("s");
    break;
  case IL8 + 1:
    /* special value for double precision float */
    oprintf("l");
    break;
  case IL10 + 1:
    break;
  default:
    FATAL((__FILE__, "putlen", "illegal length field %d", (int)l));
    break;
  }
}

/*
 * output a general addressing mode.
 */
static void putamode P2(const ADDRESS *, ap, ILEN, len) {
  static const char *regname[NUMREG + 1] = {"eax",   /* EAX */
                                            "edx",   /* EDX */
                                            "ecx",   /* ECX */
                                            "ebx",   /* EBX */
                                            "esi",   /* ESI */
                                            "edi",   /* EDI */
                                            "esp",   /* ESP */
                                            "ebp",   /* EBP */
                                            "ax",    /* AX */
                                            "dx",    /* DX */
                                            "cx",    /* CX */
                                            "bx",    /* BX */
                                            "si",    /* SI */
                                            "di",    /* DI */
                                            "sp",    /* SP */
                                            "bp",    /* BP */
                                            "al",    /* AL */
                                            "dl",    /* DL */
                                            "cl",    /* CL */
                                            "bl",    /* BL */
                                            "st",    /* ST(0) */
                                            "st(1)", /* ST(1) */
                                            "%INVALID_REGISTER",
                                            "%INVALID_REGISTER"};

  REG reg;

  switch (ap->mode) {
  case am_immed:
    oprintf("$");
    /*FALLTHRU */
  case am_direct:
    putconst(ap->u.offset);
    break;
  case am_dreg:
  case am_areg:
    reg = ap->preg;
    switch (len) {
    case IL1:
      reg = (REG)REG8(reg);
      break;
    case IL2:
      reg = (REG)REG16(reg);
      break;
    default:
      break;
    }
    oprintf("%%%s", regname[reg]);
    break;
  case am_indx:
    if (ap->u.offset != NIL_EXPR) {
      putconst(ap->u.offset);
    }
    /*FALLTHRU */
  case am_ind:
    oprintf("(%%%s)", regname[small_option ? REG16(ap->preg) : ap->preg]);
    break;
  case am_indx2:
    if (ap->u.offset != NIL_EXPR) {
      putconst(ap->u.offset);
    }
    oprintf("(%%%s,%%%s)", regname[small_option ? REG16(ap->preg) : ap->preg],
            regname[small_option ? REG16(ap->sreg) : ap->sreg]);
    break;
  case am_freg:
    oprintf("%%%s", regname[ap->preg]);
    break;
  case am_line:
  case am_str:
    putconst(ap->u.offset);
    break;
  default:
    FATAL((__FILE__, "putamode", "illegal address mode %d", ap->mode));
    break;
  }
}

/*
 * output a generic instruction.
 */
PRIVATE void put_code P1(const CODE *, ip) {
  ILEN len = ip->length;
  ILEN len1 = len;
  ILEN len2 = len1;

  /*
   * this is expensive, but some assemblers require it
   * this is to be moved to the peephole optimizer
   */
  switch (ip->opcode) {
  case op_sete:
  case op_setne:
  case op_setb:
  case op_setbe:
  case op_seta:
  case op_setae:
  case op_setl:
  case op_setle:
  case op_setg:
  case op_setge:
    len = IL0;
    break;
  case op_shl:
  case op_shr:
  case op_asl:
  case op_asr:
    len1 = IL1;
    break;
  case op_movsbw:
  case op_movzbw:
    len2 = IL2;
    /*FALLTHRU */
  case op_movsbl:
  case op_movzbl:
    len1 = IL1;
    break;
  case op_movswl:
  case op_movzwl:
    len1 = IL2;
    break;
  default:
    break;
  }

  putop(ip->opcode);
  putlen(len);
  /*
   * Sun reverses the INTEL syntax:
   * The source comes first
   * The destination comes second
   * The imul instruction is special (3 operands), this
   * is handled here
   */
  if (ip->oper1 != NIL_ADDRESS) {
    oprintf("\t");
    putamode(ip->oper1, len1);
    if (ip->oper2 != NIL_ADDRESS) {
      if (ip->opcode == op_line) {
        oprintf("%s%s>>>>\t", newline, comment);
      } else {
        oprintf(", ");
      }
      putamode(ip->oper2, len2);
      /*
       * Assembler has strange syntax
       */
      if (ip->opcode == op_imul && ip->oper1->mode == am_immed && ip->oper2->mode == am_dreg) {
        oprintf(", ");
        putamode(ip->oper2, len2);
      }
    }
  }
  oprintf("%s", newline);
}

/*
 * generate a named label.
 */
PRIVATE void put_name P1(SYM *, sp) {
  put_reference(sp);
  oprintf("%s:%s", outlate(nameof(sp)), newline);
}

/*
 * output a compiler generated label.
 */
PRIVATE void put_label P1(LABEL, lab) { oprintf("%s%u:%s", prefix, (unsigned int)lab, newline); }

static void put_header P2(enum e_gt, gtype, SIZE, al) {
  static const char *directive[] = {
      ".byte\t",  /* bytegen */
      ".value\t", /* wordgen */
      ".long\t",  /* longgen */
  };

  if (gentype != gtype || outcol >= MAX_WIDTH) {
    put_align(al);
    gentype = gtype;
    outcol = 15;
    oprintf("\t%s", directive[gtype]);
  } else {
    oprintf(",");
  }
}

PRIVATE void put_byte P1(UVAL, val) {
  put_header(bytegen, alignment_of_type(tp_char));
  oprintf("0x%lx", val & OxffUL);
  outcol += 4;
}

PRIVATE void put_word P1(UVAL, val) {
  put_header(wordgen, alignment_of_type(tp_short));
  oprintf("0x%lx", val & OxffffUL);
  outcol += 6;
}

PRIVATE void put_dword P1(UVAL, val) {
  put_header(longgen, alignment_of_type(tp_long));
  oprintf("0x%lx", val);
  outcol += 10;
}

#ifndef FLOAT_BOOTSTRAP
#ifdef FLOAT_IEEE
/*
 * Generate IEEE single and double numbers
 */
PRIVATE void put_float P1(const RVAL *, vp) {
  unsigned long ul;

  ieee_single(vp, &ul);
  put_dword(ul);
}

PRIVATE void put_double P1(const RVAL *, vp) {
  unsigned long ul[2];

  ieee_double(vp, ul, FALSE);
  put_dword(ul[0]);
  put_dword(ul[1]);
}

PRIVATE void put_longdouble P1(const RVAL *, vp) {
  unsigned long ul[3];

  ieee_longdouble(vp, ul, FALSE);
  put_dword(ul[0]);
  put_dword(ul[1]);
  put_dword(ul[2]);
}

#endif /* FLOAT_IEEE */
#endif /* FLOAT_BOOTSTRAP */

#ifndef RELOC_BUG
PRIVATE void put_char P1(const EXPR *, ep) {
  put_header(bytegen, alignment_of_type(tp_char));
  putconst(ep);
  outcol += 10;
}

PRIVATE void put_short P1(const EXPR *, ep) {
  put_header(wordgen, alignment_of_type(tp_short));
  putconst(ep);
  outcol += 10;
}

#endif /* RELOC_BUG */

PRIVATE void put_long P1(const EXPR *, ep) {
  put_header(longgen, alignment_of_type(tp_long));
  putconst(ep);
  outcol += 10;
}

PRIVATE void put_pointer P1(const EXPR *, ep) {
  put_header((tp_pointer->size == 2L ? wordgen : longgen), alignment_of_type(tp_pointer));
  putconst(ep);
  outcol += 10;
}

PRIVATE void put_storage P1(SYM *, sp) {
  SIZE size = typeof(sp)->size;

  put_bseg(alignment_of_type(typeof(sp)));
  if (is_static(sp)) {
    oprintf("\t.lcomm\t%s%u,%ld%s", prefix, (unsigned)sp->value.l, size, newline);
  } else {
    oprintf("\t.comm\t%s,%ld%s", outlate(nameof(sp)), size, newline);
  }
}

/*
 * dump the string literal pool.
 * if we are producing single copies of strings (which should therefore
 * be read only we put them in the text segment - else in the data segment.
 */
PRIVATE void put_literals P0(void) {
  const CHAR *cp;
  size_t len;

  if (trad_option) {
    put_dseg(alignment_of_type(tp_char));
  } else {
    put_kseg(alignment_of_type(tp_char));
  }
  for (; strtab != NIL_STRING; strtab = strtab->next) {
    nl();
    put_label(strtab->label);
    cp = strtab->str;
    for (len = strtab->len; len--;)
      put_byte((UVAL)*cp++);
    put_byte(Ox0UL);
  }
  nl();
}

PRIVATE void put_reference P1(SYM *, sp) {
  if (!is_symbol_output(sp)) {
    switch (storageof(sp)) {
    case sc_global:
      nl();
      oprintf(".globl\t%s%s", outlate(nameof(sp)), newline);
      break;
    case sc_external:
      nl();
      oprintf(".globl\t%s%s", outlate(nameof(sp)), newline);
      break;
    default:
      break;
    }
    symbol_output(sp);
  }
}

/* align the following data */
static void put_align P1(SIZE, al) {
  nl();
  if (al > align_type) {
    switch (al) {
    case 0L:
    case 1L:
      break;
    case 2L:
    case 4L:
      oprintf("\t.align\t%d%s", (int)al, newline);
      break;
    default:
      FATAL((__FILE__, "put_align", "align %ld", al));
    }
  }
  align_type = al;
}

/*
 * output any function epilogue code
 */
PRIVATE void put_epilogue P2(SYM *, sp, LABEL, label) {
  sp = sp;       /* keep the compiler quiet */
  label = label; /* keep the compiler quiet */
}

PRIVATE void nl P0(void) {
  if (outcol > 0) {
    oprintf("%s", newline);
    gentype = nogen;
    outcol = 0;
  }
}

static void seg P3(enum e_sg, segtype, const char *, segname, SIZE, al) {
  nl();
  if (curseg != segtype) {
    oprintf("\t%s%s", segname, newline);
    curseg = segtype;
    align_type = 0L;
  }
  put_align(al);
}

PRIVATE void put_cseg P1(SIZE, al) { seg(codeseg, ".text", al); }

PRIVATE void put_dseg P1(SIZE, al) { seg(dataseg, ".data", al); }

static void put_bseg P1(SIZE, al) { put_dseg(al); }

PRIVATE void put_kseg P1(SIZE, al) {
  if (IandD_option) {
    put_dseg(al);
  } else {
    put_cseg(al);
  }
}

PRIVATE void put_rseg P1(SIZE, al) { put_cseg(al); }

PRIVATE void put_finish P0(void) {}

PRIVATE void put_start P0(void) {
  oprintf("\t.file\t\"%s\"%s", in_file, newline);
  oprintf("\t.version\t\"01.01\"%s", newline);
}

#ifdef MULTIPLE_ASSEMBLERS
struct funcs sysvx86_func = {
    put_code,      put_name,     put_label,      put_byte,     put_word,   put_dword,
#ifndef RELOC_BUG
    put_char,      put_short,
#endif /* RELOC_BUG */
    put_long,      put_pointer,  put_storage,    put_literals, put_finish, put_start,
    put_reference, put_epilogue, put_cseg,       put_dseg,     put_kseg,   put_rseg,
#ifdef FLOAT_SUPPORT
    put_float,     put_double,   put_longdouble,
#endif /* FLOAT_SUPPORT */
};

#endif /* MULTIPLE_ASSEMBLERS */
#endif /* TARGET_SYSV */
#endif /* INTEL */
