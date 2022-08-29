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

/******************************************************************************
 *
 *  The statement module handles all of the possible c statements and builds a
 *  parse tree of the statements.
 *
 *  Each routine returns a pointer to a statement parse node which reflects the
 *  statement just parsed.
 *
 *****************************************************************************/

#include "chdr.h"
#include "expr.h"
#include "cglbdec.h"
#include "proto.h"

/********************************************************** Static Variables */

#ifndef SYNTAX_CORRECT
static BOOL return_found = FALSE; /* return statement encountered */
static int break_lvl = 0;         /* nesting depth of break statements */
static int cont_lvl = 0;          /* nesting depth of continue statements */
static int case_lvl = 0;          /* nesting depth of case statements */
static BOOL need_label = FALSE;
#endif /* SYNTAX_CORRECT */

/*********************************************** Static Function Definitions */

static STMT *mk_stmt P_((STMTTYPE));
static STMT *whilestmt P_((void));
static STMT *dostmt P_((void));
static STMT *forstmt P_((void));
static STMT *ifstmt P_((void));
static STMT *casestmt P_((void));
static STMT *switchstmt P_((void));
static STMT *retstmt P_((void));
static STMT *breakstmt P_((void));
static STMT *contstmt P_((void));
static STMT *exprstmt P_((void));
static STMT *labelstmt P_((void));
static STMT *gotostmt P_((void));
static STMT *statement P_((void));
static STMT *compound P_((void));

#ifdef TRACE
static STMT *tracestmt P_((void));

#endif /* TRACE */

/*****************************************************************************/

static STMT *mk_stmt P1(STMTTYPE, st) {
  STMT *snp;

  snp = (STMT *)xalloc(sizeof(STMT));

  snp->stype = st;
  snp->next = NIL_STMT;
  snp->exp = NIL_EXPR;
  snp->s1 = NIL_STMT;
#ifdef DEBUGOPT
  snp->line = act_line;
  snp->linetxt = mk_string(act_linetxt);
#endif /*DEBUGOPT */
  return snp;
}

#ifndef SYNTAX_CORRECT
/*
 * returns TRUE if the stmt is a "NULL" statement
 */
static BOOL is_empty_statement P1(const STMT *, snp) {
  if (snp == NIL_STMT)
    return TRUE;
  switch (snp->stype) {
  case st_compound:
    return is_empty_statement(snp->s1);
  default:
    return FALSE;
  }
}

/*
 * check to see if the expression is a constant expression ... used
 * to see if conditional expressions are "invariant".
 */
static void check_unconditional P2(const EXPR *, ep, const char *, str) {
  if (ep != NIL_EXPR) {
    if (tst_const(ep)) {
      message(WARN_CONST, str);
    } else if ((ep->nodetype == en_test) && (ep->v.p[0]->nodetype == en_sym) && is_array_type(ep->v.p[0]->etp)) {
      message(WARN_ARRAY, str);
    }
  }
}
#endif /* SYNTAX_CORRECT */

/*
 * whilestmt parses the c while statement.
 */
static STMT *whilestmt P0(void) {
  STMT *snp;

#ifndef SYNTAX_CORRECT
  break_lvl++;
  cont_lvl++;
#endif /* SYNTAX_CORRECT */
  snp = mk_stmt(st_while);
  getsym();
  needpunc(tk_openpa);
  snp->exp = condition_expression();
  needpunc(tk_closepa);
  snp->s1 = statement();
#ifndef SYNTAX_CORRECT
  need_label = FALSE;
  break_lvl--;
  cont_lvl--;
#endif /* SYNTAX_CORRECT */
  return snp;
}

/*
 * dostmt parses the c do-while construct.
 */
static STMT *dostmt P0(void) {
  STMT *snp;

#ifndef SYNTAX_CORRECT
  break_lvl++;
  cont_lvl++;
#endif /* SYNTAX_CORRECT */
  snp = mk_stmt(st_do);
  getsym();
  snp->s1 = statement();
  needpunc(kw_while);
  needpunc(tk_openpa);
  snp->exp = condition_expression();
  needpunc(tk_closepa);
  needpunc(tk_semicolon);
#ifndef SYNTAX_CORRECT
  need_label = FALSE;
  break_lvl--;
  cont_lvl--;
#endif /* SYNTAX_CORRECT */
  return snp;
}

static STMT *forstmt P0(void) {
  STMT *snp;

#ifndef SYNTAX_CORRECT
  break_lvl++;
  cont_lvl++;
#endif /* SYNTAX_CORRECT */
  snp = mk_stmt(st_for);
  getsym();
  needpunc(tk_openpa);
  snp->exp = expression();
#ifndef SYNTAX_CORRECT
  sequence_point();
  check_discard(snp->exp);
#endif /* SYNTAX_CORRECT */
  needpunc(tk_semicolon);
  if (lastst != tk_semicolon) {
    snp->v1.e = condition_expression();
  } else {
    snp->v1.e = NIL_EXPR;
  }
  needpunc(tk_semicolon);
  snp->v2.e = expression();
#ifndef SYNTAX_CORRECT
  sequence_point();
  check_discard(snp->v2.e);
#endif /* SYNTAX_CORRECT */
  needpunc(tk_closepa);
  snp->s1 = statement();
#ifndef SYNTAX_CORRECT
  need_label = FALSE;
  break_lvl--;
  cont_lvl--;
#endif /* SYNTAX_CORRECT */
  return snp;
}

/*
 * ifstmt parses the c if statement and an else clause if one is present.
 */
static STMT *ifstmt P0(void) {
  STMT *snp;

  snp = mk_stmt(st_if);
  getsym();
  if (lastst == tk_openpa) {
#ifndef SYNTAX_CORRECT
    BOOL needlab;
#endif /* SYNTAX_CORRECT */

    getsym();
    snp->exp = condition_expression();
#ifndef SYNTAX_CORRECT
    check_unconditional(snp->exp, "if");
#endif /* SYNTAX_CORRECT */
    needpunc(tk_closepa);
    snp->s1 = statement();
#ifndef SYNTAX_CORRECT
    needlab = need_label;
    need_label = FALSE;
#endif /* SYNTAX_CORRECT */
    if (lastst == kw_else) {
      getsym();
      snp->v1.s = statement();
    } else {
      snp->v1.s = NIL_STMT;
#ifndef SYNTAX_CORRECT
      /* check for a dangling else statement ... bad coding style? */
      if ((snp->s1 != NIL_STMT) && (snp->s1->stype == st_if) && (snp->s1->v1.s != NIL_STMT)) {
        message(WARN_ELSE);
      }
#endif /* SYNTAX_CORRECT */
    }
#ifndef SYNTAX_CORRECT
    need_label = need_label && needlab;
    if (is_empty_statement(snp->s1) && is_empty_statement(snp->v1.s)) {
      message(WARN_EMPTYIF);
    }
  } else {
    message(ERR_EXPREXPECT);
#endif /* SYNTAX_CORRECT */
  }
  return snp;
}

/*
 * consider the following piece of code:
 *
 *      switch (i) {
 *              case 1:
 *                      if (j) {
 *                              .....
 *                      } else
 *              case 2:
 *                      ....
 *      }
 *
 * case statements may be deep inside, so we need a global variable
 * last_case to link them
 */
static STMT *last_case;       /* last case statement within this switch */
static TYP *last_switch_type; /* type of switch controlling expression */

/*
 * cases are returned as separate statements. for normal cases label is the
 * case value and v1.i is zero. for the default case v1.i is nonzero.
 */
static STMT *casestmt P0(void) {
  STMT *snp;

  if (lastst == kw_case) {
    getsym();
    snp = mk_stmt(st_case);
    snp->v2.i = intexpr();
    /*
     * The constant expression in each case label is converted to the
     * promoted type of the controlling expression.
     */
#if 0
	if (last_switch_type) {
	    check_representable (snp->v2.i, promote_type (last_switch_type));
	}
#endif
  } else {
    /* lastst is kw_default */
    getsym();
    snp = mk_stmt(st_default);
  }
  snp->s1 = NIL_STMT;
  if (last_case) {
    last_case->s1 = snp;
  }
  last_case = snp;
  needpunc(tk_colon);
  snp->v1.s = statement();
#ifndef SYNTAX_CORRECT
  if (case_lvl == 0) {
    message(ERR_CASE);
    snp = snp->v1.s;
  }
#endif /* SYNTAX_CORRECT */
  return snp;
}

#ifndef SYNTAX_CORRECT
/*
 * check_cases will check to see if any duplicate cases exist in the case list
 * pointed to by casehead.
 */
static void check_cases P1(const STMT *, casehead) {
  const STMT *top, *cur;

  for (top = casehead; top != NIL_STMT; top = top->s1) {
    for (cur = top->s1; cur != NIL_STMT; cur = cur->s1) {
      if (cur->stype == st_default) {
        if (top->stype == st_default) {
          message(ERR_DUPDEFAULT);
          return;
        }
      } else {
        if (top->stype != st_default && cur->v2.i == top->v2.i) {
          message(ERR_DUPCASE, cur->v2.i);
          return;
        }
      }
    }
  }
}
#endif /* SYNTAX_CORRECT */

static STMT *switchstmt P0(void) {
  STMT *snp = mk_stmt(st_switch);
  STMT *local_last_case = last_case;
  TYP *local_last_switch_type = last_switch_type;

#ifndef SYNTAX_CORRECT
  break_lvl++;
  case_lvl++;
#endif /* SYNTAX_CORRECT */
  getsym();
  last_case = snp;
  snp->s1 = NIL_STMT;
  needpunc(tk_openpa);
  snp->exp = integral_expression();
#ifndef SYNTAX_CORRECT
  sequence_point();
  check_unconditional(snp->exp, "switch");
#endif /* SYNTAX_CORRECT */
  last_switch_type = snp->exp->etp;
  needpunc(tk_closepa);
#ifndef SYNTAX_CORRECT
  need_label = TRUE;
#endif /* SYNTAX_CORRECT */
  snp->v1.s = statement();
#ifndef SYNTAX_CORRECT
  need_label = FALSE; /* currently too difficult to work out if true */
  check_cases(snp->s1);
#ifdef FACIST
  {
    STMT *cur;

    for (cur = snp->s1; cur != NIL_STMT; cur = cur->s1) {
      if (cur->stype == st_default) {
        break;
      }
    }
    if (cur == NIL_STMT) {
      message(WARN_DEFAULT);
    }
  }
#endif /* FACIST */
  break_lvl--;
  case_lvl--;
#endif /* SYNTAX_CORRECT */

  last_case = local_last_case;
  last_switch_type = local_last_switch_type;
  return snp;
}

static STMT *retstmt P0(void) {
  STMT *snp;

  snp = mk_stmt(st_return);
  getsym();
  snp->exp = expression();
#ifndef SYNTAX_CORRECT
  sequence_point();
#endif /* SYNTAX_CORRECT */
  if (snp->exp != NIL_EXPR) {
#ifndef SYNTAX_CORRECT
    if (is_void(ret_type)) {
      message(ERR_VOIDFUNC);
    } else if (is_void(snp->exp->etp)) {
      message(ERR_VOIDRETURN);
    } else {
      if (snp->exp->nodetype == en_autocon) {
        message(WARN_LOCAL);
      }
      check_qualifiers(ret_type, snp->exp->etp);
#endif /* SYNTAX_CORRECT */
      snp->exp = implicit_castop(snp->exp, ret_type);
#ifndef SYNTAX_CORRECT
    }
  } else if (!is_void(ret_type)) {
    message(WARN_VALRETURN);
#endif /* SYNTAX_CORRECT */
  }
  needpunc(tk_semicolon);
#ifndef SYNTAX_CORRECT
  return_found = TRUE;
  need_label = TRUE;
#endif /* SYNTAX_CORRECT */
  return snp;
}

static STMT *breakstmt P0(void) {
  STMT *snp;

#ifndef SYNTAX_CORRECT
  if (break_lvl == 0) {
    message(ERR_BREAK);
  }
#endif /* SYNTAX_CORRECT */
  snp = mk_stmt(st_break);
  getsym();
  needpunc(tk_semicolon);
#ifndef SYNTAX_CORRECT
  need_label = TRUE;
#endif /* SYNTAX_CORRECT */
  return snp;
}

static STMT *contstmt P0(void) {
  STMT *snp;

#ifndef SYNTAX_CORRECT
  if (cont_lvl == 0) {
    message(ERR_CONT);
  }
#endif /* SYNTAX_CORRECT */
  snp = mk_stmt(st_continue);
  getsym();
  needpunc(tk_semicolon);
#ifndef SYNTAX_CORRECT
  need_label = TRUE;
#endif /* SYNTAX_CORRECT */
  return snp;
}

/*
 * exprstmt is called whenever a statement does not begin with a keyword. the
 * statement should be an expression.
 */
static STMT *exprstmt P0(void) {
  STMT *snp;

  if (lastst == tk_semicolon) {
    snp = NIL_STMT;
  } else {
    snp = mk_stmt(st_expr);
    snp->exp = expression();
#ifndef SYNTAX_CORRECT
    if (snp->exp == NIL_EXPR) {
      /* brute force fix to detect loop */
      if (errorloop) {
        message(ERR_EXPREXPECT);
      } else {
        errorloop = TRUE;
        getsym();
      }
    } else {
      check_discard(snp->exp);
    }
#endif /* SYNTAX_CORRECT */
  }
#ifndef SYNTAX_CORRECT
  sequence_point();
#endif /* SYNTAX_CORRECT */
  needpunc(tk_semicolon);
  return snp;
}

/*
 * compound processes a block of statements and forms a linked list of the
 * statements within the block.
 */
static STMT *compound P0(void) {
  STMT *stmthead, *stmttail, *snp;

  lc_auto = declaration_list(sc_auto, lc_auto);
  snp = mk_stmt(st_compound);
  if (init_node == NIL_EXPR) {
    stmthead = NIL_STMT;
  } else {
    stmthead = stmttail = mk_stmt(st_expr);
    stmthead->exp = init_node;
    stmthead->next = NIL_STMT;
  }
  init_node = NIL_EXPR;
  while (lastst != tk_end) {
    if (stmthead == NIL_STMT) {
      stmthead = stmttail = statement();
    } else {
      stmttail->next = statement();
      if (stmttail->next != NIL_STMT) {
        stmttail = stmttail->next;
      }
    }
  }
  snp->s1 = stmthead;
  return snp;
}

/*
 * labelstmt processes a label that appears before a statement as a separate
 * statement.
 */
static STMT *labelstmt P0(void) {
  STMT *snp;

  snp = mk_stmt(st_label);
  snp->v2.l = lab_define(lastsym);
  getsym();
  needpunc(tk_colon);
  snp->v1.s = statement();
  return snp;
}

/*
 * gotostmt processes the goto statement
 */
static STMT *gotostmt P0(void) {
  STMT *snp;

  snp = mk_stmt(st_goto);
  getsym();
  if (lastst == tk_id) {
    snp->v2.l = lab_search(lastsym);
    getsym(); /* get past label name */
#ifndef SYNTAX_CORRECT
  } else {
    message(ERR_IDEXPECT);
#endif /* SYNTAX_CORRECT */
  }
  needpunc(tk_semicolon);
#ifndef SYNTAX_CORRECT
  need_label = TRUE;
#endif /* SYNTAX_CORRECT */
  return snp;
}

#ifdef ASM
static STMT *asmstmt P0(void) {
  STMT *snp;

  snp = mk_stmt(st_asm);
  getsym();
  needpunc(tk_openpa);
  snp->exp = asm_expression();
  needpunc(tk_closepa);
  return snp;
}
#endif /* ASM */

#ifdef TRACE
/*
 * tracestmt adds calls to a run-time tracing/debugging routine.
 */
static STMT *tracestmt P0(void) {
  STMT *snp;

  snp = mk_stmt(st_compound);
  snp->s1 = mk_stmt(st_expr);
  snp->s1->exp = traceexpr();
  return snp;
}
#endif /* TRACE */

/*
 * statement figures out which of the statement processors should be called
 * and transfers control to the proper routine.
 */
static STMT *statement P0(void) {
  STMT *snp;

#ifdef TRACE
  STMT *tsnp;

  if (trace_option) {
    tsnp = tracestmt();
  }
#endif
#ifndef SYNTAX_CORRECT
  switch (lastst) {
  default:
    if (need_label) {
      message(WARN_NOTREACHED);
    }
    /*FALLTHRU */
  case kw_case:
  case kw_default:
    need_label = FALSE;
    /*FALLTHRU */
  case tk_begin:
  case tk_id:
    break;
  }
#endif /* SYNTAX_CORRECT */
  switch (lastst) {
#ifdef ASM
  case kw_asm:
    snp = asmstmt();
    break;
#endif /* ASM */
  case tk_begin:
    needpunc(tk_begin);
    beginblock();
    snp = compound();
    endblock();
    needpunc(tk_end);
    break;
  case kw_if:
    snp = ifstmt();
    break;
  case kw_while:
    snp = whilestmt();
    break;
  case kw_for:
    snp = forstmt();
    break;
  case kw_return:
    snp = retstmt();
    break;
  case kw_break:
    snp = breakstmt();
    break;
  case kw_goto:
    snp = gotostmt();
    break;
  case kw_continue:
    snp = contstmt();
    break;
  case kw_do:
    snp = dostmt();
    break;
  case kw_switch:
    snp = switchstmt();
    break;
  case kw_case:
  case kw_default:
    snp = casestmt();
    break;
  case tk_id:
    if (is_label(lastst)) {
#ifndef SYNTAX_CORRECT
      need_label = FALSE;
#endif /* SYNTAX_CORRECT */
      snp = labelstmt();
      break;
    }
    __attribute__((fallthrough));
    /* else fall through to process expression */
  default:
#ifndef SYNTAX_CORRECT
    if (need_label) {
      message(WARN_NOTREACHED);
    }
    need_label = FALSE;
#endif /* SYNTAX_CORRECT */
    snp = exprstmt();
    break;
  }
#ifdef TRACE
  if (trace_option) {
    tsnp->s1->next = snp;
    return tsnp;
  }
#endif /* TRACE */
  return snp;
}

/*
 * funcbody starts with the current symbol being the begin for the local
 * block or the first symbol of the parameter declaration
 */
void funcbody P2(SYM *, sp, BLOCK *, block) {
  int old_global;

#ifdef CPU_DEFINED
  STMT *stmt;
  SIZE poffset, return_block;
  LINE line = act_line;

#ifdef DEBUGOPT
  const CHAR *linetxt = mk_string(act_linetxt);

#else
  const CHAR *linetxt = (const CHAR *)NULL;

#endif /* DEBUGOPT */
  SYM *sp1;

#endif /* CPU_DEFINED */

  STARTTIME(ltime);

#ifdef VERBOSE
  if (verbose_option) {
    eprintf("%s%s", nameof(sp), newline);
  }
#endif /* VERBOSE */

  uses_structassign = FALSE;
  lc_auto_max = lc_auto = 0L;
#ifndef SYNTAX_CORRECT
  need_label = FALSE;
  return_found = FALSE;
#endif /* SYNTAX_CORRECT */
  is_leaf_function = TRUE;
  init_node = NIL_EXPR;
  old_global = global_flag;
  global_flag = 0;
  ret_type = returned_type(TYPEOF(sp));

#ifdef CPU_DEFINED
  return_block = tp_pointer->size;  /* return address */
  return_block += tp_pointer->size; /* saved frame pointer */
  if (is_structure_type(ret_type)) {
    return_block += tp_pointer->size;
  }
  poffset = return_block;

  for (sp1 = symbolsof(block); sp1; sp1 = nextsym(sp1)) {
    poffset = calculate_offset(sp1, poffset, sc_parms, !is_ansi(TYPEOF(sp)));
    addoptinfo(sp1, sc_parms);
  }
#endif /* CPU_DEFINED */

  needpunc(tk_begin);
  beginfuncblock(block);
#ifdef CPU_DEFINED
  stmt = compound();
#else
  VOIDCAST compound();

#endif /* CPU_DEFINED */
  check_labels();
#ifndef SYNTAX_CORRECT
  if (!is_void(ret_type) && !return_found) {
    message(WARN_IMPLICITRET);
  }
#endif /* SYNTAX_CORRECT */
  DOTIME(parse_time, ltime);
#ifdef ICODE
  if (icode_option) {
    iprintf("%s:\n", nameof(sp));
    genicode(stmt, 0);
  }
#endif /* ICODE */
#ifdef CPU_DEFINED
  if (code_option) {
    CSE *cse = globalopt(stmt);

    DOTIME(opt_time, ltime);
    genfunc(sp, stmt, cse, line, linetxt);
    DOTIME(gen_time, ltime);
  }
#endif /* CPU_DEFINED */
  endfuncblock();
  needpunc(tk_end);

  rel_local(); /* release local symbols */

  global_flag = old_global;
}
