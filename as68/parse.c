
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
#include "parse.h"
#include "proto.h"

int line = 1;
int sawerror = 0;
int ignore = 0;
int funny_state = -1;

jmp_buf YYljbuf;
jmp_buf YYopbuf;

YYSTYPE yylval;
int yywidth = 0;

int token = 0;

SYM *cursym = (SYM *) NULL;

extern SYM dot;
extern long newdot;

extern EXPR Yexpr(), Xexpr(), Aexpr(), Sexpr(),
	    Pexpr(), Mexpr(), Uexpr(), Yfactor();
extern LIST *Yname_list(), *Yexpr_list();
extern OPERAND *Yoperand();

VOID Yinstruction(void);
VOID Ystatement (void);

VOID
Yerror( s )
	char *s;
{
	if (! ignore) {
		fprintf( stderr, "jas: line %d ( %s )\n", line, s );
		ignore = 1;
	}
	sawerror = 1;
	funny_state = -1;
	longjmp( YYljbuf, 1 );
}

VOID
Yline(void)
{
	if ( setjmp( YYljbuf ) ) {
		/*
		 * Return here on any call to Yerror to gobble up through NL
		 */
		while ( token && token != NL )
			token = yylex();
		if ( token ) {
			line++;
			token = yylex();
		}
		ignore = 0;
		return;
	}

	if ( Ylabel_list() )
		(VOID) Ystatement();

	if ( token != NL )
		Yerror( "end of line expected" );
	line++;
	token = yylex();
	ignore = 0;
}

int yyparse(void)
{
	token = yylex();
	while ( token ) {
		Yline();
		dot.value = newdot;
	}

	return sawerror;
}

int Ylabel_list(void)
{
	while ( token == NAME ) {
		cursym = yylval.sym;
		token = yylex();
		if ( token == _EQU ) {
			EXPR val;
			if ( cursym->flags & (SEGMT|EQUATED) )
				Yerror( "symbol redefinition" );
			token = yylex();
			val = Yexpr();
			if ( val.psym )
				Yerror( "illegal equated expression" );
			cursym->value = val.value;
			cursym->flags |= EQUATED | DEFINED;
			return 0;
		}
		if ( token != COLON  )
			Yerror( "missing ':'" );
		if ( cursym->flags & (SEGMT|EQUATED) )
			Yerror( "symbol redefinition" );
		cursym->flags |= ( dot.flags & SEGMT ) | DEFINED;
		cursym->value = dot.value;
		token = yylex();
	}
	return 1;
}

VOID Ystatement(void)
{
	if ( token != NL ) {
		(VOID) Yinstruction();
		cursym = (SYM *) NULL;
	}
}

VOID Yinstruction(void)
{
	STMT *sp;
	SYM *sym;
	EXPR val;
	register short size;
	register LIST *lp, *xlp;
#ifdef MINIX
	short asciz = 0;
#endif
	extern LIST *Yname_list();
	extern LIST *Yexpr_list();

	switch ( token ) {
	default:
		Yerror( "syntax error" );
		break;
#ifdef MINIX
	case _SECT:			/* must be followed by a section name */
		switch (token = yylex()) {
		default:
			Yerror( "syntax error" );
			break;
		case _TEXT:
			chsegmt( TXT );
			token = yylex();
			break;
		case _DATA:
			chsegmt( DAT );
			token = yylex();
			break;
		case END:
		case _BSS:
			chsegmt( BSS );
			token = yylex();
			break;
		}
		break;
#endif
	case END:
		token = yylex();
		break;
	case _TEXT:
		chsegmt( TXT );
		token = yylex();
		break;
	case _DATA:
		chsegmt( DAT );
		token = yylex();
		break;
	case _BSS:
		chsegmt( BSS );
		token = yylex();
		break;
	case _GLOBL:
		token = yylex();
		lp = Yname_list();
		for ( ; lp; lp = xlp ) {
			xlp = lp->next;
			lp->u.sym->flags |= GLOBAL | DEFINED;
			free( (char *) lp );
		}
		break;
#ifdef MINIX
	case _ASCII:
		asciz = 1;
		yywidth = 8;
		/* FALL THROUGH */
#endif
	case _DC:
		size = yywidth;
		if ( size == 0 )
			Yerror( ".dc must have width specifier" );
		token = yylex();
		lp = Yexpr_list();

		if ( size != 8 ) {
			if ( newdot & 1 ) {
				zeros( 1L );
				if ( cursym != (SYM *) NULL )
					cursym->value = newdot;
			}
		}
		for ( ; lp; lp = xlp ) {
			xlp = lp->next;
#ifndef LABELDIFF
			generate(size, GENRELOC, lp->u.val.value, lp->u.sym );
#else /* LABELDIFF */
			if( !lp->sym2 )
			generate(size, GENRELOC, lp->u.val.value, lp->u.val.psym );
			else {
				LDIFF *ldp;

				ldp = ALLO(LDIFF);

				ldp->plus  = lp->u.sym;
				ldp->minus = lp->sym2;
				ldp->constant = lp->u.val.value;

				generate(size, GENLDIFF, (long) ldp, NULL);
			}
#endif /* LABELDIFF */
			free( (char *) lp );
		}
#ifdef MINIX
		if ( asciz ) {
			generate(size, GENRELOC, 0L, (SYM *) NULL );
		}
#endif
		break;
	case _ORG:
		token = yylex();
		val = Yexpr();
		if ( val.psym )
			Yerror( "illegal use of symbol in expression" );
		if ( val.value < newdot )
			Yerror( "attempt to move current location backward" );
		if ( val.value > newdot ) {
			if ( newdot & 1 )
				zeros( 1L );
			if ( val.value > newdot )
				zeros( val.value - newdot );
		}
		break;
	case _DS:
		size = yywidth;
		if ( size == 0 )
			Yerror( ".ds must have width specifier" );
		token = yylex();
		val = Yexpr();
		if ( size != 8 ) {
			if ( newdot & 1 ) {
				zeros( 1L );
				if ( cursym != (SYM *) NULL )
					cursym->value = newdot;
			}
		}
		if ( val.psym )
			Yerror( "illegal use of symbol in expression" );
		zeros( val.value * (size/8) );
		break;
#ifdef MINIX
	case _ALIGN:
		token = yylex();
		val = Yexpr();
		if ( val.psym )
			Yerror( "illegal use of symbol in expression" );
		if ( val.value <= 0 )
			Yerror( "operand of .align must be > 0" );
		if ( newdot % val.value ) {
			long n = val.value - ( newdot % val.value );

			zeros( n );
			if ( cursym != (SYM *) NULL )
				cursym->value = newdot;
		}
		break;
#endif
	case _EVEN:
		if ( newdot & 1 ) {
			zeros( 1L );
			if ( cursym != (SYM *) NULL )
				cursym->value = newdot;
		}
		token = yylex();
		break;
	case _COMM:
		if ( (token = yylex()) != NAME	)
			Yerror( "missing label" );
		sym = yylval.sym;
		if ( sym->flags & (SEGMT|EQUATED) )
			Yerror( "symbol redefinition" );
		if ( (token = yylex()) != COMMA  )
			Yerror( "missing ','" );
		token = yylex();
		val = Yexpr();
		sym->flags |= EXTERN | DEFINED;
		sym->value = val.value;
		break;
	case INSTR:
		sp = yylval.stmt;
		sp->op0 = sp->op1 = (OPERAND *) NULL;
		token = yylex();
		if ( token != NL ) {
			sp->op0 = Yoperand();
			if ( token == COMMA ) {
				token = yylex();
				sp->op1 = Yoperand();
			}
		}
		geninst( sp );
		if ( sp->op0 ) {
			free( sp->op0 );
			if ( sp->op1 )
				free( sp->op1 );
		}
		free( sp );
		break;
	}
 }

LIST *
Yname_list(void)
{
	register LIST *lp, *xlp;

	if ( token != NAME )
		Yerror( "label expected" );
	lp = xlp = ALLO(LIST);
	xlp->next = (LIST *) NULL;
	xlp->u.sym = yylval.sym;
#ifdef LABELDIFF
	xlp->sym2 = (SYM *)NULL;
#endif /* LABELDIFF */
	token = yylex();
	while ( token == COMMA ) {
		if ( (token = yylex()) != NAME )
			Yerror( "label expected" );
		xlp->next = ALLO(LIST);
		xlp = xlp->next;
		xlp->next = (LIST *) NULL;
		xlp->u.sym = yylval.sym;
		token = yylex();
	}
	return lp;
}

#ifdef LABELDIFF
static int minus_seen  = 0;
static SYM * plus_sym  = (SYM *)NULL;
static SYM * minus_sym = (SYM *)NULL;

#endif /* LABELDIFF */
LIST *
Yexpr_list(void)
{
	EXPR val;
	register LIST *lp;
	LIST *xlp, *slp;

	if ( token == STRING ) {
		lp = make_slist( &xlp, yylval.str );
		token = yylex();
		free( yylval.str );
	} else {
		val = Yexpr();
		lp = xlp = ALLO(LIST);
		xlp->next = (LIST *) NULL;
		xlp->u.val = val;
#ifdef LABELDIFF
		if( !minus_seen )
			xlp->sym2 = NULL;
		else {
			xlp->u.sym = plus_sym;
			xlp->sym2 = minus_sym;
		}
#endif /* LABELDIFF */
	}
	while ( token == COMMA ) {
		token = yylex();
		if ( token == STRING ) {
			xlp->next = make_slist( &slp, yylval.str );
			xlp = slp;
			token = yylex();
			free( yylval.str );
		} else {
			val = Yexpr();
			xlp->next = ALLO(LIST);
			xlp = xlp->next;
			xlp->next = (LIST *) NULL;
			xlp->u.val = val;
#ifdef LABELDIFF
			if( !minus_seen )
				xlp->sym2 = NULL;
			else {
				xlp->u.sym = plus_sym;
				xlp->sym2 = minus_sym;
#endif /* LABELDIFF */
		}
	}
#ifdef LABELDIFF
	}
#endif /* LABELDIFF */
	return lp;
}

LIST *
make_slist( last, string )
	LIST **last;
	char *string;
{
	char *cp;
	LIST *lp, *xlp;

	lp = xlp = (LIST *) NULL;

	for ( cp = string; *cp; cp++ ) {
		int ch;

		if ( *cp == '\\' ) {
			switch ( cp[1] ) {
			case 'n': ch = '\n'; break;
			case 't': ch = '\t'; break;
			case 'r': ch = '\r'; break;
			case 'b': ch = '\b'; break;
			case '0': ch = '\0'; break;
			default:  ch = cp[1]; break;
			}
			cp++;
		} else {
			ch = *cp;
		}
		if ( lp == (LIST *) NULL ) {
			lp = xlp = ALLO(LIST);
			xlp->next = (LIST *) NULL;
		} else {
			xlp->next = ALLO(LIST);
			xlp = xlp->next;
			xlp->next = (LIST *) NULL;
		}
		xlp->u.val.psym = (SYM *) NULL;
		xlp->u.val.value = ch;
#ifdef LABELDIFF
		xlp->sym2 = NULL;
#endif /* LABELDIFF */
	}

	*last = xlp;
	return lp;
}

OPERAND *
Yoperand(void)
{
	register OPERAND *op;
	register int reg, inx;
	int short_abs = 0;

	EXPR val;

	op = ALLO(OPERAND);
	switch ( token ) {
	case SREG:
		op->mode = yylval.val;
		token = yylex();
		break;
	case REG:
		op->mode = yylval.val < 8 ? O_DN : O_AN;
		op->reg = reg = yylval.val;
		token = yylex();
		if ( token == MINUS || token == DIV ) {
			/*
			 * we have a register mask list
			 */
			op->mode = O_REGS;
			op->expr.value = 1L << reg;
			while ( token == MINUS || token == DIV ) {
				int tok = token;

				token = yylex();
				inx = yylval.val;
				if ( token != REG )
					Yerror("invalid register list");
				if ( tok == DIV ) {
					op->expr.value |= (1L << inx);
					reg = inx;
				} else {
					int i;

					if ( inx < reg )
						Yerror("invalid register list");
					for ( i = reg+1; i <= inx; i++ )
						op->expr.value |= (1L << i);
				}
				token = yylex();
				if ( tok == MINUS && token == MINUS )
					Yerror("invalid register list");
			}
		}
		break;
	case LP:
		if ( (token = yylex()) != REG )
			Yerror( "missing register" );
		reg = yylval.val;
		if ( (token = yylex()) != RP )
			Yerror( "missing ')'" );
		token = yylex();
		if ( token == PLUS ) {
			op->mode = O_POST;
			op->reg = reg;
			token = yylex();
		} else {
			op->mode = O_INDR;
			op->reg = reg;
		}
		break;
	default:
#ifdef MINIX
		if ( token == NAME && yywidth == 16 )
			short_abs = 1;
#endif
		funny_state = 0;
		if ( setjmp( YYopbuf ) ) {
			/*
			 * Yexpr() saw MINUS LP REG
			 */
			reg = yylval.val;
			if ( (token = yylex()) != RP )
				Yerror( "missing ')'" );
			op->mode = O_PRE;
			op->reg = reg;
			token = yylex();
			funny_state = -1;
			break;
		}
		val = Yexpr();
		funny_state = -1;
		if ( token != LP ) {
#ifdef MINIX
			if ( short_abs
			     || ( val.psym==NULL
				  && val.value>=-32768
				  && val.value<=32767 )
			   )
				op->mode = O_SABS;
			else
				op->mode = O_ABS;
#else
			op->mode = O_ABS;
#endif
			op->expr = val;
			break;
		}
		token = yylex();
		if ( token == REG ) {
			reg = yylval.val;
			if ( (token = yylex()) == COMMA ) {
				if ( (token = yylex()) != REG )
					Yerror( "missing register" );
				inx = yylval.val;
				if ( yywidth == 8 )
					Yerror( "index reg can't be byte" );
				if ( yywidth == 32 )
					inx |= 0x10;
				if ( (token = yylex()) != RP )
					Yerror( "missing ')'" );
				op->mode = O_INDX;
				op->reg = reg;
				op->inx = inx;
				op->expr = val;
				token = yylex();
			} else {
				if ( token != RP )
					Yerror( "missing ')'" );
				op->mode = O_DISP;
				op->reg = reg;
				op->expr = val;
				token = yylex();
			}
		} else if ( token == PC ) {
			if ( (token = yylex()) == COMMA ) {
				if ( (token = yylex()) != REG )
					Yerror( "missing register" );
				inx = yylval.val;
				if ( yywidth == 8 )
					Yerror( "index reg can't be byte" );
				if ( yywidth == 32 )
					inx |= 0x10;
				if ( (token = yylex()) != RP )
					Yerror( "missing ')'" );
				op->mode = O_PCIX;
				op->inx = inx;
				op->expr = val;
				token = yylex();
			} else {
				if ( token != RP )
					Yerror( "missing ')'" );
				op->mode = O_PCRL;
				op->expr = val;
				token = yylex();
			}
		} else
			Yerror( "register or pc expected" );
		break;
	case POUND:
		token = yylex();
		val = Yexpr();
		op->mode = O_IMM;
		op->expr = val;
		break;
	}
	return op;
 }

EXPR
Yexpr(void)
{
	EXPR val1, val2;

#ifdef LABELDIFF
	minus_seen = 0;

#endif /* LABELDIFF */
	val1 = Xexpr();
	while ( token == OR ) {
		funny_state = -1;
		token = yylex();
		val2 = Xexpr();
		if ( val1.psym || val2.psym )
			Yerror( "illegal use of symbol in expression" );
		val1.value |= val2.value;
	}
	return val1;
}

EXPR
Xexpr(void)
{
	EXPR val1, val2;

	val1 = Aexpr();
	while ( token == XOR ) {
		funny_state = -1;
		token = yylex();
		val2 = Aexpr();
		if ( val1.psym || val2.psym )
			Yerror( "illegal use of symbol in expression" );
		val1.value ^= val2.value;
	}
	return val1;
}

EXPR
Aexpr(void)
{
	EXPR val1, val2;

	val1 = Sexpr();
	while ( token == AND ) {
		funny_state = -1;
		token = yylex();
		val2 = Sexpr();
		if ( val1.psym || val2.psym )
			Yerror( "illegal use of symbol in expression" );
		val1.value &= val2.value;
	}
	return val1;
}

EXPR
Sexpr(void)
{
	register int op;
	EXPR val1, val2;

	val1 = Pexpr();
	while ( token == LSH || token == RSH ) {
		op = token;
		funny_state = -1;
		token = yylex();
		val2 = Pexpr();
		if ( val1.psym || val2.psym )
			Yerror( "illegal use of symbol in expression" );
		if ( op == LSH )
			val1.value <<= val2.value;
		else
			val1.value >>= val2.value;
	}
	return val1;
}

EXPR
Pexpr(void)
{
	register int op;
	EXPR val1, val2;

	val1 = Mexpr();
	while ( token == PLUS || token == MINUS ) {
		op = token;
		funny_state = -1;
		token = yylex();
		val2 = Mexpr();
		if ( op == PLUS ) {
			if ( val1.psym && val2.psym )
				Yerror( "illegal use of symbol in expression" );
			if ( val2.psym )
				val1.psym = val2.psym;
			val1.value += val2.value;
		} else {
#if 0
/*
 *   Patch (that does not work) for 'label-label' constructs
 */
#ifndef GENERIC
			if ( val1.psym && val2.psym &&
			     (val1.psym->flags & SEGMT) == (val2.psym->flags & SEGMT) ) {
				val1.value += val1.psym->value;
				val1.psym = 0;
				val2.value += val2.psym->value;
				val2.psym = 0;
			}
#endif
#endif
#ifdef LABELDIFF
			if ( val1.psym && val2.psym  ) {

				minus_seen = 1;
				plus_sym   = val1.psym;
				minus_sym  = val2.psym;

				val1.value = 0;
				val1.psym = 0;
				val2.value = 0;
				val2.psym = 0;
			}

#endif /* LABELDIFF */
			if ( val2.psym )
				Yerror( "illegal use of symbol in expression" );
			val1.value -= val2.value;
		}
	}
	return val1;
}

EXPR
Mexpr(void)
{
	register int op;
	EXPR val1, val2;

	val1 = Uexpr();
	while ( token == STAR || token == DIV || token == MOD ) {
		op = token;
		funny_state = -1;
		token = yylex();
		val2 = Uexpr();
		if ( val1.psym || val2.psym )
			Yerror( "illegal use of symbol in expression" );
		if ( op == DIV ) {
			if ( val2.value == 0L )
				Yerror( "divison by zero" );
			val1.value /= val2.value;
		} else if ( op == MOD ) {
			if ( val2.value == 0L )
				Yerror( "modulo by zero" );
			val1.value %= val2.value;
		} else {
			val1.value *= val2.value;
		}
	}
	return val1;
}

EXPR
Uexpr(void)
{
	EXPR val;

	switch ( token ) {
	case MINUS:
		if ( funny_state == 0 )
			funny_state = 1;
		else
			funny_state = -1;
		token = yylex();
		val = Yfactor();
		val.value = - val.value;
		if ( val.psym )
			Yerror( "illegal use of symbol in expression" );
		break;
	case NOT:
		funny_state = -1;
		token = yylex();
		val = Yfactor();
		if ( val.psym )
			Yerror( "illegal use of symbol in expression" );
		val.value = - val.value;
		break;
	default:
		val = Yfactor();
		break;
	}
	return val;
}

EXPR
Yfactor(void)
{
     EXPR val = {NULL,0};

	switch ( token ) {
	case NUMBER:
		funny_state = -1;
		val.psym = (SYM *) NULL;
		val.value = yylval.val;
		break;
	case NAME:
		funny_state = -1;
		if ( yylval.sym->flags & EQUATED ) {
			val.psym = (SYM *) NULL;
			val.value = yylval.sym->value;
			break;
		}
		val.psym = yylval.sym;
		val.value = 0L;
		break;
	case LP:
		if ( funny_state == 1 )
			funny_state = 2;
		else
			funny_state = -1;
		token = yylex();
		val = Yexpr();
		if ( token != RP )
			Yerror( "missing ')'" );
		break;
	case REG:
		if ( funny_state == 2 ) {
			longjmp( YYopbuf, 1 );
		}
	default:
		Yerror( "illegal expression" );
        }
	token = yylex();
	return val;
}
