#ifndef JAS_PROTOS_LOADED
#define JAS_PROTOS_LOADED

#define VOID void
extern char *swapw(char *, long);
extern char *swapl(char *, long);
extern void warn(int, char *);
extern void error(int, char *);
extern char *allocate(unsigned long);
extern char *myreallocate(char *, unsigned long);
extern void setflags(int, char **);
extern void output(char *, size_t, size_t);
extern void output2fb(unsigned char *, int, int);
extern void my_free(char *);
extern void bufhead(void);
extern CBUF *cget(unsigned short);
extern CBUF *generate(short, short, long, SYM *);
extern VOID zeros(long);
extern VOID addrel(unsigned short);
extern VOID dumprel(void);
extern VOID translate(unsigned short, int null);
extern VOID chkvalue(CBUF *code);
extern void pcrossw(short, char *);
extern void pcrossl(long l, char *);
extern char *mklowbyte(long);
extern char *mklowshort(long);
extern char *mklowlong(long);
extern char *swapw(char *, long);
extern char *swapl(char *, long);
extern char *fixsym(void *);
extern int inst_ok(STMT *);
extern VOID findinst(STMT *);
extern VOID geninst(STMT *);
extern void headers(void);
extern void yyinit(void);
extern int yyprocess(char c);
extern void yymodify(char *);
extern VOID geninst(STMT *);
extern int icmpare(char *, char *, short *);
extern INST *ifind(char *, short *);
extern int chk_cond(char *, int, short *);
extern void add_brnch(CBUF *, long);
extern void do_opt(void);
extern VOID Yerror(char *);
extern VOID Yline(void);
extern int yyparse(void);
extern int Ylabel_list(void);
extern VOID Ystatement(void);
extern VOID Yinstruction(void);
extern LIST *Yname_list(void);
extern LIST *Yexpr_list(void);
extern LIST *make_slist(LIST **last, char *);
extern OPERAND *Yoperand(void);
extern EXPR Yexpr(void);
extern EXPR Xexpr(void);
extern EXPR Aexpr(void);
extern EXPR Sexpr(void);
extern EXPR Pexpr(void);
extern EXPR Mexpr(void);
extern EXPR Uexpr(void);
extern EXPR Yfactor(void);
extern void chsegmt(unsigned short);
extern void aspass1(void);
extern int getnext(void);
extern int yygetc(void);
extern void yyungetc(int);
extern int yylex(void);
extern long getnum(char *);
extern int hash(char *);
extern VOID setname(SYM *, char *);
extern VOID cpyname(char *, SYM *);
extern int cmpname(SYM *, char *);
extern SYM *newsym(void);
extern SYM *lookup(char *);
extern VOID symwalk(int acc, VOID (*fun)(SYM *));
extern VOID putsym(SYM *p);
extern VOID flexsym(void);
extern VOID dumpsym(void);
extern VOID setindex(SYM *);
extern VOID symindex(void);
extern VOID fixsymval(long, long, unsigned short);

#endif