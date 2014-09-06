#ifndef _SETJMP_H_
#define _SETJMP_H_
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params) ()
#endif
typedef struct {
long jmpret;
long jmp_d1;
long jmp_d2;
long jmp_d3;
long jmp_d4;
long jmp_d5;
long jmp_d6;
long jmp_d7;
long jmp_a1;
long jmp_a2;
long jmp_a3;
long jmp_a4;
long jmp_a5;
long jmp_a6;
long jmp_a7;
long jmp_sigmask;
} jmp_buf[1];
typedef struct {
jmp_buf jmp;
long sigmask;
} sigjmp_buf[1];
void longjmp _P_((jmp_buf,int));
int setjmp _P_((jmp_buf));
void siglongjmp _P_((sigjmp_buf,int));
int sigsetjmp _P_((sigjmp_buf,int));
#undef _P_
#endif
