#ifndef _STDIO_H
#define _STDIO_H
#define _STDIO_VA_LIST_ char *
#define _STDIO_SIZE_T_ unsigned long
#define _STDIO_USIZE_T_ size_t
#ifdef __STDC__
# ifndef _STDIO_P_
# define _STDIO_P_(x) x
# endif
# ifndef _STDIO_VA_
# define _STDIO_VA_,...
# endif
# ifndef _STDIO_UCHAR_
# define _STDIO_UCHAR_ 0
# endif
#else
# ifndef _STDIO_P_
# define _STDIO_P_(x) ()
# endif
# ifndef _STDIO_VA_
# define _STDIO_VA_
# endif
# ifndef _STDIO_UCHAR_
# define _STDIO_UCHAR_ (0xff)
# endif
#endif
#ifndef _STDIO_VA_LIST_
# define _STDIO_VA_LIST_ void *
#endif
#ifndef _STDIO_SIZE_T_
# define _STDIO_SIZE_T_ unsigned int
#endif
#ifndef _STDIO_USIZE_T_
# define _STDIO_USIZE_T_ unsigned int
#endif
#define BUFSIZ 1024
#ifndef NULL
# define NULL ((void *) 0)
#endif
#define EOF (-1)
#define FOPEN_MAX 16
#define FILENAME_MAX 127
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#ifndef TMP_MAX
#define TMP_MAX (0xffff)
#endif
#define L_tmpnam (5 + 8 + 4 + 1 + 1)
#ifndef _FPOS_T
# define _FPOS_T
typedef long fpos_t;
#endif
#ifndef _SIZE_T
# define _SIZE_T
typedef _STDIO_SIZE_T_ size_t;
#endif
#define _IOFBF 000000
#define _IOREAD 000001
#define _IOWRITE 000002
#define _IONBF 000004
#define _IOMYBUF 000010
#define _IOPOOLBUF 000020
#define _IOEOF 000040
#define _IOERR 000100
#define _IOSTRING 000200
#define _IOLBF 000400
#define _IORW 001000
#define _IOAPPEND 002000
#define _IOINSERT 004000
#define _IOSTDX 030000
#define _IOTTY 040000
#define _IOSTDIN 010000
#define _IOSTDOUT 020000
#define _IOSTDERR 030000
#define _IORETAIN (_IOSTDX | _IOINSERT)
typedef char __stdiobuf_t;
typedef _STDIO_USIZE_T_ __stdiosize_t;
typedef struct __iobuf {
__stdiobuf_t *__rptr;
__stdiobuf_t *__rend;
__stdiobuf_t *__wptr;
__stdiobuf_t *__wend;
__stdiobuf_t *__base;
__stdiosize_t __bufsiz;
short __flag;
char __file;
__stdiobuf_t __buf;
int (*__filbuf) _STDIO_P_((struct __iobuf *));
int (*__flsbuf) _STDIO_P_((int,struct __iobuf *));
int (*__flush) _STDIO_P_((struct __iobuf *));
struct __iobuf *__next;
} FILE;
extern FILE __stdin;
extern FILE __stdout;
extern FILE __stderr;
#define stdin (&__stdin)
#define stdout (&__stdout)
#define stderr (&__stderr)
#ifdef __GNUC__
static __inline int _STDIO_GETC_(FILE *fp)
{
char *rptr = fp->__rptr;
int c;
if (rptr >= fp->__rend)
c = (*fp->__filbuf)(fp);
else {
c = (unsigned char) *rptr++;
fp->__rptr = rptr;
}
return c;
}
#else
#if _STDIO_UCHAR_
# define _STDIO_GETC_(p) ((p)->__rptr>=(p)->__rend\
?(*(p)->__filbuf)(p)\
:(int)(*(p)->__rptr++&_STDIO_UCHAR_))
#else
# define _STDIO_GETC_(p) ((p)->__rptr>=(p)->__rend\
?(*(p)->__filbuf)(p)\
:(int)((unsigned char)(*(p)->__rptr++)))
#endif
#endif
int getc _STDIO_P_((FILE *));
#define getc(p) _STDIO_GETC_(p)
int getchar _STDIO_P_((void));
#define getchar() getc(stdin)
#ifdef __GNUC__
static __inline int _STDIO_PUTC_(int x,FILE *fp)
{
char *wptr = fp->__wptr;
int c;
if (wptr >= fp->__wend)
c = (*fp->__flsbuf)(x,fp);
else {
*wptr++ = x;
fp->__wptr = wptr;
c = (unsigned char) x;
}
return c;
}
#else
#if _STDIO_UCHAR_
# define _STDIO_PUTC_(x,p) ((p)->__wptr>=(p)->__wend\
?(*(p)->__flsbuf)((x),(p))\
:(int)(*(p)->__wptr++=(x)&_STDIO_UCHAR_))
#else
# define _STDIO_PUTC_(x,p) ((p)->__wptr>=(p)->__wend\
?(*(p)->__flsbuf)((x),(p))\
:(int)((unsigned char)(*(p)->__wptr++=(x))))
#endif
#endif
int putc _STDIO_P_((int,FILE *));
#define putc(x,p) _STDIO_PUTC_(x,p)
int putchar _STDIO_P_((int));
#define putchar(x) putc(x,stdout)
#define _STDIO_FEOF_(p) (((p)->__flag&_IOEOF)!=0)
int feof _STDIO_P_((FILE *));
#define feof(p) _STDIO_FEOF_(p)
#define _STDIO_FERROR_(p) (((p)->__flag&_IOERR)!=0)
int ferror _STDIO_P_((FILE *));
#define ferror(p) _STDIO_FERROR_(p)
#define _STDIO_CLEARERR_(p) ((p)->__flag&=~(_IOEOF|_IOERR))
void clearerr _STDIO_P_((FILE *));
#define clearerr(p) _STDIO_CLEARERR_(p)
FILE *fopen _STDIO_P_((const char *,const char *));
FILE *freopen _STDIO_P_((const char *,const char *,FILE *));
int fflush _STDIO_P_((FILE *));
int fclose _STDIO_P_((FILE *));
int fgetpos _STDIO_P_((FILE *,fpos_t *));
int fsetpos _STDIO_P_((FILE *,const fpos_t *));
long ftell _STDIO_P_((FILE *));
int fseek _STDIO_P_((FILE *,long,int));
void rewind _STDIO_P_((FILE *));
int fgetc _STDIO_P_((FILE *));
int fputc _STDIO_P_((int,FILE *));
__stdiosize_t fread _STDIO_P_((void *,__stdiosize_t,
__stdiosize_t,FILE *));
__stdiosize_t fwrite _STDIO_P_((const void *,__stdiosize_t,
__stdiosize_t,FILE *));
int getw _STDIO_P_((FILE *));
int putw _STDIO_P_((int,FILE *));
char *gets _STDIO_P_((char *));
char *fgets _STDIO_P_((char *,int,FILE *));
int puts _STDIO_P_((const char *));
int fputs _STDIO_P_((const char *,FILE *));
int ungetc _STDIO_P_((int,FILE *));
int printf _STDIO_P_((const char * _STDIO_VA_));
int fprintf _STDIO_P_((FILE *,const char * _STDIO_VA_));
int sprintf _STDIO_P_((char *,const char * _STDIO_VA_));
int vprintf _STDIO_P_((const char *,_STDIO_VA_LIST_));
int vfprintf _STDIO_P_((FILE *,const char *,_STDIO_VA_LIST_));
int vsprintf _STDIO_P_((char *,const char *,_STDIO_VA_LIST_));
int scanf _STDIO_P_((const char * _STDIO_VA_));
int fscanf _STDIO_P_((FILE *,const char * _STDIO_VA_));
int sscanf _STDIO_P_((const char *,const char * _STDIO_VA_));
void setbuf _STDIO_P_((FILE *,char *));
int setvbuf _STDIO_P_((FILE *,char *,int,__stdiosize_t));
int rename _STDIO_P_((const char *,const char *));
int remove _STDIO_P_((const char *));
void perror _STDIO_P_((const char *));
char * tmpnam _STDIO_P_((char *));
FILE * tmpfile _STDIO_P_((void));
#ifdef _POSIX_SOURCE
# undef _STDIO_POSIX_
# define _STDIO_POSIX_
#endif
#if _POSIX_1_SOURCE > 0
# undef _STDIO_POSIX_
# define _STDIO_POSIX_
#endif
#ifdef QDOS
# undef _STDIO_POSIX_
# define _STDIO_POSIX_
#endif
#ifdef _STDIO_POSIX_
#ifndef QDOS
# define P_tmpdir"/tmp/"
#else
# define P_tmpdir"ram1_"
#endif
# define L_ctermid 9
char * ctermid _STDIO_P_((char *s));
# define L_cuserid 9
char * cuserid _STDIO_P_((char *s));
FILE * fdopen _STDIO_P_((int,const char *));
# define _STDIO_FILENO_(p) ((p)->__file)
int fileno _STDIO_P_((FILE *));
# define fileno(p) _STDIO_FILENO_(p)
#endif
#ifdef QDOS
int pclose _STDIO_P_((FILE *));
FILE *popen _STDIO_P_((const char *,const char *));
#define clrerr clearerr
FILE *fopene _STDIO_P_((const char *,const char *,int));
int getch _STDIO_P_((void));
int getche _STDIO_P_((void));
int kbhit _STDIO_P_((void));
int putch _STDIO_P_((int));
int ungetch _STDIO_P_((int));
void setnbf _STDIO_P_((FILE *));
long fgetchid _STDIO_P_((FILE *));
FILE *fusechid _STDIO_P_((long));
#endif
#undef _STDIO_POSIX_
#undef _STDIO_P_
#undef _STDIO_VA_
#undef _STDIO_VA_LIST_
#undef _STDIO_SIZE_T_
#undef _STDIO_USIZE_T_
#endif
