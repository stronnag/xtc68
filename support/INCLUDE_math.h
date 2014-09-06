#ifndef _MATH_H
#define _MATH_H
#ifndef _SYS_TYPES_H
#include <sys/types.h>
#endif
#include <sys/math.h>
#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<=(b)?(a):(b))
struct exception {
int type;
char * name;
double arg1,arg2;
double retval;
};
#define DOMAIN 1
#define SING 2
#define OVERFLOW 3
#define UNDERFLOW 4
#define TLOSS 5
#define PLOSS 6
#define FPEUND 1
#define FPEOVF 2
#define FPEZDV 3
#define FPENAN 4
#define FPECOM 5
#define HUGE_VAL (_Infinity)
#define M_LN2 0.69314718055994530942
#define M_LOG2E 1.4426950408889633870E0
#define M_LN10 2.30258509299404568402
#define M_LOG10E 4.3429448190325182765E-1
#define M_PI 3.14159265358979323846
#define M_PI_2 1.57079632679489661923
#define M_PI_4 0.78539816339744830962
#define M_1_PI 0.31830988618379067154
#define M_2_PI 0.63661977236758134308
#define M_2_SQRTPI 1.12837916709551257390
#define M_SQRT2 1.41421356237309514556
#define M_SQRT1_2 0.70710678118654752440
#define MAXFLOAT 3.40282346638528860e+38
#define HUGE (_HUGE_VAL)
#define PI 3.14159265358979323846
#define PI2 6.28318530717958647693
#define PID2 1.57079632679489661923
#define PID4 0.78539816339744830962
#define I_PI 0.31830988618379067154
#define I_PID2 0.63661977236758134308
#define TINY 2.2e-308
#define LOGHUGE 709.778
#define LOGTINY -708.396
extern int _fperr;
extern int errno;
extern int signgam;
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
double acos _P_((double));
double asin _P_((double));
double atan _P_((double));
double atan2 _P_((double,double));
double ceil _P_((double));
double cos _P_((double));
double cosh _P_((double));
double exp _P_((double));
double fabs _P_((double));
double floor _P_((double));
double fmod _P_((double,double));
double frexp _P_((double,int *));
double ldexp _P_((double,int));
double log _P_((double));
double log10 _P_((double));
double modf _P_((double,double *));
double pow _P_((double,double));
double sin _P_((double));
double sinh _P_((double));
double sqrt _P_((double));
double tan _P_((double));
double tanh _P_((double));
int matherr _P_((struct exception *));
double acosh _P_((double));
double asinh _P_((double));
double atanh _P_((double));
double cbrt _P_((double));
double copysign _P_((double,double));
double cot _P_((double));
double erf _P_((double));
double erfc _P_((double));
double hypot _P_((double,double));
double logb _P_((double));
double nextafter _P_((double,double));
double remainder _P_((double,double));
double rint _P_((double));
double scalb _P_((double,double));
double j0 _P_((double));
double j1 _P_((double));
double jn _P_((int,double));
double y0 _P_((double));
double y1 _P_((double));
double yn _P_((int,double));
double gamma _P_((double));
double lgamma _P_((double));
int finite _P_((double));
int isnan _P_((double));
int unordered _P_((double,double));
#define acosf(param) (float)acos((double)(param))
#define asinf(param) (float)asin((double)(param))
#define atan2f(param) (float)atan2((double)(param))
#define atanf(param) (float)atan((double)(param))
float ceilf _P_((float));
#define cosf(param) (float)cos((double)(param))
#define coshf(param) (float)cosh((double)(param))
float expf _P_((float));
float fabsf _P_((float));
float floorf _P_((float));
float fmodf _P_((float,float));
float logf _P_((float));
float log10f _P_((float));
float modff _P_((float,float *));
float powf _P_((float,float));
#define sinf(param) (float)sin((double)(param))
#define sinhf(param) (float)sinh((double)(param))
float sqrtf _P_((float));
#define tanf(param) (float)tan((double)(param))
#define tanhf(param) (float)tanh((double)(param))
#define arctan atan
double except _P_((int,char *,double,double,double));
char * ecvt _P_((double,int,int *,int *));
char * fcvt _P_((double,int,int *,int *));
char * gcvt _P_((double,int,char *));
#ifdef _LIBM_SOURCE
#ifndef __LIBRARY__
#define __LIBRARY__
#endif
#endif
#ifdef __LIBRARY__
double _mult _P_((double,double));
double _poly _P_((double,double *,int));
#endif
#undef _P_
#endif
