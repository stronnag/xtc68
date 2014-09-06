#ifndef _LIMITS_H
#define _LIMITS_H
#define CHAR_BIT 8
#define SCHAR_MAX 127
#define SCHAR_MIN (-SCHAR_MAX-1)
#ifdef __STDC__
#define UCHAR_MAX 255U
#else
#define UCHAR_MAX 255
#endif
#ifdef __CHAR_UNSIGNED__
#define CHAR_MAX UCHAR_MAX
#define CHAR_MIN 0
#else
#define CHAR_MAX SCHAR_MAX
#define CHAR_MIN SCHAR_MIN
#endif
#define MB_LEN_MAX 1
#define SHRT_MAX 32767
#define SHRT_MIN (-SHRT_MAX-1)
#ifdef __STDC__
#define USHRT_MAX 65535U
#else
#define USHRT_MAX 65535
#endif
#define INT_MAX 2147483647
#define INT_MIN (-INT_MAX-1)
#ifdef __STDC__
#define UINT_MAX 4294967295U
#else
#define UINT_MAX 4294967295
#endif
#define LONG_MAX 2147483647
#define LONG_MIN (-LONG_MAX-1)
#ifdef __STDC__
#define ULONG_MAX 4294967295UL
#else
#define ULONG_MAX 4294967295
#endif
#define _POSIX_ARG_MAX 4096
#define _POSIX_CHILD_MAX 6
#define _POSIX_LINK_MAX 8
#define _POSIX_MAX_CANON 255
#define _POSIX_MAX_INPUT 255
#define _POSIX_NAME_MAX 14
#define _POSIX_NGROUPS_MAX 0
#define _POSIX_OPEN_MAX 16
#define _POSIX_PATH_MAX 255
#define _POSIXPIPE_BUF 512
#define ARG_MAX _POSIX_ARG_MAX
#define CHILD_MAX 255
#define LINK_MAX _POSIX_LINK_MAX
#define MAX_CANON _POSIX_MAX_CANON
#define MAX_INPUT _POSIX_MAX_INPUT
#define NAME_MAX 36
#define OPEN_MAX 254
#define PASS_MAX 8
#define PATH_MAX 36
#define PIPE_BUF _POSIX_PIPE_BUF
#define NGROUPS_MAX 0
#define NL_ARGMAX 9
#define NL_LANGMAX 14
#define NL_MSGMAX 32767
#define NL_NMAX 32767
#define NL_SETMAX 255
#define NL_TEXTMAX 255
#define NZERO 32
#ifndef TMP_MAX
#define TMP_MAX (0xffff)
#endif
#define MAXNAMELEN 50
#endif
