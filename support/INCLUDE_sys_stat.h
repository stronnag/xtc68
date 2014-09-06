#ifndef _SYS_STAT_H_
#define _SYS_STAT_H_
#ifndef _TYPES_H
#include <sys/types.h>
#endif
struct stat
{
dev_t st_dev;
ino_t st_ino;
mode_t st_mode;
short int st_nlink;
uid_t st_uid;
short int st_gid;
dev_t st_rdev;
off_t st_size;
time_t st_atime;
time_t st_mtime;
time_t st_ctime;
time_t st_rtime;
time_t st_btime;
size_t st_datas;
size_t st_blocks;
size_t st_blksize;
};
#define S_IFMT 0170000
#define S_IFREG 0100000
#define S_IFBLK 0060000
#define S_IFDIR 0040000
#define S_IFCHR 0020000
#define S_IFIFO 0010000
#define S_ISUID 0004000
#define S_ISGID 0002000
#define S_ISVTX 01000
#define S_IRWXU 00700
#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IXUSR 00100
#define S_IRWXG 00070
#define S_IRGRP 00040
#define S_IWGRP 00020
#define S_IXGRP 00010
#define S_IRWXO 00007
#define S_IROTH 00004
#define S_IWOTH 00002
#define S_IXOTH 00001
#define S_ISREG(m) ((m & S_IFMT) == S_IFREG)
#define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#define S_ISCHR(m) ((m & S_IFMT) == S_IFCHR)
#define S_ISBLK(m) ((m & S_IFMT) == S_IFBLK)
#define S_ISFIFO(m) ((m & S_IFMT) == S_IFIFO)
#ifdef __STDC__
#define _P_(params) params
#else
#define _P_(params)
#endif
int chmod _P_((const char *,int));
int fstat _P_((int,struct stat *));
int lstat _P_((const char *,struct stat *));
int mkdir _P_((const char *,...));
int mkfifo _P_((const char *,int));
int mknod _P_((const char * path,mode_t mode,dev_t dev));
int stat _P_((const char *,struct stat *));
mode_t umask _P_((mode_t));
#ifdef __LIBRARY__
int _Fstat _P_((chanid_t,struct stat *));
int _Stat _P_((const char *,struct stat *));
#endif
#undef _P_
#endif
