/*-------------------------------------------------------------------------.
| program to read uqlx data sizes for QDOS executables from Unix           |
| 								           |
| Install somewhere on your path (i.e. /usr/local/bin)                     |
| 								           |
| qls path                           				           |
| 								           |
| Where path is a directory contains a .-UQLX- file		           |
| 								           |
| totally not (c) Jonathan Hudson                                          |
`-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#define __USE_GNU
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#ifdef NOINLINE
#define inline
#endif

#ifdef __GNUC__
#define PACKED __attribute__((packed))
#else
#define PACKED
#endif

typedef struct PACKED {
  uint32_t d_length;          /* file length */
  unsigned char d_access;     /* file access type */
  unsigned char d_type;       /* file type */
  uint32_t d_datalen PACKED;  /* data length */
  uint32_t d_reserved PACKED; /* Unused */
  short d_szname;             /* size of name */
  char d_name[36];            /* name area */
  uint32_t d_update PACKED;   /* last update */
  short d_version;
  short d_fileno;
  uint32_t d_backup;
} QLDIR_t;

short is_big_endian;

ushort swapword(ushort val) {
  return (is_big_endian) ? val : (ushort)(val << 8) + (val >> 8);
}

uint32_t swaplong(uint32_t val) {
  return (is_big_endian) ? val
                         : (uint32_t)(((uint32_t)swapword(val & 0xFFFF) << 16) |
                                      (uint32_t)swapword(val >> 16));
}

void usage(void) {
  fputs("usage: qls path\n", stderr);
  exit(0);
}

int main(int ac, char **av) {
  uint32_t one = 1;
  char secret[PATH_MAX];
  char *p, *q;
  int fd;
  QLDIR_t qd;
  struct stat s;

  is_big_endian = 1 - *(char *)&one;

  if (*(av + 1) && stat(*(av + 1), &s) == 0 && (S_ISDIR(s.st_mode))) {
    p = stpcpy(secret, *(av + 1));
    if (*(p - 1) != '/') {
      *p++ = '/';
    }
    q = p;

    strcpy(p, ".-UQLX-");

    if ((fd = open(secret, O_RDONLY, 0)) >= 0) {
      char fnam[PATH_MAX];
      int n;
      short len;

      n = q - secret;
      strncpy(fnam, secret, n);

      while (read(fd, &qd, sizeof(qd)) == sizeof(qd)) {
        len = swapword(qd.d_szname);
        strncpy(fnam + n, qd.d_name, len);
        *(fnam + n + len) = 0;

        if (stat(fnam, &s) == 0) {
          struct tm *tm;
          tm = localtime(&s.st_mtime);
          char tbuff[64];
          strftime(tbuff, sizeof(tbuff), "%F %T", tm);
          printf("%-36.*s%9ld%8d%4d %s\n", len, qd.d_name, s.st_size,
                 swaplong(qd.d_datalen), qd.d_type, tbuff);
        }
      }
    }
    close(fd);
  } else {
    usage();
  }
  return 0;
}
