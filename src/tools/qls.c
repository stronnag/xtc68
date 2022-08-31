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

#if defined(__unix__) || defined(__APPLE__)
#include <arpa/inet.h>
#else
#include <winsock.h>
#endif

#include <libgen.h>

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

#ifdef WIN32
char *stpcpy(char *d, const char *s) {
  while ((*d++ = *s++)) /* NULL loop */
    ;
  return d - 1;
}
#endif

void usage(void) {
  fputs("usage: qls path\n", stderr);
  exit(0);
}

int main(int ac, char **av) {
  char *secret;
  char *p;
  int fd;
  QLDIR_t qd;
  struct stat s;

    if(ac > 1) {
    char *dname = av[1];
    secret = malloc(strlen(dname)+16);
    p = stpcpy(secret, dname);
    strcpy(p, "/.-UQLX-");
    if ((fd = open(secret, O_RDWR, 0)) >= 0) {
      char fnam[PATH_MAX];
      short len;

      char *q = stpcpy(fnam, dname);
      *q++ = '/';
      int ie = 0;
      while (read(fd, &qd, sizeof(qd)) == sizeof(qd)) {
        len = htons(qd.d_szname);
        if(len > 36) {
          memset(&qd, 0, sizeof(qd));
          lseek(fd, -1 * sizeof(qd), SEEK_CUR);
          write(fd, &qd, sizeof(qd));
          len = 0;
        }
        if(len != 0) {
          strncpy(q, qd.d_name, len);
          *(q + len) = 0;
          if (stat(fnam, &s) == 0) {
            struct tm *tm;
            tm = localtime(&s.st_mtime);
            char tbuff[64];
            strftime(tbuff, sizeof(tbuff), "%Y-%m-%d %H:%m:%S", tm);
            printf("%-36.*s%9zu%8u%4d %s\n", len, qd.d_name, (size_t)s.st_size, (uint32_t)htonl(qd.d_datalen), qd.d_type, tbuff);
          }
        }
        ie++;
      }
    }
    close(fd);
  } else {
    usage();
  }
  return 0;
}
