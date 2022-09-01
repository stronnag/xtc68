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
#include <sys/types.h>
#include <dirent.h>

#ifdef NOINLINE
#define inline
#endif

#if defined(__unix__) || defined(__APPLE__)
#include <arpa/inet.h>
#else
#include <winsock.h>
#endif
#include <libgen.h>

typedef struct {
  size_t d_length;          /* file length */
  time_t d_mtime;
  uint32_t d_datasize;
  uint8_t d_type;
} qldirent_t;

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

int checkXTcc(char *filename, qldirent_t *qd) {
  uint8_t xtbuf[8];
  int fd;
  int res = -1;

  if ((fd = open(filename, O_RDONLY, 0666)) > 0) {
    res = 0;
    struct stat s;
    fstat(fd, &s);
    if(S_ISREG(s.st_mode)) {
      uint32_t len = 0;
      lseek(fd, -8, SEEK_END);
      read(fd, xtbuf, sizeof(xtbuf));
      if(memcmp(xtbuf, "XTcc", 4) == 0) {
        len = htonl(*(uint32_t*)(xtbuf+4));
      }
      memset(qd, 0, sizeof(qldirent_t));
      if(len > 0) {
        qd->d_type = 1;
      }
      qd->d_datasize = len;
      qd->d_length = s.st_size;
      qd->d_mtime = s.st_mtime;
    } else {
      return -1;
    }
    close(fd);
  }
  return res;
}

void read_ql_dir(char*dname) {
  DIR *dirp;
  qldirent_t qd;
  struct dirent *dp;
  if (!(dirp = opendir(dname))) {
    fprintf(stderr, "dirp fail\n");
    return;
  }
  char buf[PATH_MAX];
  char *ptr = stpcpy(buf, dname);
  *ptr++ = '/';
  while ((dp = readdir(dirp))) {
    if (0 == strcmp(dp->d_name, ".") || 0 == strcmp(dp->d_name, "..") ||
        0 == strcmp(dp->d_name, ".-UQLX-"))
      continue;
#ifndef WIN32
    if(dp->d_type != DT_REG) {
      continue;
    }
#endif
    strcpy(ptr, dp->d_name);
    if(checkXTcc(buf, &qd) == 0) {
      struct tm *tm;
      tm = localtime(&qd.d_mtime);
      char tbuff[64];
      strftime(tbuff, sizeof(tbuff), "%Y-%m-%d %H:%M:%S", tm);
      printf("%-36.36s%9zu%8u%4d %s\n", dp->d_name, qd.d_length, qd.d_datasize, qd.d_type, tbuff);
    }
  }
}

int main(int ac, char **av) {
  if(ac > 1) {
    char *dname = av[1];
    read_ql_dir(dname);
  } else {
    usage();
  }
  return 0;
}
