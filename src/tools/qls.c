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
#define __USE_GNU
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <fcntl.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#if defined(__unix__) || defined(__APPLE__)
#include <arpa/inet.h>
#include <fnmatch.h>
#else
#include <winsock.h>
#include <windows.h>
#include <shlwapi.h>
#endif
#include <libgen.h>

typedef enum {
  QLS_FLAGS_NONE = 0,
  QLS_EXEC_ONLY = 1,
  QLS_NOEXEC_ONLY = 2,
  QLS_RECURSE = 4
} qls_flags_t;

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

static int checkXTcc(char *filename, qldirent_t *qd) {
  uint8_t xtbuf[8];
  int fd;
  int res = -1;

  if ((fd = open(filename, O_RDONLY, 0666)) > 0) {
    res = 0;
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
    close(fd);
  }
  return res;
}

static int read_ql_file(char *name, qls_flags_t flags) {
  int res = -1;
  qldirent_t qd = {0};
  struct stat s = {0};
  if(stat(name, &s) == 0) {
    if(S_ISREG(s.st_mode)) {
      res=checkXTcc(name, &qd);
      if(res == 0) {
        struct tm *tm;
        tm = localtime(&s.st_mtime);
        char tbuff[64];
        strftime(tbuff, sizeof(tbuff), "%Y-%m-%d %H:%M:%S", tm);
        if(((flags & (QLS_EXEC_ONLY|QLS_NOEXEC_ONLY)) == 0 ) ||
           (((flags & QLS_EXEC_ONLY) == QLS_EXEC_ONLY) && (qd.d_type == 1)) ||
           (((flags & QLS_NOEXEC_ONLY) == QLS_NOEXEC_ONLY) && (qd.d_type != 1))) {
          printf("%-36.36s%9zu%8u%4d %s\n", basename(name), (size_t)s.st_size, qd.d_datasize, qd.d_type, tbuff);
        }
      }
    } else if (S_ISDIR(s.st_mode)) {
      res = 1;
    }
  }
  return res;
}

static void read_ql_dir(char *dname, char*pattn, qls_flags_t flags) {
  DIR *dirp;
  struct dirent *dp;

  if (!(dirp = opendir(dname))) {
    fprintf(stderr, "dirp fail\n");
    return;
  }
  char*  buf = calloc(1,PATH_MAX);
  char *ptr = stpcpy(buf, dname);
  if(*(ptr-1) == '/') {
    *(ptr-1) = ':';
  } else {
    *ptr++ = ':';
  }
  printf("\n%s\n", buf);
  *(ptr-1) = '/';

  while ((dp = readdir(dirp))) {
    if (0 == strcmp(dp->d_name, ".") || 0 == strcmp(dp->d_name, "..") ||
        0 == strcmp(dp->d_name, ".-UQLX-"))
      continue;
    bool ismatched;

#ifndef WIN32
    ismatched = (pattn == NULL || (fnmatch(pattn, dp->d_name, FNM_CASEFOLD) == 0));
#else
    ismatched = (pattn == NULL || PathMatchSpec(dp->d_name, pattn));
#endif
    if(ismatched) {
      strcpy(ptr, dp->d_name);
      int rstat = read_ql_file(buf,flags);
      if(rstat  == 1) {
        if ((flags & QLS_RECURSE) == QLS_RECURSE) {
          read_ql_dir(buf, pattn, flags);
        } else if (flags == QLS_FLAGS_NONE) {
          printf("%s/\n", dp->d_name);
        }
      }
    }
  }
  free(buf);
}

static void read_ql(char*dname, qls_flags_t flags) {
  struct stat st;
  int res;
  res = stat(dname, &st);
  if(res == 0) {
    if(S_ISDIR(st.st_mode)) {
      read_ql_dir(dname, NULL, flags);
    } else {
        read_ql_file(dname, flags);
    }
  } else {
    char *xname = strdup(dname);
    char *pname = basename(xname);
    char *yname = dirname(xname);
    read_ql_dir(yname, pname, flags);
    free(xname);
  }
}

static void usage(void) {
  fputs(" qcp [-x|-X] [-R] inspec\n", stderr);
  exit(0);
}

int main(int argc, char **argv) {
  int c;
  qls_flags_t flags = QLS_FLAGS_NONE;
  while ((c = getopt(argc, argv, "xXRh?")) != EOF) {
    switch (c) {
    case 'x':
      flags |= QLS_EXEC_ONLY;
      break;
    case 'X':
      flags |= QLS_NOEXEC_ONLY;
      break;
    case 'R':
      flags |= QLS_RECURSE;
      break;
    case 'h':
    case 'q':
    default:
      usage();
      break;
    }
  }

  int nargs = argc-optind;
  if(nargs > 0) {
    for(int i = 0; i < nargs; i++)  {
      read_ql(argv[optind+i], flags);
    }
  } else {
    read_ql_dir("./", NULL, 0);
  }
  return 0;
}
