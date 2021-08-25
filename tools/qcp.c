
/*-------------------------------------------------------------------------.
| program to set uqlx data sizes for QDOS executables from	           |
| Unix. Features automatic recognition of xtc68 data space. If you         |
| give an outfile/path, the infile is copied.			           |
| 								           |
| Install somewhere on your path (i.e. /usr/local/bin)                     |
| 								           |
| qcp [-x data_space] infile [outfile]				           |
| 								           |
| You should give one of -x data or outfile.			           |
| 								           |
| $ qcp -x 1776 /QL/exe/gs #(set gs data size)                             |
| $ qcp	~/develop/qdos/gs262/gs /QL/exe #(copy and set data size for xtc68 |
|                                       # image)                           |
| 								           |
| n.b. symbolic links are very useful to reduce unix file to meet QDOS     |
|      file name restrictions                                              |
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

#ifdef NEED_STPCPY
char *stpcpy(char *d, const char *s) {
  while (*d++ = *s++) /* NULL loop */
    ;
  return d - 1;
}
#endif

uint32_t CheckXTcc(char *filename) {
  uint32_t len;

  static struct {
    union {
      char xtcc[4];
      uint32_t x;
    } x;
    uint32_t dlen;
  } fdat, xtcc = {{"XTcc"}, 0};
  int fd;

  len = 0;
  if ((fd = open(filename, O_RDONLY, 0666)) > 0) {
    lseek(fd, -8, SEEK_END);
    read(fd, &fdat, sizeof(xtcc));
    close(fd);
    if (fdat.x.x == xtcc.x.x) {
      len = swaplong(fdat.dlen);
    }
  }
  return len;
}

void usage(void) {
  fputs(" qcp [-x dataspace] infile [outfile]\n", stderr);
  exit(0);
}

int main(int ac, char **av) {
  uint32_t one = 1;
  uint32_t dspac = 0;
  int c;
  char *inf = NULL, *ouf = NULL;
  char onam[PATH_MAX], secret[PATH_MAX];

  is_big_endian = 1 - *(char *)&one;

  while ((c = getopt(ac, av, "x:h?")) != EOF) {
    switch (c) {
    case 'x':
      dspac = strtol(optarg, NULL, 0);
      break;
    case 'h':
    case 'q':
    default:
      usage();
      break;
    }
  }

  for (; optind < ac; optind++) {
    if (!inf) {
      inf = *(av + optind);
    } else if (!ouf) {
      ouf = *(av + optind);
    } else {
      usage();
    }
  }

  if (inf) {
    char *p, *q;
    int fd;
    QLDIR_t qd;
    short nlen;
    struct stat s;

    if (dspac == 0) {
      dspac = CheckXTcc(inf);
    }

    if (dspac) {
      if (ouf) {
        stat(ouf, &s);
        if (S_ISDIR(s.st_mode)) {
          p = stpcpy(onam, ouf);
          if (*(p - 1) != '/') {
            *p++ = '/';
          }
          if ((q = strrchr(inf, '/'))) {
            q++;
          } else {
            q = inf;
          }
          strcpy(p, q);
        } else {
          strcpy(onam, ouf);
        }
      } else {
        strcpy(onam, inf);
      }

      p = strrchr(onam, '/');
      if (p) {
        int n;

        p++;
        n = p - onam;
        strncpy(secret, onam, n);
        q = p;
        p = secret + n;
      } else {
        p = secret;
        q = onam;
      }

      strcpy(p, ".-UQLX-");
      nlen = strlen(q);

      if ((fd = open(secret, O_RDWR, 0666)) > -1) {
        while (read(fd, &qd, sizeof(qd)) == sizeof(qd)) {
          if (nlen == swapword(qd.d_szname) &&
              strncasecmp(qd.d_name, q, nlen) == 0) {
            lseek(fd, -1 * sizeof(qd), SEEK_CUR);
            break;
          }
        }
      } else {
        fd = open(secret, O_CREAT | O_WRONLY, 0666);
      }
      if (fd >= 0) {
        qd.d_szname = swapword(nlen);
        memcpy(qd.d_name, q, nlen);
        *(qd.d_name + nlen) = '\0'; // harmless, even if len==36
        qd.d_type = 1;
        qd.d_access = 0;
        qd.d_update = 0;
        qd.d_length = (stat(onam, &s) == 0) ? swaplong(s.st_size) : 1;
        qd.d_datalen = swaplong(dspac);
        write(fd, &qd, sizeof(qd));
        close(fd);
        if (ouf) {
          int fo, n;
          if ((fd = open(inf, O_RDONLY, 0)) >= 0) {
            if ((fo = open(onam, O_CREAT | O_WRONLY | O_TRUNC, 0666)) >= 0) {
              char buf[1024];
              while ((n = read(fd, buf, sizeof(buf))) > 0) {
                write(fo, buf, n);
              }
              close(fo);
            }
            close(fd);
          }
        }
      }
    } else
      fputs("Data space required\n", stderr);
  } else
    usage();

  return 0;
}
