
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
#define _GNU_SOURCE
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>
#include <libgen.h>
#include <alloca.h>
#if defined(__unix__) || defined(__APPLE__)
#include <arpa/inet.h>
#else
#include <winsock.h>
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
} qldirent_t;

typedef struct {
  union {
    char xtcc[4];
    uint32_t x;
  } x;
  uint32_t dlen;
} xtcc_t;


#ifdef WIN32
char *stpcpy(char *d, const char *s) {
  while ((*d++ = *s++)) /* NULL loop */
    ;
  return d - 1;
}
#endif

uint32_t checkXTcc(char *filename) {
  static xtcc_t xtcc = {{"XTcc"}, 0};
  uint32_t len;
  xtcc_t fdat={0};
  int fd;

  len = 0;
  if ((fd = open(filename, O_RDONLY, 0666)) > 0) {
    lseek(fd, -8, SEEK_END);
    read(fd, &fdat, sizeof(xtcc));
    close(fd);
    if (fdat.x.x == xtcc.x.x) {
      len = htonl(fdat.dlen);
    }
  }
  return len;
}

void usage(void) {
  fputs(" qcp [-x dataspace] infile [outfile]\n", stderr);
  exit(0);
}

void set_dataspace(char *fn, uint32_t dsize) {
  char *dname;
  char *fname;
  /** Note POSIX (libgen) *name() functions corrupt the input
   *  so it is necessary to (a) save it and (b) do the following in order
  **/

  char *xfn = strdup(fn);
  fname = basename(xfn);
  dname = dirname(xfn);
  char *uqlxdir = malloc(strlen(dname)+16);
  qldirent_t* qd = malloc(sizeof(qldirent_t));

  char *ptr = stpcpy(uqlxdir, dname);
  *ptr++ = '/';
  strcpy(ptr, ".-UQLX-");
  int nlen = strlen(fname);
  int fd;

  if ((fd = open(uqlxdir, O_RDWR, 0666)) > -1) {
    while (read(fd, qd, sizeof(qldirent_t)) == sizeof(qldirent_t)) {
      if (nlen == htons(qd->d_szname) && strncasecmp(qd->d_name, fname, nlen) == 0) {
        lseek(fd, -1 * sizeof(qd), SEEK_CUR);
        break;
      }
    }
  } else {
    fd = open(uqlxdir, O_CREAT | O_WRONLY, 0666);
  }

  if (fd >= 0) {
    struct stat s;
    memset(qd, 0, sizeof(qldirent_t));
    qd->d_szname = htons(nlen);
    memcpy(qd->d_name, fname, nlen);
    qd->d_type = 1;
    qd->d_length = (stat(fn, &s) == 0) ? htonl(s.st_size) : 1;
    qd->d_datalen = htonl(dsize);
    write(fd, qd, sizeof(qldirent_t));
    close(fd);
  }
  free(qd);
  free(uqlxdir);
  free(xfn);
}


void copy_file(char *infile, char *outfile, uint32_t dsize) {
  char *outf = outfile;
  struct stat s;
  if(0 == stat(outfile, &s) && S_ISDIR(s.st_mode)) {
    char *ifn = strdup(infile);
    char *ibase = basename(ifn);
    outf = alloca(strlen(outfile)+1+strlen(ibase)+1);
    char *ptr = stpcpy(outf, outfile);
    *ptr++ = '/';
    strcpy(ptr, ibase);
    free(ifn);
  }

  int fd, fo, n;
  if ((fd = open(infile, O_RDONLY, 0)) >= 0) {
    if ((fo = open(outf, O_CREAT | O_WRONLY | O_TRUNC, 0666)) >= 0) {
      char *buf = malloc(4096);
      while ((n = read(fd, buf, sizeof(buf))) > 0) {
        write(fo, buf, n);
      }
      close(fo);
      free(buf);
    }
    close(fd);
  }
  if(dsize != 0) {
    set_dataspace(outf, dsize);
  }
}

void copy_set(char *infile, char *outfile, uint32_t dsize) {
  if(outfile == NULL) {
    set_dataspace(infile, dsize);
  } else {
    copy_file(infile, outfile, dsize);
  }
}

int main(int ac, char **av) {
  uint32_t dspace = 0;
  int c;

  while ((c = getopt(ac, av, "x:h?")) != EOF) {
    switch (c) {
    case 'x':
      dspace = strtol(optarg, NULL, 0);
      break;
    case 'h':
    case 'q':
    default:
      usage();
      break;
    }
  }

  int nsrc = ac-optind;
  char *outf;

  if (nsrc > 0) {
    if(nsrc == 1) {
      outf=NULL;
    } else {
      outf = av[ac-1];
      if(nsrc > 1) {
        struct stat st;
        int ns = stat(outf, &st);
        bool isdir = ((ns == 0) && (S_ISDIR(st.st_mode) != 0));
        nsrc--;
        if(!isdir && nsrc != 1) {
          fprintf(stderr,"output must be directory for multiple files\n");
          exit(127);
        }
      }
    }
    for(int i = 0; i < nsrc; i++) {
      uint32_t dsize = checkXTcc(av[optind+i]);
      if(dsize == 0) {
        dsize = dspace;
      }

      copy_set(av[optind+i], outf, dsize);
      if(dsize ==  0) {
        fprintf(stderr,"no data for %s\n", av[optind+i]);
      }
    }
  } else {
    puts("no args");
  }
  return 0;
}
