
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

#include <libgen.h>

typedef struct {
  size_t d_length;          /* file length */
  time_t d_mtime;
  uint32_t d_datasize;
  uint8_t d_type;
} qldirent_t;

int checkXTcc(char *filename, qldirent_t *qd) {
  uint8_t xtbuf[8];
  int fd;
  int res = -1;
  if ((fd = open(filename, O_RDONLY, 0666)) > 0) {
    res = 0;
    uint32_t len = 0;
    lseek(fd, -8, SEEK_END);
    read(fd, xtbuf, sizeof(xtbuf));
    if(memcmp(xtbuf, "XTcc", 4) == 0) {
      len = ntohl(*(uint32_t*)(xtbuf+4));
    }
    struct stat s;
    fstat(fd, &s);

    memset(qd, 0, sizeof(qldirent_t));
    if(len > 0) {
      qd->d_type = 1;
    }
    qd->d_datasize = len;
    qd->d_length = s.st_size;
    qd->d_mtime = s.st_mtime;
    close(fd);
  } else {
    fprintf(stderr, "open %s:", filename);
    perror(" ");
  }
  return res;
}


#ifdef WIN32
char *stpcpy(char *d, const char *s) {
  while ((*d++ = *s++)) /* NULL loop */
    ;
  return d - 1;
}
#endif

void usage(void) {
  fputs(" qcp [-x dataspace] infile [outfile]\n", stderr);
  exit(0);
}

void set_dataspace(char *fn, uint32_t dsize) {
  int fd;
  uint8_t xtbuf[4];
  if ((fd = open(fn, O_RDWR, 0)) >= 0) {
    lseek(fd, -8, SEEK_END);
    read(fd, xtbuf, 4);
    if(memcmp(xtbuf, "XTcc", 4) != 0) {
      lseek(fd, 0, SEEK_END);
      write(fd,"XTcc", 4);
    }
    uint32_t hlen = htonl(dsize);
    write(fd, &hlen, 4);
    close(fd);
  }
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
      qldirent_t qd;
      if(checkXTcc(av[optind+i], &qd) == 0) {
        if(qd.d_datasize == 0) {
          qd.d_datasize = dspace;
        }
        copy_set(av[optind+i], outf, qd.d_datasize);
        if(qd.d_datasize == 0) {
          fprintf(stderr,"no data for %s\n", av[optind+i]);
        }
      }
    }
  } else {
    puts("no args");
  }
  return 0;
}
