/********************************************************************
 *                              ld_c                                *
 *                              ~~~~                                *
 *                                                                  *
 *      Linker for use with the QDOS C68 system.                    *
 *                                                                  *
 *      Based on a linker for the Atari ST posted to the Public     *
 *      Domain via Usenet.                                          *
 *                                                                  *
 *******************************************************************/

/*==================================================================
                        DEVELOPMENT HISTORY
                        ~~~~~~~~~~~~~~~~~~~
Dec 89              QDOS port done by Jeremy Allison
20/07/91  1.12  DJW Program version message not output if listing to
                    file unless verbose flag set
20/07/92  1.13  DJW Changed -bufp and -bufl to assume any value less
                    than 1000 is a size in K (used to be less than 250)
02/12/92  1.14  DJW Caused more error messages to give the FILE and
                    position on which a problem occurred
10/01/93  1.15  DJW Suppressed progress messages to screen unless the
                    -v flag is specified.
01/06/93  1.16  DJW Minor alterations to compile OK with new ANSI
                    version of c68
10/03/94  1.17  DJW Removed some '' keywords that were applied
                    to variables used with & operator.  This had been
                    missed by earlier releases of C68.
06/11/94  1.18  DJW Added new error message for table size exceeded,
                    and changed source accordingly.
16/02/96  1.19  DJW If a library file is not found on any of the library
                    search paths, check current directory as well.
24/03/96  1.20  DJW Incorporated Jonathan Hudson's XTC68 changes.
10/05/98  1.21  DJW Changed minimum dataspace to be 100 rather than 50
28/05/98  1.22  DJW Changed directory to be 'LIB' rather than 'lib'
===================================================================*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <stdarg.h>
#include <inttypes.h>
#include <libgen.h>
#include <limits.h>

#ifdef XTC68
#ifdef DOS_LIKE
#include <io.h>
#include <malloc.h>
#define _version ldversion
#define movmem(a, b, c) memcpy(b, a, c);
#endif
extern char *get_binary_path(void);
#endif

#define XSTR(s) STR(s)
#define STR(s) #s

char _prog_name[] = "ld";
char _version[] = "v1.22";
char _copyright[] = " QL 68000 SROFF Linker\n";

#ifdef __GNUC__
#include <unistd.h>
#include <sys/stat.h>
#ifndef O_BINARY
#define O_BINARY 0
#endif
#define movmem(a, b, c) memcpy(b, a, c);
#define stricmp strcasecmp
#define strnicmp strncasecmp
#define DBG(p1)
#endif /* __linux__ */

#ifdef VMS
#define movmem(a, b, c) memcpy(b, a, c);
extern char *strdup(char *);
#endif /* VMS */

#ifdef QDOS
#include <unistd.h>
#include <qdos.h>

void consetup_title();
void (*_consetup)() = consetup_title;
long (*_writetrans)() = NULL;
int (*_Open)(const char *, int, ...) = qopen;

long _stack = 4 * 1024; /* 4K stack */
long _stackmargin = 1024L;

#include <debug.h>
#endif /* QDOS */

#define FMSIZE 64
#define MAX_LEN 40 /* Could be 32 */
#define MAX_PDEF 1500
#define MAX_NDEF 30
#define BLEN 1024
#define XMAX 10
#define NUM_SPATHS 16

#define MIN_DATASPACE 100

char currentname[50];

#define sgn(x) ((x) < 0 ? -1 : ((x) == 0 ? 0 : 1))

typedef enum o_direct { data, source, comment, org, section, offset, xdef, xref, define, common, end, eofsy } O___DIRECT;

typedef char ALFA[MAX_LEN];

/* Operator to be applied to an XDEF relocation */
typedef struct oper {
  short id;
  char op;
} OPER;

/* Program symbol */
typedef struct symbol {
  short length;
  O___DIRECT directive;
  char string[81];
  intptr_t longword;
  short id;
  char trunc_rule;
  unsigned char data_byte;
  short n_xref;
  OPER xref_oper[XMAX];
} SYMBOL;

/* Module list */
typedef struct mod_item {
  struct mod_item *mod_next;
  char mod_name[2];
} MOD_ITEM;

/* Section, TEXT, DATA or BSS */
typedef struct section {
  struct section *sec_next;
  short sec_id; /* -1 = TEXT, -2 = DATA, -3 = BSS */
  char *sec_start;
  long sec_length;
  long sec_oldlen;
  intptr_t sec_fxref;
  intptr_t sec_xptr;
  MOD_ITEM *sec_module; /* List of modules contributing to this section */
  char sec_name[MAX_LEN];
} SECTION;

/* XDEF symbol */
typedef struct xsym {
  struct xsym *xsy_next;
  SECTION *xsy_sect;
  MOD_ITEM *xsy_mod;
  long xsy_value;
  short xsy_defd;
  struct xsym *xsy_left;
  struct xsym *xsy_right;
  char xsy_name[MAX_LEN];
} XSYMBOL;

typedef struct xoper {
  char xop_oper;
  char xop_optyp;
  union {
    SECTION *xop_sec;
    XSYMBOL *xop_sym;
  } xop_ptr;
} XOPER;

typedef struct xref {
  struct xref *xref_next;
  long xref_pos;
  long xref_abs;
  short xref_trunc;
  short xref_ops;
  struct xoper xref_oper[XMAX];
} XREF;

#define _PROTOTYPE(params) params

void app_xsy _PROTOTYPE((XSYMBOL **, XSYMBOL *));
XSYMBOL *src_xsy _PROTOTYPE((XSYMBOL *, char *));
void app_sec _PROTOTYPE((SECTION **, SECTION *));
SECTION *src_sec _PROTOTYPE((SECTION *, char *));
int read_b _PROTOTYPE((void));
void printsy _PROTOTYPE((void));
void nxsy _PROTOTYPE((void));
void move_up _PROTOTYPE((SECTION *));
SECTION *def_section _PROTOTYPE((char *));
long calc_xsy _PROTOTYPE((XSYMBOL *));

SYMBOL sy;
char module_name[80];
ALFA *pdef_name;
ALFA *ndef_name;
char lstng_name[FMSIZE];
char *prgm_name = "a_out";
char lstng_flag;
char symbol_flag;
char prgm_flag;
char unref_flag;
char *membot;
char *memtop;
char *memstart;
char *memend;
char *altstart;
char *code_ptr;
char *neustart;
char *altxref;
char *debug_start;
char *debug_end;

unsigned char *module_buffer;
unsigned char *module_end;
unsigned char *module_ptr;
unsigned char *module_top;
unsigned char *module_max;
SECTION *curr_sec;
SECTION *sec_liste;
SECTION *moved_sec;
SECTION **sec_lptr = &sec_liste;
MOD_ITEM *mod_liste;
MOD_ITEM *curr_mod;
XSYMBOL *xsy_liste;
XSYMBOL *xsy_ll;
XREF *xref_liste;
FILE *list_file;
int undefd_sym;
int double_sym;
int range_err;
int verbose_flag = 0;
unsigned char inp_buf[BLEN];
unsigned char *buf_ptr;
unsigned char *buf_end;
int inp_hnd;
long mem_size;
long buf_size;

char *errmsg[] = {"Out of memory\nuse % option (-% with cc) to increase memory allocation",
                  "Program memory too small\nuse -bufp option to increase size",
                  "Error in binary file",
                  "Too many operands in XREF",
                  "Illegal section id $%04.4x (%s)",
                  "Illegal symbol id",
                  "ORG encountered",
                  "Should not occur",
                  "Word or longword at odd address",
                  "Library module too big\nuse -bufl option to increase library space",
                  "Cannot write file correctly",
                  "Table overflow (%s)",
                  "link file error"};

#ifdef __GNUC__
char *strupr(char *s) {
  char *r = s;

  while (*s) {
    *s = toupper(*s);
    s++;
  }
  return r;
}
#endif /* linux */

void halt(int n, ...) {
  va_list ap;
  va_start(ap, n);
  (void)fprintf(stderr, "%s: Error %2d\n", _prog_name, n);
  (void)vfprintf(stderr, errmsg[n], ap);
  (void)fprintf(stderr, "\n");
  switch (n) {
  case 2:
  case 3:
  case 4:
  case 5:
  case 6:
  case 8:
  case 9:
#ifndef QDOS
  {
    long npos;
    npos = (long)(lseek(inp_hnd, 0, SEEK_CUR) - (buf_end - buf_ptr));

    (void)fprintf(stderr, "(file='%s', position = %ld (%lx))\n", currentname, npos, npos);
  }
#else
    (void)fprintf(stderr, "(file='%s', position = %ld)\n", currentname, (long)(tell(inp_hnd) - (buf_end - buf_ptr)));
#endif /* QDOS */
  }
  va_end(ap);
  exit(-1);
}

/*
 * Print out statistics on the link
 */
void statistic(void) {
  SECTION *s;

  (void)fprintf(list_file, "\n---------------------------\n");
  (void)fprintf(list_file, "SECTION      START   LENGTH\n");
  (void)fprintf(list_file, "---------------------------\n");
  for (s = sec_liste; s != NULL; s = s->sec_next)
    (void)fprintf(list_file, "%-9s %8" PRIxPTR " %8lx\n", s->sec_name, s->sec_start - membot, s->sec_length);
  fprintf(list_file, "---------------------------\n");
}

/*
 * Append a module onto a linked list
 */
MOD_ITEM *app_mod(MOD_ITEM **mod_liste, char *name) {
  MOD_ITEM *new;

  new = (MOD_ITEM *)malloc(sizeof(MOD_ITEM) + strlen(name));
  if (new == NULL)
    halt(0);
  strcpy(new->mod_name, name);
  new->mod_next = *mod_liste;
  *mod_liste = new;
  return (new);
}

/*
 * Append a symbol onto a binary tree (for speed) and a
 * linked list (for alphabetical reasons) of them
 */
void app_xsy(XSYMBOL **xsy_plist, XSYMBOL *xsy_new) {
  XSYMBOL **xpp;

  while (*xsy_plist) {
    while (stricmp(xsy_new->xsy_name, (*xsy_plist)->xsy_name) < 0)
      if (!*(xsy_plist = &(*xsy_plist)->xsy_left))
        break;
    if (!*xsy_plist)
      break;
    while (stricmp(xsy_new->xsy_name, (*xsy_plist)->xsy_name) >= 0)
      if (!*(xsy_plist = &(*xsy_plist)->xsy_right))
        break;
  }
  *xsy_plist = xsy_new;
  xsy_new->xsy_left = xsy_new->xsy_right = NULL;

  /* Now put it on the linked list */
  for (xpp = &xsy_ll; *xpp; xpp = &(*xpp)->xsy_next)
    if (stricmp(xsy_new->xsy_name, (*xpp)->xsy_name) < 0)
      break;
  xsy_new->xsy_next = *xpp;
  *xpp = xsy_new;
}

/*
 * Find a symbol with a particular name (NULL if not found)
 */
XSYMBOL *src_xsy(XSYMBOL *xsy_list, char *name) {
  int c = 1;

  while (xsy_list && c) {
    while ((c = stricmp(name, xsy_list->xsy_name)) < 0)
      if (!(xsy_list = xsy_list->xsy_left))
        break;
    if (!xsy_list || !c)
      break;
    while ((c = stricmp(name, xsy_list->xsy_name)) > 0)
      if (!(xsy_list = xsy_list->xsy_right))
        break;
  }
  return xsy_list;
}

/*
 * Calculate the value of an external symbol
 */
long calc_xsy(XSYMBOL *s) {
  long value;

  value = s->xsy_value;
  if (s->xsy_sect != NULL)
    value += s->xsy_sect->sec_start - membot;
  return (value);
}

void print_error(char *string) {
  if (errno < 0) {
    fprintf(stderr, "%s: Unknown error %d\n", string, errno);
#ifdef QDOS
    poserr("poserr:");
#endif
  } else {
    perror(string);
  }
}

#if 0
/*
 * Not needed for QL
 */
void debug_table(x)
XSYMBOL  *x;
{
    char  *p;
    short i;
   if (x!=NULL)
   {
      if (code_ptr+14>=memtop) halt(1);
      debug_table(x->xsy_left);
      p=x->xsy_name;
      for (i=8;i--;) { *code_ptr++=*p; if (*p) p++; }
      if (x->xsy_sect!=NULL) *((short*)code_ptr)=0xA200;
      else *((short*)code_ptr)=0xA000;
      code_ptr+=2;
      *((long*)code_ptr)=calc_xsy(x);
      code_ptr+=4;
      debug_table(x->xsy_right);
   }
}
#endif

/*
 * List out the symbols
 */
void list_xsy(XSYMBOL *xsy_liste, int u_flag) {
  for (; xsy_liste; xsy_liste = xsy_liste->xsy_next) {
    if (u_flag) {
      if (!(xsy_liste->xsy_defd & 1)) {
        fprintf(list_file, "Undefined Symbol: '%s'\n", xsy_liste->xsy_name);
        undefd_sym++;
      }
    } else {
      if (xsy_liste->xsy_defd & 1) {
        fprintf(list_file, "%-20s %08lX%c", xsy_liste->xsy_name, calc_xsy(xsy_liste), xsy_liste->xsy_defd & 2 ? ' ' : '*');
        if (xsy_liste->xsy_sect != NULL)
          fprintf(list_file, " %15s", xsy_liste->xsy_sect->sec_name);
        else
          fprintf(list_file, " %20s", " ");
        if (xsy_liste->xsy_mod != NULL)
          fprintf(list_file, " %15s", xsy_liste->xsy_mod->mod_name);
        fprintf(list_file, "\n");
      } else
        fprintf(list_file, "%-20s undefined\n", xsy_liste->xsy_name);
    }
  }
}

/*
 * Append a section into a linked list in order (-1 -2 -3 ->NULL)
 */
void app_sec(SECTION **sec_liste, SECTION *sec_neu) {
  if (*sec_liste == NULL) {
    *sec_liste = sec_neu;
    sec_neu->sec_next = NULL;
  } else if (sec_neu->sec_id <= (*sec_liste)->sec_id)
    app_sec(&(*sec_liste)->sec_next, sec_neu);
  else {
    sec_neu->sec_next = *sec_liste;
    *sec_liste = sec_neu;
  }
}

/*
 * Find a section with a particular name
 */
SECTION *src_sec(SECTION *sec_liste, char *name) {
  while (sec_liste != NULL) {
    if (stricmp(sec_liste->sec_name, name) == 0)
      return (sec_liste);
    sec_liste = sec_liste->sec_next;
  }
  return (NULL);
}

/*
 * Get a character from a buffer or from disk
 */
int read_b(void) {
  if (buf_end - inp_buf < BLEN)
    return (-1);
  buf_end = inp_buf + read(inp_hnd, inp_buf, BLEN);
  buf_ptr = inp_buf;
  return (buf_ptr < buf_end ? (int)*buf_ptr++ : -1);
}

int get_drct(void) { return (buf_ptr < buf_end ? (int)*buf_ptr++ : read_b()); }

int get_byte(void) { return (module_ptr < module_end ? (int)*module_ptr++ : get_drct()); }

long get_long(void) {
  long lword;
  lword = (get_byte() << 24) + (get_byte() << 16) + (get_byte() << 8) + get_byte();
  return lword;
}

short get_short(void) {
  short sword;
  sword = (get_byte() << 8) + get_byte();
  return sword;
}

void nxsy(void) {
  int c;

  if ((c = get_byte()) == 0xFB) {
    char *p;
    int i, ch;
    sy.id = 0;
    switch (ch = get_byte()) {
    case -1:
      sy.directive = eofsy;
      sy.length = 0;
      break;
    case 0x01:
      sy.directive = source;
      sy.length = 0;
      p = sy.string;
      for (i = get_byte(); i--;)
        *p++ = get_byte();
      *p = '\0';
      break;
    case 0x02:
      sy.directive = comment;
      sy.length = 0;
      p = sy.string;
      for (i = get_byte(); i--;)
        *p++ = get_byte();
      *p = '\0';
      break;
    case 0x03:
      sy.directive = org;
      sy.length = 0;
      sy.longword = get_long();
      break;
    case 0x04:
      sy.directive = section;
      sy.length = 0;
      sy.id = get_short();
      break;
    case 0x05:
      sy.directive = offset;
      sy.length = 0;
      sy.longword = get_long();
      break;
    case 0x06:
      sy.directive = xdef;
      sy.length = 0;
      p = sy.string;
      for (i = get_byte(); i--;)
        *p++ = get_byte();
      *p = '\0';
      sy.longword = get_long();
      sy.id = get_short();
      break;
    case 0x07:
      sy.directive = xref;
      sy.longword = get_long();
      sy.trunc_rule = get_byte();
      sy.length = sy.trunc_rule & 7;
      for (i = 0; (c = get_byte()) != 0xFB && i < XMAX; i++) {
        if (c != '+' && c != '-') {
          printf("Illegal XREF Operator : %c\n", c);
          halt(2);
        }
        sy.xref_oper[i].op = c;
        sy.xref_oper[i].id = get_short();
      }
      if (c != 0xFB)
        halt(3);
      sy.n_xref = i;
      break;
    case 0x10:
      sy.directive = define;
      sy.length = 0;
      sy.id = get_short();
      p = sy.string;
      for (i = get_byte(); i--;)
        *p++ = get_byte();
      *p = '\0';
      break;
    case 0x12:
      sy.directive = common;
      sy.length = 0;
      sy.id = get_short();
      break;
    case 0x13:
      sy.directive = end;
      sy.length = 0;
      break;
    case 0xFB:
      sy.directive = data;
      sy.length = 1;
      sy.data_byte = 0xFB;
      break;
    default:
      printf("Illegal Directive $%02x\n", ch);
      halt(2);
      break;
    }
  } else {
    if (c == -1) {
      sy.directive = eofsy;
      sy.length = 0;
    } else {
      sy.directive = data;
      sy.data_byte = c;
      sy.length = 1;
    }
  }
}

/*
 * Relocate a data section to the top of memory,
 * as a text section is next
 */
void move_up(SECTION *s) {
  if (memend != memtop)
    halt(-1);
  moved_sec = s;
  if (s != NULL) {
    if (s->sec_start != NULL) {
      altstart = memstart;
      memstart = s->sec_start;
      memend = memtop - (altstart - memstart);
      if (altstart > memstart)
        movmem(memstart, memend, altstart - memstart);
    } else {
      moved_sec = NULL;
    }
  }
}

/*
 * Write out a comment
 */
void comment_dir(void) {
  if (list_file != stdout || verbose_flag)
    fprintf(list_file, "COMMENT: %s\n", sy.string);
  nxsy();
}

/*
 * Define a XDEF symbol
 */
void xdef_dir(int body_flag) {
  XSYMBOL *s;

  if ((s = src_xsy(xsy_liste, sy.string)) == NULL) {
    if ((s = (XSYMBOL *)malloc(sizeof(XSYMBOL))) == NULL)
      halt(0);
    strupr(sy.string);
    strncpy(s->xsy_name, sy.string, MAX_LEN - 1);
    s->xsy_name[MAX_LEN - 1] = 0;
    s->xsy_defd = 0;
    s->xsy_mod = NULL;
    app_xsy(&xsy_liste, s);
  }
  if (s->xsy_defd & 1) {
    fprintf(list_file, "Double defined Symbol: %s\n", sy.string);
    double_sym++;
  } else {
    if (sy.id) {
      if (sy.id > 0)
        halt(4, sy.id, "Defining XDEF symbol");
      if (-sy.id > MAX_NDEF)
        halt(11, "XDEF symbols");
      if ((s->xsy_sect = src_sec(sec_liste, ndef_name[-sy.id])) == NULL) {
        if (body_flag)
          halt(2);
        s->xsy_sect = def_section(ndef_name[-sy.id]);
      }
    } else
      s->xsy_sect = NULL;
    s->xsy_value = sy.longword;
    if (s->xsy_sect != NULL)
      s->xsy_value += s->xsy_sect->sec_oldlen;
    s->xsy_defd |= 1;
    s->xsy_mod = curr_mod;
  }
  nxsy();
}

/*
 * Define a name ?
 */
void define_dir(void) {
#ifndef __clang__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-truncation"
#endif
  strupr(sy.string);
  if (sy.id > 0) {
    if (sy.id > MAX_PDEF)
      halt(11, "XDEF symbols");
    strncpy(pdef_name[sy.id], sy.string, MAX_LEN - 1);
    pdef_name[sy.id][MAX_LEN - 1] = 0;
  } else {
    if (-sy.id > MAX_NDEF)
      halt(5);
    strncpy(ndef_name[-sy.id], sy.string, MAX_LEN - 1);
    ndef_name[-sy.id][MAX_LEN - 1] = 0;
  }
#ifndef __clang__
#pragma GCC diagnostic pop
#endif
  nxsy();
}

/*
 * Define a section
 */
SECTION *def_section(char *name) {
  SECTION *sec;

  if (NULL == (sec = (SECTION *)malloc(sizeof(SECTION))))
    halt(0);
  strupr(name);
  strncpy(sec->sec_name, name, MAX_LEN - 1);
  sec->sec_name[MAX_LEN - 1] = 0;
  sec->sec_start = NULL;
  sec->sec_length = 0;
  sec->sec_oldlen = 0;
  sec->sec_id = -1;
  sec->sec_module = curr_mod;
  if (!stricmp(sec->sec_name, "DATA"))
    sec->sec_id = -2;
  if (!stricmp(sec->sec_name, "BSS"))
    sec->sec_id = -3;
  if (!stricmp(sec->sec_name, "UDATA"))
    sec->sec_id = -3;
  sec->sec_fxref = 0; /* jh was NULL */
  sec->sec_xptr = (intptr_t)&sec->sec_fxref;
  app_sec(sec_lptr, sec);
  return (sec);
}

/*
 * Deal with a new section
 */
void sec_com_dir(void) {
  if (sy.id >= 0)
    halt(4, sy.id, "New Section");
  if (-sy.id >= MAX_NDEF)
    halt(11, "Section Names");
  if (NULL == (curr_sec = src_sec(sec_liste, ndef_name[-sy.id]))) {
    curr_sec = def_section(ndef_name[-sy.id]);
    move_up(curr_sec->sec_next);
    curr_sec->sec_start = memstart;
  } else {
    move_up(curr_sec->sec_next);
    if (curr_sec->sec_start == NULL)
      curr_sec->sec_start = memstart;
  }
}

/*
 * Section directive
 */
void sect_dir(void) {
  sec_com_dir();
  code_ptr = neustart = memstart;
  nxsy();
}

/*
 * ORG directive (fail)
 */
void org_dir(void) {
  halt(6);
  nxsy();
}

/*
 * Common directive
 */
void comm_dir(void) {
  sec_com_dir();
  neustart = memstart;
  code_ptr = curr_sec->sec_start;
  nxsy();
}

/*
 * Data directive
 */
void data_dir(void) {
  if (code_ptr >= memend)
    halt(1);
  *code_ptr++ = sy.data_byte;
  nxsy();
}

/*
 * Offset directive
 */
void offset_dir(void) {
  DBG(("OFFSET_DIR", 0x11, "Enter"));
  if (code_ptr > neustart) {
    DBG(("OFFSET_DIR", 0x11, "neustart changed from %p to %p", neustart, code_ptr));
    neustart = code_ptr;
  }

  DBG(("OFFSET_DIR", 0x11, "memstart=%p, sy.longword=%p", memstart, sy.longword));
  code_ptr = memstart + sy.longword;
  DBG(("OFFSET_DIR", 0x11, "new value of 'code_ptr' = %p", code_ptr));

  if (code_ptr > neustart) {
    DBG(("OFFSET_DIR", 0x11, "neustart changed from %p to %p", neustart, code_ptr));
    neustart = code_ptr;
  }

  nxsy();
  DBG(("OFFSET_DIR", 0x11, "Exit"));
}

/*
 * XREF directive
 */
void xref_dir(void) {
  XREF *x;
  XSYMBOL *xsy;
  SECTION *sec;
  short i, xid;

  if ((x = (XREF *)malloc(sizeof(XREF) + (sy.n_xref - XMAX) * sizeof(XOPER))) == NULL)
    halt(0);
  if (curr_sec == NULL)
    halt(2);
  x->xref_pos = code_ptr - curr_sec->sec_start;
  x->xref_abs = sy.longword;
  x->xref_ops = sy.n_xref;
  x->xref_trunc = sy.trunc_rule;
  x->xref_next = NULL;
  for (i = 0; i < sy.n_xref; i++) {
    x->xref_oper[i].xop_oper = sy.xref_oper[i].op;
    xid = sy.xref_oper[i].id;
    switch ((int)(x->xref_oper[i].xop_optyp = sgn(xid))) {
    case -1:
      if (-xid > MAX_NDEF)
        halt(11, "XREF names");
      if ((sec = src_sec(sec_liste, ndef_name[-xid])) == NULL)
        sec = def_section(ndef_name[-xid]);
      x->xref_oper[i].xop_ptr.xop_sec = sec;
      if (x->xref_oper[i].xop_oper == '+')
        x->xref_abs += sec->sec_oldlen;
      else
        x->xref_abs -= sec->sec_oldlen;
      break;
    case 0:
      break;
    case 1:
      if ((xsy = src_xsy(xsy_liste, pdef_name[xid])) == NULL) {
        if ((xsy = (XSYMBOL *)malloc(sizeof(XSYMBOL))) == NULL)
          halt(0);
        strncpy(xsy->xsy_name, pdef_name[xid], MAX_LEN - 1);
        xsy->xsy_name[MAX_LEN - 1] = 0;
        xsy->xsy_defd = 0;
        xsy->xsy_mod = NULL;
        xsy->xsy_sect = NULL;
        xsy->xsy_value = 0;
        app_xsy(&xsy_liste, xsy);
      }
      xsy->xsy_defd |= 2;
      x->xref_oper[i].xop_ptr.xop_sym = xsy;
      break;
    }
  }
  *((XREF **)curr_sec->sec_xptr) = x;
  curr_sec->sec_xptr = (intptr_t)&x->xref_next;
  code_ptr += sy.trunc_rule & 7;
  if (code_ptr >= memend)
    halt(1);
  nxsy();
}

/*
 *  Deal with header commands
 */
void header_command(void) {
  int in_header_com = 1;

  while (in_header_com)
    switch (sy.directive) {
    case comment:
      comment_dir();
      break;
    case xdef:
      xdef_dir(0);
      break;
    case define:
      define_dir();
      break;
    default:
      in_header_com = 0;
      break;
    }
}

/*
 * Deal with section commands
 */
void sect_command(void) {
  switch (sy.directive) {
  case section:
    sect_dir();
    break;
  case org:
    org_dir();
    break;
  case common:
    comm_dir();
    break;
  default:
    halt(2);
    break;
  }
}

/*
 * Deal with body commands (after section)
 */
void body(void) {
  while (sy.directive == data || sy.directive == offset || sy.directive == xdef || sy.directive == xref ||
         sy.directive == define || sy.directive == comment) {
    switch (sy.directive) {
    case data:
      data_dir();
      break;
    case offset:
      offset_dir();
      break;
    case xdef:
      xdef_dir(1);
      break;
    case xref:
      xref_dir();
      break;
    case define:
      define_dir();
      break;
    case comment:
      comment_dir();
      break;
    default:
      halt(2);
      break;
    }
  }
}

/*
 * Deal with a header/body chunk
 */
void chunk(void) {
  SECTION *s;

  while (sy.directive == xdef || sy.directive == comment || sy.directive == define)
    header_command();
  if (sy.directive == section || sy.directive == org || sy.directive == common) {
    sect_command();
    body();
    if (((intptr_t)code_ptr & 1) && code_ptr >= neustart) {
      if (code_ptr >= memend)
        halt(1);
      *code_ptr++ = '\0';
    }
    if (code_ptr > neustart)
      neustart = code_ptr;
    curr_sec->sec_length += neustart - memstart;
    if (altstart != NULL) {
      if (altstart > memstart)
        movmem(memend, neustart, altstart - memstart);
      for (s = moved_sec; s != NULL; s = s->sec_next)
        if (s->sec_start != NULL)
          s->sec_start += neustart - memstart;
      memend = memtop;
      neustart += altstart - memstart;
      altstart = NULL;
    }
    memstart = neustart;
  }
}

/*
 * Deal with a new module
 */
void module(void) {
  SECTION *sec;
  short i;

  if (sy.directive != source)
    halt(2);
  curr_mod = app_mod(&mod_liste, sy.string);
  strcpy(module_name, sy.string);
  nxsy();
  while (sy.directive == xdef || sy.directive == comment || sy.directive == define || sy.directive == section ||
         sy.directive == org || sy.directive == common)
    chunk();
  if (sy.directive != end)
    halt(2);
  if (lstng_flag)
    fprintf(list_file, "%-12.12s:", module_name);
  i = 0;
  for (sec = sec_liste; sec != NULL; sec = sec->sec_next) {
    if (lstng_flag) {
      if (i++ > 3) {
        fprintf(list_file, "\n");
        i = 0;
      }
      fprintf(list_file, " %8.8s=%08lX", sec->sec_name, sec->sec_length - sec->sec_oldlen);
    }
    sec->sec_oldlen = sec->sec_length;
  }
  if (lstng_flag)
    fprintf(list_file, "\n");
  strcpy(module_name, "NO MODULE");
  nxsy();
}
void out_long(char *c, long l) {
  *c++ = l >> 24;
  *c++ = (l >> 16) & 0xff;
  *c++ = (l >> 8) & 0xff;
  *c = l & 0xff;
}
void out_short(char *c, short s) {
  *c++ = (s >> 8) & 0xff;
  *c = s & 0xff;
}
/* Calculate an xref value */

void calc_xref(XREF *x, char *c, char *modname) {
  short i;
  long value;

  value = x->xref_abs;
  c += x->xref_pos;
  /*   printf("XREF at %8X %8X\n",c-membot,value); */
  for (i = 0; i < x->xref_ops; i++) {

    switch ((int)x->xref_oper[i].xop_optyp) {
    case -1:
      if (x->xref_oper[i].xop_oper == '+')
        value += x->xref_oper[i].xop_ptr.xop_sec->sec_start - membot;
      else
        value -= x->xref_oper[i].xop_ptr.xop_sec->sec_start - membot;
      /* printf("%s/%x",x->xref_oper[i].xop_ptr.xop_sec->sec_name,
                 x->xref_oper[i].xop_ptr.xop_sec->sec_start); */
      break;
    case 0:
      if (x->xref_oper[i].xop_oper == '+')
        value += c - membot;
      else
        value -= c - membot;
      /* printf("&"); */
      break;
    case 1:
      if (x->xref_oper[i].xop_oper == '+')
        value += calc_xsy(x->xref_oper[i].xop_ptr.xop_sym);
      else
        value -= calc_xsy(x->xref_oper[i].xop_ptr.xop_sym);
      /* printf("%s"/%x",x->xref_oper[i].xop_ptr.xop_sym->xsy_name,
                 calc_xsy(x->xref_oper[i].xop_ptr.xop_sym)); */
      break;
    }
    /* printf("\n"); */
  }
  if (c < membot || c > memstart)
    halt(2);
  if (x->xref_trunc & 32)
    value -= c - membot;
  switch (x->xref_trunc & 7) {
  case 4:
    if (((intptr_t)c) & 1)
      halt(1);
    out_long(c, value);
    break;
  case 2:
    if (((intptr_t)c) & 1)
      halt(1);
    out_short(c, value);
    if (x->xref_trunc & 8) {
      if (value < -32768L || value > 32767L) {
        range_err++;
        printf("XREF.W-value out of range in module '%s'\n", modname);
      }
    } else {
      if (value > 65535L)

      {
        range_err++;
        printf("XREF.UW-value out of range in module '%s'\n", modname);
      }
    }
    break;
  case 1:
    *c = value;
    if (x->xref_trunc & 8) {
      if (value < -128L || value > 128L) {
        range_err++;
        printf("XREF.B-value out of range in module '%s'\n", modname);
      }
    } else {
      if (value > 255L) {
        range_err++;
        printf("XREF.UB-value out of range in module '%s'\n", modname);
      }
    }
    break;
  default:
    halt(2);
  }
  if (x->xref_trunc & 64) {
    if (altxref == NULL) {
      if (code_ptr >= memtop - 4)
        halt(1);
      altxref = c;
      out_long(code_ptr, (c - membot));
      code_ptr += 4;
    } else {
      while (c - altxref > 254) {
        altxref += 254;
        if (code_ptr >= memtop)
          halt(1);
        *code_ptr++ = '\001';
      }
      if (code_ptr >= memtop)
        halt(1);
      *code_ptr++ = c - altxref;
      altxref = c;
    }
  }
}

/* Deal with all the XREFS (create the relocation stream) */

void all_xrefs(SECTION *sec_liste) {
  XREF *x;

  while (sec_liste != NULL) {
    x = (XREF *)sec_liste->sec_fxref;
    while (x != NULL) {
      calc_xref(x, sec_liste->sec_start, sec_liste->sec_module->mod_name);
      x = x->xref_next;
    }
    sec_liste = sec_liste->sec_next;
  }
  if (altxref == NULL) {
    if (code_ptr >= memtop - 8)
      halt(1);
    *((long *)code_ptr) = 0;
    code_ptr += 4;
    *((long *)code_ptr) = 0;
    code_ptr += 4;
  } else {
    if (code_ptr >= memtop)
      halt(1);
    *code_ptr++ = '\0';
  }
}

/* Set up all needed arrays */
void init_mem(void) {
  pdef_name = (ALFA *)malloc(MAX_PDEF * sizeof(ALFA));
  ndef_name = (ALFA *)malloc(MAX_NDEF * sizeof(ALFA));
  membot = (char *)malloc(mem_size);
  module_buffer = (unsigned char *)malloc(buf_size);
  if (membot == NULL || module_buffer == NULL || ndef_name == NULL || pdef_name == NULL)
    halt(0);

  memtop = membot + mem_size;
  memstart = membot;
  memend = memtop;
  altstart = NULL;
  module_top = module_buffer + buf_size;
  module_max = module_ptr = module_end = module_buffer;
}
#if defined(XTC68)
int strfnd(const char *tofind, const char *tosearch) {
  char *p;
  p = strstr(tosearch, tofind);
  if (p == NULL) {
    return -1;
  } else {
    return (p - tosearch);
  }
}

void strmfe(char *out, const char *in, const char *ext) {
  int i = strfnd(ext, in);
  int extlen = strlen(ext), inlen = strlen(in);

  strcpy(out, in);
  if (i == -1 || (i + extlen != inlen))
    strcat(out, ext);
  return;
}
#endif /* XTC68 */

#if 0
void make_ext(make_name,name,ext)
 char  *make_name,*name,*ext;
{
   char  oldtext[FMSIZE];

   stcgfe(oldtext,name);
   if (!*oldtext) strmfe(make_name,name,ext);
   else           strcpy(make_name,name);
}
#endif

int test_module(void) {
  int c;
  int end_test = 0;
  int result = 0;
  short i;
  char string[80];
  char *p;
  XSYMBOL *s;

  module_end = module_ptr = module_buffer;

  do {
    c = get_drct();
    if (c < 0)
      halt(2);
    if (module_end + 128 > module_top)
      halt(9);
    *module_end++ = c;
    if (c == 0xFB) {
      c = get_drct();
      *module_end++ = c;
      switch (c) {
      case -1:
        halt(2);
        break;
      case 0xFB:
        break;
      case 0x01:
      case 0x02:
        i = get_drct();
        *module_end++ = i;
        while (i--)
          *module_end++ = get_drct();
        break;
      case 0x03:
      case 0x05:
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        break;
      case 0x12:
      case 0x04:
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        break;
      case 0x06:
        p = string;
        i = get_drct();
        *module_end++ = i;
        while (i--) {
          *p = get_drct();
          *module_end++ = *p++;
        }
        *p = '\0';
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        s = src_xsy(xsy_liste, string);
        if (s != NULL)
          if (!(s->xsy_defd & 1))
            end_test = result = 1;
        break;
      case 0x07:
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        while (1) {
          c = get_drct();
          *module_end++ = c;
          if (c == 0xFB || c == -1)
            break;
          *module_end++ = get_drct();
          *module_end++ = get_drct();
        }
        break;
      case 0x10:
        *module_end++ = get_drct();
        *module_end++ = get_drct();
        i = get_drct();
        *module_end++ = i;
        while (i--)
          *module_end++ = get_drct();
        break;
      case 0x13:
        end_test = 1;
        break;
      default:
        halt(2);
      }
    }
  } while (!end_test);
  if (module_end > module_max)
    module_max = module_end;
  return (result);
}

void link_file(char *name, int lib_mode, char **lib_paths, int file_no) {
  char fname[PATH_MAX];
  strcpy(currentname, name);

  if (lib_mode || (file_no == 0)) { /* Use library search paths for file */
    inp_hnd = -1;
    while (*lib_paths) {
      strcpy(fname, *lib_paths);
      strcat(fname, "/");
      strcat(fname, name);
      if ((inp_hnd = open(fname, O_RDONLY | O_BINARY, 0)) != -1)
        break;
      lib_paths++;
    }
    if (inp_hnd == -1)
      inp_hnd = open(name, O_RDONLY);
  } else {
    inp_hnd = open(name, O_RDONLY | O_BINARY, 0);
  }
  buf_ptr = buf_end = inp_buf + BLEN;
  strcpy(module_name, "*NO MODULE");
  if (inp_hnd < 0) {
    printf("Cannot open binary file: '%s'\n", name);
    exit(-1);
  }

  nxsy();
  while (sy.directive != eofsy) {
    if (lib_mode) {
      if (!test_module()) {
        module_end = module_ptr = module_buffer;
        nxsy();
      } else
        module();
    } else
      module();
  }
  close(inp_hnd);
}

void write_prog(void) {
#ifdef QDOS
  struct qdirect stat;
#else
  int dspace;
#endif
  int handle, bss_size;
  int n, h, psize = 0;
  SECTION *sec;
  char *start, *endcode;
  char c = 0;

#ifndef XTC68
  if ((handle = open(prgm_name, O_CREAT | O_WRONLY | O_BINARY | O_TRUNC)) < 0)
#else
  if ((handle = open(prgm_name, O_CREAT | O_WRONLY | O_BINARY | O_TRUNC, S_IRUSR | S_IWUSR)) == -1)
#endif /* XTC68 */
  {
    printf("Cannot open %s for writing\n", prgm_name);
    halt(10);
  }

  h = 1;
  sec = sec_liste;
  start = membot;

  while (sec->sec_id > -2 && sec->sec_next != NULL)
    sec = sec->sec_next;
  if (sec->sec_id == -2) {
    endcode = sec->sec_start;
    if (endcode - start) {
      n = psize = write(handle, start, (long)(endcode - start));
      if (n != endcode - start) {
        print_error("DATA section problem: ");
        halt(10);
      }
    }
    h = 3;
    start = endcode;
  }

  while (sec->sec_id > -3 && sec->sec_next != NULL)
    sec = sec->sec_next;
  if (sec->sec_id == -3) {
    endcode = sec->sec_start;
    {
      if (endcode - start) {
        psize += (n = write(handle, start, endcode - start));
        if (n != endcode - start) {
          printf("UDATA section problem\n");
          halt(10);
        }
      }
    }
    h = 5;
    start = endcode;
  }

  /* Rest in (h)-Section  */
  endcode = debug_start;
  bss_size = (endcode - start + 16) & ~15;
  if (h < 5) {
    if (endcode - start) {
      psize += (n = write(handle, start, endcode - start));
      if (n != endcode - start) {
        printf("UDATA/BSS section problem\n");
        halt(10);
      }
    }
  }

  /* Write relocation table */
  start = debug_start;
  endcode = code_ptr;
  if (endcode - start) {
    psize += (n = write(handle, start, endcode - start));
    if (n != endcode - start) {
      printf("Relocation Table problem\n");
      halt(10);
    }
  }
  if (psize & 1) { // Write an extra byte of zero to even up the program file
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    write(handle, &c, 1);
#pragma GCC diagnostic pop
  }
  /* Read the QDOS header information */
  bss_size -= (endcode - start);
#ifdef QDOS
  fs_headr(getchid(handle), -1L, &stat, sizeof(stat));
  /* Now write it out */
  stat.d_type = 1; /* Program type */
  /* BSS size minus relocation info - minimum of MIN_DATASPACE
     bytes to allow for setting up stack */
  stat.d_datalen = bss_size > 0 ? ((bss_size + MIN_DATASPACE + 1) & ~1) : MIN_DATASPACE;
  fs_heads(getchid(handle), -1L, &stat, 14);
#else
  dspace = bss_size > 0 ? ((bss_size + MIN_DATASPACE + 1) & ~1) : MIN_DATASPACE;
  fprintf(stderr, "%s: dataspace %d (%x)\n", prgm_name, dspace, dspace);
#ifdef XTC68
  {
    char x[4];
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-result"
    write(handle, "XTcc", 4);
    out_long(x, dspace);
    write(handle, x, 4);
#pragma GCC diagnostic pop

#if defined(__unix__) || defined(__APPLE__)
    fchmod(handle, S_IRUSR | S_IWUSR);
#endif
  }
#endif
#endif
  if (close(handle) < 0) {
    halt(10);
  }
}

/* Deal with command line */

void remove_trailing_slash(char *fname) {
  int n = strlen(fname);
  if (*(fname + n - 1) == '/' || *(fname + n - 1) == '\\') {
    *(fname + n - 1) = 0;
  }
}

void command_line(int *xac, char ***xav, char **paths, char *lib_arr) {
#ifdef QDOS
  extern char _prog_use[];
#endif
  char *p;
  char clrarg;
  int i, num_paths = 0, num_files = 0;
  char **argv = *xav;
  int argc = *xac;

#ifdef QDOS
  argv[0] = "crt_o"; /* Standard startup module */
#else
  argv[0] = "crt.o"; /* Standard startup module */
#endif
  lstng_flag = 0;  /* No list as yet */
  prgm_flag = 1;   /* Produce program by default */
  symbol_flag = 0; /* No symbol list by default */
  for (i = 1; i < argc; i++) {
    if (*argv[i] == '-') {
      /* Deal with options */
      clrarg = 1;
      switch (toupper(argv[i][1])) {
      case 'N': /* No program please */
        prgm_flag = 0;
        break;
      case 'S': /* New startup file */
        argv[0] = &argv[i][2];
        break;
      case 'O': /* Output program name */
        if (argv[i][2]) {
          prgm_name = &argv[i][2];
        } else {
          argv[i] = 0;
          i++;
          prgm_name = strdup(argv[i]);
          argv[i] = 0;
        }
        break;
      case 'M': /* MAP file wanted */
        lstng_flag = 1;
        if (toupper(argv[i][2]) == 'S')
          symbol_flag = 1; /* Symbols too please ! */
        if (argv[i][3] == 'U')
          unref_flag = 1; /* ... but not unreferenced ones ! */
        break;
      case 'V':
        verbose_flag++;
        break;
      case 'L':                  /* Either a new library path to add, or a new library */
        if (argv[i][1] == 'l') { /* New library */
          lib_arr[i] = 1;
          if (!(p = malloc(strlen(&argv[i][2]) + 6)))
            halt(0);
          strcpy(p, "lib");
          strcat(p, &argv[i][2]);
          strcat(p, ".a");
          argv[i] = p;
          clrarg = 0;
        } else {
          if (num_paths < NUM_SPATHS)
            paths[num_paths++] = &argv[i][2];
        }
        break;
      case 'B':                               /* Could be buffer set */
        if (!strnicmp(argv[i], "-BUFL", 5)) { /* Set library buffer size */
          buf_size = atoi(&argv[i][5]);
          if (buf_size > 2 && buf_size < 999)
            buf_size *= 1024;
          break;
        } else if (!strnicmp(argv[i], "-BUFP", 5)) { /* Set program size */
          mem_size = atoi(&argv[i][5]);
          if (mem_size > 2 && mem_size < 999)
            mem_size *= 1024;
          break;
        }
        __attribute__((fallthrough));
      default:
        fprintf(stderr, "Invalid Option: '%s'\n", argv[i]);
        break;
      }
      if (clrarg)
        argv[i] = NULL;
    }
#ifdef XTC68
    else if (*argv[i] == '@') {
      FILE *fp;
      char **nav;
      short j, k;
      char fnam[80];
      char *fn = (argv[i] + 1);

      nav = malloc(4096);

      if ((fp = fopen(fn, "r")) != NULL) {
        for (j = 0; j < i; j++) {
          nav[j] = argv[j];
        }
        k = j + 1;

        while (fscanf(fp, "%s", fnam) != EOF) {
          if (*fnam) {
            nav[i] = strdup(fnam);
            lib_arr[i] = 0;
            num_files++;
            i++;
          }
        }
        fclose(fp);

        for (j = i; k < argc; k++, j++) {
          nav[j] = argv[k];
        }

        *xav = argv = nav;
        *xac = argc = j;
        --i;
      } else {
        halt(12, fn);
      }
    }
#endif /* XTC68 */

    else {
      lib_arr[i] = 0; /* This is an object file not a library file */
      num_files++;    /* A file was specified */
    }
  }

  if (lstng_flag) {
    /* Set the MAP file name to be the program name plus _MAP */
    strcpy(lstng_name, prgm_name);
#ifdef QDOS
    strcat(lstng_name, "_MAP");
#else
    strcat(lstng_name, ".MAP");
#endif
  }
  if (!num_files) {
    printf("No files specified\n");
    exit(-15);
  }
  /* Add the last library path to search (_pdir_lib_) */
#ifdef QDOS
  if (!(p = paths[num_paths++] = malloc(strlen(getenv(_prog_use)) + 5)))
    halt(0);
  strcpy(p, getenv(_prog_use));
  strcat(p, "LIB_");
#elif defined(XTC68)
  {
    char *ldd = NULL;
    char *binpath = get_binary_path();
    if (binpath) {
      char *dirnam = strdup(dirname(binpath));
      strcpy(binpath, dirnam);
      free(dirnam);

      char *lsep = NULL;
#ifndef WIN32
      lsep = strrchr(binpath, '/');
#else
      lsep = strrchr(binpath, '\\');
#endif
      if (lsep != NULL) {
        strcpy(lsep + 1, "share/qdos/lib");
        paths[num_paths++] = binpath;
      }
    }

#ifdef PREFIX
    char *pfx = XSTR(PREFIX);
    if (strlen(pfx) > 0) {
      ldd = malloc(strlen(pfx) + 32);
      strcpy(ldd, pfx);
      strcat(ldd, "/share/qdos/lib");
      paths[num_paths++] = ldd;
    }
#endif

    ldd = getenv("QLLIB");
    if (ldd != NULL) {
      paths[num_paths++] = strdup(ldd);
    }
    paths[num_paths++] = "/usr/local/share/qdos/lib";
    for (int i = 0; i < num_paths; i++) {
      remove_trailing_slash(paths[i]);
    }
  }
#endif
  paths[num_paths] = NULL;
}

int main(int argc, char **argv) {
  char *paths[NUM_SPATHS + 2], *lib_arr;
  int i;
#ifdef QDOS
  mem_size = 500 * 1024;
  buf_size = 200 * 1024;
#else
  mem_size = 4 * 1024 * 1024;
  buf_size = 2 * 1024 * 1024;
#endif
  double_sym = undefd_sym = range_err = 0;
  if (!(lib_arr = malloc(1024)))
    halt(0);
  command_line(&argc, &argv, paths, lib_arr);
  mod_liste = NULL;
  list_file = stdout;
  if (lstng_flag) {
    list_file = fopen(lstng_name, "w");
    if (list_file == NULL) {
      fprintf(stderr, "Cannot open list_file '%s' for writing\n", lstng_name);
      exit(-1);
    }
    if (list_file != stdout)
      fprintf(list_file, "%s %s %s", _prog_name, _version, _copyright);
  }

  if (verbose_flag)
    fprintf(stderr, "%s %s %s", _prog_name, _version, _copyright);

  init_mem();
  sec_liste = curr_sec = moved_sec = NULL;
  xsy_liste = NULL;
  xref_liste = NULL;
  altxref = NULL;

  /* Link the startup file */
  link_file(argv[0], 0, paths, 0);
  /* Now go through the files in the argv array, ignoring NULL entries,
               and searching for libraries on the search path */
  for (i = 1; i < argc; i++) {
    if (!argv[i])
      continue; /* Not really an arguement */
    link_file(argv[i], lib_arr[i], paths, i);
  }
#ifdef QDOS
  link_file("libc_a", 1, paths, ++i);
#else
  link_file("libc.a", 1, paths, ++i);
#endif
  code_ptr = memstart;
  debug_start = code_ptr;
  debug_end = code_ptr;
  all_xrefs(sec_liste);

  if (list_file != stdout || verbose_flag) {
    statistic();
    fprintf(list_file, "Program length   = %8" PRIxPTR "\n", memstart - membot);
    fprintf(list_file, "Relocation table = %8" PRIxPTR "\n", code_ptr - debug_end);
    fprintf(list_file, "--------------------\n");

    fprintf(list_file, "Memory Usage     = %7ld%%\n", (long)(code_ptr - membot) * 100 / mem_size);
    fprintf(list_file, "Buffer Usage     = %7ld%%\n", (long)(module_max - module_buffer) * 100 / buf_size);
    fprintf(list_file, "--------------------\n");
  }
  list_xsy(xsy_ll, 1);
  if (prgm_flag)
    write_prog();
  if (symbol_flag) {
    fprintf(list_file, "\nSymbol Table:\n");
    fprintf(list_file, "-------------\n");
    list_xsy(xsy_ll, 0);
    fprintf(list_file, "\n");
  }
  if (undefd_sym) {
    if (list_file != stdout)
      fprintf(list_file, "Undefined Symbols: %8d\n", undefd_sym);
    fprintf(stderr, "Undefined Symbols: %8d\n", undefd_sym);
  }
  if (double_sym) {
    if (list_file != stdout)
      fprintf(list_file, "Multiply defined : %8d\n", double_sym);
    fprintf(stderr, "Multiply defined : %8d\n", double_sym);
  }
  if (range_err) {
    if (list_file != stdout)
      fprintf(list_file, "Range errors     : %8d\n", range_err);
    fprintf(stderr, "Range errors     : %8d\n", range_err);
  }

  if (verbose_flag)
    fprintf(stderr, "\nLink completed\n");
  else if (list_file != stdout)
    fprintf(list_file, "\nLink completed\n");

  if (lstng_flag)
    fclose(list_file);
  return 0;
}
