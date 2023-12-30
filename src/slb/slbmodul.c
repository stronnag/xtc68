/*      s l b m o d u l _ c
 *
 * This module contains the routines that manipulate complete modules
 * of an SROFF library
 *
 * (c) Copyright 1991 David J. Walker
 *     Email:  d.j.walker@oasis.icl.co.uk
 *
 *     Permission to copy and/or distribute granted under the
 *     following conditions:
 *
 *     1). This notice must remain intact.
 *     2). The author is not responsible for the consequences of use
 *         this software, no matter how awful, even if they
 *         arise from defects in it.
 *     3). Altered version must not be represented as being the
 *         original software.
 *
 */

#include <xtc68.h>
#include "slb.h"
#include <assert.h>

PRIVATE int copy_file _PROTOTYPE((char *, char *, FILE *, FILE *));
PRIVATE int extract_module _PROTOTYPE((char *, char *, FILE *, FILE *));
PRIVATE int add_module _PROTOTYPE((LISTPTR));
#if 0
#undef DBG
#include <stdarg.h>
#define DBG(p1) dddd p1
void dddd(char *a, int b, char *s,...)
{
    va_list ap;
    fputs(a, stderr);

    va_start (ap, s);
    vfprintf (stderr, s, ap);
    fputc('\n', stderr);
    fflush(stderr);
    va_end (ap);
}
#endif

/*============================================================= SEARCH_LIST */
PUBLIC
LISTPTR search_list(    char *modname)
    /*          ~~~~~~~~~~~
     *  Search the file list for the given modulename.  If found, return
     *  a pointer to the relevant list entry, otherwise NULL.
     *
     *  Any normalisation of the modulename passed as a parameter
     *  should have been done prior to calling this routine.
     *--------------------------------------------------------------------------*/
{
  LISTPTR ptr = filelist;

  DBG(("SEARCH_LIST", 0x801, "Enter: modname='%s'", modname));
  for (ptr = filelist; ptr; ptr = (LISTPTR)ptr->node.next) {
    DBG(("SEARCH_LIST", 0x801, "... comparing against '%s'", ptr->module));
    if (!stricmp(modname, ptr->module)) {
      DBG(("SEARCH_LIST", 0x801, "Exit: ptr=%ld", modname));
      return (ptr);
    }
  }
  DBG(("SEARCH_LIST", 0x801, "Exit: NULL (not found)"));
  return ((LISTPTR)NULL);
}

/*=========================================================== REMOVE_LIST */
PUBLIC
int remove_list(char *modname)
/*      ~~~~~~~~~~~
 *  Remove the entry containing the given modulename from the file list
 *--------------------------------------------------------------------------*/
{
  LISTPTR ptr, oldptr = NULL;

  DBG(("REMOVE_LIST", 0x801, "Enter: modname=%s", modname));
  for (ptr = filelist; ptr; ptr = (LISTPTR)ptr->node.next) {
    DBG(("REMOVE_LIST", 0x808, "... checking against %s", ptr->module));
    if (stricmp(modname, ptr->module)) {
      DBG(("REMOVE_LIST", 0x808, "... but no match found"));
      oldptr = ptr;
      continue;
    }
    DBG(("REMOVE_LIST", 0x808, "... match found, ptr=%ld", (long)ptr));
    /* Entry found - so remove from list */
    if (ptr == filelist) {
      DBG(("REMOVE_LIST", 0x804, "... remove first in list"));
      filelist = (LISTPTR)ptr->node.next;
      DBG(("REMOVE_LIST", 0x804, "... new first in list is %ld", filelist));
    } else {
      DBG(("REMOVE_LIST", 0x804, "... remove middle - oldptr=%ld", (long)oldptr));
      oldptr->node.next = ptr->node.next;
    }
    free((void *)ptr);
    DBG(("REMOVE_LIST", 0x801, "Exit: OK"));
    return (0);
  }
  /* This point reached when list exhausted */
  DBG(("REMOVE_LIST", 0x801, "Exit: NOT FOUND"));
  return (1);
}

/*========================================================== GET_MODULENAME */
PUBLIC
int get_modulename(FILE *fp, char *modname)
/*      ~~~~~~~~~~~~~~
 *  Get the module name from the file.
 *
 *  The file pointer should be pointing at the FB byte to start it, or
 *  something is wrong.
 *
 *  Leave the file pointer at the next character after the name - this
 *  may be comment data.
 *--------------------------------------------------------------------------*/
{
  int n;
  char *ptr = modname;

  DBG(("GET_MODULENAME", 0x801, "Enter: filepos=%ld, EOF=%d", (long)ftell(fp), feof(fp)));
  if (Is_Sroff(fp)) {
    Skip_Chars(fp, (size_t)2);
    n = inchar(fp); /* Read length field */
    while (n--)
      *ptr++ = inchar(fp);
    *ptr = '\0'; /* NULL terminate */
    DBG(("GET_MODULENAME", 0x801, "Exit OK: modname='%s'", modname));
    return (0);
  }
  if (!feof(fp)) {
    DBG(("GET_MODULENAME", 0x801, "ERROR: Not an SROFF file !"));
    error(20);
  }
  DBG(("GET_MODULENAME", 0x801, "Exit EOF"));
  return (EOF);
}

/*============================================================ COPY_MODULE */
PUBLIC
int copy_module(char *modname, FILE *lbfp, FILE *newfp, FILE *lstfp)
/*      ~~~~~~~~~~~
 *  This is the heart of the SLB librarian as this module is
 *  called eventually whatever options you have selected.
 *
 *  Copy a module between the source and target files.
 *  Also list the module to the listing file.
 *
 *  On entry, the file pointer should be positioned just after the
 *  modulename (but before any comments, etc following it).
 *--------------------------------------------------------------------------*/
{
  char filename[100];

  DBG(("COPY_MODULE", 0x101, "Enter: modname='%s'", modname));
  incount = 0;
  if (lstfp) {
    if (nflag) {
      sprintf(filename, "%03d_", nflag);
      if (newfp || lstfp)
        nflag++;
    } else
      filename[0] = '\0';
    strcat(filename, modname);
    if (!has_extension(filename))
      strcat(filename, DEFAULT_EXT);
    lprintf("%s", filename);
    if (stricmp(filename, modname))
      lprintf("/%s", modname);
    lprintf("\n");
  }
  if (nflag && (newfp || lstfp))
    nflag++;

  if (newfp) {
    outchar(SROFF_FLAG, newfp);
    outchar(SROFF_SOURCE, newfp);
    outchar((char)strlen(modname), newfp);
    fputs(modname, newfp);
  }
  /*
  Now go into a big loop - reading the library file and only doing
  special actions on the special escape character 0xFB.  We need to
  take into account, though, that some of the fixed fields following
  a directive may contain 0xFB bytes.

  The actions are:
      0xFB 0x01 - New object file - followed by byte of name length
                  then name itself.
                  NOTE.   This should not happen at this point as the
                          module name should already have been passed
      0xFB 0x13 - End of object file. All other FB codes are internal
                  to linker.
      0xFB 0x07 - Special XDEF (may contain one of the above sequences !)
  */
  while ((inchar(lbfp)) != EOF) {
    switch (ch) {
    default:
      DBG(("COPY_MODULE", 0x4008, "Default character $%02.2x", ch));
      outchar(ch, newfp);
      continue;
    case SROFF_FLAG:
      DBG(("COPY_MODULE", 0x4004, "SROFF_FLAG character at file position %ld", (long)ftell(lbfp)));
      switch (inchar(lbfp)) {
      case SROFF_FLAG: /* two FB's together are a directive to output FB */
      case SROFF_COMMENT:
      default:
        DBG(("COPY_MODULE", 0x4004, "SROFF_FLAG/COMMENT/default character"));
        /* Linker internal control */
        /*- put out the two characters read */
        outchar(SROFF_FLAG, newfp);
        outchar(ch, newfp);
        break;
      case SROFF_ORG:
      case SROFF_OFFSET:
        DBG(("COPY_MODULE", 0x4004, "SROFF_ORG/OFFSET character"));
        /* Copy across the longword following */
        outchar(SROFF_FLAG, newfp);
        outchar(ch, newfp);
        Copy_Chars(newfp, lbfp, (size_t)4);
        break;
      case SROFF_SECTION:
      case SROFF_DEFINE:
      case SROFF_COMMON:
        DBG(("COPY_MODULE", 0x4004, "SROFF_SECTION/DEFINE/COMMON character"));
        /* Copy across the short-word id following */
        outchar(SROFF_FLAG, newfp);
        outchar(ch, newfp);
        Copy_Chars(newfp, lbfp, (size_t)2);
        break;
      case SROFF_XDEF:
        DBG(("COPY_MODULE", 0x4004, "SROFF_XDEF character"));
        /* Copy across string, then longword, and finally id */
        outchar(SROFF_FLAG, newfp);
        outchar(ch, newfp);
        Copy_Chars(newfp, lbfp, (size_t)1);  /* string length byte */
        Copy_Chars(newfp, lbfp, (size_t)ch); /* ... now the string */
        Copy_Chars(newfp, lbfp, (size_t)6);
        break;
      case SROFF_END: /* End of object module */
        DBG(("COPY_MODULE", 0x4004, "SROFF_END character"));
        outchar(SROFF_FLAG, newfp);
        outchar(ch, newfp);
        incount = 0;
        DBG(("COPY_MODULE", 0x101, "Exit (SROFF_END found): modname='%s'", modname));
        return (0);
      case SROFF_SOURCE:
        DBG(("COPY_MODULE", 0x4004, "SROFF_SOURCE character"));
        error(18, ftell(lbfp));
	XTC68_FALLTHROUGH;

      case SROFF_XREF:
        DBG(("COPY_MODULE", 0x4004, "SROFF_XREF character"));
        outchar(SROFF_FLAG, newfp); /* Output the directive */
        outchar(SROFF_XREF, newfp);
        Copy_Chars(newfp, lbfp, (size_t)(4 + 1)); /* Copy longword/trunc rule */
        for (;;) {
          /* Copy <op><id> combinations until another 0xFB */
          Copy_Chars(newfp, lbfp, (size_t)1); /* <op> */
          if (ch == SROFF_FLAG)
            break;
          Copy_Chars(newfp, lbfp, (size_t)2); /* <id> */
        }                                     /* end of FOREVER loop */
        break;                                /* for case SROFF_XREF */
      }                                       /* end of SROFF_FLAG switch */
      break;
    } /* end of main switch */
  }   /* end of while loop */

  error(19);
  return (-1);
}

/*============================================================== COPY_FILE */
PRIVATE
int copy_file(char *filename, char *modname, FILE *lbfp, FILE *lstfp)
/*      ~~~~~~~~~
 *  Open the specified file, and then copy it to the library.
 *-------------------------------------------------------------------------*/
{
  int reply;
  FILE *newfp;
  char filemodulename[100];

  if (!(newfp = open_file(filename, "rb"))) {
    return (1);
  }
  get_modulename(newfp, filemodulename);
  reply = copy_module(modname, newfp, lbfp, lstfp);
  fclose(newfp);
  return (reply);
}

/*========================================================== EXTRACT_MODULE */
PRIVATE
int extract_module(char *filename,
                   char *modname,
                   FILE *lbfp,
                   FILE *lstfp)
/*      ~~~~~~~~~~~~~~
 *  This creates a new output file, and extracts a single module
 *  from the library into it.
 *
 *  The source file pointer should be positioned just after the
 *  module name when this routine is entered.
 ---------------------------------------------------------------------------*/
{
  int reply = 0;
  FILE *newfp = (FILE *)NULL;
  char fullfilename[100];

  if (xflag) {
    if (nflag)
      sprintf(fullfilename, "%03d_", nflag);
    else
      fullfilename[0] = '\0';
    strcat(fullfilename, filename);
    if (!has_extension(filename))
      strcat(fullfilename, DEFAULT_EXT);
    if (!(newfp = open_file(fullfilename, "wb"))) {
      error(4, fullfilename);
    }
  }

  reply = copy_module(modname, lbfp, newfp, lstfp);
  if (newfp)
    fclose(newfp);
  return (reply);
}

/*============================================================= ADD_MODULE */
PRIVATE
int add_module(LISTPTR ptr)
/*      ~~~~~~~~~~
 *  Add a module to the library
 *-------------------------------------------------------------------------*/
{
  if (vflag) {
    eprintf("adding '%s", ptr->node.name);
    if (stricmp(ptr->node.name, ptr->module))
      eprintf("/%s", ptr->module);
    eprintf("'\n");
  }
  return (copy_file(ptr->node.name, ptr->module, newlibfp, listfp));
}

/*============================================================ CREATE_MODE */
PUBLIC
int create_mode(void)
/*      ~~~~~~~~~~~
 *  Create a new library file by continually adding modules to the end
 *  until the file list is exhausted.
 *
 *  NOTE.. This routine is also called by the add function.
 *------------------------------------------------------------------------*/
{
  LISTPTR ptr;

  for (ptr = filelist; ptr; ptr = (LISTPTR)ptr->node.next) {
    add_module(ptr);
  }
  return (0);
}

#if 0
/*============================================================== ADD_MODE */
PUBLIC
int     Add_Mode (void)
/*      ~~~~~~~~
 *  Add modules to an existing library.
 *
 *  Check that these modules are not already be present in the
 *  library.
 *--------------------------------------------------------------------------*/
{
    short  errors = 0;
    char    modulename[100];

    /*
     Scan through library to check modules not already present
     */
    while (get_modulename(libfp, modulename) != EOF) {
        if (search_list(modulename)) {
            eprintf ("module %s already exists in library\n",modulename);
            errors++;
        }
        copy_module (modulename, libfp, NULL, listfp);
    }
    /*
     * Now use "create" function to add to end of library
     */
    return (errors ? -1 : create_mode());
}
#endif

/*============================================================ DELETE_MODE */
PUBLIC
int delete_mode(void)
/*      ~~~~~~~~~~~
 *  Delete modules from an existing library.
 *
 *  If any modules are not found, then report the fact.
 *  Do not abort the run, however, and continue by deleting
 *  what routines you can.
 *--------------------------------------------------------------------------*/
{
  LISTPTR ptr;
  char modname[100];
  int deletecount = 0;

  while (get_modulename(libfp, modname) != EOF) {
    if (search_list(modname)) {
      if (vflag)
        eprintf("deleting %s\n", modname);
      copy_module(modname, libfp, NULL, NULL);
      remove_list(modname);
    } else {
      copy_module(modname, libfp, newlibfp, listfp);
    }
  }
  for (ptr = filelist; ptr; ptr = (LISTPTR)ptr->node.next) {
    eprintf("Module %s not deleted (not found)\n", ptr->module);
  }
  return (deletecount);
}

/*=============================================================== REPLACE */
PUBLIC
int replace_mode(void)
/*      ~~~~~~~~~~~~
 *  Replace modules in an existing library.
 *
 *  If they are not present, then merely add them at the end.
 *-------------------------------------------------------------------------*/
{
  LISTPTR ptr;
  char modname[100];
  int reply = 0;

  DBG(("REPLACE_MODE", 0x41, "Enter"));
  while (get_modulename(libfp, modname) != EOF) {
    DBG(("REPLACE_MODE", 0x44, "next module = '%s', stackreport=%ld", modname, 0));
    if ((ptr = search_list(modname))) {
      DBG(("REPLACE_MODE", 0x44, "replacing module '%s'", modname));
      if (vflag) {
        eprintf("replacing %s", modname);
        if (stricmp(modname, ptr->node.name))
          eprintf(" from file '%s'", ptr->node.name);
        eprintf("\n");
      }
      DBG(("REPLACE_MODE", 0x44, "... start by skipping original"));
      copy_module(modname, libfp, NULL, NULL);
      DBG(("REPLACE_MODE", 0x44, "... then new one from '%s'", ptr->node.name));
      copy_file(ptr->node.name, modname, newlibfp, listfp);
      DBG(("REPLACE_MODE", 0x44, "... then remove entry from list"));
      remove_list(modname);
    } else {
      DBG(("REPLACE_MODE", 0x44, "copying original module '%s'", modname));
      copy_module(modname, libfp, newlibfp, listfp);
    }
    DBG(("REPLACE_MODE", 0x44, "module completed - go for next"));
  }
  DBG(("REPLACE_MODE", 0x44, "Finished replace phase.  filelist=%ld", (long)filelist));
  for (ptr = filelist; ptr; ptr = (LISTPTR)ptr->node.next) {
    DBG(("REPLACE_MODE", 0x44, "add module '%s'"));
    reply += add_module(ptr);
  }
  DBG(("REPLACE_MODE", 0x41, "Exit: reply=%d", reply));
  return (reply);
}

/*=========================================================== EXTRACT_MODE */
PUBLIC
int extract_mode(void)
/*      ~~~~~~~~~~~~
 *  Take any modules specified out of the main library into individual
 *  files.
 *-------------------------------------------------------------------------*/
{
  LISTPTR ptr;
  char modname[100];
  int reply = 0;

  while (get_modulename(libfp, modname) != EOF) {
    if ((ptr = search_list(modname))) {
      if (vflag && actions) {
        eprintf("extracting '%s", ptr->node.name);
        if (stricmp(modname, ptr->node.name))
          eprintf("/%s'", modname);
        eprintf("\n");
      }
      reply += extract_module(ptr->node.name, modname, libfp, listfp);
      remove_list(modname);
    } else {
      reply += copy_module(modname, libfp, NULL, listfp);
    }
  }
  for (ptr = filelist; ptr; ptr = (LISTPTR)ptr->node.next) {
    eprintf("Module %s not extracted (not found)\n", ptr->node.name);
  }
  return (filelist == (LISTPTR)NULL ? reply : -1);
}

/*============================================================= SPLIT_MODE */
PUBLIC
int split_mode(void)
/*      ~~~~~~~~~~
 *  Take all modules specified out of the main library into individual
 *  files.
 *-------------------------------------------------------------------------*/
{
  int reply = 0;
  char modname[100];

  while (get_modulename(libfp, modname) != EOF) {
    reply += extract_module(modname, modname, libfp, listfp);
    if (vflag && (!tflag))
      eprintf("extracted %s\n", modname);
  }
  return (reply);
}
