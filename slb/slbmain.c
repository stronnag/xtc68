/*
 *     s l b m a i n _ c
 *
 *     slb - SROFF librarian
 *
 *     Main control module for this program
 *
 * (c) Copyright 1991 David J. Walker
 *     Email:  d.j.walker.lon4905@oasis.icl.co.uk
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
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  01 Sep 92   DJW   - The workfile will put on the device specified by
 *    (v2.8)            the TEMP environment variable if it is defined.
 *                    - The -k flag added.
 */

#include "slb.h"
#include <stdarg.h>
#include <time.h>

#ifdef QDOS
#endif

PRIVATE void  Program_Id    _PROTOTYPE((void));
PRIVATE void  add_list      _PROTOTYPE((char *));
PRIVATE void  build_list    _PROTOTYPE((FILE *));
PRIVATE void  Action_Taken  _PROTOTYPE((char *));

PRIVATE int     errflag = 0;


/*============================================================== PROGRAM_ID */
PRIVATE
void    Program_Id ()
/*      ~~~~~~~~~~
 *--------------------------------------------------------------------------*/
{
    eprintf ("%s %s (%s) %s\n",_prog_name, _version, _copyright, copyright);
    return;
}


/*============================================================ ERROR */
PUBLIC
void    error( int num, _PARAMLIST )
/*      ~~~~~
 * Routine to report an error.  A newline is added at the end.
 *-------------------------------------------------------------------*/
{
    va_list ap;
    int     errnum;

    DBG(("ERROR",0x11,"Enter: num=%d",num));
    eprintf ("ERROR %d: ",num);
    errnum = (num > errmax) ? 0 : num;
    va_start (ap, num);
    vfprintf (stderr,errmsg[errnum],ap);
    va_end(ap);
    eprintf("\n");
    exit (-1);
}


/*================================================================ INCHAR */
PUBLIC
int     inchar( fp)
/*      ~~~~~~
 * Routine to read a character from a file.
 * Terminates with error (not including EOF).
 *------------------------------------------------------------------------*/
FILE *fp;
{
    if(fp==(FILE *)NULL)
        error(1);
    errno = 0;
    if (((ch = getc( fp ))==EOF)
    &&   ( ! feof(fp) )) {
        error(2, ftell(fp));
    }
    return ch;
}


/*============================================================ OUTCHAR */
PUBLIC
void    outchar( c, fp)
/*      ~~~~~~~
 * Routine to put a character into a file, and stop if
 * an error occurs.
 *---------------------------------------------------------------------*/
int c;
FILE *fp;
{
     if (fp) {
        errno = 0;
        if(putc( c, fp) != c) {
            error(3,ftell(fp));
        }
     }
}


/*======================================================== HAS_EXTENSION */
PUBLIC
char *  has_extension (name)
/*      ~~~~~~~~~~~~~
 *  Analyse the name supplied to see if it has an extension.
 *
 *  If it does return a pointer to the first character of the
 *  extension, and otherwise return NULL.
 *-----------------------------------------------------------------------*/
char *  name;
{
    char *ptr = &name[strlen(name) - 1];

    DBG(("HAS_EXTENSION",101,"Enter: Name=%s",name));
    while (--ptr > name) {
        if (*ptr == '_' || *ptr == '.') break;
    }
    if (ptr != name) {
        if ((stricmp(&ptr[1],"o"))
        ||  (stricmp(&ptr[1],"asm"))
        ||  (stricmp(&ptr[1],"rel")) ) {
            DBG(("HAS_EXTENSION",101,"Exit: YES=%s",ptr));
            return (ptr);
        }
    }
    DBG(("HAS_EXTENSION",101,"Exit: NO"));
    return ((char *)NULL);
}


/*===================================================== GET_BASENAME */
PUBLIC
void    get_basename (name)
/*      ~~~~~~~~~~~~
 *  Reduce the module name to its basic form without any leading
 *  device/directory or trailing extension.
 *
 *  If the -e flag is set, however, keep the extension.
 *
 *  If the -F flag is set, then do not remove any leading directory
 *  name.
 *
 *  This routine changes the actual string passed as a parameter,
 *  so make certain that it is OK to do so.
 *-----------------------------------------------------------------------*/
char *name;
{
    char *ptr;

    DBG(("GET_BASENAME",101,"Enter: Name=%s",name));
    /*
     * First handle any extension
     */
    if (ptr = has_extension (name)) {
        if (!eflag) {
            *ptr = '\0';
        }
    } else {
        ptr = &name[strlen(name)];
    }
#ifdef QDOS
    if (kflag) {
        if (isdirdev(name))
            ptr = strchr(name, '_');
        else
            ptr = name;
    } else {
#endif
        /*
         * Now any leading device/directory.
         * Allow for fact that module name may
         * begin with an '_' or '.' character.
         * This means that if more than one occurs
         * together, then only the first one is removed.
         */
        for ( ptr-- ; ptr > name ; ptr--) {
#ifdef QDOS
            if ((strchr("_.",ptr[0])  != (char *) NULL)
            &&  (strchr("_.",ptr[-1]) == (char *) NULL) ) {
#else
            if ((strchr("/.",ptr[0])  != (char *) NULL)
            &&  (strchr("/.",ptr[-1]) == (char *) NULL) ) {
#endif
                ptr++;
                break;
            }
        }
#ifdef QDOS
    }
#endif
    strcpy (name, ptr);
    DBG(("GET_BASENAME",101,"Exit: Name=%s",name));
}


/*============================================================== OPEN_FILE */
PUBLIC
FILE  * open_file (filename, mode)
/*      ~~~~~~~~~
 *  Open the requested file.  If there are any problems then
 *  report them.  Return NULL or the fp.
 ------------------------------------------------------------------------- */
char *filename;
char * mode;
{
    FILE  *fp;

   if ((fp = fopen( filename, mode))==NULL) {
    }
    return (fp);
}


/*=========================================================== ADD1_LIST */
PRIVATE
void    add1_list (filename)
/*      ~~~~~~~~
 *  Add the specified file to the list of file/module names.
 *
 *  The modulename is derived from the filename unless the
 *  -f flag has been set, in which case the file is read to
 *  determine the modulename.  In both cases, the -e flag
 *  determines whether the extension (if any) is to be removed.
 *---------------------------------------------------------------------*/
char    *filename;
{
    LISTPTR ptr = listend;
    FILE   *fp;
    char    modname[1024];
    char    newfilename[1024];

    DBG(("ADD1_LIST",0x21,"File '%s'",filename));
    strcpy (newfilename, filename);
    if (fflag) {
        if ((fp=open_file(newfilename,"rb")) == NULL) {
            error (4,newfilename);
        }
        get_modulename (fp, modname);
        fclose (fp);
    } else {
        strcpy (modname, filename);
    }
    get_basename (modname);
    if (vflag && fflag) {
        eprintf ("examined '%s' to get modname '%s'\n",newfilename,modname);
    }
    listend = (LISTPTR)Allocate_Memory(sizeof(LISTSTRUCT));
    if (filelist == (LISTPTR)NULL) filelist = listend;

    if(ptr) ptr->node.next = (NODEPTR)listend;
    ptr = listend;
    ptr->node.name = Make_String(newfilename);
    ptr->module = Make_String(modname);
    ptr->node.next = (NODEPTR)NULL;
    return;
}

/*=========================================================== ADD_LIST */
PRIVATE
void    add_list (filename)
/*      ~~~~~~~~
 *  Add the specified file(s) to the list of file/module names.
 *
 *  This module allows for wild cards in the filenames, and if
 *  they are present expands them into a longer list.
 *---------------------------------------------------------------------*/
char    *filename;
{
#ifdef QDOS
    char    flist[1024],*p;
    int     j,count;

    if (strpbrk( filename, "*?[")) {
        /* It's a wildcard, get the list of data files */
        count = getfnl( filename, flist, sizeof (flist), QDR_DATA);
        for( j = 0, p = flist; j < count; j++, p += strlen(p)+1)
                        add1_list( p );
        }
    else
        /* Not a wildcard */
#endif /* QDOS */
        add1_list(filename);
    return;
}

/*========================================================== BUILD_LIST */
PRIVATE
void    build_list (fp)
/*      ~~~~~~~~~~
 *  Read through the module file, building a list of the module names
 *  that need to be processed.
 *
 *  The # symbol at the start of a line indicates the remainder
 *  of the line is a comment.
 *  Any trailing information after the module name, but on the same
 *  line is also to be treated as comment.
 *  EOF or a null line indicate end of list
 *---------------------------------------------------------------------*/
FILE * fp;
{
    char    *ptr, *nptr;
    char    name[100], filename[100];

    for ( ; ; ) {
        if (fgets( name, (int)sizeof(name), fp)==(char *)NULL) {
            if (feof(fp) )  return;
            error (5);
        }
        if (*name == '#')  continue;
        ptr = &name[strlen(name)-1];
        if (*ptr == '\n') *ptr = '\0';  /* remove newline */
        /* Ignore any comments after the file name */
        for( nptr = filename, ptr=name; *ptr != ' ' && *ptr; )
            *nptr++ = *ptr++;
        *nptr = '\0';
        if (strlen(filename)== 0)   break;
        add_list (filename);
    }
}

/*======================================================== ACTION_TAKEN */
PRIVATE
void    Action_Taken (text)
/*      ~~~~~~~~~~~~
 *----------------------------------------------------------------------*/
char * text;
{
    DBG(("ACTION_TAKEN",0x21,"Enter: %s",text));
    if (vflag)  {
        eprintf ("%s",text);
        if (maintflag && libname) eprintf (" library '%s'",libname);
        eprintf ( "\n");
    }
    DBG(("ACTION_TAKEN",0x21,"Exit"));
    return;
}

/*================================================================ MAIN */
PUBLIC
int    main (int  argc, char **argv)
/*      ~~~~
 *  The main entry point.  Parse the parameters supplied,
 *  validate them and then perform the requested action.
 *----------------------------------------------------------------------*/

{
    extern  int  optind;
    extern  char *optarg;
    int     exit_status;
    time_t  nowtime;

#ifdef LIBDEBUG
    int     x;

    DBG_INIT();
    DBG(("MAIN",0x14,"argc=%d",argc));
    for (x=0 ; x < argc ; x++ )
        DBG(("MAIN",0x14,"argv[%d]=%s",x,argv[x]));
#endif
    my_argc = argc;
    my_argv = argv;
    string_cmp =  (int (*)())strcasecmp;
    /* Parse the arguments */

    while ((ch =getopt(argc, argv,"a:A:cCdDeEfFkKl:L:m:M:nNoOrRs:S:t:T:uUvVW:w:xXy:Y:")) != EOF ) {
        DBG(("MAIN",0x18,"... ch=%c, optarg=%s, optind=%d",ch,optarg,optind));
        switch (ch) {
        case 'a':                   /* ASSEMBLER Analysis */
        case 'A':
            Aflag++;    /* ANALYSE */
ANALOPT:
            analflag = 1;
            listname = optarg;
            break;
        case 'c':                   /* CREATE */
        case 'C':
            cflag++;
MAINTOPT:
            maintflag = 1;
            break;
        case 'd':                   /* DELETE */
        case 'D':
            dflag++;
            goto MAINTOPT;
        case 'e':                   /* EXTENSION RETAIN */
        case 'E':
            eflag++;
            break;
        case 'f':                   /* FILE COPY OF MODULE NAME */
        case 'F':
            fflag++;
            break;
#ifdef QDOS
        case 'k':                   /* KEEP FULL FILENAME */
        case 'K':
            kflag++;
            break;
#endif
        case 'l':                   /* Library order analysis */
        case 'L':
            Lflag++;
            goto ANALOPT;
        case 'm':                   /* MODULE_LIST FILE */
        case 'M':
            mflag++;
            modulename = optarg;
            break;
        case 'n':                   /* NUMBER MODULES */
        case 'N':
            nflag++;
            break;
        case 'o':                   /* OPTIMISE MODULES BEING ADDED */
        case 'O':
            nflag++;
            break;
        case 'r':                   /* REPLACE/ADD */
        case 'R':
            rflag++;
            goto MAINTOPT;
        case 's':                   /* SROFF analysis */
        case 'S':
            Sflag++;
            goto ANALOPT;
        case 't':                   /* TABLE */
        case 'T':
            tflag++;
            listname = optarg;
            goto MAINTOPT;
        case 'u':                   /* Treat all symbols as UPPER case */
        case 'U':
            string_cmp =  (int (*)())strcmp;
            Uflag++;
            break;
        case 'v':                   /* VERBOSE */
        case 'V':
            Program_Id ();
            vflag++;
            break;
        case 'w':                   /* XDEF cross reference */
        case 'W':
            Wflag++;
            goto ANALOPT;
        case 'x':                   /* EXTRACT */
        case 'X':
            xflag++;
            goto MAINTOPT;
        case 'y':                   /* XREF cross reference */
        case 'Y':
            Yflag++;
            goto ANALOPT;
        case '?' :
            errflag++;
        } /* end of switch */
    } /* end of while */
    DBG(("MAIN",0x14,"...finished parsing parameters\nargc=%d, optind=%d, maintflag=%d, analflag=%d",argc,optind,maintflag,analflag));
    /*
     * Check that parameters combinations chosen are compatible
     */
    switch (actions = (cflag||rflag) + dflag + tflag + xflag + Aflag + Lflag + Sflag + Wflag + Yflag) {
    case 0:
        eprintf ("No actions specified\n");
        errflag++;
        break;
    case 1:
        if ( (maintflag + analflag) == 1)
            break;
    default:
        DBG(("MAIN",0x14,"Incompatible actions - count=%d, maintflag=%d, analflag=%d",actions,maintflag,analflag));
        eprintf ("Incompatible combination of actions specified\n");
        errflag++;
        break;
    }
    if (tflag && mflag) {
            if (mflag) eprintf ("\n-m option not used if only action is -t\n");
            errflag++;
    }
    if (maintflag) {
        DBG(("MAIN",0x14,"Checking for library name parameter - argc=%d, optind=%d",argc,optind));
        if (argc > optind ) {
            libname = argv[optind++];
        } else {
            eprintf ("\nno library name specified\n");
            errflag++;
        }
    }
    /*
     *  Build module list
     *  If there are any module names on the command line, then take
     *  these.   After that see if a module file has been given, in
     *  which case do that.   Both options are allowed together.
     */
    if (argc > optind) {
        DBG(("MAIN",0x18,"Building module list from command line. argc=%d, optind=%d",argc,optind));
        while ( argc > optind )
        {
                add_list(argv[optind++]);
        }
    }
    if (mflag) {
        if (strcmp(modulename,"-")) {
            if ((modulefp = open_file( modulename, "rb")) == NULL) {
                error(6);
            }
            if (vflag) {
               eprintf ("Reading modulelist from file '%s'\n",modulename);
            }
        } else {
            modulefp =stdin;
        }
        DBG(("MAIN",0x18,"Building module list from file '%s'",modulename));
        build_list(modulefp);
        fclose (modulefp);
    }
    DBG(("MAIN",0x18,"Finished building module list"));
    if (analflag && (filelist==(LISTPTR)NULL)) {
        eprintf ("\nno modules specified for analysis\n");
        errflag++;
    }
    DBG (("MAIN",0x14,"...finished validating parameters - errflag=%d",errflag));
    if (errflag) {
        eprintf("usage:   slb [-e] [-k] [-n] [-o] [-v] -[c|d|r|x|t listfile]\n");
        eprintf("                [-m modulefile] library_name [modules...]\n");
        eprintf("         slb [-v][-m module_file] -[A|L|S|W|Y] [-U] listfile [modules...]\n");
        eprintf("   -c|d|r|x|t  create | delete | replace/add | extract | table library\n");
        eprintf("   -e   extension retained\n");
        eprintf("   -k   keep directory part of filename\n");
        eprintf("   -f   module name(s) taken from file(s) contents\n");
        eprintf("   -n   add number to module names\n");
        eprintf("   -o   optimise module(s) being added to library\n");
        eprintf("   -m   module names taken from specified file\n");
        eprintf("   -v   verbose mode\n");
        eprintf("   -A   Assembler analysis of module(s)\n");
        eprintf("   -L   Library Order analysis of module(s)\n");
        eprintf("   -S   SROFF analysis of module(s)\n");
        eprintf("   -W   XDEF Cross-Reference of module(s)\n");
        eprintf("   -Y   FILE/MODULE Cross-Reference of module(s)\n");
        eprintf("   -U   Treat UPPER case as different to lower case\n");
        exit (7);
    }
    /*
     * open library file.
     * If create mode specified it is valid for it to not exist,
     * in all other cases this is an error.
     */
    errno = 0;

    if (libname) {
        /*
         *  First try to open in read mode
         */
        DBG(("MAIN",0x14,"Opening library file '%s'",libname));
        if ((libfp = open_file ( libname, "rb")) != NULL) {
            /*
             * Library file already exists.
             *  If only create flag specified, then this
             *  is an error unless replace flag also given.
             */
            if (cflag) {
                if (rflag) {
                    cflag = 0;      /* Create flag irelevant as library exists */
                } else {
                    fclose (libfp);
                    error (8, libname);
                }
            }
        } else {
            /*
             *  Library file does not already exist
             *  This is OK as long as create specified.
             *  If replace also specified, it is ignored.
             */
            if (cflag) {
                if ((newlibfp = open_file ( libname, "wb")) == NULL) {
                    error (9, libname);
                } else {
                    rflag = 0;
                }
            } else {
                error (10, libname);
            }
        }
    }
    /*
     *  Open working file
     *  This is only required if in delete or replace mode
     */
    if (dflag || rflag) {
        char    *ptr;

        if (((ptr = getenv("TEMP")) == NULL)
        &&  ((ptr = getenv("TMP")) == NULL) )
            ptr="";
        strcpy (fullworkname, ptr);
#ifdef QDOS
        if (*fullworkname && isdirdev (ptr) == 0) {
            strcpy (fullworkname, getcwd(fullworkname, (int)sizeof(fullworkname)));
        }
#endif /* QDOS */
        strcat (fullworkname, WORKNAME);
        DBG(("MAIN",0x14,"Opening Workfile %s",fullworkname));
        if ((newlibfp = open_file ( fullworkname, "wb")) == NULL) {
            error(11,fullworkname);
        }
    }
    /*
     * open listing file (if required)
     */
    nowtime = time((time_t *) NULL);
    HeadTime = ctime(&nowtime);
    HeadTime[20] = '\0';    /* But chop of year and change to NULL terminated */
    if (listname) {
        DBG(("MAIN",0x14,"Opening listing file '%s'",listname));
        if (strcmp(listname,"-")) {
            if((listfp = open_file( listname,"wb")) == NULL) {
                error(12, listname);
            }
        } else {
            listfp = stdout;
        }
    }
    /*
     * Do desired action
     */
    DBG(("MAIN",0x14,"Ready to perform desired action"));
    if (cflag) {
        Action_Taken("CREATE");
        exit_status = create_mode();
    } else if (dflag) {
        Action_Taken("DELETE from");
        exit_status = delete_mode();
    } else if (rflag) {
        Action_Taken("REPLACE/ADD in");
        exit_status = replace_mode();
    } else if (xflag) {
        Action_Taken("EXTRACT from");
        if (filelist) {
            /* Specified modules */
            exit_status = extract_mode();
        } else {
            /* all modules */
            exit_status = split_mode();
        }
    } else if (tflag) {
        Action_Taken("TABLE");
        exit_status = split_mode();
    } else if (Aflag) {
        Action_Taken("Assembler ANALYSIS");
        exit_status = Library_Analysis ();
    } else if (Lflag) {
        Action_Taken("LIBRARY ORDER ANALYSIS");
        exit_status = Library_Analysis ();
    } else if (Sflag) {
        Action_Taken("SROFF ANALYSIS");
        exit_status = Library_Analysis ();
    } else if (Wflag) {
        Action_Taken("XDEF CROSS REFERENCE");
        exit_status = Library_Analysis ();
    } else if (Yflag) {
        Action_Taken("FILE/MODULE CROSS REFERENCE");
        exit_status = Library_Analysis ();
    }
    DBG(("MAIN",0x14,"Action completed - exit_status=%d",exit_status));
    /*
     * now tidy-up
     */
    if (libfp != NULL) {
        DBG(("MAIN",0x18,"Closed library file"));
        fclose (libfp);
    }
    if (listfp != NULL) {
        DBG(("MAIN",0x18,"Closed listing file"));
        fclose (listfp);
    }
    if (newlibfp != NULL) {
        DBG(("MAIN",0x18,"Closed New Lib file"));
        (void) fclose (newlibfp);
    }
    if (rflag || dflag) {
        if (exit_status) {
            DBG(("MAIN",0x18,"Delete workfile as error during run"));
            if (unlink (fullworkname)) {
                error (14,fullworkname);
            }
            DBG(("MAIN",0x11,"*** PROGRAM ENDING **** :  exit_status=%d",exit_status));
            exit (exit_status);
        }
        DBG(("MAIN",0x18,"Trying to copy workfile back to original name !"));
        /*
         *  We cannot assume rename is available,
         *  so copy new library back to old library
         */
        if (unlink (libname)) {
            error (15,libname,fullworkname);
        }
        if ( ((libfp = open_file (fullworkname, "rb")) == NULL)
        ||   ((newlibfp = open_file (libname, "wb")) == NULL)  ) {
            error (16,fullworkname);
        } else {
            int c;
            DBG(("MAIN",0x14,"Opened workfile '%s' file OK",fullworkname));
            DBG(("MAIN",0x14,"Opened new library '%s' file OK",libname));
            while ((c=fgetc(libfp)) != EOF) {
                if ((putc(c, newlibfp)) != c)
                    error(16,fullworkname);
            }
        }
        if (newlibfp) {
            DBG(("MAIN",0x14,"Closing output file"));
            fclose (newlibfp);
        }
        if (libfp && newlibfp && feof(libfp)) {
            DBG(("MAIN",0x11,"restore to original file appears to have worked!"));
            fclose (libfp);
            unlink (fullworkname);
        } else {
            DBG(("MAIN",0x11,"restore failed: newlibfp=%ld, libfp=%ld, feof(libfp)=%d !",(long)newlibfp,(long)libfp,(int)feof(libfp)));
            if (libfp) fclose (libfp);
            error(16,fullworkname);
        }
    }
    DBG(("MAIN",0x11,"*** PROGRAM ENDING **** :  exit_status=%d",exit_status));
    exit (0);
}
