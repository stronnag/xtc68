/*      s l b o r d e r _ c
 *
 *     The purpose of this module is to build up a list
 *     containing the dependencies among the files that make
 *     up a SROFF library.    The output of this program can
 *     then be fed into the 'tsort' program to build a list
 *     of files for a library order that will only contain
 *     forward references.
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
 */

#include    "slb.h"

PRIVATE char  * Add_List        _PROTOTYPE((LISTPTR *, char *));
PRIVATE XDEFPTR Make_Xdef       _PROTOTYPE((char *));
PRIVATE void    Print_Depends   _PROTOTYPE((XDEFPTR));
#ifdef FREE_AT_END
PRIVATE void    Xdef_Node_Free  _PROTOTYPE((XDEFPTR));
#endif /* FREE_AT_END */


/*============================================================== PRINT_SPACES */
PUBLIC
void    Print_Spaces(count)
/*
 *----------------------------------------------------------------------------*/
size_t  count;
{
    while (count-- > 0) outchar (' ',listfp);
    return;
}

/*========================================================== END_OF_LINE */
PRIVATE
void    End_Of_Line()
/*      ~~~~~~~~~~~
 *  Routine called when newline required.   Will generate page
 *  throws and headings if required.
 *-----------------------------------------------------------------------*/
{
    DBG (("END_OF_LINE",0x201,"Enter: LineNo=%d,PageNo=%d",LineNo,PageNo));
    if (LineNo) {
        LineNo--;
        lprintf ("\n");
    } else {
        /*
         *  Print Headings when LineNo reaches zero
         */
        int     x;

        if (PageNo++) lprintf ("\n\f");     /* No FF on first page */
        LineNo = LINES_PER_PAGE;
        Print_Spaces((size_t)INDENT_SPACES);
        lprintf(HeadTime);
        Print_Spaces ((size_t)6);
        lprintf (Heading);
        Print_Spaces(40 - strlen(Heading));
        lprintf ("Page %d\n",PageNo);
        Print_Spaces ((size_t)(26+INDENT_SPACES));
        for (x=strlen(Heading) ; x ; x-- ) {
            lprintf ("=");
        }
        lprintf ("\n\n");
        /*
         *  Also print command line on first page
         */
        if (PageNo == 1) {
            lprintf ("COMMAND LINE: ");
            for (x=0 ; x < my_argc ; x++ ) {
                lprintf ("%s ",my_argv[x]);
            }
            lprintf ("\n\n");
            LineNo -= 2;
	}
    }
    linepos = 0;
    Print_Spaces((size_t)INDENT_SPACES);
    DBG (("END_OF_LINE",0x201,"Exit: LineNo=%d, PageNo=%d",LineNo,PageNo));
}

/*============================================================== PRINT_ENTRY */
PRIVATE
void    Print_Entry(name)
/*      ~~~~~~~~~~~
 *  This routine is used to print a single cross-reference entry
 *  for the -W and -Y options.
 *---------------------------------------------------------------------------*/
char    *name;
{
    int     length;

    DBG (("PRINT_ENTRY",0x201,"Enter: name=%s",name));
    if ( ! linepos) {
        linepos = Wflag ? 10 : 20;
        Print_Spaces((size_t)linepos);
    }
    lprintf ("%-15s",name);
    for (length = strlen(name)+1; length > 0 ; length -= 15, linepos+= 15)
        ;
    if (linepos > 64) {
        End_Of_Line();
        linepos = 0;
    }
    DBG (("PRINT_ENTRY",0x201,"Exit: name=%s",name));
    return;
}

/*======================================================== PRINT_DEPENDS */
PRIVATE
void    Print_Depends(t)
/*      ~~~~~~~~~~~~
 *  Process the symbol tree, and print out the desired results.
 *  What is actually printed will depend on which flags have been
 *  set while analysing the parameters.
 *------------------------------------------------------------------------*/
XDEFPTR t;
{
    LISTPTR fp,np;

    if (! t) return;

    DBG(("PRINT_DEPENDS",0x401,"Enter: node at %ld, name='%s'",(long)t, t->node.name));
    if (t->node.prev) {
        DBG (("PRINT_DEPENDS",0x108,"... following prev link to node at %ld",(long)t->node.prev));
        Print_Depends((XDEFPTR)t->node.prev);
    }
    DBG(("PRINT_DEPENDS",0x401,"Process data for node at %ld, name='%s'",(long)t, t->node.name));
    /*
     *  Handle cross-reference listings first line
     */
    if (Wflag) {
        lprintf ("%-15s",t->node.name);
        if (fp = (LISTPTR)First_Node((NODEPTR)t->xdef)) {
            if (! Next_Node((NODEPTR)fp)) {
                lprintf (" defined in %-15s",fp->node.name);
            } else {
                lprintf(" Multiple defines");
                for ( fp=(LISTPTR)First_Node((NODEPTR)t->xdef) ;
                            np=(LISTPTR)Next_Node((NODEPTR)fp) ;
                                    fp = (LISTPTR)Next_Node((NODEPTR)fp)) {
                    DBG(("PRINT_DEPENDS",0x108,"%s  :Multiple defines %s and %s\n",t->node.name, fp->node.name,np->node.name));
                    fprintf (Lflag ? stderr : listfp," in %s and %s", fp->node.name, np->node.name);
                    End_Of_Line();
                }
            }
        } else {
            lprintf ("   %27s","**** not defined ****");
            DBG(("PRINT_DEPENDS",0x401,"Exit: NO file entry for node at %ld, name='%s'",(long)t, t->node.name));
        }
        End_Of_Line();
    }  /* End of Wflag */
    if (Yflag) {
        lprintf ("%s",t->node.name);
        End_Of_Line();
        lprintf ("%20s","Xdefs: ");
        linepos = 20;
        /*
         *  Process list of XDEF references to this node if Yflag set
         */
        DBG (("PRINT_DEPENDS",0x108,"... Printing XDEF cross-reference"));
        if (! t->xdef)  lprintf("**** NO XDEF's IN THIS MODULE ****");
        for (fp = (LISTPTR)First_Node((NODEPTR)t->xdef); fp ;
                    fp = (LISTPTR)Next_Node((NODEPTR)fp)) {
            Print_Entry(fp->node.name);
        }
        if (linepos)  End_Of_Line();
    }
    /*
     *  Process list of XREF references to this node
     */
    if (Yflag && t->xref) {
        DBG (("PRINT_DEPENDS",0x108,"... Printing XREF cross-reference"));
        lprintf ("%20s","Xrefs: ");
        linepos = 20;
    }
    for (fp = (LISTPTR)First_Node((NODEPTR)t->xref); fp ;
                    fp = (LISTPTR)Next_Node((NODEPTR)fp)) {
        DBG (("PRINT_DEPENDS",0x108,"... fp=%ld '%s')", (long)fp, fp->node.name));
        /*
         *  File Dependency list
         */
        if (Lflag ) {
            /* Allow for multiple definitions ! */
            for (np = (LISTPTR)First_Node((NODEPTR)t->xdef); np ;
                        np = (LISTPTR)Next_Node((NODEPTR)np)) {
                DBG (("PRINT_DEPENDS",0x108,"... checking '%s'(%ld) against '%s'(%ld)",np->node.name,(long)np->node.name,fp->node.name,(long)fp->node.name));
                if (np->node.name != (char *)fp->node.name) {
                    DBG (("PRINT_DEPENDS",0x108,"... writing file dependency entry for %s on %s",fp->node.name,np->node.name));
                    lprintf("%s %s\n", fp->node.name, np->node.name);
                }
            } /* end of for 'xref' loop */
        }
        /*
         *  Cross references detail lines
         */
        if (Wflag || Yflag) Print_Entry(fp->node.name);
    } /* end of for 'xdef' loop */
    if ((Wflag || Yflag) && linepos) {
        End_Of_Line();
    }
nextlevel:
    if (t->node.next) {
        DBG (("PRINT_DEPENDS",0x108,"... following next link to node at %ld",(long)t->node.next));
        Print_Depends((XDEFPTR)t->node.next);
    }
    DBG(("PRINT_DEPENDS",0x401,"Exit: node at %ld, name='%s'",(long)t, t->node.name));
    return;
}


/*======================================================== ADD_LIST */
PRIVATE
char   *Add_List(listptr,s)
/*      ~~~~~~~~
 * Add an entry to a list.
 *
 * If an entry is found to be already on the list then it is
 * not added again.
 *-----------------------------------------------------------------*/
LISTPTR *listptr;
char *s;
{
    LISTPTR p;

    DBG(("ADD_LIST",0x801,"Enter: *listptr(%ld)=%ld,s='%s'",(long)listptr,(long)*listptr,s));
    if (! (p=(LISTPTR)Find_Node ((NODEPTR)*listptr, s))) {
        p = (LISTPTR)Add_Node ((NODEBASE)listptr, OwnerNode, s, sizeof(LISTSTRUCT));
    }
    DBG(("ADD_LIST",0x801,"Exit: p->node.name='%s'",p->node.name));
    return(p->node.name);
}


/*========================================================= MAKE_XDEF */
PRIVATE
XDEFPTR    Make_Xdef(s)
/*         ~~~~~~~~~
 * This routine creates a new definition node and then adds it
 * to the global definition tree. It sets the inter-node links
 * and fills in the name, but leaves the other pointers NULL.
 * If a node of the correct name exists, then it merely exits
 * with a pointer to that node.
 *------------------------------------------------------------------*/
char *s;
{
    XDEFPTR n;

    DBG(("MAKE_XDEF",0x801,"Enter: s='%s'",s));
    if (! (n = (XDEFPTR)Find_Node((NODEPTR)XdefTree,s))) {
        DBG(("MAKE_XREF",0x404,"... no suitable XDEF, so create new one"));
        n = (XDEFPTR) Make_Node ((NODEPTR *)&XdefTree, s, sizeof (struct XDEF_NODE));
        n->file = (char *)NULL;
        n->xdef = (LISTPTR)NULL;
        n->xref = (LISTPTR) NULL;
        n->section = (char *)NULL;
    }
    DBG (("MAKE_XDEF",0x801,"Exit: n=%ld",n));
    return(n);
}


#ifdef FREE_AT_END
/*========================================================= XDEF_NODE_FREE */
PRIVATE
void    Xdef_Node_Free  (node)
/*      ~~~~~~~~~~~~~~
 *  This routine is called from Free_Nodes() when about to free the XDEF
 *  node given in the parameter.  It frees any additonal resources above
 *  those common to all node types.
 *
 *  This routined must free any XDEF or XREF trees attaached to this node,
 *  and also any strings attached to this node
 *----------------------------------------------------------------------*/
XDEFPTR   node;
{
    DBG(("XDEF_NODE_FREE",0x801,"Enter: node=%ld, name='%s'",node,node->node.name));
    if (node->file) {
        DBG(("XDEF_NODE_FREE",0x801,"... free node->file entry at %ld(%s)",node->file,node->file));
        Kill_String (node->file);
    }
    if (node->section) {
        DBG(("XDEF_NODE_FREE",0x801,"... free node->section entry at %ld(%s)",node->section,node->section));
        Kill_String (node->section);
    }
    if (node->xdef) {
        DBG(("XDEF_NODE_FREE",0x801,"... free node->xdef tree at %ld",node->xdef));
        Free_Nodes ((NODEPTR)node->xdef, (void *)NULL);
    }
    if (node->xref) {
        DBG(("XDEF_NODE_FREE",0x801,"... free node->xref tree at %ld",node->xref));
        Free_Nodes ((NODEPTR)node->xref, (void *)NULL);
    }
    DBG(("XDEF_NODE_FREE",0x801,"Exit: (for node=%ld)",node));
    return;
}


/*============================================================ FREE_XDEFS */
PRIVATE
void    Free_Xdefs  ()
/*      ~~~~~~~~~~
 *  This routine is used to free the XDEF tree and all associated nodes
 *  and memory.
 *----------------------------------------------------------------------*/
{
    DBG(("FREE_XDEFS",0x401,"Enter"));
    Free_Nodes ((NODEPTR)XdefTree, Xdef_Node_Free);
    XdefTree = (XDEFPTR)NULL;
    DBG(("FREE_XDEFS",0x401,"Exit"));
    return;
}
#endif /* FREE_AT_END */


/*========================================================== ANALYSE_FILE */
PRIVATE
int     Process_File ()
/*      ~~~~~~~~~~~~
 *  Read through the file.   We need to build up the following tables:
 *      a)  A local symbol table for this file.  This is used
 *          to convert the "Id" fields back to meaningful names.
 *      b)  A list of names in this module that are visible to
 *          other modules.
 *      c)  A list of names used by this module that need to be found
 *          in other modules.
 *------------------------------------------------------------------------*/
{
    int     lastch = 0;
    XDEFPTR xdef;

    DBG(("ANALYSE_FILE",0x801,"Enter: File position = %d",(int)ftell(libfp)));
    Free_Ids();
    while ((lastch = inchar(libfp)) != EOF) {
        switch (lastch) {
        default:
                /* Ignore any characters that are not start of directives */
                continue;
        case SROFF_FLAG:
                DBG(("ANALYSE_FILE",0x801,"SROFF Directive - file position=%d",(int)ftell(libfp)-1));
                if ((lastch = inchar(libfp)) == EOF) {
                    break;
                }
                switch(lastch) {
                default:
                        DBG(("ANALYSE_FILE",0x801,"... unrecognised %d",lastch));
                        eprintf ("%s: unrecognised directive %d\n",_prog_name,lastch);
                        return (EOF);
                case SROFF_FLAG:
                        DBG(("ANALYSE_FILE",0x801,"... FB - another SROFF_FLAG"));
                        break;
                case SROFF_SOURCE:
                        DBG(("ANALYSE_FILE",0x801,"... SOURCE Directive"));
                        Module = Make_String(Get_String());
                        if (vflag  && strcasecmp(File,Module)) {
                            eprintf ("... module %s\n",Module);
                        }
                        if (Lflag && libflag) {
                            lprintf("%s %s\n", Module, Module);
                        }
                        if (Yflag) {
                            filemodname[0] = '\0';
                            if (! libflag) {
                                strcat (filemodname, File);
                                strcat (filemodname,"/");
                            }
                            strcat (filemodname, Module);
                        } else {
                            strcpy(filemodname,Module);
                        }
                        DBG(("ANALYSE_FILE",0x801,"... filemodname='%s'",filemodname));
                        break;
                case SROFF_COMMENT:
                        DBG(("ANALYSE_FILE",0x801,"... COMMENT Directive"));
                        Get_String();
                        if (vflag) {
                            eprintf ("...COMMENT: %s\n",String);
                        }
                        break;
                case SROFF_ORG:
                        DBG(("ANALYSE_FILE",0x801,"... ORG Directive"));
                        Skip_Chars (libfp,(size_t)4);
                        break;
                case SROFF_OFFSET:
                        DBG(("ANALYSE_FILE",0x801,"... OFFSET Directive"));
                        Skip_Chars(libfp, (size_t)4);           /* LongWord */
                        break;
                case SROFF_SECTION:
                        DBG(("ANALYSE_FILE",0x801,"... SECTION Directive"));
                        if (Section)
                            Kill_String(Section);
                        Section = Make_String(Get_Id());
                        break;
                case SROFF_XDEF:
                        DBG(("ANALYSE_FILE",0x801,"... XDEF Directive"));
                        /* Add name to list for this module */
                        if (Yflag) {
                            DBG(("ANALYSE_FILE",0x808,"... XDEF node filemodname='%s'",filemodname));
                            xdef = Make_Xdef(filemodname);
                            Add_List (&(xdef->xdef), Get_String());
                        } else {
                            xdef = Make_Xdef(Get_String());
                            Add_List (&(xdef->xdef), libflag ? Module : File);
                        }
                        Skip_Chars(libfp, (size_t)4);           /* LongWord */
                        xdef->section = Get_Id();
                        break;
                case SROFF_XREF:
                        DBG(("ANALYSE_FILE",0x801,"... XREF Directive"));
                        Skip_Chars(libfp, (size_t)(4+1)); /* Skip longword + Truncation Rule */
                        while ((lastch = inchar(libfp)) != EOF) {
                            if (lastch == SROFF_FLAG)
                                break;
                            Get_Id();
                            if (Id >= 0){       /* Do not bother if Section Name */
                                if (Yflag) {
                                    DBG(("ANALYSE_FILE",0x808,"... XDEF node filemodname='%s'",filemodname));
                                    xdef = Make_Xdef(filemodname);
                                    Add_List (&(xdef->xref), IdText);
                                } else {
                                    DBG(("ANALYSE_FILE",0x808,"... XDEF node name from Idtext='%s'",IdText));
                                    xdef = Make_Xdef(IdText);
                                    Add_List (&(xdef->xref), libflag ? Module : File);
                                }
                            }
                        }
                        break;
                case SROFF_DEFINE:
                        DBG(("ANALYSE_FILE",0x801,"... DEFINE Directive"));
                        Id = ((inchar(libfp) << 8) + inchar(libfp));
                        Add_Id(Id,Get_String());
                        break;
                case SROFF_COMMON:
                        DBG(("ANALYSE_FILE",0x801,"... COMMON Directive"));
                        Skip_Chars (libfp, (size_t)2);            /* Id */
                        break;
                case SROFF_END:
                        DBG(("ANALYSE_FILE",0x801,"... END Directive"));
                        Free_Ids();
                        break;
                }   /* end of SROFF_FLAG switch */
                DBG(("ANALYSE_FILE",0x808,"... finished handling SROFF directive"));
        }   /* End of main switch */
    } /* end of file loop */

    /*
     *  Release memory that has been used for the local symbol table
     */

    /* Returns EOF on end of file */
    DBG(("ANALYSE_FILE",0x801,"Exit: EOF"));
    return(EOF);
}


/*=================================================== LIBRARY_ANALYSIS */
int     Library_Analysis()
/*      ~~~~~~~~~~~~~~~~
 *---------------------------------------------------------------------*/
{
    char    filename[100];
    LISTPTR  flist;

    DBG(("LIBRARY_ANALYSIS",0x101,"Enter"));

    libflag = filelist && (filelist->node.next==(NODEPTR)NULL);

    if (Wflag) {
        Heading = "XDEF CROSS-REFERENCE LIST";
    }
    if (Yflag) {
        Heading = "FILE/MODULE CROSS-REFERENCE LIST";
    }
    if (Wflag || Yflag) {
        End_Of_Line();              /* Dummy to get first heading */
    }
    /*
    * Work through each file in turn from the filelist.
    */
    for (flist = filelist ; flist ; flist=(LISTPTR)flist->node.next) {

        DBG(("LIBRARY_ANALYSIS",0x104,"Processing file '%s'",flist->node.name));
        strcpy(filename,flist->node.name);
        if (vflag) {
            eprintf ("Processing file %s\n",filename);
        }
        File = filename;
        if ((libfp = Open_Sroff_File(filename))==(FILE *)NULL) {
            DBG(("LIBRARY_ANALYSIS",0x101,"file ignored (not SROFF format!)"));
            if (vflag) {
                eprintf("... file ignored as not SROFF format\n");
            }
            continue;
        }
        if (Sflag  || Aflag) {
            Analyse_Mode();
        } else {
            Process_File();
        }
        fclose (libfp);
        libfp = NULL;
    } /* end of for loop */
    DBG(("LIBRARY_ANALYSIS",0x101,"PRINT DEPENDENCIES"));
    if (Lflag) {
        DBG(("LIBRARY_ANALYSIS",0x101,"Lflag set - do DEPENDENCIES"));
        if (vflag) eprintf("Creating dependency lists\n");
    }
    if (Wflag) {
        DBG(("LIBRARY_ANALYSIS",0x101,"Wflag set - do CROSS-REFERENCE LIST"));
        if (vflag) eprintf("Creating XDEF Cross-Reference list\n");
    }
    if (Yflag) {
        DBG(("LIBRARY_ANALYSIS",0x101,"Wflag set - do CROSS-REFERENCE LIST"));
        if (vflag) eprintf("Creating FILE/MODULE Cross-Reference list\n");
    }

    if (Wflag || Yflag) {
        End_Of_Line();      /* Force initial headings */
    }
    Print_Depends(XdefTree);
    if (Lflag) {
        if (! libflag) {
            /*
             *  Make certain that we know files depend on themselves !
             */
            for (flist = filelist ; flist ; flist=(LISTPTR)flist->node.next) {
                lprintf("%s %s\n", flist->node.name, flist->node.name);
            }
        }
    }

    if (! Lflag) {
        /*
         *  Add Final form feed
         */
        lprintf ("\f");
    }

#ifdef FREE_AT_END
    if (vflag) eprintf ("Freeing temporary memory areas\n");
    Free_Xdefs();
#endif /* FREE_AT_END */

    DBG(("LIBRARY_ANALYSIS",0x101,"Exit"));
    return (0);
}
