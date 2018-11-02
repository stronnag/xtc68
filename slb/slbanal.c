/*      s l b a n a l _ c
 *
 * This module contains the routines for analysing an individual
 * SROFF module.
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

#include "slb.h"

PRIVATE void    Free_Name       _PROTOTYPE((IDPTR));
PRIVATE void    Get_Op          _PROTOTYPE((char *));

PRIVATE int     PrintCount;         /* print position on line */
PRIVATE char    TextBuf[20];


/*========================================================== FREE_NAME */
PRIVATE
void    Free_Name  (ptr)
/*      ~~~~~~~~~~~~
 *  This routine frees the name string set up for an ID entry.
 *  Called as a vector via the Free_Nodes() routine.
 *----------------------------------------------------------------------*/
IDPTR   ptr;
{
    DBG(("FREE_NAME",0x801,"Enter: ptr=%ld, name='%s'",(long)ptr,ptr->name));
    if (ptr->name) Kill_String (ptr->name);
    DBG(("FREE_NAME",0x801,"Exit:"));
    return;
}


/*============================================================ FREE_IDS */
PUBLIC
void    Free_Ids  ()
/*      ~~~~~~~~
 *  This routine works through the Id list (if any) clearing it down
 *  and freeing all the associated memory.
 *----------------------------------------------------------------------*/
{
    Free_Nodes((NODEPTR)IdTree,Free_Name);
    IdTree = (IDPTR)NULL;
    return;
}


/*============================================================== ADD_ID */
PUBLIC
void    Add_Id (id, text)
/*      ~~~~~~
 *  Add the id given as a parameter to the list being built up.
 *----------------------------------------------------------------------*/
short   id;
char    *text;
{
    IDPTR   ptr;
    char    idstr[20];

    DBG(("ADD_ID",0x1001,"Enter: id=%04.4x, text='%s'",id,text));
    sprintf (idstr,"%04x",((unsigned)id & 0xFFFF));
    if (! (ptr=(IDPTR)Find_Node((NODEPTR)IdTree,idstr))) {
        ptr=(IDPTR)Add_Node ((NODEBASE)&IdTree,OwnerNode,idstr,sizeof(IDSTRUCT));
    }
    ptr->name = Make_String(text);
    DBG(("ADD_ID",0x1001,"Exit"));
    return;
}

/*========================================================== FIND_ID */
PUBLIC
char    * Find_Id (id)
/*        ~~~~~~~
 *  Search the id table to see if entry present.   If it is, then
 *  return the name, otherwise return the hex string for an Id
 *--------------------------------------------------------------------*/
short   id;
{
    static  char    buffer[10];
    IDPTR   ptr;

    DBG(("FIND_ID",0x801,"Enter: id=%04.4x",id));
    /* 0 is a special case refering to Program Counter */
    if (id == 0) {
        DBG(("FIND_ID",0x801,"Exit: 'PC'"));
        return ("PC");
    }

    sprintf(buffer,"%04x",((unsigned)id & 0xFFFF));
    if (ptr = (IDPTR)Find_Node((NODEPTR)IdTree,buffer)) {
        DBG(("FIND_ID",0x801,"Exit: Node name='%s'",ptr->name));
        return (ptr->name);
    }
    sprintf (buffer,"Id=$%04x",id);
    DBG(("FIND_ID",0x801,"Exit: '%s'",buffer));
    return (buffer);              
}


/*============================================================== GET_ID */
PUBLIC
char  * Get_Id()
/*      ~~~~~~
 *  Read in an ID from the source file.
 *  If possible, convert it to a text string represenetation,
 *  otherwise return a hex string form
 * 
 *  NOTE. id = 0 is a special case refering to Program Counter relative
 *----------------------------------------------------------------------*/
{
    DBG (("GET_ID",0x801,"Enter:"));
    Id = ((inchar(libfp) << 8)  + inchar(libfp));
    DBG (("GET_ID",0x808,"... id = %04.4x",Id));
    IdText = Find_Id (Id);
    DBG (("GET_ID",0x801,"Exit: '%s'",IdText));
    return (IdText);
}


/*=========================================================== GET_LONGWORD */
PUBLIC
char  * Get_LongWord()
/*      ~~~~~~~~~~~~
 *  Read a long-word in from the source, and return it as a hex string.
 *  N.B.  The next call will overwrite the string, so save it if required.
 *-------------------------------------------------------------------------*/
{
    LongWord = ((inchar(libfp) << 24) 
                    + (inchar(libfp) << 16) 
                    + (inchar(libfp) << 8) 
                    + inchar(libfp));
    sprintf (LongText,"$%08lx",LongWord);
    DBG(("GET_LONGWORD",0xF01,"%s",LongText));
    return (LongText);
}


/*=============================================================== GET_OP */
PRIVATE
void    Get_Op (buffer)
/*      ~~~~~~
 *  Get the operator part of an XDEF and return it in the
 *  buffer provided.
 *----------------------------------------------------------------------*/
char    *buffer;
{
    DBG(("GET_OP",0xF01,""));
    switch (inchar(libfp)) {
    case '+':
    case '-':
            if (Op != ' ')  *buffer++ = Op;
            Op = (char)ch;
            strcpy (buffer,Get_Id());
            Get_Op(&buffer[strlen(buffer)]);
            break;
    default:
            buffer[0] = '\0';
            break;
    }
    return;
}


/*====================================================== GET_TRUNCRULE */
PUBLIC
char    * Get_TruncRule()
/*        ~~~~~~~~~~~~~
 *  Get the truncate rule and convert it to a text string.
 *  N. B.  the next call will corrupt the string.
 *--------------------------------------------------------------------*/
{
    char    *length, *sign, *relative, *relocate;

    DBG(("GET_TRUNCRULE",0x801,"Enter"));
    inchar (libfp);

    switch (ch & 0x07) {
    case 0x01:
            length = "BYTE";
            break;
    case 0x02:
            length = "WORD";
            break;
    case 0x04:
            length = "LONG";
            break;
    default:
            lprintf ("INVALID TRUNCATION RULE at (%02.2x) at %ld\n",
                    ch, ftell(libfp));
            lprintf (" (only one of bits 1 and 2 should be set\n");
            length = "????";
            error (20);
    }

    switch ( ch & 0x18) {
    case 0x00:
            lprintf ("INVALID SIGN COMBINATION at (%02.2x) at %ld\n",
                   ch, ftell(libfp));
            lprintf (" (one of bits 3 and 4 should be set\n");
            sign = "    ?????";
            break;

    case 0x08:
            sign = "   signed";
            break;
    case 0x10:
            sign = " unsigned";
            break;
    default:
            lprintf ("INVALID SIGN COMBINATION at (%02.2x) at %ld\n",
                   ch, ftell(libfp));
            lprintf (" (only one of bits 3 and 4 should be set\n");
            sign = "    ?????";
            break;
    }

    relative = (ch & 0x20) ? " PC Relative" : "";
    relocate = (ch & 0x40) ? " RELOCATE at runtime" : "";

    Op = ' ' ;  Get_Op (TruncOps);

    strcpy (TruncRule,"  ");
    strcat (TruncRule,length);
    strcat (TruncRule, sign);
    strcat (TruncRule, relative);
    strcat (TruncRule, relocate);
    DBG(("GET_TRUNCRULE",0x801,"Exit : '%s'",TruncRule));
    return (TruncRule);
}


/*========================================================= FLUSH_DATA */
PRIVATE
void    Flush_Data()
/*      ~~~~~~~~~~
 *
 *  This routine is used to merely flush the data buffer.
 *---------------------------------------------------------------------*/
{
    DBG(("FLUSH_DATA",0x801,""));
    while (PrintCount++ < 16)   lprintf ("   ");
    lprintf ("    %s\n",TextBuf);
    PrintCount = 0;
    return;
}

/*====================================================== START_DIRECTIVE */

void    Start_Directive ()
/*      ~~~~~~~~~~~~~~~
 *  The start of a directive has been encountered, so write out any
 *  data currently in the buffer.
 *----------------------------------------------------------------------*/
{
    DBG(("START_DIRECTIVE",0x801,""));
    if (PrintCount) {
        Flush_Data();
        lprintf ("%05.5lx ",ftell(libfp)-2);
    }
    return;
}

/*=========================================================== DATA_CHAR */

void    Data_Char()
/*      ~~~~~~~~~
 *  A standard data character needs to be printed in both hex and
 *  character format.   The hex format is printed as we go, and the
 *  character one added to a buffer for printing at the end of line.
 *----------------------------------------------------------------------*/
{
    DBG(("DATA_CHAR",0xF01,"character=%02.2x",ch));
    lprintf (" %02.2x", ch);
    TextBuf[PrintCount] = (((ch < 32) || (ch > 127)) ? '.' : ch);
    PrintCount++;
    TextBuf[PrintCount]='\0';
    if (PrintCount >= 16) Flush_Data();
    return;
}


/*======================================================== ANALYSE_MODULE*/
PRIVATE
int     analyse_module (fp, modname)
/*      ~~~~~~~~~~~~~~
 *  This routine is called to print an analysis of any module.
 *------------------------------------------------------------------------*/

FILE *  fp;
char *  modname;
{
    long    filestart;

    DBG(("ANALYSE_NODULE",0x801,"Enter: module=%s, File position = %d",modname,(int)ftell(fp)));
    Free_Ids();
    TextBuf[0] = '\0';
    
    lprintf ("SLB Module Analysis - SROFF format:  Module = %s\n\n",modname);
    filestart = ftell (fp) - strlen(modname) - 3;
    if (filestart > 0 ) {
        lprintf ("Module starts at position %ld in file %s\n",filestart,File);
    }
    lprintf ("00000 SOURCE: %s\n", modname);
    PrintCount = 0;
                   
    while ((inchar (fp)) != EOF) {
        if (PrintCount == 0)
            lprintf ("%05.5lx ",ftell(fp)-1);
        switch (ch) {
        case SROFF_FLAG:
                DBG(("ANALYSE_MODULE",0x801,"SROFF Directive - file position=%d",(int)ftell(fp)-1));
                switch (inchar (fp)) {
                default:
                        DBG(("ANALYSE_MODULE",0x801,"... unrecognised %d",ch));
                        Start_Directive();
                        lprintf ("ILLEGAL DIRECTIVE:  %02.2x at %ld", ch, ftell(fp));
                        error (20);
                case SROFF_FLAG:
                        DBG(("ANALYSE_MODULE",0x801,"... FB - another SROFF_FLAG"));
                        Data_Char();
                        break;
                case SROFF_SOURCE:
                        DBG(("ANALYSE_MODULE",0x801,"... SOURCE Directive"));
                        Start_Directive();
                        lprintf ("SOURCE: %s\n", Get_String());
                        break;
                case SROFF_COMMENT:
                        DBG(("ANALYSE_MODULE",0x801,"... COMMENT Directive"));
                        Start_Directive();
                        lprintf ("COMMENT: %s\n", Get_String());
                        break;
                case SROFF_ORG:
                        DBG(("ANALYSE_MODULE",0x801,"... ORG Directive"));
                        Start_Directive();
                        lprintf ("ORG:    %s\n", Get_LongWord());
                        break;
                case SROFF_SECTION:
                        DBG(("ANALYSE_MODULE",0x801,"... SECTION Directive"));
                        Start_Directive();
                        lprintf ("SECTION: %s\n", Get_Id());
                        break;
                case SROFF_OFFSET:
                        DBG(("ANALYSE_MODULE",0x801,"... OFFSET Directive"));
                        Start_Directive();
                        lprintf ("OFFSET:  %s\n", Get_LongWord());
                        break;
                case SROFF_XDEF:
                        DBG(("ANALYSE_MODULE",0x801,"... XDEF Directive"));
                        Start_Directive();
                        lprintf ("XDEF: %-30.30s", Get_String());
                        Get_LongWord();
                        Get_Id();
                        lprintf (" %s", IdText);
                        lprintf ("+$%x\n", LongWord);
                        break;
                case SROFF_XREF:
                        DBG(("ANALYSE_MODULE",0x801,"... XREF Directive"));
                        Start_Directive();
                        lprintf ("XREF: "); 
                        Get_LongWord();
                        Get_TruncRule();
                        lprintf(TruncOps);
                        if (LongWord || (Id < 0)) {
                            lprintf ("%c$%lx", Op, LongWord);
                        }
                        lprintf (TruncRule);
                        lprintf ("\n");
                        if (ch != 0xFB) {
                            lprintf ("Illegal XREF at %ld (char=%02.2x)\n", 
                                    ftell(fp), ch);
                            error (20);
                        }
                        break;
                case SROFF_DEFINE:
                        DBG(("ANALYSE_MODULE",0x801,"... DEFINE Directive"));
                        Start_Directive();
                        lprintf ("DEFINE:  %s", Get_Id());
                        lprintf ("  %s = %s\n", 
                                (Id < 0) ? "Section" : "Symbol", Get_String());
                        Add_Id (Id, String);
                        break;
                case SROFF_COMMON:
                        DBG(("ANALYSE_MODULE",0x801,"... COMMON Directive"));
                        Start_Directive();
                        lprintf ("COMMON:   Id = %s\n", Get_Id());
                        break;
                case SROFF_END:
                        DBG(("ANALYSE_MODULE",0x801,"... END Directive"));
                        Start_Directive();
                        lprintf ("END\n\f");
                        Free_Ids();
                        return (0);
                }
                break;
        default:
                Data_Char();
                break;
        }
    }
    DBG(("ANALYSE_MODULE",0x81,"Exit: modname=%s",modname));
    return (0);
}

/*========================================================= ANALYSE_MODE */
PUBLIC
int     Analyse_Mode ()
/*      ~~~~~~~~~~~~
 *
 *  Analyse the modules
 *
 *  This can be either an SROFF analysis, or an assembler one.
 *----------------------------------------------------------------------*/
{
    LISTPTR ptr;
    char    modname[100];
    int     reply = 0;

    DBG(("ANALYSE_MODE",0x41,"Enter: ,libfp=%ld, listfp=%ld",(long)libfp,(long)listfp));

    while (get_modulename(libfp, modname) != EOF) {
        if (libflag || search_list(modname)) {
            if (Sflag) {
                if (vflag) {
                    eprintf ("analysing module %s\n",modname);
                }
                analyse_module (libfp, modname);
            }
            if (Aflag) {
                if (vflag) {
                    eprintf ("disassembling module %s\n",modname);
                }
                disasm_module (libfp, modname);
            }
            if (! libflag) remove_list(modname);
        } else {
            /* skip over this module */
            if (vflag) {
                eprintf ("skipping module %s\n",modname);
            }
            copy_module (modname, libfp, NULL, NULL);
        }
    }
    if (! libflag) {
        for (ptr = filelist ; ptr ; ptr=(LISTPTR)ptr->node.next) {
            eprintf ("Module %s not analysed (not found)\n",ptr->node.name);
        }
    }
    DBG(("ANALYSE_MODE",0x41,"Exit"));
    return (filelist == NULL ? reply : -1);
}

