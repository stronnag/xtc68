/*      s l b u t i l _ c
 *
 *      This module contains a number of routines that are
 *      use by a number of the other modules.  The tend to be
 *      utility routines for manipulating parts of an SROFF
 *      file, or for maintaining lists.
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
 */

#include    "slb.h"
#include <stdarg.h>
#include <assert.h>


/*================================================== ALLOCATE_MEMORY */
char   *Allocate_Memory (n)
/*      ~~~~~~~~~~~~~~~
 * Allocate the memory requested.
 * If not possible then output error message and give up.
 *------------------------------------------------------------------*/
size_t  n;
{
    char *p;

    DBG(("ALLOCATE_MEMORY",0x2001,"Enter: size=%d",n));
//    assert (stackcheck());
    if ( ! (p = (char *)malloc((size_t)n))) {
        DBG(("ALLOCATE_MEMORY",0x2001,"ERROR: alloation failed"));
        exit(ENOMEM);
    }
    DBG(("ALLOCATE_MEMORY",0x2001,"Exit: address=%d",p));
    return(p);
}


/*=============================================================== COPY_CHARS */
void    Copy_Chars (outfp, infp, count)
/*      ~~~~~~~~~~
 *  Simply copy the specified number of characters between the
 *  two files.  If the target file is passed as NULL, this acts
 *  as a 'skip' routine.
 *---------------------------------------------------------------------------*/
FILE    *outfp, *infp;
size_t  count;
{
    DBG(("COPY_CHARS",0x1001,"Enter: outfp=%ld, infp=%ld, count=%d",outfp,infp,count));
    while (count-- > 0) {
        inchar(infp);
        if (outfp) outchar(ch, outfp);
    }
    DBG(("COPY_CHARS",0x1001,"Exit"));
}


/*=================================================== OPEN_SROFF_FILE */
PUBLIC
FILE *  Open_Sroff_File (s)
/*      ~~~~~~~~~~~~~~~
 * Open a file for input.
 * Check that it is a SROFF file.
 * return FP if OK, and NULL otherwise.
 *--------------------------------------------------------------------*/
char *s;
{
    FILE *fp;

    DBG (("OPEN_SROFF_FILE",0x401,"Enter: name='%s'",s));
    if ((fp = fopen(s, "rb")) == NULL) {
        DBG (("OPEN_SROFF_FILE",0x401,"Exit: failed to open file '%s'",s));
        error (4, s);
    }
    if (! Is_Sroff (fp)) {
        fclose(fp);
        DBG (("OPEN_SROFF_FILE",0x401,"Exit: '%s' not an SROFF open file",s));
        error (17,s);
    }
    DBG (("IS_SROFF",0x401,"Exit OK"));
    return(fp);
}


/*=========================================================== IS_SROFF */
PUBLIC
int	Is_Sroff(fp)
/*      ~~~~~~~
 * Check that file is a SROFF file.
 * (or at least it starts with an FB byte !)
 *--------------------------------------------------------------------*/
FILE *fp;
{
    int  c;

    if ((c=getc(fp)) != EOF) {
        ungetc(c, fp);
    }
    return (c == 0xFB) ;
}


/*============================================================== EPRINTF */
PUBLIC
void    eprintf (char * formatstr, _PARAMLIST)
/*      ~~~~~~~
 *  This routine is used to print to the standard error  channel.
 *  Its purpose in life is to maximise code size by avoiding having
 *  to continually pass stderr as a parmeter.
 *  It takes a variable number of parameters just like printf.
 *-----------------------------------------------------------------------*/
{
    va_list ap;

    va_start(ap,formatstr);
    vfprintf (stderr, formatstr, ap);
    va_end(ap);
}


/*============================================================== LPRINTF */
PUBLIC
void    lprintf (char * formatstr,_PARAMLIST)
/*      ~~~~~~~
 *  This routine is used to print to the listing channel as long as
 *  this is the print pass.  Otherwise it does nothing.  Its purpose
 *  in life to avoid:
 *      a)  Continually testing which pass we are in.
 *      b)  Having to which pass we are in.
 *  It takes a variable number of parameters just like printf.
 *-----------------------------------------------------------------------*/
{
    va_list ap;

    va_start(ap,formatstr);
    if (printpass && listfp) {
        vfprintf (listfp, formatstr, ap);
    }
    va_end(ap);
}


/*========================================================== GET_STRING */
PUBLIC
char    *Get_String()
/*       ~~~~~~~~~~
 *  Get a string consisting of a length byte followed by text.
 *  return a pointer to this string.
 *  N.B.   next call will corrupt this string, so save it if required
 *----------------------------------------------------------------------*/
{
    static  char    buffer[260];
    char    *ptr;
    int     x;

    DBG(("GET_STRING",0x801,"Enter"));
    for (ptr = buffer, x = inchar(libfp) ; x > 0 ; x--, ptr++) {
        *ptr = inchar (libfp);
    }
    *ptr = '\0';
    String = buffer;
    DBG(("GET_STRING",0x801,"Exit: string='%s'",buffer));
    return (buffer);
}


/*========================================================= ADD_NODE */
PUBLIC
NODEPTR     Add_Node(tree, owner,s,size)
/*
 *  This is called to add a 'leaf' to a node tree when the owner node
 *  is already known.   If it is not known then Make_Node should be used
 *  instead.
 *--------------------------------------------------------------------*/
NODEBASE tree;
NODEPTR  owner;
char    *s;
size_t   size;
{
    NODEPTR n;
    int     cmp;

    DBG (("ADD_NODE",0x1001,"Enter: Tree at %ld=%ld, owner at %ld, , s='%s', size=%d",tree,*tree, owner,s,size));

    n = (NODEPTR) Allocate_Memory(size);
    n->next = (NODEPTR) NULL;
    n->prev = (NODEPTR) NULL;

    if ((tree == (NODEBASE)&StringSpace)
    &&  (! (n->name = Find_String(s)))  )  {
        DBG (("ADD_NODE",0x1008,"... new string node, so allocate string memory"));
        n->name = Allocate_Memory(strlen(s)+1);
        strcpy (n->name, s);
    } else {
        n->name = Make_String(s);
    }
    if (*tree) {
        cmp = STRCMP(s, owner->name);
        n->owner = owner;
        if (cmp < 0) {
            DBG (("ADD_NODE",0x1008,"... adding to Owner node (prev branch) at %ld",owner));
            owner->prev = n;
        } else {
            DBG (("ADD_NODE",0x1008,"... adding to Owner node (next branch) at %ld",owner));
            owner->next = n;
        }
    } else {
        DBG (("ADD_NODE",0x1008,"... making Top node in this tree at %ld",n));
        n->owner = (NODEPTR) NULL;
        *tree = n;
        }
    DBG (("ADD_NODE",0x1001,"Exit: n = %ld",n));
    return (n);
}

/*=========================================================== MAKE_NODE */
PUBLIC
NODEPTR     Make_Node(tree, s,size)
/*
 *  This is called to add a 'leaf' to a node tree when the owner node
 *  is not known.
 *--------------------------------------------------------------------*/
NODEBASE tree;
char    *s;
size_t   size;
{
    DBG (("MAKE_NODE",0x1001,"Enter: Tree at %ld=%ld, s='%s', size=%d",tree,*tree, s,size));
    Find_Node (*tree, s);
    DBG (("MAKE_NODE",0x1001,"Exit (via ADD_NODE)"));
    return (Add_Node (tree, OwnerNode, s, size));
}


/*=========================================================== FIRST_NODE */
PUBLIC
NODEPTR     First_Node (node)
/*          ~~~~~~~~~~
 *  Find the first node (alphabetically) in the tree (or branch).
 *  Basically this just involves following the 'prev' pointer until we 
 *  reach a leaf of the tree.  If there is no 'prev' branch then the
 *  given node is the first one.
 *-----------------------------------------------------------------------*/
NODEPTR node;
{
    if (! node) {
        DBG(("FIRST_NODE",0x1001,"Entry/Exit: NULL"));
        return ((NODEPTR)NULL);
    }
    DBG(("FIRST_NODE",0x1001,"Enter: node at %ld = '%s'",(void *)node,node->name));
    while (node->prev) {
        node = node->prev;
        DBG(("FIRST_NODE",0x1004,"... followed previous pointer to node at %ld = '%s'",(void *)node,node->name));
    }
    DBG(("FIRST_NODE",0x1001,"Exit: node at %ld = '%s'",(void *)node,node->name));
    return (node);
}

/*=========================================================== NEXT_NODE */
PUBLIC
NODEPTR     Next_Node (node)
/*          ~~~~~~~~~
 *  Find the next node in an existing tree.  Normally the tree will
 *  have been set up in alpahbetical order, but this is not assumed.
 *
 *  A NULL return is made if there is no 'next' node in the tree;
 *---------------------------------------------------------------------*/
NODEPTR node;
{
    if (! node) {
        DBG(("NEXT_NODE",0x1001,"Entry/Exit: NULL"));
        return ((NODEPTR)NULL);
    }
    DBG(("NEXT_NODE",0x1001,"Enter: node at %ld = '%s'",(long)node,node->name));
    /*
     *  If there is a 'next' branch of this node, then we
     *  merely need to find the first entry in that branch.
     */
    if (node->next) {
        DBG(("NEXT_NODE",0x1001,"... followed 'next' to node at %ld = '%s'",(void *)node->next));
        return (First_Node(node->next));
    }
    /*
     *  We now need to look back up the tree.  If at any time
     *  we find ourselves on a 'prev' link then that new level
     *  becomes the next node.  If we are still on a 'next'
     *  link we need to continue ascending.   If we get to the
     *  top of the tree there is no further entry.
     */
    for ( ; node->owner ; node = node->owner) {
        DBG(("NEXT_NODE",0x1001,"... node=%ld, node->owner->prev = %ld",(void *)node->owner,(void *)node->owner->prev));
        if (node == node->owner->prev) {
            DBG(("NEXT_NODE",0x1001,"Exit: node at %ld = '%s'",(void *)node,node->name));
            return (node->owner);
        }
    }
    DBG(("NEXT_NODE",0x1001,"Exit: NULL (last node)"));
    return ((NODEPTR)NULL);
}


/*=========================================================== FIND_NODE */
NODEPTR    Find_Node(tree, s)
/*         ~~~~~~~~~
 * This routine searches the given tree to see if the
 * given entry already exists.
 *
 * Return Values:
 *      NULL    Node not found
 *      other   Pointer to node.
 *
 *  'OwnerNode' will contain the owning node, or if a node
 *  does not exist the node that would be the owner. Subsequent
 *  calls will reset this value.
 *-------------------------------------------------------------------- */
NODEPTR tree;
char *s;
{
    NODEPTR n;
    int cmp;

    DBG (("FIND_NODE",0x1001,"Enter: Tree=%ld, s='%s'",tree,s));
    if (tree) {
        OwnerNode = n = tree;
        while (n && n->name) {
            OwnerNode = n;
            DBG (("FIND_NODE",0x8001,"... checking string '%s' against nodename '%s'",s, n->name));
            if (!(cmp = STRCMP(s, n->name))) {
                DBG (("FIND_NODE",0x1001,"Exit: n=%ld, OwnerNode=%ld",n, OwnerNode));
                return(n);
            } else if (cmp < 0) {
                n = n->prev;
                DBG (("FIND_NODE",0x8001,"... following prev link to %ld",n));
            } else {
                n = n->next;
                DBG (("FIND_NODE",0x8001,"... following next link to %ld",n));
            }
        }
    } else {
        DBG (("FIND_NODE",0x8001,"... empty tree - so node cannot be found"));
        OwnerNode = (NODEPTR)NULL;
    }
    DBG (("FIND_NODE",0x1001,"Exit: NOT FOUND, OwnerNode=%ld",OwnerNode));
    return((NODEPTR) NULL);
}

/*========================================================== KILL_NODE */
PUBLIC
void    Kill_Node (ptr)
/*      ~~~~~~~~~
 *  This removes a node that is a 'leaf' from the tree.
 *  It is a program error to call this routine if it is NOT
 *  a leaf.
 *---------------------------------------------------------------------*/
NODEPTR ptr;
{
    NODEPTR     owner;

    DBG(("KILL_NODE",0x1001,"Enter: (for node at %ld called '%s')",ptr,ptr->name));
#ifdef LIBDEBUG
    if (ptr->next || ptr->prev) {
        DBG(("KILL_NODE",0x1001,"**** ERROR **** Node is not a leaf"));
        return;
    }
#endif
    if (owner = ptr->owner) {
        if (owner->prev == ptr) {
            DBG(("KILL_NODE",0x1008,"Clearing LEFT node of owner at %ld (was %ld)",owner,owner->prev));
            owner->prev = (NODEPTR)NULL;
#ifdef LIBDEBUG
        } else if (owner->next != ptr) {
            DBG(("KILL_NODE",0x1001,"**** ERROR **** owner has no pointer to this node !"));
            return;
#endif
        } else {
            DBG(("KILL_NODE",0x1008,"Clearing RIGHT node of owner at %ld (was %ld",owner,owner->next));
            owner->next = (NODEPTR)NULL;
        }
#ifdef LIBDEBUG
    } else {
        DBG(("KILL_NODE",0x1008,"This was the TOP node in the tree!"));
#endif
    }
    Kill_String (ptr->name);
    DBG(("KILL_NODE",0x1008,"Freeing node memory at %ld",ptr));
//    assert (stackcheck());
    free( (void *)ptr );
    DBG(("KILL_NODE",0x1001,"Exit (for node at %ld)",ptr));
    return;
}


/*============================================================ FREE_NODES */
PUBLIC
void    Free_Nodes  (node, function)
/*      ~~~~~~~~~~
 *  This routine works through the Node Tree list (if any) clearing it down
 *  and freeing all the associated memory.   The associated function (if
 *  not NULL) is called to free any additional memory associated with a
 *  specific node type.
 *----------------------------------------------------------------------*/
NODEPTR   node;
void    (*function)();
{
    if (! node ) return;
    DBG(("FREE_NODES",0x1001,"Enter: node=%ld, name='%s', function=%ld",node,node->name,function));
    if (node->next) {
        DBG(("FREE_NODES",0x1008,"... following next node to %ld",node->next));
        Free_Nodes(node->next,function);
    }
    if (node->prev) {
        DBG(("FREE_NODES",0x1008,"... following prev node to %ld",node->prev));
        Free_Nodes(node->prev,function);
    }
    if (function) {
         DBG(("FREE_NODES",0x1001,"... calling Users extra function at %ld",function));
         (*function)(node);
    }
    Kill_Node (node);
    DBG(("FREE_NODES",0x1001,"Exit: (for node=%ld)",node));
    return;
}


/*======================================================= MAKE_STRING */
char   *Make_String (s)
/*      ~~~~~~~~~~~
 * Save a string and return its saved address.
 *-------------------------------------------------------------------*/
char *s;
{
    STRINGPTR   n;

    DBG(("MAKE_STRING",0x1001,"Enter: s='%s'",s));
    if (! (n=(STRINGPTR)Find_Node((NODEPTR)StringSpace,s))) {
        n = (STRINGPTR) Add_Node ((NODEBASE)&StringSpace, OwnerNode, s, sizeof(struct STRING_NODE));
        n->count = 0;
    }
    n->count++;
    DBG(("MAKE_STRING",0x1001,"Exit: %ld(='%s'), count=%d",n->node.name,n->node.name,n->count));
    return(n->node.name);
}

/*====================================================== FIND_STRING */
PUBLIC
char *  Find_String(s)
/*      ~~~~~~~~~~~
 *-------------------------------------------------------------------*/
char *s;
{
    STRINGPTR   n;

    if (n=(STRINGPTR)Find_Node((NODEPTR)StringSpace,s)) {
        return (n->node.name);
    }
    return ((char *)NULL); 
}

/*======================================================= KILL_STRING */
void    Kill_String (s)
/*      ~~~~~~~~~~~
 *  Reduce the count of users.
 *
 *  It is a program error if an attempt is amde to kill a string that
 *  has no users!
 *
 *  If the count reaches zero and this is a leaf, then remove the leaf
 *  and free the memory.  Recurse up if this makes the owner a leaf.
 *-------------------------------------------------------------------*/
char *s;
{
    STRINGPTR   n;
#if 0
    STRINGPTR   owner;
    char        *ptr;
#endif

    if (! s) {
        DBG(("KILL_STRING",0x1001,"*** Called with NULL pointer **"));
        return;
    }
    DBG(("KILL_STRING",0x1001,"Enter: s='%s'",s));
    if ((n=(STRINGPTR)Find_Node((NODEPTR)StringSpace,s))) {
        DBG(("KILL_STRING",0x1008,"decrementing count (was %d)",n->count));
        if (n->count) {
            n->count--;
#if 0
            while ( (! n->count)  && n->node.owner
                    && ( ! n->node.prev) && ( ! n->node.next)) {
                owner = (STRINGPTR)n->node.owner;
                ptr = n->node.name;
                Kill_Node ((NODEPTR) n);
                DBG(("KILL_STRING",0x1008,"Freeing string memory at %ld",n->node.name));
//                assert (stackcheck());
                free( (char *)ptr);
                n = owner;
            }
#endif
#ifdef LIBDEBUG
        } else {
            DBG (("KILL_STRING",0x1001,"ERROR: count = 0 trying to delete string '%s'",s));
#endif
        }
#ifdef LIBDEBUG
    } else {
        DBG (("KILL_STRING",0x1001,"ERROR: trying to delete non-existent string '%s'",s));
#endif
    }
    DBG(("KILL_STRING",0x1001,"Exit"));
    return;
}

