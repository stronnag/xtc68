/***************************************************************
 *
 *              D E C L A R A T I O N S
 *
 ***************************************************************/

#ifndef _PROTOTYPE
#ifdef __STDC__
#define _PROTOTYPE(params) params
#else
#define _PROTOTYPE(parama) ()
#endif
#endif

#ifndef LATTICE
#define _PARAMLIST ...
#else
#define _PARAMLIST
#endif

extern int(*string_cmp) _PROTOTYPE((const char *, const char *));
/*
 *  sblmain
 */
void exits _PROTOTYPE((int));
void error _PROTOTYPE((int, _PARAMLIST));
int inchar _PROTOTYPE((FILE *));
void outchar _PROTOTYPE((int, FILE *));
void get_basename _PROTOTYPE((char *));
char *has_extension _PROTOTYPE((char *));
FILE *open_file _PROTOTYPE((char *, char *));
/*
 *  slbmodul
 */
int create_mode _PROTOTYPE((void));
int Add_Mode _PROTOTYPE((void));
int replace_mode _PROTOTYPE((void));
int delete_mode _PROTOTYPE((void));
int Extract_Mode _PROTOTYPE((void));
int split_mode _PROTOTYPE((void));
int get_modulename _PROTOTYPE((FILE *, char *));
int copy_module _PROTOTYPE((char *, FILE *, FILE *, FILE *));
LISTPTR search_list _PROTOTYPE((char *));
int remove_list _PROTOTYPE((char *));
/*
 *  slbanal
 */
void Free_Ids _PROTOTYPE((void));
void Add_Id _PROTOTYPE((short, char *));
char *Find_Id _PROTOTYPE((short));
char *Get_Id _PROTOTYPE((void));
char *Get_LongWord _PROTOTYPE((void));
char *Get_TruncRule _PROTOTYPE((void));
int Analyse_Mode _PROTOTYPE((void));
void Start_Directive _PROTOTYPE((void));
void Data_Char _PROTOTYPE((void));
/*
 *  slbdis
 */
long disasm_module _PROTOTYPE((FILE *, char *));
/*
 *  slbrefs
 */
int Library_Analysis _PROTOTYPE((void));
int Process_Sroff _PROTOTYPE((FILE *, FILE *));
void Print_Spaces _PROTOTYPE((size_t));
/*
 *  slbutil
 */
char *Allocate_Memory _PROTOTYPE((size_t));
void Copy_Chars _PROTOTYPE((FILE *, FILE *, size_t));
FILE *Open_Sroff_File _PROTOTYPE((char *));
int Is_Sroff _PROTOTYPE((FILE *));
void eprintf _PROTOTYPE((char *, _PARAMLIST));
void lprintf _PROTOTYPE((char *, _PARAMLIST));
char *Get_String _PROTOTYPE((void));
#define Skip_Chars(fp, size) Copy_Chars((FILE *)NULL, fp, size)

NODEPTR Add_Node _PROTOTYPE((NODEBASE, NODEPTR, char *, size_t));
NODEPTR Make_Node _PROTOTYPE((NODEBASE, char *, size_t));
NODEPTR Find_Node _PROTOTYPE((NODEPTR, char *));
NODEPTR Next_Node _PROTOTYPE((NODEPTR));
NODEPTR First_Node _PROTOTYPE((NODEPTR));
void Kill_Node _PROTOTYPE((NODEPTR));
void Free_Nodes _PROTOTYPE((NODEPTR, void (*func)(IDPTR)));
char *Make_String _PROTOTYPE((char *));
char *Find_String _PROTOTYPE((char *));
void Kill_String _PROTOTYPE((char *));
#define String_Save(s) Make_String(s)

extern int extract_mode(void);
