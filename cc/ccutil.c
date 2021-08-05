/*
 *						ccutil.c
 *						~~~~~~~~
 *
 *	 Utility routines for use in the CC front-end for
 *	"C68 Compilation system for QDOS" and "CPOC for the Psion 3a"
 *
 * (c) Copyright 1991-1996 David J. Walker
 *	   Email:  d.j.walker@x400.icl.co.uk
 * *
 *	   Permission to copy and/or distribute granted under the
 *	   following conditions:
 *
 *	   1). This notice must remain intact.
 *	   2). The author is not responsible for the consequences of use
 *		   this software, no matter how awful, even if they
 *		   arise from defects in it.
 *	   3). Altered version must not be represented as being the
 *		   original software.
 *
 */

#include <unistd.h>
#include <stdarg.h>
#include "cc.h"

#ifdef JDBG
#include <stdio.h>
/*
 *	Simple alternative to handle DBG macros when the
 *	'libdebug' library is not available.
 */
void	jdbg (char *proc, int x, char *ctl,...)
{
	va_list va;
	va_start (va, ctl);

	fputs(proc, stdout);
	fputs(": ", stdout);

	vprintf (ctl, va);
	va_end (va);
	fputc ('\n', stdout);
	return;
}
#endif /* JDBG */


#ifndef HAVE_BASENAME
/*=============================================================== BASENAME */
char   *basename ( fname )
/*		~~~~~~~~
 *
 * Routine to return the base name of any filename.
 * (Cheats in this implementation as we know we should
 * end in "_ext".  We actually also allow for ".ext".
 *------------------------------------------------------------------------*/
char *fname;
{
#ifndef QDOS
	return FilePart(fname);
#else
	int got_under;

	char *p = &fname[strlen(fname)-1];

	for( got_under = 0; p > fname && got_under < 2; p--) {
		if( strchr("_.",*p)) {
			got_under++;
		}
	}
	return (p == fname ? p : p+2);
#endif /* QDOS */
}
#endif /* ! HAVE_BASENAME */


/****************************************************************** STRPOS */
/*
 *      s t r p o s
 *
 *  Routine to search a string for occurence of a specified
 *  character and return its position in the string.
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  26 Jan 95   DJW   - Added 'const' keyword to parameter definitions
 */

int strpos (s, c)
  const char * s;
  int    c;
{
    char *reply;

    if ((reply = strchr (s, c))==NULL) {
        return -1;
    }
    return (int)(reply - s);
}


#ifndef HAVE_LIBGEN

/******************************************************************
 **	The next few routines are used if you do not have the LIBGEN **
 ** library that is now a standard part of Unix SVR4.            **
 ** the system() call always seems to return 0 on WIN32 systems  **
 *****************************************************************/

/*
 *      c _ e s c
 *
 *  Table giving list of C escape characters,
 *  and their corresponding internal values
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  03 Sep 94   DJW   - First version.  A number of library functions use
 *                      this string, so it might as well be present only once.
 */

unsigned char _C_esc[] = "\n\t\f\v\r\a\b\\";
unsigned char _C_esc_a[] = "ntfvrab\\";

/*
 *     c _ hex
 *
 *  Table giving list of C hex and octal characters,
 *  and their corresponding internal values
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  03 Sep 94   DJW    - First version.  A number of library functions use
 *                       this string, so it might as well be present only once.
 *
 *  07 Nov 94   DJW    - Added 8 and 9 to the Hex string - they had been
 *                       omitted!    Problem reported by David Gilham.
 *
 */

unsigned char _C_oct_a[] = "01234567";
unsigned char _C_oct[] = {0,1,2,3,4,5,6,7,'\0'};

unsigned char _C_hex_a[] = "0123456789ABCDEFabcdef";
unsigned char _C_hex[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,10,11,12,13,14,15,'\0'};

/****************************************************************** _STREADD */
/*
 *      _ s t r e a d d _ c
 *
 *  Support routine used by the strecpy() and streadd().
 *  Can also be used directly from elsewhere within the library.
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  29 Aug 94   DJW   - First version
 *
 *  24 Jan 95   DJW   - Missing 'const' keyword added to function definition.
 */


char *  __streadd ( char *       output,
                    const char * input,
                    const char * exceptions,
                    const char * escape)
{
    int c, i;

    do {
        c = (int)(unsigned char)*input++;
        if (isprint(c))  {
            if (escape && (strchr(escape,c) != NULL)) {
                *output++ = '\\';
            }
            *output++ = (char)c;
            continue;
        }
        if (exceptions && strchr(exceptions, c)) {
            *output++ = (char)c;
            continue;
        }
        *output++ = '\\';
        if ((i=strpos((char *)_C_esc,c)) >= 0) {
            *output++ = _C_esc_a[i];
            continue;
        }
        *output++ = (char)('0' + (c >> 6));
        c &= 0x3f;
        *output++ = (char)('0' + (c >> 3));
        *output++ = (char)('0' + (c & 0x7));
    } while (*input);

    *output = '\0';
    return (output);
}


/****************************************************************** STRECPY */
/*
 *      s t r e c p y _ c
 *
 *  Unix compatible routine to copy a string expanding
 *  non-graphic characters to their equivalent C language
 *  escape sequencesr.  A NULL byte is appended to the end.
 *  The target area must be large enough to hold the result,
 *  but an area 4 times the size of the source is guaranteed
 *  to be large enough.
 *
 *  The exceptions string contains any characters that are
 *  not to be expanded.
 *
 *  Returns a pointer to the start of the resulting target
 *  string (cf streadd()).
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  29 Aug 94   DJW   - First version
 */

char *  strecpy (char *        output,
                 const char *  input,
				 const char *  exceptions)
{
    (void) __streadd(output, input, exceptions,"\\");
    return (output);
}


/****************************************************************** STREADD */
/*
 *      s t r e a d d _ c
 *
 *  Unix compatible routine to copy a string expanding
 *  non-graphic characters to their equivalent C language
 *  escape sequencesr.  A NULL byte is appended to the end.
 *  The target area must be large enough to hold the result,
 *  but an area 4 times the size of the source is guaranteed
 *  to be large enough.
 *
 *  The exceptions string contains any characters that are
 *  not to be expanded.
 *
 *  Returns a pointer to the NULL byte at the end of the
 *  target string (cf strecpy()).
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  29 Aug 94   DJW   - First version
 */

char *  streadd (char *        output,
                 const char *  input,
                 const char *  exceptions)
{
    return __streadd(output, input, exceptions,"\\");
}


#if 0
/****************************************************************** STRCADD */
/*
 *      s t r c a d d _ c
 *
 *  Unix compatible routine to copy a string compressing
 *  the C language escape sequences to the equivalent
 *  character.  A NULL byte is appended to the end.  The
 *  target area must be large enough to hold the result,
 *  but an area the same size as the source is guaranteed
 *  to be large enough.  It is also possible for the input
 *  and output areas to be the same to do an in-situ compress.
 *
 *  Returns a pointer to the NULL byte terminating the target.
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  29 Aug 94   DJW   - First version
 */


char *  strcadd (char * output, const char * input)
{
    int    i, j, k, c;

    do {
        c = (int)(unsigned char) *input++ ;
        if (c != '\\') {
            *output++ = (char)c;
            continue;
        }
        c = (int)(unsigned char) *input++;
        /*
         *  Allow for octal sequences
         */
        if (strpos((char *)_C_oct_a,c) >= 0) {
            for (i=0,j=0; (j < 3) && i<32 && (k=strpos((char *)_C_oct_a,c)) >= 0; j++) {
                i = 8*i + _C_oct[k];
                c = (int)(unsigned char) *input++;
            }
            input--;
            *output++ = (char)i;
            continue;
        }
        /*
         *  allow for hexadecimal sequences
         */
        if (c == 'x') {
            c = (int)(unsigned char) *input++;
            for (i=0,j=0; (j < 2) && i<16 && (k=strpos((char *)_C_hex_a,c)) >= 0; j++) {
                i = 16*i + _C_hex[k];
                c = (int)(unsigned char) *input++;
            }
            input--;
            *output++ = (char)i;
            continue;
        }
        if ((i=strpos((char *)_C_esc_a,c)) >= 0) {
            *output++ = _C_esc[i];
            continue;
        }
        *output++ = (char)c;

    } while (*input);

    *output = '\0';
    return (output);
}

/****************************************************************** STRCCPY */
/*
 *      s t r c c p y _ c
 *
 *  Unix compatible routine to copy a string compressing
 *  the C language escape sequences to the equivalent
 *  character.  A NULL byte is appended to the end.  The
 *  target area must be large enough to hold the result,
 *  but an area the same size as the source is guaranteed
 *  to be large enough.   It is also possible for the input
 *  and output areas to be the same to do an in-situ compress.
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  29 Aug 94   DJW   - First version
 */

char *  strccpy (char * output, const char * input)
{
    (void) strcadd(output, input);
    return (output);
}
#endif


#endif /* HAVE_LIBGEN */


/****************************************************************** MY_ERROR */
#ifndef QDOS
void my_error(int p1,char * p2)
{
	errno = p1;
#ifndef EPOC
	perror(p2);
#endif /* EPOC */
	exit(p1);
}

/********************************************************************** FILEPART */
char *FilePart(char *fnam)
{
	char *p = fnam;
	char *q = fnam + strlen(fnam);

	for(;q != p; --q)
	{
		if(*q == ':' || *q == DSEP)
		{
			q++;
			break;
		}
	}
	return q;
}

/********************************************************************** ADDPART */
void AddPart(char *d, char *s, int mlen)
{
	int m,n;
	m = strlen(d);
	n = strlen(s);
	if(mlen < m+n+2)
	{
		my_error(5, "Addpart len");
	}
	else
	{
		if(*(d+m-1) != DSEP && *s != DSEP)
		{
			if(*d)
				(void)strcat(d, DSEPS);
		}
		(void) strcat(d, s);
	}
}



/****************************************************************** STRPBRK */
#ifdef EPOC
char *strpbrk(const char *str, const char *pat)
{
	char * p;
	for (p = (char *)str ; *p ; p++)
	{
		if(strchr(pat, *p))
		{
			return p;
		}
	}
	return NULL;
}
#endif /* EPOC */

/******************************************************************* STRRPBRK */
char *strrpbrk(char *str, char *pat)
{
	char *q,*p;
	q = p = str+strlen(str);
	while (--p != str)
	{
		if(strchr(pat, *p))
		{
			q = p;
			break;
		}
	}
	return q;
}

#define _mkname(p1,p2) strcpy(p1,p2)

#endif /* ! QDOS */

/*****************************************************************
 **	The next few routines are to get around the fact that       **
 ** the system() call always seems to return 0 on WIN32 systems **
 ****************************************************************/

#if 0
/****************************************************************** ARGFREE */
/*
 *      a r g f r e e
 *
 *  Release memory associated with an ARGV array.
 *
 *  This routine releases all the memory that is
 *  involved in setting up an argv style array via the
 *  argvunpack() routine.  It is designed to be called
 *  either if an error occurs building up the array, or
 *  alternatively when the array is no longer required.
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  04 Sep 94   DJW   - First version.
 */

void  argfree (char *** argvptr)
{
    char ** ptr = *argvptr;

    for ( ptr = *argvptr ; *ptr ; ptr++) {
        free (*ptr);
    }
    free (*argvptr);
    *argvptr = NULL;
}

/****************************************************************** ARGUNPACK */
/*
 *      a r g u n p a c k
 *
 *  Unpack arguments.
 *
 *  This routine takes a command line and creates
 *  an array of argument pointers to strings (suitable
 *  for passing as the argv[] parameter to a program.,
 *
 *  The routine can be passed either an existing array
 *  of pointers (whose memory must have been allocated
 *  via malloc) or a NULL pointer.  In both cases any
 *  additional memory will be allocated dynamically.
 *
 *  Optionally a pointer to a function that is responsible
 *  for handling any further processing of any of the
 *  strings a function can be passed as a parameter.
 *
 *  Returns:
 *          -1      Error occurred.  All memory associated with
 *                  the array being built up will have been released.
 *
 *  AMENDMENT HISTORY
 *  ~~~~~~~~~~~~~~~~~
 *  03 Sep 94   DJW   - First version.   This is based on the code that was
 *                      in the old _cmdparse() routine.
 */


int   argunpack (const char *   cmdline,
                 char **  argvptr[],
                 int *    argcptr,
                 int (* function)(char *, char ***, int *))
{
    int     reply;
    size_t  len;
    char sc;
    const char *param;
    char *dup;

    /*
     *  See if we are setting up initial array
     *  If so initialise arrays
     */
    if (*argvptr == NULL) {
        *argvptr = malloc (sizeof (char *));
        if (*argvptr == NULL) {
            return -1;
        }
        *argcptr = 0;
    }
    /*
     *  We now work down the command line
     *  chopping out each argument in turn.
     *  Any surrounding quotes are removed.
     */
    for ( ; ; ) {
        /*
         *  Get past any leading white space
         */
        while(isspace((unsigned char)*cmdline)) {
            cmdline++;
        }
        if (*cmdline == '\0') {
            break;
        }
        /*
         *  We next work out what character will terminate
         *  the argument.  We assume space of NULL character
         *  unless the next character is single or double
         *  quotes, in which case this becomes the character
         *  that terminates the argument.
         */
        sc = ' ';
        switch (*cmdline) {
        case '\'' :
        case '"'  :
                /* Next argument is string - store terminator */
                sc = *cmdline++;
                break;
        default:
                break;
        }

        /*
         *  Parse until terminating character reached
         *  Any character preceded by a backslash escapes
         *  the next characte which allows for embedded
         *  quote characters to be allowed through.
         *
         * Allow for premature termination by NULL byte.
         */
        for ( param = cmdline ; *cmdline  ; cmdline++ ) {
            /*
             *  If escape character, then skip
             *  over next character as well
             */
            if (*cmdline == '\\') {
                cmdline++;
                continue;
            }
            if ((*cmdline == sc)
            ||  (sc == ' ' && isspace((unsigned char)*cmdline)) ) {
                break;
            }
        }
        /*
         *  Create a copy of the string.
         *  Also compress any escape sequences.
         */
        len = cmdline - param;
        dup = (char *)malloc(len + 1);
        if (dup == NULL){
            argfree (argvptr);
            return (-1);
        }
        sc = *cmdline;                  /* Save terminating character */
        *((char *)cmdline) = '\0';      /* Change it to NULL byte */
        (void) strccpy (dup, param);    /* copy compressing escape seq. */
        *((char *)cmdline) = sc;        /* restore terminating character */
        /*
         *  If we have a function to do wild card handling
         *  then call it.   If we do not, or if the function
         *  returns zero, then add it anyway.
         */
        reply = 0;
        if ((function == NULL)
        ||  ((reply = (*function)(dup, argvptr, argcptr)) == 0)) {
            char * newptr;
            newptr = realloc (*argvptr, (size_t)(((*argcptr) + 2) * sizeof (char *)));
            if (newptr == NULL) {
                argfree(argvptr);
                return -1;
            } else {
                *argvptr = (char **)newptr;
            }
            (*argvptr)[*argcptr] = dup;
            *argcptr += 1;
        } else {
            free (dup);
        }
        /*
         *  If we had a failure, then exit here
         */
        if (reply < 0) {
            argfree (argvptr);
            return -1;
        }
        if (*cmdline++ == '\0') {
            break;
        }
    }
    /*
     * Ensure the last argument is followed
     *  ny an entry containing NULL
     */
    (*argvptr)[*argcptr] = NULL;
    return (*argcptr);
}
#endif

/****************************************************************** ARGUNPACK */
/*
 *		a r g p a c k
 *
 *	Pack arguments.
 *
 *	This routine takes an array of argument pointers,
 *	and packs this into a single string suitable for
 *	passing as a command line to a program.
 *
 *	It is optional whether an argument should have
 *	additional characters added to protect embedded
 *	spaces and special characters or not.  Whether
 *	this is wanted is specified by the 'flag' parameter.
 *
 *	The memory to hold the resulting command line is
 *	allocated dynamically (using malloc).  The routine
 *	exits with a pointer to the resulting string
 *	or NULL if an error occurs.
 *
 *	AMENDMENT HISTORY
 *	~~~~~~~~~~~~~~~~~
 *	28 Aug 94	DJW   - First version.	 Some of the code is based upon
 *						ideas contributed by Erling Jacobsen.
 *
 *	02 jan 95	DJW   - Added missing 'const' qualifier to function declaration
 *
 *  06 dec 96   DJW   - Added backslash to list of characters that need
 *                      escaping when packing arguments.
 */

char * argpack (char * const * argv,
			    int 		   flag)
{
	char *	cmdline, *cmdlinenew;
	size_t	cmdlen;
	char *	workbuf = NULL;

	/*
	 *	Assume initially that we could
	 *	have a zero length command line.
	 */
	if ((cmdline = malloc((size_t)2)) == NULL) {
		DBG(("ARGPACK",0x101,"Exit: failed to allocate initial cmdline buffer"));
		return (NULL);
	}
	cmdline[0] = '\0';
	cmdlen = 0;

	/*
	 *	Now try and add any arguments
	 */
	while (*argv != NULL) {
		DBG(("ARGPACK",0x204,"argv=$%x, *argv=$%x(%s)",argv, *argv, *argv));
		/*
		 *	Add a space seperator if not first parameter
		 */
		if (cmdlen != 0) {
			(void)strcat (cmdline," ");
		}
		/*
		 *	Allocate temporary buffer to do
		 *	(potential) parameter expansion.
		 *	Play safe with size allocated!
		 */
		free (workbuf);
		if ((workbuf = malloc ((strlen(*argv) * 4) + 3)) == NULL) {
			DBG(("ARGPACK",0x101,"Exit: failed to allocate workbuf"));
			free (cmdline);
			return NULL;
		}
		/*
		 *	Get (expanded) parameter into workbuf
		 */
		if (flag == 0) {
			(void) strcpy(workbuf,*argv);
		} else {
			int 	whitespace;
			unsigned char * ptr;
			char *dest = workbuf;

			for (ptr = (unsigned char *)*argv, whitespace = 0 ; *ptr ; ptr++) {
				if (isspace(*ptr)) {
					whitespace = 1;
					break;
				}
			}
			if (whitespace) {
				*dest++ = '"';				/* add leading quote if needed */
			}
			dest = __streadd(dest,*argv,NULL,"\"\'\\");
			if (whitespace) {
				*dest++ = '"';				/* add trailing quote if needed */
			}
			*dest = '\0';					/* add terminator byte */
		}
		/*
		 *	Increase size of buffer used to build
		 *	command line by amount needed to hold
		 *	the next (expanded) parameter to be
		 *	added and then add it to end.
		 */
		cmdlen = strlen(cmdline) + strlen(workbuf) + 2;	/* Allow for NUL + space */
		if ((cmdlinenew = realloc (cmdline,cmdlen)) == NULL) {
			DBG(("ARGPACK",0x101,"Exit: failed to extend cmdline"));
			free (workbuf);
			free (cmdline);
			return NULL;
		}
		DBG(("ARGPACK",0x204,"Adding new argument of '%s' to cmdline", workbuf));
		cmdline = strcat (cmdlinenew,workbuf);
		DBG(("ARGPACK",0x204," ... giving line as follows:\n%s", cmdline));
		/*
		 *	Try for another one
		 */
		argv++;
	}
	/*
	 *	Free work buffer before returning
	 */
	free(workbuf);
	DBG(("ARGPACK",0x201,"Exit with cmdline as follows:\n%s", cmdline));
	return (cmdline);
}

#ifdef QDOS
/*=========================================================== CHECK_CPU */
int 	CheckCPU(void)
/*		~~~~~~~~
 *	This function gets the CPU type.  We need to do this
 *	check because the GWASS assembler requires at least
 *	a 68020 to be able to run.
 *----------------------------------------------------------------------*/
{
	return (*((char *)(_sys_var + 0xa1)));
}
#endif /* QDOS */

/*================================================================= PRINTSTR */
void	printstr ( char * str, ...)
/*		~~~~~~~~
 *	Print a variable number of strings to the error channel.
 *	The array of strings will be NULL terminated.
 *---------------------------------------------------------------------------*/
{
	va_list  ap;
    int res;

	DBG(("PRINTSTR",0x11,"Enter"));
	va_start(ap, str);
	while(str != NULL) {
		DBG(("PRINTSTR",0x18,"   %s",str));
#ifdef EPOC
		if (WriteToParentSetup() != NULL)
		{
			WriteToParent(str,strlen(str)+1);
		}
		else
#else
        res = write( STDERR_FILENO, str, strlen(str));
#endif
		str = (char *)va_arg(ap, char *);
	}
	va_end(ap);
	DBG(("PRINTSTR",0x11,"Exit"));
	return;
}
