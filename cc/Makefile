#
#       Makefile to build CC for CPOC or "C68 for QDOS"
#
#------------------------ CONFIGURATION -------------------------------
#
#   The exact facilities to be included in a particular version
#   of the front-en programcan be controlled by #defines.  Ideally
#   these are set through this makefile.  The options are:
#
#   Set one of the following to specify the desired target environment:
#   QL      This is for versions of C68 for QDOS
#           (whatever the host system)
#   CPOC    This is for versions of C86 for the Psion 3a
#           (whatever the host system)
#
#   The following are used to give information about the operating environment
#   in which the system is to be hosted:
#   QDOS        This is for versions of C68 that are hosted on QDOS or a
#               compatible system such as Minerva or SMSQ(E)
#   __unix__    The enviroment us a Unix-like system
#   DOS_LIKE    MSDOS based system
#   WIN32       Windows 95 or Windows NT based system.
#   EPOC        Psion 3a based system
#
#
#   The following allow you to control optional features of the program
#   ENVVAR      Support for environment variables is to be included.
#               Note that this requires the libraries to have getenv().
#   OBSOLETE    Builds in support for parameter options that are no
#               longer used, but were used in past versions.
#
#
#   The following options will only ever be set when trying to develop
#   or debug the CC front-end program itself:
#   LIBDEBUG    This can be set IF you have the "libdebug" library.
#   JDBG        This is an alternative that does an in-line print
#
#------------------------------------------------------------------------


PROG=cc
RM=rm -fv
ZIP=zip
D=/
S=.
LDFLAGS=
LIBS=
INDENT=indent
INDENTFLAGS= -bad -br -ce -di8 -i4 -npsl

#--------------------------------------------------------------
#     CC for QDOS when hosted on QDOS
#CFLAGS = -error=5 -warn=6 -maxerr=5 -O
#LDFLAGS = -ms
#DEFINES= -DQL -DENVVAR -DOBSOLETE
# DEFINES= -DQL -DENVVAR -DOBSOLETE -DLIBDEBUG
# LIBS= -ldebug
#S=_
#--------------------------------------------------------------
#     CPOC front-end when hosted on QDOS
#PROG=cpoc
#CFLAGS = -error=5 -warn=6 -maxerr=5 -O
#DEFINES= -DCPOC -DENVVAR
#S=_
#--------------------------------------------------------------
#     CC (XTC68 version) for QDOS when hosted on LINUX
PROG=qcc
DEFINES = -DXTC68 -DENVVAR -DHAVE_BASENAME
CFLAGS =  -O2
LDFLAGS =
ARCH := $(shell uname -m)
ifeq ($(ARCH),x86_64)
CFLAGS += -m32
LDFLAGS += -m32
endif
#--------------------------------------------------------------
#     CPOC front-end when hosted on LINUX
#CC = gcc
#PROG = cpoc_cc
#DEFINES = -DCPOC -D__unix__ -DENVVAR
#--------------------------------------------------------------
#     CC (XTC68 version) for QDOS when hosted on WINDOWS 95/NT
#PROG=qcc
#DEFINES = -DQL -DXTC68 -DWIN32 -DENVVAR
#ZIP=pkzip
#D=\
#--------------------------------------------------------------
#     CPOC front-end when hosted on WINDOWS 95/NT
#PROG=cpoc_cc.exe
#DEFINES = -DCPOC -DDOS_LIKE -DWIN32 -DENVVAR
#RM=del
#ZIP=pkzip
#D=\
#--------------------------------------------------------------
#     CC front-end when hosted on WINDOWS 3.1x/MSDOS
#PROG=qcc.exe
#DEFINES = -DCPOC -DDOS_LIKE -DENVVAR
#RM=del
#ZIP=pkzip
#D=\
#-------------------------------------------------------------
#     CPOC front-end when hosted on WINDOWS 3.1x/MSDOS
#PROG=cpoc.exe
#DEFINES = -DCPOC -DDOS_LIKE -DENVVAR
#RM=del
#ZIP=pkzip
#D=\
#-------------------------------------------------------------

SRC= cc$(S)c ccutil$(S)c

OBJ= cc.o ccutil.o

HDR= cc.h

#LNS=$(OBJ:.o=.ln)

# .SUFFIXES:	.o .ln

all: 	$(PROG)

$(PROG)	: $(OBJ)
	$(CC) -o $(PROG) $(LDFLAGS) $(OBJ) $(LIBS)

cc.o : cc.c cc.h
	$(CC) -c $(CFLAGS) $(DEFINES) cc.c

ccutil.o : ccutil.c cc.h
	$(CC) -c $(CFLAGS) $(DEFINES) ccutil.c

# .c.ln:
#	lint $(CFLAGS) -m -u -c $<

# $(OBJ) : $(HDR)

lint:	$(LNS)
	lint $(LNS)

indent:
	$(INDENT) $(INDENTFLAGS) $(SRC)
	@$(RM) *~
	$(INDENT) $(INDENTFLAGS) $(HDR)
	@$(RM) *~

clobber: clean
	@$(RM) $(PROG)
	@$(RM) *.exe

clean:
	@$(RM) debug/*.*
	@$(RM) release/*.*
	@$(RM) $(OBJ)
	@$(RM) *$(S)ln
	@$(RM) vc??.*
	@$(RM) *.obj
	@$(RM) *.o
	@$(RM) *.i
	@$(RM) *.s
	@$(RM) *.p??
	@$(RM) *.ilk
	@$(RM) *.ncb
	@$(RM) *.opt
	@$(RM) $(PROG)

zip:
	-$(RM) cc$(S)zip
	$(ZIP) cc$(S)zip $(PROG)
	$(ZIP) cc$(S)zip makefile cc$(S)pr $(SRC) $(HDR) cc$(S)1 cc$(S)ds?

zipbin:
	-$(RM) ccbin$(S)zip
	$(ZIP) ccbin$(S)zip $(PROG)

zipsrc:
	-$(RM) ccsrc$(S)zip
	$(ZIP) ccsrc$(S)zip makefile cc$(S)pr $(SRC) $(HDR) cc$(S)1 cc$(S)ds?
