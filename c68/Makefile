#	Makefile for c68 and c386
#
#	N.B.  Some modules will generate empty source files
#		according to the #define statements in config.h
#
#	As well as entries for c68 and c386, there are also
#	entries for the compilers that are typically used to
#	boot the c68/c386 compilers for the first time.

RM=rm -f
#CFLGS=  -g
CFLGS=  -DNDEBUG -O2

# cxref flags
CXFLAGS=-d -l -s

# indent flags
INDENTFLAGS= -bad -br -ce -di8 -i4 -npsl \
	-TADDRESS -TAMODE -TBITSIZE -TBLOCK -TBOOL -TBTYPE -TCHAR -TCODE \
	-TCONDITION -TCSE -TDEEP -TEXPR -TEXPRTYPE -TFLAGS -TILEN \
	-TIVAL -TLABEL -TLEVEL -TLINE -TMODEL -TMSGNUM -TOPCODE -TOPTENUM \
	-TOPTION -TOPTIONS -TPEEPFLAGS -TPEEPINFO -TQUALIFIER -TRANGE \
	-TREG -TREGBITMAP -TREGLIST -TREGMASK -TREGTYPE -TREGUSAGE -TRVAL \
	-TSEQNUM -TSIZE -TSTATE -TSTATUS -TSTMT -TSTMTTYPE -TSTORAGE \
	-TSTRING -TSWITCH -TSYM -TTABLE -TTARGET -TTOKEN -TTYP -TUSES -TUVAL \
	-TVISIBILITY

# File suffix seperator
S=.
# Object file extension
O=o

PROG=c68

#------------------------------------------------------------------------------
# Use the following if compiling c68/c386 with c68 on Minix
# CCFLAGS= -Qwarn=8 -Qerror=3 -Qstackopt=3 -D__STDC__=1
# LDFLAGS= -m250000
#------------------------------------------------------------------------------
# Use the following if compiling c68/c386 with c386 on Minix
# CCFLAGS= -Qwarn=8 -Qerror=3 -Qstackopt=3 -D__STDC__=1
# LDFLAGS= -m250000
#------------------------------------------------------------------------------
# Use the following if compiling c68 with ACK (68k version)
# CCFLAGS=
# LDFLAGS= -m250000
#------------------------------------------------------------------------------
# Use the following if compiling c68/c386 with GNU C on Minix
# CCFLAGS=  -Wall -ansi -pedantic -Wshadow -Wpointer-arith \
#	-Wwrite-strings -Wcast-qual -Wwrite-strings -Wcast-align \
#	-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
#	-Wredundant-decls -Wnested-externs
# LDFLAGS= -s -m250000
#------------------------------------------------------------------------------
# Use the following if compiling c386 with bcc on Minix
# CCFLAGS=
# LDFLAGS=
#------------------------------------------------------------------------------
# Use the following if compiling c68 with c68 on QDOS
#CCFLAGS= -Qmaxerr=3 -Qwarn=8 -Qerror=3 -Qstackopt=maximum
#LDFLAGS = -bufp300K -ms
#S=_
#------------------------------------------------------------------------------
# Use the following if compiling c86 with TopSpeed on DOS
# and then do
#	MAKE sibo		to build both versions
#	MAKE c86check		to build code checking only version
#	MAKE c86code		to build code check and generation version
#CC=tsc /zq /fpC86.PR
#O=obj
#RM=del
#------------------------------------------------------------------------------
# Use the following if compiling c86 with GNU C
# CCFLAGS=  -Wall -ansi -pedantic -Wshadow -Wpointer-arith \
#	-Wwrite-strings -Wcast-qual -Wwrite-strings -Wcast-align \
#	-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations \
#	-Wredundant-decls -Wnested-externs
#------------------------------------------------------------------------------
CCFLAGS=-O2
LDFLAGS += $(CFLAGS)
CFLAGS += $(CCFLAGS) $(CFLGS)


#------------------------------------------------------------------------------
#	Source files common to all versions of c68/c386
SRC1 = 	analyze$(S)c cglbdef$(S)c cmain$(S)c decl$(S)c expr$(S)c extern$(S)c
SRC2 =	genicode$(S)c genstmt$(S)c genutil$(S)c getsym$(S)c init$(S)c
SRC3 =	intexpr$(S)c  list$(S)c memmgt$(S)c msgout$(S)c optimize$(S)c
SRC4 =	outgen$(S)c stmt$(S)c symbol$(S)c system$(S)c types$(S)c
SRC =	$(SRC1) $(SRC2) $(SRC3) $(SRC4)

HDR = 	chdr$(S)h cglbdec$(S)h config$(S)h expr$(S)h message$(S)h \
	outproto$(S)h version$(S)h proto$(S)h check$(S)h

#	Source files specific to c68 680X0 versions
SRC68K =flow68k$(S)c gen68k$(S)c genffp$(S)c genieee$(S)c peep68k$(S)c\
	out68k_a$(S)c out68k_c$(S)c out68k_g$(S)c out68k_q$(S)c reg68k$(S)c
HDR68K =gen68k$(S)h

#	Source files specific to INTEL versions
SRCX86 =peepx86$(S)c regx86$(S)c \
	outx86_a$(S)c outx86_b$(S)c outx86_g$(S)c outx86_n$(S)c outx86_s$(S)c
HDRX86 =genx86$(S)h

#	Source files specific to c386 INTEL 386 versions
SRC386 =gen386$(S)c
HDR386 =gen386$(S)h

#	Source files specific to c386 INTEL 386 versions
SRC86  =gen86$(S)c
HDR86  =gen86$(S)h

#	Source files specific to carm ARM versions
SRCARM =genarm$(S)c peeparm$(S)c outarm_o$(S)c regarm$(S)c
HDRARM =genarm$(S)h

#	Source files specific to c30 TI TMS320C30 versions
SRCC30 =flowc30$(S)c genc30$(S)c peepc30$(S)c outc30_r$(S)c regc30$(S)c
HDRC30 =genc30$(S)h

#	Source files specific to PowerPC versions
SRCPPC =genppc$(S)c peepppc$(S)c outppc$(S)c regppc$(S)c
HDRPPC =genppc$(S)h

#	Source files specific to the EPOC build enviroment
SRCEPOC=c86$(S)c c86$(S)pic c86$(S)afl c86$(S)shd c86$(S)pr \
	config$(S)chk  config$(S)gen config$(S)bat

#	Extra files
SRCEXTR=cmains$(S)c c86${S}dsw c86${S}dsp c68${S}dsw c68${S}dsp \
	config$(S)c86 config${S}c68 config$(S)txt

#------------------------------------------------------------------------------
#	Object files common to all c68/c386 variants
OBJ = 	analyze$(S)$(O) cglbdef$(S)$(O) cmain$(S)$(O) decl$(S)$(O) expr$(S)$(O) extern$(S)$(O) \
	genicode$(S)$(O) genstmt$(S)$(O) genutil$(S)$(O) getsym$(S)$(O) init$(S)$(O) \
	intexpr$(S)$(O)  list$(S)$(O) memmgt$(S)$(O) msgout$(S)$(O) optimize$(S)$(O) \
	 outgen$(S)$(O) stmt$(S)$(O) symbol$(S)$(O) system$(S)$(O) types$(S)$(O)

#	Object files specific to Motorola 680X0 variants
OBJ68K =flow68k$(S)$(O) gen68k$(S)$(O) genffp$(S)$(O) genieee$(S)$(O) \
	peep68k$(S)$(O) out68k_a$(S)$(O) out68k_c$(S)$(O) out68k_g$(S)$(O) \
	out68k_q$(S)$(O) reg68k$(S)$(O)

#	Object files specific to INTEL variants
OBJX86 =peepx86$(S)$(O) regx86$(S)$(O) \
	outx86_a$(S)$(O) outx86_b$(S)$(O) outx86_g$(S)$(O) \
	outx86_n$(S)$(O) outx86_s$(S)$(O)

#	Object files specific to INTEL 386 variants
OBJ386 =gen386$(S)$(O)

#	Object files specific to INTEL 86 variants
OBJ86  =gen86$(S)$(O)

#	Object files specific to ARM variants
OBJARM =genarm$(S)$(O) peeparm$(S)$(O) outarm_o$(S)$(O) regarm$(S)$(O)

#	Object files specific to TI TMS320C30 versions
OBJC30 =flowc30$(S)$(O) genc30$(S)$(O) peepc30$(S)$(O) outc30_r$(S)$(O) \
	regc30$(S)$(O)

#	Object files specific to PowerPC versions
OBJPPC =genppc$(S)$(O) peepppc$(S)$(O) outppc$(S)$(O) regppc$(S)$(O)

#	All object files
OBJS   =$(OBJ) $(OBJ68K) $(OBJX86) $(OBJ386) $(OBJ86) $(OBJARM) $(OBJC30) \
	$(OBJPPC)

#------------------------------------------------------------------------------
#	The following is only required if your library does not
#	already contain this routine.
# SRCLIB = vfprintf$(S)c
# OBJLIB = vfprintf$(S)o
#	Use this setting if you already have vfprintf
SRCLIB =
OBJLIB =

#------------------------------------------------------------------------------
LNS    =$(OBJS:.o=.ln)
CXS    =$(OBJS:.o=.cx)

.SUFFIXES:	$(S)ln $(S)cx $(S)obj

#------------------------------------------------------------------------------
all:		$(PROG)

$(S)c$(S)obj:
		$(CC) $(CFLAGS) $*

$(S)c$(S)ln:
		lint $(CFLAGS) -m -u -c $<

$(S)c$(S)cx:
		cxref $(CXFLAGS) -C $<

$(PROG):	$(OBJS)
		$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIB)

$(OBJ):		$(HDR)
$(OBJ68K):	$(HDR) $(HDR68K)
$(OBJX86):	$(HDR) $(HDR86)  $(HDRX86)
$(OBJ386):	$(HDR) $(HDR386) $(HDRX86)
$(OBJ86):	$(HDR) $(HDR86) $(HDRX86)
$(OBJARM):	$(HDR) $(HDRASM)
$(OBJC30):	$(HDR) $(HDRC30)
$(OBJPPC):	$(HDR) $(HDRPPC)

lint:		$(LNS)
		lint $(LNS)

lclint:
		lclint $(SRC)

cxref:		$(CXS)
		cxref $(CXFLAGS) -c $(CXS)

clobber:	clean
		$(RM) *$(S)exe
		$(RM) *$(S)img
clean:
		$(RM) *$(S)cx
		$(RM) *$(S)ln
		$(RM) *$(S)$(O)
		$(RM) *$(S)i
		$(RM) *$(S)s
		$(RM) *$(S)uue
		$(RM) *$(S)rs?
		$(RM) *$(S)rzc
		$(RM) $(PROG)

#------------------------------------------------------------------------------
# Special section to handle building programs
# under the SIBO SDK (TopSpeed) compiler on DOS
# This uses completely different syntax to virtually
# all the other options.

TSOBJS   =$(OBJ) $(OBJX86) $(OBJ86)

sibo:  c86check.img c86code.img
c86code:   c86code.img
c86check:  c86check.img

c86check.img : checkok c86.rss $(TSOBJS)
		tsc /m c86
		copy c86.img c86check.img

c86code.img : codeok c86.rss $(TSOBJS)
		tsc /m c86
		copy c86.img c86code.img
checkok:
		config config.chk
codeok:
		config config.gen

c86.rss : c86.c message.h config.h
		$(RM) c86.i
		$(RM) c86.rss
		tsc c86.c /option(pre_proc=on) /zq
		mv c86.i c86.rss
		rcomp16 c86.rss
		rchuf hwim -v -ic86.rsc

#------------------------------------------------------------------------------
# reformat source files to ensure consistant style
indent:
		@for file in $(SRC) $(SRC68K) $(SRCX86) $(SRC386) \
			$(SRC86) $(SRCARM) $(SRCC30) $(SRCPPC) \
			$(HDR) $(HDR68K) $(HDRX86) $(HDR386) $(HDR86) \
			$(HDRARM) $(HDRC30) $(HDRPPC) ; do \
			echo $$file:; \
			indent $(INDENTFLAGS) $$file && rm $$file~; \
		done

#------------------------------------------------------------------------------
# build the whole compiler as one huge .c file.   This can make the
# resulting binary smaller on some systems.
c68s:	$(SRC) $(HDR) $(HDR68K)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ cmains$(S)c $(LIB)

#------------------------------------------------------------------------------
# updating david via email
david:	zip
	uuencode $(PROG)src$(S)zip $(PROG)src$(S)zip \
		| mail -s "Latest $(PROG)" d.j.walker@x400.icl.co.uk
	rm -f $(PROG)src$(S)zip

#------------------------------------------------------------------------------
# putting source on floppy to take home
home:
	tar cvf /var/tmp/$(PROG)$(S)tar \
	    Makefile $(SRCEPOC) \
	   $(SRC) $(SRC68K) $(SRCX86) $(SRC386) $(SRC86) $(SRCARM) $(SRCC30) \
	   $(SRCPPC) $(SRCEXTR) \
	   $(HDR) $(HDR68K) $(HDRX86) $(HDR386) $(HDR86) $(HDRARM) $(HDRC30) \
	   $(HDRPPC)
	gzip /var/tmp/$(PROG)$(S)tar
	rcp /var/tmp/$(PROG)$(S)tar$(S)gz melody:/var/tmp/$(PROG)$(S)tgz
	rm /var/tmp/$(PROG)$(S)tar$(S)gz
	rsh melody dd if=/var/tmp/$(PROG)$(S)tgz of=/dev/rdsk/f03ht bs=18k
	rsh melody rm /var/tmp/$(PROG)$(S)tgz

#------------------------------------------------------------------------------
# putting source on floppy
floppy:
	tar zcvf /dev/fd0 \
	   Makefile $(SRCEPOC) \
	   $(SRC) $(SRC68K) $(SRCX86) $(SRC386) $(SRC86) $(SRCARM) $(SRCC30) \
	   $(SRCPPC) $(SRCEXTR) \
	   $(HDR) $(HDR68K) $(HDRX86) $(HDR386) $(HDR86) $(HDRARM) $(HDRC30) \
	   $(HDRPPC)

#------------------------------------------------------------------------------
# putting source on 720K floppies to transfer to Atari ST minix system
floppy720:
	tar cvf /dev/fd0 [a-fh-o]??*$(S)c ???*$(S)h c86$(S)*
	@echo -n "Insert 2nd diskette:"; read DUMMY
	tar cvf /dev/fd0 [g]??*$(S)c
	@echo -n "Insert 3nd diskette:"; read DUMMY
	tar cvf /dev/fd0 [p-z]??*$(S)c

#------------------------------------------------------------------------------
# creating a uuencoded gziped tar archive
tar:
	tar cvf $(PROG)$(S)tar Makefile $(SRCEPOC) \
	   $(SRC) $(SRC68K) $(SRCX86) $(SRC386) $(SRC86) $(SRCARM) $(SRCC30) \
	   $(SRCPPC) $(SRCEXTR) \
	   $(HDR) $(HDR68K) $(HDRX86) $(HDR386) $(HDR86) $(HDRARM) $(HDRC30) \
	   $(HDRPPC)
	gzip $(PROG)$(S)tar
	uuencode $(PROG)$(S)tar$(S)gz $(PROG)$(S)tgz >$(PROG)$(S)uue
	rm -f $(PROG)$(S)tar$(S)gz

#------------------------------------------------------------------------------
# creating a zip archive
zip:
	zip $(PROG)src$(S)zip Makefile $(SRCEPOC) \
	   $(SRC) $(SRC68K) $(SRCX86) $(SRC386) $(SRC86) $(SRCARM) $(SRCC30) \
	   $(SRCPPC) $(SRCEXTR) \
	   $(HDR) $(HDR68K) $(HDRX86) $(HDR386) $(HDR86) $(HDRARM) $(HDRC30) \
	   $(HDRPPC)

#------------------------------------------------------------------------------
# creating a pkzip archive
# (because of limitations on command line length
#  we have to build up the archive in bits!)
pkzip:
	pkzip $(PROG)src$(S)zip Makefile $(SRC1)
	pkzip $(PROG)src$(S)zip $(SRC2)
	pkzip $(PROG)src$(S)zip $(SRC3)
	pkzip $(PROG)src$(S)zip $(SRC4)
	pkzip $(PROG)src$(S)zip $(SRC68K)
	pkzip $(PROG)src$(S)zip $(SRCX86) $(SRC386) $(SRC86)
	pkzip $(PROG)src$(S)zip $(SRCARM) $(SRCC30) $(SRCPPC)
	pkzip $(PROG)src$(S)zip $(SRCEXTR)  $(SRCEPOC)
	pkzip $(PROG)src$(S)zip $(HDR)
	pkzip $(PROG)src$(S)zip $(HDR68K) $(HDRX86) $(HDR386) $(HDR86) $(HDRARM) $(HDRC30) $(HDRPPC)
