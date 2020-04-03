#   Make file for SLB command for C68
#
#   It assumes that DATA_USE points to Device/Directory holding the
#   Source, and PROG_USE points to directory containing programs
#   making up the compilation system.


#  ... for compiling on MINIX
CFLAGS =  -I. -O2 -c
LDFLAGS =
ARCH := $(shell uname -m)
ifeq ($(ARCH),x86_64)
CFLAGS += -m32
LDFLAGS += -m32
endif

LIBS =

# Set the object file extension
# Set it to 's' for MINIX-PC, 'o' for other variants
O = o

RM  = rm
ARC = arc

PROG = slb
HDR = slb.h
OBJ =   slbmain.$O slbdecl.$O slbmodul.$O \
        slbutil.$O slbanal.$O slbdis.$O slborder.$O
SRC =   slbmain.c slbdecl.c slbmodul.c \
        slbutil.c slbanal.c slbdis.c slbrefs.c

all: 	slb

slb :   ${OBJ}
	${CC} ${LDFLAGS} -oslb ${OBJ} ${LIBS}

.c.o:
	${CC} ${CFLAGS} ${DEFINES} $<

clean :
	${RM} -f ${OBJ} *_i *_s ${PROG}

clobber : clean


archive :
	${ARC} a slb Makefile ${HDR} SLBLOG ${SRC}

#---------------------- Header file dependencies --------------------------

slbmain.$O  :   slb.h
slbdecl.$O  :   slb.h
slbmodul.$O :   slb.h
slbanal.$O  :   slb.h
slbutil.$O  :   slb.h
slbdis.$O   :   slb.h
slbrefs.$O  :   slb.h
