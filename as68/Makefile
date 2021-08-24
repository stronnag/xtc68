VPATH = ../

GCFLAGS = -O2
CFLAGS +=   $(GCFLAGS) -DXTC68 -DLABELDIFF -DUSELINE -DMINIX -DHOST_LITTLE_ENDIAN
LDFLAGS =

OBJS = cbuf.o cpy.o gen.o hdr.o lex.o main.o ops.o \
	 opt.o parse.o pass.o scan.o sym.o cross.o

all: 	as68

as68:    $(OBJS)
	$(CC) $(CFLAGS) -O2 -s -oas68 $(OBJS)

cbuf.o:   cbuf.c jas.h
cpy.o:    cpy.c jas.h
gen.o:    gen.c jas.h
hdr.o:    hdr.c jas.h
lex.o:    lex.c jas.h scan.h parse.h
main.o:   main.c jas.h
ops.o:    ops.c jas.h opcodes.h
opt.o:    opt.c jas.h
parse.o:  parse.c jas.h parse.h
pass.o:   pass.c jas.h
scan.o:   scan.c jas.h scan.h parse.h
sym.o:    sym.c jas.h

clean:
	rm -f *.o as68
