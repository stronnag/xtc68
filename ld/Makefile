VPATH = ../

OBJS = ld.o

# We force an ia32 application to avoid having to fix numerous long assumptions
CFLAGS += -DXTC68 -O2 -fsigned-char
LDFLAGS =

PROG = qld

all: 	$(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

ld.o:	ldold.c
	$(CC) $(CFLAGS) -c -o ld.o ldold.c
clean:
	rm -f *.o $(PROG)
