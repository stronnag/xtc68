VPATH = ../

OBJS = ldold.o appdir.o

# We force an ia32 application to avoid having to fix numerous long assumptions
CFLAGS += -DXTC68 -O2 -fsigned-char
LDFLAGS =

PROG = qld

all: 	$(PROG)

%.o : %.c
	$(CC) $(CFLAGS) -DPREFIX="$(prefix)" -c $< -o $@

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $(PROG) $(OBJS)

clean:
	rm -f *.o $(PROG) *.exe

install: $(PROG)
	install -d $(prefix)/bin
	install $(PROG) $(prefix)/bin
