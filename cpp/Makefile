PROG = qcpp

APFLAGS=  -DXTC68 -DCPLUSPLUS

CFLAGS +=  -O2 $(APFLAGS)
LDFLAGS =

OBJ = version.o cexp.o cccp.o

all: 	$(PROG)

$(PROG): $(OBJ)
	$(CC) $(CFLAGS) -oqcpp $(OBJ)

clean:
	rm -f *.o qcpp

install: $(PROG)
	install -d $(prefix)
	install $(PROG) $(prefix)
