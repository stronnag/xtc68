PROG = qcpp

APFLAGS= -DXTC68 -DCPLUSPLUS

CFLAGS += -O2 $(APFLAGS) -DPREFIX="$(prefix)"
LDFLAGS =

OBJ = version.o cexp.o cccp.o appdir.o

all: 	$(PROG)

$(PROG): $(OBJ)
	$(CC) $(CFLAGS) -oqcpp $(OBJ)

clean:
	rm -f *.o qcpp *.exe

install: $(PROG)
	install -d $(prefix)/bin
	install $(PROG) $(prefix)/bin
