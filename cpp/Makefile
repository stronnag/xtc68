BIN =

APFLAGS=  -DXTC68 -DCPLUSPLUS

CFLAGS +=  -O2 $(APFLAGS)
LDFLAGS =

OBJ = version.o cexp.o cccp.o

all: 	qcpp

qcpp: $(OBJ)
	$(CC) $(CFLAGS) -oqcpp $(OBJ)

clean:
	rm -f *.o qcpp
