BIN =

APFLAGS=  -DXTC68 -DCPLUSPLUS
CFLAGS= $(APFLAGS) -O2 -m32
#VPATH= ../

OBJ = version.o cexp.o cccp.o

all: 	qcpp

qcpp: $(OBJ)
	$(CC) $(CFLAGS) -oqcpp $(OBJ)

clean:
	rm -f *.o qcpp
