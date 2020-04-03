BIN =

APFLAGS=  -DXTC68 -DCPLUSPLUS

CFLAGS =  -O2 $(APFLAGS)
LDFLAGS =
ARCH := $(shell uname -m)
ifeq ($(ARCH),x86_64)
CFLAGS += -m32
LDFLAGS += -m32
endif
ifeq ($(ARCH),amd64)
CFLAGS += -m32
LDFLAGS += -m32
endif

OBJ = version.o cexp.o cccp.o

all: 	qcpp

qcpp: $(OBJ)
	$(CC) $(CFLAGS) -oqcpp $(OBJ)

clean:
	rm -f *.o qcpp
