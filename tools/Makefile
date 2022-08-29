# Simple copy tools for xtc68 and UQLX
# updates obsolete verions in UQLX

prefix ?= /usr/local/bin

MACHINE := $(shell $(CC) -dumpmachine)
ifneq (, $(findstring mingw, $(MACHINE)))
 WLIB=-l wsock32
endif

all: qls qcp

qls: qls.c
	$(CC) -Wall -O2 -o $@ $< $(WLIB)

qcp: qcp.c
	$(CC) -Wall -O2 -o $@ $< $(WLIB)

clean:
	rm -f qls qcp

install: qls qcp
	install -d $(prefix)/bin
	install qls qcp $(prefix)/bin
