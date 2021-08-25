# Simple copy tools for xtc68 and UQLX
# updates obsolete verions in UQLX

prefix ?= /usr/local/bin

all: qls qcp

qls: qls.c
	$(CC) -Wall -O2 -o $@ $<

qcp: qcp.c
	$(CC) -Wall -O2 -o $@ $<

clean:
	rm -f qls qcp

install: qls qcp
	install -d $(prefix)
	install qls qcp $(prefix)
