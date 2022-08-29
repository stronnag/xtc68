# Top level makefile
# Note you can build 32bit binaries on x86-64 by
# CFLAGS=-m32 make

DIRS = as68 c68  cc  cpp  ld  slb tools
CLEANDIRS = $(DIRS:%=clean-%)
INSTALLDIRS = $(DIRS:%=install-%)

prefix ?= /usr/local
CFLAGS += -Wall -Wextra -pedantic

export CFLAGS
export prefix

all: $(DIRS)
$(DIRS):
	$(MAKE) -C  $@

clean: $(CLEANDIRS)

$(CLEANDIRS):
	$(MAKE) -C $(@:clean-%=%) clean

install: $(INSTALLDIRS)

$(INSTALLDIRS):
	$(MAKE) -C $(@:install-%=%) install

.PHONY: subdirs $(DIRS)
.PHONY: subdirs $(CLEANDIRS)
.PHONY: all clean install
