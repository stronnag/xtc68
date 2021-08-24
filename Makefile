# Top level makefile
# Note you can build 32bit binaries on x86-64 by
# CFLAGS=-m32 make

DIRS = as68  c68  cc  cpp  ld  slb
CLEANDIRS = $(DIRS:%=clean-%)

all: $(DIRS)
$(DIRS):
	$(MAKE) -C $@

clean: $(CLEANDIRS)

$(CLEANDIRS):
	$(MAKE) -C $(@:clean-%=%) clean

install:
	@echo
	@echo "**** You can use install.sh to install in /usr/local ****"

.PHONY: subdirs $(DIRS)
.PHONY: subdirs $(CLEANDIRS)
.PHONY: all clean
