# Makefile definitions to use xtc68 in a GNU make 'Makefile'
# add the line (no quotes) 'include $PATH_TO/ql.mak' as the
# first line of the makefile. Then just 'make' normally.
# where $PATH_TO represents the path to the installed 'ql.mak'

CC = qcc
LD = qld
AS = as68
CPP = qcpp
AR = qdos-ar
RANLIB = qdos-ranlib



%.o : %.s
	$(CC) $(ASFLAGS) -c $< -o $@
