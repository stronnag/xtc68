# Makefile definitions to use xtc68 in a GNU make 'Makefile'
# add the line (no quotes) 'include /usr/local/qdos/etc/ql.mak' as the
# first line of the makefile. Then just 'make' normally.

CC = qcc
LD = qld
AS = as68
CPP = qcpp

%.o : %.s
	$(CC) $(ASFLAGS) -c $< -o $@
 
