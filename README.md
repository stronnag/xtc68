xtc68
=====

Cross compiler for QDOS c68 on POSIX platforms

This is a resurrection of the 1999-ish code base to compile on modern POSIX systems (well Linux at least). No effort has gone into updating the obsolete DOS and NT variants.

The compilation forces -m32 to avoid having to fix numerous 32bit long assumptions.
So you need a 32bit development environment on a x86-64 machine. For Debian / Ubuntu, that means:
```
sudo apt install libc6-dev-i386
```
