xtc68
=====

Cross compiler for QDOS c68 on POSIX platforms

This is a resurrection of the 1999-ish code base to compile on modern POSIX systems (well Linux at least). No effort has gone into updating the obsolete DOS and NT variants.

The compilation forces -m32 to avoid having to fix numerous 32bit long assumptions.
So you need a 32bit development environment on a x86-64 machine. For Debian / Ubuntu, that means:
```
sudo apt install libc6-dev-i386
```

## Runtime

You need the C68 runtime binary distribution. A good place to start looking is http://www.dilwyn.me.uk/c/index.html. If runtime disk1 is installed under `support`, the `install.sh` script will install a usable environment, such that:

```
qcc -o hw hw.c
```
will generate a QDOS executable, where `hw.c` is a trivial, standard "Hello World" application, assuming `/usr/local/bin` is on `$PATH`.

It is not purpose of this repo to provide a QDOS c68 environemnt, nor does it offer any help for QDOS development; it mere maintains a cross compiler.
