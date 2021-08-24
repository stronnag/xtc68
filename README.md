xtc68
=====

Cross compiler for QDOS c68 on POSIX platforms

This is a resurrection of the 1999-ish code base to compile on modern POSIX systems (well Linux at least). No effort has gone into updating the obsolete DOS and NT variants.

As of 2021-08-24, the codebase is 64bit clean. There is no longer any requirement to build 32bit executables.

## Runtime

You need the C68 runtime binary distribution. A good place to start looking is http://www.dilwyn.me.uk/c/index.html. If runtime disk1 is installed under `support`, the `install.sh` script will install a usable environment, such that:

```
qcc -o hw hw.c
```
will generate a QDOS executable, where `hw.c` is a trivial, standard "Hello World" application, assuming `/usr/local/bin` is on `$PATH`.

It is not purpose of this repo to provide a QDOS c68 environemnt, nor does it offer any help for QDOS development; it mere maintains a cross compiler.
