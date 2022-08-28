xtc68
=====

Cross compiler for QDOS c68 on POSIX platforms

This is a resurrection of the 1999-ish code base to compile on modern POSIX systems (well Linux at least). No effort has gone into updating the obsolete DOS and NT variants.

As of 2021-08-24, the codebase is 64bit clean. There is no longer any requirement to build 32bit executables.

## Runtime

You need the C68 runtime binary distribution. A good place to start looking is http://www.dilwyn.me.uk/c/index.html. If runtime disk1 is installed under `support`, the `sdk-install.sh` script will install a usable environment, such that:

```
qcc -o hw hw.c
```
will generate a QDOS executable, where `hw.c` is a trivial, standard "Hello World" application, assuming the installation directory `$(prefix)/bin`  is on `$PATH`.

It is not purpose of this repo to provide a QDOS c68 environemnt, nor does it offer any help for QDOS development; it mere maintains a cross compiler.

## Installation

On most POSIX (like) systems (Linux, *BSD, MacOS, Msys, Cygwin) to build and install the excutables.

```
# build and install the executables in /usr/local (default)
make && sudo make install
# on *BSD, you need GNU Make
gmake && sudo gmake install
```

A more modern practice on essentially "sole user" systems is to install under `~/.local`, thusly:

```
make install prefix=~/.local
# gmake install prefix=~/.local ## FreeBSD et al
```

`sudo` is then not needed.
To install the QDOS includes and libraries

```
./sdk-install.sh [~/.local]
```

If the optional directory is omitted, `/usr/local` is assumed. You can also use the environment variable `PREFIX` (or `prefix`) instead.

## Integration with native make (i.e. GNU Make)

* The `sdk-install.sh` script provides `ql.mak`, which it will copy to `$PREFIX/qdos/etc/ql.mak`
* In your QDOS project `Makefile`, as the first line:


  ```
  # local install, alas $$HOME is not expanded here ..
  include /home/USERNAME/.local/qdos/etc/ql.mak
  ```
  or

  ```
  # System install
  include /usr/local/qdos/etc/ql.mak
  ```


Now you can easily use GNU Make to build your QDOS project.
