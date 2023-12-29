xtc68
=====

Cross compiler for QDOS C68 on POSIX platforms (and Windows).

This is a resurrection of the 1999-ish code base to compile on modern POSIX-like systems.

As of 2021-08-24, the codebase is 64bit clean. There is no longer any requirement to build 32bit executables, even on Windows.

## Installation

You can either install a [binary distribution](https://github.com/stronnag/xtc68/releases) for Linux, MacOS, Windows or [build from source](#building). For platforms where there is no binary distribution, [build from source](#building) in required. Such platforms are:

* Anything other than x86_64 (amd64) with Linux, MacOS and Windows.

The binary installations are tar or Zip files, with the following structure:

```
xtc68/bin
xtc68/share/qdos
xtc68/share/qdos/etc
xtc68/share/qdos/include
xtc68/share/qdos/lib
```

You should then install a [c68 runtime](#runtime) into the `xtc68/share/qdos/include` (header files) and `xtc68/share/qdos/lib` (libraries and startup files).

## Runtime

You need the C68 runtime binary distribution. A good place to start looking is https://dilwyn.qlforums.co.uk/c/. If runtime disk1 is installed under `support`, the `sdk-install.sh` script will install a usable environment, such that:

```
qcc -o hw hw.c
```
will generate a QDOS executable, where `hw.c` is a trivial, standard "Hello World" application, assuming the installation directory `$(prefix)/bin`  is on `$PATH`.

Note that it is not purpose of this repository to provide a QDOS C68 development environment, nor does it offer any help for QDOS development; it mere maintains a cross compiler.

### Runtime path resolution

The search path for QDOS header files and libraries is:

* Under the `share/qdos` directory where `share` is at the same level as the binary `bin` directory (i.e. as distributed binaries)
* Under the directory defined at build time `$prefix/share/qdos`; this is not set for the binary distributions.
* In directories defined by the environment variables `QLINC` (header files) and `QLLIB` (libraries).
* In the fallback directories under `/usr/local/share/qdos`

So, if you unzip the supplied Windows Zip file to `C:\` (you now have `C:\xtc68` and sub-directories) and then added the C68 header files to `C:\xtc68\share\qdos\include` and the C68 libraries to `C:\xtc68\share\qdos\lib` (i.e. into the default directories the Zip file has conveniently provided), finally add `C:\xtc68\bin` to the `PATH` environment variable;  it all should "just work".

```
> # Powershell
> # Assume we have a trival "hello world" hw.c
> $env:PATH += "C:\xtc68\bin;"
> qcc -O -o helloworld hw.c
helloworld: dataspace 904 (388)
>
```

## Building

On most POSIX (like) systems (Linux, *BSD, MacOS, Msys) to build and install the executables.

* You need a native C compiler, `bash`, `meson` and `ninja`. It is recommended that you use GCC >= 12.0 or clang.

Modern practice on essentially "sole user" systems is to install under `~/.local`, with `~/.local/bin` appended to the `PATH`, so:

```
meson setup build --prefix=~/.local --strip
ninja install -C build
```

To use clang, e.g. for older / broken GCC:

```
## if necessary, first install clang ...
CC=clang meson setup _clbuild --prefix=~/.local --strip
ninja -C _clbuild install
```

To install the QDOS includes and libraries

```
./sdk-install.sh [~/.local]
```

If the optional `--prefix` directory is omitted, `/usr/local` is assumed.

If you *really* want to install in `/usr/local`:

```
# build and install the executables in /usr/local (default)
meson setup build --strip
ninja -C build
sudo ninja install -C build
sudo ./sdk-install.sh
```

Note `sudo` is required for a non-root user to install to `/usr/local`.

### Cross compiling

The `cross/mkcross.sh` script supports cross-compilation hosted on Linux for MacOS and Windows (as well as generating the Linux archive). Note that the MacOS recipe has a dependency on a local path for the MacOS cross-compiler).

## Integration with native make (i.e. GNU Make)

* The installation provides `ql.mk`, which it will copy to `$PREFIX/share/qdos/etc/ql.mk`
* In your QDOS project `Makefile`, as the first line (`.local` prefix install)

  ```
  # local install, alas $$HOME is not expanded here ..
  include /home/USERNAME/.local/share/qdos/etc/ql.mk
  ```

  * or

  ```
  # System install
  include /usr/local/qdos/etc/ql.mk
  ```

  * or

  ```
  mkdir ~/.config/xtc68
  cp $PREFIX/share/qdos/etc/ql.mk ~/.config/xtc68/
  # then in a Makefile
  include /home/USERNAME/.config/xtc68/ql.mak
  ```

Now you can easily use GNU Make to build your QDOS project.
