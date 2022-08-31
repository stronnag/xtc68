#!/usr/bin/env bash

PLAT=${1:-win:mac}
BASE=${2:-/tmp/xtc68}

WINPFX=$BASE/windows/xtc68
MACPFX=$BASE/macos/xtc68
LINPFX=$BASE/linux/xtc68

mkarchive() {
  local osname=$1

  XBASE=$BASE/$osname/xtc68
  mkdir -p $XBASE/share
  mkdir -p $XBASE/share/qdos/lib
  mkdir -p $XBASE/share/qdos/include
  mkdir -p $XBASE/share/qdos/etc
  [ -r support/ql.mak ] && cp -v support/ql.mak $XBASE/share/qdos/etc/
  cp sdk-install.sh README.md $XBASE/
  case $osname in
    windows)
      rm -f /tmp/xtc68-$(date +%F)-$osname-amd64.zip
      (cd $BASE/$osname && zip -9r /tmp/xtc68-$(date +%F)-$osname-amd64.zip xtc68)
      ;;
    *)
      rm -f /tmp/xtc68-$(date +%F)-$osname-amd64.tar.gz
      tar -C $BASE/$osname -czf /tmp/xtc68-$(date +%F)-$osname-amd64.tar.gz xtc68
      ;;
  esac
}

if [[ "$PLAT" =~  "lin" ]] ; then
  echo -e "\a\n*** Do the Linux release builds on Debian Stable ***\n"
  rm -rf build-linux $BASE/linux
  meson build-linux --prefix=$LINPFX --strip -Drelbuild=true
  meson install  -C build-linux
  mkarchive linux
fi

if [[ "$PLAT" =~ "win" ]] ; then
  rm -rf build-win64  $BASE/win64
  meson build-win64 --cross-file cross/x86_64-w64-mingw.txt --prefix=$WINPFX \
	--strip -Drelbuild=true
  meson install  -C build-win64
  mkarchive windows
fi

if [[ "$PLAT" =~ "mac" ]] ; then
  rm -rf build-macos $BASE/macos
  PATH=$PATH:/opt/src/osxcross/target/bin meson build-macos \
					  --cross-file cross/x86_64-w64-macos.txt \
					  --prefix=$MACPFX --strip -Drelbuild=true
  PATH=$PATH:/opt/src/osxcross/target/bin meson install  -C build-macos
  mkarchive macos
fi
