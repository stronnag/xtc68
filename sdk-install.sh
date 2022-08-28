#!/usr/bin/env bash

# Install header files to /usr/local/qdos/include and sub-directories
# Install libraries to /usr/local/qdos/lib
# This script will find the relevant files under ./, in the format
# from the original distribution runtime disk 1


[ -n "$prefix" ] && PREFIX=$prefix
PREFIX=${PREFIX:-/usr/local}
INSBIN=
BINDIR=
for L
do
  case $L in
    -e)
      INSBIN=1
      ;;
    *[\\/]*)
      PREFIX=$L
      ;;
    *)
      echo "install.sh [-e] [base directory] [-h]"
      echo " -e  install binaries as well as include and libraries"
      exit
      ;;
  esac
done
BINDIR=${BINDIR:-$PREFIX/bin}

if [ ! -w $PREFIX ] ; then
  if [ $(id -u) -ne  0 ] ; then
    if which sudo >/dev/null 2>&1 ; then
      exec sudo $0 $*
    else
      echo "No sudo found, please run ./install.sh as root if necessary"
      [ -r $PREFIX ] || exit 1
    fi
  fi
fi
CP="cp -v"

mkdir -p $PREFIX/qdos/include/sys
mkdir -p $PREFIX/qdos/include/netinet/
mkdir -p $PREFIX/qdos/include/arpa/
mkdir -p $PREFIX/qdos/lib
mkdir -p $PREFIX/qdos/etc
[ -f support/ql.mak ] && $CP support/ql.mak $PREFIX/qdos/etc

if [ -n "$INSBIN" ] ; then
  mkdir -p $BINDIR
  for B in as68/as68 c68/c68 cc/qcc cpp/qcpp ld/qld slb/slb slb/qdos-ar slb/qdos-ranlib
  do
    $CP $B $BINDIR
  done
fi

while read FILE
do
  FN=$(echo $FILE | tr [:upper:]  [:lower:])
  case $FN in
    */include_*)
      IFILE=${FN##*include_}
      case $IFILE in
	sys_*)
	  SIFILE=${IFILE##*sys_}
	  $CP $FILE $PREFIX/qdos/include/sys/$SIFILE
      ;;
	netinet_*)
	  SIFILE=${IFILE##*netinet_}
	  $CP $FILE $PREFIX/qdos/include/netinet/$SIFILE
	  ;;
	arpa_*)
	  SIFILE=${IFILE##*arpa_}
	  $CP $FILE $PREFIX/qdos/include/arpa/$SIFILE
	  ;;
	*)
	  $CP $FILE $PREFIX/qdos/include/$IFILE
	  ;;
      esac
      ;;
    */lib_*)
      LFILE=${FN##*lib_}
      $CP $FILE $PREFIX/qdos/lib/$LFILE
      ;;
  esac
done < <(find . -iname include_\* -o -iname lib_\*)
