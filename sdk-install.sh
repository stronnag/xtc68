#!/usr/bin/env bash

# Install header files to /usr/local/qdos/include and sub-directories
# Install libraries to /usr/local/qdos/lib
# This script will find the relevant files under ./, in the format
# from the original distribution runtime disk 1

[ -n "$prefix" ] && PREFIX=$prefix
PREFIX=${PREFIX:-/usr/local}
for L
do
  case $L in
    *[\\/]*)
      PREFIX=$L
      ;;
    *)
      echo "sdk-install.sh [base directory] [-h]"
      echo "    install include and libraries"
      exit
      ;;
  esac
done

if [ ! -w $PREFIX ] ; then
  if [ $(id -u) -ne  0 ] ; then
    if which sudo >/dev/null 2>&1 ; then
      exec sudo $0 $*
    else
      echo "No sudo found, please run ./sdk-install.sh as root if necessary"
      [ -r $PREFIX ] || exit 1
    fi
  fi
fi
CP="cp -v"

mkdir -p $PREFIX/share/qdos/include/sys
mkdir -p $PREFIX/share/qdos/include/netinet/
mkdir -p $PREFIX/share/qdos/include/arpa/
mkdir -p $PREFIX/share/qdos/lib

while read FILE
do
  FN=$(echo $FILE | tr [:upper:]  [:lower:])
  case $FN in
    */include_*)
      IFILE=${FN##*include_}
      case $IFILE in
	sys_*)
	  SIFILE=${IFILE##*sys_}
	  $CP $FILE $PREFIX/share/qdos/include/sys/$SIFILE
      ;;
	netinet_*)
	  SIFILE=${IFILE##*netinet_}
	  $CP $FILE $PREFIX/share/qdos/include/netinet/$SIFILE
	  ;;
	arpa_*)
	  SIFILE=${IFILE##*arpa_}
	  $CP $FILE $PREFIX/share/qdos/include/arpa/$SIFILE
	  ;;
	*)
	  $CP $FILE $PREFIX/share/qdos/include/$IFILE
	  ;;
      esac
      ;;
    */lib_*)
      LFILE=${FN##*lib_}
      $CP $FILE $PREFIX/share/qdos/lib/$LFILE
      ;;
  esac
done < <(find . -iname include_\* -o -iname lib_\*)
