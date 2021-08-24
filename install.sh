#!/usr/bin/env bash

# Install header files to /usr/local/qdos/include and sub-directories
# Install libraries to /usr/local/qdos/lib
# This script will find the relevant files under ./, in the format
# from the original distribution runtime disk 1


BINDIR=${1:-/usr/local/bin}

if [ $(uname -o) != "Cygwin" ] ; then
  [ $(id -u) -eq  0 ] || exec sudo $0 $*
fi

mkdir -p /usr/local/qdos/include/sys
mkdir -p /usr/local/qdos/include/netinet/
mkdir -p /usr/local/qdos/include/arpa/
mkdir -p /usr/local/qdos/lib
mkdir -p $BINDIR

CP="cp -v"

for B in as68/as68 c68/c68 cc/qcc cpp/qcpp ld/qld slb/slb slb/qdos-ar slb/qdos-ranlib
do
  $CP $B $BINDIR
done

while read FILE
do
  FN=$(echo $FILE | tr [:upper:]  [:lower:])
  case $FN in
    */include_*)
      IFILE=${FN##*include_}
      case $IFILE in
	sys_*)
	  SIFILE=${IFILE##*sys_}
	  $CP $FILE /usr/local/qdos/include/sys/$SIFILE
      ;;
	netinet_*)
	  SIFILE=${IFILE##*netinet_}
	  $CP $FILE /usr/local/qdos/include/netinet/$SIFILE
	  ;;
	arpa_*)
	  SIFILE=${IFILE##*arpa_}
	  $CP $FILE /usr/local/qdos/include/arpa/$SIFILE
	  ;;
	*)
	  $CP $FILE /usr/local/qdos/include/$IFILE
	  ;;
      esac
      ;;
    */lib_*)
      LFILE=${FN##*lib_}
      $CP $FILE /usr/local/qdos/lib/$LFILE
      ;;
  esac
done < <(find . -iname include_\* -o -iname lib_\*)
