#!/bin/bash

# Install header files to /usr/local/qdos/include and sub-directories
# Install libraries to /usr/local/qdos/lib
# This script will find the relevant files under ./, in the format
# from the original distribution runtime disk 1

[ $(id -u) -eq  0 ] || exec sudo $0 $*

mkdir -p /usr/local/qdos/include/sys
mkdir -p /usr/local/qdos/lib
mkdir -p /usr/local/bin

CP="cp -v"

for B in as68/as68 c68/c68 cc/qcc  cpp/qcpp ld/qld
do
  $CP $B /usr/local/bin/
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
