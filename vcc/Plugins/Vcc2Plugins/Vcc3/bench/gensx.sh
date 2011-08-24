#!/bin/sh

. config.sh

VERSION_OPTS=-3

if [ "$1" = "-2" ] ; then
  VERSION_OPTS=
  shift
fi
SX_DIR="$1"

[ "X$SX_DIR" = X ] && exit 1

rm -rf $SX_DIR
mkdir $SX_DIR

set -x
for b in $BENCHMARKS ; do
  $VCC $COMMON_OPTS "$b" $VERSION_OPTS -b:/proverLog:$SX_DIR/"$b"-@PROC@.sx
done
set +x

for sx in $SX_DIR/*.sx ; do
  grep -v "SETPARAMETER CASE_SPLIT" $sx > tmp.sx
  mv tmp.sx $sx
done

