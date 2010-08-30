#!/bin/sh

. config.sh

VERSION_OPTS=-3
SX_DIR=sx-vcc3

if [ "$1" = "-2" ] ; then
  VERSION_OPTS=
  SX_DIR=sx-vcc2
fi

rm -rf $SX_DIR
mkdir $SX_DIR

for b in $BENCHMARKS ; do
  $VCC $COMMON_OPTS "$b" $VERSION_OPTS -b:/proverLog:$SX_DIR/"$b"-@PROC@.sx
done

for sx in $SX_DIR/*.sx ; do
  grep -v "SETPARAMETER CASE_SPLIT" $sx > tmp.sx
  mv tmp.sx $sx
done

