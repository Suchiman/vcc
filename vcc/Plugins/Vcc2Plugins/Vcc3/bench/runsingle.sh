#!/bin/sh

. config.sh

id="$1"
thr="$2"
SX_DIR="$3"
test -d "$SX_DIR" || exit 1

mkdir -p tmplog
log="tmplog/`date +%Y.%m.%d.%H.%M.%S`-th$thr.log"

echo "ALL_OPTIONS $*" > $log
echo "THREAD $thr" >> $log
echo "SX_DIR $SX_DIR" >> $log
echo "ID $id" >> $log
shift
shift
shift
echo "OPTIONS $*" >> $log

for run in 0 1 ; do
  rs=$(($thr + $run))
  for sx in $SX_DIR/*.sx ; do
    echo -n "[$sx:$rs] "
    echo "FILE $sx" >> $log
    echo "RANDOM_SEED $rs" >> $log
    $Z3 -ist $sx RANDOM_SEED=$rs RESTART_INITIAL=$((100 + $rs)) CASE_SPLIT=5 "$@" 2>&1 >> $log
  done
done

echo "DONE" >> $log
mv $log .
