#!/bin/sh

. config.sh

ID=`date +%Y.%m.%d.%H.%M.%S`

echo "*** ID $ID, running $*"
for thr in 0 5 10 ; do
  sh ./runsingle.sh $ID $thr "$@" &
done

wait %1 %2 %3
echo
echo "*** DONE $ID"
