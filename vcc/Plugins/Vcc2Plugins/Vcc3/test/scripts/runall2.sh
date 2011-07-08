FILES="vcc3m.sx vcc3p.sx vcc2m.sx vcc2p.sx "
#FILES="vcc3m.sx vcc3p.sx"

for rs in $(seq 30) ; do
  for n in $(seq 30) ; do
    perl -e 's/WEIGHT 15/WEIGHT '$n'/g' -p < vcc3-weight15m.sx > t.sx
    echo "RUN $n RS=$rs"
    c:/dev/z3/release/z3.exe -st t.sx CASE_SPLIT=5 /rs:$rs RESTART_INITIAL=$((100+$rs))
  done
done 2>&1 | tee -a log-5
