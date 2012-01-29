FILES="vcc3m.sx vcc3p.sx vcc2m.sx vcc2p.sx "
#FILES="vcc3m.sx vcc3p.sx"

for rs in $(seq 30) ; do
  for f in $FILES ; do
    echo "RUN $f CS=5 RS=$rs"
    c:/dev/z3/release/z3.exe -st $f CASE_SPLIT=5 /rs:$rs RESTART_INITIAL=$((100+$rs))
    echo "RUN $f CS=3 RS=$rs"
    c:/dev/z3/release/z3.exe -st $f CASE_SPLIT=3 /rs:$rs RESTART_INITIAL=$((100+$rs))
  done
done 2>&1 | tee -a log-1
