BENCHMARKS="List.c RedBlackTrees.c Heap.c"
#BENCHMARKS="Heap.c"
FELT=c:/dev/felt
VCC=$FELT/Vcc/Host/bin/Debug/vcc.exe
Z3=$FELT/Vcc/Boogie/z3.exe
THR=-z:QI_EAGER_THRESHOLD=1000
COMMON_OPTS="-st -it -z:CASE_SPLIT=5 $THR -z:/t:5"
