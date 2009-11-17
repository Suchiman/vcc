echo "Refresh file to see test results" > ts.out
start n ts.out
..\..\Host\bin\debug\vcc.exe /b:/smoke /s vcc2 done examples /smt:3 > ts.out
