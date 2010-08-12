echo // Beginning of tests; refresh file to see test results > ts_out.txt
start ts_out.txt
..\..\Host\bin\debug\vcc.exe /smoke /s /time vcc2 done examples tutorial >> ts_out.txt
echo // End of tests >> ts_out.txt
