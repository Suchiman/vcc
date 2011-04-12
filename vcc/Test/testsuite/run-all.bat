setlocal
cd %~dp0
REM vacid-0\Heap.c has verification failures?
..\..\Host\bin\debug\vcc.exe /smoke /s /time /smt:0.7 done examples tutorial vacid-0 vcc2 vcc2ns vscomp2010 ..\..\Demo ..\..\Docs\SyntaxUpdate\FromTutorial
copy testsuite.log testsuite-1.log
REM NOTE: vcc3 must be run separatedly at this time, since /3 cannot be
REM specified inside tests. Also note that Vcc3Prelude.bpl seems to be taken
REM from the installation, and not from the build.
..\..\Host\bin\debug\vcc.exe /smoke /s /time /smt:0.7 /3 vcc3 ..\..\Docs\Tutorial\c
