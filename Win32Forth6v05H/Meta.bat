@echo off
rem build FKERNEL.IMG from FKERNEL.F
START /wait win32for.exe 0 fload SRC\meta.f SETSIZE bye
copy fkernel.exe kernel.bin
cls