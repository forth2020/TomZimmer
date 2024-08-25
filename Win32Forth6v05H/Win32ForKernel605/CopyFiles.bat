REM Note : not used in the VC++ V6 project.

REM copy the FKernel.exe to Win32For.exe ans Kernel.bin
call copy FKernel.exe Win32For.exe 
call copy FKernel.exe Kernel.bin

REM copy Kernel files up one level to the Win32For folder
REM call CopyFilesToWin32Forth.bat