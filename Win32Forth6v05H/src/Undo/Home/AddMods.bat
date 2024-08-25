@echo off
if exist src\undo\home\AddMods.bat goto skp01

REM Backing up files that are critical for recompilation from scratch.

    copy "Win32for.exe" src\undo\home
    copy "WIN32FOR.IMG" src\undo\home
    copy "kernel.bin" src\undo\home
    copy "WINCON.DLL" src\undo\home
    copy "Fkernel.exe" src\undo\home
    copy "Fkernel.img" src\undo\home
    copy "WINED.EXE" src\undo\home
    copy "WINED.IMG" src\undo\home
    copy "AddMods.bat" src\undo\home

:skp01

   cd src
   call SaveSRC.bat
   copy mods\*.*
   cd wined
   call SaveWined.bat
   copy mods\*.*

   cd ..\..
   call _MakeAll.bat

REM That's it!
REM Remember to put your mods INTO the mods dir or you won't be able to
REM UndoMods later.
REM


