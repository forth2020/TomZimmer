@echo off
echo.
echo UndoMods will restore your system to it's configuration prior
echo to having run AddMods.  If you have not run AddMods you don't
echo need to UndoMods.  (It won't hurt anything though.)  
echo.
echo UndoMods requires a full recompilation from scratch.  If you 
echo aren't sure you want to UndoMods right now press CTL-C otherwise
pause
@echo on

CD SRC
COPY UNDO\*.*
COPY UNDO\HOME\*.* ..\
DEL ..\_MARK.DIR
CD WINED
COPY UNDO\*.*
CD ..\..
CALL _MakeAll.bat

cls