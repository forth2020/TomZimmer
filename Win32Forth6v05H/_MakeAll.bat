@echo off

goto skp00

	To make a release version make sure the wined text search
	strings and all the check boxes are cleared.   Also make
	sure the install directory is in C:\Win32for so the file 
	paths created by Win32Forth will be correct.

	Usage to make a release: 

		_MakeAll RELEASE
	 
	End users can just click on the file without any parameters.

	mark.dir exists in the SRC directory of every v6.xx
	installation.  It is used as a flag.  Later _mark.dir 
	(with the underscore) may appear in the src directory 
	after an AddMod call, but the presence of the mark.dir 
	(without the underscore) means it's a brand new 
	installation in the users new directory.

	DO NOT PUT ANY MODS OR UNDOS in any new release!  Include
	them as separate zips or mod setup files.

	2022 Aug 08 changed the order of events to call Extend.bat first - this is required after FKernel.exe has been re-built. HO

:skp00
	
if "%1" == "FINALIZE" copy "src\_mark.dir" "src\mark.dir"
if not exist src\mark.dir goto skp01
if "%1" == "RELEASE" goto skp01

cls
echo. 
echo Preparing to reinitialize your MODS and UNDO directories,
echo saving the old mods and undo's in SRC\OLD.
echo.
echo OK to proceed?  (Press CTL-C to abort.)
echo.
pause

md "src\old"
md "src\old\mods"
md "src\old\undo"
md "src\old\undo\home"
md "src\old\wined\mods"
md "src\old\wined\undo"
move "src\mods\*.*" "src\old\mods\"
copy "src\mark.dir" "src\mods\_mark.dir"
move "src\undo\*.*" "src\old\undo\"
copy "src\mark.dir" "src\undo\_mark.dir"
move "src\undo\home\*.*" "src\old\undo\home\"
copy "src\mark.dir" "src\undo\home\_mark.dir"
move "src\wined\mods\*.*" "src\old\wined\mods\"
copy "src\mark.dir" "src\wined\mods\_mark.dir"
move "src\wined\undo\*.*" "src\old\wined\undo\"
copy "src\mark.dir" "src\wined\undo\_mark.dir"
del "src\mark.dir"

:skp01

cls 
@
echo Extending FKernel.img
call extend.bat
rem @
rem @echo.
rem @echo OK to proceed?  [CTL-C] to abort!  Otherwise
rem @pause
@
echo Recompiling Win32Forth...
@echo on
@
call meta.bat
rem @
rem @echo.
rem @echo OK to proceed?  [CTL-C] to abort!  Otherwise
rem @pause
@
call MakeWinEd.bat
REM cls
rem @
rem @echo.
rem @echo OK to proceed?  [CTL-C] to abort!  Otherwise
rem @pause
@
call help
@
rem cls
@
start /w Wined.exe /b hyperNew.txt
rem cls