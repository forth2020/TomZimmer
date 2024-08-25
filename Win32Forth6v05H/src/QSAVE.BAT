rem     ?SAVE used by Save{DirName}.BAT which is called by 
rem 	AddMods.BAT to selectively save undo files as needed.
rem 	DON'T MOVE THIS FILE.  (You can copy it though.)

if not exist mods\%1 goto qsexit
if not exist undo\%1 copy %1 undo\%1
:qsexit


