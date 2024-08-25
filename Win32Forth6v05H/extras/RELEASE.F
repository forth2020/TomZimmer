\ files2iss
        0 value hf
        create '"'char '"' c,
        : "writethatline hf write-file drop crlf$ count hf write-file drop ;
        : "writethat hf write-file drop ;
: ,|            ( -<string">- )
                [CHAR] | PARSE
                HERE >R DUP C,
                DUP ALLOT
                R@ 1+ SWAP MOVE
                0 C,
                ALIGN
                R> COUNT \N->CRLF ;

: S|            ( -<string">- )
                STATE @
                IF      COMPILE (S")  ,|
                ELSE    [CHAR] | WORD
                        NEW$ DUP>R OVER C@ 1+ MOVE
                        r> COUNT
                THEN ; IMMEDIATE


 : startscript  { \ string$ -- }
   max-handle localalloc: string$
s" ; " string$ place
get-local-time time-buf >month,day,year" string$ +place  s"  " string$ +place
get-local-time time-buf >time" string$ +place string$ count "writethatline
s| [Setup]| "writethatline

s| AppName=Win32Forth| "writethatline

s| AppVerName=Win32Forth | string$ place
version# 0 <# # # # # s"   Build: " "hold # '.' hold #s #> string$ +place
string$ count "writethatline
s| AppPublisher=Silicon Valley Chapter of the Forth Interest Group| "writethatline
s| AppPublisherURL=http://www.forth.org/sfig/index.htm| "writethatline
s| AppSupportURL=http://www.forth.org/sfig/index.htm| "writethatline
s| AppUpdatesURL=http://www.forth.org/sfig/index.htm| "writethatline
s| DefaultDirName={sd}\win32for| "writethatline
s| DefaultGroupName=Win32Forth | string$ place
version# 10000 / 0 <# # '.' hold #s #> string$ +place string$ count "writethatline
s| ChangesAssociations=yes| "writethatline
s| Compression=bzip/9| "writethatline
s" OutputBaseFilename=Win32For" string$ place
version# 0 <# #s #> string$ +place string$ count "writethatline

s| [Tasks]| "writethatline

s| Name: "desktopicon1"; Description: "Create a &desktop icon to Win32for.exe"; GroupDescription: "Additional icons:"| "writethatline
s| Name: "desktopicon2"; Description: "Create a &desktop icon to WinEd.exe"| "writethatline
s| Name: "desktopicon3"; Description: "Create a &desktop icon to Win32Forth directory"| "writethatline

;


: !dir->file-name { \ string$  -- }
        max-handle localalloc: string$
        _win32-find-data 11 cells+              \ adrz
        asciiz->asc-len string$ place


        string$ count   '.' scan nip 0<>                  >r    \  has a '.' ? (to be sure that this is a file not a dir)
        string$ count  s" ." compare 0<>                  >r
        string$ count  s" .."   compare 0<>               >r
        string$ count  s" release.iss"   compare 0<>      >r
        string$ count  s" FILES2ISS.F"   compare 0<>      >r
        string$ count  s" unins000.exe"  compare 0<>      >r
        string$ count  s" unins000.dat"  compare 0<>      >r
        string$ count  s" BUILTBY.F"     compare 0<>      >r
                                                 r> r> r> r> r> r> r> r>
                                                and and  and and  and  and and
        if

                                          s| Source: "c:\win32for\| "writethat
                                                      string$ count "writethat
              s| " ; DestDir: "{app}\"; CopyMode: alwaysoverwrite | "writethatline

       else
            string$ count s" BUILTBY.F" compare 0=
         if
  s| Source: "c:\win32for\builtby\builtby2.f";DestDir:"{app}"; Destname: "builtby.f";CopyMode: alwaysoverwrite| "writethatline



         then
       then
;

: _write-dir-files ( adr slen -- )


                find-first-file nip                  \ adrd ior - find first file
                0 <> if exit then                    \ if file is not found, exit

                s" [Files]" "writethatline

                !dir->file-name                      \ write the filename found

                BEGIN find-next-file nip 0=          \ ior - find next file
                WHILE
                        !dir->file-name                \ and the name of file
                REPEAT ;

: write-dir-files ( adr len  -- )

                ['] _write-dir-files catch
                IF      2drop                   \ discard abort results
                THEN
                find-close drop
                ;
: endscript
s| [Icons]| "writethatline
s| Name: "{group}\Win32Forth"; Filename: "{app}\win32for.exe"; WorkingDir: "{app}";IconFilename: "{app}\win32for.ico";IconIndex: 0| "writethatline
s| Name: "{group}\WinEd Editor"; Filename: "{app}\wined.exe"; WorkingDir: "{app}" ;IconFilename: "{app}\winedit.ico";IconIndex: 0| "writethatline
s| Name: "{group}\Win32Forth Release Notes"; Filename: "{app}\wined.exe"; Parameters: "/b win32for.new"; WorkingDir: "{app}" ;IconFilename: "{app}\winedit.ico";IconIndex: 0| "writethatline
s| Name: "{group}\Win32Forth Previous Notes"; Filename: "{app}\wined.exe"; Parameters: "/b win32for.prv"; WorkingDir: "{app}" ;IconFilename: "{app}\winedit.ico";IconIndex: 0| "writethatline
s| Name: "{group}\Make WinEd"; Filename: "{app}\win32for.exe"; Parameters: "fload wined.f bye"; WorkingDir: "{app}"| "writethatline
s| Name: "{group}\Make Win32Forth"; Filename: "{app}\fkernel.exe"; Parameters: "fload extend.f bye"; WorkingDir: "{app}"| "writethatline
s| Name: "{group}\Meta Compile Kernel"; Filename: "{app}\win32for.exe"; Parameters: "fload meta.f setsize bye"; WorkingDir: "{app}"| "writethatline
s| Name: "{group}\Uninstall"; Filename:"{uninstallexe}"| "writethatline
s| Name: "{userdesktop}\Win32Forth"; Filename: "{app}\win32for.exe"; IconFilename: "{app}\win32for.ico";IconIndex:0 ;  Tasks: desktopicon1| "writethatline
s| Name: "{userdesktop}\WinEd"; Filename: "{app}\wined.exe"; IconFilename: "{app}\winedit.ico"; IconIndex: 0; Tasks: desktopicon2| "writethatline
s| Name: "{userdesktop}\Win32forth Directory"; Filename: "{app}\"; Tasks: desktopicon3| "writethatline


s| [Registry]| "writethatline

s| Root: HKCR; Subkey: ".F"; ValueType: string; ValueName: ""; ValueData: "Win32Forth File"; Flags: uninsdeletevalue| "writethatline
s| Root: HKCR; Subkey: "Win32Forth File\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\WINED.EXE"" ""%1"""| "writethatline
s| Root: HKCR; Subkey: "Win32Forth File\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\APP_ICON.ICO,0"| "writethatline

s| [Run]| "writethatline

s| Filename: "{app}\win32for.exe"; Description: "Launch Win32Forth"; Flags: nowait postinstall skipifsilent unchecked| "writethatline
s| Filename: "{app}\wined.exe"; Description: "Launch Wined"; Parameters: " /b win32for.new"; Flags: nowait postinstall skipifsilent unchecked| "writethatline

;





create safestring 256 allot

: files2iss   {  \ script$ hfile -- }
                max-handle LocalAlloc: script$
                s" c:\win32for\" safestring place
                s" release.iss" r/w create-file 0<> abort" Create-File" to hf
                startscript

                safestring count pocket place
                s" *.*" pocket +place
                pocket count write-dir-files
                endscript
                hf close-file drop
;


