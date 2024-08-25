\ WinVersion6V05.f  2019 Mar 02

\ ---------------- Operating System Checking --------------------------

1 PROC GetVersionEx
\ 9 PROC RegCreateKeyEx
\ 6 PROC RegQueryValueEx

1  constant Win95
2  constant Win98
3  constant Winme
4  constant Winnt351
5  constant Winnt4
6  constant Win2k
7  constant Winxp
8  constant Win2003   \ Windows Server 2003 R2
9  constant Winvista  \ Windows Vista
10 constant Win2008   \ Windows Server 2008
11 constant Win2008r2 \ Windows Server 2008 R2
12 constant Win7      \ Windows 7
13 constant Win8      \ Windows 8
14 constant Win10     \ Windows 10

1 constant VER_NT_WORKSTATION

\ To check for a version, say Win2K or greater, try WINVER WIN2K >=

: ShowWindowsVersion ( c -- )
   case
   Win95     of  ." Windows 95"              endof  \  Win95     
   Win98     of  ." Windows 98"              endof  \  Win98     
   Winme     of  ." Windows ME"              endof  \  Winme     
   Winnt351  of  ." Windows NT3.51"          endof  \  Winnt351  
   Winnt4    of  ." Windows NT4"             endof  \  Winnt4    
   Win2k     of  ." Windows 2000"            endof  \  Win2k     
   Winxp     of  ." Windows XP"              endof  \  Winxp     
   Win2003   of  ." Windows Server 2003 R2"  endof  \  Win2003   
   Winvista  of  ." Windows Vista"           endof  \  Winvista  
   Win2008   of  ." Windows Server 2008"     endof  \  Win2008   
   Win2008r2 of  ." Windows Server 2008 R2"  endof  \  Win2008r2 
   Win7      of  ." Windows 7"               endof  \  Win7      
   Win8      of  ." Windows 8"               endof  \  Win8      
   Win10     of  ." Windows 10"              endof  \  Win10     
   cr ." Unknown Windows Version = " dup .
   endcase
;

variable phkResult
variable lpData
variable lpcbData
variable lpType
variable lpdwDisposition
variable ReturnedKeyValue$  252 allot  \ lpData with a preceding count byte

((
LSTATUS RegCreateKeyExA(
  HKEY                        hKey,
  LPCSTR                      lpSubKey,
  DWORD                       Reserved,
  LPSTR                       lpClass,
  DWORD                       dwOptions,
  REGSAM                      samDesired,
  const LPSECURITY_ATTRIBUTES lpSecurityAttributes,
  PHKEY                       phkResult,
  LPDWORD                     lpdwDisposition
];
))

\ Open ( or create if it does not exist ) the given registry key name in the given section of the registry 
: RegKeyOpen ( lpSubKey hKey -- phkResult | -1 )  \ read the key of a section
   0 phkResult !  
   >r          \ hKey
   >r          \ lpSubKey
   lpdwDisposition          rel>abs \ lpdwDisposition ( not used )
   phkResult                rel>abs \ phkResult  the return value
   NULL                             \ lpSecurityAttributes
   KEY_ALL_ACCESS                   \ samDesired
   REG_OPTION_NON_VOLATILE          \ dwOptions
   NULL                             \ lpClass
   0                                \ Reserved
   r> ( lpSubKey )          rel>abs \ lpSubKey
   r> ( hKey )                      \ hKey : HKEY_LOCAL_MACHINE  HKEY_CURRENT_USER  etc
\   cr 2dup swap abs>rel 30 type  2 spaces . cr  
   Call RegCreateKeyEx              \ Note : also opens the Key if it exists
   if      
      -1    \ failed
      abort" RegCreateKeyEx failed!"
   else    
      phkResult @
   then   
;

((
: ttrko ( -- )
   z" SOFTWARE\Microsoft\Windows NT\CurrentVersion"
   HKEY_LOCAL_MACHINE
   dup >r
   RegKeyOpen    
   cr ." ttrko result = " . 
   r> ( hKey ) Call RegCloseKey 
   ."   RegCloseKey = " dup . 
   drop
;
))

(( 
LSTATUS RegQueryValueExA[
  HKEY    hKey,
  LPCSTR  lpValueName,
  LPDWORD lpReserved,
  LPDWORD lpType,
  LPBYTE  lpData,    // pointer to buffer
  LPDWORD lpcbData   // size of buffer
];
))

: GetRegistryEntry   ( lpValueName lpSubKey hKey -- $ )
\ *G Retrieves the data of any registry key.
\ ** Parameters:
\ ** root-key      one of the predefined keys EG: HKEY_CLASSES_ROOT
\ ** lpValueName   null terminated registry key section string.
\ ** security access mask = KEY_EXECUTE
\ ** returns the data pointer and length of the returned value  
   0 ReturnedKeyValue$ c!     \ initially clear return buffer
   RegKeyOpen dup -1 =
   if      
      drop
      r> drop
      ReturnedKeyValue$ count
      exit                    \ return a zero length string on error
   then    
   ( handle ) >r
   ( lpValueName ) >r
   MAXCOUNTED lpcbData !            \ initialise   max length of available buffer 
   lpcbData                 rel>abs \ lpcbData     max length of available buffer 
   ReturnedKeyValue$ 1+     rel>abs \ lpData       pointer to buffer
   lpType                   rel>abs \ lpType
   0                                \ lpReserved
   r> ( lpValueName )       rel>abs \ lpValueName
   r@ ( handle )                    \ "hKey"
   Call RegQueryValueEx
   if    
      0 ReturnedKeyValue$ c!        \ Can not access the registry-key, return a 0 length string
      abort" RegQueryValueEx failed!"
   else  
      lpcbData 1- 0max ReturnedKeyValue$ c!   \ Includes the 0 for REG_SZ types
   then
   r> ( handle ) Call RegCloseKey abort" RegCloseKey failed!" 

   ReturnedKeyValue$ count
;

: GetVersionWin8+ ( -- winver )
   z" CurrentMajorVersionNumber" 
   z" SOFTWARE\Microsoft\Windows NT\CurrentVersion"
   HKEY_LOCAL_MACHINE
   GetRegistryEntry 0> if  @ 4 +  else  drop  Win8  then 
;

\ To check for a version, say Win2K or greater, try WINVER WIN2K >=

0 value winver

: WINVER-INIT ( -- )                \ set Windows version number into  winver
   148 PAD !                        \ set length of structure at PAD
   PAD REL>ABS CALL GetVersionEx    \ call OS for version
   0= ABORT" Call GetVersionEx failed"
   PAD 4 CELLS+ @                   \ get OsPlatformID
   \ cr ." GetVersionEx + 4 cells = " dup . 
   case
      1 of                          \ 95, 98, and ME
       PAD 2 CELLS+ @               \ MinorVersion
       case
         0  of  Win95  endof        \ 95
         10 of  Win98  endof        \ 98
         90 of  WinME  endof        \ ME
         ( default ) 0 swap         \ ****RO****
       endcase
      endof

      2 of                          \ NT, 2K, XP
       PAD 1 CELLS+ @               \ MajorVersion
       \ cr ." GetVersionEx + 1 cell  = " dup . 
       case
         3  of  WinNT351  endof     \ NT351
         4  of  WinNT4    endof     \ NT4
         5  of                      \ 2K or XP
            PAD 2 CELLS+ @          \ minor version
            IF  WinXP  ELSE  Win2K  THEN 
            endof 
         6  of
            ((
             \ There is a "catch 22" situation here : 
             \ GetVersionEx  in older OS's needs a structure length of 148, but
             \ we need a value that is offset 154 in the data returned by newer OS's.
             \ Newer OS's therefore need a structure length of 156 in order to find out if we are not
             \ Win2008r2 , which won't work on older OS's...
             ))
            156 PAD !                        \ set new, larger, length of structure at PAD
            PAD REL>ABS CALL GetVersionEx    \ call OS for version
            0= ABORT" Call GetVersionEx #2 failed"

            PAD 2 cells+ @  \ minor version
       \ cr ." GetVersionEx + 2 cells = " dup . 
            case
               0 of  
                  PAD 154 + C@ \ Product Type
                  VER_NT_WORKSTATION = IF  
                     Winvista    \ Windows Vista
                  ELSE  
                     Win2008     \ Windows Server 2008
                  THEN
               endof
               1 of  
                  PAD 154 + c@ \ Product Type
                  VER_NT_WORKSTATION = IF  
                     Win7  \ Windows 7
                  ELSE  
                     Win2008r2 \ Windows Server 2008 R2
                  THEN
               endof
               2 of 
                  pad 154 + c@  \ Product Type 
                  \ cr ." GetVersionEx + 154 = " dup . 
                  VER_NT_WORKSTATION = if 
                     GetVersionWin8+   \ Windows 8 or higher
                  else 
                     Win2008r2         \ Windows Server 2008 R2
                  then                   
               endof
               -2 swap   \ unknown product Type
               endcase
            endof
            -3 swap   \ unknown product Type
       endcase
      endof
      -4 swap   \ unknown product Type
   endcase 
   to WINVER 
;

((
: ttWinVer ( -- )
   WINVER-INIT
   WINVER ShowWindowsVersion
;
))

initialization-chain chain-add WINVER-INIT

