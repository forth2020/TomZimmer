(( April 7th, 2002 - 12:07
A start for security for NT or better.
It was started for the use of shutdown.

Notes:
It can be compiled when W95 is used.
Words which are marked with *W95 will also work when W95 is used ))


needs toolset.f
\ needs WERROR.F  \ Uses another version for ?win-error
needs struct.f

anew security.f \ Should work for NT or better. Tested on W2000

struct{ \ luid_and_attributes
    double  luid
    dword  attributes
 }struct luid_and_attributes

struct{ \ token_privilege
        dword PrivilegeCount
        offset luaa
        sizeof luid_and_attributes _add-struct
}struct token_privilege

sizeof token_privilege mkstruct: &token_privilege

struct{ \  _osversioninfo
    DWORD dwOSVersionInfoSize
    DWORD dwMajorVersion
    DWORD dwMinorVersion
    DWORD dwBuildNumber
    DWORD dwPlatformId
    128 add-struct szCSDVersion
}struct osversioninfo

((            W95  W98  WMe  WNT4.0  W2000  WXP
PlatformID     1    1    1    2        2     2
Major Version  4    4    4    4        5     5
Minor Version  0    10   90   0        0     1  ))

: os! ( adr - )   \ *W95
  sizeof osversioninfo over dwOSVersionInfoSize !
  rel>abs call GetVersionEx ?win-error
 ;

: nt-or-better? ( - flag )    pad dup os! dwPlatformId @ 2 >= ; \ *W95

250 string: systemname

: default-system  ( - )  systemname computername$!  ;           \ *W95

initialization-chain chain-add default-system
default-system

\ access mask to the access token
: get-token ( DesiredAccess - TokenHandle )
   nt-or-better?
      if &token_privilege luaa dup>r rel>abs swap  call GetCurrentProcess
         call OpenProcessToken ?win-error  r> @
      else true abort" Need NT for security operations."
      then
 ;

: luid?   ( z"priv" - luid-h luid-l )
    &token_privilege luaa dup>r rel>abs swap rel>abs systemname 1+ rel>abs
    call LookupPrivilegeValue 0= abort" Unknown privilege."
    r> 2@
 ;

 1 constant TokenUser
 2 constant TokenGroups
 3 constant TokenPrivileges
 4 constant TokenOwner
 5 constant TokenPrimaryGroup
 6 constant TokenDefaultDacl
 7 constant TokenSource
 8 constant TokenType
 9 constant TokenImpersonationLevel
10 constant TokenStatistics
11 constant TokenRestrictedSids
12 constant TokenSessionId

: GetTokenInformation ( type TokenHandle - buffer-with-info buffer-size )
   buffer 100 erase
   2>r pad rel>abs 1000 buffer rel>abs 2r> call GetTokenInformation ?win-error
   buffer 4 + pad @ ;

: reset_last_error     ( - )  0 call SetLastError drop ;     \ *W95

: .attribute ( attribute - )
    dup 0 = if drop ."  disabled." exit then
    dup SE_PRIVILEGE_ENABLED_BY_DEFAULT and if ."  ENABLED by default." then
    dup SE_PRIVILEGE_ENABLED            and if ."  ENABLED * " then
        SE_PRIVILEGE_USED_FOR_ACCESS    and if ." Seen by PrivilegeCheck." then
 ;

: .luid ( luid-h luid-l - )
\                       size of string  lpName,       lpLuid        pSystemName
    tmp$ 2! 500 temp$ ! temp$ rel>abs   here rel>abs  tmp$ rel>abs  systemname 1+ rel>abs
    call LookupPrivilegeName ?win-error
    here temp$ @ type
 ;

: .privs  \ Uses the buffer for storage of the privileges
    ." Priviliges at " systemname .string ." :"
    reset_last_error
    TOKENPRIVILEGES TOKEN_QUERY get-token GetTokenInformation
     4 - [ sizeof luid_and_attributes ] literal / 0
         do   cr dup i [ sizeof luid_and_attributes ] literal * +
              dup luid 2@ .luid
              attributes @  .attribute
         loop
     drop
 ;

: setpriv  ( z"Privilege" Attribute - flag )  \ for the current process
   TOKEN_ADJUST_PRIVILEGES TOKEN_QUERY or get-token >r
   reset_last_error
   &token_privilege luaa attributes !
   1 &token_privilege !
   luid?
   &token_privilege luaa 2!
   0 buffer ! buffer rel>abs
   pad  rel>abs
   400
   &token_privilege   rel>abs false
   r>
   call AdjustTokenPrivileges drop
   call GetLastError ERROR_SUCCESS =
 ;

: shutdown     ( type - )    0 swap call ExitWindowsEx  ;       \ *W95

\ Will shutdown or logoff depending on your privilege on NT or better.
\ It will shutdown when you do not use NT or better.
: down ( - )                                                    \ *W95
    nt-or-better?
       if    z" SeShutdownPrivilege"  SE_PRIVILEGE_ENABLED setpriv
       else  true
       then
       if     EWX_SHUTDOWN EWX_POWEROFF or \ EWX_REBOOT or
       else   EWX_LOGOFF
       then
    shutdown drop
 ;

\s

