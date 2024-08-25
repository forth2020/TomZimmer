anew dtop_lnk.f   \ April 5th, 2004 Tested on Win32Forth versions 4.2 and 6.09.04

needs toolset.f
needs com01.f

variable &pLink
variable &pPersistFile
variable &DesktopPidl

iid: CLSID_ShellLink   {00021401-0000-0000-C000-000000000046}
iid: IID_IShellLink    {000214EE-0000-0000-C000-000000000046}
iid: IID_IPersistFile  {0000010b-0000-0000-C000-000000000046}

#define CSIDL_DESKTOPDIRECTORY      0x0010

struct{  \ IShellLinkA
\ STDMETHODCALLTYPE QueryInterface
\ STDMETHODCALLTYPE AddRef
\ STDMETHODCALLTYPE Release
interface_IUnknown
        STDMETHODCALLTYPE *GetPath
        STDMETHODCALLTYPE *GetIDList
        STDMETHODCALLTYPE *SetIDList
        STDMETHODCALLTYPE *GetDescription
        STDMETHODCALLTYPE *SetDescription
        STDMETHODCALLTYPE *GetWorkingDirectory
        STDMETHODCALLTYPE *SetWorkingDirectory
        STDMETHODCALLTYPE *GetArguments
        STDMETHODCALLTYPE *SetArguments
        STDMETHODCALLTYPE *GetHotkey
        STDMETHODCALLTYPE *SetHotkey
        STDMETHODCALLTYPE *GetShowCmd
        STDMETHODCALLTYPE *SetShowCmd
        STDMETHODCALLTYPE *GetIconLocation
        STDMETHODCALLTYPE *SetIconLocation
        STDMETHODCALLTYPE *SetRelativePath
        STDMETHODCALLTYPE *Resolve
        STDMETHODCALLTYPE *SetPath
 }struct IShellLinkA

: ->plink  ( /..~../ interface - )   &pLink @ swap std_imethod ;

: GetSpecialFolderLocation   ( nFolder - adr count )
    &DesktopPidl rel>abs swap NULL call SHGetSpecialFolderLocation drop
    tmp$ rel>abs &DesktopPidl @ call SHGetPathFromIDList drop
    tmp$ zcount
 ;

: init_dtop_for_link
   CoInitialize
   &pLink rel>abs IID_IShellLink rel>abs CLSCTX_INPROC_SERVER
   NULL CLSID_ShellLink rel>abs    call CoCreateInstance ?failed
   &pPersistFile rel>abs IID_IPersistFile rel>abs *QueryInterface ->pLink
 ;

struct{  \ IPersistFile
\ STDMETHODCALLTYPE QueryInterface
\ STDMETHODCALLTYPE AddRef
\ STDMETHODCALLTYPE Release
interface_IUnknown
        STDMETHODCALLTYPE  *GetClassID
        STDMETHODCALLTYPE  *IsDirty
        STDMETHODCALLTYPE  *Load
        STDMETHODCALLTYPE  *Save
        STDMETHODCALLTYPE  *SaveCompleted
        STDMETHODCALLTYPE  *GetCurFile
 }struct IPersistFile

: ->pPersistFile  ( /..~../ interface - )   &pPersistFile @ swap std_imethod ;

: buf+null ( buf$ n - abs_adr$incl_0 )  pad ascii-z rel>abs  ;
: write-installer ( adr n - )  s" installer.log" drop-count file type eof ;
: set_icon_link   ( adr n - )  0 -rot buf+null *SetIconLocation ->plink ;
: set_dir_link    ( adr n - )  buf+null *SetWorkingDirectory ->plink ;

: make_link   ( z"description" s"full-path+file-name" count - )
    buf+null
             *SetPath        ->plink
    SW_SHOW  *SetShowCmd     ->plink
    rel>abs  *SetDescription ->pLink
  ;

: save_link  ( adr n - )
   buffer ansi>unicode true buffer rel>abs *Save ->pPersistFile
 ;

\ link/start performs the installation actions when installer.log does not exist
\ or when the current directory is not the same as the one in installer.log
\ The result is an auto-installer which runs when needed.

: link/start ( 'start 'installation - )
    s" installer.log" read open-file drop
    >r buffer maxstring r@ read-line 2drop 1 max
    r> close-file drop
    buffer swap current-dir$ count compareia     \ Checks if the current path is right
        if    execute                            \ Install actions when the path is not right
              current-dir$ count write-installer \ Writes the path in installer.log
        else  drop
        then
    execute     \ Starts the application
 ;

\s
