anew msgtree.f  \ October 10th, 2002 - 21:03

also forth

\ Based on TreeView.F

(( TreeView.F   A rudimentary TreeView class              by Michael Hillerström
                                                     michael-hillerstrom@usa.net


                This TreeView class hooks into Windows own library class.
                But be warned; this is a very 'stripped to the bone implementation'
                i.e. it has just what I need for DiaEdit...  Some day (soon) I
                will try to correct this.

                Please note that this code needs a new version of WINCON.DLL
                (dated September 15, 1997 or later).

                An example is included last in this file...


                Any comments/suggestions to:    michael-hillerstrom@usa.net



        Change log:
                November 23rd, 1997 - 21:59 MIH
                Added the line:   WinLibrary COMCTL32.DLL   to this source.

                September 16th, 1997 - 21:42 MIH
                Removed reference to COMMCTRL.F as Tom Zimmer has released an
                extended WINCON.DLL.  Thanks, Tom!

                August 31st, 1997 - 23:15 MIH
                First attempt...(which is VERY bare bones...)
                Need to convince Tom Zimmer to include #define's from COMMCTRL.H
                in WINCON.DLL.  For the time beeing, we'll have to cope with my
                FORTH constants in COMMCTRL.F.

July 25th, 2002 - 14:33 Customized for 4ePost by J.v.d.Ven.

))

\ .(      Loading trial version TreeView Class...) cr


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Establish Win32Forth version...   ( disabled )
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
(( defined IsVersion33  nip 0= [if]
version# 330000 339999 between value IsVersion33
version# 340000 349999 between value IsVersion34
version# 350000 509999 between value IsVersion35
[then]

 IsVersion33 IsVersion34 or  IsVersion35 or  not  [if]
        cr .( Sorry, but TreeView.F will only compile on version 3.3, 3.4, or 3.5)
        cr quit
[then]  ))


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       Prerequisites...
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

WinLibrary COMCTL32.DLL         \ Make sure that ComCtl32.dll is loaded...


classes defined ;RecordSize: nip 0= [if]

also definitions                \ stolen from Win32For35...

0 value BeginningOfRecordAddress

: Record:       ( -- )  \ define a word that returns the starting address of
                        \ a group of data fields that need be contiguous
                -1 to contiguous-data?
                header
                (&iv) ,         \ return address of array of bytes
                ^Class DFA @ dup , to BeginningOfRecordAddress
                (iv!) ,         \ store integer into first cell of array ??
                (iv+!) , ;      \ add integer to first cell of array     ??

: ;Record       ( -- )  \ end a group of data fields that need to contiguous
                0 to contiguous-data? ;

: ;RecordSize:  ( 'name' -- ) \ create a name with the size of the record
                0 to contiguous-data?
                ^Class DFA @ BeginningOfRecordAddress - CONSTANT ;

only forth also definitions

[then]


\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       TreeView Constants and their significance...
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

comment:

A treeview can have a combination of the following styles:


TVS_DISABLEDRAGDROP  disables drag n' drop of tree-view items.

TVS_LINESATROOT      draws lines linking child items to the root of the hierarchy.

TVS_HASLINES         enhances the graphic representation of a tree-view by
                     drawing lines that link child items to their parent item.
                     To link items at the root of the hierarchy, you need to
                     combine this and the TVS_LINESATROOT style.

TVS_HASBUTTONS       adds a button to the left side of each parent item. The
                     user can click the button to expand or collapse the child
                     items as an alternative to double-clicking the parent
                     item. To add buttons to items at the root of the
                     hierarchy, this style must be combined with TVS_HASLINES,
                     and TVS_LINESATROOT.

TVS_EDITLABELS       makes it possible for the user to edit the labels of
                     tree-view items.

TVS_SHOWSELALWAYS    causes a selected item to remain selected when the
                     tree-view control loses focus.


comment;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       TreeView Class...
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

:Class TreeViewControl <super Child-Window

     Record:    nmhdr
        int     hWndFrom
        int     idFrom
        int     code
    ;RecordSize: /nmhdr

     Record:    tvitem
        int     mask
        int     hItem
        int     state
        int     stateMask
        int     pszText
        int     cchTextMax
        int     iImage
        int     iSelectedImage
        int     cChildren
        int     lParam
    ;RecordSize: /tvitem

     Record:    tvins
        int     hParent
        int     hInsertAfter
\   TV_ITEM     item
/tvitem bytes   item
    ;RecordSize: /tvins

\    Record:    tvdi
\     NMHDR     hdr
\   TV_ITEM     item
\   ;Record


     Record:    tvkd
\     NMHDR     hdr
        int     wVKey
        int     flags
    ;RecordSize: /tvkd


     Record:    nmtv
\     NMHDR     hdr
        int     action
\   TV_ITEM     itemOld
        int     maskOld
        int     hItemOld
        int     stateOld
        int     stateMaskOld
        int     pszTextOld
        int     cchTextMaxOld
        int     iImageOld
        int     iSelectedImageOld
        int     cChildrenOld
        int     lParamOld
\   TV_ITEM     itemNew
        int     maskNew
        int     hItemNew
        int     stateNew
        int     stateMaskNew
        int     pszTextNew
        int     cchTextMaxNew
        int     iImageNew
        int     iSelectedImageNew
        int     cChildrenNew
        int     lParamNew
\     POINT     ptDrag
        int     x
        int     y
    ;RecordSize: /nmtv

    Record:  lv_column
      int col.mask          \ Flags indicating valid fields
      int col.fmt           \ Column alignment
      int col.cx            \ Column width
      int col.pszText       \ Address of string buffer
      int col.cchTextMax    \ Size of the buffer
      int col.iSubItem      \ Subitem index for this column
   ;RecordSize: /lv_column

: fill-nmhdr    ( l -- )
                abs>rel
                nmhdr   /nmhdr   2dup erase
                                      move
                ;
: fill-tvkd     ( l -- )
                abs>rel 3 cells+
                tvkd    /tvkd    2dup erase
                                      move
                ;
: fill-tvitem   ( l -- )
                abs>rel 3 cells+
                tvitem  /tvitem  2dup erase
                                      move
                ;

: fill-nmtv     ( addr -- )
                abs>rel 3 cells+
                nmtv    /nmtv    2dup erase
                                      move
                ;

: tvitem->tvins ( -- )
                tvitem item /tvitem   move ;


\ -------------------- Create Tree-View Control --------------------

create treeview-class   z," SysTreeView32"      \ Pre-registered class

: create-treeview ( -- hWnd )
                \ Make sure Common Controls are loaded
                Call InitCommonControls drop

                NULL                            \ Creation parameter
                appInst                         \ Instance handle
                id                              \ Child id
                Parent conhndl =
                if      conhndl
                else    GetHandle: Parent       \ parent window handle
                then
                tempRect.AddrOf GetClientRect: Parent
                tempRect.Bottom tempRect.Right  \ Size h,w
                0 0                             \ Position y,x
                WindowStyle: [ self ]           \ Style
                NULL                            \ Window name
                treeview-class rel>abs          \ Pre-registered class
                0                               \ Extended style
                Call CreateWindowEx   
                ;


:M WindowStyle: ( -- style )
                WS_CHILD
                WS_VISIBLE          or
                ;M

:M Start:       ( Parent -- )
                hWnd
                if      drop
                        SW_SHOWNOACTIVATE Show: self
                else
                        to Parent
                        create-treeview to hWnd
                then
                ;M

:M AutoSize:    ( -- )
                tempRect.AddrOf GetClientRect: Parent
                0 0 tempRect.Right tempRect.Bottom  \ x,y,h,w
                Move: self
                ;M

:M Handle_Notify: ( h m w l -- f )
                dup fill-nmhdr
                code
                case
                   TVN_BEGINDRAGA      of  fill-nmtv   On_BeginDrag:      [ self ] endof
                   TVN_BEGINRDRAGA     of  fill-nmtv   On_BeginRDrag:     [ self ] endof
                   TVN_BEGINLABELEDITA of  fill-tvitem On_BeginLabelEdit: [ self ] endof
                   TVN_DELETEITEMA     of  fill-nmtv   On_DeleteItem:     [ self ] endof
                   TVN_ENDLABELEDITA   of  fill-tvitem On_EndLabelEdit:   [ self ] endof
                   TVN_GETDISPINFOA    of  fill-tvitem On_GetDispInfo:    [ self ] endof
                   TVN_ITEMEXPANDEDA   of  fill-nmtv   On_ItemExpanded:   [ self ] endof
                   TVN_ITEMEXPANDINGA  of  fill-nmtv   On_ItemExpanding:  [ self ] endof
                   TVN_KEYDOWN         of  fill-tvkd   On_KeyDown:        [ self ] endof
                   TVN_SELCHANGEDA     of  fill-nmtv   On_SelChanged:     [ self ] endof
                   TVN_SELCHANGINGA    of  fill-nmtv   On_SelChanging:    [ self ] endof
                   TVN_SETDISPINFOA    of  fill-tvitem On_SetDispInfo:    [ self ] endof
                          false swap ( default)
                endcase
                ;M

                    \  lParam  wParam
:M ToggleExpandItem:  ( hItem -- )
                 TVE_TOGGLE  TVM_EXPAND hWnd Call SendMessage drop
                ;M

:M ExpandItem:  ( hItem -- )
                TVM_EXPAND TVM_EXPAND hWnd Call SendMessage drop
                ;M

:M DeleteItem:  ( hItem -- )   \ takes time
                0 TVM_DELETEITEM  hWnd Call SendMessage drop
                ;M

:M AddItem:     ( hAfter hParent sztext nChildren rec-ptr state -- hPrev )
                ( state )        to state
                               -1 to statemask
                ( rel-rec-ptr )   to lParam
                ( nChildren)      to cChildren    \ Depth in tree
                rel>abs           to pszText
                ( hAfter)         to hInsertAfter  \ son
                ( hParent)        to hParent       \ hprev
                [ TVIF_TEXT TVIF_CHILDREN or TVIF_PARAM or TVIF_STATE or ] literal to mask
                tvitem->tvins
                tvins rel>abs 0 TVM_INSERTITEMA hWnd Call SendMessage
                ;M

\ --------------------- Overridable methods ----------------------

:M On_BeginDrag: ( -- f )
                false
                ;M
:M On_BeginRDrag: ( -- f )
                false
                ;M
:M On_BeginLabelEdit: ( -- f )  \ f=true, cancel edit,  f=false, ok edit
                false
                ;M
:M On_DeleteItem: ( -- f )
                false
                ;M
:M On_EndLabelEdit: ( -- f )
                false
                ;M
:M On_GetDispInfo: ( -- f )
                false
                ;M
:M On_ItemExpanded: ( -- f )
                false
                ;M
:M On_ItemExpanding: ( -- f )   \ f=true, don't expand/collapse, f=false, ok go ahead
                false
                ;M
:M On_KeyDown: ( -- f )
                false
                ;M
:M On_SelChanged: ( -- f )
                false
                ;M
:M On_SelChanging: ( -- f )     \ f=true, don't change, f=false, ok change
                false
                ;M
:M On_SetDispInfo: ( -- f )
                false
                ;M
;Class




\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\       End  of Class definition(s)
\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


0 value hmessages
0 value hadresbook
0 value 4ePost-hndl

: check-account  ( - )
    database-hdr-hndl size-database 1 =
      if account>database save-cluster-to-ini
      then
 ;

: _messages>treeview ( #end #start -- )
     ?do   i
           n>record dup>r  message?
                if    r@ &R-cluster @  r>record  &h-cluster @ hmessages
                      r@ include-subject-flag 0 r> record>r TVIS_BOLD AddItem: treeview drop
                else  r>drop
                then
     loop
 ;

' _messages>treeview is messages>treeview

0 value deleting?  \ Needed in W95

: delete-from-treeview ( hitem - )
   4ePost-hndl call LockWindowUpdate drop \ For a better speed
   true to deleting?
   DeleteItem: treeview
   false to deleting? 
   0 call LockWindowUpdate drop
   ;

: _open-cluster-in-treeview  (  cluster - rec-adr )
     r>record dup &h-cluster @ ExpandItem: treeview ;

' _open-cluster-in-treeview is open-cluster-in-treeview

: open-addressbook-in-treeview  (  - )  hadresbook ExpandItem: treeview  ;

: open-clusters-in-treeview ( - )
    &last-cluster
        begin   open-cluster-in-treeview
                next-group @ dup
        0= until
   drop hmessages ToggleExpandItem: treeview
;

: cluster>treeview ( hndl record sublevel - handle-item )
   >r >r TVI_SORT r@ msg-Subject 2r> record>r 0 AddItem: treeview
 ;

: clusters>treeview
    &last-cluster
        begin
            r>record dup>r msg-flag c@ dup adresbook- >
                 if     hmessages r@ 1 cluster>treeview r@ &h-cluster !
                 then
            adresbook- account- between
                 if hadresbook r@ 0 cluster>treeview drop \ r@ &h-cluster !
                 then
            r> next-group @ dup
        0= until
    drop
 ;

: root-items   >>>
    TVI_ROOT TVI_LAST  z" Spam"        1  0 0 AddItem: treeview
    spambox >record &h-cluster  !
    TVI_ROOT TVI_LAST  z" Addressbook" 1  0 0 AddItem: treeview to hadresbook
    TVI_ROOT TVI_LAST  z" Messages"    1  0 0 AddItem: treeview to hmessages
    clusters>treeview
 ;

: FillTreeView  ( start #end -- )
    check-account    swap 2>r
    root-items
    2r>  messages>treeview
    open-clusters-in-treeview
 ;

create Prg-name   ," 4ePost "

90 string: title$

: update-title ( adr count - )
   70 min Prg-name count title$ place title$ +place title$ +null
 ;

s"  "  update-title

: browse-bdy/hdr ( rec bdy/flag - )
    deleting?
      if exit
      then
    over record-ID c@ cluster- >=
       if     2drop
       else   >r dup record>r to &msg-viewing
              dup msg-From over cnt-From c@
              update-title pad off
              r> bdy>pad
              cur-line off
              pad count file-exist? not abort" File does not exist."
              0 pad $browse
        then
 ;

: show-header ( - )
   last-selected>record   false browse-bdy/hdr  ;

: del-msg/cluster ( - )
   last-selected>record delete-item
      if   hItem-last-selected  delete-from-treeview
      then
 ;

:Class NewTVC <super TreeViewControl


:M WindowStyle: ( -- style )
                 WindowStyle: super
                WS_BORDER or WS_CHILD or WS_VISIBLE or
                LVS_REPORT or
                TVS_HASLINES        or
                TVS_SHOWSELALWAYS   or
                TVS_LINESATROOT     or \ ))
                ;M

:M Start:       ( Parent -- )
                 Start: super
                tvins  /tvins  erase
                tvitem /tvitem erase
                map-msg-base
                0  database-hdr-hndl #records-in-database FillTreeView
                check-editor
                ;M


:M On_SelChanged: ( -- f )      \ Get lParam
                TVIF_PARAM
                TVIF_HANDLE or  to mask
                hItemNew        to hItem
                0               to pszText
                0               to cchTextMax
                tvitem rel>abs 0 TVM_GETITEMA  hWnd Call SendMessage drop
                hItem to hItem-last-selected
                lParam dup to last-selected-rec r>record true browse-bdy/hdr
                false
                ;M


;Class

#define INTERNET_OPEN_TYPE_PRECONFIG    0   // use registry configuration
#define INTERNET_OPEN_TYPE_DIRECT       1   // direct to net
#define INTERNET_OPEN_TYPE_PROXY        3   // via named proxy

#define INTERNET_FLAG_NO_CACHE_WRITE    0x04000000  // don't write this item to the cache
#define INTERNET_FLAG_DONT_CACHE        INTERNET_FLAG_NO_CACHE_WRITE
#define INTERNET_FLAG_MAKE_PERSISTENT   0x02000000  // make this item persistent in cache
#define INTERNET_FLAG_OFFLINE           0x01000000  // use offline semantics

0 value Ihndl
create my-ip-name

: init-socket
   SocketsStartup  abort" SocketsStartup error."
   cr  ." IP: " my-ip-addr   NtoA type
  ;

: connect-to-the-internet ( - )
   init-socket
   0 Call InternetAttemptConnect  abort" Failed to connect."
   cr
 ;

: disconnect-from-the-internet
   cr cr ." Disconnecting."
   0 call InternetAutodialHangup drop
   get-smtp-server not abort" You are still ONLINE!" drop
   cr ." Disconnected."
   gethandle: treeview SocketsCleanup drop
 ;

: .addressbook
    wait-cursor
    0 delete-from-treeview
    root-items
    open-addressbook-in-treeview
 ;

: .new ( - )   \ data-base must mapped
   wait-cursor
   0 delete-from-treeview
    database-hdr-hndl #records-in-database dup #new-mail -
    dup n>aptr #new-mail dup 1 >
       if   shell-rel
       else 2drop
       then
    swap FillTreeView
    arrow-cursor
 ;


: _refresh-treeview ( - )   0 #new-mail .new ;

' _refresh-treeview is refresh-treeview

: .all
   0 to #new-mail
   0  delete-from-treeview
   0  database-hdr-hndl #records-in-database  FillTreeView
   arrow-cursor
 ;

: .all-sorted
   wait-cursor
   cr aptrs database-hdr-hndl #records-in-database dup .
   ." Records. Sort time: "
   muTime shell-rel .muTime
   .all
 ;

: get-mail-and-news
   cls 0 to #new-mail
   0 to &r>group
   get-smtp-server
       if   drop connect-to-the-internet get-smtp-server abort" No smtp-server online."
       then
   decimal
   muTime
   cr ." Smtp-server: "  NtoA type  init-smtp to smtp-socket
   open-nntp-server
   wait-cursor
   send-messages    \ + news
   smtp-socket  CloseSocket throw

   wait-cursor start-news   close-nntp-server

   wait-cursor
   cr cr .gmt
   cr ." Pop3-server: " get-pop3-server abort" No pop3-server online."
   NtoA type
   init-pop3 to pop3-socket
   wait-cursor   mail-authorize mail-transactions
   pop3-socket CloseSocket drop
   .muTime
   mAutodisconnect- @
      if    disconnect-from-the-internet
      else  cr cr ." Still online."
      then
   cr .gmt cr

   unmap-msg-base map-msg-base
   .new
   arrow-cursor
 ;

0 value last-read

: show-sorted ( - )    #new-mail  if .new else .all-sorted  then  ;

: bye-mail ( - )
   save-defaults-to-ini
   new-msg-buffer free
   receive-buffer free
   bye
 ;

defer refresh-menu

: invert-check ( check  - )  dup @ not swap ! refresh-menu    ;

menubar 4ePostmenu
  popup "&Internet"
     menuitem "Get/send &mail and news"   'M' +k_control get-mail-and-news ;
     menuitem "&Connect"                  'C' +k_control connect-to-the-internet ;
     menuitem "&Disconnect"               'D' +k_control disconnect-from-the-internet ;
  menuseparator
     menuitem "&Exit"                     'E' +k_control bye-mail ;
  popup "&Messages"
     menuitem "Prepare a reply or &new message" 'N' +k_control prepare-message ;
     menuitem "&Send message"             'S' +k_control msg>outbox ;
  menuseparator
     menuitem "Show database &entry"      'E' +k_control show-entry ;
     menuitem "Show &header file"         'H' +k_control show-header ;
     menuitem "Show all &messages"        'M' +k_control .all ;
\    menuitem "&Save"                     'S' +k_control noop ;
  popup "&Accounts"
     menuitem "Add to &addressbook"        'A' +k_control Person>database .addressbook ;
  menuseparator
     menuitem "Add &mailgroup or list"    'M' +k_control MsgGrp>database .new ;
     menuitem "&Subscribe to newsgroup"   'S' +k_control group>database  .new ;
     menuitem "Maximum &news messages to download"  'N' +k_control ask-maxDwn ;
  menuseparator
     menuitem "Change local &password"    'P' +k_control change_password ;
     menuitem "Change &spam string"       'S' +k_control change_spam$  ;
     menuitem "Change &timezone"          'T' +k_control ask_timezone ;
  menuseparator
        submenu  "Ser&vers"
            menuitem "Change &Pop3 server for incomming mail" 'P' +k_control ask_pop3server ;
            menuitem "Change &Smtp server for out going mail" 'S' +k_control ask_smtp3server ;
            menuitem "Change &Nntp server for news" 'N' +k_control ask_nntpserver ;
        endsubmenu
  popup "&Options"
     menuitem "&Sort by date "             'S' +k_control by-date show-sorted ;
     menuitem "S&ort by subject and date " 'O' +k_control by-subject show-sorted ;
  menuseparator
     menuitem "Start &WinView"             'W' +k_control start-editor  ;
  menuseparator
     menuitem "&Delete item"               'D' +k_control del-msg/cluster ;
  menuseparator
     menuitem "&Delete all messages from cluster" 'A' +k_control mdel-all ;
  menuseparator
   :menuitem mAutodisconnect "Autodisconnect"  mAutodisconnect- invert-check ;
   :menuitem mSpam           "Include spam"    mspam- invert-check ;
  endbar

\ 0 value fill-treeview-from-database

:Object 4ePost  <Super Window

:M On_Init:     ( -- )
                On_Init: super
                New> NewTVC to TreeView
                1001 SetId: TreeView drop
                self Start: TreeView
                4ePostmenu  SetMenuBar: self
                addr: self to msgtree-base
                gethandle: self to 4ePost-hndl
                ;M

:M WindowTitle: ( -- sztitle )
                title$ 1+
                ;M

: windowposition ( - )
   WndRect.AddrOf rel>abs  hWnd Call GetWindowRect
     if   WndRect.Left window_x !
          WndRect.Top  window_y !
          Height  _height !
          Width   _Width  !
    then
 ;

:M WM_MOVE      (  wparam lparam -- res )
                2drop windowposition 0
                ;M

:M StartPos:      ( -- x y )     window_x @ window_y @
                 ;M

:M StartSize:   ( -- w h )
                inifile count  file-exist?
                    if    load-startup-from-ini
                          map-msg-base
                    else  setup-mail-dir map-msg-base
                    then
                _width @ _height @ 21 +  \ 21 + reason unknown
                ;M

:M On_Size:     ( -- )
                 SIZE_MINIMIZED <>
                     if  windowposition
                     then
                AutoSize: TreeView
                ;M

:M Refresh: ( - )
                mAutodisconnect- @  Check: mAutodisconnect
                mSpam- @            Check: mSpam
                ReDrawMenu: CurrentMenu
                  ;M

:M WM_CLOSE     ( h m w l -- res )
                bye-mail
                0 ;M

:M WM_NOTIFY    ( h m w l -- f )
                dup abs>rel @  GetHandle: TreeView  =
                if      Handle_Notify: TreeView title$ count SetTitle: self
                else    false
                then
                ;M
;Object

: _refresh-menu ( - )  Refresh: 4ePost ; \ updates the menu + window
' _refresh-menu is refresh-menu

