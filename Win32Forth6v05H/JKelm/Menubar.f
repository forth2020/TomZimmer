\    File: Menubar.f
\  Author: Jeff Kelm
\ Created: 02-Sep-1998
\ Updated: 06-Oct-1998



NEEDS WinBase.f



: zCount ( szText -- a n)  \ conv to regular string (null not included in n)
   0 >R
   DUP BEGIN  DUP C@  WHILE  R> 1+ >R  1+  REPEAT  DROP R> ;

: ProcMenuText ( szText -- szText)   \ convert \t to <tab> in string
   DUP zCount  S" \t"  SEARCH
   IF  >R 0x09 OVER C!  1+ DUP 1+ SWAP R> MOVE   ELSE  2DROP  THEN ;



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Generic Menu class
\ Note: The AppendMenu function is obsolescent, but still supported.
\       It looks much easier to use than the InsertMenuItem function
\       that replaces it.
:Class GenericMenu  <Super BaseChildWindow

   :M AppendMenu: ( hSubmenu szText flags)
            >R ProcMenuText rel>abs SWAP R>
            GetHandle: self  Call AppendMenu  ?WinError
            ;M

   :M AddSubmenu: ( hSubmenu szText)   \ adds a submenu popup entry
            MF_STRING MF_POPUP OR  AppendMenu: self
            ;M

   :M GetItemCount: ( -- #)   \ returns number of items in popup menu
            GetHandle: self  Call GetMenuItemCount
            ;M

   :M GetString: ( posn -- a n)   \ gets string for menuitem in given position
            \ GetMenuString is obsolescent
            >R MF_BYPOSITION MAXSTRING TEMP$ rel>abs R>
            GetHandle: self  Call GetMenuString  TEMP$ SWAP
            ;M

   :M Redraw: ( -- )   \ redraws the menubar after a change has been made
            GetParent: self  Call DrawMenuBar  ?WinError
            ;M

\ specifying menuitems MUST be by position for menubar items and menuitems that
\ open submenus.
   :M Enable: ( posn)   \ enables menuitem in given position
            MF_BYPOSITION MF_ENABLED OR  SWAP
            GetHandle: self  Call EnableMenuItem  DROP
            ;M

   :M Disable: ( posn)   \ disables menuitem in given position (user can't select)
            MF_BYPOSITION MF_DISABLED OR  SWAP
            GetHandle: self  Call EnableMenuItem  DROP
            ;M

   :M Gray: ( posn)   \ disables & grays menuitem in given position
            MF_BYPOSITION MF_GRAYED OR  SWAP
            GetHandle: self  Call EnableMenuItem  DROP
            ;M

;Class



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Popup Menu class
\
:Class PopupMenu  <Super GenericMenu

   :M ClassInit: ( -- )   \ creates a handle to a blank popup menu
            ClassInit: super
            Call CreatePopupMenu  PutHandle: self
            ;M

   :M AddItem: ( id szText)   \ adds a string-type menuitem w/ optional checkmark
            OVER >R  MF_STRING  AppendMenu: self
            NULL NULL MF_BYCOMMAND R>  GetHandle: self
               Call SetMenuItemBitmaps ?WinError
            ;M

   :M Seperator: ( -- )   \ adds a seperator line
            NULL NULL MF_SEPARATOR AppendMenu: self
            ;M

   :M GetID: ( posn -- id)   \ gets ID for menuitem in given position
            GetHandle: self  Call GetMenuItemID
            ;M

   :M Checked: ( id)   \ turn on checkmark
            MF_CHECKED SWAP GetHandle: self
            Call CheckMenuItem DROP
            ;M

   :M Unchecked: ( id)   \ turn off checkmark
            MF_UNCHECKED SWAP GetHandle: self
            Call CheckMenuItem DROP
            ;M

\ specifying menuitems MUST be by position for menubar items and menuitems that
\ open submenus.  However, command ID is more natural for changing menuitems
\ so override generic's BYPOSITION behavior for popup menus

   :M Enable: ( id)   \ enables menuitem with given id
            MF_BYCOMMAND MF_ENABLED OR  SWAP
            GetHandle: self  Call EnableMenuItem  DROP
            ;M

   :M Disable: ( id)   \ disables menuitem with given id (user can't select)
            MF_BYCOMMAND MF_DISABLED OR  SWAP
            GetHandle: self  Call EnableMenuItem  DROP
            ;M

   :M Gray: ( id)   \ disables & grays menuitem with given id
            MF_BYCOMMAND MF_GRAYED OR  SWAP
            GetHandle: self  Call EnableMenuItem  DROP
            ;M

;Class



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Menubar class
\
:Class Menubar  <Super GenericMenu

   :M ClassInit: ( -- )   \ creates a handle to an empty menubar
            ClassInit: super
            Call CreateMenu  PutHandle: self
            ;M

   :M AddMenu: ( hSubmenu szText)   \ adds a menubar item
            AddSubmenu: self
            ;M

;Class



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Context Menu Class
\
:Class ContextMenu  <Super PopupMenu

:M ShowContextMenu: ( y x -- )
    2>R  NULL GetParent: self 0  2R>
    TPM_LEFTBUTTON TPM_LEFTALIGN  OR
    GetHandle: self  Call TrackPopupMenu  ?WinError
    ;M

;Class
