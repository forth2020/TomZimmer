\    File: Statbar.f
\  Author: Jeff Kelm
\ Created: 24-Sep-1998
\ Updated: 15-Oct-1998
\ Classes to handle Statusbars (simple and multipart)



NEEDS WinBase.f



\ Notes:
\ 1) SBARS_SIZEGRIP don't appear to work.  Always shows up with the size grip
\    box.



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Simple Statusbar Class
\
:Class Statusbar  <Super BaseChildWindow

      INT StatusID    \ command identifier for statusbar
      INT BorderStyle \ style of border to use

:M DefStyle: ( -- style)   \ override if another style is needed
      WS_BORDER WS_VISIBLE OR   \ WS_CHILD is forced
      ;M

:M RaisedBorder: ( -- )   \ text drawn below border (default)
      0 TO BorderStyle
      ;M

:M NoBorder: ( -- )   \ text drawn at border level (no border)
      SBT_NOBORDERS TO BorderStyle
      ;M

:M SunkenBorder: ( -- )   \ text drawn above border
      SBT_POPOUT TO BorderStyle
      ;M

:M ClassInit: ( -- )   \ initialize class
      ClassInit: super
      RaisedBorder: self           \ default text lower than border
      ;M

:M Create: ( hParent)   \ creates an empty statusbar in parent window
      PutParent: self              \ Parent Window handle
      CreateNewID TO StatusID      \ Statusbar ID
      StatusID  GetParent: self
      NULL                         \ initial string to display
      DefStyle: self  WS_CHILD OR  \ style
      Call CreateStatusWindow  DUP PutHandle: self  ?WinError
      0 TRUE SB_SIMPLE SendMessage: self DROP
      ;M

\ NULL MinHeight: self  appears to reset to the default height statusbar
:M MinHeight: ( #pixels)   \ set minimum height of text region (not including borders)
      0 SWAP SB_SETMINHEIGHT SendMessage: self DROP
      ;M

:M GetBorders: ( -- hWidth vWidth divWidth)   \ returns the border widths in pixels
      HERE rel>abs 0 SB_GETBORDERS SendMessage: self ?WinError
      HERE DUP @  SWAP CELL+ DUP @  SWAP CELL+ @
      ;M

:M Redraw: ( -- )   \ redraw the statusbar after changes (e.g., size)
      0 0 WM_SIZE SendMessage: self DROP
      ;M

:M SetStyle: ( style)   \ set style of statusbar "on the fly"
      GWL_STYLE GetHandle: self Call SetWindowLong DROP
      Redraw: self
      ;M

:M GetStyle: ( -- style)   \ get current style of statusbar
      GWL_STYLE GetHandle: self Call GetWindowLong
      ;M

:M SetText: ( szText)
      rel>abs 255 BorderStyle OR SB_SETTEXT SendMessage: self ?WinError
      ;M

:M GetID: ( -- id)
      StatusID
      ;M

;Class



\ \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
\ \\\ Multipart Statusbar Class
\
:Class MultiStatusbar <Super Statusbar

      INT nParts    \ number of parts in statusbar
      INT aWidths   \ address of widths table

:M Create: ( hParent)
      Create: super
      0 FALSE SB_SIMPLE SendMessage: self DROP
      ;M

:M SetParts: ( aWidths nParts)   \ width table address and count
      TO nParts
      TO aWidths
      aWidths rel>abs nParts SB_SETPARTS SendMessage: self ?WinError
      ;M

:M GetParts: ( -- aWidths nParts)   \ retrieve data structure info
      aWidths  nParts
      ;M

:M SetSimple: ( flag)
      0 SWAP SB_SIMPLE SendMessage: self DROP
      ;M

:M SetText: ( szText n)   \ set text in n'th part
      BorderStyle OR  SWAP rel>abs SWAP
      SB_SETTEXT SendMessage: self ?WinError
      ;M

;Class

