\ WINMULTI.F                                    by Wolfgang Engler

\ A simple example program to demonstrate multiple window, each having
\ its own unique menubar.

\ August 2nd, 1996 - 9:21 tjz
\ Slight modifications for compatibility with Win32Forth version 3.2.

0 value HistoryWindow

 : IDM_Alpha1   z" press this button"
               z" Alpha1 button"
               MB_Ok
               Messagebox: HistoryWindow drop ;

: IDM_Beta1     z" press one button"
               z" Beta1 button"
               MB_Ok
               Messagebox: HistoryWindow  drop ;

 MenuBar MyMenu1
     popup " T e x u s  archive"
         menuitem "Orsa"  IDM_Alpha1   ;
         menuitem "Magn"  IDM_Beta1    ;
         menuitem "Quit"  bye          ;
 endbar

\ Define an object "Texus" that is a super object of class "Window"

:Object Texus  <Super Window


:M On_Init:     ( -- )
                On_Init: super
                MyMenu1 SetMenuBar: self
                ;M

:M StartSize:   ( -- w h )
                200 200
                ;M

:M StartPos:    ( -- x y )
                200 200
                ;M

:M WindowTitle: ( -- Zstring )
                z" Texus Hardware"
                ;M

;object

: DemoTexus          ( -- )
                 Start: Texus  ;
: UndemoTexus
                  Close: Texus ;

  \ ----------------------------------------------------------------

 : IDM_Alpha   z" press this button"
               z" Alpha button"
               MB_Ok
               Messagebox: HistoryWindow drop ;

: IDM_Beta     z" press one button"
               z" Beta button"
               MB_Ok
               Messagebox: HistoryWindow  drop ;

 MenuBar MyMenu
     popup " Selection"
         menuitem " TEXUS"  DemoTexus   ;
         menuitem " MAXUS"  IDM_Beta    ;
         menuitem "Quit"    bye         ;
 endbar

\ ------------------------------------------------------------

\ Define an object "History" that is a super object of class "Window"

:Object History     <Super Window

:M ClassInit:   ( -- )
                ClassInit: super
                self to HistoryWindow
                ;M

:M On_Init:     ( -- )
                On_Init: super
                MyMenu SetMenuBar: self
                ;M

:M StartSize:   ( -- w h )
                200 200
                ;M

:M StartPos:    ( -- x y )
                200 200
                ;M

:M WindowTitle: ( -- Zstring )
                z" History of Hardware"
                ;M

\ This is your main window, so it should have the following two definitions,
\ so your program will terminate if you close the window.  If you don't,
\ it won't quit. July 17th, 1996 - 9:20 tjz

:M On_Done:     ( h m w l -- res )
                0 Call PostQuitMessage drop
                On_Done: super
                0 ;M

:M WM_CLOSE     ( h m w l -- res )
                WM_CLOSE WM: super
                bye
                0 ;M

;Object

: DEMO          ( -- )
                Start: History ;

: UNDEMO        ( -- )
                Close: History ;

with-img ' demo TURNKEY WINMULTI

