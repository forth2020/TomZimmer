cr .( Loading Meta Dialogs...)
cr .( -- BETA METADLG.F V3.4D --)

\ metadlg.f BETA 3.4D 10/10/2002 arm new meta dialog for extra parameters

: CURR-ORIGIN ( -- origin )
              &origin @ dup
              if exit
              else rel>abs
              then ;

load-dialog METADLG    \ load the dialogs for meta

\ -------------------- Meta Dialog --------------------


:Object META-DIALOG <SUPER dialog

IDD_META METADLG find-dialog-id constant template

int sysmem
int appmem
int origin
int optexe
int optfixed
int optcon
int optvar
int optnextexec


:M ClassInit:   ( -- )
                ClassInit: super
           &app-size @ 0x10000 naligned to appmem
           &sys-size @ 0x10000 naligned to sysmem
           curr-origin 0x10000 naligned to origin
            IDC_RADEXE to optexe
                     0 to optfixed
                     0 to optcon
                     0 to optvar
                     0 to optnextexec
                ;M

:M ClickFixed:  ( -- )
                IDC_CHKFIXED IsDlgButtonChecked: self
                IDC_RADEXE   IsDlgButtonChecked: self and
                IDC_EDTORIGIN   EnableDlgItem: self
                ;M

:M ClickBuild:  ( flag radio -- )
                IDC_RADEXE IDC_RADDLL CheckRadioButton: self
                dup IDC_CHKNEXT     EnableDlgItem: self
                dup IDC_CHKFIXED    EnableDlgItem: self
                    ClickFixed: self
                ;M

:M On_Init:     ( hWnd-focus -- f )
                appmem 0 <# #s #>               IDC_EDTAPPMEM SetDlgItemText: self
                sysmem 0 <# #s #>               IDC_EDTSYSMEM SetDlgItemText: self
 ( arm fix) hex origin 0 <# # # # # # # # # #> decimal IDC_EDTORIGIN   SetDlgItemText: self
                1 IDC_RADEXE ClickBuild: self
\ dsiable certain buttons
                0 IDC_CHKOPTCON   EnableDlgItem: self
                0 IDC_CHKOPTVAR   EnableDlgItem: self
                0 IDC_RADDLL      EnableDlgItem: self
                0 IDC_BTN_CHKADDR EnableDlgItem: self
                0 IDC_EDTCODEMEM  EnableDlgItem: self
                1 IDC_CHKNEXT     CheckDlgButton: self
                0 IDC_CHKNEXT     EnableDlgItem: self

                1 ;M

:M GetAppMem:   ( -- appmemory )
                appmem
                ;M

:M SetAppMem:   ( appmemory -- )
                TO appmem
                ;M

:M GetSysMem:   ( -- sysmemory )
                sysmem
                ;M

:M SetSysMem:   ( sysmemory -- )
                TO sysmem
                ;M

:M GetOrigin:   ( -- origin )
                origin
                ;M

:M SetOrigin:   ( origin -- )
                TO origin
                ;M        


:M Start:       ( -- n1 )       \ return size of image
                conhndl template run-dialog ;M

:M On_Command:  { hCtrl code ID \ memory$ flag -- f1 }
                64 localAlloc: memory$
                0 to flag
                ID
                case
                IDOK     of     memory$ 31 IDC_EDTAPPMEM GetDlgItemText: self
                                memory$ swap number? abs +to flag drop to appmem
                                memory$ 31 IDC_EDTSYSMEM GetDlgItemText: self
                                memory$ swap number? abs +to flag drop to sysmem
                                memory$ 31 IDC_EDTORIGIN GetDlgItemText: self
                                IDC_RADEXE   IsDlgButtonChecked: self
                                IDC_CHKFIXED IsDlgButtonChecked: self
                                and if
                                  HEX memory$ swap number? abs +to flag drop to origin DECIMAL
                                else 1 +to flag 0 to origin
                                then
                                origin 0xFFFF0000 AND to origin
                                flag 3 =        \ if all are ok, then we are done
                                if      1 end-dialog
                                else    beep
                                then            endof

                IDCANCEL of     0 end-dialog    endof
                IDC_RADEXE of   1 IDC_RADEXE ClickBuild: self endof
                IDC_RADDLL of   0 IDC_RADDLL ClickBuild: self endof
                IDC_CHKFIXED of              ClickFixed: self endof

                        false swap ( default result )
                endcase ;M

;Object



