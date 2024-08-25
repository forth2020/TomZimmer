
fload testdll.h

: MYWORDS       ( -- )
                words ;

: MyDoForthFunc { lParam WParam message function -- return }
                function
                case
                        F_WORDS of      mywords         endof
                                        beep
                endcase ;

' MyDoForthFunc is DLLFunctionLink

fsave forthdll

