\ CALLBACK.F    Windows Callback support                by Tom Zimmer

\ callback.f beta 4.1B 2002/11/26 arm Callback changes to use kernel callback functions
\ callback.f beta 4.1C 2002/11/26 arm Callback changes to use assembler

cr .( Loading Windows Callback...)
cr .( -- BETA CALLBACK.F V4.9D)

\ --------------------- Callback Support ------------------------

\ CALLBACK-RETURN and CALLBACK-BEGIN restore and save regs, set up EBP and ESP

in-application
internal

CFA-CODE CALLBACK-BEGIN                         \ general start code
                pop     ecx                     \ return address
                pop     eax                     \ thunk address
                push    esi                     \ save regs
                push    edi
                push    ebx
                push    ebp
                mov     eax, 4 [eax]            \ eax=cfa of word to execute

                call    @@1
@@1:            pop     edi
                sub     edi, # HERE 1- a;       \ edi = forth base, here 1- is @@1

                mov     edx, fs: 0x14           \ edx is now ptr TIB pvArbitrary
                push    SP0 [UP]                \ save sp0 on stack
                mov     ebp, esp                \ adjust ebp for return stack

                sub     esp, # 4096 32 -        \ room for return stack (Not RSTACKSIZE, please!)
                and     esp, # -16              \ align to 16 byte boundary
                mov     ebx, esp                \ stack pointer
                sub     ebx, edi                \ abs>rel
                mov     SP0 [UP]  , ebx         \ reset SP0
                push    ecx                     \ restore ret addr
                ret                             \ back to caller
END-CODE

CFA-CODE CALLBACK-RETURN                        \ general return code, restores all but ecx!
                pop     ecx                     \ thunk return
                mov     eax, ebx                \ return value
                mov     esp, ebp
                pop     SP0 [UP]
                pop     ebp
                pop     ebx
                pop     edi
                pop     esi
                push    ecx                     \ restore ret addr
                ret                             \ back to caller
END-CODE

\ --------------------------------------------------------------------------

in-system
external

: BUILD-CALLBACK ( n1 -- a1 a2 )        \ define a callback procedure
                        >r                                \ save number of arguments
                HERE    here    cell+ ,                   \ callreturn:
                        here    cell+ ,
[ also assembler ]      call    callback-return
                        ret     # R@ CELLS  a;            \ return deleting arguments

                HERE    call    callback-begin
[ previous ]            r@ 1 >
                        if      r@ 1- 0
                                do
[ also assembler ]                 push    i cells 6 CELLS+ [ebp] a;
[ previous ]                    loop
                        then
                        r@
                        if
[ also assembler ]              mov     ebx, r@ 1- cells 6 CELLS+ [ebp] a;
[ previous ]            then    r>drop
                        >r >r
[ also assembler ]      lea     esi, r> ( xxx ) [edi] a; \ callreturn:
                        exec a;                         \ go for it

[ previous ]     align  here                            \ a1=THE CALLBACK ADDRESS
[ also assembler ]      nop
                        nop
                        nop
                        call    r> ( yyy ) a;  \ callforth:
[ previous ]            here , here 0 , ;
: callback      ( n1 -<name function>- )        \ define a callback with "name" that has n1 arguments
                ( -- a1 )                       \ runtime, returns address of callback
                build-callback >r constant ' r> ! ;

: CallBack:     { #args \ addr temp$ -<name>- } \ makes a headerless callback function, only
                ( -- a1 )                       \ the callback structure has a header
                #args build-callback to addr
                64 localAlloc: temp$            \ use a dynamic string buffer
                s" constant &"      temp$  place
                >in @ bl word count temp$ +place
                >in !                           \ get a copy of next word in input stream
                temp$ count evaluate            \ make a constant starting with '&'
                :                               \ build the colon definitions starting structure
                here body> addr ! ;             \ patch callback structure

in-application

\ *************************************************************************
\ ***************** An example of how to use a callback *******************
\ *************************************************************************

\ The "EnumFonts" windows call requires an application callback that will be
\ called repeatedly to process each font in the system. We are just
\ displaying the fonts, so we just look at the "dwType" to decide how to
\ display each font.

\ NOTE!: CallBack: creates TWO definitions!  The first has the name you specify,
\       and the second has the same name, prefixed with a '&' meaning 'address of'
\       This second definition is the one which returns the address of the
\       callback, and must be passed to Windows.

4 CallBack: FontFunc { lplf lptm dwType lpData -- int }
                cr                                      \ rls - many additions
                dwType
                dup TRUETYPE_FONTTYPE and
                IF      ."     "
                ELSE    ." Non-"
                THEN    ." TrueType "
                dup RASTER_FONTTYPE and
                IF      ." Raster "
                ELSE    ." Vector "
                THEN
                DEVICE_FONTTYPE and
                IF      ." Device "
                ELSE    ." GDI    "
                THEN
                lplf abs>rel 28 + LF_FACESIZE 2dup 0 scan nip - type
                cr 5 spaces
                lplf abs>rel dup @ 4 .r         \ height
                4 + dup @ 4 .r                  \ width
                4 + dup @ 6 .r.1                \ escapement angle
                4 + dup @ 6 .r.1                \ orientation angle
                4 + dup @ 4 .r                  \ weight
                4 + dup c@ 1 and 2 .r           \ italics
                1 + dup c@ 1 and 2 .r           \ underline
                1 + dup c@ 1 and 2 .r           \ strike-out
                1 + dup c@ 4 .r                 \ character set
                1 + dup c@ 2 .r                 \ output precision
                1 + dup c@ 4 .r                 \ clip precision
                1 + dup c@ 2 .r                 \ output quality
                1 +     c@ 4 h.r                \ family and pitch
                1 ;             \ return "1=success" flag to windows

\ this callback as specified by "EnumFonts" passes four (4) parameters to
\ the callback procedure, so we must say "4 CallBack: FontFunc" to define
\ a callback that accepts four parameters.

\ the callback name is passed to windows as shown, after converting the
\ callback address to an absolute address for windows.

: .fonts        ( -- )
                cr 5 spaces
                ."   ht wide  esc  ornt wt  I U S set p  cp q  fp"
                0
                &FontFunc rel>abs
                0
                conDC                   \ the Device Context of the DC
                Call EnumFonts drop ;

\ *************************************************************************
\ **************** allow Forth to handle windows messages *****************
\ *************************************************************************
-1 value WM_WIN32FORTH

: HandleMessages { pMsg -- 0 }
                pMsg TRUE msg-chain do-chain nip
                if      pMsg Call TranslateMessage drop
                        pMsg Call DispatchMessage  drop
                then    0 ;

1 callback &HandleMessages HandleMessages  \ create the callback for DoForthFunc

: HandleWindowsMessages { hwnd msg wParam lParam -- flag }
                msg WM_WIN32FORTH =
                if      hwnd msg wParam lParam
                        forth-msg-chain do-chain        \ perform the handlers
                        4drop
                        0
                else    hwnd msg wParam lParam
                        DefaultWindowProc               \ use default handler
                then    ;

4 callback &WindowsMessages HandleWindowsMessages

: HandleMessages-init ( -- )
                &HandleMessages  rel>abs &message-callback !
                &WindowsMessages rel>abs &winmessage-callback !
                ;

HandleMessages-init

initialization-chain chain-add HandleMessages-init

module
