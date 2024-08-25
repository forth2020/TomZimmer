\ September 10th, 2001 - 15:06 by J.v.d.Ven.  ( http://home.planet.nl/~josv )
\
\ The actions to do for this source are:
\ 1. Download and install the Microsoft speech SDK 5.1
\      Look at: http://microsoft.com/speech then download: speechsdk51.exe 68 MB
\      OR Search for 'sapi 51' when you can not find it.
\ 2. Be sure your soundcard works.
\ 3. Unzip toolset.zip and put its sources in one directory
\ 4. Compile this source.

needs struct.f
needs toolset.f
needs com01.f

anew speak.f  \ for Win32Forth and Windows 98 or better

iid: CLSID_SpVoice  {96749377-3391-11D2-9EE3-00C04F797396}
iid: IID_ISpVoice   {6C44DF74-72B9-4992-A1EC-EF996E0422D4}

struct{  \ ISpVoiceVtbl
\ STDMETHODCALLTYPE QueryInterface
\ STDMETHODCALLTYPE AddRef
\ STDMETHODCALLTYPE Release
interface_IUnknown
    STDMETHODCALLTYPE *SetNotifySink
    STDMETHODCALLTYPE *SetNotifyWindowMessage
    STDMETHODCALLTYPE *SetNotifyCallbackFunction
    STDMETHODCALLTYPE *SetNotifyCallbackInterface
    STDMETHODCALLTYPE *SetNotifyWin32Event
    STDMETHODCALLTYPE *WaitForNotifyEvent
    STDMETHODCALLTYPE *GetNotifyEventHandle
    STDMETHODCALLTYPE *SetInterest
    STDMETHODCALLTYPE *GetEvents
    STDMETHODCALLTYPE *GetInfo
    STDMETHODCALLTYPE *SetOutput
    STDMETHODCALLTYPE *GetOutputObjectToken
    STDMETHODCALLTYPE *GetOutputStream
    STDMETHODCALLTYPE *Pause
    STDMETHODCALLTYPE *Resume
    STDMETHODCALLTYPE *SetVoice
    STDMETHODCALLTYPE *GetVoice
    STDMETHODCALLTYPE *Speak
    STDMETHODCALLTYPE *SpeakStream
    STDMETHODCALLTYPE *GetStatus
    STDMETHODCALLTYPE *Skip
    STDMETHODCALLTYPE *SetPriority
    STDMETHODCALLTYPE *GetPriority
    STDMETHODCALLTYPE *SetAlertBoundary
    STDMETHODCALLTYPE *GetAlertBoundary
    STDMETHODCALLTYPE *SetRate
    STDMETHODCALLTYPE *GetRate
    STDMETHODCALLTYPE *SetVolume
    STDMETHODCALLTYPE *GetVolume
    STDMETHODCALLTYPE *WaitUntilDone
    STDMETHODCALLTYPE *SetSyncSpeakTimeout
    STDMETHODCALLTYPE *GetSyncSpeakTimeout
    STDMETHODCALLTYPE *SpeakCompleteEvent
    STDMETHODCALLTYPE *IsUISupported
    STDMETHODCALLTYPE *DisplayUI
 }struct ISpVoiceVtbl

struct{ \ The buffer for say must contain unicodes
    DWORD  bytes-used
    OFFSET >unicode$
    maxstring 2* _add-struct
}struct max-unicode$

sizeof max-unicode$ mkstruct: unicode
variable &pVoice
variable pulStreamNumber
variable ptext

: init-tts    \ Creates a SpVoice object
  CoInitialize
  &pVoice rel>abs IID_ISpVoice rel>abs CLSCTX_ALL NULL CLSID_SpVoice rel>abs
  call CoCreateInstance ?failed
 ;

\ 80040154 Error: INIT-TTS FAILED ( When TTS is not installed )

: text-in-buffer? ( - flag )    unicode @ 0> ;

\ The Stacknotation: /..~../ means depend on which interface is used

: ->pVoice  ( /..~../ interface - )   &pVoice @ swap std_imethod ;

\ I use an * for an interface and -> before an object
\ to keep the source clear

: say-it ( - )
   unicode @ dup
     if  unicode + cell+ 0 swap ! \ The buffer must be 0 terminated
     else drop
     then
   text-in-buffer? \ Do not send an empty buffer to the tts   \ ISpVoice::Speak
   if   pulStreamNumber rel>abs  null unicode >unicode$ rel>abs *Speak ->pVoice
        0 unicode !
   then
 ;

: say        ( adr count - )
  dup 2* unicode !
  unicode >unicode$ ansi>unicode say-it  ;

(( : exit-tts       \ Does not work yet
    *Release ->pVoice
    0 &pVoice !
    CoUninitialize
 ; ))

: +char>buffer-unicode ( c - )    unicode swap +unicode ;

: (to-say"   ( - )
   ((")) count bounds
     do i c@  +char>buffer-unicode loop
   ;

: say"       ( -<string">- )  compile (to-say" ,"  ; immediate

: lowerc           ( char -- char )        \ convert char to lowercase
    dup  ascii A  ascii Z between
      if   tb 100000 or
      then
 ;

: lc-tts ( c - )   lowerc +char>buffer-unicode  ;

hex

: translate-forth-style ( c - )
   dup
   case
    2e of say"  dot" endof            \ .
    21 of say"  store" endof          \ !
    40 of say"  fetch" endof          \ @
    23 of say"  number" endof         \ #
    24 of say"  dollar" endof         \ $
    25 of say"  percent" endof        \ %
    26 of say"  ampersand" endof      \ &
    5e of say"  caret" endof          \ ^
    2a of say"  star" endof           \ *
    28 of say"  leftparent" endof     \ (
    29 of say"  rightparent" endof    \ )
    2d of say"  dash" endof           \ -
    2b of say"  plus" endof           \ +
    7b of say"  face" endof           \ {
    7d of say"  face" endof           \ }
    5b of say"  squarebracket" endof  \ [
    5d of say"  squarebracket" endof  \ ]
    22 of say"  quote" endof          \ "
    27 of say"  tick" endof           \ "
    7e of say"  tilde" endof          \ ~
    7c of say"  bar" endof            \ |
    5c of say"  backslash" endof      \ \
    2f of say"  slash" endof          \ /
    3c of say"  lessthan" endof       \ <
    3e of say"  greaterthan" endof    \ >
    3f of say"  question" endof       \ ?
    2c of say"  comma" endof          \ ,
    3a of say"  make" endof           \ :
    3b of say"  end make" endof       \ ;
    5f of say"  underline" endof      \ _
    dup  lc-tts
   endcase
   drop
 ;

decimal

defer translation

: forth-style-tts  ( - ) ['] translate-forth-style is translation  ;
: normal-tts       ( - ) ['] +char>buffer-unicode is translation ;
: lowercase-tts    ( - ) ['] lc-tts is translation ;

normal-tts

: talking          ( c - )    dup _emit translation  ;

: (type-talk       ( adr len - )
  dup 0>
   if bounds
      do
          i c@ dup bl <=
            if   drop     \ remove control characters + spaces
                 say-it
            else translation
            then
      loop say-it
  else 2drop
  then    
 ;

: type-talk        ( adr cnt - ) ?mabort "clip" 2dup _type  (type-talk ;

: talk
  0 unicode !
  ['] type-talk is type
  ['] talking is emit
 ;

' screen-only  alias silent  \ silent will switch the tts off in Win32Forth

\ \s    \ activate this line to avoid the example

\ Example:

: test-tts
   s" Hello! "  say    \ For direct use of the tts

   talk                \ Redirects the console also to the tts

   normal-tts
   cr ." normal-tts"
   ['] text-in-buffer? (see)

   lowercase-tts
   cr ." lowercase-tts"
   ['] text-in-buffer? (see)

   forth-style-tts
   cr ." forth-style-tts"
   cr 1 2 123 .s 3drop .s
   ['] text-in-buffer? (see)
   cr ." End of test-tts"
  ;

init-tts   \ use it ONE time to start the tts

test-tts

\ Here is how the tts is able to talk using speak.f:
\ ftype speak.f
\s

