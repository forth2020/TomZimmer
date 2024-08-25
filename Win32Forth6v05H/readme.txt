Win32Forth V6.05.0100 2022 Aug 03 for Windows

Type some Forth words and press the Enter key to make them happen.

Double click win32for.exe to run this version of ANS Win32Forth.
You can change directory and view your current with  CD ( like DOS )

Alternatively, right click win32for.exe and create a shortcut to it, then right click on this shortcut and edit the start directory.
When you double click the short cut, you will be running Forth in the specified directory.

When you are in the required directory, type  tt  and press enter to load the file xxxx.f , where xxxx is the directory you are in.

If you choose to name your main load file after the directory it is located in,  tt  will load your application.
It is also nice to name your main application word after the directory too...

Type :
loc myword
to locate the word myword. The source is displayed in the Forth console window.
Type :
g
to see the source in the editor, which is WinEd by default.
You can edit win32for.cfg  to specify which editor program to run for browse ( read only ) and editor functions.

Type :
empty tt
to start Forth again, the reload you application

Example :

cd          \ to see where you are
cd cweed    \ to change to the Cweed directory
tt          \ to load Cweed.f . You could also type  include cweed.f ( the .f is added for you )
cweed       \ to run the Cweed program

For more help, please go to the  comp.lang.forth  newsgroup ( you can Google for this ), or email me at howerd@inventio.co.uk

Enjoy!

Howerd Oakford   www.inventio.co.uk

 