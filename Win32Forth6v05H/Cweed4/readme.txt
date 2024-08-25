Cweed4 V4.07 2022 Aug 03

Run  go.bat  to recompile using the supplied Win32Forth V6.05H (with Win7/8/10/11 support)

Configuration options are at the top of the file Cweed4.f
File parsing is in Files.f

Run Cweed.exe and select the file to tidy up. Press F1 for Help.

Enjoy!
Howerd Oakford www.inventio.co.uk

New in Cweed4 V4.07 2022 Aug 03
Updated the Win32Forth system, removed C# and L# from Cweed4.f because they are now included in the Win32Forth system.

New in Cweed4 V4.06 2022 May 10
TwoBlankLines  shows where there are more than one blank line, except before a "//" comment block
MissingBlankLine shows where a second blankline is missing before a "//" comment block
A "//" comment block is three or more consecutive lines that start with "//"

New in Cweed4 V4.05 2022 May 08
ctrlQ to test for a space after a C++ style comment : "// comment" not, "//comment"
Also checks that the first letter of the comment is a Capital.
+File now shows the start of comment blocks with a '/', or if 1 or less preceding blanklines with a '+'

New in Cweed4 V4.04 2022 Mar 24
Improved the Barr Coding Standards function for “&” used as an address modifier instead of an AND operator, and added the number of errors detected.

New in Cweed4 V4.04 2022 Jan 14
Changes to Cweed4_script.f to improve the Barr 3.1 coding rules.
Used BigReplace to allow long strings to be processed.
Changed “Show Info” menu button to “Refresh”.

New in Cweed4 V4.03 2021 Dec 21
Changes to Cweed4_script.f to improve the Barr 3.1 coding rules.
Added ctrlQ to show and fix lowercase characters after a '// ' comment
Increased Barr Coding Standard buffer size to $400 to allow longer lines to be processed.

New in Cweed4 V4.02
The file Cweed4_script.f is now loaded when running the Barr 3.1 function. 
This allows the rules to be changed and reloaded without re-compiling Cweed.
If the file Cweed4_script.f is not found in the directory where Cweed4 is running, the default file is used
i.e. the one that existed when Cweed4 was compiled.

New in Cweed4 V4.01 
New name – Cweed4 . Searching for “Cweed” online finds lots of sites about “weed” – “Cweed4” is easier to find. The version has also been reset to Cweed4 V4.01.
Added a custom file Cweed4.script for the Barr Coding Standard (2018) – this allows modifications to be made without recompiling Cweed4.

New in V4.25
Press F2 to apply the Barr Coding Standard (2018) Rules 3.1 Spaces a, g, h, i and j.
Added source files fo Win32Forth, so that system words can be viewed.

New in V4.24 
Now packaged with Win32Forth6v05H 2019 Mar 02 which fixes a bug that forced its window to be too small.

New in V4.23 
Indenting now ignores #ifdef, #ifndef, #else and #endif if the line contains double underscores : __IGNORE_THISE_INDENT__
This is to prevent indentation in *.h files with the usual redefinition macro format, below.
Please make sure any #else and #endif has a double underscore in a comment on the same line :

#ifndef __HEADER_FILE_NAME__
#define __HEADER_FILE_NAME__

 ... your code here...

#endif // __HEADER_FILE_NAME__

New in V4.2
 “Select File” now defaults to All Files *.*,made INDENT_SPACES changeable.

New in V4.1 :
 NumName.f Howerd Oakford V1.0 2012 Feb 28  Win32Forth version  www.inventio.co.uk
 Display a 15 bit number (from 0 to 32767) as an English word, and two 15 bit numbers as a pair of words.
 The two-word phrase calculated from the checksum or hash of two different files has a roughly 1 in a billion chance of being the same
 This allows two files in different locations and/or times to be compared by human beings...
 This code requires the file NumName.txt in the same folder

New in Cweed4 V4.01 
New name – Cweed4 . Searching for “Cweed” online finds lots of sites about “weed” – “Cweed4” is easier to find. The version has also been reset to Cweed4 V4.01.
Added a custom file Cweed4.script for the Barr Coding Standard (2018) – this allows modifications to be made without recompiling Cweed4.

New in Cweed4 V4.02
The file Cweed4_script.f is now loaded when running the Barr 3.1 function. 
This allows the rules to be changed and reloaded without re-compiling Cweed.
If the file Cweed4_script.f is not found in the directory where Cweed4 is running, the default file is used i.e. the one that existed when Cweed4 was compiled.

New in Cweed4 V4.02 2021 Nov 11
Changes to Cweed4_script.f to improve the Barr 3.1 coding rules.
Previous versions of Cweed4 V4.02 could be used if you simply update the Cweed4_script.f file, but this causes an “interesting” version control effect :
Cweed4.exe is compiled with the version of Cweed4_script.f that exists at the time that it is created. If this is different to the Cweed4_script that exists when Cweed4.exe is run you can get a different behaviour if you delete Cweed4_script.f (because it would use the pre-compiled words from an earlier Cweed4_script.f file).
Not sure how to handle this yet… Maybe compare a hash of the built in script to the script file, and issue a warning if they are different… Watch this space :-)
