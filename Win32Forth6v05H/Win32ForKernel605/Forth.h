/*
 * @file Forth.h
 * @brief Forth code for the Win32Forth kernel header file.
 * @details Original file : $Id: forth.h 1.1 1994/04/01 07:51:15 andrew Exp $
 * @author Andrew McKewan -- March 1994
 * @author Rod Oakford
 * @author Howerd Oakford  2022 Aug 06 added header.
 */

#ifndef FORTH_H
#define FORTH_H

#define MAX_SBUF 300

extern char sbuf[];
extern HWND hwnd;
extern int winsize;
extern int *imageL;

#define ENTRY int __stdcall

ENTRY Bye (int);
void message (char *s);
void unload (void);
void ForthCleanup (void);
HKEY RegGetKey (void);

#define ORIG        0
#define MAGICN      1
#define DSIZE       2
#define DACTUAL     3
#define HSIZE       4
#define HACTUAL     5
#define HSEPARATE   6
#define CENTRY      7
#define TABLE       8
// #define HINST       9
#define CMDLEN      10
#define CMDADR      11
#define HCON        12
// #define EXCEPT      13
// #define EXREC       14
// #define EXRSTK      15
// #define CLFORTH     19
#define K32LIB      20
#define K32GPA      21
#define MSGNTRY     22
#define WINMSGNTRY  23
#define HEADLENGTH  24

#endif /* FORTH_H */

/* end of file */