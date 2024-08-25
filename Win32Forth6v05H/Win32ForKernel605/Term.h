/*
 * @file Term.h
 * @brief Window Terminal Output for Win32Forth header file.
 * @details Original file : $Id: term.h 1.1 1994/04/01 07:51:49 andrew Exp $
 * @datetime
 * @author Andrew McKewan -- March 1994
 * @author Rod Oakford
 * @author Howerd Oakford  2022 Aug 06  added header.
 */

#ifndef TERM_H
#define TERM_H

#define BL 32
#define BELL 7
#define BS 8
#define CR 13
#define LF 10

#define function_mask  0x10000
#define special_mask   0x20000
#define control_mask   0x40000
#define shift_mask     0x80000
#define alt_mask       0x100000
#define mouse_mask     0x200000
#define menu_mask      0x400000
#define proc_mask      0x800000
#define double_mask    0x1000000
#define down_mask      0x2000000
#define up_mask        0x4000000

extern int *image;                          // this is where forth is
extern char *screen;                        // address of screen buffer

// An ENTRY is made for these functions     // #            The execution table in Forth.c
//                                          // 0            Bye,                    // 0
ENTRY init (HANDLE, HANDLE, int);           // 1            InitConsole,            // 1
ENTRY key (void);                           // 2            key,                    // 2
ENTRY Accept (char *, int);                 // 3            Accept,                 // 3
ENTRY emit (char);                          // 4            emit,                   // 4
ENTRY type (char*, int);                    // 5            type,                   // 5
ENTRY cr (void);                            // 6            cr,                     // 6
ENTRY cls (void);                           // 7            cls,                    // 7
ENTRY keyq (void);                          // 8            keyq,                   // 8
ENTRY qcr (int);                            // 9            qcr,                    // 9
//                                          // 10           dummy,                  // 10
//                                          // 11           dummy,                  // 11
//                                          // 12           dummy,                  // 12
//                                          // 13           dummy,                  // 13
//                                          // 14           dummy,                  // 14
//                                          // 15           dummy,                  // 15
//                                          // 16           dummy,                  // 16
//                                          // 17           dummy,                  // 17
//                                          // 18           dummy,                  // 18
ENTRY gotoxy (int, int);                    // 19           gotoxy,                 // 19
ENTRY getxy (void);                         // 20           getxy,                  // 20
//                                          // 21    //     fconvert,               // 21
//                                          // 22    //     fformat,                // 22
ENTRY getcolrow (void);                     // 23           getcolrow,              // 23
//                                          // 24           dummy,                  // 24
//                                          // 25           dummy,                  // 25
//                                          // 26           dummy,                  // 26
ENTRY setfgbg (int, int);                   // 27           setfgbg,                // 27
ENTRY fpushkey (long);                      // 28           fpushkey,               // 28
ENTRY thescreen (void);                     // 29           thescreen,              // 29
ENTRY charwh (void);                        // 30           charwh,                 // 30
ENTRY shiftmask (void);                     // 31           shiftmask,              // 31
ENTRY resize (int, int);                    // 32           resize,                 // 32
//                                          // 33           dummy,                  // 33
//                                          // 34           dummy,                  // 34
ENTRY cursorshape (int );                   // 35           cursorshape,            // 35
ENTRY getcursor (void);                     // 36           getcursor,              // 36
ENTRY havemenu (int );                      // 37           havemenu,               // 37
//                                          // 36           dummy,                  // 38
ENTRY printdlg (long);                      // 39           printdlg,               // 39
ENTRY startprint (void);                    // 40           startprint,             // 40
ENTRY pageprint (void);                     // 41           pageprint,              // 41
ENTRY endprint (void);                      // 42           endprint,               // 42
ENTRY initprint (int);                      // 43           initprint,              // 43
ENTRY closeprint (void);                    // 44           closeprint,             // 44
//                                          // 45           dummy,                  // 45
//                                          // 46           dummy,                  // 46
//                                          // 47           dummy,                  // 47
//                                          // 48           dummy,                  // 48
//                                          // 49           dummy,                  // 49
//                                          // 50           dummy,                  // 50
ENTRY wscroll (int );                       // 51           wscroll,                // 51
ENTRY rowoffset (void);                     // 52           rowoffset,              // 52
ENTRY maxcolrow (void);                     // 53           maxcolrow,              // 53
ENTRY setmaxcolrow (int, int);              // 54           setmaxcolrow,           // 54
//                                          // 55           dummy,                  // 55
//                                          // 56           dummy,                  // 56
ENTRY qualityprint (void);                  // 57           qualityprint,           // 57
ENTRY startapage (void);                    // 58           startapage,             // 58
ENTRY endapage (void);                      // 59           endapage,               // 59
ENTRY printcopies (void);                   // 60           printcopies,            // 60
ENTRY printfrompages (void);                // 61           printfrompages,         // 61
ENTRY printtopages (void);                  // 62           printtopages,           // 62
//                                          // 63           dummy,                  // 63
//                                          // 64           dummy,                  // 64
//                                          // 65           dummy,                  // 65
//                                          // 66           SizeState,              // 66
ENTRY orientation (int);                    // 67           orientation,            // 67
//                                          // 68           dummy,                  // 68
//                                          // 69           dummy,                  // 69
//                                          // 70           dummy,                  // 70
ENTRY setcharwh (int, int);                 // 71           setcharwh,              // 71
ENTRY mark (long, long, long, long);        // 72           mark,                   // 72
ENTRY getfg (void);                         // 73           getfg,                  // 73
ENTRY getbg (void);                         // 74           getbg,                  // 74
//                                          // 75           dummy,                  // 75
//                                          // 76           dummy,                  // 76
ENTRY initprint2 (int, int, int, int);      // 77           initprint2,             // 77
ENTRY printflags (void);                    // 78           printflags,             // 78
//                                          // 79           dummy,                  // 79

#define typez(s)    type (s, strlen(s));
#define space()     emit(32)

extern BOOL doingdll;
extern RECT thePos;
extern void check_screen_position(RECT * thePos);
extern void save_screen_position( RECT * thePos );

#endif /* TERM_H */

/* end of file */
