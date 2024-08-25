/*
 * @file Term.c
 * @brief Window Terminal Output for Win32Forth.
 * @details Original file : $Id: term.c 1.1 1994/04/01 07:51:38 andrew Exp $
 * @datetime
 * @author Andrew McKewan -- March 1994
 * @author SMuB fixed window resizing 07-18-95
 * @author Rod Oakford
 * @author Howerd Oakford  2022 Aug 06  fixed window resizing again.
 */

#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "forth.h"
#include "resforth.h"
#include "term.h"
#include "AggressiveOptimize.h"   // RO

LRESULT WINAPI WndProc (HWND, UINT, UINT, LONG);
void paint (void);

//////////////////// Global Variables ////////////////////

char AppName[] = "Win32Forth" ;

HWND    hwnd = 0;   // handle to the window, initially zero
HDC     hdc;        // the DC of the window
RECT rcUpdate;      // RO
RECT rcGood;        // RO

//////////////////// Keyboard Input ////////////////////

#define kblength 256
unsigned long keybuf[kblength];     // circular keypad buffer
unsigned long head = 0, tail = 0;

#define next(x) ((x + 1) % kblength)

typedef void (__stdcall *FORTHFUNC)();
typedef int (__stdcall *FORTHENTRY)();
typedef void (__stdcall *MSGFUNC)(MSG*);
typedef int (__stdcall *WINMSGFUNC)(HWND, UINT, UINT, LONG);

ENTRY key (void)
{
    long c;
    MSG msg;

    while (head == tail)
    {
        MSGFUNC msgfunc = (MSGFUNC)imageL[MSGNTRY];

        if (!GetMessage(&msg, NULL, 0, 0))
        {
            Bye(0);
        }

        if (msgfunc != 0)
        {
            (*msgfunc)(&msg);   // call forth message handler
        }
        else
        {
            TranslateMessage(&msg);    // default handler
            DispatchMessage(&msg);
        }
    }

    c = keybuf[tail];
    tail = next(tail);
    return c;
}

ENTRY keyq (void)
{
    long c;
    MSG msg;

    while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE))
    {
        MSGFUNC msgfunc = (MSGFUNC)imageL[MSGNTRY];

        if (msg.message == WM_QUIT)
        {
            Bye(0);
        }

        if (msgfunc != 0)
        {
            (*msgfunc)(&msg);   // call forth message handler
        }
        else
        {
            TranslateMessage(&msg);    // default handler
            DispatchMessage(&msg);
        }
    }
    if (head != tail)
    {
        c = keybuf[tail];
        return c;
    }
    return (0);
}

//////////////////// Screen Output ////////////////////

char *screen = 0;           // address of screen buffer

int hlstlin = 0;            // four highlight text variables
int hlstcol = 0;
int hledlin = 0;
int hledcol = 0;
int rows, cols;             // size of screen
int rowoff = 0;             // offset from top of screen buffer,
// not top of screen
int maxrows = 512;          // number of lines in the screen save buffer
int maxcols = 256;          // number of columns in the screen save buffer
int charW, charH;           // size of a character
int x, y;                   // current cursor position ABSOLUTE FROM BUFFER TOP
int beepfreq = 500;         // beep sound frequency in cycles/second
int beepms = 100;           // beep sound duration in milliseconds
int theFg = -1;             // forground  color default to system color
int theBg = -1;             // background color default to system color
int cursor_height = 2;      // height of the cursor in lines
int have_menu = 0;          // default to NOT HAVE MENU
int lmouse_down = 0;        // is the left mouse button currently down?
int rmouse_down = 0;

LPCTSTR mycursor = (LPCTSTR)IDC_ARROW;
POINT maxpoint;             // point to hold max window size

#define gx ((x * charW)+1)              // graphics x position
#define gy ((y - rowoff) * charH)       // graphics y position

#define SCREEN(x,y) *(screen + (y * maxcols) + x)

void MoveCaret (void)
{
    if (GetFocus() == hwnd)
    {
        SetCaretPos(gx, gy + (charH - cursor_height));
    }
}

ENTRY mark (long edcol, long edlin, long stcol, long stlin)
{
    hlstlin = stlin;            // four highlight text variables
    hlstcol = stcol;
    hledlin = edlin;
    hledcol = edcol;
    InvalidateRect(hwnd, NULL, 0);
    UpdateWindow(hwnd);
    //  paint();
    return 0;
}

ENTRY wscroll (int newrowoff)
{
    int oldrowoff = rowoff;
    rowoff = max(min(newrowoff, y), 0);
    //  rowoff = max (min (newrowoff, y - (rows - 1)), 0);
    if (rowoff != oldrowoff)
    { int nlines = oldrowoff - rowoff;
        if (abs(nlines) < rows)
        { RECT rcBad;
            // We don't really want to scroll the partial line at the
            // bottom of the window, but Windows likes to scroll a
            // full window better than a partial window.
            ScrollWindow(hwnd, 0, charH * nlines, NULL, NULL);
            rcBad.left = 0;
            rcBad.right = charW * cols;
            if (nlines < 0)
            { // Invalidate the new bottom line in case the client
                // area is not an integral number of lines tall
                rcBad.top = charH * (rows + nlines);
                rcBad.bottom = charH * rows;
            } else
            { // Scrolling down.  Invalidate the partial line at
                // the bottom of the window.
                rcBad.top = charH * rows;
                rcBad.bottom = charH * (rows + 1);
            }
            InvalidateRect(hwnd, &rcBad, 0);
        } // scrolling less than a page
        else // scrolling more than a page, so nothing to save
        InvalidateRect(hwnd, NULL, 0);
        UpdateWindow(hwnd);
        SetScrollPos(hwnd, SB_VERT, rowoff, TRUE);
    }
    if(y>=rowoff && y<rowoff+rows)  // SMuB
    {
        MoveCaret();
    }
    return 0;
}

// scrollfix() makes sure the caret position is visible in the console
// window AND it makes sure that line y lies in the screen buffer.

void scrollfix (void)
{
    int slop = 1 + rows / 4;
    if (slop > 3)
    slop = 3;
    if(y<(rowoff+rows-slop) || y>(rowoff+rows-1) )              // SMuB
    {
        wscroll(y-rows+1);                                      // SMuB
        MoveCaret();                                            // SMuB
    }                                                           // SMuB
    if(y>=maxrows)  // Scroll the screen buffer if needed       // SMuB
    { int nlines= y-maxrows+1;                                  // SMuB
        int nshift= nlines*maxcols;                             // SMuB
        memmove(screen,screen+nshift,maxrows*maxcols-nshift);   // SMuB
        memset(screen+maxrows*maxcols-nshift,BL,nshift);        // SMuB
        rowoff -= nlines;                                       // SMuB
        y -= nlines;  // Unfortunately we change y here         // SMuB
    }                                                           // SMuB
}

ENTRY rowoffset (void)  // number of rows from buffer start to screen top
{
    return rowoff;
}

// return the MAXIMUM number of columns and rows on physical screen
ENTRY maxcolrow (void)
{
    return ((maxrows << 16) + maxcols);
}

// set the maximum number of rows and columns
ENTRY setmaxcolrow (int mxrows, int mxcols)
{
    maxrows = maxpoint.y = mxrows;
    maxcols = maxpoint.x = mxcols;
    free(screen);
    screen = NULL;
    resize(rows * charH, cols * charW);
    SetScrollRange(hwnd, SB_VERT, 0, maxrows, FALSE);
    SetScrollPos(hwnd, SB_VERT, 0, TRUE);
    cls();
    return 0;
}

// move cursor to physical screen position x,y
ENTRY gotoxy (int newx, int newy)
{
    x = min(newx, cols - 1);
    y = min(newy, rows - 1) + rowoff;
    MoveCaret();
    return 0;
}

ENTRY getxy (void)       // ret the current cursor position on physical screen
{
    return ((y - rowoff) << 16) + x;
}

ENTRY charwh (void)    // get the width and height of character cell in pixels
{
    return (charW << 16) + charH;
}

// set the width and height of character cell in pixels
ENTRY setcharwh (int newy, int newx)
{
    charW = newx;
    charH = newy;
    return 0;
}

ENTRY havemenu (int menu_flag)      // tell the wrapper whether we have a menu
{
    have_menu = menu_flag;
    return 0;
}

ENTRY cursorshape (int cheight)     // set the cursor shape (height) in pixels
{
    if (cheight > charH) cheight = charH;
    if (cheight < 0) cheight = 1;
    cursor_height = cheight;
    CreateCaret(hwnd, NULL, charW, cheight);
    SetCaretPos(gx, gy + (charH - cursor_height));
    ShowCaret(hwnd);
    return 0;
}

ENTRY getcursor (void)              // get the cursor height in pixels
{
    return cursor_height;
}

// set the foreground and background colors for the console
ENTRY setfgbg (int abg, int afg)
{

    if (afg == -1)      // if both fg and bg were -1 then we were using
    theFg = GetSysColor(COLOR_WINDOWTEXT);
    else
    theFg = afg & 0xFFFFFF;

    if (abg == -1)      // if both fg and bg were -1 then we were using
    theBg = GetSysColor(COLOR_WINDOW);
    else
    theBg = abg & 0xFFFFFF;

    InvalidateRect(hwnd, NULL, 0);
    UpdateWindow(hwnd);
    return 0;
}

ENTRY getbg (void)   // get the background color for the console
{
    return theBg;
}

ENTRY getfg (void)   // get the foreground color for the console
{
    return theFg;
}

//  clear the console buffer and move cursor to top left
ENTRY cls (void)
{
    if (!screen) return 0;
    memset(screen, BL, maxrows * maxcols);
    x = y = rowoff = 0;
    MoveCaret();
    InvalidateRect(hwnd, NULL, 0);
    UpdateWindow(hwnd);
    SetScrollPos(hwnd, SB_VERT, rowoff, TRUE);
    return 0;
}

void wrap (void)                // primitive column wrap function
{
    if (x >= cols)
    {
        x = 0;
        y += 1;
    }
    scrollfix();
}

// resize the console window to pixel height, width
ENTRY resize (int height, int width)
{
    int r = min(max(height / charH, 1), maxrows);
    int c = min(max(width / charW, 1), maxcols);
    int oldrows = rows;             // save rows
    int oldcols = cols;             // save columns
    RECT rectangle;

    if (!screen)                    // if no screen allocated yet, then allocate it
    {
        screen = (char *) malloc(maxrows * maxcols);
        if (!screen)
        MessageBox (hwnd, "Cannot Allocate Screen Memory","An Error",
            MB_ICONHAND | MB_OK);
        else
        {
            memset(screen, BL, maxrows * maxcols);
            oldrows = r;
            oldcols = c;
        }
    }

    // check if size has changed
    if (r == rows && c == cols)     // if no full character size change,
    {                               // then align window to a full character size
        r = rows * charH;
        c = cols * charW;
        rectangle.left = 0;
        rectangle.top = 0;
        rectangle.right = c + 2;
        rectangle.bottom = r + 2;
        AdjustWindowRect(&rectangle, WS_OVERLAPPEDWINDOW, have_menu);
        SetWindowPos (hwnd, HWND_TOP, 0, 0, rectangle.right - rectangle.left,
            rectangle.bottom - rectangle.top, SWP_NOMOVE);
        return 0;
    }

    rows = r;
    cols = c;

    r = rows * charH;
    c = cols * charW;
    rectangle.left = 0;
    rectangle.top = 0;
    rectangle.right = c + 2;
    rectangle.bottom = r + 2;
    AdjustWindowRect(&rectangle, WS_OVERLAPPEDWINDOW, have_menu);
    SetWindowPos (hwnd, HWND_TOP, 0, 0, rectangle.right - rectangle.left,
        rectangle.bottom - rectangle.top, SWP_NOMOVE);
    SetScrollRange(hwnd, SB_VERT, 0, maxrows - rows, FALSE);
    SetScrollPos(hwnd, SB_VERT, rowoff, TRUE);
    wrap();
    return 0;
}

// windows calable version of resize above
ENTRY winresize (int height, int width)
{
    int r = min(max(height / charH, 1), maxrows);
    int c = min(max(width / charW, 1), maxcols);
    int oldrows = rows;             // save rows
    int oldcols = cols;             // save columns
    RECT rectangle;

    if (!screen)                    // if no screen allocated yet, then allocate it
    {
        screen = (char *) malloc(maxrows * maxcols);
        if (!screen)
        MessageBox (hwnd, "Cannot Allocate Screen Memory","An Error"
            , MB_ICONHAND | MB_OK);
        else
        {
            memset(screen, BL, maxrows * maxcols);
            oldrows = r;
            oldcols = c;
        }
    }

    rows = r;
    cols = c;

    if (((height / charH) > maxrows) || ((width / charW) > maxcols))
    {
        cols = min(cols, maxcols);
        rows = min(rows, maxrows);
        r = rows * charH;
        c = cols * charW;
        rectangle.left = 0;
        rectangle.top = 0;
        rectangle.right = c + 2;
        rectangle.bottom = r + 2;
        AdjustWindowRect(&rectangle, WS_OVERLAPPEDWINDOW, have_menu);
        SetWindowPos (hwnd, HWND_TOP, 0, 0, rectangle.right - rectangle.left,
            rectangle.bottom - rectangle.top, SWP_NOMOVE);
    }

    SetScrollRange(hwnd, SB_VERT, 0, maxrows - rows, FALSE);
    SetScrollPos(hwnd, SB_VERT, rowoff, TRUE);

    wrap();
    return 0;
}

// get the columns and rows of the physical console window
ENTRY getcolrow (void)
{
    return (rows << 16) + cols;
}

void beep (void)                    // beep the user
{
    Beep(beepfreq, beepms);
}

// emit a character to the console window
ENTRY emit (char c)
{
    if (!screen) return 0;

    scrollfix();
    switch (c)
    {
        case BELL:
        beep();
        return 0;

        case BS:
        if (x) --x;
        else if (y) --y, x = cols - 1;
        break;

        case LF:
        y += 1;
        wrap();
        break;

        case CR:
        x = 0;
        break;

        default:
        HideCaret(hwnd);
        SetBkColor(hdc, theBg);
        SetTextColor(hdc, theFg);
        TextOut(hdc, gx, gy, &c, 1);
        ShowCaret(hwnd);
        SCREEN(x, y) = c;
        x += 1;
        wrap();
    }
    MoveCaret();
    return 0;
}

// type a string of characters to the console window
ENTRY type (char *addr, int len)
{
    int n;

    if (!screen) return 0;

    scrollfix();
    HideCaret(hwnd);

    SetBkColor(hdc, theBg);
    SetTextColor(hdc, theFg);
    while (len)
    { n = min(len, cols - x);
        TextOut(hdc, gx, gy, addr, n);
        memmove(&SCREEN(x, y), addr, n);
        x += n;
        wrap();

        addr += n;
        len -= n;
    }

    MoveCaret();
    ShowCaret(hwnd);
    return 0;
}

ENTRY spaces (int len)              // output 'len' spaces
{
    int n;
    char buf[80];

    if (!screen) return 0;

    memset(buf, BL, 80);

    while (len > 0)
    {
        n = min(len, cols - x);
        type(buf, n);
        len -= n;
    }
    return 0;
}

ENTRY cr (void)                     // output a carraige return
{
    if (!screen)
    return 0;
    x = 0;
    y += 1;
    scrollfix();
    MoveCaret();
    return 0;
}

ENTRY qcr (int n)                   // cr if not room for n characters
{
    if ((x + n) >= cols)
    cr();
    return 0;
}

// push a character into the keyboard typeahead buffer
void pushkey (long thekey)
{
    long keytemp;

    if (next(head) == tail)
    beep();                         // buffer full
    else
    {
        keytemp = thekey;           // a copy of the thekey
        if ((GetKeyState(VK_SHIFT) & 0x8000) && (thekey < 32))
        // if shift is down
        keytemp |= shift_mask;      // then include the shift bit
        keybuf[head] = keytemp;
        head = next(head);
    }
}

// push a character into the keyboard typeahead buffer
ENTRY fpushkey (long thekey)
{

    if (next(head) == tail)
    beep();                         // buffer full
    else
    {
        keybuf[head] = thekey;
        head = next(head);
    }

    PostMessage(hwnd, WM_NULL, thekey, 0);

    return 0;
}

// return the physical address of the console screen buffer
ENTRY thescreen (void)
{
    return (long)screen;
}

ENTRY shiftmask (void)              // get the current keybaord shift mask
{
    long themask = 0;

    if (GetKeyState (VK_CONTROL) & 0x8000)  // if control is down
    themask |= control_mask;                // then include control bit
    if (GetKeyState (VK_SHIFT) & 0x8000)    // if shift is down
    themask |= shift_mask;                  // then include the shift bit
    return themask;
}

// push a character with shift mask into the keyboard buffer
void spushkey (long thekey)
{
    long keytemp;

    if (next(head) == tail)
    beep();                                     // buffer full
    else
    {
        keytemp = thekey;                       // a copy of the thekey
        if (GetKeyState (VK_CONTROL) & 0x8000)  // if control is down
        keytemp |= control_mask;                // then include control bit
        if (GetKeyState (VK_SHIFT) & 0x8000)    // if shift is down
        keytemp |= shift_mask;                  // then include the shift bit
        keybuf[head] = keytemp;
        head = next(head);
    }
}

void pushfunctionkey (long thekey)  // test and push function and special keys
{
    switch (thekey)
    {
        case VK_F1:
        spushkey(function_mask | 0x01);
        break;
        case VK_F2:
        spushkey(function_mask | 0x02);
        break;
        case VK_F3:
        spushkey(function_mask | 0x03);
        break;
        case VK_F4:
        spushkey(function_mask | 0x04);
        break;
        case VK_F5:
        spushkey(function_mask | 0x05);
        break;
        case VK_F6:
        spushkey(function_mask | 0x06);
        break;
        case VK_F7:
        spushkey(function_mask | 0x07);
        break;
        case VK_F8:
        spushkey(function_mask | 0x08);
        break;
        case VK_F9:
        spushkey(function_mask | 0x09);
        break;
        case VK_F10:
        spushkey(function_mask | 0x10);
        break;
        case VK_F11:
        spushkey(function_mask | 0x11);
        break;
        case VK_F12:
        spushkey(function_mask | 0x12);
        break;
        case VK_HOME:
        spushkey(special_mask | 0x00);
        break;
        case VK_END:
        spushkey(special_mask | 0x01);
        break;
        case VK_INSERT:
        spushkey(special_mask | 0x02);
        break;
        case VK_DELETE:
        spushkey(special_mask | 0x03);
        break;
        case VK_LEFT:
        spushkey(special_mask | 0x04);
        break;
        case VK_RIGHT:
        spushkey(special_mask | 0x05);
        break;
        case VK_UP:
        spushkey(special_mask | 0x06);
        break;
        case VK_DOWN:
        spushkey(special_mask | 0x07);
        break;
        case VK_SCROLL:
        spushkey(special_mask | 0x08);
        break;
        case VK_PAUSE:
        spushkey(special_mask | 0x09);
        break;
        case VK_PRIOR:
        spushkey(special_mask | 0x10);
        break;
        case VK_NEXT:
        spushkey(special_mask | 0x11);
        break;
        default :
        ;
    }
}

void paint(void)          // repaint the console window from the screen buffer
{
    int i;
    int nrows = rows;

    if (!screen)
    return;

    if (rowoff + nrows > maxrows)
    nrows = maxrows - rowoff;

    SetBkColor(hdc, theBg);
    SetTextColor(hdc, theFg);

    for (i = 0 ; i < nrows ; ++ i)
    {
        if (((i + rowoff) < hlstlin) || ((i + rowoff) > hledlin))
        TextOut(hdc, 1, i * charH, screen + ((i + rowoff) * maxcols), cols);
        else
        {
            if ((i + rowoff) == hlstlin)    // if we are on the starting line
            {
                // put out beginning of starting line as normal text
                TextOut (hdc, 1, i * charH, screen + ((i + rowoff) * maxcols),
                    hlstcol);
                SetBkColor(hdc, theFg);
                SetTextColor(hdc, theBg);
                if (hlstlin == hledlin)     // if this is also the ending line
                {
                    TextOut (hdc, 1 + (charW * hlstcol), i * charH, screen +
                        ((i + rowoff) * maxcols) + hlstcol, hledcol - hlstcol);
                    SetBkColor(hdc, theBg);
                    SetTextColor(hdc, theFg);
                    TextOut (hdc, 1 + (charW * hledcol), i * charH, screen +
                        ((i + rowoff) * maxcols) + hledcol, cols - hledcol);
                }
                else        // else put out remainder of starting line reversed
                {
                    TextOut (hdc, 1 + (charW * hlstcol), i * charH, screen +
                        ((i + rowoff) * maxcols) + hlstcol, cols - hlstcol);
                    SetBkColor(hdc, theBg);
                    SetTextColor(hdc, theFg);
                }
            }
            else                                // not starting line
            {
                if ((i + rowoff) == hledlin)    // is it the ending line?
                {                               // put out first part reversed
                    SetBkColor(hdc, theFg);
                    SetTextColor(hdc, theBg);
                    TextOut (hdc, 1, i * charH, screen +
                        ((i + rowoff) * maxcols), hledcol);
                    SetBkColor(hdc, theBg);     // then remainder as normal text
                    SetTextColor(hdc, theFg);
                    TextOut (hdc, 1 + (charW * hledcol), i * charH, screen +
                        ((i + rowoff) * maxcols) + hledcol, cols - hledcol);
                }
                else                        // if not ending line either
                {                           // then put out whole line reversed
                    SetBkColor(hdc, theFg);
                    SetTextColor(hdc, theBg);
                    TextOut (hdc, 1, i * charH, screen +
                        ((i + rowoff) * maxcols), cols);
                    SetBkColor(hdc, theBg);
                    SetTextColor(hdc, theFg);
                }
            }
        }
    }
}

void vertscroll (long wParam)      // handle vertical scroll bar
{
    switch ((int) LOWORD(wParam))
    {
        case SB_BOTTOM:
        wscroll(maxrows);
        break;
        case SB_ENDSCROLL:
        break;
        case SB_LINEDOWN:
        wscroll(rowoff + 1);
        break;
        case SB_LINEUP:
        wscroll(rowoff - 1);
        break;
        case SB_PAGEDOWN:
        wscroll(rowoff + (rows - 1));
        break;
        case SB_PAGEUP:
        wscroll(rowoff + - (rows - 1));
        break;
        case SB_THUMBPOSITION:
        wscroll((int) HIWORD(wParam));
        break;
        case SB_THUMBTRACK:
        wscroll((int) HIWORD(wParam));
        break;
        case SB_TOP:
        wscroll(0);
        break;
        default :
        ;
    }
}

//////////////////// Window Procedure ////////////////////

LRESULT WINAPI WndProc (HWND hwnd, UINT message, UINT wParam, LONG lParam)
{
    PAINTSTRUCT ps;
    //    TEXTMETRIC  tm ;
    MINMAXINFO * mminfo;
    WINMSGFUNC winmsgfunc;
    SIZE Size;

    switch (message)
    {

        case WM_CREATE:
        hdc = GetDC(hwnd);
        //        SelectObject(hdc, GetStockObject(ANSI_FIXED_FONT));
        SelectObject(hdc, GetStockObject(ANSI_FIXED_FONT));
        GetTextExtentPoint32(hdc, "a", 1 , &Size );
        charW = Size.cx;
        charH = Size.cy;

        ReleaseDC(hwnd, hdc);
        return 0;

        case WM_LBUTTONDOWN:
        fpushkey((long)wParam | (long)mouse_mask | (long)down_mask);
        fpushkey((long)lParam);
        lmouse_down = 1;
        return 0;

        case WM_RBUTTONDOWN:
        fpushkey((long)wParam | (long)mouse_mask | (long)down_mask);
        fpushkey((long)lParam);
        rmouse_down = 1;
        return 0;

        case WM_LBUTTONUP:
        fpushkey((long)wParam | (long)mouse_mask | (long)up_mask);
        fpushkey((long)lParam);
        lmouse_down = 0;
        return 0;

        case WM_RBUTTONUP:
        fpushkey((long)wParam | (long)mouse_mask | (long)up_mask);
        fpushkey((long)lParam);
        rmouse_down = 0;
        return 0;

        case WM_LBUTTONDBLCLK:
        case WM_RBUTTONDBLCLK:
        fpushkey((long)wParam | (long)mouse_mask | (long)double_mask);
        fpushkey((long)lParam);
        return 0;

        case WM_SYSCOLORCHANGE:
        setfgbg(-1, -1);
        return 0;

        case WM_SYSKEYDOWN:
        if (wParam == VK_F10)
        {
            pushfunctionkey((long)wParam);
            return 0;
        }
        return DefWindowProc(hwnd, message, wParam, lParam);

        case WM_KEYDOWN:
        pushfunctionkey((long)wParam);
        return 0;

        //  case WM_SYSCHAR:
        //      pushkey ((long)wParam | (long)alt_mask);
        //      return DefWindowProc (hwnd, message, wParam, lParam) ;

        case WM_CHAR:
        pushkey((long)wParam);
        return 0;

        case WM_COMMAND:
        if (lParam == 0)
        pushkey((long)(wParam & 0xFFFF) | (long)menu_mask);
        return 0;

        case WM_SYSCOMMAND:
        if ((wParam & 0xF000) != 0xF000)
        {
            pushkey((long)(wParam & 0xFFFF) | (long)menu_mask);
            return 0;
        }
        return DefWindowProc(hwnd, message, wParam, lParam);

        case WM_SETFOCUS:
        cursorshape(cursor_height);
        return 0;

        case WM_KILLFOCUS:
        HideCaret(hwnd);
        DestroyCaret();
        return 0;

        case WM_GETMINMAXINFO:
        maxpoint.x = (maxcols + 2) * max(charW, 8);
        maxpoint.y = (maxrows + 4) * max(charH, 8);
        mminfo = (MINMAXINFO *)lParam;
        mminfo->ptMaxTrackSize = maxpoint;
        return 0;

        case WM_SIZE:
        GetUpdateRect(hwnd,&rcUpdate,0);    // RO
        InvalidateRect(hwnd,0,0);           // RO
        rcGood.right=rcGood.right-5;        // RO
        InvalidateRect(hwnd,&rcUpdate,0);   // RO
        ValidateRect(hwnd,&rcGood);         // RO
        GetClientRect(hwnd,&rcGood);        // RO
        winsize = wParam;                   // save wParam for later reference
        if (wParam == SIZE_RESTORED || wParam == SIZE_MAXIMIZED)
        winresize(HIWORD(lParam), LOWORD(lParam));
        return 0;

        case WM_PAINT:
        hdc = BeginPaint(hwnd, & ps);
        paint();
        EndPaint(hwnd, & ps);
        return 0;

        case WM_CLOSE:
        Bye(0);
        return 0;

        case WM_MOUSEMOVE:
        SetCursor(LoadCursor(NULL, mycursor));
        if (lmouse_down)
        {   // pass both UP and DOWN if mouse is just moving
            fpushkey ((long)wParam | (long)mouse_mask | (long)down_mask
                | (long)up_mask);
            fpushkey((long)lParam);
        }
        return 0;

        case WM_VSCROLL:
        vertscroll(wParam);
        return 0;
    }
    winmsgfunc = (WINMSGFUNC)imageL[WINMSGNTRY];

    if (winmsgfunc != 0)
    return (* winmsgfunc)(hwnd, message, wParam, lParam);
    // call forth message handler
    else
    return DefWindowProc(hwnd, message, wParam, lParam);
}

#define MAX_CONSOLE_STRING_LENGTH  (0x100)

// save the current screen position into the registry Console value
void save_screen_position (RECT * thePos)
{
    HKEY theKey;
    unsigned char myString[MAX_CONSOLE_STRING_LENGTH];
    DWORD slen;

    // Load string myString with the new values to be saved in the registry
    // Convert into a comma separated string e.g. "10, 300, 10, 200"
    slen = sprintf((char*)myString, "%d,%d,%d,%d\0", thePos->left, thePos->top, thePos->right, thePos->bottom);
    // slen = sprintf_s(myString, MAX_CONSOLE_STRING_LENGTH, "%d,%d,%d,%d\0", thePos->left, thePos->top, thePos->right, thePos->bottom);
    if (-1 == slen)
    {
        // error
    }
    else
    {
        // Save the new values into the registry
        theKey = RegGetKey();
        RegSetValueEx (theKey, "Console", 0, REG_SZ, myString, slen);
        RegCloseKey(theKey);
    }
}

// Make sure that the Forth window is always visible on power up
// If the registry Console value is missing, empty or corrupted, default values are set.
void check_screen_position (RECT * thePos)
{
    HKEY theKey;
    unsigned char myString[MAX_CONSOLE_STRING_LENGTH];
    DWORD slen = MAX_CONSOLE_STRING_LENGTH;
    DWORD ktype;
    DWORD RegQueryResult;
    DWORD ScanfResult = 0;
    DWORD FoundIllegalCharacter;
    DWORD i;

    theKey = RegGetKey();

    // Get the current Console registry value into myString
    RegQueryResult = RegQueryValueEx (theKey, "Console", NULL, &ktype, myString, &slen);

    RegCloseKey(theKey);

    FoundIllegalCharacter = 0;
    for (i = 0 ; i < MAX_CONSOLE_STRING_LENGTH ; i++)
    {

        // found the null terminator
        if (myString[i] == '\0')
        {
            break;
        }

        if (((myString[i] >= '0') && (myString[i] <= '9')) || myString[i] == ',' || myString[i] == ' ' || myString[i] == '-') // 0 to 9 , comma, spaces and minus are allowed
        {
            // no action
        }
        else
        {
            FoundIllegalCharacter = 1;  // found an illegal character
            break;
        }
    }

    if (FoundIllegalCharacter == 0)
    {
        // Parse the current values from the registry string myString
        ScanfResult = sscanf((char*)myString, "%d,%d,%d,%d", &thePos->left, &thePos->top, &thePos->right, &thePos->bottom);
        // ScanfResult = sscanf_s(myString, "%d,%d,%d,%d", &thePos->left, &thePos->top, &thePos->right, &thePos->bottom);
    }

    if ((RegQueryResult != 0) || (ScanfResult == - 1) || (FoundIllegalCharacter == 1))
    {
        // if the Console registry key has never been set, cannot be parsed or the string contains illegal characters, use defaults
        thePos->left    = 3;
        thePos->top     = 3;
        thePos->right   = 634;
        thePos->bottom  = 474;
    }
    else
    {
        // Limit the screen size to sensible values HO 2022 Aug 01
        if ( thePos->left   < -8   ) { thePos->left = -8;   }                           // minimum left coordinate
        if ( thePos->left   > 1024 ) { thePos->left = 1024; }                           // maximum left coordinate
        if ( thePos->top    < -8   ) { thePos->top  = -8;   }                           // minimum top coordinate
        if ( thePos->top    > 512 ) { thePos->top  = 512; }                             // maximum top coordinate
        if ( (thePos->right - thePos->left) < 256  ) { thePos->right = thePos->left + 256;  }   // minimum width
        if ( (thePos->right - thePos->left) > 2048 ) { thePos->right = thePos->left + 2048; }   // maximum width
        if ( (thePos->bottom - thePos->top) < 256  ) { thePos->bottom = thePos->top + 256;  }   // minimum height
        if ( (thePos->bottom - thePos->top) > 2048 ) { thePos->bottom = thePos->top + 2048; }   // maximum height
    }

    save_screen_position(thePos);
}

//////////////////// Initialization ////////////////////

HANDLE hInst;

ENTRY init (HANDLE hInstance, HANDLE hPrevInstance, int nCmdShow)
{
    WNDCLASS wndclass;
    RECT thePos;

    hInst = hInstance;
    if (hwnd) return(int) hwnd;

    check_screen_position(&thePos); // make sure that the Forth window is always visible

    if (!hPrevInstance)
    {
        //          wndclass.style         = CS_HREDRAW | CS_VREDRAW | CS_DBLCLKS | CS_OWNDC;
        wndclass.style         = CS_DBLCLKS | CS_OWNDC;    // RO
        wndclass.lpfnWndProc   = WndProc ;
        wndclass.cbClsExtra    = 0 ;
        wndclass.cbWndExtra    = 0 ;
        wndclass.hInstance     = hInstance ;
        wndclass.hIcon         = LoadIcon (hInstance,
            MAKEINTRESOURCE(ICON_FORTH));
        wndclass.hCursor       = NULL ;
        wndclass.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
        wndclass.lpszMenuName  = NULL ;
        wndclass.lpszClassName = AppName ;

        RegisterClass(&wndclass);
    }

    hwnd = CreateWindow (AppName,           // window class name
        "Win32Forth",                       // window caption
        WS_OVERLAPPEDWINDOW | WS_VSCROLL,   // window style
        thePos.left,
        thePos.top,
        (thePos.right - thePos.left),
        (thePos.bottom - thePos.top),
        //3 ,     //          CW_USEDEFAULT,                      // initial x position
        //3 ,     //          CW_USEDEFAULT,                      // initial y position
        //634 ,   //          CW_USEDEFAULT,                      // initial x size
        //474 ,   //          CW_USEDEFAULT,                      // initial y size
        NULL,                           // parent window handle
        NULL,                           // window menu handle
        hInstance,                      // program instance handle
        NULL) ;                                 // creation parameters

    theFg = GetSysColor(COLOR_WINDOWTEXT);
    theBg = GetSysColor(COLOR_WINDOW);

    //    if (hwnd) ShowWindow (hwnd, nCmdShow | SW_MINIMIZE);   // RO
    setfgbg(-1, -1);
    SetScrollRange(hwnd, SB_VERT, 0, maxrows, FALSE);
    SetScrollPos(hwnd, SB_VERT, 0, TRUE);

	if(nCmdShow);  // to remove compiler warning
		
    return (int) hwnd;
}

//////////////////// Line Input ////////////////////

#define backup() emit(BS); emit(BL); emit(BS)

ENTRY Accept (char * addr, int len)
{
    int c;
    int n = 0;

    while ((c = key()) != CR)
    {
        if (c == BS)
        {
            if (n)
            {
                backup();
                --n;
            }
            else
            beep();
        }
        else
        {
            if (n < len && c >= BL)
            {
                emit((char)c);
                *(addr + n) = (char)c;
                ++n;
            }
            else
            beep();
        }
    }
    emit(BL);
    return n;
}

////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
//////////////////////////   Printing   ////////////////////////////////
////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////

PRINTDLG pd;
DEVMODE dm;
HDC ghdc = NULL;

HANDLE OwnerWindow = NULL;

ENTRY printdlg (long winh)      // put up the print setup dialog
{
    //
    // Initialize a PRINTDLG struct and call PrintDlg to allow user to
    //   specify various printing options...
    //

    if (ghdc == NULL)
    {
        memset((void *) &pd, 0, sizeof(PRINTDLG));
        pd.hDevMode    = NULL; // &dm;
        //      memset ((void *) &dm, 0, sizeof(DEVMODE));
        pd.hDevNames   = NULL;
        pd.lStructSize = sizeof(PRINTDLG);
        pd.hwndOwner   = (HANDLE)winh;
        OwnerWindow    = (HANDLE)winh;
        pd.hInstance   = NULL;
        pd.nMinPage    = 1;
        pd.nMaxPage    = 999;
        pd.nFromPage   = 1;
        pd.nToPage     = 999;
        pd.nCopies     = 1;
    }

    pd.Flags = PD_RETURNDC |
    PD_HIDEPRINTTOFILE |
    PD_PAGENUMS |
    PD_NOSELECTION |
    PD_USEDEVMODECOPIES |
    PD_PRINTSETUP;

    if (PrintDlg(&pd))
    ghdc = pd.hDC;

    return (long)ghdc;
}

int     gbAbort;
HANDLE  ghwndAbort;

// initialize printing if not already initialized
ENTRY initprint (int autoinit)
{

    if (ghdc == NULL)
    {
        memset((void *) &pd, 0, sizeof(PRINTDLG));
        pd.hDevMode    = NULL;
        pd.hDevNames   = NULL;
        pd.lStructSize = sizeof(PRINTDLG);
        pd.hwndOwner   = (HANDLE)OwnerWindow;
        pd.hInstance   = NULL;
        pd.nMinPage    = 1;
        pd.nMaxPage    = 999;
        pd.nFromPage   = 1;
        pd.nToPage     = 999;
        pd.nCopies     = 1;
    }
    else                // if already initialized, just return handle
    {
        if (autoinit)
        // and autoinit flag is set, then just return the handle
        return (long)ghdc;
    }

    if (autoinit)       // if automatic, then add extra PD_ to flag
    {

        pd.Flags = PD_HIDEPRINTTOFILE |
        PD_PAGENUMS |
        PD_NOSELECTION |
        PD_RETURNDEFAULT |
        //                  PD_PRINTSETUP |
        PD_USEDEVMODECOPIES;

    }
    else                        // else remove this bit from the PD_ flag
    {

        pd.Flags = PD_HIDEPRINTTOFILE |
        PD_PAGENUMS |
        PD_NOSELECTION |
        //                  PD_PRINTSETUP |
        PD_USEDEVMODECOPIES;

    }

    if (!PrintDlg(&pd))
    return 0;               // leave on error, return NULL

    if (pd.hDC == NULL)
    {
        DEVNAMES* pDevNames;
        DEVMODE* pDevMode;

        if (pd.hDevNames == NULL)
        return 0;

        pDevNames = (DEVNAMES*) GlobalLock(pd.hDevNames);
        pDevMode = (pd.hDevMode != NULL) ?
        (DEVMODE*) GlobalLock(pd.hDevMode) : NULL;

        pd.hDC = CreateDC((LPCTSTR)pDevNames + pDevNames->wDriverOffset,
            (LPCTSTR)pDevNames + pDevNames->wDeviceOffset,
            (LPCTSTR)pDevNames + pDevNames->wOutputOffset,
            pDevMode);

        GlobalUnlock(pd.hDevNames);
        if (pd.hDevMode != NULL)
        GlobalUnlock(pd.hDevMode);
    }

    if (pd.hDC == NULL)
    {
        ghdc = NULL;
        return 0;
    }

    ghdc = pd.hDC;
    // init the global handle to the printer device context

    if (!(GetDeviceCaps(ghdc, RASTERCAPS) & RC_BITBLT))
    {
        DeleteDC(ghdc);
        MessageBox (hwnd, "Printer cannot display bitmaps.",
            "Device Error", MB_OK);
        ghdc = NULL;
        return 0;
    }

    return (long)ghdc;
}

ENTRY startprint (void)     // start a printing session
{
    DOCINFO di;
    if (ghdc == NULL)
    {
        if (!initprint(0))
        return 0;
    }
    di.cbSize       = sizeof(DOCINFO);
    di.lpszDocName  = "Document";
    di.lpszOutput   = NULL;
    di.lpszDatatype = "";
    di.fwType       = 0;

    StartDoc  (ghdc, &di);
    StartPage (ghdc);
    return 0;
}

ENTRY pageprint (void)      // finish one page and start another
{
    EndPage   (ghdc);
    StartPage (ghdc);
    return 0;
}

ENTRY endapage (void)       // finish a printed page
{
    EndPage   (ghdc);
    return 0;
}

ENTRY startapage (void)     // start a new printed page
{
    StartPage (ghdc);
    return 0;
}

// finish a printing session, start physical printing
ENTRY endprint (void)
{
    //  EndPage   (ghdc);
    EndDoc    (ghdc);
    return 0;
}

ENTRY closeprint (void)     // close printing, release printing device context
{
    if (ghdc)
    DeleteDC(ghdc);
    ghdc = NULL;
    return 0;
}

ENTRY printcopies (void)
{
    return (long)pd.nCopies;
}

ENTRY printfrompages (void)
{
    if (pd.Flags & PD_PAGENUMS)
    return (long)pd.nFromPage;
    return pd.nMinPage;
}

ENTRY printtopages (void)
{
    if (pd.Flags & PD_PAGENUMS)
    return (long)pd.nToPage;
    return pd.nMaxPage;
}

ENTRY qualityprint (void)   // get print quality set by user in print dialog
{
    short quality;
    HANDLE hdm;
    DEVMODE *pdm;

    if (!ghdc)
    return 0;

    hdm = pd.hDevMode;
    pdm = GlobalLock(hdm);
    quality = pdm->dmPrintQuality;
    GlobalUnlock(pdm);
    return (long)quality;
}

// get print quality set by user in print dialog
ENTRY orientation (int landscape)
{
    HGLOBAL hGlobal = NULL;
    HANDLE hPrinter = NULL;
    DWORD dwNeeded = 0;
    PRINTER_INFO_2 * pi2 = NULL;
    PRINTER_DEFAULTS pdef;
    BOOL bFlag;
    LONG lFlag;

    HANDLE hdm;
    DEVMODE *pdm;

    closeprint();                       // close any printer thats open
    ghdc = (HDC)initprint(1);           // do an automatic printer initialize

    hdm = pd.hDevMode;
    pdm = GlobalLock(hdm);

    /* Open printer handle (in Windows NT, you need full-access because you
    will eventually use SetPrinter) */

    ZeroMemory(&pdef, sizeof(pdef));
    pdef.DesiredAccess = PRINTER_ALL_ACCESS;
    bFlag = OpenPrinter((char*)pdm->dmDeviceName, &hPrinter, &pdef);
    if (!bFlag || (hPrinter == NULL))
    goto ABORT;

    /* The first GetPrinter() tells you how big the buffer should be in order
    to hold all of PRINTER_INFO_2. Note that this will usually return FALSE,
    which only means that the buffer (the third parameter) was not filled
    in. You don't want it filled in here. */

    GetPrinter(hPrinter, 2, 0, 0, &dwNeeded);
    if (dwNeeded == 0)
    goto ABORT;

    /* Allocate enough space for PRINTER_INFO_2. */

    hGlobal = GlobalAlloc(GHND, dwNeeded);
    if (hGlobal == NULL)
    goto ABORT;
    pi2 = (PRINTER_INFO_2 *)GlobalLock(hGlobal);
    if (pi2 == NULL)
    goto ABORT;

    /* The second GetPrinter() fills in all the current settings, so all you
    need to do is modify what you'r interested in. */

    bFlag = GetPrinter(hPrinter, 2, (LPBYTE)pi2, dwNeeded, &dwNeeded);
    if (!bFlag)
    goto ABORT;

    /* Set orientation to Landscape mode if the driver supports it. */

    if ((pi2->pDevMode != NULL) && (pi2->pDevMode->dmFields & DM_ORIENTATION))
    {
        /* Change the devmode. */
        if (landscape)
        pi2->pDevMode->dmOrientation = DMORIENT_LANDSCAPE;
        else
        pi2->pDevMode->dmOrientation = DMORIENT_PORTRAIT;

        /* Make sure the driver-dependent part of devmode is updated as
        necessary. */
        lFlag = DocumentProperties(hwnd, hPrinter,
            (char*)pdm->dmDeviceName,
            pi2->pDevMode, pi2->pDevMode,
            DM_IN_BUFFER | DM_OUT_BUFFER);
        if (lFlag != IDOK)
        goto ABORT;

        /* Update printer information. */
        bFlag = SetPrinter(hPrinter, 2, (LPBYTE)pi2, 0);
        if (!bFlag)
        /* The driver supported the change but wasn't allowed due to some
        other reason (probably lack of permission). */
        goto ABORT;
    }
    else
    /* The driver doesn't support changing this. */
    goto ABORT;

    if (pi2 != NULL)
    GlobalUnlock(hGlobal);
    if (hGlobal != NULL)
    GlobalFree(hGlobal);
    if (hPrinter != NULL)
    ClosePrinter(hPrinter);
    GlobalUnlock(pdm);

    closeprint();                       // close any printer thats open
    ghdc = (HDC)initprint(1);           // re-open for new default settings

    return (int)ghdc;

    /* Clean up. */
    ABORT:
    if (pi2 != NULL)
    GlobalUnlock(hGlobal);
    if (hGlobal != NULL)
    GlobalFree(hGlobal);
    if (hPrinter != NULL)
    ClosePrinter(hPrinter);
    GlobalUnlock(pdm);
    return 0;
}

ENTRY initprint2 (int autoinit, int ToPage, int MyFlags, int BitMapped)
// initialize printing if not already initialized
{
    if (ghdc == NULL)
    {
        memset((void *) &pd, 0, sizeof(PRINTDLG));
        pd.hDevMode    = NULL;
        pd.hDevNames   = NULL;
        pd.lStructSize = sizeof(PRINTDLG);
        pd.hwndOwner   = (HANDLE)OwnerWindow;
        pd.hInstance   = NULL;
        pd.nMinPage    = 1;
        pd.nMaxPage    = 999;
        pd.nFromPage   = 1;
        pd.nToPage     = (unsigned short)ToPage;
        pd.nCopies     = 1;
    }
    else                // if already initialized, just return handle
    {
        if (autoinit)
        // and autoinit flag is set, then just return the handle
        return (long)ghdc;
    }

    if (autoinit)       // if automatic, then add extra PD_ to flag
    {

        pd.Flags = MyFlags |
        //                    PD_HIDEPRINTTOFILE |
        PD_PAGENUMS |
        PD_NOSELECTION |
        PD_RETURNDEFAULT |
        //                  PD_PRINTSETUP |
        PD_USEDEVMODECOPIES;

    }
    else                        // else remove this bit from the PD_ flag
    {

        pd.Flags = MyFlags |
        //                    PD_HIDEPRINTTOFILE |
        PD_PAGENUMS |
        //                    PD_NOSELECTION |
        //                  PD_PRINTSETUP |
        PD_USEDEVMODECOPIES;

    }

    if (!PrintDlg(&pd))
    return 0;               // leave on error, return NULL

    if (pd.hDC == NULL)
    {
        DEVNAMES* pDevNames;
        DEVMODE* pDevMode;

        if (pd.hDevNames == NULL)
        return 0;

        pDevNames = (DEVNAMES*) GlobalLock(pd.hDevNames);
        pDevMode = (pd.hDevMode != NULL) ?
        (DEVMODE*) GlobalLock(pd.hDevMode) : NULL;

        pd.hDC = CreateDC((LPCTSTR)pDevNames + pDevNames->wDriverOffset,
            (LPCTSTR)pDevNames + pDevNames->wDeviceOffset,
            (LPCTSTR)pDevNames + pDevNames->wOutputOffset,
            pDevMode);

        GlobalUnlock(pd.hDevNames);
        if (pd.hDevMode != NULL)
        GlobalUnlock(pd.hDevMode);
    }

    if (pd.hDC == NULL)
    {
        ghdc = NULL;
        return 0;
    }

    ghdc = pd.hDC;     // init the global handle to the printer device context

    if ((!(GetDeviceCaps(ghdc, RASTERCAPS) & RC_BITBLT)) & BitMapped)
    //    if (!(GetDeviceCaps (ghdc, RASTERCAPS) & RC_BITBLT))
    {
        DeleteDC(ghdc);
        MessageBox (hwnd, "Printer cannot display bitmaps.", "Device Error",
            MB_OK);
        ghdc = NULL;
        return 0;
    }

    return (long)ghdc;
}

ENTRY printflags (void)
{
    return (long)pd.Flags;
}

/* end of file */
