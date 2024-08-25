/*
 * @file Forth.c
 * @brief Forth code for the Win32Forth kernel.
 * @details Original file : $Id: forth.c 1.1 1994/04/01 07:51:15 andrew Exp $
 * @author Andrew McKewan -- March 1994
 * @author Rod Oakford
 * @author Howerd Oakford  2022 Aug 06
 */

// See : https://docs.microsoft.com/en-us/previous-versions/bb985746(v=msdn.10)?redirectedfrom=MSDN

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#include "forth.h"
#include "term.h"

HANDLE the_instance;
HANDLE prev_instance;
LPSTR cmd_line;
int cmd_show;

char sbuf[300];

// HGLOBAL hmem = NULL;             // handle to memory block
BOOL doingdll = FALSE;              // are we running as a DLL?
int *imageL;                        // access forth as LONGs
char *imageB, *himageB;             // access forth and headers aas BYTEs
char *resourceimageB;               // pointer to the resource image
int imagesize;                      // size of image
int allocationsize;                 // how much space is allocated for Forth's use
int headallocationsize;             // how much space is allocated for Forth's symbol use
int actualsize;                     // how much space is used for Forth's use
int headactualsize;                 // how much space is used for Forth's symbols
int headseparation;                 // separation between code and heads in bytes
int *reservedaddress;
int reservedbytes;
int winsize = 0;                    // current window size state

HRSRC rHndl;
HGLOBAL rGlobal;

#define MAGIC 26350037              // magic # at imageL[1]

//////////////////// Function Table ////////////////////

ENTRY Bye (int exit_code)
{
    unload ();
    exit (exit_code);
}

ENTRY InitConsole (void)
{
    return init (the_instance, prev_instance, cmd_show);
}

// -------------------- Memory Allocation --------------------

// ------------- Current window Size State ----------------

ENTRY SizeState ()
{
    return (int) winsize;
}

// -------------------- Load Procedures from DLLs --------------------

ENTRY dummy (void)
{
    MessageBox (0,
        "Call to an invalid XCALL function", "XCALL Error", MB_OK);
    return 0;
}

// -------------------- Call Table --------------------

int (__stdcall *table[])() =
{
    Bye,                    // 0
    InitConsole,            // 1
    key,                    // 2
    Accept,                 // 3
    emit,                   // 4
    type,                   // 5
    cr,                     // 6
    cls,                    // 7
    keyq,                   // 8
    qcr,                    // 9
    dummy,                  // 10
    dummy,                  // 11
    dummy,                  // 12
    dummy,                  // 13
    dummy,                  // 14
    dummy,                  // 15
    dummy,                  // 16
    dummy,                  // 17
    dummy,                  // 18
    gotoxy,                 // 19
    getxy,                  // 20
    dummy, // fconvert,     // 21
    dummy, // fformat,      // 22
    getcolrow,              // 23
    dummy,                  // 24
    dummy,                  // 25
    dummy,                  // 26
    setfgbg,                // 27
    fpushkey,               // 28
    thescreen,              // 29
    charwh,                 // 30
    shiftmask,              // 31
    resize,                 // 32
    dummy,                  // 33
    dummy,                  // 34
    cursorshape,            // 35
    getcursor,              // 36
    havemenu,               // 37
    dummy,                  // 38
    printdlg,               // 39
    startprint,             // 40
    pageprint,              // 41
    endprint,               // 42
    initprint,              // 43
    closeprint,             // 44
    dummy,                  // 45
    dummy,                  // 46
    dummy,                  // 47
    dummy,                  // 48
    dummy,                  // 49
    dummy,                  // 50
    wscroll,                // 51
    rowoffset,              // 52
    maxcolrow,              // 53
    setmaxcolrow,           // 54
    dummy,                  // 55
    dummy,                  // 56
    qualityprint,           // 57
    startapage,             // 58
    endapage,               // 59
    printcopies,            // 60
    printfrompages,         // 61
    printtopages,           // 62
    dummy,                  // 63
    dummy,                  // 64
    dummy,                  // 65
    SizeState,              // 66
    orientation,            // 67
    dummy,                  // 68
    dummy,                  // 69
    dummy,                  // 70
    setcharwh,              // 71
    mark,                   // 72
    getfg,                  // 73
    getbg,                  // 74
    dummy,                  // 75
    dummy,                  // 76
    initprint2,             // 77
    printflags,             // 78
    dummy,                  // 79
};

// -------------------- Run Forth --------------------

void message (char *s)
{
    MessageBox (0, s, "Startup Error", MB_OK);
}

HKEY RegGetKey (void)
{
    HKEY theKey;

    RegCreateKey (HKEY_CURRENT_USER, "SOFTWARE\\Win32For\\Settings", &theKey);
    return theKey;
}

void unload (void)
{
    // char s[512];
    RECT rc;
    WINDOWPLACEMENT wp;
    BOOL result;

    //        ForthCleanup ();
    if (hwnd)
    {
        wp.length = sizeof(WINDOWPLACEMENT);
        if (!doingdll)
        {
            // Only save position if I am not minimized
            if (GetWindowPlacement(hwnd, &wp) && wp.showCmd != SW_SHOWMINIMIZED)
            {
                // Get the window's current position and size
                GetWindowRect(hwnd, &rc);
                save_screen_position(&rc); // save the current screen position into the registry Console value
            }
        }
        DestroyWindow (hwnd);
        hwnd = 0;
    }
    if (screen)
    free (screen);
    result = VirtualFree (reservedaddress, reservedbytes, MEM_DECOMMIT);
    result = VirtualFree (reservedaddress, 0,  MEM_RELEASE);
}

void CleanUp (FILE *f)      // Close and release whatever is needed, prior to finishing the load
{
    if (f != NULL)
    {
        fclose(f);
    }
    else
    {
        UnlockResource (rGlobal);
        FreeResource (rHndl);
    }
}

int load (LPSTR cmd_line)           // returns the size of the image in bytes
{
    FILE *f;
    int   temp[HEADLENGTH];
    int n;
    LPVOID requestedaddress;
    LPVOID headrequested;
    char buf[MAX_SBUF];
    char *sbuf_end;
    LPSTR imagename;

    HMODULE myhandle;

    if (!doingdll)          // if not running as a DLL, then try to open .IMG file
    {
        //       strcpy (buf, cmd_line);
        strncpy (buf, cmd_line, MAX_SBUF-1);   // RO
        // strncpy_s (buf, MAX_SBUF, cmd_line, MAX_SBUF-1);   // HO
        strupr (buf);
        // _strupr_s (buf, MAX_SBUF); // HO
        imagename = strstr(buf, "/IMAGE");
        if (imagename)          // allow commandline specification of image filename to load
        {                       // used in the form:  '/IMAGE imagename.img'
            imagename = imagename+6;
            imagename = strtok (imagename, " ");
            // imagename = strtok_s(imagename, " ", NULL); // HO
            if (imagename)
            strcpy (sbuf, imagename);
            // strcpy_s(sbuf, MAX_SBUF, imagename); // HO
        }
        else
        {
            myhandle = GetModuleHandle (NULL);
            GetModuleFileName (myhandle, sbuf, MAX_SBUF);
        }
        sbuf_end = strrchr(sbuf, '.');          // look for last occurance of a decimal point
        if  (sbuf_end)
         *sbuf_end = 0;                         // truncate name at decimal point
        strcat (sbuf, ".IMG\0");                       // then append the image extension
        // strcat_s(sbuf, MAX_SBUF, ".IMG\0");     // then append the image extension HO
        f = fopen (sbuf, "rb");                        // open from program directory
        // if (fopen_s(&f, sbuf, "rb"))            // open from program directory - does not work!!!
        // {
        //     // error condition
        // }
    }
    else
    {
        f = NULL;           // force "f" to NULL if running as a DLL
    }
    if (f == NULL)          // if "f" is NULL, then read in the resource image
    {
		rHndl = FindResource (the_instance, "#1234", "IMAGE");
        if (!rHndl)
        {
            message ("FindResource: Failed to find the Win32Forth resource or .IMG file");
            return (0);
        }
		rGlobal = LoadResource (the_instance,rHndl);
        if (!rGlobal)
        {
            FreeResource (rHndl);
            message ("LoadResource: Failed to load the Win32Forth resource or .IMG file");
            return (0);
        }
        resourceimageB = LockResource (rGlobal);
        actualsize = HEADLENGTH;                        // header length in Longs
        actualsize = actualsize * 4;                    // header length in Bytes
        MoveMemory (temp, resourceimageB, actualsize);  // move Bytes into temp buffer
    }
    else
    {
        fread (temp, sizeof(int), HEADLENGTH, f);   // read the first three cells from the image
    }

    if (temp[MAGICN] != MAGIC)                 // verify that the image is really ME!
    {
        CleanUp (f);
        message ("Bad dictionary image");
        return (0);
    }

    allocationsize     = temp[DSIZE];          // Forth dictionary size is at offset 2 long words
    actualsize         = temp[DACTUAL];
    headallocationsize = temp[HSIZE];          // Forth dictionary size is at offset 2 long words
    headactualsize     = temp[HACTUAL];
    headseparation     = temp[HSEPARATE];
    requestedaddress   = (LPVOID)temp[ORIG];   // the address where I would like to be located

    if (headallocationsize != 0)               // only allocate a bunch of room if we need it
    reservedbytes = headseparation+headallocationsize;
    else
    reservedbytes = allocationsize;

    reservedaddress = VirtualAlloc (requestedaddress, reservedbytes, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    if (reservedaddress == NULL)
    {
        CleanUp (f);
        wsprintf (buf, "Failed to reserve %d bytes of memory", reservedbytes);
        message (buf);
        return (0);
    }
    else
    {
        if ((reservedaddress != requestedaddress) && (requestedaddress != 0))
        {
            CleanUp (f);
            wsprintf (buf, "Requested address: %d was not a 64k boundry\nInstead, reserved: %d",
                requestedaddress, reservedaddress);
            message (buf);
            return (0);
        }
    }
    imageB = VirtualAlloc (reservedaddress, allocationsize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if (imageB == NULL)
    {
        CleanUp (f);
        wsprintf (buf, "Failed to allocate %d bytes at address %d", allocationsize, reservedaddress);
        message (buf);
        return (0);
    }
    imageL = (int *)imageB;             // we have a byte pointe and a Long pointer

    if (f == NULL)
    {
        MoveMemory (imageB, resourceimageB, actualsize);
        n = actualsize;
        resourceimageB = resourceimageB+actualsize;
    }
    else
    {
        rewind (f);
        n = fread (imageB, 1, actualsize, f);
    }
    headrequested = imageB+headseparation;
    // Allocate space for the headers which will be located 10 megabytes above the code/data area
    if (headallocationsize != 0)
    {
        himageB = VirtualAlloc (headrequested, headallocationsize, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
        if (himageB == NULL)
        {
            CleanUp (f);
            wsprintf (buf, "Failed to allocate %d bytes at address %d", headallocationsize, headrequested);
            message (buf);
            return (0);
        }
        if (headrequested != himageB)
        {
            CleanUp (f);
            wsprintf (buf, "HSystem Dictionary Allocation Failed!\n Requested addr: %x,\n     but Received: %x", headrequested, himageB);
            message (buf);
            return (0);
        }
        if (f == NULL)
        {
            MoveMemory (himageB, resourceimageB, headactualsize);
            n += headactualsize;
        }
        else
        {
            n += fread (headrequested, 1, headactualsize, f);
        }
    }
    CleanUp (f);
    return (n);
}

typedef int (*PFUNC)();

int run (int *imageL)
{
    PFUNC entry;
    if (imageL[ORIG]==0)
    entry=(PFUNC) ((int)imageL + imageL[CENTRY]);
    else
    entry=(PFUNC) imageL[CENTRY];
    return (*entry)();
}

int PASCAL WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
    LPSTR lpszCmdParam, int nCmdShow)
{
    LPCSTR k32_libname = "KERNEL32.DLL" ;
    HMODULE k32_libaddr ;
    int r;
    the_instance = hInstance ;
    prev_instance = hPrevInstance;
    cmd_show = nCmdShow;
    cmd_line = lpszCmdParam;

    doingdll = FALSE;                       // we are a normal windows program

    imagesize = load (cmd_line);

    if (imagesize == 0) return 0;

    imageL[TABLE]  = (int) table;
    imageL[CMDLEN] = strlen (cmd_line);
    if (imageL[ORIG]==0)
    imageL[CMDADR] = (int) cmd_line - (int) imageL; // relative address
    else

    imageL[CMDADR] = (int) cmd_line;           // absolute address
    imageL[HCON]        = 0;                   // old window NONE
    imageL[WINMSGNTRY]  = 0;                   // clear the window message handler
    imageL[MSGNTRY]     = 0;                   // clear the key message handler
    k32_libaddr = GetModuleHandle(k32_libname) ;
    imageL[K32LIB]      = (int) (k32_libaddr) ;
    imageL[K32GPA]      = (int) (GetProcAddress(k32_libaddr,"GetProcAddress")) ;

    r = run (imageL);
    Bye (r);
    return (0);
}

/* end of file */
