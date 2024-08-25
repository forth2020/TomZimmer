readme.txt  2022 Aug 06

This is a Visual C++ V6 project to create Fkernel.exe, and to copy Fkernel.exe to Win32For.exe and kernel.bin.

Each file when run will load an image file of the same name as the exe file 
e.g. Fkernel.exe loads Fkernel.img, Win32For.exe loads Win32For.img.

Run CopyFilesToWin32Forth.bat to copy these three files up one folder level to the Win32For6v05 folder.

In the Win32For6v05 folder, Extend.bat uses Fkernel.exe and src\Extend.f to create Win32For.img. 
When you run Win32For.exe it will load the new Win32For.img file with the extensions built in.
_MakeAll.bat rebuilds everything on the Forth side

To recompile this on Windows 10+ you will need :

1. VirtualBox-6.1.36-152435-Win.exe  to create a Virtual Machine  https://www.virtualbox.org/
2. VBoxGuestAdditions_6.1.36.iso  to extend Virtual Box  https://download.virtualbox.org/virtualbox/6.1.0_RC1/
3. en_windows_xp_professional_with_service_pack_3_x86_cd_vl_x14-73974.iso  to install the WinXP operating system in Virtual Box
	and its key : MRX3F-47B9T-2487J-KWKMF-RPWBY  https://archive.org/details/WinXPProSP3x86
4. Microsoft Visual CPP 6.0 Professional (ISO)	 https://winworldpc.com/download/f45515a0-588c-11e9-9db4-fa163e9022f0

A total of ~1.1 Gbytes of files to download, and a few hours to install...
Double click on Win32For.dsw (Visual C++ V6 Workspace file) and press F7.

Howerd Oakford  2022 Aug 06

06/08/2022  09:37       617,754,624 en_windows_xp_professional_with_service_pack_3_x86_cd_vl_x14-73974.iso
10/12/2020  03:54       309,757,111 Microsoft Visual CPP 6.0 Professional (ISO).7z
06/08/2022  12:38        63,803,392 VBoxGuestAdditions_6.1.36.iso
06/08/2022  09:48       111,496,224 VirtualBox-6.1.36-152435-Win.exe
06/08/2022  09:40               101 WinXP Professional key.txt