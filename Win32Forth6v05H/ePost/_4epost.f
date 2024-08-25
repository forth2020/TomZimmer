\ _4epost.f   October 12th, 2002 - 19:47

needs toolset.f
needs muTimer.f     \ A High resolution timer by Floyd
needs struct.f
needs shell_r.f     \ Thanks to discussion on comp.lang.forth and Leo Wong
needs w_search.f
needs sockets.f     \ Windows Sockets     by Andrey Cherezov
needs 4ePost.f
needs msgtree.f     \ Based on TreeView.F by Michael Hillerström

: msg-tree Start: 4ePost refresh-menu ;

\ debug .all  \ Here is the best place.

msg-tree

\s
  win32forth moderator <win32forth-owner@yahoogroups.com>
  Invitation to join the win32forth group 

\ The turnkey application works, but you do not see what is happening

 '  msg-tree turnkey 4ePost
 dos" 4ePost.exe" dos$ $exec drop
 bye

\s

