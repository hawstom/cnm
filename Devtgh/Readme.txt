INSTALL
To finish installing CNM into any AutoCAD profile, you must do one of the following:

a) Drag CNM-Install.lsp from the desktop into an open drawing.  CNM-Install.lsp is 
open source in case you want to customize it.  A copy has also been placed in the 
application directory.

b) Add the installation folder to your AutoCAD Support Files and Trusted paths.
Then use MENULOAD to load CNM.mnu (and optionally FUNKY.MNU function key osnaps).

ACTIVATE OLD PGP ALIASES OR CUSTOMIZE COMMAND ALIASES (Quick Keys)
Type Haws-AliasEdit (or HAE) to activate old Haws PGP aliases or customize CNM/HawsEDC command aliases.

VERSION 4.2.30aaa (pre-pre-alpha)
-New Cool Stuff
--Bubble notes now are all-shapes-in-one.
--Contour Elevate and/or Label (CEL)
--Show nested layer in Layer palette after LX report (LXX)
--Show layers for selected objects in Layer palette (LXXX)
--Lengthen command optimized Dynamic default. LE/LLL
--CB/VB copy/paste at 0,0
--Acres, SF, SY, and SM (sq. mi.) commands.
--Printable keyboard commands list spreadsheet
--Twist view and set UCS to new view with TZ
--Quick mview locking and unlocking MVL/MVU

-Fixes and improvements
--Bubble notes gracefully stay away from Annotative dimension styles.  They don't associate.
--CNM checks for project mistakes like a copied CNM.ini or 
  a CNM.INI and CNMPROJ.TXT in the same folder.
--"Link to Project" added to CNM menu to link multi-folder projects to a single CNM.ini.
--All commands work with Annotative scaling or DIMSCALE=0.

-Other changes
--Lots of behind-the-scenes cleanup.
--Where appropriate, old functions (THA -> LAYTHW, ONA -> LAYON) are now aliases 
  for standard AutoCAD commands.
--The old HawsEDC acad.pgp aliases have been deactivated.  You can choose how 
  to activate them using the HawsPGP command.
--A few command aliases have been shortened or changed.  Please give them a chance.
---ONA -> ON
---THA -> TH
---AS, CL, CHM -> Y, YY, YYY
---CLONE -> CCC (Copy=C, Circle=CC, Clone=CCC)
---RRR -> R (RotateBase=R, Redraw=RR)

-Commands retired
--BB Bubble
--CU Copy up (use polar or ortho)
--ELEV0
--HELV
--JOIN (now an AutoCAD command)
--NS (Notesnap)

Please send all complaints, suggestions, and praise to tom.haws@gmail.com as soon as possible.

