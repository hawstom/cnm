==INSTALL==
To finish installing CNM into any AutoCAD profile, you must do one of the following:

a) Drag CNM-Install.lsp from the desktop into an open drawing.  CNM-Install.lsp is 
open source in case you want to customize it.  A copy has also been placed in the 
application directory.

OR

b) Add the installation folder to your AutoCAD Support Files and Trusted paths.
Then use MENULOAD to load CNM.CUIX (and optionally FUNKY.CUIX function key osnaps).

==DEMONSTRATION==
Open Construction Notes Manager Examples\CNM-Demo.dwg from your Documents folder and 
run Construction Notes Manager Examples\CNM-Demo.scr.

==COMMAND REFERENCE==
Open CNM-Command-Reference.ods from the CNM folder. Sort by descending coolness 
(columns D and B) or descending category (columns A and B)

==KEYBOARD ALIASES==
CNM includes a couple dozen optimized keyboard aliases for native AutoCAD commands.
These aliases may override some of your custom PGP aliases. They will make your 
drafting easier if you take the time to learn them. (See more below.) If you find 
you cannot adapt to them or if you need to change any of them, or if you prefer 
to manage them using ACAD.PGP, you can use the CNMALIAS command to 
turn them off or change them.

==VERSION 5.0.0==
-New Cool Stuff
--Ribbon interface
--Bubble notes now are all-shapes-in-one.
--Bubble notes now include handy draggable wipeouts. Set your WIPEOUTFRAME to 2.
--Key notes table titles have empty shapes inserted.
--Easy spreadsheet editing of project notes with line wrapping
--Contour Elevate and/or Label (CEL) tool
--LX Plus (LXX) isolates layers for selected objects in Layer palette
--Lengthen command optimized with Dynamic default. LE/LLL
--CB/VB copy/paste at 0,0
--Acres, SF, SY, and SM (sq. mi.) commands.
--Printable keyboard commands list spreadsheet
--Twist view and set UCS to new view with TZ
--Quick mview locking and unlocking MVL/MVU

-Fixes and improvements
--Bubble notes gracefully stay away from NonAnnotative dimension styles.  They don't associate.
--CNM checks for project mistakes like a copied CNM.ini or 
  a CNM.INI and CNMPROJ.TXT in the same folder.
--"Link to Project" added to CNM menu to link multi-folder projects to a single CNM.ini.
--All commands work with Annotative scaling or DIMSCALE=0.

-Other changes
--Lots of behind-the-scenes cleanup.
--Where appropriate, old custom functions (THA -> LAYTHW, ONA -> LAYON) are now aliases for standard AutoCAD commands.
--The old HawsEDC acad.pgp aliases have been converted to LSP commands.  You can change
  or customize this using the CNMALIAS (CAM) command.
--A few command aliases have been shortened or changed.  Please give them a chance.
---ONA -> ON
---THA -> TH
---Y means Layer! AS/CL/CHM -> Y/YY/YYY and OFI/ONA -> YI/YU
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