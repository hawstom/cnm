;;; Command aliases for all CNM commands.
;;; To change a command name to your liking,
;;; Just change the Alias.
;;; It's strongly recommended to copy only the commands you change
;;; to a USER.LSP file in the AutoCAD support files search path above
;;; the CNM application folder. CNM will automatically load it if found.
;;;
;;; Order of commands: 1. Section 2. Random
;;;
;;; Define as AutoCAD command.
;;; https://www.theswamp.org/index.php?topic=51409.msg565523#msg565523
(load "cnmaliaslib")
;;;---------------Custom command aliases-------------
;;; All of these can be disabled as a group using the CNMAlias command.
;;; They can also be converted to custom ACAD.PGP aliases with help from the same command.
;;; (hcnm-define-command-alias '("Command" "Alias" "ActivationGroup" "Message"))
(hcnm-define-command-alias '("Aperture" "AP" "Custompgp" ""))
(hcnm-define-command-alias '("LayMCur" "AS" "Custompgp" "Use Y for improved flow (AS/CL/CHM -> Y/YY/YYY)."))
(hcnm-define-command-alias '("Break" "B" "Custompgp" ""))
(hcnm-define-command-alias '("Block" "BL" "Custompgp" ""))
(hcnm-define-command-alias '("Rectang" "BX" "Custompgp" ""))
(hcnm-define-command-alias '("Circle" "CC" "Custompgp" ""))
(hcnm-define-command-alias '("Chamfer" "CF" "Custompgp" ""))
(hcnm-define-command-alias '("Change" "CH" "Custompgp" ""))
(hcnm-define-command-alias '("CopyToLayer" "CCC" "Custompgp" ""))
(hcnm-define-command-alias '("DimAligned" "DA" "Custompgp" ""))
(hcnm-define-command-alias '("DimCenter" "DC" "Custompgp" ""))
(hcnm-define-command-alias '("DimHorizontal" "DH" "Custompgp" ""))
(hcnm-define-command-alias '("Dimscale" "DSC" "Custompgp" ""))
(hcnm-define-command-alias '("Gripsize" "GS" "Custompgp" ""))
(hcnm-define-command-alias '("Insbase" "IB" "Custompgp" ""))
(hcnm-define-command-alias '("Join" "JOIN" "Custompgp" "Join has been replaced by PJL and PJ.  Issuing command JOIN."))
(hcnm-define-command-alias '("LayUniso" "YU" "Custompgp" ""))
(hcnm-define-command-alias '("Mirror" "MM" "Custompgp" ""))
(hcnm-define-command-alias '("MLeader" "MMM" "Custompgp" ""))
(hcnm-define-command-alias '("Mtext" "TTT" "Custompgp" ""))
(hcnm-define-command-alias '("LayOn" "ONA" "Custompgp" "Use ON for improved flow (or YU for laYUniso)."))
(hcnm-define-command-alias '("LayOn" "ON" "Custompgp" "Use YU for laYUniso"))
(hcnm-define-command-alias '("Pickbox" "PB" "Custompgp" ""))
(hcnm-define-command-alias '("Polygon" "PG" "Custompgp" ""))
(hcnm-define-command-alias '("Redo" "RD" "Custompgp" ""))
(hcnm-define-command-alias '("Regenall" "RG" "Custompgp" ""))
(hcnm-define-command-alias '("Redraw" "RR" "Custompgp" ""))
(hcnm-define-command-alias '("Text" "TT" "Custompgp" ""))
(hcnm-define-command-alias '("Trim" "T" "Custompgp" "Text is TT. Mtext is TTT"))
(hcnm-define-command-alias '("Laythw" "THA" "Custompgp" "Use TH for improved flow"))
(hcnm-define-command-alias '("Laythw" "TH" "Custompgp" ""))
(hcnm-define-command-alias '("Linetype" "TY" "Custompgp" ""))
(hcnm-define-command-alias '("LayUlk" "UL" "Custompgp" ""))
(hcnm-define-command-alias '("Ddunits" "UU" "Custompgp" ""))
(hcnm-define-command-alias '("Viewports" "VP" "Custompgp" ""))
(hcnm-define-command-alias '("-Wblock" "WW" "Custompgp" ""))
(hcnm-define-command-alias '("Extend" "X" "Custompgp" ""))
(hcnm-define-command-alias '("LayMCur" "Y" "Custompgp" ""))
;;;---------------Special custom comand aliases-------------   
;;; These want to be simple aliases, but when defined as simple LISP aliases,
;;; they act different than their native commands or PGP aliases,
;;; probably to preserve legacy behavior.
;;; Behavior is corrected using more than a single line of code.   
(hcnm-define-lisp-alias '("haws-copy" "c" "Custompgp" "Circle is CC. Use CNMAlias to change."))
;;;---------------LISP versions of the Autodesk standard command aliases-------------
;;; All of these can be disabled as a group using the CNMAlias command.
;;; They can also be converted to custom ACAD.PGP aliases with help from the same command.
;;; (hcnm-define-command-alias '("Command" "Alias" "ActivationGroup" "Message"))

(hcnm-define-command-alias '("-Insert" "-I" "Standardpgp" ""))
(hcnm-define-command-alias '("-Wblock" "-W" "Standardpgp" ""))
(hcnm-define-command-alias '("-Xref" "-XR" "Standardpgp" ""))
(hcnm-define-command-alias '("Arc" "A" "Standardpgp" ""))
(hcnm-define-command-alias '("Array" "AR" "Standardpgp" ""))
(hcnm-define-command-alias '("DimAngular" "DAN" "Standardpgp" ""))
(hcnm-define-command-alias '("Distance" "DI" "Standardpgp" ""))
(hcnm-define-command-alias '("Dview" "DV" "Standardpgp" ""))
(hcnm-define-command-alias '("Erase" "E" "Standardpgp" ""))
(hcnm-define-command-alias '("Ellipse" "EL" "Standardpgp" ""))
(hcnm-define-command-alias '("Fillet" "F" "Standardpgp" ""))
(hcnm-define-command-alias '("Hatch" "H" "Standardpgp" ""))
(hcnm-define-command-alias '("Insert" "I" "Standardpgp" ""))
(hcnm-define-command-alias '("Line" "L" "Standardpgp" ""))
(hcnm-define-command-alias '("List" "LI" "Standardpgp" ""))
(hcnm-define-command-alias '("Ltscale" "LTS" "Standardpgp" ""))
(hcnm-define-command-alias '("Move" "M" "Standardpgp" ""))
(hcnm-define-command-alias '("Mspace" "MS" "Standardpgp" ""))
(hcnm-define-command-alias '("Offset" "O" "Standardpgp" ""))
(hcnm-define-command-alias '("Ddosnap" "OS" "Standardpgp" ""))
(hcnm-define-command-alias '("Pan" "P" "Standardpgp" ""))
(hcnm-define-command-alias '("Pedit" "PE" "Standardpgp" ""))
(hcnm-define-command-alias '("Pline" "PL" "Standardpgp" ""))
(hcnm-define-command-alias '("Pspace" "PS" "Standardpgp" ""))
(hcnm-define-command-alias '("Scale" "SC" "Standardpgp" ""))
(hcnm-define-command-alias '("Snap" "SN" "Standardpgp" ""))
(hcnm-define-command-alias '("View" "V" "Standardpgp" ""))
(hcnm-define-command-alias '("Wblock" "W" "Standardpgp" ""))
(hcnm-define-command-alias '("Xref" "XR" "Standardpgp" ""))
(hcnm-define-command-alias '("Zoom" "Z" "Standardpgp" ""))

;;; These are the aliases to all CNM lisp routines that are
;;; even a little bit more than a simple command alias.
;;;(hcnm-define-lisp-alias '("LongName" "Alias" "ActivationGroup" "Message")
;;;Profile Drafter
(hcnm-define-lisp-alias '("haws-pro" "pro" "Tools" ""))
(hcnm-define-lisp-alias '("haws-eop" "eop" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ellabel" "ellabel" "Tools" ""))
(hcnm-define-lisp-alias '("haws-elv" "elv" "Tools" ""))
(hcnm-define-lisp-alias '("haws-eop" "eop" "Tools" ""))
(hcnm-define-lisp-alias '("haws-grb" "grb" "Tools" ""))
(hcnm-define-lisp-alias '("haws-grc" "grc" "Tools" ""))
(hcnm-define-lisp-alias '("haws-grd" "grd" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lst" "lst" "Tools" ""))
(hcnm-define-lisp-alias '("haws-newpro" "newpro" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pc" "pc" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pldr" "pldr" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pm" "pm" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pred" "pred" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pro" "pro" "Tools" ""))
(hcnm-define-lisp-alias '("haws-procb" "procb" "Tools" ""))
(hcnm-define-lisp-alias '("haws-proe" "proe" "Tools" ""))
(hcnm-define-lisp-alias '("haws-profc" "profc" "Tools" ""))
(hcnm-define-lisp-alias '("haws-promh" "promh" "Tools" ""))
(hcnm-define-lisp-alias '("haws-propipe" "propipe" "Tools" ""))
(hcnm-define-lisp-alias '("haws-prosup" "prosup" "Tools" ""))
(hcnm-define-lisp-alias '("haws-stalabel" "stalabel" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sel" "sel" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ser" "ser" "Tools" ""))
;;;---------------Civil Drafting Section---------------   )
(hcnm-define-lisp-alias '("haws-2l" "2l" "Tools" ""))
(hcnm-define-lisp-alias '("haws-berm" "berm" "Tools" ""))
(hcnm-define-lisp-alias '("haws-acres" "ac" "Tools" ""))
(hcnm-define-lisp-alias '("haws-contelev" "cel" "Tools" ""))
(hcnm-define-lisp-alias '("haws-contvol" "cv" "Tools" ""))
(hcnm-define-lisp-alias '("haws-contvol" "contvol" "Tools" ""))
(hcnm-define-lisp-alias '("haws-curve" "curve" "Tools" ""))
(hcnm-define-lisp-alias '("haws-geodata" "cd" "Tools" ""))
(hcnm-define-lisp-alias '("haws-geodata" "gd" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dm" "dm" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dw" "dw" "Tools" ""))
(hcnm-define-lisp-alias '("haws-m40" "m40" "Tools" ""))
(hcnm-define-lisp-alias '("haws-m42" "m42" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mc2033" "mc2033" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pipe" "pipe" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tap" "tap" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tapinv" "tapinv" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ut" "ut" "Tools" ""))
(hcnm-define-lisp-alias '("haws-wall" "wall" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ws" "ws" "Tools" ""))
(hcnm-define-lisp-alias '("haws-zero" "zero" "Tools" ""))
;;;---------------Construction Notes Section---------------   )
(hcnm-define-lisp-alias '("haws-boxl" "boxl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cirl" "cirl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dial" "dial" "Tools" ""))
(hcnm-define-lisp-alias '("haws-elll" "elll" "Tools" ""))
(hcnm-define-lisp-alias '("haws-hexl" "hexl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-octl" "octl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-penl" "penl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-recl" "recl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sstl" "sstl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tril" "tril" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tcg" "tcg" "Tools" ""))
(hcnm-define-lisp-alias '("haws-txtl" "txtl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-bw" "bw" "Tools" ""))
(hcnm-define-lisp-alias '("hcnm-cnm" "cnm" "Tools" ""))
(hcnm-define-lisp-alias '("hcnm-cnmoptions" "cnmop" "Tools" ""))
(hcnm-define-lisp-alias '("hcnm-noteseditor" "noteseditor" "Tools" ""))
(hcnm-define-lisp-alias '("hcnm-notesedit" "notesedit" "Tools" ""))
(hcnm-define-lisp-alias '("haws-notesfilein" "notesfilein" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cnmmenu" "cnmmenu" "Tools" ""))
(hcnm-define-lisp-alias '("haws-phaseedit" "phaseedit" "Tools" ""))
(hcnm-define-lisp-alias '("haws-setnotephases" "setnotephases" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ntpurge" "ntpurge" "Tools" ""))
(hcnm-define-lisp-alias '("hcnm-setnotesbubblestyle" "setnotesbubblestyle" "Tools" ""))
;;; INSBLK sub-section   )
(hcnm-define-lisp-alias '("haws-GB" "GB" "Tools" ""))
(hcnm-define-lisp-alias '("haws-GC" "GC" "Tools" ""))
(hcnm-define-lisp-alias '("haws-INVL" "INVL" "Tools" ""))
(hcnm-define-lisp-alias '("haws-INVR" "INVR" "Tools" ""))
(hcnm-define-lisp-alias '("haws-LOTEL" "LOTEL" "Tools" ""))
(hcnm-define-lisp-alias '("haws-PAD" "PAD" "Tools" ""))
(hcnm-define-lisp-alias '("haws-REV" "REV" "Tools" ""))    ; From civil command group)
(hcnm-define-lisp-alias '("haws-secb" "secb" "Tools" ""))
(hcnm-define-lisp-alias '("haws-secl" "secl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-secr" "secr" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sect" "sect" "Tools" ""))
(hcnm-define-lisp-alias '("haws-SLL" "SLL" "Tools" ""))
(hcnm-define-lisp-alias '("haws-SLOPE" "SLOPE" "Tools" ""))
(hcnm-define-lisp-alias '("haws-SLR" "SLR" "Tools" ""))
(hcnm-define-lisp-alias '("haws-SPOTEL" "SPOTEL" "Tools" ""))
(hcnm-define-lisp-alias '("haws-TC" "TC" "Tools" ""))      ; From profiles command group)
(hcnm-define-lisp-alias '("haws-TCELEV" "TCELEV" "Tools" ""))
(hcnm-define-lisp-alias '("haws-TCELEVL" "TCL" "Tools" ""))
(hcnm-define-lisp-alias '("haws-TCELEVR" "TCR" "Tools" ""))
;;;---------------Horizontal Control Section---------------   )
(hcnm-define-lisp-alias '("haws-bdl" "bdl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-bdp" "bdp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ne" "ne" "Tools" ""))
(hcnm-define-lisp-alias '("haws-stacl" "stacl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xy" "xy" "Tools" ""))
;;;---------------Dimensioning Section---------------   )
(hcnm-define-lisp-alias '("haws-d1" "d1" "Tools" ""))
(hcnm-define-lisp-alias '("haws-d2" "d2" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dp" "dp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-du" "du" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ht" "ht" "Tools" ""))
(hcnm-define-lisp-alias '("haws-te" "te" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xx" "xx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-aar" "aar" "Tools" ""))
(hcnm-define-lisp-alias '("haws-chdim" "chdim" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dimsty" "dimsty" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dot" "dot" "Tools" ""))
(hcnm-define-lisp-alias '("haws-loop" "loop" "Tools" ""))
(hcnm-define-lisp-alias '("haws-none" "none" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tilde" "tilde" "Tools" ""))
(hcnm-define-lisp-alias '("haws-led" "led" "Tools" ""))
;;;---------------Text Section---------------   )
(hcnm-define-lisp-alias '("haws-add" "add" "Tools" ""))
(hcnm-define-lisp-alias '("haws-chnum" "chnum" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cht" "cht" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cmt" "cmt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-contxt" "contxt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cs" "cs" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ee" "ee" "Tools" ""))
(hcnm-define-lisp-alias '("haws-facnum" "facnum" "Tools" ""))
(hcnm-define-lisp-alias '("haws-imp_exp" "imp_exp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-incnum" "incnum" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l80" "l80" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l100" "l100" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l120" "l120" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l140" "l140" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l175" "l175" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l200" "l200" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l240" "l240" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l290" "l290" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l350" "l350" "Tools" ""))
(hcnm-define-lisp-alias '("haws-l500" "l500" "Tools" ""))
(hcnm-define-lisp-alias '("haws-letter" "letter" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lotnum" "lotnum" "Tools" ""))
(hcnm-define-lisp-alias '("haws-num" "num" "Tools" ""))
(hcnm-define-lisp-alias '("haws-presuf" "presuf" "Tools" ""))
(hcnm-define-lisp-alias '("haws-romans" "romans" "Tools" ""))
(hcnm-define-lisp-alias '("haws-round" "round" "Tools" ""))
(hcnm-define-lisp-alias '("haws-selstyle" "selstyle" "Tools" ""))
(hcnm-define-lisp-alias '("haws-to" "to" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tu" "tu" "Tools" ""))
(hcnm-define-lisp-alias '("haws-txtsum" "txtsum" "Tools" ""))
;;;---------------Layer Management Section---------------   )
(hcnm-define-lisp-alias '("haws-l0" "l0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lk0" "lk0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lk" "lk" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lka" "lka" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lki" "lki" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ofi" "ofi" "Tools" "Use YI for improved flow (OFI/ONA -> YI/YU)"))
(hcnm-define-lisp-alias '("haws-ofi" "yi" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ula" "ula" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ca" "ca" "Tools" ""))
(hcnm-define-lisp-alias '("haws-chm" "chm" "Tools" "\Use YYY for improved flow (AS/CL/CHM -> Y/YY/YYY)"))
(hcnm-define-lisp-alias '("haws-cl" "cl" "Tools" "\nUse YY for improved flow (AS/CL/CHM -> Y/YY/YYY)"))
(hcnm-define-lisp-alias '("haws-ff" "ff" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ffx" "fff" "Tools" "\nConsider using FFF instead of FFX."))
(hcnm-define-lisp-alias '("haws-ffx" "ffx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-off" "off" "Tools" ""))
(hcnm-define-lisp-alias '("haws-offx" "offx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uff" "uff" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uoff" "uoff" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uffx" "uffx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uoffx" "uoffx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ffi" "ffi" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lar" "lar" "Tools" ""))
(hcnm-define-lisp-alias '("haws-las" "las" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lcp" "lcp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lcpx" "lcpx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ltp" "ltp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ltpx" "ltpx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lwp" "lwp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lwpx" "lwpx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lk" "lk" "Tools" ""))
(hcnm-define-lisp-alias '("haws-laprn" "laprn" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ltb" "ltb" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ltc" "ltc" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lth" "lth" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lx" "lx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lxx" "lxx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-oo" "oo" "Tools" ""))
(hcnm-define-lisp-alias '("haws-offsetx" "offsetx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cl" "yy" "Tools" ""))
(hcnm-define-lisp-alias '("haws-chm" "yyy" "Tools" ""))
;;;---------------Block Management Section---------------   )
(hcnm-define-lisp-alias '("haws-a2t" "a2t" "Tools" ""))
(hcnm-define-lisp-alias '("haws-att2txt" "att2txt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-attredef" "attredef" "Tools" ""))
(hcnm-define-lisp-alias '("haws-at" "at" "Tools" ""))
(hcnm-define-lisp-alias '("haws-bl0" "bl0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xda" "xda" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xra" "xra" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xrl" "xrl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-chattrib" "chattrib" "Tools" ""))
(hcnm-define-lisp-alias '("haws-incatt" "incatt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xin" "in" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xout" "out" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xd" "xd" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xro" "xro" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xroffet" "xroffet" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xu" "xu" "Tools" ""))
;;;---------------Inquiry Section---------------   )
(hcnm-define-lisp-alias '("haws-eg" "eg" "Tools" ""))
(hcnm-define-lisp-alias '("haws-egn" "egn" "Tools" ""))
(hcnm-define-lisp-alias '("haws-acres" "ac" "Tools" ""))
(hcnm-define-lisp-alias '("haws-aee" "aee" "Tools" ""))
(hcnm-define-lisp-alias '("haws-aet" "aet" "Tools" ""))
(hcnm-define-lisp-alias '("haws-geodata" "al" "Tools" ""))
(hcnm-define-lisp-alias '("haws-adl" "adl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-goto" "goto" "Tools" ""))
(hcnm-define-lisp-alias '("haws-istan" "istan" "Tools" ""))
(hcnm-define-lisp-alias '("haws-md" "md" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sf" "sf" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sm" "sm" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sy" "sy" "Tools" ""))
(hcnm-define-lisp-alias '("haws-wl" "wl" "Tools" ""))
;;;---------------Editing Section---------------   )
(hcnm-define-lisp-alias '("haws-bf" "bf" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cb" "cb" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ec" "ec" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ew" "ew" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mc" "mc" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mp" "mp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mw" "mw" "Tools" ""))
(hcnm-define-lisp-alias '("haws-r1" "r1" "Tools" ""))
(hcnm-define-lisp-alias '("haws-r2" "r2" "Tools" ""))
(hcnm-define-lisp-alias '("haws-r4" "r4" "Tools" ""))
(hcnm-define-lisp-alias '("haws-r9" "r9" "Tools" ""))
(hcnm-define-lisp-alias '("haws-s" "s" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ub" "ub" "Tools" ""))
(hcnm-define-lisp-alias '("haws-um" "um" "Tools" ""))
(hcnm-define-lisp-alias '("haws-vb" "vb" "Tools" ""))
(hcnm-define-lisp-alias '("haws-bm" "bm" "Tools" ""))
(hcnm-define-lisp-alias '("haws-brk" "brk" "Tools" ""))
(hcnm-define-lisp-alias '("haws-clone" "clone" "Tools" "Try AutoCAD CopyToLayer (CNM alias CCC)"))
(hcnm-define-lisp-alias '("haws-copyrot" "cr" "Tools" ""))
(hcnm-define-lisp-alias '("haws-copyrotdrag" "crd" "Tools" ""))
(hcnm-define-lisp-alias '("haws-chcoord" "chcoord" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cw" "cw" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cmpro" "cmpro" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lengthen" "le" "Tools" ""))
(hcnm-define-lisp-alias '("haws-lengthen" "lll" "Tools" ""))
(hcnm-define-lisp-alias '("haws-makepoly" "makepoly" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mf" "mf" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mof" "mof" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mscr" "mscr" "Tools" ""))
(hcnm-define-lisp-alias '("haws-na" "na" "Tools" ""))
(hcnm-define-lisp-alias '("haws-newscale" "newscale" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pj" "pj" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pjl" "pjl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-rescale" "rescale" "Tools" ""))
(hcnm-define-lisp-alias '("haws-rotatebase" "r" "Tools" ""))
(hcnm-define-lisp-alias '("haws-rotatebase" "rrr" "Tools" "Try R for rotatebase"))
(hcnm-define-lisp-alias '("haws-ssx" "ssx" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ssxpro" "ssxpro" "Tools" ""))
(hcnm-define-lisp-alias '("haws-swap" "swap" "Tools" ""))
;;;---------------Drawing Section---------------)   )
(hcnm-define-lisp-alias '("haws-c2" "c2" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ct" "ct" "Tools" ""))
(hcnm-define-lisp-alias '("haws-p0" "p0" "Tools" ""))
;;;---------------Views and Zooms Section---------------   )
(hcnm-define-lisp-alias '("haws-2x" "2x" "Tools" ""))
(hcnm-define-lisp-alias '("haws-5x" "5x" "Tools" ""))
(hcnm-define-lisp-alias '("haws-9x" "9x" "Tools" ""))
(hcnm-define-lisp-alias '("haws-twz" "tz" "Tools" ""))
(hcnm-define-lisp-alias '("haws-x2" "x2" "Tools" ""))
(hcnm-define-lisp-alias '("haws-z0" "z0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-za" "za" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ze" "ze" "Tools" ""))
(hcnm-define-lisp-alias '("haws-zi" "zi" "Tools" ""))
(hcnm-define-lisp-alias '("haws-zo" "zo" "Tools" ""))
(hcnm-define-lisp-alias '("haws-zv" "zv" "Tools" ""))
(hcnm-define-lisp-alias '("haws-zz" "zz" "Tools" ""))
(hcnm-define-lisp-alias '("haws-zw" "zw" "Tools" ""))
(hcnm-define-lisp-alias '("haws-tw" "tw" "Tools" ""))
;;;---------------Setup and Drawing Environment Section---------------   )
(hcnm-define-lisp-alias '("haws-aa" "aa" "Tools" ""))
(hcnm-define-lisp-alias '("haws-adt" "adt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cet" "cet" "Tools" ""))
(hcnm-define-lisp-alias '("haws-cmd" "cmd" "Tools" ""))
(hcnm-define-lisp-alias '("haws-dia" "dia" "Tools" ""))
(hcnm-define-lisp-alias '("haws-fdt" "fdt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-il" "il" "Tools" ""))
(hcnm-define-lisp-alias '("haws-io" "io" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ir" "ir" "Tools" ""))
(hcnm-define-lisp-alias '("haws-it" "it" "Tools" ""))
(hcnm-define-lisp-alias '("haws-llt" "llt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mvl" "mvl" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mvu" "mvu" "Tools" ""))
(hcnm-define-lisp-alias '("haws-ose" "ose" "Tools" ""))
(hcnm-define-lisp-alias '("haws-osi" "osi" "Tools" ""))
(hcnm-define-lisp-alias '("haws-osm" "osm" "Tools" ""))
(hcnm-define-lisp-alias '("haws-osn" "osn" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pslt" "pslt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-rga" "rga" "Tools" ""))
(hcnm-define-lisp-alias '("haws-qt" "qt" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uf" "uf" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uf0" "uf0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-uf1" "uf1" "Tools" ""))
(hcnm-define-lisp-alias '("haws-clean" "clean" "Tools" ""))
(hcnm-define-lisp-alias '("haws-funky" "funky" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xin" "xin" "Tools" ""))
(hcnm-define-lisp-alias '("haws-xout" "xout" "Tools" ""))
(hcnm-define-lisp-alias '("haws-modestat" "modestat" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mren" "mren" "Tools" ""))
(hcnm-define-lisp-alias '("haws-mv" "mv" "Tools" ""))
(hcnm-define-lisp-alias '("haws-polarset" "polarset" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pall" "pall" "Tools" ""))
(hcnm-define-lisp-alias '("haws-proto" "proto" "Tools" ""))
(hcnm-define-lisp-alias '("haws-protox" "protox" "Tools" ""))
(hcnm-define-lisp-alias '("haws-setup" "setup" "Tools" ""))
(hcnm-define-lisp-alias '("haws-10" "10" "Tools" ""))
(hcnm-define-lisp-alias '("haws-12" "12" "Tools" ""))
(hcnm-define-lisp-alias '("haws-setdim10" "setdim10" "Tools" ""))
(hcnm-define-lisp-alias '("haws-setdim12" "setdim12" "Tools" ""))
(hcnm-define-lisp-alias '("haws-sheet" "sheet" "Tools" ""))
(hcnm-define-lisp-alias '("haws-0" "0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-1" "1" "Tools" ""))
(hcnm-define-lisp-alias '("haws-u0" "u0" "Tools" ""))
(hcnm-define-lisp-alias '("haws-u1" "u1" "Tools" ""))
(hcnm-define-lisp-alias '("haws-u2" "u2" "Tools" ""))
(hcnm-define-lisp-alias '("haws-u3" "u3" "Tools" ""))
(hcnm-define-lisp-alias '("haws-u8" "u8" "Tools" ""))
(hcnm-define-lisp-alias '("haws-us" "us" "Tools" ""))
;;;---------------Miscellaneous Section---------------   )
(hcnm-define-lisp-alias '("haws-ffa" "ffa" "Tools" ""))
(hcnm-define-lisp-alias '("haws-aliasmanage" "cam" "Tools" ""))
(hcnm-define-lisp-alias '("haws-aliasmanage" "ham" "Tools" "Use CAM (CNMAlias)"))
(hcnm-define-lisp-alias '("haws-user" "user" "Tools" ""))
(hcnm-define-lisp-alias '("haws-pgpedit" "pgp" "Tools" ""))
(hcnm-define-lisp-alias '("haws-loadandrun" "lr" "Tools" ""))
(hcnm-define-lisp-alias '("haws-loadandrun" "run" "Tools" "RUN has been replaced with LR (lisprun)."))

;; For certain legacy routines in the wild that depend on the error trapper in the legacy file lisputil.lsp
(defun erdf$@ () (haws-errdef))

;;;---------------Set a flag that aliases have been loaded
(setq *hcnm-cnmaliasloaded* t)
 ;|�Visual LISP� Format Options�
(132 2 40 2 nil "end of " 100 2 1 1 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
