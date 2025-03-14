;;;     CNMLOADER.LSP
;;;     Copyright (C) 2001 by Thomas Gail Haws
;;;
;;;
;;;     This file is loaded automatically following the menu CNM.

;;;
;;; This file no longer loads a HawsEDC pull-down.
;;; Instead, it simply loads command autoloaders for the CNM Plus tools submenu,
;;; which contains all the old HawsEDC commands.

(DEFUN
   HAWS-autoload (FILENAME COMMANDS / QFN)
  (SETQ
    QFN    (STRCAT "\"" FILENAME "\"")
  )
  (IF COMMANDS
    (MAPCAR
      '(LAMBDA (CMD / CMD FUNCTION-NAME)
         (SETQ FUNCTION-NAME (STRCAT "C:" CMD))
         (EVAL
           (READ
             (STRCAT "(defun " FUNCTION-NAME "() (PRINC \"" (STRCAT "\nAutoloading CNM command: " CMD ) "\") (c:HAWS-LOAD-FROM-APP-DIR " QFN ") (" FUNCTION-NAME "))")
           )
         )
       )
    COMMANDS
    )
    (EVAL
      (READ (STRCAT "(defun c:" (READ QFN) "()(haws-load" QFN "))"))
    )
  )
)


;The haws-autoload function loads a file
;when one of the listed commands are invoked.
;None of the files is loaded until a command calls it.

;;---------------Profiles Section---------------
(haws-autoload "eop"      '("haws-eop"))
(haws-autoload "profiles" '("haws-ellabel" "haws-elv" "haws-grb" "haws-grc" "haws-grd" "haws-lst" "haws-newpro"
                       "haws-pc" "haws-pldr" "haws-pm" "haws-pred" "haws-pro" "haws-procb" "haws-proe"
                       "haws-profc" "haws-promh" "haws-propipe" "haws-prosup" "haws-stalabel")
)
(haws-autoload "sideel"   '( "haws-sel" "haws-ser"))

;;---------------Civil Drafting Section---------------
(haws-autoload "2l"       '("haws-2l"))
(haws-autoload "berm"     '("haws-berm"))
(haws-autoload "contelev"  '("haws-contelev"))
(haws-autoload "contvol"  '("haws-contvol"))
(haws-autoload "curve"    '("haws-curve"))
(haws-autoload "geodata"  '("haws-geodata"))
(haws-autoload "stdim"    '("haws-dm"))
(haws-autoload "dw"       '("haws-dw"))
(haws-autoload "m40"      '("haws-m40"))
(haws-autoload "m42"      '("haws-m42"))
(haws-autoload "mc2033"   '("haws-mc2033"))
(haws-autoload "pipe"     '("haws-pipe"))
(haws-autoload "tap"      '("haws-tap"))
(haws-autoload "tapinv"   '("haws-tapinv"))
(haws-autoload "ut"       '("haws-ut"))
(haws-autoload "wall"     '("haws-wall"))
(haws-autoload "watserv"  '("haws-ws"))

;;---------------Construction Notes Section---------------
(haws-autoload "bw"       '("haws-bw"))
(haws-autoload "cnm"   '("haws-boxl" "haws-cirl" "haws-dial" "haws-elll"
                        "haws-hexl" "haws-octl" "haws-penl" "haws-recl"
                        "haws-sstl" "haws-tcg" "haws-tril" "haws-txtl"
	                      "hcnm-replace-bubble" "hcnm-edit-bubble"
                        "hcnm-cnmoptions"
                         "hcnm-cnm" "hcnm-cnmkt" "hcnm-cnmkti" "hcnm-cnmqt"
                        "hcnm-linkproj" "hcnm-changeprojnotes" "hcnm-noteseditor"
                        "hcnm-layereditor" "hcnm-cnmlayer"
                        "hcnm-attnoplot" "hcnm-attplot"
                        "hcnm-noteseditor" "hcnm-notesedit"
                        "haws-cnmmenu" "haws-cnmsetup"
                        "haws-phaseedit" "haws-setnotephases"
                        "haws-ntpurge" "hcnm-setnotesbubblestyle"
                        "hcnm-cnmoptions")
)
(haws-autoload "insblk"   '("haws-gb" "haws-gc" "haws-invl" "haws-invr" "haws-lotel" "haws-pad"
                            "haws-secb" "haws-secl" "haws-secr" "haws-sect"
                            "haws-sll" "haws-slope" "haws-spotel" "haws-slr"
                        "haws-tcelev" "haws-tcelevl" "haws-tcelevr"
                        "haws-rev" "haws-tc") ;;last two from the profiles and civil groups
)

;;---------------Horizontal Control Section---------------
(haws-autoload "bdl"      '("haws-bdl"))
(haws-autoload "bdp"      '("haws-bdp"))
(haws-autoload "ne"       '("haws-ne"))
(haws-autoload "stacl"    '("haws-stacl"))
(haws-autoload "xy"       '("haws-xy"))

;;---------------Dimensioning Section---------------
(haws-autoload "dms"     '("haws-d1" "haws-d2" "haws-dp" "haws-du" "haws-ht"
                       "haws-te" "haws-dimtixtoggle")
)
(haws-autoload "arcarrow" '("haws-aar"))
(haws-autoload "chdim"    '("haws-chdim"))
(haws-autoload "dimsty"   '("haws-dimsty"))
(haws-autoload "leader"   '("haws-dot" "haws-loop" "haws-none" "haws-tilde"))

;;---------------Text Section---------------
(haws-autoload "add"      '("haws-add"))
(haws-autoload "chnum"    '("haws-chnum"))
(haws-autoload "chtext"   '("haws-cht"))
(haws-autoload "cmt"      '("haws-cmt"))
(haws-autoload "contxt"   '("haws-contxt"))
(haws-autoload "cs"       '("haws-cs"))
(haws-autoload "editall"  '("haws-ee"))
(haws-autoload "facnum"   '("haws-facnum"))
(haws-autoload "imp_exp"  '("haws-imp_exp"))
(haws-autoload "incnum"   '("haws-incnum"))
(haws-autoload "qdtext"   '("haws-l80" "haws-l100" "haws-l120" "haws-l140" "haws-l175" "haws-l200" "haws-l240" "haws-l290"
                        "haws-l350" "haws-l500")
)
(haws-autoload "letter"   '("haws-letter"))
(haws-autoload "lotnum"   '("haws-lotnum"))
(haws-autoload "num"      '("haws-num"))
(haws-autoload "presuf"   '("haws-presuf"))
(haws-autoload "romans"   '("haws-romans"))
(haws-autoload "round"    '("haws-round"))
(haws-autoload "selstyle" '("haws-selstyle"))
(haws-autoload "textsize" '("haws-th"))
(haws-autoload "to"       '("haws-to"))
(haws-autoload "tu"       '("haws-tu"))
(haws-autoload "txtsum"   '("haws-txtsum"))

;;---------------Layer Management Section---------------
(haws-autoload "lam"      '("haws-l0" "haws-lk0" "haws-lka" "haws-lki" "haws-ofi" "haws-ula"))
(haws-autoload "chm"      '("haws-chm"));Changes properties, where laymch does not.
(haws-autoload "cl"       '("haws-cl"));Changes properties, where laycur does not.
(haws-autoload "lastat"   '("haws-ff" "haws-ffx" "haws-lk" "haws-off" "haws-offx" "haws-uff" "haws-uoff" "haws-uffx" "haws-uoffx"))
(haws-autoload "isolat"   '("haws-ffi"))
(haws-autoload "layersav" '("haws-lar" "haws-las"))
(haws-autoload "lcp"      '("haws-lcp" "haws-lcpx"))
(haws-autoload "lwp"      '("haws-lwp" "haws-lwpx"))
(haws-autoload "layrprn"  '("haws-laprn"))
(haws-autoload "ltchang"  '("haws-ltb" "haws-ltc" "haws-lth"))
(haws-autoload "ltp"      '("haws-ltp" "haws-ltpx"))
(haws-autoload "lx"       '("haws-lx" "haws-lxx"))
(haws-autoload "offsetx"  '("haws-oo" "haws-offsetx"))

;;---------------Block Management Section---------------
(haws-autoload "att2txt"  '("haws-a2t" "haws-att2txt"))
(haws-autoload "bl0"      '("haws-bl0"))
(haws-autoload "blm"      '("haws-w" "haws-xda" "haws-xra"))
(haws-autoload "ca"       '("haws-ca"))
(haws-autoload "chattrib" '("haws-chattrib"))
(haws-autoload "incatt"   '("haws-incatt"))
(haws-autoload "inout"    '("haws-xin" "haws-xout"))
(haws-autoload "xd"       '("haws-xd"))
(haws-autoload "xroffset" '("haws-xro" "haws-xroffet"))
(haws-autoload "xu"       '("haws-xu"))

;;---------------Inquiry Section---------------
(haws-autoload "inq"      '("haws-eg" "haws-egn"))
(haws-autoload "aee"      '("haws-aee"))
(haws-autoload "haws-qt"  '("haws-acres" "haws-aet" "haws-sf" "haws-sm" "haws-sy" "haws-adl" ))
(haws-autoload "goto"     '("haws-goto"))
(haws-autoload "istan"    '("haws-istan"))
(haws-autoload "cumdist"  '("haws-md"))
(haws-autoload "wl"       '("haws-wl"))

;;---------------Editing Section---------------
(haws-autoload "edt"      '("haws-bf" "haws-cb" "haws-copy" "haws-mp" "haws-pj" "haws-r1" "haws-r2" "haws-r4" "haws-r9" "haws-s" "haws-ub" "haws-um" "haws-vb"))
(haws-autoload "brkmat"   '("haws-bm"))
(haws-autoload "brk"      '("haws-brk"))
(haws-autoload "copyrot"  '("haws-copyrot" "haws-copyrotdrag"))
(haws-autoload "chcoord"  '("haws-chcoord"))
(haws-autoload "cmpro"    '("haws-cmpro"))
(haws-autoload "lengthen"  '("haws-lengthen"))
(haws-autoload "mfillet"  '("haws-mf"))
(haws-autoload "moffset"  '("haws-mof"))
(haws-autoload "mscript"  '("haws-mscr"))
(haws-autoload "newang"   '("haws-na"))
(haws-autoload "newscale" '("haws-newscale"))
(haws-autoload "pjl"      '("haws-pjl"))
(haws-autoload "rescale"  '("haws-rescale"))
(haws-autoload "rotatebase"'("haws-rotatebase"))
(haws-autoload "ssx"      '("haws-ssx"))
(haws-autoload "ssxpro"   '("haws-ssxpro"))
(haws-autoload "swap"     '("haws-swap"))

;;---------------Drawing Section---------------
(haws-autoload "dwg"      '("haws-c2" "haws-ct" "haws-p0"))
(haws-autoload "box"      '("haws-bx"))

;;---------------Views and Zooms Section---------------
(haws-autoload "vz"       '("haws-2x" "haws-5x" "haws-9x" "haws-twz" "haws-x2" "haws-z0" "haws-za" "haws-ze" "haws-zi" "haws-zo" "haws-zv"
                        "haws-zz" "haws-zw")
)
(haws-autoload "tw"       '("haws-tw"))

;;---------------Setup and Drawing Environment Section---------------
(haws-autoload "sde"      '("haws-0" "haws-1" "haws-aa" "haws-adt" "haws-cet" "haws-cmd" "haws-dia" 
                            "haws-fdt" "haws-ib" "haws-il" "haws-io" "haws-ir" "haws-it"
                            "haws-llt" "haws-mbt" "haws-mvl" "haws-mvu" "haws-ose" "haws-osi" "haws-osm" 
                            "haws-osn" "haws-proto" "haws-protox" "haws-pslt" "haws-rga"
                            "haws-qt" "haws-uf" "haws-uf0" "haws-uf1")
)
(haws-autoload "clean"    '("haws-clean"))
(haws-autoload "funky"    '("haws-funky"))
(haws-autoload "modestat" '("haws-modestat"))
(haws-autoload "mrename"  '("haws-mren"))
(haws-autoload "mv"       '("haws-mv"))
(haws-autoload "polarset" '("haws-polarset"))
(haws-autoload "purge"    '("haws-pall"))
(haws-autoload "setup"    '("haws-setup" "haws-10" "haws-12" "haws-setdim10" "haws-setdim12"))
(haws-autoload "sheet"    '("haws-sheet"))
(haws-autoload "unitset"  '("haws-u0" "haws-u1" "haws-u2" "haws-u3" "haws-u8" "haws-us"))
(defun ah () (alert (princ "\nAH has been retired.  Please use the built in m2p osnap.")))

;;---------------Miscellaneous Section---------------
(haws-autoload "misc"     '("haws-ffa" "haws-pgpedit" "haws-user"))
(haws-autoload "loadandrun"       '("haws-LoadAndRun"))
;-------------MODIFY THE ABOVE--------------------------------------
;;; Load legacy library
;; LISPUTIL.LSP has library functions for legacy routines some legacy users have.
(if (not haws-errdef) (load "lisputil"))
;;; Load Haws and CNM libraries
(setq c:hcnm-cnm nil)
(load "edclib")
(if (not c:hcnm-cnm) (load "cnm"))
(if (not LM:ISANNOTATIVE) (load "lee-mac"))

;;;Load aliases
;;;CNMALIAS.LSP has short names for all the commands.
(COND
  ((not *HCNM-CNMALIASLOADED*)
   (IF(= (LOAD "cnmalias.lsp" "failed") "failed")(ALERT "Couldn't find cnmalias.lsp command aliases."))
  )
  (T (PRINC "\nSkipping cnmalias.lsp command aliases.  Already loaded."))
)
;;;The following line loads user.lsp if found.
(if (setq temp(findfile "user.lsp"))(load temp))
;;;You can put personally preferred routines in user.lsp.
;;;It is suggested that you keep a user.lsp in a reserved user support files folder
;;;added to AutoCAD's Support Files Search Path.
;;;Keep user.lsp out of the program folder or it may be deleted.

;;; Place the CNM pulldown to the left of the last pulldown already loaded
;;;     Created 2/21/97 by Dominic Panholzer

(defun hcnm-PlaceCNMMenu (/ CNT)
  (setq CNT 1)
  (while (< CNT 24)
    (if (menucmd (strcat "P" (itoa CNT) ".1=?"))
      (setq CNT (1+ CNT))
      (progn
        (if (> CNT 2)
          (setq CNT (1- CNT))
          (setq CNT 2)
        )
        (menucmd (strcat "p" (itoa CNT) "=+CNM.pop1"))
        (setq CNT 25)
      )
    )
  )
)

(if (not(c:haws-icad-p))(hcnm-PlaceCNMMenu)) ;Do it if not in icad
(princ "\nConstruction Notes Manager menu utilities loaded.")
(princ)