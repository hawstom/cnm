;;; Command aliases for selected HawsEDC commands.
;;; To change a command name to your liking,
;;; Just change the name after the "defun c:"
;;;
;;; eg. (defun c:PreferredAlias () (c:haws-LongName))
;;;
;;; Don't forget to copy this file to a safe place for safekeeping.
;;;
;;; An even better strategy is to copy only the commands you change
;;; to a USER.LSP file in the AutoCAD support files search path above
;;; the CNM application folder.
;;;
;;; Order of commands: 1. Section 2. Random
;;;
;;; Define as AutoCAD command.
;;; https://www.theswamp.org/index.php?topic=51409.msg565523#msg565523
;;Profile Drafter
(defun c:pro () (c:haws-pro))
(defun c:eop () (c:haws-eop))
(defun c:ellabel () (c:haws-ellabel))
(defun c:elv () (c:haws-elv))
(defun c:eop () (c:haws-eop))
(defun c:grb () (c:haws-grb))
(defun c:grc () (c:haws-grc))
(defun c:grd () (c:haws-grd))
(defun c:lst () (c:haws-lst))
(defun c:newpro () (c:haws-newpro))
(defun c:pc () (c:haws-pc))
(defun c:pldr () (c:haws-pldr))
(defun c:pm () (c:haws-pm))
(defun c:pred () (c:haws-pred))
(defun c:pro () (c:haws-pro))
(defun c:procb () (c:haws-procb))
(defun c:proe () (c:haws-proe))
(defun c:profc () (c:haws-profc))
(defun c:promh () (c:haws-promh))
(defun c:propipe () (c:haws-propipe))
(defun c:prosup () (c:haws-prosup))
(defun c:stalabel () (c:haws-stalabel))
(defun c:sel () (c:haws-sel))
(defun c:ser () (c:haws-ser))
;;;---------------Civil Drafting Section---------------
(defun c:2l () (c:haws-2l))
(defun c:berm () (c:haws-berm))
(defun c:ac () (c:haws-acres))
(defun c:cel () (c:haws-contelev))
(defun c:cv () (c:haws-contvol))
(defun c:contvol () (c:haws-contvol))
(defun c:curve () (c:haws-curve))
(defun c:cd () (c:haws-geodata))
(defun c:gd () (c:haws-geodata))
(defun c:dm () (c:haws-dm))
(defun c:dw () (c:haws-dw))
(defun c:m40 () (c:haws-m40))
(defun c:m42 () (c:haws-m42))
(defun c:mc2033 () (c:haws-mc2033))
(defun c:pipe () (c:haws-pipe))
(defun c:tap () (c:haws-tap))
(defun c:tapinv () (c:haws-tapinv))
(defun c:ut () (c:haws-ut))
(defun c:wall () (c:haws-wall))
(defun c:ws () (c:haws-ws))
(defun c:zero () (c:haws-zero))
;;;---------------Construction Notes Section---------------
(defun c:boxl () (c:haws-boxl))
(defun c:cirl () (c:haws-cirl))
(defun c:dial () (c:haws-dial))
(defun c:elll () (c:haws-elll))
(defun c:hexl () (c:haws-hexl))
(defun c:octl () (c:haws-octl))
(defun c:penl () (c:haws-penl))
(defun c:recl () (c:haws-recl))
(defun c:sstl () (c:haws-sstl))
(defun c:tril () (c:haws-tril))
(defun c:tcg () (c:haws-tcg))
(defun c:txtl () (c:haws-txtl))
(defun c:bw () (c:haws-bw))
(defun c:cnm () (c:hcnm-cnm))
(defun c:cnmop () (c:hcnm-cnmoptions))
(defun c:noteseditor () (c:hcnm-noteseditor))
(defun c:notesedit () (c:hcnm-notesedit))
(defun c:notesfilein () (c:haws-notesfilein))
(defun c:cnmmenu () (c:haws-cnmmenu))
(defun c:phaseedit () (c:haws-phaseedit))
(defun c:setnotephases () (c:haws-setnotephases))
(defun c:ntpurge () (c:haws-ntpurge))
(defun c:setnotesbubblestyle () (c:hcnm-setnotesbubblestyle))
;;;INSBLK sub-section
(defun c:GB      () (c:haws-GB)      )
(defun c:GC      () (c:haws-GC)      )
(defun c:INVL    () (c:haws-INVL)    )
(defun c:INVR    () (c:haws-INVR)    )
(defun c:LOTEL   () (c:haws-LOTEL)   )
(defun c:PAD     () (c:haws-PAD)     )
(defun c:REV     () (c:haws-REV)     ) ; From civil command group
(defun c:secb    () (c:haws-secb)    )
(defun C:secl    () (C:haws-secl)    )
(defun c:secr    () (c:haws-secr)    )
(defun c:sect    () (c:haws-sect)    )
(defun c:haws-SLL() (c:haws-SLL))
(defun c:SLOPE   () (c:haws-SLOPE)   )
(defun c:SLR     () (c:haws-SLR)     )
(defun c:SPOTEL  () (c:haws-SPOTEL)  )
(defun c:TC      () (c:haws-TC)      ) ; From profiles command group
(defun c:TCELEV  () (c:haws-TCELEV)  )
(defun c:TCL     () (c:haws-TCL)     )
(defun c:TCR     () (c:haws-TCR)     )
;;last two from the profiles and civil groups
;;;---------------Horizontal Control Section---------------
(defun c:bdl () (c:haws-bdl))
(defun c:bdp () (c:haws-bdp))
(defun c:ne () (c:haws-ne))
(defun c:stacl () (c:haws-stacl))
(defun c:xy () (c:haws-xy))
;;;---------------Dimensioning Section---------------
(defun c:d1 () (c:haws-d1))
(defun c:d2 () (c:haws-d2))
(defun c:dp () (c:haws-dp))
(defun c:du () (c:haws-du))
(defun c:ht () (c:haws-ht))
(defun c:te () (c:haws-te))
(defun c:xx () (c:haws-xx))
(defun c:aar () (c:haws-aar))
(defun c:chdim () (c:haws-chdim))
(defun c:dimsty () (c:haws-dimsty))
(defun c:dot () (c:haws-dot))
(defun c:loop () (c:haws-loop))
(defun c:none () (c:haws-none))
(defun c:tilde () (c:haws-tilde))
(defun c:ldr () (c:haws-ldr))
(defun c:led () (c:haws-led))
;;;---------------Text Section---------------
(defun c:add () (c:haws-add))
(defun c:at () (c:haws-at))
(defun c:chnum () (c:haws-chnum))
(defun c:chgcase () (c:haws-chgcase))
(defun c:cht () (c:haws-cht))
(defun c:cmt () (c:haws-cmt))
(defun c:contxt () (c:haws-contxt))
(defun c:cs () (c:haws-cs))
(defun c:ee () (c:haws-ee))
(defun c:facnum () (c:haws-facnum))
(defun c:imp_exp () (c:haws-imp_exp))
(defun c:incnum () (c:haws-incnum))
(defun c:l80 () (c:haws-l80))
(defun c:l100 () (c:haws-l100))
(defun c:l120 () (c:haws-l120))
(defun c:l140 () (c:haws-l140))
(defun c:l175 () (c:haws-l175))
(defun c:l200 () (c:haws-l200))
(defun c:l240 () (c:haws-l240))
(defun c:l290 () (c:haws-l290))
(defun c:l350 () (c:haws-l350))
(defun c:l500 () (c:haws-l500))
(defun c:letter () (c:haws-letter))
(defun c:lotnum () (c:haws-lotnum))
(defun c:num () (c:haws-num))
(defun c:presuf () (c:haws-presuf))
(defun c:romans () (c:haws-romans))
(defun c:round () (c:haws-round))
(defun c:selstyle () (c:haws-selstyle))
(defun c:th () (c:haws-th))
(defun c:to () (c:haws-to))
(defun c:tu () (c:haws-tu))
(defun c:txtsum () (c:haws-txtsum))
;;;---------------Layer Management Section---------------
(defun c:l0 () (c:haws-l0))
(defun c:lk0 () (c:haws-lk0))
(defun c:lka () (c:haws-lka))
(defun c:on () (c:haws-ona))
(defun c:ona () (alert (princ "\nONA has been changed to ON.")) (c:haws-ona))
(defun c:th () (c:haws-tha))
(defun c:tha () (alert (princ "\nTHA has been changed to TH.")) (c:haws-ona))
(defun c:ula () (c:haws-ula))
(defun c:ca () (c:haws-ca))
(defun c:chm () (princ "\nConsider using Y, YY, and YYY instead of AS, CL, and CHM.")(c:haws-chm))
(defun c:cl () (princ "\nConsider using Y, YY, and YYY instead of AS, CL, and CHM.")(c:haws-cl))
(defun c:ff () (c:haws-ff))
(defun c:fff () (princ "\nConsider using FFF instead of FFX.")(c:haws-ffx))
(defun c:ffx () (c:haws-ffx))
(defun c:off () (c:haws-off))
(defun c:offx () (c:haws-offx))
(defun c:uff () (c:haws-uff))
(defun c:uoff () (c:haws-uoff))
(defun c:uffx () (c:haws-uffx))
(defun c:uoffx () (c:haws-uoffx))
(defun c:ffi () (c:haws-ffi))
(defun c:lki () (c:haws-lki))
(defun c:ofi () (c:haws-ofi))
(defun c:lar () (c:haws-lar))
(defun c:las () (c:haws-las))
(defun c:lcp () (c:haws-lcp))
(defun c:lcpx () (c:haws-lcpx))
(defun c:ltp () (c:haws-ltp))
(defun c:ltpx () (c:haws-ltpx))
(defun c:lwp () (c:haws-lwp))
(defun c:lwpx () (c:haws-lwpx))
(defun c:lk () (c:haws-lk))
(defun c:laprn () (c:haws-laprn))
(defun c:ltb () (c:haws-ltb))
(defun c:ltc () (c:haws-ltc))
(defun c:lth () (c:haws-lth))
(defun c:lm () (c:haws-lm))
(defun c:lx () (c:haws-lx))
(defun c:lxx () (c:haws-lxx))
(defun c:lxxx () (c:haws-lxxx))
(defun c:oo () (c:haws-oo))
(defun c:offsetx () (c:haws-offsetx))
(defun c:ul () (c:haws-ul))
(defun c:yy () (c:haws-cl))
(defun c:yyy () (c:haws-chm))
;;;---------------Block Management Section---------------
(defun c:a2t () (c:haws-a2t))
(defun c:att2txt () (c:haws-att2txt))
(defun c:attredef () (c:haws-attredef))
(defun c:at () (c:haws-at))
(defun c:bl0 () (c:haws-bl0))
(defun c:w () (c:haws-w))
(defun c:xda () (c:haws-xda))
(defun c:xra () (c:haws-xra))
(defun c:xrl () (c:haws-xrl))
(defun c:chattrib () (c:haws-chattrib))
(defun c:incatt () (c:haws-incatt))
(defun c:in () (c:haws-xin))
(defun c:out () (c:haws-xout))
(defun c:xd () (c:haws-xd))
(defun c:xro () (c:haws-xro))
(defun c:xroffet () (c:haws-xroffet))
(defun c:xu () (c:haws-xu))
;;;---------------Inquiry Section---------------
(defun c:eg () (c:haws-eg))
(defun c:egn () (c:haws-egn))
(defun c:ac () (c:haws-acres))
(defun c:aee () (c:haws-aee))
(defun c:aet () (c:haws-aet))
(defun c:al () (c:haws-al))
(defun c:adl () (c:haws-adl))
(defun c:goto () (c:haws-goto))
(defun c:istan () (c:haws-istan))
(defun c:md () (c:haws-md))
(defun c:sf () (c:haws-sf))
(defun c:sm () (c:haws-sm))
(defun c:sy () (c:haws-sy))
(defun c:wl () (c:haws-wl))
;;;---------------Editing Section---------------
(defun c:bf () (c:haws-bf))
(defun c:cb () (c:haws-cb))
(defun c:ec () (c:haws-ec))
(defun c:ew () (c:haws-ew))
(defun c:mc () (c:haws-mc))
(defun c:mp () (c:haws-mp))
(defun c:mw () (c:haws-mw))
(defun c:r1 () (c:haws-r1))
(defun c:r2 () (c:haws-r2))
(defun c:r4 () (c:haws-r4))
(defun c:r9 () (c:haws-r9))
(defun c:s () (c:haws-s))
(defun c:ub () (c:haws-ub))
(defun c:um () (c:haws-um))
(defun c:vb () (c:haws-vb))
(defun c:bt () (c:haws-bt))
(defun c:bm () (c:haws-bm))
(defun c:brk () (c:haws-brk))
(defun c:ccc () (c:haws-clone))
(defun c:clone () (c:haws-clone))
(defun c:cr () (c:haws-cr))
(defun c:copyrot () (c:haws-copyrot))
(defun c:chcoord () (c:haws-chcoord))
(defun c:cw () (c:haws-cw))
(defun c:cmpro () (c:haws-cmpro))
(defun c:date () (c:haws-date))
(defun c:join () (princ "\nJoin has been replaced by PJL and PJ.  Issuing command JOIN.")(command "._join")(princ))
(defun c:le () (c:haws-lengthen))
(defun c:lll () (c:haws-lengthen))
(defun c:makepoly () (c:haws-makepoly))
(defun c:mf () (c:haws-mf))
(defun c:mof () (c:haws-mof))
(defun c:mscr () (c:haws-mscr))
(defun c:na () (c:haws-na))
(defun c:newscale () (c:haws-newscale))
(defun c:pj () (c:haws-pj))
(defun c:pjl () (c:haws-pjl))
(defun c:rescale () (c:haws-rescale))
(defun c:r () (c:haws-rotatebase))
(defun c:ssx () (c:haws-ssx))
(defun c:ssxpro () (c:haws-ssxpro))
(defun c:swap () (c:haws-swap))
;;;---------------Drawing Section---------------
(defun c:c2 () (c:haws-c2))
(defun c:ct () (c:haws-ct))
(defun c:p0 () (c:haws-p0))
(defun c:bx () (c:haws-bx))
;;;---------------Views and Zooms Section---------------
(defun c:2x () (c:haws-2x))
(defun c:5x () (c:haws-5x))
(defun c:9x () (c:haws-9x))
(defun c:tz () (c:haws-twz))
(defun c:x2 () (c:haws-x2))
(defun c:z0 () (c:haws-z0))
(defun c:za () (c:haws-za))
(defun c:ze () (c:haws-ze))
(defun c:zi () (c:haws-zi))
(defun c:zo () (c:haws-zo))
(defun c:zv () (c:haws-zv))
(defun c:zz () (c:haws-zz))
(defun c:zw () (c:haws-zw))
(defun c:tw () (c:haws-tw))
;;;---------------Setup and Drawing Environment Section---------------
(defun c:aa () (c:haws-aa))
(defun c:adt () (c:haws-adt))
(defun c:cet () (c:haws-cet))
(defun c:cmd () (c:haws-cmd))
(defun c:dia () (c:haws-dia))
(defun c:fdt () (c:haws-fdt))
(defun c:ib () (c:haws-ib))
(defun c:il () (c:haws-il))
(defun c:io () (c:haws-io))
(defun c:ir () (c:haws-ir))
(defun c:it () (c:haws-it))
(defun c:llt () (c:haws-llt))
(defun c:mn () (c:haws-mn))
(defun c:mvl () (c:haws-mvl))
(defun c:mvu () (c:haws-mvu))
(defun c:ose () (c:haws-ose))
(defun c:osi () (c:haws-osi))
(defun c:osm () (c:haws-osm))
(defun c:osn () (c:haws-osn))
(defun c:pslt () (c:haws-pslt))
(defun c:rga () (c:haws-rga))
(defun c:qt () (c:haws-qt))
(defun c:uf () (c:haws-uf))
(defun c:uf0 () (c:haws-uf0))
(defun c:uf1 () (c:haws-uf1))
(defun c:clean () (c:haws-clean))
(defun c:edcmenu () (c:haws-edcmenu))
(defun c:hawsedc () (c:haws-hawsedc))
(defun c:funky () (c:haws-funky))
(defun c:xin () (c:haws-xin))
(defun c:xout () (c:haws-xout))
(defun c:modestat () (c:haws-modestat))
(defun c:mren () (c:haws-mren))
(defun c:mv () (c:haws-mv))
(defun c:mvhp () (c:haws-mvhp))
(defun c:polarset () (c:haws-polarset))
(defun c:pall () (c:haws-pall))
(defun c:proto () (c:haws-proto))
(defun c:protox () (c:haws-protox))
(defun c:setup () (c:haws-setup))
(defun c:10 () (c:haws-10))
(defun c:12 () (c:haws-12))
(defun c:setdim10 () (c:haws-setdim10))
(defun c:setdim12 () (c:haws-setdim12))
(defun c:sheet () (c:haws-sheet))
(defun c:0 () (c:haws-0))
(defun c:1 () (c:haws-1))
(defun c:u0 () (c:haws-u0))
(defun c:u1 () (c:haws-u1))
(defun c:u2 () (c:haws-u2))
(defun c:u3 () (c:haws-u3))
(defun c:u8 () (c:haws-u8))
(defun c:us () (c:haws-us))
;;;---------------Miscellaneous Section---------------
(defun c:ffa () (c:haws-ffa))
(defun c:hawsalias () (c:haws-hawsalias))
(defun c:hae () (c:haws-aliasmanage))
(defun c:ham () (c:haws-aliasmanage))
(defun c:user () (c:haws-user))
(defun c:pgp () (c:haws-pgpedit))
(defun c:lr () (c:haws-loadandrun))
(defun c:run () (alert (princ "\nRUN has been replaced with LR (lisprun).")))
;;;---------------Old HawsEDC ACAD.PGP aliases
(defun
   haws-pgp-activate-aliases (/ activatepgpaliases is_custom)
  (setq ActivatePgpAliases (atoi(c:hcnm-config-getvar "HawsPgpLisp")))
  ;; Edit the line above
  (cond
    ((> activatepgpaliases 0)
     (foreach
        alias (haws-pgp-aliases)
       (setq is_custom (caddr alias))
       (cond
         ((or (> activatepgpaliases 1) is_custom)
          (eval
            (read
              (strcat
                "(defun c:"
                (car alias)
                "() (princ \""
                (strcat
                  "HawsAlias.lsp "
                  (cond
                    ((caddr alias) "custom")
                    (t "standard")
                  )
                  " PGP alias: "
                  (cadr alias)
                )
                "\")(command \"._"
                (cadr alias)
                "\")(princ))"
              )
            )
          )
         )
       )
     )
    )
  )
)

(defun
   haws-pgp-aliases ()
;;;  Alias Command  Is_custom
  '
   (("AP" "Aperture" t)
    ("AS" "LayMCur" t)
    ("B" "Break" t)
    ("BL" "Block" t)
    ("BX" "Rectang" t)
    ("C" "Copy" t)
    ("CC" "Circle" t)
    ("CF" "Chamfer" t)
    ("CH" "Change" t)
    ("DA" "DimAligned" t)
    ("DC" "DimCenter" t)
    ("DH" "DimHorizontal" t)
    ("DSC" "Dimscale" t)
    ("EX" "Explode" t)
    ("GS" "Gripsize" t)
    ("IB" "Insbase" t)
    ("MM" "Mirror" t)
    ("OFI" "Layiso" t)
    ("PB" "Pickbox" t)
    ("PG" "Polygon" t)
    ("RD" "Redo" t)
    ("RG" "Regenall" t)
    ("RR" "Redraw" t)
    ("T" "Trim" t)
    ("TH" "Laythw" t)
    ("TY" "Linetype" t)
    ("UU" "Ddunits" t)
    ("VP" "Viewports" t)
    ("X" "Extend" t)
    ("Y" "LayMCur" t)
    ("-I" "-Insert" nil)
    ("-W" "-Wblock" nil)
    ("-XR" "-Xref" nil)
    ("A" "Arc" nil)
    ("AR" "Array" nil)
    ("DAN" "DimAngular" nil)
    ("DI" "Distance" nil)
    ("DV" "Dview" nil)
    ("E" "Erase" nil)
    ("EL" "Ellipse" nil)
    ("F" "Fillet" nil)
    ("H" "Hatch" nil)
    ("I" "Insert" nil)
    ("L" "Line" nil)
    ("LI" "List" nil)
    ("LTS" "Ltscale" nil)
    ("M" "Move" nil)
    ("MS" "Mspace" nil)
    ("O" "Offset" nil)
    ("OS" "Ddosnap" nil)
    ("P" "Pan" nil)
    ("PE" "Pedit" nil)
    ("PL" "Pline" nil)
    ("PS" "Pspace" nil)
    ("SC" "Scale" nil)
    ("SN" "Snap" nil)
    ("V" "View" nil)
    ("W" "Wblock" nil)
    ("XR" "Xref" nil)
    ("Z" "Zoom" nil)
   )
)

(defun
   c:haws-aliasmanage (/ HawsPgpLispOptions input1)
  (initget "Yes No")
  (setq
    input1
     (getkword
       "\nLearn about managing the old HawsEDC PGP keyboard shortcuts? [Yes/No] <No>: "
     )
  )
  (cond
    ((= input1 "Yes")
     (textpage)
     (princ "\n;Standard AutoCAD aliases")
     (foreach
        alias (haws-pgp-aliases)
       (cond ((not (caddr alias))(princ (strcat "\n" (car alias) ",\t*" (cadr alias)))))
     )
     (princ "\n;Custom CNM/HawsEDC aliases")
     (foreach
        alias (haws-pgp-aliases)
       (cond ((caddr alias)(princ (strcat "\n" (car alias) ",\t*" (cadr alias)))))
     )
     (princ
       (strcat
         "\n==================================================================================="
         "\nCNM/HawsEDC is opinionated about command shortcuts (aliases)."
         "\nOnce upon a time, CNM/HawsEDC included command aliases\nin a version of ACAD.PGP."
         "\nThis is no longer true. But CNM still installs with its\nlegacy aliases included as LISP commands. It's recommended\nthat you paste the Custom CNM/HawsEDC Aliases shown above into\nyour ACAD.PGP and turn off the LISP aliases as described below."
          "\n\n1. Add to ACAD.PGP:\n\ta) Copy the desired aliases above."
         "\n\tb) Open your acad.pgp. (The CNM PGP command will open it.)\n\tThen paste the aliases to the bottom of the file."
         "\n\tc) Use the AutoCAD REINIT command to reload your acad.pgp file."
         "\n\n2. Choose which LISP aliases are active below (LISP aliases work in scripts;\nEXPLODE command acts differently):"
         "\n\ta) None; you will have none of the aliases shown above unless\n\tyou paste the above into your ACAD.PGP."
         "\n\tb) Custom; you will have the Custom aliases shown above as LISP commands\n\tthat will override your ACAD.PGP settings."
         "\n\tc) All: You will have all the aliases shown above as LISP commands\n\tthat will override your ACAD.PGP settings."
        )
     )
     (setq HawsPgpLispOptions '(("0" "None")("1" "Custom")("2" "All")))
     (initget "None Custom All")
     (setq
       input1
        (getkword
          (strcat
            "\nSpecify which LISP shortcuts to activate [None/Custom/All] <"
            (cadr
              (assoc
                (c:hcnm-config-getvar "HawsPgpLisp")
                hawspgplispoptions
              )
            )
            ">: "
          )
        )
     )
     (cond
       (input1
        (c:hcnm-config-setvar
          "HawsPgpLisp"
          (cadr
            (assoc
              input1
              (mapcar '(lambda (x) (reverse x)) hawspgplispoptions)
            )
          )
        )
       )
     )
     (getstring "\n<continue to edit HawsAlias.lsp aliases>: ")
    )
  )
  (STARTAPP
    (STRCAT "\"notepad\" \"" (findfile "hawsalias.lsp") "\"")
  )
  (ALERT
    (STRCAT
      "HawsAlias.lsp has been opened in Notepad for you to edit.\n\nClick OK to load CNM/HawsEDC aliases after editing and saving."
    )
  )
  (load "hawsalias")
  (princ)
)

;(haws-pgp-activate-aliases)

;;;---------------Set a flag that aliases have been loaded
(setq *HAWS-HAWSALIASLOADED* T)
;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 1 1 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
