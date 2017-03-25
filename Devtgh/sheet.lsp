;SHEET.LSP
;(C) Copyright 1997 by Thomas Gail Haws
;This routine sets up a plan and profile sheet by attaching the
;required xrefs in model space and paper space and creating a
;paper space viewport.
;
;For the routine to work, either you must be prepared to select
;the center of the plan view area in model space and two points
;that define the rotation of the sheet in model space, or there
;must already be a block inserted in model space at the center
;of the plan view area and rotated the way the sheet would fit on the model.
;The block must have an attribute with the tag "SHTNO" and
;a value equal to the sheet number.
;
(if (not HAWS-attfind)(load"attfind"))
(defun c:haws-sheet
  ( / basei blname dn insscl plnblk ptmcen ptmvc1 ptmvc2
    shtnm ssblk xrname xrpath BASEI BLNAME CONT CONT1 CONT2 DN INSSCL OPT PLNBLK PTINS PTMCEN PTMVC1 PTMVC2 PTPCEN SHTANG SHTNM SHTNO SHTTP SS1 SSBLK TOTINC UCSP XRNAME XRPATH
  )
  (prompt "\nSHEET 1.1 by Thomas Gail Haws")
  ;Create ucs
  (defun HAWS-shtucs ()
    (prompt "\nCreating UCS PLAN based on plan view block for this drawing")
    (setvar "tilemode" 1)
    (command "._ucs" "w")
    (setq ucsp T)
    (cond
      ;Look for plan view block for this drawing.
      ( (setq plnblk (caar(HAWS-attfind "*" (list(list"SHTNO" shtnm))'("SHTNO") T)))
        (command "ucs" "d" "plan" "ucs" "e" plnblk "ucs" "s" "plan" "ucs" "p")
        (initget "Yes No")
        (cond
          ( (= "Yes" (getkword "\nErase other plan view insertions \"SHT*\"?<Yes/No>: "))
            (setq ssblk (ssget "X" '((2 . "SHT*"))))
            (command "._erase" ssblk "r" plnblk "" "ucs" "w")
          )
        )
      )
      ;If not found, give error message.
      ( T (prompt "\nNo plan view block for this drawing found."))
    )
    (setq ucsp nil)
    (command "ucs" "p" "tilemode" 0 "pspace")
  )
  ;Create mview
  (defun HAWS-shtmv ()
    (HAWS-MKLAYR "SHTMVW")
    (setq
      ptmvc1(getpoint "\nFirst corner for centered paper space viewport: ")
      ptmvc2(getcorner ptmvc1 "Other corner: ")
      ptpcen
      (list
        (/ (+ (car ptmvc1) (car ptmvc2)) 2)
        (/ (+ (cadr ptmvc1)(cadr ptmvc2)) 2)
        0.0
      )
    )
    (command "._mview" ptmvc1 ptmvc2)
    (prompt "\nMview created on defpoints layer.  Will not plot.  Leave thawed.")
    (command "._mspace" "._ucs" "w")
    (setq ucsp T)
    (cond
      ;Look for plan view block for this drawing.
      ( (setq plnblk (caar(HAWS-attfind "*" (list(list"SHTNO" shtnm))'("SHTNO") T)))
        (command
          "ucs" "d" "plan" "ucs" "e" plnblk "ucs" "s" "plan" "plan" ""
          "zoom" "c" "0,0" (strcat"1/"(rtos (HAWS-DWGSCALE) 2 0)"xp")
          "view""s""plan"
          "ucs" "p"
        )
      )
      ;If not found, prompt for view points.
      ( T
        (initget 1 "Yes No")
        (if (= (getkword "\nZoom extents in model space?<Yes/No>: ") "Yes")
          (command "._zoom" "e")
        )
        (setq ptmcen (getpoint "\nCenter of sheet: "))
        (setq shtang (getangle "\nSheet rotation: "))
        (command
          "ucs" "d" "plan" "ucs" "o" ptmcen "ucs" "z" shtang "ucs" "s" "plan" "plan" ""
          "zoom" "c" "0,0" (strcat"1/"(rtos (HAWS-DWGSCALE) 2 0)"xp")
          "view""s""plan"
          "ucs" "p" "ucs" "p"
        )
      )
    )
    ;Attach model space xrefs
    (setvar "clayer" "0")
    (while (/= ""(setq basei(getstring "\nModel space base drawing to attach: ")))
      (cond
        ( (tblsearch "BLOCK" basei)
          (prompt (strcat "  " basei " is already a block in drawing."))
        )
        ( (not(setq basei(findfile (strcat basei ".dwg"))))
          (prompt"  Xref not found.")
        )
        ( T (command "._xref" "a" basei "0,0" 1 1 0))
      )
    )
    (setq
      ssblk (ssget "X" '((2 . "SHT*")))
      ptins (trans(cdr(assoc 10 (entget plnblk))) plnblk 0)
    )
    (command "._erase" ssblk "r" plnblk "" "ucs" "w")
    (setvar "insbase" ptins)
    (setq ucsp nil)
    (command "ucs" "p" "pspace")
  )
  ;Attach plan view in model space
  (defun HAWS-shtpnm ()
    (if
      (= 1 (sslength(ssget "X" '((0 . "VIEWPORT")))))
      (prompt "\nUse the Mview and bases option to make a viewport first.")
      (progn
        (command "._mspace")
        (setq
          xrname (strcat(substr dn 1 (- 9 (strlen shtnm)))"n"(substr shtnm 3))
          insscl 1.0
        )
        (if
          (findfile (strcat xrname ".dwg"))
          (command "._xref" "d" xrname "._xref" "a" (findfile (strcat xrname ".dwg")) "0,0" insscl "" 0)
          (prompt (strcat"\nXref "xrname" not found."))
        )
        (command "._pspace")
      )
    )
  )
  ;Attach plan view in paper space
  (defun HAWS-shtpnp ()
    (setq
      xrname (strcat(substr dn 1 (- 9 (strlen shtnm)))"n"(substr shtnm 3))
      insscl (/ 1.0 (HAWS-DWGSCALE))
    )
    (if
      (and ptpcen shtang (findfile (strcat xrname ".dwg")))
      (command "._xref" "d" xrname "._xref" "a" (findfile (strcat xrname ".dwg")) ptpcen insscl "" (* shtang -1))
      (prompt (strcat"\nXref "xrname" not found or need to make mview first."))
    )
  )
  ;Attach profile view
  (defun HAWS-shtpr ( )
    (setq
      xrname (strcat(substr dn 1 (- 9 (strlen shtnm)))"r"(substr shtnm 3))
      xrpath xrname
      insscl (/ 1.0 (HAWS-DWGSCALE))
    )
    (while
      (not(findfile (strcat xrpath ".dwg")))
      (progn
        (prompt (strcat"\nXref "xrpath" not found."))
        (setq xrpath (strcat (getstring 1 "\nDirectory path for profile xref: ") xrname))
      )
    )
    (command "._xref" "d" xrname "._xref" "a" (findfile (strcat xrpath ".dwg")) "0,0" insscl "" 0)
  )
  ;Attach quantities
  (defun HAWS-shtpq ()
    (setq
      xrname (strcat(substr dn 1 (- 9 (strlen shtnm)))"q"(substr shtnm 3))
      insscl 1
    )
    (if
      (findfile (strcat xrname ".dwg"))
      (command "._xref" "d" xrname "._xref" "a" (findfile (strcat xrname ".dwg")) "0,0" insscl "" 0)
      (prompt (strcat"\nXref "xrname" not found."))
    )
  )
  ;Insert bdrtxt
  (defun HAWS-shtbt ()
    (setq
      blname (getstring "\nBorder text block name: ")
      insscl 1
    )
    (if (setq ss1(ssget "X" (list (cons 2 blname))))(command "._erase" ss1 ""))
    (cond
      ( (findfile (strcat blname ".dwg"))
        (setq
          cont  (getstring 1 "\nSingle line content: ")
          cont1 (getstring 1 "\nTwo line content 1: ")
          cont2 (getstring 1 "\nTwo line content 2: ")
          totinc (getint "\nAmount to add to sub-sheet number for sheet number: ")
        )
        (setvar "attreq" 0)
        (setvar "regenmode" 0)
        (command "._insert" (strcat blname "=") "0,0" insscl "" 0)
        (if
          (HAWS-attfind "blname" (list(list"CONTENT" "XX"))'("CONTENT") T)
          (command "._attedit" "n" "n" blname "content" "XX" "XX" cont)
        )
        (command
          "._attedit" "n" "n" blname "content1" "XX" "XX" cont1
          "._attedit" "n" "n" blname "content2" "XX" "XX" cont2
          "._attedit" "n" "n" blname "filename" "XX" "XX" dn
          "._attedit" "n" "n" blname "subshtno" "XX" "XX" shtnm
          "._attedit" "n" "n" blname "shtno" "XX" "XX" (itoa (+ totinc (atoi shtno)))
        )
        (setvar "attreq" 1)
      )
    )
  )
  (haws-borrow 0)
  (command "._undo" "g")
  (HAWS-VSAVE '("aunits""clayer""ucsfollow""attdia" "regenmode"))
  (setvar "ucsfollow" 0)
  (setvar "aunits" 3)
  (setvar "attdia" 0)
  (setq
    dn (strcase (HAWS-GETDN))
    shtpre (GETINT "\nNumber of characters in drawing name before sheet name: ")
    shttp (strcase(HAWS-GETSTRINGX "\nSheet type" nil (substr dn (1+ shtpre) 2)))
    shtno (strcase(HAWS-GETSTRINGX "\nThis sheet no." nil (substr dn (+ shtpre 1 (strlen shttp)) 7)))
    shtnm (strcat shttp shtno)
  )
  (setvar "tilemode" 0)(command "pspace")
  (while
    (progn
      (initget "Ucs Mview MSPlan PSPlan PRofile Quantity Border")
      (setq opt (getkword "\nUcs only/Mview and bases/MSPlan xref/PSPlan xref/PRofile xref/Quantity xref/Border text: "))
    )
    (cond
      ( (= opt "Ucs")(HAWS-shtucs))
      ( (= opt "Mview")(HAWS-shtmv))
      ( (= opt "MSPlan")(HAWS-shtpnm))
      ( (= opt "PSPlan")(HAWS-shtpnp))
      ( (= opt "PRofile")(HAWS-shtpr))
      ( (= opt "Quantity")(HAWS-shtpq))
      ( (= opt "Border")(HAWS-shtbt))
    )
  )
  (command "._undo" "e")
  (HAWS-VRSTOR)(haws-return)(princ)
)
