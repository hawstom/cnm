;;; INOUT.LSP
;;; (C) Copyright 2017 by Thomas Gail Haws
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;

;;; XRFIND
;;; Finds and returns list of xrefs to act on
;;;
;;; You can change this function to return the list of xrefs you want to see
;;;
;;; Arguments:
;;; DETACHP T/nil flag T if in detach mode instead of attach mode.  Else
(DEFUN
   HAWS-XRFIND (DETACH / DN DN-FIELD-DELIMITER-WC DN-PROJECT FN LIST-FILE-NAME STANDARD-LIST XREFI BLOCKI)
  (SETQ
    ;; The drawing name without path or extension
    DN
     (HAWS-GETDN)
;;; ============================================================================
;;;
;;; Begin custom company standards section.  Edit the desired behavior below.
;;;
;;; ============================================================================
    ;; Field delimiter to use for finding Project part of drawing name if that scheme is used.
    DN-FIELD-DELIMITER-WC
     "-"
    ;; The project portion of the drawing name for use in building xref names.
    ;;
    ;; Inactive sample: (SUBSTR DN 1 4) ; First four characters of DN
    ;; Inactive sample: (APPLY 'STRCAT (MAPCAR '(LAMBDA (X) (STRCAT X DN-FIELD-DELIMITER-WC))(REVERSE(CDR(REVERSE (HAWS-STRTOLST DN DN-FIELD-DELIMITER-WC nil T)))))) ; Everything but part after last hyphen.
    ;; Inactive sample: (APPLY 'STRCAT (MAPCAR '(LAMBDA (X) (STRCAT X DN-FIELD-DELIMITER-WC))(REVERSE(CDR(CDR(REVERSE (HAWS-STRTOLST DN DN-FIELD-DELIMITER-WC nil T))))))) ; Everything but part after 2nd to last hyphen.
    DN-PROJECT
     (SUBSTR DN 1 (- (STRLEN DN) 4)) ; Everything but last four characters in drawing name.
    ;; Xref list file name
    ;;
    ;; (Xrefs in list file cannot have the .dwg extension)
    ;; Inactive sample: "inout.txt" ; Just plain INOUT.TXT
    ;; Inactive sample: (STRCAT (SUBSTR DN 1 6) "xr.lst") ; First 6 characters of DN plus XR.LST
    LIST-FILE-NAME
     (STRCAT DN-PROJECT "xr.lst")  ; Project plus xr.lst
    ;; List of xrefs to always look for and put in attach list.
    STANDARD-LIST
          (LIST
            (STRCAT DN-PROJECT "MAS")
            (STRCAT DN-PROJECT "BASE")
            (STRCAT DN-PROJECT "OB00")
            (STRCAT DN-PROJECT "TOPO")
            (STRCAT DN-PROJECT "TS00")
            (STRCAT DN-PROJECT "TS01")
            (STRCAT DN-PROJECT "TB00")
            (STRCAT DN-PROJECT "CALC")
            (STRCAT DN-PROJECT "BDR")
            (STRCAT DN-PROJECT "BDR2")
            (STRCAT DN-PROJECT "PPNT")
          )
;;; ============================================================================
;;;
;;; End custom company standards section.  Edit the desired behavior below.
;;;
;;; ============================================================================
    I      0
    J      0
    XRLIST NIL
  )
  (COND
    ;; If in detach mode, just show all the xrefs in the drawing.
    (DETACH
     (WHILE (SETQ BLOCKI (TBLNEXT "BLOCK" (NOT BLOCKI)))
       (IF (= 4 (LOGAND 4 (CDR (ASSOC 70 BLOCKI))))
         (SETQ XRLIST (CONS (CDR (ASSOC 2 BLOCKI)) XRLIST))
       )
     )
     (IF (NOT DDINOUT)
       (LOAD "ddinout")
     )
     (SETQ XRLIST (DDINOUT "Xrefs to detach" XRLIST T))
    )
    ;; Else if a list of xrefs is found, show the xrefs in it if they are found.
    ((SETQ FN (FINDFILE LIST-FILE-NAME))
     (SETQ F1 (OPEN FN "r"))            ;If found,
     (WHILE (SETQ XREFI (READ-LINE F1)) ;read xrefs.
       (IF (FINDFILE (STRCAT XREFI ".dwg"))
         (SETQ XRLIST (CONS XREFI XRLIST))
       )
     )
     (SETQ F1 (CLOSE F1))
     (IF (NOT DDINOUT)
       (LOAD "ddinout")
     )
     (SETQ XRLIST (DDINOUT (STRCAT "Xrefs in " FN) XRLIST NIL))
    )
    ;; Else look for the company standard xrefs and show them.
    (T
     (FOREACH
        XREFI STANDARD-LIST
       (IF (FINDFILE (STRCAT XREFI ".dwg"))
         (SETQ XRLIST (CONS XREFI XRLIST))
       )
     )
     (IF (NOT DDINOUT)
       (LOAD "ddinout")
     )
     (SETQ XRLIST (DDINOUT "Xrefs to attach" XRLIST NIL))
    )
  )
)

(defun c:haws-xin ( / i j xr XRLIST)
(haws-core-init 45)
  (haws-errdef 0)
  (HAWS-VSAVE'("dragmode""clayer""ucsfollow"))
  (setvar "ucsfollow" 0)
  (setvar "dragmode" 0)
  (SETQ XRLIST (HAWS-xrfind nil))
  (setq ucsp T)
  (command "layer" "s" "0" "" "ucs" "w")
  (FOREACH XR XRLIST
    (cond
      ( (findfile(strcat xr ".dwg"))
        (princ xr)(princ " ")
        (command "xref" "a" xr "0,0" "" "" "")
  ) ) )
  (command "ucs" "previous")
  (HAWS-VRSTOR)(HAWS-ERRRST)(princ)
)

(defun c:haws-xout ( / xr xrlist)
(haws-core-init 46)
  (haws-errdef 0)
  (setq xrlist (HAWS-xrfind T))
  (foreach xr xrlist
    (princ xr)
    (princ " ")
    (command "xref" "d" xr)
  )
  (command "view" "r" "plan" "ucs" "r" "plan")
  (HAWS-ERRRST)
  (princ)
)
