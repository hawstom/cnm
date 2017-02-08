;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-xin ( / i j xr XRLIST)
  (HAWS-ERDF$@ 0)
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
  (HAWS-ERDF$@ 0)
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

;;Returns list of xrefs to act on
(DEFUN
   HAWS-XRFIND (DETACH / DN FN XREFI BLOCKI)
  (SETQ
    DN     (HAWS-GETDN)
    I      0
    J      0
    XRLIST NIL
  )
  (COND
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
    ((OR                                ;If attaching,
       (SETQ FN (FINDFILE (STRCAT (SUBSTR DN 1 6) "xr.lst")))
                                        ;look for an xref
       (SETQ FN (FINDFILE (STRCAT (SUBSTR DN 1 4) "xr.lst")))
                                        ;list file.
     )
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
    (T                                  ;If no xref list
     (FOREACH
        X                               ;file found,
          (LIST                         ;try attaching
            (STRCAT (SUBSTR DN 1 4) "MAS") ;some common
            (STRCAT (SUBSTR DN 1 4) "BASE") ;xref names.
            (STRCAT (SUBSTR DN 1 4) "OB00")
            (STRCAT (SUBSTR DN 1 4) "TOPO")
            (STRCAT (SUBSTR DN 1 4) "TS00")
            (STRCAT (SUBSTR DN 1 4) "TS01")
            (STRCAT (SUBSTR DN 1 4) "TB00")
            (STRCAT (SUBSTR DN 1 4) "CALC")
            (STRCAT (SUBSTR DN 1 4) "BDR")
            (STRCAT (SUBSTR DN 1 4) "BDR2")
            (STRCAT (SUBSTR DN 1 4) "PPNT")
          )
       (IF (FINDFILE (STRCAT X ".dwg"))
         (SETQ XRLIST (CONS X XRLIST))
       )
     )
     (IF (NOT DDINOUT)
       (LOAD "ddinout")
     )
     (SETQ XRLIST (DDINOUT "Xrefs to attach" XRLIST NIL))
    )
  )
)
