;;;(C) Copyright 2017 by Thomas Gail Haws
;;; This AutoLISP program lists a nested entity inside a XREF, BLOCK, or PLINE
(if (not c:hcnm-config-getvar)(c:haws-load-from-app-dir "cnm"))

(DEFUN C:HAWS-LX ()
(haws-core-init 255) (HAWS-XLIST 0))
(DEFUN C:HAWS-LXX ()
(haws-core-init 256) (HAWS-XLIST-MULTI))

(DEFUN
   HAWS-XLIST (EDIT-MODE / NENTSEL-RESULTS)
  (SETQ NENTSEL-RESULTS (HAWS-XLIST-NENTSEL (NENTSEL)))
  (HAWS-XLIST-LIST)
  (COND
    ((> EDIT-MODE 0)
     (HAWS-XLIST-SHOW-LAYERS
       (HAWS-XLIST-LIST-TO-STRING (CADR NENTSEL-RESULTS) EDIT-MODE)
     )
    )
  )
  (PRINC)
)

(DEFUN
   HAWS-XLIST-MULTI (/ CONTINUE-P ENAMELIST INPUT-RESULTS LAYERLIST SS-P)
  (SETQ ENAMELIST '())
  (WHILE (NOT CONTINUE-P)
    (SETQ
      INPUT-RESULTS
       (COND
         (SS-P (HAWS-XLIST-MULTI-SSGET))
         (T (HAWS-XLIST-MULTI-NENTSEL))
       )
       ENAMELIST
       (APPEND ENAMELIST (CAR INPUT-RESULTS))
      SS-P
       (CADR INPUT-RESULTS)
      CONTINUE-P
       (CADDR INPUT-RESULTS)
    )
  )
  (HAWS-XLIST-SHOW-LAYERS
    (HAWS-XLIST-LIST-TO-STRING ENAMELIST 2)
  )
)

(DEFUN
   HAWS-XLIST-MULTI-NENTSEL (/ CONTINUE-P ENAMELIST INPUT1 SS-P NENTSEL-RESULTS)
  (INITGET "Selection List Continue")
  (SETQ
    INPUT1
     (NENTSEL
       (STRCAT
         "\nSelect object or [Selection set/List mode ("
         (c:hcnm-config-getvar "LXXListMode")
         ")/Continue] <Continue>: "
       )
     )
    CONTINUE-P
     (NOT INPUT1)
    SS-P
     (= INPUT1 "Selection")
    NENTSEL-RESULTS
     (HAWS-XLIST-NENTSEL INPUT1)
    ENAMELIST
     (CADR NENTSEL-RESULTS)
  )
  (COND ((= INPUT1 "List") (HAWS-XLIST-TOGGLE-LIST-MODE)))
  (COND ((= (c:hcnm-config-getvar "LXXListMode") "yes") (HAWS-XLIST-LIST)))
  (LIST ENAMELIST SS-P CONTINUE-P)
)

(DEFUN HAWS-XLIST-LIST ()
   (COND ((/= (CAR NENTSEL-RESULTS) "") (ALERT (PRINC (CAR NENTSEL-RESULTS)))))
)

(DEFUN
   HAWS-XLIST-TOGGLE-LIST-MODE ()
  (c:hcnm-config-setvar
    "LXXListMode"
    (COND
      ((= (c:hcnm-config-getvar "LXXListMode") "yes") "no")
      (T "yes")
    )
  )
)

(DEFUN
   HAWS-XLIST-MULTI-SSGET (/ CONTINUE-P ENAMELIST INPUT1 SS-P)
  (SETQ
    INPUT1
     (SSGET)
    CONTINUE-P
     (NOT INPUT1)
    SS-P
     NIL
    ENAMELIST
    (CADR (HAWS-XLIST-SS-TO-LIST INPUT1))
  )
  (LIST ENAMELIST SS-P CONTINUE-P)
)

(DEFUN
   HAWS-XLIST-SS-TO-LIST (INPUT1 / EN I SS1 SSLIST)
  (COND
    ((AND INPUT1 (= (TYPE INPUT1) 'PICKSET))
     (SETQ
       SS1 INPUT1
       I   -1
     )
     (WHILE (SETQ EN (SSNAME SS1 (SETQ I (1+ I))))
       (SETQ SSLIST (CONS EN SSLIST))
     )
     ;; Used only for editing, not reporting, so return empty string
     (LIST "" SSLIST)
    )
  )
)

(DEFUN
   HAWS-XLIST-NENTSEL (INPUT1 / ALRTSTR ALRTSTRI EL ES ECLR I LACLR LAI
                       SHELLS ETYPE SHELLI SHELLL PREVI TEMP
                      )
  (COND
    ((AND INPUT1 (= (TYPE INPUT1) 'LIST))
     (SETQ
       ES INPUT1
       SHELLS
        (CADDDR ES)
       EL (ENTGET (CAR ES))
       ETYPE
        (HAWS-DXF 0 EL)
       ECLR
        (IF (HAWS-DXF 62 EL)
          (COND
            ((= (SETQ ECLR (RTOS (ABS (HAWS-DXF 62 EL)) 2 0)) "0")
             "BYBLOCK"
            )
            ((= ECLR "256") "BYLAYER")
            (T ECLR)
          )
          "BYLAYER"
        )
       LACLR
        (RTOS
          (ABS
            (HAWS-DXF
              62
              (TBLSEARCH "LAYER" (SETQ LAI (HAWS-DXF 8 EL)))
            )
          )
          2
          0
        )
       ALRTSTR
        (STRCAT
          "\n" ETYPE " on layer " LAI " selected.\nIts color is " ECLR
          ".  The color of its layer is " LACLR ".\nIt is "
         )
       I -1
     )
     (IF SHELLS
       (WHILE (SETQ SHELLI (NTH (SETQ I (1+ I)) SHELLS))
         (SETQ
           SHELLL
            (ENTGET SHELLI)
           ALRTSTRI
            (STRCAT
              "in a block (or xref) called "
              (SETQ
                TEMP
                 (CDR
                   (ASSOC
                     (IF
                       (= "INSERT" (CDR (ASSOC 0 SHELLL)))
                        2
                        0
                     )
                     SHELLL
                   )
                 )
              )
              " which is on layer "
              (CDR (ASSOC 8 SHELLL))
              "\n"
            )
         )
         (IF (/= PREVI ALRTSTRI)
           (SETQ ALRTSTR (STRCAT ALRTSTR ALRTSTRI))
         )
         (SETQ PREVI ALRTSTRI)
       )
     )
     (SETQ ALRTSTR (STRCAT ALRTSTR "of the current drawing."))
     (LIST ALRTSTR (CONS (CAR ES) SHELLS))
    )
    (T (LIST "" NIL))
  )
)

;;; Returns list with a leading comma
(DEFUN
   HAWS-XLIST-LIST-TO-STRING
   (ENAMELIST EDIT-MODE / DONELIST  ENAME  LAYERLISTSTRING)
  (SETQ LAYERLISTSTRING "")
  (COND ((/= EDIT-MODE 2) (SETQ ENAMELIST (CADR ENAMELIST))))
  (WHILE (SETQ ENAME (CAR ENAMELIST))
    (SETQ ENAMELIST (CDR ENAMELIST))
    (COND
      ((NOT (MEMBER ENAME DONELIST))
       (SETQ DONELIST (CONS ENAME DONELIST))
       (SETQ
         LAYERLISTSTRING
          (STRCAT
            LAYERLISTSTRING
            ","
            (CDR (ASSOC 8 (ENTGET ENAME)))
          )
       )
      )
    )
  )
  LAYERLISTSTRING
)

(DEFUN
   HAWS-XLIST-SHOW-LAYERS (LAYERLISTSTRING)
  (vl-cmdf
    "._LAYER"
    "_FILTER"
    "_DELETE"
    "HAWS-XLIST"
    ""
    "._LAYER"
    "_FILTER"
    "_NEW"
    "_GROUP"
    ""
    (SUBSTR LAYERLISTSTRING 2)
    "HAWS-XLIST"
    "_EXIT"
    ""
    "LAYERPALETTE"
  )
)

 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
