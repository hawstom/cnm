;;; UTILITY LINES
;;; (C) Copyright 2017 by Thomas Gail Haws
(DEFUN
   C:HAWS-UT (/ PT1)
  (haws-core-init 326)
  (HAWS-VSAVE
    '("ucsfollow" "clayer" "filedia" "cmdecho" "expert")
  )
  (HAWS-VSET
    '(("ucsfollow" 0) ("plinegen" 1) ("cmdecho" 0) ("expert" 5))
  )
  (IF (NOT *HAWS-UT-SETTINGS*)
    (HAWS-UT-GETUTL)
  )
  (WHILE (PROGN
           (INITGET "Setup")
           (= (SETQ PT1 (GETPOINT "\n<Start point>/Setup:")) "Setup")
         )
    (HAWS-UT-GETUTL)
  )
  (HAWS-UT-MAIN PT1)
)

(DEFUN
   HAWS-UT-MAIN (PT1 / E1 E2 ELNEXT ENEXT ENTSEL-OD-DOWN ENTSEL-OD-UP
                 PT2 PT3 TEMP UTLEN UTPL
                )
  ;; Draw centerline using PLINE command
  (HAWS-MKLAYR (STRCAT "UT" (HAWS-UT-GETVAR "Key") "SNG"))
  (COMMAND "pline" PT1 "w" 0 "")
  (SETVAR "cmdecho" 1)
  (WHILE (= 1 (LOGAND 1 (GETVAR "cmdactive")))
    (IF PT3
      (COMMAND PAUSE)
      (PROGN
        (INITGET
          "Line Undo Arc CEnter Angle Direction Line Radius Second DRag"
        )
        (SETQ PT2 (GETPOINT PT1))
        (IF (= (TYPE PT2) 'LIST)
          (SETQ PT3 PT2)
        )
        (IF (= PT2 "CEnter")
          (PROMPT "\nCEnter not allowed.")
          (COMMAND PT2)
        )
      )
    )
  )
  (SETVAR "cmdecho" 0)
  ;; Offset OD polylines
  (COND
    ((/= (HAWS-UT-GETVAR "HalfWidth") 0)
     (SETQ UTPL (ENTLAST))
     (COMMAND "._line" PT1 PT3 "")
     (SETQ
       E1   (ENTLAST)
       UCSP T
     )
     (COMMAND "._ucs" "e" E1 "._erase" E1 "")
     (SETQ ENTSEL-OD-UP (HAWS-UT-MAKE-OD UTPL 1))
     (SETQ ENTSEL-OD-DOWN (HAWS-UT-MAKE-OD UTPL -1))
     (COMMAND
       "._pedit"
       UTPL
       "_w"
       (* 2 (HAWS-UT-GETVAR "HalfWidth"))
       ""
       "._change"
       UTPL
       ""
       "_p"
       "_la"
       (HAWS-UT-GETVAR "Layer.PL")
       ""
     )
    )
  )
  ;; Create labels
  (COND
    ((AND (HAWS-UT-GETVAR "IsExisting") (HAWS-UT-GETVAR "Label"))
     (COMMAND "area" "e" (CAR ENTSEL-OD-UP))
     (SETQ UTLEN (GETVAR "perimeter"))
     (SETQ
       E1    (ENTLAST)
       ENEXT E1
     )
     (IF (= (HAWS-UT-GETVAR "HalfWidth") 0)
       (HAWS-UT-ASSURE-LABEL-BLOCK "uttxtjm" "m" PT1)
       (HAWS-UT-ASSURE-LABEL-BLOCK "uttxtjbc" "bc" PT1)
     )
     (COMMAND
       "._divide"
       ENTSEL-OD-UP
       "b"
       (IF (= (HAWS-UT-GETVAR "HalfWidth") 0)
         "uttxtjm"
         "uttxtjbc"
       )
       "y"
       (MAX 2 (ATOI (RTOS (/ UTLEN 200) 2 0)))
     )
     (HAWS-UT-RESTORE-UCS)
     (WHILE (SETQ ENEXT (ENTNEXT ENEXT))
       (IF (= (CDR (ASSOC 0 (ENTGET ENEXT))) "INSERT")
         (COMMAND "._explode" ENEXT)
       )
     )
     (SETQ ENEXT E1)
     (WHILE (SETQ ENEXT (ENTNEXT ENEXT))
       (COND
         ((= (CDR (ASSOC 0 (SETQ ELNEXT (ENTGET ENEXT)))) "TEXT")
          (SUBST
            (CONS 1 (HAWS-UT-GETVAR "Label"))
            (ASSOC 1 ELNEXT)
            ELNEXT
          )
          (ENTMOD
            (SUBST
              (CONS 40 (* (HAWS-DWGSCALE) (GETVAR "dimtxt")))
              (ASSOC 40 ELNEXT)
              (SUBST
                (CONS 8 (GETVAR "clayer"))
                (ASSOC 8 ELNEXT)
                (SUBST
                  (CONS 1 (HAWS-UT-GETVAR "Label"))
                  (ASSOC 1 ELNEXT)
                  ELNEXT
                )
              )
            )
          )
         )
       )
     )
    )
    (T (HAWS-UT-RESTORE-UCS))
  )
  (REDRAW)
  (HAWS-VRSTOR)
  (haws-core-restore)
  (PRINC)
)

(DEFUN
   HAWS-UT-GETUTL (/ TEMP LABEL-MATERIAL LABEL LABEL-SIZE LABEL-TYPE)
  (INITGET 1 "Exist Prop")
  (HAWS-UT-SETVAR
    "IsExisting"
    (= (GETKWORD "\nExist/Prop:") "Exist")
  )
  (INITGET 1 "Wat Sew SD Irr Gas Elec Tel Catv")
  (HAWS-UT-SETVAR
    "Key"
    (STRCASE (GETKWORD "\nWat/Sew/SD/Irr/Gas/Elec/Tel/Catv:"))
  )
  (SETQ LABEL-SIZE "")
  (COND
    ((HAWS-UT-GETVAR "IsExisting")
     (SETQ LABEL-SIZE (GETSTRING (STRCAT "\nSize for label <none>: ")))
    )
  )
  (HAWS-UT-SETVAR
    "HalfWidth"
    (COND
      ((HAWS-UT-GETVAR "IsExisting") (/ (ATOF LABEL-SIZE) 24))
      ((HAWS-UT-GETVAR "HalfWidth"))
      (0)
    )
  )
  (SETQ
    LABEL-SIZE
     (IF (= LABEL-SIZE "")
       ""
       (STRCAT LABEL-SIZE "\" ")
     )
  )
  (SETVAR "filedia" 0)
  (COMMAND "._vslide" "pipetabl")
  (SETVAR "filedia" 1)
  (IF (SETQ
        TEMP
         (GETREAL
           (STRCAT
             "\nSize for graphic width (0 for small)<"
             (RTOS (* 24 (HAWS-UT-GETVAR "HalfWidth")) 5 2)
             ">:"
           )
         )
      )
    (HAWS-UT-SETVAR "HalfWidth" (/ TEMP 24))
  )
  (REDRAW)
  (COND
    ((HAWS-UT-GETVAR "IsExisting")
     (SETQ
       LABEL-TYPE
        (COND
          ((= (HAWS-UT-GETVAR "Key") "WAT") "W")
          ((= (HAWS-UT-GETVAR "Key") "SEW") "S")
          ((= (HAWS-UT-GETVAR "Key") "SD") "SD")
          ((= (HAWS-UT-GETVAR "Key") "IRR") "IRR")
          ((= (HAWS-UT-GETVAR "Key") "GAS") "G")
          ((= (HAWS-UT-GETVAR "Key") "ELEC") "E")
          ((= (HAWS-UT-GETVAR "Key") "TEL") "T")
          ((= (HAWS-UT-GETVAR "Key") "CATV") "TV")
        )
       TEMP
        (GETSTRING "\nMaterial for label <none>: ")
       LABEL-MATERIAL
        (IF (= TEMP "")
          TEMP
          (STRCAT " (" TEMP ")")
        )
       LABEL
        (STRCAT LABEL-SIZE LABEL-TYPE LABEL-MATERIAL)
       TEMP
        (GETSTRING
          1
          (STRCAT "\nFull label or . for none <" LABEL ">: ")
        )
     )
     (HAWS-UT-SETVAR
       "Label"
       (COND
         ((= TEMP "") LABEL)
         ((= TEMP ".") NIL)
         (TEMP)
       )
     )
    )
  )
  (IF (HAWS-UT-GETVAR "IsExisting")
    (HAWS-UT-SETVAR "Key" (STRCAT "X" (HAWS-UT-GETVAR "Key")))
  )
  (HAWS-UT-SETVAR
    "Layer.PL"
    (CAR
      (HAWS-MKLAYR (STRCAT "UT" (HAWS-UT-GETVAR "Key") "PL"))
    )
  )
  (HAWS-UT-SETVAR
    "Layer.OD"
    (CAR
      (HAWS-MKLAYR (STRCAT "UT" (HAWS-UT-GETVAR "Key") "OD"))
    )
  )
)

(DEFUN
   HAWS-UT-GETVAR (VAR)
  (SETQ VAR (STRCASE VAR))
  (CADR (ASSOC VAR *HAWS-UT-SETTINGS*))
)

(DEFUN
   HAWS-UT-SETVAR (VAR VAL)
  (SETQ
    VAR                (STRCASE VAR)
    *HAWS-UT-SETTINGS* (COND
                         ((ASSOC VAR *HAWS-UT-SETTINGS*)
                          (SUBST
                            (LIST VAR VAL)
                            (ASSOC VAR *HAWS-UT-SETTINGS*)
                            *HAWS-UT-SETTINGS*
                          )
                         )
                         (T (CONS (LIST VAR VAL) *HAWS-UT-SETTINGS*))
                       )
  )
  VAL
)

(DEFUN
   HAWS-UT-ASSURE-LABEL-BLOCK (BLOCK-NAME JUSTIFICATION INSERTION)
  (COND
    ((AND
       (HAWS-UT-GETVAR "IsExisting")
       (HAWS-UT-GETVAR "Label")
       (NOT (TBLSEARCH "BLOCK" BLOCK-NAME))
     )
     (HAWS-MKTEXT JUSTIFICATION INSERTION 1.0 0 "X")
     (COMMAND "._block" BLOCK-NAME INSERTION (ENTLAST) "")
    )
  )
)

(DEFUN
   HAWS-UT-MAKE-OD (ENAME-PLINE DIRECTION / ENAME-OD OFFSET-POINT)
  (COMMAND
    "._offset"
    (HAWS-UT-GETVAR "HalfWidth")
    (LIST ENAME-PLINE '(0 0))
    "_non"
    (SETQ
      OFFSET-POINT
       (LIST 0 (* DIRECTION (HAWS-UT-GETVAR "HalfWidth")) 0)
    )
    ""
  )
  (SETQ ENAME-OD (ENTLAST))
  (ENTMOD
    (SUBST
      (CONS 8 (HAWS-UT-GETVAR "Layer.OD"))
      (ASSOC 8 (ENTGET ENAME-OD))
      (ENTGET ENAME-OD)
    )
  )
  (LIST ENAME-OD OFFSET-POINT)
)
(DEFUN
   HAWS-UT-RESTORE-UCS ()
  (COND (UCSP (SETQ UCSP NIL) (COMMAND "ucs" "p")))
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
