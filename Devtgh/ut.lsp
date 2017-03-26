;;; UTILITY LINES
;;; (C) Copyright 1999 by Thomas Gail Haws
(DEFUN
   c:haws-UT
       (/ E1 E2 ELNEXT ENEXT PT1 PT2 PT3 TEMP UCSP
       )
  (DEFUN
     HAWS-GETUTL
           ()
    (INITGET 1 "Exist Prop")
    (SETQ UEXIST (= (GETKWORD "\nExist/Prop:") "Exist"))
    (INITGET 1 "Wat Sew SD Irr Gas Elec Tel Catv")
    (SETQ
      UTIL  (STRCASE (GETKWORD "\nWat/Sew/SD/Irr/Gas/Elec/Tel/Catv:"))
      UTSIZ ""
    ) ;_ end of setq
    (COND
      (UEXIST
       (SETQ
         UTSIZ
          (GETSTRING
            (STRCAT "\nPipe size for label (ID in inches) <none>: ")
          ) ;_ end of getstring
       ) ;_ end of setq
      )
    ) ;_ end of cond
    (SETQ
      UTRAD
       (COND
         (UEXIST (/ (ATOF UTSIZ) 24))
         (UTRAD)
         (0)
       ) ;_ end of cond
    ) ;_ end of setq
    (SETQ
      UTSIZ
       (IF (= UTSIZ "")
         ""
         (STRCAT UTSIZ "\" ")
       ) ;_ end of if
    ) ;_ end of setq
    (SETVAR "filedia" 0)
    (COMMAND "._vslide" "pipetabl")
    (SETVAR "filedia" 1)
    (IF
      (SETQ
        TEMP
         (GETREAL
           (STRCAT
             "\nPipe size for graphic pipe width (0 for small)<"
             (RTOS (* 24 UTRAD) 5 2)
             ">:"
           ) ;_ end of strcat
         ) ;_ end of getreal
      ) ;_ end of setq
       (SETQ UTRAD (/ TEMP 24))
    ) ;_ end of if
    (REDRAW)
    (COND
      (UEXIST
       (SETQ
         UTTYP
          (COND
            ((= UTIL "WAT") "W")
            ((= UTIL "SEW") "S")
            ((= UTIL "SD") "SD")
            ((= UTIL "IRR") "IRR")
            ((= UTIL "GAS") "G")
            ((= UTIL "ELEC") "E")
            ((= UTIL "TEL") "T")
            ((= UTIL "CATV") "TV")
          ) ;_ end of cond
         TEMP
          (GETSTRING "\nMaterial for label <none>: ")
         UTMTL
          (IF (= TEMP "")
            TEMP
            (STRCAT " (" TEMP ")")
          ) ;_ end of if
         UTTXT
          (STRCAT UTSIZ UTTYP UTMTL)
         TEMP
          (GETSTRING
            1
            (STRCAT "\nUtility label or . for none <" UTTXT ">: ")
          ) ;_ end of getstring
         UTTXT
          (COND
            ((= TEMP "") UTTXT)
            ((= TEMP ".") NIL)
            (TEMP)
          ) ;_ end of cond
       ) ;_ end of setq
      )
    ) ;_ end of cond
    (IF UEXIST
      (SETQ UTIL (STRCAT "X" UTIL))
    ) ;_ end of if
    (SETQ ULAYPL (CAR (HAWS-MKLAYR (STRCAT "UT" UTIL "PL"))))
    (SETQ ULAYOD (CAR (HAWS-MKLAYR (STRCAT "UT" UTIL "OD"))))
  ) ;_ end of defun
  (haws-core-borrow 0)
  (HAWS-VSAVE '("ucsfollow" "clayer" "filedia" "cmdecho" "expert"))
  (HAWS-VSET
    '(("ucsfollow" 0) ("plinegen" 1) ("cmdecho" 0) ("expert" 5))
  ) ;_ end of vset
  (IF (NOT UTIL)
    (HAWS-GETUTL)
  ) ;_ end of if
  (WHILE
    (PROGN
      (INITGET "Setup")
      (= (SETQ PT1 (GETPOINT "\n<Start point>/Setup:")) "Setup")
    ) ;_ end of progn
     (HAWS-GETUTL)
  ) ;_ end of while
  (HAWS-MKLAYR (STRCAT "UT" UTIL "SNG"))
  (COMMAND "pline" PT1 "w" 0 "")
  (SETVAR "cmdecho" 1)
  (WHILE
    (= 1 (LOGAND 1 (GETVAR "cmdactive")))
     (IF PT3
       (COMMAND PAUSE)
       (PROGN
         (INITGET
           "Line Undo Arc CEnter Angle Direction Line Radius Second DRag"
         ) ;_ end of initget
         (SETQ PT2 (GETPOINT PT1))
         (IF (= (TYPE PT2) 'LIST)
           (SETQ PT3 PT2)
         ) ;_ end of if
         (IF (= PT2 "CEnter")
           (PROMPT "\nCEnter not allowed.")
           (COMMAND PT2)
         ) ;_ end of if
       ) ;_ end of progn
     ) ;_ end of if
  ) ;_ end of while
  (SETVAR "cmdecho" 0)
  (SETQ
    UTPL
     (ENTLAST)
  ) ;_ end of setq
  (COND
    ((AND
       UEXIST
       UTTXT
       (NOT (TBLSEARCH "BLOCK" "uttxtjm"))
     ) ;_ end of and
     (HAWS-MKTEXT "m" PT1 1.0 0 "X")
     (COMMAND "._block" "uttxtjm" PT1 (ENTLAST) "")
    )
  ) ;_ end of cond
  (COND
    ((AND
       UEXIST
       UTTXT
       (NOT (TBLSEARCH "BLOCK" "uttxtjbc"))
     ) ;_ end of and
     (HAWS-MKTEXT "bc" PT1 1.0 0 "X")
     (COMMAND "._block" "uttxtjbc" PT1 (ENTLAST) "")
    )
  ) ;_ end of cond
  (COND
    ((/= UTRAD 0)
     (COMMAND "._line" PT1 PT3 "")
     (SETQ
       E1   (ENTLAST)
       UCSP T
     ) ;_ end of setq
     (COMMAND
       "._ucs"
       "e"
       E1
       "erase"
       E1
       ""
       "offset"
       UTRAD
       (LIST UTPL '(0 0))
       (SETQ PT1 (LIST 0 (* 3 UTRAD) 0))
       ""
     ) ;_ end of command
     (SETQ E1 (ENTLAST))
     (COMMAND
       "change"
       E1
       ""
       "p"
       "la"
       ULAYOD
       ""
       "offset"
       UTRAD
       (LIST UTPL '(0 0))
       (LIST 0 (* -3 UTRAD) 0)
       ""
     ) ;_ end of command
     (SETQ E2 (ENTLAST))
     (COMMAND
       "change"
       E2
       ""
       "p"
       "la"
       ULAYOD
       ""
       "pedit"
       UTPL
       "w"
       (* 2 UTRAD)
       ""
       "change"
       UTPL
       ""
       "p"
       "la"
       ULAYPL
       ""
     ) ;_ end of command
     (SETQ UTPL E1)
    )
  ) ;_ end of cond
  (COND
    ((AND UEXIST UTTXT)
     (COMMAND "area" "e" UTPL)
     (SETQ UTLEN (GETVAR "perimeter"))
     (SETQ
       E1    (ENTLAST)
       ENEXT E1
     ) ;_ end of setq
     (COMMAND
       "._divide"
       (LIST UTPL PT1)
       "b"
       (IF (= UTRAD 0)
         "uttxtjm"
         "uttxtjbc"
       ) ;_ end of if
       "y"
       (MAX 2 (ATOI (RTOS (/ UTLEN 200) 2 0)))
     ) ;_ end of command
     (COND (UCSP (SETQ UCSP NIL) (COMMAND "ucs" "p")))
     (WHILE (SETQ ENEXT (ENTNEXT ENEXT))
       (IF (= (CDR (ASSOC 0 (ENTGET ENEXT))) "INSERT")
         (COMMAND "._explode" ENEXT)
       ) ;_ end of if
     ) ;_ end of while
     (SETQ ENEXT E1)
     (WHILE (SETQ ENEXT (ENTNEXT ENEXT))
       (COND
         ((= (CDR (ASSOC 0 (SETQ ELNEXT (ENTGET ENEXT)))) "TEXT")
          (SUBST (CONS 1 UTTXT) (ASSOC 1 ELNEXT) ELNEXT)
          (ENTMOD
            (SUBST
              (CONS 40 (* (HAWS-DWGSCALE) (GETVAR "dimtxt")))
              (ASSOC 40 ELNEXT)
              (SUBST
                (CONS 8 (GETVAR "clayer"))
                (ASSOC 8 ELNEXT)
                (SUBST (CONS 1 UTTXT) (ASSOC 1 ELNEXT) ELNEXT)
              ) ;_ end of subst
            ) ;_ end of subst
          ) ;_ end of entmod
         )
       ) ;_ end of cond
     ) ;_ end of while
    )
    (T (COND (UCSP (SETQ UCSP NIL) (COMMAND "ucs" "p"))))
  ) ;_ end of cond
  (REDRAW)
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of defun
