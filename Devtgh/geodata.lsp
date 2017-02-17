;;; GEODATA - Get curve data from an arc and save to RADIUS, LENGTH, DELTA, CHORD, and TANGENT attributes in a block.
;;; Also report bearing and distance of lines and circumference of circles.
;;; Works with heavy plines.
;;; By Thomas Gail Haws.
;;; (C) Copyright 2000 by Thomas Gail Haws
;;;
;;; CRVS is an Autodesk registered symbol to avoid conflicts with other applications.

(DEFUN
   CRVS-CIRDATA
           (EL / R)
  (SETQ
    R (CDR (ASSOC 40 EL))
  ) ;_ end of setq
  (PRINC "\Radius=")
  (PRINC (RTOS R))
  (PRINC "  Length=")
  (PRINC (RTOS (* 2 PI R)))
  NIL
) ;_ end of defun

(DEFUN
   CRVS-linedata
           (EL / P1 P2)
  (SETQ P1 (CDR (ASSOC 10 EL)))
  (SETQ P2 (CDR (ASSOC 11 EL)))
  (PRINC
    (STRCAT
      (RTOS (DISTANCE P1 P2))
      "   World bearing="
      (ANGTOS (ANGLE P1 P2) 4)
    ) ;_ end of strcat
  ) ;_ end of princ
  NIL
) ;_ end of defun

(DEFUN
   CRVS-arcdata
          (EL / DELTA DOVER2 EA R SA)
  (SETQ
    R (CDR (ASSOC 40 EL))
    SA (CDR (ASSOC 50 EL))
    EA (CDR (ASSOC 51 EL))
    DELTA
     (COND
       ((> SA EA) (- (* 2 PI) (- SA EA)))
       (T (- EA SA))
     ) ;_ end of cond
    DOVER2
     (/ DELTA 2)
  ) ;_ end of setq
  (LIST
    (RTOS R)
    (RTOS (* DELTA R))
    (ANGTOS DELTA 1)
    (RTOS (* R (/ (SIN DOVER2) (COS DOVER2))))
    (RTOS (* 2 R (SIN DOVER2)))
    (HAWS-RTOB (/ (+ SA EA (* 3 PI)) 2.0) 4)
  ) ;_ end of list
) ;_ end of defun

(DEFUN
   CRVS-pldata
         (ES / BULGE1 D DELTA DOVER2 EN ENEXT P1 P2 R)
  (SETQ
    EN (CAR ES)
    ENEXT
     (ENTNEXT EN)
    P1 (CDR (ASSOC 10 (ENTGET EN)))
    P2 (CDR (ASSOC 10 (ENTGET ENEXT)))
    D (/ (DISTANCE P1 P2) 2)
  ) ;_ end of setq
  (COND
    ((/= 0 (SETQ BULGE1 (CDR (ASSOC 42 (ENTGET EN)))))
     (SETQ
       DOVER2
        (ABS (* 2 (ATAN BULGE1)))
       DELTA
        (* 2 DOVER2)
       R (/ D (SIN DOVER2))
     ) ;_ end of setq
     (LIST
       ;;Radius
       (RTOS R)
       ;;Length
       (RTOS (* DELTA R))
       ;;Delta
       (ANGTOS DELTA 1)
       ;;Tangent
       (RTOS (* R (/ (SIN DOVER2) (COS DOVER2))))
       ;;Chord
       (RTOS (* 2 R (SIN DOVER2)))
       ;;Bearing of chord
       (HAWS-RTOB
         (ANGLE
           (CDR (ASSOC 10 (ENTGET EN)))
           (CDR (ASSOC 10 (ENTGET (ENTNEXT EN))))
         ) ;_ end of angle
         4
       ) ;_ end of HAWS-RTOB
     ) ;_ end of list
    )
    (T
     (PRINC "\nL=")
     (PRINC (* 2 D))
     NIL
    )
  ) ;_ end of cond
) ;_ end of defun

(DEFUN
   CRVS-LDR
             (CRVDATA PICKPT / ANG1 DG LEFT PTXT TXHT LLINE PT10 PT11 LBEAR LDIST PT1 PT2 PT3 PT4 ROT
             )
  (SETQ
    DG   (* (GETVAR "dimgap") (HAWS-DWGSCALE))
    TXHT (* (HAWS-DWGSCALE) (GETVAR "dimtxt"))
    PT1  (OSNAP PICKPT "nea")
    PTXT (GETPOINT PT1 "\nPick text location: ")
    ANG1 (ANGLE PT1 PTXT)
    LEFT (MINUSP (COS ANG1))
  ) ;_ end of setq
  (COND
    ((>= (ATOF (GETVAR "acadver")) 14)
     (COMMAND
       "._leader"
       PT1
       PTXT
       ""
       (STRCAT "R=" (CAR CRVDATA))
       (STRCAT "L=" (CADR CRVDATA))
       (STRCAT "DELTA=" (CADDR CRVDATA))
       ""
     ) ;_ end of command
    )
    (T
     (COMMAND
       "dim"
       "leader"
       PT1
       PTXT
       ""
       (STRCAT "R=" (CAR CRVDATA))
       "exit"
     ) ;_ end of command
     (SETQ
       PTXT
        (POLAR
          (POLAR PTXT (/ PI -2) (* 1.667 TXHT))
          (IF LEFT
            PI
            0
          ) ;_ end of if
          (+ (IF (< (ABS (SIN ANG1)) (SIN 0.25))
               0
               TXHT
             ) ;_ end of if
             DG
          ) ;_ end of +
        ) ;_ end of polar
     ) ;_ end of setq
     (HAWS-MKTEXT
       (IF LEFT
         "mr"
         "ml"
       ) ;_ end of if
       PTXT
       TXHT
       0
       (STRCAT "L=" (CADR CRVDATA))
     ) ;_ end of mktext
     (SETQ
       PTXT
        (POLAR
          (POLAR PTXT (/ PI -2) (* 1.667 TXHT))
          (IF LEFT
            PI
            0
          ) ;_ end of if
          (+ (IF (< (ABS (SIN ANG1)) (SIN 0.25))
               0
               TXHT
             ) ;_ end of if
             DG
          ) ;_ end of +
        ) ;_ end of polar
     ) ;_ end of setq
     (HAWS-MKTEXT
       (IF LEFT
         "mr"
         "ml"
       ) ;_ end of if
       PTXT
       TXHT
       0
       (STRCAT "DELTA=" (CADDR CRVDATA))
     ) ;_ end of mktext
    )
  ) ;_ end of cond
  (PRINC)
) ;_ end of defun

(DEFUN
   CRVS-savedata
           (EN CRVDATA / AT AV EL ET N SAVED)
  (TERPRI)
  (WHILE
    (AND
      (SETQ EN (ENTNEXT EN))
      (/= "SEQEND"
          (SETQ ET (CDR (ASSOC 0 (SETQ EL (ENTGET EN)))))
      ) ;_ end of /=
    ) ;_ end of and
     (COND
       ((= ET "ATTRIB")
        (SETQ
          AT (CDR (ASSOC 2 EL))
          AV (CDR (ASSOC 1 EL))
        ) ;_ end of setq
        (COND
          ((SETQ N (MEMBER AT '("BEARING" "TANGENT" "DELTA" "LENGTH" "RADIUS")))
           (entmod
             (SUBST
               (CONS 1 (NTH (1- (LENGTH N)) CRVDATA))
               (ASSOC 1 EL)
               EL
             ) ;_ end of SUBST
           ) ;_ end of ENTMOD
           (PRINC (STRCAT AT " "))
           (SETQ SAVED T)
          )
        ) ;_ end of cond
        (ENTUPD EN)
       )
     ) ;_ end of cond
  ) ;_ end of while
  (IF (NOT SAVED)
    (PRINC "No ")
  ) ;_ end of if
  (PRINC "data saved to block.")
) ;_ end of defun

;;; ========== End sub-functions to GEODATA ===========

;;; Main function
(DEFUN
   c:haws-GEODATA
            (/ CRVDATA EL EN ES ES1 PICKPT ETYPE
            )
  "\nGEODATA version 2.0, Copyright (C) 2002 Thomas Gail Haws
GEODATA comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to modify and
redistribute it under the terms of the GNU General Public License.
The latest version of GEODATA is always available at www.hawsedc.com"
  (SETQ
    ES (NENTSEL)                        ; Prompt user for an entity on screen.
    PICKPT
     (CADR ES)                          ; Save the pick point.
    EL (ENTGET (CAR ES))                ; Get info for the selected entity.
    ETYPE
     (CDR (ASSOC 0 EL))                 ; Determine the type of entity picked.
    CRVDATA
     (COND
       ((= ETYPE "ARC") (CRVS-arcdata EL))   ; For arcs...
       ((= ETYPE "LINE") (CRVS-linedata EL)) ; For lines...
       ((= ETYPE "CIRCLE") (CRVS-CIRDATA EL)) ; For circles...
       ((= ETYPE "VERTEX") (CRVS-pldata ES)) ; For plines...
       (T (COMMAND "AREA" "E" PICKPT))  ; Default, invoke AREA command.
     ) ;_ end of cond
  ) ;_ end of SETQ
  (COND
    (CRVDATA
     (PRINC
       (APPLY
         'STRCAT
         (MAPCAR
           'STRCAT
           (LIST
             "\nRadius=" "  Length=" "  Delta=" "  Tangent=" "  Chord=" "  Bearing of Chord="
            ) ;_ end of list
           CRVDATA
         ) ;_ end of mapcar
       ) ;_ end of apply
     ) ;_ end of princ
     (SETQ
       ES1
        (PROGN
          (INITGET "LEader")
          (ENTSEL
            "\n<Select block to receive Radius, Length, Delta, Chord, and Tangent data>/LEader: "
          ) ;_ end of ENTSEL
        ) ;_ end of PROGN
     ) ;_ end of SETQ
     (COND
       ((= ES1 "LEader") (CRVS-LDR CRVDATA PICKPT))
       ((SETQ EN (CAR ES1))
        (CRVS-savedata EN CRVDATA)
       )
       (T
        (PRINC "\nNo block selected.")
       )
     ) ;_ end of COND
    )
  ) ;_ end of COND
  (PRINC)
) ;_ end of defun
;;end GEODATA

