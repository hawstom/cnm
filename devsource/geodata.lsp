;;; GEODATA - Get curve data from an arc and save to RADIUS, LENGTH, DELTA, CHORD, and TANGENT attributes in a block.
;;; Also report bearing and distance of lines and circumference of circles.
;;; Works with heavy plines.
;;; By Thomas Gail Haws.
;;; (C) Copyright 2000 by Thomas Gail Haws
;;;
;;; CRVS is an Autodesk registered symbol to avoid conflicts with other applications.

(defun crvs-cirdata
           (el / r)
  (setq
    r (cdr (assoc 40 el))
  ) ;_ end of setq
  (princ "\Radius=")
  (princ (rtos r))
  (princ "  Length=")
  (princ (rtos (* 2 pi r)))
  nil
) ;_ end of defun

(defun crvs-linedata
           (el / p1 p2)
  (setq p1 (cdr (assoc 10 el)))
  (setq p2 (cdr (assoc 11 el)))
  (princ
    (strcat
      (rtos (distance p1 p2))
      "   World bearing="
      (angtos (angle p1 p2) 4)
    ) ;_ end of strcat
  ) ;_ end of princ
  nil
) ;_ end of defun

(defun crvs-arcdata
          (el / delta dover2 ea r sa)
  (setq
    r (cdr (assoc 40 el))
    sa (cdr (assoc 50 el))
    ea (cdr (assoc 51 el))
    delta
     (cond
       ((> sa ea) (- (* 2 pi) (- sa ea)))
       (t (- ea sa))
     ) ;_ end of cond
    dover2
     (/ delta 2)
  ) ;_ end of setq
  (list
    (rtos r)
    (rtos (* delta r))
    (angtos delta 1)
    (rtos (* r (/ (sin dover2) (cos dover2))))
    (rtos (* 2 r (sin dover2)))
    (haws-rtob (/ (+ sa ea (* 3 pi)) 2.0) 4)
  ) ;_ end of list
) ;_ end of defun

(defun crvs-pldata
         (es / bulge1 d delta dover2 en enext p1 p2 r)
  (setq
    en (car es)
    enext
     (entnext en)
    p1 (cdr (assoc 10 (entget en)))
    p2 (cdr (assoc 10 (entget enext)))
    d (/ (distance p1 p2) 2)
  ) ;_ end of setq
  (cond
    ((/= 0 (setq bulge1 (cdr (assoc 42 (entget en)))))
     (setq
       dover2
        (abs (* 2 (atan bulge1)))
       delta
        (* 2 dover2)
       r (/ d (sin dover2))
     ) ;_ end of setq
     (list
       ;;Radius
       (rtos r)
       ;;Length
       (rtos (* delta r))
       ;;Delta
       (angtos delta 1)
       ;;Tangent
       (rtos (* r (/ (sin dover2) (cos dover2))))
       ;;Chord
       (rtos (* 2 r (sin dover2)))
       ;;Bearing of chord
       (haws-rtob
         (angle
           (cdr (assoc 10 (entget en)))
           (cdr (assoc 10 (entget (entnext en))))
         ) ;_ end of angle
         4
       ) ;_ end of HAWS-RTOB
     ) ;_ end of list
    )
    (t
     (princ "\nL=")
     (princ (* 2 d))
     nil
    )
  ) ;_ end of cond
) ;_ end of defun

(defun crvs-ldr
             (crvdata pickpt / ang1 dg left ptxt txht lline pt10 pt11 lbear ldist pt1 pt2 pt3 pt4 rot
             )
  (setq
    dg   (* (getvar "dimgap") (haws-dwgscale))
    txht (haws-text-height-model)
    pt1  (osnap pickpt "nea")
    ptxt (getpoint pt1 "\nPick text location: ")
    ang1 (angle pt1 ptxt)
    left (minusp (cos ang1))
  ) ;_ end of setq
  (cond
    ((>= (atof (getvar "acadver")) 14)
     (vl-cmdf
       "._leader"
       pt1
       ptxt
       ""
       (strcat "R=" (car crvdata))
       (strcat "L=" (cadr crvdata))
       (strcat "DELTA=" (caddr crvdata))
       ""
     ) ;_ end of command
    )
    (t
     (vl-cmdf
       "dim"
       "leader"
       pt1
       ptxt
       ""
       (strcat "R=" (car crvdata))
       "exit"
     ) ;_ end of command
     (setq
       ptxt
        (polar
          (polar ptxt (/ pi -2) (* 1.667 txht))
          (if left
            pi
            0
          ) ;_ end of if
          (+ (if (< (abs (sin ang1)) (sin 0.25))
               0
               txht
             ) ;_ end of if
             dg
          ) ;_ end of +
        ) ;_ end of polar
     ) ;_ end of setq
     (haws-mktext
       (if left
         "mr"
         "ml"
       ) ;_ end of if
       ptxt
       nil
       0
       (strcat "L=" (cadr crvdata))
     ) ;_ end of mktext
     (setq
       ptxt
        (polar
          (polar ptxt (/ pi -2) (* 1.667 txht))
          (if left
            pi
            0
          ) ;_ end of if
          (+ (if (< (abs (sin ang1)) (sin 0.25))
               0
               txht
             ) ;_ end of if
             dg
          ) ;_ end of +
        ) ;_ end of polar
     ) ;_ end of setq
     (haws-mktext
       (if left
         "mr"
         "ml"
       ) ;_ end of if
       ptxt
       nil
       0
       (strcat "DELTA=" (caddr crvdata))
     ) ;_ end of mktext
    )
  ) ;_ end of cond
  (princ)
) ;_ end of defun

(defun crvs-savedata
           (en crvdata / at av el et n saved)
  (terpri)
  (while
    (and
      (setq en (entnext en))
      (/= "SEQEND"
          (setq et (cdr (assoc 0 (setq el (entget en)))))
      ) ;_ end of /=
    ) ;_ end of and
     (cond
       ((= et "ATTRIB")
        (setq
          at (cdr (assoc 2 el))
          av (cdr (assoc 1 el))
        ) ;_ end of setq
        (cond
          ((setq n (member at '("BEARING" "TANGENT" "DELTA" "LENGTH" "RADIUS")))
           (entmod
             (subst
               (cons 1 (nth (1- (length n)) crvdata))
               (assoc 1 el)
               el
             ) ;_ end of SUBST
           ) ;_ end of ENTMOD
           (princ (strcat at " "))
           (setq saved t)
          )
        ) ;_ end of cond
        (entupd en)
       )
     ) ;_ end of cond
  ) ;_ end of while
  (if (not saved)
    (princ "No ")
  ) ;_ end of if
  (princ "data saved to block.")
) ;_ end of defun

;;; ========== End sub-functions to GEODATA ===========

;;; Main function
(defun c:haws-geodata
            (/ crvdata el en es es1 pickpt etype
            )
(haws-core-init 222)
  "\nGEODATA version 2.0, Copyright (C) 2002 Thomas Gail Haws
GEODATA comes with ABSOLUTELY NO WARRANTY.
This is free software, and you are welcome to modify and
redistribute it under the terms of the GNU General Public License.
The latest version of GEODATA is always available at www.hawsedc.com"
  (setq
    es (nentsel)                        ; Prompt user for an entity on screen.
    pickpt
     (cadr es)                          ; Save the pick point.
    el (entget (car es))                ; Get info for the selected entity.
    etype
     (cdr (assoc 0 el))                 ; Determine the type of entity picked.
    crvdata
     (cond
       ((= etype "ARC") (crvs-arcdata el))   ; For arcs...
       ((= etype "LINE") (crvs-linedata el)) ; For lines...
       ((= etype "CIRCLE") (crvs-cirdata el)) ; For circles...
       ((= etype "VERTEX") (crvs-pldata es)) ; For plines...
       (t (vl-cmdf "._AREA" "_E" pickpt))  ; Default, invoke AREA command.
     ) ;_ end of cond
  ) ;_ end of SETQ
  (cond
    (crvdata
     (princ
       (apply
         'STRCAT
         (mapcar
           'STRCAT
           (list
             "\nRadius=" "  Length=" "  Delta=" "  Tangent=" "  Chord=" "  Bearing of Chord="
            ) ;_ end of list
           crvdata
         ) ;_ end of mapcar
       ) ;_ end of apply
     ) ;_ end of princ
     (setq
       es1
        (progn
          (initget "LEader")
          (entsel
            "\n<Select block to receive Radius, Length, Delta, Chord, and Tangent data>/LEader: "
          ) ;_ end of ENTSEL
        ) ;_ end of PROGN
     ) ;_ end of SETQ
     (cond
       ((= es1 "LEader") (crvs-ldr crvdata pickpt))
       ((setq en (car es1))
        (crvs-savedata en crvdata)
       )
       (t
        (princ "\nNo block selected.")
       )
     ) ;_ end of COND
    )
  ) ;_ end of COND
  (princ)
) ;_ end of defun
;;end GEODATA

