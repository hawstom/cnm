;;;Calculate volume between contours
;;;(C) Copyright 2008 by Thomas Gail Haws
(defun
   c:haws-contvol (/ cvcont cvadd cv-area-twice-previous
              cv-area-previous cv-area-current pt1
             )
  (HAWS-ERRDEF 0)
  (command "._undo" "g")
  (setq
    cvvol 0
    *cvmeth*
     (cond
       (*cvmeth*)
       ("Conic")
     ) ;_ end of cond
    *cvint*
     (cond
       (*cvint*)
       (1)
     ) ;_ end of cond
  ) ;_ end of setq
  (princ
    (strcat
      "Using "
      *cvmeth*
      " Volume method.  Contour interval="
      (rtos *cvint*)
      ".\n"
    )
  )
  ;;Prompt until user hits enter
  (while (progn
           (initget "Add Subtract Method Interval")
           (setq
             cvcont
              (nentsel
                (strcat
                  "\nSelect "
                  (if cvadd
                    "another contour at this elevation"
                    "contour at next elevation"
                  )
                  " or [Add more areas/Subtract areas/volume Method/contour Interval]: "
                )
              )
           )
         ) ;_ end of progn
    (cond
      ;;If user wants to change method, do it.
      ((= cvcont "Method") (HAWS-cvmethset))
      ;;Else if user wants to change interval, do it.
      ((= cvcont "Interval") (HAWS-cvintset))
      ;;Else if user wants to select another contour, flag the desire.
      ((= cvcont "Add")(setq cvadd 1))
      ((= cvcont "Subtract")(setq cvadd -1))
      ;;Else
      (t
       ;;1.  Get the area of the contour
       (command "._area" "e" (car cvcont))
       ;;2.  If user wants to add this area to the previous,
       (cond
         (cvadd
          ;;Add it to current area and remove the flag
          (setq
            cv-area-current
             (+ cv-area-current (* cvadd (getvar "area")))
            cvadd nil
          )
         )
         ;;Else
         (T
          ;;a.  Save previous values
          (setq
            cv-area-twice-previous cv-area-previous
            cv-area-previous cv-area-current
          )
          ;;b.  Save the volume up to the previous elevation
          (if cv-area-twice-previous
            (setq cvvol (haws-cvvolcalc cv-area-twice-previous cv-area-previous cvvol))
          )
          ;;c.  Set the current area
          (setq cv-area-current (getvar "area"))
         )
       )
       ;;3.  Print the current area and volume
       (princ (strcat "\nArea=" (rtos cv-area-current) "."))
       (if cv-area-previous
         (princ
           (strcat
             "\nVolume="
             (rtos (haws-cvvolcalc cv-area-previous cv-area-current cvvol))
           )
         )
       )
      )
    )
  )
  ;;When user signals finish with a return,
  ;;Save, print, and make text of the final volume.
  (if cv-area-previous
    (setq cvvol (haws-cvvolcalc cv-area-previous cv-area-current cvvol))
  )
  (setq
    pt1
     (getpoint
       (strcat
         "\nVolume="
         (rtos cvvol)
         ".  Start point for volume: "
       )
     )
  )
  (HAWS-MKTEXT
    "m"
    pt1
    (* (getvar "dimscale") (getvar "dimtxt"))
    0
    (rtos cvvol)
  )
  (command "._undo" "e")
  (HAWS-ERRRST)
  (princ)
)
(defun
   HAWS-cvmethset ()
  (initget "Average Conic")
  (setq *cvmeth* (getkword "\nVolume method <Average/Conic>: "))
  (princ)
) ;_ end of defun
(defun
   HAWS-cvintset ()
  (setq *cvint* (HAWS-GETREALX "\nContour interval" cvint 1))
  (princ)
)
(defun
   haws-cvvolcalc (cv-area-twice-previous cv-area-previous cvvol0)
     (+ cvvol0
        (* *cvint*
           (if (= *cvmeth* "Conic")
             (/ (+ cv-area-previous cv-area-twice-previous (sqrt (* cv-area-previous cv-area-twice-previous))) 3)
             (/ (+ cv-area-previous cv-area-twice-previous) 2)
           )
        )
     )
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 0 0 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
