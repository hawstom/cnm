;;;Calculate volume between contours
;;;(C) Copyright 2008 by Thomas Gail Haws
(defun
   c:haws-contvol (/ cv-cont cv-add-factor cv-area-0
                   cv-area-1 cv-area-temp cv-elev-0 CV-ELEV-1 cv-report cv-vol-0 cv-vol-1 cv-vol-inc
                   pt1
                  )
  (haws-core-init 211)
  (command "._undo" "_g")
  (setq
    cv-vol-0 0
    cv-vol-1 0
    cv-area-1 0
    cv-area-temp 0
    cv-elev-1 0
    cv-add-factor 1
    cv-report "VOLUME PROVIDED\nDEPTH\tAREA\tINC. VOL.\tVOL."
    *haws-cv-method*
     (cond
       (*haws-cv-method*)
       ("Conic")
     ) ;_ end of cond
    *haws-cv-interval*
     (cond
       (*haws-cv-interval*)
       (1)
     ) ;_ end of cond
  ) ;_ end of setq
  (princ
    (strcat
      "Using "
      *haws-cv-method*
      " Volume method.  Contour interval="
      (rtos *haws-cv-interval*)
      ".\n"
    )
  )
  ;;Prompt until user hits enter
  (while (progn
           (initget "Add Subtract Method Interval")
           (setq
             cv-cont
              (nentsel
                (strcat
                  "\nSelect "
                  (cond
                    ((/= cv-add-factor 0)
                     "another contour at this elevation"
                    )
                    (t "contour at next elevation")
                  )
                  " or [Add more areas/Subtract areas/volume Method/contour Interval]: "
                )
              )
           )
         ) ;_ end of progn
    (cond
      ;;If user wants to change method, do it.
      ((= cv-cont "Method") (HAWS-cvmethset))
      ;;Else if user wants to change interval, do it.
      ((= cv-cont "Interval") (HAWS-cvintset))
      ;;Else if user wants to select another contour, flag the desire.
      ((= cv-cont "Add") (setq cv-add-factor 1))
      ((= cv-cont "Subtract") (setq cv-add-factor -1))
      ;;Else
      (t
       ;;1.  Get the area of the contour
       (command "._area" "_e" (car cv-cont))
       ;;2.  Close out the elevation or flag it to be closed next time.
       (cond
         ((= cv-add-factor 0)
          (haws-cv-close-elev)
         )
         (t
          (setq
            cv-area-temp
             (+ cv-area-temp (* cv-add-factor (getvar "area")))
            cv-add-factor 0
          )
         )
       )
      )
    )
  )
  ;;When user signals finish with a return,
  ;;Save, print, and make text of the final volume.
  (cond
    (cv-area-temp
     (haws-cv-close-elev)
    )
  )
  (setq
    pt1
     (getpoint
       "\nStart point for volume: "
     )
  )
  (haws-make-mtext
    pt1
    "TL"
    nil
    0
    cv-report
    nil
  )
  (command "._undo" "_e")
  (haws-core-restore)
  (princ)
)
(defun
   HAWS-cvmethset ()
  (initget "Average Conic")
  (setq *haws-cv-method* (getkword "\nVolume method [Average/Conic]: "))
  (princ)
) ;_ end of defun
(defun
   HAWS-cvintset ()
  (setq *haws-cv-interval* (HAWS-GETREALX "\nContour interval" *haws-cv-interval* 1))
  (princ)
)
(defun
   haws-cv-close-elev ()
  (setq
    cv-area-0 cv-area-1
    cv-area-1 cv-area-temp
    cv-area-temp
     (getvar "area")
  )
  (cond ((and cv-elev-0 cv-area-1) (haws-cv-vol-calc)))
  (cond (cv-area-1 (haws-cv-line-print)))
  (setq
    cv-elev-0 cv-elev-1
    cv-elev-1
     (+ cv-elev-1 *haws-cv-interval*)
    cv-vol-0 cv-vol-1
    cv-vol-1 0
  )
  (princ)
)
(defun
   haws-cv-vol-calc ()
  (setq
    cv-vol-inc
     (* *haws-cv-interval*
        (if (= *haws-cv-method* "Conic")
          (/ (+ cv-area-1
                cv-area-0
                (sqrt
                  (* cv-area-1 cv-area-0)
                )
             )
             3.0
          )
          (/ (+ cv-area-1 cv-area-0) 2.0)
        )
     )
    cv-vol-1
     (+ cv-vol-0 cv-vol-inc)
  )
)
(defun
   haws-cv-line-print ()
  (setq cv-report (strcat cv-report "\n" (rtos cv-elev-1 2) "\t" (rtos cv-area-1 2)))
  (cond
    (cv-vol-inc
     (princ (strcat "\nInc. vol.="(rtos cv-vol-inc 2) " Vol.=" (rtos cv-vol-1 2)))
     (setq
       cv-report
        (strcat
          cv-report
          "\t"
          (rtos cv-vol-inc 2)
          "\t"
          (rtos cv-vol-1 2)
        )
     )
    )
  )
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 0 0 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
