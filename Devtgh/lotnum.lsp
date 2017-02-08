;(C) Copyright 1997 by Thomas Gail Haws
;Lot numbering
(defun c:haws-lotnum (/ i rot just pt1 pt2 txpt)
  (setvar "cmdecho" 0)
  (setq
    osmold (getvar "osmode")
    i (getint "\nFirst number of series: ")
  )
  (if (not rot)(setq rot 0))

  (setq pt1(getpoint"\nFirst of two points between which to center number: "))
  (while
    (progn
      (initget "First")
      (setq pt2 (getpoint pt1 (strcat"\nFirst point/<Second point for lot " (itoa i)">: ")))
    )
    (if (= pt2 "First")
      (setq pt1(getpoint"\nFirst point: ")pt2(getpoint pt1"\nSecond point: "))
    )
    (setq txpt
      (list
        (/ (+ (car pt1)  (car pt2))  2)
        (/ (+ (cadr pt1) (cadr pt2)) 2)
        (/ (+ (caddr pt1)(caddr pt2))2)
    ) )
    (setvar "osmode" 0)
    (command "text" "j" "m" txpt (* (getvar "dimtxt")(getvar "dimscale")) 0 i)
    (setvar "osmode" osmold)
    (setq i (1+ i) pt1 pt2)
  )
)
