;;; (C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-dw
            (/ ang1 ang2 ang3 ang4 ang5 bccen cl1 cl2 cl3 dwmid dwwid
             incang left osmold pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 ts rad
             wngang
            )
  (haws-core-init 215)
  (haws-vsave '("clayer"))
  (setq
    osmold
     (getvar "osmode")
    dwlen
     (if dwlen
       dwlen
       20.0
     )
    swwid
     (if swwid
       swwid
       4.0
     )
  )
  (setvar "osmode" 0)
  (haws-mklayr "DRIVEWAY")
  (setq ts (haws-text-height-model))
  (while
    (progn
      (initget "Length Sw")
      (prompt "\nCurrent SW width = ")
      (princ swwid)
      (prompt "   Length dw= ")
      (princ dwlen)
      (setvar "osmode" osmold)
      (setq
        dwmid
         (getpoint
           "\nSelect drive midpoint at back of curb or [Length/Sw width]: "
         )
      )
    )
     (cond
       ((= "Length" dwmid)
        (setq dwlen (getreal "\nDriveway length at back of curb: "))
       )
       ((= "Sw" dwmid)
        (setq
          swwid
           (getreal
             "\nSidewalk width (negative for inside of curve): "
           )
        )
       )
       ((setq bccen (osnap dwmid "cen"))
        (setq
          dwwid
           (/ (* swwid (max (abs swwid) 5.0)) (abs swwid))
          ang1
           (angle bccen dwmid)
          rad
           (distance bccen dwmid)
          incang
           (/ dwlen rad)
          wngang
           (/ 5.0 rad)
          ang2
           (- ang1 (/ incang 2.0) wngang)
          ang3
           (+ ang2 wngang)
          ang4
           (+ ang3 incang)
          ang5
           (+ ang4 wngang)
          left
           (minusp (cos (- ang1 0.7854)))
          cl1
           (polar bccen ang1 (+ rad 0.5))
          cl2
           (polar bccen ang1 (+ rad dwwid 0.5))
          cl3
           (polar cl2 ang1 ts)
          pt1
           (polar bccen ang2 rad)
          pt2
           (polar bccen ang2 (+ rad swwid))
          pt3
           (polar bccen ang3 rad)
          pt4
           (polar bccen ang3 (+ rad dwwid))
          pt5
           (polar bccen ang4 rad)
          pt6
           (polar bccen ang4 (+ rad dwwid))
          pt7
           (polar bccen ang5 rad)
          pt8
           (polar bccen ang5 (+ rad swwid))
        )
        (vl-cmdf "._undo" "_g")
        (haws-drawdw
          pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 left ang1 cl1 cl2 cl3 ts
         )
        (vl-cmdf "._arc" pt4 "_e" pt6 "_r" (+ rad dwwid) "._undo" "_e")
       )
       (t
        (setvar "osmode" 128)
        (setq
          ang1
           (getangle dwmid "\nSelect sidewalk: ")
          ang2
           (+ ang1 (/ pi 2))
          ang3
           (- ang1 (/ pi 2))
          dwwid
           (/ (* swwid (max (abs swwid) 5.0)) (abs swwid))
          dwlen
           (/ dwlen 2)
          left
           (minusp (cos (- ang1 0.7854)))
          cl1
           (polar dwmid ang1 0.5)
          cl2
           (polar cl1 ang1 dwwid)
          cl3
           (polar cl2 ang1 ts)
          pt3
           (polar dwmid ang2 dwlen)
          pt1
           (polar dwmid ang2 (+ dwlen 5))
          pt4
           (polar pt3 ang1 dwwid)
          pt2
           (polar pt1 ang1 swwid)
          pt5
           (polar dwmid ang3 dwlen)
          pt7
           (polar dwmid ang3 (+ dwlen 5))
          pt6
           (polar pt5 ang1 dwwid)
          pt8
           (polar pt7 ang1 swwid)
          dwlen
           (* dwlen 2)
        )
        (vl-cmdf "._undo" "_g")
        (haws-drawdw
          pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 left ang1 cl1 cl2 cl3 ts
         )
        (vl-cmdf "._line" pt4 pt6 "" "._undo" "_e")
       )
     )
  )
  (haws-vrstor)
  (haws-core-restore)
)
(defun haws-drawdw
              (pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 left ang1 cl1 cl2 cl3 ts
              )
  (setvar "osmode" 0)
  (vl-cmdf "._line" pt1 pt2 "")
  (vl-cmdf "._line" pt3 pt4 "")
  (vl-cmdf "._line" pt5 pt6 "")
  (vl-cmdf "._line" pt7 pt8 "")
  (vl-cmdf "._line" pt3 pt2 "")
  (vl-cmdf "._line" pt5 pt8 "")
  (vl-cmdf "._line" pt2 pt4 "")
  (vl-cmdf "._line" pt6 pt8 "")
  (haws-mklayr "DRIVEWAYTX")
  (vl-cmdf "._line" cl1 cl2 "")
  (haws-mktext
    (if left
      "MR"
      "ML"
    )
    cl3
    nil
    (if left
      (+ ang1 pi)
      ang1
    )
    "DW"
  )
  (haws-mklayr "DRIVEWAY")
)
