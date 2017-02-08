;(C) Copyright 1997 by Thomas Gail Haws
;                          |<------------dwlen->|
;                          |                    |
;                   - 7----6--------------------14----15 -     
;                -         |         ^          |           -   
;             -            |       hcwid        |              -  
;          -               |         v          |                 - 
;       -                - 4--------------------12 -                 -
;    8                -   /|         ^          | \   -                 16
;    |             -    /  |         |          |   \    -              |
;    |          -     /    |         |          |     \     -           |
;    |       -      /      |       dwwid        |       \      -        |
;    |    -       /        |         |          |         \       -     |
;    5 3         2         1         v          9          10        11 13
;  ang6ang5angc ang4 ang3 ang2 anga ang1 anga  ang7 ang8  ang9 angc ang10ang11
;
(defun c:haws-m42 ( / ang1 ang2 ang3 ang4 ang5 ang6 ang7 ang8 ang9 ang10 ang11
  anga angb angc angd ange bccen cl1 cl2 cl3
  HAWS-drawdw dwmid dwwid incang left osmold pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8
  pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16 ts rad wngang)
  (defun HAWS-drawm42 ()
    (setvar "osmode" 0)
    (command "._line" pt1  pt6  "")
    (command "._line" pt2  pt4  "")
    (command "._line" pt3  pt4  "")
    (command "._line" pt5  pt8  "")
    (command "._line" pt8  pt7  "")
    (command "._line" pt9  pt14 "")
    (command "._line" pt10 pt12 "")
    (command "._line" pt11 pt12 "")
    (command "._line" pt13 pt16 "")
    (command "._line" pt16 pt15 "")
    (HAWS-MKLAYR "DRIVEWAYTX")
    (command "._line" cl1  cl2 "")
    (HAWS-MKTEXT
      (if left "MR" "ML")
      cl3
      ts
      (if left (+ ang1 pi) ang1)
      "DW"
    )
    (HAWS-MKLAYR "DRIVEWAY")
  )
  (HAWS-ERDF$@ 0)
  (HAWS-VSAVE '("clayer"))
  (setq
    osmold (getvar "osmode")
    hcwid (if hcwid hcwid 4.0)
    dwlen(if dwlen dwlen 20.0)
    swwid (if swwid swwid 4.0)
    ts (* (getvar "dimscale")(getvar"dimtxt"))
  )
  (setvar "osmode" 0)
  (HAWS-MKLAYR "DRIVEWAY")
  (while
    (progn
      (initget "Dw Sw")
      (prompt "\nCity of Mesa M-42 driveway entrance.")
      (prompt "  Current SW width = ")(princ swwid)
      (prompt "  DW length = ")(princ dwlen)
      (setvar "osmode" osmold)
      (setq
;        dwwid (/ (* swwid (max (abs swwid) 5.0)) (abs swwid))
        dwwid 12.0
        hcwid 3.0
        dwmid(getpoint "\nDw length/Sw width(neg. for inside of curve)/<Select drive midpoint at back of curb>: ")
      )
    )
    (cond
      ( (= "Sw" dwmid)
        (setq swwid (getreal "\nSidewalk width (negative for inside of curve): "))
      )
      ( (= "Dw" dwmid)
        (setq dwlen (getreal "\nDriveway length: "))
      )
      ( (setq bccen (osnap dwmid "cen"))
        (setq
          ang1 (angle bccen dwmid)    rad (distance bccen dwmid)
          anga (/ dwlen 2.0 rad) angb (/ 4.0 rad) angc (/ 1.0 rad)
          angd (/ 5.0 rad) ange (/ (- 6.509 (* swwid 0.8333)) rad)
          ang2 (+ ang1 anga) ang3 (+ ang2  angb)
          ang4 (+ ang3 angc) ang5 (+ ang4  angd)
          ang6 (+ ang5 ange) ang7 (- ang1  anga)
          ang8 (- ang7 angb) ang9 (- ang8  angc)
          ang10(- ang9 angd) ang11(- ang10 ange)
          left (minusp (cos (- ang1 0.7854)))
          cl1 (polar bccen ang1 (+ rad 0.5))
          cl2 (polar bccen ang1 (+ rad dwwid hcwid 0.5))
          cl3 (polar cl2 ang1 ts)
          pt1  (polar bccen ang2  rad)
          pt2  (polar bccen ang4  rad)
          pt3  (polar bccen ang5  rad)
          pt4  (polar bccen ang2  (+ rad dwwid))
          pt5  (polar bccen ang6  rad)
          pt6  (polar bccen ang2  (+ rad dwwid hcwid))
          pt7  (polar bccen ang3  (+ rad dwwid hcwid))
          pt8  (polar bccen ang6  (+ rad swwid))
          pt9  (polar bccen ang7  rad)
          pt10 (polar bccen ang9  rad)
          pt11 (polar bccen ang10 rad)
          pt12 (polar bccen ang7  (+ rad dwwid))
          pt13 (polar bccen ang11 rad)
          pt14 (polar bccen ang7  (+ rad dwwid hcwid))
          pt15 (polar bccen ang8  (+ rad dwwid hcwid))
          pt16 (polar bccen ang11 (+ rad swwid))
        )
        (HAWS-drawm42)
        (command "._arc" pt12 "e" pt4 "r" (+ rad dwwid))
        (command "._arc" pt15 "e" pt7 "r" (+ rad dwwid hcwid))
      )
      ( T
        (setvar "osmode" 128)
        (setq
          ang1 (getangle dwmid "\nSelect sidewalk: ")
          ang2 (+ ang1 (/ pi 2)) ang3 (- ang1 (/ pi 2))
          dwlen (/ dwlen 2)
          left (minusp (cos (- ang1 0.7854)))
          cl1 (polar dwmid ang1 0.5)
          cl2 (polar cl1 ang1 (+ dwwid hcwid))
          cl3 (polar cl2 ang1 ts)
          pt1  (polar dwmid ang2 dwlen)
          pt2  (polar dwmid ang2 (+ dwlen 5.0))
          pt3  (polar dwmid ang2 (+ dwlen 10.0))
          pt4  (polar pt1   ang1 dwwid)
          pt5  (polar dwmid ang2 (+ dwlen 10.0 (- 6.509 (* swwid 0.8333))))
          pt6  (polar pt1   ang1 (+ dwwid hcwid))
          pt7  (polar pt6   ang2 4.0)
          pt8  (polar pt5   ang1 swwid)
          pt9  (polar dwmid ang3 dwlen)
          pt10 (polar dwmid ang3 (+ dwlen 5.0))
          pt11 (polar dwmid ang3 (+ dwlen 10.0))
          pt12 (polar pt9   ang1 dwwid)
          pt13 (polar dwmid ang3 (+ dwlen 10.0 (- 6.509 (* swwid 0.8333))))
          pt14 (polar pt9   ang1 (+ dwwid hcwid))
          pt15 (polar pt14  ang3 4.0)
          pt16 (polar pt13  ang1 swwid)
          dwlen (* dwlen 2)
        )
        (HAWS-drawm42)
        (command "._line" pt4 pt12 "")
        (command "._line" pt7 pt15 "")
      )
  ) )
  (HAWS-VRSTOR)(HAWS-ERRRST)
)

