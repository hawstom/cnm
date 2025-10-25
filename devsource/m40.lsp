;(C) Copyright 1997 by Thomas Gail Haws
;                          |<------------dwlen->|
;                          |                    |
;              7-6-----------------------------------------1415
;             /  |                   ^                     |   \
;           /    |                 hcwid                   |     \
;         /      |                   v                     |       \
;       /        5---------4--------------------12---------13        \
;      8         |        /|         ^          | \        |         16
;      |         |      /  |         |          |   \      |         |
;      |         |    /    |         |          |     \    |         |
;      |         |  /      |       dwwid        |       \  |         |
;      |         |/        |         |          |         \|         |
;      3         2         1         v          9          10        11
;     ang4 angc ang3 angb ang2 anga ang1 anga  ang5 angb  ang6 angc ang7
;
(defun c:haws-m40 ( / ang1 ang2 ang3 ang4 ang5 bccen cl1 cl2 cl3
  dwmid dwwid incang left osmold pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8
  pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16 ts rad wngang)
(haws-core-init 85)
  (defun haws-drawm40 ()
    (setvar "osmode" 0)
    (vl-cmdf "._line" pt1  pt4  "")
    (vl-cmdf "._line" pt2  pt4  "")
    (vl-cmdf "._line" pt2  pt6  "")
    (vl-cmdf "._line" pt3  pt8  "")
    (vl-cmdf "._line" pt8  pt7  "")
    (vl-cmdf "._line" pt9  pt12 "")
    (vl-cmdf "._line" pt10 pt12 "")
    (vl-cmdf "._line" pt10 pt14 "")
    (vl-cmdf "._line" pt11 pt16 "")
    (vl-cmdf "._line" pt16 pt15 "")
    (haws-mklayr "DRIVEWAYTX")
    (vl-cmdf "._line" cl1  cl2 "")
    (haws-mktext
      (if left "MR" "ML")
      cl3
      ts
      (if left (+ ang1 pi) ang1)
      "DW"
    )
    (haws-mklayr "DRIVEWAY")
  )
  (haws-vsave '("clayer"))
  (setq
    osmold (getvar "osmode")
    hcwid (if hcwid hcwid 4.0)
    dwlen(if dwlen dwlen 20.0)
    swwid (if swwid swwid 4.0)
    ts (* (haws-dwgscale)(getvar"dimtxt"))
  )
  (haws-mklayr "DRIVEWAY")
  (while
    (progn
      (initget "Hc Dw Sw")
      (prompt "\nCity of Mesa M-40.1 driveway entrance.)
      (prompt \nCurrent HC area width = ")(princ hcwid)
      (prompt "   SW width = ")(princ swwid)
      (prompt "   DW width= ")(princ dwlen)
      (setq dwmid(getpoint "\nHc area width/Dw width/Sw width(neg. for inside)/<Select drive midpoint at back of curb>: "))
    )
    (cond
      ( (= "Hc" dwmid)
        (setq hcwid (getdist "\nWidth of HC area (3 or 4): "))
      )
      ( (= "Dw" dwmid)
        (setq dwlen (getdist "\nDriveway width at back of curb: "))
      )
      ( (= "Sw" dwmid)
        (setq swwid (getdist "\nSidewalk width (negative for inside of curve): "))
      )
      ( (setq bccen (osnap dwmid "cen"))
        (vl-cmdf "._undo" "g")
        (setq
          dwwid 5.0
          ang1 (angle bccen dwmid)    rad (distance bccen dwmid)
          anga (/ dwlen 2.0 rad) angb (/ 5.0 rad)
          angc (/ 1.0 rad) angd (/ 4.0 rad)
          ang2 (+ ang1 anga) ang3 (+ ang2 angb)
          ang4 (+ ang3 angc) ang5 (+ ang4 angd)
          ang6 (- ang1 anga) ang7 (- ang6 angb)
          ang8 (- ang7 angc) ang9 (- ang8 angd)
          left (minusp (cos (- ang1 0.7854)))
          cl1 (polar bccen ang1 (+ rad 0.5))
          cl2 (polar bccen ang1 (+ rad dwwid hcwid 0.5))
          cl3 (polar cl2 ang1 ts)
          pt1  (polar bccen ang2 rad)
          pt2  (polar bccen ang3 rad)
          pt3  (polar bccen ang5 rad)
          pt4  (polar bccen ang2 (+ rad dwwid))
          pt5  (polar bccen ang3 (+ rad dwwid))
          pt6  (polar bccen ang3 (+ rad dwwid hcwid))
          pt7  (polar bccen ang4 (+ rad dwwid hcwid))
          pt8  (polar bccen ang5 (+ rad swwid))
          pt9  (polar bccen ang6 rad)
          pt10 (polar bccen ang7 rad)
          pt11 (polar bccen ang9 rad)
          pt12 (polar bccen ang6 (+ rad dwwid))
          pt13 (polar bccen ang7 (+ rad dwwid))
          pt14 (polar bccen ang7 (+ rad dwwid hcwid))
          pt15 (polar bccen ang8 (+ rad dwwid hcwid))
          pt16 (polar bccen ang9 (+ rad swwid))
        )
        (haws-drawm40)
        (vl-cmdf "._arc" pt13 "e" pt5 "r" (+ rad dwwid))
        (vl-cmdf "._arc" pt15 "e" pt7 "r" (+ rad dwwid hcwid))
        (setvar "osmode" osmold)
        (vl-cmdf "._undo" "e")
      )
      ( t
        (vl-cmdf "._undo" "g")
        (setvar "osmode" 128)
        (setq
          ang1 (getangle dwmid "\nSelect sidewalk: ")
          ang2 (+ ang1 (/ pi 2)) ang3 (- ang1 (/ pi 2))
;          dwwid (/ (* swwid (max (abs swwid) 5.0)) (abs swwid))
          dwwid 5.0
          dwlen (/ dwlen 2)
          left (minusp (cos (- ang1 0.7854)))
          cl1 (polar dwmid ang1 0.5)
          cl2 (polar cl1 ang1 (+ dwwid hcwid))
          cl3 (polar cl2 ang1 ts)
          pt1  (polar dwmid ang2 dwlen)
          pt2  (polar dwmid ang2 (+ dwlen 5.0))
          pt3  (polar dwmid ang2 (+ dwlen 10.0))
          pt4  (polar pt1   ang1 dwwid)
          pt5  (polar pt2   ang1 dwwid)
          pt6  (polar pt2   ang1 (+ dwwid hcwid))
          pt7  (polar pt6   ang2 1.0)
          pt8  (polar pt3   ang1 swwid)
          pt9  (polar dwmid ang3 dwlen)
          pt10 (polar dwmid ang3 (+ dwlen 5.0))
          pt11 (polar dwmid ang3 (+ dwlen 10.0))
          pt12 (polar pt9   ang1 dwwid)
          pt13 (polar pt10  ang1 dwwid)
          pt14 (polar pt10  ang1 (+ dwwid hcwid))
          pt15 (polar pt14  ang3 1.0)
          pt16 (polar pt11  ang1 swwid)
          dwlen (* dwlen 2)
        )
        (haws-drawm40)
        (vl-cmdf "._line" pt5 pt13 "")
        (vl-cmdf "._line" pt7 pt15 "")
        (setvar "osmode" osmold)
        (vl-cmdf "._undo" "e")
      )
  ) )
  (haws-vrstor)(haws-core-restore)
)
