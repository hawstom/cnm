;(C) Copyright 2001 by Thomas Gail Haws
;                          |<------------dwwid->|
;                          |                    |
;              7------------6--------------------14------------15             ----
;        angd /      angc   |         ^          |     angc      \ angd         ^
;           /               |       hcdep        |                 \            |
;         /                 |         v          |                   \          |
;       /         5---------4--------------------12---------13         \        |
;      8          |\        |         ^          |        / |          16     totdep
;      |          |  \      |         |          |      /   |          |        |
;      |          |    \    |         |          |    /     |          |        |
;      |          |      \  |       dwdep        |  /       |          |        |
;      |          |        \|         |          |/         |          |        |
;      3          2         1         v          9          10         11       v
;     ang5  ang4 ang3 angb ang2 anga ang1 anga  ang6 angb  ang7 ang8  ang9     --- Back of curb
;
(defun c:haws-mc2033 ( / ang1 ang2 ang3 ang4 ang5 bccen cl1 cl2 cl3
  dwmid dwdep incang left osmold pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8
  pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16 ts rad wngang)
(haws-core-init 87)
  (defun HAWS-drawmc2033 ()
    (setvar "osmode" 0)
    (command "._line" pt1  pt6  "")
    (command "._line" pt1  pt5  "")
    (command "._line" pt2  pt5  "")
    (command "._line" pt3  pt8  "")
    (command "._line" pt8  pt7  "")
    (command "._line" pt9  pt14 "")
    (command "._line" pt9  pt13 "")
    (command "._line" pt10 pt13 "")
    (command "._line" pt11 pt16 "")
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
  (HAWS-VSAVE '("clayer"))
  (setq
    osmold (getvar "osmode")
    hcdep (if hcdep hcdep 3.0)
    dwwid(if dwwid dwwid 20.0)
    swwid (if swwid swwid 4.0)
    totdep (if totdep totdep (+ (max swwid 5.0) hcdep))
    ts (* (HAWS-DWGSCALE)(getvar"dimtxt"))
  )
  (HAWS-MKLAYR "DRIVEWAY")
  (while
    (progn
      (initget "Hc Total Dw Sw ")
      (prompt "\nMCDOT 2033 driveway entrance.)
      (prompt \nCurrent HC area depth (front to back) = ")(princ hcdep)
      (prompt "   Total DW depth (front to back) = ")(princ totdep)
      (prompt "   SW width (front to back) = ")(princ swwid)
      (prompt "   DW width (side to side) = ")(princ dwwid)
      (setq dwmid(getpoint "\nHc area width/Total depth (10' max)/Dw width/Sw width(neg. for inside)/<Select drive midpoint at back of curb>: "))
    )
    (cond
      ( (= "Hc" dwmid)
        (setq hcdep (getdist "\nDepth of HC area (3' or 5' min.): "))
      )
      ( (= "Total" dwmid)
        (setq totdep (getdist "\nTotal depth of driveway entrance back from back of curb (10' max): "))
      )
      ( (= "Dw" dwmid)
        (setq dwwid (getdist "\nDriveway width at back of curb: "))
      )
      ( (= "Sw" dwmid)
        (setq swwid (getdist "\nSidewalk width (negative for inside of curve): "))
      )
      ( (setq bccen (osnap dwmid "cen"))
        (command "._undo" "g")
        (setq
          dwdep (- totdep hcdep)
          ang1 (angle bccen dwmid)    rad (distance bccen dwmid)
          anga (/ dwwid 2.0 rad) angb (/ swwid rad)
          angc (/ (* 1.5 swwid) rad) angd (/ (- totdep swwid) rad)
          ang2 (+ ang1 anga) ang3 (+ ang2 angb)
          ang4 (+ ang2 angc) ang5 (+ ang4 angd)
          ang6 (- ang1 anga) ang7 (- ang6 angb)
          ang8 (- ang6 angc) ang9 (- ang8 angd)
          left (minusp (cos (- ang1 0.7854)))
          cl1 (polar bccen ang1 (+ rad 0.5))
          cl2 (polar bccen ang1 (+ rad dwdep hcdep 0.5))
          cl3 (polar cl2 ang1 ts)
          pt1  (polar bccen ang2 rad)
          pt2  (polar bccen ang3 rad)
          pt3  (polar bccen ang5 rad)
          pt4  (polar bccen ang2 (+ rad dwdep))
          pt5  (polar bccen ang3 (+ rad dwdep))
          pt6  (polar bccen ang2 (+ rad dwdep hcdep))
          pt7  (polar bccen ang4 (+ rad dwdep hcdep))
          pt8  (polar bccen ang5 (+ rad swwid))
          pt9  (polar bccen ang6 rad)
          pt10 (polar bccen ang7 rad)
          pt11 (polar bccen ang9 rad)
          pt12 (polar bccen ang6 (+ rad dwdep))
          pt13 (polar bccen ang7 (+ rad dwdep))
          pt14 (polar bccen ang6 (+ rad dwdep hcdep))
          pt15 (polar bccen ang8 (+ rad dwdep hcdep))
          pt16 (polar bccen ang9 (+ rad swwid))
        )
        (HAWS-drawmc2033)
        (command "._arc" pt13 "e" pt5 "r" (+ rad dwdep))
        (command "._arc" pt15 "e" pt7 "r" (+ rad dwdep hcdep))
        (setvar "osmode" osmold)
        (command "._undo" "e")
      )
      ( T
        (command "._undo" "g")
        (setvar "osmode" 128)
        (setq
          ang1 (getangle dwmid "\nSelect sidewalk: ")
          ang2 (+ ang1 (/ pi 2)) ang3 (- ang1 (/ pi 2))
          dwdep (- totdep hcdep)
          dwwid (/ dwwid 2)
          left (minusp (cos (- ang1 0.7854)))
          cl1 (polar dwmid ang1 0.5)
          cl2 (polar cl1 ang1 (+ dwdep hcdep))
          cl3 (polar cl2 ang1 ts)
          pt1  (polar dwmid ang2 dwwid)
          pt2  (polar dwmid ang2 (+ dwwid swwid))
          pt3  (polar dwmid ang2 (+ dwwid (* swwid 1.5) (- totdep swwid)))
          pt4  (polar pt1   ang1 dwdep)
          pt5  (polar pt2   ang1 dwdep)
          pt6  (polar pt1   ang1 (+ dwdep hcdep))
          pt7  (polar pt6   ang2 (* swwid 1.5))
          pt8  (polar pt3   ang1 swwid)
          pt9  (polar dwmid ang3 dwwid)
          pt10 (polar dwmid ang3 (+ dwwid swwid))
          pt11 (polar dwmid ang3 (+ dwwid (* swwid 1.5) (- totdep swwid)))
          pt12 (polar pt9   ang1 dwdep)
          pt13 (polar pt10  ang1 dwdep)
          pt14 (polar pt9   ang1 (+ dwdep hcdep))
          pt15 (polar pt14  ang3 (* swwid 1.5))
          pt16 (polar pt11  ang1 swwid)
          dwwid (* dwwid 2)
        )
        (HAWS-drawmc2033)
        (command "._line" pt5 pt13 "")
        (command "._line" pt7 pt15 "")
        (setvar "osmode" osmold)
        (command "._undo" "e")
      )
  ) )
  (HAWS-VRSTOR)(haws-core-restore)
)
