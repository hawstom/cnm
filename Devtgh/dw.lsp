;;; (C) Copyright 1997 by Thomas Gail Haws
(DEFUN
   C:HAWS-DW
            (/ ANG1 ANG2 ANG3 ANG4 ANG5 BCCEN CL1 CL2 CL3 DWMID DWWID
             INCANG LEFT OSMOLD PT1 PT2 PT3 PT4 PT5 PT6 PT7 PT8 TS RAD
             WNGANG
            )
  (HAWS-ERDF$@ 0)
  (HAWS-VSAVE '("clayer"))
  (SETQ
    OSMOLD
     (GETVAR "osmode")
    DWLEN
     (IF DWLEN
       DWLEN
       20.0
     )
    SWWID
     (IF SWWID
       SWWID
       4.0
     )
  )
  (SETVAR "osmode" 0)
  (HAWS-MKLAYR "DRIVEWAY")
  (SETQ TS (* (HAWS-DWGSCALE) (GETVAR "dimtxt")))
  (WHILE
    (PROGN
      (INITGET "Length Sw")
      (PROMPT "\nCurrent SW width = ")
      (PRINC SWWID)
      (PROMPT "   Length dw= ")
      (PRINC DWLEN)
      (SETVAR "osmode" OSMOLD)
      (SETQ
        DWMID
         (GETPOINT
           "\nLength/Sw width(neg. for inside)/<Select drive midpoint at back of curb>: "
         )
      )
    )
     (COND
       ((= "Length" DWMID)
        (SETQ DWLEN (GETREAL "\nDriveway length at back of curb: "))
       )
       ((= "Sw" DWMID)
        (SETQ
          SWWID
           (GETREAL
             "\nSidewalk width (negative for inside of curve): "
           )
        )
       )
       ((SETQ BCCEN (OSNAP DWMID "cen"))
        (SETQ
          DWWID
           (/ (* SWWID (MAX (ABS SWWID) 5.0)) (ABS SWWID))
          ANG1
           (ANGLE BCCEN DWMID)
          RAD
           (DISTANCE BCCEN DWMID)
          INCANG
           (/ DWLEN RAD)
          WNGANG
           (/ 5.0 RAD)
          ANG2
           (- ANG1 (/ INCANG 2.0) WNGANG)
          ANG3
           (+ ANG2 WNGANG)
          ANG4
           (+ ANG3 INCANG)
          ANG5
           (+ ANG4 WNGANG)
          LEFT
           (MINUSP (COS (- ANG1 0.7854)))
          CL1
           (POLAR BCCEN ANG1 (+ RAD 0.5))
          CL2
           (POLAR BCCEN ANG1 (+ RAD DWWID 0.5))
          CL3
           (POLAR CL2 ANG1 TS)
          PT1
           (POLAR BCCEN ANG2 RAD)
          PT2
           (POLAR BCCEN ANG2 (+ RAD SWWID))
          PT3
           (POLAR BCCEN ANG3 RAD)
          PT4
           (POLAR BCCEN ANG3 (+ RAD DWWID))
          PT5
           (POLAR BCCEN ANG4 RAD)
          PT6
           (POLAR BCCEN ANG4 (+ RAD DWWID))
          PT7
           (POLAR BCCEN ANG5 RAD)
          PT8
           (POLAR BCCEN ANG5 (+ RAD SWWID))
        )
        (COMMAND "._undo" "g")
        (HAWS-DRAWDW
          PT1 PT2 PT3 PT4 PT5 PT6 PT7 PT8 LEFT ANG1 CL1 CL2 CL3 TS
         )
        (COMMAND "._arc" PT4 "e" PT6 "r" (+ RAD DWWID) "._undo" "e")
       )
       (T
        (SETVAR "osmode" 128)
        (SETQ
          ANG1
           (GETANGLE DWMID "\nSelect sidewalk: ")
          ANG2
           (+ ANG1 (/ PI 2))
          ANG3
           (- ANG1 (/ PI 2))
          DWWID
           (/ (* SWWID (MAX (ABS SWWID) 5.0)) (ABS SWWID))
          DWLEN
           (/ DWLEN 2)
          LEFT
           (MINUSP (COS (- ANG1 0.7854)))
          CL1
           (POLAR DWMID ANG1 0.5)
          CL2
           (POLAR CL1 ANG1 DWWID)
          CL3
           (POLAR CL2 ANG1 TS)
          PT3
           (POLAR DWMID ANG2 DWLEN)
          PT1
           (POLAR DWMID ANG2 (+ DWLEN 5))
          PT4
           (POLAR PT3 ANG1 DWWID)
          PT2
           (POLAR PT1 ANG1 SWWID)
          PT5
           (POLAR DWMID ANG3 DWLEN)
          PT7
           (POLAR DWMID ANG3 (+ DWLEN 5))
          PT6
           (POLAR PT5 ANG1 DWWID)
          PT8
           (POLAR PT7 ANG1 SWWID)
          DWLEN
           (* DWLEN 2)
        )
        (COMMAND "._undo" "g")
        (HAWS-DRAWDW
          PT1 PT2 PT3 PT4 PT5 PT6 PT7 PT8 LEFT ANG1 CL1 CL2 CL3 TS
         )
        (COMMAND "._line" PT4 PT6 "" "._undo" "e")
       )
     )
  )
  (HAWS-VRSTOR)
  (HAWS-ERRRST)
)
(DEFUN
   HAWS-DRAWDW
              (PT1 PT2 PT3 PT4 PT5 PT6 PT7 PT8 LEFT ANG1 CL1 CL2 CL3 TS
              )
  (SETVAR "osmode" 0)
  (COMMAND "._line" PT1 PT2 "")
  (COMMAND "._line" PT3 PT4 "")
  (COMMAND "._line" PT5 PT6 "")
  (COMMAND "._line" PT7 PT8 "")
  (COMMAND "._line" PT3 PT2 "")
  (COMMAND "._line" PT5 PT8 "")
  (COMMAND "._line" PT2 PT4 "")
  (COMMAND "._line" PT6 PT8 "")
  (HAWS-MKLAYR "DRIVEWAYTX")
  (COMMAND "._line" CL1 CL2 "")
  (HAWS-MKTEXT
    (IF LEFT
      "MR"
      "ML"
    )
    CL3
    TS
    (IF LEFT
      (+ ANG1 PI)
      ANG1
    )
    "DW"
  )
  (HAWS-MKLAYR "DRIVEWAY")
)
