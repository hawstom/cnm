;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-WS (/ stubpt mainpt ang1 LEFT TS)
  (HAWS-ERDF$@ 0)
  (HAWS-VSAVE '("clayer"))
  (setq ts (* (HAWS-DWGSCALE)(getvar "dimtxt")))
  (while (setq stubpt (getpoint "\nEnd of service line (Return to quit):"))
    (HAWS-MKLAYR "WATSER")
    (setq mainpt (getpoint stubpt "\nWater main connection point (use osnap):  "))
    (setq
      ang1 (angle mainpt stubpt)
      left (minusp (cos (- ang1 0.7854)))
    )
    (command "line" stubpt mainpt "")
    (HAWS-MKLAYR "WATSERTX")
    (HAWS-MKTEXT
      (if left "MR" "ML")
      (polar stubpt ang1 ts)
      ts
      (if left (+ ang1 pi) ang1)
      "WS"
    )
  )
  (HAWS-VRSTOR)(HAWS-ERRRST)
  (princ)
)
