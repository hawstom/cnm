;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-WS (/ stubpt mainpt ang1 LEFT TS)
  (haws-core-init 328)
  (HAWS-VSAVE '("clayer"))
  (setq ts (haws-text-height-model))
  (while (setq stubpt (getpoint "\nEnd of service line (Return to quit):"))
    (HAWS-MKLAYR "WATSER")
    (setq mainpt (getpoint stubpt "\nWater main connection point (use osnap):  "))
    (setq
      ang1 (angle mainpt stubpt)
      left (minusp (cos (- ang1 0.7854)))
    )
    (command "._line" stubpt mainpt "")
    (HAWS-MKLAYR "WATSERTX")
    (HAWS-MKTEXT
      (if left "_MR" "_ML")
      (polar stubpt ang1 ts)
      nil
      (if left (+ ang1 pi) ang1)
      "WS"
    )
  )
  (HAWS-VRSTOR)(haws-core-restore)
  (princ)
)
