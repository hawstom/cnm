;Written by Thomas Gail Haws
(DEFUN C:HAWS-ACRES ()
(haws-core-init 156) (HAWS-AET (/ 1.0 43560) " AC"))
(DEFUN C:HAWS-SF ()
(haws-core-init 157) (HAWS-AET 1 " SF"))
(DEFUN C:HAWS-AET ()
(haws-core-init 158) (HAWS-AET 1 ""))
(DEFUN C:HAWS-SM ()
(haws-core-init 159) (HAWS-AET (/ 1.0 27878400) " SQ. MI."))
(DEFUN C:HAWS-SY ()
(haws-core-init 160) (HAWS-AET (/ 1.0 9) " SY"))
(DEFUN
   HAWS-AET (FACTOR LABEL / AREA TS TXPT)
  (SETQ HAWS-QT-INSTANCE (HAWS-QT-NEW "haws-aet"))
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "type" "area")
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "factor" factor)
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "postfix" label)
  (HAWS-QT-SET-PROPERTY
    HAWS-QT-INSTANCE
    "precision"
    (GETVAR "luprec")
  )
  (HAWS-QT-STRING HAWS-QT-INSTANCE)
  (HAWS-QT-ADD-DRAWING-TEXT HAWS-QT-INSTANCE)
  (PRINC)
)
