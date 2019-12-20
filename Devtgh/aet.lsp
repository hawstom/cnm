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
  (PROMPT "\nSelect circle or polyline:")
  (COMMAND "._area" "_e" PAUSE)
  (SETQ
    AREA (STRCAT
           (RTOS (* (GETVAR "AREA") FACTOR) 2 (GETVAR "luprec"))
           LABEL
         )
    TXPT (GETPOINT "\nMiddle point for text:")
    TS   (* (GETVAR "dimscale") (GETVAR "dimtxt"))
  )
  (PRINC TXPT)
  (IF TXPT
    (HAWS-MKTEXT "m" TXPT nil 0 AREA)
  )
  (PRINC (STRCAT "\n" AREA))
  (PRINC)
)
