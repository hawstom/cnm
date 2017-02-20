;Written by Thomas Gail Haws
(DEFUN C:HAWS-AC () (HAWS-AET (/ 1.0 43560) "AC"))
(DEFUN C:HAWS-SF () (HAWS-AET 1 "SF"))
(DEFUN C:HAWS-AET () (HAWS-SF))
(DEFUN C:HAWS-SM () (HAWS-AET (/ 1.0 27878400) "SQ. MI."))
(DEFUN C:HAWS-SY () (HAWS-AET (/ 1.0 9) "SY"))
(DEFUN
   HAWS-AET (FACTOR LABEL / AREA TS TXPT)
  (PROMPT "\nSelect circle or polyline:")
  (COMMAND "area" "e" PAUSE)
  (SETQ
    AREA (STRCAT
           (RTOS (* (GETVAR "AREA") FACTOR) 2 (GETVAR "luprec"))
           " "
           LABEL
         )
    TXPT (GETPOINT "\nMiddle point for text:")
    TS   (* (GETVAR "dimscale") (GETVAR "dimtxt"))
  )
  (PRINC TXPT)
  (IF TXPT
    (HAWS-MKTEXT "m" TXPT TS 0 AREA)
  )
  (PRINC (STRCAT "\n" AREA))
  (PRINC)
)
