;Written by Thomas Gail Haws
(defun c:haws-ACRES (/ area TS TXPT)
  (prompt "\nSelect circle or polyline:")
  (command "area" "e" pause)
  (setq
    area (strcat (rtos (/(getvar "AREA") 43560) 2 (getvar "luprec")) " AC")
    txpt (getpoint "\nMiddle point for text:")
    ts (* (getvar"dimscale")(getvar "dimtxt"))
  )
  (if txpt
    (HAWS-MKTEXT "m" txpt ts 0 area)
    (command "text" "" area)
  )
  (princ)
)
