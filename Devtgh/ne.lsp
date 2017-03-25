;N,E Coordinates on Leader Lines
;Written by Thomas Gail Haws
(defun c:haws-NE (/ dg txht pnt1 p1e p1n ang1 left ptxt)
  (haws-borrow 0)
  (setq
    txht (* (HAWS-DWGSCALE)(getvar "dimtxt"))
    pnt1 (getpoint "\nPick coordinate point:")
    p1n (strcat "N " (rtos(cadr pnt1)))   p1e (strcat "E " (rtos(car pnt1)))
    ptxt (getpoint pnt1 "\nPick text location: ")
    ang1 (angle pnt1 ptxt)
    left (minusp (cos ang1))
  )
  (cond
    ( (>= (atof(getvar "acadver"))14)
      (command "._leader" pnt1 ptxt "" "" "None")
    )
    ( T
      (command "dim" "leader" pnt1 ptxt)(command)
    )
  )
  (setq ptxt (polar ptxt (if left pi 0) (* txht 0.5)))
  (HAWS-MKTEXT (if left "mr" "ml") ptxt txht 0 p1n)
  (setq ptxt (polar ptxt(/ pi -2)(* 1.667 txht)))
  (HAWS-MKTEXT (if left "mr" "ml") ptxt txht 0 p1e)
  (haws-return)(princ)
) ;end
