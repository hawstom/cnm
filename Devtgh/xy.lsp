;X,Y Coordinates on Leader Lines
;Written by Thomas Gail Haws
(defun c:haws-XY (/ dg txht pnt1 p1x p1y ang1 left ptxt)
  (haws-core-init 334)
  (setq
    txht (haws-text-height-model)
    pnt1 (getpoint "\nPick coordinate point:")
    p1y (strcat "Y=" (rtos(cadr pnt1)))   p1x (strcat "X=" (rtos(car pnt1)))
    ptxt (getpoint pnt1 "\nPick text location: ")
    ang1 (angle pnt1 ptxt)
    left (minusp (cos ang1))
  )
  (cond
    ( (>= (atof(getvar "acadver"))14)
      (command "._leader" pnt1 ptxt "" "" "_None")
    )
    ( T
      (command "._dim" "_leader" pnt1 ptxt)(command)
    )
  )
  (setq ptxt (polar ptxt (if left pi 0) (* txht 0.5)))
  (HAWS-MKTEXT (if left "mr" "ml") ptxt nil 0 p1x)
  (setq ptxt (polar ptxt(/ pi -2)(* 1.667 txht)))
  (HAWS-MKTEXT (if left "mr" "ml") ptxt nil 0 p1y)
  (haws-core-restore)(princ)
) ;end
