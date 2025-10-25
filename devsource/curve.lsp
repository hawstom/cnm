;Written by Thomas Gail Haws
(defun c:haws-curve ( / ang1 ang2 stpt cenpt radius ang)
(haws-core-init 214)
  (setq
    stpt (getpoint "\nStart point of curve:")
    cenpt (getpoint "\nCenter:")
    radius (distance stpt cenpt)
    ang1 (angle cenpt stpt)
    ang (/(getdist "\nLength of curve (+ = ccw, - = cw):")radius)
    ang2 (+ ang1 ang)
  )
  (entmake
    (list
      (cons 0 "ARC")
      (cons 10 cenpt)
      (cons 50 (if (minusp ang) ang2 ang1))
      (cons 51 (if (minusp ang) ang1 ang2))
      (cons 40 radius)
    )
  )
  (princ)
)
