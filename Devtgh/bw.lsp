;Backwards leader, the arrow is the last point picked
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-bw ( / ANG1 PT4 as pt1 pt2 pt3)
  (haws-core-init 167)
  (HAWS-VSAVE '("dimpost"))
  (HAWS-MKLAYR "BWLDR")
  (setvar "dimpost" "")
  (setq
    as (* (HAWS-DWGSCALE)(getvar "dimasz"))
    pt1 (getpoint "\nLeader connection point: ")
    pt2 (getpoint pt1 "\nLeader arrow point: ")
    pt3 (polar pt1 (if (minusp (cos (angle pt1 pt2))) pi 0) as)
    ang1 (angle pt2 pt3)
    pt4 (polar pt2 ang1 as)
  )
  (cond
    ( (>= (atof(getvar "acadver"))14)
      (vl-cmdf "._leader" pt2 pt1 "" " " "")
    )
    ( T
      (vl-cmdf "._pline" pt2 "_w" 0 (/ as 3) pt4 "_w" 0 0 pt3 pt1 "")(vl-cmdf)
    )
  )
  (HAWS-VRSTOR)(haws-core-restore)
)
