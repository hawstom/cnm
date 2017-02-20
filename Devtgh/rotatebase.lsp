;(C) Copyright 1997 by Thomas Gail Haws
;;Rotate from a reference point.
(defun c:haws-rotatebase ( / sset pt1 ang1)
  (setq
    sset (ssget)
    pt1 (getpoint "\nBase point of rotation: ")
  )
  (command "._rotate" sset "" pt1 "R" pt1)
  (princ)
)
