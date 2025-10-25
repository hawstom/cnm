;(C) Copyright 1997 by Thomas Gail Haws
;;Rotate from a reference point.
(defun c:haws-rotatebase ( / sset pt1 ang1)
(haws-core-init 306)
(prompt (strcat "\n" (haws_evangel_msg)))
  (setq
    sset (ssget)
    pt1 (getpoint "\nBase point of rotation: ")
  )
  (vl-cmdf "._rotate" sset "" pt1 "_R" pt1)
  (princ)
)
