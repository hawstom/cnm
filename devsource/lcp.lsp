;;; Written by Thomas Gail Haws
;;; Change layer colors by picking.
(defun c:haws-lcp
             (/ ss cc i ec lc)
(haws-core-init 240)
  (cond
    ((/= (setq cc (getstring "\nNew layer color (return to pick):"))
         ""
     )
     cc
    )
    ((setq cc (cdr (assoc 62 (setq ec (entget (car (entsel)))))))
     cc
    )
    (t
     (setq
       cc (abs
            (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 ec)))))
          )
     )
    )
  )
  (prompt
    "\nLayers to change by picking (Colors by entity won't change):"
  )
  (setq
    ss (ssget)
    i  0
  )
  (vl-cmdf "._layer")
  (while (setq ec (ssname ss i))
    (setq lc (cdr (assoc 8 (entget ec))))
    (vl-cmdf "_c" cc lc)
    (setq i (1+ i))
  )
  (vl-cmdf "")
  (princ "Color ")
  cc
)
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Change layer color by picking nested entities
(defun c:haws-lcpx
              (/ cc ec lopera nested)
(haws-core-init 241)
(princ (haws-evangel-msg))
  (cond
    ((/= (setq cc (getstring "\nNew layer color (return to pick):"))
         ""
     )
     cc
    )
    ((setq cc (cdr (assoc 62 (setq ec (entget (car (entsel)))))))
     cc
    )
    (t
     (setq
       cc (abs
            (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 ec)))))
          )
     )
    )
  )
  (prompt (strcat "\nNested layers to change by picking: "))
  (vl-cmdf "._layer")
  (haws-lcpick)
  (vl-cmdf "")
  (princ "Color ")
  cc
)
(defun haws-lcpick
              (/ es en la parntl vp)
  (while (setq es (nentsel))
    (setq
      en     (car es)
      la     (cdr (assoc 8 (entget en)))
      parntl (cadddr es)
    )
    (while (= la "0")
      (setq la (cdr (assoc 8 (entget (car parntl)))))
      (setq parntl (cdr parntl))
    )
    (redraw en 3)
    (vl-cmdf "_c" cc la)
  )
)
