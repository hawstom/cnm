;;; Written by Thomas Gail Haws
;;; Change layer lineweights by picking.
(defun c:haws-lwp
             (/ ss cc i ec lc)
(haws-core-init 253)
  (cond
    ((/= (setq cc (getstring "\nNew layer lineweight (return to pick):"))
         ""
     )
     cc
    )
    ((setq cc (/ (cdr (assoc 370 (setq ec (entget (car (entsel)))))) 100))
     cc
    )
    (t
     (setq
       cc (abs
            (/ (cdr (assoc 370 (tblsearch "layer" (cdr (assoc 8 ec))))) 100)
          )
     )
    )
  )
  (prompt
    "\nLayers to change by picking (lineweights by entity won't change):"
  )
  (setq
    ss (ssget)
    i  0
  )
  (vl-cmdf "._layer")
  (while (setq ec (ssname ss i))
    (setq lc (cdr (assoc 8 (entget ec))))
    (vl-cmdf "_lw" cc lc)
    (setq i (1+ i))
  )
  (vl-cmdf "")
  (princ "lineweight ")
  cc
)
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Change layer lineweight by picking nested entities
(defun c:haws-lwpx
              (/ cc ec lopera nested)
(haws-core-init 254)
  (cond
    ((/= (setq cc (getstring "\nNew layer lineweight (return to pick):"))
         ""
     )
     cc
    )
    ((setq cc (/ (cdr (assoc 370 (setq ec (entget (car (entsel)))))) 100))
     cc
    )
    (t
     (setq
       cc (abs
            (/ (cdr (assoc 62 (tblsearch "layer" (cdr (assoc 8 ec))))) 100)
          )
     )
    )
  )
  (setq flist nil)
  (prompt (strcat "\nNested layers to change by picking: "))
  (vl-cmdf "._layer")
  (haws-lwpick)
  (vl-cmdf "")
  (princ "lineweight ")
  cc
)
(defun haws-lwpick
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
    (setq flist (cons la flist))
    (vl-cmdf "_lw" cc la)
  )
)
