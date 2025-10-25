(defun c:haws-ltp
             (/ ss cc i ec lc)
(haws-core-init 251)
  (cond
    ((/= (setq cc (getstring "\nNew layer linetype (return to pick):"))
         ""
     )
     cc
    )
    ((setq
       cc (cdr
            (assoc
              6
              (setq ec (entget (car (nentsel "\nSelect linetype: "))))
            )
          )
     )
     cc
    )
    (t
     (setq
       cc
        (cdr (assoc 6 (tblsearch "layer" (cdr (assoc 8 ec)))))
     )
     (cond
       ((wcmatch cc "*|*")
        (alert
          "Can't set to external linetype.\nWill try using bare linetype name."
        )
        (while (wcmatch (setq cc (substr cc 2)) "*|*"))
       )
     )
    )
  )
  (prompt
    "\nLayers to change by picking (Linetypes by entity won't change):"
  )
  (setq
    ss (ssget)
    i  0
  )
  (vl-cmdf "._layer")
  (while (setq ec (ssname ss i))
    (setq lc (cdr (assoc 8 (entget ec))))
    (vl-cmdf "_lt" cc lc)
    (setq i (1+ i))
  )
  (vl-cmdf "")
  (princ "Linetype ")
  cc
)
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Change layer color by picking nested entities
(defun c:haws-ltpx
              (/ cc ec lopera nested)
(haws-core-init 252)
  (cond
    ((/= (setq cc (getstring "\nNew layer linetype (return to pick):"))
         ""
     )
     cc
    )
    ((setq
       cc (cdr
            (assoc
              6
              (setq ec (entget (car (nentsel "\nSelect linetype: "))))
            )
          )
     )
     cc
    )
    (t
     (setq
       cc (cdr (assoc 6 (tblsearch "layer" (cdr (assoc 8 ec)))))
     )
     (cond
       ((wcmatch cc "*|*")
        (alert
          "Can't set to external linetype.\nWill try using bare linetype name."
        )
        (while (wcmatch (setq cc (substr cc 2)) "*|*"))
       )
     )
    )
  )
  (prompt (strcat "\nNested layers to change by picking: "))
  (vl-cmdf "._layer")
  (haws-ltpick)
  (vl-cmdf "")
  (princ "Linetype ")
  cc
)
(defun haws-ltpick
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
    (vl-cmdf "_lt" cc la)
  )
)

