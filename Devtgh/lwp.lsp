;;; Written by Thomas Gail Haws
;;; Change layer lineweights by picking.
(DEFUN
   C:HAWS-LWP
             (/ SS CC I EC LC)
(haws-core-init 253)
  (COND
    ((/= (SETQ CC (GETSTRING "\nNew layer lineweight (return to pick):"))
         ""
     )
     CC
    )
    ((SETQ CC (/ (CDR (ASSOC 370 (SETQ EC (ENTGET (CAR (ENTSEL)))))) 100))
     CC
    )
    (T
     (SETQ
       CC (ABS
            (/ (CDR (ASSOC 370 (TBLSEARCH "layer" (CDR (ASSOC 8 EC))))) 100)
          )
     )
    )
  )
  (PROMPT
    "\nLayers to change by picking (lineweights by entity won't change):"
  )
  (SETQ
    SS (SSGET)
    I  0
  )
  (vl-cmdf "._layer")
  (WHILE (SETQ EC (SSNAME SS I))
    (SETQ LC (CDR (ASSOC 8 (ENTGET EC))))
    (vl-cmdf "_lw" CC LC)
    (SETQ I (1+ I))
  )
  (vl-cmdf "")
  (PRINC "lineweight ")
  CC
)
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Change layer lineweight by picking nested entities
(DEFUN
   C:HAWS-LWPX
              (/ CC EC LOPERA NESTED)
(haws-core-init 254)
  (COND
    ((/= (SETQ CC (GETSTRING "\nNew layer lineweight (return to pick):"))
         ""
     )
     CC
    )
    ((SETQ CC (/ (CDR (ASSOC 370 (SETQ EC (ENTGET (CAR (ENTSEL)))))) 100))
     CC
    )
    (T
     (SETQ
       CC (ABS
            (/ (CDR (ASSOC 62 (TBLSEARCH "layer" (CDR (ASSOC 8 EC))))) 100)
          )
     )
    )
  )
  (SETQ FLIST NIL)
  (PROMPT (STRCAT "\nNested layers to change by picking: "))
  (vl-cmdf "._layer")
  (HAWS-LWPICK)
  (vl-cmdf "")
  (PRINC "lineweight ")
  CC
)
(DEFUN
   HAWS-LWPICK
              (/ ES EN LA PARNTL VP)
  (WHILE (SETQ ES (NENTSEL))
    (SETQ
      EN     (CAR ES)
      LA     (CDR (ASSOC 8 (ENTGET EN)))
      PARNTL (CADDDR ES)
    )
    (WHILE (= LA "0")
      (SETQ LA (CDR (ASSOC 8 (ENTGET (CAR PARNTL)))))
      (SETQ PARNTL (CDR PARNTL))
    )
    (REDRAW EN 3)
    (SETQ FLIST (CONS LA FLIST))
    (vl-cmdf "_lw" CC LA)
  )
)
