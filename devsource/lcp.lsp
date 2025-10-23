;;; Written by Thomas Gail Haws
;;; Change layer colors by picking.
(DEFUN C:HAWS-LCP
             (/ SS CC I EC LC)
(haws-core-init 240)
  (COND
    ((/= (SETQ CC (GETSTRING "\nNew layer color (return to pick):"))
         ""
     )
     CC
    )
    ((SETQ CC (CDR (ASSOC 62 (SETQ EC (ENTGET (CAR (ENTSEL)))))))
     CC
    )
    (T
     (SETQ
       CC (ABS
            (CDR (ASSOC 62 (TBLSEARCH "layer" (CDR (ASSOC 8 EC)))))
          )
     )
    )
  )
  (PROMPT
    "\nLayers to change by picking (Colors by entity won't change):"
  )
  (SETQ
    SS (SSGET)
    I  0
  )
  (vl-cmdf "._layer")
  (WHILE (SETQ EC (SSNAME SS I))
    (SETQ LC (CDR (ASSOC 8 (ENTGET EC))))
    (vl-cmdf "_c" CC LC)
    (SETQ I (1+ I))
  )
  (vl-cmdf "")
  (PRINC "Color ")
  CC
)
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Change layer color by picking nested entities
(DEFUN C:HAWS-LCPX
              (/ CC EC LOPERA NESTED)
(haws-core-init 241)
(prompt (strcat "\n" (HAWS_EVANGEL_MSG)))
  (COND
    ((/= (SETQ CC (GETSTRING "\nNew layer color (return to pick):"))
         ""
     )
     CC
    )
    ((SETQ CC (CDR (ASSOC 62 (SETQ EC (ENTGET (CAR (ENTSEL)))))))
     CC
    )
    (T
     (SETQ
       CC (ABS
            (CDR (ASSOC 62 (TBLSEARCH "layer" (CDR (ASSOC 8 EC)))))
          )
     )
    )
  )
  (PROMPT (STRCAT "\nNested layers to change by picking: "))
  (vl-cmdf "._layer")
  (HAWS-LCPICK)
  (vl-cmdf "")
  (PRINC "Color ")
  CC
)
(DEFUN HAWS-LCPICK
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
    (vl-cmdf "_c" CC LA)
  )
)
