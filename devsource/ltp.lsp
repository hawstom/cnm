(DEFUN
   C:HAWS-LTP
             (/ SS CC I EC LC)
(haws-core-init 251)
  (COND
    ((/= (SETQ CC (GETSTRING "\nNew layer linetype (return to pick):"))
         ""
     )
     CC
    )
    ((SETQ
       CC (CDR
            (ASSOC
              6
              (SETQ EC (ENTGET (CAR (NENTSEL "\nSelect linetype: "))))
            )
          )
     )
     CC
    )
    (T
     (SETQ
       CC
        (CDR (ASSOC 6 (TBLSEARCH "layer" (CDR (ASSOC 8 EC)))))
     )
     (COND
       ((WCMATCH CC "*|*")
        (ALERT
          "Can't set to external linetype.\nWill try using bare linetype name."
        )
        (WHILE (WCMATCH (SETQ CC (SUBSTR CC 2)) "*|*"))
       )
     )
    )
  )
  (PROMPT
    "\nLayers to change by picking (Linetypes by entity won't change):"
  )
  (SETQ
    SS (SSGET)
    I  0
  )
  (vl-cmdf "._layer")
  (WHILE (SETQ EC (SSNAME SS I))
    (SETQ LC (CDR (ASSOC 8 (ENTGET EC))))
    (vl-cmdf "_lt" CC LC)
    (SETQ I (1+ I))
  )
  (vl-cmdf "")
  (PRINC "Linetype ")
  CC
)
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Change layer color by picking nested entities
(DEFUN
   C:HAWS-LTPX
              (/ CC EC LOPERA NESTED)
(haws-core-init 252)
  (COND
    ((/= (SETQ CC (GETSTRING "\nNew layer linetype (return to pick):"))
         ""
     )
     CC
    )
    ((SETQ
       CC (CDR
            (ASSOC
              6
              (SETQ EC (ENTGET (CAR (NENTSEL "\nSelect linetype: "))))
            )
          )
     )
     CC
    )
    (T
     (SETQ
       CC (CDR (ASSOC 6 (TBLSEARCH "layer" (CDR (ASSOC 8 EC)))))
     )
     (COND
       ((WCMATCH CC "*|*")
        (ALERT
          "Can't set to external linetype.\nWill try using bare linetype name."
        )
        (WHILE (WCMATCH (SETQ CC (SUBSTR CC 2)) "*|*"))
       )
     )
    )
  )
  (PROMPT (STRCAT "\nNested layers to change by picking: "))
  (vl-cmdf "._layer")
  (HAWS-LTPICK)
  (vl-cmdf "")
  (PRINC "Linetype ")
  CC
)
(DEFUN
   HAWS-LTPICK
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
    (vl-cmdf "_lt" CC LA)
  )
)

