;(C) Copyright 1997 by Thomas Gail Haws
;MFILLET.LSP--Multiple concentric fillet
;Thomas Gail Haws, Feb. 1996
(defun c:haws-mf ()
(haws-core-init 257)(c:haws-mfillet))
(defun c:haws-mfillet ( / oldrad frad fcen p1 p2)
  (haws-core-init 258)
  (setq oldrad (getvar "filletrad") radj 1)
  (initget "Adjust")
  (while
    (=
      (setq frad(getdist
          (strcat
            "\nAdjust radius growth factor/first fillet radius (not 0) <"
            (rtos oldrad)
            ">:"
      )))
      "Adjust"
    )
    (setq radj (getreal (strcat"\nRadius growth factor <"(rtos radj 2)">:")))
  )
  (if(not frad)(setq frad oldrad))
  (princ (getvar "cmdactive"))
  (command "._fillet" "r" frad)
  (setq frad1 frad)
  (setq
    es1 (entsel "\nFirst entity:")
    es2 (entsel "\nSecond entity:")
    lay1 (cdr (assoc 8 (entget (car es1))))
  )
  (princ (getvar "cmdactive"))
  (setvar "clayer" lay1)
  (command "._fillet" es1 es2)
  (setq fcen
    (if (wcmatch (cdr (assoc 0 (entget (car es1)))) "*POLY*")
      (osnap (cadr (entsel "\nNew arc: ")) "cen")
      (cdr (assoc 10 (entget (entlast))))
  ) )
  (while (setq es1 (entsel "\nFirst entity for next fillet:"))
    (setq lay1 (cdr (assoc 8 (entget (car es1)))))
    (setvar "clayer" lay1)
    (command "._dist" (trans fcen 0 1) "perp" (cadr es1))
    (setq frad (+ (*(-(getvar "distance") frad1)radj) frad1))
    (princ "\nNext radius=")(princ frad)
    (setq p2 (entsel "\nSecond entity:"))
    (command "._fillet" "r" frad "._fillet" es1 p2)
  )
  (setvar "filletrad" frad1)
  (haws-core-restore)(HAWS-VRSTOR)(princ)
)
