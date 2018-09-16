;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-2l (/ el1 el2 txt1 txt2)
  (defun HAWS-2lset ()
    (setq  ;Set three global variables
      2elinc (getreal "\nDifference between first and second elevation:")
      2elrd1 (getint "\nDecimal places to round first elevation:")
      2elrd2 (getint "\nDecimal places to round second elevation:")
    )
    (princ)
  )
  (haws-core-init 153)
  (HAWS-VSAVE '("dimzin"))
  (setvar "dimzin" 0)
  (if(not 2elinc)(HAWS-2lset))
  (while (setq el1 (getreal "\nFirst elevation (or enter -1 to change setup):"))
    (cond
      ( (= -1 el1)(HAWS-2lset))
      ( (setq
          el2 (strcat ""(rtos (+ 2elinc el1) 2 2elrd2))
          el1 (strcat ""(rtos el1 2 2elrd1))
        )
        (while (setq txt1 (entsel "\nPick first text to change:"))
          (setq txt1 (entget (car txt1)))
          (entmod (subst (cons 1 el1) (assoc 1 txt1) (subst (cons 3 "") (assoc 3 txt1) txt1)))
          (setq txt2 (entget (car (entsel "\nPick second text to change:"))))
          (entmod (subst (cons 1 el2) (assoc 1 txt2) (subst (cons 3 "") (assoc 3 txt2) txt2)))
        )
      )
    )
  )
  (haws-core-restore)(HAWS-VRSTOR)(princ)
)
