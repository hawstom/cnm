;CHDIM
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-CHDIM (/ p l n e os as ns st s nsl osl sl si chf chm )
   (setq chm     0  p (ssget '((0 . "DIMENSION"))))
   (if p (progn                      ; If any objects selected
      (setq osl (strlen (setq os (getstring t "\nOld string: "))))
      (setq nsl (strlen (setq ns (getstring t "\nNew string: "))))
      (setq l 0 n (sslength p))
      (while (< l n)                 ; For each selected object...
         (setq e (entget (ssname p l)))
         (setq chf nil si 1)
         (setq s (cdr (setq as (assoc 1 e))))
         (while (= osl (setq sl (strlen
                       (setq st (substr s si osl)))))
            (if (= st os)
                (progn
                  (setq s (strcat (substr s 1 (1- si)) ns
                                  (substr s (+ si osl))))
                  (setq chf t) ; Found old string
                  (setq si (+ si nsl))
                )
                (setq si (1+ si))
            )
         )
         (if chf (progn        ; Substitute new string for old
            (setq e (subst (cons 1 s) as e))
            (entmod e)         ; Modify the DIMENSION entity
            (setq chm (1+ chm))
         ))
         (setq l (1+ l))
      )
   ))
   (princ "Changed ")                ; Print total lines changed
   (princ chm)
   (princ " dimensions.")
   (terpri)
   (princ)
)
