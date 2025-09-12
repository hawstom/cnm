;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-EG ()
(haws-core-init 47) (princ(entget (car (entsel)))))
(DEFUN c:haws-EGN ()
(haws-core-init 48) (princ(entget (car (nentsel)))))
;;--------------------------END ONE LINERS-------------------------
