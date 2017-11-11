;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-EG () (princ(entget (car (entsel)))))
(DEFUN c:haws-EGN () (princ(entget (car (nentsel)))))
;;--------------------------END ONE LINERS-------------------------
