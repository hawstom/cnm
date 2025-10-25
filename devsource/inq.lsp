;;--------------------------ONE LINERS-------------------------
(defun c:haws-eg ()
(haws-core-init 47) (princ(entget (car (entsel)))))
(defun c:haws-egn ()
(haws-core-init 48) (princ(entget (car (nentsel)))))
;;--------------------------END ONE LINERS-------------------------
