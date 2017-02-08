;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-EG () (entget (car (entsel))))
(DEFUN c:haws-EGN () (entget (car (nentsel))))
(DEFUN c:haws-FFL () (findfile (getstring "\nFile to find on ACAD's search path: ")))
;;--------------------------END ONE LINERS-------------------------
