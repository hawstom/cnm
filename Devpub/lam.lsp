;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-AS () (COMMAND "LAYMCUR")(PRINC))
(DEFUN c:haws-CL () (COMMAND "LAYCUR")(PRINC))
(DEFUN c:haws-L0 ()(COMMAND "LAYER" "T" "0" "S" "0" "")(PRINC))
(DEFUN c:haws-LK0 ()(COMMAND "LAYER" "LOCK" "0" "")(PRINC))
(DEFUN c:haws-LK ()(COMMAND "LAYLCK")(PRINC))
(DEFUN c:haws-LKA ()(COMMAND "LAYER" "LO" "*" "")(PRINC))
(DEFUN c:haws-ONA ()(COMMAND "LAYON")(PRINC))
(DEFUN c:haws-THA ()(COMMAND "LAYTHW")(PRINC))
(DEFUN c:haws-UL ()(COMMAND "LAYULK")(PRINC))
(DEFUN c:haws-ULA ()(COMMAND "LAYER" "U" "*" "")(PRINC))
;;--------------------------END ONE LINERS-------------------------