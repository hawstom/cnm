;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-C2 ()
(haws-core-init 23)(COMMAND "CIRCLE" "2P")(PRINC))
(DEFUN c:haws-CT ()
(haws-core-init 24)(COMMAND "CIRCLE" "TTR")(PRINC))
(DEFUN c:haws-DD ()
(haws-core-init 25)(COMMAND "DONUT" "0" "5")(PRINC))
(DEFUN c:haws-P0 ()
(haws-core-init 26)(SETVAR "PLINEWID" 0)(PRINC))
;;--------------------------END ONE LINERS-------------------------
