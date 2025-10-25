;;--------------------------ONE LINERS-------------------------
(defun c:haws-c2 ()
(haws-core-init 23)(vl-cmdf "CIRCLE" "2P")(princ))
(defun c:haws-ct ()
(haws-core-init 24)(vl-cmdf "CIRCLE" "TTR")(princ))
(defun c:haws-dd ()
(haws-core-init 25)(vl-cmdf "DONUT" "0" "5")(princ))
(defun c:haws-p0 ()
(haws-core-init 26)(setvar "PLINEWID" 0)(princ))
;;--------------------------END ONE LINERS-------------------------
