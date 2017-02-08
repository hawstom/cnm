;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-D1 ()(HAWS-VTOG '("DIMSE1")))
(DEFUN c:haws-D2 ()(HAWS-VTOG '("DIMSE2")))
(DEFUN c:haws-DP ()(COMMAND "DIM1" "DIMPOST")(PRINC))
(DEFUN c:haws-DU ()(COMMAND "DIM1" "UPDATE")(PRINC))
(DEFUN c:haws-DV ()(COMMAND "DIM1" "VER")(PRINC))
(DEFUN c:haws-HT ()(COMMAND "DIM1" "HOMETEXT")(PRINC))
(DEFUN c:haws-TE ()(COMMAND "DIM1" "TEDIT")(PRINC))
(DEFUN c:haws-XX ()(HAWS-VTOG '("DIMTIX")))
;;--------------------------END ONE LINERS-------------------------
