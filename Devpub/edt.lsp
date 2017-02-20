;;--------------------------ONE LINERS-------------------------
(DEFUN C:HAWS-BF ()(COMMAND "._BREAK" PAUSE "F")(PRINC))
(DEFUN C:HAWS-CB () (COMMAND "._copybase" "0,0,0") (PRINC))
(DEFUN C:HAWS-MP ()(COMMAND "._MOVE" "P")(PRINC))
(defun c:HAWS-pj (/ SS1) (setq ss1 (ssget))(command "._pedit" "m" ss1 "" "_y" "_join" "0.005" ""))
(DEFUN C:HAWS-R1 (/ SS1) (SETQ SS1 (SSGET)) (COMMAND "._ROTATE" SS1 "" PAUSE "180") (PRINC))
(DEFUN C:HAWS-R2 (/ SS1) (SETQ SS1 (SSGET)) (COMMAND "._ROTATE" SS1 "" PAUSE "270") (PRINC))
(DEFUN C:HAWS-R4 (/ SS1) (SETQ SS1 (SSGET)) (COMMAND "._ROTATE" SS1 "" PAUSE "45") (PRINC))
(DEFUN C:HAWS-R9 (/ SS1) (SETQ SS1 (SSGET)) (COMMAND "._ROTATE" SS1 "" PAUSE "90") (PRINC))
(DEFUN C:HAWS-S  ()(COMMAND "._STRETCH" "C")(PRINC))
(DEFUN C:HAWS-UB ()(COMMAND "._UNDO" "BACK")(PRINC))
(DEFUN C:HAWS-UM ()(COMMAND "._UNDO" "M")(PRINC))
(DEFUN C:HAWS-VB () (COMMAND "._pasteclip" "0,0,0") (PRINC))
;;--------------------------END ONE LINERS-------------------------
