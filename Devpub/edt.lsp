;;--------------------------ONE LINERS-------------------------
(DEFUN C:HAWS-BF ()
(haws-core-init 28)(vl-cmdf "._BREAK" PAUSE "F")(PRINC))
(DEFUN C:HAWS-COPY ()
(haws-core-init 29)(vl-cmdf "._COPY" (SSGET) "" "M")(PRINC))
(DEFUN C:HAWS-CB ()
(haws-core-init 30) (vl-cmdf "._copybase" "0,0") (PRINC))
(DEFUN C:HAWS-MP ()
(haws-core-init 31) (vl-cmdf "._MOVE" "P")(PRINC))
(DEFUN c:HAWS-pj ()
(haws-core-init 32) (vl-cmdf "._pedit" "m" (SSGET) "" "_y" "_join" "0.005" "")(PRINC))
(DEFUN C:HAWS-R1 ()
(haws-core-init 33) (vl-cmdf "._ROTATE" (SSGET) "" PAUSE "180") (PRINC))
(DEFUN C:HAWS-R2 ()
(haws-core-init 34) (vl-cmdf "._ROTATE" (SSGET) "" PAUSE "270") (PRINC))
(DEFUN C:HAWS-R4 ()
(haws-core-init 35) (vl-cmdf "._ROTATE" (SSGET) "" PAUSE "45") (PRINC))
(DEFUN C:HAWS-R9 ()
(haws-core-init 36) (vl-cmdf "._ROTATE" (SSGET) "" PAUSE "90") (PRINC))
(DEFUN C:HAWS-S  ()
(haws-core-init 37)(vl-cmdf "._STRETCH" "C")(PRINC))
(DEFUN C:HAWS-UB ()
(haws-core-init 38)(vl-cmdf "._UNDO" "BACK")(PRINC))
(DEFUN C:HAWS-UM ()
(haws-core-init 39)(vl-cmdf "._UNDO" "M")(PRINC))
(DEFUN C:HAWS-VB ()
(haws-core-init 40) (vl-cmdf "._pasteclip" "0,0") (PRINC))
;;--------------------------END ONE LINERS-------------------------
