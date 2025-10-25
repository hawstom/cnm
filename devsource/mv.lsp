;THIS ROUTINE SETS CURRENT LAYER TO DEFPOINTS AND CREATES A PAPER SPACE VIEWPORT
;Written by Thomas Gail Haws
(defun c:haws-mv ()
(haws-core-init 265)
  (haws-vsave '("clayer")) (setvar "tilemode" 0)
  (vl-cmdf "._pspace" "._layer" "_t" "defpoints" "_m" "defpoints" "")
  (prompt "\nSelect corners:")(vl-cmdf "._MVIEW" pause pause)
  (prompt "\nMview created on defpoints layer.  Will not plot.  Leave thawed.")
  (haws-vrstor)(princ)
)
