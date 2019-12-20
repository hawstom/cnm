;THIS ROUTINE SETS CURRENT LAYER TO DEFPOINTS AND CREATES A PAPER SPACE VIEWPORT
;Written by Thomas Gail Haws
(DEFUN c:haws-MV ()
(haws-core-init 265)
  (HAWS-VSAVE '("clayer")) (setvar "tilemode" 0)
  (command "._pspace" "._layer" "_t" "defpoints" "_m" "defpoints" "")
  (prompt "\nSelect corners:")(command "._MVIEW" pause pause)
  (prompt "\nMview created on defpoints layer.  Will not plot.  Leave thawed.")
  (HAWS-VRSTOR)(princ)
)
