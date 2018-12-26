;Written by Thomas Gail Haws
(defun c:haws-contxt (/ txtht txtstr inspt rot)
  (haws-core-init 212)
  (HAWS-VSAVE '("clayer"))
  (setvar "cmdecho" 0)
  (setq txtstr (getstring "\nContour text:"))
  (HAWS-MKLAYR "CONTLBL")
  (setq inspt (getpoint "\nMiddle point:") rot (getangle inspt " Rotation: "))
  (HAWS-MKTEXT "m" inspt nil rot txtstr)
  (HAWS-VRSTOR)(haws-core-restore)(princ)
)
