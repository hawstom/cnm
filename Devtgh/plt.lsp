;Written by Craig Hendricks
;Modified by Thomas Gail Haws
(DEFUN c:haws-PLT (/ doscmd)
(haws-core-init 271)
  (setvar "cmddia" 0)
  (setvar "filedia" 0)
  (setq doscmd (strcat "del " (HAWS-GETDNPATH) ".plt"))
  (command "_sh" doscmd
  "._plot" "_w" "0,0" "36,24" "" "")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)
