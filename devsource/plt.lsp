;Written by Craig Hendricks
;Modified by Thomas Gail Haws
(defun c:haws-plt (/ doscmd)
(haws-core-init 271)
  (setvar "cmddia" 0)
  (setvar "filedia" 0)
  (setq doscmd (strcat "del " (haws-getdnpath) ".plt"))
  (vl-cmdf "_sh" doscmd
  "._plot" "_w" "0,0" "36,24" "" "")
  (setvar "cmddia" 1)
  (setvar "filedia" 1)
  (princ)
)
