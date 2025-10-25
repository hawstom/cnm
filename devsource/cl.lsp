;CHANGE ENTITIES TO CURRENT LAYER  9/25/92  BRB
(defun c:haws-cl (/ e c )
(haws-core-init 176)
  (setq c (getvar "CLAYER") e (ssget))
  (vl-cmdf "._CHANGE" e "" "_P" "_LT" "BYLAYER" "_LA" c "_C" "BYLAYER" "")
  (princ)
);end CL
