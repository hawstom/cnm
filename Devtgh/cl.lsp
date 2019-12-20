;CHANGE ENTITIES TO CURRENT LAYER  9/25/92  BRB
(DEFUN c:haws-CL (/ E C )
(haws-core-init 176)
  (SETQ C (GETVAR "CLAYER") E (SSGET))
  (COMMAND "._CHANGE" E "" "_P" "_LT" "BYLAYER" "_LA" C "_C" "BYLAYER" "")
  (PRINC)
);end CL
