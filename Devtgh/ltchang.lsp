;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-LTC (/ SS DL EC)
  (setq EC (entsel "Pick Layer to Change Entities to Continuous:"))
  (setq DL (cdr (assoc 8 (entget (car EC)))))
  (setq SS (ssget "X" (list (cons '8 dl))))
  (command "change" ss "" "p" "lt" "continuous" "")
)
(defun c:haws-LTB (/ SS DL EC)
  (setq EC (entsel "Pick Layer to Change Entities to Bylayer:"))
  (setq DL (cdr (assoc 8 (entget (car EC)))))
  (setq SS (ssget "X" (list (cons '8 dl))))
  (command "change" ss "" "p" "lt" "bylayer" "")
)

(defun c:haws-LTH (/ SS)
  (prompt "\nEntities to Change to Linetype Hidden:")
  (setq ss (ssget))
  (command "change" ss "" "p" "lt" "hidden" "")
)
