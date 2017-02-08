;CHANGE CASE OF TEXT


(defun c:haws-CHGCASE (/ SS1 TYP ENT ENTG TXT)

  (initget 6 "U L")
  (setq
    TYP (getkword "\n \nChange to Upper or Lower case <U>: ")
    i -1
    ss1 (ssget)
  )
  (if (= TYP "U") (setq TYP nil))
  (while (setq ENT (ssname ss1 (setq i (1+ i))))
    (setq ENTG (entget ENT))
    (setq TXT (strcase (cdr (assoc 1 ENTG)) TYP))
    (if (= TYP "L") (setq TXT (strcat (strcase (substr TXT 1 1)) (substr TXT 2))))
    (setq ENTG (subst (cons 1 TXT) (assoc 1 ENTG) ENTG))
    (entmod ENTG)
  )
  (princ)
)
