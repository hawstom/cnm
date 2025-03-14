;BRKMAT-BREAKS A LINE BETWEEN 2 POINTS AND DRAWS A LINE IN THE GAP TO MATCH PICK
(defun c:haws-BM (/ SS DL EC b1 b2 sl)
(haws-core-init 8)
  (prompt "Pick entity to break: ")
  (setq SS (entsel))
  (if (/= ss nil)
    (progn
      (setq b1 (getpoint "Pick first point: "))
      (setq b2 (getpoint "Pick second point: "))
      (setq EC (entsel "Entity to match: "))
      (if (/= ec nil)
        (progn
          (vl-cmdf "break" ss "f" b1 b2)
          (setq DL (cdr (assoc 8 (entget (car EC)))))
          (setq sl (getvar "clayer"))
          (vl-cmdf "layer" "s" DL "" "line" b1 b2 "" "layer" "s" sl "")
)))))
;end BRKMAT
