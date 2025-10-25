;BRKMAT-BREAKS A LINE BETWEEN 2 POINTS AND DRAWS A LINE IN THE GAP TO MATCH PICK
(defun c:haws-bm (/ ss dl ec b1 b2 sl)
(haws-core-init 8)
  (prompt "Pick entity to break: ")
  (setq ss (entsel))
  (if (/= ss nil)
    (progn
      (setq b1 (getpoint "Pick first point: "))
      (setq b2 (getpoint "Pick second point: "))
      (setq ec (entsel "Entity to match: "))
      (if (/= ec nil)
        (progn
          (vl-cmdf "break" ss "f" b1 b2)
          (setq dl (cdr (assoc 8 (entget (car ec)))))
          (setq sl (getvar "clayer"))
          (vl-cmdf "layer" "s" dl "" "line" b1 b2 "" "layer" "s" sl "")
)))))
;end BRKMAT
