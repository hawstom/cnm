;modified by Thomas Gail Haws
(defun c:haws-be () (if (> (atof (getvar "acadver")) 12.0) (c:haws-bextend) (c:haws-eblock)))
(defun c:haws-eblock (/ es1 en1 ep1 es2 en2 ep2 i ss1 emark en)
  (setq es1 (entsel "\nSelect block to extend to:  ") en1(car es1) ep1(cadr es1))
  (redraw en1 3)
  (setq
    i -1
    es2 (entsel "\nEntity to extend:  ")
    en2 (car es2) ep2 (cadr es2)
    ss1 (ssadd)
  )
  (command "._undo" "mark")
  (command "._line" ep1 ep2 "")
  (setq emark (entlast) en emark)
  (command "._explode" en1)
  (while (setq en (entnext en)) (setq ss1 (ssadd en ss1) emark en))
  (command "._extend" ss1 "" es2 "" "._copy" en2 "" "0,0" "0,0")
  (setq en emark ss1 (ssadd))
  (while (setq en (entnext en))(setq ss1 (ssadd en ss1)))
  (setq en nil)
  (command "._undo" "back" "._erase" en2 "")
  (while (setq en (ssname ss1 (setq i (1+ i))))
    (entdel  en)
  )
  (princ)
)

