;Written by Thomas Gail Haws
(defun c:haws-chnum ( / starti i inc txset en el)
(haws-core-init 173)
  (setq
    starti (getreal "\nStarting number: ")
    i 0
    inc (getreal "\nIncrement: ")
    txset (ssget '((0 . "*TEXT")))
  )
  (while (setq en (ssname txset i))
    (setq el (entget en))
    (entmod
      (subst
        (cons 1 (rtos (+ starti (* inc i))))
        (assoc 1 el)
        el
      )
    )
    (setq i (1+ i))
  )
)
