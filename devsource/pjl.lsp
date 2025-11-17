;;; PJL joins as a polyline objects that are on the layer of the selected object.
(defun c:haws-pjl (/ eg en et la peaold)
(haws-core-init 97)
(princ (haws-evangel-msg))
  (setq
    en	(car (entsel))
    eg	(entget en)
    la	(cdr (assoc 8 eg))
    et	(cdr (assoc 0 eg))
    ss1	(ssget "X" (list (cons 8 la)))
  )
  (vl-cmdf "._pedit" en)
  (cond
    ((and (wcmatch et "ARC,LINE") (= (getvar "peditaccept") 0))
     (vl-cmdf "_y")
    )
  )
  (vl-cmdf "_join" ss1 "" "")
  (princ)
)
;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 t nil nil t)
;*** DO NOT add text below the comment! ***|;
