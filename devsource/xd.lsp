(defun c:haws-xd ( / xr)
(haws-core-init 330)
  (setq xr(cdr(assoc 2(entget(car(entsel "Pick xref to detach: "))))))
(vl-cmdf "xref" "d" xr)xr)


