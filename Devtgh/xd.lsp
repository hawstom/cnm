(defun c:haws-XD ( / xr)
  (setq xr(cdr(assoc 2(entget(car(entsel "Pick xref to detach: "))))))
(command "xref" "d" xr)xr)


