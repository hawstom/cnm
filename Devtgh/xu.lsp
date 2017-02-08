(defun c:haws-XU ( / xr)
  (setq xr(cdr(assoc 2(entget(car(entsel "Pick xref to unload: "))))))
(command "xref" "U" xr)xr)


