(defun c:haws-XU ( / xr)
(haws-core-init 333)
  (setq xr(cdr(assoc 2(entget(car(entsel "Pick xref to unload: "))))))
(command "xref" "U" xr)xr)


