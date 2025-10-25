;Contributed by Craig Hendricks
;Modified by Thomas Gail Haws
(defun c:haws-pall ()
(haws-core-init 293)(haws-purge"ALL")(princ))

(defun haws-purge (a)
  (vl-cmdf
    ".PURGE"
    (cond
      ( (eq (strcase a) "ALL")
      (setq a '("block" "layer" "ltype" "style" "dimstyle")) "ALL")
      (t(car (setq a (list a))))
  ))
  (if (>= (atof (getvar"acadver")) 14)
    (vl-cmdf "*" "_n")
    (mapcar
      '(lambda (x / y)
        (while (setq y (tblnext x (not y)))
          (if
            (and
              (/= "0" (cdr (assoc 2 y)))(/= "STANDARD" (cdr (assoc 2 y)))
              (/= 64 (logand 64 (cdr (assoc 70 y))))
              (/= 16 (logand 16 (cdr (assoc 70 y))))
            )
            (vl-cmdf "Y")
      ) ) )
      a
    )
  )
  (princ)
)
