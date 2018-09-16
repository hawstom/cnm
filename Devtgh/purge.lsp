;Contributed by Craig Hendricks
;Modified by Thomas Gail Haws
(DEFUN c:haws-PALL ()
(haws-core-init 293)(HAWS-purge"ALL")(PRINC))

(defun HAWS-purge (a)
  (command
    ".PURGE"
    (cond
      ( (eq (strcase a) "ALL")
      (setq a '("block" "layer" "ltype" "style" "dimstyle")) "ALL")
      (t(car (setq a (list a))))
  ))
  (if (>= (atof (getvar"acadver")) 14)
    (command "*" "n")
    (mapcar
      '(lambda (x / y)
        (while (setq y (tblnext x (not y)))
          (if
            (and
              (/= "0" (cdr (assoc 2 y)))(/= "STANDARD" (cdr (assoc 2 y)))
              (/= 64 (logand 64 (cdr (assoc 70 y))))
              (/= 16 (logand 16 (cdr (assoc 70 y))))
            )
            (command "Y")
      ) ) )
      a
    )
  )
  (princ)
)
