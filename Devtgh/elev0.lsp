;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-elev0 (/ en el)
  (initget "Y N")
  (if
    (/=
      (getkword "\nAbout to set all elevations to zero!  Proceed?<Y/N>:")
      "Y"
    )
    (exit)
  )
  (setq en (entnext))
  (while en
    (setq el (entget en) group 10)
    (cond
      ( (and
          (/= (assoc 0 el) "UCS")
          (/= (assoc 0 el) "VIEW")
          (/= (assoc 0 el) "VPORT")
        )
        (repeat 7
          (if
            (assoc group el)
              (setq
                el
                (subst
                  (list
                    group
                    (cadr (assoc group el))
                    (caddr (assoc group el))
                    0.0
                  )
                  (assoc group el)
                  el
                )
                group (1+ group)
        ) ) )
        (entmod el)
    ) )
    (setq en(entnext en))
  )
  (princ)
)
