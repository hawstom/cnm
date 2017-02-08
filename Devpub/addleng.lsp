;Tip1442: ADDLENG.LSP    Length of Lines    (c)1998, Travis Knapp
;routine to get the sum of the lengths of all selected LINES and ARCS.
(defun c:haws-adl (/ ss1 total count n1 n2 pnt10 pnt11 dist total)
  (graphscr)
  (prompt "\nSelect the Lines to be Added together:")
  (setq ss1 (ssget (list (cons 0 "line,arc"))))
  (setq total 0)
  (setq count (sslength ss1))
  (while (/= count 0)
    (setq N1 (ssname ss1 0))
    (setq N2 (entget N1))
    (setq N3 (cdr (assoc 0 N2)))
    (cond
      ( (= N3 "LINE")
        (setq pnt10 (cdr (assoc 10 N2)))
        (setq pnt11 (cdr (assoc 11 N2)))
        (setq dist (distance pnt10 pnt11))
      )
      ( (= N3 "ARC")
        (setq
           r (cdr (assoc 40 N2))
           sa (cdr (assoc 50 N2))
           ea (cdr (assoc 51 N2))
           delta (cond ((> sa ea) (- (* 2 pi) (- sa ea))) (t (- ea sa)))
           dist (* delta r)
    ) ) )
    (setq total (+ dist total))
    (ssdel n1 ss1)
    (setq count (1- count))
	);end while
  (prompt " Total Length:  ") (princ (rtos total 2 4)) (terpri)
  (princ)
); end program


