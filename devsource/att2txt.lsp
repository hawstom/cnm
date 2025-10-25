(defun c:haws-a2t ()
(haws-core-init 161) (c:haws-att2txt))
(defun 	 c:haws-att2txt ( / aent tent ilist inum pt1)
(haws-core-init 162)
	(setq aent (entget (car (nentsel "\nSelect attribute: "))))
	(setq tent '((0 . "TEXT")))
	(foreach
		 inum	'(8 6 38 39 62 67 210 10 40 1 50 41 51 71 72 74 11)
		(if	(setq ilist (assoc inum aent))
			(progn
				(if	(= inum 74)
					(setq ilist (cons 73 (cdr (assoc inum aent))))
				) ;_ end of if
				(setq tent (cons ilist tent))
			) ;_ end of progn
		) ;_ end of if
	) ;_ end of foreach
	(entmake (reverse tent))
	(redraw (entlast) 3)
	(setq pt1 (getpoint "\nBase point or displacement for new text: "))
	(vl-cmdf "._move" "_l" "" pt1 pause)
	(princ)
) ;_ end of defun

