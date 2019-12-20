(defun
	 c:haws-eop (/ p1 p2 p3 s1 s2 s3 c1 c2 wt w1 a1 a2 a3)
(haws-core-init 221)
	(setvar "cmdecho" 0)
	(setq
		p1 (getpoint "\nPick outside end of pipe (either side): ")
		p2 (getpoint p1 "\nPick end of other side: ")
		wt (getreal "\nEnter wall thickness: ")
		p3 (list (/ (+ (car p1) (car p2)) 2) (/ (+ (cadr p1) (cadr p2)) 2))
		w1 (/ (distance p1 p2) 8.0)
		c1 (list (/ (+ (car p1) (car p3)) 2) (/ (+ (cadr p1) (cadr p3)) 2))
		c2 (list (/ (+ (car p2) (car p3)) 2) (/ (+ (cadr p2) (cadr p3)) 2))
		a1 (angle p1 p2)
		a2 (+ a1 (/ pi 2))
		a3 (+ a2 pi)
		s1 (polar c1 a2 w1)
		s2 (polar c2 a2 w1)
		s3 (polar c2 a3 w1)
	) ;_ end of SETQ
	(command "._arc" p1 s1 p3 "._arc" p3 s2 p2 "._arc" p3 s3 p2)
	(cond
		((/= wt 0)
		 (setq
			 p2	(polar p2 (+ a1 pi) wt)
			 c1	(polar c1 a1 (/ wt 2))
			 c2	(polar c2 (+ a1 pi) (/ wt 2))
			 s1	(polar c1 a2 (- w1 (/ wt 2)))
			 s2	(polar c2 a2 (- w1 (/ wt 2)))
			 s3	(polar c2 a3 (- w1 (/ wt 2)))
		 ) ;_ end of SETQ
		 (command "._arc" p2 s2 p3 "._arc" p2 s3 p3)
		)
	) ;_ end of COND
) ;_ end of DEFUN
