;;;XROFFSET
(defun c:haws-xro () (c:haws-xroffset))
(defun
	 c:haws-xroffset	(/ ofdist temp xrodistr PT1 PT2 XRODISTS)
	(haws-borrow 0)
	(setq ofdist (getvar "offsetdist"))
	(if	(not *HAWS-xrosetupdone*)
		(progn
			(initget "Through")
			(setq
				temp
				 (getdist
					 (strcat
						 "\nSpecify offset distance or [Through] <"
						 (if (= ofdist -1)
							 "Through"
							 (rtos ofdist)
						 ) ;_ end of if
						 ">: "
					 ) ;_ end of strcat
				 ) ;_ end of getdist
				xrodistr
				 (if temp
					 temp
					 ofdist
				 ) ;_ end of if
				xrodists
				 (if (= temp -1)
					 "Through"
					 (rtos xrodistr)
				 ) ;_ end of if
			) ;_ end of setq
		) ;_ end of progn
	) ;_ end of if
	(setq *HAWS-xrosetupdone* nil pt1 (cadr (nentsel)))
	(if	pt1
		(progn
			(setq
				pt2
				 (getpoint
					 (if (= xrodists "Through")
						 "\nSpecify through point: "
						 "\nSpecify point on side to offset: "
					 ) ;_ end of if
				 ) ;_ end of getpoint
			) ;_ end of setq
			(setq f1 (open (strcat (getvar "tempprefix") "xrotemp.scr") "w"))
			(princ
				(strcat
					"ncopy "
					(rtos (car pt1))
					","
					(rtos (cadr pt1))
					"  0,0  (setq xroen (entlast)) (redraw xroen 3) ._offset "
					xrodists
					" (list xroen '"
				) ;_ end of strcat
				f1
			) ;_ end of princ
			(princ pt1 f1)
			(princ ") " f1)
			(princ (strcat (rtos (car pt2)) "," (rtos (cadr pt2)) "  ._erase !xroen  (setq *HAWS-xrosetupdone* T) haws-xroffset ") f1)
			(setq f1 (close f1))
			(haws-return)
			(command "._script" (strcat (getvar "tempprefix") "xrotemp"))
		) ;_ end of progn
	) ;_ end of if
) ;_ end of defun
