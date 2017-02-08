;;; PJL joins as a polyline objects that are on the layer of the selected object.
(defun c:HAWS-pjl (/ eg en et la peaold) 
	(setq 
		en (car (entsel))
		eg (entget en)
		la (cdr (assoc 8 eg))
		et (cdr (assoc 0 eg))
		ss1 (ssget "X" (list (cons 8 la)))
	)
	(command "._pedit" en)
	(cond
		(
			(and 
				(wcmatch et "ARC,LINE")
				(= (getvar "peditaccept") 0)
			 )
			(command "_y")		
		)
	)
	(command "_join" ss1 "" "")
	(princ)
)