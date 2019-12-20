(if (not haws-attfind) (load "attfind"))
(defun
	 c:haws-incatt	(/ blki blklst ss1)
(haws-core-init 224)
	(command ".undo" "_g")
	(setq
		ss1
		 (ssget)
		incatt_attinc
		 (HAWS-GETSTRINGX "Tag of target attribute to change" incatt_attinc "")
		incatt_attbas
		 (HAWS-GETSTRINGX "Tag of base attribute to add from" incatt_attbas "")
		incatt_incx
		 (HAWS-GETREALX "Amount to increment base value" incatt_incx 0.0)
		incatt_dec
		 (HAWS-GETINTX "Decimal places to round changed attributes" incatt_dec (getvar "luprec"))
		blklst
		 (HAWS-attfind "*" nil (list incatt_attinc incatt_attbas) nil)
	) ;_ end of setq
	(foreach
		 blki	blklst
		(setq
			eninci
			 (caadr blki)
			enbasi
			 (if (= incatt_attbas incatt_attinc)
				 eninci
				 (cadadr blki)
			 ) ;_ end of if
		) ;_ end of setq
		(if	(and (ssmemb (car blki) ss1) eninci enbasi)
			(progn
				(setq
					elinci
					 (entget eninci)
					elbasi
					 (entget enbasi)
					sinci
					 (HAWS-EXTRACT (cdr (assoc 1 elinci)))
					sbasi
					 (HAWS-EXTRACT (cdr (assoc 1 elbasi)))
				) ;_ end of setq
				(entmod
					(subst
						(cons 1 (strcat (car sinci) (rtos (+ (atof (cadr sbasi)) incatt_incx) 2 incatt_dec) (caddr sinci)))
						(assoc 1 elinci)
						elinci
					) ;_ end of subst
				) ;_ end of entmod
				(entupd eninci)
			) ;_ end of progn
		) ;_ end of if
	) ;_ end of foreach
	(command "._undo" "_e")
	(prompt "\nIf you need additional options, contact Tom Haws.")
	(princ)
) ;_ end of defun
