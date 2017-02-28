;;;(C) Copyright 1997 by Thomas Gail Haws
;;;NEWANG CHANGES ENTITY ORIENTATION TO MATCH A SELECTED ENTITY.
(defun c:haws-na () (HAWS-newang))
(defun
	 HAWS-newang	(/ basept flip i sset metype men nested mel mes meang nested etype organg rotang shells sslen ent entlst)
	(haws-errdef 0) ;_ end of if
	(HAWS-VSAVE '("aunits"))
	(command "._undo" "g")
	(setq sset (ssget) flip nil)
	(cond
		((not sset) (prompt "\nNone found."))
		(t
		 (initget "Flip Nested")
		 (while	(or (= (setq
								 mes
									(if	nested
										(nentsel (strcat "\nSelect object to match or [Flip/Angle] <Angle>: "))
										(entsel (strcat "\nSelect object to match or [Nested object/Flip results/Angle] <Angle>: "))
									) ;_ end of if
							 ) ;_ end of setq
							 "Nested"
						) ;_ end of =
							 (= mes "Flip")
						)
			 (if (= mes "Nested") (setq nested t))
			 (if (= mes "Flip") (setq flip t))
		 ) ;_ end of while
		 (if (or (not mes)
						 (not
							 (or (= (setq metype (cdr (assoc 0 (setq mel (entget (setq men (car mes))))))) "LINE")
									 (= metype "ARC")
									 (= metype "VERTEX")
									 (= metype "TEXT")
									 (= metype "MTEXT")
									 (= metype "ATTDEF")
									 (= metype "ATTRIB")
									 (= metype "SHAPE")
									 (= metype "INSERT")
							 ) ;_ end of or
						 ) ;_ end of not
				 ) ;_ end of or
			 (cond
				 ((setq meang (getangle "\nNo allowable entity selected.  New angle or <Rotation>: ")))
				 ((setq rotang (getangle "\nRotation angle: ")))
			 ) ;_ end of cond
		 ) ;_ end of if
		 (setq
			 meang
				(cond
					(meang)
					(rotang)
					((= metype "LINE") (angle (trans (cdr (assoc 10 mel)) 0 1) (trans (cdr (assoc 11 mel)) 0 1)))
					((= metype "ARC")
					 (-	(angle
								(polar (cdr (assoc 10 mel)) (cdr (assoc 50 mel)) (cdr (assoc 40 mel)))
								(polar (cdr (assoc 10 mel)) (cdr (assoc 51 mel)) (cdr (assoc 40 mel)))
							) ;_ end of angle
							(angle '(0.0 0.0 0.0) (getvar "UCSXDIR"))
					 ) ;_ end of angle
					)
					((= metype "VERTEX")
					 (angle (trans (cdr (assoc 10 mel)) men 1) (trans (cdr (assoc 10 (entget (entnext men)))) men 1))
					)
					((or (= metype "TEXT")
							 (= metype "SHAPE")
							 (= metype "ATTDEF")
							 (= metype "ATTRIB")
							 (= metype "INSERT")
					 ) ;_ end of OR
					 (- (cdr (assoc 50 mel)) (angle '(0.0 0.0 0.0) (getvar "UCSXDIR")))
					)
				) ;_ end of cond
		 ) ;_ end of setq
		 (cond
			 (nested
				(setq
					shells
					 (cadddr mes)
					i	-1
				) ;_ end of setq
				(if	shells
					(progn
						(while (setq shelli (nth (setq i (1+ i)) shells))
							(setq meang (+ meang (cdr (assoc 50 (entget shelli)))))
						) ;_ end of while
					) ;_ end of progn
				) ;_ end of if
			 )
		 ) ;_ end of cond
		 ;; Change all of the entities in the selection set.
		 (prompt "\nChanging entities to match selection.  ")
		 (setq sslen (sslength sset))
		 (setvar "aunits" 3)
		 (while	(> sslen 0)
			 (setq
				 entlst
					(entget (setq ent (ssname sset (setq sslen (1- sslen)))))
				 basept
					(cond
						((and
							 (or (= (setq etype (cdr (assoc 0 entlst))) "TEXT") (= etype "ATTDEF") (= etype "ATTRIB"))
							 (not (equal (cdr (assoc 11 entlst)) '(0.0 0.0 0.0)))
						 ) ;_ end of and
						 (cdr (assoc 11 entlst))
						)
						(t (cdr (assoc 10 entlst)))
					) ;_ end of cond
			 ) ;_ end of setq
			 (if rotang
				 (setq organg 0)
				 (setq
					 organg
						(cond
							((= etype "LINE") (angle (cdr (assoc 10 entlst)) (cdr (assoc 11 entlst))))
							((or (= etype "INSERT") (= etype "TEXT") (= etype "ATTDEF") (= etype "ATTRIB") (= etype "SHAPE"))
							 (- (cdr (assoc 50 entlst)) (angle '(0.0 0.0 0.0) (getvar "UCSXDIR")))
							)
							((= etype "MTEXT") (angle '(0.0 0.0 0.0) (cdr (assoc 11 entlst))))
							((= etype "POLYLINE") (angle (cdr (assoc 10 entlst)) (cdr (assoc 10 (entget (entnext ent))))))
							((= etype "ARC")
							 (angle
								 (polar (cdr (assoc 10 entlst)) (cdr (assoc 50 entlst)) (cdr (assoc 40 entlst)))
								 (polar (cdr (assoc 10 entlst)) (cdr (assoc 51 entlst)) (cdr (assoc 40 entlst)))
							 ) ;_ end of angle
							)
							(t nil)
						) ;_ end of cond
				 ) ;_ end of setq
			 ) ;_ end of if
			 (if flip (setq organg (+ organg pi)))
			 (if organg
				 (command "._rotate" ent "" (trans basept ent 1) "R" organg meang)
				 (prompt "\nDon't know how to rotate this entity.  ")
			 ) ;_ end of if
		 ) ;_ end of while
		 (command "select" sset "")
		 (command "._undo" "e")
		 (prompt "\nDone.")
		)
	) ;_ end of cond
	(HAWS-VRSTOR)
	(HAWS-ERRRST) ;_ end of if
	(princ)
) ;_ end of defun
