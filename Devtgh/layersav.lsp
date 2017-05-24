;;;LAYERSAV.LSP version 5.0
;;;(C) Copyright 1997 by Thomas Gail Haws
;;;Revisions: 7/12/95 Added error trapping
;;;           7/21/95 Added automatic file extension to be consistent with Acad
;;;           9/01/95 Added dialogue box file selection
;;;          10/04/95 Added linetype restore and "No Matching Layer" error trap.
;;;          10/20/95 Added non-dialogue box restore option LARPLT for script use.
;;;           1/05/96 Added paper space viewport specific settings.
;;;           7/31/96 Revised paper space viewport specific setting method.
;;;                   Added wildcard (* and ?) option to LAR.
;;;           5/22/00 Added linetype loading in LAR from default .lin file.
;;Function to save layer settings to a file
(defun
	 c:haws-las (/ fname layer count mvsset mvlist layers)
	(haws-core-borrow 0) ;_ end of if
	(setq
		f1 (if (= (getvar "filedia") 1)
				 (open (GETFILED "Layer settings file" (HAWS-GETDNPATH) "lyr" 1) "w")
				 (car (HAWS-GETFIL "Layer settings file" (HAWS-GETDNPATH) "w" "lyr"))
			 ) ;_ end of if
		count	0
		mvsset
		 (ssget "X" (list (cons 0 "VIEWPORT")))
	) ;_ end of setq
	(while (setq layer (tblnext "LAYER" (not layer))) (prin1 layer f1) (princ (chr 10) f1))
	(if	mvsset
		(while (setq mvport (ssname mvsset count))
			(setq mvport (entget mvport '("ACAD")))
			(cond
				((assoc 1003 (cdadr (assoc -3 mvport)))
				 (prin1 (list (assoc 10 mvport) (assoc 40 mvport) (assoc 41 mvport)) f1)
				 (princ (chr 10) f1)
				 (setq
					 i 0
					 layers
						(cdadr (assoc -3 mvport))
				 ) ;_ end of setq
				 (while	(setq layer (nth (setq i (1+ i)) layers))
					 (cond ((= 1003 (car layer)) (prin1 (list layer) f1) (princ (chr 10) f1)))
				 ) ;_ end of while
				)
			) ;_ end of cond
			(setq count (1+ count))
		) ;_ end of while
	) ;_ end of if
	(setq f1 (close f1))
	(haws-core-return) ;_ end of if
	(princ)
)																				;end defun LAS

;;Dialogue box function to restore layer settings saved by previous function.
(defun
	 c:haws-lar (/ mvsset laline)
	(haws-core-borrow 0) ;_ end of if
	(HAWS-resfun)
	(HAWS-VSAVE '("clayer" "regenmode" "tilemode"))
	(command "._undo" "group")
	(setvar "cmdecho" 0)
	(setvar "regenmode" 0)
	(setq
		f1 (if (= (getvar "filedia") 1)
				 (open (GETFILED "Layer settings file" (HAWS-GETDNPATH) "lyr" 0) "r")
				 (car (HAWS-GETFIL "Layer settings file" (HAWS-GETDNPATH) "r" "lyr"))
			 ) ;_ end of if
	) ;_ end of setq
	(prompt "\nNow getting layer settings from file.  Please wait.  ")
	(command "layer" "un" "0,defpoints" "t" "0,defpoints" "on" "0,defpoints" "s" "0")
	(while (and (setq laline (read-line f1)) (= "LAYER" (cdr (assoc 0 (setq laline (read laline))))))
		(HAWS-restla laline)
	) ;_ end of while
	(command "")
	(if	laline
		(HAWS-restvp)
	) ;_ end of if
	(setq f1 (close f1))
	(setvar "regenmode" 1)
	(if	ltfail
		(prompt
			"\nAt least one linetype from layer settings file could not be loaded and was not restored."
		) ;_ end of prompt
	) ;_ end of if
	(prompt "\nDone.  Regen now if desired.")
	(command "._undo" "end")
	(haws-core-return) ;_ end of if
	(HAWS-VRSTOR)
	(princ)
)																				;end defun LAR

(defun
	 HAWS-resfun	()
	(defun
		 HAWS-restla	(layer / name color ltype ltfail off frozen locked)
		(cond
			((or (wcmatch (setq name (cdr (assoc 2 layer))) "*`**,*`?*") (tblsearch "LAYER" name))
			 (setq
				 name
					(cdr (assoc 2 layer))
				 color
					(cdr (assoc 62 layer))
				 off
					(< (cdr (assoc 62 layer)) 0)
				 frozen
					(= 1 (logand 1 (cdr (assoc 70 layer))))
				 locked
					(= 4 (logand 4 (cdr (assoc 70 layer))))
				 ltype
					(cdr (assoc 6 layer))
			 ) ;_ end of setq
			 (if (/= "0" name)
				 (command
					 "t"
					 name
					 "c"
					 color
					 name
					 (if off
						 "of"
						 "on"
					 ) ;_ end of if
					 name
					 (if frozen
						 "f"
						 "t"
					 ) ;_ end of if
					 name
					 (if locked
						 "lo"
						 "un"
					 ) ;_ end of if
					 name
				 ) ;_ end of command
			 ) ;_ end of if
			 (if (not (wcmatch ltype "*|*"))
				 (progn
					 (command "lt" ltype)
					 (if (tblsearch "LTYPE" ltype)
						 (command name)
						 (setq ltfail t)
					 ) ;_ end of if
				 ) ;_ end of progn
			 ) ;_ end of if
			)
		) ;_ end of cond
	) ;_ end of defun
	(defun
		 HAWS-restvp	(/ mvcen mvwid mvhgt mvpt minx miny maxx maxy lalist)
		(cond
			((or (= (getvar "tilemode") 0)
					 (progn
						 (prompt "\nViewport specific layer settings found in file.")
						 (initget 1 "Yes No")
						 (= (getkword "\nRestore viewport layers to drawing?[Yes/No]:") "Y")
					 ) ;_ end of progn
			 ) ;_ end of or
			 (command "tilemode" "0" "pspace" "._vplayer")
			 (while	laline
				 (cond
					 ((assoc 10 laline)
						(if	(and lalist mvpt)
							(command "f" lalist "s" mvpt "")
						) ;_ end of if
						(setq
							mvpt nil
							lalist ""
							mvcen
							 (cdr (assoc 10 laline))
							mvwid
							 (cdr (assoc 40 laline))
							mvhgt
							 (cdr (assoc 41 laline))
							minx
							 (- (car mvcen) (/ mvwid 2))
							miny
							 (- (cadr mvcen) (/ mvhgt 2))
							maxx
							 (+ (car mvcen) (/ mvwid 2))
							maxy
							 (+ (cadr mvcen) (/ mvhgt 2))
						) ;_ end of setq
						(grdraw (list minx miny) (list maxx maxy) 2)
						(grdraw (list minx maxy) (list maxx miny) 2)
						(prompt "\nYellow X highlights location of viewport saved in file.")
						(setq mvpt (cadr (entsel "\nSelect edge of viewport to recieve settings (return to skip):")))
						(redraw)
					 )
					 ((assoc 1003 laline) (cond (mvpt (setq lalist (strcat lalist (cdr (assoc 1003 laline)) ",")))))
				 ) ;_ end of cond
				 (setq laline (read-line f1))
				 (if laline
					 (setq laline (read laline))
				 ) ;_ end of if
			 ) ;_ end of while
			 (if (and mvpt lalist)
				 (command "f" lalist "s" mvpt "" "")
				 (command "")
			 ) ;_ end of if
			)
		) ;_ end of cond
	) ;_ end of defun
) ;_ end of defun
