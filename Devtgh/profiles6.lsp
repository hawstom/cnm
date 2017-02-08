;;;PROFILES.LSP
;;;(C) Copyright 1997 by Thomas Gail Haws
;;;A collection of related civil profile drafting tools

;;;GETHVX is used in various profile routines to get vertical exaggeration.
(defun
	 HAWS-gethvx	(/ hvold)
	(if	(not hvexag)
		(setq hvold 10.0)
		(setq hvold hvexag)
	) ;_ end of IF
	(if	(not elvscl)
		(setq elvscl 1)
	) ;_ end of if
	(setq
		hvexag
		 (getreal
			 (strcat "\nHoriz. scale / vertical scale " "(Usually 10 for P&P sheets) <" (rtos hvold 2 0) ">:")
		 ) ;_ end of GETREAL
	) ;_ end of SETQ
	(if	(not hvexag)
		(setq hvexag hvold)
	) ;_ end of IF
) ;_ end of DEFUN

(defun
	 c:haws-propipe (/ p1 p2 d1 pipsld d2 p3 p4)
	(HAWS-ERDF$@ 0)
	(HAWS-VSAVE '("filedia"))
	(HAWS-gethvx)
	(setq
		p1 (getpoint "\nEndpoint of pipe invert: ")
		p2 (getpoint p1 "\nSecond endpoint of pipe invert: ")
		d1 (* (/ (getdist "\nID of pipe (in.): ") 12) hvexag elvscl)
	) ;_ end of SETQ
	(cond ((setq pipsld (findfile "pipetabl.sld")) (setvar "filedia" 0) (command "._vslide" pipsld)))
	(setq d2 (* (/ (getdist "\nWall thickness (in.): ") 12) hvexag elvscl))
	(redraw)
	(command "line" p1 p2 "")
	(setq
		p3 (polar p1 (/ pi 2) d1)
		p4 (polar p2 (/ pi 2) d1)
	) ;_ end of SETQ
	(command "line" p3 p4 "")
	(setq
		p3 (polar p1 (/ pi -2) d2)
		p4 (polar p2 (/ pi -2) d2)
	) ;_ end of SETQ
	(cond
		((/= 0 d2)
		 (command "line" p3 p4 "")
		 (setq d1 (+ d1 d2))
		 (setq
			 p3	(polar p1 (/ pi 2) d1)
			 p4	(polar p2 (/ pi 2) d1)
		 ) ;_ end of SETQ
		 (command "line" p3 p4 "")
		)
	) ;_ end of COND
	(HAWS-ERRRST)
) ;_ end of DEFUN

(defun
	 c:haws-prosup	(/ pt1 pt2 botpip vrad pt3 pt4 ellip1 line1 line2 line3)
	(HAWS-ERDF$@ 0)
	(HAWS-VSAVE '("ucsfollow" "osmode" "clayer"))
	(HAWS-MKLAYR "PSUP")
	(setvar "ucsfollow" 0)
	(setvar "osmode" 16)
	(if	(not hvexag)
		(HAWS-gethvx)
	) ;_ end of IF
	(setq
		pt1
		 (getpoint "\nLeft ellipse quadrant: ")
		pt2
		 (getpoint pt1 "\nRight ellipse quadrant: ")
		botpip
		 (entsel "\nTop of lower pipe: ")
		vrad
		 (* hvexag elvscl (/ (distance pt1 pt2) 2))
		pt3
		 (polar pt1 (/ pi -2) vrad)
		pt4
		 (polar pt2 (/ pi -2) vrad)
	) ;_ end of SETQ
	(setvar "osmode" 0)
	(command "._ellipse" pt1 pt2 vrad)
	(setq ellip1 (entlast))
	(command "line" pt1 pt3 "")
	(setq line1 (entlast))
	(command "._line" pt2 pt4 "")
	(setq line2 (entlast))
	(command "._trim" line1 line2 "" (list ellip1 (polar pt1 (/ pi 2) (/ vrad 10))) "")
	(if	(/= "ELLIPSE" (cdr (assoc 0 (entget ellip1))))
		(setq ellip1 (entlast))
	) ;_ end of IF
	(command "._extend" botpip "" (list line1 pt3) (list line2 pt4) "")
	(setq pt3 (trans (cdr (assoc 11 (entget line1))) line1 1))
	(setq pt4 (trans (cdr (assoc 11 (entget line2))) line2 1))
	(command "._line" pt3 pt4 "")
	(setq line3 (entlast))
	(command "._hatch" "ansi31" "5" "0" ellip1 line1 line2 line3 "")
	(command "._erase" ellip1 line3 "")
	(redraw)
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
) ;_ end of DEFUN

(defun c:haws-pc () (c:haws-procb))
(defun
	 c:haws-procb (/ toppt botpt cbview width)
	(HAWS-ERDF$@ 0)
	(if	(not hvexag)
		(HAWS-gethvx)
	) ;_ end of IF
	(HAWS-VSAVE '("clayer"))
	(initget "Exist Prop")
	(setq
		exist
		 (= (getkword "Exist/Prop <Prop>: ") "Exist")
		;;    curbht (/ (getrealx "\nCurb height" (* curbht 12) 6)12)
		toppt
		 (getpoint "\nTop of curb: ")
		botpt
		 (list (car toppt) (cadr (getpoint toppt "\nInvert: ")) 0.0)
	) ;_ end of SETQ
	(initget "L R C")
	(setq cbview (getkword "Left, right, or center view?<L/R/C>?"))
	(setq
		width
		 (if (= cbview "C")
			 (/ (getreal "Width: ") 2)
			 2.0
		 ) ;_ end of IF
	) ;_ end of SETQ
	(if	exist
		(HAWS-MKLAYR "PXCB")
		(HAWS-MKLAYR "PCB")
	) ;_ end of IF
	(HAWS-drawcb botpt toppt cbview width nil)
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
) ;_ end of DEFUN

(defun
	 HAWS-drawcb	(botpt toppt cbview width brkpt / pt1 pt2 pt3 pt4 pt5 ptbl ptbr cb)
	(setq
		pt1	(polar botpt 0.0 width)
		pt2	(list (car pt1) (cadr toppt) 0.0)
		pt3	(polar botpt pi width)
		pt4	(list (car pt3) (cadr toppt) 0.0)
		pt5	(list (car botpt) (- (cadr toppt) (* hvexag elvscl 0.583)) 0.0)
	) ;_ end of SETQ
	(cond
		((= cbview "L") (command "._pline" pt4 "w" 0 "" (setq ptbl pt3) (setq ptbr botpt) pt5 ""))
		((= cbview "C")
		 (command "._pline" pt4 "w" 0 "" (setq ptbl pt3) (setq ptbr pt1) pt2 "")
		 (setq width (* width 2))
		)
		((= cbview "R") (command "._pline" pt5 "w" 0 "" (setq ptbl botpt) (setq ptbr pt1) pt2 ""))
	) ;_ end of COND
	(setq cb (entlast))
	(cond
		(brkpt
		 (setq
			 pt1 (polar (list (/ (+ (car ptbr) (car ptbl)) 2) (cadr brkpt) 0.0) 0.262 width)
			 pt2 (polar pt1 (/ pi 2) 2)
			 pt3 (polar pt1 3.403 (* width 2))
			 pt4 (polar pt2 3.403 (* width 2))
		 ) ;_ end of SETQ
		 (HAWS-MKLINE pt1 pt3)
		 (setq line1 (entlast))
		 (HAWS-MKLINE pt2 pt4)
		 (setq line2 (entlast))
		 (command "._trim" line1 line2 "" (list cb pt4) (list (entlast) pt1) "")
		)
	) ;_ end of COND
) ;_ end of DEFUN

(defun c:haws-pm () (c:haws-promh))
(defun
	 c:haws-promh (/ toppt botpt)
	(HAWS-ERDF$@ 0)
	(HAWS-VSAVE '("clayer"))
	(initget "Exist Prop")
	(setq
		exist
		 (= (getkword "Exist/Prop: ") "Exist")
		botpt
		 (getpoint "\nInvert: ")
		toppt
		 (list (car botpt) (cadr (getpoint botpt "\nRim: ")) 0.0)
	) ;_ end of SETQ
	(if	exist
		(HAWS-MKLAYR "PXMH")
		(HAWS-MKLAYR "PMH")
	) ;_ end of IF
	(HAWS-drawmh botpt toppt nil)
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
) ;_ end of DEFUN

;;Drawmh draws a cartoon manhole from botpt to toppt.
(defun
	 HAWS-drawmh	(botpt toppt brkpt / exist broken pt1 pt2 pt3 pt4 mh line1 line2)
	(setq
		pt1
		 (polar toppt 0 1)
		pt2
		 (polar toppt 0 -1)
		pt3
		 (polar botpt 0 -2)
		pt4
		 (polar botpt 0 2)
		botpt
		 (polar botpt 1.5708 -5)
		toppt
		 (polar toppt 1.5708 40)
	) ;_ end of SETQ
	(command "._pline" pt1 "w" 0 "" pt2 pt3 pt4 "close")
	(setq mh (entlast))
	(cond
		(brkpt
		 (setq
			 pt1 (polar brkpt 0.262 3)
			 pt2 (polar pt1 (/ pi 2) 2)
			 pt3 (polar pt1 3.403 6)
			 pt4 (polar pt2 3.403 6)
		 ) ;_ end of SETQ
		 (HAWS-MKLINE pt1 pt3)
		 (setq line1 (entlast))
		 (HAWS-MKLINE pt2 pt4)
		 (setq line2 (entlast))
		 (command "._trim" line1 line2 "" (list mh pt4) (list (entlast) pt1) "")
		)
	) ;_ end of COND
) ;_ end of DEFUN

(defun c:haws-pred () (command "edit" (strcat (getdnpath) ".pro")))

(defun
	 c:haws-proe	(/ laname invert diam pipsld walthk crown botpt toppt inrad outrad)
	(HAWS-ERDF$@ 0)
	(if	(not hvexag)
		(HAWS-gethvx)
	) ;_ end of IF
	(HAWS-VSAVE '("clayer" "filedia"))
	(initget "Exist Prop")
	(setq exist (= (getkword "Exist/Prop: ") "Exist"))
	(if	exist
		(HAWS-MKLAYR "PXEL")
		(HAWS-MKLAYR "PEL")
	) ;_ end of IF
	(setq
		invert
		 (getpoint "\nInvert point: ")
		diam
		 (/ (getreal "\nID (inches): ") 12)
	) ;_ end of SETQ
	(cond ((setq pipsld (findfile "pipetabl.sld")) (setvar "filedia" 0) (command "._vslide" pipsld)))
	(setq
		walthk
		 (/ (getreal "\nWall thickness (inches): ") 12)
		crown
		 (polar invert (/ pi 2.0) (* diam hvexag elvscl))
		botpt
		 (polar invert (/ pi -2.0) (* walthk hvexag elvscl))
		toppt
		 (polar crown (/ pi 2.0) (* walthk hvexag elvscl))
		inrad
		 (/ diam 2.0)
		outrad
		 (+ (/ diam 2.0) walthk)
	) ;_ end of SETQ
	(redraw)
	(command "._ellipse" invert crown inrad)
	(if	(/= 0 walthk)
		(command "._ellipse" botpt toppt outrad)
	) ;_ end of IF
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
) ;_ end of DEFUN

(defun
	 c:haws-pldr	(/ p1 p2 ds ts as dg ang left p3 p4 p5 str temp)
	(HAWS-ERDF$@ 0)
	(setq
		p1 (getpoint "\nStart point for leader:")
		p2 (getpoint p1 "\nEnd point for leader:")
		ds (getvar "dimscale")
		ts (* ds (getvar "dimtxt"))
		as (* ds (getvar "dimasz"))
		dg (* ds (abs (getvar "dimgap")))
		ang
		 (angle p1 p2)
		left
		 (minusp (cos ang))
		p3 (polar p1 ang as)
		p4 (polar
				 p2
				 (if left
					 pi
					 0
				 ) ;_ end of IF
				 as
			 ) ;_ end of POLAR
		p5 (polar
				 (polar
					 p4
					 (if left
						 pi
						 0
					 ) ;_ end of IF
					 dg
				 ) ;_ end of POLAR
				 (/ pi -2)
				 (/ ts 2)
			 ) ;_ end of POLAR
		str
		 (progn (initget "Exist Prop") (getkword "Exist/Prop: "))
		exist
		 (= str "Exist")
		str
		 (if (= str "Exist")
			 "EXIST "
			 ""
		 ) ;_ end of IF
		str
		 (strcat
			 str
			 (cond
				 ((progn (initget "Tc GUtter Pavmt Grnd") (= (setq temp (getkword "Tc/GUtter/Pavmt/Grnd: ")) "Tc"))
					"TOP OF CURB "
				 )
				 ((= temp "GUtter") "GUTTER ")
				 ((= temp "Pavmt") "PVMT ")
				 ((= temp "Grnd") "GROUND ")
			 ) ;_ end of COND
		 ) ;_ end of STRCAT
		str
		 (strcat
			 str
			 (cond
				 ((progn (initget "Left Right Center") (= (setq temp (getkword "Left/Right/Center: ")) "Left"))
					"LEFT"
				 )
				 ((= temp "Right") "RIGHT")
				 ((= temp "Center") "@ C/L")
			 ) ;_ end of COND
		 ) ;_ end of STRCAT
	) ;_ end of SETQ
	(if	exist
		(HAWS-MKLAYR "PXLDR")
		(HAWS-MKLAYR "PLDR")
	) ;_ end of IF
	(cond
		((>= (atof (getvar "acadver")) 14) (command "._leader" p1 p2 "" str ""))
		(t
		 (command "._pline" p1 "w" 0 (/ as 3) p3 "w" 0 0 p2 p4 "")
		 (HAWS-MKTEXT
			 (if left
				 "r"
				 nil
			 ) ;_ end of IF
			 p5
			 ts
			 0
			 str
		 ) ;_ end of mktext
		)
	) ;_ end of COND
	(HAWS-ERRRST)
) ;_ end of DEFUN

(defun
	 c:haws-newpro	(/ expold proinputfile ss1)
	(setq expold (getvar "expert"))
	(setvar "expert" 0)
	(command "._undo" "m" "._layer" "off" "*" "" "")
	(HAWS-MKLAYR "PROSTRT")
	(setq
		ss1					 (ssget "X" (list (cons 8 "PPRO-*")))
		proinputfile (open (strcat (getdnpath) ".pro") "r")
	) ;_ end of SETQ
	(command "._erase" ss1 "" "._layer" "u" "ppro-*" "")
	(c:haws-tgh_pro)
	(command "._layer" "on" "*" "")
	(prompt "\nType Undo Back to undo entire profile.")
	(princ)
) ;_ end of DEFUN

(defun c:haws-oldpro () (command "script" "oldpro") (princ))

;;File version converter for PRO
;;Adds or removes fields between the slope and label column.
(defun
	 c:haws-profc (/ colwid flist fm fname i m n rdlin temp tempfile)
	;;Make a string into a 10 character long string
	(defun HAWS-mkfield (string colwid) (substr (strcat string "          ") 1 colwid))
	(HAWS-ERDF$@ 0)
	(setq f1 (open (GETFILED "List of files to convert" (getdnpath) "lst" 0) "r"))
	(while (setq rdlin (read-line f1)) (setq flist (cons (findfile rdlin) flist)))
	(textscr)
	(setq
		f1 (close f1)
		colwid
		 (getint "New column width: ")
		m	(getint "Old number of fields between slope and label: ")
		n	(getint "New number of fields between slope and label: ")
		i	-1
	) ;_ end of SETQ
	(if	(findfile (setq tempfile (strcat "pro" (substr (getvar "loginname") 1 5) ".tmp")))
		(command "del" tempfile)
	) ;_ end of IF
	(while (setq fname (nth (setq i (1+ i)) flist))
		(setq f1 (open fname "r"))
		(setq f2 (open tempfile "w"))
		(while (setq rdlin (read-line f1))
			(cond
				((= (substr rdlin 1 6) "COLWID")
				 (if (zerop (setq fm (atoi (substr rdlin 8))))
					 (setq fm (substr rdlin 8))
				 ) ;_ end of IF
				 (setq rdlin (strcat "COLWID " (itoa colwid)))
				)
				((= (substr rdlin 1 6) "PRFPNT")
				 (setq
					 rdlin
						(strcat
							(HAWS-mkfield (HAWS-RDFLD 1 rdlin fm 1) colwid)
							(HAWS-mkfield (HAWS-RDFLD 2 rdlin fm 1) colwid)
							(HAWS-mkfield (HAWS-RDFLD 3 rdlin fm 1) colwid)
							(HAWS-mkfield (HAWS-RDFLD 4 rdlin fm 1) colwid)
							(HAWS-mkfield (HAWS-RDFLD 5 rdlin fm 1) colwid)
							(if	(> n 0)
								(HAWS-mkfield
									(if	(setq temp (HAWS-ATOFX (HAWS-RDFLD 6 rdlin fm 3) "*RIM*" 0))
										(rtos temp 2 2)
										""
									) ;_ end of IF
									colwid
								) ;_ end of mkfield
								""
							) ;_ end of IF
							(if	(> n 1)
								(HAWS-mkfield
									(if	(setq temp (HAWS-ATOFX (HAWS-RDFLD 6 rdlin fm 3) "*\"*,*IN*" 1))
										(rtos temp 2 2)
										""
									) ;_ end of IF
									colwid
								) ;_ end of mkfield
								""
							) ;_ end of IF
							(rdfld (+ 6 m) rdlin fm 3)
						) ;_ end of STRCAT
				 ) ;_ end of SETQ
				)
				((wcmatch rdlin "REFPNT*,PRECIS*,HVEXAG*,PROTYP*")
				 (setq
					 rdlin
						(strcat
							(mkfield (rdfld 1 rdlin fm 1) colwid)
							(mkfield (rdfld 2 rdlin fm 1) colwid)
							(mkfield (rdfld 3 rdlin fm 1) colwid)
							(mkfield (rdfld 4 rdlin fm 1) colwid)
							(mkfield (rdfld 5 rdlin fm 1) colwid)
						) ;_ end of STRCAT
				 ) ;_ end of SETQ
				)
			) ;_ end of COND
			(while (= " " (substr rdlin (max (strlen rdlin) 1)))
				(setq rdlin (substr rdlin 1 (1- (strlen rdlin))))
			) ;_ end of WHILE
			(write-line rdlin f2)
		) ;_ end of WHILE
		(setq
			f1 (close f1)
			f2 (close f2)
		) ;_ end of SETQ
		(command "sh" (strcat "copy " tempfile " " fname))
		(command "sh" (strcat "del " tempfile))
		(princ)
	) ;_ end of WHILE
) ;_ end of DEFUN
;;;;-----------------------------PRO------------------------------------------
(defun
	 c:haws-tgh_pro ;;Localize the variables
						 (/				 i				pltpt1a	 ts				lingap	 lup			fm			 sta			el1dsc	 elev1		offset
							slope		 slptyp		num2		 ptopt		lbltxt	 acadx		acady1	 acady2		lblmhrim mhrim		lblcbtc
							cbtc		 pexist		pltpt1	 pltsta		pltelv	 invmir		invup		 statxt		offtxt	 el1txt		toptx
							botpt		 lblpt1		lblpt2	 lblpt3		txlen		 txpt1		txpt2		 txpt3		txpt4		 txpt5		txpt6
							txpt7		 txpt8		toppt		 txpt11		txpt12	 txpt13		txpt14
						 )
	(prompt "\nProfile drafter version 5.06.  Copyright 2000, Thomas Gail Haws.")
	(defun
		 c:haws-tgh2_pro	()
		(while (progn
						 (initget "Ref Type Vertical Precision New File Plot")
						 (setq proopt (getkword "\nRef. point/prof. Type/Vert. exag./Precision/New prof./File/Plot: "))
						 (cond
							 ((= proopt
									 "Ref"
									 ((= proopt "Type") (gettyp))
									 ((= proopt "Vertical")
										 (gethvx)
										 (if f2
											 (write-line (strcat "HVEXAG    " hvexag) f2)
										 ) ;_ end of IF
									 )
									 ((= proopt "Precision") (getprec))
									 ((= proopt "New")
										 (setq pltpt1 nil)
										 (prompt "\nEnded previous profile, starting new profile.")
										 (if f2
											 (write-line "NEWPRF" f2)
										 ) ;_ end of IF
									 )
									 ((= proopt "File") (getfop))
									 ((= proopt "Plot") (pltpro))
								) ;_ end of =
							 )
						 ) ;_ end of cond
					 ) ;_ end of progn
		) ;_ end of while
	) ;_ end of defun
	;;Call the different setup sub-functions
	(defun
		 proset	(/ opt)
		(while (setq
						 opt
							(progn
								(initget "Ref Type Vertical Precision New File")
								(getkword "\nRef. point/prof. Type/Vert. exag./Precision/New prof./File: ")
							) ;_ end of PROGN
					 ) ;_ end of SETQ
			(cond
				((= opt "Ref") (getref))
				((= opt "Type") (gettyp))
				((= opt "Vertical")
				 (gethvx)
				 (if f2
					 (write-line (strcat "HVEXAG    " hvexag) f2)
				 ) ;_ end of IF
				)
				((= opt "Precision") (getprec))
				((= opt "New")
				 (setq pltpt1 nil)
				 (prompt "\nEnded previous profile, starting new profile.")
				 (if f2
					 (write-line "NEWPRF" f2)
				 ) ;_ end of IF
				)
				((= opt "File") (getfop))
			) ;_ end of COND
		) ;_ end of WHILE
	) ;_ end of DEFUN
	;;Get the profile reference point to tie profile to ACAD coordinates
	(defun
		 getref	(/ r)
		(while (not
						 (and
							 (setq r (getpoint "\nPick a reference point:  "))
							 (setq refsta (getreal "\nEnter reference point station as a real no.: "))
							 (setq refelv (getreal "\nEnter reference point elevation: "))
						 ) ;_ end of AND
					 ) ;_ end of NOT
		) ;_ end of WHILE
		(setq
			refptx
			 (car r)
			refpty
			 (cadr r)
		) ;_ end of SETQ
		(if	f2
			(write-line
				(strcat
					(mkfield "REFPNT" colwid)
					(mkfield (rtos refptx 2 2) colwid)
					(mkfield (rtos refpty 2 2) colwid)
					(mkfield (rtos refsta 2 2) colwid)
					(mkfield (rtos refelv 2 2) colwid)
				) ;_ end of STRCAT
				f2
			) ;_ end of WRITE-LINE
		) ;_ end of IF
	) ;_ end of DEFUN
	;;Setup layers for the profile
	(defun
		 setlay	(/ llinc llinlt ltxc ltxlt)
		(cond
			((and (= proht -20000) (wcmatch protyp "X*")) (setq laylin (getlayr "PROXDIM")))
			((= proht -20000) (setq laylin (getlayr "PRODIM")))
			((and (= proht -10000) (wcmatch protyp "X*")) (setq laylin (getlayr "PROXLINE")))
			((= proht -10000) (setq laylin (getlayr "PROLINE")))
			((and (= proht 1000) (wcmatch protyp "X*")) (mklayr "PROPNT") (setq laylin (getlayr "PROXEL")))
			((= proht 1000) (mklayr "PROPNT") (setq laylin (getlayr "PROEL")))
			((and (>= proht 0) (wcmatch protyp "X*,MESA")) (setq laylin (getlayr "PROXSTRT")))
			((>= proht 0) (setq laylin (getlayr "PROSTRT")))
			((and (< proht 0) (wcmatch protyp "X*"))
			 (setq
				 laylin
					(list
						(strcat (car (getlayr "PROPRE")) (car (setq laylin (getlayr "PROEXMID"))) (substr protyp 2))
						(cadr laylin)
						(caddr laylin)
					) ;_ end of LIST
			 ) ;_ end of SETQ
			)
			((< proht 0)
			 (setq
				 laylin
					(cond
						((getlayr (strcat "PRO" protyp)))
						((list (strcat (car (setq laylin (getlayr "PROPRE"))) protyp) (cadr laylin) (caddr laylin)))
					) ;_ end of COND
			 ) ;_ end of SETQ
			)
		) ;_ end of COND
		(setq
			laytx
			 (getlayr "PROTXSUF")
			layxtx
			 (getlayr "PROXTXSUF")
		) ;_ end of SETQ
		(if	(wcmatch protyp "X*")
			(setq
				protyp
				 (substr protyp 2)
				laytx	layxtx
				pexist t
			) ;_ end of SETQ
			(setq pexist nil)
		) ;_ end of IF
		(setq laytx (subst (strcat (car laylin) (car laytx)) (car laytx) laytx))
		(if	(not (tblsearch "LAYER" (car laylin)))
			(mklayr laylin)
		) ;_ end of IF
		(if	(not (tblsearch "LAYER" (car laytx)))
			(mklayr laytx)
		) ;_ end of IF
		(setq
			laylin
			 (car laylin)
			laytx
			 (car laytx)
			laypre
			 (car (getlayr "PROPRE"))
			laytxs
			 (car (getlayr "PROTXSUF"))
		) ;_ end of SETQ
	) ;_ end of DEFUN
	;;Get the type of profile and set up layers.
	(defun
		 gettyp	(/ pipsld)
		(setq
			walthk 0
			protyp ""
		) ;_ end of SETQ
		(initget 1 "Mesa Gutter PAv PIpe Line POints")
		(setq proht (getkword "\nMesa/Gutter/PAvement/PIpe/Line only/POints only:"))
		(setq
			proht
			 (cond
				 ((= proht "Mesa") (setq protyp "MESA") (* (/ (getreal "\nCurb height (in.): ") 12) hvexag elvscl))
				 ((= proht "Gutter") (* (/ (getreal "\nCurb height (in.): ") 12) hvexag elvscl))
				 ((= proht "PAv") 0)
				 ((= proht "PIpe")
					(setq protyp (strcase (progn (initget 1 "Wat Sew SD Irr Gas") (getkword "\nWat/Sew/SD/Irr/Gas: "))))
					(setq proht (* (/ (getreal "\nPipe ID (in): ") -12) hvexag elvscl))
					(cond ((setq pipsld (findfile "pipetabl.sld")) (setvar "filedia" 0) (command "._vslide" pipsld)))
					(setq walthk (* (/ (getreal "\nWall thickness (in): ") -12) hvexag elvscl))
					(redraw)
					(eval proht)
				 )
				 ((= proht "Line") -10000)
				 ((= proht "POints") 1000)
			 ) ;_ end of COND
		) ;_ end of SETQ
		(initget 1 "Exist Prop")
		(if	(= (getkword "Exist/Prop: ") "Exist")
			(setq protyp (strcat "X" protyp))
		) ;_ end of IF
		(if	f2
			(write-line
				(strcat
					"PROTYP    "
					(mkfield
						(rtos
							(if	(< -10000 proht 1000)
								(* (/ proht hvexag elvscl) 12.0)
								proht
							) ;_ end of IF
							2
							6
						) ;_ end of RTOS
					) ;_ end of mkfield
					(mkfield (rtos (* (/ walthk hvexag elvscl) 12) 2 6))
					protyp
				) ;_ end of STRCAT
				f2
			) ;_ end of WRITE-LINE
		) ;_ end of IF
		(setlay)
	) ;_ end of DEFUN
	;;Get precisions for numbers.
	(defun
		 getprec ()
		(while (progn
						 (initget "STa Offset Elev SLope")
						 (prompt "\nChange rounding precision for STa/Offset/Elev/SLope: ")
						 (setq temp (getkword))
					 ) ;_ end of PROGN
			(prompt (strcat "\nNumber of digits to right of decimal place for " temp ": "))
			(set (read (strcat (substr temp 1 2) "prec")) (getint))
		) ;_ end of WHILE
		(setvar "luprec" stprec)
	) ;_ end of DEFUN
	;;Increment elevations in a file
	(defun
		 incfil	(/ incsew incsd incgen)
		(setq
			incsew
			 (getreal "\nIncrement for underground sewer elevations: ")
			incsd
			 (getreal "\nIncrement for underground storm drain elevations: ")
			incgen
			 (getreal "\nIncrement for all other elevations: ")
		) ;_ end of SETQ
		(setq
			f1 (cond
					 (proinputfile (open (setq fname (strcat proinputfile ".pro")) "r"))
					 ((= (getvar "filedia") 1)
						(open (setq fname (getfiled "File to increment" (getdnpath) "pro" 0)) "r")
					 )
					 (t (car (getfil "File to increment" (getdnpath) "r" "pro")))
				 ) ;_ end of COND
		) ;_ end of SETQ
		(while (if f1
						 (rdfile)
					 ) ;_ end of IF
			(if	(and sta (/= sta "Setup"))
				;;if it is a profile point
				(progn
					(pltpro)
					;;plot the point
					(if	f2
						;;If requested, write all
						(write-line
							(strcat
								;;the point information
								"PRFPNT    "
								;;to a file (f2).
								(mkfield (rtos sta 2 3))
								(mkfield
									(if	offset
										(rtos offset 2 3)
										""
									) ;_ end of IF
								) ;_ end of mkfield
								(mkfield (rtos elev1 2 3))
								(mkfield
									(if	slope
										(rtos slope 2 4)
										""
									) ;_ end of IF
								) ;_ end of mkfield
								(mkfield
									(if	num2
										(rtos num2 2 3)
										""
									) ;_ end of IF
								) ;_ end of mkfield
								(cond
									(lbltxt)
									("")
								) ;_ end of COND
							) ;_ end of STRCAT
							f2
						) ;_ end of WRITE-LINE
					) ;_ end of IF
					(setq sta nil)
					;;Reset profile point to nil.
				) ;_ end of PROGN
				(if	(not f1)
					(proset)
				) ;_ end of IF
				;;If not a point, do setup.
			) ;_ end of IF
			(princ "\rReading line ")
			(princ (setq i (1+ i)))
		) ;_ end of WHILE
	) ;_ end of DEFUN
	;;Get file options
	(defun
		 getfop	(/ rdwrt fprmpt fbit datfil)
		(initget "Read Save Increment")
		(setq
			rdwrt
			 (getkword "\nRead from file/Save to file/Increment elevations in a file: ")
			f1 nil
			f2 nil
		) ;_ end of SETQ
		(cond
			((= rdwrt "Increment")
			 (princ "\nSorry.  Incrementing not yet available.")
			 ;;(incfil)
			)
			(t
			 (initget "Create Append")
			 (if (= rdwrt "Read")
				 (setq
					 rdwrt "r"
					 fprmpt	"File to read"
					 fbit	0
					 prover4 nil
				 ) ;_ end of SETQ
				 (setq
					 rdwrt
						(if	(= (getkword "\Create new file/Append existing file: ") "Create")
							"w"
							"a"
						) ;_ end of IF
					 fprmpt	"File to save input to"
					 fbit	1
				 ) ;_ end of SETQ
			 ) ;_ end of IF
			 (setq
				 datfil
					(cond
						((= (getvar "filedia") 1) (open (setq fname (getfiled fprmpt (getdnpath) "pro" fbit)) rdwrt))
						(t (car (getfil fprmpt (getdnpath) rdwrt "pro")))
					) ;_ end of COND
			 ) ;_ end of SETQ
			 (if (= rdwrt "r")
				 (setq f1 datfil)
				 (progn (setq f2 datfil) (write-line "COLWID 10" f2))
			 ) ;_ end of IF
			)
		) ;_ end of COND
	) ;_ end of DEFUN
	;;Prompt for station to plot
	(defun
		 getpnt	()
		(setq
			sta	t
			elev1	nil
			lbltxt ""
		) ;_ end of SETQ
		(while (and sta (not elev1))
			(while (progn (initget "Setup Label") (= (setq sta (getreal "\n<Station>/Label/Setup: ")) "Label"))
				(setq lbltxt (getstring 1 "\nEnter label for this station: "))
			) ;_ end of WHILE
			(if	(and sta (/= sta "Setup"))
				(progn
					(while (progn (initget "Offset") (= (setq elev1 (getreal "<Elevation>/Offset: ")) "Offset"))
						(setq offset (getreal "\nOffset: "))
					) ;_ end of WHILE
					(if	(and
								elev1
								(> (abs (- elev1 refelv)) 100)
								(= (strcase (getstring "\nElev. diff. is over 100 feet.  Plot? Y/N: ")) "N")
							) ;_ end of AND
						(setq elev1 nil)
					) ;_ end of IF
				) ;_ end of PROGN
				(setq elev1 t)
			) ;_ end of IF
		) ;_ end of WHILE
		;;Return nil if not given station.
		sta
	) ;_ end of DEFUN
	;;Read and interpret the lines of the data file.
	;;First six letters of each data file line are the line title.
	(defun
		 rdfile	(/ linerr rdlin title)
		(if	(not fm)
			(setq fm ",")
		) ;_ end of IF
		(setq rdlin (read-line f1))
		(if	rdlin
			(progn
				(setq title (substr rdlin 1 6))
				(cond
					((= (substr rdlin 1 2) "0,")
					 (setq
						 sta
							(rdfld 2 rdlin fm 0)
						 offset	nil
						 elev1
							(rdfld 4 rdlin fm 0)
						 slope nil
						 lbltxt	""
					 ) ;_ end of SETQ
					)
					((= title "PRFPNT")
					 (setq
						 sta
							(rdfld 2 rdlin fm 0)
						 offset
							(rdfld 3 rdlin fm 0)
						 elev1
							(rdfld 4 rdlin fm 0)
						 slope
							(rdfld 5 rdlin fm 0)
						 num2
							(rdfld 6 rdlin fm 0)
						 lbltxt
							(rdfld 7 rdlin fm 3)
							;;8 for version 4, 7 for version 5
					 ) ;_ end of SETQ
					 (if (not sta)
						 (setq linerr t)
					 ) ;_ end of IF
					)
					((= title "COLWID")
					 ;;If format line,
					 (if ;;read field width (default ",")
							 (zerop (setq fm (atoi (substr rdlin 8))))
						 (setq fm (substr rdlin 8))
					 ) ;_ end of IF
					 (if (or (not fm) (= "" fm))
						 (setq linerr t)
					 ) ;_ end of IF
					)
					((= title "REFPNT")
					 ;;If reference line,
					 (setq
						 ;;read ref. pt info.
						 refptx
							(rdfld 2 rdlin fm 0)
						 refpty
							(rdfld 3 rdlin fm 0)
						 refsta
							(rdfld 4 rdlin fm 0)
						 refelv
							(rdfld 5 rdlin fm 0)
						 stascl
							(rdfld 6 rdlin fm 0)
						 stascl
							(if	stascl
								stascl
								1
							) ;_ end of if
						 elvscl
							(rdfld 7 rdlin fm 0)
						 elvscl
							(if	elvscl
								elvscl
								1
							) ;_ end of if
					 ) ;_ end of SETQ
					 (if (not (and refptx refpty refsta refelv))
						 (setq linerr t)
					 ) ;_ end of IF
					)
					((= title "HVEXAG")
					 (setq hvexag (rdfld 2 rdlin fm 0))
					 (if (not hvexag)
						 (setq linerr t)
					 ) ;_ end of IF
					)
					((= title "PROTYP")
					 (setq
						 proht
							(rdfld 2 rdlin fm 0)
						 proht
							(if	(or (= proht 1000) (<= proht -10000))
								proht
								(/ (* proht hvexag elvscl) 12)
							) ;_ end of IF
						 walthk
							(rdfld 3 rdlin fm 0)
						 walthk
							(cond
								(walthk)
								(0)
							) ;_ end of COND
						 protyp
							(strcase (rdfld 4 rdlin fm 1))
						 slptyp
							(rdfld 5 rdlin fm 1)
					 ) ;_ end of SETQ
					 (if (and (< -10000 proht 0) (= protyp ""))
						 (setq linerr t)
						 (setlay)
					 ) ;_ end of IF
					 (if (and (/= slptyp "") (not slprecin))
						 (setq slprec 5)
					 ) ;_ end of if
					)
					((= title "PRECIS")
					 (setq
						 stprec
							(fix (rdfld 2 rdlin fm 0))
						 ofprec
							(fix (rdfld 3 rdlin fm 0))
						 elprec
							(fix (rdfld 4 rdlin fm 0))
						 slprecin
							(fix (rdfld 5 rdlin fm 0))
						 slprec	slprecin
					 ) ;_ end of SETQ
					)
					((= title "NEWPRF")
					 (setq
						 pltpt1	nil
						 toppt nil
					 ) ;_ end of setq
					)
				) ;_ end of COND
				(if	linerr
					(princ (strcat ".  Input error found:\n" rdlin "\n"))
				) ;_ end of IF
				t
			) ;_ end of PROGN
			nil
		) ;_ end of IF
	) ;_ end of DEFUN
	;;Plot profile given sta, elev1, and offset.
	(defun
		 plthardpro	()
		(setq
			;;Extract plotting instructions then actual elevation from input
			invmir
			 (cond
				 ((and elev1 (/= (distof (rtos elev1 2 2)) (distof (rtos elev1 2 3))))
					(> 0.35 (- (* 100 elev1) (fix (* 100 elev1))) 0.15)
				 )
				 ((and pltpt1a (wcmatch (strcase lbltxt) "* SHEET *,")))
			 ) ;_ end of COND
			invup
			 (cond
				 ((and elev1 (/= (distof (rtos elev1 2 2)) (distof (rtos elev1 2 3))))
					(> 0.25 (- (* 100 elev1) (fix (* 100 elev1))) 0.05)
				 )
			 ) ;_ end of COND
			elev1
			 (if (and (= proht 1000) (setq temp (distofx (strcase lbltxt) "*TOP*=*,*BOT*=*,*INV*=*" 0)))
				 temp
				 ;;elevation from ellipse label for ellipses
				 elev1
				 ;;elevation from elevation field for everything else
			 ) ;_ end of IF
			pltelv
			 (not (minusp elev1))
			elev1
			 (abs (atof (rtos elev1 2 2)))
			;;Extract plotting instructions and actual station from input
			pltsta
			 (and (/= protyp "MESA") (/= proht 1000) (= (distof (rtos sta 2 2)) (distof (rtos sta 2 3))))
			sta
			 (atof (rtos sta 2 2))
			;;Translate station and elevation to a world coordinate point
			acadx
			 (+ refptx (* stascl (- sta refsta)))
			acady1
			 (+ refpty (* hvexag elvscl (- elev1 refelv)))
			;;Extract mh rim, cb second elevation, or other y2 from input
			acady2
			 (if num2
				 (+ refpty (* hvexag elvscl (- num2 refelv)))
			 ) ;_ end of IF
			lblnum2
			 (cond
				 ((wcmatch (strcase lbltxt) "*RIM=#*") (atofx lbltxt "*RIM=*" 0)) ;Manhole rim
				 ((wcmatch (strcase lbltxt) "*TC=*") (atofx lbltxt "*TC=*" 0)) ;Inlet TC
				 ((wcmatch (strcase lbltxt) "*GRATE=*") (+ 0.67 (atofx lbltxt "*GRATE=*" 0))) ;Grate TC
				 ((= proht 1000)
					(atofx (strcase lbltxt) "*\"*,* IN*" 1) ;Ellipse diameter
				 )
			 ) ;_ end of COND
			lblacady2
			 (if lblnum2
				 (+ refpty (* hvexag elvscl (- lblnum2 refelv)))
			 ) ;_ end of IF
			mhrim
			 (if (wcmatch (strcase lbltxt) "*RIM=#*")
				 (list
					 acadx
					 (cond
						 (acady2)
						 (lblacady2)
					 ) ;_ end of COND
					 0.0
				 ) ;_ end of LIST
			 ) ;_ end of IF
			cbtc
			 (if (and (wcmatch (strcase lbltxt) "*CB*,*INLET*,*BASIN*") (or acady2 lblacady2))
				 (list
					 acadx
					 (cond
						 (acady2)
						 (lblacady2)
					 ) ;_ end of COND
					 0.0
				 ) ;_ end of LIST
			 ) ;_ end of IF
			;;Set acad plot points
			pltpt1
			 (list acadx acady1 0.0)
			pltpt2
			 (if (or num2 lblnum2)
				 (list
					 acadx
					 (cond
						 (acady2)
						 (lblacady2)
					 ) ;_ end of COND
					 0.0
				 ) ;_ end of LIST
			 ) ;_ end of IF
		) ;_ end of setq
		;;Set points for secondary lines depending on profile type
		(cond
			((< -10000 proht 1000)
			 (setq
				 toppt
					(polar
						pltpt1
						(/ pi 2)
						(if	(= proht -10000)
							0
							(abs (+ proht walthk))
						) ;_ end of IF
					) ;_ end of POLAR
				 botpt
					(polar pltpt1 (* pi 0.5) walthk)
			 ) ;_ end of setq
			)
		) ;_ end of COND
	) ;_ end of defun
	;;Plot profile given sta, elev1, and offset.
	(defun
		 HAWS-pltsoftpro	(/ cnffreept cnftestpt i j k lblcnf ldrcnf lngstr ldrcnfpt)
		(setq
			;;Make label strings
			;;Wrap label string as requested by user with semi-colons.
			lbllst
			 (HAWS-WRAP lbltxt 3 ";")
			;;Find longest string to be plotted.
			lngstr
			 (car lbllst)
			lngstr
			 (foreach
					stringi	lbllst
				 (if (> (strlen stringi) (strlen lngstr))
					 (setq lngstr stringi)
					 lngstr
				 ) ;_ end of IF
			 ) ;_ end of FOREACH
			;;Translate elev1 into text.
			el1dsc
			 (cond
				 ((= protyp "MESA") "FL ")
				 ((< 0 proht)
					(if	(/= "" protyp)
						(strcat protyp "=")
						"G="
					) ;_ end of IF
				 )
				 ((= 0 proht)
					(if	(/= "" protyp)
						(strcat protyp "=")
						"P="
					) ;_ end of IF
				 )
				 ((> 0 proht) "INV=")
			 ) ;_ end of COND
			el1txt
			 (strcat el1dsc (rtos elev1 2 elprec))
		) ;_ end of SETQ
		;;Setq points for upright labels
		(cond
			((< -10000 proht 1000)
			 (setq
				 ;;Translate station real number into station string
				 statxt
					(strcat "STA " (HAWS-RTOSTA sta stprec))
				 ;;Insert offset into station string if given
				 offtxt
					(if	offset
						(strcat
;;;O							(if	pltsta
;;;O								(strcat statxt ", ")
;;;O								""
;;;O							) ;_ end of IF
							(rtos (abs offset) 2 ofprec)
							(if	(minusp offset)
								"' LT"
								"' RT"
							) ;_ end of IF
						) ;_ end of STRCAT
						""
					) ;_ end of IF
				 ;;Set crown (top of curb) elevation, translate to text, and get length.
				 toptx
					(+ elev1 (/ (abs proht) hvexag elvscl))
				 toptx
					(cond ((< 0 proht 1000) (strcat "TC=" (rtos toptx 2 elprec))))
				 lngstr
					(if	(and pltsta (< (strlen lngstr) (strlen statxt)))
						statxt
						lngstr
					) ;_ end of IF
				 lngstr
					(if	(< (strlen lngstr) (strlen offtxt))
						offtxt
						lngstr
					) ;_ end of IF
				 lngstr
					(if	(< (strlen lngstr) (strlen el1txt))
						el1txt
						lngstr
					) ;_ end of IF
				 txlen
					(+ (caadr (textbox (list (cons 1 lngstr) (cons 40 ts)))) 0)
				 ;;Set points for text and label lines
				 lblpt1
					(polar
						(cond
							(pltpt2)
							(toppt)
						) ;_ end of COND
						(/ pi 2)
						(if	mhrim
							(* 0.05 hvexag elvscl)
							0
						) ;_ end of IF
					) ;_ end of POLAR
				 lblpt2
					(polar lblpt1 (/ pi 2) (* 2.0 ts))
				 cnftestpt lblpt2
				 cnffreept
					'(0.0 0.0 0.0)
				 ldrcnfpt
					'(0.0 0.0 0.0)
				 i 0
				 j 0
				 k 0
			 ) ;_ end of SETQ
			 ;; Find the nearest of up to twenty conflict free label positions or one conflict
			 ;; free straight up
			 ;; position.
			 (while	(and (< i 100) (< j 20) (< k 1))
				 (setq
					 lblcnf
						(ssget
							"C"
							(polar
								cnftestpt
								pi
								(* ts
									 (+	(* lingap 0.9)
											(* (1+ lingap)
												 (+	(if	pltsta
															1
															0
														) ;_ end of IF
														(if	(= offtxt "")
															0
															1
														) ;_ end of if
														(if	(= (car lbllst) "")
															0
															(length lbllst)
														) ;_ end of IF
												 ) ;_ end of +
											) ;_ end of *
									 ) ;_ end of +
								) ;_ end of *
							) ;_ end of POLAR
							(polar
								(polar
									cnftestpt
									0
									(* ts
										 (+	lingap
												(* (1+ lingap)
													 (if (< proht 0)
														 0
														 (if pltpt2
															 2
															 1
														 ) ;_ end of IF
													 ) ;_ end of IF
												) ;_ end of *
										 ) ;_ end of +
									) ;_ end of *
								) ;_ end of POLAR
								(/ pi 2)
								txlen
							) ;_ end of POLAR
							(list (cons 8 (strcat laypre "*")))
						) ;_ end of SSGET
				 ) ;_ end of SETQ
				 (setq
					 ldrcnf
						(ssget
							"F"
							(list (polar lblpt1 (angle lblpt1 cnftestpt) (* ts 1)) cnftestpt)
							(list (cons 8 (strcat laypre "*" laytxs)))
						) ;_ end of SSGET
				 ) ;_ end of SETQ
				 ;;Save nearest point that has no label conflict.
				 (if (and (not lblcnf) (< (distance lblpt2 cnftestpt) (distance lblpt2 ldrcnfpt)))
					 (setq ldrcnfpt cnftestpt)
				 ) ;_ end of IF
				 ;;Save nearest point that no conflict at all.
				 (if (not (or lblcnf ldrcnf))
					 (progn
						 (setq j (1+ j))
						 (if (< (distance lblpt2 cnftestpt) (distance lblpt2 cnffreept))
							 (setq cnffreept cnftestpt)
						 ) ;_ end of IF
					 ) ;_ end of PROGN
				 ) ;_ end of IF
				 ;;Save first straight up point that has no label conflict and quit looking.
				 (if (and (not lblcnf) (equal (car lblpt2) (car cnftestpt) 0.01))
					 (progn (setq cnffreept cnftestpt) (setq k (1+ k)))
				 ) ;_ end of IF
				 (setq
					 i (1+ i)
					 cnftestpt
						(polar
							(polar lblpt2 (* pi 0.5) (* ts (fix (* i 0.05))))
							;;Move up every 20 (1/0.05) tries.
							(* pi i)
							;;Switch side to side
							(* ts (1+ lingap) (rem (/ (1+ i) 2.0) 20))
							;;Increase horiz distance.
						) ;_ end of POLAR
				 ) ;_ end of SETQ
			 ) ;_ end of WHILE
			 (setq
				 lblpt2
					(cond
						;;First use any straight up or extra close position, even with ldr conflict.
						((or (= k 1)
								 (and ldrcnfpt (> (- (distance lblpt2 cnffreept) (distance lblpt2 ldrcnfpt)) (* ts 4.0)))
						 ) ;_ end of OR
						 ldrcnfpt
						)
						;;Next choice, use totally free position.
						((> j 0) cnffreept)
						;;Last choice, use the original location.
						(t lblpt2)
					) ;_ end of COND
				 lblpt3
					(polar lblpt2 (/ pi 2) txlen)
				 ;;Top end of label line
				 diapt1
					(polar lblpt3 (/ pi 2) (* ts 2))
				 diapt2
					(polar diapt1 (/ pi -4) (* ts (sqrt 2)))
				 diapt3
					(polar diapt1 (* 5 (/ pi 4)) (* ts (sqrt 2)))
				 brkpt
					(if	(and (or mhrim cbtc) acady2 lblacady2 (/= acady2 lblacady2))
						(polar
							toppt
							(/ pi 2)
							(/ (distance
									 toppt
									 (cond
										 (mhrim)
										 (cbtc)
									 ) ;_ end of COND
								 ) ;_ end of DISTANCE
								 2
							) ;_ end of /
						) ;_ end of POLAR
					) ;_ end of IF
				 txpt1
					(polar lblpt2 0 (* ts -1 lingap))
				 ;;1 line above line
				 txpt4
					(polar lblpt2 0 (* ts lingap))
				 ;;1 line below line
				 txpt5
					(polar lblpt2 0 (* ts (+ 1 (* 2 lingap))))
					;;2 lines below line
			 ) ;_ end of SETQ
			)
		) ;_ end of COND
		;;Setq points for horizontal labels on leaders
		(cond
			((or (= proht 1000) (< -10000 proht 0))
			 (setq
				 i 0
				 j 0
			 ) ;_ end of setq
			 ;; Find the first conflict free label position in user selected direction.
			 (while	(and (< i 15) (< j 1))
				 (setq
					 txpt6
						(polar
							pltpt1
							(* pi
								 (if invup
									 1
									 -1
								 ) ;_ end of IF
								 (if invmir
									 0.75
									 0.25
								 ) ;_ end of IF
							) ;_ end of *
							(+ (* ts (+ 3.0 i)) (* walthk -1.717))
						) ;_ end of POLAR
					 ;;Inv angle point
					 lngstr
						(if	(and (= proht 1000) (> (strlen lbltxt) 0))
							lngstr
							el1txt
						) ;_ end of IF
					 txlen
						(+ (caadr (textbox (list (cons 1 lngstr) (cons 40 ts)))) 0)
					 txpt7
						(polar
							txpt6
							(if	invmir
								pi
								0
							) ;_ end of IF
							txlen
						) ;_ end of POLAR
					 txpt8
						(polar txpt6 (* pi 0.5) (* ts 0.5 lingap))
						;;Invert text point
						;;No 9 or 10
				 ) ;_ end of SETQ
				 (setq
					 lblcnf
						(ssget
							"C"
							txpt6
							(polar txpt7 (/ pi 2) (* ts (+ 1 (* 2 lingap)))) ;_ end of POLAR
							(list (cons 8 (strcat laypre "*")))
						) ;_ end of SSGET
				 ) ;_ end of SETQ
				 ;;Flag success for first point that has no label conflict.
				 (if (not lblcnf)
					 (setq j (1+ j))
				 ) ;_ end of IF
				 (setq i (1+ i))
			 ) ;_ end of WHILE
			)
		) ;_ end of COND
		;;Set points for "MESA" profile.
		(cond
			((= protyp "MESA")
			 (setq
				 txpt11
					(polar toppt (* pi 0.25) (* 3.08 ts))
				 ;;Slant label line text point
				 txpt12
					(polar txpt11 (* pi 0.25) txlen)
				 ;;Slant label line end
				 txpt13
					(polar txpt11 (* pi 0.75) (* ts lingap))
				 ;;Slant text point above line
				 txpt14
					(polar txpt11 (* pi 0.75) (* ts -1 lingap))
					;;Slant text point below line
			 ) ;_ end of SETQ
			)
		) ;_ end of COND
		(cond
			((and (< -10000 proht 0) (or pltsta offset (/= lbltxt ""))) (HAWS-pltlbl1))
			((and (<= 0 proht 999.9) (or pltsta offset pltelv (/= lbltxt ""))) (HAWS-pltlbl1))
			((= proht 1000) (HAWS-pltpnt))
			((= proht -10000)
			 (if (not slope)
				 (setq slope 1000)
			 ) ;_ end of IF
			)
			((= proht -20000)
			 (if (not slope)
				 (setq slope 1000)
			 ) ;_ end of IF
			)
		) ;_ end of COND
		(cond ((and (< -10000 proht 0) pltelv) (HAWS-pltlbl2)))
		(setq offset nil)
	) ;_ end of DEFUN
	;;Plot a vertical upward profile point label.
	(defun
		 HAWS-pltlbl1 (/ i)
		(HAWS-MKLAYR (list laytx "" ""))
		(cond
			((= protyp "MESA")
			 ;;Slanted label
			 (HAWS-MKLINE toppt txpt12)
			 (HAWS-MKTEXT nil txpt13 ts (* pi 0.25) lbltxt)
			 (HAWS-MKTEXT "TL" txpt14 ts (* pi 0.25) el1txt)
			)
			(t
			 ;;Vertical label
			 (HAWS-MKLINE botpt lblpt1)
			 (HAWS-MKLINE lblpt1 lblpt2)
			 (HAWS-MKLINE lblpt2 lblpt3)
			 (cond
				 ((/= "" offtxt)
					(HAWS-MKTEXT nil txpt1 ts (/ pi 2) offtxt)
					(setq txpt1 (polar txpt1 0 (* ts -1 (+ 1 lingap))))
				 )
			 ) ;_ end of cond
			 (cond
				 (pltsta (HAWS-MKTEXT nil txpt1 ts (/ pi 2) statxt))
				 (t (setq txpt1 (polar txpt1 0 (* ts (+ 1 lingap)))))
			 ) ;_ end of COND
			 (setq i -1)
			 (while	(setq temp (nth (setq i (1+ i)) (reverse lbllst)))
				 (HAWS-MKTEXT nil (setq txpt1 (polar txpt1 0 (* ts -1 (+ 1 lingap)))) ts (/ pi 2) temp)
			 ) ;_ end of WHILE
			 ;;Plot elevation text for curbs.
			 (cond
				 ((and (<= 0 proht) pltelv)
					(HAWS-MKTEXT
						"tl"
						txpt4
						ts
						(/ pi 2)
						(cond
							(toptx)
							(el1txt)
						) ;_ end of COND
					) ;_ end of mktext
					(if	toptx
						(HAWS-MKTEXT "tl" txpt5 ts (/ pi 2) el1txt)
					) ;_ end of IF
				 )
			 ) ;_ end of COND
			 ;;Plot diamond
			 (cond
				 ((and (>= proht 0) (wcmatch (strcase lbltxt) "*GC*,*G`.C`.*,*DE CHANGE*"))
					(command "._pline" lblpt3 diapt2 diapt1 diapt3 "c")
				 )
				 ((and (>= proht 0) (wcmatch (strcase lbltxt) "*GB*,*G`.C`.*,*DE BREAK*"))
					(command "._solid" diapt1 diapt2 diapt3 lblpt3 "")
				 )
			 ) ;_ end of COND
			 ;;Plot manhole
			 (cond
				 ((< proht 0)
					(if	(not f1)
						(HAWS-MKTEXT "tl" txpt4 ts (/ pi 2) "RIM=")
					) ;_ end of IF
					(cond
						(mhrim
						 (if pexist
							 (HAWS-MKLAYR "PROXMH")
							 (HAWS-MKLAYR "PROMH")
						 ) ;_ end of IF
						 (HAWS-drawmh botpt mhrim brkpt)
						)
						(cbtc
						 (if pexist
							 (HAWS-MKLAYR "PPROXCB")
							 (HAWS-MKLAYR "PROCB")
						 ) ;_ end of IF
						 (HAWS-drawcb
							 botpt
							 cbtc
							 (if pltpt1a
								 "R"
								 "L"
							 ) ;_ end of IF
							 2
							 brkpt
						 ) ;_ end of drawcb
						)
					) ;_ end of COND
				 )
			 ) ;_ end of COND
			)
		) ;_ end of COND
	) ;_ end of DEFUN
	;;Plot a downward slant elevation tail.
	(defun
		 HAWS-pltlbl2 (/ i)
		(HAWS-MKLAYR (list laytx "" ""))
		(HAWS-MKLINE pltpt1 txpt6)
		(HAWS-MKLINE txpt6 txpt7)
		(HAWS-MKTEXT
			(if	invmir
				"BR"
				"BL"
			) ;_ end of IF
			txpt8
			ts
			0
			el1txt
		) ;_ end of mktext
	) ;_ end of DEFUN
	;;Plot a circle or ellipse
	(defun
		 HAWS-pltpnt	(/ i xinvert xdiam xwalthk xcrown xbotpt xtoppt xinrad xoutrad txpt1 txpt2 y)
		(cond
			((and (= lbltxt "") (not num2))
			 (prompt "Point circles are on DEFPOINTS layer. Will not plot.")
			 (HAWS-MKLAYR (list "defpoints" "" ""))
			 (entmake (list (cons 0 "CIRCLE") (append '(10) (trans pltpt1 1 0)) (cons 40 ts)))
			)
			(t
			 (foreach
					x	(quote ((xbotpt "*BOT*=*") (xtoppt "*TOP*=*") (xinvert "*INV*=*")))
				 (if (setq y (HAWS-DISTOFX (strcase lbltxt) (cadr x) 0))
					 (set (car x) y)
				 ) ;_ end of IF
			 ) ;_ end of FOREACH
			 (setq
				 xdiam
					(/ (cond
							 (num2)
							 (lblnum2)
						 ) ;_ end of COND
						 12
					) ;_ end of /
				 xwalthk
					(if	slope
						(/ slope 12)
						(if	walthk
							walthk
							0
						) ;_ end of IF
					) ;_ end of IF
				 xinvert
					(list
						(car pltpt1)
						(+ refpty
							 (*	hvexag
									elvscl
									(- (cond
											 (xinvert xinvert)
											 (xtoppt (- xtoppt xdiam xwalthk))
											 (xbotpt (+ xbotpt xwalthk))
											 (t elev1)
										 ) ;_ end of COND
										 refelv
									) ;_ end of -
							 ) ;_ end of *
						) ;_ end of +
						0.0
					) ;_ end of LIST
				 xcrown
					(polar xinvert (/ pi 2.0) (* xdiam hvexag elvscl))
				 xbotpt
					(polar xinvert (/ pi -2.0) (* xwalthk hvexag elvscl))
				 xtoppt
					(polar xcrown (/ pi 2.0) (* xwalthk hvexag elvscl))
				 xinrad
					(/ xdiam 2.0)
				 xoutrad
					(+ (/ xdiam 2.0) xwalthk)
			 ) ;_ end of SETQ
			 (HAWS-MKLAYR (list laylin "" ""))
			 (command "._ellipse" xinvert xcrown xinrad)
			 (if (/= 0 xwalthk)
				 (command "._ellipse" xbotpt xtoppt xoutrad)
			 ) ;_ end of IF
			 (cond
				 ((/= lbltxt "")
					(HAWS-MKLAYR (list laytx "" ""))
					(HAWS-MKLINE pltpt1 txpt6)
					(HAWS-MKLINE txpt6 txpt7)
					(setq i -1)
					(while (setq temp (nth (setq i (1+ i)) lbllst))
						(HAWS-MKTEXT
							(if	invmir
								"R"
								nil
							) ;_ end of IF
							txpt8
							ts
							0
							temp
						) ;_ end of mktext
						(setq txpt8 (polar txpt8 (/ pi 2) (* ts -1 (+ 1 lingap))))
					) ;_ end of WHILE
				 )
			 ) ;_ end of COND
			)
		) ;_ end of COND
	) ;_ end of DEFUN
	;;Plot a line between two points at a vertical offset
	(defun HAWS-pltlin (pt1 pt2 offset) (HAWS-MKLINE (polar pt1 (/ pi 2) offset) (polar pt2 (/ pi 2) offset)))
	;;Connect two profile points with lines
	(defun
		 HAWS-connecthard ()
		(HAWS-MKLAYR (list laylin "" ""))
		(cond
			((/= -20000 proht) (HAWS-MKLINE pltpt1a pltpt1))
			((= proht -20000)
			 (command
				 "._dim1"
				 "al"
				 pltpt1a
				 pltpt1
				 pltpt1
				 (if (= lbltxt "")
					 " "
					 lbltxt
				 ) ;_ end of IF
			 ) ;_ end of COMMAND
			)
		) ;_ end of COND
		(cond
			((and (< 0 proht 1000) toppta) (HAWS-MKLINE toppta toppt))
			((and (= 0 proht) toppta (not (equal toppta pltpt1a))) (HAWS-MKLINE toppta toppt))
			((< -10000 proht 0) (HAWS-pltlin pltpt1a pltpt1 (abs proht)))
		) ;_ end of COND
		(cond ((< walthk 0) (HAWS-pltlin pltpt1a pltpt1 walthk) (HAWS-pltlin pltpt1a pltpt1 (- 0 walthk proht))))
	) ;_ end of defun
	;;Connect two profile points with slope text, etc.
	(defun
		 HAWS-connectsoft (/ slopestring pt1 gutlip)
		;;Calculate slope
		(cond
			((not slope)
			 (setq
				 slope
					(* (/ (- (cadr pltpt1) (cadr pltpt1a)) (- (car pltpt1) (car pltpt1a)) stascl)
						 (/ 100.0 hvexag elvscl)
					) ;_ end of *
			 ) ;_ end of setq
			)
			((= slope 220.1)
			 (setq
				 gutlip
					(if	(< (cadr pltpt1) (cadr pltpt1a))
						-0.083
						0.083
					) ;_ end of IF
			 ) ;_ end of SETQ
			 (setq
				 slope
					(* (/	(- (cadr pltpt1) (cadr pltpt1a) (* hvexag elvscl gutlip))
								(max (- (car pltpt1) (car pltpt1a) 1.5) 0.001)
								stascl
						 ) ;_ end of /
						 (/ 100.0 hvexag elvscl)
					) ;_ end of *
			 ) ;_ end of SETQ
			)
		) ;_ end of COND
		(if	(and
					(not f1)
					(/= proht -10000)
					(setq temp (getreal (strcat "\nSlope (%)<" (rtos slope 2 slprec) "%>(1000 for no slope): ")))
				) ;_ end of AND
			(setq slope temp)
		) ;_ end of IF
		(HAWS-MKLAYR (list laytx "" ""))
		;;Build slope string
		(setq
			slope
			 (strcat
				 (if (minusp slope)
					 ""
					 "+"
				 ) ;_ end of IF
				 (if (= slptyp "")
					 (strcat (rtos slope 2 slprec) "%")
					 (strcat (rtos (/ slope 100) 2 slprec) " " slptyp "/" slptyp)
				 ) ;_ end of if
			 ) ;_ end of STRCAT
		) ;_ end of SETQ
		(cond
			((< proht 0)
			 (setq
				 slopestring
					(strcat
						(rtos (abs (- (car pltpt1a) (car pltpt1))) 2 0)
						" LF "
						(rtos (abs (* (/ proht hvexag elvscl) 12)) 2 0)
						"\" "
						(cond
							((= protyp "WAT") "WATER")
							((= protyp "SEW") "SEWER")
							((= protyp "SD") "SD")
							((= protyp "IRR") "IRR")
							((= protyp "GAS") "GAS")
							(t protyp)
						) ;_ end of COND
						" @ S="
						slope
					) ;_ end of STRCAT
			 ) ;_ end of SETQ
			 ;;If pipe slope string too long, shorten
			 (cond
				 ((< (distance pltpt1a pltpt1) (caadr (textbox (list (cons 1 slopestring) (cons 40 ts)))))
					(setq
						slopestring
						 (strcat
							 (rtos (abs (- (car pltpt1a) (car pltpt1))) 2 0)
							 " LF "
							 (rtos (abs (* (/ proht hvexag elvscl) 12)) 2 0)
							 "\", "
							 slope
						 ) ;_ end of STRCAT
					) ;_ end of SETQ
				 )
			 ) ;_ end of COND
			)
			(t (setq slopestring (strcat "S=" slope)))
		) ;_ end of COND
		;;Position text at middle point
		(setq
			pt1
			 (polar
				 (list (/ (+ (car pltpt1a) (car pltpt1)) 2) (/ (+ (cadr pltpt1a) (cadr pltpt1)) 2))
				 (/ pi 2)
				 (if (= proht -10000)
					 0
					 (/ (abs proht) 2.0)
				 ) ;_ end of IF
			 ) ;_ end of POLAR
		) ;_ end of SETQ
		;;Adjust text position vertically if needed
		(cond
			;;If string too long, move below lines
			((< (distance pltpt1a pltpt1) (caadr (textbox (list (cons 1 slopestring) (cons 40 ts)))))
			 (setq pt1 (polar pt1 -1.57 (+ (/ (abs proht) 2.0) (abs walthk) (* ts 1.5))))
			)
			;;If too little headroom, move above lines
			((< (abs proht) (* 2 ts)) (setq pt1 (polar pt1 1.57 (+ ts (/ (abs proht) 2.0) (abs walthk)))))
		) ;_ end of COND
		(HAWS-MKTEXT "m" pt1 ts (atan (/ (sin (angle pltpt1a pltpt1)) (cos (angle pltpt1a pltpt1)))) slopestring)
		(setq slope nil)
	) ;_ end of DEFUN
	;;End of subfunctions, begin main function.
	(HAWS-ERDF$@ 0)
	(terpri)
	(setq
		i	1
		ts (* (getvar "dimtxt") (getvar "dimscale"))
		lingap 0.5
		lup
		 (getvar "luprec")
	) ;_ end of SETQ
	(foreach
		 temp	'((hvexag 10) (stprec lup) (ofprec lup) (elprec 2) (slprec 3))
		(if	(not (eval (car temp)))
			(set (car temp) (eval (cadr temp)))
		) ;_ end of IF
	) ;_ end of FOREACH
	(if	(not hvexag)
		(setq hvexag 10)
	) ;_ end of IF
	(if	proinputfile
		(setq f1 proinputfile)
	) ;_ end of IF
	(while (not (or f1 refsta)) (prompt "\nPlease set reference point.") (HAWS-proset))
	(while (not (or f1 proht)) (prompt "\nPlease set profile type.") (HAWS-proset))
	(if	(= 0 (cdr (assoc 40 (tblsearch "STYLE" (getvar "textstyle")))))
		(command "._text" (list 0.0 0.0 0.0) ts (angtos (/ pi 2)) "TEST")
		(command "._text" (list 0.0 0.0 0.0) (angtos (/ pi 2)) "TEST")
	) ;_ end of IF
	(entdel (entlast))
	(if	(not walthk)
		(setq walthk 0)
	) ;_ end of IF
	(HAWS-VSAVE '("clayer" "osmode" "luprec" "filedia"))
	(setvar "osmode" 0)
	(setvar "dimzin" 0)
	;;While there's input,
	(while (if f1
					 (HAWS-rdfile)
					 (HAWS-getpnt)
				 ) ;_ end of IF
		;;if it is a profile point
		(if	(and sta (/= sta "Setup"))
			(progn
				(command "._undo" "g")
				;;save the last point
				(setq pltpt1a (cond (pltpt1)))	;
																				;and top of curb
				(setq toppta (cond (toppt)))
				;;plot the point
				(HAWS-plthardpro)
				;;connect to last point
				(if	(and (> 1000 proht) pltpt1a)
					(HAWS-connecthard)
				) ;_ end of IF
				;;Add slope label if
				(if
					(and
						(> 1000 proht -10000);profile type is appropriate
						pltpt1a;there was a previous point
						(/= slope 1000);user didn't supress slope
						(or slope (/= 0 (- (car pltpt1) (car pltpt1a))));slope isn't divide by zero
					)
					(HAWS-connectsoft)
				) ;_ end of IF
				;;Add point label
				(HAWS-pltsoftpro)
				(command "._undo" "e")
				;;If requested, write all
				;;the point information
				;;to a file (f2).
				(if	f2
					(write-line
						(strcat
							"PRFPNT    "
							(HAWS-mkfield (rtos sta 2 3))
							(HAWS-mkfield
								(if	offset
									(rtos offset 2 3)
									""
								) ;_ end of IF
							) ;_ end of mkfield
							(HAWS-mkfield (rtos elev1 2 3))
							(HAWS-mkfield
								(if	slope
									(rtos slope 2 4)
									""
								) ;_ end of IF
							) ;_ end of mkfield
							(HAWS-mkfield
								(if	num2
									(rtos num2 2 3)
									""
								) ;_ end of IF
							) ;_ end of mkfield
							(cond
								(lbltxt)
								("")
							) ;_ end of COND
						) ;_ end of STRCAT
						f2
					) ;_ end of WRITE-LINE
				) ;_ end of IF
				;;Reset profile point to nil.
				(setq sta nil)
			) ;_ end of PROGN
			;;Otherwise, if not reading from file, do setup.
			(if	(not f1)
				(HAWS-proset)
			) ;_ end of IF
		) ;_ end of IF
		(princ "\rReading line ")
		(princ (setq i (1+ i)))
	) ;_ end of WHILE
	(setq
		f1 (if f1
				 (close f1)
			 ) ;_ end of IF
		f2 (if f2
				 (close f2)
			 ) ;_ end of IF
	) ;_ end of SETQ
	;;Close files
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
	;;and end program.
) ;_ end of DEFUN

;;----------------------------LIST POINT----------------------------
(defun
	 c:haws-lst (/ pt x y sta el1)
	(setq
		pt	(getpoint "\nPick Point: ")
		x		(car pt)
		y		(cadr pt)
		sta	(HAWS-RTOSTA (+ refsta (- x refptx)) nil)
		el1	(rtos (+ refelv (/ (- y refpty) hvexag elvscl)))
	) ;_ end of SETQ
	(prompt "\n           STATION = ")
	(princ sta)
	(prompt "  ELEVATION = ")
	(princ el1)
	(princ)
) ;_ end of DEFUN
;;------------------------------------------------------------------
;;ELLABEL labels elevations with reference to a known point.
(defun
	 c:haws-ellabel (/ HAWS-ellplt pt1)
	(defun
		 HAWS-ellset	(/ option rpt)
		(while (progn
						 (initget "Reference Vertical Flip")
						 (setq option (getkword "\nReference point/Vertical Exaggeration/text Flip angle/<return to exit>: "))
					 ) ;_ end of PROGN
			(cond
				((= option "Flip")
				 (initget 1)
				 (setq
					 flpang
						(getangle "\nFlip text to right side of line beyond what angle?: ")
					 flpang
						(- (/ pi 2) flpang)
				 ) ;_ end of SETQ
				)
				((= option "Reference")
				 (setq
					 rpt
						(getpoint "\nReference point: ")
					 refpty
						(cadr rpt)
					 refelv
						(getreal "\nReference elevation: ")
				 ) ;_ end of SETQ
				)
				((= option "Vertical") (HAWS-gethvx))
			) ;_ end of COND
		) ;_ end of WHILE
	) ;_ end of DEFUN
	(defun
		 HAWS-ellplt	(/ ang1 ts el1 elev2 txlen left pt2 pt3 pt4 pt5)
		(setq pt5 nil)
		(while (progn
						 (initget 1 "Two")
						 (setq ang1 (getangle pt1 "\nTwo elevs/<leader rotation>: "))
						 (= ang1 "Two")
					 ) ;_ end of PROGN
			(setq pt5 (getpoint "\nSecond elevation point: "))
		) ;_ end of WHILE
		(setq
			ts (* (getvar "dimscale") (getvar "dimtxt"))
			flpang
			 (cond
				 (flpang)
				 (0)
			 ) ;_ end of COND
			el1
			 (getstring
				 (if pt5
					 "\nFirst description: "
					 "\nDescription: "
				 ) ;_ end of IF
			 ) ;_ end of GETSTRING
			el1
			 (if (= el1 "")
				 ""
				 (strcat " " el1)
			 ) ;_ end of IF
			el1
			 (strcat (rtos (+ refelv (/ (- (cadr pt1) refpty) hvexag elvscl)) 2 2) el1)
			elev2
			 (if pt5
				 (getstring "\nSecond description: ")
				 ""
			 ) ;_ end of IF
			elev2
			 (if (= elev2 "")
				 ""
				 (strcat " " elev2)
			 ) ;_ end of IF
			elev2
			 (if pt5
				 (strcat (rtos (+ refelv (/ (- (cadr pt5) refpty) hvexag elvscl)) 2 2) elev2)
				 " "
			 ) ;_ end of IF
			txlen
			 (max
				 (caadr (textbox (list (cons 1 el1) (cons 40 ts))))
				 (caadr (textbox (list (cons 1 elev2) (cons 40 ts))))
			 ) ;_ end of MAX
			left
			 (minusp (cos (+ flpang ang1)))
			pt2
			 (polar pt1 ang1 (* ts 2))
			pt3
			 (polar pt2 ang1 txlen)
			pt4
			 (polar
				 pt2
				 (+	ang1
						(/ pi
							 (if left
								 2
								 -2
							 ) ;_ end of IF
						) ;_ end of /
				 ) ;_ end of +
				 (/ ts 3)
			 ) ;_ end of POLAR
		) ;_ end of SETQ
		(setvar "osmode" 0)
		(command "._line" pt1 pt3 "")
		(HAWS-MKTEXT
			(if	left
				"BR"
				"BL"
			) ;_ end of IF
			pt2
			ts
			(if	left
				(+ ang1 pi)
				ang1
			) ;_ end of IF
			el1
		) ;_ end of mktext
		(if	pt5
			(HAWS-MKTEXT
				(if	left
					"TR"
					"TL"
				) ;_ end of IF
				pt4
				ts
				(if	left
					(+ ang1 pi)
					ang1
				) ;_ end of IF
				elev2
			) ;_ end of mktext
		) ;_ end of IF
	) ;_ end of DEFUN
	(HAWS-ERDF$@ 0)
	(HAWS-VSAVE '("osmode"))
	(setvar "dimzin" 0)
	(if	(not hvexag)
		(setq hvexag 10)
	) ;_ end of IF
	(if	(not refpty)
		(HAWS-ellset)
	) ;_ end of IF
	(while (progn (setvar "osmode" 13) (initget "Setup") (setq pt1 (getpoint "\nSetup/<point to label>: ")))
		(cond
			((= pt1 "Setup") (HAWS-ellset))
			(t (HAWS-ellplt))
		) ;_ end of COND
	) ;_ end of WHILE
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
) ;_ end of DEFUN

;;---------------------------STATION LABEL-------------------------
(defun
	 c:haws-stalabel	(/ pnt1 inc n sta1 endsta sta)
	(HAWS-ERDF$@ 0)
	(HAWS-MKLAYR "PSTALBL")
	(HAWS-VSAVE '("luprec"))
	(setvar "luprec" 0)
	(setq
		pnt1
		 (getpoint "\nTop center point of first label: ")
		inc
		 (getdist "\nDistance between labels: ")
		n	(getint "\nNumber of labels: ")
		sta1
		 (getint "\nFirst station: ")
		endsta
		 (+ sta1 (* (1- n) inc))
		sta	sta1
	) ;_ end of SETQ
	(while (<= sta endsta)
		(HAWS-MKTEXT
			"tc"
			(list (+ (car pnt1) (- sta sta1)) (cadr pnt1))
			(* (getvar "dimscale") 0.12)
			0
			(HAWS-RTOSTA sta nil)
		) ;_ end of mktext
		(setq sta (+ inc sta))
	) ;_ end of WHILE
	(HAWS-VRSTOR)
	(HAWS-ERRRST)
	(princ)
) ;_ end of DEFUN
;;---------------------------ELEVATION LABEL------------------------
(defun
	 c:haws-elv (/ pnt1 inc n elv1 endelv elv)
	(if	(not hvexag)
		(HAWS-gethvx)
	) ;_ end of IF
	(HAWS-MKLAYR "PELEVLBL")
	(setq
		pnt1
		 (getpoint "\nBottom right point of first label: ")
		inc
		 (getint "\nElevation increment: ")
		n	(getint "\nNumber of labels: ")
		elv1
		 (getint "\nFirst elevation: ")
		endelv
		 (+ elv1 (* (1- n) inc))
		elv	elv1
	) ;_ end of SETQ
	(while (<= elv endelv)
		(HAWS-MKTEXT
			"br"
			(list (car pnt1) (+ (* hvexag elvscl (- elv elv1)) (cadr pnt1)))
			(* (getvar "dimscale") 0.12)
			0
			(itoa elv)
		) ;_ end of mktext
		(setq elv (+ inc elv))
	) ;_ end of WHILE
	(princ)
) ;_ end of DEFUN
;;--------------------------SLOPE---------------------------------
(defun
	 c:haws-grd (/ a)
	(defun
		 HAWS-gr1 (/ sta1 el1 sta2 elev2 stad eld x1 gr)
		(prompt "\nTo Determine the Slope Between 2 Points.")
		(setq
			sta1
			 (getreal "\nEnter 1st Station as a real number: ")
			el1
			 (getreal "\nEnter 1st Elevation: ")
			sta2
			 (getreal "\nEnter 2nd Station as a real number: ")
			elev2
			 (getreal "\nEnter 2nd Elevation: ")
			stad
			 (- sta2 sta1)
			eld
			 (- elev2 el1)
			x1 (/ eld stad)
			gr (* x1 100.0)
		) ;_ end of SETQ
		(prompt "\nSLOPE = ")
		(prompt (rtos gr 2 4))
		(prompt "%")
	) ;_ end of DEFUN
	(defun
		 HAWS-gr2 (/ sta1 el1 sta2 gr stad elev2)
		(prompt "\nTo Determine the Elevation at a Specified Station.")
		(setq
			sta1	(getreal "\nEnter 1st Station as a real number: ")
			el1		(getreal "\nEnter 1st Elevation: ")
			sta2	(getreal "\nEnter 2nd Station as a real number: ")
			gr		(getreal "\nEnter Slope in Percent: ")
			stad	(- sta2 sta1)
			elev2	(+ el1 (* stad (/ gr 100.0)))
		) ;_ end of SETQ
		(prompt "\nELEVATION AT STATION ")
		(princ sta2)
		(prompt " = ")
		(prompt (rtos elev2 2 4))
	) ;_ end of DEFUN
	(initget 1 "Slope Elevation")
	(setq a (getkword "\nFind Slope/Elevation:"))
	(cond
		((= a "Slope") (HAWS-gr1))
		((= a "Elevation") (HAWS-gr2))
	) ;_ end of COND
	(princ)
) ;_ end of DEFUN

(defun c:haws-grc () (HAWS-linblk "*GRC" "PGC"))
(defun c:haws-grb () (HAWS-linblk "*GRB" "PGB"))
(defun
	 HAWS-linblk	(blname bllay / pt1 pt2)
	(HAWS-MKLAYR bllay)
	(setq pt1 (getpoint "\nFirst point: "))
	(command "._pline" pt1)
	(while (setq pt2 (getpoint pt1 "Next point: ")) (command pt2) (setq pt1 pt2))
	(command "")
	(command "._insert" blname pt1 (* (getvar "dimscale") (getvar "dimtxt")) "")
) ;_ end of DEFUN

(defun
	 c:haws-cu	(/ po_in)
	;;Previously named po (profile offset)
	(HAWS-ERDF$@ 0)
	(setq
		po_in
		 (getstring
			 (strcat
				 "\nDistance to copy (negative for down)<"
				 (if pofset
					 pofset
					 "0"
				 ) ;_ end of IF
				 ">:"
			 ) ;_ end of STRCAT
		 ) ;_ end of GETSTRING
	) ;_ end of SETQ
	(if	(/= po_in "")
		(setq pofset po_in)
	) ;_ end of IF
	(prompt "Select profile line to copy:")
	(command "copy" pause "" "0,0" (strcat "0," pofset))
	(HAWS-ERRRST)
) ;_ end of DEFUN
(princ)
