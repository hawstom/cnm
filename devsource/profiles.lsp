;;;PROFILES.LSP
;;;(C) Copyright 1997 by Thomas Gail Haws
;;;A collection of related civil profile drafting tools

;;;GETHVX is used in various profile routines to get vertical exaggeration.
(defun haws-gethvx
              (/ hvold)
  (if (not hvexag)
    (setq hvold 10.0)
    (setq hvold hvexag)
  ) ;_ end of IF
  (if (not *haws-elvscl*)
    (setq *haws-elvscl* 1)
  ) ;_ end of if
  (setq
    hvexag
     (getreal
       (strcat
         "\nHoriz. scale / vertical scale "
         "(Usually 10 for P&P sheets) <"
         (rtos hvold 2 0)
         ">:"
       ) ;_ end of strcat
     ) ;_ end of GETREAL
  ) ;_ end of SETQ
  (if (not hvexag)
    (setq hvexag hvold)
  ) ;_ end of IF
) ;_ end of DEFUN

(defun c:haws-propipe
            (/ p1 p2 d1 pipsld d2 p3 p4)
  (haws-core-init 273)
  (haws-vsave '("filedia"))
  (haws-gethvx)
  (setq
    p1 (getpoint "\nEndpoint of pipe invert: ")
    p2 (getpoint p1 "\nSecond endpoint of pipe invert: ")
    d1 (* (/ (getdist "\nID of pipe (in.): ") 12) hvexag *haws-elvscl*)
  ) ;_ end of SETQ
  (cond
    ((setq pipsld (findfile "pipetabl.sld"))
     (setvar "filedia" 0)
     (vl-cmdf "._vslide" pipsld)
    )
  ) ;_ end of cond
  (setq
    d2 (* (/ (getdist "\nWall thickness (in.): ") 12)
          hvexag
          *haws-elvscl*
       ) ;_ end of *
  ) ;_ end of setq
  (redraw)
  (vl-cmdf "._line" p1 p2 "")
  (setq
    p3 (polar p1 (/ pi 2) d1)
    p4 (polar p2 (/ pi 2) d1)
  ) ;_ end of SETQ
  (vl-cmdf "._line" p3 p4 "")
  (setq
    p3 (polar p1 (/ pi -2) d2)
    p4 (polar p2 (/ pi -2) d2)
  ) ;_ end of SETQ
  (cond
    ((/= 0 d2)
     (vl-cmdf "._line" p3 p4 "")
     (setq d1 (+ d1 d2))
     (setq
       p3 (polar p1 (/ pi 2) d1)
       p4 (polar p2 (/ pi 2) d1)
     ) ;_ end of SETQ
     (vl-cmdf "._line" p3 p4 "")
    )
  ) ;_ end of COND
  (haws-core-restore)
) ;_ end of DEFUN

(defun c:haws-prosup
           (/  botpip ellip1 hvexag left line1 line2 line3
              pt1 pt2 pt3 pt4 ptcen right ucsydir vrad
           )
  (haws-core-init 274)
  (haws-vsave '("ucsfollow" "osmode" "clayer"))
  (haws-setlayr "PSUP")
  (setvar "ucsfollow" 0)
  (setvar "osmode" 16)
  (if (not hvexag)
    (haws-gethvx)
  ) ;_ end of IF
  (setq
    pt1
     (getpoint "\nLeft ellipse quadrant: ")
    pt2
     (getpoint pt1 "\nRight ellipse quadrant: ")
    botpip
     (entsel "\nTop of lower pipe: ")
    vrad
     (* hvexag *haws-elvscl* (/ (distance pt1 pt2) 2))
    pt3
     (polar pt1 (/ pi -2) vrad)
    pt4
     (polar pt2 (/ pi -2) vrad)
    ptcen
     (mapcar
       '(lambda (left right) (/ (+ left right) 2.0))
       pt1
       pt2
     ) ;_ end of mapcar
  ) ;_ end of SETQ
  (setvar "osmode" 0)
  (entmake
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 (trans ptcen 1 0))
      '(41 . 0.0)
      (cons
        11
        (mapcar
          '(lambda (ucsydir) (* ucsydir vrad))
          (getvar "ucsydir")
        ) ;_ end of mapcar
      ) ;_ end of CONS
      (cons 40 (/ 1.0 (* hvexag *haws-elvscl*)))
      (cons 42 (* 2 pi))
    ) ;_ end of list
  ) ;_ end of entmake
  (vl-cmdf "._ellipse" pt1 pt2 vrad)
  (setq ellip1 (entlast))
  (vl-cmdf "._line" pt1 pt3 "")
  (setq line1 (entlast))
  (vl-cmdf "._line" pt2 pt4 "")
  (setq line2 (entlast))
  (vl-cmdf
    "._trim"
    line1
    line2
    ""
    (list ellip1 (polar pt1 (/ pi 2) (/ vrad 10)))
    ""
  ) ;_ end of command
  (if (/= "ELLIPSE" (cdr (assoc 0 (entget ellip1))))
    (setq ellip1 (entlast))
  ) ;_ end of IF
  (vl-cmdf
    "._extend"
    botpip
    ""
    (list line1 pt3)
    (list line2 pt4)
    ""
  ) ;_ end of command
  (setq pt3 (trans (cdr (assoc 11 (entget line1))) line1 1))
  (setq pt4 (trans (cdr (assoc 11 (entget line2))) line2 1))
  (vl-cmdf "._line" pt3 pt4 "")
  (setq line3 (entlast))
  (vl-cmdf
    "._hatch" "ansi31" "5" "0" ellip1 line1 line2 line3 ""
   ) ;_ end of COMMAND
 ;_ end of COMMAND
 ;_ end of command
  (vl-cmdf "._erase" ellip1 line3 "")
  (redraw)
  (haws-vrstor)
  (haws-core-restore)
  (princ)
) ;_ end of DEFUN

(defun c:haws-pc ()
(haws-core-init 275) (c:haws-procb))
(defun c:haws-procb
          (/ toppt botpt cbview width)
  (haws-core-init 276)
  (if (not hvexag)
    (haws-gethvx)
  ) ;_ end of IF
  (haws-vsave '("clayer"))
  (initget "Exist Prop")
  (setq
    exist
     (= (getkword "[Exist/Prop] <Prop>: ") "Exist")
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
  (if exist
    (haws-setlayr "PXCB")
    (haws-setlayr "PCB")
  ) ;_ end of IF
  (haws-drawcb botpt toppt cbview width nil)
  (haws-vrstor)
  (haws-core-restore)
  (princ)
) ;_ end of DEFUN

(defun haws-drawcb
              (botpt toppt cbview width brkpt / pt1 pt2 pt3 pt4 pt5 ptbl ptbr cb
              )
  (setq
    pt1 (polar botpt 0.0 width)
    pt2 (list (car pt1) (cadr toppt) 0.0)
    pt3 (polar botpt pi width)
    pt4 (list (car pt3) (cadr toppt) 0.0)
    pt5 (list
          (car botpt)
          (- (cadr toppt) (* hvexag *haws-elvscl* 0.583))
          0.0
        ) ;_ end of list
  ) ;_ end of SETQ
  (cond
    ((= cbview "L")
     (vl-cmdf
       "._pline"
       pt4
       "w"
       0
       ""
       (setq ptbl pt3)
       (setq ptbr botpt)
       pt5
       ""
     ) ;_ end of command
    )
    ((= cbview "C")
     (vl-cmdf
       "._pline"
       pt4
       "w"
       0
       ""
       (setq ptbl pt3)
       (setq ptbr pt1)
       pt2
       ""
     ) ;_ end of command
     (setq width (* width 2))
    )
    ((= cbview "R")
     (vl-cmdf
       "._pline"
       pt5
       "w"
       0
       ""
       (setq ptbl botpt)
       (setq ptbr pt1)
       pt2
       ""
     ) ;_ end of command
    )
  ) ;_ end of COND
  (setq cb (entlast))
  (cond
    (brkpt
     (setq
       pt1 (polar
             (list (/ (+ (car ptbr) (car ptbl)) 2) (cadr brkpt) 0.0)
             0.262
             width
           ) ;_ end of polar
       pt2 (polar pt1 (/ pi 2) 2)
       pt3 (polar pt1 3.403 (* width 2))
       pt4 (polar pt2 3.403 (* width 2))
     ) ;_ end of SETQ
     (haws-mkline pt1 pt3)
     (setq line1 (entlast))
     (haws-mkline pt2 pt4)
     (setq line2 (entlast))
     (vl-cmdf
       "._trim"
       line1
       line2
       ""
       (list cb pt4)
       (list (entlast) pt1)
       ""
     ) ;_ end of command
    )
  ) ;_ end of COND
) ;_ end of DEFUN

(defun c:haws-pm ()
(haws-core-init 277) (c:haws-promh))
(defun c:haws-promh
          (/ toppt botpt)
  (haws-core-init 278)
  (haws-vsave '("clayer"))
  (initget "Exist Prop")
  (setq
    exist
     (= (getkword "Exist/Prop: ") "Exist")
    botpt
     (getpoint "\nInvert: ")
    toppt
     (list (car botpt) (cadr (getpoint botpt "\nRim: ")) 0.0)
  ) ;_ end of SETQ
  (if exist
    (haws-setlayr "PXMH")
    (haws-setlayr "PMH")
  ) ;_ end of IF
  (haws-drawmh botpt toppt nil)
  (haws-vrstor)
  (haws-core-restore)
  (princ)
) ;_ end of DEFUN

;;Drawmh draws a cartoon manhole from botpt to toppt.
(defun haws-drawmh
              (botpt toppt brkpt / exist broken pt1 pt2 pt3 pt4 mh line1 line2
              )
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
  (vl-cmdf "._pline" pt1 "_w" 0 "" pt2 pt3 pt4 "_close")
  (setq mh (entlast))
  (cond
    (brkpt
     (setq
       pt1 (polar brkpt 0.262 3)
       pt2 (polar pt1 (/ pi 2) 2)
       pt3 (polar pt1 3.403 6)
       pt4 (polar pt2 3.403 6)
     ) ;_ end of SETQ
     (haws-mkline pt1 pt3)
     (setq line1 (entlast))
     (haws-mkline pt2 pt4)
     (setq line2 (entlast))
     (vl-cmdf
       "._trim"
       line1
       line2
       ""
       (list mh pt4)
       (list (entlast) pt1)
       ""
     ) ;_ end of command
    )
  ) ;_ end of COND
) ;_ end of DEFUN

(defun c:haws-pred
         ()
(haws-core-init 279)
  (vl-cmdf "_ai_editcustfile" (strcat (haws-getdnpath) ".pro"))
) ;_ end of defun

(defun c:haws-proe
         (/ laname invert diam pipsld walthk cenpt inrad outrad
         )
  (haws-core-init 280)
  (if (not hvexag)
    (haws-gethvx)
  ) ;_ end of IF
  (haws-vsave '("clayer" "filedia"))
  (initget "Exist Prop")
  (setq exist (= (getkword "Exist/Prop: ") "Exist"))
  (if exist
    (haws-setlayr "PXEL")
    (haws-setlayr "PEL")
  ) ;_ end of IF
  (setq
    invert
     (getpoint "\nInvert point: ")
    diam
     (/ (getreal "\nID (inches): ") 12)
  ) ;_ end of SETQ
  (cond
    ((setq pipsld (findfile "pipetabl.sld"))
     (setvar "filedia" 0)
     (vl-cmdf "._vslide" pipsld)
    )
  ) ;_ end of cond
  (setq
    walthk
     (/ (getreal "\nWall thickness (inches): ") 12)
    inrad
     (/ diam 2.0)
    outrad
     (+ (/ diam 2.0) walthk)
    cenpt
     (polar invert (/ pi 2.0) (* inrad hvexag *haws-elvscl*))
  ) ;_ end of SETQ
  (redraw)
  (entmake
    (list
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (cons 10 (trans cenpt 1 0))
      '(41 . 0.0)
      (cons
        11
        (mapcar
          '(lambda (ucsydir) (* ucsydir inrad hvexag *haws-elvscl*))
          (getvar "ucsydir")
        ) ;_ end of mapcar
      ) ;_ end of CONS
      (cons 40 (/ 1.0 (* hvexag *haws-elvscl*)))
      (cons 42 (* 2 pi))
    ) ;_ end of list
  ) ;_ end of entmake
  (if (/= 0 walthk)
    (entmake
      (list
        '(0 . "ELLIPSE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbEllipse")
        (cons 10 (trans cenpt 1 0))
        '(41 . 0.0)
        (cons
          11
          (mapcar
            '(lambda (ucsydir) (* ucsydir outrad hvexag *haws-elvscl*))
            (getvar "ucsydir")
          ) ;_ end of mapcar
        ) ;_ end of CONS
        (cons 40 (/ 1.0 (* hvexag *haws-elvscl*)))
        (cons 42 (* 2 pi))
      ) ;_ end of list
    ) ;_ end of entmake
  ) ;_ end of IF
  (haws-vrstor)
  (haws-core-restore)
  (princ)
) ;_ end of DEFUN

(defun c:haws-pldr
         (/ p1 p2 ds ts as dg ang left p3 p4 p5 str temp)
  (haws-core-init 281)
  (setq
    p1 (getpoint "\nStart point for leader:")
    p2 (getpoint p1 "\nEnd point for leader:")
    ds (haws-dwgscale)
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
         ((progn
            (initget "Tc GUtter Pavmt Grnd")
            (= (setq temp (getkword "Tc/GUtter/Pavmt/Grnd: ")) "Tc")
          ) ;_ end of progn
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
         ((progn
            (initget "Left Right Center")
            (= (setq temp (getkword "Left/Right/Center: ")) "Left")
          ) ;_ end of progn
          "LEFT"
         )
         ((= temp "Right") "RIGHT")
         ((= temp "Center") "@ C/L")
       ) ;_ end of COND
     ) ;_ end of STRCAT
  ) ;_ end of SETQ
  (if exist
    (haws-setlayr "PXLDR")
    (haws-setlayr "PLDR")
  ) ;_ end of IF
  (cond
    ((>= (atof (getvar "acadver")) 14)
     (vl-cmdf "._leader" p1 p2 "" str "")
    )
    (t
     (vl-cmdf "._pline" p1 "_w" 0 (/ as 3) p3 "_w" 0 0 p2 p4 "")
     (haws-mktext
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
  (haws-core-restore)
) ;_ end of DEFUN

(defun c:haws-newpro
           (/ expold proinputfile prolaywc ss1)
(haws-core-init 282)
  (setq expold (getvar "expert"))
  (setvar "expert" 0)
  (vl-cmdf "._undo" "_m" "._layer" "_off" "*" "" "")
  (haws-setlayr "PROSTRT")
  (setq
    prolaywc (strcat (car(haws-getlayr "PROPRE")) "*")
    ss1          (ssget "X" (list (cons 8 prolaywc)))
    proinputfile (open (strcat (haws-getdnpath) ".pro") "r")
  ) ;_ end of SETQ
  (if ss1 (vl-cmdf "._erase" ss1 ""))
  (vl-cmdf "._layer" "_u" prolaywc "")
  (c:haws-pro)
  (vl-cmdf "._layer" "_on" "*" "")
  (setvar "expert" expold)
  (prompt "\nType Undo Back to undo entire profile.")
  (princ)
) ;_ end of DEFUN

;;File version converter for PRO
;;Adds or removes fields between the slope and label column.
(defun c:haws-profc
          (/ colwid f1 flist fm fname i m n rdlin temp tempfile)
  (haws-core-init 283)
  (setq
    *f1* (open
         (getfiled "List of files to convert" (haws-getdnpath) "lst" 0)
         "r"
       ) ;_ end of open
  ) ;_ end of setq
  (while (setq rdlin (read-line *f1*))
    (setq flist (cons (findfile rdlin) flist))
  ) ;_ end of while
  (textscr)
  (setq
    *f1* (close *f1*)
    colwid
     (getint "New column width: ")
    m (getint "Old number of fields between slope and label: ")
    n (getint "New number of fields between slope and label: ")
    i -1
  ) ;_ end of SETQ
  (if (findfile
        (setq tempfile (strcat "pro" (substr (getvar "loginname") 1 5) ".tmp"))
      ) ;_ end of findfile
    (vl-cmdf "_del" tempfile)
  ) ;_ end of IF
  (while (setq fname (nth (setq i (1+ i)) flist))
    (setq *f1* (open fname "r"))
    (setq *f2* (open tempfile "w"))
    (while (setq rdlin (read-line *f1*))
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
              (haws-mkfld (haws-rdfld 1 rdlin fm 1) colwid)
              (haws-mkfld (haws-rdfld 2 rdlin fm 1) colwid)
              (haws-mkfld (haws-rdfld 3 rdlin fm 1) colwid)
              (haws-mkfld (haws-rdfld 4 rdlin fm 1) colwid)
              (haws-mkfld (haws-rdfld 5 rdlin fm 1) colwid)
              (if (> n 0)
                (haws-mkfld
                  (if (setq temp (haws-atofx (haws-rdfld 6 rdlin fm 3) "*RIM*" 0))
                    (rtos temp 2 2)
                    ""
                  ) ;_ end of IF
                  colwid
                ) ;_ end of mkfld
                ""
              ) ;_ end of IF
              (if (> n 1)
                (haws-mkfld
                  (if (setq temp (haws-atofx (haws-rdfld 6 rdlin fm 3) "*\"*,*IN*" 1))
                    (rtos temp 2 2)
                    ""
                  ) ;_ end of IF
                  colwid
                ) ;_ end of mkfld
                ""
              ) ;_ end of IF
              (haws-rdfld (+ 6 m) rdlin fm 3)
            ) ;_ end of STRCAT
         ) ;_ end of SETQ
        )
        ((wcmatch rdlin "REFPNT*,PRECIS*,HVEXAG*,PROTYP*")
         (setq
           rdlin
            (strcat
              (mkfld (haws-rdfld 1 rdlin fm 1) colwid)
              (mkfld (haws-rdfld 2 rdlin fm 1) colwid)
              (mkfld (haws-rdfld 3 rdlin fm 1) colwid)
              (mkfld (haws-rdfld 4 rdlin fm 1) colwid)
              (mkfld (haws-rdfld 5 rdlin fm 1) colwid)
            ) ;_ end of STRCAT
         ) ;_ end of SETQ
        )
      ) ;_ end of COND
      (while (= " " (substr rdlin (max (strlen rdlin) 1)))
        (setq rdlin (substr rdlin 1 (1- (strlen rdlin))))
      ) ;_ end of WHILE
      (write-line rdlin *f2*)
    ) ;_ end of WHILE
    (setq
      *f1* (close *f1*)
      *f2* (close *f2*)
    ) ;_ end of SETQ
    (vl-cmdf "_sh" (strcat "copy " tempfile " " fname))
    (vl-cmdf "_sh" (strcat "del " tempfile))
    (princ)
  ) ;_ end of WHILE
) ;_ end of DEFUN
;;;;-----------------------------PRO------------------------------------------
(defun c:haws-pro;;Localize the variables
             (/ i pltpt1a ts lingap lup fm sta el1dsc elev1 offset slope num2 ptopt lbltxt acadx acady1 acady2
              lblmhrim mhrim lblcbtc cbtc pexist pltpt1 pltsta pltelv invmir invup statxt offtxt el1txt toptx botpt
              lblpt1 lblpt2 lblpt3 txlen txpt1 txpt2 txpt3 txpt4 txpt5 txpt6 txpt7 txpt8 toppt txpt11 txpt12 txpt13
              txpt14
             )
(haws-core-init 284)
  (prompt
    "\nProfile drafter version 5.06.  Copyright 2000, Thomas Gail Haws."
  ) ;_ end of prompt
  (defun c:haws-tgh2-pro
               ()
    (while (progn
             (initget "Ref Type Vertical Precision New File Plot")
             (setq
               proopt
                (getkword
                  "\nRef. point/prof. Type/Vert. exag./Precision/New prof./File/Plot: "
                ) ;_ end of getkword
             ) ;_ end of setq
             (cond
               ((= proopt
                   "Ref"
                   ((= proopt "Type") (gettyp))
                   ((= proopt "Vertical")
                     (haws-gethvx)
                     (if *f2*
                       (write-line (strcat "HVEXAG    " hvexag) *f2*)
                     ) ;_ end of IF
                   )
                   ((= proopt "Precision") (getprec))
                   ((= proopt "New")
                     (setq pltpt1 nil)
                     (prompt "\nEnded previous profile, starting new profile.")
                     (if *f2*
                       (write-line "NEWPRF" *f2*)
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
  (defun haws-proset
                (/ opt)
    (while (setq
             opt
              (progn
                (initget "Ref Type Vertical Precision New File")
                (getkword
                  "\nRef. point/prof. Type/Vert. exag./Precision/New prof./File: "
                ) ;_ end of getkword
              ) ;_ end of PROGN
           ) ;_ end of SETQ
      (cond
        ((= opt "Ref") (getref))
        ((= opt "Type") (gettyp))
        ((= opt "Vertical")
         (haws-gethvx)
         (if *f2*
           (write-line (strcat "HVEXAG    " hvexag) *f2*)
         ) ;_ end of IF
        )
        ((= opt "Precision") (getprec))
        ((= opt "New")
         (setq pltpt1 nil)
         (prompt "\nEnded previous profile, starting new profile.")
         (if *f2*
           (write-line "NEWPRF" *f2*)
         ) ;_ end of IF
        )
        ((= opt "File") (getfop))
      ) ;_ end of COND
    ) ;_ end of WHILE
  ) ;_ end of DEFUN
  ;;Get the profile reference point to tie profile to ACAD coordinates
  (defun getref
           (/ r)
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
    (if *f2*
      (write-line
        (strcat
          (mkfld "REFPNT" colwid)
          (mkfld (rtos refptx 2 2) colwid)
          (mkfld (rtos refpty 2 2) colwid)
          (mkfld (rtos refsta 2 2) colwid)
          (mkfld (rtos refelv 2 2) colwid)
        ) ;_ end of STRCAT
        *f2*
      ) ;_ end of WRITE-LINE
    ) ;_ end of IF
  ) ;_ end of DEFUN
  ;;Setup layers for the profile
  (defun setlay
           (/ llinc llinlt ltxc ltxlt)
    (cond
      ((and (= proht -20000) (wcmatch protyp "X*"))
       (setq laylin (haws-getlayr "PROXDIM"))
      )
      ((= proht -20000) (setq laylin (haws-getlayr "PRODIM")))
      ((and (= proht -10000) (wcmatch protyp "X*"))
       (setq laylin (haws-getlayr "PROXLINE"))
      )
      ((= proht -10000) (setq laylin (haws-getlayr "PROLINE")))
      ((and (= proht 1000) (wcmatch protyp "X*"))
       (haws-setlayr "PROPNT")
       (setq laylin (haws-getlayr "PROXEL"))
      )
      ((= proht 1000)
       (haws-setlayr "PROPNT")
       (setq laylin (haws-getlayr "PROEL"))
      )
      ((and (>= proht 0) (wcmatch protyp "X*,MESA"))
       (setq laylin (haws-getlayr "PROXSTRT"))
      )
      ((>= proht 0) (setq laylin (haws-getlayr "PROSTRT")))
      ((and (< proht 0) (wcmatch protyp "X*"))
       (setq
         laylin
          (list
            (strcat
              (car (haws-getlayr "PROPRE"))
              (car (setq laylin (haws-getlayr "PROEXMID")))
              (substr protyp 2)
            ) ;_ end of strcat
            (cadr laylin)
            (caddr laylin)
          ) ;_ end of LIST
       ) ;_ end of SETQ
      )
      ((< proht 0)
       (setq
         laylin
          (cond
            ((haws-getlayr (strcat "PRO" protyp)))
            ((list
               (strcat (car (setq laylin (haws-getlayr "PROPRE"))) protyp)
               (cadr laylin)
               (caddr laylin)
             ) ;_ end of list
            )
          ) ;_ end of COND
       ) ;_ end of SETQ
      )
    ) ;_ end of COND
    (setq
      laytx
       (haws-getlayr "PROTXSUF")
      layxtx
       (haws-getlayr "PROXTXSUF")
    ) ;_ end of SETQ
    (if (wcmatch protyp "X*")
      (setq
        protyp
         (substr protyp 2)
        laytx
         layxtx
        pexist
         t
      ) ;_ end of SETQ
      (setq pexist nil)
    ) ;_ end of IF
    (setq laytx (subst (strcat (car laylin) (car laytx)) (car laytx) laytx))
    (if (not (tblsearch "LAYER" (car laylin)))
      (haws-setlayr laylin)
    ) ;_ end of IF
    (if (not (tblsearch "LAYER" (car laytx)))
      (haws-setlayr laytx)
    ) ;_ end of IF
    (setq
      laylin
       (car laylin)
      laytx
       (car laytx)
      laypre
       (car (haws-getlayr "PROPRE"))
      laytxs
       (car (haws-getlayr "PROTXSUF"))
    ) ;_ end of SETQ
  ) ;_ end of DEFUN
  ;;Get the type of profile and set up layers.
  (defun gettyp
           (/ pipsld)
    (setq
      walthk
       0
      protyp
       ""
    ) ;_ end of SETQ
    (initget 1 "Mesa Gutter PAv PIpe Line POints")
    (setq
      proht
       (getkword
         "\nMesa/Gutter/PAvement/PIpe/Line only/POints only:"
       ) ;_ end of getkword
    ) ;_ end of setq
    (setq
      proht
       (cond
         ((= proht "Mesa")
          (setq protyp "MESA")
          (* (/ (getreal "\nCurb height (in.): ") 12) hvexag *haws-elvscl*)
         )
         ((= proht "Gutter")
          (* (/ (getreal "\nCurb height (in.): ") 12) hvexag *haws-elvscl*)
         )
         ((= proht "PAv") 0)
         ((= proht "PIpe")
          (setq
            protyp
             (strcase
               (progn
                 (initget 1 "Wat Sew SD Irr Gas")
                 (getkword "\nWat/Sew/SD/Irr/Gas: ")
               ) ;_ end of progn
             ) ;_ end of strcase
          ) ;_ end of setq
          (setq proht (* (/ (getreal "\nPipe ID (in): ") -12) hvexag *haws-elvscl*))
          (cond
            ((setq pipsld (findfile "pipetabl.sld"))
             (setvar "filedia" 0)
             (vl-cmdf "._vslide" pipsld)
            )
          ) ;_ end of cond
          (setq
            walthk
             (* (/ (getreal "\nWall thickness (in): ") -12)
                hvexag
                *haws-elvscl*
             ) ;_ end of *
          ) ;_ end of setq
          (redraw)
          proht
         )
         ((= proht "Line") -10000)
         ((= proht "POints") 1000)
       ) ;_ end of COND
    ) ;_ end of SETQ
    (initget 1 "Exist Prop")
    (if (= (getkword "Exist/Prop: ") "Exist")
      (setq protyp (strcat "X" protyp))
    ) ;_ end of IF
    (if *f2*
      (write-line
        (strcat
          "PROTYP    "
          (mkfld
            (rtos
              (if (< -10000 proht 1000)
                (* (/ proht hvexag *haws-elvscl*) 12.0)
                proht
              ) ;_ end of IF
              2
              6
            ) ;_ end of RTOS
          ) ;_ end of mkfld
          (mkfld (rtos (* (/ walthk hvexag *haws-elvscl*) 12) 2 6))
          protyp
        ) ;_ end of STRCAT
        *f2*
      ) ;_ end of WRITE-LINE
    ) ;_ end of IF
    (setlay)
  ) ;_ end of DEFUN
  ;;Get precisions for numbers.
  (defun getprec
            ()
    (while (progn
             (initget "STa Offset Elev SLope")
             (prompt
               "\nChange rounding precision for STa/Offset/Elev/SLope: "
             ) ;_ end of prompt
             (setq temp (getkword))
           ) ;_ end of PROGN
      (prompt
        (strcat
          "\nNumber of digits to right of decimal place for "
          temp
          ": "
        ) ;_ end of strcat
      ) ;_ end of prompt
      (set (read (strcat (substr temp 1 2) "prec")) (getint))
    ) ;_ end of WHILE
    (setvar "luprec" stprec)
  ) ;_ end of DEFUN
  ;;Increment elevations in a file
  (defun incfil
           (/ incsew incsd incgen)
    (setq
      incsew
       (getreal "\nIncrement for underground sewer elevations: ")
      incsd
       (getreal
         "\nIncrement for underground storm drain elevations: "
       ) ;_ end of getreal
      incgen
       (getreal "\nIncrement for all other elevations: ")
    ) ;_ end of SETQ
    (setq
      *f1* (cond
           (proinputfile
            (open (setq fname (strcat proinputfile ".pro")) "r")
           )
           ((= (getvar "filedia") 1)
            (open
              (setq fname (getfiled "File to increment" (haws-getdnpath) "pro" 0))
              "r"
            ) ;_ end of open
           )
           (t (car (getfil "File to increment" (haws-getdnpath) "r" "pro")))
         ) ;_ end of COND
    ) ;_ end of SETQ
    (while (if *f1*
             (haws-rdfile)
           ) ;_ end of IF
      (if (and sta (/= sta "Setup"))
        ;;if it is a profile point
        (progn
          (pltpro)
          ;;plot the point
          (if *f2*
            ;;If requested, write all
            (write-line
              (strcat
                ;;the point information
                "PRFPNT    "
                ;;to a file (*f2*).
                (mkfld (rtos sta 2 3))
                (mkfld
                  (if offset
                    (rtos offset 2 3)
                    ""
                  ) ;_ end of IF
                ) ;_ end of mkfld
                (mkfld (rtos elev1 2 3))
                (mkfld
                  (if slope
                    (rtos slope 2 4)
                    ""
                  ) ;_ end of IF
                ) ;_ end of mkfld
                (mkfld
                  (if num2
                    (rtos num2 2 3)
                    ""
                  ) ;_ end of IF
                ) ;_ end of mkfld
                (cond
                  (lbltxt)
                  ("")
                ) ;_ end of COND
              ) ;_ end of STRCAT
              *f2*
            ) ;_ end of WRITE-LINE
          ) ;_ end of IF
          (setq sta nil)
          ;;Reset profile point to nil.
        ) ;_ end of PROGN
        (if (not *f1*)
          (haws-proset)
        ) ;_ end of IF
        ;;If not a point, do setup.
      ) ;_ end of IF
      (princ "\rReading line ")
      (princ (setq i (1+ i)))
    ) ;_ end of WHILE
  ) ;_ end of DEFUN
  ;;Get file options
  (defun getfop
           (/ rdwrt fprmpt fbit datfil)
    (initget "Read Save Increment")
    (setq
      rdwrt
       (getkword
         "\nRead from file/Save to file/Increment elevations in a file: "
       ) ;_ end of getkword
      *f1* nil
      *f2* nil
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
           rdwrt
            "r"
           fprmpt
            "File to read"
           fbit
            0
           prover4
            nil
         ) ;_ end of SETQ
         (setq
           rdwrt
            (if (= (getkword "\Create new file/Append existing file: ")
                   "Create"
                ) ;_ end of =
              "w"
              "a"
            ) ;_ end of IF
           fprmpt
            "File to save input to"
           fbit
            1
         ) ;_ end of SETQ
       ) ;_ end of IF
       (setq
         datfil
          (cond
            ((= (getvar "filedia") 1)
             (open
               (setq fname (getfiled fprmpt (haws-getdnpath) "pro" fbit))
               rdwrt
             ) ;_ end of open
            )
            (t (car (getfil fprmpt (haws-getdnpath) rdwrt "pro")))
          ) ;_ end of COND
       ) ;_ end of SETQ
       (if (= rdwrt "r")
         (setq *f1* datfil)
         (progn (setq *f2* datfil) (write-line "COLWID 10" *f2*))
       ) ;_ end of IF
      )
    ) ;_ end of COND
  ) ;_ end of DEFUN
  ;;Prompt for station to plot
  (defun haws-getpnt
                ()
    (setq
      sta
       t
      elev1
       nil
      lbltxt
       ""
    ) ;_ end of SETQ
    (while (and sta (not elev1))
      (while (progn
               (initget "Setup Label")
               (= (setq sta (getreal "\n<Station>/Label/Setup: ")) "Label")
             ) ;_ end of progn
        (setq lbltxt (getstring 1 "\nEnter label for this station: "))
      ) ;_ end of WHILE
      (if (and sta (/= sta "Setup"))
        (progn
          (while (progn
                   (initget "Offset")
                   (= (setq elev1 (getreal "<Elevation>/Offset: ")) "Offset")
                 ) ;_ end of progn
            (setq offset (getreal "\nOffset: "))
          ) ;_ end of WHILE
          (if (and
                elev1
                (> (abs (- elev1 refelv)) 100)
                (= (strcase
                     (getstring "\nElev. diff. is over 100 feet.  Plot? Y/N: ")
                   ) ;_ end of strcase
                   "N"
                ) ;_ end of =
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
  (defun haws-rdfile
                (/ linerr rdlin title)
    (if (not fm)
      (setq fm ",")
    ) ;_ end of IF
    (setq rdlin (read-line *f1*))
    (if rdlin
      (progn
        (setq title (substr rdlin 1 6))
        (cond
          ((= (substr rdlin 1 2) "0,")
           (setq
             sta
              (haws-rdfld 2 rdlin fm 0)
             offset
              nil
             elev1
              (haws-rdfld 4 rdlin fm 0)
             slope
              nil
             lbltxt
              ""
           ) ;_ end of SETQ
          )
          ((= title "PRFPNT")
           (setq
             sta
              (haws-rdfld 2 rdlin fm 0)
             offset
              (haws-rdfld 3 rdlin fm 0)
             elev1
              (haws-rdfld 4 rdlin fm 0)
             slope
              (haws-rdfld 5 rdlin fm 0)
             num2
              (haws-rdfld 6 rdlin fm 0)
             lbltxt
              (haws-rdfld 7 rdlin fm 3)
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
              (haws-rdfld 2 rdlin fm 0)
             refpty
              (haws-rdfld 3 rdlin fm 0)
             refsta
              (haws-rdfld 4 rdlin fm 0)
             refelv
              (haws-rdfld 5 rdlin fm 0)
             stascl
              (haws-rdfld 6 rdlin fm 0)
             stascl
              (if stascl
                stascl
                1
              ) ;_ end of if
             *haws-elvscl*
              (haws-rdfld 7 rdlin fm 0)
             *haws-elvscl*
              (if *haws-elvscl*
                *haws-elvscl*
                1
              ) ;_ end of if
           ) ;_ end of SETQ
           (if (not (and refptx refpty refsta refelv))
             (setq linerr t)
           ) ;_ end of IF
          )
          ((= title "HVEXAG")
           (setq hvexag (haws-rdfld 2 rdlin fm 0))
           (if (not hvexag)
             (setq linerr t)
           ) ;_ end of IF
          )
          ((= title "PROTYP")
           (setq
             proht
              (haws-rdfld 2 rdlin fm 0)
             proht
              (if (or (= proht 1000) (<= proht -10000))
                proht
                (/ (* proht hvexag *haws-elvscl*) 12)
              ) ;_ end of IF
             walthk
              (haws-rdfld 3 rdlin fm 0)
             walthk
              (cond
                (walthk (/ (* walthk hvexag *haws-elvscl*) 12))
                (0)
              ) ;_ end of COND
             protyp
              (strcase (haws-rdfld 4 rdlin fm 1))
             slptyp
              (haws-rdfld 5 rdlin fm 1)
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
              (fix (haws-rdfld 2 rdlin fm 0))
             ofprec
              (fix (haws-rdfld 3 rdlin fm 0))
             elprec
              (fix (haws-rdfld 4 rdlin fm 0))
             slprecin
              (fix (haws-rdfld 5 rdlin fm 0))
             slprec
              slprecin
           ) ;_ end of SETQ
          )
          ((= title "NEWPRF")
           (setq
             pltpt1
              nil
             toppt
              nil
           ) ;_ end of setq
          )
        ) ;_ end of COND
        (if linerr
          (princ (strcat ".  Input error found:\n" rdlin "\n"))
        ) ;_ end of IF
        t
      ) ;_ end of PROGN
      nil
    ) ;_ end of IF
  ) ;_ end of DEFUN
  ;;Plot profile given sta, elev1, and offset.
  (defun haws-plthardpro
                    ()
    (setq
      ;;Extract plotting instructions then actual elevation from input
      invmir
       (cond
         ((and
            elev1
            (/= (distof (rtos elev1 2 2)) (distof (rtos elev1 2 3)))
          ) ;_ end of and
          (> 0.35 (- (* 100 elev1) (fix (* 100 elev1))) 0.15)
         )
         ((and pltpt1a (wcmatch (strcase lbltxt) "* SHEET *,")))
       ) ;_ end of COND
      invup
       (cond
         ((and
            elev1
            (/= (distof (rtos elev1 2 2)) (distof (rtos elev1 2 3)))
          ) ;_ end of and
          (> 0.25 (- (* 100 elev1) (fix (* 100 elev1))) 0.05)
         )
       ) ;_ end of COND
      elev1
       (if (and
             (= proht 1000)
             (setq temp (haws-distofx (strcase lbltxt) "*TOP*=*,*BOT*=*,*INV*=*" 0))
           ) ;_ end of and
         temp
         ;;elevation from ellipse label for ellipses
         elev1
         ;;elevation from elevation field for everything else
       ) ;_ end of IF
      pltelv
       (not (minusp elev1))
      elev1
       (/ (fix (* (+ (abs elev1) 0.0001) 100.0)) 100.0)
      ;;Extract plotting instructions and actual station from input
      pltsta
       (and
         (/= protyp "MESA")
         (/= proht 1000)
         (= (distof (rtos sta 2 2)) (distof (rtos sta 2 3)))
       ) ;_ end of and
      sta
       (atof (rtos sta 2 2))
      ;;Translate station and elevation to a world coordinate point
      acadx
       (+ refptx (* stascl (- sta refsta)))
      acady1
       (+ refpty (* hvexag *haws-elvscl* (- elev1 refelv)))
      ;;Extract mh rim, cb second elevation, or other y2 from input
      acady2
       (if num2
         (+ refpty (* hvexag *haws-elvscl* (- num2 refelv)))
       ) ;_ end of IF
      lblnum2
       (cond
         ((wcmatch (strcase lbltxt) "*RIM=#*")
          (haws-atofx lbltxt "*RIM=*" 0)
         )                              ;Manhole rim
         ((wcmatch (strcase lbltxt) "*TC=*")
          (haws-atofx lbltxt "*TC=*" 0)
         )                              ;Inlet TC
         ((wcmatch (strcase lbltxt) "*GRATE=*")
          (+ 0.67 (haws-atofx lbltxt "*GRATE=*" 0))
         )                              ;Grate TC
         ((= proht 1000)
          (haws-atofx (strcase lbltxt) "*\"*,* IN*" 1) ;Ellipse diameter
         )
       ) ;_ end of COND
      lblacady2
       (if lblnum2
         (+ refpty (* hvexag *haws-elvscl* (- lblnum2 refelv)))
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
       (if (and
             (wcmatch (strcase lbltxt) "*CB*,*INLET*,*BASIN*")
             (or acady2 lblacady2)
           ) ;_ end of and
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
            (if (= proht -10000)
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
  (defun haws-pltsoftpro
                    (/ cnffreept cnftestpt i j k lblcnf ldrcnf lngstr ldrcnfpt)
    (setq
      ;;Make label strings
      ;;Wrap label string as requested by user with semi-colons.
      lbllst
       (haws-wrap lbltxt 3 ";")
      ;;Find longest string to be plotted.
      lngstr
       (car lbllst)
      lngstr
       (foreach
          stringi
                 lbllst
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
          (if (/= "" protyp)
            (strcat protyp "=")
            "G="
          ) ;_ end of IF
         )
         ((= 0 proht)
          (if (/= "" protyp)
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
          (strcat "STA " (haws-rtosta sta stprec))
         ;;Insert offset into station string if given
         offtxt
          (if offset
            (strcat
;;;O							(if	pltsta
;;;O								(strcat statxt ", ")
;;;O								""
;;;O							) ;_ end of IF
              (rtos (abs offset) 2 ofprec)
              (if (minusp offset)
                "' LT"
                "' RT"
              ) ;_ end of IF
            ) ;_ end of STRCAT
            ""
          ) ;_ end of IF
         ;;Set crown (top of curb) elevation, translate to text, and get length.
         toptx
          (+ elev1 (/ (abs proht) hvexag *haws-elvscl*))
         toptx
          (cond ((< 0 proht 1000) (strcat "TC=" (rtos toptx 2 elprec))))
         lngstr
          (if (and pltsta (< (strlen lngstr) (strlen statxt)))
            statxt
            lngstr
          ) ;_ end of IF
         lngstr
          (if (< (strlen lngstr) (strlen offtxt))
            offtxt
            lngstr
          ) ;_ end of IF
         lngstr
          (if (< (strlen lngstr) (strlen el1txt))
            el1txt
            lngstr
          ) ;_ end of IF
         txlen
          (+ (haws-txlen lngstr ts) 0)
         ;;Set points for text and label lines
         lblpt1
          (polar
            (cond
              (pltpt2)
              (toppt)
            ) ;_ end of COND
            (/ pi 2)
            (if mhrim
              (* 0.05 hvexag *haws-elvscl*)
              0
            ) ;_ end of IF
          ) ;_ end of POLAR
         lblpt2
          (polar lblpt1 (/ pi 2) (* 2.0 ts))
         cnftestpt
          lblpt2
         cnffreept
          '(0.0 0.0 0.0)
         ldrcnfpt
          '(0.0 0.0 0.0)
         i 0
         j 0
         k 0
       ) ;_ end of SETQ
       ;; Find the nearest of up to twenty conflict-free label positions or one conflict
       ;; free straight up
       ;; position.
       (while (and (< i 100) (< j 20) (< k 1))
         (setq
           lblcnf
            (ssget
              "C"
              (polar
                cnftestpt
                pi
                (* ts
                   (+ (* lingap 0.9)
                      (* (1+ lingap)
                         (+ (if pltsta
                              1
                              0
                            ) ;_ end of IF
                            (if (= offtxt "")
                              0
                              1
                            ) ;_ end of if
                            (if (= (car lbllst) "")
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
                     (+ lingap
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
              (list
                (polar lblpt1 (angle lblpt1 cnftestpt) (* ts 1))
                cnftestpt
              ) ;_ end of list
              (list (cons 8 (strcat laypre "*" laytxs)))
            ) ;_ end of SSGET
         ) ;_ end of SETQ
         ;;Save nearest point that has no label conflict.
         (if (and
               (not lblcnf)
               (< (distance lblpt2 cnftestpt) (distance lblpt2 ldrcnfpt))
             ) ;_ end of and
           (setq ldrcnfpt cnftestpt)
         ) ;_ end of IF
         ;;Save nearest point that has no conflict at all.
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
                 (and
                   ldrcnfpt
                   (> (- (distance lblpt2 cnffreept) (distance lblpt2 ldrcnfpt))
                      (* ts 4.0)
                   ) ;_ end of >
                 ) ;_ end of and
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
          (if (and (or mhrim cbtc) acady2 lblacady2 (/= acady2 lblacady2))
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
       (while (and (< i 15) (< j 1))
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
            (if (and (= proht 1000) (> (strlen lbltxt) 0))
              lngstr
              el1txt
            ) ;_ end of IF
           txlen
            (+ (haws-txlen lngstr ts) 0)
           txpt7
            (polar
              txpt6
              (if invmir
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
      ((and (< -10000 proht 0) (or pltsta offset (/= lbltxt "")))
       (haws-pltlbl1)
      )
      ((and
         (<= 0 proht 999.9)
         (or pltsta offset pltelv (/= lbltxt ""))
       ) ;_ end of and
       (haws-pltlbl1)
      )
      ((= proht 1000) (haws-pltpnt))
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
    (cond ((and (< -10000 proht 0) pltelv) (haws-pltlbl2)))
    (setq offset nil)
  ) ;_ end of DEFUN
  ;;Plot a vertical upward profile point label.
  (defun haws-pltlbl1
                 (/ i)
    (haws-setlayr (list laytx "" ""))
    (cond
      ((= protyp "MESA")
       ;;Slanted label
       (haws-mkline toppt txpt12)
       (haws-mktext nil txpt13 ts (* pi 0.25) lbltxt)
       (haws-mktext "TL" txpt14 ts (* pi 0.25) el1txt)
      )
      (t
       ;;Vertical label
       (haws-mkline botpt lblpt1)
       (haws-mkline lblpt1 lblpt2)
       (haws-mkline lblpt2 lblpt3)
       (cond
         ((/= "" offtxt)
          (haws-mktext nil txpt1 ts (/ pi 2) offtxt)
          (setq txpt1 (polar txpt1 0 (* ts -1 (+ 1 lingap))))
         )
       ) ;_ end of cond
       (cond
         (pltsta (haws-mktext nil txpt1 ts (/ pi 2) statxt))
         (t (setq txpt1 (polar txpt1 0 (* ts (+ 1 lingap)))))
       ) ;_ end of COND
       (setq i -1)
       (while (setq temp (nth (setq i (1+ i)) (reverse lbllst)))
         (haws-mktext
           nil
           (setq txpt1 (polar txpt1 0 (* ts -1 (+ 1 lingap))))
           ts
           (/ pi 2)
           temp
         ) ;_ end of HAWS-MKTEXT
       ) ;_ end of WHILE
       ;;Plot elevation text for curbs.
       (cond
         ((and (<= 0 proht) pltelv)
          (haws-mktext
            "tl"
            txpt4
            ts
            (/ pi 2)
            (cond
              (toptx)
              (el1txt)
            ) ;_ end of COND
          ) ;_ end of mktext
          (if toptx
            (haws-mktext "tl" txpt5 ts (/ pi 2) el1txt)
          ) ;_ end of IF
         )
       ) ;_ end of COND
       ;;Plot diamond
       (cond
         ((and
            (>= proht 0)
            (wcmatch (strcase lbltxt) "*GC*,*G`.C`.*,*DE CHANGE*")
          ) ;_ end of and
          (vl-cmdf "._pline" lblpt3 diapt2 diapt1 diapt3 "_c")
         )
         ((and
            (>= proht 0)
            (wcmatch (strcase lbltxt) "*GB*,*G`.C`.*,*DE BREAK*")
          ) ;_ end of and
          (vl-cmdf "._solid" diapt1 diapt2 diapt3 lblpt3 "")
         )
       ) ;_ end of COND
       ;;Plot manhole
       (cond
         ((< proht 0)
          (if (not *f1*)
            (haws-mktext "tl" txpt4 ts (/ pi 2) "RIM=")
          ) ;_ end of IF
          (cond
            (mhrim
             (if pexist
               (haws-setlayr "PROXMH")
               (haws-setlayr "PROMH")
             ) ;_ end of IF
             (haws-drawmh botpt mhrim brkpt)
            )
            (cbtc
             (if pexist
               (haws-setlayr "PPROXCB")
               (haws-setlayr "PROCB")
             ) ;_ end of IF
             (haws-drawcb
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
  (defun haws-pltlbl2
                 (/ i)
    (haws-setlayr (list laytx "" ""))
    (haws-mkline pltpt1 txpt6)
    (haws-mkline txpt6 txpt7)
    (haws-mktext
      (if invmir
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
  (defun haws-pltpnt
                (/ i xcenter xinvert xdiam xwalthk xcrown xbotpt xtoppt xinrad xoutrad txpt1 txpt2 y
                )
    (cond
      ((and (= lbltxt "") (not num2))
       (prompt
         "Point circles are on DEFPOINTS layer. Will not plot."
       ) ;_ end of prompt
       (haws-setlayr (list "defpoints" "" ""))
       (entmake
         (list
           (cons 0 "CIRCLE")
           (append '(10) (trans pltpt1 1 0))
           (cons 40 ts)
         ) ;_ end of list
       ) ;_ end of entmake
      )
      (t
       (foreach
          x
           (quote
             ((xbotpt "*BOT*=*") (xtoppt "*TOP*=*") (xinvert "*INV*=*"))
           ) ;_ end of quote
         (if (setq y (haws-distofx (strcase lbltxt) (cadr x) 0))
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
          (if slope
            (/ slope 12)
            (if walthk
              walthk
              0
            ) ;_ end of IF
          ) ;_ end of IF
         xinvert
          (list
            (car pltpt1)
            (+ refpty
               (* hvexag
                  *haws-elvscl*
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
          (polar xinvert (/ pi 2.0) (* xdiam hvexag *haws-elvscl*))
         xbotpt
          (polar
            xinvert
            (/ pi -2.0)
            (* xwalthk
               hvexag
               *haws-elvscl*
            ) ;_ end of *
          ) ;_ end of POLAR
         xtoppt
          (polar xcrown (/ pi 2.0) (* xwalthk hvexag *haws-elvscl*))
         xinrad
          (/ xdiam 2.0)
         xoutrad
          (+ (/ xdiam 2.0) xwalthk)
         xcenter
          (polar xinvert (/ pi 2.0) (* xinrad hvexag *haws-elvscl*))
       ) ;_ end of SETQ
       (haws-setlayr (list laylin "" ""))
       (entmake
         (list
           '(0 . "ELLIPSE")
           '(100 . "AcDbEntity")
           '(100 . "AcDbEllipse")
           (cons 10 (trans xcenter 1 0))
           '(41 . 0.0)
           (cons
             11
             (mapcar
               '(lambda (ucsydir) (* ucsydir xinrad hvexag *haws-elvscl*))
               (getvar "ucsydir")
             ) ;_ end of mapcar
           ) ;_ end of CONS
           (cons 40 (/ 1.0 (* hvexag *haws-elvscl*)))
           (cons 42 (* 2 pi))
         ) ;_ end of list
       ) ;_ end of entmake
       (if (/= 0 xwalthk)
         (entmake
           (list
             '(0 . "ELLIPSE")
             '(100 . "AcDbEntity")
             '(100 . "AcDbEllipse")
             (cons 10 (trans xcenter 1 0))
             '(41 . 0.0)
             (cons
               11
               (mapcar
                 '(lambda (ucsydir) (* ucsydir xoutrad hvexag *haws-elvscl*))
                 (getvar "ucsydir")
               ) ;_ end of mapcar
             ) ;_ end of CONS
             (cons 40 (/ 1.0 (* hvexag *haws-elvscl*)))
             (cons 42 (* 2 pi))
           ) ;_ end of list
         ) ;_ end of entmake
       ) ;_ end of IF
       (cond
         ((/= lbltxt "")
          (haws-setlayr (list laytx "" ""))
          (haws-mkline pltpt1 txpt6)
          (haws-mkline txpt6 txpt7)
          (setq i -1)
          (while (setq temp (nth (setq i (1+ i)) lbllst))
            (haws-mktext
              (if invmir
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
  (defun haws-pltlin
                (pt1 pt2 offset)
    (haws-mkline
      (polar pt1 (/ pi 2) offset)
      (polar pt2 (/ pi 2) offset)
    ) ;_ end of HAWS-MKLINE
  ) ;_ end of defun
  ;;Connect two profile points with lines
  (defun haws-connecthard
                     ()
    (haws-setlayr (list laylin "" ""))
    (cond
      ((/= -20000 proht) (haws-mkline pltpt1a pltpt1))
      ((= proht -20000)
       (vl-cmdf
         "._dim1"
         "_al"
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
      ((and (< 0 proht 1000) toppta) (haws-mkline toppta toppt))
      ((and (= 0 proht) toppta (not (equal toppta pltpt1a)))
       (haws-mkline toppta toppt)
      )
      ((< -10000 proht 0)
       (haws-pltlin pltpt1a pltpt1 (abs proht))
      )
    ) ;_ end of COND
    (cond
      ((< walthk 0)
       (haws-pltlin pltpt1a pltpt1 walthk)
       (haws-pltlin pltpt1a pltpt1 (- 0 walthk proht))
      )
    ) ;_ end of cond
  ) ;_ end of defun
  ;;Connect two profile points with slope text, etc.
  (defun haws-connectsoft
                     (/ slopestring pt1 gutlip)
    ;;Calculate slope
    (cond
      ((not slope)
       (setq
         slope
          (* (/ (- (cadr pltpt1) (cadr pltpt1a))
                (- (car pltpt1) (car pltpt1a))
                stascl
             ) ;_ end of /
             (/ 100.0 hvexag *haws-elvscl*)
          ) ;_ end of *
       ) ;_ end of setq
      )
      ((= slope 220.1)
       (setq
         gutlip
          (if (< (cadr pltpt1) (cadr pltpt1a))
            -0.083
            0.083
          ) ;_ end of IF
       ) ;_ end of SETQ
       (setq
         slope
          (* (/ (- (cadr pltpt1) (cadr pltpt1a) (* hvexag *haws-elvscl* gutlip))
                (max (- (car pltpt1) (car pltpt1a) 1.5) 0.001)
                stascl
             ) ;_ end of /
             (/ 100.0 hvexag *haws-elvscl*)
          ) ;_ end of *
       ) ;_ end of SETQ
      )
    ) ;_ end of COND
    (if (and
          (not *f1*)
          (/= proht -10000)
          (setq
            temp
             (getreal
               (strcat
                 "\nSlope (%)<"
                 (rtos slope 2 slprec)
                 "%>(1000 for no slope): "
               ) ;_ end of strcat
             ) ;_ end of getreal
          ) ;_ end of setq
        ) ;_ end of AND
      (setq slope temp)
    ) ;_ end of IF
    (haws-setlayr (list laytx "" ""))
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
            (rtos (abs (* (/ proht hvexag *haws-elvscl*) 12)) 2 0)
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
         ((< (distance pltpt1a pltpt1)
             (haws-txlen slopestring ts)
          ) ;_ end of <
          (setq
            slopestring
             (strcat
               (rtos (abs (- (car pltpt1a) (car pltpt1))) 2 0)
               " LF "
               (rtos (abs (* (/ proht hvexag *haws-elvscl*) 12)) 2 0)
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
         (list
           (/ (+ (car pltpt1a) (car pltpt1)) 2)
           (/ (+ (cadr pltpt1a) (cadr pltpt1)) 2)
         ) ;_ end of list
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
      ((< (distance pltpt1a pltpt1)
          (haws-txlen slopestring ts)
       ) ;_ end of <
       (setq
         pt1
          (polar
            pt1
            -1.57
            (+ (/ (abs proht) 2.0) (abs walthk) (* ts 1.5))
          ) ;_ end of polar
       ) ;_ end of setq
      )
      ;;If too little headroom, move above lines
      ((< (abs proht) (* 2 ts))
       (setq pt1 (polar pt1 1.57 (+ ts (/ (abs proht) 2.0) (abs walthk))))
      )
    ) ;_ end of COND
    (haws-mktext
      "m"
      pt1
      ts
      (atan
        (/ (sin (angle pltpt1a pltpt1))
           (cos (angle pltpt1a pltpt1))
        ) ;_ end of /
      ) ;_ end of atan
      slopestring
    ) ;_ end of HAWS-MKTEXT
    (setq slope nil)
  ) ;_ end of DEFUN
  ;;End of subfunctions, begin main function.
  (haws-core-init 285)
  (terpri)
  (setq
    i 1
    ts (* (getvar "dimtxt") (haws-dwgscale))
    lingap
     0.5
    lup
     (getvar "luprec")
  ) ;_ end of SETQ
  (if (not hvexag) (setq hvexag 10))
  (if (not stascl) (setq stascl 1))
  (if (not *haws-elvscl*) (setq *haws-elvscl* 1))
  (if (not slptyp) (setq slptyp ""))
  (if (not stprec) (setq stprec lup))
  (if (not ofprec) (setq ofprec lup))
  (if (not elprec) (setq elprec 2))
  (if (not slprec) (setq slprec 3))
  (if proinputfile
    (setq *f1* proinputfile)
  ) ;_ end of IF
  (while (not (or *f1* refsta))
    (prompt "\nPlease set reference point.")
    (haws-proset)
  ) ;_ end of while
  (while (not (or *f1* proht))
    (prompt "\nPlease set profile type.")
    (haws-proset)
  ) ;_ end of while
  (if (= 0
         (cdr (assoc 40 (tblsearch "STYLE" (getvar "textstyle"))))
      ) ;_ end of =
    (vl-cmdf
      "._text"
      (list 0.0 0.0 0.0)
      ts
      (angtos (/ pi 2))
      "TEST"
    ) ;_ end of command
    (vl-cmdf
      "._text"
      (list 0.0 0.0 0.0)
      (angtos (/ pi 2))
      "TEST"
    ) ;_ end of command
  ) ;_ end of IF
  (entdel (entlast))
  (if (not walthk)
    (setq walthk 0)
  ) ;_ end of IF
  (haws-vsave '("clayer" "osmode" "luprec" "filedia"))
  (setvar "osmode" 0)
  (setvar "dimzin" 0)
  ;;While there's input,
  (while (if *f1*
           (haws-rdfile)
           (haws-getpnt)
         ) ;_ end of IF
    ;;if it is a profile point
    (if (and sta (/= sta "Setup"))
      (progn
        (vl-cmdf "._undo" "_g")
        ;;save the last point
        (setq pltpt1a (cond (pltpt1)))  ;
                                        ;and top of curb
        (setq toppta (cond (toppt)))
        ;;plot the point
        (haws-plthardpro)
        ;;connect to last point
        (if (and (> 1000 proht) pltpt1a)
          (haws-connecthard)
        ) ;_ end of IF
        ;;Add slope label if
        (if
          (and
            (> 1000 proht -10000)       ;profile type is appropriate
            pltpt1a                     ;there was a previous point
            (/= slope 1000)             ;user didn't supress slope
            (or slope (/= 0 (- (car pltpt1) (car pltpt1a)))) ;slope isn't divide by zero
          ) ;_ end of and
           (haws-connectsoft)
        ) ;_ end of IF
        ;;Add point label
        (haws-pltsoftpro)
        (vl-cmdf "._undo" "_e")
        ;;If requested, write all
        ;;the point information
        ;;to a file (*f2*).
        (if *f2*
          (write-line
            (strcat
              "PRFPNT    "
              (haws-mkfld (rtos sta 2 3) 10)
              (haws-mkfld
                (if offset
                  (rtos offset 2 3)
                  ""
                ) ;_ end of IF
                10
              ) ;_ end of mkfld
              (haws-mkfld (rtos elev1 2 3) 10)
              (haws-mkfld
                (if slope
                  (rtos slope 2 4)
                  ""
                ) ;_ end of IF
                10
              ) ;_ end of mkfld
              (haws-mkfld
                (if num2
                  (rtos num2 2 3)
                  ""
                ) ;_ end of IF
                10
              ) ;_ end of mkfld
              (cond
                (lbltxt)
                ("")
              ) ;_ end of COND
            ) ;_ end of STRCAT
            *f2*
          ) ;_ end of WRITE-LINE
        ) ;_ end of IF
        ;;Reset profile point to nil.
        (setq sta nil)
      ) ;_ end of PROGN
      ;;Otherwise, if not reading from file, do setup.
      (if (not *f1*)
        (haws-proset)
      ) ;_ end of IF
    ) ;_ end of IF
    (princ "\rReading line ")
    (princ (setq i (1+ i)))
  ) ;_ end of WHILE
  (setq
    *f1* (if *f1*
         (close *f1*)
       ) ;_ end of IF
    *f2* (if *f2*
         (close *f2*)
       ) ;_ end of IF
  ) ;_ end of SETQ
  ;;Close files
  (haws-vrstor)
  (haws-core-restore)
  (princ)
  ;;and end program.
) ;_ end of DEFUN

;;----------------------------LIST POINT----------------------------
(defun c:haws-lst
        (/ pt x y sta el1)
(haws-core-init 286)
  (setq
    pt  (getpoint "\nPick Point: ")
    x   (car pt)
    y   (cadr pt)
    sta (haws-rtosta (+ refsta (- x refptx)) nil)
    el1 (rtos (+ refelv (/ (- y refpty) hvexag *haws-elvscl*)))
  ) ;_ end of SETQ
  (prompt "\n           STATION = ")
  (princ sta)
  (prompt "  ELEVATION = ")
  (princ el1)
  (princ)
) ;_ end of DEFUN
;;------------------------------------------------------------------
;;ELLABEL labels elevations with reference to a known point.
(defun haws-ellset
              (/ option rpt)
  (while (progn
           (initget "Reference Vertical Flip")
           (setq
             option
              (getkword
                "\nReference point/Vertical Exaggeration/text Flip angle/<return to exit>: "
              ) ;_ end of getkword
           ) ;_ end of setq
         ) ;_ end of PROGN
    (cond
      ((= option "Flip")
       (initget 1)
       (setq
         flpang
          (getangle
            "\nFlip text to right side of line beyond what angle?: "
          ) ;_ end of getangle
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
      ((= option "Vertical") (haws-gethvx))
    ) ;_ end of COND
  ) ;_ end of WHILE
) ;_ end of DEFUN
(defun haws-ellplt
              (/ ang1 ts el1 elev2 txlen left pt2 pt3 pt4 pt5)
  (setq pt5 nil)
  (while (progn
           (initget 1 "Two")
           (setq ang1 (getangle pt1 "\nLeader rotation or [Two elevs]: "))
           (= ang1 "Two")
         ) ;_ end of PROGN
    (setq pt5 (getpoint "\nSecond elevation point: "))
  ) ;_ end of WHILE
  (setq
    ts (* (haws-dwgscale) (getvar "dimtxt"))
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
     (strcat
       (rtos
         (+ refelv (/ (- (cadr pt1) refpty) hvexag *haws-elvscl*))
         2
         2
       ) ;_ end of rtos
       el1
     ) ;_ end of strcat
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
       (strcat
         (rtos
           (+ refelv (/ (- (cadr pt5) refpty) hvexag *haws-elvscl*))
           2
           2
         ) ;_ end of rtos
         elev2
       ) ;_ end of strcat
       " "
     ) ;_ end of IF
    txlen
     (max
       (haws-txlen el1 ts)
       (haws-txlen elev2 ts)
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
       (+ ang1
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
  (vl-cmdf "._line" pt1 pt3 "")
  (haws-mktext
    (if left
      "BR"
      "BL"
    ) ;_ end of IF
    pt2
    ts
    (if left
      (+ ang1 pi)
      ang1
    ) ;_ end of IF
    el1
  ) ;_ end of mktext
  (if pt5
    (haws-mktext
      (if left
        "TR"
        "TL"
      ) ;_ end of IF
      pt4
      ts
      (if left
        (+ ang1 pi)
        ang1
      ) ;_ end of IF
      elev2
    ) ;_ end of mktext
  ) ;_ end of IF
) ;_ end of DEFUN
(defun c:haws-ellabel
            (/ pt1)
  (haws-core-init 287)
  (haws-vsave '("osmode"))
  (setvar "dimzin" 0)
  (if (not hvexag)
    (setq hvexag 10)
  ) ;_ end of IF
  (if (not refpty)
    (haws-ellset)
  ) ;_ end of IF
  (while (progn
           (setvar "osmode" 13)
           (initget "Setup")
           (setq pt1 (getpoint "\nPoint to label or [Setup]: "))
         ) ;_ end of progn
    (cond
      ((= pt1 "Setup") (haws-ellset))
      (t (haws-ellplt))
    ) ;_ end of COND
  ) ;_ end of WHILE
  (haws-vrstor)
  (haws-core-restore)
  (princ)
) ;_ end of DEFUN

;;---------------------------STATION LABEL-------------------------
(defun c:haws-stalabel
             (/ pnt1 inc n sta1 endsta sta)
  (haws-core-init 288)
  (haws-setlayr "PSTALBL")
  (haws-vsave '("luprec"))
  (setvar "luprec" 0)
  (setq
    pnt1
     (getpoint "\nTop center point of first label: ")
    inc
     (getdist "\nDistance between labels: ")
    n (getint "\nNumber of labels: ")
    sta1
     (getint "\nFirst station: ")
    endsta
     (+ sta1 (* (1- n) inc))
    sta
     sta1
  ) ;_ end of SETQ
  (while (<= sta endsta)
    (haws-mktext
      "tc"
      (list (+ (car pnt1) (- sta sta1)) (cadr pnt1))
      (* (haws-dwgscale) 0.12)
      0
      (haws-rtosta sta nil)
    ) ;_ end of mktext
    (setq sta (+ inc sta))
  ) ;_ end of WHILE
  (haws-vrstor)
  (haws-core-restore)
  (princ)
) ;_ end of DEFUN
;;---------------------------ELEVATION LABEL------------------------
(defun c:haws-elv
        (/ pnt1 inc n elv1 endelv elv)
(haws-core-init 289)
  (if (not hvexag)
    (haws-gethvx)
  ) ;_ end of IF
  (haws-setlayr "PELEVLBL")
  (setq
    pnt1
     (getpoint "\nBottom right point of first label: ")
    inc
     (getint "\nElevation increment: ")
    n (getint "\nNumber of labels: ")
    elv1
     (getint "\nFirst elevation: ")
    endelv
     (+ elv1 (* (1- n) inc))
    elv
     elv1
  ) ;_ end of SETQ
  (while (<= elv endelv)
    (haws-mktext
      "br"
      (list
        (car pnt1)
        (+ (* hvexag *haws-elvscl* (- elv elv1)) (cadr pnt1))
      ) ;_ end of list
      (* (haws-dwgscale) 0.12)
      0
      (itoa elv)
    ) ;_ end of mktext
    (setq elv (+ inc elv))
  ) ;_ end of WHILE
  (princ)
) ;_ end of DEFUN
;;--------------------------SLOPE CALCULATOR------------------------------
(defun c:haws-grd
        (/ a)
(haws-core-init 290)
  (defun haws-gr1
             (/ sta1 el1 sta2 elev2 stad eld x1 gr)
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
  (defun haws-gr2
             (/ sta1 el1 sta2 gr stad elev2)
    (prompt
      "\nTo Determine the Elevation at a Specified Station."
    ) ;_ end of prompt
    (setq
      sta1  (getreal "\nEnter 1st Station as a real number: ")
      el1   (getreal "\nEnter 1st Elevation: ")
      sta2  (getreal "\nEnter 2nd Station as a real number: ")
      gr    (getreal "\nEnter Slope in Percent: ")
      stad  (- sta2 sta1)
      elev2 (+ el1 (* stad (/ gr 100.0)))
    ) ;_ end of SETQ
    (prompt "\nELEVATION AT STATION ")
    (princ sta2)
    (prompt " = ")
    (prompt (rtos elev2 2 4))
  ) ;_ end of DEFUN
  (initget 1 "Slope Elevation")
  (setq a (getkword "\nFind Slope/Elevation:"))
  (cond
    ((= a "Slope") (haws-gr1))
    ((= a "Elevation") (haws-gr2))
  ) ;_ end of COND
  (princ)
) ;_ end of DEFUN

(defun c:haws-grc ()
(haws-core-init 291) (haws-linblk "*GRC" "PGC"))
(defun c:haws-grb ()
(haws-core-init 292) (haws-linblk "*GRB" "PGB"))
(defun haws-linblk
              (blname bllay / pt1 pt2)
  (haws-setlayr bllay)
  (setq pt1 (getpoint "\nFirst point: "))
  (vl-cmdf "._pline" pt1)
  (while (setq pt2 (getpoint pt1 "Next point: "))
    (vl-cmdf pt2)
    (setq pt1 pt2)
  ) ;_ end of while
  (vl-cmdf "")
  (vl-cmdf
    "._insert"
    blname
    "_Scale"
    (* (haws-dwgscale) (getvar "dimtxt"))
    pt1
  ) ;_ end of command
) ;_ end of DEFUN
