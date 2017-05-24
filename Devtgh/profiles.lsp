;;;PROFILES.LSP
;;;(C) Copyright 1997 by Thomas Gail Haws
;;;A collection of related civil profile drafting tools

;;;GETHVX is used in various profile routines to get vertical exaggeration.
(DEFUN
   HAWS-GETHVX
              (/ HVOLD)
  (IF (NOT HVEXAG)
    (SETQ HVOLD 10.0)
    (SETQ HVOLD HVEXAG)
  ) ;_ end of IF
  (IF (NOT *HAWS-ELVSCL*)
    (SETQ *HAWS-ELVSCL* 1)
  ) ;_ end of if
  (SETQ
    HVEXAG
     (GETREAL
       (STRCAT
         "\nHoriz. scale / vertical scale "
         "(Usually 10 for P&P sheets) <"
         (RTOS HVOLD 2 0)
         ">:"
       ) ;_ end of strcat
     ) ;_ end of GETREAL
  ) ;_ end of SETQ
  (IF (NOT HVEXAG)
    (SETQ HVEXAG HVOLD)
  ) ;_ end of IF
) ;_ end of DEFUN

(DEFUN
   c:haws-PROPIPE
            (/ P1 P2 D1 PIPSLD D2 P3 P4)
  (haws-core-borrow 0)
  (HAWS-VSAVE '("filedia"))
  (HAWS-GETHVX)
  (SETQ
    P1 (GETPOINT "\nEndpoint of pipe invert: ")
    P2 (GETPOINT P1 "\nSecond endpoint of pipe invert: ")
    D1 (* (/ (GETDIST "\nID of pipe (in.): ") 12) HVEXAG *HAWS-ELVSCL*)
  ) ;_ end of SETQ
  (COND
    ((SETQ PIPSLD (FINDFILE "pipetabl.sld"))
     (SETVAR "filedia" 0)
     (COMMAND "._vslide" PIPSLD)
    )
  ) ;_ end of cond
  (SETQ
    D2 (* (/ (GETDIST "\nWall thickness (in.): ") 12)
          HVEXAG
          *HAWS-ELVSCL*
       ) ;_ end of *
  ) ;_ end of setq
  (REDRAW)
  (COMMAND "line" P1 P2 "")
  (SETQ
    P3 (POLAR P1 (/ PI 2) D1)
    P4 (POLAR P2 (/ PI 2) D1)
  ) ;_ end of SETQ
  (COMMAND "line" P3 P4 "")
  (SETQ
    P3 (POLAR P1 (/ PI -2) D2)
    P4 (POLAR P2 (/ PI -2) D2)
  ) ;_ end of SETQ
  (COND
    ((/= 0 D2)
     (COMMAND "line" P3 P4 "")
     (SETQ D1 (+ D1 D2))
     (SETQ
       P3 (POLAR P1 (/ PI 2) D1)
       P4 (POLAR P2 (/ PI 2) D1)
     ) ;_ end of SETQ
     (COMMAND "line" P3 P4 "")
    )
  ) ;_ end of COND
  (haws-core-return)
) ;_ end of DEFUN

(DEFUN
   c:haws-PROSUP
           (/  BOTPIP ELLIP1 HVEXAG LEFT LINE1 LINE2 LINE3
              PT1 PT2 PT3 PT4 PTCEN RIGHT UCSYDIR VRAD
           )
  (haws-core-borrow 0)
  (HAWS-VSAVE '("ucsfollow" "osmode" "clayer"))
  (HAWS-MKLAYR "PSUP")
  (SETVAR "ucsfollow" 0)
  (SETVAR "osmode" 16)
  (IF (NOT HVEXAG)
    (HAWS-GETHVX)
  ) ;_ end of IF
  (SETQ
    PT1
     (GETPOINT "\nLeft ellipse quadrant: ")
    PT2
     (GETPOINT PT1 "\nRight ellipse quadrant: ")
    BOTPIP
     (ENTSEL "\nTop of lower pipe: ")
    VRAD
     (* HVEXAG *HAWS-ELVSCL* (/ (DISTANCE PT1 PT2) 2))
    PT3
     (POLAR PT1 (/ PI -2) VRAD)
    PT4
     (POLAR PT2 (/ PI -2) VRAD)
    PTCEN
     (MAPCAR
       '(LAMBDA (LEFT RIGHT) (/ (+ LEFT RIGHT) 2.0))
       PT1
       PT2
     ) ;_ end of mapcar
  ) ;_ end of SETQ
  (SETVAR "osmode" 0)
  (ENTMAKE
    (LIST
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (CONS 10 (TRANS PTCEN 1 0))
      '(41 . 0.0)
      (CONS
        11
        (MAPCAR
          '(LAMBDA (UCSYDIR) (* UCSYDIR VRAD))
          (GETVAR "ucsydir")
        ) ;_ end of mapcar
      ) ;_ end of CONS
      (CONS 40 (/ 1.0 (* HVEXAG *HAWS-ELVSCL*)))
      (CONS 42 (* 2 PI))
    ) ;_ end of list
  ) ;_ end of entmake
  (COMMAND "._ellipse" PT1 PT2 VRAD)
  (SETQ ELLIP1 (ENTLAST))
  (COMMAND "line" PT1 PT3 "")
  (SETQ LINE1 (ENTLAST))
  (COMMAND "._line" PT2 PT4 "")
  (SETQ LINE2 (ENTLAST))
  (COMMAND
    "._trim"
    LINE1
    LINE2
    ""
    (LIST ELLIP1 (POLAR PT1 (/ PI 2) (/ VRAD 10)))
    ""
  ) ;_ end of command
  (IF (/= "ELLIPSE" (CDR (ASSOC 0 (ENTGET ELLIP1))))
    (SETQ ELLIP1 (ENTLAST))
  ) ;_ end of IF
  (COMMAND
    "._extend"
    BOTPIP
    ""
    (LIST LINE1 PT3)
    (LIST LINE2 PT4)
    ""
  ) ;_ end of command
  (SETQ PT3 (TRANS (CDR (ASSOC 11 (ENTGET LINE1))) LINE1 1))
  (SETQ PT4 (TRANS (CDR (ASSOC 11 (ENTGET LINE2))) LINE2 1))
  (COMMAND "._line" PT3 PT4 "")
  (SETQ LINE3 (ENTLAST))
  (COMMAND
    "._hatch" "ansi31" "5" "0" ELLIP1 LINE1 LINE2 LINE3 ""
   ) ;_ end of COMMAND
 ;_ end of COMMAND
 ;_ end of command
  (COMMAND "._erase" ELLIP1 LINE3 "")
  (REDRAW)
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of DEFUN

(DEFUN c:haws-PC () (c:haws-PROCB))
(DEFUN
   c:haws-PROCB
          (/ TOPPT BOTPT CBVIEW WIDTH)
  (haws-core-borrow 0)
  (IF (NOT HVEXAG)
    (HAWS-GETHVX)
  ) ;_ end of IF
  (HAWS-VSAVE '("clayer"))
  (INITGET "Exist Prop")
  (SETQ
    EXIST
     (= (GETKWORD "[Exist/Prop] <Prop>: ") "Exist")
    ;;    curbht (/ (getrealx "\nCurb height" (* curbht 12) 6)12)
    TOPPT
     (GETPOINT "\nTop of curb: ")
    BOTPT
     (LIST (CAR TOPPT) (CADR (GETPOINT TOPPT "\nInvert: ")) 0.0)
  ) ;_ end of SETQ
  (INITGET "L R C")
  (SETQ CBVIEW (GETKWORD "Left, right, or center view?<L/R/C>?"))
  (SETQ
    WIDTH
     (IF (= CBVIEW "C")
       (/ (GETREAL "Width: ") 2)
       2.0
     ) ;_ end of IF
  ) ;_ end of SETQ
  (IF EXIST
    (HAWS-MKLAYR "PXCB")
    (HAWS-MKLAYR "PCB")
  ) ;_ end of IF
  (HAWS-DRAWCB BOTPT TOPPT CBVIEW WIDTH NIL)
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of DEFUN

(DEFUN
   HAWS-DRAWCB
              (BOTPT TOPPT CBVIEW WIDTH BRKPT / PT1 PT2 PT3 PT4 PT5 PTBL PTBR CB
              )
  (SETQ
    PT1 (POLAR BOTPT 0.0 WIDTH)
    PT2 (LIST (CAR PT1) (CADR TOPPT) 0.0)
    PT3 (POLAR BOTPT PI WIDTH)
    PT4 (LIST (CAR PT3) (CADR TOPPT) 0.0)
    PT5 (LIST
          (CAR BOTPT)
          (- (CADR TOPPT) (* HVEXAG *HAWS-ELVSCL* 0.583))
          0.0
        ) ;_ end of list
  ) ;_ end of SETQ
  (COND
    ((= CBVIEW "L")
     (COMMAND
       "._pline"
       PT4
       "w"
       0
       ""
       (SETQ PTBL PT3)
       (SETQ PTBR BOTPT)
       PT5
       ""
     ) ;_ end of command
    )
    ((= CBVIEW "C")
     (COMMAND
       "._pline"
       PT4
       "w"
       0
       ""
       (SETQ PTBL PT3)
       (SETQ PTBR PT1)
       PT2
       ""
     ) ;_ end of command
     (SETQ WIDTH (* WIDTH 2))
    )
    ((= CBVIEW "R")
     (COMMAND
       "._pline"
       PT5
       "w"
       0
       ""
       (SETQ PTBL BOTPT)
       (SETQ PTBR PT1)
       PT2
       ""
     ) ;_ end of command
    )
  ) ;_ end of COND
  (SETQ CB (ENTLAST))
  (COND
    (BRKPT
     (SETQ
       PT1 (POLAR
             (LIST (/ (+ (CAR PTBR) (CAR PTBL)) 2) (CADR BRKPT) 0.0)
             0.262
             WIDTH
           ) ;_ end of polar
       PT2 (POLAR PT1 (/ PI 2) 2)
       PT3 (POLAR PT1 3.403 (* WIDTH 2))
       PT4 (POLAR PT2 3.403 (* WIDTH 2))
     ) ;_ end of SETQ
     (HAWS-MKLINE PT1 PT3)
     (SETQ LINE1 (ENTLAST))
     (HAWS-MKLINE PT2 PT4)
     (SETQ LINE2 (ENTLAST))
     (COMMAND
       "._trim"
       LINE1
       LINE2
       ""
       (LIST CB PT4)
       (LIST (ENTLAST) PT1)
       ""
     ) ;_ end of command
    )
  ) ;_ end of COND
) ;_ end of DEFUN

(DEFUN c:haws-PM () (c:haws-PROMH))
(DEFUN
   c:haws-PROMH
          (/ TOPPT BOTPT)
  (haws-core-borrow 0)
  (HAWS-VSAVE '("clayer"))
  (INITGET "Exist Prop")
  (SETQ
    EXIST
     (= (GETKWORD "Exist/Prop: ") "Exist")
    BOTPT
     (GETPOINT "\nInvert: ")
    TOPPT
     (LIST (CAR BOTPT) (CADR (GETPOINT BOTPT "\nRim: ")) 0.0)
  ) ;_ end of SETQ
  (IF EXIST
    (HAWS-MKLAYR "PXMH")
    (HAWS-MKLAYR "PMH")
  ) ;_ end of IF
  (HAWS-DRAWMH BOTPT TOPPT NIL)
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of DEFUN

;;Drawmh draws a cartoon manhole from botpt to toppt.
(DEFUN
   HAWS-DRAWMH
              (BOTPT TOPPT BRKPT / EXIST BROKEN PT1 PT2 PT3 PT4 MH LINE1 LINE2
              )
  (SETQ
    PT1
     (POLAR TOPPT 0 1)
    PT2
     (POLAR TOPPT 0 -1)
    PT3
     (POLAR BOTPT 0 -2)
    PT4
     (POLAR BOTPT 0 2)
    BOTPT
     (POLAR BOTPT 1.5708 -5)
    TOPPT
     (POLAR TOPPT 1.5708 40)
  ) ;_ end of SETQ
  (COMMAND "._pline" PT1 "w" 0 "" PT2 PT3 PT4 "close")
  (SETQ MH (ENTLAST))
  (COND
    (BRKPT
     (SETQ
       PT1 (POLAR BRKPT 0.262 3)
       PT2 (POLAR PT1 (/ PI 2) 2)
       PT3 (POLAR PT1 3.403 6)
       PT4 (POLAR PT2 3.403 6)
     ) ;_ end of SETQ
     (HAWS-MKLINE PT1 PT3)
     (SETQ LINE1 (ENTLAST))
     (HAWS-MKLINE PT2 PT4)
     (SETQ LINE2 (ENTLAST))
     (COMMAND
       "._trim"
       LINE1
       LINE2
       ""
       (LIST MH PT4)
       (LIST (ENTLAST) PT1)
       ""
     ) ;_ end of command
    )
  ) ;_ end of COND
) ;_ end of DEFUN

(DEFUN
   c:haws-PRED
         ()
  (command "_ai_editcustfile" (STRCAT (HAWS-GETDNPATH) ".pro"))
) ;_ end of defun

(DEFUN
   c:haws-PROE
         (/ LANAME INVERT DIAM PIPSLD WALTHK CENPT INRAD OUTRAD
         )
  (haws-core-borrow 0)
  (IF (NOT HVEXAG)
    (HAWS-GETHVX)
  ) ;_ end of IF
  (HAWS-VSAVE '("clayer" "filedia"))
  (INITGET "Exist Prop")
  (SETQ EXIST (= (GETKWORD "Exist/Prop: ") "Exist"))
  (IF EXIST
    (HAWS-MKLAYR "PXEL")
    (HAWS-MKLAYR "PEL")
  ) ;_ end of IF
  (SETQ
    INVERT
     (GETPOINT "\nInvert point: ")
    DIAM
     (/ (GETREAL "\nID (inches): ") 12)
  ) ;_ end of SETQ
  (COND
    ((SETQ PIPSLD (FINDFILE "pipetabl.sld"))
     (SETVAR "filedia" 0)
     (COMMAND "._vslide" PIPSLD)
    )
  ) ;_ end of cond
  (SETQ
    WALTHK
     (/ (GETREAL "\nWall thickness (inches): ") 12)
    INRAD
     (/ DIAM 2.0)
    OUTRAD
     (+ (/ DIAM 2.0) WALTHK)
    CENPT
     (POLAR INVERT (/ PI 2.0) (* INRAD HVEXAG *HAWS-ELVSCL*))
  ) ;_ end of SETQ
  (REDRAW)
  (ENTMAKE
    (LIST
      '(0 . "ELLIPSE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbEllipse")
      (CONS 10 (TRANS CENPT 1 0))
      '(41 . 0.0)
      (CONS
        11
        (MAPCAR
          '(LAMBDA (UCSYDIR) (* UCSYDIR INRAD HVEXAG *HAWS-ELVSCL*))
          (GETVAR "ucsydir")
        ) ;_ end of mapcar
      ) ;_ end of CONS
      (CONS 40 (/ 1.0 (* HVEXAG *HAWS-ELVSCL*)))
      (CONS 42 (* 2 PI))
    ) ;_ end of list
  ) ;_ end of entmake
  (IF (/= 0 WALTHK)
    (ENTMAKE
      (LIST
        '(0 . "ELLIPSE")
        '(100 . "AcDbEntity")
        '(100 . "AcDbEllipse")
        (CONS 10 (TRANS CENPT 1 0))
        '(41 . 0.0)
        (CONS
          11
          (MAPCAR
            '(LAMBDA (UCSYDIR) (* UCSYDIR OUTRAD HVEXAG *HAWS-ELVSCL*))
            (GETVAR "ucsydir")
          ) ;_ end of mapcar
        ) ;_ end of CONS
        (CONS 40 (/ 1.0 (* HVEXAG *HAWS-ELVSCL*)))
        (CONS 42 (* 2 PI))
      ) ;_ end of list
    ) ;_ end of entmake
  ) ;_ end of IF
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of DEFUN

(DEFUN
   c:haws-PLDR
         (/ P1 P2 DS TS AS DG ANG LEFT P3 P4 P5 STR TEMP)
  (haws-core-borrow 0)
  (SETQ
    P1 (GETPOINT "\nStart point for leader:")
    P2 (GETPOINT P1 "\nEnd point for leader:")
    DS (HAWS-DWGSCALE)
    TS (* DS (GETVAR "dimtxt"))
    AS (* DS (GETVAR "dimasz"))
    DG (* DS (ABS (GETVAR "dimgap")))
    ANG
     (ANGLE P1 P2)
    LEFT
     (MINUSP (COS ANG))
    P3 (POLAR P1 ANG AS)
    P4 (POLAR
         P2
         (IF LEFT
           PI
           0
         ) ;_ end of IF
         AS
       ) ;_ end of POLAR
    P5 (POLAR
         (POLAR
           P4
           (IF LEFT
             PI
             0
           ) ;_ end of IF
           DG
         ) ;_ end of POLAR
         (/ PI -2)
         (/ TS 2)
       ) ;_ end of POLAR
    STR
     (PROGN (INITGET "Exist Prop") (GETKWORD "Exist/Prop: "))
    EXIST
     (= STR "Exist")
    STR
     (IF (= STR "Exist")
       "EXIST "
       ""
     ) ;_ end of IF
    STR
     (STRCAT
       STR
       (COND
         ((PROGN
            (INITGET "Tc GUtter Pavmt Grnd")
            (= (SETQ TEMP (GETKWORD "Tc/GUtter/Pavmt/Grnd: ")) "Tc")
          ) ;_ end of progn
          "TOP OF CURB "
         )
         ((= TEMP "GUtter") "GUTTER ")
         ((= TEMP "Pavmt") "PVMT ")
         ((= TEMP "Grnd") "GROUND ")
       ) ;_ end of COND
     ) ;_ end of STRCAT
    STR
     (STRCAT
       STR
       (COND
         ((PROGN
            (INITGET "Left Right Center")
            (= (SETQ TEMP (GETKWORD "Left/Right/Center: ")) "Left")
          ) ;_ end of progn
          "LEFT"
         )
         ((= TEMP "Right") "RIGHT")
         ((= TEMP "Center") "@ C/L")
       ) ;_ end of COND
     ) ;_ end of STRCAT
  ) ;_ end of SETQ
  (IF EXIST
    (HAWS-MKLAYR "PXLDR")
    (HAWS-MKLAYR "PLDR")
  ) ;_ end of IF
  (COND
    ((>= (ATOF (GETVAR "acadver")) 14)
     (COMMAND "._leader" P1 P2 "" STR "")
    )
    (T
     (COMMAND "._pline" P1 "w" 0 (/ AS 3) P3 "w" 0 0 P2 P4 "")
     (HAWS-MKTEXT
       (IF LEFT
         "r"
         NIL
       ) ;_ end of IF
       P5
       TS
       0
       STR
     ) ;_ end of mktext
    )
  ) ;_ end of COND
  (haws-core-return)
) ;_ end of DEFUN

(DEFUN
   c:haws-NEWPRO
           (/ EXPOLD PROINPUTFILE prolaywc SS1)
  (SETQ EXPOLD (GETVAR "expert"))
  (SETVAR "expert" 0)
  (COMMAND "._undo" "m" "._layer" "off" "*" "" "")
  (HAWS-MKLAYR "PROSTRT")
  (SETQ
    prolayWC (STRCAT (CAR(HAWS-GETLAYR "PROPRE")) "*")
    SS1          (SSGET "X" (LIST (CONS 8 prolayWC)))
    PROINPUTFILE (OPEN (STRCAT (HAWS-GETDNPATH) ".pro") "r")
  ) ;_ end of SETQ
  (COMMAND "._erase" SS1 "" "._layer" "u" prolayWC "")
  (c:HAWS-PRO)
  (COMMAND "._layer" "on" "*" "")
  (PROMPT "\nType Undo Back to undo entire profile.")
  (PRINC)
) ;_ end of DEFUN

;;File version converter for PRO
;;Adds or removes fields between the slope and label column.
(DEFUN
   c:haws-PROFC
          (/ COLWID FLIST FM FNAME I M N RDLIN TEMP TEMPFILE)
  (haws-core-borrow 0)
  (SETQ
    F1 (OPEN
         (GETFILED "List of files to convert" (HAWS-GETDNPATH) "lst" 0)
         "r"
       ) ;_ end of open
  ) ;_ end of setq
  (WHILE (SETQ RDLIN (READ-LINE F1))
    (SETQ FLIST (CONS (FINDFILE RDLIN) FLIST))
  ) ;_ end of while
  (TEXTSCR)
  (SETQ
    F1 (CLOSE F1)
    COLWID
     (GETINT "New column width: ")
    M (GETINT "Old number of fields between slope and label: ")
    N (GETINT "New number of fields between slope and label: ")
    I -1
  ) ;_ end of SETQ
  (IF (FINDFILE
        (SETQ TEMPFILE (STRCAT "pro" (SUBSTR (GETVAR "loginname") 1 5) ".tmp"))
      ) ;_ end of findfile
    (COMMAND "del" TEMPFILE)
  ) ;_ end of IF
  (WHILE (SETQ FNAME (NTH (SETQ I (1+ I)) FLIST))
    (SETQ F1 (OPEN FNAME "r"))
    (SETQ F2 (OPEN TEMPFILE "w"))
    (WHILE (SETQ RDLIN (READ-LINE F1))
      (COND
        ((= (SUBSTR RDLIN 1 6) "COLWID")
         (IF (ZEROP (SETQ FM (ATOI (SUBSTR RDLIN 8))))
           (SETQ FM (SUBSTR RDLIN 8))
         ) ;_ end of IF
         (SETQ RDLIN (STRCAT "COLWID " (ITOA COLWID)))
        )
        ((= (SUBSTR RDLIN 1 6) "PRFPNT")
         (SETQ
           RDLIN
            (STRCAT
              (HAWS-MKFLD (haws-rdfld 1 RDLIN FM 1) COLWID)
              (HAWS-MKFLD (haws-rdfld 2 RDLIN FM 1) COLWID)
              (HAWS-MKFLD (haws-rdfld 3 RDLIN FM 1) COLWID)
              (HAWS-MKFLD (haws-rdfld 4 RDLIN FM 1) COLWID)
              (HAWS-MKFLD (haws-rdfld 5 RDLIN FM 1) COLWID)
              (IF (> N 0)
                (HAWS-MKFLD
                  (IF (SETQ TEMP (HAWS-ATOFX (haws-rdfld 6 RDLIN FM 3) "*RIM*" 0))
                    (RTOS TEMP 2 2)
                    ""
                  ) ;_ end of IF
                  COLWID
                ) ;_ end of mkfld
                ""
              ) ;_ end of IF
              (IF (> N 1)
                (HAWS-MKFLD
                  (IF (SETQ TEMP (HAWS-ATOFX (haws-rdfld 6 RDLIN FM 3) "*\"*,*IN*" 1))
                    (RTOS TEMP 2 2)
                    ""
                  ) ;_ end of IF
                  COLWID
                ) ;_ end of mkfld
                ""
              ) ;_ end of IF
              (haws-rdfld (+ 6 M) RDLIN FM 3)
            ) ;_ end of STRCAT
         ) ;_ end of SETQ
        )
        ((WCMATCH RDLIN "REFPNT*,PRECIS*,HVEXAG*,PROTYP*")
         (SETQ
           RDLIN
            (STRCAT
              (MKFLD (haws-rdfld 1 RDLIN FM 1) COLWID)
              (MKFLD (haws-rdfld 2 RDLIN FM 1) COLWID)
              (MKFLD (haws-rdfld 3 RDLIN FM 1) COLWID)
              (MKFLD (haws-rdfld 4 RDLIN FM 1) COLWID)
              (MKFLD (haws-rdfld 5 RDLIN FM 1) COLWID)
            ) ;_ end of STRCAT
         ) ;_ end of SETQ
        )
      ) ;_ end of COND
      (WHILE (= " " (SUBSTR RDLIN (MAX (STRLEN RDLIN) 1)))
        (SETQ RDLIN (SUBSTR RDLIN 1 (1- (STRLEN RDLIN))))
      ) ;_ end of WHILE
      (WRITE-LINE RDLIN F2)
    ) ;_ end of WHILE
    (SETQ
      F1 (CLOSE F1)
      F2 (CLOSE F2)
    ) ;_ end of SETQ
    (COMMAND "sh" (STRCAT "copy " TEMPFILE " " FNAME))
    (COMMAND "sh" (STRCAT "del " TEMPFILE))
    (PRINC)
  ) ;_ end of WHILE
) ;_ end of DEFUN
;;;;-----------------------------PRO------------------------------------------
(DEFUN
   c:HAWS-PRO;;Localize the variables
             (/ I PLTPT1A TS LINGAP LUP FM STA EL1DSC ELEV1 OFFSET SLOPE NUM2 PTOPT LBLTXT ACADX ACADY1 ACADY2
              LBLMHRIM MHRIM LBLCBTC CBTC PEXIST PLTPT1 PLTSTA PLTELV INVMIR INVUP STATXT OFFTXT EL1TXT TOPTX BOTPT
              LBLPT1 LBLPT2 LBLPT3 TXLEN TXPT1 TXPT2 TXPT3 TXPT4 TXPT5 TXPT6 TXPT7 TXPT8 TOPPT TXPT11 TXPT12 TXPT13
              TXPT14
             )
  (PROMPT
    "\nProfile drafter version 5.06.  Copyright 2000, Thomas Gail Haws."
  ) ;_ end of prompt
  (DEFUN
     c:haws-TGH2_PRO
               ()
    (WHILE (PROGN
             (INITGET "Ref Type Vertical Precision New File Plot")
             (SETQ
               PROOPT
                (GETKWORD
                  "\nRef. point/prof. Type/Vert. exag./Precision/New prof./File/Plot: "
                ) ;_ end of getkword
             ) ;_ end of setq
             (COND
               ((= PROOPT
                   "Ref"
                   ((= PROOPT "Type") (GETTYP))
                   ((= PROOPT "Vertical")
                     (GETHVX)
                     (IF F2
                       (WRITE-LINE (STRCAT "HVEXAG    " HVEXAG) F2)
                     ) ;_ end of IF
                   )
                   ((= PROOPT "Precision") (GETPREC))
                   ((= PROOPT "New")
                     (SETQ PLTPT1 NIL)
                     (PROMPT "\nEnded previous profile, starting new profile.")
                     (IF F2
                       (WRITE-LINE "NEWPRF" F2)
                     ) ;_ end of IF
                   )
                   ((= PROOPT "File") (GETFOP))
                   ((= PROOPT "Plot") (PLTPRO))
                ) ;_ end of =
               )
             ) ;_ end of cond
           ) ;_ end of progn
    ) ;_ end of while
  ) ;_ end of defun
  ;;Call the different setup sub-functions
  (DEFUN
     HAWS-PROSET
                (/ OPT)
    (WHILE (SETQ
             OPT
              (PROGN
                (INITGET "Ref Type Vertical Precision New File")
                (GETKWORD
                  "\nRef. point/prof. Type/Vert. exag./Precision/New prof./File: "
                ) ;_ end of getkword
              ) ;_ end of PROGN
           ) ;_ end of SETQ
      (COND
        ((= OPT "Ref") (GETREF))
        ((= OPT "Type") (GETTYP))
        ((= OPT "Vertical")
         (GETHVX)
         (IF F2
           (WRITE-LINE (STRCAT "HVEXAG    " HVEXAG) F2)
         ) ;_ end of IF
        )
        ((= OPT "Precision") (GETPREC))
        ((= OPT "New")
         (SETQ PLTPT1 NIL)
         (PROMPT "\nEnded previous profile, starting new profile.")
         (IF F2
           (WRITE-LINE "NEWPRF" F2)
         ) ;_ end of IF
        )
        ((= OPT "File") (GETFOP))
      ) ;_ end of COND
    ) ;_ end of WHILE
  ) ;_ end of DEFUN
  ;;Get the profile reference point to tie profile to ACAD coordinates
  (DEFUN
     GETREF
           (/ R)
    (WHILE (NOT
             (AND
               (SETQ R (GETPOINT "\nPick a reference point:  "))
               (SETQ REFSTA (GETREAL "\nEnter reference point station as a real no.: "))
               (SETQ REFELV (GETREAL "\nEnter reference point elevation: "))
             ) ;_ end of AND
           ) ;_ end of NOT
    ) ;_ end of WHILE
    (SETQ
      REFPTX
       (CAR R)
      REFPTY
       (CADR R)
    ) ;_ end of SETQ
    (IF F2
      (WRITE-LINE
        (STRCAT
          (MKFLD "REFPNT" COLWID)
          (MKFLD (RTOS REFPTX 2 2) COLWID)
          (MKFLD (RTOS REFPTY 2 2) COLWID)
          (MKFLD (RTOS REFSTA 2 2) COLWID)
          (MKFLD (RTOS REFELV 2 2) COLWID)
        ) ;_ end of STRCAT
        F2
      ) ;_ end of WRITE-LINE
    ) ;_ end of IF
  ) ;_ end of DEFUN
  ;;Setup layers for the profile
  (DEFUN
     SETLAY
           (/ LLINC LLINLT LTXC LTXLT)
    (COND
      ((AND (= PROHT -20000) (WCMATCH PROTYP "X*"))
       (SETQ LAYLIN (HAWS-GETLAYR "PROXDIM"))
      )
      ((= PROHT -20000) (SETQ LAYLIN (HAWS-GETLAYR "PRODIM")))
      ((AND (= PROHT -10000) (WCMATCH PROTYP "X*"))
       (SETQ LAYLIN (HAWS-GETLAYR "PROXLINE"))
      )
      ((= PROHT -10000) (SETQ LAYLIN (HAWS-GETLAYR "PROLINE")))
      ((AND (= PROHT 1000) (WCMATCH PROTYP "X*"))
       (HAWS-MKLAYR "PROPNT")
       (SETQ LAYLIN (HAWS-GETLAYR "PROXEL"))
      )
      ((= PROHT 1000)
       (HAWS-MKLAYR "PROPNT")
       (SETQ LAYLIN (HAWS-GETLAYR "PROEL"))
      )
      ((AND (>= PROHT 0) (WCMATCH PROTYP "X*,MESA"))
       (SETQ LAYLIN (HAWS-GETLAYR "PROXSTRT"))
      )
      ((>= PROHT 0) (SETQ LAYLIN (HAWS-GETLAYR "PROSTRT")))
      ((AND (< PROHT 0) (WCMATCH PROTYP "X*"))
       (SETQ
         LAYLIN
          (LIST
            (STRCAT
              (CAR (HAWS-GETLAYR "PROPRE"))
              (CAR (SETQ LAYLIN (HAWS-GETLAYR "PROEXMID")))
              (SUBSTR PROTYP 2)
            ) ;_ end of strcat
            (CADR LAYLIN)
            (CADDR LAYLIN)
          ) ;_ end of LIST
       ) ;_ end of SETQ
      )
      ((< PROHT 0)
       (SETQ
         LAYLIN
          (COND
            ((HAWS-GETLAYR (STRCAT "PRO" PROTYP)))
            ((LIST
               (STRCAT (CAR (SETQ LAYLIN (HAWS-GETLAYR "PROPRE"))) PROTYP)
               (CADR LAYLIN)
               (CADDR LAYLIN)
             ) ;_ end of list
            )
          ) ;_ end of COND
       ) ;_ end of SETQ
      )
    ) ;_ end of COND
    (SETQ
      LAYTX
       (HAWS-GETLAYR "PROTXSUF")
      LAYXTX
       (HAWS-GETLAYR "PROXTXSUF")
    ) ;_ end of SETQ
    (IF (WCMATCH PROTYP "X*")
      (SETQ
        PROTYP
         (SUBSTR PROTYP 2)
        LAYTX
         LAYXTX
        PEXIST
         T
      ) ;_ end of SETQ
      (SETQ PEXIST NIL)
    ) ;_ end of IF
    (SETQ LAYTX (SUBST (STRCAT (CAR LAYLIN) (CAR LAYTX)) (CAR LAYTX) LAYTX))
    (IF (NOT (TBLSEARCH "LAYER" (CAR LAYLIN)))
      (HAWS-MKLAYR LAYLIN)
    ) ;_ end of IF
    (IF (NOT (TBLSEARCH "LAYER" (CAR LAYTX)))
      (HAWS-MKLAYR LAYTX)
    ) ;_ end of IF
    (SETQ
      LAYLIN
       (CAR LAYLIN)
      LAYTX
       (CAR LAYTX)
      LAYPRE
       (CAR (HAWS-GETLAYR "PROPRE"))
      LAYTXS
       (CAR (HAWS-GETLAYR "PROTXSUF"))
    ) ;_ end of SETQ
  ) ;_ end of DEFUN
  ;;Get the type of profile and set up layers.
  (DEFUN
     GETTYP
           (/ PIPSLD)
    (SETQ
      WALTHK
       0
      PROTYP
       ""
    ) ;_ end of SETQ
    (INITGET 1 "Mesa Gutter PAv PIpe Line POints")
    (SETQ
      PROHT
       (GETKWORD
         "\nMesa/Gutter/PAvement/PIpe/Line only/POints only:"
       ) ;_ end of getkword
    ) ;_ end of setq
    (SETQ
      PROHT
       (COND
         ((= PROHT "Mesa")
          (SETQ PROTYP "MESA")
          (* (/ (GETREAL "\nCurb height (in.): ") 12) HVEXAG *HAWS-ELVSCL*)
         )
         ((= PROHT "Gutter")
          (* (/ (GETREAL "\nCurb height (in.): ") 12) HVEXAG *HAWS-ELVSCL*)
         )
         ((= PROHT "PAv") 0)
         ((= PROHT "PIpe")
          (SETQ
            PROTYP
             (STRCASE
               (PROGN
                 (INITGET 1 "Wat Sew SD Irr Gas")
                 (GETKWORD "\nWat/Sew/SD/Irr/Gas: ")
               ) ;_ end of progn
             ) ;_ end of strcase
          ) ;_ end of setq
          (SETQ PROHT (* (/ (GETREAL "\nPipe ID (in): ") -12) HVEXAG *HAWS-ELVSCL*))
          (COND
            ((SETQ PIPSLD (FINDFILE "pipetabl.sld"))
             (SETVAR "filedia" 0)
             (COMMAND "._vslide" PIPSLD)
            )
          ) ;_ end of cond
          (SETQ
            WALTHK
             (* (/ (GETREAL "\nWall thickness (in): ") -12)
                HVEXAG
                *HAWS-ELVSCL*
             ) ;_ end of *
          ) ;_ end of setq
          (REDRAW)
          PROHT
         )
         ((= PROHT "Line") -10000)
         ((= PROHT "POints") 1000)
       ) ;_ end of COND
    ) ;_ end of SETQ
    (INITGET 1 "Exist Prop")
    (IF (= (GETKWORD "Exist/Prop: ") "Exist")
      (SETQ PROTYP (STRCAT "X" PROTYP))
    ) ;_ end of IF
    (IF F2
      (WRITE-LINE
        (STRCAT
          "PROTYP    "
          (MKFLD
            (RTOS
              (IF (< -10000 PROHT 1000)
                (* (/ PROHT HVEXAG *HAWS-ELVSCL*) 12.0)
                PROHT
              ) ;_ end of IF
              2
              6
            ) ;_ end of RTOS
          ) ;_ end of mkfld
          (MKFLD (RTOS (* (/ WALTHK HVEXAG *HAWS-ELVSCL*) 12) 2 6))
          PROTYP
        ) ;_ end of STRCAT
        F2
      ) ;_ end of WRITE-LINE
    ) ;_ end of IF
    (SETLAY)
  ) ;_ end of DEFUN
  ;;Get precisions for numbers.
  (DEFUN
     GETPREC
            ()
    (WHILE (PROGN
             (INITGET "STa Offset Elev SLope")
             (PROMPT
               "\nChange rounding precision for STa/Offset/Elev/SLope: "
             ) ;_ end of prompt
             (SETQ TEMP (GETKWORD))
           ) ;_ end of PROGN
      (PROMPT
        (STRCAT
          "\nNumber of digits to right of decimal place for "
          TEMP
          ": "
        ) ;_ end of strcat
      ) ;_ end of prompt
      (SET (READ (STRCAT (SUBSTR TEMP 1 2) "prec")) (GETINT))
    ) ;_ end of WHILE
    (SETVAR "luprec" STPREC)
  ) ;_ end of DEFUN
  ;;Increment elevations in a file
  (DEFUN
     INCFIL
           (/ INCSEW INCSD INCGEN)
    (SETQ
      INCSEW
       (GETREAL "\nIncrement for underground sewer elevations: ")
      INCSD
       (GETREAL
         "\nIncrement for underground storm drain elevations: "
       ) ;_ end of getreal
      INCGEN
       (GETREAL "\nIncrement for all other elevations: ")
    ) ;_ end of SETQ
    (SETQ
      F1 (COND
           (PROINPUTFILE
            (OPEN (SETQ FNAME (STRCAT PROINPUTFILE ".pro")) "r")
           )
           ((= (GETVAR "filedia") 1)
            (OPEN
              (SETQ FNAME (GETFILED "File to increment" (HAWS-GETDNPATH) "pro" 0))
              "r"
            ) ;_ end of open
           )
           (T (CAR (GETFIL "File to increment" (HAWS-GETDNPATH) "r" "pro")))
         ) ;_ end of COND
    ) ;_ end of SETQ
    (WHILE (IF F1
             (HAWS-RDFILE)
           ) ;_ end of IF
      (IF (AND STA (/= STA "Setup"))
        ;;if it is a profile point
        (PROGN
          (PLTPRO)
          ;;plot the point
          (IF F2
            ;;If requested, write all
            (WRITE-LINE
              (STRCAT
                ;;the point information
                "PRFPNT    "
                ;;to a file (f2).
                (MKFLD (RTOS STA 2 3))
                (MKFLD
                  (IF OFFSET
                    (RTOS OFFSET 2 3)
                    ""
                  ) ;_ end of IF
                ) ;_ end of mkfld
                (MKFLD (RTOS ELEV1 2 3))
                (MKFLD
                  (IF SLOPE
                    (RTOS SLOPE 2 4)
                    ""
                  ) ;_ end of IF
                ) ;_ end of mkfld
                (MKFLD
                  (IF NUM2
                    (RTOS NUM2 2 3)
                    ""
                  ) ;_ end of IF
                ) ;_ end of mkfld
                (COND
                  (LBLTXT)
                  ("")
                ) ;_ end of COND
              ) ;_ end of STRCAT
              F2
            ) ;_ end of WRITE-LINE
          ) ;_ end of IF
          (SETQ STA NIL)
          ;;Reset profile point to nil.
        ) ;_ end of PROGN
        (IF (NOT F1)
          (HAWS-PROSET)
        ) ;_ end of IF
        ;;If not a point, do setup.
      ) ;_ end of IF
      (PRINC "\rReading line ")
      (PRINC (SETQ I (1+ I)))
    ) ;_ end of WHILE
  ) ;_ end of DEFUN
  ;;Get file options
  (DEFUN
     GETFOP
           (/ RDWRT FPRMPT FBIT DATFIL)
    (INITGET "Read Save Increment")
    (SETQ
      RDWRT
       (GETKWORD
         "\nRead from file/Save to file/Increment elevations in a file: "
       ) ;_ end of getkword
      F1 NIL
      F2 NIL
    ) ;_ end of SETQ
    (COND
      ((= RDWRT "Increment")
       (PRINC "\nSorry.  Incrementing not yet available.")
       ;;(incfil)
      )
      (T
       (INITGET "Create Append")
       (IF (= RDWRT "Read")
         (SETQ
           RDWRT
            "r"
           FPRMPT
            "File to read"
           FBIT
            0
           PROVER4
            NIL
         ) ;_ end of SETQ
         (SETQ
           RDWRT
            (IF (= (GETKWORD "\Create new file/Append existing file: ")
                   "Create"
                ) ;_ end of =
              "w"
              "a"
            ) ;_ end of IF
           FPRMPT
            "File to save input to"
           FBIT
            1
         ) ;_ end of SETQ
       ) ;_ end of IF
       (SETQ
         DATFIL
          (COND
            ((= (GETVAR "filedia") 1)
             (OPEN
               (SETQ FNAME (GETFILED FPRMPT (HAWS-GETDNPATH) "pro" FBIT))
               RDWRT
             ) ;_ end of open
            )
            (T (CAR (GETFIL FPRMPT (HAWS-GETDNPATH) RDWRT "pro")))
          ) ;_ end of COND
       ) ;_ end of SETQ
       (IF (= RDWRT "r")
         (SETQ F1 DATFIL)
         (PROGN (SETQ F2 DATFIL) (WRITE-LINE "COLWID 10" F2))
       ) ;_ end of IF
      )
    ) ;_ end of COND
  ) ;_ end of DEFUN
  ;;Prompt for station to plot
  (DEFUN
     HAWS-GETPNT
                ()
    (SETQ
      STA
       T
      ELEV1
       NIL
      LBLTXT
       ""
    ) ;_ end of SETQ
    (WHILE (AND STA (NOT ELEV1))
      (WHILE (PROGN
               (INITGET "Setup Label")
               (= (SETQ STA (GETREAL "\n<Station>/Label/Setup: ")) "Label")
             ) ;_ end of progn
        (SETQ LBLTXT (GETSTRING 1 "\nEnter label for this station: "))
      ) ;_ end of WHILE
      (IF (AND STA (/= STA "Setup"))
        (PROGN
          (WHILE (PROGN
                   (INITGET "Offset")
                   (= (SETQ ELEV1 (GETREAL "<Elevation>/Offset: ")) "Offset")
                 ) ;_ end of progn
            (SETQ OFFSET (GETREAL "\nOffset: "))
          ) ;_ end of WHILE
          (IF (AND
                ELEV1
                (> (ABS (- ELEV1 REFELV)) 100)
                (= (STRCASE
                     (GETSTRING "\nElev. diff. is over 100 feet.  Plot? Y/N: ")
                   ) ;_ end of strcase
                   "N"
                ) ;_ end of =
              ) ;_ end of AND
            (SETQ ELEV1 NIL)
          ) ;_ end of IF
        ) ;_ end of PROGN
        (SETQ ELEV1 T)
      ) ;_ end of IF
    ) ;_ end of WHILE
    ;;Return nil if not given station.
    STA
  ) ;_ end of DEFUN
  ;;Read and interpret the lines of the data file.
  ;;First six letters of each data file line are the line title.
  (DEFUN
     HAWS-RDFILE
                (/ LINERR RDLIN TITLE)
    (IF (NOT FM)
      (SETQ FM ",")
    ) ;_ end of IF
    (SETQ RDLIN (READ-LINE F1))
    (IF RDLIN
      (PROGN
        (SETQ TITLE (SUBSTR RDLIN 1 6))
        (COND
          ((= (SUBSTR RDLIN 1 2) "0,")
           (SETQ
             STA
              (haws-rdfld 2 RDLIN FM 0)
             OFFSET
              NIL
             ELEV1
              (haws-rdfld 4 RDLIN FM 0)
             SLOPE
              NIL
             LBLTXT
              ""
           ) ;_ end of SETQ
          )
          ((= TITLE "PRFPNT")
           (SETQ
             STA
              (haws-rdfld 2 RDLIN FM 0)
             OFFSET
              (haws-rdfld 3 RDLIN FM 0)
             ELEV1
              (haws-rdfld 4 RDLIN FM 0)
             SLOPE
              (haws-rdfld 5 RDLIN FM 0)
             NUM2
              (haws-rdfld 6 RDLIN FM 0)
             LBLTXT
              (haws-rdfld 7 RDLIN FM 3)
              ;;8 for version 4, 7 for version 5
           ) ;_ end of SETQ
           (IF (NOT STA)
             (SETQ LINERR T)
           ) ;_ end of IF
          )
          ((= TITLE "COLWID")
           ;;If format line,
           (IF ;;read field width (default ",")
               (ZEROP (SETQ FM (ATOI (SUBSTR RDLIN 8))))
             (SETQ FM (SUBSTR RDLIN 8))
           ) ;_ end of IF
           (IF (OR (NOT FM) (= "" FM))
             (SETQ LINERR T)
           ) ;_ end of IF
          )
          ((= TITLE "REFPNT")
           ;;If reference line,
           (SETQ
             ;;read ref. pt info.
             REFPTX
              (haws-rdfld 2 RDLIN FM 0)
             REFPTY
              (haws-rdfld 3 RDLIN FM 0)
             REFSTA
              (haws-rdfld 4 RDLIN FM 0)
             REFELV
              (haws-rdfld 5 RDLIN FM 0)
             STASCL
              (haws-rdfld 6 RDLIN FM 0)
             STASCL
              (IF STASCL
                STASCL
                1
              ) ;_ end of if
             *HAWS-ELVSCL*
              (haws-rdfld 7 RDLIN FM 0)
             *HAWS-ELVSCL*
              (IF *HAWS-ELVSCL*
                *HAWS-ELVSCL*
                1
              ) ;_ end of if
           ) ;_ end of SETQ
           (IF (NOT (AND REFPTX REFPTY REFSTA REFELV))
             (SETQ LINERR T)
           ) ;_ end of IF
          )
          ((= TITLE "HVEXAG")
           (SETQ HVEXAG (haws-rdfld 2 RDLIN FM 0))
           (IF (NOT HVEXAG)
             (SETQ LINERR T)
           ) ;_ end of IF
          )
          ((= TITLE "PROTYP")
           (SETQ
             PROHT
              (haws-rdfld 2 RDLIN FM 0)
             PROHT
              (IF (OR (= PROHT 1000) (<= PROHT -10000))
                PROHT
                (/ (* PROHT HVEXAG *HAWS-ELVSCL*) 12)
              ) ;_ end of IF
             WALTHK
              (haws-rdfld 3 RDLIN FM 0)
             WALTHK
              (COND
                (WALTHK (/ (* WALTHK HVEXAG *HAWS-ELVSCL*) 12))
                (0)
              ) ;_ end of COND
             PROTYP
              (STRCASE (haws-rdfld 4 RDLIN FM 1))
             SLPTYP
              (haws-rdfld 5 RDLIN FM 1)
           ) ;_ end of SETQ
           (IF (AND (< -10000 PROHT 0) (= PROTYP ""))
             (SETQ LINERR T)
             (SETLAY)
           ) ;_ end of IF
           (IF (AND (/= SLPTYP "") (NOT SLPRECIN))
             (SETQ SLPREC 5)
           ) ;_ end of if
          )
          ((= TITLE "PRECIS")
           (SETQ
             STPREC
              (FIX (haws-rdfld 2 RDLIN FM 0))
             OFPREC
              (FIX (haws-rdfld 3 RDLIN FM 0))
             ELPREC
              (FIX (haws-rdfld 4 RDLIN FM 0))
             SLPRECIN
              (FIX (haws-rdfld 5 RDLIN FM 0))
             SLPREC
              SLPRECIN
           ) ;_ end of SETQ
          )
          ((= TITLE "NEWPRF")
           (SETQ
             PLTPT1
              NIL
             TOPPT
              NIL
           ) ;_ end of setq
          )
        ) ;_ end of COND
        (IF LINERR
          (PRINC (STRCAT ".  Input error found:\n" RDLIN "\n"))
        ) ;_ end of IF
        T
      ) ;_ end of PROGN
      NIL
    ) ;_ end of IF
  ) ;_ end of DEFUN
  ;;Plot profile given sta, elev1, and offset.
  (DEFUN
     HAWS-PLTHARDPRO
                    ()
    (SETQ
      ;;Extract plotting instructions then actual elevation from input
      INVMIR
       (COND
         ((AND
            ELEV1
            (/= (DISTOF (RTOS ELEV1 2 2)) (DISTOF (RTOS ELEV1 2 3)))
          ) ;_ end of and
          (> 0.35 (- (* 100 ELEV1) (FIX (* 100 ELEV1))) 0.15)
         )
         ((AND PLTPT1A (WCMATCH (STRCASE LBLTXT) "* SHEET *,")))
       ) ;_ end of COND
      INVUP
       (COND
         ((AND
            ELEV1
            (/= (DISTOF (RTOS ELEV1 2 2)) (DISTOF (RTOS ELEV1 2 3)))
          ) ;_ end of and
          (> 0.25 (- (* 100 ELEV1) (FIX (* 100 ELEV1))) 0.05)
         )
       ) ;_ end of COND
      ELEV1
       (IF (AND
             (= PROHT 1000)
             (SETQ TEMP (HAWS-DISTOFX (STRCASE LBLTXT) "*TOP*=*,*BOT*=*,*INV*=*" 0))
           ) ;_ end of and
         TEMP
         ;;elevation from ellipse label for ellipses
         ELEV1
         ;;elevation from elevation field for everything else
       ) ;_ end of IF
      PLTELV
       (NOT (MINUSP ELEV1))
      ELEV1
       (/ (FIX (* (+ (ABS ELEV1) 0.0001) 100.0)) 100.0)
      ;;Extract plotting instructions and actual station from input
      PLTSTA
       (AND
         (/= PROTYP "MESA")
         (/= PROHT 1000)
         (= (DISTOF (RTOS STA 2 2)) (DISTOF (RTOS STA 2 3)))
       ) ;_ end of and
      STA
       (ATOF (RTOS STA 2 2))
      ;;Translate station and elevation to a world coordinate point
      ACADX
       (+ REFPTX (* STASCL (- STA REFSTA)))
      ACADY1
       (+ REFPTY (* HVEXAG *HAWS-ELVSCL* (- ELEV1 REFELV)))
      ;;Extract mh rim, cb second elevation, or other y2 from input
      ACADY2
       (IF NUM2
         (+ REFPTY (* HVEXAG *HAWS-ELVSCL* (- NUM2 REFELV)))
       ) ;_ end of IF
      LBLNUM2
       (COND
         ((WCMATCH (STRCASE LBLTXT) "*RIM=#*")
          (HAWS-ATOFX LBLTXT "*RIM=*" 0)
         )                              ;Manhole rim
         ((WCMATCH (STRCASE LBLTXT) "*TC=*")
          (HAWS-ATOFX LBLTXT "*TC=*" 0)
         )                              ;Inlet TC
         ((WCMATCH (STRCASE LBLTXT) "*GRATE=*")
          (+ 0.67 (HAWS-ATOFX LBLTXT "*GRATE=*" 0))
         )                              ;Grate TC
         ((= PROHT 1000)
          (HAWS-ATOFX (STRCASE LBLTXT) "*\"*,* IN*" 1) ;Ellipse diameter
         )
       ) ;_ end of COND
      LBLACADY2
       (IF LBLNUM2
         (+ REFPTY (* HVEXAG *HAWS-ELVSCL* (- LBLNUM2 REFELV)))
       ) ;_ end of IF
      MHRIM
       (IF (WCMATCH (STRCASE LBLTXT) "*RIM=#*")
         (LIST
           ACADX
           (COND
             (ACADY2)
             (LBLACADY2)
           ) ;_ end of COND
           0.0
         ) ;_ end of LIST
       ) ;_ end of IF
      CBTC
       (IF (AND
             (WCMATCH (STRCASE LBLTXT) "*CB*,*INLET*,*BASIN*")
             (OR ACADY2 LBLACADY2)
           ) ;_ end of and
         (LIST
           ACADX
           (COND
             (ACADY2)
             (LBLACADY2)
           ) ;_ end of COND
           0.0
         ) ;_ end of LIST
       ) ;_ end of IF
      ;;Set acad plot points
      PLTPT1
       (LIST ACADX ACADY1 0.0)
      PLTPT2
       (IF (OR NUM2 LBLNUM2)
         (LIST
           ACADX
           (COND
             (ACADY2)
             (LBLACADY2)
           ) ;_ end of COND
           0.0
         ) ;_ end of LIST
       ) ;_ end of IF
    ) ;_ end of setq
    ;;Set points for secondary lines depending on profile type
    (COND
      ((< -10000 PROHT 1000)
       (SETQ
         TOPPT
          (POLAR
            PLTPT1
            (/ PI 2)
            (IF (= PROHT -10000)
              0
              (ABS (+ PROHT WALTHK))
            ) ;_ end of IF
          ) ;_ end of POLAR
         BOTPT
          (POLAR PLTPT1 (* PI 0.5) WALTHK)
       ) ;_ end of setq
      )
    ) ;_ end of COND
  ) ;_ end of defun
  ;;Plot profile given sta, elev1, and offset.
  (DEFUN
     HAWS-PLTSOFTPRO
                    (/ CNFFREEPT CNFTESTPT I J K LBLCNF LDRCNF LNGSTR LDRCNFPT)
    (SETQ
      ;;Make label strings
      ;;Wrap label string as requested by user with semi-colons.
      LBLLST
       (HAWS-WRAP LBLTXT 3 ";")
      ;;Find longest string to be plotted.
      LNGSTR
       (CAR LBLLST)
      LNGSTR
       (FOREACH
          STRINGI
                 LBLLST
         (IF (> (STRLEN STRINGI) (STRLEN LNGSTR))
           (SETQ LNGSTR STRINGI)
           LNGSTR
         ) ;_ end of IF
       ) ;_ end of FOREACH
      ;;Translate elev1 into text.
      EL1DSC
       (COND
         ((= PROTYP "MESA") "FL ")
         ((< 0 PROHT)
          (IF (/= "" PROTYP)
            (STRCAT PROTYP "=")
            "G="
          ) ;_ end of IF
         )
         ((= 0 PROHT)
          (IF (/= "" PROTYP)
            (STRCAT PROTYP "=")
            "P="
          ) ;_ end of IF
         )
         ((> 0 PROHT) "INV=")
       ) ;_ end of COND
      EL1TXT
       (STRCAT EL1DSC (RTOS ELEV1 2 ELPREC))
    ) ;_ end of SETQ
    ;;Setq points for upright labels
    (COND
      ((< -10000 PROHT 1000)
       (SETQ
         ;;Translate station real number into station string
         STATXT
          (STRCAT "STA " (HAWS-RTOSTA STA STPREC))
         ;;Insert offset into station string if given
         OFFTXT
          (IF OFFSET
            (STRCAT
;;;O							(if	pltsta
;;;O								(strcat statxt ", ")
;;;O								""
;;;O							) ;_ end of IF
              (RTOS (ABS OFFSET) 2 OFPREC)
              (IF (MINUSP OFFSET)
                "' LT"
                "' RT"
              ) ;_ end of IF
            ) ;_ end of STRCAT
            ""
          ) ;_ end of IF
         ;;Set crown (top of curb) elevation, translate to text, and get length.
         TOPTX
          (+ ELEV1 (/ (ABS PROHT) HVEXAG *HAWS-ELVSCL*))
         TOPTX
          (COND ((< 0 PROHT 1000) (STRCAT "TC=" (RTOS TOPTX 2 ELPREC))))
         LNGSTR
          (IF (AND PLTSTA (< (STRLEN LNGSTR) (STRLEN STATXT)))
            STATXT
            LNGSTR
          ) ;_ end of IF
         LNGSTR
          (IF (< (STRLEN LNGSTR) (STRLEN OFFTXT))
            OFFTXT
            LNGSTR
          ) ;_ end of IF
         LNGSTR
          (IF (< (STRLEN LNGSTR) (STRLEN EL1TXT))
            EL1TXT
            LNGSTR
          ) ;_ end of IF
         TXLEN
          (+ (HAWS-TXLEN lngstr ts) 0)
         ;;Set points for text and label lines
         LBLPT1
          (POLAR
            (COND
              (PLTPT2)
              (TOPPT)
            ) ;_ end of COND
            (/ PI 2)
            (IF MHRIM
              (* 0.05 HVEXAG *HAWS-ELVSCL*)
              0
            ) ;_ end of IF
          ) ;_ end of POLAR
         LBLPT2
          (POLAR LBLPT1 (/ PI 2) (* 2.0 TS))
         CNFTESTPT
          LBLPT2
         CNFFREEPT
          '(0.0 0.0 0.0)
         LDRCNFPT
          '(0.0 0.0 0.0)
         I 0
         J 0
         K 0
       ) ;_ end of SETQ
       ;; Find the nearest of up to twenty conflict-free label positions or one conflict
       ;; free straight up
       ;; position.
       (WHILE (AND (< I 100) (< J 20) (< K 1))
         (SETQ
           LBLCNF
            (SSGET
              "C"
              (POLAR
                CNFTESTPT
                PI
                (* TS
                   (+ (* LINGAP 0.9)
                      (* (1+ LINGAP)
                         (+ (IF PLTSTA
                              1
                              0
                            ) ;_ end of IF
                            (IF (= OFFTXT "")
                              0
                              1
                            ) ;_ end of if
                            (IF (= (CAR LBLLST) "")
                              0
                              (LENGTH LBLLST)
                            ) ;_ end of IF
                         ) ;_ end of +
                      ) ;_ end of *
                   ) ;_ end of +
                ) ;_ end of *
              ) ;_ end of POLAR
              (POLAR
                (POLAR
                  CNFTESTPT
                  0
                  (* TS
                     (+ LINGAP
                        (* (1+ LINGAP)
                           (IF (< PROHT 0)
                             0
                             (IF PLTPT2
                               2
                               1
                             ) ;_ end of IF
                           ) ;_ end of IF
                        ) ;_ end of *
                     ) ;_ end of +
                  ) ;_ end of *
                ) ;_ end of POLAR
                (/ PI 2)
                TXLEN
              ) ;_ end of POLAR
              (LIST (CONS 8 (STRCAT LAYPRE "*")))
            ) ;_ end of SSGET
         ) ;_ end of SETQ
         (SETQ
           LDRCNF
            (SSGET
              "F"
              (LIST
                (POLAR LBLPT1 (ANGLE LBLPT1 CNFTESTPT) (* TS 1))
                CNFTESTPT
              ) ;_ end of list
              (LIST (CONS 8 (STRCAT LAYPRE "*" LAYTXS)))
            ) ;_ end of SSGET
         ) ;_ end of SETQ
         ;;Save nearest point that has no label conflict.
         (IF (AND
               (NOT LBLCNF)
               (< (DISTANCE LBLPT2 CNFTESTPT) (DISTANCE LBLPT2 LDRCNFPT))
             ) ;_ end of and
           (SETQ LDRCNFPT CNFTESTPT)
         ) ;_ end of IF
         ;;Save nearest point that has no conflict at all.
         (IF (NOT (OR LBLCNF LDRCNF))
           (PROGN
             (SETQ J (1+ J))
             (IF (< (DISTANCE LBLPT2 CNFTESTPT) (DISTANCE LBLPT2 CNFFREEPT))
               (SETQ CNFFREEPT CNFTESTPT)
             ) ;_ end of IF
           ) ;_ end of PROGN
         ) ;_ end of IF
         ;;Save first straight up point that has no label conflict and quit looking.
         (IF (AND (NOT LBLCNF) (EQUAL (CAR LBLPT2) (CAR CNFTESTPT) 0.01))
           (PROGN (SETQ CNFFREEPT CNFTESTPT) (SETQ K (1+ K)))
         ) ;_ end of IF
         (SETQ
           I (1+ I)
           CNFTESTPT
            (POLAR
              (POLAR LBLPT2 (* PI 0.5) (* TS (FIX (* I 0.05))))
              ;;Move up every 20 (1/0.05) tries.
              (* PI I)
              ;;Switch side to side
              (* TS (1+ LINGAP) (REM (/ (1+ I) 2.0) 20))
              ;;Increase horiz distance.
            ) ;_ end of POLAR
         ) ;_ end of SETQ
       ) ;_ end of WHILE
       (SETQ
         LBLPT2
          (COND
            ;;First use any straight up or extra close position, even with ldr conflict.
            ((OR (= K 1)
                 (AND
                   LDRCNFPT
                   (> (- (DISTANCE LBLPT2 CNFFREEPT) (DISTANCE LBLPT2 LDRCNFPT))
                      (* TS 4.0)
                   ) ;_ end of >
                 ) ;_ end of and
             ) ;_ end of OR
             LDRCNFPT
            )
            ;;Next choice, use totally free position.
            ((> J 0) CNFFREEPT)
            ;;Last choice, use the original location.
            (T LBLPT2)
          ) ;_ end of COND
         LBLPT3
          (POLAR LBLPT2 (/ PI 2) TXLEN)
         ;;Top end of label line
         DIAPT1
          (POLAR LBLPT3 (/ PI 2) (* TS 2))
         DIAPT2
          (POLAR DIAPT1 (/ PI -4) (* TS (SQRT 2)))
         DIAPT3
          (POLAR DIAPT1 (* 5 (/ PI 4)) (* TS (SQRT 2)))
         BRKPT
          (IF (AND (OR MHRIM CBTC) ACADY2 LBLACADY2 (/= ACADY2 LBLACADY2))
            (POLAR
              TOPPT
              (/ PI 2)
              (/ (DISTANCE
                   TOPPT
                   (COND
                     (MHRIM)
                     (CBTC)
                   ) ;_ end of COND
                 ) ;_ end of DISTANCE
                 2
              ) ;_ end of /
            ) ;_ end of POLAR
          ) ;_ end of IF
         TXPT1
          (POLAR LBLPT2 0 (* TS -1 LINGAP))
         ;;1 line above line
         TXPT4
          (POLAR LBLPT2 0 (* TS LINGAP))
         ;;1 line below line
         TXPT5
          (POLAR LBLPT2 0 (* TS (+ 1 (* 2 LINGAP))))
          ;;2 lines below line
       ) ;_ end of SETQ
      )
    ) ;_ end of COND
    ;;Setq points for horizontal labels on leaders
    (COND
      ((OR (= PROHT 1000) (< -10000 PROHT 0))
       (SETQ
         I 0
         J 0
       ) ;_ end of setq
       ;; Find the first conflict free label position in user selected direction.
       (WHILE (AND (< I 15) (< J 1))
         (SETQ
           TXPT6
            (POLAR
              PLTPT1
              (* PI
                 (IF INVUP
                   1
                   -1
                 ) ;_ end of IF
                 (IF INVMIR
                   0.75
                   0.25
                 ) ;_ end of IF
              ) ;_ end of *
              (+ (* TS (+ 3.0 I)) (* WALTHK -1.717))
            ) ;_ end of POLAR
           ;;Inv angle point
           LNGSTR
            (IF (AND (= PROHT 1000) (> (STRLEN LBLTXT) 0))
              LNGSTR
              EL1TXT
            ) ;_ end of IF
           TXLEN
            (+ (HAWS-TXLEN LNGSTR TS) 0)
           TXPT7
            (POLAR
              TXPT6
              (IF INVMIR
                PI
                0
              ) ;_ end of IF
              TXLEN
            ) ;_ end of POLAR
           TXPT8
            (POLAR TXPT6 (* PI 0.5) (* TS 0.5 LINGAP))
            ;;Invert text point
            ;;No 9 or 10
         ) ;_ end of SETQ
         (SETQ
           LBLCNF
            (SSGET
              "C"
              TXPT6
              (POLAR TXPT7 (/ PI 2) (* TS (+ 1 (* 2 LINGAP)))) ;_ end of POLAR
              (LIST (CONS 8 (STRCAT LAYPRE "*")))
            ) ;_ end of SSGET
         ) ;_ end of SETQ
         ;;Flag success for first point that has no label conflict.
         (IF (NOT LBLCNF)
           (SETQ J (1+ J))
         ) ;_ end of IF
         (SETQ I (1+ I))
       ) ;_ end of WHILE
      )
    ) ;_ end of COND
    ;;Set points for "MESA" profile.
    (COND
      ((= PROTYP "MESA")
       (SETQ
         TXPT11
          (POLAR TOPPT (* PI 0.25) (* 3.08 TS))
         ;;Slant label line text point
         TXPT12
          (POLAR TXPT11 (* PI 0.25) TXLEN)
         ;;Slant label line end
         TXPT13
          (POLAR TXPT11 (* PI 0.75) (* TS LINGAP))
         ;;Slant text point above line
         TXPT14
          (POLAR TXPT11 (* PI 0.75) (* TS -1 LINGAP))
          ;;Slant text point below line
       ) ;_ end of SETQ
      )
    ) ;_ end of COND
    (COND
      ((AND (< -10000 PROHT 0) (OR PLTSTA OFFSET (/= LBLTXT "")))
       (HAWS-PLTLBL1)
      )
      ((AND
         (<= 0 PROHT 999.9)
         (OR PLTSTA OFFSET PLTELV (/= LBLTXT ""))
       ) ;_ end of and
       (HAWS-PLTLBL1)
      )
      ((= PROHT 1000) (HAWS-PLTPNT))
      ((= PROHT -10000)
       (IF (NOT SLOPE)
         (SETQ SLOPE 1000)
       ) ;_ end of IF
      )
      ((= PROHT -20000)
       (IF (NOT SLOPE)
         (SETQ SLOPE 1000)
       ) ;_ end of IF
      )
    ) ;_ end of COND
    (COND ((AND (< -10000 PROHT 0) PLTELV) (HAWS-PLTLBL2)))
    (SETQ OFFSET NIL)
  ) ;_ end of DEFUN
  ;;Plot a vertical upward profile point label.
  (DEFUN
     HAWS-PLTLBL1
                 (/ I)
    (HAWS-MKLAYR (LIST LAYTX "" ""))
    (COND
      ((= PROTYP "MESA")
       ;;Slanted label
       (HAWS-MKLINE TOPPT TXPT12)
       (HAWS-MKTEXT NIL TXPT13 TS (* PI 0.25) LBLTXT)
       (HAWS-MKTEXT "TL" TXPT14 TS (* PI 0.25) EL1TXT)
      )
      (T
       ;;Vertical label
       (HAWS-MKLINE BOTPT LBLPT1)
       (HAWS-MKLINE LBLPT1 LBLPT2)
       (HAWS-MKLINE LBLPT2 LBLPT3)
       (COND
         ((/= "" OFFTXT)
          (HAWS-MKTEXT NIL TXPT1 TS (/ PI 2) OFFTXT)
          (SETQ TXPT1 (POLAR TXPT1 0 (* TS -1 (+ 1 LINGAP))))
         )
       ) ;_ end of cond
       (COND
         (PLTSTA (HAWS-MKTEXT NIL TXPT1 TS (/ PI 2) STATXT))
         (T (SETQ TXPT1 (POLAR TXPT1 0 (* TS (+ 1 LINGAP)))))
       ) ;_ end of COND
       (SETQ I -1)
       (WHILE (SETQ TEMP (NTH (SETQ I (1+ I)) (REVERSE LBLLST)))
         (HAWS-MKTEXT
           NIL
           (SETQ TXPT1 (POLAR TXPT1 0 (* TS -1 (+ 1 LINGAP))))
           TS
           (/ PI 2)
           TEMP
         ) ;_ end of HAWS-MKTEXT
       ) ;_ end of WHILE
       ;;Plot elevation text for curbs.
       (COND
         ((AND (<= 0 PROHT) PLTELV)
          (HAWS-MKTEXT
            "tl"
            TXPT4
            TS
            (/ PI 2)
            (COND
              (TOPTX)
              (EL1TXT)
            ) ;_ end of COND
          ) ;_ end of mktext
          (IF TOPTX
            (HAWS-MKTEXT "tl" TXPT5 TS (/ PI 2) EL1TXT)
          ) ;_ end of IF
         )
       ) ;_ end of COND
       ;;Plot diamond
       (COND
         ((AND
            (>= PROHT 0)
            (WCMATCH (STRCASE LBLTXT) "*GC*,*G`.C`.*,*DE CHANGE*")
          ) ;_ end of and
          (COMMAND "._pline" LBLPT3 DIAPT2 DIAPT1 DIAPT3 "c")
         )
         ((AND
            (>= PROHT 0)
            (WCMATCH (STRCASE LBLTXT) "*GB*,*G`.C`.*,*DE BREAK*")
          ) ;_ end of and
          (COMMAND "._solid" DIAPT1 DIAPT2 DIAPT3 LBLPT3 "")
         )
       ) ;_ end of COND
       ;;Plot manhole
       (COND
         ((< PROHT 0)
          (IF (NOT F1)
            (HAWS-MKTEXT "tl" TXPT4 TS (/ PI 2) "RIM=")
          ) ;_ end of IF
          (COND
            (MHRIM
             (IF PEXIST
               (HAWS-MKLAYR "PROXMH")
               (HAWS-MKLAYR "PROMH")
             ) ;_ end of IF
             (HAWS-DRAWMH BOTPT MHRIM BRKPT)
            )
            (CBTC
             (IF PEXIST
               (HAWS-MKLAYR "PPROXCB")
               (HAWS-MKLAYR "PROCB")
             ) ;_ end of IF
             (HAWS-DRAWCB
               BOTPT
               CBTC
               (IF PLTPT1A
                 "R"
                 "L"
               ) ;_ end of IF
               2
               BRKPT
             ) ;_ end of drawcb
            )
          ) ;_ end of COND
         )
       ) ;_ end of COND
      )
    ) ;_ end of COND
  ) ;_ end of DEFUN
  ;;Plot a downward slant elevation tail.
  (DEFUN
     HAWS-PLTLBL2
                 (/ I)
    (HAWS-MKLAYR (LIST LAYTX "" ""))
    (HAWS-MKLINE PLTPT1 TXPT6)
    (HAWS-MKLINE TXPT6 TXPT7)
    (HAWS-MKTEXT
      (IF INVMIR
        "BR"
        "BL"
      ) ;_ end of IF
      TXPT8
      TS
      0
      EL1TXT
    ) ;_ end of mktext
  ) ;_ end of DEFUN
  ;;Plot a circle or ellipse
  (DEFUN
     HAWS-PLTPNT
                (/ I XCENTER XINVERT XDIAM XWALTHK XCROWN XBOTPT XTOPPT XINRAD XOUTRAD TXPT1 TXPT2 Y
                )
    (COND
      ((AND (= LBLTXT "") (NOT NUM2))
       (PROMPT
         "Point circles are on DEFPOINTS layer. Will not plot."
       ) ;_ end of prompt
       (HAWS-MKLAYR (LIST "defpoints" "" ""))
       (ENTMAKE
         (LIST
           (CONS 0 "CIRCLE")
           (APPEND '(10) (TRANS PLTPT1 1 0))
           (CONS 40 TS)
         ) ;_ end of list
       ) ;_ end of entmake
      )
      (T
       (FOREACH
          X
           (QUOTE
             ((XBOTPT "*BOT*=*") (XTOPPT "*TOP*=*") (XINVERT "*INV*=*"))
           ) ;_ end of quote
         (IF (SETQ Y (HAWS-DISTOFX (STRCASE LBLTXT) (CADR X) 0))
           (SET (CAR X) Y)
         ) ;_ end of IF
       ) ;_ end of FOREACH
       (SETQ
         XDIAM
          (/ (COND
               (NUM2)
               (LBLNUM2)
             ) ;_ end of COND
             12
          ) ;_ end of /
         XWALTHK
          (IF SLOPE
            (/ SLOPE 12)
            (IF WALTHK
              WALTHK
              0
            ) ;_ end of IF
          ) ;_ end of IF
         XINVERT
          (LIST
            (CAR PLTPT1)
            (+ REFPTY
               (* HVEXAG
                  *HAWS-ELVSCL*
                  (- (COND
                       (XINVERT XINVERT)
                       (XTOPPT (- XTOPPT XDIAM XWALTHK))
                       (XBOTPT (+ XBOTPT XWALTHK))
                       (T ELEV1)
                     ) ;_ end of COND
                     REFELV
                  ) ;_ end of -
               ) ;_ end of *
            ) ;_ end of +
            0.0
          ) ;_ end of LIST
         XCROWN
          (POLAR XINVERT (/ PI 2.0) (* XDIAM HVEXAG *HAWS-ELVSCL*))
         XBOTPT
          (POLAR
            XINVERT
            (/ PI -2.0)
            (* XWALTHK
               HVEXAG
               *HAWS-ELVSCL*
            ) ;_ end of *
          ) ;_ end of POLAR
         XTOPPT
          (POLAR XCROWN (/ PI 2.0) (* XWALTHK HVEXAG *HAWS-ELVSCL*))
         XINRAD
          (/ XDIAM 2.0)
         XOUTRAD
          (+ (/ XDIAM 2.0) XWALTHK)
         XCENTER
          (POLAR XINVERT (/ PI 2.0) (* XINRAD HVEXAG *HAWS-ELVSCL*))
       ) ;_ end of SETQ
       (HAWS-MKLAYR (LIST LAYLIN "" ""))
       (ENTMAKE
         (LIST
           '(0 . "ELLIPSE")
           '(100 . "AcDbEntity")
           '(100 . "AcDbEllipse")
           (CONS 10 (TRANS XCENTER 1 0))
           '(41 . 0.0)
           (CONS
             11
             (MAPCAR
               '(LAMBDA (UCSYDIR) (* UCSYDIR XINRAD HVEXAG *HAWS-ELVSCL*))
               (GETVAR "ucsydir")
             ) ;_ end of mapcar
           ) ;_ end of CONS
           (CONS 40 (/ 1.0 (* HVEXAG *HAWS-ELVSCL*)))
           (CONS 42 (* 2 PI))
         ) ;_ end of list
       ) ;_ end of entmake
       (IF (/= 0 XWALTHK)
         (ENTMAKE
           (LIST
             '(0 . "ELLIPSE")
             '(100 . "AcDbEntity")
             '(100 . "AcDbEllipse")
             (CONS 10 (TRANS XCENTER 1 0))
             '(41 . 0.0)
             (CONS
               11
               (MAPCAR
                 '(LAMBDA (UCSYDIR) (* UCSYDIR XOUTRAD HVEXAG *HAWS-ELVSCL*))
                 (GETVAR "ucsydir")
               ) ;_ end of mapcar
             ) ;_ end of CONS
             (CONS 40 (/ 1.0 (* HVEXAG *HAWS-ELVSCL*)))
             (CONS 42 (* 2 PI))
           ) ;_ end of list
         ) ;_ end of entmake
       ) ;_ end of IF
       (COND
         ((/= LBLTXT "")
          (HAWS-MKLAYR (LIST LAYTX "" ""))
          (HAWS-MKLINE PLTPT1 TXPT6)
          (HAWS-MKLINE TXPT6 TXPT7)
          (SETQ I -1)
          (WHILE (SETQ TEMP (NTH (SETQ I (1+ I)) LBLLST))
            (HAWS-MKTEXT
              (IF INVMIR
                "R"
                NIL
              ) ;_ end of IF
              TXPT8
              TS
              0
              TEMP
            ) ;_ end of mktext
            (SETQ TXPT8 (POLAR TXPT8 (/ PI 2) (* TS -1 (+ 1 LINGAP))))
          ) ;_ end of WHILE
         )
       ) ;_ end of COND
      )
    ) ;_ end of COND
  ) ;_ end of DEFUN
  ;;Plot a line between two points at a vertical offset
  (DEFUN
     HAWS-PLTLIN
                (PT1 PT2 OFFSET)
    (HAWS-MKLINE
      (POLAR PT1 (/ PI 2) OFFSET)
      (POLAR PT2 (/ PI 2) OFFSET)
    ) ;_ end of HAWS-MKLINE
  ) ;_ end of defun
  ;;Connect two profile points with lines
  (DEFUN
     HAWS-CONNECTHARD
                     ()
    (HAWS-MKLAYR (LIST LAYLIN "" ""))
    (COND
      ((/= -20000 PROHT) (HAWS-MKLINE PLTPT1A PLTPT1))
      ((= PROHT -20000)
       (COMMAND
         "._dim1"
         "al"
         PLTPT1A
         PLTPT1
         PLTPT1
         (IF (= LBLTXT "")
           " "
           LBLTXT
         ) ;_ end of IF
       ) ;_ end of COMMAND
      )
    ) ;_ end of COND
    (COND
      ((AND (< 0 PROHT 1000) TOPPTA) (HAWS-MKLINE TOPPTA TOPPT))
      ((AND (= 0 PROHT) TOPPTA (NOT (EQUAL TOPPTA PLTPT1A)))
       (HAWS-MKLINE TOPPTA TOPPT)
      )
      ((< -10000 PROHT 0)
       (HAWS-PLTLIN PLTPT1A PLTPT1 (ABS PROHT))
      )
    ) ;_ end of COND
    (COND
      ((< WALTHK 0)
       (HAWS-PLTLIN PLTPT1A PLTPT1 WALTHK)
       (HAWS-PLTLIN PLTPT1A PLTPT1 (- 0 WALTHK PROHT))
      )
    ) ;_ end of cond
  ) ;_ end of defun
  ;;Connect two profile points with slope text, etc.
  (DEFUN
     HAWS-CONNECTSOFT
                     (/ SLOPESTRING PT1 GUTLIP)
    ;;Calculate slope
    (COND
      ((NOT SLOPE)
       (SETQ
         SLOPE
          (* (/ (- (CADR PLTPT1) (CADR PLTPT1A))
                (- (CAR PLTPT1) (CAR PLTPT1A))
                STASCL
             ) ;_ end of /
             (/ 100.0 HVEXAG *HAWS-ELVSCL*)
          ) ;_ end of *
       ) ;_ end of setq
      )
      ((= SLOPE 220.1)
       (SETQ
         GUTLIP
          (IF (< (CADR PLTPT1) (CADR PLTPT1A))
            -0.083
            0.083
          ) ;_ end of IF
       ) ;_ end of SETQ
       (SETQ
         SLOPE
          (* (/ (- (CADR PLTPT1) (CADR PLTPT1A) (* HVEXAG *HAWS-ELVSCL* GUTLIP))
                (MAX (- (CAR PLTPT1) (CAR PLTPT1A) 1.5) 0.001)
                STASCL
             ) ;_ end of /
             (/ 100.0 HVEXAG *HAWS-ELVSCL*)
          ) ;_ end of *
       ) ;_ end of SETQ
      )
    ) ;_ end of COND
    (IF (AND
          (NOT F1)
          (/= PROHT -10000)
          (SETQ
            TEMP
             (GETREAL
               (STRCAT
                 "\nSlope (%)<"
                 (RTOS SLOPE 2 SLPREC)
                 "%>(1000 for no slope): "
               ) ;_ end of strcat
             ) ;_ end of getreal
          ) ;_ end of setq
        ) ;_ end of AND
      (SETQ SLOPE TEMP)
    ) ;_ end of IF
    (HAWS-MKLAYR (LIST LAYTX "" ""))
    ;;Build slope string
    (SETQ
      SLOPE
       (STRCAT
         (IF (MINUSP SLOPE)
           ""
           "+"
         ) ;_ end of IF
         (IF (= SLPTYP "")
           (STRCAT (RTOS SLOPE 2 SLPREC) "%")
           (STRCAT (RTOS (/ SLOPE 100) 2 SLPREC) " " SLPTYP "/" SLPTYP)
         ) ;_ end of if
       ) ;_ end of STRCAT
    ) ;_ end of SETQ
    (COND
      ((< PROHT 0)
       (SETQ
         SLOPESTRING
          (STRCAT
            (RTOS (ABS (- (CAR PLTPT1A) (CAR PLTPT1))) 2 0)
            " LF "
            (RTOS (ABS (* (/ PROHT HVEXAG *HAWS-ELVSCL*) 12)) 2 0)
            "\" "
            (COND
              ((= PROTYP "WAT") "WATER")
              ((= PROTYP "SEW") "SEWER")
              ((= PROTYP "SD") "SD")
              ((= PROTYP "IRR") "IRR")
              ((= PROTYP "GAS") "GAS")
              (T PROTYP)
            ) ;_ end of COND
            " @ S="
            SLOPE
          ) ;_ end of STRCAT
       ) ;_ end of SETQ
       ;;If pipe slope string too long, shorten
       (COND
         ((< (DISTANCE PLTPT1A PLTPT1)
             (HAWS-TXLEN SLOPESTRING TS)
          ) ;_ end of <
          (SETQ
            SLOPESTRING
             (STRCAT
               (RTOS (ABS (- (CAR PLTPT1A) (CAR PLTPT1))) 2 0)
               " LF "
               (RTOS (ABS (* (/ PROHT HVEXAG *HAWS-ELVSCL*) 12)) 2 0)
               "\", "
               SLOPE
             ) ;_ end of STRCAT
          ) ;_ end of SETQ
         )
       ) ;_ end of COND
      )
      (T (SETQ SLOPESTRING (STRCAT "S=" SLOPE)))
    ) ;_ end of COND
    ;;Position text at middle point
    (SETQ
      PT1
       (POLAR
         (LIST
           (/ (+ (CAR PLTPT1A) (CAR PLTPT1)) 2)
           (/ (+ (CADR PLTPT1A) (CADR PLTPT1)) 2)
         ) ;_ end of list
         (/ PI 2)
         (IF (= PROHT -10000)
           0
           (/ (ABS PROHT) 2.0)
         ) ;_ end of IF
       ) ;_ end of POLAR
    ) ;_ end of SETQ
    ;;Adjust text position vertically if needed
    (COND
      ;;If string too long, move below lines
      ((< (DISTANCE PLTPT1A PLTPT1)
          (HAWS-TXLEN SLOPESTRING TS)
       ) ;_ end of <
       (SETQ
         PT1
          (POLAR
            PT1
            -1.57
            (+ (/ (ABS PROHT) 2.0) (ABS WALTHK) (* TS 1.5))
          ) ;_ end of polar
       ) ;_ end of setq
      )
      ;;If too little headroom, move above lines
      ((< (ABS PROHT) (* 2 TS))
       (SETQ PT1 (POLAR PT1 1.57 (+ TS (/ (ABS PROHT) 2.0) (ABS WALTHK))))
      )
    ) ;_ end of COND
    (HAWS-MKTEXT
      "m"
      PT1
      TS
      (ATAN
        (/ (SIN (ANGLE PLTPT1A PLTPT1))
           (COS (ANGLE PLTPT1A PLTPT1))
        ) ;_ end of /
      ) ;_ end of atan
      SLOPESTRING
    ) ;_ end of HAWS-MKTEXT
    (SETQ SLOPE NIL)
  ) ;_ end of DEFUN
  ;;End of subfunctions, begin main function.
  (haws-core-borrow 0)
  (TERPRI)
  (SETQ
    I 1
    TS (* (GETVAR "dimtxt") (HAWS-DWGSCALE))
    LINGAP
     0.5
    LUP
     (GETVAR "luprec")
  ) ;_ end of SETQ
  (IF (NOT HVEXAG) (SETQ HVEXAG 10))
  (IF (NOT STASCL) (SETQ STASCL 1))
  (IF (NOT *HAWS-ELVSCL*) (SETQ *HAWS-ELVSCL* 1))
  (IF (NOT SLPTYP) (SETQ SLPTYP ""))
  (IF (NOT STPREC) (SETQ STPREC LUP))
  (IF (NOT OFPREC) (SETQ OFPREC LUP))
  (IF (NOT ELPREC) (SETQ ELPREC 2))
  (IF (NOT SLPREC) (SETQ SLPREC 3))
  (IF PROINPUTFILE
    (SETQ F1 PROINPUTFILE)
  ) ;_ end of IF
  (WHILE (NOT (OR F1 REFSTA))
    (PROMPT "\nPlease set reference point.")
    (HAWS-PROSET)
  ) ;_ end of while
  (WHILE (NOT (OR F1 PROHT))
    (PROMPT "\nPlease set profile type.")
    (HAWS-PROSET)
  ) ;_ end of while
  (IF (= 0
         (CDR (ASSOC 40 (TBLSEARCH "STYLE" (GETVAR "textstyle"))))
      ) ;_ end of =
    (COMMAND
      "._text"
      (LIST 0.0 0.0 0.0)
      TS
      (ANGTOS (/ PI 2))
      "TEST"
    ) ;_ end of command
    (COMMAND
      "._text"
      (LIST 0.0 0.0 0.0)
      (ANGTOS (/ PI 2))
      "TEST"
    ) ;_ end of command
  ) ;_ end of IF
  (ENTDEL (ENTLAST))
  (IF (NOT WALTHK)
    (SETQ WALTHK 0)
  ) ;_ end of IF
  (HAWS-VSAVE '("clayer" "osmode" "luprec" "filedia"))
  (SETVAR "osmode" 0)
  (SETVAR "dimzin" 0)
  ;;While there's input,
  (WHILE (IF F1
           (HAWS-RDFILE)
           (HAWS-GETPNT)
         ) ;_ end of IF
    ;;if it is a profile point
    (IF (AND STA (/= STA "Setup"))
      (PROGN
        (COMMAND "._undo" "g")
        ;;save the last point
        (SETQ PLTPT1A (COND (PLTPT1)))  ;
                                        ;and top of curb
        (SETQ TOPPTA (COND (TOPPT)))
        ;;plot the point
        (HAWS-PLTHARDPRO)
        ;;connect to last point
        (IF (AND (> 1000 PROHT) PLTPT1A)
          (HAWS-CONNECTHARD)
        ) ;_ end of IF
        ;;Add slope label if
        (IF
          (AND
            (> 1000 PROHT -10000)       ;profile type is appropriate
            PLTPT1A                     ;there was a previous point
            (/= SLOPE 1000)             ;user didn't supress slope
            (OR SLOPE (/= 0 (- (CAR PLTPT1) (CAR PLTPT1A)))) ;slope isn't divide by zero
          ) ;_ end of and
           (HAWS-CONNECTSOFT)
        ) ;_ end of IF
        ;;Add point label
        (HAWS-PLTSOFTPRO)
        (COMMAND "._undo" "e")
        ;;If requested, write all
        ;;the point information
        ;;to a file (f2).
        (IF F2
          (WRITE-LINE
            (STRCAT
              "PRFPNT    "
              (HAWS-MKFLD (RTOS STA 2 3) 10)
              (HAWS-MKFLD
                (IF OFFSET
                  (RTOS OFFSET 2 3)
                  ""
                ) ;_ end of IF
                10
              ) ;_ end of mkfld
              (HAWS-MKFLD (RTOS ELEV1 2 3) 10)
              (HAWS-MKFLD
                (IF SLOPE
                  (RTOS SLOPE 2 4)
                  ""
                ) ;_ end of IF
                10
              ) ;_ end of mkfld
              (HAWS-MKFLD
                (IF NUM2
                  (RTOS NUM2 2 3)
                  ""
                ) ;_ end of IF
                10
              ) ;_ end of mkfld
              (COND
                (LBLTXT)
                ("")
              ) ;_ end of COND
            ) ;_ end of STRCAT
            F2
          ) ;_ end of WRITE-LINE
        ) ;_ end of IF
        ;;Reset profile point to nil.
        (SETQ STA NIL)
      ) ;_ end of PROGN
      ;;Otherwise, if not reading from file, do setup.
      (IF (NOT F1)
        (HAWS-PROSET)
      ) ;_ end of IF
    ) ;_ end of IF
    (PRINC "\rReading line ")
    (PRINC (SETQ I (1+ I)))
  ) ;_ end of WHILE
  (SETQ
    F1 (IF F1
         (CLOSE F1)
       ) ;_ end of IF
    F2 (IF F2
         (CLOSE F2)
       ) ;_ end of IF
  ) ;_ end of SETQ
  ;;Close files
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
  ;;and end program.
) ;_ end of DEFUN

;;----------------------------LIST POINT----------------------------
(DEFUN
   c:haws-LST
        (/ PT X Y STA EL1)
  (SETQ
    PT  (GETPOINT "\nPick Point: ")
    X   (CAR PT)
    Y   (CADR PT)
    STA (HAWS-RTOSTA (+ REFSTA (- X REFPTX)) NIL)
    EL1 (RTOS (+ REFELV (/ (- Y REFPTY) HVEXAG *HAWS-ELVSCL*)))
  ) ;_ end of SETQ
  (PROMPT "\n           STATION = ")
  (PRINC STA)
  (PROMPT "  ELEVATION = ")
  (PRINC EL1)
  (PRINC)
) ;_ end of DEFUN
;;------------------------------------------------------------------
;;ELLABEL labels elevations with reference to a known point.
(DEFUN
   HAWS-ELLSET
              (/ OPTION RPT)
  (WHILE (PROGN
           (INITGET "Reference Vertical Flip")
           (SETQ
             OPTION
              (GETKWORD
                "\nReference point/Vertical Exaggeration/text Flip angle/<return to exit>: "
              ) ;_ end of getkword
           ) ;_ end of setq
         ) ;_ end of PROGN
    (COND
      ((= OPTION "Flip")
       (INITGET 1)
       (SETQ
         FLPANG
          (GETANGLE
            "\nFlip text to right side of line beyond what angle?: "
          ) ;_ end of getangle
         FLPANG
          (- (/ PI 2) FLPANG)
       ) ;_ end of SETQ
      )
      ((= OPTION "Reference")
       (SETQ
         RPT
          (GETPOINT "\nReference point: ")
         REFPTY
          (CADR RPT)
         REFELV
          (GETREAL "\nReference elevation: ")
       ) ;_ end of SETQ
      )
      ((= OPTION "Vertical") (HAWS-GETHVX))
    ) ;_ end of COND
  ) ;_ end of WHILE
) ;_ end of DEFUN
(DEFUN
   HAWS-ELLPLT
              (/ ANG1 TS EL1 ELEV2 TXLEN LEFT PT2 PT3 PT4 PT5)
  (SETQ PT5 NIL)
  (WHILE (PROGN
           (INITGET 1 "Two")
           (SETQ ANG1 (GETANGLE PT1 "\nLeader rotation or [Two elevs]: "))
           (= ANG1 "Two")
         ) ;_ end of PROGN
    (SETQ PT5 (GETPOINT "\nSecond elevation point: "))
  ) ;_ end of WHILE
  (SETQ
    TS (* (HAWS-DWGSCALE) (GETVAR "dimtxt"))
    FLPANG
     (COND
       (FLPANG)
       (0)
     ) ;_ end of COND
    EL1
     (GETSTRING
       (IF PT5
         "\nFirst description: "
         "\nDescription: "
       ) ;_ end of IF
     ) ;_ end of GETSTRING
    EL1
     (IF (= EL1 "")
       ""
       (STRCAT " " EL1)
     ) ;_ end of IF
    EL1
     (STRCAT
       (RTOS
         (+ REFELV (/ (- (CADR PT1) REFPTY) HVEXAG *HAWS-ELVSCL*))
         2
         2
       ) ;_ end of rtos
       EL1
     ) ;_ end of strcat
    ELEV2
     (IF PT5
       (GETSTRING "\nSecond description: ")
       ""
     ) ;_ end of IF
    ELEV2
     (IF (= ELEV2 "")
       ""
       (STRCAT " " ELEV2)
     ) ;_ end of IF
    ELEV2
     (IF PT5
       (STRCAT
         (RTOS
           (+ REFELV (/ (- (CADR PT5) REFPTY) HVEXAG *HAWS-ELVSCL*))
           2
           2
         ) ;_ end of rtos
         ELEV2
       ) ;_ end of strcat
       " "
     ) ;_ end of IF
    TXLEN
     (MAX
       (HAWS-TXLEN el1 TS)
       (HAWS-TXLEN elev2 TS)
     ) ;_ end of MAX
    LEFT
     (MINUSP (COS (+ FLPANG ANG1)))
    PT2
     (POLAR PT1 ANG1 (* TS 2))
    PT3
     (POLAR PT2 ANG1 TXLEN)
    PT4
     (POLAR
       PT2
       (+ ANG1
          (/ PI
             (IF LEFT
               2
               -2
             ) ;_ end of IF
          ) ;_ end of /
       ) ;_ end of +
       (/ TS 3)
     ) ;_ end of POLAR
  ) ;_ end of SETQ
  (SETVAR "osmode" 0)
  (COMMAND "._line" PT1 PT3 "")
  (HAWS-MKTEXT
    (IF LEFT
      "BR"
      "BL"
    ) ;_ end of IF
    PT2
    TS
    (IF LEFT
      (+ ANG1 PI)
      ANG1
    ) ;_ end of IF
    EL1
  ) ;_ end of mktext
  (IF PT5
    (HAWS-MKTEXT
      (IF LEFT
        "TR"
        "TL"
      ) ;_ end of IF
      PT4
      TS
      (IF LEFT
        (+ ANG1 PI)
        ANG1
      ) ;_ end of IF
      ELEV2
    ) ;_ end of mktext
  ) ;_ end of IF
) ;_ end of DEFUN
(DEFUN
   c:haws-ELLABEL
            (/ PT1)
  (haws-core-borrow 0)
  (HAWS-VSAVE '("osmode"))
  (SETVAR "dimzin" 0)
  (IF (NOT HVEXAG)
    (SETQ HVEXAG 10)
  ) ;_ end of IF
  (IF (NOT REFPTY)
    (HAWS-ELLSET)
  ) ;_ end of IF
  (WHILE (PROGN
           (SETVAR "osmode" 13)
           (INITGET "Setup")
           (SETQ PT1 (GETPOINT "\nPoint to label or [Setup]: "))
         ) ;_ end of progn
    (COND
      ((= PT1 "Setup") (HAWS-ELLSET))
      (T (HAWS-ELLPLT))
    ) ;_ end of COND
  ) ;_ end of WHILE
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of DEFUN

;;---------------------------STATION LABEL-------------------------
(DEFUN
   c:haws-STALABEL
             (/ PNT1 INC N STA1 ENDSTA STA)
  (haws-core-borrow 0)
  (HAWS-MKLAYR "PSTALBL")
  (HAWS-VSAVE '("luprec"))
  (SETVAR "luprec" 0)
  (SETQ
    PNT1
     (GETPOINT "\nTop center point of first label: ")
    INC
     (GETDIST "\nDistance between labels: ")
    N (GETINT "\nNumber of labels: ")
    STA1
     (GETINT "\nFirst station: ")
    ENDSTA
     (+ STA1 (* (1- N) INC))
    STA
     STA1
  ) ;_ end of SETQ
  (WHILE (<= STA ENDSTA)
    (HAWS-MKTEXT
      "tc"
      (LIST (+ (CAR PNT1) (- STA STA1)) (CADR PNT1))
      (* (HAWS-DWGSCALE) 0.12)
      0
      (HAWS-RTOSTA STA NIL)
    ) ;_ end of mktext
    (SETQ STA (+ INC STA))
  ) ;_ end of WHILE
  (HAWS-VRSTOR)
  (haws-core-return)
  (PRINC)
) ;_ end of DEFUN
;;---------------------------ELEVATION LABEL------------------------
(DEFUN
   c:haws-ELV
        (/ PNT1 INC N ELV1 ENDELV ELV)
  (IF (NOT HVEXAG)
    (HAWS-GETHVX)
  ) ;_ end of IF
  (HAWS-MKLAYR "PELEVLBL")
  (SETQ
    PNT1
     (GETPOINT "\nBottom right point of first label: ")
    INC
     (GETINT "\nElevation increment: ")
    N (GETINT "\nNumber of labels: ")
    ELV1
     (GETINT "\nFirst elevation: ")
    ENDELV
     (+ ELV1 (* (1- N) INC))
    ELV
     ELV1
  ) ;_ end of SETQ
  (WHILE (<= ELV ENDELV)
    (HAWS-MKTEXT
      "br"
      (LIST
        (CAR PNT1)
        (+ (* HVEXAG *HAWS-ELVSCL* (- ELV ELV1)) (CADR PNT1))
      ) ;_ end of list
      (* (HAWS-DWGSCALE) 0.12)
      0
      (ITOA ELV)
    ) ;_ end of mktext
    (SETQ ELV (+ INC ELV))
  ) ;_ end of WHILE
  (PRINC)
) ;_ end of DEFUN
;;--------------------------SLOPE CALCULATOR------------------------------
(DEFUN
   c:haws-GRD
        (/ A)
  (DEFUN
     HAWS-GR1
             (/ STA1 EL1 STA2 ELEV2 STAD ELD X1 GR)
    (PROMPT "\nTo Determine the Slope Between 2 Points.")
    (SETQ
      STA1
       (GETREAL "\nEnter 1st Station as a real number: ")
      EL1
       (GETREAL "\nEnter 1st Elevation: ")
      STA2
       (GETREAL "\nEnter 2nd Station as a real number: ")
      ELEV2
       (GETREAL "\nEnter 2nd Elevation: ")
      STAD
       (- STA2 STA1)
      ELD
       (- ELEV2 EL1)
      X1 (/ ELD STAD)
      GR (* X1 100.0)
    ) ;_ end of SETQ
    (PROMPT "\nSLOPE = ")
    (PROMPT (RTOS GR 2 4))
    (PROMPT "%")
  ) ;_ end of DEFUN
  (DEFUN
     HAWS-GR2
             (/ STA1 EL1 STA2 GR STAD ELEV2)
    (PROMPT
      "\nTo Determine the Elevation at a Specified Station."
    ) ;_ end of prompt
    (SETQ
      STA1  (GETREAL "\nEnter 1st Station as a real number: ")
      EL1   (GETREAL "\nEnter 1st Elevation: ")
      STA2  (GETREAL "\nEnter 2nd Station as a real number: ")
      GR    (GETREAL "\nEnter Slope in Percent: ")
      STAD  (- STA2 STA1)
      ELEV2 (+ EL1 (* STAD (/ GR 100.0)))
    ) ;_ end of SETQ
    (PROMPT "\nELEVATION AT STATION ")
    (PRINC STA2)
    (PROMPT " = ")
    (PROMPT (RTOS ELEV2 2 4))
  ) ;_ end of DEFUN
  (INITGET 1 "Slope Elevation")
  (SETQ A (GETKWORD "\nFind Slope/Elevation:"))
  (COND
    ((= A "Slope") (HAWS-GR1))
    ((= A "Elevation") (HAWS-GR2))
  ) ;_ end of COND
  (PRINC)
) ;_ end of DEFUN

(DEFUN c:haws-GRC () (HAWS-LINBLK "*GRC" "PGC"))
(DEFUN c:haws-GRB () (HAWS-LINBLK "*GRB" "PGB"))
(DEFUN
   HAWS-LINBLK
              (BLNAME BLLAY / PT1 PT2)
  (HAWS-MKLAYR BLLAY)
  (SETQ PT1 (GETPOINT "\nFirst point: "))
  (COMMAND "._pline" PT1)
  (WHILE (SETQ PT2 (GETPOINT PT1 "Next point: "))
    (COMMAND PT2)
    (SETQ PT1 PT2)
  ) ;_ end of while
  (COMMAND "")
  (COMMAND
    "._insert"
    BLNAME
    PT1
    (* (HAWS-DWGSCALE) (GETVAR "dimtxt"))
    ""
  ) ;_ end of command
) ;_ end of DEFUN
