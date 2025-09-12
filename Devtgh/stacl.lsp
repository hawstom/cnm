;;;STACL.LSP--Thomas Gail Haws  March 1996
;;;(C) Copyright 1997 by Thomas Gail Haws
;;; 2007-08-17 Fixed the pline that was being left at end.
(defun c:haws-stacl ( / ds style0 txht blpt1 blpt2 inspt ststa meas1 cl strtpt
  circ1 enext elast sset totlen stablk count brkpt workcl wclend cl1 cl2)

;Set layer, error trapper, undo group, etc.

  (haws-core-init 309)
  (if HAWS-VSAVE (HAWS-VSAVE '("clayer" "osmode" "expert")))
  (vl-cmdf "._undo" "_group")
  (vl-cmdf "._layer" "_n" "sta" "_t" "sta" "_s" "sta" "_c" 1 "" "")
  (setvar "osmode" 0)
  (gc)

;Get input from user
  (textpage)
  (prompt"     STACL.LSP--Adds numbered station ticks to a polyline centerline")
  (prompt"\n\n\nNote that this routine will only work properly on polylines")
  (prompt"\nwhich have been created running up-station. If you get bad")
  (prompt"\nresults, try drawing a new polyline on top of the existing one.")
  (prompt"\n\nCurrent style will be used for text.")
  (prompt"\n\nDimscale and dimtxt will be used to scale text.")
  (setq
    ds (HAWS-DWGSCALE)
    style0 (= 0 (cdr (assoc 40 (tblsearch "STYLE" (getvar "textstyle")))))
    txht (* ds (getvar "dimtxt"))
    blpt1 (list 0 (* -0.5 txht) 0)  blpt2 (list 0 (* 0.5 txht) 0)
    inspt (list 0.0 txht 0.0)
    ststa (getreal "\n\nEnter starting station as a real number:")
  )
  (setq
    meas1 (-(*(float(fix(+(/ ststa 100.0)1)))100.0) ststa)
    cl (entsel "\nSelect polyline centerline:")
  )

;Define "STACL" block for use with measure command

  (vl-cmdf "._circle" (cadr cl) txht)
  (setq circ1 (entlast))
  (if style0
    (vl-cmdf "._line" blpt1 blpt2 "" "._text" "_j" "_bc" inspt txht "0" "10")
    (vl-cmdf
      "._line" blpt1 blpt2 ""
      "._text" "_j" "_bc" inspt "0" "10"
    )
  )
  (setq sset (ssadd) enext circ1)
  (while (setq enext (entnext enext))
    (ssadd enext sset)
  )
  (setvar "expert" 5)
  (vl-cmdf "._block" "stacl" "0,0" sset "")
  (setvar "expert" 0)

;Get length of centerline and make a working copy of centerline

  (vl-cmdf "._area" "_e" cl)(setq totlen (getvar "perimeter"))
  (vl-cmdf "._copy" cl "" "0,0" "0,0")
  (setq workcl (entlast) wclend workcl)
  (cond
    ( (= (cdr (assoc 0 (entget workcl))) "POLYLINE")
      (while (/= (cdr (assoc 0 (entget wclend))) "SEQEND")
        (setq wclend (entnext wclend))
      )
    )
  )

;Place first station marker
  (vl-cmdf "._measure" cl "_b" "stacl" "_y" meas1)
  (setq enext (entnext wclend))
  (setq brkpt (trans(cdr(assoc 10 (entget enext)))enext 1))
  (while (setq enext (entnext enext))
    (ssadd enext sset)
  )
  (vl-cmdf "._erase" sset "")

;Break and erase working cl up to first station marker
  (setq elast (entlast))
  (vl-cmdf "._break" workcl brkpt brkpt)
  (setq cl1 (entnext elast) cl2 (entlast))
  (vl-cmdf
    "._erase"
    (if (= (cdr (assoc 0 (entget workcl))) "POLYLINE") cl1 workcl)
    ""
  )

;Place remaining station markers, explode, and edit to proper station labels

  (setq enext wclend count 0)
  (vl-cmdf "._measure" (list cl2 brkpt) "_b" "stacl" "_y" "100")
  (while (setq enext (entnext enext))
    (if (= (cdr (assoc 0 (entget enext))) "INSERT")(vl-cmdf "._explode" enext))
  )
  (setq enext wclend)
  (while (setq enext (entnext enext))
    (if (= (cdr (assoc 0 (entget enext))) "TEXT")
      (progn
        (setq sta (+ ststa meas1 (* count 100.0)))
        (vl-cmdf "._change" enext "" "" "" "" "")
        (if style0 (vl-cmdf ""))
        (vl-cmdf (rtos (/ sta 100.0) 2 0))
        (setq count (1+ count))
  ) ) )
  (vl-cmdf "._erase" cl1 cl2  circ1 "" "._undo" "_end" "._redraw")
  (haws-core-restore)(HAWS-VRSTOR)
  (princ)
)
