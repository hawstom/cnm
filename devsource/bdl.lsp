;;Label line with bearing and distance
;;(C) Copyright 1997 by Thomas Gail Haws
;;Written by Thomas Gail Haws
(defun c:haws-bdl
	(/ ang1 dg lbear ldist left lline ltop pt1 pt10 pt11 pt2 pt3 pt4 ptxt rot txht ucsp)
  (haws-core-init 163)
  (haws-vsave
    '("lunits" "luprec" "aunits" "auprec" "dimtad" "ucsfollow")
  )
  (haws-vset
    '(("lunits" 2)
      ("luprec" 2)
      ("aunits" 4)
      ("auprec" 4)
      ("dimtad" 0)
      ("dimtvp" 0)
      ("ucsfollow" 0)
     )
  )
  (vl-cmdf "._ucs" "_w")
  (setq
    lline
     (nentsel "\nSelect line to label: ")
    ucsp
     t
  )
  (setq
    txht
     (* (haws-dwgscale) (getvar "dimtxt"))
    pt1
     (osnap (cadr lline) "nea")
    lline
     (entget (car lline))
    pt10
     (cdr (assoc 10 lline))
    pt11
     (cdr (assoc 11 lline))
    lbear
     (haws-rtob (angle pt10 pt11) 4)
    ldist
     (strcat (rtos (distance pt10 pt11)) "'")
    pt2
     (list
       (/ (+ (car pt10) (car pt11)) 2)
       (/ (+ (cadr pt10) (cadr pt11)) 2)
       (/ (+ (caddr pt10) (caddr pt11)) 2)
     )
    rot
     (angle pt2 pt1)
    pt3
     (polar pt2 (+ rot (/ pi 2)) (* txht 0.667))
    pt4
     (polar pt2 (- rot (/ pi 2)) (* txht 0.667))
  )
  (setq
    ltop
     (getstring
       1
       "Enter text, return for line data, or L for a leader: "
     )
  )
  (cond
    ((= ltop "")
     (haws-mktext "c" pt3 nil rot lbear)
     (haws-mktext "tc" pt4 nil rot ldist)
    )
    ((= (strcase ltop) "L")
     (vl-cmdf "._ucs" "_p")
     (setq
         ucsp nil
         ptxt (getpoint (trans pt1 0 1) "\nPick text location: ")
         ang1 (angle (trans pt1 0 1) ptxt)
   	 left (minusp (cos ang1))
)
     (cond
       ((>= (atof (getvar "acadver")) 14)
	(vl-cmdf "._leader" (trans pt1 0 1) ptxt "" lbear ldist "")
       )
       (t
	(vl-cmdf "._dim" "_leader" (trans pt1 0 1) ptxt "" lbear "_exit")
	(setq
	  ptxt
	   (polar
	     (polar ptxt (/ pi -2) (* 1.667 txht))
	     (if left
	       pi
	       0
	     )
	     (+	(if (< (abs (sin ang1)) (sin 0.25))
		  0
		  txht
		)
		dg
	     )
	   )
	)
	(haws-mktext
	  (if left
	    "mr"
	    "ml"
	  )
	  ptxt
	  txht
	  0
	  ldist
	)
       )
     )
    )
    (t
     (haws-mktext "c" pt3 nil rot ltop)
     (vl-cmdf "._dtext" "_j" "_tc" pt4 txht (angtos rot))
    )
  )
  (if ucsp (vl-cmdf "._ucs" "p"))
  (setq ucsp nil)
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)
