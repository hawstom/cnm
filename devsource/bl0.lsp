;change all entities within selected block to layer 0
(defun c:haws-bl0 (/ cl en ent)
	(haws-core-init 166)
	(haws-vsave '("clayer" "highlight"))
  (setvar "highlight" 0)
  (vl-cmdf "._layer" "_s" "0" "")
  (setq en nil ent nil)
  (while (= en nil)
    (setq en (car (entsel
    "\nSelect block to redefine on Layer 0: ")))
  )
  (setq ent (entget en))
  (if (= (haws-dxf 0 ent) "INSERT")
    (progn
      (vl-cmdf "._copy" en "" "@" "@")
      (setq en (entlast))
      (haws-block0 en)
    )
    (progn (princ "\nError: A block must be selected.") (c:haws-bl0))
  )
	(haws-vrstor)(haws-core-restore)
  (princ)
)
(defun haws-block0 (en / en2 ent inspt blkname ssblk)
  (vl-cmdf "._ucs" "_w")
  (setq
    ent (entget en)
    ucsp t
    inspt (haws-dxf 10 ent)
    blkname (haws-dxf 2 ent)
  )
  (setq ent (subst (cons 41 1) (assoc 41 ent) ent)) ;x-scale
  (setq ent (subst (cons 42 1) (assoc 42 ent) ent)) ;y-scale
  (setq ent (subst (cons 43 1) (assoc 43 ent) ent)) ;z-scale
  (setq ent (subst (cons 50 0) (assoc 50 ent) ent)) ;angle
  (entmod ent)
  (vl-cmdf "._explode" en)
  (setq en (entnext en))
  (setq ssblk (ssadd))
  (while (/= en nil)
    (setq ent (entget en))
    (setq ent (subst (cons 8 "0") (assoc 8 ent) ent))   ;layer
    (setq ent (subst (cons 62 256) (assoc 62 ent) ent)) ;color
    (setq ent (subst (cons 6 "bylayer") (assoc 6 ent) ent))
    (entmod ent)
    (setq ssblk (ssadd en ssblk))
    (if (= (haws-dxf 0 ent) "INSERT") ;if en is a block
      (progn
        (vl-cmdf "._copy" en "" "@" "@")
        (setq en (entlast))
        (haws-block0 en2)
      )
    )
    (setq en (entnext en))
  )
  (vl-cmdf "._block" blkname "_y" inspt ssblk "" "._ucs" "_p")
  (setq ucsp nil)
)
; end
