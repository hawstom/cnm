;change all entities within selected block to layer 0
(defun c:haws-bl0 (/ cl en ent)
	(haws-core-init 166)
	(HAWS-VSAVE '("clayer" "highlight"))
  (setvar "highlight" 0)
  (command "._layer" "_s" "0" "")
  (setq en nil ent nil)
  (while (= en nil)
    (setq en (car (entsel
    "\nSelect block to redefine on Layer 0: ")))
  )
  (setq ent (entget en))
  (if (= (HAWS-DXF 0 ent) "INSERT")
    (progn
      (command "._copy" en "" "@" "@")
      (setq en (entlast))
      (HAWS-block0 en)
    )
    (progn (princ "\nError: A block must be selected.") (c:haws-bl0))
  )
	(HAWS-VRSTOR)(haws-core-restore)
  (princ)
)
(defun HAWS-block0 (en / en2 ent inspt blkname ssblk)
  (command "._ucs" "_w")
  (setq
    ent (entget en)
    ucsp T
    inspt (HAWS-DXF 10 ent)
    blkname (HAWS-DXF 2 ent)
  )
  (setq ent (subst (cons 41 1) (assoc 41 ent) ent)) ;x-scale
  (setq ent (subst (cons 42 1) (assoc 42 ent) ent)) ;y-scale
  (setq ent (subst (cons 43 1) (assoc 43 ent) ent)) ;z-scale
  (setq ent (subst (cons 50 0) (assoc 50 ent) ent)) ;angle
  (entmod ent)
  (command "._explode" en)
  (setq en (entnext en))
  (setq ssblk (ssadd))
  (while (/= en nil)
    (setq ent (entget en))
    (setq ent (subst (cons 8 "0") (assoc 8 ent) ent))   ;layer
    (setq ent (subst (cons 62 256) (assoc 62 ent) ent)) ;color
    (setq ent (subst (cons 6 "bylayer") (assoc 6 ent) ent))
    (entmod ent)
    (setq ssblk (ssadd en ssblk))
    (if (= (HAWS-DXF 0 ent) "INSERT") ;if en is a block
      (progn
        (command "._copy" en "" "@" "@")
        (setq en (entlast))
        (HAWS-block0 en2)
      )
    )
    (setq en (entnext en))
  )
  (command "._block" blkname "_y" inspt ssblk "" "._ucs" "_p")
  (setq ucsp nil)
)
; end
