;March 16, 1989
;Routine that works identical to the Autocad "OFFSET" command
;only all new entities are created in the current layer. Rev 7/01/96
(defun c:haws-oo ()
(haws-core-init 92)(c:haws-offsetx))
(defun c:haws-Offsetx (/ enm0 temp pp clay)
(haws-core-init 93)
  (haws-errdef)
  (princ "\nOffset distance or Through <")
  (princ (if (= (type qdofst) 'REAL) (rtos qdofst) "Through"))
  (princ ">: ")
  (initget 6 "T")
  (if (setq temp (getdist)) (setq qdofst temp))
  (setq clay (getvar "CLAYER"))
  (setq pp (cadr (entsel "\nSelect object to offset: ")))
  (while pp
    (princ (if (= "T" qdofst) "\nThrough point: " "\nSide to offset: "))
    (setq enm0 (entlast))
    (command ".OFFSET" qdofst pp PAUSE "")
    (setq enm (entlast))
    (if (not (eq enm0 enm))
      (progn
        (setq elst (entget enm))
        (setq elst (subst (cons 8 clay) (assoc 8 elst) elst))
        (entmod elst)
      )
    )
    (setq pp (cadr (entsel "\nSelect object to offset: ")))
  )
  (haws-errrst)(princ)
);end OO
