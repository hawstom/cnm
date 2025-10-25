;;;(C) Copyright 1997 by Thomas Gail Haws
;;;NEWANG CHANGES ENTITY ORIENTATION TO MATCH A SELECTED ENTITY.
(defun c:haws-na () (haws-newang))
(defun haws-newang (/ sset)
  (haws-core-init 267)
  (prompt (strcat "\n" (haws_evangel_msg)))
  (haws-vsave '("aunits"))
  (vl-cmdf "._undo" "_g")
  (setq sset (ssget))
  (cond
    ((not sset) (prompt "\nNone found."))
    (t (haws-newang-match sset))
  )
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)

(defun haws-newang-match (sset / angle-p basept continue-p ent entlst etype
                      flip-p i meang mel men mes metype nested-p organg
                      rotang shelli shells sslen
                     )
  (initget "Flip Nested Angle")
  (setq continue-p nil)
  (while (not continue-p)
    (setq
      mes
       (entsel
         (strcat
           "\nSelect object to match or [Nested object/Flip results/Angle] <Angle>: "
         )
       )
    )
    (cond
      ((or (not mes) (= mes "Angle"))
       (setq
         angle-p t
         continue-p t
       )
      )
      ((/= (type mes) 'STR) (setq continue-p t))
      ((= mes "Nested")
       (setq
         nested-p t
         continue-p t
       )
      )
      ((= mes "Flip")
       (setq
         flip-p t
         continue-p nil
       )
      )
    )
  )
  (cond
    (nested-p (setq continue-p nil))
    (t (setq continue-p t))
  )
  (while (not continue-p)
    (setq
      mes
       (nentsel
         (strcat
           "\nSelect object to match or [Flip/Angle] <Angle>: "
         )
       )
    )
    (cond
      ((or (not mes) (= mes "Angle"))
       (setq
         angle-p t
         continue-p t
       )
      )
      ((= mes "Flip")
       (setq
         flip-p t
         continue-p nil
       )
      )
      ((/= (type mes) 'STR) (setq continue-p t))
    )
  )
  (cond
    ((or (not mes)
         angle-p
         (not
           (or (= (setq
                    metype
                     (cdr
                       (assoc
                         0
                         (setq mel (entget (setq men (car mes))))
                       )
                     )
                  )
                  "LINE"
               )
               (= metype "ARC")
               (= metype "VERTEX")
               (= metype "TEXT")
               (= metype "MTEXT")
               (= metype "ATTDEF")
               (= metype "ATTRIB")
               (= metype "SHAPE")
               (= metype "INSERT")
           )
         )
     )
     (cond
       ((setq
          meang
           (getangle
             "\nNo allowable entity selected.  New angle or <Rotation>: "
           )
        )
       )
       ((setq rotang (getangle "\nRotation angle: ")))
     )
    )
  )
  (setq
    meang
     (cond
       (meang)
       (rotang)
       ((= metype "LINE")
        (angle
          (trans (cdr (assoc 10 mel)) 0 1)
          (trans (cdr (assoc 11 mel)) 0 1)
        )
       )
       ((= metype "ARC")
        (- (angle
             (polar
               (cdr (assoc 10 mel))
               (cdr (assoc 50 mel))
               (cdr (assoc 40 mel))
             )
             (polar
               (cdr (assoc 10 mel))
               (cdr (assoc 51 mel))
               (cdr (assoc 40 mel))
             )
           ) ;_ end of angle
           (angle '(0.0 0.0 0.0) (getvar "UCSXDIR"))
        ) ;_ end of angle
       )
       ((= metype "VERTEX")
        (angle
          (trans (cdr (assoc 10 mel)) men 1)
          (trans (cdr (assoc 10 (entget (entnext men)))) men 1)
        )
       )
       ((or (= metype "TEXT")
            (= metype "SHAPE")
            (= metype "ATTDEF")
            (= metype "ATTRIB")
            (= metype "INSERT")
        ) ;_ end of OR
        (- (cdr (assoc 50 mel))
           (angle '(0.0 0.0 0.0) (getvar "UCSXDIR"))
        )
       )
     ) ;_ end of cond
  ) ;_ end of setq
  (cond
    (nested-p
     (setq
       shells
        (cadddr mes)
       i -1
     ) ;_ end of setq
     (if shells
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
  (while (> sslen 0)
    (setq
      entlst
       (entget (setq ent (ssname sset (setq sslen (1- sslen)))))
      basept
       (cond
         ((and
            (or (= (setq etype (cdr (assoc 0 entlst))) "TEXT")
                (= etype "ATTDEF")
                (= etype "ATTRIB")
            )
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
           ((= etype "LINE")
            (angle
              (trans (cdr (assoc 10 entlst)) 0 1)
              (trans (cdr (assoc 11 entlst)) 0 1)
            )
           )
           ((or (= etype "INSERT")
                (= etype "TEXT")
                (= etype "ATTDEF")
                (= etype "ATTRIB")
                (= etype "SHAPE")
            )
            (- (cdr (assoc 50 entlst))
               (angle '(0.0 0.0 0.0) (getvar "UCSXDIR"))
            )
           )
           ((= etype "MTEXT")
            (cdr (assoc 50 entlst))
           )
           ((= etype "POLYLINE")
            (angle
              (cdr (assoc 10 entlst))
              (cdr (assoc 10 (entget (entnext ent))))
            )
           )
           ((= etype "ARC")
            (angle
              (polar
                (cdr (assoc 10 entlst))
                (cdr (assoc 50 entlst))
                (cdr (assoc 40 entlst))
              )
              (polar
                (cdr (assoc 10 entlst))
                (cdr (assoc 51 entlst))
                (cdr (assoc 40 entlst))
              )
            ) ;_ end of angle
           )
           (t nil)
         ) ;_ end of cond
      ) ;_ end of setq
    ) ;_ end of if
    (if flip-p
      (setq organg (+ organg pi))
    )
    (if organg
      (vl-cmdf
        "._rotate"
        ent
        ""
        (trans basept ent 1)
        "_R"
        organg
        meang
      )
      (prompt "\nDon't know how to rotate this entity.  ")
    ) ;_ end of if
  ) ;_ end of while
  (vl-cmdf "._select" sset "")
  (vl-cmdf "._undo" "_e")
  (prompt "\nDone.")
)
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|
