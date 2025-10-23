;;;(C) Copyright 1997 by Thomas Gail Haws
;;;NEWANG CHANGES ENTITY ORIENTATION TO MATCH A SELECTED ENTITY.
(DEFUN C:HAWS-NA () (HAWS-NEWANG))
(DEFUN HAWS-NEWANG (/ SSET)
  (haws-core-init 267)
  (prompt (strcat "\n" (HAWS_EVANGEL_MSG)))
  (HAWS-VSAVE '("aunits"))
  (vl-cmdf "._undo" "_g")
  (SETQ SSET (SSGET))
  (COND
    ((NOT SSET) (PROMPT "\nNone found."))
    (T (HAWS-NEWANG-MATCH SSET))
  )
  (HAWS-VRSTOR)
  (haws-core-restore)
  (PRINC)
)

(DEFUN HAWS-NEWANG-MATCH (SSET / ANGLE-P BASEPT CONTINUE-P ENT ENTLST ETYPE
                      FLIP-P I MEANG MEL MEN MES METYPE NESTED-P ORGANG
                      ROTANG SHELLI SHELLS SSLEN
                     )
  (INITGET "Flip Nested Angle")
  (SETQ CONTINUE-P NIL)
  (WHILE (NOT CONTINUE-P)
    (SETQ
      MES
       (ENTSEL
         (STRCAT
           "\nSelect object to match or [Nested object/Flip results/Angle] <Angle>: "
         )
       )
    )
    (COND
      ((OR (NOT MES) (= MES "Angle"))
       (SETQ
         ANGLE-P T
         CONTINUE-P T
       )
      )
      ((/= (TYPE MES) 'STR) (SETQ CONTINUE-P T))
      ((= MES "Nested")
       (SETQ
         NESTED-P T
         CONTINUE-P T
       )
      )
      ((= MES "Flip")
       (SETQ
         FLIP-P T
         CONTINUE-P NIL
       )
      )
    )
  )
  (COND
    (NESTED-P (SETQ CONTINUE-P NIL))
    (T (SETQ CONTINUE-P T))
  )
  (WHILE (NOT CONTINUE-P)
    (SETQ
      MES
       (NENTSEL
         (STRCAT
           "\nSelect object to match or [Flip/Angle] <Angle>: "
         )
       )
    )
    (COND
      ((OR (NOT MES) (= MES "Angle"))
       (SETQ
         ANGLE-P T
         CONTINUE-P T
       )
      )
      ((= MES "Flip")
       (SETQ
         FLIP-P T
         CONTINUE-P NIL
       )
      )
      ((/= (TYPE MES) 'STR) (SETQ CONTINUE-P T))
    )
  )
  (COND
    ((OR (NOT MES)
         ANGLE-P
         (NOT
           (OR (= (SETQ
                    METYPE
                     (CDR
                       (ASSOC
                         0
                         (SETQ MEL (ENTGET (SETQ MEN (CAR MES))))
                       )
                     )
                  )
                  "LINE"
               )
               (= METYPE "ARC")
               (= METYPE "VERTEX")
               (= METYPE "TEXT")
               (= METYPE "MTEXT")
               (= METYPE "ATTDEF")
               (= METYPE "ATTRIB")
               (= METYPE "SHAPE")
               (= METYPE "INSERT")
           )
         )
     )
     (COND
       ((SETQ
          MEANG
           (GETANGLE
             "\nNo allowable entity selected.  New angle or <Rotation>: "
           )
        )
       )
       ((SETQ ROTANG (GETANGLE "\nRotation angle: ")))
     )
    )
  )
  (SETQ
    MEANG
     (COND
       (MEANG)
       (ROTANG)
       ((= METYPE "LINE")
        (ANGLE
          (TRANS (CDR (ASSOC 10 MEL)) 0 1)
          (TRANS (CDR (ASSOC 11 MEL)) 0 1)
        )
       )
       ((= METYPE "ARC")
        (- (ANGLE
             (POLAR
               (CDR (ASSOC 10 MEL))
               (CDR (ASSOC 50 MEL))
               (CDR (ASSOC 40 MEL))
             )
             (POLAR
               (CDR (ASSOC 10 MEL))
               (CDR (ASSOC 51 MEL))
               (CDR (ASSOC 40 MEL))
             )
           ) ;_ end of angle
           (ANGLE '(0.0 0.0 0.0) (GETVAR "UCSXDIR"))
        ) ;_ end of angle
       )
       ((= METYPE "VERTEX")
        (ANGLE
          (TRANS (CDR (ASSOC 10 MEL)) MEN 1)
          (TRANS (CDR (ASSOC 10 (ENTGET (ENTNEXT MEN)))) MEN 1)
        )
       )
       ((OR (= METYPE "TEXT")
            (= METYPE "SHAPE")
            (= METYPE "ATTDEF")
            (= METYPE "ATTRIB")
            (= METYPE "INSERT")
        ) ;_ end of OR
        (- (CDR (ASSOC 50 MEL))
           (ANGLE '(0.0 0.0 0.0) (GETVAR "UCSXDIR"))
        )
       )
     ) ;_ end of cond
  ) ;_ end of setq
  (COND
    (NESTED-P
     (SETQ
       SHELLS
        (CADDDR MES)
       I -1
     ) ;_ end of setq
     (IF SHELLS
       (PROGN
         (WHILE (SETQ SHELLI (NTH (SETQ I (1+ I)) SHELLS))
           (SETQ MEANG (+ MEANG (CDR (ASSOC 50 (ENTGET SHELLI)))))
         ) ;_ end of while
       ) ;_ end of progn
     ) ;_ end of if
    )
  ) ;_ end of cond
  ;; Change all of the entities in the selection set.
  (PROMPT "\nChanging entities to match selection.  ")
  (SETQ SSLEN (SSLENGTH SSET))
  (SETVAR "aunits" 3)
  (WHILE (> SSLEN 0)
    (SETQ
      ENTLST
       (ENTGET (SETQ ENT (SSNAME SSET (SETQ SSLEN (1- SSLEN)))))
      BASEPT
       (COND
         ((AND
            (OR (= (SETQ ETYPE (CDR (ASSOC 0 ENTLST))) "TEXT")
                (= ETYPE "ATTDEF")
                (= ETYPE "ATTRIB")
            )
            (NOT (EQUAL (CDR (ASSOC 11 ENTLST)) '(0.0 0.0 0.0)))
          ) ;_ end of and
          (CDR (ASSOC 11 ENTLST))
         )
         (T (CDR (ASSOC 10 ENTLST)))
       ) ;_ end of cond
    ) ;_ end of setq
    (IF ROTANG
      (SETQ ORGANG 0)
      (SETQ
        ORGANG
         (COND
           ((= ETYPE "LINE")
            (ANGLE
              (TRANS (CDR (ASSOC 10 ENTLST)) 0 1)
              (TRANS (CDR (ASSOC 11 ENTLST)) 0 1)
            )
           )
           ((OR (= ETYPE "INSERT")
                (= ETYPE "TEXT")
                (= ETYPE "ATTDEF")
                (= ETYPE "ATTRIB")
                (= ETYPE "SHAPE")
            )
            (- (CDR (ASSOC 50 ENTLST))
               (ANGLE '(0.0 0.0 0.0) (GETVAR "UCSXDIR"))
            )
           )
           ((= ETYPE "MTEXT")
            (CDR (ASSOC 50 ENTLST))
           )
           ((= ETYPE "POLYLINE")
            (ANGLE
              (CDR (ASSOC 10 ENTLST))
              (CDR (ASSOC 10 (ENTGET (ENTNEXT ENT))))
            )
           )
           ((= ETYPE "ARC")
            (ANGLE
              (POLAR
                (CDR (ASSOC 10 ENTLST))
                (CDR (ASSOC 50 ENTLST))
                (CDR (ASSOC 40 ENTLST))
              )
              (POLAR
                (CDR (ASSOC 10 ENTLST))
                (CDR (ASSOC 51 ENTLST))
                (CDR (ASSOC 40 ENTLST))
              )
            ) ;_ end of angle
           )
           (T NIL)
         ) ;_ end of cond
      ) ;_ end of setq
    ) ;_ end of if
    (IF FLIP-P
      (SETQ ORGANG (+ ORGANG PI))
    )
    (IF ORGANG
      (vl-cmdf
        "._rotate"
        ENT
        ""
        (TRANS BASEPT ENT 1)
        "_R"
        ORGANG
        MEANG
      )
      (PROMPT "\nDon't know how to rotate this entity.  ")
    ) ;_ end of if
  ) ;_ end of while
  (vl-cmdf "._select" SSET "")
  (vl-cmdf "._undo" "_e")
  (PROMPT "\nDone.")
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|
