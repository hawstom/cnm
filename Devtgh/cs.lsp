;;; CS (COPY STRING) CHANGES TEXT STRING TO MATCH A SELECTED TEXT ENTITY.
;;; Written by Thomas Gail Haws
(DEFUN
   C:HAWS-CS (/ SSET MTEN MTCLR MTLYR MTSTR ENI ENT ENTLST SSLEN)
  (HAWS-CORE-INIT 213)
  (PROMPT "\nText/Multileader to change/<RETURN to change an attribute>:")
  (SETQ SSET (SSGET '((0 . "*TEXT,MULTILEADER"))))
  (COND
    ((NOT SSET)
     (SETQ EN1 (CAR (NENTSEL "\nAttribute to change: ")))
     (IF EN1
       (SETQ
         SSET (SSADD)
         SSET (SSADD EN1 SSET)
       )
     )
    )
  )
  (IF (NOT SSET)
    (PROGN (PROMPT "\nNone found.") (EXIT))
    (PROGN
      (SETQ
        MTEN  (CAR (NENTSEL "Text/Multileader to match:"))
        MTSTR (HAWS-CS-STRING-GET MTEN)
      )
      ;; Change all of the entities in the selection set.
      (PROMPT "\nChanging text to match selection...")
      (COMMAND "._undo" "_g")
      (SETQ SSLEN (SSLENGTH SSET))
      (WHILE (> SSLEN 0)
        (SETQ ENI (SSNAME SSET (SETQ SSLEN (1- SSLEN))))
        (HAWS-CS-STRING-PUT ENI MTSTR)
      )
      (COMMAND "._undo" "_e")
      (PROMPT "done.")
    )
  )
  (HAWS-CORE-RESTORE)
  (PRINC)
)

(DEFUN
   HAWS-CS-STRING-GET (EN1 / ET)
  (SETQ ET (CDR (ASSOC 0 (ENTGET EN1))))
  (COND
    ((OR (= ET "TEXT")(= ET "MTEXT"))
     (CDR (ASSOC 1 (ENTGET EN1)))
    )
    ((= ET "MULTILEADER")
     (CDR (ASSOC 304 (ENTGET EN1)))
    )
  )
)

(DEFUN
   HAWS-CS-STRING-PUT (EN1 MTSTR / ENTLST ET GROUP)
  (SETQ
    ENTLST
     (ENTGET EN1)
    ET (CDR (ASSOC 0 ENTLST))
    GROUP
     (COND
       ((OR (= ET "TEXT") (= ET "MTEXT"))
        1
       )
       ((= ET "MULTILEADER") 304)
     )
  )
  (ENTMOD (SUBST (CONS GROUP MTSTR) (ASSOC GROUP ENTLST) ENTLST))
  (ENTUPD EN1)
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
