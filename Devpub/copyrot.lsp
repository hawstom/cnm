;;;COPY AND ROTATE AN OBJECT
(DEFUN C:HAWS-COPYROT ()
(haws-core-init 11) (HAWS-COPYROT NIL))
(DEFUN C:HAWS-COPYROTDRAG ()
(haws-core-init 12) (HAWS-COPYROT T))
(DEFUN
   HAWS-COPYROT (DRAG-MODE-P / EN-I EN-LAST INPUT1 PT-BASE ANG-REF ANG-ROT SS1)
  (SETQ SS1 (SSGET))
  (COND (SS1 (SETQ PT-BASE (GETPOINT "\nSpecify base point: "))))
  (COND
    (PT-BASE
     (SETQ ANG-REF (GETANGLE PT-BASE "\nSpecify reference angle: "))
    )
  )
  (COND
    (ANG-REF
     (COMMAND "._undo" "_g")
     (WHILE
       (COND
         (DRAG-MODE-P
          (INITGET "eXit")
          (SETQ INPUT1 (GETKWORD "\n[eXit] <continue>: "))
          (NOT INPUT1)
         )
         (T (SETQ ANG-ROT (GETANGLE PT-BASE "\nSpecify rotation angle: ")))
       )
       (SETQ
         EN-I (ENTLAST)
         SS2  (SSADD)
       )
       (COMMAND "._copy" SS1 "" "0,0" "0,0")
       (WHILE (SETQ EN-I (ENTNEXT EN-I))
         (SETQ SS2 (SSADD EN-I SS2))
       )
       (COMMAND
         "._rotate"
         SS2
         ""
         PT-BASE
         "_r"
         PT-BASE
         (POLAR PT-BASE ANG-REF 1)
         (COND (DRAG-MODE-P PAUSE)((POLAR PT-BASE ANG-ROT 1)))
       )
     )
    )
  )
  (COMMAND "._undo" "_e")
  (PRINC)
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
