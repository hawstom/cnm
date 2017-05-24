;;; PJL joins as a polyline objects that are on the layer of the selected object.
(DEFUN
   C:HAWS-PJL (/ EG EN ET LA PEAOLD)
  (SETQ
    EN	(CAR (ENTSEL))
    EG	(ENTGET EN)
    LA	(CDR (ASSOC 8 EG))
    ET	(CDR (ASSOC 0 EG))
    SS1	(SSGET "X" (LIST (CONS 8 LA)))
  )
  (COMMAND "._pedit" EN)
  (COND
    ((AND (WCMATCH ET "ARC,LINE") (= (GETVAR "peditaccept") 0))
     (COMMAND "_y")
    )
  )
  (COMMAND "_join" SS1 "" "")
  (PRINC)
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 T nil nil T)
;*** DO NOT add text below the comment! ***|;
