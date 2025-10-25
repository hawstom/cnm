;;;COPY AND ROTATE AN OBJECT
(defun c:haws-copyrot ()
(haws-core-init 11) (haws-copyrot nil))
(defun c:haws-copyrotdrag ()
(haws-core-init 12) (haws-copyrot t))
(defun haws-copyrot (drag-mode-p / en-i en-last input1 pt-base ang-ref ang-rot ss1)
  (setq ss1 (ssget))
  (cond (ss1 (setq pt-base (getpoint "\nSpecify base point: "))))
  (cond
    (pt-base
     (setq ang-ref (getangle pt-base "\nSpecify reference angle: "))
    )
  )
  (cond
    (ang-ref
     (vl-cmdf "._undo" "_g")
     (while
       (cond
         (drag-mode-p
          (initget "eXit")
          (setq input1 (getkword "\n[eXit] <continue>: "))
          (not input1)
         )
         (t (setq ang-rot (getangle pt-base "\nSpecify rotation angle: ")))
       )
       (setq
         en-i (entlast)
         ss2  (ssadd)
       )
       (vl-cmdf "._copy" ss1 "" "0,0" "0,0")
       (while (setq en-i (entnext en-i))
         (setq ss2 (ssadd en-i ss2))
       )
       (vl-cmdf
         "._rotate"
         ss2
         ""
         pt-base
         "_r"
         pt-base
         (polar pt-base ang-ref 1)
         (cond (drag-mode-p pause)((polar pt-base ang-rot 1)))
       )
     )
    )
  )
  (vl-cmdf "._undo" "_e")
  (princ)
)
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
