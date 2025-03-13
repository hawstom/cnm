;; EE.LSP
;; combined dialogue editor for
;; blocks, attdefs, text, and dimensions.

(defun
   c:haws-ee ()
  (haws-core-init 27)
  (haws-editall nil)
  (haws-core-restore)
)
(defun
   haws-editall (cnm-p / e etype set1 obj1)
  (vl-cmdf "._undo" "_group")
  (prompt "\nText Editor:")
  (setq set1 (ssget))
  (while (and set1 (setq obj1 (ssname set1 0)))
    (setq
      e     (entget obj1)
      etype (cdr (assoc 0 e))
    )
    (redraw obj1 3)
    (cond
      ((= etype "ATTDEF") (vl-cmdf "._TEXTEDIT" obj1 ""))
      ((= etype "TEXT") (vl-cmdf "._TEXTEDIT" obj1 ""))
      ((= etype "MTEXT") (vl-cmdf "._TEXTEDIT" obj1 ""))
      ((= etype "DIMENSION") (vl-cmdf "._TEXTEDIT" obj1 ""))
      ((or cnm-p
           (wcmatch
             (vla-get-effectivename (vlax-ename->vla-object obj1))
             "cnm-bubble-*"
           )
       )
       (hcnm_edit_bubble obj1)
      )
      ((and (= etype "INSERT") (cdr (assoc 66 e)))
       (vl-cmdf "._DDATTE" obj1)
      )
    )
    (redraw obj1 4)
    (if (/= obj1 nil)
      (ssdel obj1 set1)
    )
  )
  (vl-cmdf "._undo" "_end")
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 1 1 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
