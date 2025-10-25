;;; CS (COPY STRING) CHANGES TEXT STRING TO MATCH A SELECTED TEXT ENTITY.
;;; Written by Thomas Gail Haws
(defun c:haws-cs (/ sset mten mtclr mtlyr mtstr eni ent entlst sslen)
  (haws-core-init 213)
  (prompt "\nText/Multileader to change/<RETURN to change an attribute>:")
  (setq sset (ssget '((0 . "*TEXT,MULTILEADER"))))
  (cond
    ((not sset)
     (setq en1 (car (nentsel "\nAttribute to change: ")))
     (if en1
       (setq
         sset (ssadd)
         sset (ssadd en1 sset)
       )
     )
    )
  )
  (if (not sset)
    (progn (prompt "\nNone found.") (exit))
    (progn
      (setq
        mten  (car (nentsel "Text/Multileader to match:"))
        mtstr (haws-cs-string-get mten)
      )
      ;; Change all of the entities in the selection set.
      (prompt "\nChanging text to match selection...")
      (vl-cmdf "._undo" "_g")
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq eni (ssname sset (setq sslen (1- sslen))))
        (haws-cs-string-put eni mtstr)
      )
      (vl-cmdf "._undo" "_e")
      (prompt "done.")
    )
  )
  (haws-core-restore)
  (princ)
)

(defun haws-cs-string-get (en1 / et)
  (setq et (cdr (assoc 0 (entget en1))))
  (cond
    ((or (= et "TEXT")(= et "MTEXT"))
     (cdr (assoc 1 (entget en1)))
    )
    ((= et "MULTILEADER")
     (cdr (assoc 304 (entget en1)))
    )
  )
)

(defun haws-cs-string-put (en1 mtstr / entlst et group)
  (setq
    entlst
     (entget en1)
    et (cdr (assoc 0 entlst))
    group
     (cond
       ((or (= et "TEXT") (= et "MTEXT"))
        1
       )
       ((= et "MULTILEADER") 304)
     )
  )
  (entmod (subst (cons group mtstr) (assoc group entlst) entlst))
  (entupd en1)
)
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
