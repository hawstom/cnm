;CS (COPY STRING) CHANGES TEXT STRING TO MATCH A SELECTED TEXT ENTITY.
;Written by Thomas Gail Haws
(defun c:haws-CS (/ sset mt mtclr mtlyr mtstr en1 ent entlst sslen)
  (haws-errdef 1)
  (prompt "\nText to change/<RETURN to change an attribute>:")
  (setq sset (ssget '((0 . "*TEXT"))))
  (cond
    ( (not sset)
      (setq en1 (car(nentsel "\nAttribute to change: ")))
      (if en1 (setq sset (ssadd) sset (ssadd en1 sset)))
    )
  )
  (if (not sset)
    (progn (prompt "\nNone found.")(exit))
    (progn
      (setq
        mt (entget (car (nentsel "Text to match:"))) mtstr (assoc 1 mt)
      )
      ;;; Change all of the entities in the selection set.
      (prompt "\nChanging text to match selection...")
      (command "._undo" "g")
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq entlst (entget (setq en1 (ssname sset (setq sslen (1- sslen))))))
        (entmod (subst mtstr(assoc 1 entlst)entlst))
        (entupd en1)
      )
      (command "._undo" "e")
      (prompt "done.")
  ) )
  (HAWS-ERRRST)
  (princ)
)
