;CMT CHANGES TEXT TO MATCH A SELECTED TEXT ENTITY.
(defun c:haws-CMT (/ sset mt mtclr mtlyr ent entlst sslen)
  (haws-core-init 178)
  (prompt "\nText to change:")(setq sset (ssget '((0 . "*TEXT"))))
  (if (not sset)
    (progn (prompt "\nNone found.")(exit))
    (progn
      (setq
        mt (entget (car (entsel "Text to match:"))) mtclr (cdr (assoc 62 mt))
        mtlyr (cdr (assoc 8 mt))
      )
      ;;; Change all of the entities in the selection set.
      (prompt "\nChanging text to match selection...")
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq ent (ssname sset (setq sslen (1- sslen))))
        (setq entlst (entget ent))
        (foreach group '(7 40 41 51)
          (setq entlst
            (subst(cons group(cdr(assoc group mt)))(assoc group entlst)entlst)
          )
        )
        (entmod entlst)
        (if (not mtclr)(setq mtclr "BYLAYER"))
        (if (not mtlyr)(setq mtlyr "BYLAYER"))
        (command "._change" ent "" "_p" "_la" mtlyr "_c" mtclr "")
      )
      (prompt "done.")
    )
  )
  (haws-core-restore)
  (princ)
)
