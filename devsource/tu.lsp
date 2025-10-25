;TO REMOVES ALL UNDERLINING AND OVERLINING AND
;COMPLETELY UNDERLINES THE TEXT IN A SELECTION SET.
;Written by Thomas Gail Haws
(defun c:haws-tu (/ sset en1 ent entlst i oldstr newstr sslen)
  (haws-core-init 317)
  (prompt "\nText to completely underline/<RETURN to underline an attribute>:")
  (setq sset (ssget '((0 . "*TEXT"))))
  (cond
    ( (not sset)
      (setq en1 (car(nentsel "\nAttribute to underline: ")))
      (if en1 (setq sset (ssadd) sset (ssadd en1 sset)))
    )
  )
  (if (not sset)
    (progn (prompt "\nNone found.")(exit))
    (progn
      ;;; Change all of the entities in the selection set.
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq
          entlst (entget (setq en1 (ssname sset (setq sslen (1- sslen)))))
          oldstr (cdr (assoc 1 entlst))
          i 0
          newstr ""
        )
        (while (/= "" (substr oldstr (setq i (1+ i)) 1))
          (if (wcmatch (strcase(substr oldstr i 3)) "%%O,%%U")(setq i (+ i 3)))
          (setq newstr (strcat newstr (substr oldstr i 1)))
        )
        (setq newstr (strcat "%%u" newstr))
        (entmod
          (subst
            (cons
              1
              newstr
            )
            (assoc 1 entlst)
            entlst
        ) )
        (entupd en1)
      )
      (prompt "done.")
  ) )
  (haws-core-restore)
  (princ)
)
