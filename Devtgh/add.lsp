;Written by Thomas Gail Haws
;ADD
(defun c:haws-ADD (/ sset addt addtstr ent entlst sslen stri enta1)
  (HAWS-ERDF$@ 1)
  (prompt "\nText to change:")(setq sset (ssget '((0 . "*TEXT"))))
  (if (not sset)
    (progn (prompt "\nNone found.")(exit))
    (progn
      (setq
        addt (entget (car (nentsel "Text to add:"))) addtstr (cdr(assoc 1 addt))
      )
      ;;; Change all of the entities in the selection set.
      (prompt "\nAdding selected string...")
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq
          entlst (entget (ssname sset (setq sslen (1- sslen))))
          stri (cdr (setq enta1 (assoc 1 entlst)))
        )
        (entmod (subst (cons 1(strcat stri " " addtstr)) enta1 entlst))
      )
      (prompt "done.")
  ) )
  (HAWS-ERRRST)
  (princ)
)
