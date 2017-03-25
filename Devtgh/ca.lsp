;;CA (COPY ATTRIBUTES) CHANGES ATTRIBUTES IN A BLOCK TO MATCH A SELECTED BLOCK.
;;(C) Copyright 2001 by Thomas Gail Haws
(defun
   c:haws-CA
       (/ at av el en et sset mlist mv en sslen)
    (haws-borrow 1)
  (prompt "\nBlocks to change: ")
  (setq sset (ssget '((0 . "INSERT"))))
  (if (not sset)
    (progn (prompt "\nNone found.") (exit))
    (progn
      (setq
        en (car (entsel "Block to match/<Match single attribute>: "))
      ) ;_ end of setq
      (if en
        (while
          (and
            (setq en (entnext en))
            (/= "SEQEND"
                (setq et (cdr (assoc 0 (setq el (entget en)))))
            ) ;_ end of /=
          ) ;_ end of and
           (cond
             ((= et "ATTRIB")
              (setq
                at    (cdr (assoc 2 el))
                av    (cdr (assoc 1 el))
                mlist (cons (list at av) mlist)
              ) ;_ end of setq
             )
           ) ;_ end of cond
        ) ;_ end of while
        (if (setq
              en (car
                   (nentsel "\nAttribute to match/<enter by typing>: ")
                 ) ;_ end of car
            ) ;_ end of setq
          (setq
            el    (entget en)
            mlist (list (list (cdr (assoc 2 el)) (cdr (assoc 1 el))))
          ) ;_ end of setq
          (setq
            mlist
             (list
               (list
                 (strcase
                   (getstring "\nTag of attribute to change: ")
                 ) ;_ end of strcase
                 (getstring "\nNew value: ")
               ) ;_ end of list
             ) ;_ end of list
          ) ;_ end of setq
        ) ;_ end of if
      ) ;_ end of if
;;; Change all of the entities in the selection set.
      (prompt "\nChanging blocks to match selection...")
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq
          en (ssname sset (setq sslen (1- sslen)))
        ) ;_ end of setq
        (while
          (and
            (setq en (entnext en))
            (/= "SEQEND"
                (setq et (cdr (assoc 0 (setq el (entget en)))))
            ) ;_ end of /=
          ) ;_ end of and
           (cond
             ((and
                (= et "ATTRIB")
                (setq
                  at (cdr (assoc 2 el))
                ) ;_ end of setq
                (assoc at mlist)
              ) ;_ end of and
              (entmod
                (subst (cons 1 (cadr (assoc at mlist))) (assoc 1 el) el)
              ) ;_ end of entmod
              (entupd en)
             )
           ) ;_ end of cond
        ) ;_ end of while
      ) ;_ end of while
      (prompt "done.")
    ) ;_ end of progn
  ) ;_ end of if
  (haws-return) ;_ end of if
  (princ)
) ;_ end of defun