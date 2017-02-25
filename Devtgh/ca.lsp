;;CA (COPY ATTRIBUTES) CHANGES ATTRIBUTES IN A BLOCK TO MATCH A SELECTED BLOCK.
;;(C) Copyright 2001 by Thomas Gail Haws
(defun
   c:haws-CA
       (/ at av el en et sset mlist mv en sslen)
    (HAWS-ERDF$@ 1)
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
  (HAWS-ERRRST) ;_ end of if
  (princ)
) ;_ end of defun

;;; Attribute Set/Get Fields/TEXT
;;; blk = The attribute block
;;; mode = T for Field Values
;;;      = nil for String Values
;;; lst = If supplied, function in Set Mode
;;;    = nil for Get Mode
(defun haws-FieldsVal (blk mode lst / m f)
    (setq f (if	mode
	      (lambda (e)
		(strcat "%<\\AcObjProp Object(%<\\_ObjId "
			(itoa (vla-get-ObjectId e))
			">%).TextString>%")
	      )
	      (lambda (e) (vla-get-textstring e))
	    )
    )
    (mapcar '(lambda (at)
	       (if lst
		 (if (setq m (assoc (vla-get-tagstring at) lst))
		   (vla-put-textstring at (cadr m))
		 )
		 (list (vla-get-tagstring at) (f at))
	       )
	       )
	    (vlax-invoke (vlax-ename->vla-object blk) 'Getattributes)
    )
  )