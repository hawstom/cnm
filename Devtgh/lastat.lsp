;;;Freeze or off by selection set
;;;(C) Copyright 1997 by Thomas Gail Haws

(HAWS-MILEPOST "Loading lastat.lsp version a")
(DEFUN C:HAWS-FF () (HAWS-LASTAT "freeze" NIL)(princ))
(DEFUN C:HAWS-OFF () (HAWS-LASTAT "off" NIL)(princ))

(DEFUN
   HAWS-LASTAT (LOPERA NESTED)
  (haws-core-borrow 1)
  (HAWS-VSAVE '("EXPERT"))
  (SETVAR "EXPERT" 5)
  (INITGET "Yes No")
  (IF (AND
	(= LOPERA "freeze")
	(= (GETVAR "tilemode") 0)
	(/= (GETVAR "cvport") 1)
	(= (GETKWORD "\nFreeze in current viewport only? [Yes/No] <No>: ")
	   "Yes"
	)
      )
    (PROGN
      (PROMPT "\nPick layers to freeze in current viewport:")
      (COMMAND "._mspace" "._vplayer")
      (HAWS-LALOOP T)
      (COMMAND "" "")
    )
    (PROGN
      (PROMPT (STRCAT "\nPick layers for " LOPERA ":"))
      (COMMAND "._layer")
      (HAWS-LALOOP NIL)
      (COMMAND "")
    )
  )
  (HAWS-VRSTOR)
  (haws-core-return)
)

(DEFUN
   HAWS-LALOOP (VP / SS1 I EN LA LALIST)
  (SETQ
    LASSET
     (SSGET)
    I 0
  )
  (COND
    (LASSET
     (WHILE (SETQ EN (SSNAME LASSET I))
       (SETQ LA (CDR (ASSOC 8 (ENTGET EN))))
       (COMMAND LOPERA LA)
       (IF VP
	 (COMMAND "")
       )
       (SETQ I (1+ I))
       (IF (NOT (MEMBER LA LALIST))
	 (SETQ LALIST (CONS LA LALIST))
       )
     )
     (IF VP
       (SETQ LALIST (CONS 1 LALIST))
     )
     (SETQ
       *HAWS-LASTATLIST*
	(IF (ASSOC LOPERA *HAWS-LASTATLIST*)
	  (SUBST
	    (CONS LOPERA LALIST)
	    (ASSOC LOPERA *HAWS-LASTATLIST*)
	    *HAWS-LASTATLIST*
	  )
	  (CONS (CONS LOPERA LALIST) *HAWS-LASTATLIST*)
	)
     )
    )
  )
)

;;;(C) Copyright 1997 by Thomas Gail Haws
;;;Freeze or off by picking nested entities

(DEFUN C:HAWS-FFX () (HAWS-NLSTAT "freeze")(princ))
(DEFUN C:HAWS-OFFX () (HAWS-NLSTAT "off")(princ))

(DEFUN
   HAWS-NLSTAT (LOPERA / NESTED)
  (haws-core-borrow 1)
  (HAWS-VSAVE '("EXPERT"))
  (SETVAR "EXPERT" 5)
  (INITGET "Yes No")
  (IF (AND
	(= LOPERA "freeze")
	(= (GETVAR "tilemode") 0)
	(/= (GETVAR "cvport") 1)
	(= (GETKWORD "\nFreeze in current viewport only? [Yes/No] <No>: ")
	   "Yes"
	)
      )
    (PROGN
      (PROMPT "\nPick layers to freeze in current viewport:")
      (COMMAND "._mspace" "._vplayer")
      (HAWS-LSPICK LOPERA T)
      (COMMAND "")
    )
    (PROGN
      (PROMPT (STRCAT "\nPick layers for " LOPERA ":"))
      (COMMAND "._layer")
      (HAWS-LSPICK LOPERA NIL)
      (COMMAND "")
    )
  )
  (HAWS-VRSTOR)
  (haws-core-return)
)
(DEFUN
   HAWS-LSPICK (LOPERA VP / ES EN LA PARNTL LALIST LOPERAkey)
  (WHILE (SETQ ES (NENTSEL))
    (SETQ
      EN     (CAR ES)
      LA     (CDR (ASSOC 8 (ENTGET EN)))
      PARNTL (CADDDR ES)
    )
    (COND
      ((AND (= LA "0") (NOT PARNTL))
       (PROMPT (strcat "\nCan't find non-0 layer to " lopera "."))
      )
      (T
       (WHILE (= LA "0")
	 (SETQ LA (CDR (ASSOC 8 (ENTGET (CAR PARNTL)))))
	 (SETQ PARNTL (CDR PARNTL))
       )
       (REDRAW EN 3)
       (SETQ LALIST (CONS LA LALIST))
       (COMMAND LOPERA LA)
       (IF VP
	 (COMMAND "")
       )
       (SETQ
	 LOPERAKEY
	  (STRCAT "n" LOPERA)
	 *HAWS-LASTATLIST*
	  (IF (ASSOC LOPERAkey *HAWS-LASTATLIST*)
	    (SUBST
	      (CONS LOPERAkey LALIST)
	      (ASSOC LOPERAkey *HAWS-LASTATLIST*)
	      *HAWS-LASTATLIST*
	    )
	    (CONS
	      (CONS LOPERAkey LALIST)
	      *HAWS-LASTATLIST*
	    )
	  )
       )
      )
    )
  )
)

;;;Undo selection set freeze or off
;;;Written by Thomas Gail Haws
;;;*HAWS-LASTATLIST* is a list of layer lists in assoc groups
;;; "freeze" "nfreeze" "off" "noff"
;;; also, a freeze list may have a 1 as its second element which indicates vplayer freezing
(DEFUN C:HAWS-UFF () (HAWS-ULSTAT "freeze" "thaw")(princ))
(DEFUN C:HAWS-UFFX () (HAWS-ULSTAT "nfreeze" "thaw")(princ))
(DEFUN C:HAWS-UOFF () (HAWS-ULSTAT "off" "on")(princ))
(DEFUN C:HAWS-UOFFX () (HAWS-ULSTAT "noff" "on")(princ))
(DEFUN
   HAWS-ULSTAT (LKEY LOPERA / EN LT I)
  ;; Undo layers frozen/off by picking
  (COND
    ((NOT (ASSOC LKEY *HAWS-LASTATLIST*))
     (PROMPT "\nNo layer list found to undo.")
    )
    (T
     (COND
       ((= (CADR (ASSOC LKEY *HAWS-LASTATLIST*)) 1)
	(COMMAND "._vplayer")
	(FOREACH
	   LA (CDDR (ASSOC LKEY *HAWS-LASTATLIST*))
	  (COMMAND LOPERA LA "")
	)
	(PRINC
	  "\nThe last layer set frozen was viewport specific.\nPlease be sure to have the appropriate viewport current."
	)
       )
       (T
	(COMMAND "._layer")
	(FOREACH
	   LA (CDR (ASSOC LKEY *HAWS-LASTATLIST*))
	  (COMMAND LOPERA LA)
	)
       )
     )
     (COMMAND "")
    )
  )
  (PRINC)
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 T nil nil T)
;*** DO NOT add text below the comment! ***|;
