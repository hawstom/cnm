;;;Freeze or off by selection set
;;;(C) Copyright 1997 by Thomas Gail Haws

(haws-milepost "Loading lastat.lsp version a")
(defun c:haws-ff ()
(haws-core-init 229) (haws-lastat "freeze" nil)(princ))
(defun c:haws-lk ()
(haws-core-init 230) (haws-lastat "lock" nil)(princ))
(defun c:haws-off () (haws-lastat "off" nil)(princ))

(defun haws-lastat (lopera nested)
  (haws-core-init 231)
  (haws-vsave '("EXPERT"))
  (setvar "EXPERT" 5)
  (if (and
	(= lopera "freeze")
	(= (getvar "tilemode") 0)
	(/= (getvar "cvport") 1)
	(progn (initget "Yes No")(= (getkword "\nFreeze in current viewport only? [Yes/No] <No>: ")
	   "Yes"
	))
      )
    (progn
      (prompt "\nPick layers to freeze in current viewport:")
      (vl-cmdf "._mspace" "._vplayer")
      (haws-laloop t)
      (vl-cmdf "" "")
    )
    (progn
      (prompt (strcat "\nPick layers for " lopera ":"))
      (vl-cmdf "._layer")
      (haws-laloop nil)
      (vl-cmdf "")
    )
  )
  (haws-vrstor)
  (haws-core-restore)
)

(defun haws-laloop (vp / ss1 i en la lalist)
  (setq
    lasset
     (ssget)
    i 0
  )
  (cond
    (lasset
     (while (setq en (ssname lasset i))
       (setq la (cdr (assoc 8 (entget en))))
       (vl-cmdf (strcat "_" lopera) la)
       (if vp
	 (vl-cmdf "")
       )
       (setq i (1+ i))
       (if (not (member la lalist))
	 (setq lalist (cons la lalist))
       )
     )
     (if vp
       (setq lalist (cons 1 lalist))
     )
     (setq
       *haws-lastatlist*
	(if (assoc lopera *haws-lastatlist*)
	  (subst
	    (cons lopera lalist)
	    (assoc lopera *haws-lastatlist*)
	    *haws-lastatlist*
	  )
	  (cons (cons lopera lalist) *haws-lastatlist*)
	)
     )
    )
  )
)

;;;(C) Copyright 1997 by Thomas Gail Haws
;;;Freeze or off by picking nested entities

(defun c:haws-ffx ()
  (prompt (strcat "\n" (haws_evangel_msg)))
  (haws-core-init 232)
  (haws-nlstat "freeze")
  (princ))

(defun c:haws-offx ()
  (prompt (strcat "\n" (haws_evangel_msg)))
  (haws-core-init 233)
  (haws-nlstat "off")
  (princ))

(defun haws-nlstat (lopera / nested)
  (haws-vsave '("EXPERT"))
  (setvar "EXPERT" 5)
  (if (and
	(= lopera "freeze")
	(= (getvar "tilemode") 0)
	(/= (getvar "cvport") 1)
	(progn (initget "Yes No")(= (getkword "\nFreeze in current viewport only? [Yes/No] <No>: ")   "Yes"
	))
      )
    (progn
      (prompt "\nPick layers to freeze in current viewport:")
      (vl-cmdf "._mspace" "._vplayer")
      (haws-lspick lopera t)
      (vl-cmdf "")
    )
    (progn
      (prompt (strcat "\nPick layers for " lopera ":"))
      (vl-cmdf "._layer")
      (haws-lspick lopera nil)
      (vl-cmdf "")
    )
  )
  (haws-vrstor)
  (haws-core-restore)
)
(defun haws-lspick (lopera vp / es en la parntl lalist loperakey)
  (while (setq es (nentsel))
    (setq
      en     (car es)
      la     (cdr (assoc 8 (entget en)))
      parntl (cadddr es)
    )
    (cond
      ((and (= la "0") (not parntl))
       (prompt (strcat "\nCan't find non-0 layer to " lopera "."))
      )
      (t
       (while (= la "0")
	 (setq la (cdr (assoc 8 (entget (car parntl)))))
	 (setq parntl (cdr parntl))
       )
       (redraw en 3)
       (setq lalist (cons la lalist))
       (vl-cmdf (strcat "_" lopera) la)
       (if vp
	 (vl-cmdf "")
       )
       (setq
	 loperakey
	  (strcat "n" lopera)
	 *haws-lastatlist*
	  (if (assoc loperakey *haws-lastatlist*)
	    (subst
	      (cons loperakey lalist)
	      (assoc loperakey *haws-lastatlist*)
	      *haws-lastatlist*
	    )
	    (cons
	      (cons loperakey lalist)
	      *haws-lastatlist*
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
(defun c:haws-uff ()
(haws-core-init 234) (haws-ulstat "freeze" "thaw")(princ))
(defun c:haws-uffx ()
(haws-core-init 235) (haws-ulstat "nfreeze" "thaw")(princ))
(defun c:haws-uoff ()
(haws-core-init 236) (haws-ulstat "off" "on")(princ))
(defun c:haws-uoffx ()
(haws-core-init 237) (haws-ulstat "noff" "on")(princ))
(defun haws-ulstat (lkey lopera / en lt i)
  ;; Undo layers frozen/off by picking
  (cond
    ((not (assoc lkey *haws-lastatlist*))
     (prompt "\nNo layer list found to undo.")
    )
    (t
     (cond
       ((= (cadr (assoc lkey *haws-lastatlist*)) 1)
	(vl-cmdf "._vplayer")
	(foreach
	   la (cddr (assoc lkey *haws-lastatlist*))
	  (vl-cmdf (strcat "_" lopera) la "")
	)
	(princ
	  "\nThe last layer set frozen was viewport specific.\nPlease be sure to have the appropriate viewport current."
	)
       )
       (t
	(vl-cmdf "._layer")
	(foreach
	   la (cdr (assoc lkey *haws-lastatlist*))
	  (vl-cmdf (strcat "_" lopera) la)
	)
       )
     )
     (vl-cmdf "")
    )
  )
  (princ)
)
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 t nil nil t)
;*** DO NOT add text below the comment! ***|
