;;; haws-tip.lsp - Tip/evangelism opt-out system for HAWS/EDC/CNM
;;; See HAWS-CODE-STYLE-GUIDELINES.md for conventions

;; Opt-out list file (LISP list of tip IDs)
(setq *HAWS_TIP_HIDE_FILE* "haws-tip-hide.lsp")

(defun HAWS_TIP_HIDE_LIST ()
  (if (findfile *HAWS_TIP_HIDE_FILE*)
    (read (open *HAWS_TIP_HIDE_FILE* "r"))
    '()
  )
)

(defun HAWS_TIP_SAVE_HIDE_LIST (lst)
  (with-open-file (f *HAWS_TIP_HIDE_FILE* :direction :output :if-exists :supersede)
    (vl-prin1-to-string lst f)
  )
  lst
)

;; Show a tip if not opted out. TIP_ID is a unique integer, MSG is the tip string.
(defun HAWS_TIP_SHOW (TIP_ID MSG)
  (if (not (member TIP_ID (HAWS_TIP_HIDE_LIST)))
    (progn
      (HAWS_TIP_DIALOG TIP_ID MSG)
    )
  )
)

;; Placeholder for dialog call

;; Show tip dialog with opt-out checkbox using DCL
(defun HAWS_TIP_DIALOG (TIP_ID MSG)
  (setq dcl_id (load_dialog "haws-tip.dcl"))
  (if (not (new_dialog "haws_tip" dcl_id))
    (progn (unload_dialog dcl_id) (alert MSG))
    (progn
      (set_tile "tip_msg" MSG)
      (set_tile "opt_out" "0")
      (action_tile "opt_out" 
        (strcat "(if (= $value \"1\") (HAWS_TIP_HIDE " (itoa TIP_ID) "))"))
      (start_dialog)
      (unload_dialog dcl_id)
    )
  )
)

(defun HAWS_TIP_HIDE (TIP_ID)
  (HAWS_TIP_SAVE_HIDE_LIST (cons TIP_ID (remove TIP_ID (HAWS_TIP_HIDE_LIST))))
  (princ (strcat "\nTip " (itoa TIP_ID) " will no longer be shown."))
)
