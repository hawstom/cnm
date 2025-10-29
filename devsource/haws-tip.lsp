;;; haws-tip.lsp - Tip/evangelism opt-out system for HAWS/EDC/CNM
;;; See devtools/docs/standards_03_names_and_symbols.md for naming conventions
;;; See devtools/docs/standards_05_architecture.md S05.6 for tip system architecture
;; Get list of hidden tip IDs from AutoCAD config storage
(defun haws_tip_hide_list (/ hide_list_str hide_list)
  (setq hide_list_str (haws-readcfg (list "HawsEDC" "TipsHidden")))
  (cond
    ((and
        hide_list_str  ; key exists from getcfg (not nil)
        (/= hide_list_str "") ; value is not empty
      )
      (setq hide_list (read hide_list_str))
    )
  )
  hide_list  ; returns nil if not set, which is equivalent to '()
)

;; Save list of hidden tip IDs to AutoCAD config storage
(defun haws_tip_save_hide_list (lst)
  (haws-writecfg (list "HawsEDC" "TipsHidden") (vl-prin1-to-string lst))
  lst
)
;; Show a tip if not opted out. TIP_ID is a unique integer, MSG is the tip string.
(defun haws_tip_show (tip_id msg) 
  (if (not (member tip_id (haws_tip_hide_list))) 
    (progn 
      (haws_tip_dialog tip_id msg)
    )
  )
)

;; Show tip dialog with opt-out checkbox using DCL
(defun haws_tip_dialog (tip_id msg) 
  (setq dcl_id (load_dialog "haws-tip.dcl"))
  (if (not (new_dialog "haws_tip" dcl_id)) 
    (progn (unload_dialog dcl_id) (alert msg))
    (progn 
      (set_tile "tip_msg" msg)
      (set_tile "opt_in" "1")
      (action_tile "opt_in" 
                   (strcat "(if (= $value \"0\") (HAWS_TIP_HIDE " 
                           (itoa tip_id)
                           "))"
                   )
      )
      (start_dialog)
      (unload_dialog dcl_id)
    )
  )
)

;; Hide a tip by adding its ID to the hidden list
(defun haws_tip_hide (tip_id) 
  (haws_tip_save_hide_list (cons tip_id (vl-remove tip_id (haws_tip_hide_list))))
  (princ (strcat "\nTip " (itoa tip_id) " will no longer be shown."))
)

;; Reset all hidden tips (opt back in to all tips) - useful for testing
(defun haws_tip_reset () 
  (haws_tip_save_hide_list '())
  (princ "\nAll tips reset. You will now see all tips again.")
  (princ)
)