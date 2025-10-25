;;; haws-tip.lsp - Tip/evangelism opt-out system for HAWS/EDC/CNM
;;; See HAWS-CODE-STYLE-GUIDELINES.md for conventions

;;; haws-tip.lsp - Tip/evangelism opt-out system for HAWS/EDC/CNM
;;; See HAWS-CODE-STYLE-GUIDELINES.md for conventions

;; Get list of hidden tip IDs from AutoCAD config storage
(DEFUN HAWS_TIP_HIDE_LIST (/ HIDE_LIST_STR HIDE_LIST)
  (SETQ HIDE_LIST_STR (HAWS-READCFG (LIST "HawsEDC" "TipsHidden")))
  (COND
    ((AND
        HIDE_LIST_STR  ; key exists from getcfg (not nil)
        (/= HIDE_LIST_STR "") ; value is not empty
      )
      (SETQ HIDE_LIST (READ HIDE_LIST_STR))
    )
  )
  HIDE_LIST  ; returns nil if not set, which is equivalent to '()
)

;; Save list of hidden tip IDs to AutoCAD config storage
(DEFUN HAWS_TIP_SAVE_HIDE_LIST (LST)
  (HAWS-WRITECFG (LIST "HawsEDC" "TipsHidden") (VL-PRIN1-TO-STRING LST))
  LST
)
;; Show a tip if not opted out. TIP_ID is a unique integer, MSG is the tip string.
(DEFUN HAWS_TIP_SHOW (TIP_ID MSG) 
  (IF (NOT (MEMBER TIP_ID (HAWS_TIP_HIDE_LIST))) 
    (PROGN 
      (HAWS_TIP_DIALOG TIP_ID MSG)
    )
  )
)

;; Show tip dialog with opt-out checkbox using DCL
(DEFUN HAWS_TIP_DIALOG (TIP_ID MSG) 
  (SETQ DCL_ID (LOAD_DIALOG "haws-tip.dcl"))
  (IF (NOT (NEW_DIALOG "haws_tip" DCL_ID)) 
    (PROGN (UNLOAD_DIALOG DCL_ID) (ALERT MSG))
    (PROGN 
      (SET_TILE "tip_msg" MSG)
      (SET_TILE "opt_in" "1")
      (ACTION_TILE "opt_in" 
                   (STRCAT "(if (= $value \"0\") (HAWS_TIP_HIDE " 
                           (ITOA TIP_ID)
                           "))"
                   )
      )
      (START_DIALOG)
      (UNLOAD_DIALOG DCL_ID)
    )
  )
)

;; Hide a tip by adding its ID to the hidden list
(DEFUN HAWS_TIP_HIDE (TIP_ID) 
  (HAWS_TIP_SAVE_HIDE_LIST (CONS TIP_ID (VL-REMOVE TIP_ID (HAWS_TIP_HIDE_LIST))))
  (PRINC (STRCAT "\nTip " (ITOA TIP_ID) " will no longer be shown."))
)