;;; haws-tip.lsp - Tip/evangelism opt-out system for HAWS/EDC/CNM
;;; See HAWS-CODE-STYLE-GUIDELINES.md for conventions

;; Opt-out list file (LISP list of tip IDs)
(SETQ *HAWS_TIP_HIDE_FILE* "haws-tip-hide.lsp")

(DEFUN HAWS_TIP_HIDE_LIST (/ FNAME RESULT) 
  (AND
    (SETQ FNAME  (FINDFILE *HAWS_TIP_HIDE_FILE*))
    (SETQ F1     (OPEN FNAME "r"))
    (SETQ RESULT (READ-LINE F1))
    (SETQ RESULT (READ RESULT))
    (SETQ F1     (CLOSE F1))
  )
  RESULT
)
(DEFUN HAWS_TIP_SAVE_HIDE_LIST (LST / FNAME) 
  (AND
    (SETQ FNAME  (FINDFILE *HAWS_TIP_HIDE_FILE*))
    (SETQ F2     (OPEN FNAME "w"))
    (PRIN1 LST F2)
    (SETQ F2     (CLOSE F2))
  )
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
(DEFUN HAWS_TIP_HIDE (TIP_ID) 
  (HAWS_TIP_SAVE_HIDE_LIST (CONS TIP_ID (VL-REMOVE TIP_ID (HAWS_TIP_HIDE_LIST))))
  (PRINC (STRCAT "\nTip " (ITOA TIP_ID) " will no longer be shown."))
)