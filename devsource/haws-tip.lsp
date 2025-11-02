;;; haws-tip.lsp - Tip/evangelism opt-out system for HAWS/EDC/CNM
;;; See devtools/docs/standards_03_names_and_symbols.md for naming conventions
;;; See devtools/docs/standards_05_architecture.md S05.6 for tip system architecture

;; Get snooze until value for a tip ID
;; tip-id: Can be integer or string
;; Returns nil if not snoozed, 0 if snoozed forever, or snooze-until (real number)
(princ "\nHaws-Tip functions ... ")
(defun haws-tip-get-snooze (tip-id / key-path snooze-str tip-id-str)
  (setq tip-id-str (if (numberp tip-id) (itoa tip-id) (vl-princ-to-string tip-id)))
  (setq key-path (list "HawsEDC" (strcat "Tip" tip-id-str "Snooze")))
  (setq snooze-str (haws-readcfg key-path))
  (cond
    ((or (not snooze-str) (= snooze-str ""))
      nil  ; Not snoozed
    )
    ((= snooze-str "0")
      0  ; Snoozed forever
    )
    (t
      (atof snooze-str)  ; Return snooze-until as real number
    )
  )
)

;; Set snooze for a tip ID
;; tip-id: Can be integer or string
;; snooze-days: number of days to snooze, or 0 for forever
(defun haws-tip-set-snooze (tip-id snooze-days / key-path snooze-until tip-id-str)
  (setq tip-id-str (if (numberp tip-id) (itoa tip-id) (vl-princ-to-string tip-id)))
  (setq key-path (list "HawsEDC" (strcat "Tip" tip-id-str "Snooze")))
  (if (= snooze-days 0)
    (haws-writecfg key-path "0")  ; Forever
    (progn
      (setq snooze-until (+ (getvar "DATE") snooze-days))
      (haws-writecfg key-path (rtos snooze-until 2 8))
    )
  )
  snooze-days
)

;; Clear snooze for a tip ID (resurrect it)
;; tip-id: Can be integer or string
(defun haws-tip-clear-snooze (tip-id / key-path tip-id-str)
  (setq tip-id-str (if (numberp tip-id) (itoa tip-id) (vl-princ-to-string tip-id)))
  (setq key-path (list "HawsEDC" (strcat "Tip" tip-id-str "Snooze")))
  (haws-writecfg key-path "")  ; Empty string means not snoozed
  nil
)

;; Check if a tip is currently snoozed
;; Returns T if snoozed, nil if should be shown
(defun haws-tip-is-snoozed (tip-id / snooze-until)
  (setq snooze-until (haws-tip-get-snooze tip-id))
  (cond
    ((not snooze-until)
      nil  ; Not snoozed
    )
    ((= snooze-until 0)
      T  ; Snoozed forever
    )
    (t
      (< (getvar "DATE") snooze-until)  ; Still snoozed if current < snooze-until
    )
  )
)

;;; ============================================================================
;;; TIP REGISTRY - Universal tip ID collision prevention
;;; ============================================================================

;; Normalize tip ID to lowercase string for case-insensitive comparison
;; Accepts: integer or string
;; Returns: lowercase string
(defun haws-tip-normalize-id (tip-id)
  (strcase 
    (if (numberp tip-id)
      (itoa tip-id)
      (vl-princ-to-string tip-id)
    )
    T  ; T = convert to lowercase
  )
)

;; Load tip registry from CSV file
;; Returns: List of (normalized-id . note) pairs, or nil if file doesn't exist
;; File format: tip-id,note (CSV with optional header and comments starting with #)
(defun haws-tip-registry-load (/ registry-file file line-str parts id note result)
  (setq registry-file (findfile "haws-tip-registry.csv"))
  (if registry-file
    (progn
      (setq file (open registry-file "r"))
      (if file
        (progn
          (setq result nil)
          ;; Read each line from file
          (while (setq line-str (read-line file))
            ;; Skip comments, empty lines, and header row
            (if (and
                  line-str
                  (/= line-str "")
                  (/= (substr line-str 1 1) "#")
                  (/= (strcase line-str) "TIP-ID,NOTE")
                )
              (progn
                ;; Split on comma
                (setq parts (haws-tip-split-csv line-str))
                (if (>= (length parts) 1)
                  (progn
                    (setq id (haws-tip-normalize-id (car parts)))
                    (setq note (if (>= (length parts) 2) (cadr parts) ""))
                    (setq result (cons (cons id note) result))
                  )
                )
              )
            )
          )
          (close file)
          (reverse result)  ; Return in file order
        )
        nil  ; File couldn't be opened
      )
    )
    nil  ; File not found
  )
)

;; Split CSV line on comma (simple implementation, doesn't handle quoted commas)
;; Returns: list of strings
(defun haws-tip-split-csv (line-str / comma-pos parts)
  (setq parts nil)
  (while (setq comma-pos (vl-string-search "," line-str))
    (setq parts (cons (substr line-str 1 comma-pos) parts))
    (setq line-str (substr line-str (+ comma-pos 2)))
  )
  (setq parts (cons line-str parts))
  (reverse parts)
)

;; Check if tip ID exists in registry (case-insensitive)
;; registry: List of (id . note) pairs from haws-tip-registry-load
;; Returns: Note string if found, nil if not found
(defun haws-tip-registry-find (tip-id registry / normalized-id entry)
  (setq normalized-id (haws-tip-normalize-id tip-id))
  (setq entry (assoc normalized-id registry))
  (if entry
    (cdr entry)
    nil
  )
)

;; Check if tip ID is registered and warn if not
;; This is called by haws-tip-show to provide collision prevention
;; Returns: T if registered or registry not available, nil never (always continues)
(defun haws-tip-check-registry (tip-id / registry note)
  (setq registry (haws-tip-registry-load))
  (if registry
    (progn
      (setq note (haws-tip-registry-find tip-id registry))
      (if note
        (princ (strcat "\nTip ID " (haws-tip-normalize-id tip-id) " registered: " note))
        (princ (strcat "\nWARNING: Tip ID " (haws-tip-normalize-id tip-id) " is not registered in haws-tip-registry.csv"))
      )
    )
    ;; Registry file not found - silently continue
    (princ (strcat "\nNote: Tip registry not found (optional)"))
  )
  T  ; Always return T to allow tip to continue
)


;; Legacy functions for backward compatibility (deprecated)
(defun haws-tip-hide-list (/ hide-list-str hide-list)
  (setq hide-list-str (haws-readcfg (list "HawsEDC" "TipsHidden")))
  (cond
    ((and
        hide-list-str  ; key exists from getcfg (not nil)
        (/= hide-list-str "") ; value is not empty
      )
      (setq hide-list (read hide-list-str))
    )
  )
  hide-list  ; returns nil if not set, which is equivalent to '()
)

(defun haws-tip-save-hide-list (lst)
  (haws-writecfg (list "HawsEDC" "TipsHidden") (vl-prin1-to-string lst))
  lst
)
;; Show a tip if not snoozed. TIP-ID is a unique integer or string, MSG is the tip string.
(defun haws-tip-show (tip-id msg) 
  ;; Check registry for collision prevention (optional, warns only)
  (haws-tip-check-registry tip-id)
  ;; Show tip if not snoozed
  (if (not (haws-tip-is-snoozed tip-id))
    (progn 
      (haws-tip-dialog tip-id msg)
    )
  )
)

;; Show tip dialog with snooze options using DCL
(defun haws-tip-dialog (tip-id msg / dcl-id snooze-choice opt-in-val dialog-result done-code) 
  (setq dcl-id (load_dialog "haws-tip.dcl"))
  (if (not dcl-id)
    (alert (princ msg))  ; Fallback if DCL can't load
    (progn
      ;; Initialize state
      (setq opt-in-val "1")
      (setq snooze-choice "4")  ; Default to "No snooze" (index 4)
      (setq done-code 2)  ; 2 = show dialog initially
      
      ;; Dialog loop - keeps showing until done-code = -1
      (while (>= done-code 0)
        (cond
          ;; Show the dialog
          ((= done-code 2)
            (if (not (new_dialog "haws_tip" dcl-id))
              (setq done-code -1)  ; Exit if dialog fails
              (progn
                (set_tile "tip_msg" msg)
                (set_tile "opt_in" opt-in-val)
                
                ;; Opt-in checkbox action
                (action_tile "opt_in" "(setq opt-in-val $value)")
                
                ;; Snooze dropdown action  
                (action_tile "snooze_dropdown" "(setq snooze-choice $value)")
                
                ;; Set dropdown value - DCL default is already "4" so only set if different
                (if (/= snooze-choice "4")
                  (set_tile "snooze_dropdown" snooze-choice)
                )
                
                ;; Resurrect button action - returns special code
                (action_tile "resurrect_btn" "(done_dialog 99)")
                
                ;; OK button action
                (action_tile "accept" "(done_dialog 1)")
                
                ;; Cancel button action
                (action_tile "cancel" "(done_dialog 0)")
                
                ;; Show dialog and wait for user
                (setq done-code (start_dialog))
              )
            )
          )
          
          ;; User clicked Resurrect button (code 99)
          ((= done-code 99)
            (princ "\n** Resurrect all tips? **")
            (princ "\nAll snoozed tips will be shown again.")
            (initget "Yes No")
            (if (= "Yes" (getkword "\nContinue? [Yes/No] <No>: "))
              (progn
                (haws-tip-resurrect-all)
                (princ "\n** All tips resurrected! **")
              )
            )
            ;; Reset dialog state before showing again
            (setq snooze-choice "4")  ; Reset to "No snooze"
            (setq opt-in-val "1")     ; Reset to checked
            (setq done-code 2)  ; Return to dialog
          )
          
          ;; User clicked OK (code 1)
          ((= done-code 1)
            (cond
              ;; If opt-in unchecked, hide forever (overrides snooze choice)
              ((= opt-in-val "0")
                (haws-tip-set-snooze tip-id 0)
              )
              ;; If snooze selected (not "No snooze"), apply duration
              ((/= snooze-choice "4")  ; "No snooze" is now index 4
                (cond
                  ((= snooze-choice "0") (haws-tip-set-snooze tip-id 7))
                  ((= snooze-choice "1") (haws-tip-set-snooze tip-id 30))
                  ((= snooze-choice "2") (haws-tip-set-snooze tip-id 180))
                  ((= snooze-choice "3") (haws-tip-set-snooze tip-id 0))  ; Forever
                )
              )
            )
            (setq done-code -1)  ; Exit loop
          )
          
          ;; User clicked Cancel (code 0)
          ((= done-code 0)
            (setq done-code -1)  ; Exit loop without saving
          )
        )
      )
      
      (unload_dialog dcl-id)
    )
  )
  (princ)
)

;; Hide a tip forever (for backward compatibility and direct use)
;; tip-id: Can be integer or string
(defun haws-tip-hide (tip-id / tip-id-str) 
  (setq tip-id-str (if (numberp tip-id) (itoa tip-id) (vl-princ-to-string tip-id)))
  (haws-tip-set-snooze tip-id 0)
  (princ (strcat "\nTip " tip-id-str " will no longer be shown."))
)

;; Reset all snoozed tips (resurrect all tips)
(defun haws-tip-reset () 
  (haws-tip-resurrect-all)
  (princ "\nAll tips reset. You will now see all tips again.")
  (princ)
)

;; Resurrect all tips by clearing all snooze settings
;; This function scans for Tip*Snooze keys and clears them
(defun haws-tip-resurrect-all (/ tip-id)
  ;; Clear snooze for tip IDs 1-999 (reasonable range)
  (setq tip-id 1)
  (while (<= tip-id 999)
    (if (haws-tip-get-snooze tip-id)
      (haws-tip-clear-snooze tip-id)
    )
    (setq tip-id (1+ tip-id))
  )
  ;; Also clear legacy TipsHidden list for complete reset
  (haws-writecfg (list "HawsEDC" "TipsHidden") "")
  (princ)
)
(princ "loaded.")
(princ)