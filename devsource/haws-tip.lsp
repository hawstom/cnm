;;; haws-tip.lsp - Tip opt-out system for HAWS/EDC/CNM
;;; See devtools/docs/standards_03_names_and_symbols.md for naming conventions
;;; See devtools/docs/standards_05_architecture.md S05.6 for tip system architecture
;;;
;;; TIP ID REGISTRY:
;;; AI agents implementing new tips should use the courtesy registry at:
;;;   devsource/haws-tip-registry.csv
;;; This prevents ID collisions and provides documentation of all tips.
;;; Format: tip-id,description,implementation-date,status
;;; See registry file for instructions on adding new tips.

(princ "\nHaws-tip functions ... ")

;;; ============================================================================
;;; HAWS-CONFIG REGISTRATION
;;; ============================================================================
;; Register TIP app with haws-config
;; Storage: Windows Registry (scope 4 = User)
;; Format: Single assoc list for all tip states
;;         '((tip-id . "timestamp-or-0") ...)
;;         where "0" = hidden forever, timestamp = snoozed until date
(haws-config-register-app "TIP"
  '(("SnoozeData" "" 4))   ; All tip snooze/hide data in one variable
)

;;; ============================================================================
;;; INTERNAL HELPER FUNCTIONS
;;; ============================================================================

;; Get all snooze data as assoc list
;; Returns: '((1002 . "12345.67890") (1005 . "0") ...) or nil
(defun haws-tip-get-all-snooze (/ data-str)
  (setq data-str (haws-config-getvar "TIP" "SnoozeData" nil nil))
  (if (and data-str (/= data-str ""))
    (read data-str)
    nil
  )
)

;; Save all snooze data from assoc list
;; snooze-alist: '((tip-id . "value") ...)
(defun haws-tip-set-all-snooze (snooze-alist)
  (haws-config-setvar "TIP" "SnoozeData" 
    (if snooze-alist
      (vl-prin1-to-string snooze-alist)
      ""
    )
    nil nil
  )
  snooze-alist
)

;;; ============================================================================
;;; PUBLIC API FUNCTIONS
;;; ============================================================================

;; Get snooze until value for a tip ID
;; tip-id: Can be integer or string
;; Returns nil if not snoozed, 0 if snoozed forever, or snooze-until (real number)
(defun haws-tip-get-snooze (tip-id / tip-id-num snooze-alist pair snooze-str)
  (setq tip-id-num (if (numberp tip-id) tip-id (atoi (vl-princ-to-string tip-id))))
  (setq snooze-alist (haws-tip-get-all-snooze))
  (setq pair (assoc tip-id-num snooze-alist))
  (if pair
    (progn
      (setq snooze-str (cdr pair))
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
    nil  ; Not in list = not snoozed
  )
)

;; Set snooze for a tip ID
;; tip-id: Can be integer or string
;; snooze-days: number of days to snooze, or 0 for forever
(defun haws-tip-set-snooze (tip-id snooze-days / tip-id-num snooze-alist snooze-until snooze-str pair)
  (setq tip-id-num (if (numberp tip-id) tip-id (atoi (vl-princ-to-string tip-id))))
  (setq snooze-alist (haws-tip-get-all-snooze))
  
  ;; Calculate snooze value
  (setq snooze-str
    (if (= snooze-days 0)
      "0"  ; Forever
      (progn
        (setq snooze-until (+ (getvar "DATE") snooze-days))
        (rtos snooze-until 2 8)
      )
    )
  )
  
  ;; Update or add to assoc list
  (setq pair (assoc tip-id-num snooze-alist))
  (if pair
    ;; Update existing
    (setq snooze-alist (subst (cons tip-id-num snooze-str) pair snooze-alist))
    ;; Add new
    (setq snooze-alist (cons (cons tip-id-num snooze-str) snooze-alist))
  )
  
  ;; Save and return
  (haws-tip-set-all-snooze snooze-alist)
  snooze-days
)

;; Clear snooze for a tip ID (resurrect it)
;; tip-id: Can be integer or string
(defun haws-tip-clear-snooze (tip-id / tip-id-num snooze-alist pair)
  (setq tip-id-num (if (numberp tip-id) tip-id (atoi (vl-princ-to-string tip-id))))
  (setq snooze-alist (haws-tip-get-all-snooze))
  
  ;; Remove from assoc list
  (setq pair (assoc tip-id-num snooze-alist))
  (if pair
    (progn
      (setq snooze-alist (vl-remove pair snooze-alist))
      (haws-tip-set-all-snooze snooze-alist)
    )
  )
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

;; Legacy functions for backward compatibility (deprecated)
;; These existed to support a separate "hidden" list, but now
;; hidden tips are just snooze = 0 in the main SnoozeData
(defun haws-tip-hide-list (/ snooze-alist result)
  ;; Return list of tip IDs that are hidden forever (snooze = "0")
  (setq snooze-alist (haws-tip-get-all-snooze))
  (setq result '())
  (foreach pair snooze-alist
    (if (equal (cdr pair) "0")
      (setq result (cons (car pair) result))
    )
  )
  result
)

(defun haws-tip-save-hide-list (lst / snooze-alist pair)
  ;; Convert list of tip IDs to snooze = "0" entries
  ;; First, remove all existing "0" entries
  (setq snooze-alist (haws-tip-get-all-snooze))
  (foreach pair snooze-alist
    (if (equal (cdr pair) "0")
      (setq snooze-alist (vl-remove pair snooze-alist))
    )
  )
  ;; Then add new "0" entries for provided list
  (foreach tip-id lst
    (setq snooze-alist (cons (cons tip-id "0") snooze-alist))
  )
  (haws-tip-set-all-snooze snooze-alist)
  lst
)
;; Show a tip if not snoozed. TIP-ID is a unique integer or string, MSG is the tip string.
;; This is the main public API function
(defun haws-tip (tip-id msg / is-snoozed) 
  ;; Show tip if not snoozed
  (setq is-snoozed (haws-tip-is-snoozed tip-id))
  (if (not is-snoozed)
    (progn 
      (haws-tip-dialog tip-id msg)
    )
  )
)

;; Backward compatibility alias
(defun haws-tip-show (tip-id msg)
  (haws-tip tip-id msg)
)

;; Show tip dialog with snooze options using DCL
(defun haws-tip-dialog (tip-id msg / dcl-id snooze-choice opt-in-val dialog-result done-code) 
  (setq dcl-id (load_dialog "haws-tip.dcl"))
  (if (not dcl-id)
    (alert (princ msg))  ; Fallback if DCL can't load
    (progn
      ;; Initialize state
      (setq snooze-choice "0")  ; Default to "No snooze" (index 5)
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
                
                ;; Snooze dropdown action  
                (action_tile "snooze_dropdown" "(setq snooze-choice $value)")
                
                ;; Set dropdown value - DCL default is already "5" so only set if different
                (if (/= snooze-choice "0")
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
            (setq snooze-choice "0")  ; Reset to "No snooze"
            (setq done-code 2)  ; Return to dialog
          )
          
          ;; User clicked OK (code 1)
          ((= done-code 1)
            ;; Apply snooze duration if selected (not "No snooze")
            (if (/= snooze-choice "0")  ; "No snooze" is index 0
              (cond
                ((= snooze-choice "1") (haws-tip-set-snooze tip-id 1))
                ((= snooze-choice "2") (haws-tip-set-snooze tip-id 7))
                ((= snooze-choice "3") (haws-tip-set-snooze tip-id 30))
                ((= snooze-choice "4") (haws-tip-set-snooze tip-id 180)) 
                ((= snooze-choice "5") (haws-tip-set-snooze tip-id 0))  ; Forever
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

;; Command: Clear snooze for a specific tip (resurrect one tip)
(defun c:haws-tip-show-1002 ()
  (princ "\n=== Clearing snooze for tip 1002...")
  (princ (strcat "\n=== Before: snoozed=" (vl-princ-to-string (haws-tip-is-snoozed 1002))))
  (haws-tip-clear-snooze 1002)
  (princ (strcat "\n=== After: snoozed=" (vl-princ-to-string (haws-tip-is-snoozed 1002))))
  (princ "\nTip 1002 (Auto Text and Editing) has been re-enabled.")
  (princ)
)

;; Reset all snoozed tips (resurrect all tips)
(defun haws-tip-reset () 
  (haws-tip-resurrect-all)
  (princ "\nAll tips reset. You will now see all tips again.")
  (princ)
)

;; Resurrect all tips by clearing all snooze settings
;; NEW: Simply clear the single storage variable (MUCH more efficient!)
(defun haws-tip-resurrect-all (/)
  ;; Clear all snooze data (includes both timed snoozes and hidden tips)
  (haws-config-setvar "TIP" "SnoozeData" "" nil nil)
  (princ)
) 
(princ "loaded.")
(princ)