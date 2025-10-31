;;; haws-tip-test.lsp - Test functions for haws-tip snooze system
;;; Load haws-tip.lsp before using these tests

;; Test 1: Show a test tip
(defun c:test-tip-show ()
  (haws-tip-show 999 "This is a test tip!\n\nTry the snooze options:\n- 7 days\n- 30 days\n- 180 days\n- Forever\n- No snooze\n\nOr uncheck 'Keep showing' to hide forever.")
  (princ)
)

;; Test 2: Check if tip is snoozed
(defun c:test-tip-check ()
  (if (haws-tip-is-snoozed 999)
    (alert (princ "Tip 999 IS snoozed"))
    (alert (princ "Tip 999 is NOT snoozed"))
  )
  (princ)
)

;; Test 3: Get snooze info
(defun c:test-tip-getexp (/ snooze-until snooze-days)
  (setq snooze-until (haws-tip-get-snooze 999))
  (cond
    ((not snooze-until)
      (alert (princ "Tip 999 is not snoozed"))
    )
    ((= snooze-until 0)
      (alert (princ "Tip 999 is snoozed FOREVER"))
    )
    (t
      (setq snooze-days (fix (- snooze-until (getvar "DATE"))))
      (alert (princ (strcat "Snooze for " (itoa snooze-days) " more days")))
    )
  )
  (princ)
)

;; Test 4: Manually snooze for 7 days
(defun c:test-tip-snooze7 ()
  (haws-tip-set-snooze 999 7)
  (alert (princ "Snooze for 7 days"))
  (princ)
)

;; Test 5: Clear snooze
(defun c:test-tip-clear ()
  (haws-tip-clear-snooze 999)
  (alert (princ "Tip 999 snooze cleared"))
  (princ)
)

;; Test 6: Test snooze durations
(defun c:test-tip-dates ()
  (alert (princ (strcat
    "Snooze durations:\n"
    "7 days\n"
    "30 days\n"
    "180 days\n"
    "Forever (0)"
  )))
  (princ)
)

;; Test 7: Reset all tips
(defun c:test-tip-reset ()
  (haws-tip-reset)
  (princ)
)

(princ "\n--- HAWS Tip Test Commands Loaded ---")
(princ "\nTEST-TIP-SHOW    - Show test tip dialog")
(princ "\nTEST-TIP-CHECK   - Check if tip 999 is snoozed")
(princ "\nTEST-TIP-GETEXP  - Get snooze expiration for tip 999")
(princ "\nTEST-TIP-SNOOZE7 - Manually snooze tip 999 for 7 days")
(princ "\nTEST-TIP-CLEAR   - Clear snooze for tip 999")
(princ "\nTEST-TIP-DATES   - Show snooze durations")
(princ "\nTEST-TIP-RESET   - Reset all tips")
(princ "\n")
(princ)
