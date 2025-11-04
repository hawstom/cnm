;;; haws-tip-debug.lsp - Debug commands to inspect haws-tip data storage
;;; NOW USING HAWS-CONFIG (Registry storage)

;; Command to inspect all tip snooze data
(defun c:haws-tip-debug-snooze (/ snooze-alist)
  (princ "\n=== HAWS TIP SNOOZE DATA (haws-config/Registry) ===")
  
  ;; Get all snooze data
  (setq snooze-alist (haws-tip-get-all-snooze))
  
  (if snooze-alist
    (progn
      (princ "\nStored snooze data:")
      (foreach pair snooze-alist
        (princ (strcat "\n  Tip " (itoa (car pair)) ": " (cdr pair)
                       (cond
                         ((= (cdr pair) "0") " (hidden forever)")
                         ((> (atof (cdr pair)) (getvar "DATE")) " (snoozed)")
                         (t " (expired)")
                       )))
      )
    )
    (princ "\nNo snooze data stored (all tips active)")
  )
  
  (princ "\n=== END ===\n")
  (princ)
)

;; Command to show raw storage paths
(defun c:haws-tip-debug-paths (/)
  (princ "\n=== HAWS-CONFIG STORAGE INFO ===")
  (princ "\nApp: TIP")
  (princ "\nVariable: SnoozeData")
  (princ "\nScope: 4 (User/Registry)")
  (princ "\nLocation: HKCU\\Software\\HawsEDC\\TIP\\SnoozeData")
  (princ "\nFormat: Assoc list '((tip-id . \"timestamp-or-0\") ...)")
  (princ "\n=== END ===\n")
  (princ)
)

;; Command to test reading/writing a value
(defun c:haws-tip-debug-test (/ test-id snooze-alist before-value after-value)
  (princ "\n=== TEST READ/WRITE ===")
  
  (setq test-id 9999)
  (princ (strcat "\nTest tip ID: " (itoa test-id)))
  
  ;; Read current value
  (setq before-value (haws-tip-get-snooze test-id))
  (princ (strcat "\nBefore: " (if before-value (rtos before-value 2 8) "nil")))
  
  ;; Write test value (snooze for 7 days)
  (princ "\nWriting snooze for 7 days...")
  (haws-tip-set-snooze test-id 7)
  
  ;; Read back
  (setq after-value (haws-tip-get-snooze test-id))
  (princ (strcat "\nAfter: " (if after-value (rtos after-value 2 8) "nil")))
  
  ;; Verify it's actually stored
  (setq snooze-alist (haws-tip-get-all-snooze))
  (princ "\nFull snooze list:")
  (princ (strcat "\n  " (vl-prin1-to-string snooze-alist)))
  
  ;; Clean up
  (princ "\n\nCleaning up test data...")
  (haws-tip-clear-snooze test-id)
  
  (princ "\n=== END ===\n")
  (princ)
)

;; Command to check current profile and haws-config registration
(defun c:haws-tip-debug-profile (/ tip-def)
  (princ "\n=== AUTOCAD & HAWS-CONFIG INFO ===")
  (princ (strcat "\nCurrent Profile: " (getvar "CPROFILE")))
  (princ (strcat "\nAutoCAD Version: " (getvar "ACADVER")))
  (princ (strcat "\nProduct: " (getvar "PRODUCT")))
  (princ (strcat "\nCurrent Date (Julian): " (rtos (getvar "DATE") 2 8)))
  
  ;; Check if TIP app is registered
  (princ "\n\nHAWS-CONFIG Registration:")
  (setq tip-def (haws-config-get-definitions "TIP"))
  (if tip-def
    (progn
      (princ "\n  TIP app: REGISTERED")
      (princ "\n  Variables:")
      (foreach var-def tip-def
        (princ (strcat "\n    " (car var-def) " (scope " (itoa (caddr var-def)) ")"))
      )
    )
    (princ "\n  TIP app: NOT REGISTERED")
  )
  
  (princ "\n=== END ===\n")
  (princ)
)

;; Command to view raw Registry value (Windows only)
(defun c:haws-tip-debug-registry (/ raw-value)
  (princ "\n=== RAW REGISTRY VALUE ===")
  (setq raw-value (haws-config-getvar "TIP" "SnoozeData" nil nil))
  (princ "\nSnoozeData raw string:")
  (if (and raw-value (/= raw-value ""))
    (princ (strcat "\n  \"" raw-value "\""))
    (princ "\n  [empty]")
  )
  (princ "\n=== END ===\n")
  (princ)
)

(princ "\nHaws-tip debug commands loaded (haws-config version):")
(princ "\n  HAWS-TIP-DEBUG-SNOOZE   - Show all tip snooze values")
(princ "\n  HAWS-TIP-DEBUG-PATHS    - Show storage location info")
(princ "\n  HAWS-TIP-DEBUG-TEST     - Test read/write cycle")
(princ "\n  HAWS-TIP-DEBUG-PROFILE  - Show AutoCAD & haws-config info")
(princ "\n  HAWS-TIP-DEBUG-REGISTRY - Show raw Registry value")
(princ)
