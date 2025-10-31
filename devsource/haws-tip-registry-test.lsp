;;; haws-tip-registry-test.lsp - Test functions for tip registry system
;;; Load haws-tip.lsp before using these tests

;; Test 1: Load and display registry
(defun c:test-registry-load (/ registry)
  (setq registry (haws-tip-registry-load))
  (if registry
    (progn
      (princ "\n--- Tip Registry Contents ---")
      (foreach entry registry
        (princ (strcat "\n  " (car entry) " => " (cdr entry)))
      )
      (princ "\n")
    )
    (princ "\nRegistry file not found or empty")
  )
  (princ)
)

;; Test 2: Check if specific ID is registered
(defun c:test-registry-find (/ registry note)
  (setq registry (haws-tip-registry-load))
  (if registry
    (progn
      (princ "\n--- Testing tip ID lookups ---")
      ;; Test numeric ID
      (setq note (haws-tip-registry-find 999 registry))
      (princ (strcat "\nID 999: " (if note note "NOT FOUND")))
      ;; Test string ID (if exists)
      (setq note (haws-tip-registry-find "test-tip" registry))
      (princ (strcat "\nID test-tip: " (if note note "NOT FOUND")))
      ;; Test unregistered ID
      (setq note (haws-tip-registry-find 12345 registry))
      (princ (strcat "\nID 12345: " (if note note "NOT FOUND")))
      (princ "\n")
    )
    (princ "\nRegistry file not found")
  )
  (princ)
)

;; Test 3: Test ID normalization (case-insensitive)
(defun c:test-registry-normalize ()
  (princ "\n--- Testing ID normalization ---")
  (princ (strcat "\n999 => " (haws-tip-normalize-id 999)))
  (princ (strcat "\n\"MyTip\" => " (haws-tip-normalize-id "MyTip")))
  (princ (strcat "\n\"test-TIP-123\" => " (haws-tip-normalize-id "test-TIP-123")))
  (princ "\n")
  (princ)
)

;; Test 4: Show tip with registered ID
(defun c:test-registry-show-registered ()
  (haws-tip-show 999 "This is a REGISTERED tip (ID 999).\n\nThe registry check should show it's registered.")
  (princ)
)

;; Test 5: Show tip with unregistered ID
(defun c:test-registry-show-unregistered ()
  (haws-tip-show 12345 "This is an UNREGISTERED tip (ID 12345).\n\nThe registry check should warn it's not registered.")
  (princ)
)

;; Test 6: Show tip with string ID
(defun c:test-registry-show-string ()
  (haws-tip-show "my-test-tip" "This is a tip with STRING ID (my-test-tip).\n\nCheck the command line for registry status.")
  (princ)
)

;; Test 7: Test case-insensitive matching
(defun c:test-registry-case (/ registry note1 note2)
  (setq registry (haws-tip-registry-load))
  (if registry
    (progn
      (princ "\n--- Testing case-insensitive matching ---")
      (setq note1 (haws-tip-registry-find "test-tip" registry))
      (setq note2 (haws-tip-registry-find "TEST-TIP" registry))
      (princ (strcat "\n\"test-tip\": " (if note1 note1 "NOT FOUND")))
      (princ (strcat "\n\"TEST-TIP\": " (if note2 note2 "NOT FOUND")))
      (if (and note1 note2 (= note1 note2))
        (princ "\n✓ Case-insensitive matching WORKS")
        (princ "\n✗ Case-insensitive matching FAILED")
      )
      (princ "\n")
    )
    (princ "\nRegistry file not found")
  )
  (princ)
)

(princ "\n--- HAWS Tip Registry Test Commands Loaded ---")
(princ "\nTEST-REGISTRY-LOAD          - Load and display registry")
(princ "\nTEST-REGISTRY-FIND          - Test ID lookups")
(princ "\nTEST-REGISTRY-NORMALIZE     - Test ID normalization")
(princ "\nTEST-REGISTRY-SHOW-REGISTERED   - Show registered tip")
(princ "\nTEST-REGISTRY-SHOW-UNREGISTERED - Show unregistered tip")
(princ "\nTEST-REGISTRY-SHOW-STRING   - Show tip with string ID")
(princ "\nTEST-REGISTRY-CASE          - Test case-insensitive matching")
(princ "\n")
(princ)
