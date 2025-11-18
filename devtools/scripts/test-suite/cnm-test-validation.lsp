;;;; CNM Test Suite - Validation Functions
;;;; AutoLISP functions for automated test validation
;;;;
;;;; Purpose: Query drawing state and verify test results
;;;; Usage: Loaded by cnm-test.scr during test execution
;;;;
;;;; Created: 2025-11-17
;;;; Status: Foundation (Phase B) - helpers only, tests in Phase C
;;;;
;;;; Architecture:
;;;;   - Test functions query XDATA, reactors, attributes
;;;;   - Results annotated on C-ANNO-TEST-RESULTS layer
;;;;   - Report written to cnm-test-report.md for AI analysis
;;;;
;;;; Note: Phase C will add actual test validation functions
;;;;       (test-validate-station, test-validate-pipe-dia, etc.)
(defun c:test-setup-layers ( / )
  ;; Create test results layer for MTEXT annotations
  (command "._layer" "_make" "C-ANNO-TEST-RESULTS" "")
  (princ "\nTest suite layers created")
  (princ)
)
(defun c:test-annotate-result (location status message / pt color)
  ;; Place MTEXT annotation at specified location with color-coded status
  ;; Args:
  ;;   location - String "x,y" coordinates for annotation
  ;;   status - String "PASS", "FAIL", or "INFO"
  ;;   message - String detailed message text
  ;; Returns: nil (princ suppresses return value)
  (setq pt (read (strcat "(" location ")")))
  (cond
    ((= status "PASS") (setq color "3"))
    ((= status "FAIL") (setq color "1"))
    ((= status "INFO") (setq color "4"))
    (T (setq color "7"))
  )
  (command "._layer" "_make" "C-ANNO-TEST-RESULTS" "_color" color "" "")
  (command "._mtext" pt "_justify" "_tl"
           (list (+ (car pt) 400) (- (cadr pt) 100))
           (strcat "[" status "]\n" message)
           "")
  (princ)
)
(defun c:test-generate-summary-mtext ( / pass-count fail-count info-count ss i ent obj-text text-str summary-msg)
  ;; Count PASS/FAIL/INFO annotations and generate summary MTEXT
  ;; Scans all MTEXT on C-ANNO-TEST-RESULTS layer
  ;; Places summary at top of drawing (100,1100)
  (setq pass-count 0
        fail-count 0
        info-count 0)
  (setq ss (ssget "_X" '((0 . "MTEXT") (8 . "C-ANNO-TEST-RESULTS"))))
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq obj-text (vlax-ename->vla-object ent))
        (setq text-str (vlax-get-property obj-text 'TextString))
        (cond
          ((vl-string-search "[PASS" text-str) (setq pass-count (1+ pass-count)))
          ((vl-string-search "[FAIL" text-str) (setq fail-count (1+ fail-count)))
          ((vl-string-search "[INFO" text-str) (setq info-count (1+ info-count)))
        )
        (setq i (1+ i))
      )
    )
  )
  (setq summary-msg
    (strcat "=== CNM TEST SUITE SUMMARY ===\n"
            "Date: " (menucmd "m=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM)") "\n"
            "Total Tests: " (itoa (+ pass-count fail-count)) "\n"
            "PASSED: " (itoa pass-count) "\n"
            "FAILED: " (itoa fail-count) "\n"
            "INFO: " (itoa info-count) " (performance reports)\n\n"
            (if (= fail-count 0)
              "ALL TESTS PASSED\n\nReview annotations for details."
              "SOME TESTS FAILED\n\nReview RED annotations for failure details.")
    )
  )
  (command "._layer" "_make" "C-ANNO-TEST-RESULTS" "_color" "7" "" "")
  (command "._mtext" "100,1100" "_justify" "_tl" "700,950"
    summary-msg
    ""
  )
  (princ (strcat "\n\n" summary-msg))
  (princ "\n\n>>> USER: Copy summary MTEXT and paste to AI for analysis <<<")
  (princ)
)
(defun test-write-report-header ( / report-file timestamp)
  ;; Initialize cnm-test-report.md with header
  ;; Called at start of test suite execution
  (setq report-file (open "cnm-test-report.md" "w"))
  (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)"))
  (write-line "# CNM Test Suite Report\n" report-file)
  (write-line (strcat "**Date:** " timestamp "\n") report-file)
  (write-line "**Status:** Running tests...\n" report-file)
  (write-line "---\n" report-file)
  (write-line "## Test Results\n" report-file)
  (close report-file)
  (princ "\nTest report initialized: cnm-test-report.md")
  (princ)
)
(defun test-write-report-entry (test-name status message / report-file)
  ;; Append test result entry to cnm-test-report.md
  ;; Args:
  ;;   test-name - String name of test (e.g., "TEST 1: Station Auto-Text")
  ;;   status - String "PASS", "FAIL", or "INFO"
  ;;   message - String detailed message
  (setq report-file (open "cnm-test-report.md" "a"))
  (write-line (strcat "### " test-name "\n") report-file)
  (write-line (strcat "**Status:** " status "\n") report-file)
  (write-line (strcat "**Details:**\n```\n" message "\n```\n") report-file)
  (close report-file)
  (princ)
)
(defun test-write-report-summary (pass-count fail-count info-count / report-file)
  ;; Write summary section to cnm-test-report.md
  ;; Called at end of test suite execution
  (setq report-file (open "cnm-test-report.md" "a"))
  (write-line "\n---\n" report-file)
  (write-line "## Summary\n" report-file)
  (write-line (strcat "- **Total Tests:** " (itoa (+ pass-count fail-count)) "\n") report-file)
  (write-line (strcat "- **Passed:** " (itoa pass-count) "\n") report-file)
  (write-line (strcat "- **Failed:** " (itoa fail-count) "\n") report-file)
  (write-line (strcat "- **Info:** " (itoa info-count) " (performance reports)\n") report-file)
  (if (= fail-count 0)
    (write-line "\n**Result:** ✅ ALL TESTS PASSED\n" report-file)
    (write-line "\n**Result:** ❌ SOME TESTS FAILED - Review failures above\n" report-file)
  )
  (close report-file)
  (princ "\nTest report complete: cnm-test-report.md")
  (princ)
)
(princ "\nCNM Test Suite validation functions loaded")
(princ "\n  Commands: c:test-setup-layers, c:test-annotate-result, c:test-generate-summary-mtext")
(princ "\n  Helpers: test-write-report-header, test-write-report-entry, test-write-report-summary")
(princ "\n  Phase C: Actual test validation functions coming next")
(princ)
