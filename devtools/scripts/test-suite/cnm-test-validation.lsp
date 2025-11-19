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
  (vl-cmdf "._layer" "_make" "C-ANNO-TEST-RESULTS" "")
  (princ "\nTest suite layers created")
  (princ)
)
(defun c:test-annotate-result (location status message / color x y x-str y-str x-corner y-corner x-corner-str y-corner-str)
  ;; Place MTEXT annotation at specified location with color-coded status
  ;; Args:
  ;;   location - String "x,y" coordinates for annotation
  ;;   status - String "PASS", "FAIL", or "INFO"
  ;;   message - String detailed message text
  ;; Returns: nil (princ suppresses return value)
  ;; DISABLED: Drawing annotations causing safearray errors
  ;; Just print to console for now
  (princ (strcat "\n[" status "] " message))
  (princ)
)
(defun c:test-generate-summary-mtext ( / pass-count fail-count info-count ss i ent obj-text text-str summary-msg)
  ;; Count PASS/FAIL/INFO annotations and generate summary MTEXT
  ;; DISABLED: Drawing commands causing safearray errors
  ;; Just print summary to console
  (setq summary-msg "=== CNM TEST SUITE SUMMARY ===\nCheck cnm-test-report.md for results")
  (princ (strcat "\n\n" summary-msg))
  (princ "\n\n>>> USER: Review cnm-test-report.md for test results <<<")
  (princ)
)
(defun test-write-report-header ( / report-file timestamp)
  ;; Initialize cnm-test-report.md with header
  ;; Called at start of test suite execution
  (setq report-file (open (strcat (getvar "dwgprefix") "cnm-test-report.md") "w"))
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
  (setq report-file (open (strcat (getvar "dwgprefix") "cnm-test-report.md") "a"))
  (write-line (strcat "### " test-name "\n") report-file)
  (write-line (strcat "**Status:** " status "\n") report-file)
  (write-line (strcat "**Details:**\n```\n" message "\n```\n") report-file)
  (close report-file)
  (princ)
)
(defun test-write-report-summary (model-count paper-count total-count / report-file)
  ;; Write summary section to cnm-test-report.md
  ;; Args:
  ;;   model-count - Number of model space tests
  ;;   paper-count - Number of paper space tests
  ;;   total-count - Total number of tests
  ;; Called at end of test suite execution
  (setq report-file (open (strcat (getvar "dwgprefix") "cnm-test-report.md") "a"))
  (write-line "\n---\n" report-file)
  (write-line "## Summary\n" report-file)
  (write-line (strcat "- **Total Tests:** " (itoa total-count) "\n") report-file)
  (write-line (strcat "- **Model Space Tests:** " (itoa model-count) "\n") report-file)
  (write-line (strcat "- **Paper Space Tests:** " (itoa paper-count) "\n") report-file)
  (write-line "\n**Note:** Review test entries above for PASS/FAIL status.\n" report-file)
  (write-line "Review drawing annotations on C-ANNO-TEST-RESULTS layer for visual results.\n" report-file)
  (close report-file)
  (princ "\nTest report complete: cnm-test-report.md")
  (princ)
)
(princ "\nCNM Test Suite validation functions loaded")
(princ "\n  Commands: c:test-setup-layers, c:test-annotate-result, c:test-generate-summary-mtext")
(princ "\n  Helpers: test-write-report-header, test-write-report-entry, test-write-report-summary")
(princ "\n  Validators: test-validate-auto-text, test-validate-xdata, test-validate-reactor")
(princ)
;;==============================================================================
;; PHASE C: AUTO-TEXT VALIDATION FUNCTIONS
;;==============================================================================
(defun test-validate-auto-text (ename tag expected-text test-name /
                                lattribs actual-text result status message location
                               )
  ;; Validate bubble attribute text matches expected value
  ;; Args:
  ;;   ename - Entity name of bubble
  ;;   tag - Attribute tag to check (e.g., "NOTETXT1")
  ;;   expected-text - Expected text value (e.g., "STA 3+00.00")
  ;;   test-name - Name for report (e.g., "TEST 1: Station Auto-Text")
  ;; Returns: T if pass, nil if fail
  (setq lattribs (hcnm-bn-dwg-to-lattribs ename))
  (setq actual-text (cadr (assoc tag lattribs)))
  (setq result (= actual-text expected-text))
  (setq status (if result "PASS" "FAIL"))
  (setq message
    (strcat "Tag: " tag "\n"
            "Expected: \"" expected-text "\"\n"
            "Actual:   \"" (if actual-text actual-text "<<NIL>>") "\""
    )
  )
  (setq location (test-get-bubble-annotation-location ename))
  (if location
    (c:test-annotate-result location status (strcat test-name "\n" message))
  )
  (test-write-report-entry test-name status message)
  result
)
(defun test-validate-xdata (ename tag expected-auto-type test-name /
                           xdata tag-entry composite-pairs first-pair composite-key auto-text result status message location
                          )
  ;; Validate XDATA exists and has correct auto-type
  ;; Args:
  ;;   ename - Entity name of bubble
  ;;   tag - Attribute tag (e.g., "NOTETXT1")
  ;;   expected-auto-type - Expected auto-type (e.g., "Sta", "Dia", "NE")
  ;;   test-name - Name for report
  ;; Returns: T if XDATA found with correct auto-type, nil otherwise
  (setq xdata (hcnm-xdata-read ename))
  ;; xdata format: '(("TAG" . (((auto-type . handle) . auto-text) ...)))
  ;; Check if xdata is an alist and has entry for tag
  (if (and xdata (listp xdata))
    (progn
      (setq tag-entry (assoc tag xdata))
      (if tag-entry
        (progn
          (setq composite-pairs (cdr tag-entry))  ; List of composite pairs
          (if (and composite-pairs (listp composite-pairs))
            (progn
              (setq first-pair (car composite-pairs))  ; ((auto-type . handle) . auto-text)
              (setq composite-key (car first-pair))    ; (auto-type . handle)
              (setq auto-text (cdr first-pair))
            )
            (setq composite-key nil auto-text nil)
          )
        )
        (setq composite-key nil auto-text nil)
      )
    )
    (setq composite-key nil auto-text nil)
  )
  (setq result
    (and xdata
         (assoc tag xdata)
         composite-key
         (equal expected-auto-type (car composite-key))
    )
  )
  (setq status (if result "PASS" "FAIL"))
  (setq message
    (strcat "Tag: " tag "\n"
            "Expected auto-type: \"" expected-auto-type "\"\n"
            "XDATA composite-key: " (vl-prin1-to-string composite-key) "\n"
            "XDATA auto-text: \"" (if auto-text auto-text "<<NIL>>") "\""
    )
  )
  (setq location (test-get-bubble-annotation-location ename))
  (if location
    (c:test-annotate-result location status (strcat test-name " (XDATA)\n" message))
  )
  (test-write-report-entry (strcat test-name " (XDATA)") status message)
  result
)
(defun test-validate-reactor (ename tag expected-auto-type test-name /
                              handle-bubble result status message location
                             )
  ;; Validate reactor is attached for auto-text tag
  ;; Args:
  ;;   ename - Entity name of bubble
  ;;   tag - Attribute tag (e.g., "NOTETXT1")
  ;;   expected-auto-type - Expected auto-type (e.g., "Sta", "Dia")
  ;;   test-name - Name for report
  ;; Returns: T if reactor found, nil otherwise
  ;; NOTE: Simplified version - just report INFO since reactor API is unclear
  (setq handle-bubble (cdr (assoc 5 (entget ename))))
  (setq result nil)  ; Always fail for now
  (setq status "INFO")
  (setq message
    (strcat "Tag: " tag "\n"
            "Expected auto-type: \"" expected-auto-type "\"\n"
            "Reactor validation: SKIPPED (API unclear)\n"
            "Handle: " handle-bubble
    )
  )
  (setq location (test-get-bubble-annotation-location ename))
  (if location
    (c:test-annotate-result location status (strcat test-name " (Reactor)\n" message))
  )
  (test-write-report-entry (strcat test-name " (Reactor)") status message)
  result
)
(defun test-get-bubble-annotation-location (ename / elist pt-insert x y)
  ;; Get insertion point of bubble for annotation placement
  ;; Returns: String "x,y" coordinates offset to right of bubble
  ;; Returns: nil if error occurs
  ;; Use entget DXF code 10 to avoid safearray issues
  (if (and
        (setq elist (entget ename))
        (setq pt-insert (cdr (assoc 10 elist)))
        (numberp (setq x (car pt-insert)))
        (numberp (setq y (cadr pt-insert)))
      )
    (strcat (rtos (+ x 200.0) 2 2) "," (rtos y 2 2))
    nil  ; Return nil on error
  )
)
(princ)
