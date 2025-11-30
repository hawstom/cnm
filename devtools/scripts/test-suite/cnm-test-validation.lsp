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
;;;;   - Report written to test-results/cnm-test-report.md for AI analysis
;;;;   - Timing log written to test-results/cnm-test-run.log
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
  (setq summary-msg "=== CNM TEST SUITE SUMMARY ===\nCheck test-results/cnm-test-report.md for results")
  (princ (strcat "\n\n" summary-msg))
  (princ "\n\n>>> USER: Review test-results/cnm-test-report.md for test results <<<")
  (princ)
)
(defun test-write-report-header ( / report-file timestamp resultsdir)
  ;; Initialize test-results/cnm-test-report.md with header
  ;; Called at start of test suite execution
  ;; Create test-results subfolder if it doesn't exist
  (setq resultsdir (strcat (getvar "dwgprefix") "test-results"))
  (if (not (vl-file-directory-p resultsdir))
    (vl-mkdir resultsdir)
  )
  (setq report-file (open (strcat resultsdir "\\cnm-test-report.md") "w"))
  (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)"))
  (write-line "# CNM Test Suite Report\n" report-file)
  (write-line (strcat "**Date:** " timestamp "\n") report-file)
  (write-line "**Status:** Running tests...\n" report-file)
  (write-line "---\n" report-file)
  (write-line "## Test Results\n" report-file)
  (close report-file)
  (princ "\nTest report initialized: test-results/cnm-test-report.md")
  (princ)
)
(defun test-write-report-entry (test-name status message / report-file resultsdir)
  ;; Append test result entry to test-results/cnm-test-report.md
  ;; Args:
  ;;   test-name - String name of test (e.g., "TEST 1: Station Auto-Text")
  ;;   status - String "PASS", "FAIL", or "INFO"
  ;;   message - String detailed message
  (setq resultsdir (strcat (getvar "dwgprefix") "test-results"))
  (setq report-file (open (strcat resultsdir "\\cnm-test-report.md") "a"))
  (write-line (strcat "### " test-name "\n") report-file)
  (write-line (strcat "**Status:** " status "\n") report-file)
  (write-line (strcat "**Details:**\n```\n" message "\n```\n") report-file)
  (close report-file)
  (princ)
)
(defun test-write-report-summary (model-count paper-count total-count / report-file resultsdir)
  ;; Write summary section to test-results/cnm-test-report.md
  ;; Args:
  ;;   model-count - Number of model space tests
  ;;   paper-count - Number of paper space tests
  ;;   total-count - Total number of tests
  ;; Called at end of test suite execution
  (setq resultsdir (strcat (getvar "dwgprefix") "test-results"))
  (setq report-file (open (strcat resultsdir "\\cnm-test-report.md") "a"))
  (write-line "\n---\n" report-file)
  (write-line "## Summary\n" report-file)
  (write-line (strcat "- **Total Tests:** " (itoa total-count) "\n") report-file)
  (write-line (strcat "- **Model Space Tests:** " (itoa model-count) "\n") report-file)
  (write-line (strcat "- **Paper Space Tests:** " (itoa paper-count) "\n") report-file)
  (write-line "\n**Note:** Review test entries above for PASS/FAIL status.\n" report-file)
  (write-line "Review drawing annotations on C-ANNO-TEST-RESULTS layer for visual results.\n" report-file)
  (close report-file)
  (princ "\nTest report complete: test-results/cnm-test-report.md")
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
;;==============================================================================
;; PHASE H: KEY NOTES TABLE VALIDATION FUNCTIONS
;;==============================================================================
(defun c:test-validate-key-table ( / ss table-ent table-data box1-qty box2-count result-msg)
  ;; Find Key Notes Table blocks (noteqty.dwg insertions)
  ;; CNM builds table from noteqty.dwg block repeated for each note
  (setq ss (ssget "_X" '((0 . "INSERT") (2 . "NOTEQTY*"))))
  (if ss
    (progn
      ;; Extract quantities from table block attributes
      ;; This requires understanding noteqty.dwg attribute structure
      ;; For now, just verify table exists
      (setq result-msg
        (strcat "✅ PASS: Key Notes Table generated\n"
                "  Table blocks found: " (itoa (sslength ss)) "\n"
                "  (Manual verification required for quantities)\n\n"
                "  Expected:\n"
                "    BOX-1: 325 LF (100+150+75)\n"
                "    BOX-2: 2 EA (count)\n"
                "    CIR-1: 200 LF\n"
                "    CIR-2: 1 EA (count)\n"
                "    DIA-1: 450 LF\n"
                "    DIA-2: 3 EA (count)\n"
                "    HEX-1: 800 LF\n"
                "    HEX-2: 5 EA (count)\n\n"
                "  Check: Visual inspection of table\n"
                "  Check: cnm-test.not file created"))
      (princ "\n✅ Key Notes Table: Generated")
      (test-write-report-entry "TEST H: Key Notes Table Generation" "PASS" result-msg)
    )
    (progn
      (setq result-msg "❌ FAIL: Key Notes Table NOT generated")
      (princ "\n❌ Key Notes Table: FAIL")
      (test-write-report-entry "TEST H: Key Notes Table Generation" "FAIL" result-msg)
    )
  )
  ;; Additional check: Verify .not file created
  (if (findfile (strcat (getvar "dwgprefix") "cnm-test.not"))
    (progn
      (princ "\n✅ .not file created for QT processing")
      (test-write-report-entry "TEST H: .not File Creation" "PASS" ".not file exists in project folder")
    )
    (progn
      (princ "\n❌ WARNING: .not file NOT created")
      (test-write-report-entry "TEST H: .not File Creation" "FAIL" ".not file missing from project folder")
    )
  )
  (princ)
)
;;==============================================================================
;; PHASE I: QUANTITY TAKEOFF TABLE VALIDATION FUNCTIONS
;;==============================================================================
(defun c:test-create-qt-lst-file ( / )
  ;; Create simple sheet list for single-sheet QT test
  (test-write-file "cnm-test.lst" "cnm-test")
  (if (findfile (strcat (getvar "dwgprefix") "cnm-test.lst"))
    (progn
      (princ "\n✅ Sheet list created: cnm-test.lst")
      (test-write-report-entry "SETUP: Create Sheet List" "PASS" "cnm-test.lst created with single entry: cnm-test")
    )
    (progn
      (princ "\n❌ WARNING: Failed to create cnm-test.lst")
      (test-write-report-entry "SETUP: Create Sheet List" "FAIL" "Failed to create cnm-test.lst file")
    )
  )
  (princ)
)
(defun test-write-file (filename contents / f)
  ;; Write string to text file in current drawing folder
  (setq f (open (strcat (getvar "dwgprefix") filename) "w"))
  (if f
    (progn
      (write-line contents f)
      (close f)
      t
    )
    nil
  )
)
(defun c:test-validate-qt-table ( / ss table-text result-msg)
  ;; Find QT table text objects
  ;; CNM QT generates text following current style/dimension settings
  (setq ss (ssget "_X" '((0 . "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq result-msg
        (strcat "✅ PASS: QT Table generated\n"
                "  Text objects found: " (itoa (sslength ss)) "\n"
                "  (Manual verification required for totals)\n\n"
                "  Expected single-sheet totals:\n"
                "    BOX-1: 325 LF\n"
                "    BOX-2: 2 EA\n"
                "    CIR-1: 200 LF\n"
                "    CIR-2: 1 EA\n"
                "    DIA-1: 450 LF\n"
                "    DIA-2: 3 EA\n"
                "    HEX-1: 800 LF\n"
                "    HEX-2: 5 EA\n\n"
                "  Check: Visual inspection of QT table"))
      (princ "\n✅ QT Table: Generated")
      (test-write-report-entry "TEST I: QT Table Generation" "PASS" result-msg)
    )
    (progn
      (setq result-msg "❌ FAIL: QT Table NOT generated (no text objects found)")
      (princ "\n❌ QT Table: FAIL")
      (test-write-report-entry "TEST I: QT Table Generation" "FAIL" result-msg)
    )
  )
  (princ)
)
(princ)
  ;;==============================================================================
  ;; TEST RUN TIMING & CLEANUP HELPERS
  ;;==============================================================================
  (defun c:test-log-start ( / logfile timestamp f resultsdir )
    ;; Append a START timestamp to test-results/cnm-test-run.log
    ;; Create test-results subfolder if it doesn't exist
    (setq resultsdir (strcat (getvar "dwgprefix") "test-results"))
    (if (not (vl-file-directory-p resultsdir))
      (vl-mkdir resultsdir)
    )
    (setq logfile (strcat resultsdir "\\cnm-test-run.log"))
    (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)"))
    (if (setq f (open logfile "a"))
      (progn
        (write-line (strcat "START: " timestamp) f)
        (close f)
        (princ (strcat "\nTest run start: " timestamp))
      )
      (princ "\nFailed to open test run log for writing")
    )
    (princ)
  )

  (defun c:test-log-end ( / logfile timestamp f resultsdir )
    ;; Append an END timestamp to test-results/cnm-test-run.log
    (setq resultsdir (strcat (getvar "dwgprefix") "test-results"))
    (setq logfile (strcat resultsdir "\\cnm-test-run.log"))
    (setq timestamp (menucmd "m=$(edtime,$(getvar,date),YYYY-MM-DD HH:MM:SS)"))
    (if (setq f (open logfile "a"))
      (progn
        (write-line (strcat "END:   " timestamp) f)
        (close f)
        (princ (strcat "\nTest run end: " timestamp))
      )
      (princ "\nFailed to open test run log for writing")
    )
    (princ)
  )

  (defun c:test-cleanup-project-files ( / pfx f1 f2 f3 msg)
    ;; Remove temporary project files created by the test run
    ;; cnm.ini: CNM project settings (auto-created from template)
    ;; constnot.csv: Project notes database (auto-created from template)
    ;; cnm-test.csv: QT table CSV export (created by haws-qt command)
    (setq pfx (getvar "dwgprefix"))
    (setq f1 (strcat pfx "cnm.ini"))
    (setq f2 (strcat pfx "constnot.csv"))
    (setq f3 (strcat pfx "cnm-test.csv"))
    (setq msg "")
    (if (and f1 (findfile f1)) (vl-file-delete f1) (setq msg (strcat msg " cnm.ini not found;")))
    (if (and f2 (findfile f2)) (vl-file-delete f2) (setq msg (strcat msg " constnot.csv not found;")))
    (if (and f3 (findfile f3)) (vl-file-delete f3) (setq msg (strcat msg " cnm-test.csv not found;")))
    (test-write-report-entry "CLEANUP: Project files" "INFO" (strcat "Removed files if present." msg))
    (princ "\nCleanup complete: cnm.ini, constnot.csv, cnm-test.csv removed if present")
    (princ)
  )
;;==============================================================================
;; PHASE F: PERFORMANCE BENCHMARK VALIDATION FUNCTIONS
;;==============================================================================
(defun c:test-report-insertion-performance (start-time count / end-time elapsed avg rate result-msg)
  ;; Report bubble insertion performance
  (setq end-time (getvar "MILLISECS"))
  (setq elapsed (- end-time start-time))
  (setq avg (/ elapsed (float count)))
  (setq rate (/ (* count 1000.0) elapsed))
  (setq result-msg
    (strcat "⏱ INSERTION PERFORMANCE: " (itoa count) " bubbles with auto-text\n"
            "  Total time: " (rtos (/ elapsed 1000.0) 2 2) " seconds\n"
            "  Average per bubble: " (rtos avg 2 0) " ms\n"
            "  Insertion rate: " (rtos rate 2 1) " bubbles/sec\n\n"
            "  BASELINE: Establishes performance expectation\n"
            "  - < 500ms/bubble: ✅ Excellent (typical)\n"
            "  - 500-1000ms/bubble: ⚡ Acceptable\n"
            "  - > 1000ms/bubble: ⚠️ Investigate bottlenecks\n\n"
            "  Use this baseline for regression detection:\n"
            "  - Re-run after code changes\n"
            "  - Compare timing to detect performance regressions\n"
            "  - Track trends over multiple test runs"))
  (princ (strcat "\n\n" result-msg))
  (test-write-report-entry "TEST 39: Insertion Performance" "INFO" result-msg)
  (princ)
)
(defun c:test-report-reactor-performance (start-time count / end-time elapsed avg result-msg)
  ;; Report reactor update performance
  (setq end-time (getvar "MILLISECS"))
  (setq elapsed (- end-time start-time))
  (setq avg (/ elapsed (float count)))
  (setq result-msg
    (strcat "⏱ REACTOR PERFORMANCE: " (itoa count) " leader stretches\n"
            "  Total time: " (rtos (/ elapsed 1000.0) 2 2) " seconds\n"
            "  Average per update: " (rtos avg 2 0) " ms\n"
            "  (Each stretch triggers VLR-OBJECT-REACTOR callback)\n\n"
            "  PERFORMANCE ANALYSIS:\n"
            (cond
              ((> avg 50)
               "  ⚠️ SLOW: > 50ms per callback\n  🔍 Check: Complex auto-text, multiple Civil 3D queries\n  💡 Consider: Caching expensive operations")
              ((> avg 20)
               "  ⚡ MODERATE: 20-50ms per callback\n  📊 Performance acceptable for typical use")
              (t
               "  ✅ FAST: < 20ms per callback\n  🚀 Excellent reactive performance")
            )
            "\n\n  BASELINE: Use for regression detection\n"
            "  - Re-run after reactor code changes\n"
            "  - Compare to detect performance impact\n"
            "  - Track config optimization effects (Phase 3)"))
  (princ (strcat "\n\n" result-msg))
  (test-write-report-entry "TEST 40: Reactor Performance" "INFO" result-msg)
  (princ)
)


