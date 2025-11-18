;;;==============================================================================
;;; REACTOR PERFORMANCE TEST SCRIPT
;;;==============================================================================
;;; Copyright © 2025 Thomas Gail Haws
;;; 
;;; Purpose: Quick script to test reactor callback performance
;;; Usage: Load this after placing bubbles with auto-text in a drawing
;;;
;;; Instructions:
;;; 1. Open a drawing with Civil 3D alignments/pipes
;;; 2. Insert bubble notes with auto-text (Station/Offset, coordinates, etc.)
;;; 3. Load this script: (load "devtools/scripts/test-reactor-performance.lsp")
;;; 4. Run: (c:test-reactor-quick)
;;;==============================================================================

;;; QUICK REACTOR TEST
(defun c:test-reactor-quick (/ start)
  (princ "\n========================================")
  (princ "\n QUICK REACTOR PERFORMANCE TEST")
  (princ "\n========================================")
  
  ;; Load the benchmark system if not already loaded
  (if (not (boundp 'haws-profile-start))
    (progn
      (princ "\n Loading benchmark system...")
      (load (strcat (vl-filename-directory (vl-filename-directory (findfile "cnm.lsp"))) "\\devtools\\scripts\\performance-benchmark.lsp"))
    )
    (princ "\n Benchmark system already loaded ✓")
  )
  
  ;; Check if we have any bubbles with reactors
  (princ "\n Checking for reactive bubbles in drawing...")
  (if (hcnm-test-find-reactive-bubbles)
    (progn
      (princ "\n ✅ Found reactive bubbles")
      (princ "\n")
      (princ "\n Ready to test! Available commands:")
      (princ "\n   BENCHMARK-REACTOR       - Interactive test (10 callbacks)")
      (princ "\n   BENCHMARK-REACTOR-STRESS- Intensive test (100 callbacks)")
      (princ "\n")
      (princ "\n Run (c:benchmark-reactor) to start testing.")
    )
    (progn
      (princ "\n")
      (princ "\n ❌ No reactive bubbles found in current drawing")
      (princ "\n")
      (princ "\n To create test bubbles:")
      (princ "\n 1. Open drawing with Civil 3D alignments/pipes")
      (princ "\n 2. Run CNM bubble insertion command")
      (princ "\n 3. Choose auto-text options (Station/Offset, coordinates, etc.)")
      (princ "\n 4. Return to this test")
    )
  )
  (princ "\n========================================")
  (princ)
)

;;; HELPER: Check for reactive bubbles in drawing  
;;; Uses Lee Mac's anonymous block approach for dynamic blocks
(defun hcnm-test-find-reactive-bubbles (/ ss i ename elist xdata effective-name count reactive-count)
  (setq count 0
        reactive-count 0)
  ;; Get all INSERTs and check each one
  (if (setq ss (ssget "X" '((0 . "INSERT"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ename (ssname ss i)
              elist (entget ename)
              xdata (assoc -3 (entget ename '("HCNM-BUBBLE")))
              effective-name (hcnm-test-effective-name ename))
        ;; Check if it's a CNM bubble by XDATA OR effective name pattern
        (if (or xdata (wcmatch effective-name "cnm-bubble-*"))
          (progn
            (setq count (1+ count))
            (if xdata (setq reactive-count (1+ reactive-count)))
          )
        )
        (setq i (1+ i))
      )
    )
  )
  (princ (strcat "\n Found " (itoa count) " CNM bubbles (" (itoa reactive-count) " reactive)"))
  (> reactive-count 0)
)

;;; Get effective block name (handles anonymous blocks from dynamic blocks)
;;; Simplified version of Lee Mac's LM:al-effectivename
(defun hcnm-test-effective-name (ent / blk rep)
  (setq blk (cdr (assoc 2 (entget ent))))
  ;; If anonymous block (starts with `*), try to get effective name
  (if (wcmatch blk "`**")
    (if (and (setq rep (cdadr (assoc -3 (entget (cdr (assoc 330 (entget (tblobjname "block" blk)))) '("acdbblockrepbtag")))))
             (setq rep (handent (cdr (assoc 1005 rep)))))
      (setq blk (cdr (assoc 2 (entget rep))))
    )
  )
  blk
)

;;; AUTO-LOAD BENCHMARK SYSTEM
(defun c:benchmark-auto ()
  (if (not (boundp 'c:benchmark-reactor))
    (load (strcat (vl-filename-directory (vl-filename-directory (findfile "cnm.lsp"))) "\\devtools\\scripts\\performance-benchmark.lsp"))
  )
  (c:benchmark-reactor)
)

;;; VALIDATION TEST
(defun c:test-reactor-validation (/ reactor-obj data)
  (princ "\n========================================")
  (princ "\n REACTOR SYSTEM VALIDATION")
  (princ "\n========================================")
  
  ;; Check if reactor system is active
  (if (and (boundp '*hcnm-bubble-reactor*)
           *hcnm-bubble-reactor*)
    (progn
      (princ "\n ✅ Global reactor exists: *hcnm-bubble-reactor*")
      (setq reactor-obj *hcnm-bubble-reactor*
            data (vlr-data reactor-obj))
      (princ (strcat "\n ✅ Reactor has " (itoa (length (cadr (assoc "HCNM-BUBBLE" data)))) " owners"))
      (princ "\n ✅ Reactor system is active and ready")
    )
    (progn
      (princ "\n ❌ No global reactor found")
      (princ "\n 💡 Insert bubbles with auto-text to activate reactor system")
    )
  )
  
  ;; Check for profiling capability in reactor callback
  (if (boundp 'hcnm-bn-reactor-callback)
    (princ "\n ✅ Reactor callback function exists with profiling")
    (princ "\n ❌ Reactor callback function not found")
  )
  
  (princ "\n========================================")
  (princ)
)

;;; ANNOUNCE
(princ "\n========================================")
(princ "\n REACTOR PERFORMANCE TEST LOADED")
(princ "\n========================================")
(princ "\n Commands:")
(princ "\n   TEST-REACTOR-QUICK      - Check setup and provide instructions") 
(princ "\n   TEST-REACTOR-VALIDATION - Validate reactor system status")
(princ "\n   BENCHMARK-AUTO          - Auto-load benchmarks and start test")
(princ "\n")
(princ "\n Quick Start: (c:test-reactor-quick)")
(princ "\n========================================")
(princ)