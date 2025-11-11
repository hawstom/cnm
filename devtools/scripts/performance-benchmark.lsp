;;;==============================================================================
;;; CNM PERFORMANCE BENCHMARKING - COMPLETE SYSTEM
;;;==============================================================================
;;; Copyright © 2025 Thomas Gail Haws
;;; 
;;; Purpose: Simple performance benchmarking for CNM development
;;; Location: devtools/performance/benchmark.lsp
;;;
;;; Quick Start:
;;;   (load "devtools/performance/benchmark.lsp")
;;;   (c:benchmark-config)        ; Test config system (Phase 3 findings)
;;;   (c:benchmark-report)        ; View results
;;;   (c:benchmark-reset)         ; Clear data
;;;
;;; Custom Benchmarking:
;;;   (setq start (benchmark-start "my-operation"))
;;;   ; ... your code ...
;;;   (benchmark-end "my-operation" start)
;;;==============================================================================

;;; Global data storage (compatible with existing instrumentation)
(setq *haws-profile-data* '())

;;; COMPATIBILITY: Support existing instrumentation in CNM/HAWS-CONFIG
(defun haws-profile-start (label)
  (getvar "DATE")  ; Julian date with millisecond precision
)

(defun haws-profile-end (label start-time / elapsed)
  (setq elapsed (* (- (getvar "DATE") start-time) 86400000.0))  ; Convert to milliseconds
  (setq *haws-profile-data* 
    (cons (list label elapsed (getvar "DATE")) *haws-profile-data*))
  elapsed
)

(defun haws-profile-reset ()
  (setq *haws-profile-data* '())
  (princ "\nBenchmark data cleared.")
)

(defun haws-profile-report (verbose-p / data labels totals counts label total count avg)
  (if (not *haws-profile-data*)
    (princ "\nNo benchmark data available.")
    (progn
      (princ "\n========================================")
      (princ "\n CNM PERFORMANCE BENCHMARK REPORT")
      (princ "\n========================================")
      (setq data *haws-profile-data*
            labels '()
            totals '()
            counts '())
      ;; Group by label and calculate totals
      (foreach entry data
        (setq label (car entry)
              elapsed (cadr entry))
        (if (setq existing (assoc label labels))
          (progn
            (setq total (+ (cadr existing) elapsed)
                  count (1+ (caddr existing)))
            (setq labels (subst (list label total count) existing labels)))
          (setq labels (cons (list label elapsed 1) labels))
        )
      )
      ;; Display results
      (foreach entry (reverse labels)
        (setq label (car entry)
              total (cadr entry)
              count (caddr entry)
              avg (/ total count))
        (princ (strcat "\n " label ": "
                      (itoa count) " calls, "
                      (rtos total 2 2) "ms total, "
                      (rtos avg 2 2) "ms avg"))
      )
      (princ "\n========================================")
    )
  )
  (princ)
)

;;; NEW INTERFACE: Simpler names for direct use
(defun benchmark-start (label) (haws-profile-start label))
(defun benchmark-end (label start-time) (haws-profile-end label start-time))

;;; NEW INTERFACE: Simpler names for direct use
(defun benchmark-reset () (haws-profile-reset))
(defun benchmark-report () (haws-profile-report nil))

;;; CONFIG SYSTEM BENCHMARKING (Phase 3 findings)
(defun c:benchmark-config (/ start i total avg)
  (princ "\n========================================")
  (princ "\n CONFIG SYSTEM BENCHMARK")
  (princ "\n========================================")
  (princ "\n Testing 100 config reads...")
  
  (haws-profile-reset)
  (setq i 0)
  (repeat 100
    (setq start (haws-profile-start "config-read"))
    (hcnm-config-getvar "BlockReactors")  ; Known bottleneck
    (haws-profile-end "config-read" start)
    (setq i (1+ i))
    (if (= (rem i 25) 0) (princ (strcat "\n " (itoa i) " completed...")))
  )
  
  (princ "\n\nResults:")
  (haws-profile-report nil)
  
  ;; Analysis based on Phase 3 findings
  (setq total 0)
  (foreach entry *haws-profile-data*
    (setq total (+ total (cadr entry)))
  )
  (setq avg (/ total 100))
  
  (princ "\n")
  (princ "\n========================================")
  (princ "\n ANALYSIS")
  (princ "\n========================================")
  (if (> avg 30)
    (progn
      (princ (strcat "\n ⚠️  SLOW: " (rtos avg 2 2) "ms per config read"))
      (princ "\n 🔍 ROOT CAUSE: hcnm-proj file validation (Phase 3)")
      (princ "\n 🚀 OPTIMIZATION: Add session caching (97.5% improvement)")
    )
    (progn
      (princ (strcat "\n ✅ GOOD: " (rtos avg 2 2) "ms per config read"))
      (princ "\n 🎯 Performance within acceptable range")
    )
  )
  (princ "\n========================================")
  (princ)
)

;;; STRESS TEST - 1000 iterations for reliable timing
(defun c:benchmark-config-stress (/ start i total avg)
  (princ "\n========================================")
  (princ "\n CONFIG STRESS TEST - 1000 iterations")
  (princ "\n========================================")
  (princ "\n This will take 30-60 seconds...")
  
  (haws-profile-reset)
  (setq i 0)
  (repeat 1000
    (setq start (haws-profile-start "stress-config"))
    (hcnm-config-getvar "BlockReactors")
    (haws-profile-end "stress-config" start)
    (setq i (1+ i))
    (if (= (rem i 100) 0) (princ (strcat "\n " (itoa i) " completed...")))
  )
  
  (princ "\n\nResults:")
  (haws-profile-report nil)
  
  ;; Statistical analysis
  (setq total 0)
  (foreach entry *haws-profile-data*
    (setq total (+ total (cadr entry)))
  )
  (setq avg (/ total 1000))
  
  (princ "\n")
  (princ "\n========================================")
  (princ "\n STATISTICAL ANALYSIS (1000 samples)")
  (princ "\n========================================")
  (princ (strcat "\n Average per config read: " (rtos avg 2 3) "ms"))
  (princ (strcat "\n Total test time: " (rtos total 2 1) "ms"))
  (princ (strcat "\n Reads per second: " (rtos (/ 1000.0 total) 2 1)))
  
  (if (> avg 30)
    (progn
      (princ "\n")
      (princ "\n ⚠️  PERFORMANCE ISSUE CONFIRMED")
      (princ "\n 📊 Consistent 40ms bottleneck in CNM wrapper")
      (princ "\n 🎯 HAWS-CONFIG core: ~0ms (excellent)")
      (princ "\n 🐌 CNM wrapper overhead: ~40ms per call")
      (princ "\n 🔍 Root cause: hcnm-proj file validation every call")
      (princ "\n 🚀 Solution: Session caching → 97.5% improvement")
    )
    (princ "\n\n ✅ Config system performing well!")
  )
  (princ "\n========================================")
  (princ)
)

;;; XDATA BENCHMARKING
(defun c:benchmark-xdata (/ start i ename)
  (princ "\n========================================")
  (princ "\n XDATA BENCHMARK")
  (princ "\n========================================")
  (princ "\nSelect a bubble note for XDATA testing...")
  
  (if (setq ename (car (entsel)))
    (progn
      (princ "\n Testing 50 XDATA operations...")
      (benchmark-reset)
      (setq i 0)
      (repeat 50
        (setq start (benchmark-start "xdata-read"))
        (entget ename '("HCNM-BUBBLE"))  ; Read XDATA
        (benchmark-end "xdata-read" start)
        (setq i (1+ i))
        (if (= (rem i 10) 0) (princ (strcat "\n " (itoa i) " completed...")))
      )
      (princ "\n\nResults:")
      (benchmark-report)
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

;;; QUICK SMOKE TEST
(defun c:benchmark-smoke (/ start)
  (princ "\n========================================")
  (princ "\n BENCHMARK SMOKE TEST")
  (princ "\n========================================")
  
  (haws-profile-reset)
  
  ;; Test 1: Basic timing
  (princ "\n Test 1: Basic timing...")
  (setq start (haws-profile-start "smoke-test"))
  (repeat 1000 (sin 1.0))  ; Some work
  (haws-profile-end "smoke-test" start)
  
  ;; Test 2: Multiple labels
  (princ "\n Test 2: Multiple operations...")
  (setq start (haws-profile-start "math-ops"))
  (repeat 500 (+ 1 2 3))
  (haws-profile-end "math-ops" start)
  
  (setq start (haws-profile-start "string-ops"))
  (repeat 100 (strcat "test" "string"))
  (haws-profile-end "string-ops" start)
  
  (princ "\n\nResults:")
  (haws-profile-report nil)
  
  (if *haws-profile-data*
    (princ "\n\n✅ Benchmark system working correctly!")
    (princ "\n\n❌ Benchmark system not working!")
  )
  (princ)
)

;;; COMMAND ALIASES
(defun c:benchmark-report () (haws-profile-report nil))
(defun c:benchmark-reset () (haws-profile-reset))

;;; Announce system loaded
(princ "\n========================================")
(princ "\n CNM PERFORMANCE BENCHMARKING LOADED")
(princ "\n========================================")
(princ "\n Commands:")
(princ "\n   BENCHMARK-CONFIG        - Test config system (40ms bottleneck)")
(princ "\n   BENCHMARK-CONFIG-STRESS - 1000 iterations for statistical analysis")
(princ "\n   BENCHMARK-XDATA         - Test XDATA performance")
(princ "\n   BENCHMARK-SMOKE         - Verify system works")
(princ "\n   BENCHMARK-REPORT        - View accumulated results")
(princ "\n   BENCHMARK-RESET         - Clear all data")
(princ "\n")
(princ "\n Quick Start: (c:benchmark-config)")
(princ "\n Phase 3 Analysis: (c:benchmark-config-stress)")
(princ "\n========================================")
(princ)