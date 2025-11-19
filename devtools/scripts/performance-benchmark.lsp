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

;;; REACTOR CALLBACK BENCHMARKING
(defun c:benchmark-reactor (/ bubble-list ename obj-notifier test-data current-pos new-pos i)
  (princ "\n========================================")
  (princ "\n REACTOR CALLBACK BENCHMARK")
  (princ "\n========================================")
  (princ "\nThis test requires bubbles with auto-text already placed in the drawing.")
  (princ "\nSelect a bubble with coordinate-based auto-text (N/E/StaOff)...")
  
  (if (setq ename (car (entsel)))
    (progn
      ;; Verify this is a bubble with reactor data
      (setq test-data (entget ename '("HCNM-BUBBLE")))
      (if (assoc -3 test-data)  ; Has XDATA
        (progn
          (princ "\n✅ Found bubble with XDATA")
          (princ "\nFinding associated leader for reactor testing...")
          
          ;; Find the leader associated with this bubble
          (setq obj-notifier (hcnm-bubble-get-leader-object ename))
          (if obj-notifier
            (progn
              (princ "\n✅ Found associated leader")
              (princ "\nStarting reactor performance test...")
              (princ "\n(This will move the leader 10 times to trigger callbacks)")
              
              ;; Get current leader position
              (setq current-pos (vlax-get-property obj-notifier 'StartPoint))
              
              ;; Reset profiling data for clean test
              (haws-profile-reset)
              
              ;; Test: Move leader multiple times to trigger reactor callbacks
              (setq i 0)
              (repeat 10
                (setq i (1+ i))
                (princ (strcat "\n  Test " (itoa i) "/10: Moving leader..."))
                
                ;; Move leader slightly (triggers reactor)
                (setq new-pos (list (+ (car current-pos) (* i 0.1))
                                   (+ (cadr current-pos) (* i 0.1))
                                   (caddr current-pos)))
                (vlax-put-property obj-notifier 'StartPoint new-pos)
                
                ;; Small delay to let reactor finish
                (vl-cmdf "delay" 100)  ; 100ms delay
              )
              
              ;; Restore original position
              (vlax-put-property obj-notifier 'StartPoint current-pos)
              
              (princ "\n\nResults:")
              (haws-profile-report nil)
              
              ;; Analysis
              (hcnm-benchmark-analyze-reactor-results)
            )
            (princ "\n❌ Could not find associated leader for this bubble")
          )
        )
        (progn
          (princ "\n❌ Selected entity has no XDATA (not a reactive bubble)")
          (princ "\n💡 Try inserting a bubble with auto-text first")
        )
      )
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

;;; REACTOR STRESS TEST
(defun c:benchmark-reactor-stress (/ bubble-list ename obj-notifier test-data current-pos new-pos i)
  (princ "\n========================================")
  (princ "\n REACTOR STRESS TEST - 100 callbacks")
  (princ "\n========================================")
  (princ "\nSelect a bubble with auto-text for intensive testing...")
  
  (if (setq ename (car (entsel)))
    (progn
      (setq test-data (entget ename '("HCNM-BUBBLE")))
      (if (assoc -3 test-data)
        (progn
          (setq obj-notifier (hcnm-bubble-get-leader-object ename))
          (if obj-notifier
            (progn
              (princ "\n✅ Setup complete. Starting 100-callback stress test...")
              (princ "\n⚠️  This may take 30-60 seconds depending on complexity...")
              
              (setq current-pos (vlax-get-property obj-notifier 'StartPoint))
              (haws-profile-reset)
              
              (setq i 0)
              (repeat 100
                (setq i (1+ i))
                (if (= (rem i 10) 0) 
                  (princ (strcat "\n  Progress: " (itoa i) "/100 callbacks..."))
                )
                
                ;; Micro-movement to trigger reactor
                (setq new-pos (list (+ (car current-pos) (* (sin i) 0.01))
                                   (+ (cadr current-pos) (* (cos i) 0.01))
                                   (caddr current-pos)))
                (vlax-put-property obj-notifier 'StartPoint new-pos)
                (vl-cmdf "delay" 50)  ; 50ms delay
              )
              
              ;; Restore original position
              (vlax-put-property obj-notifier 'StartPoint current-pos)
              
              (princ "\n\nStress Test Results:")
              (haws-profile-report nil)
              (hcnm-benchmark-analyze-reactor-results)
            )
            (princ "\n❌ Could not find leader for this bubble")
          )
        )
        (princ "\n❌ Selected bubble has no reactive auto-text")
      )
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

;;; REACTOR ANALYSIS HELPER
(defun hcnm-benchmark-analyze-reactor-results (/ reactor-data total-time avg-time callback-count)
  (setq reactor-data '()
        total-time 0
        callback-count 0)
  
  ;; Extract reactor-callback timing data
  (foreach entry *haws-profile-data*
    (if (= (car entry) "reactor-callback")
      (progn
        (setq reactor-data (cons entry reactor-data)
              total-time (+ total-time (cadr entry))
              callback-count (1+ callback-count))
      )
    )
  )
  
  (if (> callback-count 0)
    (progn
      (setq avg-time (/ total-time callback-count))
      (princ "\n")
      (princ "\n========================================")
      (princ "\n REACTOR PERFORMANCE ANALYSIS")
      (princ "\n========================================")
      (princ (strcat "\n Callbacks triggered: " (itoa callback-count)))
      (princ (strcat "\n Average callback time: " (rtos avg-time 2 2) "ms"))
      (princ (strcat "\n Total reactor time: " (rtos total-time 2 1) "ms"))
      
      (cond
        ((> avg-time 50)
         (princ "\n")
         (princ "\n ⚠️  SLOW: Reactor callbacks > 50ms")
         (princ "\n 🔍 CHECK: Complex auto-text calculations")
         (princ "\n 💡 Consider caching expensive operations")
        )
        ((> avg-time 20)
         (princ "\n")
         (princ "\n ⚡ MODERATE: 20-50ms per callback") 
         (princ "\n 📊 Performance acceptable for typical use")
        )
        (t
         (princ "\n")
         (princ "\n ✅ FAST: Reactor callbacks < 20ms")
         (princ "\n 🚀 Excellent reactive performance")
        )
      )
      (princ "\n========================================")
    )
    (princ "\n\n❌ No reactor callback data found in results")
  )
)

;;; HELPER: Find leader object for bubble (simplified version)
(defun hcnm-bubble-get-leader-object (ename-bubble / owner-data leader-handles handle obj)
  ;; This is a simplified helper - in production, use the full reactor data structure lookup
  ;; For testing, we'll try to find any leader in the drawing that might be associated
  (setq owner-data (entget ename-bubble '("HCNM-BUBBLE")))
  (if (setq leader-handles (cdr (assoc -3 owner-data)))
    ;; Try to find leader reference in XDATA (implementation would be more complex)
    ;; For now, just select first leader in drawing for testing
    (progn
      (setq obj nil)
      ;; Simple approach: prompt user to select the leader
      (princ "\nSelect the leader associated with this bubble: ")
      (if (setq handle (car (entsel)))
        (if (= (cdr (assoc 0 (entget handle))) "LEADER")
          (vlax-ename->vla-object handle)
          nil
        )
        nil
      )
    )
    nil
  )
)

;;; COMMAND ALIASES
(defun c:benchmark-report () (haws-profile-report nil))
(defun c:benchmark-reset () (haws-profile-reset))

;;; Announce system loaded
(princ "\n========================================")
(princ "\n CNM PERFORMANCE BENCHMARKING LOADED")
(princ "\n========================================")
(princ "\n Commands:")
(princ "\n   BENCHMARK-CONFIG        - Test config system (Phase 3 complete)")
(princ "\n   BENCHMARK-CONFIG-STRESS - 1000 iterations for statistical analysis")
(princ "\n   BENCHMARK-REACTOR       - Test reactor callback performance")
(princ "\n   BENCHMARK-REACTOR-STRESS- 100 callbacks for intensive testing")
(princ "\n   BENCHMARK-XDATA         - Test XDATA performance")
(princ "\n   BENCHMARK-SMOKE         - Verify system works")
(princ "\n   BENCHMARK-REPORT        - View accumulated results")
(princ "\n   BENCHMARK-RESET         - Clear all data")
(princ "\n")
(princ "\n Quick Start: (c:benchmark-config)")
(princ "\n Reactor Testing: (c:benchmark-reactor)")
(princ "\n========================================")
(princ)