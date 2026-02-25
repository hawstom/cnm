;;;==============================================================================
;;; HAWS PERFORMANCE TIMING SYSTEM
;;;==============================================================================
;;; Copyright � 2025 Thomas Gail Haws
;;; 
;;; PURPOSE:
;;;   Performance timing for development and profiling.
;;;   Accumulates timing data and generates grouped summary reports.
;;;
;;; QUICK START:
;;;   1. Load timing system: (load "haws-clock.lsp")
;;;   2. Your instrumented code runs automatically
;;;   3. Check results: Open haws-clock.log (auto-written after each timing)
;;;      OR: (haws-clock-report nil)  ; Console output + write to file
;;;
;;; WHAT YOU GET:
;;;   Grouped summary showing calls/total/average per operation label:
;;;
;;;   HawsEDC Performance Timing Log - 2025-12-02 14:30:15
;;;   ======================================================================
;;;   insert-auto-alignment: 12 calls, 2400ms total, 200ms avg
;;;   reactor-callback: 156 calls, 78000ms total, 500ms avg
;;;   config-getvar-total: 45 calls, 1250ms total, 27ms avg
;;;   ======================================================================
;;;
;;; HOW TO INSTRUMENT CODE:
;;;   ;; Start timing
;;;   (setq start (haws-clock-start "operation-name"))
;;;   
;;;   ;; Your code here...
;;;   
;;;   ;; End timing (auto-writes to log)
;;;   (haws-clock-end "operation-name" start)
;;;   
;;;   ;; Custom detail messages (optional)
;;;   (haws-clock-console-log "  [DETAIL] Substep took 15ms")
;;;
;;; PRODUCTION SAFETY:
;;;   - If haws-clock.lsp NOT loaded: dummy stubs in edclib.lsp return NIL
;;;   - Zero overhead when not loaded (negligible function call)
;;;   - Load manually for development/profiling only
;;;   - Production code can safely call timing functions without guards
;;;
;;; AVAILABLE FUNCTIONS:
;;;   (haws-clock-start "label")       - Begin timing, returns start time
;;;   (haws-clock-end "label" start)   - End timing, auto-writes to log
;;;   (haws-clock-report nil)          - Console summary + write to file
;;;   (haws-clock-reset)               - Clear accumulated data
;;;   (haws-clock-console-log "msg")   - Write custom message to log
;;;
;;; DATA STRUCTURE:
;;;   Global: *haws-clock-data*
;;;   Format: ((label elapsed timestamp) ...)
;;;   Example: (("reactor-callback" 500 15234567) 
;;;             ("reactor-callback" 485 15235100) ...)
;;;   
;;;   Grouping: Multiple calls to same label accumulate automatically
;;;   Output: Shows total calls, total time, average time per label
;;;
;;; FILE OUTPUT LOCATION:
;;;   haws-clock.log in drawing folder (getvar "dwgprefix")
;;;   Auto-written after EACH timing (cumulative results)
;;;
;;; WHY "CLOCKING" NOT "PROFILING"?
;;;   "Profiling" conflicts with civil engineering terms (road profiles,
;;;   surface profiles). We use "clocking" to avoid confusion.
;;;
;;; TECHNICAL DETAILS:
;;;   - Uses MILLISECS system variable (AutoCAD 2017+) for accuracy
;;;   - Chronological order preserved in *haws-clock-data*
;;;   - Summary groups by label, shows aggregate statistics
;;;   - No commands needed (previous c:write-performance-log removed)
;;;
;;; COMPILATION:
;;;   Compiles normally, distributes with HawsEDC.
;;;   Users can load for development/troubleshooting.
;;;==============================================================================
;;; Global data storage
(setq *haws-clock-data* '())
;;; Start timing - returns current millisecond counter
(defun haws-clock-start (label)
  (getvar "MILLISECS")
)
;;; End timing - calculates elapsed time and stores result
(defun haws-clock-end (label start-time / elapsed)
  (setq elapsed (- (getvar "MILLISECS") start-time))
  (setq *haws-clock-data* 
    (cons (list label elapsed (getvar "MILLISECS")) *haws-clock-data*))
  (haws-clock-file-log)
  elapsed
)
;;; Write accumulated timing data to log file with grouped summary
(defun haws-clock-file-log (/ log-path f group-data label-totals
                                 label elapsed call-count total-time avg-time)
  (setq log-path (strcat (getvar "dwgprefix") "haws-clock.log"))
  (setq label-totals '())
  (foreach entry *haws-clock-data*
    (setq label (car entry)
          elapsed (cadr entry))
    (if (setq group-data (assoc label label-totals))
      (setq label-totals
        (subst
          (list label
                (1+ (cadr group-data))
                (+ (caddr group-data) elapsed))
          group-data
          label-totals))
      (setq label-totals
        (cons (list label 1 elapsed) label-totals))))
  (if (setq f (haws-open log-path "w"))
    (progn
      (write-line (strcat "HawsEDC Performance Timing Log - " 
                          (menucmd "M=$(edtime,$(getvar,date),YYYY-MO-DD HH:MM:SS)"))
                  f)
      (write-line "======================================================================" f)
      (if label-totals
        (progn
          (setq label-totals (reverse label-totals))
          (foreach group label-totals
            (setq label (car group)
                  call-count (cadr group)
                  total-time (caddr group)
                  avg-time (/ total-time call-count))
            (write-line
              (strcat label ": "
                      (itoa call-count) " calls, "
                      (itoa total-time) "ms total, "
                      (itoa avg-time) "ms avg")
              f)))
        (write-line "No timing data collected." f))
      (haws-close f)
      T)
    nil))
;;; Display timing summary to console AND write to file
(defun haws-clock-report (verbose-p / label-totals group label call-count
                                      total-time avg-time group-data elapsed)
  (setq label-totals '())
  (foreach entry *haws-clock-data*
    (setq label (car entry)
          elapsed (cadr entry))
    (if (setq group-data (assoc label label-totals))
      (setq label-totals
        (subst
          (list label
                (1+ (cadr group-data))
                (+ (caddr group-data) elapsed))
          group-data
          label-totals))
      (setq label-totals
        (cons (list label 1 elapsed) label-totals))))
  (princ "\n======================================================================")
  (princ "\nHawsEDC Performance Timing Summary")
  (princ "\n======================================================================")
  (if label-totals
    (progn
      (setq label-totals (reverse label-totals))
      (foreach group label-totals
        (setq label (car group)
              call-count (cadr group)
              total-time (caddr group)
              avg-time (/ total-time call-count))
        (princ (strcat "\n" label ": "
                       (itoa call-count) " calls, "
                       (itoa total-time) "ms total, "
                       (itoa avg-time) "ms avg"))))
    (princ "\nNo timing data collected."))
  (princ "\n======================================================================")
  (princ)
  (haws-clock-file-log)
)
;;; Clear accumulated timing data and write empty log
(defun haws-clock-reset ()
  (setq *haws-clock-data* '())
  (haws-clock-file-log)
  (princ "\nTiming data cleared.")
  (princ)
)
;;; Write custom message to log file (append mode)
(defun haws-clock-console-log (message / log-path f)
  (setq log-path (strcat (getvar "dwgprefix") "haws-clock.log"))
  (if (setq f (haws-open log-path "a"))
    (progn
      (write-line message f)
      (haws-close f)
      T)
    nil)
)

