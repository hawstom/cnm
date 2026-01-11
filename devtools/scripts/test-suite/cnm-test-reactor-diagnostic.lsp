;; CNM Test Suite - Reactor Diagnostic
;; Comprehensive reactor state analysis
(defun c:test-reactor-diagnostic ( / reactor-data reactor owners-list data-owners vlr-owner-handles
                                     missing-from-vlr extra-in-vlr report-lines msg owner-list owner-handle
                                     owners-list-2 owners-list-3 obj-owner en-owner eg-owner)
  ;; Comprehensive diagnostic of reactor state
  ;; Compares data structure vs vlr-data vs vlr-owners list to find discrepancies
  (setq msg "\n\n=== REACTOR DIAGNOSTIC REPORT ===\n")
  (haws-debug msg)(princ msg)
  (setq report-lines (list))
  ;; Workaround: vlr-reactors returns nil on first call (see cnm.lsp line 11597)
  ;; Call multiple times to ensure we get valid reactor
  (setq msg "Priming vlr-reactors (call 1)...")
  (haws-debug msg)(print msg)
  (vlr-reactors)
  (setq msg "Priming vlr-reactors (call 2)...")
  (haws-debug msg)(print msg)
  (vlr-reactors)
  (setq msg "Priming vlr-reactors (call 3)...")
  (haws-debug msg)(print msg)
  (vlr-reactors)
  ;; Get reactor directly - let it crash if function doesn't exist
  (setq msg "Calling hcnm-bn-get-reactor...")
  (haws-debug msg)(print msg)
  (setq reactor (hcnm-bn-get-reactor))
  (setq msg (strcat "Reactor type: " (vl-prin1-to-string (type reactor))))
  (haws-debug msg)(print msg)
  (cond
    ((not reactor)
     (setq msg "\nERROR: No VLR-OBJECT-REACTOR found (hcnm-bn-get-reactor returned nil)")
     (haws-debug msg)(print msg)
     (setq report-lines (cons "ERROR: No VLR-OBJECT-REACTOR" report-lines))
    )
    (t
     ;; Get data from reactor
     (setq msg "Getting vlr-data from reactor...")
     (haws-debug msg)(print msg)
     (setq reactor-data (vlr-data reactor))
     (setq msg (strcat "vlr-data type: " (vl-prin1-to-string (type reactor-data))))
     (haws-debug msg)(print msg)
     (setq msg (strcat "vlr-data length: " (if reactor-data (itoa (length reactor-data)) "0")))
     (haws-debug msg)(print msg)
     (cond
       ((not reactor-data)
        (setq msg "\nERROR: Reactor exists but vlr-data returned nil")
        (haws-debug msg)(print msg)
        (setq report-lines (cons "ERROR: No reactor data structure" report-lines))
       )
       ((not (assoc "HCNM-BUBBLE" reactor-data))
        (setq msg "\nERROR: Reactor data exists but no HCNM-BUBBLE key found")
        (haws-debug msg)(print msg)
        (setq msg (strcat "Available keys: " (vl-prin1-to-string (mapcar 'car reactor-data))))
        (haws-debug msg)(print msg)
        (setq report-lines (cons "ERROR: No HCNM-BUBBLE key in reactor data" report-lines))
       )
       (t
        ;; Extract all leader handles from data structure
        (setq msg "Extracting leader handles from data structure...")
        (haws-debug msg)(print msg)
        (setq data-leaders (list))
        (setq msg (strcat "HCNM-BUBBLE assoc: " (vl-prin1-to-string (assoc "HCNM-BUBBLE" reactor-data))))
        (haws-debug msg)(print msg)
        ;; Use haws-nested-list-get utility (handles uniform list structure)
        (setq owner-list (haws-nested-list-get reactor-data '("HCNM-BUBBLE")))
        (setq msg (strcat "Owner entries count: " (itoa (length owner-list))))
        (haws-debug msg)(print msg)
        ;; Extract all owner handles from data structure
        ;; These are reference object handles (alignments, pipes, surfaces) or "" for handleless coords
        (setq data-owners (list))
        (foreach owner-entry owner-list
          (setq owner-handle (car owner-entry))
          (if (not (equal owner-handle ""))  ; Skip empty handles (N/E/NE types)
            (setq data-owners (cons owner-handle data-owners))
          )
        )
        ;; Extract owner handles from vlr-owners list
        (setq msg (strcat "Data owners extracted: " (itoa (length data-owners))))
        (haws-debug msg)(print msg)
        (setq msg "Getting vlr-owners list (call 1)...")
        (haws-debug msg)(print msg)
        (setq owners-list (vlr-owners reactor))
        (setq msg (strcat "vlr-owners count (call 1): " (itoa (length owners-list))))
        (haws-debug msg)(print msg)
        ;; Call vlr-owners multiple times to verify consistency
        (setq msg "Getting vlr-owners list (call 2)...")
        (haws-debug msg)(print msg)
        (setq owners-list-2 (vlr-owners reactor))
        (setq msg (strcat "vlr-owners count (call 2): " (itoa (length owners-list-2))))
        (haws-debug msg)(print msg)
        (setq msg "Getting vlr-owners list (call 3)...")
        (haws-debug msg)(print msg)
        (setq owners-list-3 (vlr-owners reactor))
        (setq msg (strcat "vlr-owners count (call 3): " (itoa (length owners-list-3))))
        (haws-debug msg)(print msg)
        ;; Use first call for comparison (they should all be identical)
        (setq msg (strcat "vlr-owners final count: " (itoa (length owners-list))))
        (haws-debug msg)(print msg)
        ;; Extract handles from VLR owners (vlr-owners returns VLA-OBJECTs)
        (setq vlr-owner-handles (list))
        (foreach obj-owner owners-list
          ;; vlr-owners returns VLA-OBJECTs - convert to ENAME to safely check validity
          (setq msg (strcat "Owner type: " (vl-prin1-to-string (type obj-owner))))
          (haws-debug msg)(print msg)
          ;; Convert VLA-OBJECT to ENAME
          (setq en-owner (vl-catch-all-apply 'vlax-vla-object->ename (list obj-owner)))
          (cond
            ((not (vl-catch-all-error-p en-owner))
             ;; Successfully converted - now get entity data
             (setq eg-owner (entget en-owner))
             (cond
               (eg-owner
                (setq owner-handle (cdr (assoc 5 eg-owner)))
                (setq msg (strcat "  Handle: " owner-handle))
                (haws-debug msg)(print msg)
                (setq vlr-owner-handles (cons owner-handle vlr-owner-handles))
               )
               (t
                (setq msg "  ERROR: Entity erased or not found")
                (haws-debug msg)(print msg)
               )
             )
            )
            (t
             (setq msg (strcat "  ERROR converting to ENAME: " (vl-catch-all-error-message en-owner)))
             (haws-debug msg)(print msg)
            )
          )
        )
        ;; Find discrepancies
        (setq missing-from-vlr (list))
        (foreach data-handle data-owners
          (cond
            ((not (member data-handle vlr-owner-handles))
             (setq missing-from-vlr (cons data-handle missing-from-vlr))
            )
          )
        )
        (setq extra-in-vlr (list))
        (foreach vlr-handle vlr-owner-handles
          (cond
            ((not (member vlr-handle data-owners))
             (setq extra-in-vlr (cons vlr-handle extra-in-vlr))
            )
          )
        )
        ;; Print detailed report
        (setq msg (strcat "\nData structure owners: " (itoa (length data-owners))))
        (haws-debug msg)(print msg)
        (setq msg (strcat "\nvlr-owners list count: " (itoa (length vlr-owner-handles))))
        (haws-debug msg)(print msg)
        (setq report-lines (cons 
          (strcat "Data owners: " (itoa (length data-owners))
                 ", vlr-owners: " (itoa (length vlr-owner-handles)))
          report-lines))
        (cond
          (missing-from-vlr
           (setq msg "\n\n*** MISSING FROM vlr-owners LIST (in vlr-data but not in vlr-owners): ***")
           (haws-debug msg)(print msg)
           (foreach handle missing-from-vlr
             (setq msg (strcat "\n  Owner handle: " handle))
             (haws-debug msg)(print msg)
           )
           (setq report-lines (cons 
             (strcat "MISSING FROM vlr-owners: " (itoa (length missing-from-vlr)) " owners")
             report-lines))
          )
        )
        (cond
          (extra-in-vlr
           (setq msg "\n\n*** EXTRA IN vlr-owners LIST (in vlr-owners but not in vlr-data): ***")
           (haws-debug msg)(print msg)
           (foreach handle extra-in-vlr
             (setq msg (strcat "\n  Owner handle: " handle))
             (haws-debug msg)(print msg)
           )
           (setq report-lines (cons 
             (strcat "EXTRA IN vlr-owners: " (itoa (length extra-in-vlr)) " owners")
             report-lines))
          )
        )
        (cond
          ((and (= (length missing-from-vlr) 0) (= (length extra-in-vlr) 0))
           (setq msg "\n\n✅ vlr-data and vlr-owners list ARE SYNCHRONIZED")
           (haws-debug msg)(print msg)
          )
          (t
           (setq msg "\n\n⚠️ vlr-data and vlr-owners list ARE OUT OF SYNC")
           (haws-debug msg)(print msg)
          )
        )
       )
      )
    )
  )
  ;; Write to report if test-write-report-entry available
  (cond
    ((and (boundp 'test-write-report-entry) test-write-report-entry)
     (test-write-report-entry 
       "REACTOR DIAGNOSTIC"
       (if (and reactor-data reactor 
                (= (length missing-from-vlr) 0) 
                (= (length extra-in-vlr) 0))
         "PASS"
         "FAIL")
       (apply 'strcat (reverse report-lines)))
    )
  )
  (princ "\n")
  (princ)
)
(princ "\nReactor diagnostic loaded. Type TEST-REACTOR-DIAGNOSTIC to run.")
(princ)

