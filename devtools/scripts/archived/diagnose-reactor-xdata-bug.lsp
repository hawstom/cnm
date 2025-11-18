;; Diagnostic script for reactor/XDATA consistency bug
;; Shows XDATA vs reactor data structure for comparison

(defun c:diagnose-reactor-xdata-bug ( / ss ename xdata-alist reactor reactor-data handle-bubble)
  (princ "\n=== REACTOR/XDATA CONSISTENCY DIAGNOSTIC ===")
  
  ;; Find CNM bubbles using EffectiveName method (same as working test)
  (setq ss (ssget "_X" '((0 . "INSERT")))
        bubbles-with-xdata nil)
  
  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq ename (ssname ss i)
              obj-blk (vlax-ename->vla-object ename))
        (if (vlax-property-available-p obj-blk 'EffectiveName)
          (progn
            (setq effective-name (vla-get-EffectiveName obj-blk))
            (if (and effective-name
                     (wcmatch (strcase effective-name) "CNM-BUBBLE*")
                     (entget ename '("HCNM-BUBBLE")))
              (setq bubbles-with-xdata (cons ename bubbles-with-xdata))
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  
  ;; Create selection set from found bubbles
  (if bubbles-with-xdata
    (progn
      (setq ss (ssadd))
      (foreach ename bubbles-with-xdata
        (ssadd ename ss)
      )
    )
    (setq ss nil)
  )
  
  (if (not ss)
    (princ "\nNo bubbles with XDATA found")
    (progn
      (princ (strcat "\nFound " (itoa (sslength ss)) " bubbles with XDATA"))
      
      ;; Find the reactor
      (setq reactor (car (vl-remove-if-not
                           '(lambda (r)
                              (and (listp (vlr-data r))
                                   (assoc "HCNM-BUBBLE" (vlr-data r))))
                           (cdar (vlr-reactors :vlr-object-reactor)))))
      
      (if reactor
        (progn
          (setq reactor-data (cdr (assoc "HCNM-BUBBLE" (vlr-data reactor))))
          (princ "\n✓ Found HCNM-BUBBLE reactor")
          
          ;; Check first bubble in detail  
          (setq ename (ssname ss 0)
                handle-bubble (cdr (assoc 5 (entget ename)))
                xdata-alist (hcnm-xdata-read ename))
                
          (princ (strcat "\n\nBUBBLE ANALYSIS: " handle-bubble))
          (princ "\n--- XDATA ---")
          (foreach pair xdata-alist
            (princ (strcat "\n  " (car pair) ": " (vl-prin1-to-string (cdr pair))))
          )
          
          (princ "\n--- REACTOR DATA ---")
          (foreach handle-entry reactor-data
            (foreach bubble-entry (cdr handle-entry)
              (if (= (car bubble-entry) handle-bubble)
                (progn
                  (princ (strcat "\n  Reference handle: " (car handle-entry)))
                  (princ (strcat "\n  Tags: " (vl-prin1-to-string (cdr bubble-entry))))
                )
              )
            )
          )
          
          (princ "\n--- BUG ANALYSIS ---")
          (setq tag-xdata (cdr (assoc "NOTETXT2" xdata-alist)))
          (if (and tag-xdata (> (length tag-xdata) 1))
            (progn
              (princ (strcat "\n  PROBLEM: Multiple StaOff entries with different handles:"))
              (foreach entry tag-xdata
                (princ (strcat "\n    Handle: '" (cdar entry) "' -> '" (cdr entry) "'"))
              )
              (princ "\n  SOLUTION: Clean up mixed handle storage - StaOff should use alignment handle only")
            )
            (princ "\n  No multiple entries detected")
          )
        )
        (princ "\n❌ No HCNM-BUBBLE reactor found")
      )
    )
  )
  (princ "\n=== DIAGNOSTIC COMPLETE ===")
  (princ)
)

(princ "\nReactor/XDATA diagnostic loaded. Run (c:diagnose-reactor-xdata-bug)")