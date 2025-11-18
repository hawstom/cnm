;; Repair function for mixed handle storage bug
;; Cleans up XDATA where handle-based auto-text has both empty and real handles

(defun c:repair-mixed-handle-xdata ( / ss i ename xdata-alist cleaned-xdata changed-p)
  (princ "\n=== REPAIRING MIXED HANDLE XDATA ===")
  
  ;; Find CNM bubbles with XDATA
  (setq ss (ssget "_X" '((0 . "INSERT"))))
  
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
              (progn
                (princ (strcat "\nChecking bubble: " (cdr (assoc 5 (entget ename)))))
                (setq xdata-alist (hcnm-xdata-read ename)
                      cleaned-xdata nil
                      changed-p nil)
                
                ;; Check each tag for mixed handle storage
                (foreach tag-pair xdata-alist
                  (setq tag (car tag-pair)
                        tag-data (cdr tag-pair)
                        cleaned-tag-data nil)
                  
                  (if (and (listp tag-data) (> (length tag-data) 1))
                    (progn
                      ;; Multiple entries - check for mixed handles
                      (setq has-empty nil has-real nil)
                      (foreach entry tag-data
                        (if (= (cdar entry) "") 
                          (setq has-empty t)
                          (setq has-real t)
                        )
                      )
                      
                      (if (and has-empty has-real)
                        (progn
                          (princ (strcat "\n  " tag ": Mixed handles detected - removing empty handle entries"))
                          ;; Keep only non-empty handle entries
                          (foreach entry tag-data
                            (if (/= (cdar entry) "")
                              (setq cleaned-tag-data (cons entry cleaned-tag-data))
                            )
                          )
                          (setq cleaned-tag-data (reverse cleaned-tag-data)
                                changed-p t)
                        )
                        (setq cleaned-tag-data tag-data)
                      )
                    )
                    (setq cleaned-tag-data tag-data)
                  )
                  
                  (setq cleaned-xdata (cons (cons tag cleaned-tag-data) cleaned-xdata))
                )
                
                ;; Write cleaned XDATA if changes made
                (if changed-p
                  (progn
                    (setq cleaned-xdata (reverse cleaned-xdata))
                    (hcnm-xdata-set-autotext ename cleaned-xdata)
                    (princ "\n  ✓ XDATA cleaned and saved")
                  )
                  (princ "\n  ✓ No mixed handles found")
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo INSERT entities found")
  )
  
  (princ "\n=== REPAIR COMPLETE ===")
  (princ)
)

(princ "\nMixed handle repair function loaded. Run (c:repair-mixed-handle-xdata)")