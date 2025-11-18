;; Test Lee Mac's anonymous block detection approach
;; Simple verification that we can find the user's reactive bubble

(defun c:test-anonymous-detection ( / ss i ename obj-blk effective-name found-count)
  (princ "\n=== Testing Anonymous Block Detection ===")
  
  ;; Test 1: Find all blocks with effective names starting with cnm-bubble
  (setq ss (ssget "_X" '((0 . "INSERT")))
        found-count 0)
  
  (if ss
    (progn
      (princ (strcat "\nFound " (itoa (sslength ss)) " total INSERT entities"))
      (setq i 0)
      (while (< i (sslength ss))
        (setq ename (ssname ss i)
              obj-blk (vlax-ename->vla-object ename))
        (if (vlax-property-available-p obj-blk 'EffectiveName)
          (progn
            (setq effective-name (vla-get-EffectiveName obj-blk))
            (if (and effective-name
                     (wcmatch (strcase effective-name) "CNM-BUBBLE*"))
              (progn
                (setq found-count (1+ found-count))
                (princ (strcat "\nFound CNM bubble: " effective-name
                              " (actual name: " (vla-get-Name obj-blk) ")")))
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n\nTotal CNM bubbles found: " (itoa found-count)))
    )
    (princ "\nNo INSERT entities found in drawing")
  )
  
  ;; Test 2: Check for XDATA on found bubbles
  (if (> found-count 0)
    (progn
      (princ "\n\n=== Testing XDATA Detection ===")
      (setq i 0)
      (while (< i (sslength ss))
        (setq ename (ssname ss i)
              obj-blk (vlax-ename->vla-object ename))
        (if (vlax-property-available-p obj-blk 'EffectiveName)
          (progn
            (setq effective-name (vla-get-EffectiveName obj-blk))
            (if (and effective-name
                     (wcmatch (strcase effective-name) "CNM-BUBBLE*"))
              (progn
                ;; Check for XDATA
                (if (entget ename '("HCNM-BUBBLE"))
                  (princ (strcat "\n✓ " effective-name " has HCNM-BUBBLE XDATA"))
                  (princ (strcat "\n✗ " effective-name " has NO HCNM-BUBBLE XDATA"))
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
    )
  )
  
  (princ "\n=== Test Complete ===")
  (princ)
)

(princ "\nAnonymous block detection test loaded. Run (c:test-anonymous-detection)")