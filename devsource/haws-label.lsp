;;; HAWS-LABEL.lsp - C:HAWS-LABEL command for CNM/HAWSEDC
;;; See devtools/docs/standards-03-names-and-symbols.md for naming conventions
;;;
;;; Command: haws-label (aliases: LABEL, LAB)
;;; Tracker ID: 339
;;;
;;; TO CUSTOMIZE: Edit haws-label-settings.lsp to define layer-specific text
;;; styles and labels for your drawing standards.
;;;
;;; Purpose:
;;;   Labels lines, arcs, and polylines with text aligned to the entity at the
;;;   pick point. Text orientation is perpendicular to the radial for curved
;;;   segments. Layer-specific text styles and labels from haws-label-settings.lsp.
;;;
;;; Usage:
;;;   1. Type haws-label (or LABEL or LAB)
;;;   2. Select line, arc, or polyline at desired label point
;;;   3. Text inserted at pick point, aligned with entity
;;;
;;; Settings File: haws-label-settings.lsp
;;;   READ-BIAS-DEGREES - Angle threshold for flipping text (default 140)
;;;   TEXT-STYLE - (key layer-name style-name) mappings
;;;   LAYER - (layer-pattern style-key label-text) mappings
;;;     Keys and layer names must be UPPERCASE
;;;     Layer patterns support wildcards for matching
;;;
;;; Features:
;;;   - Supports LINE, ARC, LWPOLYLINE, POLYLINE entities
;;;   - Wildcard layer matching for flexible configuration
;;;   - Automatic text style and layer selection per settings
;;;   - Handles curved polyline segments with bulge geometry
;;;   - Readability bias keeps text "right-side up"
;;;   - Text height from dimension style (haws-text-height-model)
;;(vl-acad-defun 'HAWS-MKLAYR)

(defun haws-clock-start (label) nil)
(defun haws-clock-end (label start-time) nil)
(defun haws-clock-report (sorted) nil)
(defun haws-clock-reset () nil)
(defun haws-clock-console-log (message) nil)

;;; HELPER FUNCTIONS FOR NUMBER EXTRACTION AND SUBSTITUTION

;; haws-ut-extract-number-after-tilde
;; Extracts the number (all digits) after the LAST tilde (|) in layer name
;; If no tilde found, returns empty string ""
;; Input: layer-name (e.g., "amr6-x-water-offsite|12" or "PROP-LPS-2")
;; Output: number string (e.g., "12") or "" if not found
(defun haws-label (layer-name / tilde-pos after-tilde char-code number-str)
  (setq tilde-pos (vl-string-search "|" layer-name))
  
  ;; If no tilde found, return empty string
  (if (not tilde-pos)
    ""
    (progn
      ;; Extract everything after the tilde
      (setq after-tilde (substr layer-name (+ tilde-pos 2)))  ; +2 to skip tilde and go to next char
      
      ;; Extract only numeric characters
      (setq number-str "")
      (foreach char (vl-string->list after-tilde)
        (if (and (>= char 48) (<= char 57))  ; ASCII 48-57 = 0-9
          (setq number-str (strcat number-str (chr char)))
        )
      )
      number-str
    )
  )
)

;; haws-ut-substitute-number
;; Replaces "#" in label text with the extracted number
;; If no number is found (empty string) and text has #", removes both # and "
;; Input: label-text (e.g., "#\"S LPS" or "#\"g"), number (e.g., "12" or "")
;; Output: substituted text (e.g., "12\"S LPS" or "g")
(defun haws-ut-substitute-number (label-text number-str / hash-pos)
  (if (and label-text (vl-string-search "#" label-text))
    (progn
      ;; If number is empty and label has #", remove both characters
      (if (and (equal number-str "") (vl-string-search "#\"" label-text))
        (vl-string-subst "" "#\"" label-text)
        ;; Otherwise just replace # with the number
        (vl-string-subst number-str "#" label-text)
      )
    )
    label-text
  )
)

(defun haws-clock-start (label) nil)

(defun c:haws-ut (/ angle-mode ent-data ent-name ent-pick ent-type label-text layer-name extracted-number
                        layer-table pick-point pt1 pt2 readability-bias settings text-angle
                        text-height text-style-key text-style-name text-style-table user-choice)
  (haws-core-init 339)
  (haws-vsave '("CLAYER"))
  
  ;; Initialize angle mode to AUTOMATIC if not already set
  (if (not (boundp '*haws-label-angle-mode*))
    (setq *haws-label-angle-mode* "AUTOMATIC")
  )
  
  (setq settings (haws-label-read-settings))
  (setq readability-bias (car settings)
        text-style-table (cadr settings)
        layer-table (caddr settings))
  
  ;; Convert readability bias to radians once
  (setq readability-bias (/ (* readability-bias pi) 180.0))
  
  (while T
    ;; Prompt for object selection with mode change option
    (setq ent-pick nil)
    (while (not ent-pick)
      (initget "Change")
      (setq ent-pick (nentsel (strcat "\nSelect line, arc, or polyline or [Change mode: Current " *haws-label-angle-mode* "]: ")))
      
      (if (= ent-pick "Change")
        (progn
          (if (= *haws-label-angle-mode* "MANUAL")
            (setq *haws-label-angle-mode* "AUTOMATIC")
            (setq *haws-label-angle-mode* "MANUAL")
          )
          (princ (strcat "\nMode changed to " *haws-label-angle-mode*))
          (setq ent-pick nil)
        )
        (if ent-pick
          (progn
            (setq ent-name (car ent-pick)
                  pick-point (cadr ent-pick)
                  ent-data (entget ent-name)
                  ent-type (cdr (assoc 0 ent-data)))
            (if (not (haws-label-valid-entity-type ent-type))
              (progn
                (alert (strcat "Unsupported entity type: " ent-type "\nPlease select a LINE, ARC, or POLYLINE."))
                (setq ent-pick nil)
              )
            )
          )
        )
      )
    )
    
    (if (not ent-pick)
      (progn (princ "\nNo entity selected.") (haws-vrstor) (haws-core-restore) (exit))
    )
  
  (setq layer-name (cdr (assoc 8 ent-data)))
  
  (princ (strcat "\nLayer name: " layer-name))
  
  (setq label-text (haws-label-find-label layer-name layer-table))
  (if (not label-text)
    (progn
      (alert (strcat "No label defined for layer: " layer-name "\n\nCheck haws-label-settings.lsp"))
      (haws-vrstor)
      (exit)
    )
  )
  
  ;; Extract number after tilde (if any) and substitute "#" in label text
  (setq extracted-number (haws-ut-extract-number-after-tilde layer-name))
  ;; Always substitute, even if number is empty (will remove #" if no number found)
  (setq label-text (haws-ut-substitute-number label-text extracted-number))
  (princ (strcat "\nLabel text after: " label-text))
  
  (setq text-style-key (haws-label-find-style-key layer-name layer-table))
  
  (setq text-style-name (haws-label-apply-style text-style-key text-style-table))
  
  ;; Calculate angle based on stored mode preference
  (if (= *haws-label-angle-mode* "MANUAL")
    (progn
      (setq pt1 (getpoint "\nFirst point for angle: ")
            pt2 (getpoint pt1 "\nSecond point for angle: "))
      (if (and pt1 pt2)
        (setq text-angle (angle pt1 pt2))
        (setq text-angle (haws-label-calc-angle ent-type ent-data ent-name pick-point))
      )
    )
    (progn
      (setq text-angle (haws-label-calc-angle ent-type ent-data ent-name pick-point))
      ;; Apply readability bias - flip text if upside-down
      ;; READABILITY-BIAS is the angle threshold (default 110 degrees)
      ;; Text between READABILITY-BIAS and (READABILITY-BIAS + 180) gets flipped
      (if (< readability-bias text-angle (+ readability-bias pi))
        (setq text-angle (+ text-angle pi))
      )
    )
  )
  ;; Normalize to 0-2π range
  (while (< text-angle 0) (setq text-angle (+ text-angle (* 2 pi))))
  (while (>= text-angle (* 2 pi)) (setq text-angle (- text-angle (* 2 pi))))
  
  ;; Snap pick point to nearest point on entity
  (setq pick-point (osnap pick-point "near"))
  
  (setq text-height (haws-text-height-model))
  
  ;; Create MTEXT with background mask
  (entmake (list
    '(0 . "MTEXT")
    '(100 . "AcDbEntity")
    '(100 . "AcDbMText")
    (cons 10 pick-point)              ; Insertion point
    (cons 40 text-height)             ; Text height
    (cons 71 5)                       ; Attachment point: 5 = Middle Center
    (cons 50 text-angle)              ; Rotation angle
    (cons 1 label-text)               ; Text content
    (cons 7 text-style-name)          ; Text style
    '(90 . 3)                         ; Background mask flag: 3 = use background fill
    '(63 . 256)                       ; Background fill color: 256 = drawing background
    '(45 . 1.1)                       ; Fill box scale (border offset factor)
    '(441 . 0)                        ; Background fill setting
  ))
  )
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)

(defun haws-label-read-settings (/ f1 i key layer-table rdlin readability-bias
                                    settings-data settings-file temp text-style-table)
  (setq settings-file (findfile "haws-label-settings.lsp"))
  (if (not settings-file)
    (progn (alert "Could not find haws-label-settings.lsp") (exit))
  )
  (setq *f1* (open settings-file "r"))
  (if (not *f1*)
    (progn (alert "Could not open haws-label-settings.lsp") (exit))
  )
  (setq readability-bias 110.0
        text-style-table '()
        layer-table '()
        settings-data '()
        i 0)
  (princ "\n")
  (while (setq rdlin (read-line *f1*))
    (princ "\rReading line ")
    (princ (setq i (1+ i)))
    (if (= 'LIST (type (setq temp (read rdlin))))
      (setq settings-data (cons temp settings-data))
    )
  )
  (close *f1*)
  (setq settings-data (reverse settings-data))
  (foreach rdlin settings-data
    (setq key (car rdlin))
    (cond
      ((= key "READ-BIAS-DEGREES") (setq readability-bias (cadr rdlin)))
      ((= key "TEXT-STYLE") (setq text-style-table (cons (cdr rdlin) text-style-table)))
      ((= key "LAYER") (setq layer-table (cons (cdr rdlin) layer-table)))
    )
  )
  (if (not text-style-table)
    (progn (alert "Error: No TEXT-STYLE entries in settings file") (exit))
  )
  (if (not layer-table)
    (progn (alert "Error: No LAYER entries in settings file") (exit))
  )
  (list readability-bias text-style-table layer-table)
)

(defun haws-label-valid-entity-type (ent-type)
  (or (= ent-type "LINE") (= ent-type "ARC") 
      (= ent-type "LWPOLYLINE") (= ent-type "POLYLINE"))
)

(defun haws-label-find-label (layer-name layer-table / entry result)
  (foreach entry layer-table
    (if (and (not result) (wcmatch (strcase layer-name) (car entry)))
      (setq result (caddr entry))
    )
  )
  result
)

(defun haws-label-find-style-key (layer-name layer-table / entry result)
  (foreach entry layer-table
    (if (and (not result) (wcmatch (strcase layer-name) (car entry)))
      (setq result (cadr entry))
    )
  )
  result
)

(defun haws-label-apply-style (text-style-key text-style-table / entry style-info)
  (setq style-info (assoc text-style-key text-style-table))
  (if style-info
    (progn
      (haws-mklayr (list (cadr style-info) "" ""))
      (setq style-info (caddr style-info))
      (if (tblsearch "STYLE" style-info)
        (setvar "TEXTSTYLE" style-info)
        (alert (strcat "Warning: Text style '" style-info "' not found in drawing"))
      )
      style-info
    )
    nil
  )
)

(defun haws-label-calc-angle (ent-type ent-data ent-name pick-point / text-angle)
  (setq text-angle 0.0)
  (cond
    ((= ent-type "LINE")
     (setq text-angle (angle (cdr (assoc 10 ent-data)) (cdr (assoc 11 ent-data))))
    )
    ((= ent-type "ARC")
     (setq text-angle (+ (angle (cdr (assoc 10 ent-data)) pick-point) (/ pi 2)))
    )
    ((or (= ent-type "LWPOLYLINE") (= ent-type "POLYLINE"))
     (setq text-angle (haws-label-calc-pline-angle ent-type ent-data ent-name pick-point))
    )
  )
  text-angle
)

(defun haws-label-calc-pline-angle (ent-type ent-data ent-name pick-point 
                                     / ang1 bulge cenpt closest-index d dist1 dist2
                                       i min-dist pair pt1 pt2 r vertex-list)
  (setq vertex-list (haws-label-get-vertices ent-type ent-data ent-name))
  (setq closest-index (haws-label-find-closest-seg vertex-list pick-point))
  (setq pt1 (car (nth closest-index vertex-list))
        pt2 (car (nth (1+ closest-index) vertex-list))
        bulge (cadr (nth closest-index vertex-list)))
  (if (and bulge (/= bulge 0))
    (progn
      (setq d (/ (distance pt1 pt2) 2)
            ang1 (atan (/ 1 bulge))
            r (/ d (sin (- pi (* 2 ang1))))
            cenpt (polar pt1 (+ (angle pt1 pt2) (- (* 2 ang1) (/ pi 2))) r))
      (+ (angle cenpt pick-point) (/ pi 2))
    )
    (angle pt1 pt2)
  )
)

(defun haws-label-get-vertices (ent-type ent-data ent-name 
                                 / bulge i pair pt1 vertex-list)
  (setq vertex-list '())
  (if (= ent-type "LWPOLYLINE")
    (progn
      (foreach pair ent-data
        (if (= (car pair) 10)
          (progn
            (setq bulge 0
                  pt1 (cdr (member pair ent-data)))
            (while (and pt1 (not (or (= (caar pt1) 42) (= (caar pt1) 10))))
              (setq pt1 (cdr pt1))
            )
            (if (and pt1 (= (caar pt1) 42))
              (setq bulge (cdar pt1))
            )
            (setq vertex-list (append vertex-list (list (list (cdr pair) bulge))))
          )
        )
      )
    )
    (progn
      (setq i ent-name)
      (while (setq i (entnext i))
        (setq pt1 (entget i))
        (if (= "VERTEX" (cdr (assoc 0 pt1)))
          (setq vertex-list
                (append vertex-list
                        (list (list (cdr (assoc 10 pt1))
                                   (if (assoc 42 pt1) (cdr (assoc 42 pt1)) 0)))))
        )
      )
    )
  )
  vertex-list
)

(defun haws-label-find-closest-seg (vertex-list pick-point 
                                     / closest-index dist1 dist2 i min-dist pt1 pt2)
  (setq min-dist 1e99
        closest-index 0
        i 0)
  (while (< i (1- (length vertex-list)))
    (setq pt1 (car (nth i vertex-list))
          pt2 (car (nth (1+ i) vertex-list))
          dist1 (distance pick-point pt1)
          dist2 (distance pick-point pt2))
    (if (< (setq dist1 (/ (+ dist1 dist2) 2)) min-dist)
      (setq min-dist dist1
            closest-index i)
    )
    (setq i (1+ i))
  )
  closest-index
)

(princ
  (strcat
    "\nHAWS-LABEL.LSP loaded. Type haws-label (or LABEL or LAB) to start."
    "\nTo customize labels: Edit haws-label-settings.lsp"
  )
)
(princ)
