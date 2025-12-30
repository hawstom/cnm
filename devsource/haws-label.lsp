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
;;;   READ-BIAS-DEGREES - Angle threshold for flipping text (default 110)
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

(defun c:haws-label (/ ent-data ent-name ent-pick ent-type label-text layer-name
                        layer-table pick-point readability-bias settings text-angle
                        text-height text-style-key text-style-name text-style-table)
  (haws-core-init 339)
  (haws-vsave '("CLAYER"))
  
  (setq settings (haws-label-read-settings))
  (setq readability-bias (car settings)
        text-style-table (cadr settings)
        layer-table (caddr settings))
  
  (prompt "\nSelect line, arc, or polyline: ")
  (setq ent-pick (nentsel))
  (if (not ent-pick)
    (progn (princ "\nNo entity selected.") (haws-vrstor) (exit))
  )
  
  (setq ent-name (car ent-pick)
        pick-point (cadr ent-pick)
        ent-data (entget ent-name)
        ent-type (cdr (assoc 0 ent-data))
        layer-name (cdr (assoc 8 ent-data)))
  
  (if (not (haws-label-valid-entity-type ent-type))
    (progn
      (alert (strcat "Unsupported entity type: " ent-type "\nPlease select a LINE, ARC, or POLYLINE."))
      (haws-vrstor)
      (exit)
    )
  )
  
  (setq label-text (haws-label-find-label layer-name layer-table))
  (if (not label-text)
    (progn
      (alert (strcat "No label defined for layer: " layer-name))
      (haws-vrstor)
      (exit)
    )
  )
  (setq text-style-key (haws-label-find-style-key layer-name layer-table))
  
  (setq text-style-name (haws-label-apply-style text-style-key text-style-table))
  
  (setq text-angle (haws-label-calc-angle ent-type ent-data ent-name pick-point))
  
  ;; Apply readability bias - flip text if upside-down
  ;; READABILITY-BIAS is the angle threshold (default 110 degrees)
  ;; Text between READABILITY-BIAS and (READABILITY-BIAS + 180) gets flipped
  (setq readability-bias (/ (* readability-bias pi) 180.0)) ; Convert to radians
  (if (< readability-bias text-angle (+ readability-bias pi))
    (setq text-angle (+ text-angle pi))
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
      (haws-setlayr (list (cadr style-info) "" ""))
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
