;;; HAWS-LABEL.lsp - C:HAWS-LABEL command for CNM/HAWSEDC
;;; See devtools/docs/standards_03_names_and_symbols.md for naming conventions
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
;;;   TEXT_STYLE - (key layer-name style-name) mappings
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

(defun c:haws-label (/ ent_data ent_name ent_pick ent_type label_text layer_name
                        layer_table pick_point readability_bias settings text_angle
                        text_height text_style_key text_style_name text_style_table)
  (haws-core-init 339)
  (haws-vsave '("CLAYER"))
  
  (setq settings (haws_label_read_settings))
  (setq readability_bias (car settings)
        text_style_table (cadr settings)
        layer_table (caddr settings))
  
  (prompt "\nSelect line, arc, or polyline: ")
  (setq ent_pick (nentsel))
  (if (not ent_pick)
    (progn (princ "\nNo entity selected.") (haws-vrstor) (exit))
  )
  
  (setq ent_name (car ent_pick)
        pick_point (cadr ent_pick)
        ent_data (entget ent_name)
        ent_type (cdr (assoc 0 ent_data))
        layer_name (cdr (assoc 8 ent_data)))
  
  (if (not (haws_label_valid_entity_type ent_type))
    (progn
      (alert (strcat "Unsupported entity type: " ent_type "\nPlease select a LINE, ARC, or POLYLINE."))
      (haws-vrstor)
      (exit)
    )
  )
  
  (setq label_text (haws_label_find_label layer_name layer_table))
  (if (not label_text)
    (progn
      (alert (strcat "No label defined for layer: " layer_name))
      (haws-vrstor)
      (exit)
    )
  )
  (setq text_style_key (haws_label_find_style_key layer_name layer_table))
  
  (setq text_style_name (haws_label_apply_style text_style_key text_style_table))
  
  (setq text_angle (haws_label_calc_angle ent_type ent_data ent_name pick_point))
  
  ;; Apply readability bias - flip text if upside-down
  ;; READABILITY_BIAS is the angle threshold (default 110 degrees)
  ;; Text between READABILITY_BIAS and (READABILITY_BIAS + 180) gets flipped
  (setq readability_bias (/ (* readability_bias pi) 180.0)) ; Convert to radians
  (if (< readability_bias text_angle (+ readability_bias pi))
    (setq text_angle (+ text_angle pi))
  )
  ;; Normalize to 0-2π range
  (while (< text_angle 0) (setq text_angle (+ text_angle (* 2 pi))))
  (while (>= text_angle (* 2 pi)) (setq text_angle (- text_angle (* 2 pi))))
  
  ;; Snap pick point to nearest point on entity
  (setq pick_point (osnap pick_point "near"))
  
  (setq text_height (haws-text-height-model))
  
  ;; Create MTEXT with background mask
  (entmake (list
    '(0 . "MTEXT")
    '(100 . "AcDbEntity")
    '(100 . "AcDbMText")
    (cons 10 pick_point)              ; Insertion point
    (cons 40 text_height)             ; Text height
    (cons 71 5)                       ; Attachment point: 5 = Middle Center
    (cons 50 text_angle)              ; Rotation angle
    (cons 1 label_text)               ; Text content
    (cons 7 text_style_name)          ; Text style
    '(90 . 3)                         ; Background mask flag: 3 = use background fill
    '(63 . 256)                       ; Background fill color: 256 = drawing background
    '(45 . 1.1)                       ; Fill box scale (border offset factor)
    '(441 . 0)                        ; Background fill setting
  ))

  
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)

(defun haws_label_read_settings (/ f1 i key layer_table rdlin readability_bias
                                    settings_data settings_file temp text_style_table)
  (setq settings_file (findfile "haws-label-settings.lsp"))
  (if (not settings_file)
    (progn (alert "Could not find haws-label-settings.lsp") (exit))
  )
  (setq f1 (open settings_file "r"))
  (if (not f1)
    (progn (alert "Could not open haws-label-settings.lsp") (exit))
  )
  (setq readability_bias 110.0
        text_style_table '()
        layer_table '()
        settings_data '()
        i 0)
  (princ "\n")
  (while (setq rdlin (read-line f1))
    (princ "\rReading line ")
    (princ (setq i (1+ i)))
    (if (= 'LIST (type (setq temp (read rdlin))))
      (setq settings_data (cons temp settings_data))
    )
  )
  (close f1)
  (setq settings_data (reverse settings_data))
  (foreach rdlin settings_data
    (setq key (car rdlin))
    (cond
      ((= key "READ-BIAS-DEGREES") (setq readability_bias (cadr rdlin)))
      ((= key "TEXT_STYLE") (setq text_style_table (cons (cdr rdlin) text_style_table)))
      ((= key "LAYER") (setq layer_table (cons (cdr rdlin) layer_table)))
    )
  )
  (if (not text_style_table)
    (progn (alert "Error: No TEXT_STYLE entries in settings file") (exit))
  )
  (if (not layer_table)
    (progn (alert "Error: No LAYER entries in settings file") (exit))
  )
  (list readability_bias text_style_table layer_table)
)

(defun haws_label_valid_entity_type (ent_type)
  (or (= ent_type "LINE") (= ent_type "ARC") 
      (= ent_type "LWPOLYLINE") (= ent_type "POLYLINE"))
)

(defun haws_label_find_label (layer_name layer_table / entry result)
  (foreach entry layer_table
    (if (and (not result) (wcmatch (strcase layer_name) (car entry)))
      (setq result (caddr entry))
    )
  )
  result
)

(defun haws_label_find_style_key (layer_name layer_table / entry result)
  (foreach entry layer_table
    (if (and (not result) (wcmatch (strcase layer_name) (car entry)))
      (setq result (cadr entry))
    )
  )
  result
)

(defun haws_label_apply_style (text_style_key text_style_table / entry style_info)
  (setq style_info (assoc text_style_key text_style_table))
  (if style_info
    (progn
      (haws-mklayr (list (cadr style_info) "" ""))
      (setq style_info (caddr style_info))
      (if (tblsearch "STYLE" style_info)
        (setvar "TEXTSTYLE" style_info)
        (alert (strcat "Warning: Text style '" style_info "' not found in drawing"))
      )
      style_info
    )
    nil
  )
)

(defun haws_label_calc_angle (ent_type ent_data ent_name pick_point / text_angle)
  (setq text_angle 0.0)
  (cond
    ((= ent_type "LINE")
     (setq text_angle (angle (cdr (assoc 10 ent_data)) (cdr (assoc 11 ent_data))))
    )
    ((= ent_type "ARC")
     (setq text_angle (+ (angle (cdr (assoc 10 ent_data)) pick_point) (/ pi 2)))
    )
    ((or (= ent_type "LWPOLYLINE") (= ent_type "POLYLINE"))
     (setq text_angle (haws_label_calc_pline_angle ent_type ent_data ent_name pick_point))
    )
  )
  text_angle
)

(defun haws_label_calc_pline_angle (ent_type ent_data ent_name pick_point 
                                     / ang1 bulge cenpt closest_index d dist1 dist2
                                       i min_dist pair pt1 pt2 r vertex_list)
  (setq vertex_list (haws_label_get_vertices ent_type ent_data ent_name))
  (setq closest_index (haws_label_find_closest_seg vertex_list pick_point))
  (setq pt1 (car (nth closest_index vertex_list))
        pt2 (car (nth (1+ closest_index) vertex_list))
        bulge (cadr (nth closest_index vertex_list)))
  (if (and bulge (/= bulge 0))
    (progn
      (setq d (/ (distance pt1 pt2) 2)
            ang1 (atan (/ 1 bulge))
            r (/ d (sin (- pi (* 2 ang1))))
            cenpt (polar pt1 (+ (angle pt1 pt2) (- (* 2 ang1) (/ pi 2))) r))
      (+ (angle cenpt pick_point) (/ pi 2))
    )
    (angle pt1 pt2)
  )
)

(defun haws_label_get_vertices (ent_type ent_data ent_name 
                                 / bulge i pair pt1 vertex_list)
  (setq vertex_list '())
  (if (= ent_type "LWPOLYLINE")
    (progn
      (foreach pair ent_data
        (if (= (car pair) 10)
          (progn
            (setq bulge 0
                  pt1 (cdr (member pair ent_data)))
            (while (and pt1 (not (or (= (caar pt1) 42) (= (caar pt1) 10))))
              (setq pt1 (cdr pt1))
            )
            (if (and pt1 (= (caar pt1) 42))
              (setq bulge (cdar pt1))
            )
            (setq vertex_list (append vertex_list (list (list (cdr pair) bulge))))
          )
        )
      )
    )
    (progn
      (setq i ent_name)
      (while (setq i (entnext i))
        (setq pt1 (entget i))
        (if (= "VERTEX" (cdr (assoc 0 pt1)))
          (setq vertex_list
                (append vertex_list
                        (list (list (cdr (assoc 10 pt1))
                                   (if (assoc 42 pt1) (cdr (assoc 42 pt1)) 0)))))
        )
      )
    )
  )
  vertex_list
)

(defun haws_label_find_closest_seg (vertex_list pick_point 
                                     / closest_index dist1 dist2 i min_dist pt1 pt2)
  (setq min_dist 1e99
        closest_index 0
        i 0)
  (while (< i (1- (length vertex_list)))
    (setq pt1 (car (nth i vertex_list))
          pt2 (car (nth (1+ i) vertex_list))
          dist1 (distance pick_point pt1)
          dist2 (distance pick_point pt2))
    (if (< (setq dist1 (/ (+ dist1 dist2) 2)) min_dist)
      (setq min_dist dist1
            closest_index i)
    )
    (setq i (1+ i))
  )
  closest_index
)

(princ
  (strcat
    "\nHAWS-LABEL.LSP loaded. Type haws-label (or LABEL or LAB) to start."
    "\nTo customize labels: Edit haws-label-settings.lsp"
  )
)
(princ)
