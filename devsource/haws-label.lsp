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
;;(vl-acad-defun 'HAWS-MKLAYR)

(defun haws-clock-start (label) nil)
(defun haws-clock-end (label start-time) nil)
(defun haws-clock-report (sorted) nil)
(defun haws-clock-reset () nil)
(defun haws-clock-console-log (message) nil)

;;; UCS-INDEPENDENT GEOMETRY HELPERS
;;; ANGLE, POLAR and OSNAP work in the current UCS. NENTSEL and ENTGET data are WCS.
;;; Geometry here is computed in WCS; UCS is used only for the readability flip,
;;; manual angle picks and OSNAP.
;; Angle (0 to 2pi) of vector p1->p2 in WCS, ignoring the current UCS.
(defun haws-label-ang (p1 p2 / a)
  (setq a (atan (- (cadr p2) (cadr p1)) (- (car p2) (car p1))))
  (if (< a 0) (+ a (* 2 pi)) a)
)
;; 2D point at angle/distance from pt in WCS, ignoring the current UCS.
(defun haws-label-polar (pt a d)
  (list (+ (car pt) (* d (cos a))) (+ (cadr pt) (* d (sin a))))
)
;; Convert a WCS angle to the equivalent angle in the current UCS (0 to 2pi).
(defun haws-label-wcs-ang-to-ucs (a)
  (haws-label-ang '(0.0 0.0 0.0) (trans (list (cos a) (sin a) 0.0) 0 1 T))
)
;;; HELPER FUNCTIONS FOR NUMBER EXTRACTION AND SUBSTITUTION

;; haws-label-extract-number-after-tilde
;; Extracts the number (all digits) after the LAST tilde (|) in layer name
;; If no tilde found, returns empty string ""
;; Input: layer-name (e.g., "amr6-x-water-offsite|12" or "PROP-LPS-2")
;; Output: number string (e.g., "12") or "" if not found
;; Extracts digits representing pipe size from a layer name.
;; For xref layers (containing "|"): grabs digits after the separator.
;; For native layers: looks for trailing -<digits>IN pattern (e.g. WTR-36IN -> "36").
(defun haws-label-extract-number-after-tilde (layer-name / tilde-pos after-tilde number-str
                                                         search-str i last-dash found)
  (setq tilde-pos (vl-string-search "|" layer-name))
  (if tilde-pos
    ;; xref layer: collect digits immediately after "|"
    (progn
      (setq after-tilde (substr layer-name (+ tilde-pos 2)))
      (setq number-str "")
      (foreach char (vl-string->list after-tilde)
        (if (and (>= char 48) (<= char 57))
          (setq number-str (strcat number-str (chr char)))
        )
      )
      number-str
    )
    ;; native layer: scan left-to-right for the first -<digits>IN segment
    ;; anywhere in the name (handles trailing material suffixes like -DIP, -CIP, -PE)
    (progn
      (setq search-str (strcase layer-name))
      (setq i 1  found nil  number-str "")
      (while (and (<= i (strlen search-str)) (not found))
        (if (= (substr search-str i 1) "-")
          (progn
            ;; isolate the segment between this hyphen and the next (or end)
            (setq after-tilde "")
            (setq last-dash (1+ i))
            (while (and (<= last-dash (strlen search-str))
                        (not (= (substr search-str last-dash 1) "-")))
              (setq after-tilde (strcat after-tilde (substr search-str last-dash 1)))
              (setq last-dash (1+ last-dash))
            )
            ;; segment must end in "IN" with at least one digit before it
            ;; number may be integer or decimal (e.g. "2IN", "2.5IN", "12IN")
            (if (and (>= (strlen after-tilde) 3)
                     (= (substr after-tilde (- (strlen after-tilde) 1)) "IN"))
              (progn
                (setq number-str (substr after-tilde 1 (- (strlen after-tilde) 2)))
                (setq found (> (strlen number-str) 0))
                (setq last-dash 1)
                (while (<= last-dash (strlen number-str))
                  (if (not (wcmatch (substr number-str last-dash 1) "#,."))
                    (setq found nil)
                  )
                  (setq last-dash (1+ last-dash))
                )
                ;; must start and end with a digit, not a bare dot
                (if (and found
                         (or (not (wcmatch (substr number-str 1 1) "#"))
                             (not (wcmatch (substr number-str (strlen number-str) 1) "#"))))
                  (setq found nil)
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
      (if found number-str "")
    )
  )
)

;; haws-label-substitute-number
;; Replaces "#" in label text with the extracted number
;; If no number is found (empty string) and text has #", removes both # and "
;; Input: label-text (e.g., "#\"S LPS" or "#\"g"), number (e.g., "12" or "")
;; Output: substituted text (e.g., "12\"S LPS" or "g")
(defun haws-label-substitute-number (label-text number-str / hash-pos)
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

;; haws-label-extract-material
;; Returns the material code that follows the -###IN size token in a layer name.
;; E.g. "EX-GAS-2IN-PE"       -> "PE"
;;      "EX-WTR-12IN-DIP"     -> "DIP"
;;      "EX-WTR-16IN"         -> ""   (no material suffix)
;; For xref layers (containing "|"), parses the portion AFTER "|" for the
;; material suffix — e.g. "sr-x-water|wtr-8in-dip" -> "DIP".
(defun haws-label-extract-material (layer-name / search-str i slen seg-start seg after-tilde
                                               number-str last-dash found mat-start tilde-pos)
  ;; For xref layers: work on the portion after "|" rather than bailing out.
  (setq tilde-pos (vl-string-search "|" layer-name))
  (if tilde-pos
    (setq search-str (strcase (substr layer-name (+ tilde-pos 2))))
    (setq search-str (strcase layer-name))
  )
  (setq slen (strlen search-str)  i 1  found nil  mat-start 0)
  (while (and (<= i slen) (not found))
    (if (= (substr search-str i 1) "-")
      (progn
        ;; isolate the segment between this hyphen and the next
        (setq seg "")
        (setq seg-start (1+ i))
        (setq last-dash seg-start)
        (while (and (<= last-dash slen)
                    (not (= (substr search-str last-dash 1) "-")))
          (setq seg (strcat seg (substr search-str last-dash 1)))
          (setq last-dash (1+ last-dash))
        )
        ;; does this segment look like ###IN ?
        (if (and (>= (strlen seg) 3)
                 (= (substr seg (- (strlen seg) 1)) "IN"))
          (progn
            (setq number-str (substr seg 1 (- (strlen seg) 2)))
            (setq after-tilde T)
            (setq seg-start 1)
            (while (<= seg-start (strlen number-str))
              (if (not (wcmatch (substr number-str seg-start 1) "#,."))
                (setq after-tilde nil)
              )
              (setq seg-start (1+ seg-start))
            )
            (if (and after-tilde
                     (> (strlen number-str) 0)
                     (wcmatch (substr number-str 1 1) "#")
                     (wcmatch (substr number-str (strlen number-str) 1) "#"))
              (progn
                (setq found T)
                (setq mat-start last-dash)
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  ;; If a size token was found and something follows it, extract the material
  (if (and found (< mat-start slen))
    (progn
      (setq seg "")
      (setq i (1+ mat-start))   ; skip the hyphen
      (while (and (<= i slen)
                  (not (= (substr search-str i 1) "-")))
        (setq seg (strcat seg (substr search-str i 1)))
        (setq i (1+ i))
      )
      seg
    )
    ""
  )
)

;; haws-label-label-case
;; Infers the desired case from the label template by looking at the first
;; alphabetic character outside of the %m% token.
;; Returns 'lower or 'upper (defaults to 'upper if no alpha found).
(defun haws-label-label-case (label-text / i ch result slen)
  (setq i 1  result nil  slen (strlen label-text))
  (while (and (<= i slen) (not result))
    (setq ch (substr label-text i 1))
    ;; skip the literal characters of the %m% token
    (if (= (substr label-text i 3) "%m%")
      (setq i (+ i 3))
      (progn
        (if (wcmatch ch "[a-zA-Z]")
          (setq result (if (= ch (strcase ch T)) 'lower 'upper))
        )
        (setq i (1+ i))
      )
    )
  )
  (if result result 'upper)
)

;; haws-label-substitute-material
;; Replaces the token %m% in label-text with " <material>\"" (space + material + inch-mark).
;; Case of material is matched to the surrounding label text.
;; If material is empty (or token not present), drops %m% entirely (no trailing space).
;; Token in settings file: %m%  (no quote escaping needed)
;; Input:  label-text  e.g. "#\"g%m%"   material-str e.g. "PE"
;; Output: e.g. "2\"g pe"  or  "2\"g"  (when no material)
(defun haws-label-substitute-material (label-text material-str / token cased-mat)
  (setq token "%m%")
  (if (and label-text (vl-string-search token label-text))
    (if (and material-str (> (strlen material-str) 0))
      (progn
        ;; match case to the label
        (if (= (haws-label-label-case label-text) 'lower)
          (setq cased-mat (strcase material-str T))   ; lowercase
          (setq cased-mat (strcase material-str))     ; uppercase
        )
        ;; replace %m% with <space><material>
        (vl-string-subst (strcat " " cased-mat) token label-text)
      )
      ;; no material: drop the token entirely
      (vl-string-subst "" token label-text)
    )
    label-text
  )
)

(defun haws-clock-start (label) nil)

;; Global *error* for haws-label - defined at load time so haws-core-init cannot overwrite it.
;; *haws-label-olayer* is set at the top of c:haws-label before haws-core-init runs.
(defun *error* (msg)
  (haws-vrstor)
  (haws-core-restore)
  ;; Restore AFTER vrstor/core-restore so framework calls cannot clobber it
  (if *haws-label-olayer* (setvar "CLAYER" *haws-label-olayer*))
  (if (and msg (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*")))
    (princ (strcat "\nError: " msg))
  )
  (princ)
)

(defun c:haws-label (/ angle-mode ent-data ent-name ent-pick ent-type label-text layer-name
                        extracted-number extracted-material
                        layer-table pick-point pt1 pt2 readability-bias settings snapped text-angle
                        text-height text-style-key text-style-name text-style-table user-choice)
  ;; Save layer to global BEFORE haws-core-init (which overwrites *error*)
  (setq *haws-label-olayer* (getvar "CLAYER"))
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
            ;; nentsel returns the pick point in the current UCS; entget data is WCS
            (setq ent-name (car ent-pick)
                  pick-point (trans (cadr ent-pick) 1 0)
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
      (progn (princ "\nNo entity selected.") (haws-vrstor) (haws-core-restore) (if *haws-label-olayer* (setvar "CLAYER" *haws-label-olayer*)) (exit))
    )
  
  (setq layer-name (cdr (assoc 8 ent-data)))
  (haws-debug (list "haws-label pick: type=" ent-type " layer=" layer-name " pick=" (vl-princ-to-string pick-point)))
  
  (setq label-text (haws-label-find-label layer-name layer-table))
  (if (not label-text)
    (progn
      (alert (strcat "No label defined for layer: " layer-name "\n\nCheck haws-label-settings.lsp"))
      (haws-vrstor)
      (if *haws-label-olayer* (setvar "CLAYER" *haws-label-olayer*))
      (exit)
    )
  )
  
  ;; Extract number after tilde (if any) and substitute "#" in label text
  (setq extracted-number (haws-label-extract-number-after-tilde layer-name))
  ;; Always substitute, even if number is empty (will remove #" if no number found)
  (setq label-text (haws-label-substitute-number label-text extracted-number))

  ;; Extract material suffix (e.g. "PE", "DIP", "CIP") and substitute m\" in label text
  (setq extracted-material (haws-label-extract-material layer-name))
  ;; Always substitute, even if material is empty (will remove m\" if no material found)
  (setq label-text (haws-label-substitute-material label-text extracted-material))
  
  (setq text-style-key (haws-label-find-style-key layer-name layer-table))
  
  (setq text-style-name (haws-label-apply-style text-style-key text-style-table))
  
  ;; Calculate angle based on stored mode preference
  (if (= *haws-label-angle-mode* "MANUAL")
    (progn
      (setq pt1 (getpoint "\nFirst point for angle: ")
            pt2 (getpoint pt1 "\nSecond point for angle: "))
      (if (and pt1 pt2)
        ;; getpoint returns UCS coords; convert to WCS before taking the angle
        (setq text-angle (haws-label-ang (trans pt1 1 0) (trans pt2 1 0)))
        (setq text-angle (haws-label-calc-angle ent-type ent-data ent-name pick-point))
      )
    )
    (progn
      (setq text-angle (haws-label-calc-angle ent-type ent-data ent-name pick-point))
      ;; Apply readability bias - flip text if upside-down
      ;; READABILITY-BIAS is the angle threshold (default 110 degrees)
      ;; Text between READABILITY-BIAS and (READABILITY-BIAS + 180) gets flipped
      ;; Test readability against the CURRENT UCS (so a View UCS keeps text
      ;; right-side-up on screen), but keep text-angle itself in WCS because
      ;; that is what the MTEXT entity needs.
      (if (< readability-bias (haws-label-wcs-ang-to-ucs text-angle) (+ readability-bias pi))
        (setq text-angle (+ text-angle pi))
      )
    )
  )
  ;; Normalize to 0-2π range
  (while (< text-angle 0) (setq text-angle (+ text-angle (* 2 pi))))
  (while (>= text-angle (* 2 pi)) (setq text-angle (- text-angle (* 2 pi))))
  
  ;; Snap pick point to nearest point on entity
  ;; OSNAP works in UCS; nentsel's point is WCS, and entmake wants WCS.
  (setq snapped (osnap (trans pick-point 0 1) "near"))
  (haws-debug (list "haws-label osnap near: " (vl-princ-to-string snapped)))
  (if snapped (setq pick-point (trans snapped 1 0)))
  
  (setq text-height (haws-text-height-model))
  
  (haws-debug (list "haws-label text-angle (WCS deg): " (rtos (* text-angle (/ 180.0 pi)) 2 2) " ucs deg: " (rtos (* (haws-label-wcs-ang-to-ucs text-angle) (/ 180.0 pi)) 2 2)))
  ;; Create MTEXT with background mask
  (entmake (list
    '(0 . "MTEXT")
    '(100 . "AcDbEntity")
    '(100 . "AcDbMText")
    (cons 10 pick-point)              ; Insertion point
    (cons 40 text-height)             ; Text height
    (cons 71 5)                       ; Attachment point: 5 = Middle Center
    (cons 11 (list (cos text-angle) (sin text-angle) 0.0)) ; X-axis direction vector (WCS)
    (cons 1 label-text)               ; Text content
    (cons 7 text-style-name)          ; Text style
    '(90 . 3)                         ; Background mask flag: 3 = use background fill
    '(63 . 256)                       ; Background fill color: 256 = drawing background
    '(45 . 1.1)                       ; Fill box scale (border offset factor)
    '(441 . 0)                        ; Background fill setting
  ))
  ;; Restore original layer after each label placement before looping back
  (if *haws-label-olayer* (setvar "CLAYER" *haws-label-olayer*))
  ) ;; end while T
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
    (setq temp (vl-catch-all-apply 'read (list rdlin)))
    (if (and (not (vl-catch-all-error-p temp))
             (= 'LIST (type temp)))
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
    (progn
      ;; Style key not found in TEXT-STYLE table: warn and fall back to the
      ;; current text style instead of returning nil (nil -> bad DXF group 7)
      (princ (strcat "\n** Text style key '" (if text-style-key text-style-key "nil")
                     "' is not defined in TEXT-STYLE entries of haws-label-settings.lsp. Using current text style."))
      (getvar "TEXTSTYLE")
    )
  )
)

(defun haws-label-calc-angle (ent-type ent-data ent-name pick-point / text-angle)
  (setq text-angle 0.0)
  (cond
    ((= ent-type "LINE")
     (setq text-angle (haws-label-ang (cdr (assoc 10 ent-data)) (cdr (assoc 11 ent-data))))
    )
    ((= ent-type "ARC")
     (setq text-angle (+ (haws-label-ang (cdr (assoc 10 ent-data)) pick-point) (/ pi 2)))
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
  (haws-debug (list "haws-label vertices: " (vl-princ-to-string vertex-list)))
  (setq closest-index (haws-label-find-closest-seg vertex-list pick-point))
  (setq pt1 (car (nth closest-index vertex-list))
        pt2 (car (nth (1+ closest-index) vertex-list))
        bulge (cadr (nth closest-index vertex-list)))
  (if (and bulge (/= bulge 0))
    (progn
      (setq d (/ (distance pt1 pt2) 2)
            ang1 (atan (/ 1 bulge))
            r (/ d (sin (- pi (* 2 ang1))))
            cenpt (haws-label-polar pt1 (+ (haws-label-ang pt1 pt2) (- (* 2 ang1) (/ pi 2))) r))
      (+ (haws-label-ang cenpt pick-point) (/ pi 2))
    )
    (haws-label-ang pt1 pt2)
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
