;;; HAWS-LABEL.lsp - C:HAWS_LABEL command for CNM/HAWSEDC
;;; See haws-code-style-guidelines.md for conventions
;;;
;;; Command: HAWS_LABEL (aliases: LABEL, LAB)
;;; Tracker ID: 339
;;;
;;; Purpose:
;;;   Labels lines, arcs, and polylines with text aligned to the entity at the
;;;   pick point. Text orientation is perpendicular to the radial for curved
;;;   segments. Layer-specific text styles and labels from haws-label-settings.lsp.
;;;
;;; Usage:
;;;   1. Type HAWS_LABEL (or LABEL or LAB)
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

(defun C:HAWS_LABEL (/ ENT_DATA ENT_NAME ENT_PICK ENT_TYPE LABEL_TEXT LAYER_NAME
                        LAYER_TABLE PICK_POINT READABILITY_BIAS SETTINGS TEXT_ANGLE
                        TEXT_HEIGHT TEXT_STYLE_KEY TEXT_STYLE_NAME TEXT_STYLE_TABLE)
  (haws-core-init 339)
  (HAWS-VSAVE '("CLAYER"))
  
  (SETQ SETTINGS (HAWS_LABEL_READ_SETTINGS))
  (SETQ READABILITY_BIAS (CAR SETTINGS)
        TEXT_STYLE_TABLE (CADR SETTINGS)
        LAYER_TABLE (CADDR SETTINGS))
  
  (PROMPT "\nSelect line, arc, or polyline: ")
  (SETQ ENT_PICK (NENTSEL))
  (IF (NOT ENT_PICK)
    (PROGN (PRINC "\nNo entity selected.") (HAWS-VRSTOR) (EXIT))
  )
  
  (SETQ ENT_NAME (CAR ENT_PICK)
        PICK_POINT (CADR ENT_PICK)
        ENT_DATA (ENTGET ENT_NAME)
        ENT_TYPE (CDR (ASSOC 0 ENT_DATA))
        LAYER_NAME (CDR (ASSOC 8 ENT_DATA)))
  
  (IF (NOT (HAWS_LABEL_VALID_ENTITY_TYPE ENT_TYPE))
    (PROGN
      (ALERT (STRCAT "Unsupported entity type: " ENT_TYPE "\nPlease select a LINE, ARC, or POLYLINE."))
      (HAWS-VRSTOR)
      (EXIT)
    )
  )
  
  (SETQ LABEL_TEXT (HAWS_LABEL_FIND_LABEL LAYER_NAME LAYER_TABLE))
  (IF (NOT LABEL_TEXT)
    (PROGN
      (ALERT (STRCAT "No label defined for layer: " LAYER_NAME))
      (HAWS-VRSTOR)
      (EXIT)
    )
  )
  (SETQ TEXT_STYLE_KEY (HAWS_LABEL_FIND_STYLE_KEY LAYER_NAME LAYER_TABLE))
  
  (SETQ TEXT_STYLE_NAME (HAWS_LABEL_APPLY_STYLE TEXT_STYLE_KEY TEXT_STYLE_TABLE))
  
  (SETQ TEXT_ANGLE (HAWS_LABEL_CALC_ANGLE ENT_TYPE ENT_DATA ENT_NAME PICK_POINT))
  
  ;; Apply readability bias - flip text if upside-down
  ;; READABILITY_BIAS is the angle threshold (default 110 degrees)
  ;; Text between READABILITY_BIAS and (READABILITY_BIAS + 180) gets flipped
  (SETQ READABILITY_BIAS (/ (* READABILITY_BIAS PI) 180.0)) ; Convert to radians
  (IF (< READABILITY_BIAS TEXT_ANGLE (+ READABILITY_BIAS PI))
    (SETQ TEXT_ANGLE (+ TEXT_ANGLE PI))
  )
  ;; Normalize to 0-2π range
  (WHILE (< TEXT_ANGLE 0) (SETQ TEXT_ANGLE (+ TEXT_ANGLE (* 2 PI))))
  (WHILE (>= TEXT_ANGLE (* 2 PI)) (SETQ TEXT_ANGLE (- TEXT_ANGLE (* 2 PI))))
  
  ;; Snap pick point to nearest point on entity
  (SETQ PICK_POINT (OSNAP PICK_POINT "near"))
  
  (SETQ TEXT_HEIGHT (haws-text-height-model))
  (HAWS-MKTEXT "MC" PICK_POINT TEXT_HEIGHT TEXT_ANGLE LABEL_TEXT)
  
  (HAWS-VRSTOR)
  (haws-core-restore)
  (PRINC)
)

(defun HAWS_LABEL_READ_SETTINGS (/ F1 I KEY LAYER_TABLE RDLIN READABILITY_BIAS
                                    SETTINGS_DATA SETTINGS_FILE TEMP TEXT_STYLE_TABLE)
  (SETQ SETTINGS_FILE (FINDFILE "haws-label-settings.lsp"))
  (IF (NOT SETTINGS_FILE)
    (PROGN (ALERT "Could not find haws-label-settings.lsp") (EXIT))
  )
  (SETQ F1 (OPEN SETTINGS_FILE "r"))
  (IF (NOT F1)
    (PROGN (ALERT "Could not open haws-label-settings.lsp") (EXIT))
  )
  (SETQ READABILITY_BIAS 110.0
        TEXT_STYLE_TABLE '()
        LAYER_TABLE '()
        SETTINGS_DATA '()
        I 0)
  (PRINC "\n")
  (WHILE (SETQ RDLIN (READ-LINE F1))
    (PRINC "\rReading line ")
    (PRINC (SETQ I (1+ I)))
    (IF (= 'LIST (TYPE (SETQ TEMP (READ RDLIN))))
      (SETQ SETTINGS_DATA (CONS TEMP SETTINGS_DATA))
    )
  )
  (CLOSE F1)
  (SETQ SETTINGS_DATA (REVERSE SETTINGS_DATA))
  (FOREACH RDLIN SETTINGS_DATA
    (SETQ KEY (CAR RDLIN))
    (COND
      ((= KEY "READ-BIAS-DEGREES") (SETQ READABILITY_BIAS (CADR RDLIN)))
      ((= KEY "TEXT_STYLE") (SETQ TEXT_STYLE_TABLE (CONS (CDR RDLIN) TEXT_STYLE_TABLE)))
      ((= KEY "LAYER") (SETQ LAYER_TABLE (CONS (CDR RDLIN) LAYER_TABLE)))
    )
  )
  (IF (NOT TEXT_STYLE_TABLE)
    (PROGN (ALERT "Error: No TEXT_STYLE entries in settings file") (EXIT))
  )
  (IF (NOT LAYER_TABLE)
    (PROGN (ALERT "Error: No LAYER entries in settings file") (EXIT))
  )
  (LIST READABILITY_BIAS TEXT_STYLE_TABLE LAYER_TABLE)
)

(defun HAWS_LABEL_VALID_ENTITY_TYPE (ENT_TYPE)
  (OR (= ENT_TYPE "LINE") (= ENT_TYPE "ARC") 
      (= ENT_TYPE "LWPOLYLINE") (= ENT_TYPE "POLYLINE"))
)

(defun HAWS_LABEL_FIND_LABEL (LAYER_NAME LAYER_TABLE / ENTRY RESULT)
  (FOREACH ENTRY LAYER_TABLE
    (IF (AND (NOT RESULT) (WCMATCH (STRCASE LAYER_NAME) (CAR ENTRY)))
      (SETQ RESULT (CADDR ENTRY))
    )
  )
  RESULT
)

(defun HAWS_LABEL_FIND_STYLE_KEY (LAYER_NAME LAYER_TABLE / ENTRY RESULT)
  (FOREACH ENTRY LAYER_TABLE
    (IF (AND (NOT RESULT) (WCMATCH (STRCASE LAYER_NAME) (CAR ENTRY)))
      (SETQ RESULT (CADR ENTRY))
    )
  )
  RESULT
)

(defun HAWS_LABEL_APPLY_STYLE (TEXT_STYLE_KEY TEXT_STYLE_TABLE / ENTRY STYLE_INFO)
  (SETQ STYLE_INFO (ASSOC TEXT_STYLE_KEY TEXT_STYLE_TABLE))
  (IF STYLE_INFO
    (PROGN
      (HAWS-MKLAYR (LIST (CADR STYLE_INFO) "" ""))
      (SETQ STYLE_INFO (CADDR STYLE_INFO))
      (IF (TBLSEARCH "STYLE" STYLE_INFO)
        (SETVAR "TEXTSTYLE" STYLE_INFO)
        (ALERT (STRCAT "Warning: Text style '" STYLE_INFO "' not found in drawing"))
      )
      STYLE_INFO
    )
    NIL
  )
)

(defun HAWS_LABEL_CALC_ANGLE (ENT_TYPE ENT_DATA ENT_NAME PICK_POINT / TEXT_ANGLE)
  (SETQ TEXT_ANGLE 0.0)
  (COND
    ((= ENT_TYPE "LINE")
     (SETQ TEXT_ANGLE (ANGLE (CDR (ASSOC 10 ENT_DATA)) (CDR (ASSOC 11 ENT_DATA))))
    )
    ((= ENT_TYPE "ARC")
     (SETQ TEXT_ANGLE (+ (ANGLE (CDR (ASSOC 10 ENT_DATA)) PICK_POINT) (/ PI 2)))
    )
    ((OR (= ENT_TYPE "LWPOLYLINE") (= ENT_TYPE "POLYLINE"))
     (SETQ TEXT_ANGLE (HAWS_LABEL_CALC_PLINE_ANGLE ENT_TYPE ENT_DATA ENT_NAME PICK_POINT))
    )
  )
  TEXT_ANGLE
)

(defun HAWS_LABEL_CALC_PLINE_ANGLE (ENT_TYPE ENT_DATA ENT_NAME PICK_POINT 
                                     / ANG1 BULGE CENPT CLOSEST_INDEX D DIST1 DIST2
                                       I MIN_DIST PAIR PT1 PT2 R VERTEX_LIST)
  (SETQ VERTEX_LIST (HAWS_LABEL_GET_VERTICES ENT_TYPE ENT_DATA ENT_NAME))
  (SETQ CLOSEST_INDEX (HAWS_LABEL_FIND_CLOSEST_SEG VERTEX_LIST PICK_POINT))
  (SETQ PT1 (CAR (NTH CLOSEST_INDEX VERTEX_LIST))
        PT2 (CAR (NTH (1+ CLOSEST_INDEX) VERTEX_LIST))
        BULGE (CADR (NTH CLOSEST_INDEX VERTEX_LIST)))
  (IF (AND BULGE (/= BULGE 0))
    (PROGN
      (SETQ D (/ (DISTANCE PT1 PT2) 2)
            ANG1 (ATAN (/ 1 BULGE))
            R (/ D (SIN (- PI (* 2 ANG1))))
            CENPT (POLAR PT1 (+ (ANGLE PT1 PT2) (- (* 2 ANG1) (/ PI 2))) R))
      (+ (ANGLE CENPT PICK_POINT) (/ PI 2))
    )
    (ANGLE PT1 PT2)
  )
)

(defun HAWS_LABEL_GET_VERTICES (ENT_TYPE ENT_DATA ENT_NAME 
                                 / BULGE I PAIR PT1 VERTEX_LIST)
  (SETQ VERTEX_LIST '())
  (IF (= ENT_TYPE "LWPOLYLINE")
    (PROGN
      (FOREACH PAIR ENT_DATA
        (IF (= (CAR PAIR) 10)
          (PROGN
            (SETQ BULGE 0
                  PT1 (CDR (MEMBER PAIR ENT_DATA)))
            (WHILE (AND PT1 (NOT (OR (= (CAAR PT1) 42) (= (CAAR PT1) 10))))
              (SETQ PT1 (CDR PT1))
            )
            (IF (AND PT1 (= (CAAR PT1) 42))
              (SETQ BULGE (CDAR PT1))
            )
            (SETQ VERTEX_LIST (APPEND VERTEX_LIST (LIST (LIST (CDR PAIR) BULGE))))
          )
        )
      )
    )
    (PROGN
      (SETQ I ENT_NAME)
      (WHILE (SETQ I (ENTNEXT I))
        (SETQ PT1 (ENTGET I))
        (IF (= "VERTEX" (CDR (ASSOC 0 PT1)))
          (SETQ VERTEX_LIST
                (APPEND VERTEX_LIST
                        (LIST (LIST (CDR (ASSOC 10 PT1))
                                   (IF (ASSOC 42 PT1) (CDR (ASSOC 42 PT1)) 0)))))
        )
      )
    )
  )
  VERTEX_LIST
)

(defun HAWS_LABEL_FIND_CLOSEST_SEG (VERTEX_LIST PICK_POINT 
                                     / CLOSEST_INDEX DIST1 DIST2 I MIN_DIST PT1 PT2)
  (SETQ MIN_DIST 1e99
        CLOSEST_INDEX 0
        I 0)
  (WHILE (< I (1- (LENGTH VERTEX_LIST)))
    (SETQ PT1 (CAR (NTH I VERTEX_LIST))
          PT2 (CAR (NTH (1+ I) VERTEX_LIST))
          DIST1 (DISTANCE PICK_POINT PT1)
          DIST2 (DISTANCE PICK_POINT PT2))
    (IF (< (SETQ DIST1 (/ (+ DIST1 DIST2) 2)) MIN_DIST)
      (SETQ MIN_DIST DIST1
            CLOSEST_INDEX I)
    )
    (SETQ I (1+ I))
  )
  CLOSEST_INDEX
)
