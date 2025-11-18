;;;==============================================================================
;;; CNM AUTO-TEXT TYPE CLASSIFICATION REFERENCE
;;;==============================================================================
;;; Copyright © 2025 Thomas Gail Haws
;;; 
;;; This table classifies all auto-text types by two orthogonal properties:
;;; 1. Handle-based: Does it store/need a reference object handle?
;;; 2. Coordinate-based: Does it need world coordinate transformation (VPTRANS XRECORD)?
;;;
;;;==============================================================================

;;; CLASSIFICATION TABLE
;;; 
;;; Type      | Handle-Based | Coordinate-Based | Reference Object | Notes
;;; ----------|--------------|------------------|------------------|------------------
;;; N         | No           | Yes              | None             | Pure northing coordinates
;;; E         | No           | Yes              | None             | Pure easting coordinates  
;;; NE        | No           | Yes              | None             | Combined northing/easting
;;; STa       | Yes          | Yes              | Alignment        | Station from alignment at coordinates
;;; Off       | Yes          | Yes              | Alignment        | Offset from alignment at coordinates
;;; stAoff    | Yes          | Yes              | Alignment        | Station+Offset from alignment at coordinates
;;; STAName   | Yes          | Yes              | Alignment        | Station+Name from alignment at coordinates
;;; NAme      | Yes          | No               | Alignment        | Alignment name only (no coordinates needed)
;;; Z         | Yes          | Yes              | Surface          | Elevation from surface at coordinates (unimplemented)
;;; Dia       | Yes          | No               | Pipe             | Pipe diameter property
;;; SLope     | Yes          | No               | Pipe             | Pipe slope property  
;;; L         | Yes          | No               | Pipe             | Pipe length property
;;; LF        | No           | No               | None             | User-selected length (no stored reference)
;;; SF        | No           | No               | None             | User-selected area (no stored reference)
;;; SY        | No           | No               | None             | User-selected area (no stored reference)
;;; Text      | No           | No               | None             | Static user text
;;; ENtry     | No           | No               | None             | Static entry number
;;;
;;; XDATA STORAGE PATTERNS:
;;;
;;; Handle-based + Coordinate-based (STa/Off/stAoff/STAName/Z):
;;;   - XDATA: (("StaOff" . "ABC123") . "STA 10+25.00 OFF 5.0' RT")
;;;   - VPTRANS: Required for paper space bubbles
;;;   - Reactor owners: Both leader AND alignment (dual ownership)
;;;
;;; Handle-based + Non-coordinate (NAme/Dia/SLope/L):  
;;;   - XDATA: (("Dia" . "DEF456") . "Ø24\"")
;;;   - VPTRANS: Not needed
;;;   - Reactor owners: Reference object only (single ownership)
;;;
;;; Non-handle + Coordinate-based (N/E/NE):
;;;   - XDATA: (("N" . "") . "N 123456.78") 
;;;   - VPTRANS: Required for paper space bubbles
;;;   - Reactor owners: Leader only (single ownership)
;;;
;;; Non-handle + Non-coordinate (LF/SF/SY/Text/ENtry):
;;;   - XDATA: None (static text, no reactors)
;;;   - VPTRANS: Not needed
;;;   - Reactor owners: None
;;;
;;;==============================================================================

(defun hcnm-auto-text-classification-table ()
  "Returns classification table for all auto-text types.
   Format: ((type handle-based-p coordinate-based-p reference-type) ...)
   
   handle-based-p: T if stores reference object handle, nil otherwise
   coordinate-based-p: T if needs VPTRANS for paper space, nil otherwise  
   reference-type: 'AL 'PIPE 'SU nil"
  '(("N"       nil t   nil)    ; Handleless + Coordinate-based
    ("E"       nil t   nil)    ; Handleless + Coordinate-based
    ("NE"      nil t   nil)    ; Handleless + Coordinate-based
    ("STa"     t   t   AL)     ; Handle-based + Coordinate-based
    ("Off"     t   t   AL)     ; Handle-based + Coordinate-based  
    ("stAoff"  t   t   AL)     ; Handle-based + Coordinate-based
    ("STAName" t   t   AL)     ; Handle-based + Coordinate-based
    ("NAme"    t   nil AL)     ; Handle-based + Non-coordinate
    ("Z"       t   t   SU)     ; Handle-based + Coordinate-based (unimplemented)
    ("Dia"     t   nil PIPE)   ; Handle-based + Non-coordinate
    ("SLope"   t   nil PIPE)   ; Handle-based + Non-coordinate
    ("L"       t   nil PIPE)   ; Handle-based + Non-coordinate
    ("LF"      nil nil nil)    ; Handleless + Non-coordinate (static)
    ("SF"      nil nil nil)    ; Handleless + Non-coordinate (static)
    ("SY"      nil nil nil)    ; Handleless + Non-coordinate (static)
    ("Text"    nil nil nil)    ; Handleless + Non-coordinate (static)
    ("ENtry"   nil nil nil)    ; Handleless + Non-coordinate (static)
   )
)

(defun hcnm-auto-text-handle-based-p (auto-type)
  "Returns T if auto-type requires reference object handle storage."
  (nth 1 (assoc auto-type (hcnm-auto-text-classification-table)))
)

(defun hcnm-auto-text-coordinate-based-p (auto-type)
  "Returns T if auto-type requires coordinate transformation (VPTRANS) for paper space."
  (nth 2 (assoc auto-type (hcnm-auto-text-classification-table)))
)

(defun hcnm-auto-text-reference-type (auto-type)
  "Returns reference object type ('AL 'PIPE 'SU) or nil."
  (nth 3 (assoc auto-type (hcnm-auto-text-classification-table)))
)

(princ "\nAuto-text classification functions loaded:")
(princ "\n  (hcnm-auto-text-handle-based-p \"StaOff\")     ; -> T")  
(princ "\n  (hcnm-auto-text-coordinate-based-p \"StaOff\") ; -> T")
(princ "\n  (hcnm-auto-text-reference-type \"StaOff\")     ; -> AL")
(princ "\n  (hcnm-auto-text-handle-based-p \"N\")          ; -> nil")
(princ "\n  (hcnm-auto-text-coordinate-based-p \"N\")      ; -> T")
(princ)