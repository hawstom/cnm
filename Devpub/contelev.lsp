;;; AutoCAD Wiki AutoLISP code header.  
;;;
;;; Copy this code to a file on your computer. 
;;; Start highlighting OUTSIDE the code boxes and use the mouse or keyboard to
;;; highlight all the code.
;;; If you select too much, simply delete any extra from your destination file.
;;; In Windows you may want to start below the code and use [Shift]+[Ctrl]+[Home] 
;;; key combination to highlight all the way to the top of the article,
;;; then still holding the [Shift] key, use the arrow keys to shrink the top of
;;; the selection down to the beginning of the code.  Then copy and paste.
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The working version of this software is located at the AutoCAD Wiki.
;;; Please Be Bold in adding clarifying comments and improvements at
;;; http://autocad.wikia.com/wiki/Contour_Elevations_at_Intervals_with_Labels_(AutoLISP_application)

;;; Contour Elevations at Intervals with Labels
;;; Copyleft 2017 Thomas Gail Haws licensed under the terms of the GNU GPL
;;; http://www.hawsedc.com tom.haws@gmail.com
;;; Version: 1.0.0
;;; Official Repository: http://autocad.wikia.com/wiki/Contour_Elevations_at_Intervals_with_Labels_(AutoLISP_application
;;; Haws is a registered reserved symbol with Autodesk that will never conflict with other apps.
;;;
;;; Features:
;;; -Uses an exploded block for labeling so you can customize label as needed.
;;; -Saves to (setcfg) to remember settings between sessions.  Saves to a single global variable during a session.
;;; -Lets you ignore Interval, Label Precision, Label Spacing, Temporary Color, LabelBlockName, and Elevation or change them on the fly.
;;; -For programmers, demonstrates small functions with self-documenting names and variable names.  Also demonstrates settings management.

;; Customizable out-of-the-box defaults
(defun
   haws-cei:define-settings (/ luprec)
  (setq luprec (itoa (getvar "LUPREC")))
  (list
    (list "CurrentElevation" "" 'real)
    (list "ContourInterval" "1.0" 'real)
    (list "LabelSpacing" "500.0" 'real)
    (list "TemporaryColor" "." 'str)
    (list "LabelPrecision" luprec 'int)
    (list "LabelBlockName" "cei-contour-label" 'str)
    (list "LUPREC" luprec 'int)
    (list "ElevationPrompt" "Yes" 'str) ;"Yes" or "No"
    (list "LabelWithFields" "Yes" 'str) ;"Yes" or "No"
  )
)

(defun c:haws-contelev () (haws-cei:main))

(defun
   haws-cei:main (/ input-main)
  (haws-cei:initialize-settings)
  (haws-cei:initialize-elevation)
  (command "._undo" "_group")
  (while (setq input-main (haws-cei:get-input-main))
    (haws-cei:do-input-main input-main)
  )
  (command "._undo" "_end")
  (princ)
)


;; Start with default settings and supplement with stored settings.
(defun
   haws-cei:initialize-settings (/ luprec setting)
  (cond ((not *haws-cei:settings*) (haws-cei:get-default-settings)))
  (haws-cei:get-stored-settings)
  ;; If drawing LUPREC changed, use it.
  (cond
    ((/= (setq luprec (getvar "LUPREC"))
         (haws-cei:getvar "LUPREC")
     )
     (haws-cei:setvar "LUPREC" luprec)
     (haws-cei:setvar "LabelPrecision" luprec)
    )
  )
)


;; Define-Settings is at top of file for customization convenience.
(defun
   haws-cei:get-default-settings ()
  (setq *haws-cei:settings* (haws-cei:define-settings))
)

;; Get settings from AutoCAD's AutoLISP permananent storage system
;; The setcfg/getcfg functions might be removed in a future release.
(defun
   haws-cei:get-stored-settings (/ settings-definition valuei)
  (setq settings-definition (haws-cei:define-settings))
  (cond
    ;; If stored settings location exists
    ((getcfg (strcat (haws-cei:storage-location) "Dummy"))
     (foreach
        setting settings-definition
       (cond
         ;; If setting exists (even missing settings return "")
         ((/= ""
              (setq
                valuei
                 (getcfg
                   (strcat (haws-cei:storage-location) (car setting))
                 )
              )
          )
          (haws-cei:save-to-settings-list (car setting) valuei)
         )
       )
     )
    )
  )
)

(defun haws-cei:storage-location () "Appdata/Haws/CEI/")

(defun
   haws-cei:save-to-settings-list (var val)
  (setq
    *haws-cei:settings*
     (subst
       (list var val (haws-cei:getvar-type var))
       (assoc var *haws-cei:settings*)
       *haws-cei:settings*
     )
  )
)

(defun
   haws-cei:getvar (var / val-string)
  (setq
    val-string
     (cadr (assoc var *haws-cei:settings*))
    var-type
     (caddr (assoc var *haws-cei:settings*))
    val
     (cond
       ((= var-type 'real) (distof val-string)) ; Returns nil for ""
       ((= var-type 'int) (atoi val-string))
       ((= var-type 'str) val-string)
     )
  )
)

(defun
   haws-cei:getvar-type (var / val-string)
  (caddr (assoc var *haws-cei:settings*))
)

(defun
   haws-cei:setvar (var val / var-type)
  (setq var-type (haws-cei:getvar-type var))
  (cond
    ((/= (type val) var-type)
     (alert
       (strcat
         "Warning in haws-cei:SETVAR.\n\nVariable: "
         var
         "\nType expected: "
         (vl-prin1-to-string var-type)
         "\nType provided: "
         (vl-prin1-to-string (type val))
       )
     )
     (exit)
    )
  )
  (cond ((/= (type val) 'str) (setq val (vl-prin1-to-string val))))
  (haws-cei:save-to-settings-list var val)
  (haws-cei:save-to-storage var val)
  val
)

(defun
   haws-cei:save-to-storage (var val)
  (setcfg (strcat (haws-cei:storage-location) var) val)
)

(defun
   haws-cei:initialize-elevation ()
  (cond
    ((or (not (haws-cei:getvar "CurrentElevation"))
         (= (haws-cei:getvar "ElevationPrompt") "Yes")
     )
     (haws-cei:get-elevation)
    )
  )
)

(defun
   haws-cei:get-input-main (/ input-main current-elevation)
  (haws-cei:print-settings)
  (initget
    "Elevation Interval Spacing Color Precision Multilabel"
  )
  (entsel
    "\nSelect contour to elevate and label or [new Elevation/contour Interval/label Spacing/temporary Color/Precision/Multilabel]: "
  )
)

(defun
   haws-cei:print-settings (/ setting)
  (princ "\nCurrent settings: ")
  (foreach
     setting *haws-cei:settings*
    (cond
      ((and
         (/= (car setting) "LUPREC")
         (/= (car setting) "LabelBlockName")
       )
       (princ (car setting))
       (princ "=")
       (princ (cadr setting))
       (princ " ")
      )
    )
  )
)

(defun
   haws-cei:do-input-main (input-main /)
  (cond
    ((or (= input-main "Elevation") (= input-main ""))
     (haws-cei:get-elevation)
    )
    ((= input-main "Interval") (haws-cei:get-contour-interval))
    ((= input-main "Spacing") (haws-cei:get-label-spacing))
    ((= input-main "Color") (haws-cei:get-temporary-color))
    ((= input-main "Precision") (haws-cei:get-precision))
    ((= input-main "Multilabel") (haws-cei:do-multilabel))
    (t (haws-cei:do-next-contour input-main))
  )
)

(defun
   haws-cei:get-elevation (/ elevation-point)
  (initget "Text Prompt Multilabel")
  (setq
    elevation-point
     (getpoint
       "\nSpecify point at starting elevation or [enter Text/elevation Prompt mode/Multilabel] <Text>: "
     )
  )
  (cond
    ((= elevation-point "Prompt")
     (haws-cei:get-elevation-prompt)
    )
    ((= elevation-point "Multilabel") (haws-cei:do-multilabel))
    ((or (= elevation-point "Text") (not elevation-point))
     (haws-cei:get-elevation-text)
    )
    (t (haws-cei:get-elevation-from-point elevation-point))
  )
)

(defun
   haws-cei:get-elevation-from-point (elevation-point)
  (haws-cei:setvar "CurrentElevation" (caddr elevation-point))
)

(defun
   haws-cei:get-elevation-text ()
  (haws-cei:setvar
    "CurrentElevation"
    (getreal "\nNew current elevation: ")
  )
)

(defun
   haws-cei:get-elevation-prompt ()
  (initget "Yes No")
  (haws-cei:get-input-generic
    "ElevationPrompt"
    'getkword
    "Always prompt for elevation before selecting contours? [Yes/No]"
  )
  (haws-cei:get-elevation)
)

(defun
   haws-cei:get-contour-interval ()
  (haws-cei:get-input-generic
    "ContourInterval"
    'getreal
    "Contour interval"
  )
)

(defun
   haws-cei:get-label-spacing ()
  (haws-cei:get-input-generic
    "LabelSpacing"
    'getreal
    "Spacing for labels along contours"
  )
)

(defun
   haws-cei:get-temporary-color ()
  (haws-cei:get-input-generic
    "TemporaryColor"
    'getstring
    "Temporary color to distinguish elevated contours or . for none"
  )
)

(defun
   haws-cei:get-precision ()
  (haws-cei:get-input-generic
    "LabelPrecision"
    'getint
    "Decimal places of label precision"
  )
)
(defun
   haws-cei:get-input-generic (var function-symbol prompt1 / input1)
  (setq
    input1
     (apply
       function-symbol
       (list
         (strcat "\n" prompt1 " <" (haws-cei:getvar var) ">: ")
       )
     )
  )
  (cond ((and input1 (/= input1 "")) (haws-cei:setvar var input1)))
)

(defun
   haws-cei:do-multilabel ( / CONTOURSET EN ENAME-CONTOUR ENTSEL-CONTOUR I)
  (setq
    contourset (ssget)
    i   -1
  )
  (while (setq ename-contour (ssname contourset (setq i (1+ i))))
    (setq entsel-contour (list ename-contour (cdr (assoc 10 (entget ename-contour)))))
    (haws-cei:label-contour
      entsel-contour
    )
  )
)

(defun
   haws-cei:do-next-contour (entsel1 /)
  (haws-cei:elevate-contour entsel1)
  (haws-cei:color-contour entsel1)
  (haws-cei:label-contour entsel1)
  (haws-cei:setvar
    "CurrentElevation"
    (+ (haws-cei:getvar "CurrentElevation")
       (haws-cei:getvar "ContourInterval")
    )
  )
)


(defun
   haws-cei:elevate-contour
   (entsel1 / current-elevation pline1 pline1data)
  (setq
    pline1
     (car entsel1)
    current-elevation
     (haws-cei:getvar "CurrentElevation")
    pline1data
     (entget pline1)
  )
  (setq
    pline1data
     (cond
       ((= (cdr (assoc 0 pline1data)) "POLYLINE")
        (subst
          (reverse
            (cons
              (haws-cei:getvar "CurrentElevation")
              (cdr (reverse (assoc 10 pline1data)))
            )
          )
          (assoc 10 pline1data)
          pline1data
        )
       )
       ((= (cdr (assoc 0 pline1data)) "LWPOLYLINE")
        (cond
          ((not (assoc 38 pline1data))
           (reverse
             (cons
               (cons 38 (haws-cei:getvar "CurrentElevation"))
               (reverse pline1data)
             )
           )
          )
          (t
           (subst
             (cons 38 (haws-cei:getvar "CurrentElevation"))
             (assoc 38 pline1data)
             pline1data
           )
          )
        )
       )
     )
  )
  (entmod pline1data)
)

(defun
   haws-cei:color-contour (entsel1 / color)
  (cond
    ((/= "." (setq color (haws-cei:getvar "TemporaryColor")))
     (command "._chprop" entsel1 "" "_color" color "")
    )
  )
)

(defun
   haws-cei:label-contour (entsel1 /)
  (haws-cei:initialize-measure-block)
  (haws-cei:add-labels entsel1)
)

(defun
   haws-cei:initialize-measure-block (/ ds ts)
  (cond
    ((and
       (not (tblsearch "BLOCK" "cei-contour-label"))
       (not (findfile "cei-contour-label.dwg"))
     )
     (alert
       (strcat
         (strcase (haws-cei:getvar "LabelBlockName"))
         " block not found in drawing or on disk.\nCreating vanilla label based on dimscale and dimtxt.\n\nMake your own mtext block if you need to."
       )
     )
     (setq
       ds (getvar "dimscale")
       ts (* ds (getvar "dimtxt"))
     )
     (haws-cei:make-masked-mtext "0,0,0" "_mc" ts "0" "%<\\AcVar Filename \\f \"%fn7\">%")
     (command
       "._block"
       (haws-cei:getvar "LabelBlockName")
       "0,0,0"
       (entlast)
       ""
     )
    )
  )
)

(defun
   haws-cei:make-masked-mtext (i j h w s / ename-mtext)
  (command "._mtext" i "_j" j "_h" h "_w" w s "")
  (setq ename-mtext (entlast))
  (entmod
    (append
      (entget ename-mtext)
      '((90 . 3) (63 . 256) (45 . 1.1) (441 . 0))
    )
  )
)

(defun
   haws-cei:add-labels (entsel1 / eg1 en1 enext entsel1-length)
  (setq enext (entlast))
  (command "._area" "_object" entsel1)
  (setq entsel1-length (getvar "perimeter"))
  (command
    "._divide"
    entsel1
    "b"
    (haws-cei:getvar "LabelBlockName")
    "y"
    (max
      (+ (fix (/ entsel1-length (haws-cei:getvar "LabelSpacing")))
         1
      )
      2
    )
  )
  (while (setq enext (entnext enext))
    (if (= (cdr (assoc 2 (entget enext)))
           (haws-cei:getvar "LabelBlockName")
        )
      (command "._explode" enext)
    )
    (setq eg1 (entget (setq en1 (entlast))))
    (cond
      ((= (cdr (assoc 0 eg1)) "MTEXT")
       (cond
         ((= (haws-cei:getvar "LabelWithFields") "Yes")
          (haws-cei:update-linked-elevation-field (car entsel1) en1)
         )
         (T
          (haws-cei:make-elevation-string)
         )
       )
      )
    )
  )
)

(defun
   haws-cei:make-elevation-string ()
  (entmod
    (subst
      (cons
        1
        (rtos
          (haws-cei:getvar "CurrentElevation")
          2
          (haws-cei:getvar "LabelPrecision")
        )
      )
      (assoc 1 eg1)
      eg1
    )
  )
)

(defun
   haws-cei:update-linked-elevation-field (enamecontour enamemtext / DICT EGMTEXT ENAMEMTEXTVLA FLST NEWSTR)
  (setq egmtext (entget enamecontour))
  (setq
    dict
     (vlax-vla-object->ename
       (vla-getextensiondictionary
         (vlax-ename->vla-object enamemtext)
       )
     )
  )
  (setq
    flst
     (entget
       (cdr
         (assoc
           360
           (entget (cdr (last (dictnext dict "ACAD_FIELD"))))
         )
       )
     )
  )
  (setq
    newstr
     (strcat
       "%<\\AcObjProp Object(%<\\_ObjId "
       (itoa
         (vla-get-objectid (vlax-ename->vla-object enamecontour))
       )
       ">%).Elevation \\f \"%lu2%pr"(itoa(haws-cei:getvar "LabelPrecision"))"\">%"
     )
  )
  (setq flst (subst (cons 2 newstr) (assoc 2 flst) flst))
  (entmod flst)
  (setq enamemtextvla (vlax-ename->vla-object enamemtext))
  (vla-put-textstring enamemtextvla (vla-fieldcode enamemtextvla))
)

;; Reset settings on load
(haws-cei:get-default-settings)

 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 1 1 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
