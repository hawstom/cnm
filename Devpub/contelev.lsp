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
;;; Please AutoCAD:Be_bold in adding clarifying comments and improvements at
;;; http://autocad.wikia.com/wiki/Contour_Elevations_at_Intervals_with_Labels_(AutoLISP_application)

;;; Contours Elevate and/or Label
;;; Copyleft 2017 Thomas Gail Haws licensed under the terms of the GNU GPL
;;; http://www.hawsedc.com tom.haws@gmail.com
;;; Version: 1.0.1
;;; Official Repository: http://autocad.wikia.com/wiki/Contour_Elevations_at_Intervals_with_Labels_(AutoLISP_application
;;; Haws is a registered reserved symbol with Autodesk that will never conflict with other apps.
;;;
;;; Features:
;;; -Uses an exploded block for labeling so you can customize label as needed.
;;; -Saves to (setcfg) to remember settings between sessions.  Saves to a single global variable during a session.
;;; -Lets you ignore Interval, Label Precision, Label Spacing, Temporary Color, LabelBlockName, and Elevation or change them on the fly.
;;; -For programmers, demonstrates small functions with self-documenting names and variable names.  Also demonstrates settings management.

;; Customizable out-of-the-box defaults you can edit are at very end of file

(defun c:haws-contelev ()
(haws-core-init 10) (haws-cel:main))

(defun
   haws-cel:main (/ input-main)
  (haws-cel:initialize-settings)
  (haws-cel:initialize-elevation)
  (command "._undo" "_group")
  (while (setq input-main (haws-cel:get-input-main))
    (haws-cel:do-input-main input-main)
  )
  (command "._undo" "_end")
  (princ)
)

(defun
   haws-cel:initialize-elevation ()
  (cond
    ((or (not (haws-cel:getvar "CurrentElevation"))
         (= (haws-cel:getvar "ElevationPrompt") "Yes")
     )
     (haws-cel:get-elevation)
    )
  )
)

(defun
   haws-cel:get-input-main (/ input-main current-elevation)
  (haws-cel:print-settings)
  (initget
    "Elevation Interval Spacing Color Precision Multilabel"
  )
  (entsel
    "\nSelect contour to elevate and label or [new Elevation/contour Interval/label Spacing/temporary Color/Precision/Multilabel]: "
  )
)

(defun
   haws-cel:print-settings (/ setting)
  (princ "\nCurrent settings: ")
  (foreach
     setting *haws-cel:settings*
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
   haws-cel:do-input-main (input-main /)
  (cond
    ((or (= input-main "Elevation") (= input-main ""))
     (haws-cel:get-elevation)
    )
    ((= input-main "Interval") (haws-cel:get-contour-interval))
    ((= input-main "Spacing") (haws-cel:get-label-spacing))
    ((= input-main "Color") (haws-cel:get-temporary-color))
    ((= input-main "Precision") (haws-cel:get-precision))
    ((= input-main "Multilabel") (haws-cel:do-multilabel))
    (t (haws-cel:do-next-contour input-main))
  )
)

(defun
   haws-cel:get-elevation (/ elevation-point)
  (initget "Text Prompt Multilabel")
  (setq
    elevation-point
     (getpoint
       "\nSpecify point at starting elevation or [enter Text/elevation Prompt mode/Multilabel] <Text>: "
     )
  )
  (cond
    ((= elevation-point "Prompt")
     (haws-cel:get-elevation-prompt)
    )
    ((= elevation-point "Multilabel") (haws-cel:do-multilabel))
    ((or (= elevation-point "Text") (not elevation-point))
     (haws-cel:get-elevation-text)
    )
    (t (haws-cel:get-elevation-from-point elevation-point))
  )
)

(defun
   haws-cel:get-elevation-from-point (elevation-point)
  (haws-cel:setvar "CurrentElevation" (caddr elevation-point))
)

(defun
   haws-cel:get-elevation-text ()
  (haws-cel:setvar
    "CurrentElevation"
    (getreal "\nNew current elevation: ")
  )
)

(defun
   haws-cel:get-elevation-prompt ()
  (initget "Yes No")
  (haws-cel:get-input-generic
    "ElevationPrompt"
    'getkword
    "Always prompt for elevation before selecting contours? [Yes/No]"
  )
  (haws-cel:get-elevation)
)

(defun
   haws-cel:get-contour-interval ()
  (haws-cel:get-input-generic
    "ContourInterval"
    'getreal
    "Contour interval"
  )
)

(defun
   haws-cel:get-label-spacing ()
  (haws-cel:get-input-generic
    "LabelSpacing"
    'getreal
    "Spacing for labels along contours"
  )
)

(defun
   haws-cel:get-temporary-color ()
  (haws-cel:get-input-generic
    "TemporaryColor"
    'getstring
    "Temporary color to distinguish elevated contours or . for none"
  )
)

(defun
   haws-cel:get-precision ()
  (haws-cel:get-input-generic
    "LabelPrecision"
    'getint
    "Decimal places of label precision"
  )
)

(defun
   haws-cel:get-input-generic (var function-symbol prompt1 / input1)
  (setq
    input1
     (apply
       function-symbol
       (list
         (strcat "\n" prompt1 " <" (haws-cel:getvar-string var) ">: ")
       )
     )
  )
  (cond ((and input1 (/= input1 "")) (haws-cel:setvar var input1)))
)

(defun
   haws-cel:do-multilabel ( / CONTOURSET EN ENAME-CONTOUR ENTSEL-CONTOUR I)
  (setq
    contourset (ssget)
    i   -1
  )
  (while (setq ename-contour (ssname contourset (setq i (1+ i))))
    (setq entsel-contour (list ename-contour (cdr (assoc 10 (entget ename-contour)))))
    (haws-cel:label-contour
      entsel-contour
    )
  )
)

(defun
   haws-cel:do-next-contour (entsel1 /)
  (haws-cel:elevate-contour entsel1)
  (haws-cel:color-contour entsel1)
  (haws-cel:label-contour entsel1)
  (haws-cel:setvar
    "CurrentElevation"
    (+ (haws-cel:getvar "CurrentElevation")
       (haws-cel:getvar "ContourInterval")
    )
  )
)


(defun
   haws-cel:elevate-contour
   (entsel1 / current-elevation pline1 pline1data)
  (setq
    pline1
     (car entsel1)
    current-elevation
     (haws-cel:getvar "CurrentElevation")
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
              (haws-cel:getvar "CurrentElevation")
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
               (cons 38 (haws-cel:getvar "CurrentElevation"))
               (reverse pline1data)
             )
           )
          )
          (t
           (subst
             (cons 38 (haws-cel:getvar "CurrentElevation"))
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
   haws-cel:color-contour (entsel1 / color)
  (cond
    ((/= "." (setq color (haws-cel:getvar "TemporaryColor")))
     (command "._chprop" entsel1 "" "_color" color "")
    )
  )
)

(defun
   haws-cel:label-contour (entsel1 /)
  (haws-cel:initialize-measure-block)
  (haws-cel:add-labels entsel1)
)

(defun
   haws-cel:initialize-measure-block (/ dimscale dimtxt preset-string text-height)
  (cond
    ((and
       (not (tblsearch "BLOCK" (haws-cel:getvar "LabelBlockName")))
       (not (findfile (strcat (haws-cel:getvar "LabelBlockName") ".dwg")))
     )
     (alert
       (strcat
         "Creating vanilla label block\n"
         (strcase (haws-cel:getvar "LabelBlockName"))
         "\nbased on current text style and dimension text height.\n\nMake your own mtext block if you need to."
       )
     )
     (setq
       preset-string
        (cond
          ((= (haws-cel:getvar "LabelWithFields") "Yes")
           "%<\\AcVar Filename \\f \"%fn7\">%"
          )
          (t "Label")
        )
     )
     (haws-cel:make-masked-mtext "0,0,0" "_mc" (* (if (LM:isAnnotative (getvar "textstyle")) 1 (haws-dwgscale))(getvar "dimtxt")) "0" preset-string)
     (command
       "._block"
       (haws-cel:getvar "LabelBlockName")
       "0,0,0"
       (entlast)
       ""
     )
    )
  )
)

(defun
   haws-cel:make-masked-mtext (i j h w s / ename-mtext)
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
   haws-cel:add-labels (entsel1 / eg1 en1 enext entsel1-length)
  (setq enext (entlast))
  (command "._area" "_object" entsel1)
  (setq entsel1-length (getvar "perimeter"))
  (command
    "._divide"
    entsel1
    "b"
    (haws-cel:getvar "LabelBlockName")
    "y"
    (max
      (+ (fix (/ entsel1-length (haws-cel:getvar "LabelSpacing")))
         1
      )
      2
    )
  )
  (while (setq enext (entnext enext))
    (if (= (cdr (assoc 2 (entget enext)))
           (haws-cel:getvar "LabelBlockName")
        )
      (command "._explode" enext)
    )
    (setq eg1 (entget (setq en1 (entlast))))
    (cond
      ((= (cdr (assoc 0 eg1)) "MTEXT")
       (cond
         ((= (haws-cel:getvar "LabelWithFields") "Yes")
          (haws-cel:update-linked-elevation-field (car entsel1) en1)
         )
         (T
          (haws-cel:make-elevation-string)
         )
       )
      )
    )
  )
)

(defun
   haws-cel:make-elevation-string ()
  (entmod
    (subst
      (cons
        1
        (rtos
          (haws-cel:getvar "CurrentElevation")
          2
          (haws-cel:getvar "LabelPrecision")
        )
      )
      (assoc 1 eg1)
      eg1
    )
  )
)

;;; TODO: It would be good for somebody who understands this function to improve the symbol names for readability.
(defun
   haws-cel:update-linked-elevation-field (enamecontour enamemtext / DICT EGMTEXT ENAMEMTEXTVLA FLST NEWSTR)
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
       ">%).Elevation \\f \"%lu2%pr"(itoa(haws-cel:getvar "LabelPrecision"))"\">%"
     )
  )
  (setq flst (subst (cons 2 newstr) (assoc 2 flst) flst))
  (entmod flst)
  (setq enamemtextvla (vlax-ename->vla-object enamemtext))
  (vla-put-textstring enamemtextvla (vla-fieldcode enamemtextvla))
)
;;; ============================================================================
;;; Settings stuff.  Last part of code; not fun to read for new project member.
;;; ============================================================================
;; Start with default settings and supplement with stored settings.
(defun
   haws-cel:initialize-settings (/ luprec setting)
  (cond ((not *haws-cel:settings*) (haws-cel:get-default-settings)))
  (haws-cel:get-stored-settings)
  ;; If drawing LUPREC changed, use it.
  (cond
    ((/= (setq luprec (getvar "LUPREC"))
         (haws-cel:getvar "LUPREC")
     )
     (haws-cel:setvar "LUPREC" luprec)
     (haws-cel:setvar "LabelPrecision" luprec)
    )
  )
)

;; Define-Settings is at bottom of code for customization convenience.
(defun
   haws-cel:get-default-settings ()
  (setq *haws-cel:settings* (haws-cel:define-settings))
)

;; Get settings from AutoCAD's AutoLISP permananent storage system
;; The setcfg/getcfg functions might be removed in a future release.
(defun
   haws-cel:get-stored-settings (/ settings-definition valuei)
  (setq settings-definition (haws-cel:define-settings))
  (cond
    ;; If stored settings location exists
    ((getcfg (strcat (haws-cel:storage-location) "Dummy"))
     (foreach
        setting settings-definition
       (cond
         ;; If setting exists (even missing settings return "")
         ((/= ""
              (setq
                valuei
                 (getcfg
                   (strcat (haws-cel:storage-location) (car setting))
                 )
              )
          )
          (haws-cel:save-to-settings-list (car setting) valuei)
         )
       )
     )
    )
  )
)

(defun haws-cel:storage-location () "Appdata/Haws/CEI/")

(defun
   haws-cel:save-to-settings-list (var val)
  (setq
    *haws-cel:settings*
     (subst
       (list var val (haws-cel:getvar-type var))
       (assoc var *haws-cel:settings*)
       *haws-cel:settings*
     )
  )
)

(defun
   haws-cel:getvar-string (var / val-string)
     (cadr (assoc var *haws-cel:settings*))
)

(defun
   haws-cel:getvar (var / val-string)
  (setq
    val-string
     (haws-cel:getvar-string var)
    var-type
     (caddr (assoc var *haws-cel:settings*))
    val
     (cond
       ((= var-type 'real) (distof val-string)) ; Returns nil for ""
       ((= var-type 'int) (atoi val-string))
       ((= var-type 'str) val-string)
     )
  )
)

(defun
   haws-cel:getvar-type (var / val-string)
  (caddr (assoc var *haws-cel:settings*))
)

(defun
   haws-cel:setvar (var val / var-type)
  (setq var-type (haws-cel:getvar-type var))
  (cond
    ((/= (type val) var-type)
     (alert
       (strcat
         "Warning in haws-cel:SETVAR.\n\nVariable: "
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
  (haws-cel:save-to-settings-list var val)
  (haws-cel:save-to-storage var val)
  val
)

(defun
   haws-cel:save-to-storage (var val)
  (setcfg (strcat (haws-cel:storage-location) var) val)
)

;; Customizable out-of-the-box defaults.  You can edit these.
(defun
   haws-cel:define-settings (/ luprec)
  (setq luprec (itoa (getvar "LUPREC")))
  (list
    ;; Save so we know if user changes it.
    ;; We're assuming they may expect label precision to follow AutoCAD units.
    (list "LUPREC" luprec 'int)
    ;; At runtime retrieval, each setting is converted 
    ;; from its storage as a string to the given data type.
    ;;    Name             Value Data_type
    (list "CurrentElevation" "" 'real)
    (list "ContourInterval" "1.0" 'real)
    (list "LabelSpacing" "500.0" 'real)
    (list "TemporaryColor" "." 'str)
    (list "LabelPrecision" luprec 'int)
    (list "LabelBlockName" "cei-contour-label" 'str)
    (list "ElevationPrompt" "Yes" 'str) ;"Yes" or "No"
    (list "LabelWithFields" "No" 'str) ;"Yes" or "No"
  )
)

;; Initialize settings on load
(haws-cel:get-default-settings)

 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 1 1 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;