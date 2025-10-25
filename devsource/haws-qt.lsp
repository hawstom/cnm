;;; Written by Thomas Gail Haws
;;; AET section
(defun c:haws-acres ()
  (haws-core-init 156)
  (haws-aet (/ 1.0 43560) " AC")
)
(defun c:haws-sf () (haws-core-init 157) (haws-aet 1 " SF"))
(defun haws_evangel_msg_prompt ()
  (prompt (strcat "\n" (haws_evangel_msg)))
)

(defun c:haws-aet () (haws-core-init 158) (haws_evangel_msg_prompt) (haws-aet 1 ""))
(defun c:haws-sm ()
  (haws-core-init 159)
  (haws-aet (/ 1.0 27878400) " SQ. MI.")
)
(defun c:haws-sy ()
  (haws-core-init 160)
  (haws-aet (/ 1.0 9) " SY")
)
(defun haws-aet (factor label / area ts txpt)
  (setq haws-qt-instance (haws-qt-new "haws-aet"))
  (haws-qt-set-property haws-qt-instance "type" "area")
  (haws-qt-set-property haws-qt-instance "factor" factor)
  (haws-qt-set-property haws-qt-instance "postfix" label)
  (haws-qt-set-property
    haws-qt-instance
    "precision"
    (getvar "luprec")
  )
  (haws-qt-string haws-qt-instance)
  (haws-qt-add-drawing-text haws-qt-instance)
  (princ)
)
;;; ADL section
(defun c:haws-adl () (haws-adl 1 "LF"))
(defun haws-adl (factor label / e ss len i haws-qt-instance)
  (haws-core-init 0)
  (setq haws-qt-instance (haws-qt-new "haws-adl"))
  (haws-qt-set-property haws-qt-instance "type" "length")
  (haws-qt-set-property
    haws-qt-instance
    "precision"
    (getvar "luprec")
  )
  (haws-qt-string haws-qt-instance)
  (haws-qt-add-drawing-text haws-qt-instance)
  (princ)
)
;;; HAWS-QT "Class" library section

;; Constructs an instance of the HAWS-QT object if not already present
;; NAME is the string that identifies this instance.
;; type can be "area", "length"
;; mode can be "ss", "nes"
;; destination can be "return", "prompt", "drawing", "text", "attribute"
;; action can be "replace", "prepend", "append"
;; space can be "mspace" or "pspace"
(defun haws-qt-new (name / instance)
  (setq
    instance
     (list
       name
       (cons "name" name)
       (cons "mode" "ss")
       (cons "type" "area")
       (cons "factor" 1)
       (cons "underline_string" "%%u")
       (cons "underline_p" nil)
       (cons "overline_string" "%%o")
       (cons "overline_p" nil)
       (cons "prefix" "")
       (cons "postfix" "")
       (cons "precision" 0)
       (cons "destination" "prompt")
       (cons "position" "replace")
       (cons "string" "")
       (cons "space" nil)
     )
  )
  (cond
    ((not (assoc name *haws-qt-instances*))
     (setq *haws-qt-instances* (cons instance *haws-qt-instances*))
    )
  )
  name
)

(defun haws-qt-set-property (instance property value)
  (setq
    *haws-qt-instances*
     (subst
       (cond
         ((assoc
            property
            (cdr
              (assoc instance *haws-qt-instances*)
            )
          )
          ;; Update it if it exists
          (subst
            (cons property value)
            (assoc
              property
              (cdr
                (assoc instance *haws-qt-instances*)
              )
            )
            (assoc instance *haws-qt-instances*)
          )
         )
         (t
          ;; Add it if it doesn't exist
          (reverse
            (cons
              (cons property value)
              (reverse
                (assoc instance *haws-qt-instances*)
              )
            )
          )
         )
       )
       (assoc instance *haws-qt-instances*)
       *haws-qt-instances*
     )
  )
  value
)

(defun haws-qt-get-property (instance property)
  (cdr
    (assoc property (cdr (assoc instance *haws-qt-instances*)))
  )
)

;; Gives user custom prompt for selection.
;; Follows HAWS-QT instance properties for formatting: factor, underline, overline, prefix, postfix, precision
;; Sets HAWS-QT instance string property
(defun haws-qt-string (instance / ss total)
  (setq total (haws-qt-ss-total instance))
  (haws-qt-set-property
    instance
    "string"
    (strcat
      (cond
        ((haws-qt-get-property instance "underline_p")
         (haws-qt-get-property instance "underline_string")
        )
        ((haws-qt-get-property instance "overline_p")
         (haws-qt-get-property instance "overline_string")
        )
        ("")
      )
      (haws-qt-get-property instance "prefix")
      (rtos
        (* total (haws-qt-get-property instance "factor"))
        2
        (haws-qt-get-property instance "precision")
      )
      (haws-qt-get-property instance "postfix")
    )
  )
)
;; Returns a real number in native units.
;; Valid types: "length", "area"
(defun haws-qt-ss-total (instance / e enamelist i q qi qtype smode)
  (setq
    q 0.0
    qtype
     (haws-qt-get-property instance "type")
    enamelist
     (haws-qt-select)
  )
  (cond
    (enamelist
     (foreach e enamelist (setq q (haws-qt-add-ent e q qtype)))
    )
  )
  q
)
(defun haws-qt-select (/ continue-p enamelist input-results layerlist ss-p)
  (setq enamelist '())
  (while (not continue-p)
    (setq
      input-results
       (cond
         (ss-p (haws-qt-select-ssget))
         (t (haws-qt-select-multi-nentsel))
       )
      ss-p
       (cadr input-results)
      continue-p
       (caddr input-results)
    )
    (cond
      ((car input-results)
       (setq enamelist (append enamelist (car input-results)))
      )
    )
  )
  enamelist
)
(defun haws-qt-select-ssget (/ continue-p enamelist input1 ss-p)
  (setq
    input1
     (ssget)
    continue-p
     (not input1)
    ss-p nil
    enamelist
     (haws-qt-select-ss-to-list input1)
  )
  (list enamelist ss-p continue-p)
)
(defun haws-qt-select-ss-to-list (input1 / en i ss1 sslist)
  (cond
    ((and input1 (= (type input1) 'PICKSET))
     (setq
       ss1 input1
       i   -1
     )
     (while (setq en (ssname ss1 (setq i (1+ i))))
       (setq sslist (cons en sslist))
     )
     sslist
    )
  )
)
(defun haws-qt-select-multi-nentsel
   (/ continue-p enamelist input1 ss-p nentsel-results)
  (initget "Selection Continue")
  (setq
    input1
     (nentsel
       (strcat
         "\nSelect nested object or [Selection set/Continue] <Continue>: "
       )
     )
    continue-p
     (not input1)
    ss-p
     (= input1 "Selection")
    enamelist
     (cond
       ((and input1 (= (type input1) 'LIST))
        (princ (strcat "\n" (cdr(assoc 0 (entget (car input1)))) " was selected."))
        (list (car input1))
       )
       (t nil)
     )
  )
  (list enamelist ss-p continue-p)
)
(defun haws-qt-add-ent (e q qtype / obj qi)
  (if (and
        (= (cdr (assoc 0 (entget e))) "VERTEX")
        (assoc 330 (entget e))
      )
    (setq e (cdr (assoc 330 (entget e))))
  )
  (setq
    obj
     (vlax-ename->vla-object e)
    errobj
     (vl-catch-all-apply 'VLAX-CURVE-GETSTARTPOINT (list obj))
    qi (cond
         ((vl-catch-all-error-p errobj)
          (princ
            (strcat
              "\nCan't get quantity from a "
              (cdr (assoc 0 (entget e)))
              "."
            )
          )
          0
         )
         ((= qtype "length")
          (cond
            ((= (cdr (assoc 0 (entget e))) "AECC_PIPE")
             (princ "\nGetting 3D length of AECC_PIPE object.")
             (vlax-get-property obj 'LENGTH3D)
            )
            (t
             (vlax-curve-getdistatparam
               obj
               (vlax-curve-getendparam obj)
             )
            )
          )
         )
         ((= qtype "area") (vlax-curve-getarea obj))
       )
    q (cond
        (qi (+ q qi))
        (q)
      )
  )
  q
)
(defun haws-qt-add-drawing-text (instance / qt-string ts txpt)
  (princ (strcat "\n"))
  (setq
    txpt (getpoint
           (strcat
             "\nMiddle point for text \""
             (setq qt-string (haws-qt-get-property instance "string"))
             "\": "
           )
         )
    ts   (* (getvar "dimscale") (getvar "dimtxt"))
  )
  (if txpt
    (haws-mktext "m" txpt nil 0 qt-string)
  )
)
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|
