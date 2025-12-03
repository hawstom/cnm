;;; UTILITY LINES
;;; (C) Copyright 2017 by Thomas Gail Haws
(defun c:haws-ut (/ pt1)
  (haws-core-init 326)
  (haws-vsave
    '("ucsfollow" "clayer" "filedia" "cmdecho" "expert"  "osmode")
  )
  (haws-vset
    '(("ucsfollow" 0) ("plinegen" 1) ("cmdecho" 0) ("expert" 5))
  )
  (if (not *haws-ut-settings*)
    (haws-ut-getutl)
  )
  (while (progn
           (initget "Setup")
           (= (setq pt1 (getpoint "\n<Start point>/Setup:")) "Setup")
         )
    (haws-ut-getutl)
  )
  (haws-ut-main pt1)
)

(defun haws-ut-main (pt1 / e1 e2 elnext enext entsel-od-down entsel-od-up
                 pt2 pt3 temp utlen utpl
                )
  ;; Draw centerline using PLINE command
  (haws-setlayr (strcat "UT" (haws-ut-getvar "Key") "SNG"))
  (vl-cmdf "pline" pt1 "_w" 0 "")
  (setvar "cmdecho" 1)
  (while (= 1 (logand 1 (getvar "cmdactive")))
    (if pt3
      (vl-cmdf pause) ; If not the first point, just follow command
      (progn ; If first point, save for offset.
        (initget
          "Line Undo Arc CEnter Angle Direction Line Radius Second DRag"
        )
        (setq pt2 (getpoint pt1))
        (if (= (type pt2) 'LIST)
          (setq pt3 pt2)
        )
        (if (= pt2 "CEnter")
          (prompt "\nCEnter not allowed.")
          (vl-cmdf pt2)
        )
      )
    )
  )
  (setvar "cmdecho" 0)
  ;; Offset OD polylines
  (cond
    ((/= (haws-ut-getvar "HalfWidth") 0)
     (setq utpl (entlast))
     (vl-cmdf "._line" pt1 pt3 "")
     (setq
       e1   (entlast)
       ucsp t
     )
     (setvar "osmode" 0)
     (vl-cmdf "._ucs" "_e" e1 "._erase" e1 "")
     (setq entsel-od-up (haws-ut-make-od utpl 1))
     (setq entsel-od-down (haws-ut-make-od utpl -1))
     (vl-cmdf
       "._pedit"
       utpl
       "_w"
       (* 2 (haws-ut-getvar "HalfWidth"))
       ""
       "._change"
       utpl
       ""
       "_p"
       "_la"
       (haws-ut-getvar "Layer.PL")
       ""
     )
    )
  )
  ;; Create labels
  (cond
    ((and (haws-ut-getvar "IsExisting") (haws-ut-getvar "Label"))
     (vl-cmdf "._area" "_e" (car entsel-od-up))
     (setq utlen (getvar "perimeter"))
     (setq
       e1    (entlast)
       enext e1
     )
     (if (= (haws-ut-getvar "HalfWidth") 0)
       (haws-ut-assure-label-block "uttxtjm" "m" pt1)
       (haws-ut-assure-label-block "uttxtjbc" "bc" pt1)
     )
     (vl-cmdf
       "._divide"
       entsel-od-up
       "b"
       (if (= (haws-ut-getvar "HalfWidth") 0)
         "uttxtjm"
         "uttxtjbc"
       )
       "y"
       (max 2 (atoi (rtos (/ utlen 200) 2 0)))
     )
     (haws-ut-restore-ucs)
     (while (setq enext (entnext enext))
       (if (= (cdr (assoc 0 (entget enext))) "INSERT")
         (vl-cmdf "._explode" enext)
       )
     )
     (setq enext e1)
     (while (setq enext (entnext enext))
       (cond
         ((= (cdr (assoc 0 (setq elnext (entget enext)))) "TEXT")
          (subst
            (cons 1 (haws-ut-getvar "Label"))
            (assoc 1 elnext)
            elnext
          )
          (entmod
            (subst
              (cons 40 (* (haws-dwgscale) (getvar "dimtxt")))
              (assoc 40 elnext)
              (subst
                (cons 8 (getvar "clayer"))
                (assoc 8 elnext)
                (subst
                  (cons 1 (haws-ut-getvar "Label"))
                  (assoc 1 elnext)
                  elnext
                )
              )
            )
          )
         )
       )
     )
    )
    (t (haws-ut-restore-ucs))
  )
  (redraw)
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)

(defun haws-ut-getutl (/ temp label-material label label-size label-type)
  (initget 1 "Exist Prop")
  (haws-ut-setvar
    "IsExisting"
    (= (getkword "\nExist/Prop:") "Exist")
  )
  (initget 1 "Wat Sew SD Irr Gas Elec Tel Catv")
  (haws-ut-setvar
    "Key"
    (strcase (getkword "\nWat/Sew/SD/Irr/Gas/Elec/Tel/Catv:"))
  )
  (setq label-size "")
  (cond
    ((haws-ut-getvar "IsExisting")
     (setq label-size (getstring (strcat "\nSize for label <none>: ")))
    )
  )
  (haws-ut-setvar
    "HalfWidth"
    (cond
      ((haws-ut-getvar "IsExisting") (/ (atof label-size) 24))
      ((haws-ut-getvar "HalfWidth"))
      (0)
    )
  )
  (setq
    label-size
     (if (= label-size "")
       ""
       (strcat label-size "\" ")
     )
  )
  (setvar "filedia" 0)
  (vl-cmdf "._vslide" "pipetabl")
  (setvar "filedia" 1)
  (if (setq
        temp
         (getreal
           (strcat
             "\nSize for graphic width (0 for small)<"
             (rtos (* 24 (haws-ut-getvar "HalfWidth")) 5 2)
             ">:"
           )
         )
      )
    (haws-ut-setvar "HalfWidth" (/ temp 24))
  )
  (redraw)
  (cond
    ((haws-ut-getvar "IsExisting")
     (setq
       label-type
        (cond
          ((= (haws-ut-getvar "Key") "WAT") "W")
          ((= (haws-ut-getvar "Key") "SEW") "S")
          ((= (haws-ut-getvar "Key") "SD") "SD")
          ((= (haws-ut-getvar "Key") "IRR") "IRR")
          ((= (haws-ut-getvar "Key") "GAS") "G")
          ((= (haws-ut-getvar "Key") "ELEC") "E")
          ((= (haws-ut-getvar "Key") "TEL") "T")
          ((= (haws-ut-getvar "Key") "CATV") "TV")
        )
       temp
        (getstring "\nMaterial for label <none>: ")
       label-material
        (if (= temp "")
          temp
          (strcat " (" temp ")")
        )
       label
        (strcat label-size label-type label-material)
       temp
        (getstring
          1
          (strcat "\nFull label or . for none <" label ">: ")
        )
     )
     (haws-ut-setvar
       "Label"
       (cond
         ((= temp "") label)
         ((= temp ".") nil)
         (temp)
       )
     )
    )
  )
  (if (haws-ut-getvar "IsExisting")
    (haws-ut-setvar "Key" (strcat "X" (haws-ut-getvar "Key")))
  )
  (haws-ut-setvar
    "Layer.PL"
    (car
      (haws-setlayr (strcat "UT" (haws-ut-getvar "Key") "PL"))
    )
  )
  (haws-ut-setvar
    "Layer.OD"
    (car
      (haws-setlayr (strcat "UT" (haws-ut-getvar "Key") "OD"))
    )
  )
)

(defun haws-ut-getvar (var)
  (setq var (strcase var))
  (cadr (assoc var *haws-ut-settings*))
)

(defun haws-ut-setvar (var val)
  (setq
    var                (strcase var)
    *haws-ut-settings* (cond
                         ((assoc var *haws-ut-settings*)
                          (subst
                            (list var val)
                            (assoc var *haws-ut-settings*)
                            *haws-ut-settings*
                          )
                         )
                         (t (cons (list var val) *haws-ut-settings*))
                       )
  )
  val
)

(defun haws-ut-assure-label-block (block-name justification insertion)
  (cond
    ((and
       (haws-ut-getvar "IsExisting")
       (haws-ut-getvar "Label")
       (not (tblsearch "BLOCK" block-name))
     )
     (haws-mktext justification insertion 1.0 0 "X")
     (vl-cmdf "._block" block-name insertion (entlast) "")
    )
  )
)

(defun haws-ut-make-od (ename-pline direction / ename-od offset-point)
  (vl-cmdf
    "._offset"
    (haws-ut-getvar "HalfWidth")
    (list ename-pline '(0 0))
    "_non"
    (setq
      offset-point
       (list 0 (* direction (haws-ut-getvar "HalfWidth")) 0)
    )
    ""
  )
  (setq ename-od (entlast))
  (entmod
    (subst
      (cons 8 (haws-ut-getvar "Layer.OD"))
      (assoc 8 (entget ename-od))
      (entget ename-od)
    )
  )
  (list ename-od offset-point)
)
(defun haws-ut-restore-ucs ()
  (cond (ucsp (setq ucsp nil) (vl-cmdf "._ucs" "_p")))
)
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
