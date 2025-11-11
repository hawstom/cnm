;;;(C) Copyright 2017 by Thomas Gail Haws
;;; This AutoLISP program lists a nested entity inside a XREF, BLOCK, or PLINE
(if (not hcnm-config-getvar)(c:haws-load-from-app-dir "cnm"))

(defun c:haws-lx ()
(haws-core-init 255)
(prompt (strcat "\n" (haws_evangel_msg)))
(haws-xlist 0))
(defun c:haws-lxx ()
(haws-core-init 256) (haws-xlist-multi))

(defun haws-xlist (edit-mode / nentsel-results)
  (setq nentsel-results (haws-xlist-nentsel (nentsel)))
  (haws-xlist-list)
  (cond
    ((> edit-mode 0)
     (haws-xlist-show-layers
       (haws-xlist-list-to-string (cadr nentsel-results) edit-mode)
     )
    )
  )
  (princ)
)

(defun haws-xlist-multi (/ continue-p enamelist input-results layerlist ss-p)
  (setq enamelist '())
  (while (not continue-p)
    (setq
      input-results
       (cond
         (ss-p (haws-xlist-multi-ssget))
         (t (haws-xlist-multi-nentsel))
       )
       enamelist
       (append enamelist (car input-results))
      ss-p
       (cadr input-results)
      continue-p
       (caddr input-results)
    )
  )
  (haws-xlist-show-layers
    (haws-xlist-list-to-string enamelist 2)
  )
)

(defun haws-xlist-multi-nentsel (/ continue-p enamelist input1 ss-p nentsel-results)
  (initget "Selection List Continue")
  (setq
    input1
     (nentsel
       (strcat
         "\nSelect object or [Selection set/List mode ("
         (hcnm-config-getvar "LXXListMode")
         ")/Continue] <Continue>: "
       )
     )
    continue-p
     (not input1)
    ss-p
     (= input1 "Selection")
    nentsel-results
     (haws-xlist-nentsel input1)
    enamelist
     (cadr nentsel-results)
  )
  (cond ((= input1 "List") (haws-xlist-toggle-list-mode)))
  (cond ((= (hcnm-config-getvar "LXXListMode") "yes") (haws-xlist-list)))
  (list enamelist ss-p continue-p)
)

(defun haws-xlist-list ()
   (cond ((/= (car nentsel-results) "") (alert (princ (car nentsel-results)))))
)

(defun haws-xlist-toggle-list-mode ()
  (hcnm-config-setvar
    "LXXListMode"
    (cond
      ((= (hcnm-config-getvar "LXXListMode") "yes") "no")
      (t "yes")
    )
  )
)

(defun haws-xlist-multi-ssget (/ continue-p enamelist input1 ss-p)
  (setq
    input1
     (ssget)
    continue-p
     (not input1)
    ss-p
     nil
    enamelist
    (cadr (haws-xlist-ss-to-list input1))
  )
  (list enamelist ss-p continue-p)
)

(defun haws-xlist-ss-to-list (input1 / en i ss1 sslist)
  (cond
    ((and input1 (= (type input1) 'PICKSET))
     (setq
       ss1 input1
       i   -1
     )
     (while (setq en (ssname ss1 (setq i (1+ i))))
       (setq sslist (cons en sslist))
     )
     ;; Used only for editing, not reporting, so return empty string
     (list "" sslist)
    )
  )
)

(defun haws-xlist-nentsel (input1 / alrtstr alrtstri el es eclr i laclr lai
                       shells etype shelli shelll previ temp
                      )
  (cond
    ((and input1 (= (type input1) 'LIST))
     (setq
       es input1
       shells
        (cadddr es)
       el (entget (car es))
       etype
        (haws-dxf 0 el)
       eclr
        (if (haws-dxf 62 el)
          (cond
            ((= (setq eclr (rtos (abs (haws-dxf 62 el)) 2 0)) "0")
             "BYBLOCK"
            )
            ((= eclr "256") "BYLAYER")
            (t eclr)
          )
          "BYLAYER"
        )
       laclr
        (rtos
          (abs
            (haws-dxf
              62
              (tblsearch "LAYER" (setq lai (haws-dxf 8 el)))
            )
          )
          2
          0
        )
       alrtstr
        (strcat
          "\n" etype " on layer " lai " selected.\nIts color is " eclr
          ".  The color of its layer is " laclr ".\nIt is "
         )
       i -1
     )
     (if shells
       (while (setq shelli (nth (setq i (1+ i)) shells))
         (setq
           shelll
            (entget shelli)
           alrtstri
            (strcat
              "in a block (or xref) called "
              (setq
                temp
                 (cdr
                   (assoc
                     (if
                       (= "INSERT" (cdr (assoc 0 shelll)))
                        2
                        0
                     )
                     shelll
                   )
                 )
              )
              " which is on layer "
              (cdr (assoc 8 shelll))
              "\n"
            )
         )
         (if (/= previ alrtstri)
           (setq alrtstr (strcat alrtstr alrtstri))
         )
         (setq previ alrtstri)
       )
     )
     (setq alrtstr (strcat alrtstr "of the current drawing."))
     (list alrtstr (cons (car es) shells))
    )
    (t (list "" nil))
  )
)

;;; Returns list with a leading comma
(defun haws-xlist-list-to-string
   (enamelist edit-mode / donelist  ename  layerliststring)
  (setq layerliststring "")
  (cond ((/= edit-mode 2) (setq enamelist (cadr enamelist))))
  (while (setq ename (car enamelist))
    (setq enamelist (cdr enamelist))
    (cond
      ((not (member ename donelist))
       (setq donelist (cons ename donelist))
       (setq
         layerliststring
          (strcat
            layerliststring
            ","
            (cdr (assoc 8 (entget ename)))
          )
       )
      )
    )
  )
  layerliststring
)

(defun haws-xlist-show-layers (layerliststring)
  (vl-cmdf
    "._LAYER"
    "_FILTER"
    "_DELETE"
    "HAWS-XLIST"
    ""
    "._LAYER"
    "_FILTER"
    "_NEW"
    "_GROUP"
    ""
    (substr layerliststring 2)
    "HAWS-XLIST"
    "_EXIT"
    ""
    "LAYERPALETTE"
  )
)

 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|
