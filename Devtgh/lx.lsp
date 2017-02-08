;(C) Copyright 2017 by Thomas Gail Haws
; This AutoLISP program lists a nested entity inside a XREF, BLOCK, or PLINE
(defun c:haws-lx () (haws-xlist 0))
(defun c:haws-lxx () (haws-xlist 1))
(defun c:haws-lxxx () (haws-xlist 2))
(defun haws-xlist (edit-mode / alrtstr alrtstri el es eclr laclr lai shells etype shelli shelll previ temp)
  (setq
    es (nentsel) shells (cadddr es) el (entget (car es)) etype (HAWS-DXF 0 el)
    eclr
    (if(HAWS-DXF 62 el)
      (cond
        ( (= (setq eclr (rtos(abs(HAWS-DXF 62 el))2 0)) "0") "BYBLOCK")
        ( (= eclr "256") "BYLAYER")
        ( T eclr)
      )
      "BYLAYER"
    )
    laclr (rtos(abs(HAWS-DXF 62 (tblsearch "LAYER" (setq lai (HAWS-DXF 8 el)))))2 0)
    alrtstr
    (strcat "\n" etype  " on layer " lai " selected.\nIts color is " eclr ".  The color of its layer is " laclr ".\nIt is ")
    i -1
  )
  (prompt alrtstr)
  (if shells
    (while (setq shelli (nth (setq i (1+ i)) shells))
      (setq
        shelll (entget shelli)
        alrtstri
        (strcat
          "in a block (or xref) called "
          (setq temp(cdr (assoc (if (= "INSERT" (cdr(assoc 0 shelll))) 2 0) shelll)))
          " which is on layer "
          (cdr (assoc 8 shelll))
          "\n"
      ) )
      (if (/= previ alrtstri)
        (setq
          alrtstr
          (strcat
            alrtstr
            alrtstri
      ) ) )
      (setq previ alrtstri)
      (prompt alrtstri)
  ) )
  (setq alrtstr (PRINC(strcat alrtstr "of the current drawing.")))
  (alert alrtstr)
  (cond ((> edit-mode 0) (haws-xlist-edit es shells edit-mode)))
  (PRINC)
)

(DEFUN
   HAWS-XLIST-EDIT (ES SHELLS EDIT-MODE / CEOLD)
  (SETQ LAYERLISTSTRING (CDR (ASSOC 8 (ENTGET (CAR ES)))))
  (COND
    ((= 2 EDIT-MODE)
     (FOREACH
        SHELLI SHELLS
       (SETQ
         LAYERLISTSTRING
          (STRCAT
            LAYERLISTSTRING
            ","
            (CDR (ASSOC 8 (ENTGET SHELLI)))
          )
       )
     )
    )
  )
  (COMMAND
    "._LAYER" "_FILTER" "_DELETE" "HAWS-LX-EDIT" "" "._LAYER" "_FILTER"
    "_NEW" "_GROUP" "" LAYERLISTSTRING "HAWS-LX-EDIT" "_EXIT" ""
    "LAYERPALETTE"
   )
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
