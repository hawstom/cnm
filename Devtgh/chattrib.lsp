;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-chattrib ( / a b c d z)
  (haws-core-init 169)
  (HAWS-VSAVE '("cmdecho"))
  (setq
    blname (HAWS-GETSTRINGX "Block name specification" blname "*")
    attag  (HAWS-GETSTRINGX "Attribute tag specification" attag "*")
    atval  (HAWS-GETSTRINGX "Attribute value specification" atval "*")
  )
  (vl-cmdf "._zoom" "_extents" "._attedit")
  (setq viewp T)
  (vl-cmdf "_y" blname attag atval)
  (vl-cmdf "_w")
  (setvar "cmdecho" 0)
  (vl-cmdf (getvar"extmin")(getvar "extmax"))
  (initget "Value Position Height Angle Style Layer Color")
  (setq a(getkword "\nValue/Position/Height/Angle/Style/Layer/Color: "))
  (cond
    ( (= a "Value")
      (vl-cmdf (strcat "_" a))
      (initget "Change Replace")
      (setq b (getkword "[Change/Replace] <R>: "))
      (if (not b) (setq b "Replace"))
      (cond
        ( (= b "Change")(setq c (getstring "\nOld string: "))
          (vl-cmdf (strcat "_" b))
          (setvar "cmdecho" 1)
          (vl-cmdf c)
        )
        ( T (setvar "cmdecho" 1)(vl-cmdf (strcat "_" b)))
      )
    )
    ( T (setvar "cmdecho" 1)(vl-cmdf (strcat "_" a)))
  )
  (setq z (getstring))(vl-cmdf z)(vl-cmdf "_n")
  (while (= 1 (logand (getvar "cmdactive" )1 ))
    (vl-cmdf (strcat "_" a))(if b (vl-cmdf (strcat "_" b)))(if c (vl-cmdf c))(vl-cmdf z "_n")
  )
  (vl-cmdf "._zoom" "_p")(setq viewp nil)
  (HAWS-VRSTOR)(haws-core-restore)
)
