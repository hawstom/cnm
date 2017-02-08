;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-chattrib ( / a b c d z)
  (HAWS-ERDF$@ 0)
  (HAWS-VSAVE '("cmdecho"))
  (setq
    blname (HAWS-GETSTRINGX "Block name specification" blname "*")
    attag  (HAWS-GETSTRINGX "Attribute tag specification" attag "*")
    atval  (HAWS-GETSTRINGX "Attribute value specification" atval "*")
  )
  (command "._zoom" "extents" "._attedit")
  (setq viewp T)
  (command "y" blname attag atval)
  (command "w")
  (setvar "cmdecho" 0)
  (command (getvar"extmin")(getvar "extmax"))
  (initget "Value Position Height Angle Style Layer Color")
  (setq a(getkword "\nValue/Position/Height/Angle/Style/Layer/Color: "))
  (cond
    ( (= a "Value")
      (command a)
      (initget "Change Replace")
      (setq b (getkword "Change or Replace <R>: "))
      (if (not b) (setq b "Replace"))
      (cond
        ( (= b "Change")(setq c (getstring "\nOld string: "))
          (command b)
          (setvar "cmdecho" 1)
          (command c)
        )
        ( T (setvar "cmdecho" 1)(command b))
      )
    )
    ( T (setvar "cmdecho" 1)(command a))
  )
  (setq z (getstring))(command z)(command "n")
  (while (= 1 (logand (getvar "cmdactive" )1 ))
    (command a)(if b (command b))(if c (command c))(command z "n")
  )
  (command "zoom" "p")(setq viewp nil)
  (HAWS-VRSTOR)(HAWS-ERRRST)
)
