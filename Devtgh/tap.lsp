;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-TAP (/ stubpt mainln taplin tmpsew)
  (HAWS-ERDF$@ 0)
  (setvar "cmdecho" 0)
  (HAWS-VSAVE '("osmode" "clayer"))
  (setq
    osuser (getvar "osmode")
    ts (* (HAWS-DWGSCALE)(getvar "dimtxt"))
  )
  (if (= 0 (getvar "chamfera"))
    (progn(setvar "chamfera" 3.0)(setvar "chamferb" 3.0))
  )
  (princ "\nStarting SEWTAP using current chamfer distances:  ")
  (princ (getvar "chamfera"))(princ " and ")
  (princ (getvar "chamferb"))(princ ".")
  (princ "\nUse the chamfer command to change.")
  ;;;Put sewer taps on    sewtap layer,33 color.
  (while (setq stubpt (getpoint "\nEnd of sewer tap (Return to quit):"))
    (HAWS-MKLAYR "SEWTAP")
    (setq mainln (entsel "\nSewer main downstream of connection point:"))
    (command "line" stubpt "perp")
    (command (cadr mainln))
    (setq
      taplin (entlast)
      ang1 (angle (trans (cdr (assoc 11 (entget taplin)))taplin 1) stubpt)
      left (minusp (cos (- ang1 0.7854)))
    )
    (command "nea" (cadr mainln) "")
    (setq tmpsew (entlast))
    (command "chamfer" (list taplin stubpt) (list tmpsew (cadr mainln)))
    (command "erase" tmpsew "")
    (setvar "osmode" osuser)
    (HAWS-MKLAYR "SEWTAPTX")
    (HAWS-MKTEXT
      (if left "MR" "ML")
      (polar stubpt ang1 ts)
      ts
      (if left (+ ang1 pi) ang1)
      "HC"
    )
  )
  (command "redraw")
  (HAWS-VRSTOR)(HAWS-ERRRST)
  (princ)
)
