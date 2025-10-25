;What Layer?
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-wl (/ e la lalst lt ltla clr clrla status)
(haws-core-init 329)
  (setq e(entget(car(entsel"\nPick entity on the Layer you want Listed: ")))
    lalst (tblsearch "LAYER" (setq la (haws-dxf 8 e)))
    lt (if(haws-dxf 6 e)(haws-dxf 6 e)"BYLAYER")
    ltla (haws-dxf 6 lalst)
    clr (if(haws-dxf 62 e)(rtos(abs(haws-dxf 62 e))2 0) "BYLAYER")
    clrla (rtos(abs(haws-dxf 62 lalst))2 0)
    status (if(= 4 (logand 4 (haws-dxf 70 lalst))) "Locked" "Unlocked")
  )
  (alert(strcat "Entity Type=" (haws-dxf 0 e) "\nLayer=" la
      "\nLinetype=" lt "\nLayer linetype=" ltla
      "\nColor=" clr "\nLayer color=" clrla
      "\nLayer is " status
  ))
  (princ)
)
