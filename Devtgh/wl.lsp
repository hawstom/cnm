;What Layer?
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-WL (/ e la lalst lt ltla clr clrla status)
  (setq e(entget(car(entsel"\nPick entity on the Layer you want Listed: ")))
    lalst (tblsearch "LAYER" (setq la (HAWS-DXF 8 e)))
    lt (if(HAWS-DXF 6 e)(HAWS-DXF 6 e)"BYLAYER")
    ltla (HAWS-DXF 6 lalst)
    clr (if(HAWS-DXF 62 e)(rtos(abs(HAWS-DXF 62 e))2 0) "BYLAYER")
    clrla (rtos(abs(HAWS-DXF 62 lalst))2 0)
    status (if(= 4 (logand 4 (HAWS-DXF 70 lalst))) "Locked" "Unlocked")
  )
  (alert(strcat "Entity Type=" (HAWS-DXF 0 e) "\nLayer=" la
      "\nLinetype=" lt "\nLayer linetype=" ltla
      "\nColor=" clr "\nLayer color=" clrla
      "\nLayer is " status
  ))
  (princ)
)
