(defun c:haws-mvhp ( / ds)
  (haws-core-borrow 0)
  (HAWS-VSAVE'("clayer"))
  (c:haws-0)
  (HAWS-MKLAYR '("defpoints" "" ""))
  (setq ds (HAWS-DWGSCALE))
  (command
    "._pspace"
    "._mview" "0.7,0.39" "35.8,23.61"
    "._mspace"
    "._zoom" "c"
    (list (* 18.25 ds) (* 12.0 ds))
    (strcat "1/" (rtos ds 2 0) "xp")
    "._pspace"
  )
  (prompt "\nHP plotter sized floating viewport created.\nCurrent UCS origin assumed to be at sheet corner.")
  (HAWS-VRSTOR)(haws-core-return)
)
