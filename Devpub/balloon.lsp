;BALLOON.LSP
(defun c:haws-BB (/ CR S TS SP EP ANG DIST CPL)
  (haws-errdef)
  (haws-vsave '("CMDECHO" "BLIPMODE" "ATTDIA" "CLAYER"))
  (haws-vset '(("orthomode"0)("cmdecho"0)("blipmode"0)("dragmode"2)("attdia"1)))
  (haws-mklayr "BB")
  (SETQ TS(*(HAWS-DWGSCALE)(GETVAR "DIMTXT"))  CR(* TS 1.2)
    SP(getpoint "\nArrow start point: ") EP(getpoint sp "\nLeader endpoint: ")
    ANG (angle SP EP)  DIST (distance SP EP)  CPL (polar SP ANG (+ CR DIST))
  )
  (cond
    ( (>= (atof(getvar "acadver"))14)(command "._leader" sp ep "" "" "n"))
    ( T (command "DIM1" "LEADER" SP EP)(COMMAND))
  )
  (COMMAND "CIRCLE" CPL CR)
  (setq s (getstring "\nNote number: "))
  (haws-mktext "M" CPL TS 0 S)
  (haws-vrstor)(haws-errrst)(princ)
);end
