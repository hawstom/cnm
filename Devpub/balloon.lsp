;BALLOON.LSP
(defun c:haws-BB (/ CR S TS SP EP ANG DIST CPL)
  (erdf$@)
  (vsave '("CMDECHO" "BLIPMODE" "ATTDIA" "CLAYER"))
  (vset '(("orthomode"0)("cmdecho"0)("blipmode"0)("dragmode"2)("attdia"1)))
  (mklayr "BB")
  (SETQ TS(*(GETVAR "DIMSCALE")(GETVAR "DIMTXT"))  CR(* TS 1.2)
    SP(getpoint "\nArrow start point: ") EP(getpoint sp "\nLeader endpoint: ")
    ANG (angle SP EP)  DIST (distance SP EP)  CPL (polar SP ANG (+ CR DIST))
  )
  (cond
    ( (>= (atof(getvar "acadver"))14)(command "._leader" sp ep "" "" "n"))
    ( T (command "DIM1" "LEADER" SP EP)(COMMAND))
  )
  (COMMAND "CIRCLE" CPL CR)
  (setq s (getstring "\nNote number: "))
  (mktext "M" CPL TS 0 S)
  (vrstor)(errrst)(princ)
);end
