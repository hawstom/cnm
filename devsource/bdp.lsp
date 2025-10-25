;Label bearing and distance between two picks
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-bdp (/ txht lline pt10 pt11 lbear ldist pt1 pt2 pt3 pt4 pt5 rot ltop)
  (haws-core-init 164)
  (haws-vsave '("lunits" "aunits" "auprec" "ucsfollow"))
  (haws-vset '(("lunits" 2)("aunits" 4)("auprec" 4)("ucsfollow" 0)))
  (vl-cmdf "._ucs" "_w")
  (setq
    pt1 (getpoint "\nFirst point: ")
    pt2 (getpoint pt1 "\nSecond point: ")
    ucsp t
  )
  (setq
    txht (* (haws-dwgscale)(getvar "dimtxt"))
    lbear (haws-rtob (angle pt1 pt2) 4)        ldist (strcat(rtos (distance pt1 pt2))"'")
    pt3
    (list
      (/ (+ (car pt1)(car pt2)) 2)
      (/ (+ (cadr pt1)(cadr pt2)) 2)
      (/ (+ (caddr pt1)(caddr pt2)) 2)
    )
    rot (angle pt1 pt2)
    pt4(polar pt3 (+ rot (/ pi 2))(* txht 0.667))
    pt5(polar pt3 (- rot (/ pi 2))(* txht 0.667))
  )
  (if (= (setq ltop (getstring 1 "Enter text or <Use line data>: ")) "")
    (progn(haws-mktext "c" pt4 nil rot lbear)(haws-mktext "tc" pt5 nil rot ldist))
    (progn
      (haws-mktext "c" pt4 nil rot ltop)
      (vl-cmdf "._dtext" "_j" "_tc" pt5 txht (angtos rot))
  ) )
  (vl-cmdf "._ucs" "_p")
  (setq ucsp nil)
  (haws-vrstor)(haws-core-restore)(princ)
)
