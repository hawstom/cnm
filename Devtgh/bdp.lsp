;Label bearing and distance between two picks
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-bdp (/ txht lline pt10 pt11 lbear ldist pt1 pt2 pt3 pt4 PT5 rot ltop)
  (HAWS-ERDF$@ 0)
  (HAWS-VSAVE '("lunits" "aunits" "auprec" "ucsfollow"))
  (HAWS-VSET '(("lunits" 2)("aunits" 4)("auprec" 4)("ucsfollow" 0)))
  (command "._ucs" "w")
  (setq
    pt1 (getpoint "\nFirst point: ")
    pt2 (getpoint pt1 "\nSecond point: ")
    ucsp T
  )
  (setq
    txht (* (HAWS-DWGSCALE)(getvar "dimtxt"))
    lbear (HAWS-RTOB (angle pt1 pt2) 4)        ldist (strcat(rtos (distance pt1 pt2))"'")
    pt3
    (list
      (/ (+ (car pt1)(car pt2)) 2)
      (/ (+ (cadr pt1)(cadr pt2)) 2)
      (/ (+ (caddr pt1)(caddr pt2)) 2)
    )
    rot (angle pt1 pt2)
    pt4(polar pt3 (+ rot (/ PI 2))(* txht 0.667))
    pt5(polar pt3 (- rot (/ PI 2))(* txht 0.667))
  )
  (if (= (setq ltop (getstring 1 "Enter text or return for line data: ")) "")
    (progn(HAWS-MKTEXT "c" pt4 txht rot lbear)(HAWS-MKTEXT "tc" pt5 txht rot ldist))
    (progn
      (HAWS-MKTEXT "c" pt4 txht rot ltop)
      (command "._dtext" "j" "tc" pt5 txht (angtos rot))
  ) )
  (command "._ucs" "p")
  (setq ucsp nil)
  (HAWS-VRSTOR)(HAWS-ERRRST)(princ)
)
