;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-dm12 ( / pt1 pt2 ang upsdwn d1 pt3 txht)
  (haws-core-init 311)
  (haws-vsave '("osmode" "clayer"))
  (haws-vset'(("dimse1"1)("dimse2"1)("dimsoxd"1)("dimtad"1)("dimtih"0)("dimtix" 1)("dimtofl"1)("osmode" 512)))
  (princ "\nDM just set the following dimension variables:\nDIMSE1  1\nDIMSE2  1\nDIMSOXD 1\nDIMTAD  1\nDIMTIH  0\nDIMTIX  1\nDIMTOFL 1")
  (while
    (=
      (progn
        (initget "Set")
        (setq pt1 (getpoint "\nEntity to dimension or [Set dimension layer]: "))
      )
      "Set"
    )
    (initget "Stdim Dim Xdim Current")
    (setq dimlay (getkword "Stdim layer/Dim/Xdim/Current layer: "))
  )
  (cond
    ( (= dimlay "Stdim")(haws-setlayr "ST-DIM"))
    ( (= dimlay "Dim")(haws-setlayr "DIM"))
    ( (= dimlay "Xdim")(haws-setlayr "XDIM"))
  )
  (setvar "osmode" 128)
  (setq
    pt2 (getpoint pt1 "\nEntity to dimension from: ")
    ang (angle pt1 pt2)
    upsdwn (minusp (cos ang))
    d1 (/ (distance pt1 pt2) 2)
    txht (* (haws-dwgscale)(getvar "dimtxt"))
    pt3 (polar (polar pt1 ang d1) (+ (/ pi (if upsdwn 2 -2)) ang) txht)
    ang (+ ang (if upsdwn pi 0))
  )
  (setvar "osmode" 0)
  (prompt "\nAdditional text: ")
  (vl-cmdf
    "._dim" "al" pt1 pt2 pt1 "" "e"
    "._dtext" "j" "m" pt3 txht (angtos ang)
  )
  (prompt (strcat
    "\nOutside dimension arrows have been suppressed."
    "\nIf dim is short and you want arrows back, use DIMSOXD 0 to allow arrows outside."
  ) )
  (haws-vrstor)(haws-core-restore)(princ)
)
