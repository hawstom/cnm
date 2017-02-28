;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-dm12 ( / pt1 pt2 ang upsdwn d1 pt3 txht)
  (haws-errdef 0)
  (HAWS-VSAVE '("osmode" "clayer"))
  (HAWS-VSET'(("dimse1"1)("dimse2"1)("dimsoxd"1)("dimtad"1)("dimtih"0)("dimtix" 1)("dimtofl"1)("osmode" 512)))
  (princ "\nDM just set the following dimension variables:\nDIMSE1  1\nDIMSE2  1\nDIMSOXD 1\nDIMTAD  1\nDIMTIH  0\nDIMTIX  1\nDIMTOFL 1")
  (while
    (=
      (progn
        (initget "Set")
        (setq pt1 (getpoint "\nSet dimension layer/<Entity to dimension>: "))
      )
      "Set"
    )
    (initget "Stdim Dim Xdim Current")
    (setq dimlay (getkword "Stdim layer/Dim/Xdim/Current layer: "))
  )
  (cond
    ( (= dimlay "Stdim")(HAWS-MKLAYR "ST-DIM"))
    ( (= dimlay "Dim")(HAWS-MKLAYR "DIM"))
    ( (= dimlay "Xdim")(HAWS-MKLAYR "XDIM"))
  )
  (setvar "osmode" 128)
  (setq
    pt2 (getpoint pt1 "\nEntity to dimension from: ")
    ang (angle pt1 pt2)
    upsdwn (minusp (cos ang))
    d1 (/ (distance pt1 pt2) 2)
    txht (* (HAWS-DWGSCALE)(getvar "dimtxt"))
    pt3 (polar (polar pt1 ang d1) (+ (/ pi (if upsdwn 2 -2)) ang) txht)
    ang (+ ang (if upsdwn pi 0))
  )
  (setvar "osmode" 0)
  (prompt "\nAdditional text: ")
  (command
    "._dim" "al" pt1 pt2 pt1 "" "e"
    "._dtext" "j" "m" pt3 txht (angtos ang)
  )
  (prompt (strcat
    "\nOutside dimension arrows have been suppressed."
    "\nIf dim is short and you want arrows back, use DIMSOXD 0 to allow arrows outside."
  ) )
  (HAWS-VRSTOR)(HAWS-ERRRST)(princ)
)
