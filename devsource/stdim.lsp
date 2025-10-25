;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-dm ( / addtxt dimstr pt1 pt2)
  (haws-core-init 310)
  (haws-vsave '("osmode" "clayer"))
  (haws-vset'(("osmode" 512)))
  (while
    (=
      (progn
        (initget "Set")
        (setq pt1 (getpoint "\nEntity to dimension or [Set dimension layer]: "))
      )
      "Set"
    )
    (initget "Stdim Dim Xdim Current")
    (setq *haws-dimlay* (getkword "Stdim layer/Dim/Xdim/Current layer: "))
  )
  (cond
    ( (= *haws-dimlay* "Stdim")(haws-mklayr "ST-DIM"))
    ( (= *haws-dimlay* "Dim")(haws-mklayr "DIM"))
    ( (= *haws-dimlay* "Xdim")(haws-mklayr "XDIM"))
  )
  (setvar "osmode" 128)
  (prompt"\nTip: Type DIMSTY to load dimension styles, then select STREET style before using DM.")
  (setq pt2 (getpoint pt1 "\nEntity to dimension from: ") dimstr "")
  (setvar "osmode" 0)
  (setq addtxt (getstring 1 "\nAdditional same line text: "))
  (if (/= "" addtxt) (setq dimstr (strcat dimstr "<> " addtxt)))
  (while (/= "" (setq addtxt (getstring 1 "\nAdditional text: ")))
    (cond
      ( (= dimstr "")(setq dimstr (strcat "<>\\X" addtxt)))
      ( (not (wcmatch dimstr "*\\X*")) (setq dimstr (strcat dimstr "\\X" addtxt)))
      ( t (setq dimstr (strcat dimstr "\\P" addtxt)))
    )
  )
  (vl-cmdf "._dim1" "_al" pt1 pt2 pt1 dimstr)
  (haws-vrstor)(haws-core-restore)(princ)
)
