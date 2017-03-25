;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-dm ( / addtxt dimstr pt1 pt2)
  (haws-borrow 0)
  (HAWS-VSAVE '("osmode" "clayer"))
  (HAWS-VSET'(("osmode" 512)))
  (while
    (=
      (progn
        (initget "Set")
        (setq pt1 (getpoint "\nSet dimension layer/<Entity to dimension>: "))
      )
      "Set"
    )
    (initget "Stdim Dim Xdim Current")
    (setq *HAWS-dimlay* (getkword "Stdim layer/Dim/Xdim/Current layer: "))
  )
  (cond
    ( (= *HAWS-dimlay* "Stdim")(HAWS-MKLAYR "ST-DIM"))
    ( (= *HAWS-dimlay* "Dim")(HAWS-MKLAYR "DIM"))
    ( (= *HAWS-dimlay* "Xdim")(HAWS-MKLAYR "XDIM"))
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
      ( T (setq dimstr (strcat dimstr "\\P" addtxt)))
    )
  )
  (command "._dim1" "al" pt1 pt2 pt1 dimstr)
  (HAWS-VRSTOR)(haws-return)(princ)
)
