(DEFUN c:haws-AEE ()
(haws-core-init 155)
  (PROMPT "\nSelect object: ")
  (vl-cmdf "._AREA" "_E" pause)
  (PRINC "Area=")
  (PRINC (GETVAR "AREA"))
  (PRINC "  Length=")
  (PRINC (GETVAR "PERIMETER"))
  (PRINC "  Area in acres=")
  (PRINC (/ (GETVAR "AREA") 43560))
  (PRINC)
)
