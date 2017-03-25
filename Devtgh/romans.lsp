;(C) Copyright 1997 by Thomas Gail Haws
(DEFUN c:haws-ROMANS ( / st sn sf)
  (haws-core-borrow 0)
  (HAWS-VSAVE '("cmddia" "filedia" "regenmode"))
  (command "style" "romans" "romans" "0" ".95" "0" "" "" "")
  (command "dim1" "style" "romans");For R13 compatibility
  (setvar "dimtxsty" "romans")
  (setvar "cmddia" 0)
  (setvar "filedia" 0)
  (setvar "regenmode" 0)
  (while (setq st (tblnext "STYLE" (not st)))
    (setq sn (strcase(cdr (assoc 2 st))) sf (strcase(cdr (assoc 3 st))))
    (cond
      ( (and;Don't romanize the following cases
          (/= (logand(cdr(assoc 70 st)) 16)  16)
          (not (wcmatch sf "*ROM*,SAS*"));Standard Acad font files
          (not (wcmatch sf "*HLV*,*HEL*,"));Permissible 3rd party files
          (not (wcmatch sf "*TTF"));True type font files
          (not (wcmatch sn "*C*"));Italics and others
          (or
            (wcmatch sn "L#*,LER*")
            (wcmatch sf "TXT")
            (= sn "STANDARD")
            (=
              (progn
                (initget "Yes No")
                (getkword
                  (strcat "\nChange font file for "sn" to ROMANS.SHX?<No>: ")
              ) )
              "Yes"
        ) ) )
        (command "._style" sn "ROMANS" "" "0.95" "" "" "" "")
  ) ) )
  (setvar "textstyle" "ROMANS")
  (HAWS-VRSTOR)(haws-core-return)(princ)
)
