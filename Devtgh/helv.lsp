;Written by Thomas Gail Haws
(DEFUN c:haws-HELV ( / st sn sf)
  (HAWS-ERDF$@ 0)
  (HAWS-VSAVE '("cmddia" "filedia" "regenmode"))
  (command "style" "X-HLVM1S" "X-HLVM1S" "0" "1" "" "" "")
  (setvar "cmddia" 0)
  (setvar "filedia" 0)
  (setvar "regenmode" 0)
  (while (setq st (tblnext "STYLE" (not st)))
    (setq sn (strcase(cdr (assoc 2 st))) sf (strcase(cdr (assoc 3 st))))
    (cond
      ( (and;Don't helveticize the following cases
          (/= (logand(cdr(assoc 70 st)) 16)  16)
          (not (wcmatch sf "*ROM*,SAS*"));Standard Acad font files
          (not (wcmatch sn "*C*"));Italics and others
          (or
            (wcmatch sn "*HEL*,*HLV*,*BOLD*")
            (=
              (progn
                (initget "Yes No")
                (getkword
                  (strcat "\nChange font file for "sn" to X-HLVM1S.SHX?<No>: ")
              ) )
              "Yes"
        ) ) )
        (command "._style" sn "X-HLVM1S" "" "" "" "" "")
  ) ) )
  (setvar "textstyle" "X-HLVM1S")
  (HAWS-VRSTOR)(HAWS-ERRRST)(princ)
)
