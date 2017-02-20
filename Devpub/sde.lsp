;;--------------------------ONE LINERS-------------------------
(DEFUN c:haws-0 ()(SETVAR "TILEMODE" 0)(PRINC))
(DEFUN c:haws-1 ()(SETVAR "TILEMODE" 1)(PRINC))
(DEFUN c:haws-AA ()(COMMAND "SNAPANG" 0 "SNAPANG")(PRINC))
(DEFUN c:haws-ADT ()(HAWS-VTOG '("ATTDIA")))
(DEFUN c:haws-CET ()(HAWS-VTOG '("CMDECHO")))
(DEFUN c:haws-CMD ()(HAWS-VTOG '("CMDDIA")))
(DEFUN c:haws-DIA ()(HAWS-VTOG '("FILEDIA" "CMDDIA" "ATTDIA")))
(DEFUN c:haws-FDT ()(HAWS-VTOG '("FILEDIA")))
(DEFUN c:haws-QT ()(HAWS-VTOG '("QTEXTmoDE")))
(DEFUN c:haws-IB ()(COMMAND "INSBASE")(PRINC))
(DEFUN c:haws-IL ()(COMMAND "SNAP""S""I" "" "SNAP""OF" "ISOPLANE" "L")(PRINC))
(DEFUN c:haws-IO ()(COMMAND "SNAP""S""S" "" "SNAP""OF")(PRINC))
(DEFUN c:haws-IR ()(COMMAND "SNAP""S""I" "" "SNAP""OF" "ISOPLANE" "R")(PRINC))
(DEFUN c:haws-IT ()(COMMAND "SNAP""S""I" "" "SNAP""OF" "ISOPLANE" "T")(PRINC))
(DEFUN c:haws-LLT ( / EOLD)(SETQ EOLD (GETVAR "EXPERT"))(SETVAR "EXPERT" 5)(COMMAND "LINETYPE" "L" "*" "" "")(SETVAR "EXPERT" EOLD)(PRINC))
(DEFUN C:HAWS-MVL () (COMMAND "._mview" "lock" "on" PAUSE "")(PRINC))
(DEFUN C:HAWS-MVU () (COMMAND "._mview" "lock" "off" PAUSE "")(PRINC))
(DEFUN c:haws-OSE ()(COMMAND "OSNAP" "END")(PRINC))
(DEFUN c:haws-OSI ()(COMMAND "OSNAP" "INT")(PRINC))
(DEFUN c:haws-OSM ()(COMMAND "OSNAP" "MID")(PRINC))
(DEFUN c:haws-OSN ()(COMMAND "OSNAP" "NON")(PRINC))
(DEFUN c:haws-PSLT ()(HAWS-VTOG '("PSLTSCALE")))
(DEFUN c:haws-PROTO ()(COMMAND "INSERT" "*PROTO" "0,0" "" "")(PRINC))
(DEFUN c:haws-PROTOX ()(COMMAND "INSERT" "*PROTOX" "0,0" "" "")(PRINC))
(DEFUN c:haws-RGA ()(HAWS-VTOG '("REGENMODE")))
(DEFUN c:haws-UF ()(HAWS-VTOG '("UCSFOLLOW")))
(DEFUN c:haws-UF0 ()(PRINC "\nType UF to toggle UCSFOLLOW")(PRINC))
(DEFUN c:haws-UF1 ()(PRINC "\nType UF to toggle UCSFOLLOW")(PRINC))
(DEFUN c:haws-VSR ()(HAWS-VTOG '("VISRETAIN")))
;;--------------------------END ONE LINERS-------------------------