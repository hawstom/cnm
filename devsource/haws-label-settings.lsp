;;; Configuration settings for haws-label.lsp
;;;
;;;
;; Notes: Layer and key names must be upper case. The script reads the LAYER_NAME entires top-down and will use the last entry that matches the selected object. 
;; Less specific LAYER_NAMES should be listed before more specific LAYER_NAMES if there are overlapping categories (e.g. WTR-6IN above WTR-6IN-DIP)
("READ-BIAS-DEGREES" 140)
; Table 1: TEXT_STYLE
;; TEXT-STYLE_KEY: The name of your defined label style
;; LAYER_NAME: The layer the mtext will be placed on
;; TEXT_STYLE_NAME: The name of the C3D font style the mtext will use
; 				TEXT_STYLE_KEY    LAYER_NAME   TEXT_STYLE_NAME
("TEXT-STYLE"       "EX"         "TOPO-TEXT-LABELS"   "SEI-3D"       )
("TEXT-STYLE"       "PROP"       "NOTES"          "SEI-3D"         )
("TEXT-STYLE"       "PROP-WTR"       "WTR-TEXT"          "SEI-3D"         )
("TEXT-STYLE"       "PROP-SWR"       "SWR-TEXT"          "SEI-3D"         )
("TEXT-STYLE"       "PROP-SD"       "SD-TEXT"          "SEI-3D"         )
;; Table 2: 
;; LAYER; Used by program
;; LAYER_NAME: The layer that is pulled from C3D.
;; TEXT_STYLE_KEY: The TEXT_STYLE that was defined above and the mtext will use.
;;             "LAYER_NAME"       "TEXT_STYLE_KEY"   "LABEL_TEXT"
;; Use '#' to indicate the number found in the layer name will be inserted (e.g. "#\"W" on amr6-water|wtr-6in will display "6"W"). The function will drop any "#\" if the layer has no number
("LAYER"      "*LPS*"       "PROP-SWR"            "#\"S LPS"              )
("LAYER"      "*WTR*DIP*"   "PROP-WTR"            "#\"W DIP"              )
("LAYER"      "*WTR*PVC*"   "PROP-WTR"            "#\"W PVC"              )
("LAYER"      "*WTR*"       "PROP-WTR"            "#\"W"                  )
("LAYER"      "*SWR*VCP*"   "PROP-SWR"            "#\"S VCP"              )
("LAYER"      "*SWR*"       "PROP-SWR"            "#\"S"                  )
("LAYER"      "*SD*"        "PROP-SD"             "#\"SD"                 )
("LAYER"      "*EX*SWR*"    "EX"                  "#\"s"                  )
("LAYER"      "*EX*WTR*"    "EX"                  "#\"w"                  )
("LAYER"      "*EX*SD*"     "EX"                  "#\"sd"                 )
("LAYER"      "*EX*GAS*"     "EX"                  "#\"g"                 )
("LAYER"      "*EX*FM*"     "EX"                  "#\"fm"                 )
;;                                 !!! You MUST use \" for " !!!