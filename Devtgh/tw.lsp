;; TW 1.0 by Matthew Matkin
;; This routine performs a dview twist of a user specified
;; number of degrees.
;; Modified by Thomas Gail Haws

(DEFUN c:haws-TW (/ TWIST)
  (haws-core-init 318)
  (HAWS-VSAVE '("AUNITS"))
  (SETQ UCSP T)
  (vl-cmdf "._UCS" "_WORLD")
  (SETQ TWIST (GETANGLE "\nEnter angle to make horizontal: "))
  (SETVAR"AUNITS"3)
  (vl-cmdf "._DVIEW" "" "_TWIST" (* -1 TWIST) "")
  (vl-cmdf "._UCS" "_PREVIOUS")
  (HAWS-VRSTOR)
  (haws-core-restore)
  (PRINC)
)
