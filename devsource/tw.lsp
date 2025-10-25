;; TW 1.0 by Matthew Matkin
;; This routine performs a dview twist of a user specified
;; number of degrees.
;; Modified by Thomas Gail Haws

(defun c:haws-tw (/ twist)
  (haws-core-init 318)
  (haws-vsave '("AUNITS"))
  (setq ucsp t)
  (vl-cmdf "._UCS" "_WORLD")
  (setq twist (getangle "\nEnter angle to make horizontal: "))
  (setvar"AUNITS"3)
  (vl-cmdf "._DVIEW" "" "_TWIST" (* -1 twist) "")
  (vl-cmdf "._UCS" "_PREVIOUS")
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)
