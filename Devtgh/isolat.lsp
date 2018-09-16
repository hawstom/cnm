;Freeze, off, or lock all except picked layers
;Written by Thomas Gail Haws

(defun c:haws-FFI ()
(haws-core-init 226) (HAWS-isolat "freeze" nil))
(defun HAWS-isolat (lopera nested / ss1 i en)
  (haws-core-init 76)
  (HAWS-VSAVE '("EXPERT"))
  (SETVAR "EXPERT" 5)
  (prompt "\nLayers to isolate")
  (setq issset (ssget))
  (command "layer" lopera "*")
  (setq
    i 0
    lopera
    (cond
      ((= lopera "off") "on")
      ((= lopera "freeze") "thaw")
      ((= lopera "lock") "unlock")
  ) )
  (while
    (setq en (ssname issset i))
    (command lopera (cdr(assoc 8 (entget en))))
    (setq i (1+ i))
  )
  (command "")
  (HAWS-VRSTOR)(haws-core-restore)
);end freeze/off/lock isolate by picking
