;Freeze, off, or lock all except picked layers
;Written by Thomas Gail Haws

(defun c:haws-OFI () (HAWS-isolat "off" nil))
(defun c:haws-FFI () (HAWS-isolat "freeze" nil))
(defun c:haws-LKI () (HAWS-isolat "lock" nil))
(defun HAWS-isolat (lopera nested / ss1 i en)
  (HAWS-ERDF$@ 1)
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
  (HAWS-VRSTOR)(HAWS-ERRRST)
);end freeze/off/lock isolate by picking
