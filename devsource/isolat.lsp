;Freeze, off, or lock all except picked layers
;Written by Thomas Gail Haws

(defun c:haws-ffi ()
(haws-core-init 226) (haws-isolat "_freeze" nil))
(defun haws-isolat (lopera nested / ss1 i en)
  (haws-core-init 76)
  (haws-vsave '("EXPERT"))
  (setvar "EXPERT" 5)
  (prompt "\nLayers to isolate")
  (setq issset (ssget))
  (vl-cmdf "._layer" lopera "*")
  (setq
    i 0
    lopera
    (cond
      ((= lopera "_off") "_on")
      ((= lopera "_freeze") "_thaw")
      ((= lopera "_lock") "_unlock")
  ) )
  (while
    (setq en (ssname issset i))
    (vl-cmdf lopera (cdr(assoc 8 (entget en))))
    (setq i (1+ i))
  )
  (vl-cmdf "")
  (haws-vrstor)(haws-core-restore)
);end freeze/off/lock isolate by picking
