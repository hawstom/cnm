;Leaders with special heads
;Written by Craig Hendricks and Thomas Gail Haws
(defun c:haws-loop ()
(haws-core-init 242) (vl-cmdf "._insert" "dimloop")(vl-cmdf)(HAWS-leader "dimloop"))
(defun c:haws-tilde ()
(haws-core-init 243) (HAWS-leader "Integral"))
(defun c:haws-dot ()
(haws-core-init 244) (HAWS-leader "DotSmall"))
(defun c:haws-none ()(HAWS-leader "None"))
(defun HAWS-leader (dimblk / dbold pt txt)
  (haws-core-init 245)
  (vl-cmdf "._Dim1" "_Dimldrblk" dimblk)
  (vl-cmdf "._Leader")
  (HAWS-VRSTOR)(haws-core-restore)(princ)
)
