;Leaders with special heads
;Written by Craig Hendricks and Thomas Gail Haws
(defun c:haws-loop () (command "._insert" "dimloop")(command)(HAWS-leader "dimloop"))
(defun c:haws-tilde () (HAWS-leader "Integral"))
(defun c:haws-dot () (HAWS-leader "DotSmall"))
(defun c:haws-none ()(HAWS-leader "None"))
(defun HAWS-leader (dimblk / dbold pt txt)
  (HAWS-ERDF$@ 0)
  (command "Dim1" "Dimldrblk" dimblk)
  (command "Leader")
  (HAWS-VRSTOR)(HAWS-ERRRST)(princ)
)
