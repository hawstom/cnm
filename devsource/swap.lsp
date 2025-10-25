;SWAP TWO THINGS
(defun c:haws-swap (/ p ss2 pt1 pt2)
(haws-core-init 138)
  (princ "\nFirst swap-set ")
  (setq p (ssget)
    ss2 (progn(princ "\nSecondswap-set ")(ssget))
    pt1 (getpoint "Base point of displacement: ")
  pt2 (getpoint pt1 "Second point of displacement: "))
  (vl-cmdf ".move" p "" pt1 pt2 ".move" ss2 "" pt2 pt1" .redraw")
); end swap.lsp
