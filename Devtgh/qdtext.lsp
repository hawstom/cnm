;QUICK DTEXT COMMANDS
;Written by Thomas Gail Haws
(defun HAWS-qdtext (ts / pt1)
  (setq pt1 (getpoint "\nStart point: "))
  (prompt "\nText: ")
  (command "._dtext" pt1 (* ts (getvar "dimscale")) "0")
)
(defun c:haws-l80  () (HAWS-qdtext 0.08 ))
(defun c:haws-l100 () (HAWS-qdtext 0.10 ))
(defun c:haws-l120 () (HAWS-qdtext 0.12 ))
(defun c:haws-l140 () (HAWS-qdtext 0.14 ))
(defun c:haws-l175 () (HAWS-qdtext 0.175))
(defun c:haws-l200 () (HAWS-qdtext 0.20 ))
(defun c:haws-l240 () (HAWS-qdtext 0.24 ))
(defun c:haws-l290 () (HAWS-qdtext 0.29 ))
(defun c:haws-l350 () (HAWS-qdtext 0.35 ))
(defun c:haws-l500 () (HAWS-qdtext 0.50 ))
;END QUICK DTEXT
