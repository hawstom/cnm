;  This routine will change the size
;  of only the text.
;                 created by:
;                 David R. Williams
;TEXTSIZE.LSP
(defun c:haws-TH (/ ss1 new slen count ent entinfo)
  (setq ss1 (ssget '((0 . "TEXT"))))
  (while ss1
    (progn
      (initget (+ 1 2 4))
      (setq new (cons 40 (getreal "\nNew text height: ")))
      (setq slen (sslength ss1))
      (setq count 0)
      (while (< count slen)
        (setq ent (ssname ss1 count))
        (setq entinfo (entget ent))
        (entmod (subst new (assoc 40 entinfo) entinfo))
        (setq count (1+ count))
      )
    )
    (setq ss1 (ssget))
  )
  (print)
)
