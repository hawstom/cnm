;CONSECUTIVE NUMBERING LISP
;MODIFIED 8/21/95 TO ADD JUSTIFICATION OPTION. Thomas Gail Haws
(defun c:haws-num (/ i just rot PT1 TH)
(haws-core-init 269)
  (setvar "cmdecho" 0)
  (setq
    i (getint "\nFirst number of series: ")
    rot (getreal "\nText rotation <0>: ")
    just (progn
        (initget "C M R TL TC TR ML MC MR BL BC BR")
        (getkword "\nJustification [L/C/M/R/TL/TC/ML/MC/MR/BL/BC/BR] <Left>: ")
      )
  )
  (if (= rot nil)
    (setq rot 0)
  )
  (setq pt1 (getpoint "\nFirst insertion point: "))
  (HAWS-MKTEXT just pt1 nil rot (itoa i))
  (while (setq pt1 (getpoint pt1 "\nNext insertion point: "))
    (setq i (+ i 1))
    (HAWS-MKTEXT just pt1 nil rot (itoa i))
  )
)
