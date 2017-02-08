;CONSECUTIVE LETTERING LISP
;10/04/95  Thomas Gail Haws
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-letter (/ a b c d e f g)
  (setvar "cmdecho" 0)
  (setq
    a (HAWS-getlet "\nFirst letter of series (between a and zy):")
    b (HAWS-getlet "\nLast letter of series (between b and zz):")
    c (getreal "\nText height: ")
    d (getreal "\nText rotation <0>: ")
    e (progn
        (initget "C M R TL TC TR ML MC MR BL BC BR")
        (getkword "\nJustification C/M/R/TL/TC/TR/ML/MC/MR/BL/BC/BR <Left>: ")
      )
  )
  (if (= d nil)
    (setq d 0)
  )
  (while (<= a b)
    (setq f (getpoint "\nLocation of letter: "))
    (setq g
      (strcat
        (if (= 0 (fix(/ (1- a) 26))) "" (chr(+(fix(/ (1- a) 26))64)) )
        (chr(if (= (rem a 26) 0) 90 (+(rem a 26)64)))
      )
    )
    (command "._text")
    (if e (command "j" e))
    (command f c d g)
    (setq a (+ a 1))
  )
)
;Function getlet prompts for a letter code between a and zz.
;returns an integer code for the letter entered (1 for A, 26 for Z, 28 for AB)
(defun HAWS-getlet (prmpt / letter)
  (while (< 2 (strlen (setq letter (strcase(getstring prmpt)))))
    (prompt "\nLimit 2 characters.  Try again.")
  )
  (if (= (strlen letter) 1)
    (- (ascii letter) 64)
    (+
      (* (- (ascii letter) 64) 26)
      (- (ascii (substr letter 2)) 64)
    )
  )
)
