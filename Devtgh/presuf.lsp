;(C) Copyright 1997 by Thomas Gail Haws
;Thomas Gail Haws, Feb. 1996
(defun c:haws-PRESUF (/ a n i e presuf frag string)
(haws-core-init 272)
  (setq a (ssget) n (sslength a) i 0)
  (initget 1 "Prefix Suffix")
  (setq presuf (getkword "Specify what to add [Prefix/Suffix]:"))
  (initget 1)
  (setq frag (getstring T "\nEnter string to add:"))
  (while (setq e (ssname a i))
    (cond ((wcmatch (cdr (assoc 0 (setq e (entget e)))) "*TEXT")
        (if (= presuf "Prefix") (setq string (strcat frag (cdr (assoc 1 e))))
        (setq string (strcat (cdr (assoc 1 e)) frag)))
        (setq e(subst(cons '1 string)(assoc 1 e)e))
        (entmod e)
    ))
    (setq i (1+ i))
    (princ)
  )
)
