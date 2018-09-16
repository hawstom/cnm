;(C) Copyright 1998 by Thomas Gail Haws
(defun c:haws-tapinv ( /  dmain dtap emain invmh invtap invtxt mh
  tapptf tapptn)
  (defun HAWS-tiset ()
    (HAWS-VSAVE '("luprec"))
    (setvar "luprec" 5)
    (setq  ;Set four global variables
      timdia (HAWS-GETINTX "\nMain diameter in inches" timdia 8)
      tismain (HAWS-GETREALX "\nSlope of main sewer line up from manhole" tismain 0.0033)
      tishc (HAWS-GETREALX "\nSlope of house connection up from main line crown" tishc 0.0104)
    )
    (HAWS-VRSTOR)
    timdia
  )
  (haws-core-init 313)
  (graphscr)
  (while
    (cond
      ( (progn
          (setvar "osmode" 4)
          (initget "Setup")
          (or
            (not timdia)
            (not tismain)
            (not tishc)
            (=(setq mh(getpoint"\nSelect downstream manhole or [Setup]: "))"Setup")
          )
        )
        (HAWS-tiset)
      )
      ( mh
        (setq
          invmh (HAWS-GETREALX "\nInvert of mainline at downstream manhole" tiinvmh 0.0)
          emain (entsel "\nSewer main line: ")
        )
        (setvar "osmode" 1)
        (while (setq tapptf (getpoint "\nSelect sewer tap end:"))
          (command "._line" tapptf "perp" (cadr emain) "")
          (setq
            tapptn (trans(cdr (assoc 11 (entget (entlast)))) (entlast) 1)
            dmain (distance mh tapptn)  dtap (distance tapptn tapptf)
            invtap (+ invmh (/ timdia 12.0) (* dmain tismain) (* dtap tishc))
            invtxt (entsel "\nSelect text to replace with calculated invert:")
          )
          (entdel (entlast))
          (entmod
            (subst
              (cons 1 (rtos invtap 2))
              (assoc 1 (entget (car invtxt)))
              (entget(car invtxt))
            )
          )
        )
      )
      ( T nil)
    )
  )
  (haws-core-restore)
  (princ)
)
