;(C) Copyright 1997 by Thomas Gail Haws
;MOFFSET.LSP--Multiple offsets to different layers.
;Thomas Gail Haws, Feb. 1996
(defun c:haws-mof ()
(haws-core-init 259)(c:haws-moffset))
(defun c:haws-moffset ( / HAWS-moset)
  (defun HAWS-moset ( / i offi toti p1)
    (setq i 1 oflist nil
      toti (getint "\nNumber of offsets <return to read list from saved drawing text>:")
    )
    (cond
      ( toti
        (repeat toti
          (setq offi(list(getreal(strcat "\nDistance for offset " (itoa i) ": "))
              (getstring (strcat "\nLayer for offset " (itoa i) ": "))
          )         )
          (setq oflist (if oflist (cons offi oflist) (list offi)))
          (setq i (1+ i))
        )
      )
      ( T (setq oflist (read(cdr(assoc 1(entget(car(entsel"\nSelect text:"))))))))
    )
    (princ "\nList of offsets:  ")(prin1 oflist)
    (HAWS-mo)
  )
  (defun HAWS-mo ( / p1 p2 offi disti layi)
    (cond
      ( (progn
          (initget "Setup")
          (and
            oflist
            (setq p1 (entsel"\nEntity to offset or [Setup]: "))
            (/= "Setup" p1)
          )
        )
        (setq p2(getpoint "\nSide to offset? "))
        (setq offi oflist)
        (while (car offi)
          (setq disti (caar offi) layi (cadar offi))
          (if (not(tblsearch"LAYER"layi))(command"layer""n"layi""))
          (command "offset" disti p1 p2 "" "change" "last" "" "p" "la" layi "")
          (setq offi (cdr offi))
        )
        T
      )
      ((or (not oflist)(= "Setup" p1))(HAWS-moset))
      (T nil)
    )
  )
  (haws-core-init 260)
  (while(HAWS-mo))
  (haws-core-restore)
)
