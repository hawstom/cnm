;Written by Thomas Gail Haws
(defun c:haws-10 ()
(haws-core-init 129)(setup 20.0 0.10))
(defun c:haws-12 ()
(haws-core-init 130)(setup 20.0 0.12))
(defun c:haws-setdim10 ()
(haws-core-init 131)(alert "Please note that the SETDIM10 command has been changed to \"10\"")(c:haws-10))
(defun c:haws-setdim12 ()
(haws-core-init 132)(alert "Please note that the SETDIM12 command has been changed to \"12\"")(c:haws-12))
(defun c:haws-setup ( / ds ts dtype)
(haws-core-init 133)
  (prompt "\nSETUP sets ltscale and the dimension variables dimasz, dimcen, dimdli, dimexe, dimexo, dimgap, dimscale, and dimtxt.")
  (setq
    ds (getreal "\nDrawing scale: 1\" = ")
    ts (getreal "\nText height in inches: ")
    dwgsetupdone t
  )
  (setup ds ts)
)

(defun setup (ds ts)
  (setup:vset
    (list
      (list "dimasz"   (* ts 1.0))
      (list "dimcen"   (* ts 0.5))
      (list "dimdli"   (* ts 2.0))
      (list "dimexe"   (* ts 0.5))
      (list "dimexo"   (* ts 0.5))
      (list "dimgap"   (* ts 0.25))
      (list "dimscale" ds)
      (list "dimtxt"   ts)
  ) )
  (if (or (and (= (getvar "tilemode") 0)(= (getvar "psltscale") 0))(and (= (getvar "tilemode") 1)(= (getvar "msltscale") 0))) (setvar "ltscale"  ds))
  (prompt"\nEnter DIMSTY to set up dimension styles.")
  (princ)
)
(defun setup:vset
            (vlst)
  (foreach
     v
      vlst
    (if (getvar (car v))
      (setvar (car v) (cadr v))
    ) ;_ end of if
  ) ;_ end of foreach
) ;_ end of defun

