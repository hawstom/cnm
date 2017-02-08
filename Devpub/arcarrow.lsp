;This routine, copied from the Dec 88 issue of CADalyst magazine by
;LeRoy Cordes of the Greater Chicago AutoCAD Users' Group is supposed
;to create a curved or straight polyline leader line.
;In CADalyst it was called "ARROW", but to avoid confusion I renamed it
;"ARCARROW"
;9-11-96 gm I renamed this to "aar" to ease typing

(defun c:haws-aar (/ sp ang arrang leng ep)
  (HAWS-VSET '(("plinegen" 0)("orthomode" 0)("osmode" 0)))
  (setq sp (getpoint  "\n Arrow start point:")
    angp (getpoint sp "\n Arrow angle:")
    arrang (angle sp angp)
    as (* (getvar "dimasz")(getvar "dimscale"))
    ep (polar sp arrang as)
  )
  (command "pline" sp "w" "0.0" (/ as 3.0) ep "w" "0.0" "0.0" "arc")
)
