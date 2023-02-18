(defun c:haws-adl () (haws-adl 1 "LF"))
(defun haws-adl (factor label / e ss len i)
(haws-core-init 0)
  (setq len 0.0 ss (ssget '((0 . "LINE,SPLINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE"))))
  (foreach e (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))
    (setq len (+ len (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))))
  )
  (SETQ
    len (STRCAT 
          (RTOS (* len FACTOR) 2 (GETVAR "luprec"))
          " "
          LABEL
        )
    TXPT (GETPOINT "\nMiddle point for text:")
    TS   (* (GETVAR "dimscale") (GETVAR "dimtxt"))
  )
  (IF TXPT
    (HAWS-MKTEXT "m" TXPT nil 0 len)
  )
  (PRINC (STRCAT "\n" len)) 
  (princ)
); end program