;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-istan ( / e1 e2 angdif dist1)
  (defun HAWS-getent ( / e)
    (setq e (nentsel "\nSelect Arc, Line, or Polyline: "))
    (if e
      (progn
        (redraw (car e) 3)
        (setq e(append (list(cdr(assoc 0(entget(car e)))))e))
  ) ) )
  (defun HAWS-chktyp ( / e)
    (while
      (or
        (not (setq e (HAWS-getent)))
        (not (or
            (= (car e) "ARC")
            (= (car e) "LINE")
            (= (car e) "VERTEX")
      ) ) )
      (prompt "That is not an Arc, Line, or Polyline.")
    )
    (setq e e)
  )
  (defun HAWS-jntang (e / en el pickpt enext p1 p2 d swap bulge1 ang1 r cenpt)
    (setq el (entget (setq en(cadr e))))
    (cond
      ( (=(car e)"ARC")
        (angle(trans(cdr(assoc 10 el))en 0)(trans(osnap (caddr e)"endp")1 0))
      )
      ( (=(car e)"LINE")
        (+ (/ pi 2)(angle (trans(cdr(assoc 10 el))en 0)(trans(cdr(assoc 11 el))en 0)))
      )
      ( (= (car e) "VERTEX")
        (setq
          pickpt (trans (caddr e) 1 0)
          enext (entnext en)
          p1 (trans (cdr (assoc 10 (entget en))) en 0)
          p2 (trans (cdr (assoc 10 (entget enext))) enext 0)
          d  (/ (distance p1 p2) 2)
          swap (if(>(distance pickpt p1)(distance pickpt p2))T nil)
        )
        (cond
          ( (/= 0 (setq bulge1 (cdr (assoc 42 el))))
            (setq
              ang1 (atan(/ 1 bulge1))
              r (/ d (sin (- pi (* 2 ang1))))
              cenpt (polar p1 (+ (angle p1 p2) (-(* 2 ang1)(/ pi 2))) r)
              ang1 (angle cenpt (if swap p2 p1))
            )
          )
          ( T
            (setq ang1 (+ (/ pi 2) (angle p1 p2)))
        ) )
  ) ) )
  (haws-errdef 0)
  (prompt "\nIs it Tangent?")
  (setq
    e1 (HAWS-chktyp)
    e2 (HAWS-chktyp)
  )
  (redraw (cadr e1) 4)(redraw(cadr e2) 4)
  (setq angdif (-(HAWS-jntang e1)(HAWS-jntang e2))
    angdif (angtos(atan (/ (sin angdif)(cos angdif)))1 8)
    dist1(distance(osnap(caddr e1)"endp")(osnap (caddr e2)"endp"))
  )
  (princ "\n      Distance between selected ends=")(princ dist1)
  (princ "  Angular difference=")(princ angdif)
  (HAWS-ERRRST))(princ)
