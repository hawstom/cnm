;Written by Thomas Gail Haws
(defun c:haws-pipe (/ p1 p2 p3 p4 d1 d2 a1)
  (haws-core-init 270)
  (setvar "cmdecho" 0)
  (setq p1 (getpoint "\nPipe centerline start point: ")
    p2 (getpoint p1 "\nEnd point: ")
    d1 (/ (getdist "\nID of pipe (ft.): ") 2.0)
    d2 (getdist "\nWall thickness (ft.): ")
  d1 (+ d1 d2)  a1 (angle p1 p2) )
  (command "._line" p1 p2 "")
  (setq p3 (polar p1 (+ a1 (/ pi 2)) d1)   p4 (polar p2 (+ a1 (/ pi 2)) d1))
  (command "._line" p3 p4 "")
  (setq p3 (polar  p1 (- a1 (/ pi 2)) d1)  p4 (polar  p2 (- a1 (/ pi 2)) d1))
  (command "._line" p3 p4 "")
  (setq d1 (- d1 d2))
  (setq p3 (polar p1 (+ a1 (/ pi 2)) d1)   p4 (polar p2 (+ a1 (/ pi 2)) d1))
  (command "._line" p3 p4 "")
  (setq p3 (polar  p1 (- a1 (/ pi 2)) d1)  p4 (polar  p2 (- a1 (/ pi 2)) d1))
  (command "._line" p3 p4 "")
  (haws-core-restore)
)
