;TIP567.LSP    Cumulative Distance    (c)1990, Bob Thomas
;modified to work as claimed, additions noted. Kent M. Taylor 9/90
;
(defun md (/ pt1 npt m_dist l_dist)    ;kmt add last dist variable
  (setq m_dist 0)
  (if
    (setq pt1 (getpoint "First point: "))
    (progn
      (while (setq npt (getpoint pt1"\nNext pt: "))                                 ;kmt setq
        (setq
          m_dist (+ m_dist (setq l_dist (distance pt1 npt)))
          pt1 npt
        )
        (princ (strcat "\nDist from last pt: " (rtos l_dist) ))
        (princ (strcat "  Cumulative dist: "   (rtos m_dist) ))
      );while
      (if (/= m_dist 0)(eval m_dist)(princ))
    )
    (princ)
  )
)
(defun c:haws-md ()
(haws-core-init 13) ;main routine
  (md)
  (princ)
)
;end CUMDIST
