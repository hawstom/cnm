;TIP567.LSP    Cumulative Distance    (c)1990, Bob Thomas
;modified to work as claimed, additions noted. Kent M. Taylor 9/90
;
(defun MD (/ PT1 NPT M_DIST L_DIST)    ;kmt add last dist variable
  (setq M_DIST 0)
  (if 
   (setq PT1 (getpoint "First point: "))
   (progn
    (while
    (if                                ;kmt if no next pt given, exit
     (setq
       NPT (getpoint PT1"\nNext pt: ")
     )                                 ;kmt setq 
     (progn                            ;kmt continue 
      (setq  
       M_DIST (+ M_DIST (setq L_DIST (distance PT1 NPT)))
       PT1 NPT
      )
      (princ (strcat "\nDist from last pt: "  ;kmt provide added dist
        (rtos L_DIST) ))                      ;kmt  
      (princ (strcat "  Cumulative dist: "
        (rtos M_DIST) ))
     )                                        ;kmt progn
    )                                         ;kmt if 
    );while
    (if (/= M_DIST 0)
     (eval M_DIST)
     (princ))
   )
   (princ)
  )
)
(defun c:haws-MD () ;main routine
  (MD)
  (princ)
)
