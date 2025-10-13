(DEFUN C:HAWS-EOP (/ P1 P2 P3 S1 S2 S3 C1 C2 WT W1 A1 A2 A3)
  (HAWS-CORE-INIT 221)
  (SETQ
    P1 (GETPOINT "\nPick outside end of pipe (either side): ")
    P2 (GETPOINT P1 "\nPick end of other side: ")
    WT (GETREAL "\nEnter wall thickness: ")
    P3 (LIST
         (/ (+ (CAR P1) (CAR P2)) 2)
         (/ (+ (CADR P1) (CADR P2)) 2)
       )
    W1 (/ (DISTANCE P1 P2) 8.0)
    C1 (LIST
         (/ (+ (CAR P1) (CAR P3)) 2)
         (/ (+ (CADR P1) (CADR P3)) 2)
       )
    C2 (LIST
         (/ (+ (CAR P2) (CAR P3)) 2)
         (/ (+ (CADR P2) (CADR P3)) 2)
       )
    A1 (ANGLE P1 P2)
    A2 (+ A1 (/ PI 2))
    A3 (+ A2 PI)
    S1 (POLAR C1 A2 W1)
    S2 (POLAR C2 A2 W1)
    S3 (POLAR C2 A3 W1)
  ) ;_ end of SETQ
  (vl-cmdf "._arc" P1 S1 P3 "._arc" P3 S2 P2 "._arc" P3 S3 P2)
  (COND
    ((/= WT 0)
     (SETQ
       P2 (POLAR P2 (+ A1 PI) WT)
       C1 (POLAR C1 A1 (/ WT 2))
       C2 (POLAR C2 (+ A1 PI) (/ WT 2))
       S1 (POLAR C1 A2 (- W1 (/ WT 2)))
       S2 (POLAR C2 A2 (- W1 (/ WT 2)))
       S3 (POLAR C2 A3 (- W1 (/ WT 2)))
     ) ;_ end of SETQ
     (vl-cmdf "._arc" P2 S2 P3 "._arc" P2 S3 P3)
    )
  ) ;_ end of COND
  (HAWS-VRSTOR)
  (HAWS-CORE-RESTORE)
  (PRINC)
) ;_ end of DEFUN
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
