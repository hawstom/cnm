(defun c:haws-FACNUM ( / ss off e d s s1 i j fltr dec)
  (setvar "dimzin" 0)
  (setq fltr (list(cons 0"TEXT") (cons 1"*#*")))
  (cond
    ((not (setq ss (ssget fltr))))               ; 4
    ((not (setq off (getstring "\nFactor: "))))  ; 4
    (  (zerop (atof off))
       (princ "\nValue must be non-zero."))
    (t (setvar "cmdecho" 0)
       (command ".undo" "g")
       (setq off (atof off))
       (repeat (setq i (sslength ss))
          (setq e (ssname ss (setq i (1- i)))   ; 3
                d (entget e)
                s (cdr (assoc 1 d)))            ; 3
          (if (/= "" (cadr (setq s (HAWS-EXTRACT s))))              ; 3
            (progn
              (setq dec 0 j 0)
              (cond
                ((not (wcmatch (cadr s) "*.*")))
                (t(repeat (strlen (cadr s))
                    (setq j (+ 1 j))
                    (if (= (substr (cadr s) j 1) ".")
                      (setq dec 0)
                      (setq dec (+ 1 dec))))))
              (entmod
                (list
                  (cons -1 e)
                  (cons 1
                    (strcat
                      (car s)
                      (rtos (* off (atof (cadr s))) 2 dec) ; 4
                      (caddr s))))))))             ; 7
       (command ".undo" "e")))                  ; 3
  (princ)
)
