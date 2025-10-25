;BREAKS LINES, ARCS, POLYLINES WITH THE SAME INTERSECTION
;BY SELECTING AT INTERSECTION WHERE OBJECTS MEET.
;
(defun c:haws-brk (/ os cmd a brk be ls less no)
(haws-core-init 7)
  (setq cmd (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq os (getvar "OSmoDE"))
  (vl-cmdf "OSNAP" "INT")
  (prompt "SELECT INTERSECTION OF OBJECTS TO BREAK. . .")
  (setq a (getpoint))
  (setq brk (list (car a) (cadr a)))
  (setq be (ssget "C" brk brk))
  (setq
    ls (sslength be)
    less (sslength be)
    no 0
  );SETQ
  (if (= 1 less)
    (progn
      (prompt "\nInvalid point")
    );PROGN
    (progn
      (repeat ls
        (vl-cmdf "BREAK" (ssname be no) brk brk)
        (setq no (1+ no))
      );REPEAT
    );PROGN
  );IF
  (setvar "OSmoDE" os)
  (setvar "CMDECHO" cmd)
  (princ)
  (prin1)
);DEFUN
