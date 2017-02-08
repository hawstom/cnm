;BREAKS LINES, ARCS, POLYLINES WITH THE SAME INTERSECTION
;BY SELECTING AT INTERSECTION WHERE OBJECTS MEET.
;
(DEFUN c:haws-BRK (/ OS CMD a brk be ls less no)
  (SETQ CMD (GETVAR "CMDECHO"))
  (SETVAR "CMDECHO" 0)
  (SETQ OS (GETVAR "OSmoDE"))
  (COMMAND "OSNAP" "INT")
  (PROMPT "SELECT INTERSECTION OF OBJECTS TO BREAK. . .")
  (SETQ A (GETPOINT))
  (SETQ BRK (LIST (CAR A) (CADR A)))
  (SETQ BE (SSGET "C" BRK BRK))
  (SETQ
    LS (SSLENGTH BE)
    LESS (SSLENGTH BE)
    NO 0
  );SETQ
  (IF (= 1 LESS)
    (PROGN
      (PROMPT "\nInvalid point")
    );PROGN
    (PROGN
      (REPEAT LS
        (COMMAND "BREAK" (SSNAME BE NO) BRK BRK)
        (SETQ NO (1+ NO))
      );REPEAT
    );PROGN
  );IF
  (SETVAR "OSmoDE" OS)
  (SETVAR "CMDECHO" CMD)
  (PRINC)
  (PRIN1)
);DEFUN
