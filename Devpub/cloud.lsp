;Program Name: c:haws-CLOUD
;Description:  Creates a cloud type image as the user specifies points in
;              a counter-clockwise formation.
;Author:       Craig Allen (415) 842-8350

(defun c:haws-cld ()
  (erdf$@)(vsave'("clayer"))
  (mklayr "CLOUD")
  (cond ((c:haws-revcloud))((c:haws-cld)))
  (vrstor)(errrst)(PRINC)
)
(DEFUN c:haws-CLOUD (/ PT1 PT2 oldlay)
  (SETQ PT1 (GETPOINT "\nEnter first point: "))
  (IF PT1 (progn
      (PROMPT "\nProceed in COUNTER-CLOCKWISE direction...")
      (command "pline" PT1 "w" 0 "" "arc")
  ))
  (WHILE PT1
    (SETQ PT2 (GETPOINT "\nEnter next point: "))
    (IF PT2
      (COMMAND "R" (/ (DISTANCE PT1 PT2) 1.75) PT2)
    ) ;End of if pt2.
    (SETQ PT1 PT2)
  ) ;End of while pt1.
  (command "")
);End of defun c:haws-CLOUD.
