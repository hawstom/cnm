;leader.lsp - author unknown
(Defun Cla ()
  (Setq P1 (Getpoint "\nLeader start: "))
  (Setq P2 (Getpoint P1 "\nTo point: "))
  (Setq ANG (/ (* 180.0 (Angle P1 P2)) pi))
  (Command "Insert" "Clarw" P1 (* DS (Getvar "Dimasz")) "" ANG)
  (Prompt "\nTo point: ")
  (Command "Pline" P1 P2 Pause "")
  (Setq p1 p2 p2 (Getvar "Lastpoint"))
  (Command "Pedit" "l" "S" "")
  (firstpt 0.1)
(addtxt)(Princ))

(Defun SLA ()
  (setq p1 (getpoint "\nArrow: ")
  p2 (getpoint p1 "\nEnd: "))
  (Command "Dim1" "Lea" p1 p2 "")
  (command)
(firstpt 0.25)(addtxt)(princ))

(defun addtxt ()
  (while (/= txt "")
    (cond ((< (car p1) (car p2))
        (setq txt (getstring t "\nText l: "))
      (mktext nil p2 thgt 0 txt))
      ((> (car p1) (car p2))
        (setq txt (getstring t "\nText r: "))
      (mktext "r" p2 thgt 0 txt))
    )(setq p2 (list (car p2) (- (cadr p2) (* thgt 1.5))))
)(Princ))

(defun firstpt (dst)
  (if (< (car p1) (car p2))
    (progn (setq p2 (list (+ (car p2) (* dst (getvar "dimscale")))
    (- (cadr p2) (/ thgt 2.0)))))
    (progn (setq p2 (list (- (car p2) (* dst (getvar "dimscale")))
  (- (cadr p2) (/ thgt 2.0))))))
)

(Defun c:haws-LDR ()
  (erdf$@)
  (Setq X "S" DS (Getvar "Dimscale")
    THGT (* DS (Getvar "Dimtxt"))
  txt "a")
  (Initget "C S")
  (Setq X (Getkword "\nCurved leader/<Std leader>: "))
(graphscr) (If (/= X "C")(SLA)(CLA))(errrst));end Leader
;end LDR
