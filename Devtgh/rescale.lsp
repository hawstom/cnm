;(C) Copyright 1997 by Thomas Gail Haws
;SCALE A SELECTION SET OF ENTITIES ABOUT A USER SPECIFIED ENTITY POINT.
(defun c:haws-rescale ( / dxfpt sset sslen ent xfactr ins ed SFACTR)
  (haws-core-init 304)
  (command "._undo" "_group")
  (setq sset (ssget))
  (textpage)
  (prompt
    (strcat   "\n\n\nEvery Autocad entity has key points and each point has a code."
              "\nA line, for example, has a start point (10) and an end point (11)."
            "\n\nAUTOCAD POINT CODES"
              "\nStart (line, arc, text); center (arc, circle); or insertion (block) point-10"
              "\nSecond point (if any) or text justification point (if not left justified)-11"
              "\nThird point (if any)------------------------------------------------------12"
              "\nFourth point (if any)-----------------------------------------------------13"
              "\n\nChoose a point code to scale about: "
  ) )
  (setq dxfpt (getint))
  (setq sfactr (getdist "\nScale factor: "))
;;; Scale all of the entities in the selection set.
  (prompt "\nRescaling...")
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq ent (ssname sset (setq sslen (1- sslen)))
          ed (entget ent)
          ins (cdr (assoc dxfpt ed))
    )
    (command "._scale" ent "" ins sfactr)
  )
  (prompt "done.")
  (command "._undo" "_end")
  (haws-core-restore)
  (princ)
)
