;(C) Copyright 1997 by Thomas Gail Haws
;GIVE A SELECTION SET OF ENTITIES A NEW SCALE ABOUT A USER SPECIFIED ENTITY POINT.
(defun c:haws-newscale ( / dxfpt dxfsc sset sslen svalue ent xfactr ins ed)
  (HAWS-ERDF$@ 0)
  (setvar "cmdecho" 0)
  (command "._undo" "group")
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
  (textpage)
  (prompt
    (strcat   "\n\n\nSome Autocad entities have group codes that give scale values."
              "\nA block, for example, has X, Y, and Z scale codes (41, 42, and 43)."
              "\nAn arc has a radius value (40)."
            "\n\nAUTOCAD SCALE CODES"
              "\nSize or scale (arcs, circles)-40"
              "\nX size or scale---------------41"
              "\nY size or scale---------------42"
              "\nZ size or scale---------------43"
              "\nThis routine uses the SCALE command to scale each entity."
              "\n\nChoose a scale code to base old scale on for each entity: "
  ) )
  (setq dxfsc (getint))
  (setq svalue (getdist "\nNew scale value: "))
  (prompt "Entities to scale:")
  (setq sset (ssget))
;;; Scale all of the entities in the selection set.
  (prompt "\nRescaling...")
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq ent (ssname sset (setq sslen (1- sslen)))
          ed (entget ent)
          ins (cdr (assoc dxfpt ed))
          sfactr (/ svalue (cdr (assoc dxfsc ed)))
    )
    (command "._scale" ent "" ins sfactr)
  )
  (prompt "done.")
  (command "._undo" "end")
  (HAWS-ERRRST)
  (princ)
)
