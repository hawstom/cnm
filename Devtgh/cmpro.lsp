;(C) Copyright 1997 by Thomas Gail Haws
;CHANGE ANY GROUP CODE OF A SELECTION SET OF ENTITIES TO MATCH SELECTED ENTITY.
(defun c:haws-cmpro ( / group mg me sset sslen ent ed eg)
  (haws-core-borrow 0)
  (setvar "cmdecho" 0)
  (command "._undo" "group")
  (textpage)
  (prompt
    (strcat   "\n\n\nThe properties of every Autocad entity are found in group codes."
              "\nA line, for example, has a start point (group 10) and an end point (group 11)."
              "\nAll entities have a layer (group 8)."
              "\n\nUSE THE EG OR EGN COMMAND TO LIST ALL THE GROUP CODES FOR AN ENTITY."
              "\n\nChoose a group code to change to match: "
  ) )
  (setq group (getint))
  (prompt "Entities to change:  ")
  (setq sset (ssget)
    me (entsel "\nEntity to match: ")
    mg (assoc group (entget (car me)))
  )
;;; Change all of the entities in the selection set.
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq ent (ssname sset (setq sslen (1- sslen)))
          ed (entget ent)
          eg (assoc group ed)
    )
    (entmod (subst mg eg ed))
  )
  (command "._undo" "end")
  (haws-core-return)
  (princ)
)
