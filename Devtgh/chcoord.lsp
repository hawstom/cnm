;(C) Copyright 1997 by Thomas Gail Haws
;CHANGE A COORDINATE OF AN ENTITY TO A SPECIFIED VALUE.
(defun c:haws-chcoord ( / dxfpt el en newpt newval n oldpt sset sslen xyz)
  (haws-core-borrow 0)
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
              "\n\nChoose a point code to change: "
  ) )
  (setq dxfpt (getint))
  (initget "X Y Z")
  (setq
    xyz (getkword "\nCoordinate to change [X/Y/Z]: ")
    newval (getreal "\nNew value in current UCS: ")
    n (cond ((= xyz "X") 1)((= xyz "Y") 2)((= xyz "Z") 3))
  )
  (prompt "Entities to change:")
  (setq sset (ssget))
;;; Scale all of the entities in the selection set.
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq
      en (ssname sset (setq sslen (1- sslen)))
      el (entget en)
      oldpt (trans (cdr (assoc dxfpt el)) en 1)
      newpt
      (trans
        (list
          (if (= n 1) newval (car oldpt))
          (if (= n 2) newval (cadr oldpt))
          (if (= n 3) newval (caddr oldpt))
        )
        1
        en
      )
      el (subst (cons dxfpt newpt) (assoc dxfpt el) el)
    )
    (entmod el)
  )
  (command "._undo" "end")
  (haws-core-return)
  (princ)
)
