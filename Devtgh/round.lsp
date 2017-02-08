;(C) Copyright 1997 by Thomas Gail Haws
(defun
  c:haws-ROUND
  ( / ss1 i enamei r ELISTI NUM)
  (setq
    ss1
    (ssget                       ;Get a selection set, ss1
      (list                      ;Add all entities matching the following list
        (cons 0  "TEXT")         ;Insertion entities (blocks, xrefs, etc.)
      )
    )
    r (getint "Places to round to:  ")
    i 0                          ;Set a counter, i, to 0
  )
  (if ss1                        ;If there were any entities found in the set,
    (while                       ;Start a loop to repeat as long as
      (setq enamei               ;there is an enamei,
        (ssname ss1 i)           ;the ith entity in the set.
      )
      (setq                      ;Set
        elisti                   ;elisti to
        (entget enamei)          ;the association list for enamei
        num (atof (cdr (assoc 1 elisti)))
      )
      (entmod                  ;Then modify the entity by
        (subst                 ;substituting
          (cons 1 (rtos num 2 r))
          (assoc               ;  for the current association group
            1                  ;   1 (string)
            elisti             ;   of the association list for the ith entity
          )
          elisti               ;in elisti
        )
      )
      (setq i (1+ i))            ;Bump up i to move to the next entity.
    )
  )                              ;Close the while loop.
  (princ)
)

