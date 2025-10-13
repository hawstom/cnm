;(C) Copyright 1997 by Thomas Gail Haws
(DEFUN c:haws-txtsum
  ( / ss1 i sum enamei blname ELISTI TS TXPT)
(haws-core-init 319)
  (setq
    ss1
    (ssget                       ;Get a selection set, ss1
      (list                      ;Add all entities matching the following list
        (cons 0  "*TEXT")        ;Insertion entities (blocks, xrefs, etc.)
      )
    )
    i 0                          ;Set a counter, i, to 0
    sum 0
  )
  (if ss1                        ;If there were any entities found in the set,
    (while                       ;Start a loop to repeat as long as
      (setq enamei               ;you can set enamei
        (ssname ss1 i)           ;the ith entity in the set.
      )
      (setq                      ;Set
        elisti                   ;elisti to
        (entget enamei)          ;the association list for enamei
      )
      (setq sum (+ sum (atof (cadr (haws-extract (cdr (assoc 1 elisti)))))))
      (setq i (1+ i))            ;Bump up i to move to the next entity.
    )
  )                              ;Close the while loop.
  (prompt "\nSum: ")
  (princ sum)
  (setq
    txpt (getpoint "\nMiddle point for text:")
    ts (* (HAWS-DWGSCALE)(getvar "dimtxt"))
  )
  (if txpt(HAWS-MKTEXT "m" txpt nil 0 (strcat (rtos sum) " TOTAL")))
  (princ)
)

