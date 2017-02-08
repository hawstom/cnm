;;;Written by Thomas Gail Haws
(DEFUN
   C:HAWS-0 ()
  (SETVAR "tilemode" 0)
  (HAWS-TILEMODE-LTSCALE-ADJUST "ps")
  (PRINC)
)
(DEFUN
   C:HAWS-1 (/ DS LT)
  (SETVAR "tilemode" 1)
  (HAWS-TILEMODE-LTSCALE-ADJUST "ms")
  (PRINC)
)
(DEFUN
   HAWS-TILEMODE-LTSCALE-ADJUST (NEWSPACE)
  (SETVAR "regenmode" 0)
  (COND
    ;;If psltscale and msltscale are equal, leave ltscale alone
    ((= (GETVAR "psltscale") (GETVAR "msltscale")) NIL)
    ;;Else if the space we are going to has its ltscale mode at 0, set ltscale to dimscale or 1/cannoscalevalue.
    ((= (GETVAR (STRCAT NEWSPACE "ltscale")) 0)
     (COND
       ((/= (GETVAR "dimscale") 0)
        (SETVAR "ltscale" (GETVAR "dimscale"))
        (PRINC (strcat "\nSetting ltscale equal to current dimscale value (" (rtos(getvar "dimscale") 2) ")."))
       )
       (T
        (SETVAR "ltscale" (/ 1 (GETVAR "cannoscalevalue")))
        (PRINC
          (strcat "\nSetting ltscale to match drawing annotation scale (1 / cannoscale) value (" (rtos(/ 1 (getvar "cannoscalevalue")) 2) ").")
        )
       )
     )
    )
    ;;Else set ltscale to 1.
    (T (SETVAR "ltscale" 1) (PRINC "\nSetting ltscale to 1."))
  )
  (SETVAR "regenmode" 1)
)
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
