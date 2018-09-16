;(C) Copyright 1998 by Thomas Gail Haws
;   ssxpro.lsp
(defun c:haws-ssxpro ( / dxfi fltr ss1 vtype)
(haws-core-init 308)
  (textpage)
  (prompt
    (strcat   "\n\n\nThe properties of every Autocad entity are found in group codes."
              "\nA line, for example, has a start point (group 10) and an end point (group 11)."
              "\nAll entities have a layer (group 8)."
              "\nFor example, the following group codes might define a line:"
  ) )
  (prompt
    (strcat
              "\n((-1 . <Entity name: 60000014>) Entity name"
              "\n(0 . \"LINE\")                    Object type (a string)"
              "\n(8 . \"0\")                       Layer (a string)"
              "\n(10 1.0 2.0 0.0)                Start point (a coordinate)"
              "\n(11 6.0 6.0 0.0))               Endpoint (a coordinate)"
  ) )
  (prompt
    (strcat
              "\n\nUSE THE EG OR EGN COMMAND TO LIST ALL THE GROUP CODES FOR ANY ENTITY."
              "\n\nUse SSXPRO to create a selection set with particular"
              "\nproperties by using group codes: "
              "\nFirst g"
  ) )
  (while (setq dxfi (getint "roup code to use for selection: "))
    (initget "Numeric String Coordinate")
    (setq vtype (getkword "\nGroup value type [Numeric/String/Coordinate]: "))
    (cond
      ( (= vtype "Numeric")
        (initget "Yes No")
        (cond
          ( (= "Yes"(getkword "\nUse a relational test( *, <, >, etc.)? [Yes/No]: "))
            (textpage)
            (prompt
        "\nThe following relational operators can be used.

         Operator  Description
         *    Anything goes (always true)
         =    Equals
         !=   Not equal to
         /=   Not equal to
         <>   Not equal to
         <    Less than
         <=   Less than or equal to
         >    Greater than
         >=   Greater than or equal to
         &    Bitwise AND (integer groups only)
         &=   Bitwise masked equals (integer groups only)

Operator to use: "
            )
            (setq fltr (cons (cons -4 (getstring)) fltr))
          )
        )
        (setq fltr (cons (cons dxfi (getreal "\nValue to test: ")) fltr))
      )
      ( (= vtype "String")
        (setq fltr (cons (cons dxfi (getstring 1 "\nEnter wild card selection test: ")) fltr))
      )
      ( (= vtype "Coordinate")
        (prompt "\nCoordinate selection not programmed yet.")
      )
    )
    (prompt "Filter list: ")
    (prin1 (reverse fltr))
    (prompt "\nNext g")
  )
  (setq ss1(ssget "X" (reverse fltr)))
  (if ss1
    (princ (strcat "\n"(itoa (sslength ss1)) " found."))
    (princ "\nNone found. ")
  )
  (princ)
)
