;; EE.LSP
;; combined dialogue editor for
;; blocks, attdefs, text, and dimensions.

(defun c:haws-ee (/ e etype set1 obj1 )
(haws-core-init 27)
  (PROMPT "\n \nText Editor  :")
  (SETQ SET1 (SSGET))
  (WHILE (AND SET1
    (SETQ OBJ1 (SSNAME SET1 0)))
    (SETQ E (ENTGET OBJ1) ETYPE (cdr(assoc 0 e)))
    (REDRAW OBJ1 3)
    (cond
      ( (AND (= etype "INSERT") (CDR (ASSOC 66 E)))
        (command "._TEXTEDIT" OBJ1)
      )
      ( (= etype "ATTDEF")
        (command "._TEXTEDIT" OBJ1 "")
      )
      ( (= etype "TEXT")
        (command "._TEXTEDIT" OBJ1 "")
      )
      ( (= etype "MTEXT")
        (command "._TEXTEDIT" OBJ1 "")
      )
      ( (= etype "DIMENSION")
        (command "._TEXTEDIT" OBJ1 "")
    ) )
    (REDRAW OBJ1 4)
    (IF (/= OBJ1 NIL)(SSDEL OBJ1 SET1))
) )
;END EE.LSP
