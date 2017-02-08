;Used to set the current text style. User can select an existing text string
;to retrieve a style name. Rev 7/01/96
(defun c:haws-SELSTYLE (/ ans tstyl)
  (erdf$@)
  (setq tstyl nil ans nil)
  (while (not ans)
    (princ (strcat "\nCurrent text style " (getvar "TEXTSTYLE")))
    (setq tstyl (strcase
    (getstring "\nNew current text style <return for entity selection>: ")))
    (if (= tstyl "")
      (setq
        enm (car(selcerob "\nSelect a TEXT string for current textstyle: " "TEXT"))
        elst (entget enm)
        tstyl (cdr (assoc 7 elst))
      )
    )
    (setq ans (tblsearch "style" tstyl))
    (if (not ans) (princ "**Invalid text style**"))
  )
  (princ tstyl)
  (command ".STYLE" tstyl "" "" "" "" "" "" "")
  (errrst)(princ)
)
;end selstyle
