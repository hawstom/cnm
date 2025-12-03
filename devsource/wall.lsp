;BLOCK WALLS
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-wall
  ( / wexist rtemp  wallpl wlt wltpl wlaclr wlayer wlaypl pt1 pt2 pt3 pt4 r e1)
  (haws-core-init 327)
  (haws-vsave '("ucsfollow" "clayer"))
  (setvar "ucsfollow" 0)
  (if(not (tblsearch "LTYPE" "utilpl"))
    (vl-cmdf "._linetype" "_l" "*" "ut" "")
  )
  (initget 1 "Exist Prop")(setq wexist (=(getkword "\nExist/Prop:")"Exist"))
  (if (not r) (setq r 0.33333))
  (if
    (setq rtemp(getreal(strcat"\nWidth (0 for narrow)<"(rtos(* 24 r))">:")))
    (setq r (/ rtemp 24))
  )
  (setq wlaypl (car(haws-setlayr (if wexist "WALLXPL" "WALLPL"))))
  (setq wlayer (car(haws-setlayr (if wexist "WALLXEDGE" "WALLEDGE"))))
  (setq pt1 (getpoint "\nStart point:") pt2 pt1 pt3 nil pt4 nil)
  (vl-cmdf "._pline" pt1 "_w" 0)
  (initget "Line Undo Arc CEnter Angle Direction Line Radius Second DRag")
  (vl-cmdf "")
  (while (setq pt3
      (getpoint pt2
    "\nSpecify endpoint or [Line/Undo/Arc/CEnter/Angle/Direction/Line/Radius/Second/DRag]:"))
    (if (and (= (type pt3) 'LIST)(not pt4))(setq pt4 pt3))
    (if (= pt3 "DRag")
      (progn
        (prompt "\nSelect point: ")
        (vl-cmdf pause)
      )
      (vl-cmdf pt3)
    )
    (initget "Line Undo Arc CEnter Angle Direction Line Radius Second DRag")
    (if (= (type pt3) 'LIST)(setq pt2 pt3))
  )
  (vl-cmdf "")
  (setq wallpl (entlast))
  (cond
    ((= r 0)(vl-cmdf "._change" wallpl "" "_p" "_la" wlayer ""))
    ( t
      (vl-cmdf "._line" pt1 pt4 "")
      (setq e1 (entlast))
      (vl-cmdf "._ucs" "_e" e1 "._erase" e1 ""
      "offset" r (list wallpl '(0 0)) (list 0 (* 3 r) 0) "")
      (setq e1 (entlast))
      (vl-cmdf "._change" e1 "" "_p" "_la" wlayer ""
      "offset" r (list wallpl '(0 0)) (list 0 (* -3 r) 0) "")
      (setq e1 (entlast))
      (vl-cmdf "._change" e1 "" "_p" "_la" wlayer "" "._pedit" wallpl "_w" (* 2 r) ""
      "._change" wallpl "" "_p" "_la" wlaypl ""  "._ucs" "_p")
  ) )
  ;The remaining lines change the pline ltype gen method.
  (setq wallpl (entget wallpl))
  (entmod
    (subst
      (cons 70 (logior (cdr (assoc 70 wallpl)) 128))
      (assoc 70 wallpl)
      wallpl
  ) )
  (redraw)(haws-vrstor)(haws-core-restore)(princ)
)
