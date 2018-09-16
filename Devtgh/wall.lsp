;BLOCK WALLS
;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-WALL
  ( / wexist rtemp  WALLPL wlt wltpl wlaclr wlayer wlaypl pt1 pt2 pt3 pt4 r e1)
  (haws-core-init 327)
  (HAWS-VSAVE '("ucsfollow" "clayer"))
  (setvar "ucsfollow" 0)
  (if(not (tblsearch "LTYPE" "utilpl"))
    (command "._linetype" "l" "*" "ut" "")
  )
  (initget 1 "Exist Prop")(setq wexist (=(getkword "\nExist/Prop:")"Exist"))
  (if (not r) (setq r 0.33333))
  (if
    (setq rtemp(getreal(strcat"\nWidth (0 for narrow)<"(rtos(* 24 r))">:")))
    (setq r (/ rtemp 24))
  )
  (setq wlaypl (car(HAWS-MKLAYR (if wexist "WALLXPL" "WALLPL"))))
  (setq wlayer (car(HAWS-MKLAYR (if wexist "WALLXEDGE" "WALLEDGE"))))
  (setq pt1 (getpoint "\nStart point:") pt2 pt1 pt3 nil pt4 nil)
  (command "pline" pt1 "w" 0)
  (initget "Line Undo Arc CEnter Angle Direction Line Radius Second DRag")
  (command "")
  (while (setq pt3
      (getpoint pt2
    "\nSpecify endpoint or [Line/Undo/Arc/CEnter/Angle/Direction/Line/Radius/Second/DRag]:"))
    (if (and (= (type pt3) 'LIST)(not pt4))(setq pt4 pt3))
    (if (= pt3 "DRag")
      (progn
        (prompt "\nSelect point: ")
        (command pause)
      )
      (command pt3)
    )
    (initget "Line Undo Arc CEnter Angle Direction Line Radius Second DRag")
    (if (= (type pt3) 'LIST)(setq pt2 pt3))
  )
  (command "")
  (setq wallpl (entlast))
  (cond
    ((= r 0)(command "change" wallpl "" "p" "la" wlayer ""))
    ( T
      (command "._line" pt1 pt4 "")
      (setq e1 (entlast))
      (command "._ucs" "e" e1 "erase" e1 ""
      "offset" r (list wallpl '(0 0)) (list 0 (* 3 r) 0) "")
      (setq e1 (entlast))
      (command "change" e1 "" "p" "la" wlayer ""
      "offset" r (list wallpl '(0 0)) (list 0 (* -3 r) 0) "")
      (setq e1 (entlast))
      (command "change" e1 "" "p" "la" wlayer "" "pedit" wallpl "w" (* 2 r) ""
      "change" wallpl "" "p" "la" wlaypl ""  "ucs" "p")
  ) )
  ;The remaining lines change the pline ltype gen method.
  (setq wallpl (entget wallpl))
  (entmod
    (subst
      (cons 70 (logior (cdr (assoc 70 wallpl)) 128))
      (assoc 70 wallpl)
      wallpl
  ) )
  (redraw)(HAWS-VRSTOR)(haws-core-restore)(princ)
)
