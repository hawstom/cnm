;   Joins lines and polylines into a single polyline entity
;   useful in "cleaning up" random digitized lines (maps, etc.)
;   for deweeding or entity enhancement (width, type, color).

;=========================JOIN.LSP===========================
;   Jerry Workman  		    Written:   December 27, 1987
;   Compuserve ID: 70717,3564 	    modified:  July 2, 1988
;
;   This program is donated to the public domain.
;============================================================


;============================================================
(defun c:haws-PLJOIN( / cmdecho blipmode changed ss1 ss2)

  (princ "\n\n\n\n=========================JOIN.LSP===========================")
  (princ "\n\nJerry Workman\n")
  (princ "\n\nJOIN connects lines together into a single polyline")
  (princ "\neven when the endpoints do not meet exactly.")
  (princ "\nJOIN may need to be used more than once to connect")
  (princ "\nseveral lines. Each use will attempt to connect the")
  (princ "\nendpoints of the selected line or polyline to")
  (princ "\nendpoints of other lines within the snap distance")
  (princ "\nspecified. NOTE: The snap distance is scaled to")
  (princ "\nthe screen using the AutoCAD APERTURE internal varible")
  (princ "\nwhich can only be set to within 1 and 50. If JOIN does not")
  (princ "\nwork as expected then ZOOM out so that endpoints can be")
  (princ "\ncaptured in the aperature box. Endpoints must be on the")
  (princ "\nscreen and World UCS must be current for JOIN to work.")

  (setq dist
      (* (getvar "aperture")
        (/ (getvar "viewsize")(cadr(getvar "screensize")))))
  (princ (strcat "\n\nInitial Snap distance is " (rtos dist)))
  (princ "\n\nLoading...")

  ;*----- Error Routine
  (setq olderr *error*)
  (defun *ERROR* (s) (redraw) (grtext) (princ s) (join_exit))

  ;*----- EXTRACT a field from a list

  (defun JOIN-FLD (num lst) (cdr (assoc num lst)))

  ;*----- Plot a temporary box

  (defun box (pnt / s x1 y1 x2 y2 p1 p2 p3 p4)

    (setq s  dist
          x1 (+ (car pnt) s)
          y1 (+ (cadr pnt) s)
          x2 (- (car pnt) s)
          y2 (- (cadr pnt) s)
          p1 (list x1 y1) p2 (list x2 y1)
          p3 (list x2 y2) p4 (list x1 y2))
    (grdraw p1 p2 -1) (grdraw p2 p3 -1)
    (grdraw p3 p4 -1) (grdraw p4 p1 -1)
  )

  ;*----- Get Parameters from user

  (defun GetParm(message default / parm)
    (setq parm (getreal (strcat message "<" (rtos default)  "> ")))
    (if parm (setq parm parm) (setq parm default))
  )

  ;*----- Reset system varibles

  (defun join_exit()
    (setvar "cmdecho"  cmdecho)
    (setvar "blipmode" blipmode)
    (setvar "aperture" aperture)
    (princ) ; to prevent (nil) at join_exit
  )

  ;*----- Convert a line to a polyline entity

  (defun line_pline(ent / ss1)
    (princ "\nConverting entity to PLINE")
    (setq ss2 (ssget "C" (getvar "EXTMIN") (getvar "EXTMAX")))
    (command "pedit" ent "y" "j" ss1 "" "x")
    (entlast) ; return the new entity name
  )

  ;============================================================
  ;  Get a polyline or line entity

  (defun select_pline(/ pl name)
    (setq name nil)
    (while (not (or (= name "LINE") (= name "ARC")(= name "POLYLINE"))) (progn
      (setq ename nil)
      (setq e (car (entsel "\nSelect a PolyLine, Line, or Arc: ")))
      (if e (progn
        (setq pl (entget e)
              name (JOIN-FLD 0 pl)
              ename e)
        (if (or (= name "LINE") (= name "ARC") (= name "POLYLINE")) (progn
          (princ (strcat "\n" name " selected"))
          (if (or (= name "LINE")(= name "ARC"))(setq ename (line_pline e)))
        );else
          (progn  (princ "\nThat's not a LINE or POLYLINE, it's a ") (princ name))
        ); end if
      ); end progn
      ; else
        (princ "\nNothing Selected")
      ); end if
    )); end while
    ename
  )

  ;============================================================
  ; step through subentities and EXTRACT vertex

  (defun get_vert()
    (setq vert nil
          etype nil)
  ;(print ename)
  ;(print e)
    (while (and e (null vert) (not (= etype "SEQEND"))) (progn
      (setq e     (entnext e) ;program crashes here if the original entity
            etype nil)        ;is a single line (bug in Autolisp ?)
      (if e (progn
        (setq se    (entget e)
              etype (JOIN-FLD 0 se))
        (if (= etype "VERTEX") (progn
          (setq vert (JOIN-FLD 10 se)
                sub_ent se)
  ;(print vert)
  ;(print sub_ent)
        ); else
          nil
        )
      ))
    ))
    vert
  )

  ; *----- Get the PolyLine endpoints

  (defun get_pl()
    (setq name nil)
    (select_pline)
    (setq first T
          e     ename)
    (while (not (= etype "SEQEND")) (progn
      (get_vert)
      (if vert (progn
          (setq last_vert vert)
          (if first (progn
            (setq first_vert vert
                  first      nil)
            (mod_end first_vert)
          )); end if
      )); end if
    )); end while
    (mod_end last_vert)
    (setq  etype nil)
  ;  (princ "\nFirst Vertex is ")  (princ first_vert)
  ;  (princ "\nLast Vertex is " )  (princ last_vert)
  )

  (defun set_aperture(/ app)
    (setq app
      (atoi(rtos(* dist(/ (cadr(getvar "screensize"))(getvar "viewsize"))))))
    (cond
      ((< app 1) 1)
      ((> app 50) 50)
      (T app))
  )

  ; *----- Change PolyLine ends to adjoining entities

  (defun mod_end(pnt / new_pnt)
    (box pnt)
    (entdel ename) ; temporily delete polyline
    (setq   new_pnt (osnap pnt "endp")) ;snap to whatever is close
    (entdel ename) ; undelete polyline
    (box pnt)
    (if new_pnt (progn
      (princ "\nmoving end point to ") (princ new_pnt)
      (setq  new_ent (subst  (cons 10 new_pnt) (cons 10 pnt) sub_ent)
            changed T)
      (entmod new_ent)
      new_pnt
    ))
  )

; The main routine...

  (setq cmdecho  (getvar "cmdecho")
        blipmode (getvar "blipmode")
        aperture (getvar "aperture")
        changed  nil
   min_adist
     (/ (getvar "viewsize")(cadr(getvar "screensize")))
   max_adist
     (* min_adist 50))
  (princ (strcat "\nSnap distance minimum: " (rtos min_adist)))
  (princ (strcat "\nSnap distance maximum: " (rtos max_adist)))
  (setq dist (getparm "\nEnter snap distance: " dist))
  (setvar "aperture" (set_aperture))
  (setvar "cmdecho" 0)
  (setvar "blipmode" 0)
  (get_pl)
  (if changed (progn
    (setq ss1 (ssadd ename)
          ss2 (ssget "C" (getvar "EXTMIN") (getvar "EXTMAX")))
    (ssdel ename ss2)
    (princ "\nJoining to adjacent lines...\n")
    (command "pedit" ss1 "j" ss2 "" "x")
  ); end progn
    (princ "\nNothing changed")
  )
  (setq *error* olderr)
)
