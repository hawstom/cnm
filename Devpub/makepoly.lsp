;   +----------------------------------------------------------------+
;   | MakePoly.LSP -- COPYRIGHT 1991 BY LOOKING GLASS MICROPRODUCTS  |
;   +----------------------------------------------------------------+
;
(Setq version "1.0" ucs 1)
;-----------------------------------------------------
; Item from association list
;-----------------------------------------------------
(Defun item (n e)
   (CDR (Assoc n e))
)
;-----------------------------------------------------
; Returns the selection set of all entities
; which are members of both ss1 and ss2. 
;-----------------------------------------------------
(Defun meet (ss1 ss2 / pt1 ssa ssb)
   (If (And ss1 ss2)
      (Progn
         (Command
            ".point"
            (GetVar "limmin")
         )
         (Setq pt1 (EntLast))
         (Command
            ".select" pt1 ss1 "r" ss2 ""
         )
         (Setq ssa (SsGet "P"))
         (SsDel pt1 ssa)
         (Command
            ".select" pt1 ss1 "r" ssa ""
         )
         (Setq ssb (SsGet "P"))
         (SsDel pt1 ssb)
         (EntDel pt1)
         (If (> (SsLength ssb) 0) ssb)
      )
   )
)
;-----------------------------------------------------
; BIT SET
;-----------------------------------------------------
(Defun bitset (a b)
   (/= (Boole 1 a b) 0)
)
;-----------------------------------------------------
; Error Handler
;-----------------------------------------------------
(Defun MakePoly-error (s)
   (If (/= S "Function cancelled")
      (Princ s)
   )
   (Command)
   (Command)
   (Command ".UNDO" "e")
   (If undoit
      (Progn
         (Princ "\nUndoing...")
         (Command ".undo" 1)
      )
   )
   (moder)
)
;-----------------------------------------------------
; System variable save
;-----------------------------------------------------
(Defun modes (a)
   (Setq MLST Nil)
   (Repeat
      (Length a)
      (Setq
         MLST (Append
                 MLST
                 (List
                    (List
                       (CAR a)
                       (GetVar (CAR a))
                    )
                 )
              )
      )
      (Setq a (CDR a))
   )
)
;-----------------------------------------------------
; System variable restore
;-----------------------------------------------------
(Defun moder ()
   (Repeat
      (Length MLST)
      (Setvar
         (CAAR MLST) (CADAR MLST)
      )
      (Setq MLST (CDR MLST))
   )
   (Setq *Error* olderror)
   (Princ)
)
;-----------------------------------------------------
; System variable set
;-----------------------------------------------------
(Defun setvars (mlst)
   (Repeat
      (Length MLST)
      (Setvar
         (CAAR MLST) (CADAR MLST)
      )
      (Setq MLST (CDR MLST))
   )
)
;-----------------------------------------------------
; MATCH the layer, linetype, and color of
; ename1 to ename2
;-----------------------------------------------------
(Defun match (ename1 ename2 / ent2 layer color ltype)
   (Setq
      ent2  (EntGet ename2)
      layer (item 8 ent2)
      ltype (item 6 ent2)
      color (item 62 ent2)
   )
   (If (Null ltype)
      (Setq ltype "BYLAYER")
   )
   (If (Null color)
      (Setq color "BYLAYER")
      (If (Zerop color)
         (Setq color "BYBLOCK")
      )
   )
   (Command
      ".chprop" ename1 "" "la" layer "lt" ltype "color" color ""
   )
   ename1
)
;-----------------------------------------------------
; Wide Poly
;-----------------------------------------------------
(Defun widepoly (pname)
   (If polywidth
      (Command
         ".pedit" pname "w" polywidth ""
      )
      (Redraw pname)
   )
)

;-----------------------------------------------------
; Do Closed
;-----------------------------------------------------
(Defun do-closed (/ i l ename ent etype r c p1)
   (Setq
      i 0
      l (SsLength ss-closed)
   )
   (While (< i l)
      (Setq
         ename (SsName ss-closed i)
         i     (1+ i)
         ent   (EntGet ename)
         etype (item 0 ent)
      )
      (If (= "CIRCLE" etype)
         (Progn
            (Command ".ucs" "e" ename)
            (Setq
               r  (item 40 ent)
               c  (Trans
                     (item 10 ent)
                     ename
                     ucs
                  )
               p1 (Polar c 0 r)
            )
            (Command
               ".pline" p1 "w" 0 0 "arc" "ce" c "an" 180 "cl"
            )
            (Setq pname (EntLast))
            (match pname ename)
            (EntDel ename)
            (widepoly pname)
            (Command ".ucs" "p")
         )
         (If (= mode "Polylines")
            (widepoly ename)
            (Command ".explode" ename)
         )
      )
   )
)
;-----------------------------------------------------
; Do Open
;-----------------------------------------------------
(Defun do-open (/ i l ent etype ename)
   (Setq
      i 0
      l (SsLength ss-open)
   )
   (While (< i l)
      (Setq
         ename (SsName ss-open i)
         i     (1+ i)
         ent   (EntGet ename)
         etype (item 0 ent)
      )
      (If ent
         (If (= mode "Polylines")
            (Progn
               (Redraw ename 4)
               (Command
                  ".ucs" "e" ename ".pedit" ename
               )
               (If (= "ARC" etype)
                  (Command "Yes")
               )
               (Command
                  "join" ss-open ss-lines ""
               )
               (If polywidth
                  (Command "w" polywidth)
               )
               (Command "" ".ucs" "p")
            )
            (Command ".explode" ename)
         )
      )
   )
)
;-----------------------------------------------------
; Do Lines
;-----------------------------------------------------
(Defun do-lines (/ i l ename ent etype)
   (Setq
      i 0
      l (SsLength ss-lines)
   )
   (While (< i l)
      (Setq
         ename (SsName ss-lines i)
         i     (1+ i)
         ent   (EntGet ename)
         etype (item 0 ent)
      )
      (If ent
         (Progn
            (Command
               ".ucs" "e" ename ".pedit" ename "Y" "join" ss-lines
               ""
            )
            (If polywidth
               (Command "w" polywidth)
            )
            (Command "" ".ucs" "p")
         )
      )
   )
)
;-----------------------------------------------------
; Classify Objects by Entity Type
;-----------------------------------------------------
(Defun classify (ss1 / i l pflags ename ent etype)
   (Setq
      ss-lines  (SsAdd) ; lines
      ss-open   (SsAdd) ; open polylines and arcs
      ss-closed (SsAdd) ; closed polylines and circles
      i         0
      l         (SsLength ss1)
   )
   (While (< i l)
      (Setq
         ename (SsName ss1 i)
         i     (1+ i)
         ent   (EntGet ename)
         etype (item 0 ent)
      )
      (Redraw ename 3)
      (Cond
         ((= etype "LINE")
            (If (= mode "Polylines")
               (SsAdd ename ss-lines)
            )
         )
         ((= etype "ARC")
            (If (= mode "Polylines")
               (SsAdd ename ss-open)
            )
         )
         ((= etype "CIRCLE")
            (If (= mode "Polylines")
               (SsAdd ename ss-closed)
            )
         )
         ((And
             (= etype "POLYLINE")
             (<
                (Setq pflags (item 70 ent))
                8
             )
          )
            (SsAdd
               ename
               (If (bitset pflags 1)
                  ss-closed
                  ss-open
               )
            )
         )
         ((Redraw ename 4))
      )
   )
   (Apply
      '+
      (MapCar
         'SsLength
         (List
            ss-lines
            ss-open
            ss-closed
         )
      )
   )
)
;-----------------------------------------------------
; MakePoly Main Routine
;-----------------------------------------------------
(Defun MakePoly (/ ss1 ss-lines ss-open ss-closed ok found mode)
  (Princ (strcat "\nMakePoly.Lsp v " version " Copyright 1991 by Looking Glass Microproducts"))
   (Setq ss1 (SsGet) ok ss1)
   (If ok
      (Progn
         (Setvar "highlight" 0)
         (Setvar "blipmode" 0)
         (Setq
            lname (GetString "\nOn layer <*>: ")
         )
         (If (= "*" lname)
            (Setq lname "")
         )
         (If (/= "" lname)
            (Progn
               (Setq
                  ss1   (meet
                           ss1
                           (SsGet
                              "x"
                              (List (Cons 8 lname))
                           )
                        )
                  found (If ss1 (SsLength ss1) 0)
                  ok    ss1
               )
               (Princ
                  (Strcat
                     "\n"
                     (ItoA found)
                     " found."
                  )
               )
            )
         )
      )
   )
   (If ok
      (Progn
         (InitGet 1 "Lines Polylines")
         (Setq
            mode (GetKword
                    "\nConvert to Lines or Polylines? "
                 )
         )
         (Setq undoit T)
         (Setq
            qualified (classify ss1)
            ok        (> qualified 0)
         )
         (Princ
            (Strcat
               "\n"
               (RtoS qualified 2 0)
               " qualified."
            )
         )
      )
   )
   (If ok
      (Progn
         (If (= mode "Polylines")
            (Progn
               (InitGet 4)
               (Setq
                  polywidth (GetDist
                               "\nNew width for all segments: "
                            )
               )
            )
         )
         (do-closed)
         (do-open)
         (do-lines)
      )
   )
)
;-----------------------------------------------------
; MakePoly COMMAND
;-----------------------------------------------------
(Defun c:haws-MakePoly (/ olderror undoit)
   (modes
      '("cmdecho"
         "osmode"
         "elevation"
         "thickness"
         "blipmode"
         "highlight"
      )
   )
;   (Setq
;      olderror *Error*
;      *Error*  MakePoly-error
;   )
   (setvars
      '(("cmdecho" 0)
         ("osmode" 0)
         ("elevation" 0.0)
         ("thickness" 0.0)
      )
   )
   (Command ".undo" "group")
   (MakePoly)
   (Command ".undo" "e")
   (moder)
)
