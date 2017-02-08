;    +-------------------------------------------------------------+
;    | BURST.LSP -- COPYRIGHT 1990 BY LOOKING GLASS MICROPRODUCTS  |
;    +-------------------------------------------------------------+
;
(setq VERSION "1.00" REDRAW-COMMAND ".redrawall")
;----------------------------------------------------------
; Item from association list
;----------------------------------------------------------
(defun ITEM (N E) (cdr (assoc N E)))
;----------------------------------------------------------
; Error Handler
;----------------------------------------------------------
(defun BURST-ERROR (S)
   (if (/= S "Function cancelled") (princ S))
   (command) (command)
   (command ".UNDO" "e")
   (if UNDOIT
      (progn (princ "\nUndoing...")(command ".undo" 1))
      )
      (MODER)
)
;----------------------------------------------------------
; System variable save
;----------------------------------------------------------
(defun MODES (A)
   (setq MLST nil)
   (repeat (length A)
      (setq
         MLST (append
                 MLST
                 (list (list (car A) (getvar (car A))))
              )
         A    (cdr A)
      )
   )
)
;----------------------------------------------------------
; System variable restore
;----------------------------------------------------------
(defun MODER ()
   (repeat (length MLST)
      (setvar (caar MLST)  (cadar MLST))
      (setq MLST (cdr MLST))
   )
   (setq *error* OLDERROR)
   (princ)
)
;----------------------------------------------------------
; BIT SET
;----------------------------------------------------------
(defun BITSET (A B) (= (boole 1 A B) B))
;----------------------------------------------------------
; Qualify Selection Set
;----------------------------------------------------------
(defun QUALIFY (SS1 / SS2 NUM-SEL NUM-BAD OK ENAME ENT X Y Z)
   (if SS1
      (progn
         (setq
            SS2     (ssadd)
            NUM-SEL (sslength SS1)
            NUM-BAD 0
         )
         (prompt
            (strcat "\n  " (itoa NUM-SEL) " selected.  ")
         )
         (repeat NUM-SEL
            (setq ENAME (ssname SS1 0))
            (ssdel ENAME SS1)
            (setq
               ENT (entget ENAME)
               OK  (and
                      (= "INSERT" (ITEM 0 ENT))
                      (setq
                         X (ITEM 41 ENT)
                         Y (ITEM 42 ENT)
                         Z (ITEM 43 ENT)
                      )
                      (> Z 0)
                      (or (= X Y Z) (= (- X) (- Y) Z))
                   )
            )
            (if OK
               (ssadd ENAME SS2)
               (setq NUM-BAD (1+ NUM-BAD))
            )
         )
         (if (> NUM-BAD 0)
            (prompt (strcat (itoa NUM-BAD) " invalid. "))
         )
         (if (> (sslength SS2) 0) SS2)
      )
   )
)
;----------------------------------------------------------
; Convert Attribute Entity to Text Entity
;----------------------------------------------------------
(defun ATT-TEXT (AENT / TENT ILIST INUM)
   (setq TENT '((0 . "TEXT")))
   (foreach INUM '(8 6 38 39 62 67 210 10 40 1 50 41 51 7
                  71 72 74 11)
      (if (setq ILIST (assoc INUM AENT))
         (progn
           (if (= inum 74) (setq ILIST (cons 73 (cdr(assoc INUM AENT)))))
           (setq TENT (cons ILIST TENT))
         )
      )
      )
      (entmake (reverse TENT))
)
;----------------------------------------------------------
; Find True last entity
;----------------------------------------------------------
(defun LASTENT (/ E0 EN)
   (setq E0 (entlast))
   (while (setq EN (entnext E0)) (setq E0 EN))
   E0
)
;----------------------------------------------------------
; Burst one entity
;----------------------------------------------------------
(defun BURST-ONE (BNAME / BENT ANAME ENT ATYPE AENT AGAIN ENAME
                  ENT SS-COLOR SS-LAYER SS-LTYPE)
   (setq
      BENT   (entget BNAME)
      BLAYER (ITEM 8 BENT)
      BCOLOR (ITEM 62 BENT)
      BCOLOR (cond
                ((> BCOLOR 0) BCOLOR)
                ((= BCOLOR 0) "BYBLOCK")
                ("BYLAYER")
             )
      BLTYPE (cond ((ITEM 6 BENT)) ("BYLAYER"))
   )
   (setq ELAST (LASTENT))
   (if (= 1 (ITEM 66 BENT))
      (progn
         (setq ANAME BNAME)
         (while (setq
                   ANAME (entnext ANAME)
                   AENT  (entget ANAME)
                   ATYPE (ITEM 0 AENT)
                   AGAIN (= "ATTRIB" ATYPE)
                )
            (ATT-TEXT AENT)
         )
      )
   )
   (command ".explode" BNAME)
   (setq
      SS-LAYER (ssadd)
      SS-COLOR (ssadd)
      SS-LTYPE (ssadd)
      ENAME    ELAST
   )
   (while (setq ENAME (entnext ENAME))
      (setq
         ENT   (entget ENAME)
         ETYPE (ITEM 0 ENT)
      )
      (if (= "ATTDEF" ETYPE)
         (progn
            (if (BITSET (ITEM 70 ENT) 2) (ATT-TEXT ENT))
            (entdel ENAME)
         )
         (progn
            (if (= "0" (ITEM 8 ENT)) (ssadd ENAME SS-LAYER))
            (if (= 0 (ITEM 62 ENT)) (ssadd ENAME SS-COLOR))
            (if (= "BYBLOCK" (ITEM 6 ENT))
               (ssadd ENAME SS-LTYPE)
            )
         )
      )
   )
   (if (> (sslength SS-LAYER) 0)
      (command ".chprop" SS-LAYER "" "la" BLAYER "")
   )
   (if (> (sslength SS-COLOR) 0)
      (command ".chprop" SS-COLOR "" "c" BCOLOR "")
   )
   (if (> (sslength SS-LTYPE) 0)
      (command ".chprop" SS-LTYPE "" "lt" BLTYPE "")
   )
)
;----------------------------------------------------------
; BURST MAIN ROUTINE
;----------------------------------------------------------
(defun BURST (/ SS1)
   (setq SS1 (if (< (atof (getvar "acadver")) 14.0) (QUALIFY (ssget))(ssget)))
   (if SS1
      (progn
         (setq UNDOIT t)
         (repeat (sslength SS1)
            (setq ENAME (ssname SS1 0))
            (ssdel ENAME SS1)
            (BURST-ONE ENAME)
         )
         (command REDRAW-COMMAND)
      )
   )
   (princ "Note that BURST does not work if an attribute is the last entity in the drawing.")
)
;----------------------------------------------------------
; BURST COMMAND
;----------------------------------------------------------
(defun c:haws-BURST (/ OLDERROR UNDOIT MLST)
   (princ
      (strcat
         "\rBURST version " VERSION
         " -- Copyright 1991 by Looking Glass Microproducts"
      )
   )
   (MODES '("highlight"))
   (setq
      OLDERROR *error*
      *error*  BURST-ERROR
   )
   (command ".undo" "group")
   (BURST)
   (command ".undo" "e")
   (MODER)
)
