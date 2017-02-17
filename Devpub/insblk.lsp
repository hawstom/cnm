;;; INSBLK.LSP
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Simplifies the creation of block insertion lisp command routines.
;;;
;;; This was a Standage and Truitt (then Agra then Amec) block insertion system.
;;;
;;;Usage:
;;;  (insblk
;;;    "blockname"
;;;    blocklayerlist
;;;    "blockrotation"
;;;    options
;;;  )
;;;Argument definitions:
;;;     "blockname"     A valid block name string in quotes.
;;;                     This is the name of the block to be inserted.
;;;     blocklayerlist  A list containing:
;;;                       1. A valid layer name string in quotes.
;;;                       2. A valid layer color
;;;                       3. A valid linetype name string in quotes.
;;;                     This is the layer the block will be inserted on.
;;;     "blockrotation" A string in quotes.
;;;                     If the string supplied is "P" (capital p),
;;;                     The function will prompt for a rotation angle.
;;;                     Any other string, such as "0" or "90",
;;;                     will be used as a preset rotation angle.
;;;     options         An integer argument representing the sum of the
;;;                     following options desired.
;;;                       Option   Value  Description
;;;                        none      0
;;;                       dimscale   1     Scale up block using dimscale.
;;;                       explode    2     Explode block after insertion.
;;;                       mirror     4     Mirror block.
;;;                       dimtxt     8     Scale block per dimtxt.
;;;                       chg layer 16    If explode, change sub-entities to given layer.
;;;
;;;Usage examples
;;;
;;;The following line would define a command "door" to insert "door3".
;;;Puts door3 on int layer, color cyan.  Prompt rot.  No scaleup, expl, or mirr.
;;;(defun c:haws-door () (insblk "door3" '("int" "cyan" "") "P" 0))

;;;The following line would define a command "b2" to insert "blk".
;;;B2 inserts blk on bill layer, color y.  0 rot.  Exploded.
;;;(defun c:b2 () (insblk "blk" '("bill" "2" "") "0" 2))

;;;(defun c:b3 () (insblk "blk" '("sally" "r" "") "90" 1))
;;; B3 inserts blk on sally layer, color r.  90 rot.  Scaled up per dimscale.

;;;(defun c:b4 () (insblk "blk" '("joe" "m" "") "45" 3))
;;; B4 inserts blk on joe layer, color m.  45 rot.  Dimscale & exploded.

;;;(defun c:b5 () (insblk "blk" '("sue" "b" "") "P" 5))
;;; B5 inserts blk on sue layer, color b.  Prompt rot.  Dimscaled & mirrored.

;;; Dependencies:
;;; lisputil.lsp Legacy library routines
;;; Layers.dat for commands that give a lyer key instead of name, color, ltype list.
;;; Blocks for respective commands

(defun insblk
  (blname bllay blrot blopt /
  blsclx blscly bllayc blexpl)
  (erdf$@)
  (vsave '("clayer" "cmdecho"))
  (setvar "cmdecho" 0)
  (setq
    blexpl (= 2 (logand blopt 2))
  )
  (cond
    ( (and (= 1 (logand blopt 1)) (= 0 (HAWS-DWGSCALE)))
      (prompt "\nPlease enter dimscale (scale of drawing):")
      (command "dimscale" pause)
    )
    ( (and (= 8 (logand blopt 8)) (= 0 (getvar "dimtxt")))
      (prompt "\nPlease enter dimtxt (plotted text height):")
      (command "dimtxt" pause)
  ) )
  (if bllay (mklayr bllay))
  (prompt (strcat "\nInsertion point for " blname ":"))
  (command "._insert" blname pause)
  (setq blsclx (if (= 1 (logand blopt 1)) (HAWS-DWGSCALE) 1))
  (if (= 8 (logand blopt 8)) (setq blsclx (* blsclx (getvar "dimtxt"))))
  (setq blscly (if (= 4 (logand blopt 4))(* -1 blsclx) blsclx))
  (if (= blrot "P")
    (progn(prompt "\nRotation angle:")(command blsclx blscly pause))
    (command blsclx blscly blrot)
  )
  (cond
    (blexpl 
     (command "._explode" "last")
     (cond
       ((= 16 (logand blopt 16))
        (command "._change" "p" "" "p" "la" (getvar "clayer") "")
       )
     )
    )
  )
  ;;; (if (= (cdr(assoc 0(entget(entlast)))) "TEXT")(command "._._textedit" (entlast)))
  (vrstor)(errrst)(princ)
)
;;;ROTATION note: Enter insertion rotation or "P" to prompt for rotation.
;;;
;;;OPTIONS key: Add up the options needed. 
;;;                                        1=scale block per dimscale
;;;                                        2=explode
;;;                                        4=mirror
;;;                                        8=scale block per dimtxt
;;;                                       16=change sub-entities to given layer if explode
;;;
;;;Hint: Toggle the [INSERT] key off to fill in the blanks for a new command.
;;;
;;;       |COMMAND|               |BLOCK     ||LAYER KEY OR LIST         |ROT||OPTIONS
;;;---------------------------------------------------------------------
(defun c:haws-GB      ()(insblk "gb"        "ANNOMISC"                 "P"  27))
(defun c:haws-GC      ()(insblk "gc"        "ANNOMISC"                 "P"  27))
(defun c:haws-INVL    ()(insblk "invert_l"  "ANNOMISC"                 "P"  27))
(defun c:haws-INVR    ()(insblk "invert_r"  "ANNOMISC"                 "P"  27))
(defun c:haws-LOTEL   ()(insblk "lotel"     "ANNOMISC"                 "P"  27))
(defun c:haws-PAD     ()(insblk "pad"       "ANNOMISC"                 "0"  11))
(defun c:haws-REV     ()(insblk "delta"     "IB-DELTA"                 "0"  27))
(defun c:haws-secb    ()(insblk "secb"      "IB-SEC"                   "P"   3))
(defun C:haws-secl    ()(insblk "secl"      "IB-SEC"                   "P"   3))
(defun c:haws-secr    ()(insblk "secr"      "IB-SEC"                   "P"   3))
(defun c:haws-sect    ()(insblk "sect"      "IB-SEC"                   "P"   3))
(defun c:haws-SLL     ()(insblk "sta_offl"  "ANNOMISC"                 "P"  27))
(defun c:haws-SLOPE   ()(insblk "slope"     "ANNOMISC"                 "P"  27))
(defun c:haws-SLR     ()(insblk "sta_off"   "ANNOMISC"                 "P"  27))
(defun c:haws-SPOTEL  ()(insblk "spotel"    "ANNOMISC"                 "P"  27))
(defun c:haws-TC      ()(insblk "p-tcg-tx"  "ANNOMISC"                 "0"  27))
(defun c:haws-TCELEV  ()(insblk "tcelev"    "ANNOMISC"                 "P"  27))
(defun c:haws-TCELEVL ()(insblk "l-tcelev"  "ANNOMISC"                 "P"  27))
(defun c:haws-TCELEVR ()(insblk "r-tcelev"  "ANNOMISC"                 "P"  27))
;;;End of block insertion definitions