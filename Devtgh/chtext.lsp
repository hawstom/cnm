;;;   CHTEXT.lsp
;;;   (C) Copyright 1988-1992 by Autodesk, Inc.
;;;
;;;   This program is copyrighted by Autodesk, Inc. and is  licensed
;;;   to you under the following conditions.  You may not distribute
;;;   or  publish the source code of this program in any form.   You
;;;   may  incorporate this code in object form in derivative  works
;;;   provided  such  derivative  works  are  (i.) are  designed and
;;;   intended  to  work  solely  with  Autodesk, Inc. products, and
;;;   (ii.)  contain  Autodesk's  copyright  notice  "(C)  Copyright
;;;   1988-1992 by Autodesk, Inc."
;;;
;;;   AUTODESK  PROVIDES THIS PROGRAM "AS IS" AND WITH  ALL  FAULTS.
;;;   AUTODESK  SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF  MER-
;;;   CHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK,  INC.
;;;   DOES  NOT  WARRANT THAT THE OPERATION OF THE PROGRAM  WILL  BE
;;;   UNINTERRUPTED OR ERROR FREE.
;;;
;;;   by Jan S. Yoder
;;;   09 March  1990
;;;
;;;   REVISIONS
;;;   1.01  22 May 1991  DTP -- Minor bug fixes.
;;;   1.02  18 Jun 1991  JSY, DTP -- Minor bug fixes.
;;;   1.10  28 Sep 1995 Thomas Gail Haws -- Added obliquing angle option.
;;;   1.11  26 Sep 1996 Thomas Gail Haws -- Automated justification option.
;;;   1.12  16 Jan 1998 Thomas Gail Haws -- Changed verification to ssget option.
;;;
;;;--------------------------------------------------------------------------;
;;; DESCRIPTION
;;;   This is a "text processor" which operates in a global manner
;;;   on all of the text entities that the user selects; i.e., the
;;;
;;;   Height, Justification, Location, Rotation, Obliquing angle,
;;;   Style, Text, and Width can all be changed globally or
;;;   individually, and the range of values for a given parameter
;;;   can be listed.
;;;
;;;   The command is called with CHT from the command line at which
;;;   time the user is asked to select the objects to change.
;;;
;;;     Select text to change.
;;;     Select objects:
;;;
;;;   If nothing is selected the message "ERROR: Nothing selected."
;;;   is displayed and the command is terminated.  If more than 25
;;;   entities are selected the following message is displayed while
;;;   the text entities are sorted out from the non-text entities.
;;;   A count of the text entities found is then displayed.
;;;
;;;     Verifying the selected entities -- please wait.
;;;     nnn  text entities found.
;;;     CHText:  Height/Just/Location/Rot/Obl/Style/Text/Undo/Width:
;;;
;;;   A typical example of the prompts you may encounter follows:
;;;
;;;   If you select a single text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;
;;;     CHText:  Height/Just/Location/Rot/Obl/Style/Text/Undo/Width:
;;;     New text height for text entity. <0.08750000>:
;;;
;;;   If you select more than one text entity to change and ask to change
;;;   the height, the prompt looks like this:
;;;
;;;     CHText:  Height/Just/Location/Rot/Obl/Style/Text/Undo/Width:
;;;     Individual/List/<New height for all entities>:
;;;
;;;   Typing "L" at this prompt returns a prompt showing you the range of
;;;   values that you are using for your text.
;;;
;;;     Height -- Min: 0.05000000  Max: 0.10000000  Ave: 0.08392857
;;;
;;;   Typing "I" at this prompt puts you in a loop, processing the text
;;;   entities you have selected one at a time, and giving the same prompt
;;;   you get for a single text entity shown above.
;;;
;;;   Pressing RETURN at this point puts you back at the Command: prompt.
;;;   Selecting any of the other options allows you to change the text
;;;   entities selected.
;;;
;;;   All of the Release 11 text alignment options have been supported.
;;;   This is based on the system variable "DIMCLRD" being present.
;;;   If it is not present, then only the  Release 10 alignment options
;;;   are allowed.
;;;
;;;---------------------------------------------------------------------------;
;;;
;;; Main function -- no arguments
;;;
(defun chtxt (/ sset opt nsset temp unctr ct_ver cht_er cht_oe
                sslen style hgt rot txt ent cht_oc cht_ot cht_oh
                loc loc1 justp justq opt orthom prmpt)

  (setq ct_ver "1.12")                ; Reset this local if you make a change.
  ;; Set undo groups and ends
  (defun cht_UG () (command "_.undo" "_group"))
  (defun cht_ue () (command "_.undo" "_en"))
  ;;
  ;; Internal error handler defined locally
  ;;
  (defun cht_er (s)                   ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
    (if (/= s "Function cancelled")
      (if (= s "quit / exit abort")
        (princ)
        (princ (strcat "\nError: " s))
      )
    )
    (cht_ue)
    (if cht_oe                        ; If an old error routine exists
      (setq *error* cht_oe)           ; then, reset it
    )
    (if temp (redraw temp 1))
    (if cht_oc (setvar "cmdecho" cht_oc)) ; Reset command echoing
    (if cht_ot (setvar "texteval" cht_ot))
    (if cht_oh (setvar "highlight" cht_oh))
    (princ)
  )
  ;;
  ;; Body of function
  ;;
  (if *error*                         ; Set our new error handler
    (setq cht_oe *error* *error* cht_er)
    (setq *error* cht_er)
  )

  (setq cht_oc (getvar "cmdecho"))
  (setq cht_oh (getvar "highlight"))
  (setvar "cmdecho" 0)

  (CHT_UG)

  (princ (strcat "\nChange text, Version " ct_ver
                 ", (c) 1990-1991 by Autodesk, Inc. "))
  (prompt "\nSelect text to change. ")
  (setq sset (ssget '((0 . "*TEXT"))))
  (if (null sset)
    (progn
      (princ "\nERROR: Nothing selected.")
      (exit)
    )
  )
  ;; This is the main option loop.
  (cht_ol)

  (if cht_oe (setq *error* cht_oe))   ; Reset old error function if error
  (cht_UE)
  (if cht_ot (setvar "texteval" cht_ot))
  (if cht_oh (setvar "highlight" cht_oh))
  (if cht_oc (setvar "cmdecho" cht_oc)) ; Reset command echoing
  (princ)
)
;;;
;;; The option loop.
;;;
(defun cht_ol ()
  (setq opt T unctr 0)
  (while (and opt (> (sslength sset) 0))
    (setq unctr (1+ unctr))
    (command "_.UNDO" "_GROUP")
    (initget "Locat Just Style Height Rot Obl Width Text Undo")
    (setq opt (getkword
      "\nHeight/Just/Location/Rot/Obl/Style/Text/Undo/Width: "))
    (if opt
      (cond
        ((= opt "Undo")
          (cht_ue)                    ; Undo the previous command.
        )
        ((= opt "Locat")
          (cht_le)                    ; Change the location.
        )
        ((= opt "Just")
          (cht_je)                    ; Change the justification.
        )
        ((= opt "Style")    (cht_pe "Style"    "style name"       7) )
        ((= opt "Height")   (cht_pe "Height"   "height"          40) )
        ((= opt "Rot")      (cht_pe "Rot"      "rotation angle"  50) )
        ((= opt "Obl")      (cht_pe "Obl"      "obliquing angle" 51) )
        ((= opt "Width")    (cht_pe "Width"    "width factor"    41) )
        ((= opt "Text")
          (cht_te)                    ; Change the text.
        )
      )
      (setq opt nil)
    )
    (command "_.UNDO" "_END")
  )
)
;;;
;;; Undo an entry.
;;;
(defun cht_ue ()
  (if (> unctr 1)
    (progn
      (command "_.UNDO" "_END")
      (command "_.UNDO" "2")
      (setq unctr (- unctr 2))
    )
    (progn
      (princ "\nNothing to undo. ")
      (setq unctr (- unctr 1))
    )
  )
)
;;;
;;; Change the location of an entry.
;;;
(defun cht_le ()
  (setq sslen (sslength sset)
        style ""
        hgt   ""
        rot   ""
        txt   ""
  )
  (command "_.CHANGE" sset "" "")
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          opt (list (cadr (assoc 11 ent))
                    (caddr (assoc 11 ent))
                    (cadddr (assoc 11 ent)))
    )
    (prompt "\nNew text location: ")
    (command pause)
    (if (null loc)
      (setq loc opt)
    )
    (command style hgt rot txt)
  )
  (command)
)
;;;
;;; Change the justification of an entry.
;;;
(defun cht_je ()
  (if (getvar "DIMCLRD")
    (initget (strcat "TLeft TCenter TRight "
                     "MLeft MCenter MRight "
                     "BLeft BCenter BRight "
                     "Aligned Center Fit Left Middle Right ?"))
    (initget "Aligned Center Fit Left Middle Right ?")
  )
  (setq sslen (sslength sset))
  (setq justp (getkword (strcat "\nJustification point(s) - "
              "Aligned/Center/Fit/Left/Middle/Right/<?>: ")))
  (cond
    ((= justp "Left")    (setq justp 0 justq 0) )
    ((= justp "Center")  (setq justp 1 justq 0) )
    ((= justp "Right")   (setq justp 2 justq 0) )
    ((= justp "Aligned") (setq justp 3 justq 0) )
    ((= justp "Fit")     (setq justp 5 justq 0) )
    ((= justp "TLeft")   (setq justp 0 justq 3) )
    ((= justp "TCenter") (setq justp 1 justq 3) )
    ((= justp "TRight")  (setq justp 2 justq 3) )
    ((= justp "MLeft")   (setq justp 0 justq 2) )
    ((= justp "Middle")  (setq justp 4 justq 0) )
    ((= justp "MCenter") (setq justp 1 justq 2) )
    ((= justp "MRight")  (setq justp 2 justq 2) )
    ((= justp "BLeft")   (setq justp 0 justq 1) )
    ((= justp "BCenter") (setq justp 1 justq 1) )
    ((= justp "BRight")  (setq justp 2 justq 1) )
    ((= justp "?")       (setq justp nil)       )
    (T                   (setq justp nil)       )
  )
  (if justp
    (justpt) ; Process them...
    (justpn) ; List options...
  )
  (command)
)

(defun justpt ()
  (while (> sslen 0)
    (setq ent (entget(ssname sset (setq sslen (1- sslen))))
          ent (subst (cons 72 justp) (assoc 72 ent) ent)
          stpt1 (cdr (assoc 10 ent))
    )
    (if (getvar "DIMCLRD")
      (setq ent (subst (cons 73 justq) (assoc 73 ent) ent))
    )
    (cond
      ((or (= justp 3) (= justp 5))
;;;
;;; Get alignment points for "aligned" or "fit" text.
;;;
        (prompt "\nNew text alignment points: ")
        (if (= (setq orthom (getvar "orthomode")) 1)
          (setvar "orthomode" 0)
        )
        (redraw (cdr(assoc -1 ent)) 3)
        (initget 1)
        (setq loc (getpoint))
        (initget 1)
        (setq loc1 (getpoint loc))
        (redraw (cdr(assoc -1 ent)) 1)
        (setvar "orthomode" orthom)
        (setq ent (subst (cons 10 loc) (assoc 10 ent) ent))
        (setq ent (subst (cons 11 loc1) (assoc 11 ent) ent))
      )
      ((or (/= justp 0) (/= justq 0))
;;;
;;; Move text to old start point.
;;;
        (entmod ent)
        (setq ent (entget (cdr (assoc -1 ent)))
              stpt2 (cdr (assoc 10 ent))
              juspt1 (cdr (assoc 11 ent))
              juspt2 (list
                       (+ (car juspt1)(-(car stpt1)(car stpt2)))
                       (+ (cadr juspt1)(-(cadr stpt1)(cadr stpt2)))
                       (+ (caddr juspt1)(-(caddr stpt1)(caddr stpt2)))
                     )
              ent (subst (cons 11 juspt2) (assoc 11 ent) ent))
      )
    )
    (entmod ent)
  )
)

;;;
;;; List the options.
;;;
(defun justpn ()
  (if (getvar "DIMCLRD") (textpage))
  (princ "\nAlignment options: ")
  (princ "\n\t TLeft   TCenter   TRight ")
  (princ "\n\t MLeft   MCenter   MRight ")
  (princ "\n\t BLeft   BCenter   BRight ")
  (princ "\n\t  Left    Center    Right")
  (princ "\n\tAligned   Middle    Fit")
  (if (not (getvar "DIMCLRD")) (textscr))
  (princ "\n\nPress any key to return to your drawing. ")
  (grread)
  (princ "\r                                           ")
  (graphscr)
)
;;;
;;; Change the text of an entity.
;;;
(defun cht_te ()
  (setq sslen (sslength sset))
  (initget "Globally Individually Retype")
  (setq ans (getkword
    "\nSearch and replace text.  [Individually/Retype/Globally] <Globally>:"))
  (setq cht_ot (getvar "texteval"))
  (setvar "texteval" 1)
  (cond
    ((= ans "Individually")
      (if (= (getvar "popups") 1)
        (progn
          (initget "Yes No")
          (setq ans (getkword "\nEdit text in dialogue? <Yes>:"))
        )
        (setq ans "No")
      )

      (while (> sslen 0)
        (redraw (setq sn (ssname sset (setq sslen (1- sslen)))) 3)
        (setq ss (ssadd))
        (ssadd (ssname sset sslen) ss)
        (if (= ans "No")
          (chgtext ss)
          (command "_.DDEDIT" sn "")
        )
        (redraw sn 1)
      )
    )
    ((= ans "Retype")
      (while (> sslen 0)
        (setq ent (entget(ssname sset (setq sslen (1- sslen)))))
        (redraw (cdr(assoc -1 ent)) 3)
        (prompt (strcat "\nOld text: " (cdr(assoc 1 ent))))
        (setq nt (getstring  T "\nNew text: "))
        (redraw (cdr(assoc -1 ent)) 1)
        (if (> (strlen nt) 0)
          (entmod (subst (cons 1 nt) (assoc 1 ent) ent))
        )
      )
    )
    (T
      (chgtext sset)                  ; Change 'em all
    )
  )
  (setvar "texteval" cht_ot)
)
;;;
;;; The old CHGTEXT command - rudimentary text editor
;;;
;;;
(defun c:haws-CHGTEXT ()
(haws-core-init 174) (chgtext nil))

(defun chgtext (objs / last_o tot_o ent o_str n_str st s_temp
                       n_slen o_slen si chf chm cont ans)
  (if (null objs)
    (setq objs (ssget))               ; Select objects if running standalone
  )
  (setq chm 0)
  (if objs
    (progn                   ; If any objects selected
      (if (= (type objs) 'ENAME)
        (progn
          (setq ent (entget objs))
          (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
        )
        (if (= (sslength objs) 1)
          (progn
            (setq ent (entget (ssname objs 0)))
            (princ (strcat "\nExisting string: " (cdr (assoc 1 ent))))
          )
        )
      )
      (setq o_str (getstring "\nMatch string   : " t))
      (if (wcmatch o_str "*``*,*`,*,*`?*,*`@*,*`#*,*`.*,*`~*,*`[*,*`]*")
        (progn(princ "\nThis version of CHT is interpreting your match string as a wildcard expression.")
        (princ "\nYou may make one wildcard character literal by putting a \"`\" before it."))
      )
      (cond
        ( (wcmatch o_str "*``*")
          (princ "\nThis version of CHT will match wildcards with only one escape character (`).")
          (setq o_slen (1- (strlen o_str)))
        )
        (T (setq o_slen (strlen o_str)))
      )
      (if (/= o_slen 0)
        (progn
          (setq n_str (getstring "\nNew string     : " t))
          (setq n_slen (strlen n_str))
          (setq last_o 0
                tot_o  (if (= (type objs) 'ENAME)
                         1
                         (sslength objs)
                       )
          )
          (while (< last_o tot_o)     ; For each selected object...
            (if (= "TEXT"             ; Look for TEXT entity type (group 0)
                   (cdr (assoc 0 (setq ent (entget (ssname objs last_o))))))
              (progn
                (setq chf nil si 1)
                (setq s_temp (cdr (assoc 1 ent)))
                (while (= o_slen (strlen (setq st (substr s_temp si o_slen))))
                  (if (wcmatch st o_str)
                    (progn
                      (setq s_temp (strcat
                                     (if (> si 1)
                                       (substr s_temp 1 (1- si))
                                       ""
                                     )
                                     n_str
                                     (substr s_temp (+ si o_slen))
                                   )
                      )
                      (setq chf t)    ; Found old string
                      (setq si (+ si n_slen))
                    )
                    (setq si (1+ si))
                  )
                )
                (if chf
                  (progn              ; Substitute new string for old
                    ; Modify the TEXT entity
                    (entmod (subst (cons 1 s_temp) (assoc 1 ent) ent))
                    (setq chm (1+ chm))
                  )
                )
              )
            )
            (setq last_o (1+ last_o))
          )
        )
        ;; else go on to the next line...
      )
    )
  )
  (if (/= (type objs) 'ENAME)
    (if (/= (sslength objs) 1)        ; Print total lines changed
      (princ (strcat "Changed "
                     (rtos chm 2 0)
                     " text lines."
             )
      )
    )
  )
  (terpri)
)
;;;
;;; Main procedure for manipulating text entities
;;; ARGUMENTS:
;;;   typ   -- Type of operation to perform
;;;   prmpt -- Partial prompt string to insert in standard prompt line
;;;   fld   -- Assoc field to be changed
;;; GLOBALS:
;;;   sset  -- The selection set of text entities
;;;
(defun cht_pe (typ prmpt FLD / temp ow nw ent tw sty w hw lw
                              sslen n sn ssl)
  (if (= (sslength sset) 1)           ; Special case if there is only
                                      ; one entity selected
    ;; Process one entity.
    (cht_p1)
    ;; Else
    (progn
      ;; Set prompt string.
      (cht_sp)
      (if (= nw "List")
        ;; Process List request.
        (cht_pl)
        (if (= nw "Individual")
          ;; Process Individual request.
          (cht_pi)
          (if (= nw "Select")
            ;; Process Select request.
            (cht_ps)
            ;; Else
            (progn
              (if (or (= typ "Rot") (= typ "Obl"))
                (setq nw (* (/ nw 180.0) pi))
              )
              (if (= (type nw) 'STR)
                (if (not (tblsearch "style" nw))
                  (progn
                    (princ (strcat "\nStyle " nw " not found. "))
                  )
                  (cht_pa)
                )
                (cht_pa)
              )
            )
          )
        )
      )
    )
  )
)
;;;
;;; Change all of the entities in the selection set.
;;;
(defun cht_pa (/ cht_oh temp)
  (setq sslen (sslength sset))
  (setq cht_oh (getvar "highlight"))
  (setvar "highlight" 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (entmod (subst (cons FLD nw)
                   (assoc FLD (setq ent (entget temp)))
                   ent
            )
    )

  )
  (setvar "highlight" cht_oh)
)
;;;
;;; Change one text entity.
;;;
(defun cht_p1 ()
  (setq temp (ssname sset 0))
  (setq ow (cdr(assoc FLD (entget temp))))
  (if (or (= opt "Rot")(= opt "Obl"))
    (setq ow (/ (* ow 180.0) pi))
  )
  (redraw (cdr(assoc -1 (entget temp))) 3)
  (initget 0)
  (if (= opt "Style")
    (setq nw (getstring (strcat "\nNew " prmpt ". <"
                              ow ">: ")))
    (setq nw (getreal (strcat "\nNew " prmpt ". <"
                              (rtos ow 2) ">: ")))
  )
  (if (or (= nw "") (= nw nil))
    (setq nw ow)
  )
  (redraw (cdr(assoc -1 (entget temp))) 1)
  (if (or (= opt "Rot")(= opt "Obl"))
    (setq nw (* (/ nw 180.0) pi))
  )
  (if (= opt "Style")
    (if (null (tblsearch "style" nw))
      (princ (strcat "\nStyle " nw " not found. "))

      (entmod (subst (cons FLD nw)
                     (assoc FLD (setq ent (entget temp)))
                     ent
              )
      )
    )
    (entmod (subst (cons FLD nw)
                   (assoc FLD (setq ent (entget temp)))
                   ent
            )
    )
  )
)
;;;
;;; Set the prompt string.
;;;
(defun cht_sp ()
  (if (= typ "Style")
    (progn
      (initget "Individual List New Select ")
      (setq nw (getkword (strcat "\nIndividual/List/Select style/<New "
                                 prmpt
                                 " for all text entities>: ")))
      (if (or (= nw "") (= nw nil) (= nw "Enter"))
        (setq nw (getstring (strcat "\nNew "
                                    prmpt
                                    " for all text entities: ")))
      )
    )
    (progn
      (initget "List Individual" 1)
      (setq nw (getreal (strcat "\nIndividual/List/<New "
                                 prmpt
                                 " for all text entities>: ")))
    )
  )
)
;;;
;;; Process List request.
;;;
(defun cht_pl ()
  (setq unctr (1- unctr))
  (setq sslen (sslength sset))
  (setq tw 0)
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (if (= typ "Style")
      (progn
        (if (= tw 0)
          (setq tw (list (cdr(assoc FLD (entget temp)))))
          (progn
            (setq sty (cdr(assoc FLD (entget temp))))
            (if (not (member sty tw))
              (setq tw (append tw (list sty)))
            )
          )
        )
      )
      (progn
        (setq tw (+ tw (setq w (cdr(assoc FLD (entget temp))))))
        (if (= (sslength sset) (1+ sslen)) (setq lw w hw w))
        (if (< hw w) (setq hw w))
        (if (> lw w) (setq lw w))
      )
    )
  )
  (if (or (= typ "Rot")(= typ "Obl"))
    (setq tw (* (/ tw pi) 180.0)
          lw (* (/ lw pi) 180.0)
          hw (* (/ hw pi) 180.0))
  )
  (if (= typ "Style")
    (progn
      (princ (strcat "\n"
                     typ
                     "(s) -- "))
      (princ tw)
    )
    (princ (strcat "\n"
                     typ
                     " -- Min: "
                     (rtos lw 2)
                     "\t Max: "
                     (rtos hw 2)
                     "\t Avg: "
                     (rtos (/ tw (sslength sset)) 2) ))
  )
)
;;;
;;; Process Individual request.
;;;
(defun cht_pi ()
  (setq sslen (sslength sset))
  (while (> sslen 0)
    (setq temp (ssname sset (setq sslen (1- sslen))))
    (setq ow (cdr(assoc FLD (entget temp))))
    (if (or (= typ "Rot")(= typ "Obl"))
      (setq ow (/ (* ow 180.0) pi))
    )
    (initget 0)
    (redraw (cdr(assoc -1 (entget temp))) 3)
    (if (= typ "Style")
      (progn
        (setq nw (getstring (strcat "\nNew "
                                   prmpt
                                   ". <"
                                   ow ">: ")))
      )
      (progn
        (setq nw (getreal (strcat "\nNew "
                                   prmpt
                                   ". <"
                                (rtos ow 2) ">: ")))
      )
    )
    (if (or (= nw "") (= nw nil))
      (setq nw ow)
    )
    (if (or (= typ "Rot")(= typ "Obl"))
      (setq nw (* (/ nw 180.0) pi))
    )
    (entmod (subst (cons FLD nw)
                   (assoc FLD (setq ent (entget temp)))
                   ent
            )
    )
    (redraw (cdr(assoc -1 (entget temp))) 1)
  )
)
;;;
;;; Process the Select option.
;;;
(defun cht_ps ()
  (princ "\nSearch for which Style name?  <*>: ")
  (setq sn  (strcase (getstring))
        n   -1
        nsset (ssadd)
        ssl (1- (sslength sset))
        )
  (if (or (= sn "*") (null sn) (= sn ""))
    (setq nsset sset sn "*")
    (while (and sn (< n ssl))
      (setq temp (ssname sset (setq n (1+ n))))
      (if (= (cdr(assoc 7 (entget temp))) sn)
        (ssadd temp nsset)
      )
    )
  )
  (setq ssl (sslength nsset))
  (princ "\nFound ")
  (princ ssl)
  (princ " text entities with STYLE of ")
  (princ sn)
  (princ ". ")
)
;;;
;;; The c:haws- function definition.
;;;
(defun c:haws-cht    ()
(haws-core-init 175) (chtxt))
(princ)
