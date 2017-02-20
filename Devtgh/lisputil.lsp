;;; Vintage HawsEDC Lisp library
;;; Utility functions including the error trapper

;;; Atofx extracts a real number from a text string when text before or after
;;; the number matches a give wild card spec.  Requires EXTRACTX.
;;; Type 0 tries to match the wild cards with text preceding a number.
;;; Type 1 tries to match the wild cards with text following a number
;;;Returns 0.0 if search unsuccesful
(defun atofx (s wc opt / x)
  (setq x (cadr (extractx s wc opt)))
  (if x (atof x) 0.0)
)

;;; Distofx extracts a real number from a text string when text before or after
;;; the number matches a give wild card spec.  Requires EXTRACTX.
;;; Type 0 tries to match the wild cards with text preceding a number.
;;; Type 1 tries to match the wild cards with text following a number
;;; Returns nil if search unsuccesful
(defun distofx (s wc opt / x)
  (setq x (cadr (extractx s wc opt)))
  (if x (distof x) nil)
)

(defun dxf (gcode entlst) (cdr (assoc gcode entlst)))

;;; Endstr returns a substring of s starting with the ith to last character
;;; and continuing l characters.
(defun endstr (s i l)
  (substr s (1+(-(max(strlen s)1)i))l)
)

;;; Legacy errdef.
;;; The new standard HawsEDC error trapper in EDCLIB is called HAWS-ERRDEF.
;;; Only legacy routines (not maintained with the HawsEDC set) will call these routines.
;;; Internal error handler function.  Call erdf$@ at the beginning of a routine.
;;; Call errrst at the end to restore old *error* handler.
;;; To restore previous UCS, set a global symbol 'ucsp to non-nil.
;;; To restore another previous UCS, set a global symbol 'ucspp to non-nil.
(defun errdef () (erdf$@))
(DEFUN
   ERDF$@ ()
  (PRINC "\nThank you for using this HawsEDC tool.  See HawsEDC.com for support.")
  ;;Load extended visual lisp functions
  (VL-LOAD-COM)
  (SETQ
    OLDERR
     *ERROR*
    *ERROR*
     STPERR
  ) ;_ end of setq
) ;_ end of defun

;;; Stperr replaces the standard error function.
;;; It sets everything back in case of an error.
(defun stperr (s)
  (cond
    ( (/= s "Function cancelled")
      (princ (strcat "\nTrapped error: " s))
  ) )
  (command)
  (if (= (type f1) (quote FILE))(setq f1(close f1))); Close files
  (if (= (type f2) (quote FILE))(setq f2(close f2)))
  (if (= (type f3) (quote FILE))(setq f3(close f3)))
  (if (= 8 (logand (getvar"undoctl") 8))(command "._undo" "end")); End undo group
  (if (and vrstor vstr) (vrstor))     ; Restore variables to previous values
  (if ucsp (command"._UCS""P"))       ; Restore previous UCS
  (if ucspp (command"._UCS""P"))      ; Restore previous UCS
  (if enm (redraw enm))               ; Redraw work entity
  (if errosm (setvar "osmode" errosm))
  (setq ucsp nil ucspp nil enm nil)
  (if olderr (setq *error* olderr olderr nil))   ; Restore old *error* handler
  (princ)
)
(defun errrst ()
  (setq ucsp nil ucspp nil enm nil f1 nil f2 nil *error* olderr olderr nil)
)
;;; END ERROR HANDLER

;;; Extract used to extract numerical info from a text string.
(defun extract (s / c i prefix number suffix)
   (setq i 0 prefix "" number "" suffix "")
   (repeat (strlen s)
      (setq c (substr s (setq i (1+ i)) 1))
      (cond
         (  (and (wcmatch c "#")
                 (eq suffix ""))
            (setq number (strcat number c)))
         (  (and (eq c "-")
                 (= suffix number "")
                 (wcmatch
                    (substr s (1+ i) 1) "#"))
            (setq number (strcat number c)))
         (  (and (eq c ".")
                 (= suffix "")
                 (wcmatch
                    (substr s (1+ i) 1) "#"))
            (setq number (strcat number c)))
         (  (eq number "")
            (setq prefix (strcat prefix c)))
         (t (setq suffix (strcat suffix c)))))
       (list prefix number suffix)
)

;;; Extractx used to extract numerical info from a text string with extended options.
(defun extractx (s wc opt / c done i pre prei number suf sufi)
  (setq
    i (if(= opt 0) 0 (1+ (strlen s)))
    pre "" number "" suf ""
  )
  (repeat (strlen s)
    (setq
      c (substr s (setq i (if(= opt 0)(1+ i)(1- i))) 1)
      prei (substr s 1 (1- i))
      sufi (substr s (1+ i))
    )
    (cond
      (  (not(wcmatch(if(= opt 0)prei sufi)wc))
         (if(= opt 0)(setq pre (strcat pre c))(setq suf (strcat c suf)))
      )
      (  (and
            (wcmatch c "#")
            (not done)
         )
         (setq number (if (= opt 0)(strcat number c)(strcat c number)))
      )
      (  (and (eq c "-")
              (= number "")
              (not done)
              (wcmatch
                 (substr s (1+ i) 1) "#"))
         (setq number (if (= opt 0)(strcat number c)(strcat c number))))
      (  (and (eq c ".")
              (not done)
              (wcmatch
                 (substr s (1+ i) 1) "#"))
         (setq number (if (= opt 0)(strcat number c)(strcat c number))))
      (  (eq number "")
         (if(= opt 0)(setq pre (strcat pre c))(setq suf (strcat c suf)))
      )
      (  t
         (setq done t)
         (if(= opt 0)(setq suf (strcat suf c))(setq pre (strcat c pre)))
      )
    )
  )
  (if (not (zerop (strlen number)))
      (list pre number suf)
  )
)

;;; ----  GETANGLE WITH DEFAULT PROMPT  ------------------------------------------
(defun getanglex (anchor prmpt curval dflt / input)
  (if (equal curval nil) (setq curval dflt))
  (setq prmpt (strcat prmpt " <" (angtos curval) ">: "))
  (setq input (if anchor (getangle anchor prmpt)(getangle prmpt)))
  (if (equal input nil) (setq curval curval) input)
)
;;; ----  GETDIST WITH DEFAULT PROMPT  -------------------------------------------
(defun getdistx (anchor prmpt curval dflt / input)
  (if (equal curval nil) (setq curval dflt))
  (setq prmpt (strcat prmpt " <" (rtos curval) ">: "))
  (setq input (if anchor (getdist anchor prmpt)(getdist prmpt)))
  (if (equal input nil) (setq curval curval) input)
)
(defun getdn ( / dn)
  (setq
    dn (getvar "dwgname")
  )
  (if
    (wcmatch (strcase dn) "*`.DWG")
    (setq dn (substr dn 1 (- (strlen dn) 4)))
    (if (wcmatch dn "*\\*")(setq dn (substr dn (1+ (strlen (getvar "dwgprefix"))))))
  )
  dn
)

(defun getdnpath ( / dnpath)
  (setq
    dnpath (getvar "dwgname")
  )
  (if
    (wcmatch (strcase dnpath) "*`.DWG")
    (setq
      dnpath (strcat (getvar "dwgprefix") dnpath)
      dnpath (substr dnpath 1 (- (strlen dnpath) 4))
    )
  )
  dnpath
)

(defun getfil (fprmpt fdflt ftype fext / file fname fninp)
  (setq file nil)
  (while
    (not file)
    (setq
      fninp (getstringx fprmpt fninp fdflt)
      fname (strcat fninp "." fext)
    )
    (cond
      ( (and (= (strcase ftype) "W") (findfile fname))
        (initget "Yes No")
        (if (= (getkword "File already exists.  Overwrite?<Y/N>:") "Yes")
          (setq file (open fname ftype))
        )
      )
      (T (setq file (open fname ftype)))
    )
    (if
      (not file)
      (prompt (strcat "Invalid path or filename.  Please try again.\n")
  ) ) )
  (list file fname)
)

;;; ----  GETSTRING WITH DEFAULT PROMPT  -----------------------------------------
(defun getstringx (prmpt curval dflt / input)
  (if (not curval) (setq curval dflt))
  (setq input (getstring T (strcat prmpt " <"  curval ">: ")))
  (if (= input "") curval input)
)

;;; ----  GETINT WITH DEFAULT PROMPT  --------------------------------------------
(defun getintx (prmpt curval dflt / input)
  (if (not curval) (setq curval dflt))
  (setq input (getint (strcat prmpt " <" (itoa curval) ">: ")))
  (if (not input) curval input)
)
;;; ----  GETREAL WITH DEFAULT PROMPT  -------------------------------------------
(defun getrealx (prmpt curval dflt / input)
  (if (not curval) (setq curval dflt))
  (setq input (getreal (strcat prmpt " <" (rtos curval) ">: ")))
  (if (not input) curval input)
)
;;; ----  GETPOINT WITH DEFAULT PROMPT  ------------------------------------------
(defun getpointx (anchor prmpt curval dflt / input)
  (if (equal curval nil) (setq curval dflt))
  (setq prmpt (strcat prmpt
      " <"(rtos(car curval))
      ","(rtos(cadr curval))
      ","(rtos(caddr curval))
      ">: "))
  (setq input (if anchor (getpoint anchor prmpt)(getpoint prmpt)))
  (if (not input) curval input)
)
(setq hawsedcdate(getvar "date"))

;;;MKFLD sub-function makes a field string out of a string.
;;;If format
;;;Usage: (mkfld
;;;         [string to place into a field]
;;;         [uniform field width or field delimiter character]
;;;       )
(defun mkfld (string format / char i mkfld_field mkfld_literal)
  (cond
    ((= (type format) 'STR)
     (setq i 0 mkfld_field "")
     (cond
       ((wcmatch string (strcat "*`" format "*,*\"*"))
        (setq mkfld_literal T mkfld_field "\"")
     ) )
     (while (<= (setq i (1+ i)) (strlen string))
       (setq mkfld_field
         (strcat mkfld_field
           (cond
             ((= (setq char (substr string i 1)) "\"") "\"\"")
             (T char)
           )
         )
       )
     )
     (if mkfld_literal (setq mkfld_field (strcat mkfld_field "\"")))
     (setq mkfld_field (strcat mkfld_field format))
    )
    (T
     (setq mkfld_field string)
     (while (< (strlen(setq mkfld_field(substr mkfld_field 1 format))) format)
       (setq mkfld_field (strcat mkfld_field " "))
     )
    )
  )
  mkfld_field
)
;;; MKLAYR sub-function defines and makes current a layer for another routine.
;;;  Usage: (mklayr (list "laname" "lacolr" "laltyp"))
;;;  Use empty quotes for default color and linetype (eg. (mklay (list "AZ" "" ""))
(defun getlayr ( key / temp)
  (defun getusl (/ rdlin temp)
    (setq temp (findfile"layers.dat"))
    (COND
      (TEMP
       (PROMPT "\nReading layer settings from ")
       (PRINC TEMP)
      )
      ((PROMPT "\nLayer settings file not found.") (EXIT))
    )
    (SETQ
      F3 (OPEN TEMP "r")
      I 0
    )
    (WHILE (SETQ RDLIN (READ-LINE F3))
      (princ "\rReading line ")(princ (setq I (1+ i)))
      (IF (= 'LIST (TYPE (SETQ TEMP (READ RDLIN))))
        (SETQ *HAWS:LAYERS* (CONS TEMP *HAWS:LAYERS*))
      )
    )
    (SETQ F3 (CLOSE F3))
  )
  (if (not *HAWS:LAYERS*)(getusl))
  (cond
    ( (cdr (assoc key *HAWS:LAYERS*)))
    ( T
      (prompt (strcat "\nSettings for \"" key "\" not found in LAYERS.DAT.  Using current layer."))
    )
  )
)
(defun mklayr (laopt / laname lacolr laltyp ltfile)
  (if
    (= 'STR (type laopt))
    (setq laopt (cond ((getlayr laopt)) ('("" "" "")) ))
  )
  (setq laname (car laopt) lacolr (cadr laopt) laltyp (caddr laopt) ltfile "acad")
  (if (not (or (= laltyp "")(tblsearch "LTYPE" laltyp)))
    (progn (command "._linetype" "l" laltyp "acad")(command)(command "._linetype" "l" laltyp "hawsedc")(command))
  )
  (while (not (or (= laltyp "")(tblsearch "LTYPE" laltyp)))
    (alert
      (strcat
        "AutoCAD could not find "
        laltyp
        " linetype in the specified file.\nPlease follow prompts to try a different linetype or file."
      )
    )
    (setq laltyp (getstringx "\nEnter substitute linetype name or enter to try another file" laltyp laltyp))
    (command "._linetype" "l" laltyp ltfile)(command)
    (cond
      ( (not (tblsearch "LTYPE" laltyp))
        (setq ltfile (getfiled (strcat"File for " laltyp " Linetype")"" "LIN" 6))
      )
    )
    (command "._linetype" "l" laltyp ltfile)(command)
  )
  (command "._layer")
  (if(not(tblsearch "LAYER" laname))
    (command "m" laname)
    (command "t" laname "on" laname "u" laname "s" laname)
  )
  (if (/= lacolr "")(command "c"  lacolr ""))
  (if (/= laltyp "")(command "lt" laltyp ""))
  (command "")
  laopt
)

(defun mktext (j i h r s / jx jy)
  (setq
    i (trans (if (= 2 (length i)) (append i '(0.0)) i) 1 0)
    j (if (= j nil) "L" (strcase j))
    jx
    (cond
      ( (wcmatch j "L,BL*,ML*,TL*") 0)
      ( (wcmatch j "C*,?C*") 1)
      ( (wcmatch j "R*,?R*") 2)
      ( (wcmatch j "A*") 3)
      ( (wcmatch j "M*") 4)
      ( (wcmatch j "F*") 5)
    )
    jy
    (cond
      ( (wcmatch j "L,C*,R*,A*,F*") 0)
      ( (wcmatch j "B*") 1)
      ( (wcmatch j "M*") 2)
      ( (wcmatch j "T*") 3)
  ) )
  (entmake
    (list
      (cons 0  "TEXT")
      (cons 1 s)
      (cons 7 (getvar"textstyle"))
      (append '(10) i)
      (cons 40 h)
      (assoc 41 (tblsearch "STYLE" (getvar"textstyle")))
      (cons 50 (+ r (angle '(0.0 0.0 0.0) (getvar "ucsxdir"))))
      (cons 51 (cdr(assoc 50 (tblsearch "STYLE" (getvar"textstyle")))))
      (cons 72 jx)
      (cons 73 jy)
    )
  )
  (setq
    ent (entget(entlast))
    ent (subst (cons 11 i) (assoc 11 ent) ent)
  )
  (entmod ent)
)
(defun mkline (pt1 pt2)
  (setq
    pt1 (if (= 2 (length pt1)) (append pt1 '(0.0)) pt1)
    pt2 (if (= 2 (length pt2)) (append pt2 '(0.0)) pt2)
  )
  (entmake
    (list
      (cons 0 "LINE")
      (append '(10) (trans pt1 1 0))
      (append '(11) (trans pt2 1 0))
) ) )
(setq hawsedcexpdate 2452792);EXPIRES ON JUNE 1, 2003

;;; Read fields from a text string delimited by a field width or a delimiter
;;; character.
;;; Usage: (rdfld
;;;          [field number]
;;;          [string containing fields]
;;;          [uniform field width or field delimiter character]
;;;          [sum of options: 1 (non-numerical character field)
;;;                           2 (unlimited length field at end of string)
;;;          ]
;;;        )
(defun rdfld
  (fldno string fldwid opt / ischr islong i j atomx char literal firstquote)
  (setq ischr (= 1 (logand 1 opt)) islong (= 2 (logand 2 opt)))
  (cond
    ( (= (type fldwid) 'STR)
      (setq i 1 j 0 atomx "")
      (while
        (and
          (/= i fldno)
          (if(> (setq j (1+ j)) 1000)(prompt"\nFields or delimiters missing?") T)
        )
        (if (= (setq char (substr string j 1)) "\"")
          (if (not literal)(setq literal T)(setq literal nil))
        )
        (if (and (not literal)(= (substr string j 1) fldwid))(setq i (1+ i)))
      )
      (while
        (and
          (or (/= (setq char (substr string (setq j (1+ j)) 1))fldwid)literal)
          (<= j (strlen string))
        )
        (cond
          ((= char "\"")
           (if (not literal)(setq literal T)(setq literal nil))
           (if (not firstquote) (setq firstquote T)(setq firstquote nil))
          )
          (T (setq firstquote nil))
        )
        (if (not firstquote)(setq atomx (strcat atomx char)))
      )
    )
    ( T
      (setq atomx
        (substr
          string
          (1+ (* (1- fldno) fldwid))
          (if islong 1000 fldwid)
      ) )
      (cond
        ( (and ischr (not islong))
          (while (= " " (substr atomx 1 1))
            (setq atomx (substr atomx 2))
          )
          (while (= " " (endstr atomx 1 1))
            (setq atomx (substr atomx 1 (1- (strlen atomx))))
          )
      ) )
    )
  )
  (setq atomx (if ischr atomx (distof atomx)))
)

;;; Convert a radian angle to a presentation quality bearing.
(defun rtob (rad au / b i)
  (setq
    b (angtos rad au)
  )
  (if (wcmatch b "*d*") (progn (setq i 0) (while (/= "d" (substr b (setq i (1+ i)) 1)))(setq b (strcat (substr b 1 (1- i)) "%%d" (substr b (1+ i))))))
  (if (wcmatch b "*d#[`.']*") (progn (setq i 0) (while (/= "d" (substr b (setq i (1+ i)) 1)))(setq b (strcat (substr b 1 i) "0" (substr b (1+ i))))))
  (if (wcmatch b "*'#[`.\"]*") (progn(setq i 0) (while (/= "'" (substr b (setq i (1+ i)) 1)))(setq b (strcat (substr b 1 i) "0" (substr b (1+ i))))))
  (setq b
    (cond
      ( (= b "N") "NORTH")
      ( (= b "S") "SOUTH")
      ( (= b "E") "EAST")
      ( (= b "W") "WEST")
      ( b)
    )
  )
)

;;; RTOSTA sub-function converts a real number to a base 100 road station.
(defun rtosta (sta lup / isneg after before)
  (setq
    lup (cond (lup)((getvar "luprec")))
    isneg (minusp sta)
    sta (rtos (abs sta) 2 lup)
  )
  (while (< (strlen sta) (if(= lup 0) 3 (+ lup 4)))
    (setq sta (strcat "0" sta))
  )
  (setq
    after (if (= lup 0) (- (strlen sta) 1) (- (strlen sta) lup 2))
    before (substr sta 1 (1- after))
    after (substr sta after)
  )
  (if isneg (setq before (strcat "-(" before) after (strcat after ")")))
  (strcat before "+" after)
)
(defun vset (vlst)
  (foreach v vlst (if (getvar (car v))(setvar (car v)(cadr v))))
)

(defun vtog (vlst)
  (foreach v vlst
    (princ (strcat "\n" v " toggled to "))
    (setvar v (princ(if (= (getvar v) 0) 1 0)))
  )
  (princ)
)

(defun vsave (vlst)
  (setq vstr '())
  (repeat (length vlst)
    (setq vstr (append vstr (list (list (car vlst) (getvar (car vlst)))))
    vlst (cdr vlst))
  )
)

(defun vrstor ()
  (repeat (length vstr)
    (setvar (caar vstr) (cadar vstr))
    (setq vstr (cdr vstr))
  )
)

;;; This function does a word wrap on a string by cutting the string into
;;; pieces no more than "maxlen" characters long after places where
;;; "char" character is matched.  Leading and trailng spaces and the used break
;;; characters are stripped.
;;; Example: (wrap "Go home, eat dinner, comb, brush, sleep" 15 ",")
;;; Returns  ("Go home" "eat dinner" "comb, brush" "sleep")
(defun wrap (strng1 maxlen char / first i lstrni stripc strips
  strng2 strngi temp wlist)
  (setq
    i 1 char (strcat "`" char) first T
    wlist nil strng2 "" strngi "" lstrni 0
  )
  ;Break strng1 at every break point
  (while (/= "" (substr strng1 i))
    (cond
      (
        ;For every break or at end of string
        (or
          (wcmatch (substr strng1 1 i) (strcat "*" char))
          (= i (strlen strng1))
        )
        (setq
          strngi (substr strng1 1 i)
          strips strngi
          stripc strngi
          strng1 (substr strng1 (1+ i))
          i 1
        )
        ;Strip leading spaces from all but first piece.  Save as strips.
        (if (not first)
          (while (= (substr strips 1 1) " ")(setq strips (substr strips 2)))
        )
        ;Strip break character.  Save as stripc
        (if (wcmatch stripc (strcat "*" char))
          (setq stripc(substr stripc 1 (1- (strlen stripc))))
        )
        ;Add strngi to strng2 if possible, otherwise, call strng2 full.
        (cond
          ;If strng2 is empty set to strips
          ( (= "" strng2) (setq strng2 strips))
          ;else add strngi to strng2 if it fits stripped.
          ( (<= (strlen (setq temp(strcat strng2 stripc))) maxlen)
            (setq strng2 (strcat strng2 strngi))
          )
          (
            (if (wcmatch strng2 (strcat "*" char))
              (setq strng2(substr strng2 1 (1- (strlen strng2))))
            )
            (setq
              wlist (cons strng2 wlist)
              strng2 strips
            )
          )
        )
        (setq
          first nil
        )
      )
      ( T
        (setq
          i (1+ i)
        )
      )
    )
  )
  (reverse (cons strng2 wlist))
)

;;; Functions for oo, selstyle, and le

;;;Selcerob--Selects a certain type of object. Returns entsel list.
(defun selcerob (prmpt serch / e ok)
  (while (not ok)
   (while (not (setq e (entsel prmpt))))
    (setq elst (entget (setq enm (car e))))
    (if(/=  (cdr(assoc 0 elst)) serch)
      (princ (strcat "**Not a " serch ", try again**"))
      (setq ok T)
    )
  )
  e
)

(defun fld (f elst)
  (cdr (assoc f elst))
)

;;; end sub-functions

;(princ "\nHawsEDC legacy utility lisp functions loaded. ")
;;; end utility functions