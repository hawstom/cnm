;#region Header comments
;;; CONSTRUCTION NOTES MANAGER
;;;
;;; PHASING
;;; NOTES allows up to 9 phases named 1 through 9.
;;; To use phases, first build the table block NOTEQTY using TBLQTY1 through TBLQTY9
;;; attributes for NOTES to fill out in columns as it makes the table.
;;; then when drafting, put the phase for each bubble note in the NOTEPHASE attribute
;;; of the bubble note block.  NOTES will read the phase for each bubble note and
;;; put the quantity into the proper column (phase).
;;; NOTES lets you use different phase columns for different sheets in a plan set,
;;; up to a total of 9.  You just have to define the NOTEQTY block the way you want it in
;;; each sheet.
;;; If you aren't using phases, you can use bubble notes with or without a NOTEPHASE attribute
;;; and you can use a TBLQTY attribute for a quantites column in the NOTEQTY block.
;;; You can't use a TBLQTY attribute if you are using multiple phasing.
;;;
;;; (C) Copyright 2004 by Thomas Gail Haws
;;; Revision history
;;;
;;; 20080410 v4.2.05 See HawsEDC list from here on.  Versions now in sync.
;;; 20050831 v4.2.00 Months of work.  Added ini.  Changed project management. Enhanced menus.  Combined with HawsEDC tools.
;;; 20050415 v4.1.19 Fix a bug for M2Group about Titles.  Was adding limitless zeros internally to NOTETITLES, then having error.
;;; 20050413 v4.1.18 Recompiled for M2Group with correct v4.1 ldrblk.lsp. (4.1.16 had some 4.2 functions ref'd) Yes QT VBA call
;;; 20050413 v4.1.17 Recompiled for M2Group with correct v4.1 ldrblk.lsp. (4.1.16 had some 4.2 functions ref'd) No QT VBA call.
;;; 20050412 v4.1.16 Added workaround for AutoCAD 2005 selection set add, remove, add bug to bubredef.
;;; 20050411 v4.1.15 Enhanced bubredef to redefine old names (without 1 and 2 style code).
;;; 20050204 v4.1.14 Added ShowKeyGrid (now ShowKeyTableGrid) and ShowKeyQuantities (now ShowKeyTableQuantities) registry options and layers in NOTEQTYs. Made TITLES behavior better and consistent between KT and QT.
;;; 20040813 v4.1.13 Fixed LDRBLK.LSP and a few bubble block problems so that bubbles work with variable number of text fields and prompts depending on preset attribute flag.
;;; 20040811 v4.1.12 Improved flow and error trapping in new constnot.txt user wizard: What to do/prompt if no constnot.txt is found.
;;; 20040310 v4.1.11 Fixed bug introduced in v4.1.10.  Search and Save now writes string quantities to NOT, but Tally was expecting numbers.  Now Tally reads string quantities.
;;; 20040309 v4.1.10 Fixed 6 significant figure limitation prin1ing atof 1234.567898 by using rtos 2 8.  Max decimal places is 8, but can be increased.
;;; 20040307 v4.1.09 Changed QT (was Tally) File Selection VBA macro.  Now called via USERS1-4 ACAD setvar/getvars
;;; 20040227 v4.1.08b Revert making tables draw from actual upper-left corner instead of text insertion point.  Change prompt to "Start point"
;;; 20040220 v4.1.08a Made Import always show file dialogue box.  Fixed some CTABONLY bugs.  Made layer edit refresh on exit.
;;; 20040208 v4.1.07 Made Tally compatible with VBA File Select form.
;;; 20040108 v4.1.06 Bug fix. If a .NOT was missing a note, tally crashed while using (assoc (notnum ...) shtlst)
;;; 20040108 v4.1.05 Made tables draw from actual upper-left corner instead of text insertion point.
;;; 20040107 v4.1.04 Changed CONSTNOT.TXT location search and added command to change path.  Made SET TXTHT do nothing if missing 3rd field.
;;; 20031125 v4.1.03 Added user choice for CONSTNOT.TXT location. Combined Bubble1 and Bubble2 styles into one package.
;;;                  Changed various command names, spiffed up menus.
;;;                  Temporarily eliminated package choice from protection/licensing scheme.
;;; 20031117 v4.1.02 Made Tally drawing table look more like sheet list table.  Copied alert messages to prompt line.
;;; 20031113 v4.1.01 Tidied up some variables.
;;; 20030916 v4.1.00 Changed .NOT to native NOTELIST format.
;;;                  Added the SET variables CTABONLY, TBLWID, and PHASEWID to CONSTNOT.TXT
;;;                  Relegated all other wid variables to the tally table only
;;;                  Moved the note description into NOTEQTY as an attribute
;;;                  Removed the special drawing name recognition for AGRA style naming.
;;;                  Added call to authorization
;;;                  Improved column wrapping.
;;;                  Added respect for current viewport freeze
;;;                  Added multiple description lines to tally .CSV
;;; 20030805 v4.0.10 Added TRI to the allowed shape types.
;;; 20030501 v4.0.9 Added SET PHASES to CONSTNOT.TXT, warning message for skipped phases, made SET variables space insensitive
;;; 20030417 v4.0.8 Fixed bug in MAKENOTELIST no phasing PHASELIST '("" 0 "") to '("" 1 "").
;;; 20030317 v4.0.7 Added ACAD.PGP write for TALLY wildcard.  Added error alerts.
;;; 20030304 v4.0.6 Debugged option to select TALLY list file.
;;; 20030227 v4.0.5 Added option to select TALLY list file.
;;; 20030220 v4.0.4 Fixed TXTHT.  Wasn't affecting other size variables.  Added "other drawing spaces" check to notes purge.
;;; 20021120 v4.0.3 Fixed listing excess extra descriptions from next type because of failure to recognize end of type
;;; 12/01    v4.0   Added phasing, editor changer
;;; 2/27/01  v3.23  Stopped the perpetuation of non-count items in the table if no bubble note in drawing.
;;; 2000     v3.22  Stopped the counting of frozen, off, and xref bubble notes.
;;; 2000     v3.21  Stopped the counting of table quantities by renaming table attributes.
;;; 2000            Added user size variables to CONSTNOT.TXT
;;; 1999     v3.20  Improved performance by rewriting code.
;#endregion
;#region Table from search
(defun hcnm_getphaselistfromtblqty (/ el en i dsctag noteqtyondisk oldtags
                                phasealias phaselist
                               )
  ;;Check for phasing in qty table block.  Phasing is controlled by presence of TBLQTY? attributes.
  ;;Construct phaselist as '((phase1 1 alias1)(phase2 2 alias2)(phasei i aliasi)).
  ;;Alias=phase for changing later by CNM.INI.
  ;;Phases are numbered in order they appear in block. (This number could be very unstable, but it is the key to phase order on this sheet.)
  ;;
  ;;Insert table line NOTEQTY block if not exist
  (setq
    j (if (= (c:hcnm-config-getvar "InsertTablePhases") "No")
        ""
        (c:hcnm-config-getvar "InsertTablePhases")
      )
  )
  (cond
    ((not (tblsearch "BLOCK" "NOTEQTY"))
     (setvar "attreq" 0)
     (vl-cmdf "._insert" (strcat "NOTEQTY=NOTEQTY" j) "_Scale" "1" "_Rotate" "0" "0,0")
     (setvar "attreq" 1)
     (entdel (entlast))
    )
  )
  ;;Check how many phases are in current block.
  (setq
    en (cdr (assoc -2 (tblsearch "BLOCK" "NOTEQTY")))
    i  0
  )
  (while en
    (setq el (entget en))
    (if (and
          (= "ATTDEF" (cdr (assoc 0 el)))
          (wcmatch (cdr (assoc 2 el)) "TBLQTY?")
        )
      (setq i (1+ i))
    )
    (setq en (entnext en))
  )
  ;;Redefine it if wrong number of phases
  (cond
    ((and
       (/= j "")                        ;Inserting phases requested
       (setq j (atoi j))
       (/= j i)                         ;Wrong number of phases currently inserted
     )
     (vl-cmdf "._insert" (strcat "noteqty=noteqty" (itoa j)))
     (vl-cmdf)
    )
  )
  (setq
    en (cdr (assoc -2 (tblsearch "BLOCK" "NOTEQTY")))
    i  1
  )
  (while en
    (setq el (entget en))
    (cond
      ((and
         (= "ATTDEF" (cdr (assoc 0 el)))
         (wcmatch (cdr (assoc 2 el)) "TBLQTY?")
       )
       (setq
         phaselist
          (cons
            (list
              (substr (cdr (assoc 2 el)) 7 1)
              i
              (substr (cdr (assoc 2 el)) 7 1)
            )
            phaselist
          )
         i (1+ i)
       )
      )
      ((and
         (= "ATTDEF" (cdr (assoc 0 el)))
         (= "NOTETYPE" (cdr (assoc 2 el)))
       )
       (setq oldtags t)
      )
      ((and
         (= "ATTDEF" (cdr (assoc 0 el)))
         (= "TBLDSC" (cdr (assoc 2 el)))
       )
       (setq dsctag t)
      )
    )
    (setq en (entnext en))
  )
  (setq phaselist (reverse phaselist))
  (if (not phaselist)
    (setq phaselist '(("" 1 "")))
  )
  ;;Add phasealias configs to the phaselist.
  (mapcar
    '(lambda (phase)
       (setq
         phaselist
          (subst
            (reverse
              (cons
                (c:hcnm-config-getvar (cadr phase))
                (cdr (assoc (itoa (car phase)) phaselist))
              )
            )
            (assoc (itoa (car phase)) phaselist)
            phaselist
          )
       )
     )
    '((1 "PhaseAlias1")
      (2 "PhaseAlias2")
      (3 "PhaseAlias3")
      (4 "PhaseAlias4")
      (5 "PhaseAlias5")
      (6 "PhaseAlias6")
      (7 "PhaseAlias7")
      (8 "PhaseAlias8")
      (9 "PhaseAlias9")
     )
  )
  (cond
    ((or oldtags (not dsctag))
     (vl-cmdf
       "._insert"
       (strcat
         "noteqty="
         (setq
           noteqtyondisk
            (findfile
              (strcat
                "noteqty"
                (if (= (caar phaselist) "")
                  "0"
                  (itoa (length phaselist))
                )
                ".dwg"
              )
            )
         )
       )
     )
     (vl-cmdf)
     (alert
       (princ
         (strcat
           "The NOTEQTY block in this drawing had the wrong attributes for this version of Construction Notes Manager."
           "\nor it was missing the description text attribute, TBLDSC."
           "\n\nConstruction Notes Manager tried to fix the problem by inserting\n"
           noteqtyondisk " from disk."
           "\n\nIf results are still not satisfactory, please edit the drawing\n"
           noteqtyondisk
           "\nto meet your needs and include the following attributes (in order):"
           "\n\nTBLTYPE\nTBLNUM\nTBLDSC\nTBLQTY\nTBLUNT"
          )
       )
     )
    )
  )
  phaselist
)
;;; SEARCHANDSAVE reads through CONSTNOT.TXT and lists all notes and counting instructions.
;;; Also checks NOTEQTY block for version and phases
;;;
;;; Section 1.
;;;
;;; Constructs a notelist in the following format:
;;;   <----phaselist------->
;;;    '(((phasej j aliasj)...)((typi(notenumi txtlinesi qtyopti qtyi1 qtyij)...)...))
;;; as '(((phasej j aliasj)...)((typi(notenumi txtlinesi qtyopti nil  )...)...))
;;; If TBLQTY is the only quantity attribute in the NOTEQTY block, the phaselist is '("" 1 "").
;;;
;;; Section 2.
;;;
;;; Then it searches through all non-dependent block insertions in drawing that have attributes
;;; and records the appropriate presence and quantities in notelist.
;;; It then searches through the qty table in the drawing
;;; and records its quantities for any notes without counting instructions.
;;; Fills notelist in the following format:
;;;   <----phaselist------->
;;;    '(((phasej j aliasj)...)((typi(notenumi qtyopti qtyi1[nil if not found] qtyij[nil if not found])...)...))
;;; Then saves all the notes and quantities for drawing in file nfname.
;;;
;;;Set up list from CONSTNOT.TXT and NOTEQTY block.
(defun hcnm_key_table_searchandsave (dn projnotes / aliaslist at attributes av blki
                                   blkss count ctabonly el en et i j
                                   mvport mvsset n nfname notefnd notei
                                   notelines notelist notenum notephase
                                   noteqty notetxt notetype notnum
                                   nottyp phase phaselist qtyopt
                                   skippedphases usrvar vplayers x
                                  )
  ;;
  ;; Section 1.  Make an empty NOTELIST from tblqty and constnot.txt.  TGHI can use this section for Tally, except there is a conflict in the way they do PHASELIST.
  ;;
  (setq
    phaselist
     (hcnm_getphaselistfromtblqty)
    ctabonly
     (= "1" (c:hcnm-config-getvar "DoCurrentTabOnly"))
    nottyp ""
  )
  (foreach
     entry *hcnm_cnmprojectnotes*
    (cond
      ;;If it's a note
      ((= 3 (car entry))
       ;;make a new type entry if necessary
       (cond
         ((/= nottyp (setq nottyp (cadr entry)))
          (setq notelist (cons (list nottyp) notelist))
         )
       )
       ;;and add the note to the list.
       (setq
         notnum
          (caddr entry)
         qtyopt
          (nth 4 entry)
         i 0
         notelines
          (length (nth 6 entry))
       )
       (setq
         notelist
          (subst
            (reverse
              (cons
                (append
                  (list notnum notelines qtyopt)
                  (mapcar '(lambda (phase) nil) phaselist)
                                        ;Add a nil for each phase
                )
                (reverse (assoc nottyp notelist))
              )
            )
            (assoc nottyp notelist)
            notelist
          )
       )
      )
    )
  )
  (setq notelist (append (list phaselist) (list notelist)))
  ;;
  ;; Section 2.  Get quantities from bubble notes and save to file
  ;;
  ;; Make a list of all layers frozen in current viewport.
  (setq
    count 0
    mvsset
     (ssget "X" (list (cons 0 "VIEWPORT")))
  )
  (if mvsset
    (while (setq mvport (ssname mvsset count))
      (setq mvport (entget mvport '("ACAD")))
      (cond
        ((and
           (= (getvar "CTAB") (cdr (assoc 410 mvport)))
                                        ;Viewport is on current tab
           (= (getvar "Cvport") (cdr (assoc 69 mvport)))
                                        ;Viewport has current viewport number
           (assoc 1003 (cdadr (assoc -3 mvport)))
                                        ;Viewport has vp frozen layers
         )
         (foreach
            dxfgroup (cdadr (assoc -3 mvport))
           (cond
             ((= 1003 (car dxfgroup))
              (setq vplayers (cons (cdr dxfgroup) vplayers))
             )
           )
         )
        )
      )
      (setq count (1+ count))
    )
  )
  ;;Get bubbles selection set
  (setq
    blkss
     (ssget "X" (list (cons 0 "INSERT")))
    i -1
  )
  ;;Remove frozen and off blocks, frozen in current viewport,
  ;;xrefs, and xref dependent blocks from the set
  ;;Remove all blocks not in current space if CTABONLY = 1.
  (while (and blkss (setq blki (ssname blkss (setq i (1+ i)))))
    (if
      (or
        (= 1
           (logand
             1
             (cdr
               (assoc
                 70
                 (tblsearch "LAYER" (cdr (assoc 8 (entget blki))))
               )
             )
           )
        )
        (minusp
          (cdr
            (assoc 62 (tblsearch "LAYER" (cdr (assoc 8 (entget blki)))))
          )
        )
        (= 4
           (logand
             4
             (cdr
               (assoc
                 70
                 (tblsearch "BLOCK" (cdr (assoc 2 (entget blki))))
               )
             )
           )
        )
        (= 16
           (logand
             16
             (cdr
               (assoc
                 70
                 (tblsearch "BLOCK" (cdr (assoc 2 (entget blki))))
               )
             )
           )
        )
        ;;On a layer frozen in the current viewport
        (member (cdr (assoc 8 (entget blki))) vplayers)
        ;;Not in space of current tab if only doing ctab
        (and
          ctabonly
          (/= (cdr (assoc 410 (entget blki))) (getvar "CTAB"))
        )
      )
       (setq
         blkss
          (ssdel blki blkss)
         i (1- i)
       )
    )
  )
  ;;Search through bubble notes and add their quantities to NOTELIST
  (setq
    i -1
    aliaslist
     (mapcar '(lambda (phase) (reverse phase)) (car notelist))
  )
  (while (and blkss (setq blki (ssname blkss (setq i (1+ i)))))
    (setq
      en blki
      notetype
       (cond
         ((lm:getdynpropvalue
            (vlax-ename->vla-object en)
            "Shape"
          )
         )
         (t nil)
       )
      notenum nil
      notetxt
       '(0 1 2 3 4 5 6 7 8 9)
      notephase ""
      attributes (hcnm_get_attributes en nil)
    )
    ;;Substitute the value of each NOTETXT attribute for its respective member of the pre-filled NOTETXT list.
    (setq
      notenum
       (cadr (assoc "NOTENUM" attributes))
      notetxt
       (mapcar
         '(lambda (i)
            (cadr (assoc (strcat "NOTETXT" (itoa i)) attributes))
          )
         '(0 1 2 3 4 5 6 7 8 9)
       )
      notephase
       (cadr (assoc "NOTEPHASE" attributes))
      notei
       (assoc notenum (cdr (assoc notetype (cadr notelist))))
    )
    (cond
      ;;If there is such a note and phase, or no phasing is being used.
      ((and
         notei
         (setq
           n (if (= (caar aliaslist) "") ;If no phasing
               1                        ;the quantity will be 4th atom in list '(notei txtlines qtyopt qty1) (N=3)
               (cadr (assoc notephase aliaslist))
             )
         )
       )
       ;;get the quantity as instructed
       (setq
         n (+ n 2)
         qtyopt
          (caddr notei)
         noteqty
          (cond
            ((nth n notei))             ;If there's already a quantity growing, use it.
            (0.0)                       ;Otherwise add a real zero.
          )
       )
       (cond
         ((= qtyopt "COUNT") (setq noteqty (1+ noteqty)))
         ((wcmatch qtyopt "LINE#")
          (setq
            noteqty
             (+ noteqty
                (atof
                  (cadr
                    (haws-extract
                      (cond
                        ((nth (atoi (substr qtyopt 5 1)) notetxt)
                        )
                        ("0")
                      )
                    )
                  )
                )
             )
          )
         )
         ((= qtyopt "") (setq noteqty ""))
       )
       ;;Add quantity to notelist
       (setq j -1)
       (setq
         notelist
          (list
            (car notelist)
            (subst
              (subst
                (mapcar                 ;Substitute NOTEQTY for the Nth of NOTEI
                  '(lambda (x)
                     (if (= (setq j (1+ j)) n)
                       noteqty
                       x
                     )
                   )
                  notei
                )
                notei
                (assoc notetype (cadr notelist))
              )
              (assoc notetype (cadr notelist))
              (cadr notelist)
            )
          )
       )
      )
      ;;If there isn't such a phase, note it in SKIPPEDPHASES
      (notei
       (if (or (not notephase) (= notephase ""))
         (setq notephase "<none>")
       )
       (if (not skippedphases)
         (setq skippedphases '(0 ""))
       )
       (setq
         skippedphases
          (list
            (1+ (car skippedphases))
            (if (not
                  (wcmatch notephase (cadr skippedphases))
                )
              (cond
                ((= "" (cadr skippedphases)) notephase)
                (t
                 (strcat
                   (cadr skippedphases)
                   ","
                   notephase
                 )
                )
              )
              (cadr skippedphases)
            )
          )
       )
      )
    )
  )
  ;;After searching bubbles for presence and quantities,
  ;;get quantities from qty table if no counting instructions
  (setq
    blkss
     (ssget "X" (list (cons 2 "NOTEQTY")))
    i -1
  )
  (while (and blkss (setq blki (ssname blkss (setq i (1+ i)))))
    (cond
      (;;If only doing ctab and table block is not in current tab, do nothing
       (and
         ctabonly
         (/= (cdr (assoc 410 (entget blki))) (getvar "CTAB"))
       )
      )
      (t
       (setq en blki)
       ;;Get the table quantities from each tblqty attribute,
       ;;and put them into the NOTEQTY variable
       ;;as '(("phase" "qty")...)
       (setq noteqty nil)
       (while
         (and
           (setq en (entnext en))
           (= "ATTRIB" (setq et (cdr (assoc 0 (setq el (entget en))))))
         )
          (setq
            at (cdr (assoc 2 el))
            av (cdr (assoc 1 el))
          )
          (cond
            ((= at "TBLTYPE") (setq notetype av))
            ((= at "TBLNUM") (setq notenum av))
            ((wcmatch at "TBLQTY*")
             (setq noteqty (cons (list (substr at 7 1) av) noteqty))
            )
          )
       )
       (setq
         notei
          (assoc notenum (cdr (assoc notetype (cadr notelist))))
       )
       ;;If there aren't any counting instructions given for note, check if it was found
       ;;then put its quantities for all phases in NOTELIST.
       (cond
         ((and
            ;;No counting instructions for note
            (= "" (caddr notei))
            ;;and note found at least once in allowed phases
            (progn
              (foreach
                 phase aliaslist
                (if (nth (+ 2 (cadr phase)) notei)
                  (setq notefnd t)
                )
              )
              notefnd
            )
          )
          ;;Reset the found flag
          (setq notefnd nil)
          ;;Insert the quantities from NOTEQTY into NOTELIST
          (setq
            notelist
             (list
               (car notelist)           ;Phaselist
               (subst
                 (subst
                   (cons
                     (car notei)
                     (cons
                       (cadr notei)
                       (cons
                         (caddr notei)
                         ;;Get the table quantities stored in NOTEQTY
                         (mapcar
                           '(lambda (x)
                              (cond
                                ((cadr
                                   (assoc (caddr x) noteqty)
                                 )
                                )
                                (0.0)
                              )
                            )
                           (car notelist)
                         )
                       )
                     )
                   )
                   (assoc
                     notenum
                     (cdr (assoc notetype (cadr notelist)))
                   )
                   (assoc notetype (cadr notelist))
                 )
                 (assoc notetype (cadr notelist))
                 (cadr notelist)
               )
             )
          )
         )
       )
      )
    )
  )
  ;;Now that NOTELIST is filled, alert user if any phases in bubbles were skipped.
  (if skippedphases
    (alert
      (princ
        (strcat
          "\nThese unexpected phase names: \""
          (cadr skippedphases)
          "\" were found and skipped.\n"
          (itoa (car skippedphases))
          " blocks in total were not counted.\n\nClick OK to continue."
        )
      )
    )
  )
  ;;Save notelist to file
  (setq
    nfname
     (strcat
       dn
       (if ctabonly
         (strcat "-" (getvar "CTAB"))
         ""
       )
       ".not"
     )
    f2 (open nfname "w")
  )
  ;;Write NOTELIST to the work file
  (princ "(" f2)
  (prin1 (car notelist) f2)
  (princ "(" f2)
  (foreach
     nottyp (cadr notelist)
    (princ "(" f2)
    (prin1 (car nottyp) f2)
    (foreach
       notnum (cdr nottyp)
      (princ "(" f2)
      (prin1 (car notnum) f2)
      (prin1 (cadr notnum) f2)
      (prin1 (caddr notnum) f2)
      (foreach
         noteqty (cdddr notnum)
        (cond
          ((= (type noteqty) 'STR) (prin1 noteqty f2))
          (noteqty (prin1 (rtos noteqty 2 8) f2))
          ((princ "nil " f2))
        )
        f2
      )
      (princ ")" f2)                    ;End of notnum
    )
    (princ ")" f2)                      ;End of nottyp
  )
  (princ "))" f2)                       ;End of (cadr noteqty) and noteqty
  (setq
    f2 (close f2)
       ;;Close notes file for this drawing (program work file)
  )
  (princ
    (strcat
      "\nUsed Project Notes at " projnotes
      "\nSaved notes and quantities in " nfname "."
     )
  )
)
;;MAKENOTETABLE reads NOTELIST from file nfname and makes a table of notes and quantities.
;;Uses the qty block.
;;Puts table at qtypt.
;; TGH to use this for TALLY, maybe I just need to read NOTELIST as an argument instead of from a file in this function.
(defun hcnm_key_table_make (nfsource qtypt qtyset dn txtht / ctabonly icol
                        iphase column_height note_first_line_p
                        column_height_pending nfname notdsc notelist
                        notesmaxheight notetitles notnum notqty nottyp
                        notunt numfnd phaselist prompteachcol qty qtypt1
                        rdlin txthttemp typfnd usrvar
                       )
  (setq phaselist (hcnm_getphaselistfromtblqty))
  (setvar "osmode" 0)
  (setvar "attreq" 1)
  (initget "Prompt")
  (setq
    notesmaxheight
     (haws-getdistx
       qtypt
       "Maximum height of each notes column"
                                        ; or [Prompt for each column]"
       notesmaxheight
       9999.0
     )
  )
  (cond
    ((= notesmaxheight "Prompt")
     (setq
       prompteachcol t
       notesmaxheight 9999.0
     )
     (alert
       "The option to prompt for each column is not yet operational."
     )
    )
  )
  (setq ctabonly (= (c:hcnm-config-getvar "DoCurrentTabOnly") "1"))
  (if (= nfsource "E")
    (setq
      nfname
       (cond
         (ctabonly
          (findfile (strcat dn "-" (getvar "CTAB") ".not"))
         )
         (t (findfile (strcat dn ".not")))
       )
    )
    (setq nfname (getfiled "Select Drawing and Layout" "" "NOT" 0))
  )
  (setq
    f1 (open nfname "r")
    notelist
     (read (read-line f1))
    f1 (close f1)
  )
  ;;Check that we got a valid NOTELIST from file.
  (if (/= (type notelist) 'LIST)
    (alert
      (strcat
        "\nThe file"
        nfname
        "appears to be out of date.\nIt doesn't have valid information to make a notes table.\n\nPlease search and save notes again."
      )
    )
  )
  ;;All prompts done.  Let's make table!
  (hcnm_projinit)                       ;Initialize project after user pauses
  (hcnm_readcf (hcnm_projnotes))
  (setq
    linspc
     (atof (c:hcnm-config-getvar "LineSpacing"))
    notspc
     (atof (c:hcnm-config-getvar "NoteSpacing"))
    tblwid
     (atof (c:hcnm-config-getvar "TableWidth"))
    phasewid
     (atof (c:hcnm-config-getvar "PhaseWidthAdd"))
    icol 1
    column_height 0
    iphase 1
    qtypt1 qtypt
  )
  ;;Check that the right NOTEQTY block is inserted.
  (if (or (/= (length phaselist) (length (car notelist)))
                                        ;Wrong number of current phases
          (and                          ;or counted 1 phase, but current block has no phasing
            (= 1 (length (car notelist)))
            (= 1 (atof (caaar notelist)))
            (/= 1 (atof (caar phaselist)))
          )
          (and                          ;or counted no phasing, but current block has it
            (= 1 (length (car notelist)))
            (/= 1 (atof (caaar notelist)))
            (= 1 (atof (caar phaselist)))
          )
      )
    (progn
      (vl-cmdf
        "._insert"
        (strcat
          "noteqty=noteqty"
          (car (nth notelist (1- (length notelist))))
        )
      )
      (vl-cmdf)
    )
  )
  (vl-cmdf "._undo" "_group")
  (if qtyset
    (vl-cmdf "._erase" qtyset "")
  )
  (foreach
     entry *hcnm_cnmprojectnotes*
    (cond
      ;;If it's a variable config, set it.
      ((= 1 (car entry))
       (setq usrvar (cadr entry))
       (cond
         ((and (= "TXTHT" usrvar) (setq usrvar (caddr entry)))
          (setq
            txtht
             (* (haws-dwgscale)
                (cond
                  ((distof usrvar))
                  ((getvar "dimtxt"))
                )
             )
          )
         )
       )
      )
      ;;If its a title, save it for future use.
      ;;If a number intervened (found or not) since last titles
      ;;and added a zero to front of NOTETITLES, clear them first.
      ;;Note: Titles are meant to serve for any notes found until the next titles
      ;;If you want to use titles by shape, you can, but CNM doesn't know.
      ((= 2 (car entry))
       (setq
         notetitles
          (cons
            (list txtht (caddr entry))
            ;; If clear titles flag (a note came between this title and the last),
            ;; start titles fresh.
            (if (= 0 (car notetitles))
              nil
              notetitles
            )
          )
         nottyp
          (cadr entry)
       )
      )
      ;;If it's a note number,
      ;;flag any NOTETITLES as complete with a 0.
      ;;If it is found in NOTELIST, write it with quantities
      ;;and any pending titles to the table.
      ;;
      ;; STYLE GUIDE
      ;; LINSPC is the spacing/height of a line from its own top to bottom.
      ;; NOTSPC is the spacing above each note or group of titles.
      ;; (- NOTSPC LINSPC) is the additional space above a note or group of titles.
      ;; Insertion point is vertically (y coordinate) at the middle of each line.
      ((and
         (= 3 (car entry))
         (if (and notetitles (/= 0 (car notetitles)))
           (setq notetitles (cons 0 notetitles))
           t
         )
         (setq
           notnum
            (assoc
              (caddr entry)
              (cdr
                (assoc (setq nottyp (cadr entry)) (cadr notelist))
              )
            )
         )
         (progn
           (setq numfnd nil)
           (foreach
              phase (cdddr notnum)
             (if phase
               (setq numfnd t)
             )
           )
           numfnd
         )
       )
       (setq
         column_height_pending 0
         notunt
          (cadddr entry)
         notqty
          ;;Convert quantities to strings, preserving input precision for all quantities
          ;;Trim extra zeros from quantities
          (mapcar
            '(lambda (qty)
               (while (wcmatch qty "*.*0,*.")
                 (setq qty (substr qty 1 (1- (strlen qty))))
               )
               qty
             )
            ;;Turn quantities into strings
            (mapcar
              '(lambda (qty)
                 (cond
                   ((= (type qty) 'STR) qty)
                   ((= (type qty) 'REAL) (rtos qty 2 8))
                   (t "")
                 )
               )
              (cdddr notnum)
            )
          )
         notetitles
          (cdr notetitles)              ;If note was found, unflag and write titles.
       )
       ;; Calculate height of titles plus a paragraph space
       (cond
         (notetitles
          (setq
            txthttemp txtht
            i_title 0
          )
          (foreach
             notetitle (reverse notetitles)
            (setq txtht (car notetitle))
            (cond
              ((= i_title 0)
               (cond
                 ;; At top, rewind before first title
                 ((= column_height 0)
                  (setq
                    column_height_pending
                     (+ column_height_pending
                        (* -0.5 txtht linspc)
                     )
                  )
                 )
                 ;; Else add a paragraph space before first title
                 (t
                  (setq
                    column_height_pending
                     (+ column_height_pending
                        (* txtht (- notspc linspc))
                     )
                  )
                 )
               )
              )
            )
            ;; Space for each title
            (setq
              column_height_pending
               (+ column_height_pending
                  (* txtht linspc)
               )
              i_title
               (1+ i_title)
            )
          )
          (setq txtht txthttemp)
         )
       )
       ;; Calculate height of note
       (cond
         ;; At top, rewind before note
         ((and (not notetitles) (= column_height 0))
          (setq
            column_height_pending
             (+ column_height_pending
                (* -0.5 txtht linspc)
             )
          )
         )
         ;; Else add a paragraph space before note
         (t
          (setq
            column_height_pending
             (+ column_height_pending
                (* txtht (- notspc linspc))
             )
          )
         )
       )
       (setq
         column_height_pending
          ;; Add note height
          (+ column_height_pending
             (* (cadr notnum) (* txtht linspc))
          )
       )
       ;; Add titles and note
       ;; If titles _and note_ won't fit and column isn't empty, advance to new column
       (cond
         ((and
            (> (+ column_height_pending column_height) notesmaxheight)
                                        ; Won't fit
            (/= column_height 0)        ; Not first note in column
          )
          (hcnm_key_table_advance_column)
         )
       )
       ;; Add any titles
       (cond
         (notetitles
          (setq
            txthttemp txtht
            i_title 0
          )
          (foreach
             notetitle (reverse notetitles)
            (setq txtht (car notetitle))
            ;; If not first note, space appropriately
            (cond
              ((/= column_height 0)     ; Not first note in column
               (cond
                 ;; Add a paragraph space above first title based on its height
                 ((= i_title 0)
                  (hcnm_key_table_advance_down (* 0.5 (- notspc linspc)))
                 )
               )
               (hcnm_key_table_advance_down (* 0.5 linspc))
              )
            )
            (cond
              ((= (c:hcnm-config-getvar "ShowKeyTableTitleShapes") "1")
               (hcnm_key_table_insert_shape)
              )
            )
            (setq notdsc (cadr notetitle))
            (hcnm_key_table_insert_text)
            (hcnm_key_table_advance_down (* 0.5 linspc))
          )
          (hcnm_key_table_advance_down (* 0.5 (- notspc linspc)))
          (setq txtht txthttemp)
         )
       )
       ;; If note won't fit in new column with titles, advance column again.
       (cond
         (;; Titles were added
          (/= column_height 0)
          (cond
            (;; Note won't fit after titles.
             (> (+ column_height_pending column_height) notesmaxheight)
             (hcnm_key_table_advance_column)
            )
            (;; Note will fit after titles.
             t
             ;; Paragraph spacing
             (hcnm_key_table_advance_down (* 0.5 (- notspc linspc)))
             ;; Down to middle of first note
             (hcnm_key_table_advance_down (* 0.5 linspc))
            )
          )
         )
       )
       ;; Now add note
       (setq note_first_line_p t)
       (hcnm_key_table_insert_shape)
       (hcnm_key_table_advance_down (* -0.5 linspc))
       (foreach
          notdsc (nth 6 entry)
         (hcnm_key_table_advance_down (* 0.5 linspc))
         (hcnm_key_table_insert_text)
         (hcnm_key_table_advance_down (* 0.5 linspc))
         (setq
           notetitles nil
           note_first_line_p nil
         )
       )
       (hcnm_key_table_advance_down (* 0.5 (- notspc linspc)))
      )
    )
  )
  ;;Apply table display configs from ini.  If no configs (legacy), show both.
  (mapcar
    '(lambda (layerkey / layershow layerlist)
       (setq layershow (/= "0" (c:hcnm-config-getvar (cadr layerkey))))
       (cond
         (layershow (haws-mklayr (car layerkey)))
         (t
          (setq
            layerlist
             (tblsearch
               "LAYER"
               (car (haws-getlayr (car layerkey)))
             )
          )
          ;; If thawed and on, freeze
          (if (and
                (cdr (assoc 70 layerlist))
                (/= 1 (logand 1 (cdr (assoc 70 layerlist))))
                (< 0 (cdr (assoc 62 layerlist)))
              )
	            (vl-cmdf "._layer" "_f" (cdr (assoc 2 layerlist)) "")
          )
         )
       )
     )
    '(("NOTESKEYGRID" "ShowKeyTableGrid")
      ("NOTESKEYQTYS" "ShowKeyTableQuantities")
     )
  )
  (vl-cmdf "._undo" "_end")
)




(defun hcnm_key_table_advance_column ()
  (setq
    column_height 0
    icol
     (1+ icol)
    qtypt
     (polar
       qtypt1
       0
       (* (1- icol)
          (+ (* txtht tblwid)
             (* txtht phasewid (1- (length phaselist)))
          )
       )
     )
  )
)

(defun hcnm_key_table_advance_down (space / down_height)
  (setq
    down_height (* space txtht)
    qtypt
     (polar qtypt (* pi -0.5) down_height)
    column_height
     (+ column_height down_height)
    column_height_pending
     (- column_height_pending down_height)
  )
)

(defun hcnm_key_table_insert_shape ()
  (vl-cmdf
    "._insert"
    (strcat "cnm" nottyp)
    "_Scale"
    txtht
    "_Rotate"
    "0"
    qtypt
  )
)

(defun hcnm_key_table_insert_text ()
;;;All this stuff is to make the attribute order insensitive.
;;;    (setvar "attreq" 0)
;;;       (VL-CMDF
;;;  "._insert" "NOTEQTY" "non"   
;;;  QTYPT
;;;    "_Scale"
;;;    TXTHT
;;;    "_Rotate"
;;;    "0"
;;; )
;;;      ;;Change attribute values
;;;     (SETQ EN (ENTLAST))
;;;    (WHILE (AND
;;;  (SETQ EN (ENTNEXT EN))
;;;  (/= "SEQEND" (CDR (ASSOC 0 (SETQ EL (ENTGET EN)))))
;;;       )
;;;  (COND
;;;    ((= "ATTRIB" (CDR (ASSOC 0 EL)))
;;;     (SETQ
;;;       ATAG
;;;     (CDR (ASSOC 2 EL))
;;;       AVAL
;;;     (COND
;;;       ((= ATAG "TBLTYPE") NOTTYP)
;;;       ((= ATAG "TBLNUM") (CAR NOTNUM))
;;;       ((= ATAG "TBLDSC") NOTDSC)
;;;       ((= ATAG "DELTA")
;;;        (VL-STRING-SUBST "%%d" "d" (ANGTOS DELTA 1 4))
;;;       )
;;;       ((= ATAG "CHORD") (RTOS (* 2 RAD (SIN DOVER2)) 2 2))
;;;       ((= ATAG "TANGENT")
;;;        (RTOS (* RAD (/ (SIN DOVER2) (COS DOVER2))) 2 2)
;;;       )
;;;       ((= ATAG "BEARING")
;;;        (VL-STRING-SUBST "%%d" "d" (ANGTOS BEARING 4 4))
;;;       )
;;;     )
;;;     )
;;;     (ENTMOD (SUBST (CONS 1 AVAL) (ASSOC 1 EL) EL))
;;;     (ENTUPD EN)
;;;    )
;;;  )
;;;    )
  (vl-cmdf
    "._insert"
    "NOTEQTY"
    "_Scale"
    txtht
    "_Rotate"
    "0"
    qtypt
    (if note_first_line_p
      nottyp
      ""
    )
    (if note_first_line_p
      (car notnum)
      ""
    )
    notdsc
  )
  (foreach
     x notqty
    (vl-cmdf
      (if note_first_line_p
        x
        ""
      )
    )
  )
  (vl-cmdf
    (if note_first_line_p
      notunt
      ""
    )
  )
)

;;HCNM_KEY_TABLE_FROM_SEARCH
;;In the NOTES strategy, this routine is first of three main routines.
;;Gets project info from CONSTNOT.TXT
;;Gets drawing info from bubbles or table.
;;Saves all in .NOT file for other two routines
(defun hcnm_key_table_from_search (dn projnotes txtht linspc tblwid phasewid /
                           el en i notelist qtypt qtyset tablespace
                          )
  (setq
    qtyset
     (ssget "X" (list (cons 8 (car (haws-mklayr "NOTESEXP")))))
  )
  (cond
    (qtyset
     (setq
       i (if (c:haws-icad-p)
           1
           (sslength qtyset)
         )
     )
     (while (setq en (ssname qtyset (setq i (1- i))))
       (setq el (entget en))
       (cond
         ((or (= (getvar "CTAB") (setq tablespace (cdr (assoc 410 el))))
              (and (= "Model" tablespace) (< 1 (getvar "cvport")))
              (c:haws-icad-p)             ;If we are in intellicad, which doesn't have the tab information in the entget data
          )
          (if (not qtypt)
            (setq
              qtypt
               (trans
                 (cdr
                   (cond
                     ((assoc 11 el))
                     ((assoc 10 el))
                   )
                 )
                 0
                 1
               )
            )
          )
         )
         (t (ssdel (cdr (assoc -1 el)) qtyset))
       )
     )
    )
  )
  (if (not qtypt)
    (setq qtypt (getpoint "\nStart point for key notes table: "))
  )
  (hcnm_key_table_searchandsave dn projnotes)
  ;;Make a new notes table
  (hcnm_key_table_make "E" qtypt qtyset dn txtht)
)
;#endregion
;#region Table from import
;;HCNM_IMPORT
;;In the NOTES strategy, this routine is second of three main routines.
;;Reads from .NOT file, created by HCNM_KEY_TABLE_FROM_SEARCH, everything necessary and creates a table. 
(defun hcnm_import (dn projnotes txtht linspc tblwid phasewid / el en i qtypt
               qtyset tablespace
              )
  (setq
    qtyset
     (ssget "X" (list (cons 8 (car (haws-mklayr "NOTESIMP")))))
  )
  (cond
    (qtyset
     (setq
       i (if (c:haws-icad-p)
           1
           (sslength qtyset)
         )
     )
     (while (setq en (ssname qtyset (setq i (1- i))))
       (setq el (entget en))
       (cond
         ((or (= (getvar "CTAB") (setq tablespace (cdr (assoc 410 el))))
              (and (= "Model" tablespace) (< 1 (getvar "cvport")))
              (c:haws-icad-p)             ;If we are in intellicad, which doesn't have the tab information in the entget data
          )
          (if (not qtypt)
            (setq
              qtypt
               (trans
                 (cdr
                   (cond
                     ((assoc 11 el))
                     ((assoc 10 el))
                   )
                 )
                 0
                 1
               )
            )
          )
         )
         (t (ssdel (cdr (assoc -1 el)) qtyset))
       )
     )
    )
  )
  (if (not qtypt)
    (setq
      qtypt
       (getpoint "\nStart point for imported key notes table: ")
    )
  )
  ;;Make a new notes table after erasing qtyset
  (hcnm_key_table_make "I" qtypt qtyset dn txtht)
)
;#endregion
;#region Tally
;;HCNM_TALLY
;;In the NOTES strategy, this routine is the third of three main routines.
;;Reads from a group of .NOT files everything necessary to create a list of total quantities for job.
;;Reads CONSTNOT.TXT to put the .NOT files in order.


;;1. Build an empty phase checklist PHASELIST '(("1" 1 nil) (phasej j nil)...("9" 9 nil)).
;;2. Fill PHASELIST with phase aliases from CONSTNOT.TXT.
;;2. Read NOTELIST from each .NOT file and combine with sheet name into a master QTYLIST.
;;   '((cons shtnoi notelisti)(...))
;;2. Fill PHASELIST list from .NOTs by putting each phase from each sheet into the list if not already there.
;;7. FILL QTYLIST note by note, sheet by sheet, filling the full number of phase qty positions
;;   for all notes, and using (cadr phaselisti) to know which position in qtylist to
;;   put the qtys.  Use "" for any unused phases on a sheet.
;;   '((shti (typj (notek qty1 qty2 qtyk))))
(defun hcnm_tally (dn projnotes txtht linspc tblwid phasewid / allnot
                  all_sheets_quantities col1x column dqwid el
                  flspec i input ndwid notdesc notetitles note_first_line_p
                  notnum notprice notqty notspc nottyp notunt numfnd
                  numlist pgp_defines_run pgp_filename pgp_file_contents
                  pgp_file_line phase phasenumi phases_definition pt1z q
                  qqwid qtypt1 qtyset quwid row1y sheet_filename
                  sheet_filenames sheet_file_name sheet_headings sheet_list_filename
                  sheet_list_line sheet_quantities tablespace total
                  txthttemp usrvar writelist x y z
                 )
;;;
;;;  Section 1.
;;;  Determine list of drawings to tally.
;;;
  (cond
    ((and
       (or (setq sheet_list_filename (findfile (strcat dn ".lst")))
           (setq
             sheet_list_filename
              (findfile (strcat (getvar "DWGPREFIX") "tally.lst"))
           )
       )
       (= "Yes"
          (progn
            (initget 1 "Yes No")
            (getkword
              (strcat
                "\nKeep and use existing list file, \""
                sheet_list_filename
                "\"? <Yes/No>: "
              )
            )
          )
       )
     )
    )
    (t
     (prompt
       (strcat
         "\n\nHow will you specify drawings to tally?"
         "\nUse a text file you have prepared with a List of drawings, "
         "\n(CNM will automatically use tally.lst if present), "
         "\nenter Wildcards (eg. * or grad\\unit1*), "
         "\nor Select drawings one at a time from a dialogue box?"
        )
     )
     (initget 1 "List Wildcards Select")
     (setq
       input
        (getkword "\n[List file/Wildcards/Select one at a time]: ")
     )
     (cond
       ((= input "List")
        (setq sheet_list_filename (getfiled "Select a List File" dn "LST" 0))
       )
       ((= input "Wildcards")
        ;;Add function to user's ACAD.PGP to shell and wait for attrib command to finish.
        (setq
          sheet_list_filename
           (strcat dn ".lst")
          pgp_filename
           (findfile "acad.pgp")
          f1 (open pgp_filename "r")
        )
        (while (setq pgp_file_line (read-line f1))
          (if (= "RUN," (substr pgp_file_line 1 4))
            (setq pgp_defines_run t)
          )
          (if (= "SH," (substr pgp_file_line 1 3))
            (setq
              pgp_file_contents
               (cons
                 "RUN,       cmd /c,         0,*Batch file to run: ,"
                 pgp_file_contents
               )
            )
          )
          (setq pgp_file_contents (cons pgp_file_line pgp_file_contents))
        )
        (setq f1 (close f1))
        (if (not pgp_defines_run)
          (progn
            (setq
              f1     (open pgp_filename "w")
              pgp_file_contents (reverse pgp_file_contents)
            )
            (foreach pgp_file_line pgp_file_contents (write-line pgp_file_line f1))
            (setq f1 (close f1))
            (setvar "re-init" 16)
          )
        )
        (while (not column)
          (setq
            flspec
             (getstring
               t
               "\nFiles to tally using OS wildcards (eg. * or grad\\*): "
             )
          )
          (vl-cmdf
            "run"
            (strcat "attrib \"" flspec ".not\" > \"" sheet_list_filename "\"")
          )
          (setq
            f1 (open sheet_list_filename "r")
            sheet_filename
             (read-line f1)
            column
             (strlen sheet_filename)
          )
          (cond
            ((wcmatch sheet_filename "* not found *")
             (setq
               column nil
               f1 (close f1)
             )
             (alert
               (princ
                 (strcat
                   "The operating system could not find\nany files found matching the wildcard:\n\n "
                   flspec
                   ".not\n\nPlease try again."
                 )
               )
             )
            )
            (t
             (while (not
                      (and
                        (wcmatch
                          (strcase (substr sheet_filename column))
                          (strcase (strcat flspec "`.NOT"))
                        )
                        (or (= "\\" (substr sheet_filename (1- column) 1))
                            (= "\\" (substr sheet_filename column 1))
                            (= ":" (substr sheet_filename (1+ column) 1))
                        )
                      )
                    )
               (setq column (1- column))
             )
             (setq
               f1     (close f1)
               f1     (open sheet_list_filename "r")
               sheet_filenames nil
             )
             (while (setq sheet_filename (read-line f1))
               (setq
                 sheet_filename
                  (substr sheet_filename column)
                 sheet_filenames
                  (cons
                    (substr sheet_filename 1 (- (strlen sheet_filename) 4))
                    sheet_filenames
                  )
               )
             )
             (setq
               f1 (close f1)
               f1 (open sheet_list_filename "w")
             )
             (setq sheet_filenames (reverse sheet_filenames))
             (foreach sheet_filename sheet_filenames (write-line sheet_filename f1))
             (setq f1 (close f1))
            )
          )
        )
       )
       ((= input "Select")
        (setq
          sheet_list_filename
           (strcat dn ".lst")
          f1 (open sheet_list_filename "w")
        )
        (while (setq
                 sheet_filename
                  (getfiled
                    "File to tally (Cancel when Finished)"
                    ""
                    "NOT"
                    6
                  )
               )
          (write-line (substr sheet_filename 1 (- (strlen sheet_filename) 4)) f1)
        )
        (setq f1 (close f1))
       )
     ) ;_ end cond
    )
  )
  ;;Build an empty phase list of (phase number alias).  
  ;;The reason we do this instead of just adding the
  ;;new phases as they come is to avoid sorting the list when we're done.
  ;;In other words, this list is nothing but a definition of the presentation order for phases.
  (setq
    phases_definition
     '(("" 0 nil)
       ("1" 1 nil)
       ("2" 2 nil)
       ("3" 3 nil)
       ("4" 4 nil)
       ("5" 5 nil)
       ("6" 6 nil)
       ("7" 7 nil)
       ("8" 8 nil)
       ("9" 9 nil)
       ("A" 10 nil)
       ("B" 11 nil)
       ("C" 12 nil)
       ("D" 13 nil)
       ("E" 14 nil)
       ("F" 15 nil)
       ("G" 16 nil)
       ("H" 17 nil)
       ("I" 18 nil)
       ("J" 19 nil)
       ("K" 20 nil)
       ("L" 21 nil)
       ("M" 22 nil)
       ("N" 23 nil)
       ("O" 24 nil)
       ("P" 25 nil)
       ("Q" 26 nil)
       ("R" 27 nil)
       ("S" 28 nil)
       ("T" 29 nil)
       ("U" 30 nil)
       ("V" 31 nil)
       ("W" 32 nil)
       ("X" 33 nil)
       ("Y" 34 nil)
       ("Z" 35 nil)
      )
  )
;;;
;;;  Section 2.
;;;  Read all .NOT's into a master ALL_SHEETS_QUANTITIES
;;;  Add phases from all .NOTs to the list if not already there.  And if aliases in conflict, alert user.
;;;  
  (setq f1 (open sheet_list_filename "r"))
  (princ "\n")
  (while (and (setq sheet_list_line (read-line f1)) (/= "" sheet_list_line))
    ;;Read in this sheet's notelist '( ((alias number phase)) ((type1 (notenum txtlines countmethod qty1...))))
    ;;Alert user of possible incompatibility with old-style list.
    (setq
      sheet_file_name
       (cond
         ((findfile sheet_list_line))
         ((findfile (strcat sheet_list_line ".not")))
         (t
          (alert
            (princ
              (strcat
                "The file \"" sheet_list_line "\" listed in \"" sheet_list_filename
                "\" cannot be found.\nConstruction Notes Manager cannot continue."
               )
            )
          )
         )
       )
      f2 (open sheet_file_name "r")
      sheet_quantities
       (read (read-line f2))
      all_sheets_quantities
       (cons (cons sheet_file_name sheet_quantities) all_sheets_quantities)
    )
    (if (read-line f2)
      (alert
        (princ
          (strcat
            "Error:  Sheet quantities file for "
            sheet_file_name
            " is out of date.\nPlease search and save quantities again."
          )
        )
      )
    )
    (setq f2 (close f2))
    ;;Set all phases discovered.
    ;;In .NOT files, phases are ("alias" order "number"), but here they are ("number" order "alias")
    (foreach
       phase (car sheet_quantities)
      (cond
        ;;If its alias is not yet in PHASES_DEFINITION, add the phase.
        ;;The reason we substitute instead of just adding the
        ;;new phases as they come is to avoid sorting the list when we're done.
        ((not (caddr (assoc (caddr phase) phases_definition)))
         (setq
           phases_definition
            (subst
              ;;Substitute the alias for the nil.
              (subst
                (car phase)
                nil
                (assoc (caddr phase) phases_definition)
              )
              (assoc (caddr phase) phases_definition)
              phases_definition
            )
         )
        )
        ;;If alias in PHASES_DEFINITION isn't same as alias in this sheet, alert user.
        ((/= (caddr (assoc (car phase) phases_definition)) (caddr phase))
         (alert
           (princ
             (strcat
               sheet_quantities
               " is trying to assign alias \""
               (caddr phase)
               "\" to phase \""
               (car phase)
               "\", which already has alias \""
               (caddr (assoc (car phase) phases_definition))
               "\".\n\nGrouping alias \""
               (caddr phase)
               "\" on this sheet with phase \""
               (car phase)
               "\", alias \""
               (caddr (assoc (car phase) phases_definition))
               "."
             )
           )
         )
        )
      )
    )
  )
  (setq f1 (close f1))
  ;;Condense list to standard PHASES_DEFINITION format: '((phasej j aliasj)...)
  ;;and renumber for only sheets being tallied.
  (setq i 0)
  (foreach
     phase phases_definition
    (if (caddr phase)
      (setq x (cons (list (car phase) (setq i (1+ i)) (caddr phase)) x))
    )
  )
  (setq phases_definition (reverse x))
;;;
;;;  Section 3.
;;;  Write requested totals to drawing and sheet-by-sheet quantities to dwg.csv.
;;;
  (initget "All Used")
  (setq
    allnot
     (= (getkword
          "\nList which notes from CONSTNOT.TXT? All/Used: "
        )
        "All"
     )
    qtypt1
     (cond
       ((and
          (setq
            qtyset
             (ssget
               "X"
               (list
                 (cons 8 (car (haws-mklayr "NOTESTAL")))
               )
             )
          )
          (setq
            el (entget
                 (ssname
                   qtyset
                   (if (c:haws-icad-p)
                     0
                     (1- (sslength qtyset))
                   )
                 )
               )
          )
          (or (= (getvar "CTAB")
                 (setq tablespace (cdr (assoc 410 el)))
              )
              (and (< 1 (getvar "cvport")) (= tablespace "Model"))
              (c:haws-icad-p)
          )
        )
        (trans
          (cdr
            (cond
              ((assoc 11 el))
              ((assoc 10 el))
            )
          )
          0
          1
        )
       )
       (t
        (getpoint "\nStart point for quantity take-off table: ")
       )
     )
  )
  (hcnm_projinit)                       ;Initialize project after user pauses
  (hcnm_readcf (hcnm_projnotes))
  (setq
    linspc
     (atof (c:hcnm-config-getvar "LineSpacing"))
    notspc
     (atof (c:hcnm-config-getvar "NoteSpacing"))
    tblwid
     (atof (c:hcnm-config-getvar "TableWidth"))
    phasewid
     (atof (c:hcnm-config-getvar "PhaseWidthAdd"))
    col1x
     (car qtypt1)
    row1y
     (cadr qtypt1)
    pt1z
     (caddr qtypt1)
    x col1x
    y row1y
    z pt1z
    ;;width from middle of number to left point of description text
    ndwid
     (atof (c:hcnm-config-getvar "NumberToDescriptionWidth"))
    ;;width from left point of description text to right point of quantity
    dqwid
     (atof (c:hcnm-config-getvar "DescriptionToQuantityWidth"))
    ;;width from right point of one quantity phase to right point of next quantity phase
    qqwid
     (atof (c:hcnm-config-getvar "QuantityToQuantityWidth"))
    ;;width from right point of quantity to left point of unit
    quwid
     (atof (c:hcnm-config-getvar "QuantityToUnitsWidth"))
  )
  (setvar "osmode" 0)
  ;;Write column headings to the file
  (setq f2 (haws-file-open (strcat dn ".csv") "w"))
  (princ "TYPE,NO,ITEM,UNIT,PRICE," f2) ;; Price and cost
  (setq sheet_headings "")
  (foreach
     sheet_quantities all_sheets_quantities
    (foreach
       phase phases_definition
      (setq
        sheet_headings
         (strcat
           sheet_headings
           (haws-mkfld
             (strcat
               (strcase (car sheet_quantities))
               (if (= (car phase) "")
                 " (SINGLE PHASE)"
                 " PHASE "
               )
               (caddr phase)
             )
             ","
           )
         )
      )
    )
  )
  (princ sheet_headings f2)
  (foreach
     phase phases_definition
    (princ
      (strcat
        "TOTAL"
        (if (= (car phase) "")
          " (SINGLE PHASE)"
          " PHASE "
        )
        (caddr phase)
        ",COST"
         (if (= (car phase) "")
          " (SINGLE PHASE)"
          " PHASE "
        )
        (caddr phase)
        ","
     )
      f2
    )
  )
  (write-line "" f2)
  (if qtyset
    (vl-cmdf "._erase" qtyset "")
  )
  ;;For each line in project file
  (foreach
     entry *hcnm_cnmprojectnotes*
    (cond
      ;;If it's a config setting, set it.
      ((= 1 (car entry))
       (setq usrvar (cadr entry))
       (cond
         ((and (= "TXTHT" usrvar) (setq usrvar (caddr entry)))
          (setq
            txtht
             (* (haws-dwgscale)
                (cond
                  ((distof usrvar))
                  ((getvar "dimtxt"))
                )
             )
          )
         )
       )
      )
      ;;If its a title, save it for future use.
      ;;If a number intervened since last titles, clear them first.
      ((= 2 (car entry))
       (setq
         notetitles
          (cons
            (list txtht (caddr entry))
            ;; If clear titles flag (a note came between this title and the last)
            ;; or nottyp has changed, clear titles.
            (if (= 0 (car notetitles))
              nil
              notetitles
            )
          )
         nottyp
          (cadr entry)
       )
      )
      ;;If it's a note number,
      ;;flag the NOTETITLES as complete with a 0.
      ;;If it is found in the qty lst,
      ;;get and add the quantities from qty list
      ;;and add the note with quantities to the table.
      ((and
         (= 3 (car entry))
         (if (and notetitles (/= 0 (car notetitles)))
           (setq notetitles (cons 0 notetitles))
           t
         )
         (setq
           ;; Price and cost
           notprice
            (nth 5 entry)
           nottyp
            (cadr entry)
           notnum
            (caddr entry)
         )
         (or allnot
             (setq
               numfnd nil
               numfnd
                (foreach
                   sheet_quantities all_sheets_quantities
                  (foreach
                     phasei (cdddr
                              (assoc
                                notnum
                                (cdr (assoc nottyp (caddr sheet_quantities)))
                              )
                            )
                    (if phasei
                      (setq numfnd notnum)
                    )
                  )
                  numfnd
                )
             )
         )
       )
       ;;If note was found, unflag and write titles.
       (cond
         (notetitles
          (setq txthttemp txtht)
          (foreach
             notetitle (reverse (cdr notetitles))
            (setq txtht (car notetitle))
            (setq x col1x)
            (if (/= (cadr notetitle) "")
              (haws-mktext "ML" (list x y z) txtht 0 (cadr notetitle))
            )
            (setq y (- y (* txtht linspc)))
            (write-line (cadr notetitle) f2)
          )
          (setq
            y     (- y (* txtht (- notspc linspc)))
            txtht txthttemp
          )
         )
       )
       ;;Print most note info to both drawing and file.
       ;;Print unit to file before quantities (because lots of columns), but wait in drawing 'til after quantities.
       ;;
       ;;Insert shape block
       (setq x col1x)
       (setq y (- y (/ (* txtht linspc) 2)))
       (vl-cmdf
         "._insert"
         (strcat "cnm" nottyp)
         "_Scale"
         txtht
         "_Rotate"
         "0"
         (list x y z)
       )
       ;;Make number text
       (haws-mktext "M" (list x y z) txtht 0 notnum)
       (setq
         notetitles nil
         notunt
          (cadddr entry)
       )
       ;;Print the quantity for each phase from each sheet to file, and increment the total.
       (setq
         x (+ x (* txtht (- (+ ndwid dqwid) qqwid)))
         writelist
          (list nottyp notnum (nth 6 entry) notunt notprice)
         ;;Initialize running totals for each phase '(qty price)
         notqty
          (mapcar '(lambda (x) (list 0 0)) phases_definition)
       )
       (foreach
          sheet_quantities all_sheets_quantities
         (setq
           notqty
            (mapcar
              '(lambda (x)
                 (setq
                   total
                    (car (nth (1- (cadr x)) notqty))
                   ;;Get the current total from notqty
                   q
                    (cond
                      ((and
                         ;;If the current sheet has the current phase
                         (setq
                           phasenumi
                            (cadr
                              (assoc
                                (car x)
                                (cadr sheet_quantities)
                              )
                            )
                         )
                         ;;and if the current sheet has the current note
                         (setq
                           numlist
                            (assoc
                              notnum
                              (cdr
                                (assoc
                                  nottyp
                                  (caddr sheet_quantities)
                                )
                              )
                            )
                         )
                         ;;and if the quantity isn't nil,
                         (setq q (nth (+ 2 phasenumi) numlist))
                       )
                       ;; use its numeric conversion
                       (atof q)
                      )
                      (0)
                    )
                   total
                    (+ total q)
                 )
                 (setq
                   writelist
                    (reverse
                      (cons
                        (haws-prin1-to-string q)
                        (reverse writelist)
                      )
                    )
                 )
                 (list total (* total (atof notprice))) ;Price and cost 2020-12
               )
              phases_definition
            )
         )
       )
       ;;convert quantities and costs to strings, preserving quantities input precision.
       (setq
         notqty ; List of qty and price for each phase.
          (mapcar
            '(lambda (phase / qty_string)
               (setq
                 qty_string
                  (rtos (car (nth (1- (cadr phase)) notqty)) 2 8) ;Price and cost 2020-12
               )
               (while (wcmatch qty_string "*.*0,*.")
                 (setq qty_string (substr qty_string 1 (1- (strlen qty_string))))
               )
               (list qty_string (rtos (cadr (nth (1- (cadr phase)) notqty)) 2 2)) ;Price and cost 2020-12
             )
            phases_definition
          )
       )
       ;;Print totals to drawing and file.
       (mapcar
         '(lambda (phase)
            (setq x (+ x (* txtht qqwid)))
            ;; Quantity total for phase
            (haws-mktext
              "MR"
              (list x y z)
              txtht
              0
              (car (nth (1- (cadr phase)) notqty))
            )
            (setq
              writelist
               (reverse
                 (cons
                   (car (nth (1- (cadr phase)) notqty))
                   (reverse writelist)
                 )
               )
            )
            ;; Cost total for phase
            (setq
              writelist
               (reverse
                 (cons
                   (cadr (nth (1- (cadr phase)) notqty))
                   (reverse writelist)
                 )
               )
            )
          )
         phases_definition
       )
       ;;Write unit to drawing
       (setq x (+ x (* txtht quwid)))
       (if (/= notunt "")
         (haws-mktext "ML" (list x y z) txtht 0 notunt)
       )
       (setq
         x         (+ col1x (* txtht ndwid))
         note_first_line_p t
       )
       (foreach
          notdsc (nth 6 entry)
         (if (/= notdsc "")
           (haws-mktext "ML" (list x y z) txtht 0 notdsc)
         )
         (setq y (- y (* txtht linspc)))
       )
       (setq y (- y (* txtht (- notspc linspc))))
       ;;Write note to file.
       (foreach
          x writelist
         (if (= (type x) 'LIST)
           (progn
             (setq notdesc "")
             (foreach y x (setq notdesc (strcat notdesc "\n" y)))
             (princ (haws-mkfld (substr notdesc 2) ",") f2)
           )
           (princ (strcat x ",") f2)
         )
       )
       (setq writelist nil)
       (write-line "" f2)
      )
    )
  )
  (setq f2 (close f2))
  (prompt
    (strcat "\nUsed project notes file found at " projnotes)
  )
)
;#endregion
;#region CNM Main
;;CNM main commands
(defun c:hcnm-cnm ()
(haws-core-init 179) (hcnm_cnm nil)(haws-core-restore))
(defun c:hcnm-cnmkt ()
  (haws-core-init 180)
  (prompt (strcat "\n" (haws_cnm_evangel_msg)))
  (hcnm_cnm "Search")
  (haws-core-restore))
(defun c:hcnm-cnmkti ()
(haws-core-init 181) (hcnm_cnm "Import")(haws-core-restore))
(defun c:hcnm-cnmqt ()
(haws-core-init 338) (hcnm_cnm "Tally")(haws-core-restore))
;;CNM main function
(defun hcnm_cnm (opt / cfname dn linspc phasewid tblwid txtht)
  ;;Main function
  (haws-vsave
    '("attdia" "attreq" "clayer" "osmode")
  )
  (setvar "attdia" 0)
  (cond
    ((not opt)
     (prompt
       "\nConstruction Notes Manager searches, saves, and lists notes and quantities from attributed bubble notes."
     )
     (prompt
       "\nConstruction Notes Manager can also import the notes and quantities list into this or another tab or drawing."
     )
     (prompt
       "\nConstruction Notes Manager tallies quantities from several drawings previously searched and saved."
     )
     (prompt "\nSee www.ConstructionNotesManager.com")
     (initget "Search Import Tally")
     (setq
       opt
        (getkword
          "\nSearch notes and make table/Import table/Tally drawings: "
        )
     )
    )
  )
  (hcnm_projinit)                       ;Initialize after pauses
  ;;Set user's desired dimstyle.
  (hcnm_set_dimstyle "NotesKeyTableDimstyle")
  (setq
    dn (haws-getdnpath)
    projnotes
     (hcnm_projnotes)
    txtht
     (* (getvar "dimtxt") (haws-dwgscale))
    ;;Column and line spacing widths (half width for middle justified columns)
    ;;line spacing
    linspc
     (atof (c:hcnm-config-getvar "LineSpacing"))
    ;;width of single sheet table with only one phase
    tblwid
     (atof (c:hcnm-config-getvar "TableWidth"))
    ;;width for each extra phase on single sheet table.
    phasewid
     (atof (c:hcnm-config-getvar "PhaseWidthAdd"))
  )
  (hcnm_readcf projnotes)
  (cond
    ((= opt "Search")
     (hcnm_key_table_from_search
       dn projnotes txtht linspc tblwid phasewid
      )
    )
    ((= opt "Import")
     (hcnm_import dn projnotes txtht linspc tblwid phasewid)
    )
    ((= opt "Tally")
     (hcnm_tally dn projnotes txtht linspc tblwid phasewid)
    )
  )
  ;;Restore old dimstyle
  (hcnm_restore_dimstyle)
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)
;;;
;;;End of CNM
;;;

;#endregion
;#region Project Management
;;;================================================================================================================
;;;
;;; Begin Project Management functions
;;;
;;;================================================================================================================

;;
;;HCNM_PROJINIT initializes the CNM project variables
;;because there is good reason to believe they need to
;;be checked again (a pause for user input or a new user command)
;;All the functions assume if they are present they are valid.
;;
(defun hcnm_projinit ()
  (setq
    *hcnm_config* nil
    *hcnm_cnmprojectroot* nil
    *hcnm_cnmprojectnotes* nil
  )
)


;;Does nothing but strcat, since the existence of the file
;;is validated by (HCNM_PROJ)
(defun hcnm_ini_name (proj)
  (hcnm_project_folder_to_ini proj)
)

;; HCNM_PROJ gets a valid project root folder for this drawing's folder.
;; While it returns the folder only, that folder is qualified to have CNM.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\projroot" path to other functions.
(defun hcnm_proj (/ dwgdir linked_project_folder linked_project_marker
              local_project_folder local_project_marker
             )
  (setq
    dwgdir
     (haws-filename-directory (getvar "dwgprefix"))
    local_project_marker
     (hcnm_local_project_marker dwgdir)
    linked_project_marker
     (hcnm_linked_project_marker dwgdir)
  )
  (cond
    (local_project_marker
     (setq
       local_project_folder
        (hcnm_assure_local_project
          local_project_marker
        )
     )
    )
  )
  (cond
    (linked_project_marker
     (setq
       linked_project_folder
        (hcnm_assure_linked_project
          linked_project_marker
        )
     )
    )
  )
  (setq
    *hcnm_cnmprojectroot*
     (cond
       ;;If project is already defined this session, use it.
       ;;(Assume it's valid.  Calling function should init project if there's been a chance of change or loss by user.)
       (*hcnm_cnmprojectroot*)
       ((and
          local_project_marker
          linked_project_marker
        )
        (hcnm_error_ambiguous_project_markers
          local_project_folder
          linked_project_folder
        )
       )
       ;;Else well-formed simple (single-folder) projects. CNM.INI is here.
       (local_project_marker
        (hcnm_assure_local_project
          local_project_marker
        )
       )
       ;;Well-formed complex (multi-folder) projects.  CNMPROJ.TXT is here and
       ;;we'll make sure it really points to a CNM.INI.
       (linked_project_marker
        (hcnm_assure_linked_project
          linked_project_marker
        )
       )
       ;;Make a project in this drawing's folder.
       (t
        (alert
          (princ
            (strcat
              "This drawing's folder is new to CNM."
            )
          )
        )
        (hcnm_initialize_project dwgdir)
        dwgdir
       )
     )
  )
)

(defun hcnm_local_project_marker (dwgdir)
  (findfile (hcnm_project_folder_to_ini dwgdir))
)

(defun hcnm_linked_project_marker (dwgdir)
  (findfile (hcnm_project_folder_to_link dwgdir))
)

(defun hcnm_error_not_writeable
   ()
  (alert
    (princ
      (strcat
        "Fatal error:\n\nThis drawing must be saved before CNM can be used."
        "\nCNM cannot continue."
       )
    )
  )
  (exit)
)

(defun hcnm_error_ambiguous_project_markers
   (local_project_folder linked_project_folder)
  (alert
    (princ
      (strcat
        "Error:\nThis drawing's folder\n" local_project_folder
        "\nhas both its own project settings (CNM.ini) and a link (in CNMPROJ.TXT) to a project in another folder:\n"
        linked_project_folder
        "\n\nCNM cannot continue. File names will be printed to the command history for your use resolving the ambiguity."
       )
    )
  )
  (princ
    (strcat
      "\nLocal project: "
      (hcnm_project_folder_to_ini local_project_folder)
    )
  )
  (princ
    (strcat
      "\nLink to another project: "
      (hcnm_project_folder_to_link local_project_folder)
    )
  )
  (exit)
)


(defun hcnm_assure_local_project (local_marker_file)
  (hcnm_check_moved_project local_marker_file)
  (haws-filename-directory local_marker_file)
)

(defun hcnm_assure_linked_project (link_marker / projroot rdlin)
  (cond
    ((and
       (setq f1 (open link_marker "r"))
       (progn
         (while (and (setq rdlin (read-line f1)) (not projroot))
           (cond
             ((haws-vlisp-p)
              (if (vl-file-directory-p rdlin)
                (setq projroot rdlin)
              )
             )
             ;;Bricscad option
             (t
              (if (/= ";" (substr rdlin 1 1))
                (setq projroot rdlin)
              )
             )
           )
         )
         (setq f1 (close f1))
         projroot
       )
     )
    )
  )
  (if (not (findfile (hcnm_ini_name projroot)))
    (hcnm_initialize_project projroot)
  )
  (hcnm_check_moved_project
    (hcnm_project_folder_to_ini projroot)
  )
  (princ
    (strcat
      "\nUsing project settings from another folder as directed by CNMPROJ.TXT in this drawing's folder."
      "Using project settings from another folder located at "
      projroot
      "\\CNM.INI as directed by CNMPROJ.TXT in this drawing's folder."
    )
  )
  projroot
)

(defun hcnm_check_moved_project
   (project_file_name / input1 pnname thisfile_value)
  (cond
    ((and
       (setq
         thisfile_value
          (ini_readentry
            project_file_name
            "CNM"
            "ThisFile"
          )
       )
       (setq
         pnname
          (ini_readentry project_file_name "CNM" "ProjectNotes")
       )
       (/= thisfile_value "")
       (/= thisfile_value project_file_name)
     )
     (alert
       (princ
         (strcat
           "Warning!\nYou are using these project notes:\n\n"
           pnname
           "\n\nand the CNM.ini for this folder says \n\"ThisFile=\""
           thisfile_value
           "\n\nIt appears it may have been copied from another project."
           "\nYou may be about to edit the wrong Project Notes file."
         )
       )
     )
     (initget "Yes No")
     (setq input1 (getkword "\nContinue with this file? [Yes/No]: "))
     (cond
       ((= input1 "Yes")(ini_writeentry project_file_name "CNM" "ThisFile" ""))
       (t (exit))
     )
    )
  )
)


(defun hcnm_project_ini_name () "cnm.ini")
(defun hcnm_project_link_name () "cnmproj.txt")

(defun hcnm_project_folder_to_ini (project_folder)
  (strcat project_folder "\\" (hcnm_project_ini_name))
)
(defun hcnm_project_folder_to_link (project_folder)
  (strcat project_folder "\\" (hcnm_project_link_name))
)


;;as posted the autodesk discussion customization group by Tony Tanzillo
(defun ale_browseforfolder
   (prmstr ioptns deffld / shlobj folder fldobj outval)
  (setq
    shlobj
     (vla-getinterfaceobject
       (vlax-get-acad-object)
       "Shell.Application"
     )
    folder
     (vlax-invoke-method
       shlobj 'BROWSEFORFOLDER 0 prmstr ioptns deffld
      )
  )
  (vlax-release-object shlobj)
  (if folder
    (progn
      (setq
        fldobj
         (vlax-get-property folder 'SELF)
        outval
         (vlax-get-property fldobj 'PATH)
      )
      (vlax-release-object folder)
      (vlax-release-object fldobj)
      outval
    )
  )
)

;;Prompts user for a Project Root folder and links to it by creating
;;or modifying this drawing's folder's cnmproj.txt
;;returns project root
(defun c:hcnm-linkproj ()
(haws-core-init 183) (hcnm_linkproj nil) (haws-core-restore)(princ))

;; Sets the CNM project to the given folder. Includes wizards, alerts, and error checks.
(defun hcnm_linkproj (proj / dwgdir localproj localprojbak oldlink)
  (setq
    dwgdir
     (haws-filename-directory (getvar "dwgprefix"))
    *hcnm_cnmprojectroot*
     (cond
       (*hcnm_cnmprojectroot*)
       (dwgdir)
     )
  )
  (cond ((not proj)(setq proj (hcnm_browseproj *hcnm_cnmprojectroot*))))
  (cond
    (proj
     (setq *hcnm_cnmprojectroot* proj)
     (cond
       ((= proj dwgdir)
        (cond
          ((setq oldlink (findfile (hcnm_project_folder_to_link proj)))
           (alert
             (princ
               "Setting project to this drawing's folder by deleting an existing link to another folder."
             )
           )
           (vl-file-delete oldlink)
          )
          (t
           (alert
             (strcat
               "Project Folder\n"
               *hcnm_cnmprojectroot*
               "\nnot changed."
             )
           )
          )
        )
       )
       (proj
        (hcnm_makeprojtxt proj dwgdir)
        (alert
          (princ
            (strcat
              "Created link in this drawing's folder to CNM project settings in\n"
              proj
            )
          )
        )
        (cond
          ((setq
             localproj
              (findfile (hcnm_project_folder_to_ini dwgdir))
           )
           (setq localprojbak (strcat localproj ".bak"))
           (alert
             (princ
               (strcat
                 "Note: CNM renamed the existing\n" localproj "\nto\n"
                 localprojbak
                 "\nbecause you linked to a project in another folder."
                )
             )
           )
           (vl-file-rename localproj localprojbak)
          )
        )
       )
     )
    )
    (*hcnm_cnmprojectroot*
     (alert
       (strcat
         "Project Folder\n"
         *hcnm_cnmprojectroot*
         "\nnot changed."
       )
     )
    )
  )
)

(defun hcnm_browseproj (oldproj)
  (cond
    ((haws-vlisp-p)
     (ale_browseforfolder
       (hcnm_shorten_path oldproj 50)
       48
       ""
     )
    )
    (t
     (haws-filename-directory
       (getfiled "Select any file in Project Folder" "" "" 0)
     )
    )
  )
)

(defun hcnm_shorten_path (path nshort)
  (cond
    ((< (strlen path) nshort) path)
    ((strcat
       "Cancel to keep current Project Folder:\n"
       (substr path 1 3)
       "..."
       (haws-endstr path (- nshort 3) (- nshort 3))
     )
    )
  )
)


;;Makes a project root reference file CNMPROJ.TXT in this drawing's folder
;;Returns nil.
(defun hcnm_makeprojtxt (projdir dwgdir)
  (setq f2 (open (hcnm_project_folder_to_link dwgdir) "w"))
  (princ
    (strcat
      ";For simple projects, all project drawings are in one folder, 
;and Construction Notes Manager keeps settings (CNM.INI) 
;in that folder with the drawings.
;
;For complex projects (ones that that have drawings in
;multiple folders all using the same Project Notes file and settings), 
;CNMPROJ.TXT (this file) points from each folder to 
;the Project Root Folder, given below:
"     projdir
    )
    f2
  )
  (setq f2 (close f2))
)

;#endregion
;#region Config
;;;================================================================================================================
;;;
;;; Begin Settings Config functions
;;;
;;;================================================================================================================
;;; Multi-app planning thoughts and code
;;; ===================================================================
;;;
;;; 2017 note: Looking over all this 10 years later, I think the best
;;; path forward is to implement this in CNM.LSP little by little.  Then
;;; when it is functional, copy it to EDCLIB.
;;; Most of this code is obsolete, but there are some ideas here I still want to implement.
;;;
;;; Begin Settings input/output functions  NOT YET FUNCTIONAL
;;;
;;; Should FILE be an argument to HAWS-GETVAR and HAWS-SETVAR?  If
;;; so,
;;; how does it affect
;;; (HAWS-INI)?
;;; I don't at this moment 2008-09-14 know what the above question means.
;;;
;;; I can make these into generic input/output functions,
;;; but to do so, I need to have the calling application provide the following
;;; (in its own wrapper function):
;;; 1.  section name for its section in *HAWS-SETTINGS*
;;; 2.  app name for ini file name or other storage division
;;; 3.  scope code indicating the scope of the settings, which determines the method of storage and retrieval: 
;;;      "a" APPLICATION settings are currently stored in the program folder in a given section of a given ini file
;;;      "u" USER settings are currently stored in ACAD.CFG with (setcfg)/(getcfg) or the Windows registry
;;;      "p" PROJECT settings are kept in the current project folder in a given section of a given ini file, with editable defaults in the the program folder
;;;      "d" DRAWING settings are kept in the current drawing
;;; 4.  location indicator file name in case we have to find an ini
;;;     (indicator file can be optionally a fully qualified path)
;;; 5.  application defaults for all variables
;;;
;;; Here's how this can work:
;;; In the application, you have a function like (app-getvar key)
;;; Then in that function you pass this general-purpose getvar the key along with the
;;; application name, application defaults, ini file name, and location test file
;;;
;;; PROJECTS
;;;
;;; Projects are sets of drawings grouped by folder.  Normally an ini file
;;; in a folder applies just to the drawings in that folder.
;;; But multiple folders can use a single project ini file in a project root folder.
;;; This is controlled by placing a pointer file in all folders other than the project root.
;;; For CNM, this file was called CNMPROJ.TXT.  I suppose that same convention could be followed
;;; for other apps by pasting the app key name to "PROJ.TXT".
;;;
;;; ===================================================================
;;; One question or to-do here is the method of initializing (erasing) the settings in the event of a pause when user could edit them.
;;; On the other hand, it seems common for programs to ignore any changes to settings that are made directly while the program is running.
;;; This could be turned on an off by the calling application.  See HAWS-REMOVESETTINGS
;;;
;;; Settings are stored in *HAWS-SETTINGS*, which is in the likeness
;;; of multiple ini files:
;;; I removed the inifile path from here, but maybe it's really needed.
;;; *HAWS-SETTINGS*
;;; '("SCOPE"            Every scope may have a different storage mechanism
;;;    ("APPNAME"        Key Name of an application using a single inifile the settings are stored in
;;;      (0 . "INIFILE") Known path to settings for this scope and application
;;;      ("SECTION"      Ini section or path to variable
;;;        ("VAR"        Ini variable
;;;         "VAL"        Ini value
;;;        )
;;;      )
;;;    )
;;;  )
;;;

;;;Test functions
(defun c:testset ()
(haws-core-init 184)
  (hcnm_concept_testsetvar
    (getstring "\nVariable name: ")
    (getstring "\nValue: ")
  )
 (haws-core-restore)
)
(defun c:testget ()
(haws-core-init 185)
  (hcnm_concept_testgetvar (getstring "\nVariable name: "))
  (haws-core-restore)
)

;;==============================================================================
;; C:PRETEST - Clean up before testing bubble notes
;;==============================================================================
;; Removes all reactors and deletes all bubbles with HCNM-BUBBLE XDATA
;; Usage: Type PRETEST at command line before testing
(defun c:pretest (/ ename ename_next ename_leader xdata count_ms count_ps count_ldr reactors)
  (princ "\n=== PRE-TEST CLEANUP ===")
  (setq count_ms 0 count_ps 0 count_ldr 0)
  
  ;; Step 1: Remove all reactors
  (princ "\nRemoving all object reactors...")
  (setq reactors (cdar (vlr-reactors :vlr-object-reactor)))
  (cond
    (reactors
     (foreach reactor reactors
       (vlr-remove reactor)
       (princ ".")
     )
     (princ (strcat "\nRemoved " (itoa (length reactors)) " reactor(s)"))
    )
    (t (princ "\nNo reactors found"))
  )
  
  ;; Step 2: Erase all bubbles with HCNM-BUBBLE XDATA and their leaders
  (princ "\nScanning for bubbles with XDATA...")
  (setq ename (entnext))
  (while ename
    (setq ename_next (entnext ename))  ; Get next before potential deletion
    ;; Check if entity has HCNM-BUBBLE XDATA
    (setq xdata (assoc -3 (entget ename '("HCNM-BUBBLE"))))
    (cond
      (xdata
       ;; Found a bubble - determine if it's in model or paper space
       (cond
         ((hcnm_ldrblk_is_on_model_tab ename)
          (setq count_ms (1+ count_ms))
         )
         (t
          (setq count_ps (1+ count_ps))
         )
       )
       ;; Find and delete the associated leader first
       (setq ename_leader (hcnm_ldrblk_bubble_leader ename))
       (cond
         (ename_leader
          (entdel ename_leader)
          (setq count_ldr (1+ count_ldr))
         )
       )
       ;; Erase the bubble
       (entdel ename)
       (princ ".")
      )
    )
    (setq ename ename_next)
  )
  
  ;; Step 3: Report results
  (princ (strcat "\n\nDeleted " (itoa count_ms) " bubble(s) from Model Space"))
  (princ (strcat "\nDeleted " (itoa count_ps) " bubble(s) from Paper Space"))
  (princ (strcat "\nDeleted " (itoa count_ldr) " leader(s)"))
  (princ (strcat "\nTotal: " (itoa (+ count_ms count_ps)) " bubble(s) deleted"))
  (princ "\n\n=== CLEANUP COMPLETE ===")
  (princ)
)

(defun hcnm_concept_testsetvar (var val)
  (hcnm_concept_setvar
    ;; variable
    var
    ;;value
    val
    ;; application name for its section in *hcnm_concept_SETTINGS*
    "test"
    ;; indicator file name for default location of ini or vanilla ini
    ;; (indicator file can be optionally a fully qualified path)
    "hawsedc.mnl"
    ;; ini section
    "Test"
    ;; Scope of setting.  Can be "a" (application)  "u" (user)  "p" (project) or "d" (drawing).  Determines where to store and get setting.
    "a"
   )
)
;;This is a sample wrapper function that an application would use
;;to call hcnm_concept_GETVAR.
(defun hcnm_concept_testgetvar (var)
  (hcnm_concept_getvar
    ;;variable
    var
    ;; application name for its section in *hcnm_concept_SETTINGS*
    "test"
    ;; indicator file name for default location of ini or vanilla ini
    ;; (indicator file can be optionally a fully qualified path)
    "hawsedc.mnl"
    ;; ini section
    "Test"
    ;; Scope of setting.  Can be "a" (application)  "u" (user)  "p" (project) or "d" (drawing).  Determines where to store and get setting.
    "a" ;;Fallback defaults
        '
         (("1" "1val")
          ("2" "2val")
         )
   )
)

;; hcnm_concept_INI
;; Finds INI file
;; Returns a fully qualified path, that folder is qualified to have
;; HAWSEDC.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\INIFOLDER" path to other functions.


;;; hcnm_concept_INIFOLDER gets a valid INI folder.
;;; This function is wrong because there isn't a single *hcnm_concept_INIFOLDER* that this function
;;; can throw around globally.
(defun hcnm_concept_inifile (app scope testfile / inifile assumedinifolder
                 assumedinifile
                )
  (cond ((not testfile) (setq testfile (strcat app ".cui"))))
  (cond
    ;; Project already defined this session
    ;; (Assume it's valid.  Calling function should init project
    ;; if there's been a chance of loss.)
    ((cdr
       (assoc
         0
         (cdr (assoc app (cdr (assoc scope *hcnm_concept_settings*))))
       )
     )
    )
    ;;TGH OK.  Here we need to take the case of each scope and treat them differently
    ;;App scope ini needs to be searched using testfile in all cases.  Project scope only needs to be search with testfile
    ;;if project ini isn't found.  But wait, for project scope, you want to know the project ini location, but then
    ;;if you need to make a project ini from the app default, you have to find the application default ini.
    ;;so for project scope, there are two ini path building searches.
    ;;
    ;;Or try to find inifile in folder with testfile
    ((and
       (setq assumedinifolder (findfile testfile))
       (setq
         assumedinifolder
          (hcnm_concept_filename_directory assumedinifolder)
       )
       (setq
         assumedinifile
          (findfile (strcat assumedinifolder app ".ini"))
       )
     )
     assumedinifile
    )
    ;;Or make an ini file in folder with testfile in it or in proj folder
    ((hcnm_concept_getinidefaults assumedinifolder) assumedinifolder)
  )
)


;;Gets all settings from an inifile if it can.
(defun hcnm_concept_getsettings (inifile testfile apporproj)
  (setq
    *hcnm_concept_settings*
     (ini_readini
       (hcnm_concept_ini (hcnm_concept_inifolder inifile testfile))
     )
  )
)

;;Sets a variable in the global lisp list and in HAWSEDC.INI
(defun hcnm_concept_setvar (inifile inisection var val / setting)
  ;; Call GETVAR before setting var.  Why?  To populate *hcnm_concept_SETTINGS*?
  (hcnm_concept_getvar
    var inisection inifile testfile apporproj defaults
   )
  (hcnm_concept_addvartolist var val inisection inifile)
  (ini_writeentry
    (hcnm_concept_ini (hcnm_concept_inifolder))
    inisection
    setting
    val
  )
)

;; hcnm_concept_GETVAR
;; hcnm_concept_GETVAR is called by wrapper functions like HCNM_GETVAR or hcnm_concept_EDCGETVAR
;; It gets a variable without opening a file if it can.
;; (Higher calling functions may use functions like HCNM_PROJINIT or
;; hcnm_concept_REMOVESETTINGS to remove settings and force a file
;; read.)
;; hcnm_concept_GETVAR gets a program setting from
;; 1. The global *hcnm_concept_SETTINGS* list if found
;; 2. An ini file or other location
;; 3. reverts to a default value without fail
;;
;; INIFILE is an ini filename. Ini file might not be used for a given scope in the current strategy.  
;; If there is no such ini file found and is needed, hcnm_concept_getvar creates it.
;; If hcnm_concept_getvar can't create the file, it sends an alert.
;;
;; SECTION is the ini section/path/tree path/location the var is in or goes in
;;
;; DEFAULTS is a list of defaults in the HawsEDC standard settings format as follows:
;;  '((VAR1 DEFAULT1)(VAR2 DEFAULT2))
;;
;; variable to get
;; ini section name
;; ini file name
;; test file name for location of ini defaults or app based active ini
;; A flag indicating whether the active settings are to be kept in the
;; app folder or the project folder ("app" "prj")
;; application defaults for all variables
(defun hcnm_concept_getvar (var sect app scope testfile defaults / addtolist
                addtoini dir ini setting val
               )
  (setq
    ;; Does the variable need to be added to the *hcnm_concept_SETTINGS* list? Assume yes initially.
    addtolist
     t
    ;; Does the variable need to be added to the appropriate ini file? Assume yes initially
    addtoini
     t
  )
  ;;Get var list if no var list
  (if (not *hcnm_concept_settings*)
    (hcnm_concept_getsettings)
  )
  (cond
    ;;Try getting from list
    ((setq
       val
        (hcnm_concept_vartoval
          var
          (cadr (assoc sect (caddr (assoc app *hcnm_concept_settings*))))
        )
     )
     (setq
       addtolist nil
       addtoini nil
     )
    )
    ;;Try getting from ini.
    ((setq
       val
        (ini_readentry app sect setting)
       addtoini nil
     )
    )
    ;;Get from app ini if not.
    ((and
       (setq dir (findfile testfile))
       (setq
         ini
          (findfile (strcat (hcnm_concept_filename_directory dir) "\\" app))
       )
       (setq val (ini_readentry ini sect setting))
     )
    )
    ;;Use default if there is one
    ((setq val (hcnm_concept_vartoval var defaults)))
    ;;Otherwise fail.
    (t
     (alert
       (strcat
         "Fatal error in "
         sect
         ":\nCould not initialize the variable\n"
         var
       )
     )
     (setq
       addtolist nil
       addtoini nil
     )
    )
  )
  (if addtolist
    (hcnm_concept_addvartolist var val sect app)
  )
  (if addtoini
    (ini_writeentry (hcnm_concept_ini (hcnm_concept_inifolder)) app setting val)
  )
  val
)

(defun hcnm_concept_addvartolist (var val inisection inifile)
  (setq
    setting
     (hcnm_concept_vartosetting var)
    *hcnm_concept_settings*
     (subst
       (subst
         (subst
           (list setting val)
           (assoc
             inisetting
             (assoc
               inisection
               (assoc inifile *hcnm_concept_settings*)
             )
           )
           (assoc
             inisection
             (assoc inifile *hcnm_concept_settings*)
           )
         )
         (assoc inisection (assoc file *hcnm_concept_settings*))
         (assoc inifile *hcnm_concept_settings*)
       )
       (assoc inifile *hcnm_concept_settings*)
       *hcnm_concept_settings*
     )
  )
)


;;Gets an entire ini file from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(defun hcnm_concept_getinidefaults (proj / app appini projini)
  (alert
    (princ
      (strcat
        "Note: Program settings not found in program folder\n"
        proj
        "\n\nUsing default settings."
      )
    )
  )
  (setq projini (strcat proj "\\" "cnm.ini"))
  (cond
    ((and
       (setq app (c:hcnm-config-getvar "AppFolder"))
       (setq
         appini
          (findfile
            (strcat (hcnm_concept_filename_directory app) "\\" "cnm.ini")
          )
       )
       (hcnm_concept_file_copy appini projini)
     )
     (while (not (findfile projini)))
     projini
    )
    (t
     (setq f2 (open projini "w"))
     (princ
       "[CNM]
ProjectNotes=constnot.csv
ProjectNotesEditor=csv
NoteTypes=BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI
DoCurrentTabOnly=0
PhaseAlias1=1
PhaseAlias2=2
PhaseAlias3=3
PhaseAlias4=4
PhaseAlias5=5
PhaseAlias6=6
PhaseAlias7=7
PhaseAlias8=8
PhaseAlias9=9
InsertTablePhases=No
TableWidth=65
PhaseWidthAdd=9
DescriptionWrap=9999
LineSpacing=1.5
NoteSpacing=3
NumberToDescriptionWidth=2.5
DescriptionToQuantityWidth=56
QuantityToQuantityWidth=9
QuantityToUnitsWidth=1
ShowKeyTableTitleShapes=1
ShowKeyTableGrid=0
ShowKeyTableQuantities=1
BubbleHooks=1
ImportLayerSettings=No
"      f2
     )
     (setq f2 (close f2))
     projini
    )
  )
)

;;Saves *hcnm_concept_SETTINGS* to the requested inifile
(defun hcnm_concept_savesettingstoini (inifile testfile apporproj)
  (ini_writesection
    (hcnm_concept_ini (hcnm_concept_inifolder inifile testfile))
    inifile
    *hcnm_concept_settings*
  )
)
;;;================================================================================================================
;;; End multi-app planning thoughts and code
;;;================================================================================================================
;;; CONFIG: Name of the app
;;; DEFINITIONS: Name of an entry in the def
;;; SCOPE_KEY: eg. "User"
;;; SCOPE_CODE: eg. 4
;;; ENTRY: An entry in the var list
;;; VAR: The string name of a var
;;; VAL: The string value of a var
;;;
(defun hcnm_config_definitions (/)
  (list
    (list "Scope"
     (list "Session" 0)
     (list "Drawing" 1) ; Does not work yet?
     (list "Project" 2)
     (list "App" 3) ; Does not work yet?
     (list "User" 4)
    )
    (list "Var"
     (list "ProjectFolder" "" 1)
     (list "AppFolder"  (haws-filename-directory (findfile "cnm.mnl")) 0)
     (list "LXXListMode" "yes" 4)
     (list "CNMAliasActivation" "0" 4)
     (list "ProjectNotesEditor" (hcnm_config_default_projectnoteseditor) 2) ; text, csv, or cnm
     (list "LayersEditor" "notepad" 4) ; notepad or cnm
     (list "ProjectNotes" "constnot.csv" 2)
     (list "ThisFile" "" 2)
     (list "ImportLayerSettings" "No" 2)
     (list "NoteTypes" "BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI" 2)
     (list "DoCurrentTabOnly" "0" 2)
     (list "PhaseAlias1" "1" 2)
     (list "PhaseAlias2" "2" 2)
     (list "PhaseAlias3" "3" 2)
     (list "PhaseAlias4" "4" 2)
     (list "PhaseAlias5" "5" 2)
     (list "PhaseAlias6" "6" 2)
     (list "PhaseAlias7" "7" 2)
     (list "PhaseAlias8" "8" 2)
     (list "PhaseAlias9" "9" 2)
     (list "InsertTablePhases" "No" 2)
     (list "TableWidth" "65" 2)
     (list "PhaseWidthAdd" "9" 2)
     (list "DescriptionWrap" "9999" 2)
     (list "LineSpacing" "1.5" 2)
     (list "NoteSpacing" "3" 2)
     (list "NumberToDescriptionWidth" "2.5" 2)
     (list "DescriptionToQuantityWidth" "56" 2)
     (list "QuantityToQuantityWidth" "9" 2)
     (list "QuantityToUnitsWidth" "1" 2)
     (list "ShowKeyTableTitleShapes" "1" 2)
     (list "ShowKeyTableGrid" "0" 2)
     (list "ShowKeyTableQuantities" "1" 2)
     (list "BubbleHooks" "0" 2)
     (list "BubbleMtext" "0" 2)
     (list "BubbleAreaIntegral" "0" 2)
     (list "NotesLeaderDimstyle" "" 2)
     (list "NotesKeyTableDimstyle" "" 2)
     (list "TCGLeaderDimstyle" "TCG Leader" 2)
     (list "BubbleTextLine1PromptP" "1" 4)
     (list "BubbleTextLine2PromptP" "1" 4)
     (list "BubbleTextLine3PromptP" "0" 4)
     (list "BubbleTextLine4PromptP" "0" 4)
     (list "BubbleTextLine5PromptP" "0" 4)
     (list "BubbleTextLine6PromptP" "0" 4)
     (list "BubbleTextLine0PromptP" "0" 4)
     (list "BubbleSkipEntryPrompt" "0" 4)
     (list "BubbleOffsetDropSign" "1" 2)
     (list "BubbleTextPrefixLF" "" 2)
     (list "BubbleTextPrefixSF" "" 2)
     (list "BubbleTextPrefixSY" "" 2)
     (list "BubbleTextPrefixSta" "STA " 2)
     (list "BubbleTextPrefixOff+" "" 2)
     (list "BubbleTextPrefixOff-" "" 2)
     (list "BubbleTextPrefixN" "N " 2)
     (list "BubbleTextPrefixE" "E " 2)
     (list "BubbleTextPrefixZ" "" 2)
     (list "BubbleTextPrefixPipeDia" "" 2)
     (list "BubbleTextPrefixPipeSlope" "" 2)
     (list "BubbleTextPrefixPipeLength" "L=" 2)
     (list "BubbleTextPostfixLF" " LF" 2)
     (list "BubbleTextPostfixSF" " SF" 2)
     (list "BubbleTextPostfixSY" " SY" 2)
     (list "BubbleTextPostfixSta" "" 2)
     (list "BubbleTextPostfixOff+" " RT" 2)
     (list "BubbleTextPostfixOff-" " LT" 2)
     (list "BubbleTextPostfixN" "" 2)
     (list "BubbleTextPostfixE" "" 2)
     (list "BubbleTextPostfixZ" "" 2)
     (list "BubbleTextPostfixPipeDia" "\"" 2)
     (list "BubbleTextPostfixPipeSlope" "%" 2)
     (list "BubbleTextPostfixPipeLength" "'" 2)
     (list "BubbleTextJoinDelSta" ", " 2)
     (list "BubbleTextJoinDelN" ", " 2)
     (list "BubbleTextPrecisionLF" "0" 4)
     (list "BubbleTextPrecisionSF" "0" 4)
     (list "BubbleTextPrecisionSY" "0" 4)
     (list "BubbleTextPrecisionOff+" "2" 4)
     (list "BubbleTextPrecisionN" "2" 4)
     (list "BubbleTextPrecisionE" "2" 4)
     (list "BubbleTextPrecisionZ" "2" 4)
     (list "BubbleTextPrecisionPipeDia" "0" 4)
     (list "BubbleTextPrecisionPipeSlope" "2" 4)
     (list "BubbleTextPrecisionPipeLength" "2" 4)
     (list "BubbleCurrentAlignment" "" 0)
     (list "AllowReactors" "1" 0)
     (list "BubbleArrowIntegralPending" "0" 0)
    )
   )
)

(defun hcnm_config_default_projectnoteseditor (/ reg_val)
  (cond
    ((and
       (haws-vlisp-p)
       (setq
         reg_val
          (vl-registry-read
            "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
            "ProjectNoteseditor"
          )
       )
       (wcmatch (strcase reg_val) "*CNMEDIT*")
     )
     "cnm"
    )
    (t "csv")
  )
)

;; Strips scope stuff and returns just defaults list
(defun hcnm_config_defaults ()
  (cond
    (*hcnm_config_defaults*)
    ((mapcar
       '(lambda (var) (hcnm_config_entry_strip_scope var))
       (cdr (assoc "Var" (hcnm_config_definitions)))
     )
    )
  )
)

(defun hcnm_config_entry_strip_scope (entry)
  (reverse (cdr (reverse entry)))
)

(defun hcnm_config_defaults_single_scope
   (scope_key / scope_code scope_list)
  (setq scope_code (hcnm_config_scope_code scope_key))
  (foreach
     entry (cdr (assoc "Var" (hcnm_config_definitions)))
    (cond
      ((= (hcnm_config_entry_scope_code entry) scope_code)
       (setq
         scope_list
          (cons
            (hcnm_config_entry_strip_scope entry)
            scope_list
          )
       )
      )
    )
  )
  (reverse scope_list)
)

(defun hcnm_config_scope_code (scope_key)
  (cadr
    (assoc
      scope_key
      (cdr (assoc "Scope" (hcnm_config_definitions)))
    )
  )
)

(defun hcnm_config_scope_eq (var scope_key)
  (= (hcnm_config_entry_scope_code
       (assoc var (cdr (assoc "Var" (hcnm_config_definitions))))
     )
     (hcnm_config_scope_code scope_key)
  )
)

(defun hcnm_config_entry_var (entry) (car entry))

(defun hcnm_config_entry_val (entry) (cadr entry))

(defun hcnm_config_entry_scope_code (entry) (caddr entry))

(defun hcnm_config_get_default (var)
  (hcnm_config_entry_val (assoc var (hcnm_config_defaults)))
)

(defun hcnm_config_read_all (/ ini_configs)
  (append
    (hcnm_config_read_all_user)
    (hcnm_config_read_all_project)
  )
)
(defun hcnm_config_read_all_user (/ )
  ;; This function doesn't need to setq an ini_configs since it does HCNM_CONFIG_READ_USER var by var.
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm_config_entry_var entry)
         val (hcnm_config_read_user var)
       )
       (list var val)
     )
    (hcnm_config_defaults_single_scope "User")
  )
)

(defun hcnm_config_read_all_project (/ ini_configs)
  (setq ini_configs (ini_readsection (hcnm_ini_name (hcnm_proj)) "CNM"))
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm_config_entry_var entry)
         val (hcnm_config_entry_val (assoc var ini_configs))
       )
       (list var val)
     )
    (hcnm_config_defaults_single_scope "Project")
  )
)

(defun hcnm_config_read_all_session (/ ini_configs)
  ;; Maybe this function can do what HCNM_CONFIG_READ_ALL_USER does; it doesn't need to setq an ini_configs since it does HCNM_CONFIG_READ_USER var by var.
  (setq ini_configs *hcnm_config_session*)
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm_config_entry_var entry)
         val (hcnm_config_entry_val (assoc var ini_configs))
       )
       (list var val)
     )
    (hcnm_config_defaults_single_scope "Session")
  )
)

;;;Sets a variable in a temporary global lisp list
(defun hcnm_config_temp_setvar (var val)
  (cond
    ((assoc var *hcnm_config_temp*)
     (setq
       *hcnm_config_temp*
        (subst
          (list var val)
          (assoc var *hcnm_config_temp*)
          *hcnm_config_temp*
        )
     )
    )
    (t
     (setq *hcnm_config_temp* (cons (list var val) *hcnm_config_temp*))
    )
  )
)

;;;Gets a variable in a temporary global lisp list
;;;If it's not present there, gets real value.
(defun hcnm_config_temp_getvar (var)
  (cond
    ((cadr (assoc var *hcnm_config_temp*)))
    (t (c:hcnm-config-getvar var))
  )
)


;;;Saves the global list of temporary configs in the real global lisp list and in CNM.INI
(defun hcnm_config_temp_save ()
  (foreach
     entry *hcnm_config_temp*
    (c:hcnm-config-setvar
      (hcnm_config_entry_var entry)
      (hcnm_config_entry_val entry)
    )
  )
)

;;;Saves the global list of temporary configs in the real global lisp list and in CNM.INI
(defun hcnm_config_temp_clear ()
  (setq *hcnm_config_temp* nil)
)

;;;Sets a variable in the global lisp list and in CNM.INI
(defun c:hcnm-config-setvar (var val)
  (setq
    *hcnm_config*
     (cond
       ((assoc var *hcnm_config*)
        (subst
          (list var val)
          (assoc var *hcnm_config*)
          *hcnm_config*
        )
       )
       (t (cons (list var val) *hcnm_config*))
     )
  )
  (cond
    ((hcnm_config_scope_eq var "User")
     (hcnm_config_write_user var val)
    )
    ((hcnm_config_scope_eq var "Project")
     (ini_writeentry (hcnm_ini_name (hcnm_proj)) "CNM" var val)
    )
    ((hcnm_config_scope_eq var "Session")
     (hcnm_config_write_session var val)
    )
  )
  val
)


;;; c:hcnm-config-getvar
;;; Var is case sensitive
(defun c:hcnm-config-getvar
   (var / setvar_p define_configs dir ini projroot config val)
  (setq setvar_p t)
  (cond
    ;; Initialize configs as needed
    ((not (assoc var *hcnm_config*))
     (cond
       ((hcnm_config_scope_eq var "Project")
        (setq
          *hcnm_config*
           (append
             *hcnm_config*
             ;; If one project var is missing, all project vars are missing
             (hcnm_config_read_all_project)
           )
        )
       )
       ((hcnm_config_scope_eq var "User")
        (setq
          *hcnm_config*
           (append
             *hcnm_config*
             ;; If one user var is missing, all user vars are missing
             (hcnm_config_read_all_user)
           )
        )
       )
       ((hcnm_config_scope_eq var "Session")
        (setq
          *hcnm_config*
           (append
             *hcnm_config*
             ;; If one session var is missing, all session vars are missing
             (hcnm_config_read_all_session)
           )
        )
       )       
     )
    )
  )
  (cond
    ;;Try getting from list
    ((setq val (cadr (assoc var *hcnm_config*)))
     (setq setvar_p nil)
    )
    ;;Use default if there is one
    ((setq val (hcnm_config_get_default var)))
    ;;Otherwise fail.
    (t
     (alert
       (strcat
         "Fatal error in CNM:\nCould not initialize the variable\n"
         (haws-prin1-to-string var)
       )
     )
     (setq setvar_p nil)
    )
  )
  (if setvar_p
    (c:hcnm-config-setvar var val)
  )
  val
)

(defun hcnm_config_read_user (var / )
  (cond
    ((haws-vlisp-p)
     (vl-registry-read
       "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
       var
     )
    )
    (t nil)
  )
)

(defun hcnm_config_write_user (var val)
  (cond
    ((haws-vlisp-p)
     (vl-registry-write
       "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
       var
       val
     )
    )
  )
)

(defun hcnm_config_read_session (var / )
    (cadr (assoc var *hcnm_config_session*))
)

(defun hcnm_config_write_session (var val)
  (setq
    *hcnm_config_session*
     (cond
       ((assoc var *hcnm_config_session*)
        (subst
          (list var val)
          (assoc var *hcnm_config_session*)
          *hcnm_config_session*
        )
       )
       (t (cons (list var val) *hcnm_config_session*))
     )
  )
)

;;Gets an entire ini file (per CNM forum) from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(defun hcnm_initialize_project (proj / app appini projini mark_file_p)
  (setq projini (hcnm_project_folder_to_ini proj))
  (cond
    ((and
       (setq app (c:hcnm-config-getvar "AppFolder"))
       (setq appini (findfile (hcnm_project_folder_to_ini app)))
     )
     (if (not (haws-file-copy appini projini)) (hcnm_error_not_writeable)) 
     (alert
       (princ
         (strcat
           "CNM is copying settings found in\n" appini "\nto\n" projini
           "\nfor this project."
          )
       )
     )
     (while (not (findfile projini)))
     (setq mark_file_p t)
     projini
    )
    (t
     (alert
       (princ
         (strcat
           "CNM could not find a settings file in\n" app
           "\n\nPutting hard-coded defaults in\n" projini
           "\nfor this project."
          )
       )
     )
     ;|(SETQ F2 (OPEN PROJINI "w"))
     (princ "[CNM]" f2) ; TODO TEST AND REMOVE
     (setq f2 (close f2))|;
     (setq *hcnm_config* (hcnm_config_defaults_single_scope "Project"))
     (hcnm_config_write_project proj)
     (setq *hcnm_config* (hcnm_config_defaults))
     (setq mark_file_p t)
     projini
    )
  )
  (cond
    (mark_file_p
     (ini_writeentry projini "CNM" "ThisFile" projini)
    )
  )
)

;;Saves *HCNM_CONFIG* to this project's ini
(defun hcnm_config_write_project (proj)
  (ini_writesection
    (hcnm_ini_name
      (cond
        (proj)
        ((hcnm_proj))
      )
    )
    "CNM"
    *hcnm_config*
  )
)

(defun hcnm_set_dimstyle (key / dsty)
  ;;Set dimstyle as requested by calling function and set by user
  ;;First, get dimstyle name
  (setq dsty (c:hcnm-config-getvar key))
  ;;Second, if the style is TCGLeader and doesn't already exist, set the _DotSmall ldrblk.
  (cond
    ((and
       (= key "TCGLeaderDimstyle")
       (not (tblsearch "DIMSTYLE" dsty))
     )
     (vl-cmdf "._dim1" "_dimldrblk" "_DotSmall")
    )
  )
  ;;Third, if the desired style exists, save current style for later, then restore the desired style.
  (cond
    ((and (/= key "") (tblsearch "DIMSTYLE" dsty))
     (setq *hcnm_dimstyleold* (getvar "dimstyle"))
     (vl-cmdf "._dimstyle" "_restore" dsty)
    )
  )
)
(defun hcnm_restore_dimstyle ()
  (cond
    (*hcnm_dimstyleold*
     (vl-cmdf "._dimstyle" "_restore" *hcnm_dimstyleold*)
    )
  )
)

;#endregion
;#region Project Notes
;;;============================================================================
;;;
;;; Begin Project Notes functions
;;;
;;;============================================================================

;; HCNM_PROJNOTES gets a valid project notes file
;; It should resolve all errors and user conditions.
;; and return a "drive:\\...\\projroot\\pnname" filename to other functions.
(defun hcnm_projnotes (/ app apppn format opt1 pnname projnotes)
  (setq pnname (c:hcnm-config-getvar "ProjectNotes"))
  (if (= pnname "")
    (c:hcnm-config-setvar
      "ProjectNotes"
      (setq pnname "constnot.txt")
    )
  )
  (haws-milepost
    (strcat
      "HCNM_PROJNOTES is beginning with ProjectNotes="
      pnname
    )
  )
  (cond
    ;;First, if there is a directory given, try to find project notes there.
    ((and
       (/= "" (haws-filename-directory pnname))
       (setq projnotes (findfile pnname))
     )
     projnotes
    )
    ;;Second, try to find the pnname (ProjectNotes=) given in CNM.INI
    ;;in the project folder ignoring any directory in the name.
    ((findfile
       (setq
         projnotes
          (strcat
            (hcnm_proj)
            "\\"
            (haws-filename-base pnname)
            (haws-filename-extension pnname)
          )
       )
     )
     ;;Record the find in the INI
     (c:hcnm-config-setvar "ProjectNotes" projnotes)
    )
    ;;Third choice, we couldn't find the Project Notes specified,
    ;;so try to get the appropriate style Project Notes from the app folder
    ;;and put it in the location tried above.
    ;;The CFREAD functions will later evaluate the necessity of changing the file
    ;;format and name.
    ((and
       (setq app (c:hcnm-config-getvar "AppFolder"))
       (setq
         format (hcnm_config_project_notes_format)
         apppn
          (findfile
            (strcat
              app
              "\\"
              (cond
                ((= format "txt2") "constnot-default.txt")
                ((= format "csv") "constnot-default.csv")
                (t (alert(princ "\nUnexpected Project Notes format. CNM cannot continue. Contact developer."))(exit))
              )
            )
          )
       )
     )
     ;;If CONSTNOT.TXT was found in the app folder,
     ;;try to copy it to this project.
     (haws-file-copy apppn projnotes)
     ;;Record the find in the INI
     (c:hcnm-config-setvar "ProjectNotes" projnotes)
    )
    ;;Third and last choice, fail with alert.
    (t
     (alert
       (princ
         (strcat
           "Fatal error in CNM:\nCouldn't find or create Project Notes.\n\nPlease create project notes at "
           projnotes
           "\nor change the current Project Notes or Project Folder."
         )
       )
     )
    )
  )
)

(defun hcnm_getprojnotes (/ dpname oldprojnotes projnotes)
  (hcnm_projinit)                       ;Initialize variables in case any files changed.
  (setq oldprojnotes (hcnm_projnotes))
  (setq dpname (strcat (getvar "dwgprefix") "constnot.txt"))
  (setq
    projnotes
     (getfiled
       "Select Project Notes Filename"
       (c:hcnm-config-getvar "ProjectNotes")
       ""
       37
     )
  )
  ;;Remove path if project notes is in project folder.
  (cond
    ((and projnotes (= (haws-filename-directory projnotes) (hcnm_proj)))
     (setq
       projnotes
        (strcat
          (haws-filename-base projnotes)
          (haws-filename-extension projnotes)
        )
     )
    )
  )
  projnotes
)

;; HCNM_READCF
;; Reads any acceptable Project Notes file format to a *HCNM_CNMPROJECTNOTES* list of the following format
;; '((0 . "comment")(1 "var" "val1" "val2")(2 . "title")(3 "type" "num" "unit" "count" "text"))
;; The acceptable file formats are:
;; TXT1 Fixed field ;Comment\nSET VAR VAL\nNUM (comment)\nBOX (type)\nTITLE \n1    Text thru column 67...UNTCOUNT\n     Cont. text.
;; TXT2 White space delimited ;Comment\n
;; Excel CSV
;; Doesn't do project management except to write txt2 configs to cnm.ini in the same folder as projnotes.
(defun hcnm_readcf (projnotes / bakprojnotes pnformat rdlin requested_format)
  ;;Do a file read to figure out what the file format is.
  ;;For now, assume that a file that has any of the shape keys followed by a comma ("BOX,", etc.) is CSV
  ;;any other file is TXT2
  (haws-milepost
    (strcat
      "HCNM_READCF is deciphering the format of "
      projnotes
      "\nand evaluating the need for format conversion."
    )
  )
  (setq f1 (open projnotes "r"))
  (while (and (not pnformat) (setq rdlin (read-line f1)))
    (cond
      ((wcmatch
         (substr rdlin 1 4)
         "BOX`,,CIR`,,DIA`,,ELL`,,HEX`,,OCT`,,PEN`,,REC`,,SST`,,TRI`,"
       )
       (setq pnformat "csv")
      )
      ((wcmatch
         (substr rdlin 1 3)
         (c:hcnm-config-getvar "NoteTypes")
       )
       (setq pnformat "txt2")
      )
    )
  )
  (setq f1 (close f1)
    requested_format (hcnm_config_project_notes_format)
  )
  (cond
    ((= pnformat "txt2")
     (hcnm_readcftxt2 projnotes)
     (cond
       ((= requested_format "csv")
        (setq bakprojnotes projnotes)
        (haws-file-copy
          projnotes
          (progn
            (while (findfile
                     (setq
                       bakprojnotes
                        (strcat
                          (haws-filename-directory bakprojnotes)
                          "\\"
                          (haws-filename-base bakprojnotes)
                          "0"
                          (haws-filename-extension bakprojnotes)
                        )
                     )
                   )
            )
            bakprojnotes
          )
        )
        (alert
          (princ
            (strcat
              "CNM needs to convert\n"
              projnotes
              "\nto comma-separated (csv) format.\n\nCurrent version backed up as\n"
              bakprojnotes
            )
          )
        )
        (hcnm_writecfcsv projnotes)
       )
     )
    )
    ((= pnformat "csv")
     (hcnm_readcfcsv projnotes)
     (cond
       ((= requested_format "txt2")
        (setq bakprojnotes projnotes)
        (haws-file-copy
          projnotes
          (progn
            (while (findfile
                     (setq
                       bakprojnotes
                        (strcat
                          (haws-filename-directory bakprojnotes)
                          "\\"
                          (haws-filename-base bakprojnotes)
                          "0"
                          (haws-filename-extension bakprojnotes)
                        )
                     )
                   )
            )
            bakprojnotes
          )
        )
        (alert
          (princ
            (strcat
              "CNM needs to convert\n"
              projnotes
              "\nto traditional text format.\n\nCurrent version backed up as\n"
              bakprojnotes
            )
          )
        )
        (hcnm_writecftxt2 projnotes)
       )
     )
    )
    ((not pnformat)
     (alert
       (princ
         (strcat
           "Current Project Notes file\n"
           projnotes
           "\ndoes not contain recognizable project notes.\n\nPlease correct the file and try again."
         )
       )
     )
     (exit)
    )
  )
)

(defun hcnm_readcftxt2 (projnotes / alertnote alerttitle cfitem cflist
                    cflist2 commentbegin filev42 iline ininame nottyp
                    rdlin val1 val2 var varlist n notdesc notnum typwc
                   )
  (setq
    typwc
     (c:hcnm-config-getvar "NoteTypes")   ; Get typwc (which may open f1) before opening f1
    f1 (open projnotes "r")
  )
  (while (setq rdlin (read-line f1))
    (cond
      ;;Comment
      ((= ";" (substr rdlin 1 1))
       (setq cflist (cons (cons 0 (substr rdlin 2)) cflist))
      )
      ;;Config setting
      ((= "SET" (haws-rdfld 1 rdlin "W" 1))
       (setq
         var  (haws-rdfld 2 rdlin "W" 1)
         val1 (haws-rdfld 3 rdlin "W" 1)
       )
       (cond
         ;;CNMVERSION greater than 4.1 triggers ignoring SET variables other than TXTHT
         ((and (= var "CNMVERSION") (< 4.1 (atof val1)))
          (setq
            filev42 t
            cflist
             (cons (list 1 var val1) cflist)
          )
         )
         ;;TXTHT gets added to CFLIST
         ((= var "TXTHT")
          (setq cflist (cons (list 1 var val1) cflist))
         )
         ;;If file hasn't been converted yet (configs put in ini)
         ;;All others (unless deprecated) get put in CNM.INI
         ;;and left in with a note for backward compatibility.
         ((not filev42)
          (setq
            var
             (cond
               ((= var "LINSPC") "LineSpacing")
               ((= var "TBLWID") "TableWidth")
               ((= var "PHASEWID") "PhaseWidthAdd")
               ((= var "NDWID") "NumberToDescriptionWidth")
               ((= var "DQWID") "DescriptionToQuantityWidth")
               ((= var "QQWID") "QuantityToQuantityWidth")
               ((= var "QUWID") "QuantityToUnitsWidth")
               ((= var "PHASES") "InsertTablePhases")
               ((= var "CTABONLY") "DoCurrentTabOnly")
               ((= var "PHASEALIAS") "PhaseAlias")
               (t nil)                  ;Don't use unlisted/deprecated variables
             )
          )
          (cond
            ((= var "PhaseAlias")
             (setq
               val2 (haws-rdfld 4 rdlin "W" 1)
               var  (strcat var val1)
               val1 val2
             )
            )
          )
          (if var
            (setq varlist (cons (list var val1) varlist))
          )
          (if (= var "LineSpacing")
            (setq varlist (cons (list "NoteSpacing" val1) varlist))
          )
         )
       )
      )
      ;;TXT2 header.  Turn into comment.
      ((= "NUM" (substr rdlin 1 3))
       (setq
         cflist
          (cons (cons 0 (strcat "NUM" (substr rdlin 5))) cflist)
       )
      )
      ;;Note type/shape heading.
      ((wcmatch (substr rdlin 1 3) typwc)
       (setq nottyp (substr rdlin 1 3))
      )
      ;;Title.
      ((= "TITLE" (substr rdlin 1 5))
       (cond
         (nottyp
          (setq
            cflist
             (cons
               (list
                 2
                 nottyp
                 (haws-rdfld 1 (substr rdlin 6 62) 62 1)
               )
               cflist
             )
          )
         )
         (t (setq alerttitle t))
       )
      )
      ;;Note number.
      ((/= "" (setq notnum (haws-rdfld 1 rdlin 5 1)))
       (cond
         (nottyp
          (setq
            cflist
             (cons
               (list
                 3
                 nottyp
                 notnum
                 (haws-rdfld 1 (substr rdlin 68 3) 3 1)
                 (haws-rdfld 15 rdlin 5 1)
                 (haws-rdfld 1 (substr rdlin 77) 12 3) ;Price
                 (list (haws-rdfld 1 (substr rdlin 6 62) 62 1))
               )
               cflist
             )
          )
         )
         (t (setq alertnote t))
       )
      )
      ;;Additional note description.
      ((= "" (haws-rdfld 1 rdlin 5 1))
       (setq n -1)
       ;;If there's no CFLIST, educate the user.
       (cond
         ((not cflist)
          (alert
            (princ
              (strcat
                "A note continuation line was found in "
                projnotes
                " before any notes.\nPlease edit the file to correct the problem.\n(Note continuations are empty or begin with five spaces.\nFile comments must begin with a semi-colon \";\".)"
              )
            )
          )
          (exit)
         )
         (t
          ;;Find first note in list (there may be comments before it, or in other words, after it in the file).
          (while (and
                   (/= 3 (car (nth (setq n (1+ n)) cflist)))
                   (< n (length cflist))
                 )
          )
          (if (/= n (length cflist))
            (setq
              notdesc   (nth 6 (nth n cflist))
              cflist (cons
                       (subst
                         (reverse
                           (cons
                             (haws-rdfld 1 (substr rdlin 6 62) 62 1)
                             (cond
                               ((= notdesc '("")) nil)
                               ((reverse notdesc))
                             )
                           )
                         )
                         notdesc
                         (nth n cflist)
                       )
                       (cdr cflist)
                     )
            )
          )
         )
       )
      )
    )
  )
  (setq f1 (close f1))
  (if alerttitle
    (alert
      (princ
        (strcat
          "Title(s) were found in"
          projnotes
          "\nthat came before any shape.\n\nThe title(s) will never be printed."
        )
      )
    )
  )
  (if alertnote
    (alert
      (princ
        (strcat
          "Note(s) were found in "
          projnotes
          "\nthat came before any shape.\n\nThe note(s) will never be found or printed."
        )
      )
    )
  )
  ;;Put configs from v4 Project Notes into CNM.INI and alert.
  (cond
    ((and (not filev42) varlist)
     (alert
       (princ
         "\nCNM is moving project settings from version 4.1 Project Notes to CNM.INI."
       )
     )
     (foreach
        entry varlist
       (c:hcnm-config-setvar
         (hcnm_config_entry_var entry)
         (hcnm_config_entry_val entry)
       )
     )
    )
  )
  (setq *hcnm_cnmprojectnotes* (reverse cflist))
  (haws-milepost
    (strcat
      "HCNM_READCFTXT2 read "
      (itoa (length *hcnm_cnmprojectnotes*))
      " lines from "
      projnotes
      "."
    )
  )
  ;;Add comments and version number to old file.
  (cond
    ((not filev42)
     (setq
       *hcnm_cnmprojectnotes*
        (cons
          (list 1 "CNMVERSION" "4.2")
          (reverse cflist)
        )
     )
     (alert
       (princ
         (strcat
           "\nCNM is converting "
           projnotes
           " to a version 4.2 file.\n\nNote: The meaning of the TXTHT setting has changed\nfrom \"Text height in AutoCAD units\"\nto \"Plotted text height\"\n\nCNM is using the current value of DIMSCALE to convert text heights."
         )
       )
     )
     (setq
       f1 (open projnotes "r")
       cflist nil
       iline 0
     )
     (while (setq rdlin (read-line f1))
       (setq iline (1+ iline))
       ;;If the line is recognizable as a vestige of version 4.1 config settings,
       ;;make a note of it for adding a comment.
       (if (or (and
                 (not commentbegin)
                 (= (substr rdlin 1 3) "SET")
                 (/= (haws-rdfld 2 rdlin "W" 1) "TXTHT")
               )
               (= (substr rdlin 1 5) ";SET ")
           )
         (setq commentbegin (1- iline))
       )
       (if
         (=
           rdlin
           ";This section shows how to override program size/scale defaults"
         )
          (setq commentbegin (- iline 2))
       )
       (setq cflist (cons rdlin cflist))
     )
     (setq
       f1    (close f1)
       f2    (open projnotes "w")
       iline 0
     )
     (write-line "SET CNMVERSION 4.2" f2)
     (foreach
        cfitem (reverse cflist)
       (if (= iline commentbegin)
         (write-line
           ";The variable settings section below is not used by CNM 4.2.\n;All variables except TXTHT (optional) and CNMVERSION are in CNM.INI.\n;You can use TXTHT to vary text heights from one line to the next.\n;CNM uses the current DIMTXT for the whole table if TXTHT is omitted,\n;."
           f2
         )
       )
       (setq iline (1+ iline))
       (write-line cfitem f2)
     )
     (setq f2 (close f2))
    )
  )
)


(defun hcnm_readcfcsv (projnotes / cflist notdscstr nottyp rdlin typwc val var wrap
                  )
  (setq
    wrap (atoi (c:hcnm-config-getvar "DescriptionWrap"))
    typwc
     (c:hcnm-config-getvar "NoteTypes")   ; Get typwc (which may open f1) before opening f1
    f1 (open projnotes "r")
  )
  (while (setq rdlin (read-line f1))
    (cond
      ;;Comment
      ((= ";" (substr rdlin 1 1))
       (setq cflist (cons (cons 0 (haws-rdfld 3 rdlin "," 1)) cflist))
      )
      ;;Variable setting
      ((= "SET" (haws-rdfld 1 rdlin "," 1))
       (setq
         cflist
          (cons
            (list
              1
              (setq var (haws-rdfld 2 rdlin "," 1))
              (setq val (haws-rdfld 3 rdlin "," 1))
            )
            cflist
          )
       )
       (cond ((= (strcat var) "WRAP")(setq wrap (atoi val))))
      )
      ;;Note or title
      ((wcmatch (setq nottyp (substr rdlin 1 3)) typwc)
       (cond
         ((= "TITLE" (haws-rdfld 2 rdlin "," 1))
          (setq
            cflist
             (cons
               (list 2 nottyp (haws-rdfld 3 rdlin "," 1))
               cflist
             )
          )
         )
         (t
          (setq
            notdscstr
             (haws-rdfld 3 rdlin "," 1)
            cflist 
             (cons
               (list
                 3
                 nottyp
                 (cond ((haws-rdfld 2 rdlin "," 1)) (""))
                 (cond ((haws-rdfld 4 rdlin "," 1)) (""))
                 (cond ((haws-rdfld 5 rdlin "," 1)) (""))
                 (cond ((haws-rdfld 6 rdlin "," 1)) ("")); Price
                 (hcnm_wrap_description (cond ((haws-rdfld 3 rdlin "," 1)) ("")) wrap)
               )
               cflist
             )
          )
         )
       )
      )
    )
  )
  (setq f1 (close f1))
  (setq *hcnm_cnmprojectnotes* (reverse cflist))
)

(defun hcnm_wrap_description (notdscstr wrap / character_i i i_endline i_newline_prev
                          i_newword_prev inword_p need_wrap_p notdsclst
                          word_provided_p wrap_exceeded_p
                         )
  (setq
    notdsclst nil
    i_newline_prev 1
    i_newword_prev 1
    inword_p t
    i 0
  )
  (while (<= (setq i (1+ i)) (1+ (strlen notdscstr)))
    (setq
      character_i
       (substr notdscstr i 1)
      wrap_exceeded_p
       (>= (- i i_newline_prev) wrap)
      word_provided_p
       (> i_newword_prev i_newline_prev)
    )
    (cond ((or (= character_i "") (and wrap_exceeded_p word_provided_p)) (setq need_wrap_p t)))
    (cond
      ((= "\\n" (substr notdscstr i 2))
       (setq
         notdsclst
          (cons
            (list i_newline_prev (- i i_newline_prev))
            notdsclst
          )
         i_newline_prev
          (+ i 2)
         i_newword_prev
          (+ i 2)
         inword_p t
         need_wrap_p nil
       )
      )
      ((wcmatch character_i " ,\t")
       (setq inword_p nil)
      )
      (t
       (cond
         ((and (/= character_i "")(not inword_p))
          (setq
            i_newword_prev i
            inword_p t
          )
         )
       )
       (cond
         (need_wrap_p
          (setq
            i_newline
             (cond
               ((= character_i "") i)
               (t i_newword_prev)
             )
            notdsclst
             (cons (list i_newline_prev (- i_newline i_newline_prev)) notdsclst)
            i_newline_prev
             i_newline
            need_wrap_p nil
          )
         )
       )
      )
    )
  )
  (setq
    notdsclst
     (mapcar
       '(lambda (i) (substr notdscstr (car i) (cadr i)))
       notdsclst
     )
  )
  (reverse notdsclst)
)

;|
(defun hcnm_wrap_description_test ( / errorstring notdscstr wrap)
  (setq
    notdscstr "A23456789 B23456789 C23456789"
    wrap 2
    errorstring "List of assertions violated:"
    errorstring
     (strcat
       errorstring
       (cond
         ((/= (car (hcnm_wrap_description notdscstr wrap))
              "A23456789 "
          )
          "\nMust leave at least one word on each line"
         )
         ("")
       )
     )
    wrap 11
    errorstring
     (strcat
       errorstring
       (cond
         ((/= (car (hcnm_wrap_description notdscstr wrap))
              "A23456789 "
          )
          "\nMust wrap word 2 to line 3 if it exceeds by many."
         )
         ("")
       )
     )
    wrap 28
    errorstring
     (strcat
       errorstring
       (cond
         ((/= (car (hcnm_wrap_description notdscstr wrap))
              "A23456789 B23456789 "
          )
          "\nMust wrap word 3 to line 3 if it exceeds by one."
         )
         ("")
       )
     )
    wrap 21
    errorstring
     (strcat
       errorstring
       (cond
         ((/= (car (hcnm_wrap_description notdscstr wrap))
              "A23456789 B23456789 "
          )
          "\nMust wrap word 3 to line two if it exceeds by many."
         )
         ("")
       )
     )
    wrap 18
    errorstring
     (strcat
       errorstring
       (cond
         ((/= (car (hcnm_wrap_description notdscstr wrap))
              "A23456789 "
          )
          "\nMust wrap word 2 to line two if it exceeds by one."
         )
         ("")
       )
     )
  )
  ;;(HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP)
  errorstring
)
|;

(defun hcnm_writecftxt2 (projnotes / i item nottyp nottxt nottxtnew)
  (alert
    (princ
      (strcat
        "CNM is converting\n"
        projnotes
        "\nto traditional text format."
      )
    )
  )
  (setq f2 (open projnotes "w"))
  (foreach
     item *hcnm_cnmprojectnotes*
    (cond
      ;;Comment
      ((= 0 (car item)) (write-line (strcat ";" (cdr item)) f2))
      ;;Set variable (TXTHT only at this time)
      ((= 1 (car item))
       (write-line (strcat "SET " (cadr item) " " (caddr item)) f2)
      )
      ;;Title
      ((= 2 (car item))
       (if (/= nottyp (setq nottyp (cadr item)))
         (write-line nottyp f2)
       )
       (write-line (strcat "TITLE " (caddr item)) f2)
      )
      ;;Note
      ((= 3 (car item))
       (if (/= nottyp (setq nottyp (cadr item)))
         (write-line nottyp f2)
       )
       (princ
         (strcat
           (haws-mkfld (caddr item) 5)
           (haws-mkfld (car (nth 5 item)) 62)
           (haws-mkfld (cadddr item) 3)
           (nth 4 item)
           "\n"
         )
         f2
       )
       (foreach
          item (cdr (nth 5 item))
         (princ (strcat "     " item "\n") f2)
       )
      )
    )
  )
  (setq f2 (close f2))
  *hcnm_cnmprojectnotes*
)

(defun hcnm_writecfcsv (projnotes / desc descline item nottyp)
  (alert
    (princ
      (strcat
        "CNM is converting\n"
        projnotes
        "\nto comma-separated format."
      )
    )
  )
  (setq f2 (open projnotes "w"))
  (foreach
     item *hcnm_cnmprojectnotes*
    (cond
      ((= 0 (car item))
       (write-line
         (strcat
           ";"
           (cond
             (nottyp)
             ("CIR")
           )
           ",N/A,"
           (haws-mkfld (cdr item) ",")
         )
         f2
       )
      )
      ((= 1 (car item))
       (write-line (strcat "SET," (cadr item) "," (caddr item)) f2)
      )
      ((= 2 (car item))
       (setq nottyp (cadr item))
       (write-line (strcat nottyp ",TITLE," (caddr item) ",,") f2)
      )
      ((= 3 (car item))
       (setq
         desc ""
         nottyp
          (cadr item)
       )
       (foreach
          descline (nth 5 item)
         (setq desc (strcat desc "\\n" descline))
       )
       (setq desc (substr desc 3))
       (write-line
         (strcat
           nottyp
           ","
           (caddr item)
           ","
           (haws-mkfld desc ",")
           (cadddr item)
           ","
           (nth 4 item)
         )
         f2
       )
      )
    )
  )
  (setq f2 (close f2))
  *hcnm_cnmprojectnotes*
)

(defun hcnm_config_project_notes_format (/ editor format valid_editors)
  (setq
    valid_editors
     (list
       (list "text" "txt2")
       (list "csv" "csv")
       (list "cnm" "csv")
     )
    editor
     (c:hcnm-config-getvar "ProjectNotesEditor")
    format
     (cadr (assoc editor valid_editors))
  )
  (cond
    ((not format)
     (alert
       (princ
         (strcat
           "\nInvalid ProjectNotesEditor. CNM cannot continue.\nUse HCNM-CNMOptions to select your desired editor.\n\nFound ProjectNotesEditor="
           editor
           "\n\nExpected one of these: "
           (apply 'strcat (mapcar '(lambda (x) (strcat "\n" (car x)))valid_editors))
         )
       )
     )
     (exit)
    )
  )
  format
)

;#endregion
;#region Project Notes Editor
;;;================================================================================================================
;;;
;;; Begin Project Notes Editor functions section
;;;
;;;================================================================================================================
(defun c:hcnm-notesedit (/ cnmedit_p noteseditor pnname)
  (setq
    noteseditor (c:hcnm-config-getvar "ProjectNotesEditor")
    ;; wcmatch is legacy hack to be removed when 4.2.29 is deprecated and replaced with translation/conversion on getvar.
    cnmedit_p (wcmatch (strcase noteseditor) "*CNM*")
  )
  (if cnmedit_p
    (haws-core-init 335)
    (haws-core-init 188)
  )
  ;; Since this is a user command, possibly after deletion of project root files,
  ;; refresh project root at beginning.
  (hcnm_projinit)
  ;; Read to convert project notes if necessary before editing
  (setq pnname (hcnm_projnotes))
  (hcnm_readcf pnname)
  (setq pnname (hcnm_projnotes_match_extension pnname noteseditor))
  (princ (strcat "\nEditing " (hcnm_projnotes) "."))
  (cond
    (cnmedit_p
     (startapp
       (strcat
         "\""
         (c:hcnm-config-getvar "AppFolder")
         "\\CNMEdit.exe"
         "\" "
         "\""
         pnname
         "\" "
         "\""
         (hcnm_proj)
         "\\cnm.ini\""
       )
     )
    )
    (t (vl-cmdf "._SH"  (strcat "\"" pnname "\"")))
  )
  (haws-core-restore)
  (princ)
)

(defun hcnm_projnotes_match_extension (projnotes noteseditor)
  (cond
    ((= noteseditor "text")(hcnm_change_filename_extension projnotes "txt"))
    (t(hcnm_change_filename_extension projnotes "csv"))
  )
)

(defun hcnm_change_filename_extension
   (old_filename new_extension / new_filename)
  (cond
    ((/= (haws-filename-extension old_filename) new_extension)
     (setq
       new_filename
        (strcat
          (haws-filename-directory old_filename)
          "\\"
          (haws-filename-base old_filename)
          "."
          new_extension
        )
     )
     (vl-file-rename old_filename new_filename)
     (c:hcnm-config-setvar "ProjectNotes" new_filename)
    )
  )
  new_filename
)

;#endregion
;#region Layers Editor
;;;================================================================================================================
;;;
;;; Begin Layers Editor functions section
;;;
;;;================================================================================================================
;; Edit layer defaults
(defun c:hcnm-cnmlayer (/ layerseditor layersfile wshshell)
(haws-core-init 189)
  (setq
    *haws:layers* nil
    layerseditor
     ;; wcmatch is legacy hack to be removed when 4.2.29 is deprecated and replaced with translation/conversion on getvar.
     (cond
       ((wcmatch
          (strcase (c:hcnm-config-getvar "LayersEditor"))
          "*CNM*"
        )
        (strcat (c:hcnm-config-getvar "AppFolder") "\\CNMLayer.exe")
       )
       (t "notepad.exe")
     )
    layersfile
     (findfile "layers.dat")
  )
  (startapp
    (strcat "\"" layerseditor "\" " "\"" layersfile "\" ")
  )
  (alert
    (strcat
      "Click OK to import layer settings after editing and saving."
    )
  )
  ;;Get a layer to renew *HAWS:LAYERS*  
  (haws-getlayr "NOTES-EXPORT")
  (vl-cmdf "._layer")
  (foreach
     layer *haws:layers*
    (vl-cmdf "_n" (cadr layer))
    (if (/= (caddr layer) "")
      (vl-cmdf "_c" (caddr layer) (cadr layer))
    )
    (vl-cmdf
      "_lt"
      (if (tblsearch "LTYPE" (cadddr layer))
        (cadddr layer)
        ""
      )
      (cadr layer)
    )
  )
  (vl-cmdf "")
  (haws-core-restore)
  (princ)
)


;#endregion
;#region Misc commands
;;;================================================================================================================================================================
;;;
;;; Begin Miscellaneous commands
;;;
;;;================================================================================================================================================================
;;;

;;SETNOTEPHASES
;;Sets the number of phases for this drawing or this folder.
(defun c:haws-setnotephases (/ cflist opt1 phases rdlin)
(haws-core-init 194)
  (initget 1 "Drawing Project")
  (setq
    opt1
     (getkword
       "\nSet number of phases for this drawing only or this project <Drawing/Project>: "
     )
  )
  (initget 1 "None")
  (setq
    phases
     (getint
       "\nNumber of phases to use (or None to ignore bubble note phases): "
     )
  )
  (if (= phases "None")
    (setq phases "0")
    (setq phases (itoa phases))
  )
  (vl-cmdf "._insert" (strcat "noteqty=noteqty" phases))
  (vl-cmdf)
  (cond
    ((= opt1 "Drawing")
     (prompt
       (strcat
         (findfile (strcat "noteqty" phases ".dwg"))
         " inserted to drawing as noteqty."
       )
     )
    )
    ((= opt1 "Project")
     (setq
       f1     (open (hcnm_projnotes) "r")
       cflist nil
     )
     (while (setq rdlin (read-line f1))
       (cond
         ;;If there is a SET PHASES line already, remove it.
         ((and
            (= (haws-rdfld 1 rdlin "W" 1) "SET")
            (= (haws-rdfld 2 rdlin "W" 1) "PHASES")
          )
         )
         ;;Otherwise regurgitate any other line
         ((setq cflist (cons rdlin cflist)))
       )
     )
     (setq
       f1 (close f1)
       cflist
        ;;Put the SET PHASES line at the beginning of the file.
        (cons (strcat "SET PHASES " phases) (reverse cflist))
       f1 (open (hcnm_projnotes) "W")
     )
     (foreach rdlin cflist (write-line rdlin f1))
     (setq f1 (close f1))
    )
  )
  (haws-core-restore)
  (princ)
)

(defun c:haws-cnmmenu ()
(haws-core-init 195)
  (vl-cmdf "._menuunload" "cnm" "._menuload" "cnm.mnu")
  (haws-core-restore)
)

(defun c:haws-cnmsetup (/ acadpathprefix acadpathsuffix i oldacadpath
                    oldprogramfolder programfolder matchlength
                   )
(haws-core-init 196)
  (setq
    programfolder
     (getvar "dwgprefix")
    programfolder
     (substr
       programfolder
       10
       (1- (strlen programfolder (getvar "dwgprefix")))
     )
  )
  (setq
    oldprogramfolder
     (vl-registry-read
       "HKEY_LOCAL_MACHINE\\Software\\HawsEDC\\CNM"
       "ProgramFolder"
     )
    oldacadpath
     (getvar "acadprefix")
    acadpathprefix ""
    acadpathsuffix oldacadpath
  )
  ;;If the old program folder is still in the ACAD path, remove it.
  (if (and
        oldprogramfolder
        (wcmatch oldacadpath (strcat "*" oldprogramfolder "*"))
      )
    (progn
      (while (< (set
                  matchlength
                  (vl-string-mismatch
                    oldacadpath
                    oldprogramfolder
                    (setq
                      i (if i
                          (1+ i)
                          0
                        )
                    )
                  )
                )
                (strlen oldprogramfolder)
             )
      )
      (setq
        acadpathprefix
         (substr oldacadpath 1 i)
        acadpathsuffix
         (substr
           oldacadpath
           (+ i 1 (strlen oldprogramfolder))
         )
      )
    )
  )
  (alert
    (strcat
      "Construction Notes Manager Setup will now add\n"
      programfolder
      "\nto the current user profile's\nAutoCAD Support Files Search Path\nand load the CNM menu."
    )
  )
  (vl-registry-write
    "HKEY_LOCAL_MACHINE\\Software\\HawsEDC\\CNM"
    "ProgramFolder"
    programfolder
  )
  (vl-registry-write
    (strcat
      "HKEY_CURRENT_USER\\"
      (vlax-product-key)
      "\\Profiles\\"
      (getvar "CPROFILE")
      "\\General"
    )
    "ACAD"
    (strcat acadpathprefix programfolder ";" acadpathsuffix)
  )
  (vl-cmdf "._menuunload" "cnm" "._menuload" "cnm")
  (vl-file-delete (strcat programfolder "\\acaddoc.lsp"))
  (alert
    "Construction Notes Manager setup is done.\n\nYou may now explore the CNM menus and toolbar\nafter restarting AutoCAD."
  )
  (haws-core-restore)
)
(defun c:haws-ntpurge (/ ol pl plss)
(haws-core-init 197)
  (setq
    ol (getvar "clayer")
    pl (car (haws-getlayr "NOTESEXP"))
  )
  (vl-cmdf "._erase" (ssget "X" (list (cons 8 pl))) "")
  (setvar "clayer" ol)
  (vl-cmdf "._purge" "_b" "noteqty*,cnm*" "_n")
  (if (setq plss (ssget "X" (list (cons 8 pl))))
    (alert
      (strcat
        "All entities on the "
        pl
        " layer\nhave been erased from the current tab.\n\n"
        (itoa (sslength plss))
        " objects were not in the current tab\nand must still be erased."
      )
    )
  )
  (haws-core-restore)
  (princ)
)

;#endregion
;#region Bubble notes utility commands
;;;================================================================================================================
;;;
;;; Begin Bubble Notes commands
;;;
;;;================================================================================================================
;;; SETNOTESBUBBLESTYLE
;;; Saves the users preferred Notes Bubble Style to the registry
(defun c:hcnm-setnotesbubblestyle (/ bubblehooks)
(haws-core-init 190)
  (initget "Yes No")
  (setq
    bubblehooks
     (getkword
       "\nInsert bubble notes with hooks? [Yes/No]: "
     )
  )
  (if bubblehooks
    (c:hcnm-config-setvar
      "BubbleHooks"
      (cond
        ((= bubblehooks "Yes") "1")
        ("0")
      )
    )
  )
  (haws-core-restore)
  (princ)
)
;;; Global edit of bubble note phases
(defun c:haws-phaseedit (/ newphase oldphase)
(haws-core-init 191)
  (setq
    oldphase
     (getstring "\nEnter phase to change: ")
    newphase
     (getstring "\nEnter new phase: ")
  )
  (vl-cmdf
    "._attedit" "_n" "_n" "note???l,note???r" "notephase" "*" oldphase
    newphase
   )
  (graphscr)
  (haws-core-restore)
  (princ)
)
;;; Put attributes on NOPLOT layer
(defun c:hcnm-attnoplot ()
(haws-core-init 192)
  (hcnm_attlayer "NOTESNOPLOT")
  (vl-cmdf
    "._layer"
    "_Plot"
    "_No"
    (haws-getlayr "NOTESNOPLOT")
    ""
  )
  (haws-core-restore)
)
(defun c:hcnm-attplot () (hcnm_attlayer "0"))
(defun hcnm_attlayer (layer / at el en et nplayer nplist sset sslen)
  (haws-core-init 193)
  (haws-vsave '("CLAYER"))
  (vl-cmdf "._undo" "_g")
  (setq nplayer (car (haws-getlayr layer)))
  (if (not (tblsearch "LAYER" nplayer))
    (haws-mklayr layer)
  )
  (prompt "\nBlocks to change: ")
  (setq sset (ssget '((0 . "INSERT"))))
  (if (not sset)
    (progn (prompt "\nNone found.") (exit))
    (progn
      (while (setq
               en (car
                    (nentsel
                      (strcat
                        "\nAttributes to change to layer "
                        nplayer
                        " by example/<enter when finished>: "
                      )
                    )
                  )
             )
        (if (= "ATTRIB" (cdr (assoc 0 (entget en))))
          (progn
            (redraw en 3)
            (setq
              nplist
               (cons (list (cdr (assoc 2 (entget en))) en) nplist)
            )
          )
        )
      )
      (foreach en nplist (redraw (cadr en) 4))
      ;; Change all of the entities in the selection set.
      (prompt
        (strcat "\nPutting attributes on " nplayer " layer...")
      )
      (setq sslen (sslength sset))
      (while (> sslen 0)
        (setq en (ssname sset (setq sslen (1- sslen))))
        (while (and
                 (setq en (entnext en))
                 (/= "SEQEND"
                     (setq et (cdr (assoc 0 (setq el (entget en)))))
                 )
               )
          (cond
            ((and
               (= et "ATTRIB")
               (setq at (cdr (assoc 2 el)))
               (assoc at nplist)
             )
             (entmod (subst (cons 8 nplayer) (assoc 8 el) el))
             (entupd en)
            )
          )
        )
      )
      (prompt "done.")
    )
  )
  (vl-cmdf "._undo" "_e")
  (haws-vrstor)
  (haws-core-restore)
  (princ)
)

;#endregion
;#region Legacy LDRBLK (not for CNM)
;;; ------------------------------------------------------------------------------
;;; LDRBLK.LSP
;;; (C) Copyright 1997 by Thomas Gail Haws
;;; Thomas Gail Haws, Feb. 1996
;;; LDRBLK attaches a left or right block to an AutoCAD LEADER and fills in 
;;; special attributes for bubble notes.
;;; LDRBLK explodes any block inserted that doesn't have attributes.
;;; 
;;; Developer notes for NOTE* blocks:
;;; Each NOTE???D block contains a NOTE???L, a NOTE???R block, and left and right NOTEHK blocks.
;;; Each L or R block contains a pline shape and attributes.
;;; The NOTETYPE attribute is preset to the shape of the block.
;;; I use a script to make changes to all the blocks by exploding, editing,
;;; then wblocking from each D block to the L and R blocks.
;;; I make the root entities layer zero so layers.dat truly controls all layers.
;;;
;;; Hint: Toggle the [INSERT] key off to fill in the blanks for a new command.
;;;       This will keep the columns aligned and easy to read.
;;; 
;;;|COMMAND|LEFT BLK |RIGHT BLK |DRAG BLK |LAYER KEY |DIMSTYLE KEY
;;; -------------------------------------------------------------------------
(defun c:haws-tcg ()
(haws-core-init 208)
  (haws-ldrblk
    "ldrtcgl" "ldrtcgr" "ldrtcgd" "TCGLDR" "TCGLeader"
   )
  (haws-core-restore)
)
(defun c:haws-txtl ()
  (haws-ldrblk
    "ldrtxtl" "ldrtxtr" "ldrtxtd" "NOTESLDR" "NotesLeader"
   )
)

(defun haws-ldrblk (blleft blrght bldrag bllay bldsty / apold as associate_p ang auold
                blgf blline blk dsty dstyold dtold el en enblk endrag
                fixhook fixphase fixtxt3 i p1 p2 p3 p4 p5 p6 p7 p8
                pfound r1 ds ts left num txt1 txt2 ang1 ang2 fixorder
                osmold
               )
  (haws-core-init 209)
  (haws-vsave
    '("aperture" "attdia" "attreq" "aunits" "clayer" "cmdecho" "osmode"
      "plinegen" "regenmode"
     )
  )
  (vl-cmdf "._undo" "_g")
  ;; Block isn't annotative. Can't associate with annotative leader.
  (setq
    associate_p
     (cond
       ((= (getvar "DIMANNO") 1) nil)
       (t)
     )
  )
  (hcnm_projinit)
  (hcnm_set_dimstyle (strcat bldsty "Dimstyle"))
  (setvar "osmode" 0)
  (haws-mklayr bllay)
  (setq
    p1 (getpoint "\nStart point for leader:")
  )
  (setq
    ds (haws-dwgscale)
    ts (* ds (getvar "dimtxt"))
    as (* ds (getvar "dimasz"))
  )
  (vl-cmdf
    "._insert"
    bldrag
    "_Scale"
    ts
    "_Rotate"
    (angtos (getvar "snapang"))
    p1
  )
  (setq en (entlast))
  (prompt "\nEnd point for leader: ")
  (vl-cmdf "._move" en "" p1 pause)
  (setq
    p2     (trans (cdr (assoc 10 (entget (entlast)))) (entlast) 1)
    ang    (angle p1 p2)
    left   (minusp (cos (+ ang (getvar "snapang"))))
    p3     (polar p1 ang as)
    p4     (polar
             p2
             (if left
               pi
               0
             )
             as
           )
    p5     (polar p4 (/ pi 2) (* ts 3.0))
    p6     (polar
             (polar
               p5
               (if left
                 pi
                 0
               )
               (* ts 10)
             )
             (/ pi -2)
             (* ts 6.0)
           )
    endrag (entlast)
  )
  (setvar "attdia" 0)
  (setvar "attreq" 1)
  (cond
    ((>= (atof (getvar "acadver")) 14)
     (vl-cmdf "._leader" p1 p2 "" "")
     (cond
       (associate_p (vl-cmdf "_block"))
       (t (vl-cmdf "_none" "._INSERT"))
     )
    )
  )
  (setq auold (getvar "aunits"))
  (setvar "aunits" 3)
  (vl-cmdf
    (if left
      blleft
      blrght
    )
    p2
    ts
    ts
    (getvar "snapang")
  )
  (setvar "aunits" auold)
  (vl-cmdf "._erase" endrag "")
  (if (not (entnext (entlast)))
    (vl-cmdf
      "._explode"
      "_l"
      "._change"
      "_p"
      ""
      "_p"
      "_la"
      (getvar "clayer")
      ""
    )
  )
  (hcnm_restore_dimstyle)
  (haws-vrstor)
  (vl-cmdf "._undo" "_e")
  (haws-core-restore)
  (princ)
)
;;end of LDRBLK

;#endregion
;#region Bubble insertion and editing


(defun c:haws-boxl () (haws-core-init 198) (hcnm_ldrblk_dynamic "BOX"))
(defun c:haws-cirl () (haws-core-init 199) (hcnm_ldrblk_dynamic "CIR"))
(defun c:haws-dial () (haws-core-init 200) (hcnm_ldrblk_dynamic "DIA"))
(defun c:haws-elll () (haws-core-init 201) (hcnm_ldrblk_dynamic "ELL"))
(defun c:haws-hexl () (haws-core-init 202) (hcnm_ldrblk_dynamic "HEX"))
(defun c:haws-octl () (haws-core-init 203) (hcnm_ldrblk_dynamic "OCT"))
(defun c:haws-penl () (haws-core-init 204) (hcnm_ldrblk_dynamic "PEN"))
(defun c:haws-recl () (haws-core-init 205) (hcnm_ldrblk_dynamic "REC"))
(defun c:haws-sstl () (haws-core-init 206) (hcnm_ldrblk_dynamic "SST"))
(defun c:haws-tril () (haws-core-init 207) (hcnm_ldrblk_dynamic "TRI"))
(defun c:hcnm-replace-bubble () (haws-core-init 338) (hcnm_ldrblk_dynamic nil))

(defun hcnm_ldrblk_dynamic (notetype / blockname bubble_data bubblehooks
                        ename_bubble_old replace_bubble_p
                        th
                       )
  ;; Workaround for intermittent first-insertion crash bug:
  ;; On first bubble insertion in a fresh drawing session, AutoCAD's command/input
  ;; system can be uninitialized, causing crashes at GETKWORD prompts (specifically
  ;; the dimension scale prompt). Issuing any command first initializes the system.
  ;; Must use (COMMAND) not (VL-CMDF) - synchronous execution is required.
  (princ "\nCNM version: ")
  (princ (haws-unified-version))
  (princ "\nCNM bubble insertion often crashes the first time in a drawing session, possibly when it's the first command or the first block insertion. Please let us know if you can confirm a pattern.")
  (command "._REDRAW")
  (haws-vsave '("attreq" "aunits" "clayer" "cmdecho"))
  (cond
    ((and (getvar "wipeoutframe") (/= (getvar "wipeoutframe") 2))
     (alert "Setting WIPEOUTFRAME to 2 to show but not plot")
     (setvar "wipeoutframe" 2)
    )
  )
  (cond
    ((= (getvar "ANNOALLVISIBLE") 0)
     (initget "Yes No")
     (cond
       ((/=
          (getkword
            "ANNOALLVISIBLE is 0. Set to 1 so that bubble notes appear at all scales? [Yes/No] <Yes>: "
          )
          "No"
        )
        (setvar "ANNOALLVISIBLE" 1)
       )
     )
    )
  )
  (vl-cmdf "._undo" "_g")
  (hcnm_projinit)
  (hcnm_set_dimstyle "NotesLeaderDimstyle")
  (setq
    bubblehooks
     (c:hcnm-config-getvar "BubbleHooks")
    blockname
     (strcat
       "cnm-bubble-"
       (hcnm_ldrblk_get_mtext_string)
       (cond
         ((= (strcase bubblehooks) "YES") "1")
         ((= (strcase bubblehooks) "NO") "0")
         (t bubblehooks)
       )
     )
    th (* (getvar "dimtxt")
          (if (getvar "DIMANNO")
            1
            (getvar "DIMTXT")
          )
       )
  )
  (haws-mklayr "NOTESLDR")
  (setvar "attreq" 0)
  (setq bubble_data (hcnm_ldrblk_bd_set bubble_data "TH" th)
        bubble_data (hcnm_ldrblk_bd_set bubble_data "BLOCKNAME" blockname)
        bubble_data (hcnm_ldrblk_bd_set bubble_data "NOTETYPE" notetype)
        bubble_data (hcnm_ldrblk_bd_set bubble_data "REPLACE_BUBBLE_P" (not notetype))
        bubble_data (hcnm_ldrblk_get_ename_bubble_old bubble_data)
        bubble_data (cond 
                      ((hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE_OLD")
                       (hcnm_ldrblk_bd_ensure_p1_world bubble_data) ;  WE REALLY ONLY NEED ENAME_LEADER_OLD AND P1_OCS, BUT THIS ISN'T A BAD WAY TO GET IT.
                      )
                      (t
                       (hcnm_ldrblk_get_user_start_point bubble_data)
                      )
                    )
        notetype    (cond 
                      (notetype)
                      ((hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
                       (lm:getdynpropvalue 
                         (vlax-ename->vla-object 
                           (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
                         )
                         "Shape"
                       )
                      )
                      (t notetype)
                    )
        bubble_data (hcnm_ldrblk_bd_set bubble_data "NOTETYPE" notetype)
  )
  ;; Draw bubble, update BUBBLE_DATA with P2 and new entities
  (setq bubble_data (hcnm_ldrblk_get_p2_data bubble_data))
  (setq bubble_data (hcnm_ldrblk_draw_bubble bubble_data))
  (setq bubble_data (hcnm_ldrblk_get_bubble_data bubble_data))
  (hcnm_ldrblk_finish_bubble bubble_data)
  (hcnm_restore_dimstyle)
  (haws-vrstor)
  (vl-cmdf "._undo" "_e")
  (haws-core-restore)
  (princ)
)
;; Create a bubble data structure (alist) for passing state
;; All parameters optional - pass nil for unset fields
(defun hcnm_ldrblk_bd_def ()
  (list
    (cons "ATTRIBUTES" nil)
    (cons "AVPORT" nil)
    (cons "BLOCKNAME" nil)
    (cons "ENAME_BUBBLE" nil)
    (cons "ENAME_BUBBLE_OLD" nil)
    (cons "ENAME_LAST" nil)
    (cons "ENAME_LEADER" nil)
    (cons "ENAME_LEADER_OLD" nil)
    (cons "NOTETYPE" nil)
    (cons "P1_OCS" nil)
    (cons "P1_UCS" nil)
    (cons "P1_WORLD" nil)
    (cons "P2" nil)
    (cons "PSPACE_BUBBLE_P" nil)
    (cons "REPLACE_BUBBLE_P" nil)
    (cons "TH" nil)  )
)

;;==============================================================================
;; HCNM_LDRBLK_BD Module - Bubble Data Accessors
;;==============================================================================
;; Provides typed accessors for bubble data alist structure.
;; BD = "Bubble Data" (commonly used abbreviation in this module)
;; LDRBLK = "Leader Block" (consistent with rest of codebase)
;;==============================================================================

;; Get a value from bubble data using HAWS_NESTED_LIST_GET
(defun hcnm_ldrblk_bd_get (bd key)
  (haws_nested_list_get bd (list key))
)

;; Set a value in bubble data using HAWS_NESTED_LIST_UPDATE
;; Validates key against known schema
(defun hcnm_ldrblk_bd_set (bd key val )
   (if (not (assoc key (hcnm_ldrblk_bd_def)))
    (progn
      (princ (strcat "\nError: Invalid BUBBLE_DATA key: " key))
      bd
    )
    (haws_nested_list_update bd (list key) val)
  )
)

;; Ensure P1_WORLD is present in bubble data (computes if missing)
(defun hcnm_ldrblk_bd_ensure_p1_world (bubble_data / ename_bubble ename_leader p1_ocs p1_world)
  (and
    (setq ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE"))
    (or
      (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER")
      (setq bubble_data (hcnm_ldrblk_bd_set
                          bubble_data
                          "ENAME_LEADER"
                          (hcnm_ldrblk_bubble_leader ename_bubble)
                        )
      )
      (princ "\nError in HCNM_LDRBLK_BD_ENSURE_P1_WORLD: Could not find leader associated with bubble note.")
    )
    (setq ename_leader (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER"))
    (or
      (hcnm_ldrblk_bd_get bubble_data "P1_OCS")
      (setq bubble_data (hcnm_ldrblk_bd_set bubble_data "P1_OCS" (hcnm_ldrblk_p1_ocs ename_leader)))
      (princ "\nError in HCNM_LDRBLK_BD_ENSURE_P1_WORLD: Could not determine P1_OCS from leader.")
    )
    (setq p1_ocs (hcnm_ldrblk_bd_get bubble_data "P1_OCS"))
    (or
      (hcnm_ldrblk_bd_get bubble_data "P1_WORLD")
      (setq bubble_data (hcnm_ldrblk_bd_set bubble_data "P1_WORLD" (hcnm_ldrblk_p1_world ename_leader p1_ocs ename_bubble)))
      (princ "\nError in HCNM_LDRBLK_BD_ENSURE_P1_WORLD: Could not compute P1_WORLD from P1_OCS.")
    )
    (setq p1_world (hcnm_ldrblk_bd_get bubble_data "P1_WORLD"))
    (princ (strcat "\nDebug P1_OCS: " (vl-princ-to-string p1_ocs) " P1_WORLD: " (vl-princ-to-string p1_world)))
  )
  bubble_data
)

(defun hcnm_ldrblk_get_ename_bubble_old (bubble_data / elist_block_old ename_bubble_old replace_bubble_p)
  (setq replace_bubble_p (hcnm_ldrblk_bd_get bubble_data "REPLACE_BUBBLE_P"))
  (cond
    (replace_bubble_p
     ;; Prompt and check for old block.
     (while (or (not
                  (setq
                    ename_bubble_old
                     (car (entsel "\nSelect bubble note: "))
                  )
                )
                (not (setq elist_block_old (entget ename_bubble_old)))
                (not
                  (and
                    (= (cdr (assoc 0 elist_block_old)) "INSERT")
                    (wcmatch
                      (strcase
                        (vla-get-effectivename
                          (vlax-ename->vla-object ename_bubble_old)
                        )
                      )
                      "CNM-BUBBLE-*"
                    )
                  )
                )
            )
       (princ "\nSelected entity is not a CNM bubble note.")
     )
     (setq bubble_data (hcnm_ldrblk_bd_set bubble_data "ENAME_BUBBLE_OLD" ename_bubble_old))
    )
    (t nil)
  )
  bubble_data
)
(defun hcnm_ldrblk_get_user_start_point (bubble_data) 
    (hcnm_ldrblk_bd_set bubble_data "P1_UCS" (getpoint "\nStart point for leader:"))
)
(defun hcnm_ldrblk_bubble_leader (ename_bubble / elist_bubble ename_330 ename_leader) 
  (setq elist_bubble (entget ename_bubble))
  ;; Get start point
  ;; Find associated leader.
  (while  ;; Check all 330 groups
    (and 
      (not ename_leader)
      (setq ename_330 (cdr (assoc 330 elist_bubble)))
    )
    ;; Use the one that refers back to this block. Or move to the next one.
    (cond 
      ((eq (cdr (assoc 340 (entget ename_330))) ename_bubble)
       (setq ename_leader ename_330)
      )
      (t
       (setq elist_bubble (cdr 
                            (member 
                              (assoc 330 
                                     elist_bubble
                              )
                              elist_bubble
                            )
                          )
             ename_leader nil
       )
      )
    )
  )
  ename_leader
)
(defun hcnm_ldrblk_p1_ocs (ename_leader)
  (cond 
    (ename_leader
     (cdr (assoc 10 (entget ename_leader)))
    )
    (t
     nil
    )
  )
)
;; Gets insertion point of bubble in UCS coordinates
;; Bubble still doesn't exist. Draws temp bubbles only.
(defun hcnm_ldrblk_get_p2_data (bubble_data / ename_bubble_temp p1_ucs p2 ss1 obj_bubble_temp th blockname notetype)
  (setq
    p1_ucs (hcnm_ldrblk_bd_get bubble_data "P1_UCS")
    th (hcnm_ldrblk_bd_get bubble_data "TH")
    blockname (hcnm_ldrblk_bd_get bubble_data "BLOCKNAME")
    notetype (hcnm_ldrblk_bd_get bubble_data "NOTETYPE")
    ss1 (ssadd)
  )
  (foreach flipstate '("right" "left")
    (vl-cmdf
      "._insert"
      (strcat blockname "-" flipstate)
      "_Scale"
      th
      "_Rotate"
      (angtos (getvar "snapang"))
      p1_ucs
    )
    (setq
      ename_bubble_temp (entlast)
      obj_bubble_temp (vlax-ename->vla-object ename_bubble_temp)
    )
    (lm:setdynpropvalue obj_bubble_temp "Shape" notetype)
    (ssadd ename_bubble_temp ss1)
  )
  (prompt "\nLocation for bubble: ")
  (vl-cmdf "._MOVE" ss1 "" p1_ucs pause)
  (setq
    p2 (trans (cdr (assoc 10 (entget ename_bubble_temp))) ename_bubble_temp 1)
    bubble_data (hcnm_ldrblk_bd_set bubble_data "P2" p2)
  )
  (vl-cmdf "._erase" ss1 "")
  bubble_data
)
;; Draw bubble and update BUBBLE_DATA with new leader/block info
(defun hcnm_ldrblk_draw_bubble (bubble_data / p1_ucs ename_bubble ename_bubble_old 
                                ename_leader p2 ang1 flipstate associate_p auold th 
                                blockname notetype input1 elist_leader_old
                               ) 
  (setq p1_ucs           (hcnm_ldrblk_bd_get bubble_data "P1_UCS")
        ename_bubble_old (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE_OLD")
        ename_leader_old (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER_OLD")
        p2               (hcnm_ldrblk_bd_get bubble_data "P2")
        th               (hcnm_ldrblk_bd_get bubble_data "TH")
        blockname        (hcnm_ldrblk_bd_get bubble_data "BLOCKNAME")
        notetype         (hcnm_ldrblk_bd_get bubble_data "NOTETYPE")
        ang1             (- (angle p1_ucs p2) (getvar "snapang"))
        flipstate        (cond ((minusp (cos ang1)) "left") (t "right"))
  )
  (cond 
    ;; If it's not a new insertion, don't draw a leader.
    (ename_bubble_old
     (setq auold (getvar "aunits"))
     (setvar "aunits" 3)
     (vl-cmdf 
       "._insert"
       (strcat blockname "-" flipstate)
       "_Scale"
       th
       "_Rotate"
       (getvar "snapang")
       p2
     )
     (setvar "aunits" auold)
     (setq ename_bubble (entlast))
     ;; If there is an old leader, stretch it and associate it.
     (cond 
       (ename_leader_old
        (setq elist_leader_old (entget ename_leader_old))
        ;; Change its arrowhead if needed.
        (hcnm_ldrblk_change_arrowhead ename_leader_old)
        ;; Stretch it.
        (entmod 
          (subst 
            (cons 10 p2)
            (assoc 
              10
              (cdr 
                (member (assoc 10 elist_leader_old) elist_leader_old)
              )
            )
            elist_leader_old
          )
        )
        (vl-cmdf 
          "._qldetachset"
          ename_leader_old
          ""
          "._qlattach"
          ename_leader_old
          (entlast)
          "._draworder"
          (entlast)
          ""
          "_front"
        )
       )
     )
    )
    (t
     (setq associate_p (cond ((= (getvar "DIMANNO") 1) t) (nil)))
     (cond 
       ((and (not associate_p) 
             (getvar "CANNOSCALEVALUE")
             (/= (getvar "DIMSCALE") (/ 1.0 (getvar "CANNOSCALEVALUE")))
        )
        (alert 
          (princ 
            (strcat 
              "\nDimension scale ("
              (rtos (getvar "DIMSCALE") 2 2)
              ") and\nAnnotation scale ("
              (rtos (/ 1.0 (getvar "CANNOSCALEVALUE")) 2 2)
              ")\nare not equal.\nCNM recommends setting dimension scale to match annotation scale."
            )
          )
        )
        ;; Workaround: Initialize command/input system before GETKWORD
        (command "._REDRAW")
        (initget 1 "Yes No")
        (setq input1 (getkword "\nSet dimension scale to match annotation scale? [Yes/No]: "))
        (cond 
          ((= input1 "Yes") (setvar "DIMSCALE" (/ 1.0 (getvar "CANNOSCALEVALUE"))))
        )
       )
     )
     (setq ang1      (- (angle p1_ucs p2) (getvar "snapang"))
           flipstate (cond ((minusp (cos ang1)) "left") (t "right"))
     )
      ;; SAVE LAST ENTITY FOR ENTNEXT USAGE.
     (setq bubble_data (hcnm_ldrblk_bd_set bubble_data "ENAME_LAST" (entlast)))
     ;;Start insertion
     (cond 
       ((>= (atof (getvar "acadver")) 14)
        (vl-cmdf "._leader" p1_ucs p2 "_Annotation" "")
        (cond (associate_p (vl-cmdf "_block")) (t (vl-cmdf "_none" "._INSERT")))
       )
       (t
        (alert (princ "\nThe bubble notes inserter in CNM 4.2.3 and higher is not compatible with AutoCAD pre-R14."))
       )
     )
     (setq auold (getvar "aunits"))
     (setvar "aunits" 3)
     (vl-cmdf (strcat blockname "-" flipstate) "_Scale" th p2 (getvar "snapang"))
     (setvar "aunits" auold)
     (setq ename_bubble (entlast)
           bubble_data  (hcnm_ldrblk_bd_set bubble_data "ENAME_BUBBLE" ename_bubble)
     )
    )
  )
  bubble_data
)
(defun hcnm_ldrblk_get_bubble_data (bubble_data / attribute_list ename_bubble p1_ucs num)
  (setq
    replace_bubble_p (hcnm_ldrblk_bd_get bubble_data "REPLACE_BUBBLE_P")
    ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
    p1_ucs (hcnm_ldrblk_bd_get bubble_data "P1_UCS")
  )
  (cond
    (ename_replace_bubble_p
     (setq attribute_list (hcnm_get_attributes ename_bubble t))
    )
    (t
     (initget 128 "Copy")
     (setq num (getkword "\nNote number or [Copy note]: "))
     (cond
       ((= num "Copy")
        (setq
          attribute_list
           (hcnm_get_attributes
             (setq ename_bubble (car (entsel)))
             t
           )
        )
       )
       (t
        (setq attribute_list (hcnm_ldrblk_initialize_attribute_list))
        (mapcar
          '(lambda (index)
             (setq
               attribute_list
                (hcnm_ldrblk_get_text_entry
                  ename_bubble
                  index
                  attribute_list
                )
             )
           )
          '(1 2 3 4 5 6 0)
        )
       )
     )
    )
  )
  (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" (hcnm_ldrblk_adjust_formats attribute_list))
)
(defun hcnm_ldrblk_finish_bubble (bubble_data / ename_bubble ename_bubble_old ename_last ename_leader ename_temp replace_bubble_p attributes notetype)
  (setq
    ename_last (hcnm_ldrblk_bd_get bubble_data "ENAME_LAST")
    ename_temp ename_last
    ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
    ename_bubble_old (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE_OLD")
    replace_bubble_p (hcnm_ldrblk_bd_get bubble_data "REPLACE_BUBBLE_P")
    attributes (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
    notetype (hcnm_ldrblk_bd_get bubble_data "NOTETYPE")
  )
  (hcnm_ldrblk_set_dynprops ename_bubble ename_bubble_old notetype replace_bubble_p)
  (if replace_bubble_p (entdel ename_bubble_old))
  (hcnm_set_attributes ename_bubble attributes)
  ;; Use entnext to look for a leader starting from the last entity before this bubble.
  (while
    (and
      (/= "LEADER" (cdr (assoc 0 (entget (setq ename_temp (entnext ename_temp))))))
    )
  )
  (setq ename_leader ename_temp)
  ;; Change leader arrowhead if needed.
  (hcnm_ldrblk_change_arrowhead ename_leader)
)
(defun hcnm_ldrblk_get_mtext_string ()
  (cond
    ((= (c:hcnm-config-getvar "BubbleMtext") "1") "m-")
    (t "")
  )
)
(defun hcnm_ldrblk_change_arrowhead (ename_leader)
  (cond
    ((= (c:hcnm-config-getvar "BubbleArrowIntegralPending") "1")
     ;; 18 is "Integral" arrowhead type.
     (vla-put-arrowheadtype
       (vlax-ename->vla-object ename_leader)
       18
     )
     (c:hcnm-config-setvar "BubbleArrowIntegralPending" "0")
    )
  )
)
(defun hcnm_ldrblk_set_dynprops (ename_bubble_new ename_bubble_old notetype replace_bubble_p /  dyn_props_old dyn_props_old_i vlaobj_block_new vlaobj_block_old)
  (setq
    vlaobj_block_new
     (vlax-ename->vla-object ename_bubble_new)
  )
  (cond
    (ename_bubble_old
     (setq
       vlaobj_block_old
        (vlax-ename->vla-object ename_bubble_old)
       dyn_props_old
        (mapcar
          '(lambda (x)
             (list
               (vlax-get-property x 'PROPERTYNAME)
               (vlax-get-property x 'VALUE)
               x
             )
           )
          (vlax-invoke
            vlaobj_block_old
            'GETDYNAMICBLOCKPROPERTIES
          )
        )
     )
     (foreach
        vlaobj_property_new
        (vlax-invoke vlaobj_block_new 'GETDYNAMICBLOCKPROPERTIES)
       (if (and
             (setq
               dyn_props_old_i
                (assoc
                  (vlax-get-property
                    vlaobj_property_new
                    'PROPERTYNAME
                  )
                  dyn_props_old
                )
             )
             (/= (vlax-get-property vlaobj_property_new 'READONLY)
                 :vlax-true
             )
           )
         (vlax-put-property
           vlaobj_property_new
           'VALUE
           (cadr dyn_props_old_i)
         )
       )
     )
    )
    (t (lm:setdynpropvalue vlaobj_block_new "Shape" notetype))
  )
)
(defun hcnm_ldrblk_initialize_attribute_list (/ attribute_list)
  (setq
    attribute_list
     (mapcar
       '(lambda (index)
          (list
            (strcat "NOTETXT" (itoa index))
            ""
          )
        )
       '(1 2 3 4 5 6 0)
     )
    attribute_list
     (cons (list "NOTEDATA" "") attribute_list)
    attribute_list
     (cons (list "NOTEGAP" "") attribute_list)
    attribute_list
     (cons (list "NOTENUM" num) attribute_list)
  )
  attribute_list
)
;;; Save attribute value to attribute list (replaces entire value)
(defun hcnm_ldrblk_save_attribute_to_list (tag value attribute_list / attr)
  (setq attr (assoc tag attribute_list))
  (subst (list tag value) attr attribute_list)
)
;;; Save auto-generated text using search-and-replace in delimiter structure
;;; - Gets old auto-text from "auto" field (between delimiters)
;;; - Replaces it with new auto-text
;;; - Preserves prefix and postfix from user edits
;;; This allows users to add prefix/postfix text while still enabling auto-updates
(defun hcnm_ldrblk_save_auto_to_list (tag auto_new attribute_list / attr value parts prefix postfix value_new)
  (setq attr (assoc tag attribute_list)
        value (cadr attr)
        parts (hcnm_eb:split_on_nbsp value)
        prefix (nth 0 parts)
        postfix (nth 2 parts)
        value_new (hcnm_eb:concat_parts prefix auto_new postfix))
  (subst (list tag value_new) attr attribute_list)
)
(defun hcnm_ldrblk_adjust_formats (attribute_list / bubblemtext txt1 txt2 txt1_raw txt2_raw
                               gap overline underline
                              )
  ;; Adjust underlining and overlining
  ;; Only add underline to line 1 and overline to line 2 if they have any content
  (setq
    bubblemtext
     (hcnm_ldrblk_get_mtext_string)
    underline
     (cond
       ((= bubblemtext "") "%%u")
       (t "\\L")
     )
    overline
     (cond
       ((= bubblemtext "") "%%o")
       (t "\\O")
     )
    txt1_raw (cadr (assoc "NOTETXT1" attribute_list))
    txt2_raw (cadr (assoc "NOTETXT2" attribute_list))
    ;; Only apply formatting if line has actual content
    txt1
     (cond
       ((hcnm_ldrblk_attr_has_content_p txt1_raw)
        (hcnm_ldrblk_adjust_format txt1_raw underline)
       )
       (t txt1_raw)  ; No content, return as-is (could be "" or "§§")
     )
    txt2
     (cond
       ((hcnm_ldrblk_attr_has_content_p txt2_raw)
        (hcnm_ldrblk_adjust_format txt2_raw overline)
       )
       (t txt2_raw)  ; No content, return as-is (could be "" or "§§")
     )
    ;; GAP gets underline + two spaces if either line has content
    ;; NOTEGAP is system-controlled and goes in prefix field with delimiters
    gap
     (cond
       ;; If both lines empty, gap is empty (with empty delimiter structure)
       ((and (not (hcnm_ldrblk_attr_has_content_p txt1_raw))
             (not (hcnm_ldrblk_attr_has_content_p txt2_raw))
        )
        (hcnm_eb:concat_parts "" "" "")
       )
       ;; Otherwise, gap is "%%u  " (underline + 2 spaces) in prefix field
       (t 
        (hcnm_eb:concat_parts "%%u  " "" "")
       )
     )
    attribute_list
     (hcnm_ldrblk_save_attribute_to_list
       "NOTETXT1"
       txt1
       attribute_list
     )
    attribute_list
     (hcnm_ldrblk_save_attribute_to_list
       "NOTETXT2"
       txt2
       attribute_list
     )
    attribute_list
     (hcnm_ldrblk_save_attribute_to_list
       "NOTEGAP"
       gap
       attribute_list
     )
  )
  ;; Strip mtext codes if not using mtext
  ;; Disabled because LM:Unformat breaks field codes
  ;|(COND
    ((= bubblemtext "")
     (setq
       attribute_list
        (mapcar
          '(lambda (attribute)
             (cond
               ((wcmatch (cadr attribute) "*\\*")
                (list
                  (car attribute)
                  (lm:unformat (cadr attribute) nil)
                )
               )
               (attribute)
             )
           )
          attribute_list
        )
     )
    )
  )
  |;
)
;;; Check if attribute has actual content (not just empty or delimiters).
;;; Returns T if there's text content, NIL otherwise.
(defun hcnm_ldrblk_attr_has_content_p (string / sep pos prefix auto postfix)
  (cond
    ;; Empty string has no content
    ((= string "") nil)
    ;; Check if string has delimiters
    ((vl-string-search (setq sep (chr 160)) string)
     ;; Parse into parts
     (setq pos (vl-string-search sep string))
     (setq prefix (substr string 1 pos))
     (setq string (substr string (+ pos 2)))
     (setq pos (vl-string-search sep string))
     (if pos
       (progn
         (setq auto (substr string 1 pos))
         (setq postfix (substr string (+ pos 2)))
       )
       (progn
         (setq auto string)
         (setq postfix "")
       )
     )
     ;; Has content if any part is non-empty
     (or (/= prefix "") (/= auto "") (/= postfix ""))
    )
    ;; No delimiters, has content if non-empty
    (t t)
  )
)
;;; Underline or overline string unless it's empty.
;;; With prefix/auto/postfix split using chr(160) as delimiter,
;;; the format code goes in the prefix, not the auto text.
;;; If format codes are found in the auto text, move them to prefix.
(defun hcnm_ldrblk_adjust_format (string code / sep pos prefix auto postfix auto_has_format)
  (cond
    ;; If empty, do nothing
    ((= string "") string)
    ;; Otherwise, split by chr(160) delimiter and add format code to prefix
    (t
     (setq sep (chr 160))
     (setq pos (vl-string-search sep string))
     (cond
       ;; If no delimiter, treat entire string as prefix (old-style attribute)
       ((not pos)
        (cond
          ;; If already underlined or overlined, strip that. Assumes that's the first code.
          ((wcmatch string "\\*") (strcat code (substr string 3)))
          ((wcmatch string "%%*") (strcat code (substr string 4)))
          ;; Otherwise just underline or overline.
          (t (strcat code string))
        )
       )
       ;; If delimiter exists, extract prefix/auto/postfix and format the prefix
       (t
        (setq prefix (substr string 1 pos))
        (setq string (substr string (+ pos 2)))  ; Skip separator
        (setq pos (vl-string-search sep string))
        (cond
          ;; Extract auto and postfix
          (pos
           (setq auto (substr string 1 pos))
           (setq postfix (substr string (+ pos 2)))
          )
          ;; No second delimiter, rest is auto
          (t
           (setq auto string)
           (setq postfix "")
          )
        )
        ;; Check if auto text has format codes and move them to prefix
        (setq auto_has_format nil)
        (cond
          ;; If auto has mtext format codes, strip them and flag
          ((wcmatch auto "\\L*")
           (setq auto (substr auto 3))
           (setq auto_has_format "\\L")
          )
          ((wcmatch auto "\\O*")
           (setq auto (substr auto 3))
           (setq auto_has_format "\\O")
          )
          ;; If auto has dtext format codes, strip them and flag
          ((wcmatch auto "%%u*")
           (setq auto (substr auto 4))
           (setq auto_has_format "%%u")
          )
          ((wcmatch auto "%%o*")
           (setq auto (substr auto 4))
           (setq auto_has_format "%%o")
          )
        )
        ;; Add format code to prefix (use code from auto if found, else use CODE parameter)
        (setq prefix
          (cond
            ;; If auto had format, use that instead of CODE
            (auto_has_format
             (cond
               ((= prefix "") auto_has_format)
               ((wcmatch prefix "\\*") (strcat auto_has_format (substr prefix 3)))
               ((wcmatch prefix "%%*") (strcat auto_has_format (substr prefix 4)))
               (t (strcat auto_has_format prefix))
             )
            )
            ;; Otherwise use CODE parameter
            (t
             (cond
               ((= prefix "") code)  ; Empty prefix gets just the code
               ((wcmatch prefix "\\*") (strcat code (substr prefix 3)))
               ((wcmatch prefix "%%*") (strcat code (substr prefix 4)))
               (t (strcat code prefix))
             )
            )
          )
        )
        ;; Reassemble with delimiters
        (strcat prefix sep auto sep postfix)
       )
     )
    )
  )
)
(defun hcnm_ldrblk_get_text_entry (ename_bubble line_number attribute_list /
                               input skip_entry_p input loop-p prompt-p string tag
                              )
  (setq
    loop-p t
    prompt-p
     (= (c:hcnm-config-getvar
          (strcat "BubbleTextLine" (itoa line_number) "PromptP")
        )
        "1"
     )
    skip_entry_p
     (= (c:hcnm-config-getvar "BubbleSkipEntryPrompt") "1")
    string ""
    tag
     (strcat "NOTETXT" (itoa line_number))
  )
  (while (and prompt-p loop-p)
    (cond
      ((or skip_entry_p
           (= (setq
                input
                 (getstring
                   1
                   (strcat
                     "\nLine "
                     (itoa line_number)
                     " text or . for automatic text: "
                   )
                 )
              )
              "."
           )
       )
       (setq
         attribute_list
          (hcnm_ldrblk_get_auto_type
            ename_bubble
            line_number
            tag
            attribute_list
          )
         string
          (cadr (assoc tag attribute_list))
       )
      )
      (t
       (setq
         string input
         attribute_list
          (hcnm_ldrblk_save_attribute_to_list
            tag
            string
            attribute_list
          )
       )
      )
    )
    (setq
      skip_entry_p
       (and skip_entry_p (/= string "ENtry"))
      loop-p
       (or (not string) (= string "ENtry"))
    )
  )
  attribute_list
)
;;; bubble-data-update: I believe that this needs to also define any data reference types.
(defun hcnm_ldrblk_get_auto_type_keys ()
  ;; Returns list of auto-text type definitions
  ;; Structure: (Input_Key Display_Type Reference_Type Requires_Coordinates)
  ;; - Input_Key: Keyword entered by user (initget format)
  ;; - Display_Type: Canonical type name used in code
  ;; - Reference_Type: Type of reference object ("AL"=Alignment, "SU"=Surface, nil=none)
  ;; - Requires_Coordinates: T if needs P1_WORLD from leader, nil otherwise
  '(
    ("LF" "LF" nil nil)        ; Length (QTY) - user picks objects
    ("SF" "SF" nil nil)        ; Square Feet (QTY) - user picks objects
    ("SY" "SY" nil nil)        ; Square Yards (QTY) - user picks objects
    ("STa" "Sta" "AL" t)       ; Station - needs P1_WORLD for alignment query
    ("Off" "Off" "AL" t)       ; Offset - needs P1_WORLD for alignment query
    ("stAoff" "StaOff" "AL" t) ; Station+Offset - needs P1_WORLD for alignment query
    ("Name" "AlName" "AL" nil)     ; Alignment Name - no coordinates needed
    ("STAName" "StaName" "AL" t)     ; Station + Alignment Name - needs P1_WORLD
    ("N" "N" nil t)            ; Northing - needs P1_WORLD for coordinate
    ("E" "E" nil t)            ; Easting - needs P1_WORLD for coordinate
    ("NE" "NE" nil t)          ; Northing+Easting - needs P1_WORLD for coordinate
    ("Z" "Z" "SU" t)           ; Elevation - needs P1_WORLD for surface query (unimplemented)
    ("Dia" "Dia" "PIPE" nil)   ; Pipe Diameter - user selects pipe object
    ("SLope" "Slope" "PIPE" nil) ; Pipe Slope - user selects pipe object
    ("L" "L" "PIPE" nil)       ; Pipe Length - user selects pipe object
    ("Text" "Text" nil nil)    ; Static text - user enters manually
    ("ENtry" "ENtry" nil nil)  ; Entry number - static text
  )
)
(defun hcnm_ldrblk_get_auto_type (ename_bubble line_number tag attribute_list /
                              cvport_old haws-qt-new input space string
                             )
  (initget
    (substr
      (apply
        'STRCAT
        (mapcar
          '(lambda (x) (strcat " " (car x)))
          (hcnm_ldrblk_get_auto_type_keys)
        )
      )
      2
    )
  )
  (setq
    input
     (getkword
       (strcat
         "\nLine "
         (itoa line_number)
         " automatic text. Enter an option ["
         (substr
           (apply
             'STRCAT
             (mapcar
               '(lambda (x) (strcat "/" (car x)))
               (hcnm_ldrblk_get_auto_type_keys)
             )
           )
           2
         )
         "] <"
         (car (last (hcnm_ldrblk_get_auto_type_keys)))
         ">: "
       )
     )
  )
  (cond
    ((or (not input) (= input "ENtry"))
      (setq
        attribute_list
         (hcnm_ldrblk_save_attribute_to_list
           tag
           "ENtry"
           attribute_list
         )
      )
    )
    (t
     (setq
       attribute_list
        (hcnm_ldrblk_auto_dispatch
          ename_bubble
          attribute_list
          (strcat "NOTETXT" (itoa line_number))
          (cadr
            (assoc input (hcnm_ldrblk_get_auto_type_keys))
          )
          nil
        )
     )
    )
  )
  ;; bubble-data-update: This is broken because the calling function expects "ENtry" to be returned sometimes.
  ;; I feel like I really should find a way to abstract the interface from this other business logic. There may be a good example in my recent subdivision tools.
  attribute_list
)
;; bubble-data-update: HCNM_LDRBLK_AUTO_DISPATCH is called from command line (insertion) and from edit box (editing) to get string as requested by user. It needs to get not only string, but also data (reference object and reference type).
;; 
;; This is how bubble note auto text works.
;; 
;; Bubble note creation process from inside out:
;; HCNM_LDRBLK_AUTO_DISPATCH returns ATTRIBUTE_LIST including NOTEDATA information
;; HCNM_LDRBLK_GET_AUTO_TYPE returns ATTRIBUTE_LIST
;; HCNM_LDRBLK_GET_TEXT_ENTRY returns ATTRIBUTE_LIST
;; HCNM_LDRBLK_GET_BUBBLE_DATA returns BLOCK_DATA that includes ATTRIBUTE_LIST after adjusting formatting  (overline and underline)
;; HCNM_SET_ATTRIBUTES puts ATTRIBUTE_LIST into bubble note
;; 
;; Bubble note editing process from inside out:
;; HCNM_LDRBLK_AUTO_DISPATCH returns ATTRIBUTE_LIST including NOTEDATA information
;; HCNM_EB:GET_TEXT modifies semi-global HCNM_EB:ATTRIBUTE_LIST after adjusting formatting (overline and underline)
;; HCNM_EB:SAVE calls HCNM_SET_ATTRIBUTES to save semi-global HCNM_EB:ATTRIBUTE_LIST
;; HCNM_EDIT_BUBBLE top level manages editing dialog
;; 
;; Need to add these functions:
;; 1. Get/save/return reference object and reference type
;; 2. Get/save/return/use string from reference object, reference type, and point of interest
;; 3. Update "NOTEDATA" block attribute to include reference object, attribute, and reference type
;; bubble-data-update: this 
;; We also need to update this data in the attribute list.
;; Possibly we call a function here:
;; (HCNM_LDRBLK_UPDATE_DATA KEY OBJECT)
;; Since what has to be done on update varies widely, we pass the attribute list to each sub-function and let it decided what to do with it. I think we also need to pass in an indication of whether this is a refresh or a user prompt.
;; We return the modified attribute_list.
;; INPUT IS THE OBJECT (ENAME OR VLA-OBJECT; COULD BE STANDARDIZED) OR STRING WE NEED TO EXAMINE IF WE AREN'T ASKING THE USER FOR IT OR NIL IF WE NEED TO GET IT.
;; Returns ATTRIBUTE_LIST with the requested auto data added.
(defun hcnm_ldrblk_auto_dispatch (ename_bubble attribute_list tag auto_type input / bubble_data)
    ;; bubble-data-update: Build BUBBLE_DATA and pass to subfunctions
  (setq 
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ENAME_BUBBLE" ename_bubble)
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  ;; Ensure ENAME_LEADER is in BUBBLE_DATA (needed for reactor attachment)
  (cond
    ((not (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER"))
     (setq bubble_data (hcnm_ldrblk_bd_set 
                         bubble_data 
                         "ENAME_LEADER" 
                         (hcnm_ldrblk_bubble_leader ename_bubble)))
    )
  )
  ;; Only calculate P1_WORLD for auto-types that require coordinates
  ;; BUT: For alignment auto-text, only calculate P1_WORLD when we have INPUT (reactor update)
  ;; When INPUT is NIL (user selection), AUTO_AL will handle viewport selection AFTER user picks alignment
  (cond
    ((and (hcnm_ldrblk_auto_type_is_coordinate_p auto_type)
          (or input  ; Reactor update - have alignment already
              (not (member auto_type '("Sta" "Off" "StaOff" "AlName" "StaName")))))  ; Not alignment-based
     (setq bubble_data (hcnm_ldrblk_bd_ensure_p1_world bubble_data))
    )
  )
  (setq
    bubble_data
     (cond
       ((= auto_type "Text") (hcnm_ldrblk_auto_es bubble_data tag auto_type input))
       ((= auto_type "LF") (hcnm_ldrblk_auto_qty bubble_data tag auto_type "Length" "1" input))
       ((= auto_type "SF") (hcnm_ldrblk_auto_qty bubble_data tag auto_type "Area" "1" input))
       ((= auto_type "SY")
        (hcnm_ldrblk_auto_qty bubble_data tag auto_type "Area" "0.11111111" input)
       )
       ((= auto_type "Sta") (hcnm_ldrblk_auto_al bubble_data tag auto_type input))
       ((= auto_type "Off") (hcnm_ldrblk_auto_al bubble_data tag auto_type input))
       ((= auto_type "StaOff")
        (hcnm_ldrblk_auto_al bubble_data tag auto_type input)
       )
       ((= auto_type "AlName") (hcnm_ldrblk_auto_al bubble_data tag auto_type input))
       ((= auto_type "StaName") (hcnm_ldrblk_auto_al bubble_data tag auto_type input))
       ((= auto_type "N") (hcnm_ldrblk_auto_ne bubble_data tag auto_type input))
       ((= auto_type "E") (hcnm_ldrblk_auto_ne bubble_data tag auto_type input))
       ((= auto_type "NE") (hcnm_ldrblk_auto_ne bubble_data tag auto_type input))
       ((= auto_type "Z") (hcnm_ldrblk_auto_su bubble_data tag auto_type input))
       ((= auto_type "Dia") (hcnm_ldrblk_auto_pipe bubble_data tag auto_type input))
       ((= auto_type "Slope") (hcnm_ldrblk_auto_pipe bubble_data tag auto_type input))
       ((= auto_type "L") (hcnm_ldrblk_auto_pipe bubble_data tag auto_type input))
     )
    attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
  )
  attribute_list
)

(defun hcnm_ldrblk_auto_es (bubble_data tag auto_type input / ename attribute_list) 
  (setq
    attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
    ename
    (cond
      (input)
      (t (car (nentsel (strcat "\nSelect object with " auto_type ": "))))
    )     
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (setq  
    attribute_list 
     (hcnm_ldrblk_save_auto_to_list 
       tag
       (cond 
         (ename (cdr (assoc 1 (entget ename))))
         (t "")
       )
       attribute_list
     )
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  bubble_data
)
(defun hcnm_ldrblk_auto_qty (bubble_data tag auto_type qt_type factor input / attribute_list str_backslash input1 pspace_bubble_p
                         ss-p string)
  (setq attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES"))
  (cond
    ((setq string input))
    (t  
      (cond
        ((and
          (= qt_type "Area")
          (= (c:hcnm-config-getvar "BubbleAreaIntegral") "1")
          )
        (c:hcnm-config-setvar "BubbleArrowIntegralPending" "1")
        )
      )
      (setq pspace_bubble_p (hcnm_ldrblk_space_set_model))
      (initget "Selection")
      (setq
        input1
        (nentsel
          (strcat
            "\nSelect object to link dynamically or [Selection set (not dynamic) including AECC_PIPEs] <Selection set>: "
          )
        )
        ss-p
        (or (not input1) (= input1 "Selection"))
        string
        (cond
          (ss-p
            (if (not haws-qt-new)
              (load "HAWS-QT")
            )
            (haws-qt-new "ldrblk")
            (haws-qt-set-property "ldrblk" "type" (strcase qt_type t))
            (haws-qt-set-property "ldrblk" "factor" (read factor))
            (haws-qt-set-property
              "ldrblk"
              "postfix"
              (c:hcnm-config-getvar
                (strcat "BubbleTextPostfix" auto_type)
              )
            )
            (haws-qt-string "ldrblk")
            (haws-qt-get-property "ldrblk" "string")
          )
          (t
            (strcat
              (c:hcnm-config-getvar
                (strcat "BubbleTextPrefix" auto_type)
              )
              "%<\\AcObjProp Object(%<\\_ObjId "
              (vla-getobjectidstring
                (vla-get-utility
                  (vla-get-activedocument (vlax-get-acad-object))
                )
                (vlax-ename->vla-object (car input1))
                :vlax-false
              )
              ">%)."
              qt_type
              " \\f \"%lu2%pr"
              (c:hcnm-config-getvar
                (strcat "BubbleTextPrecision" auto_type)
              )
              "%ct8["
              factor
              "]\">%"
              (c:hcnm-config-getvar
                (strcat "BubbleTextPostfix" auto_type)
              )
            )
          )
        )
      )
      (hcnm_ldrblk_space_restore pspace_bubble_p)
      
    )
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (setq
    attribute_list
    (hcnm_ldrblk_save_attribute_to_list 
      tag
      string
      attribute_list
    )
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  bubble_data
)
;; NOT USED
(defun hcnm_ldrblk_mtextatribute_p (en)
  (setq obj (vlax-ename->vla-object en))
  (and
    (vlax-property-available-p obj 'MTEXTATTRIBUTE)
    (vla-get-mtextattribute obj)
  )
)
(defun hcnm_ldrblk_space_set_model ()
  (c:hcnm-config-setvar "AllowReactors" "0")
  (cond ((= (getvar "CVPORT") 1) (vl-cmdf "._MSPACE") t))
)
(defun hcnm_ldrblk_space_restore (pspace_bubble_p)
  (cond (pspace_bubble_p (vl-cmdf "._PSPACE")))
  (c:hcnm-config-setvar "AllowReactors" "1")
)

;;==============================================================================
;; COORDINATE AUTO-TEXT VALIDATION AND WARNING SYSTEM
;;==============================================================================
;; These functions determine if an auto-text type requires coordinates and
;; warn users about paper space viewport behavior to prevent drawing issues.

;; Check if AUTO_TYPE is coordinate-based using GET_AUTO_TYPE_KEYS
;; Returns: T if auto-type requires coordinates (Sta/Off/N/E/Z), NIL otherwise
;; Usage: Determines if paper space warning should be shown
(defun hcnm_ldrblk_auto_type_is_coordinate_p (auto_type / type_def)
  (setq type_def 
    (car 
      (vl-remove-if-not 
        '(lambda (x) (= (cadr x) auto_type))
        (hcnm_ldrblk_get_auto_type_keys)
      )
    )
  )
  (cond
    (type_def (cadddr type_def))  ; Return 4th element (Requires_Coordinates)
    (t nil)
  )
)

;; Display critical warning about paper space coordinate behavior
;; Why: Prevents users from changing viewport views which breaks coordinate accuracy
;; When: Only shown for coordinate-based auto-text in paper space bubbles
;; TODO: Replace with dismissable tip system when implemented
;; Capture and store viewport transformation matrix for paper space bubble
;; This captures 3 reference points to calculate rotation, scale, and translation
;; Returns T if successful, NIL if failed
(defun hcnm_ldrblk_capture_viewport_transform (ename_bubble cvport / ref_ocs_1 ref_ocs_2 ref_ocs_3 ref_wcs_1 ref_wcs_2 ref_wcs_3)
  (cond
    ((and cvport (> cvport 1))
     ;; We're in a viewport - capture transformation matrix
     ;; Use 3 reference points: origin, X-axis unit vector, Y-axis unit vector
     (setq ref_ocs_1 '(0.0 0.0 0.0)    ; Origin
           ref_ocs_2 '(1.0 0.0 0.0)    ; X-axis unit vector
           ref_ocs_3 '(0.0 1.0 0.0)    ; Y-axis unit vector
           ref_wcs_1 (trans (trans ref_ocs_1 3 2) 2 0)
           ref_wcs_2 (trans (trans ref_ocs_2 3 2) 2 0)
           ref_wcs_3 (trans (trans ref_ocs_3 3 2) 2 0))
     (hcnm_ldrblk_set_viewport_transform_xdata ename_bubble cvport 
                                                 ref_ocs_1 ref_wcs_1
                                                 ref_ocs_2 ref_wcs_2
                                                 ref_ocs_3 ref_wcs_3)
     (princ (strcat "\nStored viewport " (itoa cvport) " transformation matrix"))
     t  ; Success
    )
    (t nil)  ; Failed - not in a viewport
  )
)

;; Queue a paper space coordinate warning tip to be shown after any modal dialogs close
;; This is necessary because you can't show a modal dialog from inside another modal dialog
;; Show paper space coordinate warning tip
;; Can be called from anywhere - shows tip immediately
(defun hcnm_ldrblk_warn_pspace_coordinates (ename_bubble auto_type)
  (cond
    ((and ename_bubble 
          (not (hcnm_ldrblk_is_on_model_tab ename_bubble))
          (hcnm_ldrblk_auto_type_is_coordinate_p auto_type))
     ;; Bubble is in paper space and auto-type is coordinate-based - show warning
     (haws_tip_show 1001 ; Unique tip ID for paper space warning
       "IMPORTANT: Paper space bubble notes don't react to viewport changes.\n\nTo avoid causing chaos when changing viewport views, auto text for coordinates\ndoes not react to viewport view changes.\n\nYou must use the ____ command if you want to refresh the world coordinates of selected bubble notes on a viewport.")
    )
  )
)

;;==============================================================================
;; ALIGNMENT AUTO-TEXT (Sta/Off/StaOff)
;;==============================================================================
;; Calculates station and offset values from alignment and leader position
;; Workflow: Get alignment → Calculate Sta/Off → Format text → Attach reactor
;;
;; REFACTORED: Split into modular functions for better maintainability
;;   - HCNM_LDRBLK_AUTO_ALIGNMENT_CALCULATE: Pure calculation (testable)
;;   - HCNM_LDRBLK_AUTO_ALIGNMENT_FORMAT_STATION: Format station string
;;   - HCNM_LDRBLK_AUTO_ALIGNMENT_FORMAT_OFFSET: Format offset string
;;   - HCNM_LDRBLK_AUTO_AL: Main orchestrator (backward compatible)

;; Calculate raw station and offset from alignment and world point
;; Pure function: No side effects, easily testable
;; Arguments:
;;   ALIGNMENT_OBJECT - VLA-OBJECT of Civil 3D alignment
;;   P1_WORLD - (X Y Z) point in world coordinates
;; Returns:
;;   (DRAWSTATION . OFFSET) on success
;;   NIL on failure
(defun hcnm_ldrblk_auto_alignment_calculate (alignment_object p1_world / drawstation offset)
  (cond
    ((and (= (type alignment_object) 'VLA-OBJECT) p1_world)
     ;; Call Civil 3D alignment method to get station/offset
     ;; http://docs.autodesk.com/CIV3D/2012/ENU/API_Reference_Guide/com/AeccXLandLib__IAeccAlignment__StationOffset
     (vlax-invoke-method 
       alignment_object
       'STATIONOFFSET
       (vlax-make-variant (car p1_world) vlax-vbdouble)
       (vlax-make-variant (cadr p1_world) vlax-vbdouble)
       'DRAWSTATION
       'OFFSET
     )
     ;; Return as dotted pair
     (cons drawstation offset)
    )
    (t nil)  ; Invalid inputs
  )
)

;; Format station value with config-based prefix/postfix
;; Arguments:
;;   ALIGNMENT_OBJECT - VLA-OBJECT to get station string with equations
;;   DRAWSTATION - Raw station value from StationOffset method
;; Returns: Formatted station string (e.g., "STA 10+50.00")
(defun hcnm_ldrblk_auto_alignment_format_station (alignment_object drawstation)
  (strcat 
    (c:hcnm-config-getvar "BubbleTextPrefixSta")
    (vlax-invoke-method 
      alignment_object
      'GETSTATIONSTRINGWITHEQUATIONS
      drawstation
    )
    (c:hcnm-config-getvar "BubbleTextPostfixSta")
  )
)

;; Format offset value with config-based prefix/postfix and sign handling
;; Arguments:
;;   OFFSET - Raw offset value (positive = right, negative = left)
;; Returns: Formatted offset string (e.g., "25.00 RT" or "LT 10.50")
(defun hcnm_ldrblk_auto_alignment_format_offset (offset / offset_value)
  ;; Determine offset value (absolute or with sign)
  (setq offset_value
    (cond 
      ((= (c:hcnm-config-getvar "BubbleOffsetDropSign") "1")
       (abs offset)  ; Drop sign, show absolute value
      )
      (t offset)  ; Keep sign
    )
  )
  ;; Build offset string with appropriate prefix/postfix
  (strcat 
    ;; Prefix depends on offset direction
    (cond 
      ((minusp offset)
       (c:hcnm-config-getvar "BubbleTextPrefixOff-")
      )
      (t (c:hcnm-config-getvar "BubbleTextPrefixOff+"))
    )
    ;; Format number with configured precision
    (rtos 
      offset_value
      2
      (atoi (c:hcnm-config-getvar "BubbleTextPrecisionOff+"))
    )
    ;; Postfix depends on offset direction
    (cond 
      ((minusp offset)
       (c:hcnm-config-getvar "BubbleTextPostfixOff-")
      )
      (t (c:hcnm-config-getvar "BubbleTextPostfixOff+"))
    )
  )
)

;; ============================================================================
;; Civil 3D Pipe Network Auto-Text Functions
;; ============================================================================

;;==============================================================================
;; HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT
;;==============================================================================
;; Purpose:
;;   Prompts user to select a Civil 3D pipe network pipe object.
;;
;; Arguments:
;;   ENAME_BUBBLE - Entity name of bubble being edited (required)
;;   TAG - Attribute tag being updated (required)
;;   AUTO_TYPE - Property type: "Dia", "Slope", or "L" (required)
;;
;; Returns:
;;   VLA-OBJECT of selected pipe, or NIL if selection failed
;;
;; Side Effects:
;;   - Prompts user to select pipe with custom message
;;
;; Related:
;;   HCNM_LDRBLK_AUTO_PIPE
;;
;; Example:
;;   (SETQ OBJ_PIPE (HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT ENAME_BUBBLE "NOTETXT1" "Dia"))
;;==============================================================================
(defun hcnm_ldrblk_auto_pipe_get_object (ename_bubble tag auto_type / esapipe obj_pipe)
  (setq esapipe
    (nentsel 
      (strcat 
        "\nSelect Civil 3D pipe for "
        (cond
          ((= auto_type "Dia") "diameter")
          ((= auto_type "Slope") "slope")
          ((= auto_type "L") "length")
          (t "property")
        )
        ": "
      )
    )
  )
  (cond
    (esapipe
     (setq obj_pipe
       (vl-catch-all-apply 
         'VLAX-ENAME->vla-object
         (list (car esapipe))
       )
     )
     (cond
       ((vl-catch-all-error-p obj_pipe)
        (princ "\nError: Could not get pipe object.")
        nil
       )
       (t obj_pipe)
     )
    )
    (t nil)
  )
)

;;==============================================================================
;; HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
;;==============================================================================
;; Purpose:
;;   Formats pipe diameter value with user-configured prefix, postfix, and precision.
;;   Converts from Civil 3D units (feet) to inches for display.
;;
;; Arguments:
;;   OBJ_PIPE - VLA-OBJECT of Civil 3D pipe (required)
;;
;; Returns:
;;   Formatted string (e.g., "12 IN", "18\"") or error string if property unavailable
;;
;; Side Effects:
;;   - Reads config variables: BubbleTextPrefixPipeDia, PostfixPipeDia, PrecisionPipeDia
;;
;; Related:
;;   HCNM_LDRBLK_AUTO_PIPE
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_SLOPE
;;
;; Example:
;;   (SETQ TEXT (HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER OBJ_PIPE))
;;==============================================================================
(defun hcnm_ldrblk_auto_pipe_format_diameter (obj_pipe / dia_value dia_inches)
  (setq 
    dia_value (vl-catch-all-apply 'VLAX-GET-PROPERTY (list obj_pipe 'InnerDiameterOrWidth))
  )
  (cond
    ((vl-catch-all-error-p dia_value)
     (princ (strcat "\nError getting diameter: " (vl-princ-to-string dia_value)))
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
    )
    (t
     ;; Civil 3D returns diameter in drawing units (typically feet for US)
     ;; Convert to inches for display
     (setq dia_inches (* dia_value 12.0))
     (strcat 
       (c:hcnm-config-getvar "BubbleTextPrefixPipeDia")
       (rtos 
         dia_inches
         2
         (atoi (c:hcnm-config-getvar "BubbleTextPrecisionPipeDia"))
       )
       (c:hcnm-config-getvar "BubbleTextPostfixPipeDia")
     )
    )
  )
)

;;==============================================================================
;; HCNM_LDRBLK_AUTO_PIPE_FORMAT_SLOPE
;;==============================================================================
;; Purpose:
;;   Formats pipe slope value with user-configured prefix, postfix, and precision.
;;   Converts from Civil 3D units (decimal) to percentage for display.
;;
;; Arguments:
;;   OBJ_PIPE - VLA-OBJECT of Civil 3D pipe (required)
;;
;; Returns:
;;   Formatted string (e.g., "2.00%", "0.5%") or error string if property unavailable
;;
;; Side Effects:
;;   - Reads config variables: BubbleTextPrefixPipeSlope, PostfixPipeSlope, PrecisionPipeSlope
;;
;; Related:
;;   HCNM_LDRBLK_AUTO_PIPE
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
;;
;; Example:
;;   (SETQ TEXT (HCNM_LDRBLK_AUTO_PIPE_FORMAT_SLOPE OBJ_PIPE))
;;==============================================================================
(defun hcnm_ldrblk_auto_pipe_format_slope (obj_pipe / slope_value slope_percent)
  (setq 
    slope_value (vl-catch-all-apply 'VLAX-GET-PROPERTY (list obj_pipe 'Slope))
  )
  (cond
    ((vl-catch-all-error-p slope_value)
     (princ (strcat "\nError getting slope: " (vl-princ-to-string slope_value)))
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
    )
    (t
     ;; Civil 3D returns slope as decimal (e.g., 0.02 for 2%)
     ;; Convert to percentage for display (take absolute value)
     (setq slope_percent (* (abs slope_value) 100.0))
     (strcat 
       (c:hcnm-config-getvar "BubbleTextPrefixPipeSlope")
       (rtos 
         slope_percent
         2
         (atoi (c:hcnm-config-getvar "BubbleTextPrecisionPipeSlope"))
       )
       (c:hcnm-config-getvar "BubbleTextPostfixPipeSlope")
     )
    )
  )
)

;;==============================================================================
;; HCNM_LDRBLK_AUTO_PIPE_FORMAT_LENGTH
;;==============================================================================
;; Purpose:
;;   Formats pipe length value with user-configured prefix, postfix, and precision.
;;   Uses 2D length from Civil 3D (horizontal projection).
;;
;; Arguments:
;;   OBJ_PIPE - VLA-OBJECT of Civil 3D pipe (required)
;;
;; Returns:
;;   Formatted string (e.g., "L=125.50", "125.5'") or error string if property unavailable
;;
;; Side Effects:
;;   - Reads config variables: BubbleTextPrefixPipeLength, PostfixPipeLength, PrecisionPipeLength
;;
;; Related:
;;   HCNM_LDRBLK_AUTO_PIPE
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
;;
;; Example:
;;   (SETQ TEXT (HCNM_LDRBLK_AUTO_PIPE_FORMAT_LENGTH OBJ_PIPE))
;;==============================================================================
(defun hcnm_ldrblk_auto_pipe_format_length (obj_pipe / length_value)
  (setq 
    length_value (vl-catch-all-apply 'VLAX-GET-PROPERTY (list obj_pipe 'Length2D))
  )
  (cond
    ((vl-catch-all-error-p length_value)
     (princ (strcat "\nError getting length: " (vl-princ-to-string length_value)))
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
    )
    (t
     (strcat 
       (c:hcnm-config-getvar "BubbleTextPrefixPipeLength")
       (rtos 
         length_value
         2
         (atoi (c:hcnm-config-getvar "BubbleTextPrecisionPipeLength"))
       )
       (c:hcnm-config-getvar "BubbleTextPostfixPipeLength")
     )
    )
  )
)

;;==============================================================================
;; HCNM_LDRBLK_AUTO_PIPE
;;==============================================================================
;; Purpose:
;;   Main pipe network auto-text orchestrator. Gets pipe object, extracts
;;   specified property (diameter/slope/length), formats with config settings,
;;   and attaches reactor for automatic updates when pipe changes.
;;
;; Arguments:
;;   BUBBLE_DATA - Bubble data alist (required)
;;   TAG - Attribute tag to update (required)
;;   AUTO_TYPE - Property type: "Dia", "Slope", or "L" (required)
;;   INPUT - Pre-selected pipe VLA-OBJECT (optional, for reactor callbacks)
;;
;; Returns:
;;   Updated BUBBLE_DATA with new attribute value
;;
;; Side Effects:
;;   - Prompts user for pipe selection if INPUT is NIL
;;   - Attaches VLR-OBJECT-REACTOR to pipe for automatic updates
;;   - Switches to model space temporarily if bubble is in paper space
;;   - Updates ATTRIBUTE_LIST within BUBBLE_DATA
;;
;; Related:
;;   HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_SLOPE
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_LENGTH
;;   HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR
;;
;; Example:
;;   (SETQ BUBBLE_DATA
;;     (HCNM_LDRBLK_AUTO_PIPE BUBBLE_DATA "NOTETXT1" "Dia" NIL)
;;   )
;;==============================================================================
(defun hcnm_ldrblk_auto_pipe (bubble_data tag auto_type input / attribute_list ename_bubble ename_leader obj_pipe pspace_bubble_p string)
  (setq 
    attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
    ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
    ename_leader (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER")
  )
  
  ;; STEP 1: Get pipe object (user selection or provided input)
  (cond 
    ((setq obj_pipe input)
     ;; Pipe provided (reactor update or programmatic call)
    )
    (t
     ;; Get pipe from user
     (setq pspace_bubble_p (hcnm_ldrblk_space_set_model)
           obj_pipe         (hcnm_ldrblk_auto_pipe_get_object ename_bubble tag auto_type)
     )
     ;; Attach reactor to watch for pipe changes (no leader needed since not coordinate-based)
     (cond
       (obj_pipe
        (hcnm_ldrblk_assure_auto_text_has_reactor obj_pipe ename_bubble nil tag auto_type)
       )
     )
     ;; Restore space after selection
     (hcnm_ldrblk_space_restore pspace_bubble_p)
    )
  )
  
  ;; STEP 2: Extract and format the property based on AUTO_TYPE
  (setq string
    (cond 
      ((not obj_pipe)
       "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
      )
      ((= auto_type "Dia")
       (hcnm_ldrblk_auto_pipe_format_diameter obj_pipe)
      )
      ((= auto_type "Slope")
       (hcnm_ldrblk_auto_pipe_format_slope obj_pipe)
      )
      ((= auto_type "L")
       (hcnm_ldrblk_auto_pipe_format_length obj_pipe)
      )
      (t "!!!!!!!!!!!!!!!!!INVALID TYPE!!!!!!!!!!!!!!!!!!!!!!!")
    )
  )
  
  ;; STEP 3: Save the formatted string to the attribute list and update BUBBLE_DATA
  (setq
    attribute_list
    (hcnm_ldrblk_save_auto_to_list 
      tag
      string
      attribute_list
    )
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  bubble_data
)

;; Main alignment auto-text function (backward compatible)
;; Orchestrates: get alignment → calculate → format → attach reactor
;; Arguments:
;;   BUBBLE_DATA - Bubble data alist
;;   TAG - Attribute tag to update
;;   AUTO_TYPE - "Sta", "Off", "StaOff", "AlName", or "StaName"
;;   INPUT - Optional: Pre-selected alignment object (used by reactor updates)
;; Returns: Updated BUBBLE_DATA with new attribute value
(defun hcnm_ldrblk_auto_al (bubble_data tag auto_type input / alignment_name attribute_list ename_bubble ename_leader sta_off_pair drawstation offset objalign p1_world pspace_bubble_p sta_string off_string string cvport ref_ocs_1 ref_ocs_2 ref_ocs_3 ref_wcs_1 ref_wcs_2 ref_wcs_3)
  (setq 
    attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
    ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
    ename_leader (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER")
    p1_world (hcnm_ldrblk_bd_get bubble_data "P1_WORLD")
  )
  
  ;; STEP 1: Get alignment object (user selection or provided input)
  (cond 
    ((setq objalign input)
     ;; Alignment provided (reactor update or programmatic call)
    )
    (t
     ;; Get alignment from user
     (setq pspace_bubble_p (hcnm_ldrblk_space_set_model)
           objalign        (hcnm_ldrblk_auto_al_get_alignment ename_bubble tag auto_type)
     )
     ;; NOTE: Still in MSPACE at this point - capture viewport transformation if needed
     ;; Capture viewport transformation data if bubble is in paper space
     (cond
       ((and objalign ename_bubble (not (hcnm_ldrblk_is_on_model_tab ename_bubble)))
        ;; Bubble is in paper space - capture transformation matrix
        (setq cvport (getvar "CVPORT"))
        (cond
          ((and cvport (> cvport 1))
           ;; We're in a viewport - capture transformation matrix
           ;; Use 3 reference points to capture rotation, scale, and translation
           (setq ref_ocs_1 '(0.0 0.0 0.0)    ; Origin
                 ref_ocs_2 '(1.0 0.0 0.0)    ; X-axis unit vector
                 ref_ocs_3 '(0.0 1.0 0.0)    ; Y-axis unit vector
                 ref_wcs_1 (trans (trans ref_ocs_1 3 2) 2 0)
                 ref_wcs_2 (trans (trans ref_ocs_2 3 2) 2 0)
                 ref_wcs_3 (trans (trans ref_ocs_3 3 2) 2 0))
           (hcnm_ldrblk_set_viewport_transform_xdata ename_bubble cvport 
                                                       ref_ocs_1 ref_wcs_1
                                                       ref_ocs_2 ref_wcs_2
                                                       ref_ocs_3 ref_wcs_3)
           (princ (strcat "\nStored viewport " (itoa cvport) " transformation matrix"))
          )
        )
       )
     )
     ;; Now calculate P1_WORLD if needed for coordinate-based types and not already in BUBBLE_DATA
     (cond
       ((and (not p1_world)
             (or (= auto_type "Sta") (= auto_type "Off") (= auto_type "StaOff") (= auto_type "StaName")))
        (setq bubble_data (hcnm_ldrblk_bd_ensure_p1_world bubble_data)
              p1_world (hcnm_ldrblk_bd_get bubble_data "P1_WORLD"))
       )
     )
     ;; Attach reactor to watch for alignment/leader changes
     (hcnm_ldrblk_assure_auto_text_has_reactor objalign ename_bubble ename_leader tag auto_type)
     ;; Now restore space after everything is done
     (hcnm_ldrblk_space_restore pspace_bubble_p)
    )
  )
  
  ;; STEP 2: Calculate station and offset (only needed for coordinate-based types)
  (cond
    ((or (= auto_type "Sta") (= auto_type "Off") (= auto_type "StaOff") (= auto_type "StaName"))
     (setq sta_off_pair (hcnm_ldrblk_auto_alignment_calculate objalign p1_world))
    )
  )
  
  ;; STEP 3: Format the result based on AUTO_TYPE
  (cond 
    ((= auto_type "AlName")
     ;; Alignment name only - no coordinates needed
     (cond
       (objalign
        (setq string (vl-catch-all-apply 'VLAX-GET-PROPERTY (list objalign 'NAME)))
        (cond
          ((vl-catch-all-error-p string)
           (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
          )
        )
       )
       (t
        (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
       )
     )
    )
    (sta_off_pair
     ;; Calculation succeeded - extract and format
     (setq drawstation (car sta_off_pair)
           offset      (cdr sta_off_pair)
           sta_string  (hcnm_ldrblk_auto_alignment_format_station objalign drawstation)
           off_string  (hcnm_ldrblk_auto_alignment_format_offset offset)
           string
           (cond 
             ((= auto_type "Sta") sta_string)
             ((= auto_type "Off") off_string)
             ((= auto_type "StaOff")
              (strcat 
                sta_string
                (c:hcnm-config-getvar "BubbleTextJoinDelSta")
                off_string
              )
             )
             ((= auto_type "StaName")
              ;; Station + alignment name
              (setq string (vl-catch-all-apply 'VLAX-GET-PROPERTY (list objalign 'NAME)))
              (cond
                ((vl-catch-all-error-p string)
                 (setq string sta_string)  ; If name fails, just use station
                )
                (t
                 (setq string (strcat sta_string " " string))
                )
              )
              string
             )
           )  ; End inner COND for STRING
     )  ; End SETQ
    )  ; End first branch of outer COND
    (t 
     ;; Calculation failed - couldn't get coordinates or invalid alignment
     (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
    )
  )  ; End outer COND
  
  ;; Step 4: Save the formatted string to the attribute list and update BUBBLE_DATA
  (setq
    attribute_list
    (hcnm_ldrblk_save_attribute_to_list 
      tag
      string
      attribute_list
    )
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  bubble_data
)
(defun hcnm_ldrblk_auto_al_get_alignment
   (ename_bubble tag auto_type / avport cvport esalign name objalign objalign_old 
    ref_ocs_1 ref_ocs_2 ref_ocs_3 ref_wcs_1 ref_wcs_2 ref_wcs_3 )
  (setq
    objalign_old
     (c:hcnm-config-getvar "BubbleCurrentAlignment")
    name
     (cond
       ((and (= (type objalign_old) 'VLA-OBJECT) 
             (not (vl-catch-all-error-p (vl-catch-all-apply 'VLAX-GET-PROPERTY (list objalign_old 'NAME)))))
        (vl-catch-all-apply 'VLAX-GET-PROPERTY (list objalign_old 'NAME))
       )
       (t (setq objalign_old nil) "")
     )
    esalign
     (nentsel
       (strcat
         "\nSelect alignment"
         (cond
           ((= name "") ": ")
           (t (strcat " or <" name ">: "))
         )
       )
     )
  )
  (cond
    ((and
       esalign
       (= (cdr (assoc 0 (entget (car esalign)))) "AECC_ALIGNMENT")
     )
     (setq objalign (vlax-ename->vla-object (car esalign)))
     (c:hcnm-config-setvar "BubbleCurrentAlignment" objalign)
     ;; Show paper space warning if bubble is in paper space and auto-type requires coordinates
     (cond
       ((and ename_bubble (not (hcnm_ldrblk_is_on_model_tab ename_bubble))
             (hcnm_ldrblk_auto_type_is_coordinate_p auto_type))
        (hcnm_ldrblk_warn_pspace_coordinates ename_bubble auto_type)
       )
     )
    )
    (esalign 
      (alert (princ "\nSelected object is not an alignment. Keeping previous alignment."))
      (setq objalign objalign_old)
    )
    (t 
      (princ "\nNo object selected. Keeping previous alignment.")
      (setq objalign objalign_old)
      ;; If using previous alignment and bubble is in paper space, check if we need viewport selection
      (cond
        ((and objalign ename_bubble (not (hcnm_ldrblk_is_on_model_tab ename_bubble)))
         ;; Bubble is in paper space with previous alignment
         ;; Only prompt for viewport if transformation data doesn't already exist
         (cond
           ((not (hcnm_ldrblk_get_viewport_transform_xdata ename_bubble))
            ;; No XDATA exists - show warning and prompt for viewport
            (hcnm_ldrblk_warn_pspace_coordinates ename_bubble auto_type)
            (setq avport (hcnm_ldrblk_get_target_vport))
            ;; Capture viewport transformation data now while in MSPACE with selected viewport
            (setq cvport avport)
            (cond
              ((and cvport (> cvport 1))
               ;; We're in a viewport - capture transformation matrix
               ;; Use 3 reference points to capture rotation, scale, and translation
               (setq ref_ocs_1 '(0.0 0.0 0.0)    ; Origin
                     ref_ocs_2 '(1.0 0.0 0.0)    ; X-axis unit vector
                     ref_ocs_3 '(0.0 1.0 0.0)    ; Y-axis unit vector
                     ref_wcs_1 (trans (trans ref_ocs_1 3 2) 2 0)
                     ref_wcs_2 (trans (trans ref_ocs_2 3 2) 2 0)
                     ref_wcs_3 (trans (trans ref_ocs_3 3 2) 2 0))
               (hcnm_ldrblk_set_viewport_transform_xdata ename_bubble cvport 
                                                           ref_ocs_1 ref_wcs_1
                                                           ref_ocs_2 ref_wcs_2
                                                           ref_ocs_3 ref_wcs_3)
               (princ (strcat "\nStored viewport " (itoa cvport) " transformation matrix"))
              )
            )
           )
           (t
            ;; XDATA already exists - no need to prompt, will use stored transformation
            (princ "\nUsing existing viewport transformation from XDATA")
           )
         )
        )
      )
    )
  )
  objalign  ; Return the alignment object
)
(defun hcnm_ldrblk_auto_ne (bubble_data tag auto_type input / attribute_list e ename_bubble ename_leader n ne p1_ocs p1_world reactor_update_p string)
  (setq 
    attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
    ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
    ename_leader (hcnm_ldrblk_bd_get bubble_data "ENAME_LEADER")
    ;; INPUT = NIL means initial creation
    ;; INPUT = T means reactor update for coordinate types (sentinel value)
    ;; INPUT = VLA-OBJECT means reactor update with reference object (not used for N/E/NE)
    reactor_update_p (and input (or (= input t) (= (type input) 'VLA-OBJECT)))
  )
  ;; Show paper space warning only during initial insertion, not reactor updates
  (cond
    ((not reactor_update_p)
     (hcnm_ldrblk_warn_pspace_coordinates ename_bubble auto_type)
    )
  )
  ;; Calculate or get P1_WORLD
  (cond
    (reactor_update_p
     ;; Reactor update - recalculate P1_WORLD from current leader position using stored transformation
     (setq p1_ocs (hcnm_ldrblk_p1_ocs ename_leader))
     (setq p1_world (hcnm_ldrblk_p1_world ename_leader p1_ocs ename_bubble))
    )
    (t
     ;; Initial creation - get from BUBBLE_DATA (already calculated by BD_ENSURE_P1_WORLD which prompts for viewport)
     (setq p1_world (hcnm_ldrblk_bd_get bubble_data "P1_WORLD"))
    )
  )
  ;; Calculate coordinates from P1_WORLD
  (cond
    (p1_world
      (setq
        n  (hcnm_ldrblk_auto_rtos (cadr p1_world) "N")
        e  (hcnm_ldrblk_auto_rtos (car p1_world) "E")
        ne (strcat
            n
            (c:hcnm-config-getvar (strcat "BubbleTextJoinDel" "N"))
            e
          )
      )
      (setq string
        (cond
          ((= auto_type "N") n)
          ((= auto_type "E") e)
          ((= auto_type "NE") ne)
        )
      )
    )
    (t
      ;; P1_WORLD is NIL - couldn't get world coordinates
      (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
    )
  )
  ;; Attach reactor to leader for coordinate updates (only on initial creation, not reactor updates)
  (cond
    ((not reactor_update_p)
     (hcnm_ldrblk_assure_auto_text_has_reactor nil ename_bubble ename_leader tag auto_type)
    )
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (setq
    attribute_list
    (hcnm_ldrblk_save_attribute_to_list 
      tag
      string
      attribute_list
    )
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  bubble_data
)
(defun hcnm_ldrblk_auto_rtos (number key)
  (strcat
    (c:hcnm-config-getvar (strcat "BubbleTextPrefix" key))
    (rtos
      number
      2
      (atoi(c:hcnm-config-getvar (strcat "BubbleTextPrecision" key)))
    )
    (c:hcnm-config-getvar (strcat "BubbleTextPostfix" key))
  )
)
;; Civil 3D Surface query auto-text (Z elevation)
;; Currently unimplemented - returns apology message
(defun hcnm_ldrblk_auto_su (bubble_data tag auto_type input / attribute_list ename_bubble)
  (setq 
    attribute_list (hcnm_ldrblk_bd_get bubble_data "ATTRIBUTES")
    ename_bubble (hcnm_ldrblk_bd_get bubble_data "ENAME_BUBBLE")
  )
  ;; Show paper space warning if applicable
  (hcnm_ldrblk_warn_pspace_coordinates ename_bubble auto_type)
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (setq
    attribute_list
    (hcnm_ldrblk_save_attribute_to_list 
      tag
      (hcnm_ldrblk_auto_apology auto_type)
      attribute_list
    )
    bubble_data (hcnm_ldrblk_bd_set bubble_data "ATTRIBUTES" attribute_list)
  )
  bubble_data
)
;; Gets the target viewport from user. This would only be called because we needed it before we could determine it automatically or when user clicks the button to change association.
;; NOTE: Warning should be shown BEFORE calling this function (via HCNM_LDRBLK_WARN_PSPACE_COORDINATES)
;; NOTE: This function does NOT restore space - caller must handle that after capturing transformation matrix
(defun hcnm_ldrblk_get_target_vport ( / input)
  ;; Ensure user is in model space so they can activate a viewport (caller should have already done this)
  (cond
    ((= (getvar "CVPORT") 1)
     (princ "\nWARNING: Not in model space - switching to MSPACE first")
     (vl-cmdf "._MSPACE")
    )
  )
  (while 
    (not 
      (progn 
        (princ "\nSet the TARGET viewport active and press ENTER to continue: ")
        (equal (setq input (grread nil 10)) '(2 13)) ; WAIT FOR ENTER (ASCII 13)
      )
    )
  )
  (setq input (getvar "CVPORT")) ; Capture the viewport ID
  ;; DO NOT restore space here - caller needs to capture transformation matrix first!
  input ; Return the viewport ID
)
;; Apply affine transformation using 3-point correspondence
;; Given 3 OCS points and their corresponding 3 WCS points, transform any OCS point to WCS
;; Uses barycentric coordinates to interpolate the transformation
(defun hcnm_ldrblk_apply_transform_matrix (p_ocs 
                                            ocs1 wcs1 
                                            ocs2 wcs2 
                                            ocs3 wcs3
                                            / dx dy d11 d12 d21 d22 det u v w px py)
  ;; Calculate barycentric coordinates of P_OCS relative to the OCS triangle
  ;; First, express P_OCS in terms of the basis vectors (OCS2-OCS1) and (OCS3-OCS1)
  (setq dx  (- (car p_ocs) (car ocs1))
        dy  (- (cadr p_ocs) (cadr ocs1))
        d11 (- (car ocs2) (car ocs1))
        d12 (- (cadr ocs2) (cadr ocs1))
        d21 (- (car ocs3) (car ocs1))
        d22 (- (cadr ocs3) (cadr ocs1))
        det (- (* d11 d22) (* d12 d21)))
  
  (cond
    ((not (equal det 0.0 1e-10))
      ;; Calculate barycentric coordinates
      (setq u (/ (- (* dx d22) (* dy d21)) det)
            v (/ (- (* d11 dy) (* d12 dx)) det)
            w (- 1.0 u v))
      ;; Apply same barycentric coordinates to WCS points
      (setq px (+ (* w (car wcs1)) (* u (car wcs2)) (* v (car wcs3)))
            py (+ (* w (cadr wcs1)) (* u (cadr wcs2)) (* v (cadr wcs3))))
      (list px py 0.0)
    )
    (t
      ;; Degenerate case - matrix not invertible, fall back to simple translation
      (princ "\nWarning: Degenerate transformation matrix, using translation only")
      (list (+ (car p_ocs) (- (car wcs1) (car ocs1)))
            (+ (cadr p_ocs) (- (cadr wcs1) (cadr ocs1)))
            0.0)
    )
  )
)

;; Get viewport transformation matrix from bubble's XDATA
;; Returns list: (CVPORT REF_OCS_1 REF_WCS_1 REF_OCS_2 REF_WCS_2 REF_OCS_3 REF_WCS_3) or NIL
(defun hcnm_ldrblk_get_viewport_transform_xdata (ename_bubble / xdata appname cvport points)
  (setq appname "HCNM-BUBBLE")
  (cond
    ((setq xdata (assoc -3 (entget ename_bubble '("HCNM-BUBBLE"))))
      (setq xdata (cdr (assoc appname (cdr xdata))))
      (princ (strcat "\nDebug XDATA found: " (vl-princ-to-string xdata)))
      ;; Check if this is new format (VPTRANS) or old format (AVPORT)
      (cond
        ((= (cdr (assoc 1000 xdata)) "VPTRANS")
          ;; New format - extract transformation matrix
          (setq points '())
          ;; Collect all 1010 (3D point) entries
          (foreach pair xdata
            (cond
              ((= (car pair) 1070) (setq cvport (cdr pair)))
              ((= (car pair) 1010) (setq points (append points (list (cdr pair)))))
            )
          )
          (cond
            ((and cvport (= (length points) 6))
              ;; We have CVPORT and 6 points (3 OCS + 3 WCS)
              (princ (strcat "\nDebug transform matrix: CVPORT=" (itoa cvport) 
                            " " (itoa (length points)) " points"))
              (cons cvport points)  ; Return (CVPORT pt1 pt2 pt3 pt4 pt5 pt6)
            )
            (t
              (princ "\nDebug: Incomplete VPTRANS data")
              nil
            )
          )
        )
        (t
          ;; Old format - just AVPORT integer, no transformation data
          (princ "\nDebug: Old AVPORT format (no transform data)")
          nil
        )
      )
    )
    (t
      (princ "\nDebug: No XDATA found on bubble")
      nil
    )
  )
)

;; DEPRECATED: Old function - use HCNM_LDRBLK_GET_VIEWPORT_TRANSFORM_XDATA instead
(defun hcnm_ldrblk_get_avport_xdata (ename_bubble / xdata appname result)
  (setq appname "HCNM-BUBBLE")
  (setq result
    (cond
      ((setq xdata (assoc -3 (entget ename_bubble '("HCNM-BUBBLE"))))
        (setq xdata (cdr (assoc appname (cdr xdata))))
        (cdr (assoc 1070 xdata)) ; Return integer value
      )
    )
  )
  result
)
;; Set viewport transformation matrix in bubble's XDATA
;; Stores CVPORT and 3 pairs of reference points (OCS and WCS)
;; These 3 points define the full transformation including rotation and scale
(defun hcnm_ldrblk_set_viewport_transform_xdata (ename_bubble cvport 
                                                   ref_ocs_1 ref_wcs_1
                                                   ref_ocs_2 ref_wcs_2
                                                   ref_ocs_3 ref_wcs_3
                                                   / appname xdata_new)
  (setq appname "HCNM-BUBBLE")
  ;; Register application if not already registered
  (cond
    ((not (tblsearch "APPID" appname))
      (regapp appname)
    )
  )
  ;; Create XDATA structure with transformation matrix data
  ;; Format: (1000 "VPTRANS") (1070 CVPORT) 
  ;;         (1010 REF_OCS_1) (1010 REF_WCS_1)
  ;;         (1010 REF_OCS_2) (1010 REF_WCS_2)
  ;;         (1010 REF_OCS_3) (1010 REF_WCS_3)
  (setq xdata_new (list (cons -3 
                          (list (cons appname 
                                  (list (cons 1000 "VPTRANS")
                                        (cons 1070 cvport)
                                        (cons 1010 ref_ocs_1)
                                        (cons 1010 ref_wcs_1)
                                        (cons 1010 ref_ocs_2)
                                        (cons 1010 ref_wcs_2)
                                        (cons 1010 ref_ocs_3)
                                        (cons 1010 ref_wcs_3)))))))
  ;; Apply XDATA to entity
  (entmod (append (entget ename_bubble) xdata_new))
)

;; Clear viewport transformation XDATA from bubble
;; Used when user wants to change viewport association via "Chg View" button
(defun hcnm_ldrblk_clear_viewport_transform_xdata (ename_bubble / appname elist elist_no_xdata)
  (setq appname "HCNM-BUBBLE")
  ;; Get entity list without XDATA
  (setq elist (entget ename_bubble)
        elist_no_xdata (vl-remove-if '(lambda (x) (= (car x) -3)) elist))
  ;; Update entity without XDATA
  (entmod elist_no_xdata)
)

;; DEPRECATED: Old function that only stored AVPORT integer
;; Kept for reference - now replaced by HCNM_LDRBLK_SET_VIEWPORT_TRANSFORM_XDATA
(defun hcnm_ldrblk_set_avport_xdata (ename_bubble avport / appname xdata_new)
  (setq appname "HCNM-BUBBLE")
  (cond
    ((not (tblsearch "APPID" appname))
      (regapp appname)
    )
  )
  (setq xdata_new (list (cons -3 
                          (list (cons appname 
                                  (list (cons 1000 "AVPORT")
                                        (cons 1070 avport)))))))
  (entmod (append (entget ename_bubble) xdata_new))
)
;; RETURNS P1_WORLD GIVEN P1_OCS
;; Uses viewport transformation data from bubble's XDATA if available
;; This allows coordinate transformation without switching viewports
;; If bubble is on Model tab, no viewport processing needed
(defun hcnm_ldrblk_p1_world (ename_leader p1_ocs ename_bubble / elist_leader layout_name
                             pspace_current_p on_model_tab_p transform_data
                             cvport_stored ref_ocs_1 ref_wcs_1 ref_ocs_2 ref_wcs_2 ref_ocs_3 ref_wcs_3
                            ) 
  (setq elist_leader   (entget ename_leader)
        layout_name    (cdr (assoc 410 elist_leader))
        on_model_tab_p (or 
                         (= layout_name "Model")
                         (= layout_name "MODEL")
                         (not layout_name)  ;; Older drawings without layout
                       )
  )
  (cond 
    ((not on_model_tab_p)
     ;; Bubble is on a layout tab - need viewport processing
     ;; Try to get viewport transformation data from XDATA
     (setq transform_data (cond 
                            (ename_bubble (hcnm_ldrblk_get_viewport_transform_xdata ename_bubble))
                            (t nil)))
     (cond
       (transform_data
         ;; We have transformation matrix - use it to transform without switching viewports
         (setq cvport_stored (car transform_data)
               ref_ocs_1 (nth 1 transform_data)
               ref_wcs_1 (nth 2 transform_data)
               ref_ocs_2 (nth 3 transform_data)
               ref_wcs_2 (nth 4 transform_data)
               ref_ocs_3 (nth 5 transform_data)
               ref_wcs_3 (nth 6 transform_data))
         (princ (strcat "\nUsing stored viewport " (itoa cvport_stored) " transformation matrix"))
         ;; Apply affine transformation using the 3-point matrix
         ;; Calculate the transformation: P1_WORLD = f(P1_OCS)
         (setq p1_world (hcnm_ldrblk_apply_transform_matrix 
                          p1_ocs
                          ref_ocs_1 ref_wcs_1
                          ref_ocs_2 ref_wcs_2
                          ref_ocs_3 ref_wcs_3))
         (princ (strcat "\nTransformed P1_WORLD: " (vl-princ-to-string p1_world)))
       )
       (t
         ;; No transformation data stored - user must select viewport
         (princ "\nDebug: No transformation data - prompting for viewport selection")
         (setq pspace_current_p (hcnm_ldrblk_space_set_model))
         ;; Prompt user to select target viewport
         (setq cvport_stored (hcnm_ldrblk_get_target_vport))
         (cond
           ((and cvport_stored (> cvport_stored 1) ename_bubble)
            ;; User selected a viewport - capture transformation matrix and store in XDATA
            (setq ref_ocs_1 '(0.0 0.0 0.0)    ; Origin
                  ref_ocs_2 '(1.0 0.0 0.0)    ; X-axis unit vector
                  ref_ocs_3 '(0.0 1.0 0.0)    ; Y-axis unit vector
                  ref_wcs_1 (trans (trans ref_ocs_1 3 2) 2 0)
                  ref_wcs_2 (trans (trans ref_ocs_2 3 2) 2 0)
                  ref_wcs_3 (trans (trans ref_ocs_3 3 2) 2 0))
            ;; Calculate P1_WORLD using TRANS
            (setq p1_world (trans (trans p1_ocs 3 2) 2 0))
            ;; Store transformation matrix in XDATA for future use
            (hcnm_ldrblk_set_viewport_transform_xdata ename_bubble cvport_stored 
                                                        ref_ocs_1 ref_wcs_1
                                                        ref_ocs_2 ref_wcs_2
                                                        ref_ocs_3 ref_wcs_3)
            (princ (strcat "\nCaptured and stored viewport " (itoa cvport_stored) " transformation matrix"))
           )
           (t
            ;; Not in a viewport or no bubble entity - just calculate P1_WORLD
            (setq p1_world (trans (trans p1_ocs 3 2) 2 0))
           )
         )
         (hcnm_ldrblk_space_restore pspace_current_p)
         (princ (strcat "\nDebug P1_WORLD: " (vl-princ-to-string p1_world)))
       )
     )
     p1_world
    )
    (t
     ;; Bubble is on Model tab - simple transformation, no viewport processing
     (trans p1_ocs 1 0)
    )
  )
)
(defun hcnm_ldrblk_auto_apology (auto_type)
  (alert (princ (strcat "Sorry. Selection of " auto_type " is not fully programmed yet and is not anticipated to be dynamic once programmed.\n\nPlease let Tom Haws <tom.haws@gmail.com> know if you are eager for this as static text.")))
  "N/A"
)
;; bubble-data-update: 2025-10-15 update: I think NOTEDATA (new attribute for this reactor project) may be repurposed to hold information about whether each bubble line is auto text (and what kind) or manual.
;; Placeholder. Does nothing . Needs to adjust the NOTEDATA element of ATTRIBUTE_LIST to contain DATA.
;; Steps:
;; 1. Delete any references whose STRING has been deleted by user.
;; 2. Delete any objects that are no longer needed in object list. Renumber references as needed.
;; 3. Add object to object list if needed. Otherwise, find its index value.
;; 4. Add reference to reference list.
;; 5. Update string in appropriate attribute of list.
;; Returns update attribute list.
(defun hcnm_ldrblk_adjust_notedata (data attribute_list / tag value)
  (setq
    tag   (car data)
    value (cadddr data)
  )
  (hcnm_ldrblk_save_attribute_to_list tag value attribute_list)
)
(defun c:hcnm-edit-bubbles ()
  (haws-core-init 337)
  (princ "\nCNM version: ")
  (princ (haws-unified-version))
  (if (not haws-editall)(load "editall"))
  (haws-editall t)
  (haws-core-restore)
)
;;; Add delimiter structure to plain text attributes for editing
(defun hcnm_eb:add_delimiters (attribute_list ename_bubble / result)
  (setq result '())
  (foreach attr attribute_list
    (setq result 
      (append result 
        (list 
          (list 
            (car attr)  ; TAG
            (hcnm_eb:expand_value_to_delimited (car attr) (cadr attr))
          )
        )
      )
    )
  )
  result
)
;;; Expand plain text value to delimiter structure using XDATA if available
;;; If value doesn't have chr(160) delimiters, try to parse using XDATA auto-text
;;; Otherwise migrate legacy format codes (%%u, %%o, \L, \O) to prefix field
;;; Special handling for NOTENUM, NOTEPHASE, NOTEGAP - these go to prefix field
;; Expand plain attribute value to delimited structure for editing.
;; Returns value with CHR 160 delimiters: prefix§auto§postfix
;; System-controlled tags (NOTENUM, NOTEPHASE, NOTEGAP) go entirely to prefix.
;; Legacy format codes (%%u, %%o, \L, \O) are migrated to prefix field.
;; User manual text is preserved in prefix field.
(defun hcnm_eb:expand_value_to_delimited (tag value / migrated delim_pos)
  (cond
    ((not value) (hcnm_eb:concat_parts "" "" ""))  ; NIL -> empty structure
    ((vl-string-search (chr 160) value) value)  ; Already has delimiters
    ((= value "") (hcnm_eb:concat_parts "" "" ""))  ; Empty -> empty structure
    ;; NOTENUM, NOTEPHASE, NOTEGAP are system-controlled - put in prefix, not auto
    ((member tag '("NOTENUM" "NOTEPHASE" "NOTEGAP"))
     (hcnm_eb:concat_parts value "" "")
    )
    (t 
     ;; Try legacy format code migration
     (setq migrated (hcnm_eb:migrate_legacy_format value))
     (if (setq delim_pos (vl-string-search (chr 160) migrated))
       ;; Migration created delimiter structure: prefix§auto - split it properly
       ;; VL-STRING-SEARCH returns 0-based position, SUBSTR uses 1-based
       (hcnm_eb:concat_parts 
         (substr migrated 1 delim_pos)  ; prefix: chars 1 through DELIM_POS
         (substr migrated (+ delim_pos 2))  ; auto: skip delimiter (position is 0-based, add 2 for 1-based + skip char)
         ""  ; empty postfix
       )
       ;; No format codes, put entire value in PREFIX to preserve user's manual text
       ;; (Auto field gets replaced by auto-text buttons, prefix/postfix are preserved)
       (hcnm_eb:concat_parts value "" "")
     )
    )
  )
)

(defun hcnm_edit_bubble (ename_bubble / bubble_data dclfile
                     ename_leader hcnm_eb:attribute_list
                     notetextradiocolumn return_list tag done_code
                    )
  (setq
    ename_leader
      (hcnm_ldrblk_bubble_leader ename_bubble)
    ;; Semi-global variable. Global to the HCNM-EB: functions called from here.
    ;; Add delimiter structure for editing
    hcnm_eb:attribute_list
      (hcnm_eb:add_delimiters (hcnm_get_attributes ename_bubble t) ename_bubble)
    notetextradiocolumn "RadioNOTETXT1"
    dclfile
      (load_dialog "cnm.dcl")
    done_code 2
  )
  ;; Show delimiter tip to help users understand the internal structure
  (haws_tip_show 1002  ; Unique tip ID for delimiter explanation
    "CNM uses a non-breaking space (ASCII code 160 or Alt+0160) to separate your text from auto text.\n\nKeep this in mind in the event you edit bubble notes without this editor.")
  (while (> done_code -1)
    (cond
      ((= done_code 0) (setq done_code (hcnm_edit_bubble_cancel)))
      ((= done_code 1)
       (setq done_code (hcnm_eb:save ename_bubble))
      )
      ((= done_code 2)
       ;; Show the CNM Bubble Note Editor dialog with the requested text line's radio button selected.
       (setq
         return_list
          (hcnm_eb:show dclfile notetextradiocolumn ename_bubble)
         done_code
          (car return_list)
         notetextradiocolumn
          (cadr return_list)
         tag
          (substr notetextradiocolumn 6)
       )
      )
      ((= done_code 29)
         ;; Change View button - clear viewport transformation data and immediately prompt for new association
         (hcnm_ldrblk_clear_viewport_transform_xdata ename_bubble)
         (princ "\nViewport association cleared. Please select the new target viewport.")
         (hcnm_ldrblk_warn_pspace_coordinates ename_bubble "Sta")
         (hcnm_ldrblk_capture_viewport_transform ename_bubble (hcnm_ldrblk_get_target_vport))
         (setq done_code 2)  ; Return to dialog
      )
      (t
       ;; Process clicked action tile (button) other than cancel or save.
       ;; bubble-data-update: This is start point 2 of 2 of the bubble data logic. This one is for the bubble note editing process.
       ;; this is called whenever a dialog auto-text button is clicked.
       (hcnm_eb:get_text ename_bubble done_code tag)
       (setq done_code 2)
      )
    )
  )
  ;; Change its arrowhead if needed.
  (hcnm_ldrblk_change_arrowhead ename_leader)
  (haws-core-restore)
  (princ)
)
;;; bubble-data-update: this or something below it needs to populate the NOTEDATA attribute.
(defun hcnm_eb:get_text (ename_bubble done_code tag / auto_string auto_type parts prefix auto postfix value)
  (setq
    auto_type
     (cadr (assoc done_code (hcnm_edit_bubble_done_codes)))
  )
  (cond
    ;; Handle ClearAuto button (code 28)
    ((= done_code 28)
     ;; Get current value, split it, clear the auto part, and save
     (setq value (cadr (assoc tag hcnm_eb:attribute_list))
           parts (hcnm_eb:split_on_nbsp value)
           prefix (nth 0 parts)
           postfix (nth 2 parts))
     ;; Save with empty auto field
     (setq hcnm_eb:attribute_list
       (hcnm_ldrblk_adjust_formats
         (hcnm_ldrblk_save_attribute_to_list
           tag
           (hcnm_eb:concat_parts prefix "" postfix)
           hcnm_eb:attribute_list
         )
       )
     )
    )
    ;; Handle auto-text generation buttons (only if AUTO_TYPE is valid)
    ((and auto_type (not (= auto_type "")))
     ;; bubble-data-update: this is called from command line and from edit box to get string as requested by user.
     (setq hcnm_eb:attribute_list
       (hcnm_ldrblk_adjust_formats
         (hcnm_ldrblk_auto_dispatch
           ename_bubble
           hcnm_eb:attribute_list
           tag
           auto_type
           nil  ; NIL = prompt user to select/confirm alignment
         )
       )
     )
     ;; Show paper space warning if coordinate auto-text was just added
     (hcnm_ldrblk_warn_pspace_coordinates ename_bubble auto_type)
    )
    ;; Invalid DONE_CODE - just ignore
    (t
     (princ (strcat "\nWarning: Invalid button code " (itoa done_code)))
    )
  )
)

(defun hcnm_edit_bubble_cancel ()
 -1
)
;;; Remove delimiters from ATTRIBUTE_LIST before saving
;;; Concatenates prefix+auto+postfix into plain text
(defun hcnm_eb:remove_delimiters (attribute_list / result)
  (setq result '())
  (foreach attr attribute_list
    (setq result 
      (append result 
        (list 
          (list 
            (car attr)  ; TAG
            (hcnm_eb:flatten_value (cadr attr))  ; Remove delimiters from VALUE
          )
        )
      )
    )
  )
  result
)
;;; Flatten a delimited value to plain text
;;; If value contains chr(160) delimiters, concatenate parts with spaces between non-empty parts
;;; Otherwise return as-is
(defun hcnm_eb:flatten_value (value / parts prefix auto postfix result)
  (cond
    ((not value) "")
    ((vl-string-search (chr 160) value)
     (setq parts (hcnm_eb:split_on_nbsp value)
           prefix (nth 0 parts)
           auto (nth 1 parts)
           postfix (nth 2 parts)
           result "")
     ;; Add prefix
     (if (and prefix (/= prefix ""))
       (setq result prefix)
     )
     ;; Add auto with space if needed
     (if (and auto (/= auto ""))
       (setq result 
         (if (= result "")
           auto
           (strcat result " " auto)
         )
       )
     )
     ;; Add postfix with space if needed
     (if (and postfix (/= postfix ""))
       (setq result 
         (if (= result "")
           postfix
           (strcat result " " postfix)
         )
       )
     )
     result
    )
    (t value)
  )
)
;; Save auto-text to XDATA for each attribute tag
(defun hcnm_eb:save (ename_bubble)
  ;; Save WITH delimiters (chr 160 non-breaking space is invisible in AutoCAD)
  ;; This preserves prefix/auto/postfix structure for next edit
  (hcnm_set_attributes ename_bubble hcnm_eb:attribute_list)
 -1
) 
(defun hcnm_edit_bubble_done_codes ( / eb_done)
  (setq eb_done t)
  '((11 "LF" eb_done)
    (12 "SF" eb_done)
    (13 "SY" eb_done)
    (14 "Sta" eb_done)
    (15 "Off" eb_done)
    (16 "StaOff" eb_done)
    (17 "AlName" eb_done)
    (18 "StaName" eb_done)
    (19 "N" eb_done)
    (20 "E" eb_done)
    (21 "NE" eb_done)
    (22 "Z" eb_done)
    (23 "Text" eb_done)
    (25 "Dia" eb_done)
    (26 "Slope" eb_done)
    (27 "L" eb_done)
   )
)
;;; Migrate legacy format codes from auto field to prefix field.
;;; If VALUE doesn't have delimiters but starts with format codes,
;;; move the codes to a new prefix field and create delimiter structure.
;;; Returns migrated value with prefix§auto§postfix structure if codes found,
;;; otherwise returns VALUE unchanged.
(defun hcnm_eb:migrate_legacy_format (value / sep format_code text)
  (cond
    ;; Empty or nil - return as-is
    ((or (not value) (= value "")) value)
    ;; Already has delimiters - no migration needed
    ((vl-string-search (chr 160) value) value)
    ;; Check for mtext underline
    ((wcmatch value "\\L*")
     (setq format_code "\\L"
           text (substr value 3))
     (strcat format_code (chr 160) text)
    )
    ;; Check for mtext overline
    ((wcmatch value "\\O*")
     (setq format_code "\\O"
           text (substr value 3))
     (strcat format_code (chr 160) text)
    )
    ;; Check for dtext underline
    ((wcmatch value "%%u*")
     (setq format_code "%%u"
           text (substr value 4))
     (strcat format_code (chr 160) text)
    )
    ;; Check for dtext overline
    ((wcmatch value "%%o*")
     (setq format_code "%%o"
           text (substr value 4))
     (strcat format_code (chr 160) text)
    )
    ;; No format codes - return as-is
    (t value)
  )
)
(defun hcnm_eb:show
   (dclfile notetextradiocolumn ename_bubble / tag value parts prefix auto postfix on_model_tab_p)
  (new_dialog "HCNMEditBubble" dclfile)
  (set_tile "Title" "Edit CNM Bubble Note")
  ;; Check if bubble is in paper space
  (setq on_model_tab_p (or (not ename_bubble) (hcnm_ldrblk_is_on_model_tab ename_bubble)))
  ;; Show/hide paper space disclaimer and Chg View button
  ;; Always show paper space warning above OKCancel
  ;; EXECUTIVE: The general disclaimer in the edit dialog is sufficient.
  ;; When user actually adds coordinate auto-text (Sta/Off/etc), they get
  ;; the detailed dismissable tip via HCNM_LDRBLK_WARN_PSPACE_COORDINATES.
  (set_tile 
    "Message"         
    (strcat 
      "Note: Paper space bubbles don't react to viewport changes."
     (haws_evangel_msg)
    )
  )  
  (mode_tile "ChgView" 0)  ; Always enable
  ;; Note attribute edit boxes - split into prefix/auto/postfix
  ;; Migration already happened in EXPAND_VALUE during ADD_DELIMITERS
  (foreach
     attribute hcnm_eb:attribute_list
    (setq tag (car attribute)
          value (cadr attribute)
          ;; Split by chr(1) delimiter
          parts (hcnm_eb:split_on_nbsp value)
          prefix (nth 0 parts)
          auto (nth 1 parts)
          postfix (nth 2 parts))
    ;; Set prefix field
    (set_tile (strcat "Prefix" tag) prefix)
    (action_tile
      (strcat "Prefix" tag)
      (strcat "(HCNM_EB:SAVE_PREFIX \"" tag "\" $value)")
    )
    ;; Set auto field (disabled, for display only)
    (set_tile (strcat "Edit" tag) auto)
    ;; Set postfix field
    (set_tile (strcat "Postfix" tag) postfix)
    (action_tile
      (strcat "Postfix" tag)
      (strcat "(HCNM_EB:SAVE_POSTFIX \"" tag "\" $value)")
    )
  )
  ;;Radio buttons
  (set_tile
    "NoteTextRadioColumn"
    notetextradiocolumn
  )
  (action_tile
    "NoteTextRadioColumn"
    "(SETQ NoteTextRadioColumn $value)"
  )
  ;;Auto text buttons
  (mapcar
    '(lambda (code)
       (action_tile
         (cadr code)
         (strcat "(DONE_DIALOG " (itoa (car code)) ")")
       )
     )
    (hcnm_edit_bubble_done_codes)
  )
  ;; Clear Auto Text button
  (action_tile "ClearAuto" "(DONE_DIALOG 28)")
  ;; Change View button (paper space only)
  (action_tile "ChgView" "(DONE_DIALOG 29)")
  (action_tile "accept" "(DONE_DIALOG 1)")
  (action_tile "cancel" "(DONE_DIALOG 0)")
  (list (start_dialog) notetextradiocolumn)
)
;; Split value on chr(1) delimiter into (prefix auto postfix)
;; Returns list of three strings, using "" for missing parts
(defun hcnm_eb:split_on_nbsp (value / nbsp parts)
  (setq nbsp (chr 160))  ; Non-breaking space - invisible delimiter
  (cond
    ((not value) '("" "" ""))
    ((not (vl-string-search nbsp value))
     ;; No separator - treat entire value as PREFIX (preserves user manual text)
     (list value "" ""))
    (t
     ;; Split on NBSP
     (setq parts (hcnm_eb:split_string value nbsp))
     (cond
       ((= (length parts) 3) parts)
       ((= (length parts) 2) (append parts '("")))
       ((= (length parts) 1) (append '("") parts '("")))
       (t '("" "" ""))
     )
    )
  )
)

;; Split string on delimiter
(defun hcnm_eb:split_string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq result (append result (list (substr str 1 pos)))
          str (substr str (+ pos 2)))
  )
  (append result (list str))
)

;; Concatenate prefix/auto/postfix with chr(1) delimiter
(defun hcnm_eb:concat_parts (prefix auto postfix / nbsp)
  (setq nbsp (chr 160))  ; Non-breaking space - invisible delimiter
  (strcat 
    (if prefix prefix "")
    (if (and prefix auto (> (strlen prefix) 0) (> (strlen auto) 0)) nbsp "")
    (if auto auto "")
    (if (and auto postfix (> (strlen auto) 0) (> (strlen postfix) 0)) nbsp "")
    (if postfix postfix "")
  )
)

;; Save prefix - get current auto and postfix, then concatenate
(defun hcnm_eb:save_prefix (tag prefix_new / attr parts auto postfix value_new updated_parts)
  (setq attr (assoc tag hcnm_eb:attribute_list)
        parts (hcnm_eb:split_on_nbsp (cadr attr))
        auto (nth 1 parts)
        postfix (nth 2 parts)
        value_new (hcnm_eb:concat_parts prefix_new auto postfix))
  (setq hcnm_eb:attribute_list
    (hcnm_ldrblk_adjust_formats
      (hcnm_ldrblk_save_attribute_to_list tag value_new hcnm_eb:attribute_list)
    )
  )
  ;; Update dialog display to reflect any changes from ADJUST_FORMATS
  (setq updated_parts (hcnm_eb:split_on_nbsp (cadr (assoc tag hcnm_eb:attribute_list))))
  (set_tile (strcat "Prefix" tag) (nth 0 updated_parts))
  (set_tile (strcat "Edit" tag) (nth 1 updated_parts))
  (set_tile (strcat "Postfix" tag) (nth 2 updated_parts))
)

;; Save postfix - get current prefix and auto, then concatenate
(defun hcnm_eb:save_postfix (tag postfix_new / attr parts prefix auto value_new updated_parts)
  (setq attr (assoc tag hcnm_eb:attribute_list)
        parts (hcnm_eb:split_on_nbsp (cadr attr))
        prefix (nth 0 parts)
        auto (nth 1 parts)
        value_new (hcnm_eb:concat_parts prefix auto postfix_new))
  (setq hcnm_eb:attribute_list
    (hcnm_ldrblk_adjust_formats
      (hcnm_ldrblk_save_attribute_to_list tag value_new hcnm_eb:attribute_list)
    )
  )
  ;; Update dialog display to reflect any changes from ADJUST_FORMATS
  (setq updated_parts (hcnm_eb:split_on_nbsp (cadr (assoc tag hcnm_eb:attribute_list))))
  (set_tile (strcat "Prefix" tag) (nth 0 updated_parts))
  (set_tile (strcat "Edit" tag) (nth 1 updated_parts))
  (set_tile (strcat "Postfix" tag) (nth 2 updated_parts))
)

(defun hcnm_eb:save_edit_box (tag input)
  (setq
    hcnm_eb:attribute_list
     (hcnm_ldrblk_adjust_formats
       (hcnm_ldrblk_save_attribute_to_list
         tag
         input
         hcnm_eb:attribute_list
       )
     )
  )
)
;; FIELD_CODE_P NIL SIMPLIFIES PROCESSING WHEN BLOCKS LIKE NOTEQTY ARE KNOWN NOT TO HAVE FIELD CODES IN THEM
(defun hcnm_get_attributes (ename_block field_code_p / attribute_list elist ename_next etype field_code obj_next)
  (setq ename_next ename_block)
  (while (and
           (setq ename_next (entnext ename_next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename_next)))))
           )
         )
    (cond
      ((= etype "ATTRIB")
       (setq
         obj_next (vlax-ename->vla-object ename_next)
         attribute_list
          (cons
            (list 
              (cdr (assoc 2 elist)) 
              (cond 
                ((and field_code_p (setq field_code (lm:fieldcode ename_next))) field_code)
                (t (vla-get-textstring obj_next)))
            )
            attribute_list
          )
       )
      ) ;_ end of and
    )
  )
  attribute_list
)
(defun lm:fieldcode ( en / fd id )
    (cond
        (   (and
                (wcmatch (cdr (assoc 0 (setq en (entget en)))) "TEXT,MTEXT,ATTRIB")
                (setq en (cdr (assoc 360 en)))
                (setq en (dictsearch en "ACAD_FIELD"))
                (setq en (dictsearch (cdr (assoc -1 en)) "TEXT"))
                (setq fd (entget (cdr (assoc 360 en))))
            )
            (if (vl-string-search "\\_FldIdx " (cdr (assoc 2 en)))
                (vl-string-subst
                    (if (setq id (cdr (assoc 331 fd)))
                        (vl-string-subst
                            (strcat "ObjId " (itoa (vla-get-objectid (vlax-ename->vla-object id))))
                            "ObjIdx 0"
                            (cdr (assoc 2 fd))
                        )
                        (cdr (assoc 2 fd))
                    )
                    "\\_FldIdx 0"
                    (cdr (assoc 2 en))
                )
                (cdr (assoc 2 en))
            )
        )
    )
)

(defun hcnm_set_attributes (ename_block attribute_list / atag elist ename_next etype obj_next)
  (setq ename_next ename_block)
  (while (and
           (setq ename_next (entnext ename_next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename_next)))))
           )
         )
    (cond
      ((and
         (= etype "ATTRIB")
         (setq atag (cdr (assoc 2 elist)))
         (assoc atag attribute_list)
       ) ;_ end of and
        (setq obj_next (vlax-ename->vla-object ename_next))
        (vla-put-textstring
          obj_next
          (cadr (assoc atag attribute_list))
        )
        ;; UPDATEFIELD commented out to avoid "0 field(s) found/updated" messages
        ;; May be necessary for some bubble types - uncomment if needed
        ;(COND ((= (HCNM_LDRBLK_GET_MTEXT_STRING) "")(VL-CMDF "._updatefield" ENAME_NEXT "")))
      )
    )
  )
)
;;; NEED TO DEFINE THE USER EVENTS THAT TRIGGER REACTOR CLEANUP. OR MAYBE THE CALLBACK DOES THE CLEANUP. CLEANUP ENTAILS THE FOLLOWING STEPS
;;; -	USE THE REACTOR DATA AND NOTEDATA ATTRIBUTE OF ALL BUBBLES TO FIND ATTRIBUTES THAT ARE AFFECTED BY THE REACTOR
;;; -	IF AN ATTRIBUTE IS EMPTY, REMOVE ITS REFERENCE IN ITS BUBBLE'S NOTEDATA AND IN THE REACTOR DATA.
;;; -	IF THE BUBBLE HAS NO NOTEDATA LEFT FOR THE OWNER OBJECT THAT TRIGGERED THE REACTOR, REMOVE THE BUBBLE FROM THE REACTOR DATA.
;;; 
;;; NEED TO CHECK FOR REACTOR BEFORE ATTACHING ONE
;;; CHECK THE DATA (VLR-DATA REACTOR) OF ALL REACTORS (VLR-REACTORS :VLR-OBJECT-REACTOR) OR (VLR-PERS-LIST) TO SEE IF THIS OBJECT IS ALREADY ATTACHED. IF SO, DON'T ADD. SEE ALSO OBJECTS (VLR-OWNERS REACTOR) AND CALLBACKS (VLR-REACTIONS REACTOR)
;;; 
;;; IF THE REACTOR ALREADY EXISTS:
;;; 1.	(VLR-DATA-SET) OVERWRITES THE DATA (Integer, Real, String, List, VLA-object, Safearray, Variant, T, or nil) ADDED BY MY APPLICATION TO THE REACTOR.
;;; 2.	(vlr-owner-add) ADDS AN OBJECT AS AN OWNER ON THE REACTOR. (vlr-owner-remove) REMOVES ONE 
;;; 3.	(vlr-remove) DISABLES A REACTOR. (vlr-remove-all) DISABLES ALL OF SPECIFIED TYPE. (vlr-add) ENABLES A DISABLED REACTOR.
;;; 
;;; ABOUT MODIFYING REACTORS: https://help.autodesk.com/view/ACDLT/2025/ENU/?guid=GUID-F6B719E4-537B-42C2-8D22-9A313FE900A0
;;; You can add and remove owners (objects) of a reactor.
;;; You can reset the data of a reactor.

;;Playing with reactors
(defun hcnm_ldrblk_list_reactors ( / reactors)
  (setq reactors (cdar(vlr-reactors :vlr-object-reactor)))
  (foreach reactor reactors
    ;;(vlax-dump-object REACTOR)
    (print (vl-prin1-to-string (vlr-data reactor)))
    (print (vl-prin1-to-string (vlr-reactions reactor)))
    ;; (vlr-remove REACTOR)
  )
)
;; ABOUT MODIFYING REACTORS: https://help.autodesk.com/view/ACDLT/2025/ENU/?guid=GUID-F6B719E4-537B-42C2-8D22-9A313FE900A0
;; You can add and remove owners (objects) of a reactor.
;; You can reset the data of a reactor.
;; (VLR-REMOVE-ALL :VLR-OBJECT-REACTOR) TO REMOVE ALL
;; (VLR-DATA (CADAR (VLR-REACTORS :VLR-OBJECT-REACTOR))) IF THERE IS ONLY ONE REACTOR
;; (VLR-OWNERS (CADAR (VLR-REACTORS :VLR-OBJECT-REACTOR))) IF THERE IS ONLY ONE REACTOR
;; New structure: KEYS = '("HCNM-BUBBLE" HANDLE_REFERENCE HANDLE_BUBBLE TAG)
;; VALUE = AUTO_TYPE (just the string)
(defun hcnm_ldrblk_assure_auto_text_has_reactor (objref ename_bubble ename_leader tag auto_type / callbacks data data_old reactor 
                                                 handle_bubble handle_reference keys key_app reactor_old reactors_old owner owners object_leader
                                                 hcnm_reactors reactor_count
                                                ) 
  (setq callbacks       '((:vlr-modified . hcnm_ldrblk_reactor_callback)
                          ;; vlr-trace-reaction IS A CANNED CALLBACK PROVIDED BY AUTODESK FOR TESTING. IT PRINTS A MESSAGE. IT CAN BE REMOVED.
                          ;; (:vlr-modified . vlr-trace-reaction)
                         )
        reactors_old    (cdar (vlr-reactors :vlr-object-reactor))
        object_leader   (cond (ename_leader (vlax-ename->vla-object ename_leader)))
        owners          (cond 
                          ;; If OBJREF is NIL (for N/E/NE), only attach to leader
                          ((and (not objref) object_leader) (list object_leader))
                          (object_leader (list objref object_leader))
                          (t (list objref))
                        )
        key_app         "HCNM-BUBBLE"
        handle_reference (cond (objref (vla-get-handle objref)) (t ""))
        handle_bubble   (cdr (assoc 5 (entget ename_bubble)))
        keys            (list key_app handle_reference handle_bubble tag)
        reactor_old     nil  ; Initialize to nil
  )
  ;; Check for reactor proliferation (FATAL CONDITION)
  (setq hcnm_reactors
    (vl-remove-if-not
      '(lambda (r)
         (and (listp (vlr-data r))
              (assoc key_app (vlr-data r)))
       )
      reactors_old
    )
    reactor_count (length hcnm_reactors)
  )
  ;; Handle reactor proliferation
  (cond
    ((> reactor_count 1)
     ;; FATAL: Multiple HCNM-BUBBLE reactors found - this is a programming error
     (vlr-remove-all :vlr-object-reactor)
     (alert (princ (strcat
       "\n*** PROGRAMMING ERROR DETECTED ***\n\n"
       "Found " (itoa reactor_count) " HCNM-BUBBLE reactors.\n"
       "There should be exactly ONE reactor for all bubbles.\n\n"
       "All reactors have been removed to prevent data corruption.\n"
       "The current bubble will get a new reactor and should be reactive.\n"
       "All later bubbles should also be reactive to leader or reference object changes.\n\n"
       "Please report this bug with details about what operations\n"
       "you performed before this error occurred.\n\n"
       "GitHub: https://github.com/hawstom/cnm/issues"
     )))
     (princ "\n*** REACTOR PROLIFERATION ERROR - All reactors removed, creating new reactor for current bubble ***")
     ;; Set REACTOR_OLD to NIL so we create a fresh reactor below
     (setq reactor_old nil)
    )
    (t
     ;; Normal operation - 0 or 1 reactor
     (setq reactor_old (car hcnm_reactors))
    )
  )
  ;; Now handle reactor attachment/creation based on REACTOR_OLD
  (cond 
    (reactor_old
     ;; ATTACH THIS OWNER NOTIFIER IF NOT ALREADY ATTACHED.
     (foreach owner owners
       (cond
         ((not (member owner (vlr-owners reactor_old)))
          (vlr-owner-add reactor_old owner)
         )
       )
     )
     ;; UPDATE THE DATA
     (vlr-data-set reactor_old 
                   (setq data (haws_nested_list_update 
                                (vlr-data reactor_old)
                                keys
                                auto_type
                              )
                   )
     )
    )
    (t
     ;; ELSE MAKE REACTOR AND MAKE IT PERSISTENT
     (setq data    (haws_nested_list_update 
                     nil
                     keys
                     auto_type
                   )
           reactor (vlr-object-reactor 
                     owners ; ATTACHED OWNERS OF REACTOR
                     data
                     callbacks
                   )
           reactor (vlr-pers reactor)
     )
    )
  )
)
(defun hcnm_ldrblk_reactor_callback (obj_notifier obj_reactor parameter-list / key_app data_old data handle_notifier reference_list found_p handle_reference bubble_list handle_bubble tag_list tag auto_type)
  ;; Skip reactor processing during space transitions to avoid spurious modification events
  (cond
    ((= (c:hcnm-config-getvar "AllowReactors") "0") 
     nil  ; Return early if reactors are not allowed
    )
    (t
     ;; Wrap in error handler to catch erased objects
     (if (not (vl-catch-all-error-p 
               (vl-catch-all-apply 'VLA-GET-HANDLE (list obj_notifier))))
       (progn
         ;; Normal reactor processing - object is valid
         (setq key_app         "HCNM-BUBBLE"
               data_old        (vlr-data obj_reactor)
               data            data_old
               handle_notifier (vla-get-handle obj_notifier)
               reference_list  (cadr (assoc key_app data))
               found_p         nil
         )
     ;; Iterate through all references in the reactor data
     (foreach reference reference_list
    (setq handle_reference (car reference)
          bubble_list      (cadr reference))
    ;; Check if this is the reference that was modified
    (cond
      ((= handle_notifier handle_reference)
       ;; Reference object modified - update all bubbles using this reference
       (setq found_p t)
       (princ (strcat "\nReference modified: " handle_reference))
       ;; Temporarily disable reactors to prevent infinite loop during updates
       (c:hcnm-config-setvar "AllowReactors" "0")
       (foreach bubble bubble_list
         (setq handle_bubble (car bubble)
               tag_list      (cadr bubble))
         ;; Update all tags for this bubble
         (foreach tag_data tag_list
           (setq tag       (car tag_data)
                 auto_type (cadr tag_data))
           (hcnm_ldrblk_update_bubble_tag handle_bubble tag auto_type handle_reference)
         )
       )
       ;; Re-enable reactors after updates complete
       (c:hcnm-config-setvar "AllowReactors" "1")
      )
      (t
       ;; Notifier is not this reference, might be a leader - check each bubble
       (foreach bubble bubble_list
         (setq handle_bubble (car bubble)
               tag_list      (cadr bubble))
         ;; Check if this bubble's leader moved by comparing with notifier
         ;; We need to check if the leader for this bubble is the notifier
         ;; Since leaders are associated with bubbles, we check the bubble's leader
         (cond
           ((hcnm_ldrblk_bubble_has_leader handle_bubble handle_notifier)
            ;; This bubble's leader moved - update all its tags with the correct reference
            (setq found_p t)
            (princ (strcat "\nLeader modified for bubble: " handle_bubble " using reference: " handle_reference))
            ;; Temporarily disable reactors to prevent infinite loop during updates
            (c:hcnm-config-setvar "AllowReactors" "0")
            (foreach tag_data tag_list
              (setq tag       (car tag_data)
                    auto_type (cadr tag_data))
              (hcnm_ldrblk_update_bubble_tag handle_bubble tag auto_type handle_reference)
            )
            ;; Re-enable reactors after updates complete
            (c:hcnm-config-setvar "AllowReactors" "1")
           )
         )
       )
      )
    )
  )
  (cond
    ((not found_p)
      (princ (strcat "\nWarning: Notifier " handle_notifier " not found in reactor data"))
    )
  )
         (cond 
           ;; DETACH THE NOTIFIER IF USER REMOVED ALL ITS DEPENDENT AUTO-TEXT FROM ALL BUBBLES.
           ((not reference_list)
            (vlr-owner-remove obj_reactor obj_notifier)
           )
           ;; UPDATE THE REACTOR DATA IF USER REMOVED SOME OF ITS DEPENDENT AUTO-TEXT
           ((not (equal data data_old))
            (vlr-data-set obj_reactor data)
           )
         )
       )  ; End of PROGN for valid object
       ;; ELSE: Object was erased, silently ignore
       (princ "\nReactor fired on erased object - ignoring")
     )  ; End of IF checking for valid object
    )  ; End of normal reactor processing (T branch)
  )  ; End of suppression check COND
)
;; Helper function to check if a bubble has a specific leader
(defun hcnm_ldrblk_bubble_has_leader (handle_bubble handle_leader / ename_bubble ename_leader)
  (setq ename_bubble (handent handle_bubble))
  (cond
    (ename_bubble
      (setq ename_leader (hcnm_ldrblk_bubble_leader ename_bubble))
      (cond
        (ename_leader
          (= handle_leader (cdr (assoc 5 (entget ename_leader))))
        )
        (t nil)
      )
    )
    (t nil)
  )
)
;; Helper function to check if entity is on the "Model" tab
(defun hcnm_ldrblk_is_on_model_tab (ename / layout_name)
  (setq layout_name (cdr (assoc 410 (entget ename))))
    (= (strcase layout_name) "MODEL")
)
;; Updates a specific tag in a bubble based on reactor notification
;; Called when either the leader moves or the reference object changes
(defun hcnm_ldrblk_update_bubble_tag (handle_bubble tag auto_type handle_reference / ename_bubble ename_reference attribute_list attribute_list_old objref)
  (setq ename_bubble       (handent handle_bubble)
        ;; Handle empty string for N/E/NE which have no reference object
        ename_reference    (cond 
                             ((= handle_reference "") nil)
                             (t (handent handle_reference))
                           )
        attribute_list_old (hcnm_get_attributes ename_bubble t)
        attribute_list     attribute_list_old
  )
  (cond
    (ename_bubble
      ;; For reactor updates, use a special marker for coordinate types (no reference object)
      ;; This allows auto-text functions to distinguish between initial creation (NIL) and reactor update (T)
      (setq objref (cond 
                     (ename_reference (vlax-ename->vla-object ename_reference))
                     (t t)  ; Use T as sentinel value for reactor updates with no reference object
                   ))
      (setq attribute_list 
        (hcnm_ldrblk_auto_dispatch 
          ename_bubble
          attribute_list
          tag
          auto_type
          objref  ; Pass the reference object as INPUT, or T for coordinate-only reactor updates
        )
      )
      (cond 
        ((/= attribute_list attribute_list_old)
         ;; UPDATE BLOCK INSERTION
         (hcnm_set_attributes 
           ename_bubble
           (hcnm_ldrblk_adjust_formats 
             attribute_list
           )
         )
        )
      )
    )
    (t
      (princ (strcat "\nError in HCNM_LDRBLK_UPDATE_BUBBLE_TAG: BUBBLE not found"))
    )
  )
)
;; UPDATES A BUBBLE INSERTION. RETURNS BUBBLE WITH ATTRIBUTES REMOVED IF USER REMOVED THEM (SINCE WE DON'T YET HAVE A STRUCTURED INTERFACE THAT WOULD LET THE USER REMOVE IN REAL TIME. MAYBE IT WOULD BE EASIEST TO JUST HAVE A WAY TO DISABLE EDITING AUTO ATTRIBUTES UNLESS USER CLICKS A "MANUAL" BUTTON. I LOVE THAT. EDIT BUBBLE COULD READ THE REACTOR DATA OR EACH BUBBLE COULD HAVE A LIST IN NOTEDATA)
;; @returns {list} Testing
;; NOTE: This function is now deprecated in favor of HCNM_LDRBLK_UPDATE_BUBBLE_TAG but kept for reference
(defun hcnm_ldrblk_update_bubble (lst_bubble obj_notifier / attribute_list attribute_list_old ename_bubble pt_leader_start) 
  (setq ename_bubble       (handent (car lst_bubble))
        attribute_list_old (hcnm_get_attributes ename_bubble t)
        attribute_list     attribute_list_old
  )
  (foreach attribute (cadr lst_bubble) 
    (cond 
      ;; AT THE MOMENT, ONLY TOTAL ATTRIBUTE DELETION CANCELS A REACTOR.
      ((= (cadr (assoc (car attribute) attribute_list)) "")
       ;; I AM CURIOUS WHETHER MODIFYING BUBBLE WITHIN FOREACH WILL CAUSE A BUG. I THINK NOT SINCE IT ITERATES OVER A COPY.
       (setq lst_bubble (vl-remove attribute lst_bubble))
      )
      (t
       (setq attribute_list (hcnm_ldrblk_auto_dispatch 
                              ename_bubble
                              attribute_list
                              (car attribute) ; TAG
                              (cdr attribute) ; KEY
                              obj_notifier ; INPUT bubble-data-update: This is not right. We need, not the notifier object (which could be a leader or an alignment or other), but the reference object. So the reactor data has to include handle, tag, ref_type, and ref_object
                            )
       )
      )
    )
  )
  (cond 
    ((/= attribute_list attribute_list_old)
     ;; UPDATE BLOCK INSERTION
     (hcnm_set_attributes 
       ename_bubble
       (hcnm_ldrblk_adjust_formats 
         attribute_list
       )
     )
    )
  )
  lst_bubble
)
;#endregion
;#region CNM Options dialog
(defun c:hcnm-cnmoptions (/ cnmdcl done_code retn)
  (haws-core-init 210)
  (hcnm_projinit)
  (hcnm_proj)
 ;; Load Dialog
  (setq cnmdcl (load_dialog "cnm.dcl"))
  (setq done_code 2)
  (while (> done_code -1)
    (setq done_code
      (cond
        ((= done_code 0)(hcnm_dcl_options_cancel))
        ((= done_code 1)(hcnm_dcl_options_save))
        ((= done_code 2)(hcnm_dcl_options_show cnmdcl))
        ((= done_code 11)(hcnm_dcl_general_show cnmdcl))
        ((= done_code 12)(hcnm_dcl_bubble_show cnmdcl))
        ((= done_code 13)(hcnm_dcl_key_show cnmdcl))
        ((= done_code 14)(hcnm_dcl_qt_show cnmdcl))
      )
    )
  )
 (haws-core-restore)  
 (princ)
)

(defun hcnm_dcl_options_cancel()
 (hcnm_config_temp_clear)
 -1
)

;; Saves, then passes control to temp var clear function.
(defun hcnm_dcl_options_save()
 (hcnm_config_temp_save)
 0
)

(defun hcnm_dcl_options_show (cnmdcl)
  (new_dialog "HCNMOptions" cnmdcl)
  (set_tile "Title" "CNM Options")
  (action_tile "General" "(DONE_DIALOG 11)")
  (action_tile "Bubble" "(DONE_DIALOG 12)")
  (action_tile "Key" "(DONE_DIALOG 13)")
  (action_tile "QT" "(DONE_DIALOG 14)")
  (action_tile "accept" "(DONE_DIALOG 1)")
  (action_tile "cancel" "(DONE_DIALOG 0)")
  (start_dialog)
)

(defun hcnm_dcl_general_show (cnmdcl)
  (new_dialog "HCNMGeneral" cnmdcl)
  ;; Dialog Actions
  (set_tile "Title" "CNM General Options")
  (hcnm_config_set_action_tile "DoCurrentTabOnly")
  (hcnm_config_dcl_list "InsertTablePhases")
  (hcnm_config_set_action_tile "PhaseAlias1")
  (hcnm_config_set_action_tile "PhaseAlias2")
  (hcnm_config_set_action_tile "PhaseAlias3")
  (hcnm_config_set_action_tile "PhaseAlias4")
  (hcnm_config_set_action_tile "PhaseAlias5")
  (hcnm_config_set_action_tile "PhaseAlias6")
  (hcnm_config_set_action_tile "PhaseAlias7")
  (hcnm_config_set_action_tile "PhaseAlias8")
  (hcnm_config_set_action_tile "PhaseAlias9")
  (hcnm_config_set_action_tile "NotesKeyTableDimstyle")
  (hcnm_config_set_action_tile "NotesLeaderDimstyle")
  (set_tile
    "ProjectFolder"
    (strcat
      "Project folder "
      (hcnm_shorten_path (hcnm_proj) 100)
    )
  )
  (hcnm_config_set_action_tile "ProjectNotes")
  (action_tile
    "ProjectNotesBrowse"
    "(HCNM_CONFIG_TEMP_SETVAR \"ProjectNotes\"(HCNM_GETPROJNOTES))(SET_TILE \"ProjectNotes\" (HCNM_CONFIG_TEMP_GETVAR \"ProjectNotes\"))"
  )
  (hcnm_config_dcl_list "LayersEditor")
  (hcnm_config_dcl_list "ProjectNotesEditor")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm_dcl_bubble_show (cnmdcl)
  (new_dialog "HCNMBubble" cnmdcl)
  (set_tile "Title" "CNM Bubble Options")
  (hcnm_config_set_action_tile "BubbleHooks")
  (hcnm_config_set_action_tile "BubbleMtext")
  (hcnm_config_set_action_tile "BubbleAreaIntegral")
  (hcnm_config_set_action_tile "NoteTypes")
  (hcnm_config_set_action_tile "BubbleTextLine1PromptP")
  (hcnm_config_set_action_tile "BubbleTextLine2PromptP")
  (hcnm_config_set_action_tile "BubbleTextLine3PromptP")
  (hcnm_config_set_action_tile "BubbleTextLine4PromptP")
  (hcnm_config_set_action_tile "BubbleTextLine5PromptP")
  (hcnm_config_set_action_tile "BubbleTextLine6PromptP")
  (hcnm_config_set_action_tile "BubbleTextLine0PromptP")
  (hcnm_config_set_action_tile "BubbleSkipEntryPrompt")
  (hcnm_config_set_action_tile "BubbleOffsetDropSign")
  (hcnm_config_set_action_tile "BubbleTextPrefixLF")
  (hcnm_config_set_action_tile "BubbleTextPrefixSF")
  (hcnm_config_set_action_tile "BubbleTextPrefixSY")
  (hcnm_config_set_action_tile "BubbleTextPrefixSta")
  (hcnm_config_set_action_tile "BubbleTextPrefixOff+")
  (hcnm_config_set_action_tile "BubbleTextPrefixOff-")
  (hcnm_config_set_action_tile "BubbleTextPrefixN")
  (hcnm_config_set_action_tile "BubbleTextPrefixE")
  (hcnm_config_set_action_tile "BubbleTextPrefixZ")
  (hcnm_config_set_action_tile "BubbleTextPostfixLF")
  (hcnm_config_set_action_tile "BubbleTextPostfixSF")
  (hcnm_config_set_action_tile "BubbleTextPostfixSY")
  (hcnm_config_set_action_tile "BubbleTextPostfixSta")
  (hcnm_config_set_action_tile "BubbleTextPostfixOff+")
  (hcnm_config_set_action_tile "BubbleTextPostfixOff-")
  (hcnm_config_set_action_tile "BubbleTextPostfixN")
  (hcnm_config_set_action_tile "BubbleTextPostfixE")
  (hcnm_config_set_action_tile "BubbleTextPostfixZ")
  (hcnm_config_set_action_tile "BubbleTextJoinDelSta")
  (hcnm_config_set_action_tile "BubbleTextJoinDelN")
  (hcnm_config_set_action_tile "BubbleTextPrecisionLF")
  (hcnm_config_set_action_tile "BubbleTextPrecisionSF")
  (hcnm_config_set_action_tile "BubbleTextPrecisionSY")
  (hcnm_config_set_action_tile "BubbleTextPrecisionOff+")
  (hcnm_config_set_action_tile "BubbleTextPrecisionN")
  (hcnm_config_set_action_tile "BubbleTextPrecisionE")
  (hcnm_config_set_action_tile "BubbleTextPrecisionZ")
  (hcnm_config_set_action_tile "BubbleTextPrefixPipeDia")
  (hcnm_config_set_action_tile "BubbleTextPostfixPipeDia")
  (hcnm_config_set_action_tile "BubbleTextPrecisionPipeDia")
  (hcnm_config_set_action_tile "BubbleTextPrefixPipeSlope")
  (hcnm_config_set_action_tile "BubbleTextPostfixPipeSlope")
  (hcnm_config_set_action_tile "BubbleTextPrecisionPipeSlope")
  (hcnm_config_set_action_tile "BubbleTextPrefixPipeLength")
  (hcnm_config_set_action_tile "BubbleTextPostfixPipeLength")
  (hcnm_config_set_action_tile "BubbleTextPrecisionPipeLength")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm_dcl_key_show(cnmdcl)
  (new_dialog "HCNMKey" cnmdcl)
  ;; Dialog Actions
  (set_tile "Title" "CNM Key Notes Table Options")
  (hcnm_config_set_action_tile "DescriptionWrap")
  (hcnm_config_set_action_tile "LineSpacing")
  (hcnm_config_set_action_tile "NoteSpacing")
  (hcnm_config_set_action_tile "ShowKeyTableTitleShapes")
  (hcnm_config_set_action_tile "ShowKeyTableQuantities")
  (hcnm_config_set_action_tile "ShowKeyTableGrid")
  (hcnm_config_set_action_tile "TableWidth")
  (hcnm_config_set_action_tile "PhaseWidthAdd")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm_dcl_qt_show(cnmdcl)
  (new_dialog "HCNMQT" cnmdcl)
  ;; Dialog Actions
  (set_tile "Title" "CNM Quantity Take-off Table Options")
  (hcnm_config_set_action_tile "NumberToDescriptionWidth")
  (hcnm_config_set_action_tile "DescriptionToQuantityWidth")
  (hcnm_config_set_action_tile "QuantityToQuantityWidth")
  (hcnm_config_set_action_tile "QuantityToUnitsWidth")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm_options_list_data ()
  '(
    ("ProjectNotesEditor" (("text" "System Text Editor") ("csv" "System CSV (spreadsheet)") ("cnm" "CNM Pro Editor")))
    ("LayersEditor" (("notepad" "Notepad") ("cnm" "CNM Pro Editor")))
    ("InsertTablePhases" (("No" "No")("1" "1")("2" "2")("3" "3")("4" "4")("5" "5")("6" "6")("7" "7")("8" "8")("9" "9")("10" "10")))
  )
)
(defun hcnm_config_set_action_tile (var)
  (set_tile var (hcnm_config_temp_getvar var))
  (action_tile
    var
    (strcat "(HCNM_CONFIG_TEMP_SETVAR \"" var "\" $value)")
  )
)
(defun hcnm_config_dcl_list (key /)
  (hcnm_set_tile_list
    key
    (mapcar
      '(lambda (x) (cadr x))
      (cadr (assoc key (hcnm_options_list_data)))
    )
    (cadr
      (assoc
        (c:hcnm-config-getvar key)
        (cadr (assoc key (hcnm_options_list_data)))
      )
    )
  )
  (action_tile
    key
    "(HCNM_CONFIG_DCL_LIST_CALLBACK $key $value)"
  )
)
(defun hcnm_set_tile_list (key options selected / item)
  (start_list key 3)
  (mapcar 'ADD_LIST options)
  (end_list)
  (foreach
     item (if (listp selected)
            selected
            (list selected)
          )
    (if (member item options)
      (set_tile
        key
        (itoa (- (length options) (length (member item options))))
      )
    )
  )
)
(defun hcnm_config_dcl_list_callback (key value /)
  (hcnm_config_temp_setvar
    key
    (car (nth (read value) (cadr (assoc key (hcnm_options_list_data)))))
  )
)

;; Shows reactor, its data, and XDATA of a selected bubble note
(defun hcnm_dsbr ()
  (hcnm_debug_show_bubble_reactor_xdata)
  (princ)
)
(defun hcnm_debug_show_bubble_reactor_xdata (/ en reactors reactor data handle_bubble reactor_count hcnm_reactor)
  (vl-load-com)
  (princ "\nSelect a bubble note: ")
  (setq en (car (entsel)))
  (if en
    (progn
      (setq handle_bubble (cdr (assoc 5 (entget en)))
            reactors (cdar (vlr-reactors :vlr-object-reactor))
            reactor_count 0
            hcnm_reactor nil)
      ;; Find THE ONE HCNM reactor (there should be only one)
      (foreach reactor reactors
        (setq data (vlr-data reactor))
        (if (and (listp data) (assoc "HCNM-BUBBLE" data))
          (progn
            (setq reactor_count (1+ reactor_count)
                  hcnm_reactor reactor)
          )
        )
      )
      (alert 
        (princ
          (cond
            ((= reactor_count 0)
             "ERROR: No HCNM-BUBBLE reactor found!")
            ((> reactor_count 1)
             (strcat "ERROR: Reactor proliferation! Found " (itoa reactor_count) " reactors.\nThere should be only ONE reactor for all bubbles."))
            (t
             (strcat
               "ONE HCNM-BUBBLE Reactor (correct)\n\n"
               "Reactor object:\n" (vl-prin1-to-string hcnm_reactor) "\n\n"
               "Reactor data (nested list for all bubbles):\n" (vl-prin1-to-string (vlr-data hcnm_reactor)) "\n\n"
               "Reactor owners count: " (itoa (length (vlr-owners hcnm_reactor))) "\n\n"
               "Selected bubble handle: " handle_bubble "\n\n"
               "Selected bubble XDATA:\n" (vl-prin1-to-string (assoc -3 (entget en '("HCNM-BUBBLE"))))
             ))
          )
        )
      )
    )
    (alert (princ "No entity selected."))
  )
  (princ)
)

;#endregion

(load "ini-edit")
;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 100 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
