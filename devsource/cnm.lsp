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
(defun hcnm-getphaselistfromtblqty (/ el en i dsctag noteqtyondisk oldtags
                                phasealias phaselist
                               )
  ;;Check for phasing in qty table block.  Phasing is controlled by presence of TBLQTY? attributes.
  ;;Construct phaselist as '((phase1 1 alias1)(phase2 2 alias2)(phasei i aliasi)).
  ;;Alias=phase for changing later by CNM.INI.
  ;;Phases are numbered in order they appear in block. (This number could be very unstable, but it is the key to phase order on this sheet.)
  ;;
  ;;Insert table line NOTEQTY block if not exist
  (setq
    j (if (= (hcnm-config-getvar "InsertTablePhases") "No")
        ""
        (hcnm-config-getvar "InsertTablePhases")
      )
  )
  (cond
    ((not (tblsearch "BLOCK" "NOTEQTY"))
     (setvar "attreq" 0)
     (vl-cmdf
       "._insert"
       (strcat "NOTEQTY=NOTEQTY" j)
       "_Scale"
       "1"
       "_Rotate"
       "0"
       "0,0"
     )
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
                (hcnm-config-getvar (cadr phase))
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
(defun hcnm-key-table-searchandsave (dn projnotes / aliaslist at attributes
                                 av blki blkss count ctabonly el en et i
                                 j mvport mvsset n nfname notefnd notei
                                 notelines notelist notenum notephase
                                 noteqty notetxt notetype notnum nottyp
                                 phase phaselist qtyopt skippedphases
                                 usrvar vplayers x
                                )
  ;;
  ;; Section 1.  Make an empty NOTELIST from tblqty and constnot.txt.  TGHI can use this section for Tally, except there is a conflict in the way they do PHASELIST.
  ;;
  (setq
    phaselist
     (hcnm-getphaselistfromtblqty)
    ctabonly
     (= "1" (hcnm-config-getvar "DoCurrentTabOnly"))
    nottyp ""
  )
  (foreach
     entry *hcnm-cnmprojectnotes*
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
  ]
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
      attributes
       (hcnm-get-attributes en nil)
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
          ((= (type noteqty) 'str) (prin1 noteqty f2))
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
(defun hcnm-key-table-make (nfsource qtypt qtyset dn txtht / ctabonly icol
                        iphase column-height note-first-line-p
                        column-height-pending nfname notdsc notelist
                        notesmaxheight notetitles notnum notqty nottyp
                        notunt numfnd phaselist prompteachcol qty qtypt1
                        rdlin txthttemp typfnd usrvar
                       )
  (setq phaselist (hcnm-getphaselistfromtblqty))
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
  (setq ctabonly (= (hcnm-config-getvar "DoCurrentTabOnly") "1"))
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
  (if (/= (type notelist) 'list)
    (alert
      (strcat
        "\nThe file"
        nfname
        "appears to be out of date.\nIt doesn't have valid information to make a notes table.\n\nPlease search and save notes again."
      )
    )
  )
  ;;All prompts done.  Let's make table!
  (hcnm-projinit)                       ;Initialize project after user pauses
  (hcnm-readcf (hcnm-projnotes))
  (setq
    linspc
     (atof (hcnm-config-getvar "LineSpacing"))
    notspc
     (atof (hcnm-config-getvar "NoteSpacing"))
    tblwid
     (atof (hcnm-config-getvar "TableWidth"))
    phasewid
     (atof (hcnm-config-getvar "PhaseWidthAdd"))
    icol 1
    column-height 0
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
     entry *hcnm-cnmprojectnotes*
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
         column-height-pending 0
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
                   ((= (type qty) 'str) qty)
                   ((= (type qty) 'real) (rtos qty 2 8))
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
            i-title 0
          )
          (foreach
             notetitle (reverse notetitles)
            (setq txtht (car notetitle))
            (cond
              ((= i-title 0)
               (cond
                 ;; At top, rewind before first title
                 ((= column-height 0)
                  (setq
                    column-height-pending
                     (+ column-height-pending
                        (* -0.5 txtht linspc)
                     )
                  )
                 )
                 ;; Else add a paragraph space before first title
                 (t
                  (setq
                    column-height-pending
                     (+ column-height-pending
                        (* txtht (- notspc linspc))
                     )
                  )
                 )
               )
              )
            )
            ;; Space for each title
            (setq
              column-height-pending
               (+ column-height-pending
                  (* txtht linspc)
               )
              i-title
               (1+ i-title)
            )
          )
          (setq txtht txthttemp)
         )
       )
       ;; Calculate height of note
       (cond
         ;; At top, rewind before note
         ((and (not notetitles) (= column-height 0))
          (setq
            column-height-pending
             (+ column-height-pending
                (* -0.5 txtht linspc)
             )
          )
         )
         ;; Else add a paragraph space before note
         (t
          (setq
            column-height-pending
             (+ column-height-pending
                (* txtht (- notspc linspc))
             )
          )
         )
       )
       (setq
         column-height-pending
          ;; Add note height
          (+ column-height-pending
             (* (cadr notnum) (* txtht linspc))
          )
       )
       ;; Add titles and note
       ;; If titles _and note_ won't fit and column isn't empty, advance to new column
       (cond
         ((and
            (> (+ column-height-pending column-height) notesmaxheight)
                                        ; Won't fit
            (/= column-height 0)        ; Not first note in column
          )
          (hcnm-key_table_advance_column)
         )
       )
       ;; Add any titles
       (cond
         (notetitles
          (setq
            txthttemp txtht
            i-title 0
          )
          (foreach
             notetitle (reverse notetitles)
            (setq txtht (car notetitle))
            ;; If not first note, space appropriately
            (cond
              ((/= column-height 0)     ; Not first note in column
               (cond
                 ;; Add a paragraph space above first title based on its height
                 ((= i-title 0)
                  (hcnm-key-table-advance-down
                    (* 0.5 (- notspc linspc))
                  )
                 )
               )
               (hcnm-key-table-advance-down (* 0.5 linspc))
              )
            )
            (cond
              ((= (hcnm-config-getvar "ShowKeyTableTitleShapes") "1")
               (hcnm-key-table-insert-shape)
              )
            )
            (setq notdsc (cadr notetitle))
            (hcnm-key-table-insert-text)
            (hcnm-key-table-advance-down (* 0.5 linspc))
          )
          (hcnm-key-table-advance-down (* 0.5 (- notspc linspc)))
          (setq txtht txthttemp)
         )
       )
       ;; If note won't fit in new column with titles, advance column again.
       (cond
         (;; Titles were added
          (/= column-height 0)
          (cond
            (;; Note won't fit after titles.
             (> (+ column-height-pending column-height) notesmaxheight)
             (hcnm-key_table_advance_column)
            )
            (;; Note will fit after titles.
             t
             ;; Paragraph spacing
             (hcnm-key-table-advance-down (* 0.5 (- notspc linspc)))
             ;; Down to middle of first note
             (hcnm-key-table-advance-down (* 0.5 linspc))
            )
          )
         )
       )
       ;; Now add note
       (setq note-first-line-p t)
       (hcnm-key-table-insert-shape)
       (hcnm-key-table-advance-down (* -0.5 linspc))
       (foreach
          notdsc (nth 6 entry)
         (hcnm-key-table-advance-down (* 0.5 linspc))
         (hcnm-key-table-insert-text)
         (hcnm-key-table-advance-down (* 0.5 linspc))
         (setq
           notetitles nil
           note-first-line-p nil
         )
       )
       (hcnm-key-table-advance-down (* 0.5 (- notspc linspc)))
      )
    )
  )
  ;;Apply table display configs from ini.  If no configs (legacy), show both.
  (mapcar
    '(lambda (layerkey / layershow layerlist)
       (setq layershow (/= "0" (hcnm-config-getvar (cadr layerkey))))
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




(defun hcnm-key_table_advance_column ()
  (setq
    column-height 0
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

(defun hcnm-key-table-advance-down (space / down-height)
  (setq
    down-height
     (* space txtht)
    qtypt
     (polar qtypt (* pi -0.5) down-height)
    column-height
     (+ column-height down-height)
    column-height-pending
     (- column-height-pending down-height)
  )
)

(defun hcnm-key-table-insert-shape ()
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

(defun hcnm-key-table-insert-text ()
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
    (if note-first-line-p
      nottyp
      ""
    )
    (if note-first-line-p
      (car notnum)
      ""
    )
    notdsc
  )
  (foreach
     x notqty
    (vl-cmdf
      (if note-first-line-p
        x
        ""
      )
    )
  )
  (vl-cmdf
    (if note-first-line-p
      notunt
      ""
    )
  )
)

;;hcnm-key-table-from-search
;;In the NOTES strategy, this routine is first of three main routines.
;;Gets project info from CONSTNOT.TXT
;;Gets drawing info from bubbles or table.
;;Saves all in .NOT file for other two routines
(defun hcnm-key-table-from-search (dn projnotes txtht linspc tblwid phasewid
                               / el en i notelist qtypt qtyset
                               tablespace
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
              (c:haws-icad-p)           ;If we are in intellicad, which doesn't have the tab information in the entget data
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
  (hcnm-key-table-searchandsave dn projnotes)
  ;;Make a new notes table
  (hcnm-key-table-make "E" qtypt qtyset dn txtht)
)
;#endregion
;#region Table from import
;;hcnm-IMPORT
;;In the NOTES strategy, this routine is second of three main routines.
;;Reads from .NOT file, created by hcnm-key-table-from-search, everything necessary and creates a table. 
(defun hcnm-import (dn projnotes txtht linspc tblwid phasewid / el en i
                qtypt qtyset tablespace
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
              (c:haws-icad-p)           ;If we are in intellicad, which doesn't have the tab information in the entget data
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
  (hcnm-key-table-make "I" qtypt qtyset dn txtht)
)
;#endregion
;#region Tally
;;hcnm-TALLY
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
(defun hcnm-tally (dn projnotes txtht linspc tblwid phasewid / allnot
               all-sheets-quantities col1x column dqwid el flspec i
               input ndwid notdesc notetitles note-first-line-p notnum
               notprice notqty notspc nottyp notunt numfnd numlist
               pgp-defines-run pgp-filename pgp-file-contents
               pgp-file-line phase phasenumi phases-definition pt1z q
               qqwid qtypt1 qtyset quwid row1y sheet-filename
               sheet-filenames sheet-file-name sheet-headings
               sheet-list-filename sheet-list-line sheet-quantities
               tablespace total txthttemp usrvar writelist x y z
              )
;;;
;;;  Section 1.
;;;  Determine list of drawings to tally.
;;;
  (cond
    ((and
       (or (setq sheet-list-filename (findfile (strcat dn ".lst")))
           (setq
             sheet-list-filename
              (findfile
                (strcat
                  (getvar "DWGPREFIX")
                  "tally.lst"
                )
              )
           )
       )
       (= "Yes"
          (progn
            (initget 1 "Yes No")
            (getkword
              (strcat
                "\nKeep and use existing list file, \""
                sheet-list-filename
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
        (setq
          sheet-list-filename
           (getfiled "Select a List File" dn "LST" 0)
        )
       )
       ((= input "Wildcards")
        ;;Add function to user's ACAD.PGP to shell and wait for attrib command to finish.
        (setq
          sheet-list-filename
           (strcat dn ".lst")
          pgp-filename
           (findfile "acad.pgp")
          f1 (open pgp-filename "r")
        )
        (while (setq pgp-file-line (read-line f1))
          (if (= "RUN," (substr pgp-file-line 1 4))
            (setq pgp-defines-run t)
          )
          (if (= "SH," (substr pgp-file-line 1 3))
            (setq
              pgp-file-contents
               (cons
                 "RUN,       cmd /c,         0,*Batch file to run: ,"
                 pgp-file-contents
               )
            )
          )
          (setq
            pgp-file-contents
             (cons pgp-file-line pgp-file-contents)
          )
        )
        (setq f1 (close f1))
        (if (not pgp-defines-run)
          (progn
            (setq
              f1                (open pgp-filename "w")
              pgp-file-contents (reverse pgp-file-contents)
            )
            (foreach
               pgp-file-line pgp-file-contents
              (write-line pgp-file-line f1)
            )
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
            (strcat
              "attrib \"" flspec ".not\" > \"" sheet-list-filename "\""
             )
          )
          (setq
            f1 (open sheet-list-filename "r")
            sheet-filename
             (read-line f1)
            column
             (strlen sheet-filename)
          )
          (cond
            ((wcmatch sheet-filename "* not found *")
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
             (while
               (not
                 (and
                   (wcmatch
                     (strcase (substr sheet-filename column))
                     (strcase (strcat flspec "`.NOT"))
                   )
                   (or (= "\\" (substr sheet-filename (1- column) 1))
                       (= "\\" (substr sheet-filename column 1))
                       (= ":" (substr sheet-filename (1+ column) 1))
                   )
                 )
               )
                (setq column (1- column))
             )
             (setq
               f1              (close f1)
               f1              (open sheet-list-filename "r")
               sheet-filenames nil
             )
             (while (setq sheet-filename (read-line f1))
               (setq
                 sheet-filename
                  (substr sheet-filename column)
                 sheet-filenames
                  (cons
                    (substr
                      sheet-filename
                      1
                      (- (strlen sheet-filename) 4)
                    )
                    sheet-filenames
                  )
               )
             )
             (setq
               f1 (close f1)
               f1 (open sheet-list-filename "w")
             )
             (setq sheet-filenames (reverse sheet-filenames))
             (foreach
                sheet-filename sheet-filenames
               (write-line sheet-filename f1)
             )
             (setq f1 (close f1))
            )
          )
        )
       )
       ((= input "Select")
        (setq
          sheet-list-filename
           (strcat dn ".lst")
          f1 (open sheet-list-filename "w")
        )
        (while (setq
                 sheet-filename
                  (getfiled
                    "File to tally (Cancel when Finished)"
                    ""
                    "NOT"
                    6
                  )
               )
          (write-line
            (substr sheet-filename 1 (- (strlen sheet-filename) 4))
            f1
          )
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
    phases-definition
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
;;;  Read all .NOT's into a master all-sheets-quantities
;;;  Add phases from all .NOTs to the list if not already there.  And if aliases in conflict, alert user.
;;;
  (setq f1 (open sheet-list-filename "r"))
  (princ "\n")
  (while (and
           (setq sheet-list-line (read-line f1))
           (/= "" sheet-list-line)
         )
    ;;Read in this sheet's notelist '( ((alias number phase)) ((type1 (notenum txtlines countmethod qty1...))))
    ;;Alert user of possible incompatibility with old-style list.
    (setq
      sheet-file-name
       (cond
         ((findfile sheet-list-line))
         ((findfile (strcat sheet-list-line ".not")))
         (t
          (alert
            (princ
              (strcat
                "The file \"" sheet-list-line "\" listed in \""
                sheet-list-filename
                "\" cannot be found.\nConstruction Notes Manager cannot continue."
               )
            )
          )
         )
       )
      f2 (open sheet-file-name "r")
      sheet-quantities
       (read (read-line f2))
      all-sheets-quantities
       (cons
         (cons sheet-file-name sheet-quantities)
         all-sheets-quantities
       )
    )
    (if (read-line f2)
      (alert
        (princ
          (strcat
            "Error:  Sheet quantities file for "
            sheet-file-name
            " is out of date.\nPlease search and save quantities again."
          )
        )
      )
    )
    (setq f2 (close f2))
    ;;Set all phases discovered.
    ;;In .NOT files, phases are ("alias" order "number"), but here they are ("number" order "alias")
    (foreach
       phase (car sheet-quantities)
      (cond
        ;;If its alias is not yet in phases-definition, add the phase.
        ;;The reason we substitute instead of just adding the
        ;;new phases as they come is to avoid sorting the list when we're done.
        ((not (caddr (assoc (caddr phase) phases-definition)))
         (setq
           phases-definition
            (subst
              ;;Substitute the alias for the nil.
              (subst
                (car phase)
                nil
                (assoc (caddr phase) phases-definition)
              )
              (assoc (caddr phase) phases-definition)
              phases-definition
            )
         )
        )
        ;;If alias in phases-definition isn't same as alias in this sheet, alert user.
        ((/= (caddr (assoc (car phase) phases-definition))
             (caddr phase)
         )
         (alert
           (princ
             (strcat
               sheet-quantities
               " is trying to assign alias \""
               (caddr phase)
               "\" to phase \""
               (car phase)
               "\", which already has alias \""
               (caddr (assoc (car phase) phases-definition))
               "\".\n\nGrouping alias \""
               (caddr phase)
               "\" on this sheet with phase \""
               (car phase)
               "\", alias \""
               (caddr (assoc (car phase) phases-definition))
               "."
             )
           )
         )
        )
      )
    )
  )
  (setq f1 (close f1))
  ;;Condense list to standard phases-definition format: '((phasej j aliasj)...)
  ;;and renumber for only sheets being tallied.
  (setq i 0)
  (foreach
     phase phases-definition
    (if (caddr phase)
      (setq x (cons (list (car phase) (setq i (1+ i)) (caddr phase)) x))
    )
  )
  (setq phases-definition (reverse x))
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
  (hcnm-projinit)                       ;Initialize project after user pauses
  (hcnm-readcf (hcnm-projnotes))
  (setq
    linspc
     (atof (hcnm-config-getvar "LineSpacing"))
    notspc
     (atof (hcnm-config-getvar "NoteSpacing"))
    tblwid
     (atof (hcnm-config-getvar "TableWidth"))
    phasewid
     (atof (hcnm-config-getvar "PhaseWidthAdd"))
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
     (atof (hcnm-config-getvar "NumberToDescriptionWidth"))
    ;;width from left point of description text to right point of quantity
    dqwid
     (atof (hcnm-config-getvar "DescriptionToQuantityWidth"))
    ;;width from right point of one quantity phase to right point of next quantity phase
    qqwid
     (atof (hcnm-config-getvar "QuantityToQuantityWidth"))
    ;;width from right point of quantity to left point of unit
    quwid
     (atof (hcnm-config-getvar "QuantityToUnitsWidth"))
  )
  (setvar "osmode" 0)
  ;;Write column headings to the file
  (setq f2 (haws-file-open (strcat dn ".csv") "w"))
  (princ "TYPE,NO,ITEM,UNIT,PRICE," f2)
  ;; Price and cost
  (setq sheet-headings "")
  (foreach
     sheet-quantities all-sheets-quantities
    (foreach
       phase phases-definition
      (setq
        sheet-headings
         (strcat
           sheet-headings
           (haws-mkfld
             (strcat
               (strcase (car sheet-quantities))
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
  (princ sheet-headings f2)
  (foreach
     phase phases-definition
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
     entry *hcnm-cnmprojectnotes*
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
                   sheet-quantities all-sheets-quantities
                  (foreach
                     phasei
                     (cdddr
                       (assoc
                         notnum
                         (cdr
                           (assoc nottyp (caddr sheet-quantities))
                         )
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
          (mapcar '(lambda (x) (list 0 0)) phases-definition)
       )
       (foreach
          sheet-quantities all-sheets-quantities
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
                                (cadr sheet-quantities)
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
                                  (caddr sheet-quantities)
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
                 (list total (* total (atof notprice)))
                                        ;Price and cost 2020-12
               )
              phases-definition
            )
         )
       )
       ;;convert quantities and costs to strings, preserving quantities input precision.
       (setq
         notqty                         ; List of qty and price for each phase.
          (mapcar
            '(lambda (phase / qty-string)
               (setq
                 qty-string
                  (rtos
                    (car (nth (1- (cadr phase)) notqty))
                    2
                    8
                  )                     ;Price and cost 2020-12
               )
               (while (wcmatch qty-string "*.*0,*.")
                 (setq
                   qty-string
                    (substr
                      qty-string
                      1
                      (1- (strlen qty-string))
                    )
                 )
               )
               (list
                 qty-string
                 (rtos (cadr (nth (1- (cadr phase)) notqty)) 2 2)
               )                        ;Price and cost 2020-12
             )
            phases-definition
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
         phases-definition
       )
       ;;Write unit to drawing
       (setq x (+ x (* txtht quwid)))
       (if (/= notunt "")
         (haws-mktext "ML" (list x y z) txtht 0 notunt)
       )
       (setq
         x (+ col1x (* txtht ndwid))
         note-first-line-p t
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
         (if (= (type x) 'list)
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
  (haws-core-init 179)
  (hcnm-cnm nil)
  (haws-core-restore)
)
(defun c:hcnm-cnmkt ()
  (haws-core-init 180)
  (prompt (strcat "\n" (haws_cnm_evangel_msg)))
  (hcnm-cnm "Search")
  (haws-core-restore)
)
(defun c:hcnm-cnmkti ()
  (haws-core-init 181)
  (hcnm-cnm "Import")
  (haws-core-restore)
)
(defun c:hcnm-cnmqt ()
  (haws-core-init 338)
  (hcnm-cnm "Tally")
  (haws-core-restore)
)
;;CNM main function
(defun hcnm-cnm (opt / cfname dn linspc phasewid tblwid txtht)
  ;;Main function
  (haws-vsave '("attdia" "attreq" "clayer" "osmode"))
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
  (hcnm-projinit)                       ;Initialize after pauses
  ;;Set user's desired dimstyle.
  (hcnm-set-dimstyle "NotesKeyTableDimstyle")
  (setq
    dn (haws-getdnpath)
    projnotes
     (hcnm-projnotes)
    txtht
     (* (getvar "dimtxt") (haws-dwgscale))
    ;;Column and line spacing widths (half width for middle justified columns)
    ;;line spacing
    linspc
     (atof (hcnm-config-getvar "LineSpacing"))
    ;;width of single sheet table with only one phase
    tblwid
     (atof (hcnm-config-getvar "TableWidth"))
    ;;width for each extra phase on single sheet table.
    phasewid
     (atof (hcnm-config-getvar "PhaseWidthAdd"))
  )
  (hcnm-readcf projnotes)
  (cond
    ((= opt "Search")
     (hcnm-key-table-from-search
       dn projnotes txtht linspc tblwid phasewid
      )
    )
    ((= opt "Import")
     (hcnm-import dn projnotes txtht linspc tblwid phasewid)
    )
    ((= opt "Tally")
     (hcnm-tally dn projnotes txtht linspc tblwid phasewid)
    )
  )
  ;;Restore old dimstyle
  (hcnm-restore-dimstyle)
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
;;hcnm-PROJINIT initializes the CNM project variables
;;because there is good reason to believe they need to
;;be checked again (a pause for user input or a new user command)
;;All the functions assume if they are present they are valid.
;;
(defun hcnm-projinit ()
  (setq
    *hcnm-config* nil
    *hcnm-cnmprojectroot* nil
    *hcnm-cnmprojectnotes* nil
  )
)


;;Does nothing but strcat, since the existence of the file
;;is validated by (hcnm-PROJ)
(defun hcnm-ini-name (proj)
  (hcnm-project-folder-to-ini proj)
)

;; hcnm-PROJ gets a valid project root folder for this drawing's folder.
;; While it returns the folder only, that folder is qualified to have CNM.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\projroot" path to other functions.
(defun hcnm-proj (/ dwgdir linked-project-folder linked-project-marker
              local-project-folder local-project-marker
             )
  (setq
    dwgdir
     (haws-filename-directory (getvar "dwgprefix"))
    local-project-marker
     (hcnm-local-project-marker dwgdir)
    linked-project-marker
     (hcnm-linked-project-marker dwgdir)
  )
  (cond
    (local-project-marker
     (setq
       local-project-folder
        (hcnm-assure-local-project
          local-project-marker
        )
     )
    )
  )
  (cond
    (linked-project-marker
     (setq
       linked-project-folder
        (hcnm-assure-linked-project
          linked-project-marker
        )
     )
    )
  )
  (setq
    *hcnm-cnmprojectroot*
     (cond
       ;;If project is already defined this session, use it.
       ;;(Assume it's valid.  Calling function should init project if there's been a chance of change or loss by user.)
       (*hcnm-cnmprojectroot*)
       ((and
          local-project-marker
          linked-project-marker
        )
        (hcnm-error-ambiguous-project-markers
          local-project-folder
          linked-project-folder
        )
       )
       ;;Else well-formed simple (single-folder) projects. CNM.INI is here.
       (local-project-marker
        (hcnm-assure-local-project
          local-project-marker
        )
       )
       ;;Well-formed complex (multi-folder) projects.  CNMPROJ.TXT is here and
       ;;we'll make sure it really points to a CNM.INI.
       (linked-project-marker
        (hcnm-assure-linked-project
          linked-project-marker
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
        (hcnm-initialize-project dwgdir)
        dwgdir
       )
     )
  )
)

(defun hcnm-local-project-marker (dwgdir)
  (findfile (hcnm-project-folder-to-ini dwgdir))
)

(defun hcnm-linked-project-marker (dwgdir)
  (findfile (hcnm-project-folder-to-link dwgdir))
)

(defun hcnm-error-not-writeable ()
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

(defun hcnm-error-ambiguous-project-markers
   (local-project-folder linked-project-folder)
  (alert
    (princ
      (strcat
        "Error:\nThis drawing's folder\n" local-project-folder
        "\nhas both its own project settings (CNM.ini) and a link (in CNMPROJ.TXT) to a project in another folder:\n"
        linked-project-folder
        "\n\nCNM cannot continue. File names will be printed to the command history for your use resolving the ambiguity."
       )
    )
  )
  (princ
    (strcat
      "\nLocal project: "
      (hcnm-project-folder-to-ini local-project-folder)
    )
  )
  (princ
    (strcat
      "\nLink to another project: "
      (hcnm-project-folder-to-link local-project-folder)
    )
  )
  (exit)
)


(defun hcnm-assure-local-project (local-marker-file)
  (hcnm-check-moved-project local-marker-file)
  (haws-filename-directory local-marker-file)
)

(defun hcnm-assure-linked-project (link-marker / projroot rdlin)
  (cond
    ((and
       (setq f1 (open link-marker "r"))
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
  (if (not (findfile (hcnm-ini-name projroot)))
    (hcnm-initialize-project projroot)
  )
  (hcnm-check-moved-project
    (hcnm-project-folder-to-ini projroot)
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

(defun hcnm-check-moved-project
   (project-file-name / input1 pnname thisfile-value)
  (cond
    ((and
       (setq
         thisfile-value
          (ini_readentry
            project-file-name
            "CNM"
            "ThisFile"
          )
       )
       (setq
         pnname
          (ini_readentry project-file-name "CNM" "ProjectNotes")
       )
       (/= thisfile-value "")
       (/= thisfile-value project-file-name)
     )
     (alert
       (princ
         (strcat
           "Warning!\nYou are using these project notes:\n\n" pnname
           "\n\nand the CNM.ini for this folder says \n\"ThisFile=\""
           thisfile-value
           "\n\nIt appears it may have been copied from another project."
           "\nYou may be about to edit the wrong Project Notes file."
          )
       )
     )
     (initget "Yes No")
     (setq input1 (getkword "\nContinue with this file? [Yes/No]: "))
     (cond
       ((= input1 "Yes")
        (ini_writeentry project-file-name "CNM" "ThisFile" "")
       )
       (t (exit))
     )
    )
  )
)

(defun hcnm-project-ini-name () "cnm.ini")
(defun hcnm-project-link-name () "cnmproj.txt")

(defun hcnm-project-folder-to-ini (project-folder)
  (strcat project-folder "\\" (hcnm-project-ini-name))
)
(defun hcnm-project-folder-to-link (project-folder)
  (strcat project-folder "\\" (hcnm-project-link-name))
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
       shlobj 'browseforfolder 0 prmstr ioptns deffld
      )
  )
  (vlax-release-object shlobj)
  (if folder
    (progn
      (setq
        fldobj
         (vlax-get-property folder 'self)
        outval
         (vlax-get-property fldobj 'path)
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
  (haws-core-init 183)
  (hcnm-linkproj nil)
  (haws-core-restore)
  (princ)
)

;; Sets the CNM project to the given folder. Includes wizards, alerts, and error checks.
(defun hcnm-linkproj (proj / dwgdir localproj localprojbak oldlink)
  (setq
    dwgdir
     (haws-filename-directory (getvar "dwgprefix"))
    *hcnm-cnmprojectroot*
     (cond
       (*hcnm-cnmprojectroot*)
       (dwgdir)
     )
  )
  (cond
    ((not proj)
     (setq proj (hcnm-browseproj *hcnm-cnmprojectroot*))
    )
  )
  (cond
    (proj
     (setq *hcnm-cnmprojectroot* proj)
     (cond
       ((= proj dwgdir)
        (cond
          ((setq oldlink (findfile (hcnm-project-folder-to-link proj)))
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
               *hcnm-cnmprojectroot*
               "\nnot changed."
             )
           )
          )
        )
       )
       (proj
        (hcnm-makeprojtxt proj dwgdir)
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
              (findfile (hcnm-project-folder-to-ini dwgdir))
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
    (*hcnm-cnmprojectroot*
     (alert
       (strcat
         "Project Folder\n"
         *hcnm-cnmprojectroot*
         "\nnot changed."
       )
     )
    )
  )
)

(defun hcnm-browseproj (oldproj)
  (cond
    ((haws-vlisp-p)
     (ale_browseforfolder (hcnm-shorten-path oldproj 50) 48 "")
    )
    (t
     (haws-filename-directory
       (getfiled "Select any file in Project Folder" "" "" 0)
     )
    )
  )
)

(defun hcnm-shorten-path (path nshort)
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
(defun hcnm-makeprojtxt (projdir dwgdir)
  (setq f2 (open (hcnm-project-folder-to-link dwgdir) "w"))
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
  (hcnm-concept-testsetvar
    (getstring "\nVariable name: ")
    (getstring "\nValue: ")
  )
  (haws-core-restore)
)
(defun c:testget ()
  (haws-core-init 185)
  (hcnm-concept-testgetvar (getstring "\nVariable name: "))
  (haws-core-restore)
)

;;==============================================================================
;; C:PRETEST - Clean up before testing bubble notes
;;==============================================================================
;; Removes all reactors and deletes all bubbles with HCNM-BUBBLE XDATA
;; Usage: Type PRETEST at command line before testing
(defun c:pretest (/ ename ename-next ename-leader xdata count-ms count-ps
              count-ldr reactors elist is-bubble
             )
  (princ "\n=== PRE-TEST CLEANUP ===")
  (setq
    count-ms 0
    count-ps 0
    count-ldr 0
  )
  ;; Step 1: Make all reactors transient (so they won't be saved with drawing)
  (princ "\nMaking all object reactors transient...")
  (setq reactors (cdar (vlr-reactors :vlr-object-reactor)))
  (cond
    (reactors
     (foreach reactor reactors (vlr-pers-release reactor) (princ "."))
     (princ
       (strcat "\nMade " (itoa (length reactors)) " reactor(s) transient")
     )
    )
    (t (princ "\nNo reactors found"))
  )
  ;; Step 2: Erase all bubbles with HCNM-BUBBLE XDATA or matching block names
  (princ "\nScanning for bubbles...")
  (setq ename (entnext))
  (while ename
    (setq ename-next (entnext ename))   ; Get next before potential deletion
    ;; Check if entity has HCNM-BUBBLE XDATA or is a bubble block
    (setq 
      xdata (assoc -3 (entget ename '("HCNM-BUBBLE")))
      elist (entget ename)
      is-bubble (and
                  (= (cdr (assoc 0 elist)) "INSERT")
                  (wcmatch (cdr (assoc 2 elist)) "cnm-bubble-*")
                )
    )
    (cond
      ((or xdata is-bubble)
       ;; Found a bubble - determine if it's in model or paper space
       (cond
         ((hcnm-ldrblk-is-on-model-tab ename)
          (setq count-ms (1+ count-ms))
         )
         (t (setq count-ps (1+ count-ps)))
       )
       ;; Find and delete the associated leader first
       (setq ename-leader (hcnm-ldrblk-bubble-leader ename))
       (cond
         (ename-leader
          (entdel ename-leader)
          (setq count-ldr (1+ count-ldr))
         )
       )
       ;; Erase the bubble
       (entdel ename)
       (princ ".")
      )
    )
    (setq ename ename-next)
  )
  ;; Step 3: Report results
  (princ
    (strcat
      "\n\nDeleted "
      (itoa count-ms)
      " bubble(s) from Model Space"
    )
  )
  (princ
    (strcat
      "\nDeleted "
      (itoa count-ps)
      " bubble(s) from Paper Space"
    )
  )
  (princ (strcat "\nDeleted " (itoa count-ldr) " leader(s)"))
  (princ
    (strcat
      "\nTotal: "
      (itoa (+ count-ms count-ps))
      " bubble(s) deleted"
    )
  )
  (princ "\n\n=== CLEANUP COMPLETE ===")
  (princ)
)

(defun hcnm-concept-testsetvar (var val)
  (hcnm-concept-setvar
    ;; variable
    var
    ;;value
    val
    ;; application name for its section in *hcnm-concept-settings*
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
;;to call hcnm-concept-getvar.
(defun hcnm-concept-testgetvar (var)
  (hcnm-concept-getvar
    ;;variable
    var
    ;; application name for its section in *hcnm-concept-settings*
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

;; hcnm-concept-ini
;; Finds INI file
;; Returns a fully qualified path, that folder is qualified to have
;; HAWSEDC.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\INIFOLDER" path to other functions.


;;; hcnm-concept-inifolder gets a valid INI folder.
;;; This function is wrong because there isn't a single *hcnm-concept-inifolder* that this function
;;; can throw around globally.
(defun hcnm-concept-inifile (app scope testfile / inifile assumedinifolder
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
         (cdr
           (assoc app (cdr (assoc scope *hcnm-concept-settings*)))
         )
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
          (hcnm-concept-filename-directory
            assumedinifolder
          )
       )
       (setq
         assumedinifile
          (findfile (strcat assumedinifolder app ".ini"))
       )
     )
     assumedinifile
    )
    ;;Or make an ini file in folder with testfile in it or in proj folder
    ((hcnm-concept-getinidefaults assumedinifolder)
     assumedinifolder
    )
  )
)


;;Gets all settings from an inifile if it can.
(defun hcnm-concept-getsettings (inifile testfile apporproj)
  (setq
    *hcnm-concept-settings*
     (ini_readini
       (hcnm-concept-ini
         (hcnm-concept-inifolder
           inifile
           testfile
         )
       )
     )
  )
)

;;Sets a variable in the global lisp list and in HAWSEDC.INI
(defun hcnm-concept-setvar (inifile inisection var val / setting)
  ;; Call GETVAR before setting var.  Why?  To populate *hcnm-concept-settings*?
  (hcnm-concept-getvar
    var inisection inifile testfile apporproj defaults
   )
  (hcnm-concept-addvartolist var val inisection inifile)
  (ini_writeentry
    (hcnm-concept-ini (hcnm-concept-inifolder))
    inisection
    setting
    val
  )
)

;; hcnm-concept-getvar
;; hcnm-concept-getvar is called by wrapper functions like hcnm-GETVAR or hcnm-concept-edcgetvar
;; It gets a variable without opening a file if it can.
;; (Higher calling functions may use functions like hcnm-PROJINIT or
;; hcnm-concept-removesettings to remove settings and force a file
;; read.)
;; hcnm-concept-getvar gets a program setting from
;; 1. The global *hcnm-concept-settings* list if found
;; 2. An ini file or other location
;; 3. reverts to a default value without fail
;;
;; INIFILE is an ini filename. Ini file might not be used for a given scope in the current strategy.  
;; If there is no such ini file found and is needed, hcnm-concept-getvar creates it.
;; If hcnm-concept-getvar can't create the file, it sends an alert.
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
(defun hcnm-concept-getvar (var sect app scope testfile defaults / addtolist
                        addtoini dir ini setting val
                       )
  (setq
    ;; Does the variable need to be added to the *hcnm-concept-settings* list? Assume yes initially.
    addtolist
     t
    ;; Does the variable need to be added to the appropriate ini file? Assume yes initially
    addtoini
     t
  )
  ;;Get var list if no var list
  (if (not *hcnm-concept-settings*)
    (hcnm-concept-getsettings)
  )
  (cond
    ;;Try getting from list
    ((setq
       val
        (hcnm-concept-vartoval
          var
          (cadr
            (assoc sect (caddr (assoc app *hcnm-concept-settings*)))
          )
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
          (findfile
            (strcat (hcnm-concept-filename-directory dir) "\\" app)
          )
       )
       (setq val (ini_readentry ini sect setting))
     )
    )
    ;;Use default if there is one
    ((setq val (hcnm-concept-vartoval var defaults)))
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
    (hcnm-concept-addvartolist var val sect app)
  )
  (if addtoini
    (ini_writeentry
      (hcnm-concept-ini (hcnm-concept-inifolder))
      app
      setting
      val
    )
  )
  val
)

(defun hcnm-concept-addvartolist (var val inisection inifile)
  (setq
    setting
     (hcnm-concept-vartosetting var)
    *hcnm-concept-settings*
     (subst
       (subst
         (subst
           (list setting val)
           (assoc
             inisetting
             (assoc
               inisection
               (assoc
                 inifile
                 *hcnm-concept-settings*
               )
             )
           )
           (assoc
             inisection
             (assoc
               inifile
               *hcnm-concept-settings*
             )
           )
         )
         (assoc
           inisection
           (assoc file *hcnm-concept-settings*)
         )
         (assoc inifile *hcnm-concept-settings*)
       )
       (assoc inifile *hcnm-concept-settings*)
       *hcnm-concept-settings*
     )
  )
)


;;Gets an entire ini file from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(defun hcnm-concept-getinidefaults (proj / app appini projini)
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
       (setq app (hcnm-config-getvar "AppFolder"))
       (setq
         appini
          (findfile
            (strcat
              (hcnm-concept-filename-directory app)
              "\\"
              "cnm.ini"
            )
          )
       )
       (hcnm-concept-file-copy appini projini)
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

;;Saves *hcnm-concept-settings* to the requested inifile
(defun hcnm-concept-savesettingstoini (inifile testfile apporproj)
  (ini_writesection
    (hcnm-concept-ini (hcnm-concept-inifolder inifile testfile))
    inifile
    *hcnm-concept-settings*
  )
)
;;;================================================================================================================
;;; End multi-app planning thoughts and code
;;;================================================================================================================
;;; CONFIG: Name of the app
;;; DEFINITIONS: Name of an entry in the def
;;; scope-key: eg. "User"
;;; scope-code: eg. 4
;;; ENTRY: An entry in the var list
;;; VAR: The string name of a var
;;; VAL: The string value of a var
;;;
(defun hcnm-config-definitions (/)
  (list
    (list "ProjectFolder" "" 1)
    (list
      "AppFolder"
      (haws-filename-directory (findfile "cnm.mnl"))
      0
    )
    (list "LXXListMode" "yes" 4)
    (list "CNMAliasActivation" "0" 4)
    (list "ProjectNotesEditor" "csv" 2) ; text, csv, or cnm
    (list "LayersEditor" "notepad" 4)   ; notepad or cnm
    (list "ProjectNotes" "constnot.csv" 2)
    (list "ThisFile" "" 2)
    (list "ImportLayerSettings" "No" 2)
    (list
      "NoteTypes"
      "BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI"
      2
    )
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
    (list "BlockReactors" "0" 0)  ; "0"=normal, "1"=block (at arrowhead style change and nested callbacks)
    (list "BubbleArrowIntegralPending" "0" 0)
  )
)

;; Strips scope stuff and returns just defaults list
(defun hcnm-config-defaults ()
  (cond
    (*hcnm-config-defaults*)
    ((mapcar
       '(lambda (var) (hcnm-config-entry-strip-scope var))
       (hcnm-config-definitions)
     )
    )
  )
)

(defun hcnm-config-entry-strip-scope (entry)
  (reverse (cdr (reverse entry)))
)

(defun hcnm-config-defaults-single-scope
   (scope-key / scope-code scope-list)
  (setq scope-code (hcnm-config-scope-code scope-key))
  (foreach
     entry (cdr (assoc "Var" (hcnm-config-definitions)))
    (cond
      ((= (hcnm-config-entry-scope-code entry) scope-code)
       (setq
         scope-list
          (cons
            (hcnm-config-entry-strip-scope entry)
            scope-list
          )
       )
      )
    )
  )
  (reverse scope-list)
)

(defun hcnm-config-scope-code (scope-key)
  (cadr
    (assoc
      scope-key
      (cdr (assoc "Scope" (hcnm-config-definitions)))
    )
  )
)

(defun hcnm-config-scope-eq (var scope-key)
  (= (hcnm-config-entry-scope-code
       (assoc var (cdr (assoc "Var" (hcnm-config-definitions))))
     )
     (hcnm-config-scope-code scope-key)
  )
)

(defun hcnm-config-entry-var (entry) (car entry))

(defun hcnm-config-entry-val (entry) (cadr entry))

(defun hcnm-config-entry-scope-code (entry) (caddr entry))

(defun hcnm-config-get-default (var)
  (hcnm-config-entry-val (assoc var (hcnm-config-defaults)))
)

(defun hcnm-config-read-all (/ ini_configs)
  (append
    (hcnm-config-read-all-user)
    (hcnm-config-read-all-project)
  )
)
(defun hcnm-config-read-all-user (/)
  ;; This function doesn't need to setq an ini_configs since it does hcnm-config-read-user var by var.
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm-config-entry-var entry)
         val (hcnm-config-read-user var)
       )
       (list var val)
     )
    (hcnm-config-defaults-single-scope "User")
  )
)

(defun hcnm-config-read-all-project (/ ini_configs)
  (setq ini_configs (ini_readsection (hcnm-ini-name (hcnm-proj)) "CNM"))
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm-config-entry-var entry)
         val (hcnm-config-entry-val (assoc var ini_configs))
       )
       (list var val)
     )
    (hcnm-config-defaults-single-scope "Project")
  )
)

(defun hcnm-config-read-all-session (/ ini_configs)
  ;; Maybe this function can do what hcnm-config-read-all-user does; it doesn't need to setq an ini_configs since it does hcnm-config-read-user var by var.
  (setq ini_configs *hcnm-config-session*)
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm-config-entry-var entry)
         val (hcnm-config-entry-val (assoc var ini_configs))
       )
       (list var val)
     )
    (hcnm-config-defaults-single-scope "Session")
  )
)

;;;Sets a variable in a temporary global lisp list
(defun hcnm-config-temp-setvar (var val)
  (cond
    ((assoc var *hcnm-config-temp*)
     (setq
       *hcnm-config-temp*
        (subst
          (list var val)
          (assoc var *hcnm-config-temp*)
          *hcnm-config-temp*
        )
     )
    )
    (t
     (setq *hcnm-config-temp* (cons (list var val) *hcnm-config-temp*))
    )
  )
)

;;;Gets a variable in a temporary global lisp list
;;;If it's not present there, gets real value.
(defun hcnm-config-temp-getvar (var)
  (cond
    ((cadr (assoc var *hcnm-config-temp*)))
    (t (hcnm-config-getvar var))
  )
)


;;;Saves the global list of temporary configs in the real global lisp list and in CNM.INI
(defun hcnm-config-temp-save ()
  (foreach
     entry *hcnm-config-temp*
    (hcnm-config-setvar
      (hcnm-config-entry-var entry)
      (hcnm-config-entry-val entry)
    )
  )
)

;;;Saves the global list of temporary configs in the real global lisp list and in CNM.INI
(defun hcnm-config-temp-clear ()
  (setq *hcnm-config-temp* nil)
)

;;;Sets a variable in the global lisp list and in CNM.INI
;;; UPDATED: Now uses HAWS-CONFIG library (Issue #11)
(defun hcnm-config-setvar (var val /)
  ;; Call haws-config with appropriate parameters (scope auto-looked-up)
  (haws-config-setvar
    "CNM"                               ; app
    var                                 ; var
    val                                 ; val
    (hcnm-ini-name (hcnm-proj))         ; ini-path for Project scope
    "CNM"                               ; section for Project scope
  )
)


;;; hcnm-config-getvar  
;;; Var is case sensitive
;;; UPDATED: Now uses HAWS-CONFIG library (Issue #11)
(defun hcnm-config-getvar (var / val start)
  ;; PROFILING: Start timing CNM config wrapper
  (setq start (haws-profile-start "cnm-config-getvar-wrapper"))
  ;; Call haws-config with appropriate parameters (scope auto-looked-up)
  (setq
    val
     (haws-config-getvar
       "CNM"                            ; app
       var                              ; var
       (hcnm-ini-name (hcnm-proj))      ; ini-path for Project scope
       "CNM"                            ; section for Project scope
     )
  )
  ;; PROFILING: End timing CNM config wrapper
  (haws-profile-end "cnm-config-getvar-wrapper" start)
  val
)

(defun hcnm-config-read-user (var /)
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

(defun hcnm-config-write-user (var val)
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

(defun hcnm-config-read-session (var /)
  (cadr (assoc var *hcnm-config-session*))
)

(defun hcnm-config-write-session (var val)
  (setq
    *hcnm-config-session*
     (cond
       ((assoc var *hcnm-config-session*)
        (subst
          (list var val)
          (assoc var *hcnm-config-session*)
          *hcnm-config-session*
        )
       )
       (t
        (cons (list var val) *hcnm-config-session*)
       )
     )
  )
)

;;Gets an entire ini file (per CNM forum) from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(defun hcnm-initialize-project (proj / app appini projini mark-file-p)
  (setq projini (hcnm-project-folder-to-ini proj))
  (cond
    ((and
       (setq app (hcnm-config-getvar "AppFolder"))
       (setq appini (findfile (hcnm-project-folder-to-ini app)))
     )
     (if (not (haws-file-copy appini projini))
       (hcnm-error-not-writeable)
     )
     (alert
       (princ
         (strcat
           "CNM is copying settings found in\n" appini "\nto\n" projini
           "\nfor this project."
          )
       )
     )
     (while (not (findfile projini)))
     (setq mark-file-p t)
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
     (setq *hcnm-config* (hcnm-config-defaults-single-scope "Project"))
     (hcnm-config-write-project proj)
     (setq *hcnm-config* (hcnm-config-defaults))
     (setq mark-file-p t)
     projini
    )
  )
  (cond
    (mark-file-p
     (ini_writeentry projini "CNM" "ThisFile" projini)
    )
  )
)

;;Saves *hcnm-CONFIG* to this project's ini
(defun hcnm-config-write-project (proj)
  (ini_writesection
    (hcnm-ini-name
      (cond
        (proj)
        ((hcnm-proj))
      )
    )
    "CNM"
    *hcnm-config*
  )
)

(defun hcnm-set-dimstyle (key / dsty)
  ;;Set dimstyle as requested by calling function and set by user
  ;;First, get dimstyle name
  (setq dsty (hcnm-config-getvar key))
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
     (setq *hcnm-dimstyleold* (getvar "dimstyle"))
     (vl-cmdf "._dimstyle" "_restore" dsty)
    )
  )
)
(defun hcnm-restore-dimstyle ()
  (cond
    (*hcnm-dimstyleold*
     (vl-cmdf "._dimstyle" "_restore" *hcnm-dimstyleold*)
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

;; hcnm-PROJNOTES gets a valid project notes file
;; It should resolve all errors and user conditions.
;; and return a "drive:\\...\\projroot\\pnname" filename to other functions.
(defun hcnm-projnotes (/ app apppn format opt1 pnname projnotes)
  (setq pnname (hcnm-config-getvar "ProjectNotes"))
  (if (= pnname "")
    (hcnm-config-setvar
      "ProjectNotes"
      (setq pnname "constnot.txt")
    )
  )
  (haws-debug
    (list
      "hcnm-PROJNOTES is beginning with ProjectNotes="
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
            (hcnm-proj)
            "\\"
            (haws-filename-base pnname)
            (haws-filename-extension pnname)
          )
       )
     )
     ;;Record the find in the INI
     (hcnm-config-setvar "ProjectNotes" projnotes)
    )
    ;;Third choice, we couldn't find the Project Notes specified,
    ;;so try to get the appropriate style Project Notes from the app folder
    ;;and put it in the location tried above.
    ;;The CFREAD functions will later evaluate the necessity of changing the file
    ;;format and name.
    ((and
       (setq app (hcnm-config-getvar "AppFolder"))
       (setq
         format
          (hcnm-config-project-notes-format)
         apppn
          (findfile
            (strcat
              app
              "\\"
              (cond
                ((= format "txt2") "constnot-default.txt")
                ((= format "csv") "constnot-default.csv")
                (t
                 (alert
                   (princ
                     "\nUnexpected Project Notes format. CNM cannot continue. Contact developer."
                   )
                 )
                 (exit)
                )
              )
            )
          )
       )
     )
     ;;If CONSTNOT.TXT was found in the app folder,
     ;;try to copy it to this project.
     (haws-file-copy apppn projnotes)
     ;;Record the find in the INI
     (hcnm-config-setvar "ProjectNotes" projnotes)
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

(defun hcnm-getprojnotes (/ dpname oldprojnotes projnotes)
  (hcnm-projinit)                       ;Initialize variables in case any files changed.
  (setq oldprojnotes (hcnm-projnotes))
  (setq dpname (strcat (getvar "dwgprefix") "constnot.txt"))
  (setq
    projnotes
     (getfiled
       "Select Project Notes Filename"
       (hcnm-config-getvar "ProjectNotes")
       ""
       37
     )
  )
  ;;Remove path if project notes is in project folder.
  (cond
    ((and
       projnotes
       (= (haws-filename-directory projnotes) (hcnm-proj))
     )
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

;; hcnm-READCF
;; Reads any acceptable Project Notes file format to a *hcnm-CNMPROJECTNOTES* list of the following format
;; '((0 . "comment")(1 "var" "val1" "val2")(2 . "title")(3 "type" "num" "unit" "count" "text"))
;; The acceptable file formats are:
;; TXT1 Fixed field ;Comment\nSET VAR VAL\nNUM (comment)\nBOX (type)\nTITLE \n1    Text thru column 67...UNTCOUNT\n     Cont. text.
;; TXT2 White space delimited ;Comment\n
;; Excel CSV
;; Doesn't do project management except to write txt2 configs to cnm.ini in the same folder as projnotes.
(defun hcnm-readcf
   (projnotes / bakprojnotes pnformat rdlin requested-format)
  ;;Do a file read to figure out what the file format is.
  ;;For now, assume that a file that has any of the shape keys followed by a comma ("BOX,", etc.) is CSV
  ;;any other file is TXT2
  (haws-debug
    (list
      "hcnm-READCF is deciphering the format of "
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
         (hcnm-config-getvar "NoteTypes")
       )
       (setq pnformat "txt2")
      )
    )
  )
  (setq
    f1 (close f1)
    requested-format
     (hcnm-config-project-notes-format)
  )
  (cond
    ((= pnformat "txt2")
     (hcnm-readcftxt2 projnotes)
     (cond
       ((= requested-format "csv")
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
        (hcnm-writecfcsv projnotes)
       )
     )
    )
    ((= pnformat "csv")
     (hcnm-readcfcsv projnotes)
     (cond
       ((= requested-format "txt2")
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
        (hcnm-writecftxt2 projnotes)
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

(defun hcnm-readcftxt2 (projnotes / alertnote alerttitle cfitem cflist
                    cflist2 commentbegin filev42 iline ininame nottyp
                    rdlin val1 val2 var varlist n notdesc notnum typwc
                   )
  (setq
    typwc
     (hcnm-config-getvar "NoteTypes") ; Get typwc (which may open f1) before opening f1
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
              notdesc
               (nth 6 (nth n cflist))
              cflist
               (cons
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
       (hcnm-config-setvar
         (hcnm-config-entry-var entry)
         (hcnm-config-entry-val entry)
       )
     )
    )
  )
  (setq *hcnm-cnmprojectnotes* (reverse cflist))
  (haws-debug
    (list
      "hcnm-READCFTXT2 read "
      (itoa (length *hcnm-cnmprojectnotes*))
      " lines from "
      projnotes
      "."
    )
  )
  ;;Add comments and version number to old file.
  (cond
    ((not filev42)
     (setq
       *hcnm-cnmprojectnotes*
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


(defun hcnm-readcfcsv (projnotes / cflist notdscstr nottyp rdlin typwc val
                   var wrap
                  )
  (setq
    wrap
     (atoi (hcnm-config-getvar "DescriptionWrap"))
    typwc
     (hcnm-config-getvar "NoteTypes") ; Get typwc (which may open f1) before opening f1
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
       (cond ((= (strcat var) "WRAP") (setq wrap (atoi val))))
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
                 (cond
                   ((haws-rdfld 2 rdlin "," 1))
                   ("")
                 )
                 (cond
                   ((haws-rdfld 4 rdlin "," 1))
                   ("")
                 )
                 (cond
                   ((haws-rdfld 5 rdlin "," 1))
                   ("")
                 )
                 (cond
                   ((haws-rdfld 6 rdlin "," 1))
                   ("")
                 )                      ; Price
                 (hcnm-wrap-description
                   (cond
                     ((haws-rdfld 3 rdlin "," 1))
                     ("")
                   )
                   wrap
                 )
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
  (setq *hcnm-cnmprojectnotes* (reverse cflist))
)

(defun hcnm-wrap-description (notdscstr wrap / character-i i i-endline
                          i-newline-prev i-newword-prev inword-p
                          need-wrap-p notdsclst word-provided-p
                          wrap-exceeded-p
                         )
  (setq
    notdsclst nil
    i-newline-prev 1
    i-newword-prev 1
    inword-p t
    i 0
  )
  (while (<= (setq i (1+ i)) (1+ (strlen notdscstr)))
    (setq
      character-i
       (substr notdscstr i 1)
      wrap-exceeded-p
       (>= (- i i-newline-prev) wrap)
      word-provided-p
       (> i-newword-prev i-newline-prev)
    )
    (cond
      ((or (= character-i "")
           (and wrap-exceeded-p word-provided-p)
       )
       (setq need-wrap-p t)
      )
    )
    (cond
      ((= "\\n" (substr notdscstr i 2))
       (setq
         notdsclst
          (cons
            (list i-newline-prev (- i i-newline-prev))
            notdsclst
          )
         i-newline-prev
          (+ i 2)
         i-newword-prev
          (+ i 2)
         inword-p t
         need-wrap-p nil
       )
      )
      ((wcmatch character-i " ,\t") (setq inword-p nil))
      (t
       (cond
         ((and (/= character-i "") (not inword-p))
          (setq
            i-newword-prev i
            inword-p t
          )
         )
       )
       (cond
         (need-wrap-p
          (setq
            i-newline
             (cond
               ((= character-i "") i)
               (t i-newword-prev)
             )
            notdsclst
             (cons
               (list
                 i-newline-prev
                 (- i-newline i-newline-prev)
               )
               notdsclst
             )
            i-newline-prev i-newline
            need-wrap-p nil
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
(defun hcnm-wrap-description-test ( / errorstring notdscstr wrap)
  (setq
    notdscstr "A23456789 B23456789 C23456789"
    wrap 2
    errorstring "List of assertions violated:"
    errorstring
     (strcat
       errorstring
       (cond
         ((/= (car (hcnm-wrap-description notdscstr wrap))
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
         ((/= (car (hcnm-wrap-description notdscstr wrap))
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
         ((/= (car (hcnm-wrap-description notdscstr wrap))
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
         ((/= (car (hcnm-wrap-description notdscstr wrap))
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
         ((/= (car (hcnm-wrap-description notdscstr wrap))
              "A23456789 "
          )
          "\nMust wrap word 2 to line two if it exceeds by one."
         )
         ("")
       )
     )
  )
  ;;(hcnm-wrap-description NOTDSCSTR WRAP)
  errorstring
)
|;

(defun hcnm-writecftxt2 (projnotes / i item nottyp nottxt nottxtnew)
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
     item *hcnm-cnmprojectnotes*
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
  *hcnm-cnmprojectnotes*
)

(defun hcnm-writecfcsv (projnotes / desc descline item nottyp)
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
     item *hcnm-cnmprojectnotes*
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
  *hcnm-cnmprojectnotes*
)

(defun hcnm-config-project-notes-format (/ editor format valid-editors)
  (setq
    valid-editors
     (list
       (list "text" "txt2")
       (list "csv" "csv")
       (list "cnm" "csv")
     )
    editor
     (hcnm-config-getvar "ProjectNotesEditor")
    format
     (cadr (assoc editor valid-editors))
  )
  (cond
    ((not format)
     (alert
       (princ
         (strcat
           "\nInvalid ProjectNotesEditor. CNM cannot continue.\nUse HCNM-CNMOptions to select your desired editor.\n\nFound ProjectNotesEditor="
           editor
           "\n\nExpected one of these: "
           (apply
             'strcat
             (mapcar '(lambda (x) (strcat "\n" (car x))) valid-editors)
           )
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
(defun c:hcnm-notesedit (/ cnmedit-p noteseditor pnname)
  (setq
    noteseditor
     (hcnm-config-getvar "ProjectNotesEditor")
    ;; wcmatch is legacy hack to be removed when 4.2.29 is deprecated and replaced with translation/conversion on getvar.
    cnmedit-p
     (wcmatch (strcase noteseditor) "*CNM*")
  )
  (if cnmedit-p
    (haws-core-init 335)
    (haws-core-init 188)
  )
  ;; Since this is a user command, possibly after deletion of project root files,
  ;; refresh project root at beginning.
  (hcnm-projinit)
  ;; Read to convert project notes if necessary before editing
  (setq pnname (hcnm-projnotes))
  (hcnm-readcf pnname)
  (setq pnname (hcnm-projnotes-match-extension pnname noteseditor))
  (princ (strcat "\nEditing " (hcnm-projnotes) "."))
  (cond
    (cnmedit-p
     (startapp
       (strcat
         "\""
         (hcnm-config-getvar "AppFolder")
         "\\CNMEdit.exe"
         "\" "
         "\""
         pnname
         "\" "
         "\""
         (hcnm-proj)
         "\\cnm.ini\""
       )
     )
    )
    (t (vl-cmdf "._SH" (strcat "\"" pnname "\"")))
  )
  (haws-core-restore)
  (princ)
)

(defun hcnm-projnotes-match-extension (projnotes noteseditor)
  (cond
    ((= noteseditor "text")
     (hcnm-change-filename-extension projnotes "txt")
    )
    (t (hcnm-change-filename-extension projnotes "csv"))
  )
)

(defun hcnm-change-filename-extension
   (old-filename new-extension / new-filename)
  (cond
    ((/= (haws-filename-extension old-filename) new-extension)
     (setq
       new-filename
        (strcat
          (haws-filename-directory old-filename)
          "\\"
          (haws-filename-base old-filename)
          "."
          new-extension
        )
     )
     (vl-file-rename old-filename new-filename)
     (hcnm-config-setvar "ProjectNotes" new-filename)
    )
  )
  new-filename
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
          (strcase (hcnm-config-getvar "LayersEditor"))
          "*CNM*"
        )
        (strcat
          (hcnm-config-getvar "AppFolder")
          "\\CNMLayer.exe"
        )
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
       f1     (open (hcnm-projnotes) "r")
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
       f1 (open (hcnm-projnotes) "W")
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
    (hcnm-config-setvar
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
  (hcnm-attlayer "NOTESNOPLOT")
  (vl-cmdf
    "._layer"
    "_Plot"
    "_No"
    (haws-getlayr "NOTESNOPLOT")
    ""
  )
  (haws-core-restore)
)
(defun c:hcnm-attplot () (hcnm-attlayer "0"))
(defun hcnm-attlayer (layer / at el en et nplayer nplist sset sslen)
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

(defun haws-ldrblk (blleft blrght bldrag bllay bldsty / apold as associate-p
                ang auold blgf blline blk dsty dstyold dtold el en enblk
                endrag fixhook fixphase fixtxt3 i p1 p2 p3 p4 p5 p6 p7
                p8 pfound r1 ds ts left num txt1 txt2 ang1 ang2 fixorder
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
    associate-p
     (cond
       ((= (getvar "DIMANNO") 1) nil)
       (t)
     )
  )
  (hcnm-projinit)
  (hcnm-set-dimstyle (strcat bldsty "Dimstyle"))
  (setvar "osmode" 0)
  (haws-mklayr bllay)
  (setq p1 (getpoint "\nStart point for leader:"))
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
       (associate-p (vl-cmdf "_block"))
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
  (hcnm-restore-dimstyle)
  (haws-vrstor)
  (vl-cmdf "._undo" "_e")
  (haws-core-restore)
  (princ)
)
;;end of LDRBLK

;#endregion
;#region Bubble insertion and editing
;#region Bubble note insertion commands main and loops
(defun c:haws-boxl ()
  (haws-core-init 198)
  (hcnm-ldrblk-insert "BOX")
)
(defun c:haws-cirl ()
  (haws-core-init 199)
  (hcnm-ldrblk-insert "CIR")
)
(defun c:haws-dial ()
  (haws-core-init 200)
  (hcnm-ldrblk-insert "DIA")
)
(defun c:haws-elll ()
  (haws-core-init 201)
  (hcnm-ldrblk-insert "ELL")
)
(defun c:haws-hexl ()
  (haws-core-init 202)
  (hcnm-ldrblk-insert "HEX")
)
(defun c:haws-octl ()
  (haws-core-init 203)
  (hcnm-ldrblk-insert "OCT")
)
(defun c:haws-penl ()
  (haws-core-init 204)
  (hcnm-ldrblk-insert "PEN")
)
(defun c:haws-recl ()
  (haws-core-init 205)
  (hcnm-ldrblk-insert "REC")
)
(defun c:haws-sstl ()
  (haws-core-init 206)
  (hcnm-ldrblk-insert "SST")
)
(defun c:haws-tril ()
  (haws-core-init 207)
  (hcnm-ldrblk-insert "TRI")
)
(defun c:hcnm-replace-bubble ()
  (haws-core-init 338)
  (hcnm-ldrblk-insert nil)
)

(defun hcnm-ldrblk-insert (notetype / blockname bubble-data bubblehooks
                       ename-bubble-old replace-bubble-p th
                       profile-start
                      )
  ;;===========================================================================
  ;; PROFILING: Start timing bubble insertion (complete process)
  ;;===========================================================================
  (setq profile-start (haws-profile-start "insert-bubble"))
  (princ "\nCNM version: ")
  (princ (haws-unified-version))
  (haws-tip
    1
    "\nIn some AutoCAD installations, CNM bubble insertion crashes the first time in each drawing session, possibly when it's the first command or the first block insertion. Please let us know if you can confirm a pattern."
  )
  (haws-vsave '("attreq" "aunits" "clayer" "cmdecho"))
  (cond
    ((and (getvar "wipeoutframe") (/= (getvar "wipeoutframe") 2))
     (alert
       (princ "\nSetting WIPEOUTFRAME to 2 to show but not plot")
     )
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
  (hcnm-projinit)
  (hcnm-set-dimstyle "NotesLeaderDimstyle")
  (setq
    bubblehooks
     (hcnm-config-getvar "BubbleHooks")
    blockname
     (strcat
       "cnm-bubble-"
       (hcnm-ldrblk-get-mtext-string)
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
  (setq
    bubble-data
     (hcnm-ldrblk-bubble-data-set bubble-data "TH" th)
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "BLOCKNAME"
       blockname
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "NOTETYPE"
       notetype
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "replace-bubble-p"
       (not notetype)
     )
    bubble-data
     (hcnm-ldrblk-get-ename-bubble-old bubble-data)
    bubble-data
     (cond
       ((hcnm-ldrblk-bubble-data-get
          bubble-data
          "ename-bubble-old"
        )
        (hcnm-ldrblk-bubble-data-ensure-p1-world bubble-data)
                                        ;  We really only need ename-leader-old and p1-ucs, but this isn't a bad way to get it.
       )
       (t (hcnm-ldrblk-get-user-start-point bubble-data))
     )
    notetype
     (cond
       (notetype)
       ;; Otherwise get from old bubble note
       ((hcnm-ldrblk-bubble-data-get bubble-data "ename-bubble-old")
        (lm:getdynpropvalue
          (vlax-ename->vla-object
            (hcnm-ldrblk-bubble-data-get
              bubble-data
              "ename-bubble-old"
            )
          )
          "Shape"
        )
       )
       (t notetype)
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "NOTETYPE"
       notetype
     )
  )
  ;; BLOCK REACTORS during replace-bubble to prevent premature auto-text calculation
  ;; Problem: ._qlattach modifies leader, triggers reactor, but VPTRANS not copied yet
  ;; Solution: Block reactor callbacks until after VPTRANS/XDATA copied in finish-bubble
  (cond
    ((hcnm-ldrblk-bubble-data-get bubble-data "replace-bubble-p")
     (hcnm-config-setvar "BlockReactors" "1")
    )
  )
  ;; Draw bubble, update bubble-data with P2 and new entities
  (setq bubble-data (hcnm-ldrblk-get-p2-data bubble-data))
  (setq bubble-data (hcnm-ldrblk-draw-bubble bubble-data))
  (setq bubble-data (hcnm-ldrblk-get-bubble-data bubble-data))
  (hcnm-ldrblk-finish-bubble bubble-data)
  (haws-tip 7 "You can align/move bubble note text lines by moving their ATTDEF objects in the block editor (BEDIT). Save the results to a personal or team CNM customizations location so that you can copy in your versions after every CNM install.\n\nNote that there have been no changes to the bubble note block definition since version 5.0.07 (you can always check your version using HAWS-ABOUT).")
  (hcnm-restore-dimstyle)
  (haws-vrstor)
  (vl-cmdf "._undo" "_e")
  
  ;; DEFENSIVE: Reset BlockReactors flag after insertion completes
  ;; Ensures stuck flags don't persist across user actions
  (hcnm-config-setvar "BlockReactors" "0")
  
  (haws-core-restore)
  ;;===========================================================================
  ;; PROFILING: End timing bubble insertion
  ;;===========================================================================
  (haws-profile-end "insert-bubble" profile-start)
  (princ)
)
(defun hcnm-ldrblk-get-user-start-point (bubble-data)
  (hcnm-ldrblk-bubble-data-set
    bubble-data
    "p1-ucs"
    (getpoint "\nStart point for leader:")
  )
)
;; Gets insertion point of bubble in UCS coordinates
;; Bubble still doesn't exist. Draws temp bubbles only.
(defun hcnm-ldrblk-get-p2-data (bubble-data / ename-bubble-temp p1-ucs p2
                            ss1 obj-bubble-temp th blockname notetype
                           )
  (setq
    p1-ucs
     (hcnm-ldrblk-bubble-data-get bubble-data "p1-ucs")
    th (hcnm-ldrblk-bubble-data-get bubble-data "TH")
    blockname
     (hcnm-ldrblk-bubble-data-get bubble-data "BLOCKNAME")
    notetype
     (hcnm-ldrblk-bubble-data-get bubble-data "NOTETYPE")
    ss1
     (ssadd)
  )
  (foreach
     flipstate '("right" "left")
    (vl-cmdf
      "._insert"
      (strcat blockname "-" flipstate)
      "_Scale"
      th
      "_Rotate"
      (angtos (getvar "snapang"))
      p1-ucs
    )
    (setq
      ename-bubble-temp
       (entlast)
      obj-bubble-temp
       (vlax-ename->vla-object ename-bubble-temp)
    )
    (lm:setdynpropvalue obj-bubble-temp "Shape" notetype)
    (ssadd ename-bubble-temp ss1)
  )
  (prompt "\nLocation for bubble: ")
  (vl-cmdf "._MOVE" ss1 "" p1-ucs pause)
  (setq
    p2          (trans
                  (cdr (assoc 10 (entget ename-bubble-temp)))
                  ename-bubble-temp
                  1
                )
    bubble-data (hcnm-ldrblk-bubble-data-set bubble-data "P2" p2)
  )
  (vl-cmdf "._erase" ss1 "")
  bubble-data
)
;; Draw bubble and update bubble-data with new leader/block info
(defun hcnm-ldrblk-draw-bubble (bubble-data / p1-ucs ename-bubble
                            ename-bubble-old ename-leader p2 ang1
                            flipstate associate-p auold th blockname
                            notetype input1 elist-leader-old
                           )
  (setq
    p1-ucs
     (hcnm-ldrblk-bubble-data-get bubble-data "p1-ucs")
    ename-bubble-old
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble-old"
     )
    ename-leader-old
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-leader-old"
     )
    p2 (hcnm-ldrblk-bubble-data-get bubble-data "P2")
    th (hcnm-ldrblk-bubble-data-get bubble-data "TH")
    blockname
     (hcnm-ldrblk-bubble-data-get bubble-data "BLOCKNAME")
    notetype
     (hcnm-ldrblk-bubble-data-get bubble-data "NOTETYPE")
    ang1
     (- (angle p1-ucs p2) (getvar "snapang"))
    flipstate
     (cond
       ((minusp (cos ang1)) "left")
       (t "right")
     )
  )
  (cond
    ;; If it's not a new insertion, don't draw a leader.
    (ename-bubble-old
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
     (setq ename-bubble (entlast))
     ;; Save ename-bubble to bubble-data for replace-bubble path
     (setq
       bubble-data
        (hcnm-ldrblk-bubble-data-set
          bubble-data
          "ename-bubble"
          ename-bubble
        )
     )
     ;; If there is an old leader, stretch it and associate it.
     (cond
       (ename-leader-old
        (setq elist-leader-old (entget ename-leader-old))
        ;; Change its arrowhead if needed.
        (hcnm-ldrblk-change-arrowhead ename-leader-old)
        ;; Stretch it.
        (entmod
          (subst
            (cons 10 p2)
            (assoc
              10
              (cdr
                (member (assoc 10 elist-leader-old) elist-leader-old)
              )
            )
            elist-leader-old
          )
        )
        (vl-cmdf
          "._qldetachset"
          ename-leader-old
          ""
          "._qlattach"
          ename-leader-old
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
     (setq
       associate-p
        (cond
          ((= (getvar "DIMANNO") 1) t)
          (nil)
        )
     )
     (cond
       ((and
          (not associate-p)
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
        (setq
          input1
           (getkword
             "\nSet dimension scale to match annotation scale? [Yes/No]: "
           )
        )
        (cond
          ((= input1 "Yes")
           (setvar "DIMSCALE" (/ 1.0 (getvar "CANNOSCALEVALUE")))
          )
        )
       )
     )
     (setq
       ang1      (- (angle p1-ucs p2) (getvar "snapang"))
       flipstate (cond
                   ((minusp (cos ang1)) "left")
                   (t "right")
                 )
     )
     ;; SAVE LAST ENTITY FOR ENTNEXT USAGE.
     (setq
       bubble-data
        (hcnm-ldrblk-bubble-data-set
          bubble-data
          "ename-last"
          (entlast)
        )
     )
     ;;Start insertion
     (cond
       ((>= (atof (getvar "acadver")) 14)
        (vl-cmdf "._leader" p1-ucs p2 "_Annotation" "")
        (cond
          (associate-p (vl-cmdf "_block"))
          (t (vl-cmdf "_none" "._INSERT"))
        )
       )
       (t
        (alert
          (princ
            "\nThe bubble notes inserter in CNM 4.2.3 and higher is not compatible with AutoCAD pre-R14."
          )
        )
       )
     )
     (setq auold (getvar "aunits"))
     (setvar "aunits" 3)
     (vl-cmdf
       (strcat blockname "-" flipstate)
       "_Scale"
       th
       p2
       (getvar "snapang")
     )
     (setvar "aunits" auold)
     (setq
       ename-bubble
        (entlast)
       bubble-data
        (hcnm-ldrblk-bubble-data-set
          bubble-data
          "ename-bubble"
          ename-bubble
        )
     )
    )
  )
  bubble-data
)
;; Bubble note insertion experience outer loop data prompts.
;; Get input from user. ename-bubble already exists so that we can do auto text.
(defun hcnm-ldrblk-get-bubble-data
   (bubble-data / lattribs ename-bubble p1-ucs num)
  (setq
    replace-bubble-p
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "replace-bubble-p"
     )
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    ename-bubble-old
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble-old"
     )
    p1-ucs
     (hcnm-ldrblk-bubble-data-get bubble-data "p1-ucs")
  )
  (cond
    (replace-bubble-p
     ;; Read attributes from OLD bubble, not the newly drawn one
     (setq lattribs (hcnm-get-attributes ename-bubble-old t))
    )
    (t
     (initget 128 "Copy")
     (setq num (getkword "\nNote number or [Copy note]: "))
     (cond
       ((= num "Copy")
        (setq
          lattribs
           (hcnm-get-attributes
             (setq ename-bubble (car (entsel)))
             t
           )
        )
       )
       (t
        ;; Create empty spec and populate NOTENUM (2-element lattribs)
        (setq
          lattribs
           (hcnm-ldrblk-lattribs-spec)
          lattribs
           (hcnm-ldrblk-lattribs-put-element
             "NOTENUM"
             num
             lattribs
           )
        )
        (mapcar
          '(lambda (index)
             (setq
               lattribs
                (hcnm-ldrblk-get-text-entry
                  ename-bubble
                  index
                  lattribs
                )
             )
           )
          '(1 2 3 4 5 6 0)
        )
       )
     )
    )
  )
  ;; NOTE: XDATA is written by reactor attachment during insertion
  ;; hcnm-ldrblk-xdata-save is ONLY for dialog save path (requires semi-global)
  ;; Validate structure, but don't apply deprecated underover function
  (hcnm-ldrblk-bubble-data-set
    bubble-data
    "ATTRIBUTES"
    (hcnm-ldrblk-lattribs-validate lattribs)
  )
)
(defun hcnm-ldrblk-finish-bubble (bubble-data / ename-bubble
                              ename-bubble-old ename-last ename-leader
                              ename-leader-old ename-temp replace-bubble-p 
                              attributes notetype
                             )
  (setq
    ename-last
     (hcnm-ldrblk-bubble-data-get bubble-data "ename-last")
    ename-temp ename-last
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    ename-bubble-old
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble-old"
     )
    ename-leader-old
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-leader-old"
     )
    replace-bubble-p
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "replace-bubble-p"
     )
    attributes
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
    notetype
     (hcnm-ldrblk-bubble-data-get bubble-data "NOTETYPE")
  )
  (hcnm-ldrblk-set-dynprops
    ename-bubble
    ename-bubble-old
    notetype
    replace-bubble-p
  )
  ;; REPLACE-BUBBLE: Copy XDATA/XRECORD before erasing old bubble
  (cond
    (replace-bubble-p
     ;; Phase 2: Copy VPTRANS XRECORD (viewport transform) if exists
     (hcnm-ldrblk-copy-vptrans ename-bubble-old ename-bubble)
     ;; Phase 3: Copy XDATA (auto-text metadata) if exists
     (hcnm-ldrblk-copy-xdata ename-bubble-old ename-bubble)
     ;; Phase 4: Update reactor data structure (bubble handle old → new)
     (hcnm-ldrblk-reactor-update-bubble-handle ename-bubble-old ename-bubble)
     ;; Erase old bubble
     (entdel ename-bubble-old)
     ;; UNBLOCK REACTORS: Now safe to process auto-text updates
     ;; VPTRANS and XDATA copied, new bubble fully initialized
     (hcnm-config-setvar "BlockReactors" "0")
    )
  )
  (hcnm-ldrblk-lattribs-to-dwg ename-bubble attributes)
  ;; Find or reuse leader
  (cond
    (replace-bubble-p
     ;; Replace-bubble: reuse old leader (already stretched and associated)
     (setq ename-leader ename-leader-old)
    )
    (t
     ;; New insertion: search for leader starting from last entity before bubble
     (while
       (and
         (/= "LEADER"
             (cdr
               (assoc 0 (entget (setq ename-temp (entnext ename-temp))))
             )
         )
       )
     )
     (setq ename-leader ename-temp)
    )
  )
  ;; Change leader arrowhead if needed.
  (hcnm-ldrblk-change-arrowhead ename-leader)
)
;;==============================================================================
;; REPLACE BUBBLE - Helper Functions for Copying XDATA/XRECORD/Reactor Data
;;==============================================================================
;; Get the persistent reactor (or nil if none exists)
;; Returns first reactor with "HCNM-BUBBLE" data
(defun hcnm-ldrblk-get-reactor (/ reactors)
  (setq reactors
    (vl-remove-if-not
      '(lambda (r)
         (and
           (listp (vlr-data r))
           (assoc "HCNM-BUBBLE" (vlr-data r))
         )
       )
      (cdar (vlr-reactors :vlr-object-reactor))
    )
  )
  (cond
    (reactors (car reactors))
    (t nil)
  )
)
;; Copy viewport transform (VPTRANS XRECORD) from old bubble to new bubble
;; Used during replace-bubble operation to preserve paper space coordinate transforms
(defun hcnm-ldrblk-copy-vptrans (ename-old ename-new / vptrans-data)
  (cond
    ((setq vptrans-data (hcnm-xdata-get-vptrans ename-old))
     ;; Old bubble has VPTRANS - copy to new bubble
     (hcnm-xdata-set-vptrans ename-new vptrans-data)
     (haws-debug
       (list
         "[REPLACE] Copied VPTRANS from old bubble to new bubble: "
         (vl-princ-to-string vptrans-data)
       )
     )
     t
    )
    (t
     ;; No VPTRANS on old bubble - nothing to copy
     (haws-debug "[REPLACE] Old bubble has no VPTRANS (legacy or model space)")
     nil
    )
  )
)
;; Copy auto-text metadata (XDATA) from old bubble to new bubble
;; Used during replace-bubble operation to preserve auto-text associations
(defun hcnm-ldrblk-copy-xdata (ename-old ename-new / xdata-alist)
  (cond
    ((setq xdata-alist (hcnm-xdata-get-autotext ename-old))
     ;; Old bubble has auto-text XDATA - copy to new bubble
     (hcnm-xdata-set-autotext ename-new xdata-alist)
     (haws-debug
       (list
         "[REPLACE] Copied XDATA from old bubble to new bubble: "
         (vl-princ-to-string xdata-alist)
       )
     )
     t
    )
    (t
     ;; No XDATA on old bubble - nothing to copy (legacy bubble)
     (haws-debug "[REPLACE] Old bubble has no XDATA (legacy bubble)")
     nil
    )
  )
)
;; Update reactor data structure: replace old bubble handle with new bubble handle
;; Used during replace-bubble operation to maintain auto-text reactivity
;; Leader handle stays the same (stretched, not recreated)
(defun hcnm-ldrblk-reactor-update-bubble-handle (ename-old ename-new / 
                                                 reactor handle-old handle-new
                                                 data key-app owner-list updated-p
                                                )
  ;; Get the persistent reactor
  (setq reactor (hcnm-ldrblk-get-reactor))
  (cond
    ((not reactor)
     ;; No reactor exists - nothing to update (legacy drawing)
     (haws-debug "[REPLACE] No reactor exists - legacy drawing")
     nil
    )
    (t
     ;; Reactor exists - update bubble handles in data structure
     (setq
       handle-old (vla-get-handle (vlax-ename->vla-object ename-old))
       handle-new (vla-get-handle (vlax-ename->vla-object ename-new))
       data (vlr-data reactor)
       key-app "HCNM-BUBBLE"
       owner-list (cadr (assoc key-app data))
       updated-p nil
     )
     (haws-debug
       (list
         "[REPLACE] Updating reactor data: old handle="
         handle-old
         " new handle="
         handle-new
       )
     )
     ;; Walk through owner-list, find all references to old bubble handle
     (setq owner-list
       (mapcar
         '(lambda (owner-entry / owner-handle bubble-list)
            (setq
              owner-handle (car owner-entry)
              bubble-list (cadr owner-entry)
            )
            ;; Check if this owner has the old bubble in its list
            (cond
              ((assoc handle-old bubble-list)
               ;; Found old bubble handle - replace with new
               (setq
                 bubble-list
                  (subst
                    (cons handle-new (cdr (assoc handle-old bubble-list)))
                    (assoc handle-old bubble-list)
                    bubble-list
                  )
                 updated-p t
               )
               (haws-debug
                 (list
                   "[REPLACE] Updated bubble handle under owner: "
                   owner-handle
                 )
               )
              )
            )
            ;; Return possibly-modified owner-entry
            (list owner-handle bubble-list)
          )
         owner-list
       )
     )
     ;; Write updated data back to reactor
     (cond
       (updated-p
        (vlr-data-set reactor (list (list key-app owner-list)))
        (haws-debug "[REPLACE] Reactor data updated successfully")
        t
       )
       (t
        (haws-debug "[REPLACE] Old bubble not found in reactor data (legacy)")
        nil
       )
     )
    )
  )
)
;; Bubble note insertion experience inner loop data prompts.
;; Returns lattribs
(defun hcnm-ldrblk-get-text-entry (ename-bubble line-number lattribs / input
                               skip-entry-p input loop-p prompt-p string
                               tag attr
                              )
  (setq
    loop-p t
    prompt-p
     (= (hcnm-config-getvar
          (strcat "BubbleTextLine" (itoa line-number) "PromptP")
        )
        "1"
     )
    skip-entry-p
     (= (hcnm-config-getvar "BubbleSkipEntryPrompt") "1")
    string ""
    tag
     (strcat "NOTETXT" (itoa line-number))
  )
  (while (and prompt-p loop-p)
    (cond
      ((or skip-entry-p
           (= (setq
                input
                 (getstring
                   1
                   (strcat
                     "\nLine "
                     (itoa line-number)
                     " text or . for automatic text: "
                   )
                 )
              )
              "."
           )
       )
       (setq
         lattribs
          (hcnm-ldrblk-get-auto-type
            ename-bubble
            line-number
            tag
            lattribs
          )
       )
       ;; Restore PSPACE if auto-text selection switched to MSPACE for viewport selection
       ;; This ensures subsequent prompts (Line 2, etc.) happen in correct space
       (cond
         (*hcnm-pspace-restore-needed*
          (vl-cmdf "._PSPACE")
          (setq *hcnm-pspace-restore-needed* nil) ; Clear the flag
         )
       )
       ;; Get text value for string comparison (2-element lattribs)
       (setq
         attr   (assoc tag lattribs)
         string (cadr attr)
       )
      )
      (t
       (setq
         string input
         lattribs
          (hcnm-ldrblk-lattribs-put-element tag string lattribs)
       )
      )
    )
    (setq
      skip-entry-p
       (and skip-entry-p (/= string "ENtry"))
      loop-p
       (or (not string) (= string "ENtry"))
    )
  )
  lattribs
)
;; Bubble note insertion experience innermost data prompts.
;; Returns lattribs
(defun hcnm-ldrblk-get-auto-type (ename-bubble line-number tag lattribs /
                              cvport-old haws-qt-new input space string
                             )
  (initget
    (substr
      (apply
        'strcat
        (mapcar
          '(lambda (x) (strcat " " (car x)))
          (hcnm-ldrblk-get-auto-type-keys)
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
         (itoa line-number)
         " automatic text. Enter an option ["
         (substr
           (apply
             'strcat
             (mapcar
               '(lambda (x) (strcat "/" (car x)))
               (hcnm-ldrblk-get-auto-type-keys)
             )
           )
           2
         )
         "] <"
         (car (last (hcnm-ldrblk-get-auto-type-keys)))
         ">: "
       )
     )
  )
  (cond
    ((or (not input) (= input "ENtry"))
     (setq
       lattribs
        (hcnm-ldrblk-lattribs-put-element tag "ENtry" lattribs)
     )
    )
    (t
     (setq
       lattribs
        (hcnm-ldrblk-auto-dispatch
          ename-bubble
          lattribs
          (strcat "NOTETXT" (itoa line-number))
          (cadr (assoc input (hcnm-ldrblk-get-auto-type-keys)))
          nil
        )
     )
    )
  )
  ;; bubble-data-update: This is broken because the calling function expects "ENtry" to be returned sometimes.
  ;; I feel like I really should find a way to abstract the interface from this other business logic. There may be a good example in my recent subdivision tools.
  lattribs
)
;#endregion
;#region Bubble data module
;;==============================================================================
;; hcnm-ldrblk-bubble-data Module - Bubble Data Accessors
;;==============================================================================
;; Provides typed accessors for bubble data alist structure.
;; BD = "Bubble Data" (commonly used abbreviation in this module)
;; LDRBLK = "Leader Block" (consistent with rest of codebase)
;;==============================================================================
;; Create a bubble data structure (alist) for passing state
;; All parameters optional - pass nil for unset fields
(defun hcnm-ldrblk-bubble-data-def ()
  (list
    (cons "ATTRIBUTES" nil)
    (cons "AVPORT" nil)
    (cons "BLOCKNAME" nil)
    (cons "ename-bubble" nil)
    (cons "ename-bubble-old" nil)
    (cons "ename-last" nil)
    (cons "ename-leader" nil)
    (cons "ename-leader-old" nil)
    (cons "NOTETYPE" nil)
    (cons "p1-ocs" nil)
    (cons "p1-ucs" nil)
    (cons "p1-world" nil)
    (cons "P2" nil)
    (cons "pspace-bubble-p" nil)
    (cons "pspace-for-restore-p" nil)   ; Tracks if we need to restore to pspace after auto-text selection
    (cons "replace-bubble-p" nil)
    (cons "TH" nil)
  )
)

;; Get a value from bubble data using haws_nested_list_get
(defun hcnm-ldrblk-bubble-data-get (bd key)
  (haws_nested_list_get bd (list key))
)

;; Set a value in bubble data using haws_nested_list_update
;; Validates key against known schema
(defun hcnm-ldrblk-bubble-data-set (bd key val /)
  (if (not (assoc key (hcnm-ldrblk-bubble-data-def)))
    (progn
      (princ (strcat "\nError: Invalid bubble-data key: " key))
      bd
    )
    (haws_nested_list_update bd (list key) val)
  )
)
;#region Bubble data utilities
;; Helper functions for working with bubble blocks and their properties
;; (not lattribs-specific)
;;==============================================================================
;; Ensure p1-world is present in bubble data (computes if missing)
(defun hcnm-ldrblk-bubble-data-ensure-p1-world
   (bubble-data / ename-bubble ename-leader p1-ocs p1-world replace-bubble-p)
  (setq replace-bubble-p (hcnm-ldrblk-bubble-data-get 
                           bubble-data
                           "replace-bubble-p"
                         )
  )
  (and
    (setq
      ename-bubble
       (hcnm-ldrblk-bubble-data-get
         bubble-data
         (if replace-bubble-p "ename-bubble-old" "ename-bubble")
       )
    )
    (or
      (hcnm-ldrblk-bubble-data-get bubble-data "ename-leader")
      (setq
        bubble-data
         (hcnm-ldrblk-bubble-data-set
           bubble-data
           (if replace-bubble-p "ename-leader-old" "ename-leader")
           (hcnm-ldrblk-bubble-leader ename-bubble)
         )
      )
      ;; No leader found - will be handled by caller with appropriate message
      nil
    )
    (setq
      ename-leader
       (hcnm-ldrblk-bubble-data-get
         bubble-data
         (if replace-bubble-p "ename-leader-old" "ename-leader")
       )
    )
    (or
      (hcnm-ldrblk-bubble-data-get bubble-data "p1-ocs")
      (setq
        bubble-data
         (hcnm-ldrblk-bubble-data-set
           bubble-data
           "p1-ocs"
           (hcnm-ldrblk-p1-ocs ename-leader)
         )
      )
      (princ
        "\nError in hcnm-ldrblk-bubble-data-ensure-p1-world: Could not determine p1-ocs from leader."
      )
    )
    (setq p1-ocs (hcnm-ldrblk-bubble-data-get bubble-data "p1-ocs"))
    (setq
      bubble-data
        (hcnm-ldrblk-bubble-data-set
          bubble-data
          "p1-ucs"
          (trans p1-ocs ename-leader 1)
        )
    )   
    ;; Try to calculate p1-world - may return nil for legacy bubbles without viewport XDATA
    (or
      (hcnm-ldrblk-bubble-data-get bubble-data "p1-world")
      (setq
        bubble-data
         (hcnm-ldrblk-bubble-data-set
           bubble-data
           "p1-world"
           (hcnm-ldrblk-p1-world
             ename-leader
             p1-ocs
             ename-bubble
           )
         )
      )
    )
    ;; Note: p1-world may be nil here for legacy bubbles in paper space
    ;; This is expected - caller will show "NOT FOUND!" message
    (setq p1-world (hcnm-ldrblk-bubble-data-get bubble-data "p1-world"))
    (haws-debug
      (list
        "Debug p1-ocs: "
        (vl-princ-to-string p1-ocs)
        " p1-world: "
        (vl-princ-to-string p1-world)
      )
    )
  )
  bubble-data
)

(defun hcnm-ldrblk-get-ename-bubble-old (bubble-data / elist-block-old
                                     ename-bubble-old replace-bubble-p
                                    )
  (setq
    replace-bubble-p
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "replace-bubble-p"
     )
  )
  (cond
    (replace-bubble-p
     ;; Prompt and check for old block.
     (while (or (not
                  (setq
                    ename-bubble-old
                     (car (entsel "\nSelect bubble note: "))
                  )
                )
                (not (setq elist-block-old (entget ename-bubble-old)))
                (not
                  (and
                    (= (cdr (assoc 0 elist-block-old)) "INSERT")
                    (wcmatch
                      (strcase
                        (vla-get-effectivename
                          (vlax-ename->vla-object ename-bubble-old)
                        )
                      )
                      "CNM-BUBBLE-*"
                    )
                  )
                )
            )
       (princ "\nSelected entity is not a CNM bubble note.")
     )
     (setq
       bubble-data
        (hcnm-ldrblk-bubble-data-set
          bubble-data
          "ename-bubble-old"
          ename-bubble-old
        )
     )
    )
    (t nil)
  )
  bubble-data
)
(defun hcnm-ldrblk-bubble-leader
   (ename-bubble / elist-bubble ename-330 ename-leader)
  (setq elist-bubble (entget ename-bubble))
  ;; Get start point
  ;; Find associated leader.
  (while ;; Check all 330 groups
         (and
           (not ename-leader)
           (setq ename-330 (cdr (assoc 330 elist-bubble)))
         )
    ;; Use the one that refers back to this block. Or move to the next one.
    (cond
      ((eq (cdr (assoc 340 (entget ename-330))) ename-bubble)
       (setq ename-leader ename-330)
      )
      (t
       (setq
         elist-bubble
          (cdr
            (member (assoc 330 elist-bubble) elist-bubble)
          )
         ename-leader nil
       )
      )
    )
  )
  ename-leader
)
(defun hcnm-ldrblk-p1-ocs (ename-leader)
  (cond
    (ename-leader (cdr (assoc 10 (entget ename-leader))))
    (t nil)
  )
)
(defun hcnm-ldrblk-get-mtext-string ()
  (cond
    ((= (hcnm-config-getvar "BubbleMtext") "1") "m-")
    (t "")
  )
)
(defun hcnm-ldrblk-change-arrowhead (ename-leader /)
  (cond
    ((= (hcnm-config-getvar "BubbleArrowIntegralPending") "1")
     ;; Disable reactors during arrowhead change to prevent reactor from triggering
     ;; and overwriting the newly saved lattribs with stale XDATA
     (hcnm-config-setvar "BlockReactors" "1")
     ;; 18 is "Integral" arrowhead type.
     (vla-put-arrowheadtype
       (vlax-ename->vla-object ename-leader)
       18
     )
     (hcnm-config-setvar "BubbleArrowIntegralPending" "0")
     (hcnm-config-setvar "BlockReactors" "0")
    )
  )
)
(defun hcnm-ldrblk-set-dynprops (ename-bubble-new ename-bubble-old notetype
                             replace-bubble-p / dyn-props-old
                             dyn-props-old-i vlaobj-block-new
                             vlaobj-block-old
                            )
  (setq vlaobj-block-new (vlax-ename->vla-object ename-bubble-new))
  (cond
    (ename-bubble-old
     (setq
       vlaobj-block-old
        (vlax-ename->vla-object ename-bubble-old)
       dyn-props-old
        (mapcar
          '(lambda (x)
             (list
               (vlax-get-property x 'propertyname)
               (vlax-get-property x 'value)
               x
             )
           )
          (vlax-invoke
            vlaobj-block-old
            'getdynamicblockproperties
          )
        )
     )
     (foreach
        vlaobj-property-new
        (vlax-invoke vlaobj-block-new 'getdynamicblockproperties)
       (if (and
             (setq
               dyn-props-old-i
                (assoc
                  (vlax-get-property
                    vlaobj-property-new
                    'propertyname
                  )
                  dyn-props-old
                )
             )
             (/= (vlax-get-property vlaobj-property-new 'readonly)
                 :vlax-true
             )
           )
         (vlax-put-property
           vlaobj-property-new
           'value
           (cadr dyn-props-old-i)
         )
       )
     )
    )
    (t (lm:setdynpropvalue vlaobj-block-new "Shape" notetype))
  )
)
;#endregion
;#region lattribs data model
;;==============================================================================
;; lattribs - Core attribute list data structure
;;==============================================================================
;; Structure: '(("TAG" "text") ...)
;; - All 11 tags required (NOTENUM NOTEPHASE NOTEGAP NOTETXT0-6)
;; - Always 2-element lists (tag text-value)
;; - Validation: Fail loudly on violations
;;==============================================================================
(defun hcnm-ldrblk-lattribs-spec (/ lattribs)
  ;; Pure spec - returns empty 2-element structure for all bubble attributes
  ;; To populate with values, use lattribs-put-element
  (setq
    lattribs
     (mapcar
       '(lambda (index)
          (list (strcat "NOTETXT" (itoa index)) "")
        )
       '(1 2 3 4 5 6 0)
     )
    lattribs
     (cons (list "NOTEGAP" "") lattribs)
    lattribs
     (cons (list "NOTEPHASE" "") lattribs)
    lattribs
     (cons (list "NOTENUM" "") lattribs); Empty, will be filled by caller
  )
  lattribs
)
;;; Save attribute value to attribute list (replaces entire value)
;;; If element doesn't exist, adds it
(defun hcnm-ldrblk-lattribs-put-element (tag value lattribs / attr)
  ;; Value must be a string in 2-element architecture
  (if (not (= (type value) 'str))
    (progn
      (alert
        (princ
          (strcat
            "\nhcnm-ldrblk-lattribs-put-element: value must be string, got: "
            (vl-princ-to-string value)
          )
        )
      )
      (exit)
    )
  )
  (setq attr (assoc tag lattribs))
  (cond
    ;; Element exists - replace it
    (attr (subst (list tag value) attr lattribs))
    ;; Element doesn't exist - add it
    (t (append lattribs (list (list tag value))))
  )
)

;;; Smart search/replace for auto-text updates
;;; Preserves user's manual text (prefix/postfix) while updating auto-generated portion
;;; Used by both dialog path (eb-get-text) and reactor path (update-bubble-tag)
;;;
;;; PARAMETERS:
;;;   current-text - Full text from attribute (may include format codes like %%u or %%o)
;;;   old-auto-text - Previous auto-text value from XDATA (nil if first time)
;;;   new-auto-text - New auto-text value to insert
;;;   ename-bubble - Entity name (for XDATA reading if needed)
;;;
;;; RETURNS:
;;;   New text with auto-text replaced, manual text preserved
;;;
;;; SEARCH PRIORITY:
;;;   1. Delimiter ``` in clean text - REPLACE delimiter
;;;   2. Old auto-text from XDATA in clean text - REPLACE old value
;;;   3. Fallback - APPEND WITHOUT SPACE (user must use delimiter for control)
;;;
;;; SIDE EFFECTS: None (pure function)
(defun hcnm-ldrblk-smart-replace-auto (current-text old-auto-text
                                   new-auto-text / clean-current-text
                                   pos new-text
                                  )
  ;; Strip format codes from current text for clean searching
  (setq clean-current-text current-text)
  (cond
    ((wcmatch clean-current-text "\\L*")
     (setq clean-current-text (substr clean-current-text 3))
    )
    ((wcmatch clean-current-text "\\O*")
     (setq clean-current-text (substr clean-current-text 3))
    )
  )
  (cond
    ((wcmatch clean-current-text "%%u*")
     (setq clean-current-text (substr clean-current-text 4))
    )
    ((wcmatch clean-current-text "%%o*")
     (setq clean-current-text (substr clean-current-text 4))
    )
  )
  ;; Search priority: 1) Delimiter, 2) Old XDATA value, 3) Append (no space)
  (setq
    new-text
     (cond
       ;; Priority 1: If delimiter found, replace it
       ((setq pos (vl-string-search "```" clean-current-text))
        (strcat
          (substr clean-current-text 1 pos)
          new-auto-text
          (if (> (strlen clean-current-text) (+ pos 3))
                                        ; Skip "```" (3 chars)
            (substr clean-current-text (+ pos 4))
                                        ; +4 to skip delimiter
            ""
          )
        )
       )
       ;; Priority 2: If old auto-text found in XDATA, replace it
       ((and
          old-auto-text
          (setq
            pos
             (vl-string-search
               old-auto-text
               clean-current-text
             )
          )
        )
        (strcat
          (substr clean-current-text 1 pos)
          new-auto-text
          (if (> (strlen clean-current-text)
                 (+ pos (strlen old-auto-text))
              )
            (substr
              clean-current-text
              (+ pos (strlen old-auto-text) 1)
            )
            ""
          )
        )
       )
       ;; Priority 3: Empty field - just use auto-text
       ((= clean-current-text "") new-auto-text)
       ;; Fallback: append WITHOUT space (user must add space or use delimiter)
       (t (strcat clean-current-text new-auto-text))
     )
  )
  new-text
)

;;; Update auto-text value in lattribs (2-element architecture)
;;; SIMPLE REPLACEMENT - just sets the tag value
;;; Smart search/replace is handled by CALLER (eb-get-text for dialog, update-bubble-tag for reactor)
(defun hcnm-ldrblk-lattribs-put-auto
   (tag auto-new lattribs ename-bubble / attr)
  ;; Simple replacement - caller handles search/replace logic
  (setq attr (assoc tag lattribs))
  (cond
    (attr (subst (list tag auto-new) attr lattribs))
    (t (append lattribs (list (list tag auto-new))))
  )
)
;;; Ensure all bubble attributes have proper 2-element list structure.
;;;
;;; PURPOSE: Normalize lattribs to consistent (tag text) format
;;;          before applying format codes or writing to drawing.
;;;
;;; INPUT: lattribs from various sources:
;;;   - hcnm-get-attributes: Returns attributes from existing bubble
;;;   - hcnm-ldrblk-lattribs-spec: Returns fresh 2-element format
;;;   - After auto-text or manual text entry: Should already be 2-element format
;;;
;;; RETURNS: lattribs with ALL attributes in 2-element format:
;;;   ("NOTETXT1" "full text value")
;;;   ("NOTENUM" "123")
;;;   ("NOTEPHASE" "phase text")
;;;   ("NOTEGAP" "0.25")
;;;
;;; EXAMPLES:
;;;   Input:  (("NOTENUM" "123") ("NOTETXT1" "Storm Drain STA 10+25 RT"))
;;;   Output: (("NOTENUM" "123") ("NOTETXT1" "Storm Drain STA 10+25 RT")) - unchanged, valid
;;;
;;;   Input:  (("NOTENUM" "123" "" "") ("NOTETXT1" "Storm Drain"))  ; OLD 4-element
;;;   Output: NIL with ALERT - invalid schema (migration needed)
;;;
;;; ARCHITECTURE: NO BACKWARD COMPATIBILITY - Fail loudly on violations
;;;               This is a wrapper around lattribs-validate-schema for consistency
;;;
(defun hcnm-ldrblk-lattribs-validate (lattribs /)
  ;; Strict validation - fail loudly on any schema violations
  (if (not (hcnm-ldrblk-lattribs-validate-schema lattribs))
    (progn
      (alert
        (princ
          "\nCRITICAL: lattribs-validate failed - invalid schema"
        )
      )
      nil
    )                                   ; Return nil to indicate failure
    lattribs
  )                                     ; Return validated lattribs
)

;;; ============================================================================
;;; STRICT SCHEMA VALIDATOR (Fail Loudly)
;;; ============================================================================
;;; 
;;; PHILOSOPHY: "Fail loudly" - catch data integrity violations early
;;;             Don't silently fix problems that indicate bugs
;;;
;;; PURPOSE: Validate lattribs has complete schema with correct 2-element structure
;;;
;;; CHECKS:
;;;   1. All required tags are present
;;;   2. Each element is a 2-part list (tag text)
;;;   3. No duplicate tags
;;;   4. Both parts are strings (never nil, use "" for empty)
;;;
;;; RETURNS: T if valid
;;;          NIL with ALERT if invalid (stops execution)
;;;
;;; WHEN TO CALL:
;;;   - After reading from drawing (dwg-to-lattribs)
;;;   - Before writing to drawing (lattribs-to-dwg)
;;;   - After dialog edits (eb-save)
;;;   - After auto-text generation
;;;   - Any time you want to assert data integrity
;;;
;;; EXAMPLE USAGE:
;;;   (if (not (hcnm-ldrblk-lattribs-validate-schema lattribs))
;;;     (exit))  ; Abort operation if validation fails
;;;
(defun hcnm-ldrblk-lattribs-validate-schema (lattribs / required-tags
                                         missing-tags tag-counts
                                         duplicate-tags attr tag parts
                                         error-msgs
                                        )
  (setq
    required-tags
     '("NOTENUM" "NOTEPHASE" "NOTEGAP" "NOTETXT0" "NOTETXT1" "NOTETXT2"
       "NOTETXT3" "NOTETXT4" "NOTETXT5" "NOTETXT6"
      )
    missing-tags
     '()
    tag-counts
     '()
    duplicate-tags
     '()
    error-msgs
     '()
  )
  ;; Check 1: All required tags present
  (foreach
     tag required-tags
    (if (not (assoc tag lattribs))
      (setq missing-tags (cons tag missing-tags))
    )
  )
  ;; Check 2: Each element is 2-part list with valid structure
  (foreach
     attr lattribs
    (setq tag (car attr))
    (cond
      ;; Not a list
      ((/= (type attr) 'list)
       (setq
         error-msgs
          (cons
            (strcat tag ": Not a list structure")
            error-msgs
          )
       )
      )
      ;; Wrong number of elements
      ((/= (length attr) 2)
       (setq
         error-msgs
          (cons
            (strcat
              tag
              ": Must have 2 elements (tag text), has "
              (itoa (length attr))
            )
            error-msgs
          )
       )
      )
      ;; Check both parts are strings
      ((not
         (and
           (= (type (nth 0 attr)) 'str) ; tag
           (= (type (nth 1 attr)) 'str)
         )
       )                                ; text
       (setq
         error-msgs
          (cons
            (strcat
              tag
              ": Both parts must be strings (never nil)"
            )
            error-msgs
          )
       )
      )
    )
    ;; Count tag occurrences for duplicate check
    (if (assoc tag tag-counts)
      (setq
        tag-counts
         (subst
           (cons tag (1+ (cdr (assoc tag tag-counts))))
           (assoc tag tag-counts)
           tag-counts
         )
      )
      (setq tag-counts (cons (cons tag 1) tag-counts))
    )
  )
  ;; Check 3: No duplicate tags
  (foreach
     tag-count tag-counts
    (if (> (cdr tag-count) 1)
      (setq duplicate-tags (cons (car tag-count) duplicate-tags))
    )
  )
  ;; Compile error report
  (if missing-tags
    (setq
      error-msgs
       (cons
         (strcat
           "MISSING REQUIRED TAGS: "
           (apply
             'strcat
             (mapcar
               '(lambda (tag) (strcat tag " "))
               missing-tags
             )
           )
         )
         error-msgs
       )
    )
  )
  (if duplicate-tags
    (setq
      error-msgs
       (cons
         (strcat
           "DUPLICATE TAGS: "
           (apply
             'strcat
             (mapcar
               '(lambda (tag) (strcat tag " "))
               duplicate-tags
             )
           )
         )
         error-msgs
       )
    )
  )
  ;; Fail loudly if errors found
  (cond
    (error-msgs
     (alert
       (princ
         (strcat
           "\nLATTRIBS SCHEMA VALIDATION FAILED:\n\n"
           (apply
             'strcat
             (mapcar
               '(lambda (msg) (strcat msg "\n"))
               (reverse error-msgs)
             )
           )
           "\nThis indicates a programming error or data corruption.\n"
           "Operation aborted to prevent data loss."
         )
       )
     )
     nil                                ; Return NIL to indicate failure
    )
    (t
     t                                  ; Return T to indicate success
    )
  )
)

;;; Adds underlining to NOTETXT1 and overlining to NOTETXT2.
;;;
;;; RESPONSIBILITY: Decide which attributes need formatting and apply format codes.
;;;                 Does NOT parse or normalize structure (that's ensure-fields).
;;;
;;; ASSUMES: All inputs have proper prefix?auto?postfix structure (two chr(160))
;;;
;;; DATA FLOW:
;;; 1. Get attribute values (already normalized by ensure-fields)
;;; 2. If line has content: apply underline (TXT1) or overline (TXT2) to entire value
;;; 3. Handle NOTEGAP (underline + spaces if either line has content)
;;; 4. Save formatted values back to lattribs
;;;
;;; Check if attribute has actual content (not just empty or delimiters).
;;; Returns T if there's text content, NIL otherwise.
(defun hcnm-ldrblk-attr-has-content-p (string)
  ;; Simple check: is string non-empty?
  (and string (/= string ""))
)

;;; ARCHITECTURE: Core underover functions (operates on FULL lattribs list)
;;;
;;; DATA FLOW PATTERN:
;;; - hcnm-ldrblk-underover-add: Adds format codes (%%u, %%o) and sets NOTEGAP (for display: dwg/dlg)
;;; - hcnm-ldrblk-underover-remove: Strips format codes and clears NOTEGAP (for reading: dwg/dlg)
;;;
;;; Called by:
;;; - underover-add: Used by lattribs-to-dwg, lattribs-to-dlg, and after auto-text generation
;;; - underover-remove: Used by dwg-to-lattribs, dlg-to-lattribs for reading
;;;
;;; CRITICAL: These functions MUST operate on the full lattribs list because
;;; the underover logic requires seeing TXT1 AND TXT2 together to make decisions.

;;; ============================================================================
;;; DATA STRUCTURE TRANSFORMATIONS
;;; ============================================================================

;;; Concatenate structured lattribs into flat strings
;;;
;;; PURPOSE: Transform lattribs from structured (prefix/auto/postfix) to
;;;          concatenated (single string per tag) format.
;;;
;;; INPUT: lattribs (structured)
;;;   '(("NOTETXT1" "prefix" "auto" "postfix")
;;;     ("NOTETXT2" "prefix" "auto" "postfix")
;;;     ("NOTEGAP" "" "" ""))
;;;
;;; OUTPUT: lattribs-cat (concatenated)
;;;   '(("NOTETXT1" "prefixautopostfix")
;;;     ("NOTETXT2" "prefixautopostfix")
;;;     ("NOTEGAP" ""))
;;;
;;; NOTE: This is pure data transformation, no business logic.
;;;
;;; DEPRECATED - No longer needed in 2-element architecture
;;; Concatenate 4-element structured lattribs to 2-element format
;;; This function is obsolete now that lattribs is always 2-element
(defun hcnm-ldrblk-lattribs-concat
   (lattribs / result attr tag concat-value)
  (alert
    (princ
      "\nDEPRECATED: lattribs-concat called but no longer needed in 2-element architecture"
    )
  )
  ;; Return as-is (already 2-element)
  lattribs
)

;;; Split concatenated lattribs using xdata delimiters
;;;
;;; PURPOSE: Transform lattribs from concatenated (single string) back to
;;;          structured (prefix/auto/postfix) format using xdata delimiters.
;;;
;;; INPUT: 
;;;   lattribs-cat - Concatenated format '(("NOTETXT1" "prefixautopostfix") ...)
;;;   xdata-alist - XDATA from drawing with delimiter positions
;;;
;;; OUTPUT: lattribs (structured)
;;;   '(("NOTETXT1" "prefix" "auto" "postfix") ...)
;;;
;;; NOTE: This is pure data transformation, no business logic.
;;;       Uses xdata to find CHR 160 delimiter positions and split.
;;;
;;; DEPRECATED - No longer needed in 2-element architecture
;;; Split concatenated lattribs using xdata delimiters (CHR 160 based)
;;; This function is obsolete now that we use search-based XDATA parsing


;;; ============================================================================
;;; UNDEROVER FORMAT CODE LOGIC (Business Logic)
;;; ============================================================================

;;; Add format codes to concatenated strings
;;;
;;; BUSINESS LOGIC (documented 2025-10-30):
;;; - If TXT1 has content → add underline (%%u for dtext, \L for mtext)
;;; - If TXT2 has content → add overline (%%o for dtext, \O for mtext)
;;; - If EITHER has content → NOTEGAP = "%%u  ", else ""
;;;
;;; INPUT: lattribs-cat (concatenated)
;;;   '(("NOTETXT1" "text1") ("NOTETXT2" "text2") ("NOTEGAP" ""))
;;;
;;; OUTPUT: lattribs-cat (formatted)
;;;   '(("NOTETXT1" "%%utext1") ("NOTETXT2" "%%otext2") ("NOTEGAP" "%%u  "))
;;;
;;; Used by: lattribs-to-dwg, lattribs-to-dlg
;;;
;;; Add underline/overline format codes to structured lattribs
;;;
;;; BUSINESS LOGIC: Check if TXT1/TXT2 have content (any of prefix/auto/postfix)
;;;                 Add %%u to TXT1 prefix, %%o to TXT2 prefix if not empty
;;;                 Set NOTEGAP based on emptiness
;;;
;;; INPUT: lattribs (structured)
;;;   '(("NOTETXT1" "prefix" "auto" "postfix") ("NOTETXT2" "p" "a" "p") ...)
;;;
;;; OUTPUT: lattribs (structured with format codes in prefix)
;;;   '(("NOTETXT1" "%%uprefix" "auto" "postfix") ("NOTETXT2" "%%op" "a" "p") ...)
;;;
;;; Used by: lattribs-to-dlg, lattribs-to-dwg
;;;
(defun hcnm-ldrblk-underover-add (lattribs / bubblemtext underline overline
                              txt1-attr txt2-attr txt1-empty-p
                              txt2-empty-p txt1-value txt2-value
                              txt1-clean txt2-clean gap-value result
                             )
  ;; CRITICAL: Strip any existing format codes first to prevent proliferation
  ;; This handles the case where lattribs already has format codes from a previous pass
  (setq lattribs (hcnm-ldrblk-underover-remove lattribs))
  ;; Determine format codes based on mtext vs dtext
  (setq
    bubblemtext
     (hcnm-ldrblk-get-mtext-string)
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
  )
  ;; Get TXT1 and TXT2 attributes
  (setq
    txt1-attr
     (assoc "NOTETXT1" lattribs)
    txt2-attr
     (assoc "NOTETXT2" lattribs)
  )
  ;; Check if empty (2-element: just check text value)
  (setq
    txt1-empty-p
     (or (not txt1-attr) (= "" (cadr txt1-attr)))
    txt2-empty-p
     (or (not txt2-attr) (= "" (cadr txt2-attr)))
  )
  ;; Add format code to text value if NOT empty
  (setq
    txt1-value
     (cond
       ((not txt1-empty-p)
        (strcat underline (cadr txt1-attr))
       )
       (t (cadr txt1-attr))
     )
  )
  ;; Add format code to text value if NOT empty
  (setq
    txt2-value
     (cond
       ((not txt2-empty-p)
        (strcat overline (cadr txt2-attr))
       )
       (t (cadr txt2-attr))
     )
  )
  ;; Set NOTEGAP based on whether either line has content
  (setq
    gap-value
     (cond
       ((or (not txt1-empty-p) (not txt2-empty-p)) "%%u  ")
       (t "")
     )
  )
  ;; Build result with formatted values (2-element structure)
  (setq result lattribs)
  (setq result (subst (list "NOTETXT1" txt1-value) txt1-attr result))
  (setq result (subst (list "NOTETXT2" txt2-value) txt2-attr result))
  (setq
    result
     (subst
       (list "NOTEGAP" gap-value)
       (assoc "NOTEGAP" result)
       result
     )
  )
  result
)

;;; Remove underline/overline format codes from 2-element lattribs
;;;
;;; BUSINESS LOGIC: Strip %%u, %%o, \L, \O codes from text values + clear NOTEGAP
;;;
;;; INPUT: lattribs (2-element with format codes)
;;;   '(("NOTETXT1" "%%utext value") ("NOTETXT2" "%%otext value") ...)
;;;
;;; OUTPUT: lattribs (2-element, clean)
;;;   '(("NOTETXT1" "text value") ("NOTETXT2" "text value") ...)
;;;
;;; Used by: dwg-to-lattribs (already strips during read)
;;;
(defun hcnm-ldrblk-underover-remove
   (lattribs / result attr tag value clean-value)
  (setq result nil)
  (foreach
     attr lattribs
    (setq tag (car attr))
    (cond
      ;; Handle NOTETXT1 and NOTETXT2 (2-element lists with text value)
      ((or (= tag "NOTETXT1") (= tag "NOTETXT2"))
       (setq
         value
          (cadr attr)
         clean-value value
       )
       ;; Strip mtext format codes (\L, \O)
       (cond
         ((wcmatch clean-value "\\L*")
          (setq clean-value (substr clean-value 3))
         )
         ((wcmatch clean-value "\\O*")
          (setq clean-value (substr clean-value 3))
         )
       )
       ;; Strip dtext format codes (%%u, %%o)
       (cond
         ((wcmatch clean-value "%%u*")
          (setq clean-value (substr clean-value 4))
         )
         ((wcmatch clean-value "%%o*")
          (setq clean-value (substr clean-value 4))
         )
       )
       (setq result (append result (list (list tag clean-value))))
      )
      ;; Handle NOTEGAP (clear it)
      ((= tag "NOTEGAP")
       (setq result (append result (list (list tag ""))))
      )
      ;; Handle all other attributes (2-element format, pass through)
      (t (setq result (append result (list attr))))
    )
  )
  result
)

;;; DATA FLOW FUNCTIONS: lattribs ← → dlg
;;;
;;; These functions transform between clean lattribs (internal format) and
;;; dialog display format (with format codes visible).

;;; Transform lattribs to dialog display format
;;;
;;; INPUT: Clean lattribs with prefix/auto/postfix structure
;;;   Example: (("NOTETXT1" "Storm " "STA 1+00" " RT") ...)
;;;
;;; OUTPUT: Structured lattribs with format codes added to prefix
;;;   Example: (("NOTETXT1" "%%uStorm " "STA 1+00" " RT") ...)
;;;
;;; ARCHITECTURE: Just calls underover-add (preserves 3-part structure for dialog)
;;;
(defun hcnm-ldrblk-lattribs-to-dlg (lattribs)
  (hcnm-ldrblk-underover-add lattribs)
)

;;; Transform dialog input back to clean lattribs
;;;
;;; INPUT: Dialog format (structured with format codes in prefix)
;;;   Example: (("NOTETXT1" "%%uStorm " "STA 1+00" " RT") ...)
;;;
;;; OUTPUT: Clean structured lattribs (format codes stripped)
;;;   Example: (("NOTETXT1" "Storm " "STA 1+00" " RT") ...)
;;;
;;; ARCHITECTURE: Just calls underover-remove (strips codes from prefix)
;;;
(defun hcnm-ldrblk-dlg-to-lattribs (dlg-lattribs)
  (hcnm-ldrblk-underover-remove dlg-lattribs)
)
;#endregion
;#endregion
;#region Auto-text
;; Used by multiple levels of the insertion user experience 
;; including the command prompts and the auto text dispatcher
;; Returns list of auto-text type definitions
;; Structure: (input-key display-type reference-type requires-coordinates)
;; - input-key: Keyword entered by user (initget format)
;; - display-type: Canonical type name used in code
;; - reference-type: Type of reference object ("AL"=Alignment, "SU"=Surface, nil=none)
;; - requires-coordinates: T if needs p1-world from leader, nil otherwise
(defun hcnm-ldrblk-get-auto-type-keys ()
  '(("LF" "LF" nil nil)                 ; Length (QTY) - user picks objects
    ("SF" "SF" nil nil)                 ; Square Feet (QTY) - user picks objects
    ("SY" "SY" nil nil)                 ; Square Yards (QTY) - user picks objects
    ("STa" "Sta" "AL" t)                ; Station - needs p1-world for alignment query
    ("Off" "Off" "AL" t)                ; Offset - needs p1-world for alignment query
    ("stAoff" "StaOff" "AL" t)          ; Station+Offset - needs p1-world for alignment query
    ("NAme" "AlName" "AL" nil)          ; Alignment Name - no coordinates needed
    ("STAName" "StaName" "AL" t)        ; Station + Alignment Name - needs p1-world
    ("N" "N" nil t)                     ; Northing - needs p1-world for coordinate
    ("E" "E" nil t)                     ; Easting - needs p1-world for coordinate
    ("NE" "NE" nil t)                   ; Northing+Easting - needs p1-world for coordinate
    ("Z" "Z" "SU" t)                    ; Elevation - needs p1-world for surface query (unimplemented)
    ("Dia" "Dia" "PIPE" nil)            ; Pipe Diameter - user selects pipe object
    ("SLope" "Slope" "PIPE" nil)        ; Pipe Slope - user selects pipe object
    ("L" "L" "PIPE" nil)                ; Pipe Length - user selects pipe object
    ("Text" "Text" nil nil)             ; Static text - user enters manually
    ("ENtry" "ENtry" nil nil)           ; Entry number - static text
   )
)
;; hcnm-ldrblk-auto-dispatch is called from command line (insertion) and from edit box (editing) to get string as requested by user. It needs to get not only string, but also data (reference object and reference type).
;; 
;; This is how bubble note auto text works.
;; 
;; Bubble note creation process from inside out:
;; hcnm-ldrblk-auto-dispatch returns lattribs
;; hcnm-ldrblk-get-auto-type returns lattribs
;; hcnm-ldrblk-get-text-entry returns lattribs
;; hcnm-ldrblk-get-bubble-data returns block-data that includes lattribs after adjusting formatting  (overline and underline)
;; hcnm-set-attributes puts lattribs into bubble note
;; 
;; Bubble note editing process from inside out:
;; hcnm-ldrblk-auto-dispatch returns lattribs
;; hcnm-ldrblk-eb-get-text modifies semi-global hcnm-ldrblk-eb-lattribs after adjusting formatting (overline and underline)
;; hcnm-ldrblk-eb-save calls hcnm-set-attributes to save semi-global hcnm-ldrblk-eb-lattribs
;; hcnm-edit-bubble top level manages editing dialog
;;
;; Reactor update process from inside out:
;; hcnm-ldrblk-auto-dispatch returns lattribs
;; hcnm-ldrblk-update-bubble-tag modifies bubble-data that includes lattribs after adjusting formatting  (overline and underline)
;; hcnm-ldrblk-reactor-update calls hcnm-ldrblk-update-bubble-tag to update semi-global bubble-data
;; hcnm-ldrblk-reactor top level manages reactor update
;;;
;;; PARAMETERS:
;; obj-target is the target object provided by the reactor callback (not used in insertion/editing)
;; tag is the attribute tag being processed (e.g., "NOTETXT1")
;; Returns lattribs with the requested auto data added.
(defun hcnm-ldrblk-auto-dispatch (ename-bubble lattribs tag auto-type
                              obj-target / bubble-data
                             )
  ;; bubble-data-update: Build bubble-data and pass to subfunctions
  (setq
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ename-bubble"
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  ;; Ensure ename-leader is in bubble-data (needed for reactor attachment)
  (cond
    ((not
       (hcnm-ldrblk-bubble-data-get bubble-data "ename-leader")
     )
     (setq
       bubble-data
        (hcnm-ldrblk-bubble-data-set
          bubble-data
          "ename-leader"
          (hcnm-ldrblk-bubble-leader ename-bubble)
        )
     )
    )
  )
  ;; NOTE: Coordinate-based auto-text handlers (auto-ne, auto-al, auto-su) each call helpers
  ;; in a parallel way to get AVPORT and p1-world at the top of their function body
  (setq
    bubble-data
     (cond
       ((= auto-type "Text")
        (hcnm-ldrblk-auto-es
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "LF")
        (hcnm-ldrblk-auto-qty
          bubble-data tag auto-type "Length" "1" obj-target
         )
       )
       ((= auto-type "SF")
        (hcnm-ldrblk-auto-qty
          bubble-data tag auto-type "Area" "1" obj-target
         )
       )
       ((= auto-type "SY")
        (hcnm-ldrblk-auto-qty
          bubble-data tag auto-type "Area" "0.11111111" obj-target
         )
       )
       ((= auto-type "Sta")
        (hcnm-ldrblk-auto-al
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "Off")
        (hcnm-ldrblk-auto-al
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "StaOff")
        (hcnm-ldrblk-auto-al
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "AlName")
        (hcnm-ldrblk-auto-al
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "StaName")
        (hcnm-ldrblk-auto-al
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "N")
        (hcnm-ldrblk-auto-ne
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "E")
        (hcnm-ldrblk-auto-ne
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "NE")
        (hcnm-ldrblk-auto-ne
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "Z")
        (hcnm-ldrblk-auto-su
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "Dia")
        (hcnm-ldrblk-auto-pipe
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "Slope")
        (hcnm-ldrblk-auto-pipe
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
       ((= auto-type "L")
        (hcnm-ldrblk-auto-pipe
          bubble-data
          tag
          auto-type
          obj-target
        )
       )
     )
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
  )
  lattribs
)

;#region Auto text/mtext
(defun hcnm-ldrblk-auto-es (bubble-data tag auto-type obj-target / ename
                        lattribs ename-bubble
                       )
  (setq
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
    ename
     (cond
       (obj-target)
       (t
        (car
          (nentsel (strcat "\nSelect object with " auto-type ": "))
        )
       )
     )
  )
  ;; END hcnm-ldrblk-auto-get-input SUBFUNCTION
  ;; START hcnm-ldrblk-auto-update SUBFUNCTION
  (setq
    lattribs
     (hcnm-ldrblk-lattribs-put-auto
       tag
       (cond
         (ename (cdr (assoc 1 (entget ename))))
         (t "")
       )
       lattribs
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  bubble-data
)
;#endregion
;#region Auto quantity (LF/SF/SY)
(defun hcnm-ldrblk-auto-qty (bubble-data tag auto-type qt-type factor
                         obj-target / lattribs str-backslash input1
                         pspace-bubble-p ss-p string ename-bubble
                        )
  (setq
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
  )
  (cond
    (obj-target
     (setq string "Programming error. See command line.")
     (princ
       (strcat
         "\nProgramming error: "
         auto-type
         " auto text uses AutoCAD fields, which don't need to be updated by CNM. But hcnm-ldrblk-auto-qty was given an an obj-target to update."
       )
     )
    )
    (t
     (cond
       ((and
          (= qt-type "Area")
          (= (hcnm-config-getvar "BubbleAreaIntegral") "1")
        )
        (hcnm-config-setvar "BubbleArrowIntegralPending" "1")
       )
     )
     (setq pspace-bubble-p (hcnm-ldrblk-space-set-model))
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
           (haws-qt-set-property
             "ldrblk"
             "type"
             (strcase qt-type t)
           )                            ; "length" or "area"
           (haws-qt-set-property "ldrblk" "factor" (read factor))
           (haws-qt-set-property
             "ldrblk"
             "postfix"
             (hcnm-config-getvar
               (strcat "BubbleTextPostfix" auto-type)
             )
           )
           (haws-qt-string "ldrblk")
           (haws-qt-get-property "ldrblk" "string")
          )
          (t
           (strcat
             (hcnm-config-getvar
               (strcat "BubbleTextPrefix" auto-type)
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
             qt-type                    ; "Length" or "Area"
             " \\f \"%lu2%pr"
             (hcnm-config-getvar
               (strcat "BubbleTextPrecision" auto-type)
             )
             "%ct8["
             factor
             "]\">%"
             (hcnm-config-getvar
               (strcat "BubbleTextPostfix" auto-type)
             )
           )
          )
        )
     )
     (hcnm-ldrblk-space-restore pspace-bubble-p)
    )
  )
  ;; END hcnm-ldrblk-auto-get-input SUBFUNCTION
  ;; START hcnm-ldrblk-auto-update SUBFUNCTION
  (setq
    lattribs
     (hcnm-ldrblk-lattribs-put-auto
       tag
       string
       lattribs
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  bubble-data
)
;#endregion
;#region Auto alignment
;;==============================================================================
;; ALIGNMENT AUTO-TEXT (Sta/Off/StaOff)
;;==============================================================================
;; Calculates station and offset values from alignment and leader position
;; Workflow: Get alignment ? Calculate Sta/Off ? Format text ? Attach reactor
;;
;; REFACTORED: Split into modular functions for better maintainability
;;   - hcnm-ldrblk-auto-alignment-calculate: Pure calculation (testable)
;;   - hcnm-ldrblk-auto-al-station-to-string: Format station string
;;   - hcnm-ldrblk-auto-al-offset-to-string: Format offset string
;;   - hcnm-ldrblk-auto-al: Main orchestrator (backward compatible)

;; Calculate raw station and offset from alignment and world point
;; Pure function: No side effects, easily testable
;; Arguments:
;;   alignment-object - VLA-OBJECT of Civil 3D alignment
;;   p1-world - (X Y Z) point in world coordinates
;; Returns:
;;   (DRAWSTATION . OFFSET) on success
;;   NIL on failure
(defun hcnm-ldrblk-auto-alignment-calculate
   (alignment-object p1-world / drawstation offset)
  (cond
    ((and (= (type alignment-object) 'vla-object) p1-world)
     ;; Call Civil 3D alignment method to get station/offset
     ;; http://docs.autodesk.com/CIV3D/2012/ENU/API_Reference_Guide/com/AeccXLandLib__IAeccAlignment__StationOffset
     (vlax-invoke-method
       alignment-object
       'stationoffset
       (vlax-make-variant (car p1-world) vlax-vbdouble)
       (vlax-make-variant (cadr p1-world) vlax-vbdouble)
       'drawstation
       'offset
     )
     ;; Return as dotted pair
     (cons drawstation offset)
    )
    (t nil)                             ; Invalid inputs
  )
)

;; Format station value with config-based prefix/postfix
;; Arguments:
;;   alignment-object - VLA-OBJECT to get station string with equations
;;   DRAWSTATION - Raw station value from StationOffset method
;; Returns: Formatted station string (e.g., "STA 10+50.00")
(defun hcnm-ldrblk-auto-al-station-to-string (alignment-object drawstation)
  (strcat
    (hcnm-config-getvar "BubbleTextPrefixSta")
    (vlax-invoke-method
      alignment-object
      'getstationstringwithequations
      drawstation
    )
    (hcnm-config-getvar "BubbleTextPostfixSta")
  )
)

;; Format offset value with config-based prefix/postfix and sign handling
;; Arguments:
;;   OFFSET - Raw offset value (positive = right, negative = left)
;; Returns: Formatted offset string (e.g., "25.00 RT" or "LT 10.50")
(defun hcnm-ldrblk-auto-al-offset-to-string (offset / offset-value)
  ;; Determine offset value (absolute or with sign)
  (setq
    offset-value
     (cond
       ((= (hcnm-config-getvar "BubbleOffsetDropSign")
           "1"
        )
        (abs offset)                    ; Drop sign, show absolute value
       )
       (t offset)                       ; Keep sign
     )
  )
  ;; Build offset string with appropriate prefix/postfix
  (strcat
    ;; Prefix depends on offset direction
    (cond
      ((minusp offset)
       (hcnm-config-getvar "BubbleTextPrefixOff-")
      )
      (t (hcnm-config-getvar "BubbleTextPrefixOff+"))
    )
    ;; Format number with configured precision
    (rtos
      offset-value
      2
      (atoi (hcnm-config-getvar "BubbleTextPrecisionOff+"))
    )
    ;; Postfix depends on offset direction
    (cond
      ((minusp offset)
       (hcnm-config-getvar "BubbleTextPostfixOff-")
      )
      (t (hcnm-config-getvar "BubbleTextPostfixOff+"))
    )
  )
)
;; Main alignment auto-text function (backward compatible)
;; Orchestrates: get alignment ? calculate ? format ? attach reactor
;; Arguments:
;;   bubble-data - Bubble data alist
;;   TAG - Attribute tag to update
;;   auto-type - "Sta", "Off", "StaOff", "AlName", or "StaName"
;;   obj-target - Optional: Pre-selected alignment object (used by reactor updates)
;; Returns: Updated bubble-data with new attribute value
(defun hcnm-ldrblk-auto-al (bubble-data tag auto-type obj-target /
                        alignment-name lattribs ename-bubble
                        ename-leader sta-off-pair drawstation offset
                        obj-align p1-world pspace-bubble-p sta-string
                        off-string string cvport ref-ocs-1 ref-ocs-2
                        ref-ocs-3 ref-wcs-1 ref-wcs-2 ref-wcs-3
                        profile-start
                       )
  ;;===========================================================================
  ;; PROFILING: Start timing alignment auto-text generation
  ;;===========================================================================
  (setq profile-start (haws-profile-start "insert-auto-alignment"))
  (setq
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    ename-leader
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-leader"
     )
    p1-world
     (hcnm-ldrblk-bubble-data-get bubble-data "p1-world")
  )
  ;; STEP 1: Get alignment object (user selection or provided obj-target)
  (cond
    (obj-target
     ;; Reactor callback - for coordinate types, just recalculate p1-world from current leader
     ;; No gateway call needed - gate 3 (not-reactor) will be closed
     (setq obj-align obj-target)
     (cond
       ((or (= auto-type "Sta")
            (= auto-type "Off")
            (= auto-type "StaOff")
            (= auto-type "StaName")
        )
        (setq
          bubble-data
           (hcnm-ldrblk-bubble-data-ensure-p1-world
             bubble-data
           )
          p1-world
           (hcnm-ldrblk-bubble-data-get bubble-data "p1-world")
        )
       )
     )
    )
    (t
     ;; Initial creation - get alignment from user (they're picking it NOW or reusing previous)
     (setq
       pspace-bubble-p
        (hcnm-ldrblk-space-set-model)
       obj-align
        (hcnm-ldrblk-auto-al-get-alignment
          ename-bubble
          tag
          auto-type
        )
     )
     ;; Now calculate p1-world if needed for coordinate-based types
     (cond
       ((or (= auto-type "Sta")
            (= auto-type "Off")
            (= auto-type "StaOff")
            (= auto-type "StaName")
        )
        (setq
          bubble-data
           (hcnm-ldrblk-bubble-data-ensure-p1-world
             bubble-data
           )
          p1-world
           (hcnm-ldrblk-bubble-data-get bubble-data "p1-world")
        )
       )
     )
     ;; Attach reactor to watch for alignment/leader changes
     (hcnm-ldrblk-assure-auto-text-has-reactor
       obj-align ename-bubble ename-leader tag auto-type
      )
    )
  )
  ;; STEP 2: Calculate station and offset (only needed for coordinate-based types)
  (cond
    ((or (= auto-type "Sta")
         (= auto-type "Off")
         (= auto-type "StaOff")
         (= auto-type "StaName")
     )
     (setq
       sta-off-pair
        (hcnm-ldrblk-auto-alignment-calculate
          obj-align
          p1-world
        )
     )
    )
  )
  ;; STEP 3: Format the result based on auto-type
  (cond
    ((= auto-type "AlName")
     ;; Alignment name only - no coordinates needed
     (cond
       (obj-align
        (setq
          string
           (vl-catch-all-apply
             'vlax-get-property
             (list obj-align 'name)
           )
        )
        (cond
          ((vl-catch-all-error-p string)
           (setq
             string
              "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
           )
          )
        )
       )
       (t
        (setq
          string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
        )
       )
     )
    )
    (sta-off-pair
     ;; Calculation succeeded - extract and format
     (setq
       drawstation
        (car sta-off-pair)
       offset
        (cdr sta-off-pair)
       sta-string
        (hcnm-ldrblk-auto-al-station-to-string
          obj-align
          drawstation
        )
       off-string
        (hcnm-ldrblk-auto-al-offset-to-string offset)
       string
        (cond
          ((= auto-type "Sta") sta-string)
          ((= auto-type "Off") off-string)
          ((= auto-type "StaOff")
           (strcat
             sta-string
             (hcnm-config-getvar "BubbleTextJoinDelSta")
             off-string
           )
          )
          ((= auto-type "StaName")
           ;; Station + alignment name
           (setq
             string
              (vl-catch-all-apply
                'vlax-get-property
                (list obj-align 'name)
              )
           )
           (cond
             ((vl-catch-all-error-p string)
              (setq string sta-string)  ; If name fails, just use station
             )
             (t (setq string (strcat sta-string " " string)))
           )
           string
          )
        )                               ; End inner COND for STRING
     )                                  ; End SETQ
    )                                   ; End first branch of outer COND
    (t
     ;; Calculation failed - couldn't get coordinates or invalid alignment
     (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
    )
  )                                     ; End outer COND
  ;; Step 4: Save the formatted string to the attribute list and update bubble-data
  (setq
    lattribs
     (hcnm-ldrblk-lattribs-put-auto
       tag
       string
       lattribs
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  ;; Step 5: Write XDATA during initial insertion (not during reactor updates)
  ;; Reactor updates already write XDATA in update-bubble-tag
  (cond
    ((and (not obj-target) obj-align)  ; Initial insertion AND alignment found
     (hcnm-ldrblk-xdata-update-one
       ename-bubble
       tag
       auto-type
       (vla-get-handle obj-align)
       string
      )
     ;; Restore space after everything is done
     (hcnm-ldrblk-space-restore pspace-bubble-p)
    )
  )
  ;;===========================================================================
  ;; PROFILING: End timing alignment auto-text generation
  ;;===========================================================================
  (haws-profile-end "insert-auto-alignment" profile-start)
  bubble-data
)
(defun hcnm-ldrblk-auto-al-get-alignment (ename-bubble tag auto-type /
                                      avport cvport es-align name
                                      obj-align obj-align-old ref-ocs-1
                                      ref-ocs-2 ref-ocs-3 ref-wcs-1
                                      ref-wcs-2 ref-wcs-3
                                     )
  (setq
    obj-align-old
     (hcnm-config-getvar "BubbleCurrentAlignment")
    name
     (cond
       ((and
          (= (type obj-align-old) 'vla-object)
          (not
            (vl-catch-all-error-p
              (vl-catch-all-apply
                'vlax-get-property
                (list obj-align-old 'name)
              )
            )
          )
        )
        (vl-catch-all-apply
          'vlax-get-property
          (list obj-align-old 'name)
        )
       )
       (t (setq obj-align-old nil) "")
     )
    es-align
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
  (hcnm-ldrblk-gateways-to-viewport-selection-prompt
    ename-bubble
    auto-type
    nil                                 ; obj-target=nil for initial creation
    (if es-align
      "PICKED"
      "REUSED"
    )                                   ; Based on whether user selected something
    nil
  )                                     ; Normal auto-text flow (not super-clearance)
  (cond
    ((and
       es-align
       (= (cdr (assoc 0 (entget (car es-align)))) "AECC_ALIGNMENT")
     )
     (setq obj-align (vlax-ename->vla-object (car es-align)))
     (hcnm-config-setvar "BubbleCurrentAlignment" obj-align)
    )
    (es-align
     (alert
       (princ
         "\nSelected object is not an alignment. Keeping previous alignment."
       )
     )
     (setq obj-align obj-align-old)
    )
    (t
     (princ "\nNo object selected. Keeping previous alignment.")
     (setq obj-align obj-align-old)
    )
  )
  obj-align                             ; Return the alignment object
)
;#endregion
;#region Auto NE
(defun hcnm-ldrblk-auto-ne (bubble-data tag auto-type obj-target / lattribs
                        e ename-bubble ename-leader n ne p1-ocs p1-world
                        reactor-update-p string
                       )
  (setq
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    ename-leader
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-leader"
     )
    ;; obj-target = NIL means initial creation
    ;; obj-target = T means reactor update for coordinate types (sentinel value)
    ;; obj-target = VLA-OBJECT means reactor update with reference object (not used for N/E/NE)
    reactor-update-p
     (and
       obj-target
       (or (= obj-target t)
           (= (type obj-target) 'vla-object)
       )
     )
  )
  ;; CRITICAL: Check for leader BEFORE gateway call
  ;; Gateway tries to capture viewport, which is pointless without a leader
  (cond
    ((not ename-leader)
     ;; No leader - can't calculate coordinates
     (haws-tip
       6  ; Tip ID from haws-tip-registry.csv: "No leader for coordinate-based auto-text"
       (strcat
         "Cannot calculate coordinates for this bubble note!"
         "\n"
         "\nThis bubble has no associated leader."
         "\nCoordinate-based auto-text (N/E/NE/Sta/Off) requires a leader"
         "\nto determine the point location."
         "\n"
         "\nPossible causes:"
         "\n  • Bubble was inserted manually (not via CNM commands)"
         "\n  • Leader was deleted after bubble creation"
         "\n  • Bubble was copied without its leader"
         "\n"
         "\nSolution:"
         "\n  • Use CNM insertion commands (BOXL, CIRL, etc.) which create"
         "\n    bubble and leader together"
         "\n  • Or use non-coordinate auto-text types (text, mtext, quantities)"
       )
     )
     ;; Set string to NOT FOUND and skip all coordinate calculation
     (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
    )
    (t
     ;; Normal flow - has leader, proceed with gateway and coordinate calculation
     ;; Ensure viewport transform is captured if needed (gateway architecture)
     ;; MUST happen BEFORE p1-world calculation below, which depends on viewport transform
     (haws-debug ">>> DEBUG: Before gateway call")
     (hcnm-ldrblk-gateways-to-viewport-selection-prompt
    ;; N/E/NE don't use reference objects
    ename-bubble auto-type obj-target "NO-OBJECT"
    ;; Normal auto-text flow (not super-clearance)                               
    nil
   )                                    
  (haws-debug ">>> DEBUG: After gateway call")
  ;; Calculate or get p1-world
  (cond
    (reactor-update-p
     ;; Reactor update - recalculate p1-world from current leader position using stored transformation
     (haws-debug ">>> DEBUG: Reactor update path")
     (setq p1-ocs (hcnm-ldrblk-p1-ocs ename-leader))
     (haws-debug (list ">>> DEBUG: p1-ocs=" (vl-princ-to-string p1-ocs)))
     (setq
       p1-world
        (hcnm-ldrblk-p1-world ename-leader p1-ocs ename-bubble)
     )
     (haws-debug (list ">>> DEBUG: p1-world=" (vl-princ-to-string p1-world)))
    )
    (t
     ;; Initial creation - ensure p1-world is calculated (now that viewport transform is in XDATA)
     (haws-debug ">>> DEBUG: Initial creation path - calling ensure-p1-world")
     (setq
       bubble-data
        (hcnm-ldrblk-bubble-data-ensure-p1-world bubble-data)
       p1-world
        (hcnm-ldrblk-bubble-data-get bubble-data "p1-world")
     )
     (haws-debug (list ">>> DEBUG: After ensure-p1-world, p1-world=" (vl-princ-to-string p1-world)))
    )
  )
  ;; Calculate coordinates from p1-world
  (haws-debug ">>> DEBUG: Before coordinate calculation")
  (haws-debug (list ">>> DEBUG: p1-world value=" (vl-princ-to-string p1-world)))
  (cond
    (p1-world
     (haws-debug ">>> DEBUG: p1-world exists, calculating N/E")
     (haws-debug (list ">>> DEBUG: (car p1-world)=" (vl-princ-to-string (car p1-world))))
     (haws-debug (list ">>> DEBUG: (cadr p1-world)=" (vl-princ-to-string (cadr p1-world))))
     (haws-debug ">>> DEBUG: About to call hcnm-ldrblk-auto-rtos for N")
     (setq
       n  (hcnm-ldrblk-auto-rtos (cadr p1-world) "N")
     )
     (haws-debug (list ">>> DEBUG: N calculated=" n))
     (haws-debug ">>> DEBUG: About to call hcnm-ldrblk-auto-rtos for E")
     (setq
       e  (hcnm-ldrblk-auto-rtos (car p1-world) "E")
     )
     (haws-debug (list ">>> DEBUG: E calculated=" e))
     (haws-debug ">>> DEBUG: About to concatenate NE string")
     (setq
       ne (strcat
            n
            (hcnm-config-getvar (strcat "BubbleTextJoinDel" "N"))
            e
          )
     )
     (haws-debug (list ">>> DEBUG: NE concatenated=" ne))
     (haws-debug ">>> DEBUG: About to select final string based on auto-type")
     (setq
       string
        (cond
          ((= auto-type "N") n)
          ((= auto-type "E") e)
          ((= auto-type "NE") ne)
        )
     )
     (haws-debug (list ">>> DEBUG: Final string selected=" string))
    )
    (t
     ;; p1-world is NIL - couldn't get world coordinates
     (setq string "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
    )
  )
  ;; Attach reactor to leader for coordinate updates (only on initial creation, not reactor updates)
  (cond
    ((not reactor-update-p)
     (hcnm-ldrblk-assure-auto-text-has-reactor
       nil ename-bubble ename-leader tag auto-type
      )
     ;; Write initial XDATA in composite key format (reactor updates need this)
     ;; Reference handle is "" for coordinate-based auto-text (no reference object)
     (hcnm-ldrblk-xdata-update-one ename-bubble tag auto-type "" string)
    )
  )
     )  ; End (t ...) branch - normal flow with leader
  )  ; End outer (cond ...) - leader check
  ;; END hcnm-ldrblk-auto-get-input SUBFUNCTION
  ;; START hcnm-ldrblk-auto-update SUBFUNCTION
  (setq
    lattribs
     (hcnm-ldrblk-lattribs-put-auto
       tag
       string
       lattribs
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  bubble-data
)
;#endregion
;#region Auto pipe
;; ============================================================================
;; Civil 3D Pipe Network Auto-Text Functions
;; ============================================================================

;;==============================================================================
;; hcnm-ldrblk-auto-pipe-get-object
;;==============================================================================
;; Purpose:
;;   Prompts user to select a Civil 3D pipe network pipe object.
;;
;; Arguments:
;;   ename-bubble - Entity name of bubble being edited (required)
;;   TAG - Attribute tag being updated (required)
;;   auto-type - Property type: "Dia", "Slope", or "L" (required)
;;
;; Returns:
;;   VLA-OBJECT of selected pipe, or NIL if selection failed
;;
;; Side Effects:
;;   - Prompts user to select pipe with custom message
;;
;; Related:
;;   hcnm-ldrblk-auto-pipe
;;
;; Example:
;;   (SETQ obj-pipe (hcnm-ldrblk-auto-pipe-get-object ename-bubble "NOTETXT1" "Dia"))
;;==============================================================================
(defun hcnm-ldrblk-auto-pipe-get-object
   (ename-bubble tag auto-type / esapipe obj-pipe)
  (setq
    esapipe
     (nentsel
       (strcat
         "\nSelect Civil 3D pipe for "
         (cond
           ((= auto-type "Dia") "diameter")
           ((= auto-type "Slope") "slope")
           ((= auto-type "L") "length")
           (t "property")
         )
         ": "
       )
     )
  )
  (cond
    (esapipe
     (setq
       obj-pipe
        (vl-catch-all-apply
          'vlax-ename->vla-object
          (list (car esapipe))
        )
     )
     (cond
       ((vl-catch-all-error-p obj-pipe)
        (princ "\nError: Could not get pipe object.")
        nil
       )
       (t obj-pipe)
     )
    )
    (t nil)
  )
)

;;==============================================================================
;; hcnm-ldrblk-auto-pipe-dia-to-string
;;==============================================================================
;; Purpose:
;;   Formats pipe diameter value with user-configured prefix, postfix, and precision.
;;   Converts from Civil 3D units (feet) to inches for display.
;;
;; Arguments:
;;   obj-pipe - VLA-OBJECT of Civil 3D pipe (required)
;;
;; Returns:
;;   Formatted string (e.g., "12 IN", "18\"") or error string if property unavailable
;;
;; Side Effects:
;;   - Reads config variables: BubbleTextPrefixPipeDia, PostfixPipeDia, PrecisionPipeDia
;;
;; Related:
;;   hcnm-ldrblk-auto-pipe
;;   hcnm-ldrblk-auto-pipe-slope-to-string
;;
;; Example:
;;   (SETQ TEXT (hcnm-ldrblk-auto-pipe-dia-to-string obj-pipe))
;;==============================================================================
(defun hcnm-ldrblk-auto-pipe-dia-to-string
   (obj-pipe / dia-value dia-inches)
  (setq
    dia-value
     (vl-catch-all-apply
       'vlax-get-property
       (list obj-pipe 'innerdiameterorwidth)
     )
  )
  (cond
    ((vl-catch-all-error-p dia-value)
     (princ
       (strcat
         "\nError getting diameter: "
         (vl-princ-to-string dia-value)
       )
     )
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
    )
    (t
     ;; Civil 3D returns diameter in drawing units (typically feet for US)
     ;; Convert to inches for display
     (setq dia-inches (* dia-value 12.0))
     (strcat
       (hcnm-config-getvar "BubbleTextPrefixPipeDia")
       (rtos
         dia-inches
         2
         (atoi (hcnm-config-getvar "BubbleTextPrecisionPipeDia"))
       )
       (hcnm-config-getvar "BubbleTextPostfixPipeDia")
     )
    )
  )
)

;;==============================================================================
;; hcnm-ldrblk-auto-pipe-slope-to-string
;;==============================================================================
;; Purpose:
;;   Formats pipe slope value with user-configured prefix, postfix, and precision.
;;   Converts from Civil 3D units (decimal) to percentage for display.
;;
;; Arguments:
;;   obj-pipe - VLA-OBJECT of Civil 3D pipe (required)
;;
;; Returns:
;;   Formatted string (e.g., "2.00%", "0.5%") or error string if property unavailable
;;
;; Side Effects:
;;   - Reads config variables: BubbleTextPrefixPipeSlope, PostfixPipeSlope, PrecisionPipeSlope
;;
;; Related:
;;   hcnm-ldrblk-auto-pipe
;;   hcnm-ldrblk-auto-pipe-dia-to-string
;;
;; Example:
;;   (SETQ TEXT (hcnm-ldrblk-auto-pipe-slope-to-string obj-pipe))
;;==============================================================================
(defun hcnm-ldrblk-auto-pipe-slope-to-string
   (obj-pipe / slope-value slope-percent)
  (setq
    slope-value
     (vl-catch-all-apply
       'vlax-get-property
       (list obj-pipe 'slope)
     )
  )
  (cond
    ((vl-catch-all-error-p slope-value)
     (princ
       (strcat
         "\nError getting slope: "
         (vl-princ-to-string slope-value)
       )
     )
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
    )
    (t
     ;; Civil 3D returns slope as decimal (e.g., 0.02 for 2%)
     ;; Convert to percentage for display (take absolute value)
     (setq slope-percent (* (abs slope-value) 100.0))
     (strcat
       (hcnm-config-getvar "BubbleTextPrefixPipeSlope")
       (rtos
         slope-percent
         2
         (atoi (hcnm-config-getvar "BubbleTextPrecisionPipeSlope"))
       )
       (hcnm-config-getvar "BubbleTextPostfixPipeSlope")
     )
    )
  )
)

;;==============================================================================
;; hcnm-ldrblk-auto-pipe-length-to-string
;;==============================================================================
;; Purpose:
;;   Formats pipe length value with user-configured prefix, postfix, and precision.
;;   Uses 2D length from Civil 3D (horizontal projection).
;;
;; Arguments:
;;   obj-pipe - VLA-OBJECT of Civil 3D pipe (required)
;;
;; Returns:
;;   Formatted string (e.g., "L=125.50", "125.5'") or error string if property unavailable
;;
;; Side Effects:
;;   - Reads config variables: BubbleTextPrefixPipeLength, PostfixPipeLength, PrecisionPipeLength
;;
;; Related:
;;   hcnm-ldrblk-auto-pipe
;;   hcnm-ldrblk-auto-pipe-dia-to-string
;;
;; Example:
;;   (SETQ TEXT (hcnm-ldrblk-auto-pipe-length-to-string obj-pipe))
;;==============================================================================
(defun hcnm-ldrblk-auto-pipe-length-to-string (obj-pipe / length-value)
  (setq
    length-value
     (vl-catch-all-apply
       'vlax-get-property
       (list obj-pipe 'length2d)
     )
  )
  (cond
    ((vl-catch-all-error-p length-value)
     (princ
       (strcat
         "\nError getting length: "
         (vl-princ-to-string length-value)
       )
     )
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
    )
    (t
     (strcat
       (hcnm-config-getvar "BubbleTextPrefixPipeLength")
       (rtos
         length-value
         2
         (atoi
           (hcnm-config-getvar "BubbleTextPrecisionPipeLength")
         )
       )
       (hcnm-config-getvar "BubbleTextPostfixPipeLength")
     )
    )
  )
)

;;==============================================================================
;; hcnm-ldrblk-auto-pipe
;;==============================================================================
;; Purpose:
;;   Main pipe network auto-text orchestrator. Gets pipe object, extracts
;;   specified property (diameter/slope/length), formats with config settings,
;;   and attaches reactor for automatic updates when pipe changes.
;;
;; Arguments:
;;   bubble-data - Bubble data alist (required)
;;   TAG - Attribute tag to update (required)
;;   auto-type - Property type: "Dia", "Slope", or "L" (required)
;;   obj-target - Pre-selected pipe VLA-OBJECT (optional, for reactor callbacks)
;;
;; Returns:
;;   Updated bubble-data with new attribute value
;;
;; Side Effects:
;;   - Prompts user for pipe selection if obj-target is NIL
;;   - Attaches VLR-OBJECT-REACTOR to pipe for automatic updates
;;   - Switches to model space temporarily if bubble is in paper space
;;   - Updates lattribs within bubble-data
;;
;; Related:
;;   hcnm-ldrblk-auto-pipe-get-object
;;   hcnm-ldrblk-auto-pipe-dia-to-string
;;   hcnm-ldrblk-auto-pipe-slope-to-string
;;   hcnm-ldrblk-auto-pipe-length-to-string
;;   hcnm-ldrblk-assure-auto-text-has-reactor
;;
;; Example:
;;   (SETQ bubble-data
;;     (hcnm-ldrblk-auto-pipe bubble-data "NOTETXT1" "Dia" NIL)
;;   )
;;==============================================================================
(defun hcnm-ldrblk-auto-pipe (bubble-data tag auto-type obj-target /
                          lattribs ename-bubble ename-leader obj-pipe
                          pspace-bubble-p string profile-start
                         )
  ;;===========================================================================
  ;; PROFILING: Start timing pipe auto-text generation
  ;;===========================================================================
  (setq profile-start (haws-profile-start "insert-auto-pipe"))
  (setq
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
    ename-leader
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-leader"
     )
  )
  ;; NOTE: Pipe auto-text (Dia/Slope/L) does not use world coordinates.
  ;; At this point, coordinate-based auto-text functions get world coordinates
  ;; and the associated viewport as needed (via gateway + p1-world helpers).
  ;; STEP 1: Get pipe object (user selection or provided obj-target)
  (cond
    ((setq obj-pipe obj-target)
     ;; Pipe provided (reactor update or programmatic call)
    )
    (t
     ;; Get pipe from user
     (setq
       pspace-bubble-p
        (hcnm-ldrblk-space-set-model)
       obj-pipe
        (hcnm-ldrblk-auto-pipe-get-object
          ename-bubble
          tag
          auto-type
        )
     )
     ;; Attach reactor to watch for pipe changes (no leader needed since not coordinate-based)
     (cond
       (obj-pipe
        (hcnm-ldrblk-assure-auto-text-has-reactor
          obj-pipe ename-bubble nil tag auto-type
         )
       )
     )
    )
  )
  ;; STEP 2: Extract and format the property based on auto-type
  (setq
    string
     (cond
       ((not obj-pipe)
        "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
       )
       ((= auto-type "Dia")
        (hcnm-ldrblk-auto-pipe-dia-to-string obj-pipe)
       )
       ((= auto-type "Slope")
        (hcnm-ldrblk-auto-pipe-slope-to-string obj-pipe)
       )
       ((= auto-type "L")
        (hcnm-ldrblk-auto-pipe-length-to-string obj-pipe)
       )
       (t "!!!!!!!!!!!!!!!!!INVALID TYPE!!!!!!!!!!!!!!!!!!!!!!!")
     )
  )
  ;; STEP 3: Save the formatted string to the attribute list and update bubble-data
  (setq
    lattribs
     (hcnm-ldrblk-lattribs-put-auto
       tag
       string
       lattribs
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  ;; STEP 4: Write XDATA during initial insertion (not during reactor updates)
  ;; Reactor updates already write XDATA in update-bubble-tag
  (cond
    ((and (not obj-target) obj-pipe)  ; Initial insertion AND pipe found
     (hcnm-ldrblk-xdata-update-one
       ename-bubble
       tag
       auto-type
       (vla-get-handle obj-pipe)
       string
      )
     ;; Restore space after everything is done
     (hcnm-ldrblk-space-restore pspace-bubble-p)
    )
  )
  ;;===========================================================================
  ;; PROFILING: End timing pipe auto-text generation
  ;;===========================================================================
  (haws-profile-end "insert-auto-pipe" profile-start)
  bubble-data
)
;#endregion
;#region Auto surface
;; Civil 3D Surface query auto-text (Z elevation)
;; Currently unimplemented - returns apology message
(defun hcnm-ldrblk-auto-su (bubble-data tag auto-type obj-target / lattribs
                        ename-bubble
                       )
  (setq
    lattribs
     (hcnm-ldrblk-bubble-data-get bubble-data "ATTRIBUTES")
    ename-bubble
     (hcnm-ldrblk-bubble-data-get
       bubble-data
       "ename-bubble"
     )
  )
  ;; Ensure viewport transform is captured if needed (gateway architecture)
  ;; FUTURE FEATURE: When Z elevation is implemented, this will be needed for coordinate calculations
  (hcnm-ldrblk-gateways-to-viewport-selection-prompt
    ename-bubble auto-type obj-target "NO-OBJECT"
                                        ; Z elevation doesn't use reference objects
    nil
   )                                    ; Normal auto-text flow (not super-clearance)
  ;; FUTURE FEATURE: When Z implemented, calculate p1-world here after gateway call
  ;; (setq bubble-data (hcnm-ldrblk-bubble-data-ensure-p1-world bubble-data))
  ;; END hcnm-ldrblk-auto-get-input SUBFUNCTION
  ;; START hcnm-ldrblk-auto-update SUBFUNCTION
  (setq
    lattribs
     (hcnm-ldrblk-lattribs-put-auto
       tag
       (hcnm-ldrblk-auto-apology auto-type)
       lattribs
       ename-bubble
     )
    bubble-data
     (hcnm-ldrblk-bubble-data-set
       bubble-data
       "ATTRIBUTES"
       lattribs
     )
  )
  bubble-data
)
;#endregion
;#region Auto helpers
;;==============================================================================
;; SHARED AUTO-TEXT UTILITIES
;;==============================================================================
;; Format number with config prefix/postfix (used by all numeric auto-text types)
(defun hcnm-ldrblk-auto-rtos (number key)
  (strcat
    (hcnm-config-getvar (strcat "BubbleTextPrefix" key))
    (rtos
      number
      2
      (atoi
        (hcnm-config-getvar (strcat "BubbleTextPrecision" key))
      )
    )
    (hcnm-config-getvar (strcat "BubbleTextPostfix" key))
  )
)

;; Show apology for unimplemented auto-text types
(defun hcnm-ldrblk-auto-apology (auto-type)
  (alert
    (princ
      (strcat
        "Sorry. Selection of "
        auto-type
        " is not fully programmed yet and is not anticipated to be dynamic once programmed.\n\nPlease let Tom Haws <tom.haws@gmail.com> know if you are eager for this as static text."
      )
    )
  )
  "N/A"
)

;;==============================================================================
;; PAPER SPACE / MODEL SPACE MANAGEMENT
;;==============================================================================
;; These functions manage switching between paper space and model space during
;; auto-text operations. They are called by different auto-text types for
;; different purposes:
;;
;; CALLERS AND PURPOSE:
;; - hcnm-ldrblk-auto-qty   : Switches to MSPACE for reference object selection
;; - hcnm-ldrblk-auto-pipe  : Switches to MSPACE for pipe object selection  
;; - hcnm-ldrblk-auto-al    : Switches to MSPACE for alignment object selection
;; - hcnm-ldrblk-auto-ne    : Does NOT call (no reference object selection needed)
;;
;; CRITICAL DISTINCTION:
;; This space switching is for REFERENCE OBJECT SELECTION, not viewport selection.
;; Viewport selection (for AVPORT capture) happens separately through the gateway
;; system (hcnm-ldrblk-gateways-to-viewport-selection-prompt) and is needed by
;; ALL coordinate-based auto-text types including N/E/NE.
;;
;; WHY auto-ne DOESN'T CALL THESE:
;; N/E/NE auto-text has no reference objects to select (it calculates coordinates
;; directly from p1-world). It still needs viewport transform for paper space
;; coordinate conversion, but that's handled by the gateway system.
;;==============================================================================

(defun hcnm-ldrblk-space-set-model ()
  (cond ((= (getvar "CVPORT") 1) (vl-cmdf "._MSPACE") t))
)
(defun hcnm-ldrblk-space-restore (pspace-bubble-p /)
  (cond (pspace-bubble-p (vl-cmdf "._PSPACE")))
)
;#endregion
;#region Auto text user experience interruptions
;;==============================================================================
;; WORLD COORDINATES AUTO-TEXT VALIDATION AND WARNING SYSTEM
;;==============================================================================
;; These functions determine if an auto-text type requires world coordinates and
;; warn users about paper space viewport behavior to prevent drawing issues.

;; Check if auto-type is world-coordinate-based using get-auto-type-keys
;; Returns: T if auto-type requires world coordinates (Sta/Off/N/E/Z), NIL otherwise
;; Usage: It's one of the gateways to pass before showing a paper space warning.
(defun hcnm-ldrblk-auto-type-is-coordinate-p (auto-type / type-def)
  (setq
    type-def
     (car
       (vl-remove-if-not
         '(lambda (x) (= (cadr x) auto-type))
         (hcnm-ldrblk-get-auto-type-keys)
       )
     )
  )
  (cond
    (type-def (cadddr type-def))        ; Return 4th element (requires-coordinates)
    (t nil)
  )
)
;; User experience interruptions when getting coordinate-based auto-text 
                                        ;  1. Display educational warning about paper space coordinate translation behavior
;; Why: Warns users that we've decided not to react to changing viewport views.
;; When: When is not critical since this is only an educational tip.
;; It's natural to show when we get coordinate-based auto-text for paper space bubbles
;; 2. Capture and store viewport transformation matrix for paper space bubble
;; Why: For bubble Reactor can't recalculate any world-coordinate-based auto text without a transformation
;; This is the ONLY function that should call hcnm-ldrblk-set-viewport-transform-xdata
;; All viewport capture logic is centralized here to maintain architectural clarity
;;
;; ARCHITECTURE: Two user experiences call this function:
;;   1. Auto-text generation (via hcnm-ldrblk-auto-al and similar dispatch-auto functions)
;;      - When inserting bubble with coordinate-based auto-text in paper space
;;      - When editing bubble and switching to coordinate-based auto-text  
;;   2. Viewport linking (explicit user actions)
;;      - "Change View" button in edit dialog
;;      - Future: CNMCHGVPORT command for selection sets (TODO)
;;
;; This captures 3 reference points to calculate rotation, scale, and translation
;; Returns T if successful, NIL if failed
(defun hcnm-ldrblk-capture-viewport-transform (ename-bubble cvport /
                                           ref-ocs-1 ref-ocs-2 ref-ocs-3
                                           ref-wcs-1 ref-wcs-2 ref-wcs-3
                                          )
  (cond
    ((and cvport (> cvport 1))
     ;; We're in a viewport - capture transformation matrix
     ;; Use 3 reference points: origin, X-axis unit vector, Y-axis unit vector
     (setq
       ref-ocs-1
        '(0.0 0.0 0.0)                  ; Origin
       ref-ocs-2
        '(1.0 0.0 0.0)                  ; X-axis unit vector
       ref-ocs-3
        '(0.0 1.0 0.0)                  ; Y-axis unit vector
       ref-wcs-1
        (trans (trans ref-ocs-1 3 2) 2 0)
       ref-wcs-2
        (trans (trans ref-ocs-2 3 2) 2 0)
       ref-wcs-3
        (trans (trans ref-ocs-3 3 2) 2 0)
     )
     (hcnm-ldrblk-set-viewport-transform-xdata
       ename-bubble cvport ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2
       ref-ocs-3 ref-wcs-3
      )
     (haws-debug
       (list
         "Stored viewport "
         (itoa cvport)
         " transformation matrix"
       )
     )
     t                                  ; Success
    )
    (t nil)                             ; Failed - not in a viewport
  )
)

;; Queue a paper space coordinate warning tip to be shown after any modal dialogs close
;; This is necessary because you can't show a modal dialog from inside another modal dialog
;; Show paper space coordinate warning tip
;; Can be called from anywhere - shows tip immediately
(defun hcnm-ldrblk-tip-explain-avport-selection (ename-bubble auto-type /)
  (cond
    ((and
       ename-bubble
       (not (hcnm-ldrblk-is-on-model-tab ename-bubble))
       (hcnm-ldrblk-auto-type-is-coordinate-p auto-type)
     )
     ;; Bubble is in paper space and auto-type is coordinate-based - show warning
     (haws-tip
       4                                ; Unique tip ID for AVPORT selection explanation
       "You must now tell CNM which viewport this bubble note belongs to. Every new bubble note with a coordinate-based auto-text needs this action when you choose to use the previous reference object.\n\nThis is a little faster than selecting the reference object again because you don't need to pay close attention."
     )
    )
  )
)

;; Queue a paper space coordinate warning tip to be shown after any modal dialogs close
;; This is necessary because you can't show a modal dialog from inside another modal dialog
;; Show paper space coordinate warning tip
;; Can be called from anywhere - shows tip immediately
(defun hcnm-ldrblk-tip-warn-pspace-no-react (ename-bubble auto-type /)
  (cond
    ((and
       ename-bubble
       (not (hcnm-ldrblk-is-on-model-tab ename-bubble))
       (hcnm-ldrblk-auto-type-is-coordinate-p auto-type)
     )
     ;; Bubble is in paper space and auto-type is coordinate-based - show warning
     (haws-tip
       2                                ; Unique tip ID for paper space warning
       "ALERT: CNM doesn't adjust paper space bubble notes when viewports change.\n\nTo avoid causing chaos when viewports change, auto text for coordinates does not react to viewport view changes.\n\nYou must use the 'Change View' button in the edit dialog (or the future CNMCHGVPORT command) if you want to refresh the viewport association and world coordinates of selected bubble notes."
     )
    )
  )
)
;#endregion
;#endregion
;#region Associate viewport
;; hcnm-ldrblk-gateways-to-viewport-selection-prompt - Gateway architecture for AVPORT prompting
;;
;; SIDE EFFECT PROCEDURE (returns nil): Determines whether to prompt user for viewport, 
;; use CVPORT, or skip viewport capture entirely. Uses 5 named boolean gateways that 
;; must all be open to prompt. Super-clearance bypasses all gates.
;;
;; When this function executes a capture path, it calls hcnm-ldrblk-capture-viewport-transform
;; which stores the viewport transformation matrix in the bubble's XDATA. This stored
;; transform allows coordinate calculations to convert from OCS to WCS correctly.
;;
;; PARAMETERS:
;;   ename-bubble - Entity name of bubble block reference
;;   auto-type - String: "N", "E", "NE", "Sta", "Off", etc. (for gateway 1 check and warning)
;;   obj-target - Object reference (alignment) or nil
;;   object-reference-status - String indicating how object was obtained:
;;     "NO-OBJECT" - No object needed/used (N/E/NE)
;;     "PICKED" - User just picked object in this session
;;     "REUSED" - Using object from previous session (bubble-data)
;;   request-type - String indicating context:
;;     nil - Normal auto-text generation flow
;;     "LINK-VIEWPORT" - Explicit user request (super-clearance)
;;
;; THREE OUTCOMES (via side effects):
;;   1. Prompt user for viewport, then capture transform
;;   2. Use current CVPORT without prompting, then capture
;;   3. Skip entirely (no capture needed)
;;
;; RETURNS: nil (this is a procedure with side effects, not a value-returning function)
;;
(defun hcnm-ldrblk-gateways-to-viewport-selection-prompt
   (ename-bubble auto-type obj-target object-reference-status
    request-type / avport-coordinates-gateway-open-p
    avport-paperspace-gateway-open-p avport-reactor-gateway-open-p
    avport-xdata-gateway-open-p avport-object-gateway-open-p
    has-super-clearance-p cvport
   )
  ;; Gateway 1: Coordinate-based auto-text
  (setq
    avport-coordinates-gateway-open-p
     (hcnm-ldrblk-auto-type-is-coordinate-p
       auto-type
     )
  )
  (haws-debug
    (list
      "  Gateway 1 (coordinates): "
      (if avport-coordinates-gateway-open-p
        "OPEN"
        "CLOSED"
      )
      " ["
      (if auto-type
        auto-type
        "nil"
      )
      "]"
    )
  )
  ;; Gateway 2: Paper space (not model space)
  (setq
    avport-paperspace-gateway-open-p
     (and
       ename-bubble
       (not
         (hcnm-ldrblk-is-on-model-tab
           ename-bubble
         )
       )
     )
  )
  (haws-debug
    (list
      "  Gateway 2 (paperspace): "
      (if avport-paperspace-gateway-open-p
        "OPEN"
        "CLOSED"
      )
    )
  )
  ;; Gateway 3: Not a reactor update  (obj-target is nil during insertion/editing)
  (setq avport-reactor-gateway-open-p (not obj-target))
  (haws-debug
    (list
      "  Gateway 3 (not-reactor): "
      (if avport-reactor-gateway-open-p
        "OPEN"
        "CLOSED"
      )
      " [input="
      (if obj-target
        "exists"
        "nil"
      )
      "]"
    )
  )
  ;; Gateway 4: No existing viewport XDATA
  (setq
    avport-xdata-gateway-open-p
     (not
       (hcnm-xdata-get-vptrans ename-bubble)
     )
  )
  (haws-debug
    (list
      "  Gateway 4 (no-xdata): "
      (if avport-xdata-gateway-open-p
        "OPEN"
        "CLOSED"
      )
    )
  )
  ;; Gateway 5: Object not picked (either no object, or reused from previous)
  ;; When user picks an object in this session, we trust CVPORT without prompting
  (setq
    avport-object-gateway-open-p
     (not
       (equal
         object-reference-status
         "PICKED"
       )
     )
  )
  (haws-debug
    (list
      "  Gateway 5 (not-picked): "
      (if avport-object-gateway-open-p
        "OPEN"
        "CLOSED"
      )
      " [status="
      (if object-reference-status
        object-reference-status
        "nil"
      )
      "]"
    )
  )
  ;; Super clearance: Explicit user request bypasses all gates
  (setq has-super-clearance-p (equal request-type "LINK-VIEWPORT"))
  (haws-debug
    (list
      "  Super clearance: "
      (if has-super-clearance-p
        "YES"
        "no"
      )
      " [request="
      (if request-type
        request-type
        "nil"
      )
      "]"
    )
  )
  ;; Decision logic
  (cond
    ;; Path 1: Super clearance - always prompt
    (has-super-clearance-p
     (haws-debug "  >>> DECISION: Prompt for viewport (super clearance)")
     (hcnm-ldrblk-tip-explain-avport-selection
       ename-bubble
       auto-type
     )
     (hcnm-ldrblk-capture-viewport-transform
       ename-bubble
       (hcnm-ldrblk-get-target-vport)
     )
     (hcnm-ldrblk-tip-warn-pspace-no-react
       ename-bubble
       auto-type
     )
    )
    ;; Path 2: All gates open - prompt user
    ((and
       avport-coordinates-gateway-open-p
       avport-paperspace-gateway-open-p avport-reactor-gateway-open-p
       avport-xdata-gateway-open-p avport-object-gateway-open-p
      )
     (haws-debug "  >>> DECISION: Prompt for viewport (all gates open)")
     (hcnm-ldrblk-tip-explain-avport-selection
       ename-bubble
       auto-type
     )
     (hcnm-ldrblk-capture-viewport-transform
       ename-bubble
       (hcnm-ldrblk-get-target-vport)
     )
     (hcnm-ldrblk-tip-warn-pspace-no-react
       ename-bubble
       auto-type
     )
    )
    ;; Path 3: Only object gateway closed - use CVPORT without prompting
    ((and
       avport-coordinates-gateway-open-p
       avport-paperspace-gateway-open-p
       avport-reactor-gateway-open-p
       avport-xdata-gateway-open-p
       (not avport-object-gateway-open-p)
     )
     (haws-debug "  >>> DECISION: Use CVPORT silently (object just picked)")
     (setq cvport (getvar "CVPORT"))
     (if cvport
       (hcnm-ldrblk-capture-viewport-transform ename-bubble cvport)
       (haws-debug "  WARNING: CVPORT is nil - cannot capture viewport")
     )
    )
    ;; Path 4: Any other gate closed - skip
    (t
     (haws-debug
       "  >>> DECISION: Skip viewport capture (gate closed)"
     )
    )
  )
  (princ)                               ; Return nil with clean output (this is a side-effect procedure)
)
;; Gets the target viewport from user. This would only be called because we needed it before we could determine it automatically or when user clicks the button to change association.
;; NOTE: Warning should be shown BEFORE calling this function (via hcnm-ldrblk-tip-warn-pspace-no-react)
;; NOTE: This function does NOT restore space - caller must handle that after capturing transformation matrix
(defun hcnm-ldrblk-get-target-vport (/ input pspace-before-p)
  ;; Check if we're in paper space before switching
  (setq pspace-before-p (= (getvar "CVPORT") 1))
  ;; Ensure user is in model space so they can activate a viewport
  (cond
    (pspace-before-p
     (princ
       "\nWARNING: Not in model space - switching to MSPACE first"
     )
     (vl-cmdf "._MSPACE")
     ;; Set global flag so top-level can restore
     (setq *hcnm-pspace-restore-needed* t)
    )
  )
  (while
    (not
      (progn
        (princ
          "\nSet the TARGET viewport active and press ENTER to continue: "
        )
        (equal (setq input (grread nil 10)) '(2 13))
                                        ; WAIT FOR ENTER (ASCII 13)
      )
    )
  )
  (setq input (getvar "CVPORT"))        ; Capture the viewport ID
  input                                 ; Return the viewport ID
)
;; Apply affine transformation using 3-point correspondence
;; Given 3 OCS points and their corresponding 3 WCS points, transform any OCS point to WCS
;; Uses barycentric coordinates to interpolate the transformation
(defun hcnm-ldrblk-apply-transform-matrix (p-ocs ocs1 wcs1 ocs2 wcs2 ocs3
                                       wcs3 / dx dy d11 d12 d21 d22 det
                                       u v w px py
                                      )
  ;; Calculate barycentric coordinates of p-ocs relative to the OCS triangle
  ;; First, express p-ocs in terms of the basis vectors (OCS2-OCS1) and (OCS3-OCS1)
  (setq
    dx  (- (car p-ocs) (car ocs1))
    dy  (- (cadr p-ocs) (cadr ocs1))
    d11 (- (car ocs2) (car ocs1))
    d12 (- (cadr ocs2) (cadr ocs1))
    d21 (- (car ocs3) (car ocs1))
    d22 (- (cadr ocs3) (cadr ocs1))
    det (- (* d11 d22) (* d12 d21))
  )
  (cond
    ((not (equal det 0.0 1e-10))
     ;; Calculate barycentric coordinates
     (setq
       u (/ (- (* dx d22) (* dy d21)) det)
       v (/ (- (* d11 dy) (* d12 dx)) det)
       w (- 1.0 u v)
     )
     ;; Apply same barycentric coordinates to WCS points
     (setq
       px (+ (* w (car wcs1)) (* u (car wcs2)) (* v (car wcs3)))
       py (+ (* w (cadr wcs1)) (* u (cadr wcs2)) (* v (cadr wcs3)))
     )
     (list px py 0.0)
    )
    (t
     ;; Degenerate case - matrix not invertible, fall back to simple translation
     (princ
       "\nWarning: Degenerate transformation matrix, using translation only"
     )
     (list
       (+ (car p-ocs) (- (car wcs1) (car ocs1)))
       (+ (cadr p-ocs) (- (cadr wcs1) (cadr ocs1)))
       0.0
     )
    )
  )
)

;; Get viewport transformation matrix from bubble's XDATA
;; Returns list: (CVPORT ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3 ref-wcs-3) or NIL
(defun hcnm-ldrblk-get-viewport-transform-xdata (ename-bubble)
  ;; Use service layer to get viewport transform
  ;; Returns: (cvport ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3 ref-wcs-3) or nil
  (hcnm-xdata-get-vptrans ename-bubble)
)

;; DEPRECATED: Old function - use hcnm-ldrblk-get-viewport-transform-xdata instead

;; Set viewport transformation matrix in bubble's XDATA
;; Stores CVPORT and 3 pairs of reference points (OCS and WCS)
;; These 3 points define the full transformation including rotation and scale
;; Preserves existing auto-text XDATA if present
;;
;; DESIGN: The following user experiences need this function:
;;   1. Getting auto text (during insertion or editing; done through dispatch-auto)
;;      - When user inserts bubble with N/E/NE/Sta/Off in paper space
;;      - When user edits existing bubble and switches to coordinate-based auto-text
;;   2. Linking a viewport (from edit dialog or separate command)
;;      - "Change View" button in edit dialog (explicit user action)
;;      - Future: Separate "Change Viewport" command for selection sets (TODO)
;;
;; IMPORTANT: This function should NEVER be called by:
;;   - Reactor updates (they should USE existing XDATA, not create new)
;;   - Coordinate calculation helpers (read-only operations)
;;   - Any automatic/defensive "let me fix missing data" logic
;;
;; If viewport transform is missing when needed, that's a legitimate error
;; state that should be handled gracefully (warn user, fail gracefully),
;; not silently "fixed" by prompting during background operations.
(defun hcnm-ldrblk-set-viewport-transform-xdata (ename-bubble cvport
                                             ref-ocs-1 ref-wcs-1
                                             ref-ocs-2 ref-wcs-2
                                             ref-ocs-3 ref-wcs-3
                                            )
  ;; Build viewport data list
  ;; Format: (cvport ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3 ref-wcs-3)
  (hcnm-xdata-set-vptrans
    ename-bubble
    (list
      cvport ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3 ref-wcs-3
     )
  )
)

;; Clear viewport transformation XDATA from bubble
;; Used when user wants to change viewport association via "Chg View" button
(defun hcnm-ldrblk-clear-viewport-transform-xdata
   (ename-bubble / appname elist elist-no-xdata)
  (setq appname "HCNM-BUBBLE")
  ;; Get entity list without XDATA
  (setq
    elist
     (entget ename-bubble)
    elist-no-xdata
     (vl-remove-if '(lambda (x) (= (car x) -3)) elist)
  )
  ;; Update entity without XDATA
  (entmod elist-no-xdata)
)


;; RETURNS p1-world GIVEN p1-ocs
;; Uses viewport transformation data from bubble's XDATA if available
;; This allows coordinate transformation without switching viewports
;; If bubble is on Model tab, no viewport processing needed
(defun hcnm-ldrblk-p1-world (ename-leader p1-ocs ename-bubble / elist-leader
                         layout-name pspace-current-p on-model-tab-p
                         transform-data cvport-stored ref-ocs-1
                         ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3
                         ref-wcs-3
                        )
  (setq
    elist-leader
     (entget ename-leader)
    layout-name
     (cdr (assoc 410 elist-leader))
    on-model-tab-p
     (or (= layout-name "Model")
         (= layout-name "MODEL")
         (not layout-name)
         ;; Older drawings without layout
     )
  )
  (cond
    ((not on-model-tab-p)
     ;; Bubble is on a layout tab - need viewport processing
     ;; Try to get viewport transformation data from XDATA
     (setq
       transform-data
        (cond
          (ename-bubble
           (hcnm-ldrblk-get-viewport-transform-xdata
             ename-bubble
           )
          )
          (t nil)
        )
     )
     (cond
       (transform-data
        ;; We have transformation matrix - use it to transform without switching viewports
        (setq
          cvport-stored
           (car transform-data)
          ref-ocs-1
           (nth 1 transform-data)
          ref-wcs-1
           (nth 2 transform-data)
          ref-ocs-2
           (nth 3 transform-data)
          ref-wcs-2
           (nth 4 transform-data)
          ref-ocs-3
           (nth 5 transform-data)
          ref-wcs-3
           (nth 6 transform-data)
        )
        (haws-debug
          (list
            "Using stored viewport "
            (itoa cvport-stored)
            " transformation matrix"
          )
        )
        ;; Apply affine transformation using the 3-point matrix
        ;; Calculate the transformation: p1-world = f(p1-ocs)
        (setq
          p1-world
           (hcnm-ldrblk-apply-transform-matrix
             p1-ocs ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3
             ref-wcs-3
            )
        )
        (haws-debug
          (list
            "Transformed p1-world: "
            (vl-princ-to-string p1-world)
          )
        )
       )
       (t
        ;; No transformation data stored in XDATA
        ;; This is an error state - coordinate-based auto-text in paper space requires viewport association
        ;; Do NOT prompt user here - this function may be called by reactors during background updates
        (princ
          "\nError: Viewport transformation data missing. Cannot calculate world coordinates."
        )
        (princ
          "\nUse 'Change View' button in edit dialog to associate bubble with a viewport."
        )
        ;; Return nil to signal error - caller must handle this gracefully
        (setq p1-world nil)
       )
     )
     p1-world
    )
    (t
     ;; Bubble is on Model tab - simple transformation, no viewport processing
     (trans p1-ocs 1 0)
    )
  )
)
;#endregion
;#region XDATA
;;==============================================================================
;; BUBBLE DATA - Read/Write with XDATA for Auto Text
;;==============================================================================
;; These functions handle reading and writing bubble attributes with auto text
;; stored in XDATA (not visible in attribute display text)

;; Check if XDATA auto text is found within attribute string
;; Returns T if xdata string is substring of attribute string, NIL otherwise
(defun hcnm-xdata-found-in-attribute-p (str-attribute str-xdata)
  (and
    str-xdata
    (/= str-xdata "")
    (vl-string-search str-xdata str-attribute)
  )
)

;; Split attribute string by XDATA auto text substring
;; Returns (prefix auto postfix) where auto comes from XDATA
;; 
;; DATA MODEL RULE:
;; - When auto field is empty: all user text goes to prefix, postfix MUST be empty
;; - When auto field has a value: prefix comes before auto, postfix comes after auto
;; This prevents confusion when there's no auto-text to split on.
;; 
;; The only source of postfix values is the edit dialog, which disables the postfix
;; field when auto is empty. Therefore, finding postfix without auto indicates a
;; programming error or data corruption.
;;
;; SPLITTING LOGIC when reading from block attributes:
;; - If XDATA found in attribute: split on it, extract prefix/auto/postfix
;; - If XDATA not found: entire attribute goes to prefix, auto and postfix are empty
;;   (Handles migration case: old blocks without XDATA, or user-edited attributes)
(defun hcnm-split-attribute-on-xdata
   (str-attribute str-xdata / pos prefix postfix)
  (cond
    ((hcnm-xdata-found-in-attribute-p str-attribute str-xdata)
     ;; XDATA found - split on it
     (setq
       pos
        (vl-string-search str-xdata str-attribute)
       prefix
        (substr str-attribute 1 pos)
       postfix
        (substr str-attribute (+ pos (strlen str-xdata) 1))
     )
     ;; Validate: if we have postfix but no auto (XDATA), that's an error
     (cond
       ((and
          postfix
          (/= postfix "")
          (or (not str-xdata) (= str-xdata ""))
        )
        (alert
          (princ
            (strcat
              "\nMessage from the CNM hcnm-split-attribute-on-xdata function:\n\n"
              "Whoops! We thought that by disabling the postfix field in our\n"
              "Bubble Note Editor when auto-text is empty, postfix would never\n"
              "exist without auto text. But exist it does, go figure.\n\n"
              "Attribute value: ["
              str-attribute
              "]\n"
              "Auto text (XDATA): ["
              (if str-xdata
                str-xdata
                "empty"
              )
              "]\n"
              "Postfix found: ["
              postfix
              "]\n\n"
              "So this is an unhandled exception to our thinking.\n"
              "Kindly report this oversight to the developer.\n\n"
              "We'll handle this by treating the entire attribute as prefix\n"
              "(user text), but this doesn't match our design intent."
            )
          )
        )
        (list
          (if str-attribute
            str-attribute
            ""
          )
          ""
          ""
        )
       )                                ; Fail safe: move everything to prefix
       (t
        (list
          (if prefix
            prefix
            ""
          )
          (if str-xdata
            str-xdata
            ""
          )
          (if postfix
            postfix
            ""
          )
        )
       )
     )
    )
    (t
     ;; XDATA not found - entire string is prefix, auto and postfix empty
     (list
       (if str-attribute
         str-attribute
         ""
       )
       ""
       ""
     )
    )
  )
)

;;; Strip underover format codes from lattribs parts (prefix auto postfix)
;;; Returns cleaned parts list with format codes removed from prefix
;;; ARCHITECTURE: lattribs must be clean - no format codes
(defun hcnm-ldrblk-strip-format-codes-from-parts
   (parts / prefix auto postfix)
  (setq
    prefix
     (car parts)
    auto
     (cadr parts)
    postfix
     (caddr parts)
  )
  ;; Strip mtext format codes
  (cond
    ((wcmatch prefix "\\L*") (setq prefix (substr prefix 3)))
    ((wcmatch prefix "\\O*") (setq prefix (substr prefix 3)))
  )
  ;; Strip dtext format codes
  (cond
    ((wcmatch prefix "%%u*") (setq prefix (substr prefix 4)))
    ((wcmatch prefix "%%o*") (setq prefix (substr prefix 4)))
  )
  (list prefix auto postfix)
)

;; Read bubble data from attributes and XDATA
;; Returns association list in 2-element format: (("TAG" "full-text") ...)
;; XDATA stores auto-text values separately for search/replace during updates
;; Format: (("NOTETXT0" "text") ("NOTETXT1" "text") ("NOTENUM" "123") ...)
(defun hcnm-ldrblk-dwg-to-lattribs (ename-bubble field-code-p / lattribs
                                xdata-alist xdata-raw appname ename-next
                                etype elist obj-next tag value
                                field-code
                               )
  (setq
    appname "HCNM-BUBBLE"
    lattribs
     '()
    xdata-alist
     '()
  )
  ;; Step 1: Read XDATA for auto-text values (stored separately from display text)
  (setq xdata-alist (hcnm-xdata-read ename-bubble))
  ;; Step 2: Read attributes - just store full text value
  (setq ename-next ename-bubble)
  (while (and
           (setq ename-next (entnext ename-next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename-next)))))
           )
         )
    (cond
      ((= etype "ATTRIB")
       (setq
         tag
          (cdr (assoc 2 elist))
         obj-next
          (vlax-ename->vla-object ename-next)
         value
          (cond
            ((and
               field-code-p
               (setq field-code (lm:fieldcode ename-next))
             )
             field-code
            )
            (t (vla-get-textstring obj-next))
          )
       )
       ;; ARCHITECTURE: lattribs is clean - strip format codes
       (cond
         ((member tag '("NOTETXT1" "NOTETXT2"))
          ;; Strip underover format codes if present
          (cond
            ((wcmatch value "\\L*") (setq value (substr value 3)))
            ((wcmatch value "\\O*") (setq value (substr value 3)))
            ((wcmatch value "%%u*") (setq value (substr value 4)))
            ((wcmatch value "%%o*") (setq value (substr value 4)))
          )
         )
         ((= tag "NOTEGAP")
          ;; NOTEGAP should always be empty in lattribs (calculated on write)
          (setq value "")
         )
       )
       ;; Add to lattribs: 2-element (tag text)
       (setq
         lattribs
          (cons
            (list
              tag
              (if value
                value
                ""
              )
            )
            lattribs
          )
       )
      )
    )
  )
  ;; ARCHITECTURE: Validate before returning - fail loudly on corruption
  (if (not (hcnm-ldrblk-lattribs-validate-schema lattribs))
    (progn
      (alert
        (princ
          "\nCRITICAL: dwg-to-lattribs produced invalid lattribs structure"
        )
      )
      nil
    )                                   ; Return nil on validation failure
    lattribs
  )                                     ; Return validated lattribs
)

;; Set attributes on a block (used by reactors and other update paths)
;; Takes: ename-block, lattribs in format '(("TAG" "value") ...)
(defun hcnm-set-attributes (ename-block lattribs / atag elist ename-next
                        etype obj-next
                       )
  (setq ename-next ename-block)
  (while (and
           (setq ename-next (entnext ename-next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename-next)))))
           )
         )
    (cond
      ((and
         (= etype "ATTRIB")
         (setq atag (cdr (assoc 2 elist)))
         (assoc atag lattribs)
       )
       (setq obj-next (vlax-ename->vla-object ename-next))
       (vla-put-textstring obj-next (cadr (assoc atag lattribs)))
       ;; UPDATEFIELD commented out to avoid "0 field(s) found/updated" messages
       ;; May be necessary for some bubble types - uncomment if needed
                                        ;(COND ((= (hcnm-ldrblk-get-mtext-string) "")(VL-CMDF "._updatefield" ename-next "")))
      )
    )
  )
)

;; field-code-p NIL SIMPLIFIES PROCESSING WHEN BLOCKS LIKE NOTEQTY ARE KNOWN NOT TO HAVE FIELD CODES IN THEM
(defun hcnm-get-attributes (ename-block field-code-p / lattribs elist
                        ename-next etype field-code obj-next
                       )
  (setq ename-next ename-block)
  (while (and
           (setq ename-next (entnext ename-next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename-next)))))
           )
         )
    (cond
      ((= etype "ATTRIB")
       (setq
         obj-next
          (vlax-ename->vla-object ename-next)
         lattribs
          (cons
            (list
              (cdr (assoc 2 elist))
              (cond
                ((and
                   field-code-p
                   (setq field-code (lm:fieldcode ename-next))
                 )
                 field-code
                )
                (t (vla-get-textstring obj-next))
              )
            )
            lattribs
          )
       )
      ) ;_ end of and
    )
  )
  lattribs
)
(defun lm:fieldcode (en / fd id)
  (cond
    ((and
       (wcmatch
         (cdr (assoc 0 (setq en (entget en))))
         "TEXT,MTEXT,ATTRIB"
       )
       (setq en (cdr (assoc 360 en)))
       (setq en (dictsearch en "ACAD_FIELD"))
       (setq en (dictsearch (cdr (assoc -1 en)) "TEXT"))
       (setq fd (entget (cdr (assoc 360 en))))
     )
     (if (vl-string-search "\\_FldIdx " (cdr (assoc 2 en)))
       (vl-string-subst
         (if (setq id (cdr (assoc 331 fd)))
           (vl-string-subst
             (strcat
               "ObjId "
               (itoa (vla-get-objectid (vlax-ename->vla-object id)))
             )
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
;; Debug function to examine bubble XDATA
(defun c:hcnm-debug-bubble-xdata (/ ss ename xdata)
  (if (setq ss (ssget "_:S" '((0 . "INSERT"))))
    (progn
      (setq
        ename
         (ssname ss 0)
        xdata
         (entget ename '("HCNM-BUBBLE"))
      )
      (princ "\n=== Bubble XDATA Debug ===")
      (princ (strcat "\nEntity: " (vl-princ-to-string ename)))
      (foreach
         item xdata
        (cond
          ((= (car item) -3)
           (princ "\nXDATA:")
           (foreach
              xitem (cdr item)
             (princ (strcat "\n  " (vl-princ-to-string xitem)))
           )
          )
          ((member (car item) '(0 2 8 10))
           (princ (strcat "\n" (vl-princ-to-string item)))
          )
        )
      )
      (princ "\n========================\n")
    )
    (princ "\nNo bubble selected.")
  )
  (princ)
)


;#region Extension Dictionary Service Layer
;;==============================================================================
;; EXTENSION DICTIONARY - HCNM-BUBBLE PERSISTENT DATA
;;==============================================================================
;; Uses XRECORD in extension dictionary for large/permanent data (VPTRANS)
;; Uses XDATA for small/dynamic data (autotext tag-value pairs)
;;==============================================================================

;; Get or create extension dictionary for bubble
;; Returns: ename of "HCNM" dictionary in bubble's extension dictionary
(defun hcnm-extdict-get (ename-bubble / vla-obj vla-extdict dict-ename
                     hcnm-dict-data hcnm-ename
                    )
  (setq vla-obj (vlax-ename->vla-object ename-bubble))
  ;; Get or create extension dictionary (VLA method creates if needed)
  (setq vla-extdict (vla-getextensiondictionary vla-obj))
  (setq dict-ename (vlax-vla-object->ename vla-extdict))
  ;; Look for our HCNM dictionary
  (cond
    ((setq hcnm-dict-data (dictsearch dict-ename "HCNM"))
     ;; Return ename of existing HCNM dictionary
     (cdr (assoc -1 hcnm-dict-data))
    )
    (t
     ;; Create HCNM dictionary
     (setq
       hcnm-ename
        (entmakex
          '((0 . "DICTIONARY") (100 . "AcDbDictionary"))
        )
     )
     (dictadd dict-ename "HCNM" hcnm-ename)
     hcnm-ename
    )
  )
)

;; Write VPTRANS to XRECORD in extension dictionary
;; viewport-data: (cvport ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3 ref-wcs-3)
(defun hcnm-vptrans-write (ename-bubble viewport-data / dict-ename xrec-data
                       cvport ref-points
                      )
  (setq dict-ename (hcnm-extdict-get ename-bubble))
  (cond
    (viewport-data
     (setq
       cvport
        (car viewport-data)
       ref-points
        (cdr viewport-data)
     )
     ;; Build XRECORD data list with labeled fields
     (setq
       xrec-data
        (list
          '(0 . "XRECORD")
          '(100 . "AcDbXrecord")
          (cons 70 cvport)
        )
     )                                  ; 70 = short integer for cvport
     ;; Add all 6 reference points as 3D points (code 10)
     (foreach
        pt ref-points
       (setq xrec-data (append xrec-data (list (cons 10 pt))))
     )
     ;; Create or update VPTRANS xrecord
     (dictadd dict-ename "VPTRANS" (entmakex xrec-data))
     t
    )
    (t nil)
  )
)

;; Read VPTRANS from XRECORD in extension dictionary  
;; Returns: (cvport ref-ocs-1 ref-wcs-1 ... ref-wcs-3) or nil
(defun hcnm-vptrans-read
   (ename-bubble / dict-ename vptrans-rec cvport ref-points)
  (setq dict-ename (hcnm-extdict-get ename-bubble))
  (cond
    ((and
       dict-ename
       (setq vptrans-rec (dictsearch dict-ename "VPTRANS"))
     )
     (setq vptrans-rec (entget (cdr (assoc -1 vptrans-rec))))
     ;; Extract cvport (code 70)
     (setq cvport (cdr (assoc 70 vptrans-rec)))
     ;; Extract all points (code 10)
     (setq ref-points '())
     (foreach
        item vptrans-rec
       (cond
         ((= (car item) 10)
          (setq ref-points (append ref-points (list (cdr item))))
         )
       )
     )
     ;; Return viewport data
     (cond
       ((and cvport (= (length ref-points) 6))
        (cons cvport ref-points)
       )
       (t nil)
     )
    )
    (t nil)
  )
)

;#endregion

;#region XDATA Service Layer
;;==============================================================================
;; XDATA SERVICE LAYER - HCNM-BUBBLE AUTO-TEXT STORAGE
;;==============================================================================
;; XDATA now only stores auto-text tag-value pairs for quick read/write access.
;; Format: (1000 "TAG") (1001 "value") pairs
;; 
;; VPTRANS moved to XRECORD in extension dictionary (see above functions).
;;==============================================================================

;#region XDATA Operations
;;==============================================================================
;; XDATA read/write functions for bubble auto-text storage.
;; Handle-based XDATA format: (("TAG" ((handle1 . "auto1") (handle2 . "auto2"))) ...)
;; Supports multiple auto-texts per tag, each linked to reference object handle.
;;==============================================================================

;; Read HCNM-BUBBLE XDATA (autotext only)
;; Returns: (("TAG1" . "value1") ("TAG2" . "value2") ...) or nil
;; Parses multiple 1000 codes: (1000 . "TAG1") (1000 . "value1") ...
(defun hcnm-xdata-read
   (ename-bubble / appname xdata-raw pairs current-tag item values all-values idx
    values-copy composite-pairs auto-type handle auto-text)
  (setq appname "HCNM-BUBBLE")
  (setq xdata-raw (assoc -3 (entget ename-bubble (list appname))))
  (cond
    (xdata-raw
     (setq xdata-raw (cdr (assoc appname (cdr xdata-raw))))
     ;; Extract all 1000 code values
     (setq all-values '())
     (foreach item xdata-raw
       (cond
         ((= (car item) 1000)
          (setq all-values (append all-values (list (cdr item))))
         )
       )
     )
     
     ;; Parse values: first is always tag, then determine format by next tag position
     (setq pairs '())
     (setq idx 0)
     (while (< idx (length all-values))
       (setq current-tag (nth idx all-values))
       (setq idx (1+ idx))
       
       ;; Find next tag (attribute tags are in our known set)
       ;; Tags: NOTENUM, NOTEPHASE, NOTEGAP, NOTETXT0-6
       (setq values '())
       (while (and (< idx (length all-values))
                   (not (member (nth idx all-values) 
                                '("NOTENUM" "NOTEPHASE" "NOTEGAP" 
                                  "NOTETXT0" "NOTETXT1" "NOTETXT2" "NOTETXT3" 
                                  "NOTETXT4" "NOTETXT5" "NOTETXT6"))))
         (setq values (append values (list (nth idx all-values))))
         (setq idx (1+ idx))
       )
       
       ;; Create entry based on value count
       (cond
         ;; Single value is legacy simple format - convert gracefully
         ((= (length values) 1)
          (haws-debug
            (strcat
              "Legacy XDATA format detected - converting to composite-key format"
              "\n  Tag: " current-tag
              "\n  Value: " (car values)
              "\n  This is normal for bubbles created before composite-key format."
            )
          )
          ;; Convert legacy format: treat as unknown auto-type with empty handle
          ;; When this bubble is saved (via reactor or edit dialog), it will write composite-key format
          (setq composite-pairs (list (cons (cons "UNKNOWN" "") (car values))))
          (setq pairs (append pairs (list (cons current-tag composite-pairs))))
         )
         ;; Multiple values - composite key format (triplets of auto-type/handle/auto-text)
         ((> (length values) 1)
          (setq composite-pairs '())
          (setq values-copy values)
          (while values-copy
            (cond
              ;; Need at least 3 values for a triplet
              ((>= (length values-copy) 3)
               (setq auto-type (car values-copy))
               (setq handle (cadr values-copy))
               (setq auto-text (caddr values-copy))
               ;; Build composite key: ((auto-type . handle) . auto-text)
               (setq composite-pairs 
                 (append composite-pairs 
                   (list (cons (cons auto-type handle) auto-text))))
               (setq values-copy (cdddr values-copy))
              )
              (t
               ;; Not enough values for triplet, skip remaining
               (setq values-copy nil)
              )
            )
          )
          ;; Use (cons tag composite-pairs) so (cdr) extracts composite-pairs directly
          (setq pairs (append pairs (list (cons current-tag composite-pairs))))
         )
         ;; No values - empty (shouldn't happen but handle gracefully)
         (t
          (setq pairs (append pairs (list (cons current-tag ""))))
         )
       )
     )
     pairs
    )
  )
)

;; Write HCNM-BUBBLE XDATA (autotext only)
;; autotext-alist: (("TAG1" . "value1") ("TAG2" . "value2") ...)
;; Uses multiple 1000 codes: (1000 . "TAG1") (1000 . "value1") ...
(defun hcnm-xdata-write (ename-bubble autotext-alist / appname xdata-list
                     result pair ent-list has-xdata xdata-struct new-ent
                     composite-key auto-text
                    )
  (setq appname "HCNM-BUBBLE")
  ;; Check if app is registered
  (setq result (tblsearch "APPID" appname))
  ;; Register application if needed
  (cond
    ((not result)
     (setq result (regapp appname))
     ;; Verify registration worked
     (setq result (tblsearch "APPID" appname))
     (cond
       ((not result)
        (alert
          (princ
            (strcat "ERROR: Failed to register application " appname)
          )
        )
        (setq appname nil)
       )
     )
    )
  )
  (cond
    (appname
     ;; Build list of alternating TAG/VALUE as 1000 codes
     ;; Format: (1000 . "TAG1") (1000 . "value1") (1000 . "TAG2") (1000 . "value2")
     ;; NOTE: Do NOT include 1001 - appname goes as key, not in data list
     (setq xdata-list '())
     (cond
       (autotext-alist
        (foreach
           pair autotext-alist
          ;; DEBUG: Show what we're processing
          (haws-debug
            (list
              "    [XDATA-WRITE] pair: " (vl-prin1-to-string pair)
              "    [XDATA-WRITE] (cdr pair): " (vl-prin1-to-string (cdr pair))
              "    [XDATA-WRITE] (listp (cdr pair)): " (vl-prin1-to-string (listp (cdr pair)))
              (cond
                ((and (listp (cdr pair)) (listp (car (cdr pair))))
                 "    [XDATA-WRITE] Using handle-based format"
                )
                (t
                 "    [XDATA-WRITE] Using simple format"
                )
              )
            )
          )
          
          ;; Add tag as 1000
          (setq
            xdata-list
             (append xdata-list (list (cons 1000 (car pair))))
          )
          ;; Add value(s) as 1000
          ;; Handle both simple format (string) and composite key format (list of pairs)
          (cond
            ;; Composite key format: (((auto-type . handle) . "auto-text") ...)
            ((and (listp (cdr pair)) (listp (car (cdr pair))))
             (foreach
                handle-pair (cdr pair)
               (setq composite-key (car handle-pair))  ; (auto-type . handle)
               (setq auto-text (cdr handle-pair))      ; "auto-text"
               ;; Write triplet: auto-type, handle, auto-text
               (setq xdata-list (append xdata-list (list (cons 1000 (car composite-key)))))  ; auto-type
               (setq xdata-list (append xdata-list (list (cons 1000 (cdr composite-key)))))  ; handle
               (setq xdata-list (append xdata-list (list (cons 1000 auto-text))))            ; auto-text
             )
            )
            ;; Simple format: just a string
            (t
             (setq xdata-list (append xdata-list (list (cons 1000 (cdr pair)))))
            )
          )
        )
       )
     )
     ;; Write XDATA
     (cond
       ((> (length xdata-list) 0)       ; Have data to write
        ;; Get entity WITHOUT existing XDATA first (important for updates!)
        (setq ent-list (entget ename-bubble))
        ;; DEBUG: Check if entity read succeeded
        (cond
          ((not ent-list)
           (alert
             (princ (strcat "\nERROR: Could not read bubble entity"))
           )
          )
          (t
           ;; Remove any existing -3 to avoid conflicts
           (setq
             ent-list
              (vl-remove-if
                '(lambda (x) (= (car x) -3))
                ent-list
              )
           )
           ;; Build XDATA structure: (-3 . ((appname xdata-list)))
           (setq
             xdata-struct
              (list
                (cons -3 (list (cons appname xdata-list)))
              )
           )
           ;; Append and modify
           (setq new-ent (append ent-list xdata-struct))
           (setq result (entmod new-ent))
           (cond
             ((not result)
              (alert
                (princ
                  (strcat
                    "\nERROR: entmod failed writing XDATA"
                    "\n  Handle: "
                    (cdr (assoc 5 ent-list))
                    "\n  Layer: "
                    (cdr (assoc 8 ent-list))
                    "\n  Items: "
                    (itoa (length xdata-list))
                    "\n  Check: locked layer or command conflict"
                  )
                )
              )
             )
           )
          )
        )
       )
     )
     t
    )
  )
)

;; Update viewport transform (now uses XRECORD)
;; viewport-data: (cvport ref-ocs-1 ref-wcs-1 ... ref-wcs-3)
(defun hcnm-xdata-set-vptrans (ename-bubble viewport-data)
  (hcnm-vptrans-write ename-bubble viewport-data)
)

;; Update auto-text (uses XDATA)
;; autotext-alist: (("TAG1" . "value1") ("TAG2" . "value2") ...)
(defun hcnm-xdata-set-autotext (ename-bubble autotext-alist)
  (hcnm-xdata-write ename-bubble autotext-alist)
)

;; Get viewport transform data (now from XRECORD)
;; Returns: (cvport ref-ocs-1 ref-wcs-1 ... ref-wcs-3) or nil
(defun hcnm-xdata-get-vptrans (ename-bubble)
  (hcnm-vptrans-read ename-bubble)
)

;; Get auto-text data (from XDATA)
;; Returns: (("TAG1" . "value1") ("TAG2" . "value2") ...) or nil
(defun hcnm-xdata-get-autotext (ename-bubble)
  (hcnm-xdata-read ename-bubble)
)

;#endregion

;;==============================================================================
;; LEGACY WRAPPERS - Maintain existing API during transition
;;==============================================================================

;; Save only XDATA for auto-text (helper for dialog save path)
;;
;; REQUIRES: Semi-global hcnm-ldrblk-eb-auto-handles must be bound (dialog context only)
;; WRITES: Composite-key format XDATA (handles + auto-text)
;;
;; The HCNM-BUBBLE section of XDATA for a bubble note stores:
;; 1. Auto-text values (separately from display attributes) in composite-key format
;; 2. Viewport transformation matrix (for paper space coordinate conversion)
;;
;; This function preserves existing viewport transform data when updating auto-text.
;;
;; ARCHITECTURAL NOTE (2025-11-06):
;; Simple format DEPRECATED. Only composite-key format supported.
;; Reactor path uses hcnm-ldrblk-xdata-update-one (maintains composite-key format).
;; This function is ONLY for dialog save path where semi-global is bound.
(defun hcnm-ldrblk-xdata-save (ename-bubble lattribs / autotext-alist)
  ;; FAIL LOUDLY: Semi-global must be bound (programming error if not)
  (cond
    ((not (boundp 'hcnm-ldrblk-eb-auto-handles))
     (alert
       (princ
         (strcat
           "\nPROGRAMMING ERROR: hcnm-ldrblk-xdata-save called without semi-global bound!"
           "\n"
           "\nThis function requires hcnm-ldrblk-eb-auto-handles (dialog context)."
           "\nReactor path should use hcnm-ldrblk-xdata-update-one instead."
           "\n"
           "\nPlease report this error to the developer."
         )
       )
     )
     nil  ; Return nil, don't crash
    )
    ((not hcnm-ldrblk-eb-auto-handles)
     ;; Semi-global bound but empty - this is OK (user cleared all auto-text)
     (setq autotext-alist '())
     (hcnm-xdata-set-autotext ename-bubble autotext-alist)
    )
    (t
     ;; Normal case: Write composite-key format from semi-global
     (setq autotext-alist hcnm-ldrblk-eb-auto-handles)
     (hcnm-xdata-set-autotext ename-bubble autotext-alist)
    )
  )
)

;; Save bubble data to attributes and XDATA
;; Takes association list in 2-element format (tag text-value)
;; Format: (("NOTETXT0" "text") ("NOTETXT1" "text") ...)
;;
;; The HCNM-BUBBLE section of XDATA for a bubble note stores:
;; 1. Auto-text values (separately from display attributes)
;; 2. Viewport transformation matrix (for paper space coordinate conversion)
;;
;; This function saves text to visible attributes with format codes added.
;; XDATA is managed separately by model layer functions.
(defun hcnm-ldrblk-lattribs-to-dwg (ename-bubble lattribs / appname
                                xdata-list ename-next etype elist atag
                                obj-next lattribs-formatted text-value
                               )
  (setq
    appname "HCNM-BUBBLE"
    xdata-list
     '()
  )
  ;; Register application if not already registered
  (cond ((not (tblsearch "APPID" appname)) (regapp appname)))
  ;; Step 1: Build XDATA list for auto-text values
  ;; XDATA stores verbatim auto-text for search/replace during updates
  ;; Managed by model layer functions (set-auto, set-free)
  ;; Format: ((1000 "TAG1") (1000 "VALUE1") (1000 "TAG2") (1000 "VALUE2") ...)
  ;; NOTE: This function writes lattribs only. XDATA is managed separately.
  ;; Step 2: Add format codes to text lines (beautifully-architected underover-add!)
  (setq lattribs-formatted (hcnm-ldrblk-underover-add lattribs))
  ;; Step 3: Write formatted values to drawing attributes
  (setq ename-next ename-bubble)
  (while (and
           (setq ename-next (entnext ename-next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename-next)))))
           )
         )
    (cond
      ((and
         (= etype "ATTRIB")
         (setq atag (cdr (assoc 2 elist)))
         (setq text-value (cadr (assoc atag lattribs-formatted)))
       )
       ;; Write text value to attribute
       (setq obj-next (vlax-ename->vla-object ename-next))
       (vla-put-textstring obj-next text-value)
      )
    )
  )
)

;#endregion
;#endregion
;#region Reactors

;; Check and cleanup reactor proliferation
;; Returns: T if cleanup occurred, NIL if no problems found
;; Should be called at start of any bubble operation and on drawing open
(defun hcnm-check-reactor-proliferation (/ hcnm-reactors reactor-count)
  (setq
    hcnm-reactors
     (vl-remove-if-not
       '(lambda (r)
          (and
            (listp (vlr-data r))
            (assoc "HCNM-BUBBLE" (vlr-data r))
          )
        )
       (cdar (vlr-reactors :vlr-object-reactor))
     )
    reactor-count
     (length hcnm-reactors)
  )
  (cond
    ((> reactor-count 1)
     ;; Multiple HCNM-BUBBLE reactors found - make extras transient (die at session end)
     ;; NEVER use vlr-remove on persistent reactors - they become ghosts in the DWG!
     (foreach
        reactor (cdr hcnm-reactors)     ; Keep first, release rest
       (if (vlr-pers-p reactor)
         (vlr-pers-release reactor)
       )
     )
     (princ
       (strcat
         "\n*** REACTOR CLEANUP: Released "
         (itoa (1- reactor-count))
         " duplicate reactors (will disappear next session) ***\n"
       )
     )
     t                                  ; Return T to indicate cleanup occurred
    )
    (t nil)                             ; Return NIL if no problems
  )
)

;;Playing with reactors
(defun hcnm-ldrblk-list-reactors (/ reactors)
  (setq reactors (cdar (vlr-reactors :vlr-object-reactor)))
  (foreach
     reactor reactors
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
;; New structure: KEYS = '("HCNM-BUBBLE" handle-reference handle-bubble TAG)
;; VALUE = auto-type (just the string)
(defun hcnm-ldrblk-auto-type-requires-coordinates-p (auto-type / keys-entry)
  ;; Returns T if auto-type needs leader position (coordinates), nil otherwise
  ;; This determines if leader should be a reactor owner
  ;; Note: auto-type is the SECOND element in the keys list, not the first
  (setq
    keys-entry
     (vl-member-if
       '(lambda (entry)
          (equal auto-type (cadr entry))
        )
       (hcnm-ldrblk-get-auto-type-keys)
     )
  )
  (cond
    (keys-entry
     (cadddr (car keys-entry))
    )
    (t nil)
  )
)
(defun hcnm-ldrblk-debug-reactor-attachment (auto-type handle-reference handle-leader handle-bubble keys-leader owners data)
  ;; Debug output for reactor attachment
  (haws-debug
    (list
      "=== Reactor attachment complete ==="
      "Auto-type: " auto-type
      "Handle-reference: " (if handle-reference handle-reference "nil")
      "Handle-leader: " (if handle-leader handle-leader "nil")
      "Handle-bubble: " handle-bubble
      "Keys-leader: " (if keys-leader (vl-prin1-to-string keys-leader) "nil")
      "Owners count: " (itoa (length owners))
      "Final data structure: " (vl-prin1-to-string data)
    )
  )
)

;#region Reactor System Core
;;==============================================================================
;; Persistent reactor system for auto-updating bubble notes.
;; ONE reactor per drawing tracks multiple owners (alignments, pipes, leaders).
;; Data structure: owner → bubble → tag → auto-type → reference-handle
;; 
;; Call flow: AutoCAD event → callback → notifier-update → bubble-update → update-bubble-tag
;; Lookup pattern: Search owner-list → find notifier → drill to reference handles
;;==============================================================================

(defun hcnm-ldrblk-cleanup-reactor-data (reactor / data key-app owner-list cleaned-owner-list owner handle-owner bubble-list cleaned-bubble-list bubble handle-bubble tag-list)
  ;; Remove entries for deleted bubbles and empty references
  ;; Returns cleaned data structure
  (setq
    data
     (vlr-data reactor)
    key-app "HCNM-BUBBLE"
    owner-list
     (cadr (assoc key-app data))
    cleaned-owner-list nil
  )
  ;; Iterate through each reference/leader owner
  (foreach
     owner owner-list
    (setq
      handle-owner
       (car owner)
      bubble-list
       (cadr owner)
      cleaned-bubble-list nil
    )
    ;; Keep only bubbles that still exist
    (foreach
       bubble bubble-list
      (setq
        handle-bubble
         (car bubble)
      )
      (cond
        ((handent handle-bubble)
         (setq
           cleaned-bubble-list
            (append cleaned-bubble-list (list bubble))
         )
        )
      )
    )
    ;; Only keep this reference if it has remaining bubbles
    (cond
      (cleaned-bubble-list
       (setq
         cleaned-owner-list
          (append
            cleaned-owner-list
            (list (list handle-owner cleaned-bubble-list))
          )
       )
      )
    )
  )
  ;; Return cleaned data
  (list (list key-app cleaned-owner-list))
)
;;==============================================================================
;; hcnm-ldrblk-reactor-add-auto
;;==============================================================================
;; Purpose:
;;   Helper function to add or update auto-text entry in reactor data structure.
;;   Wraps haws_nested_list_update for cleaner API.
;;
;; Arguments:
;;   data - Current reactor data structure (or NIL to create new)
;;   handle-owner - Handle of owner object (reference or leader)
;;   handle-bubble - Handle of bubble block
;;   tag - Attribute tag (e.g., "NOTETXT1")
;;   auto-type - Auto-text type (e.g., "StaOff", "Dia")
;;   handle-reference - Handle of actual reference object (stored at leaf)
;;
;; Returns:
;;   Updated data structure with new entry added
;;
;; Path Structure:
;;   ("HCNM-BUBBLE" owner bubble tag auto-type) → reference-handle
;;
;; Terminology (Semantic Hierarchy):
;;   handle-owner = Owner that triggers reactor (reference object OR leader)
;;                  - References: Trigger when object geometry changes
;;                  - Leaders: Trigger when arrowhead moves (coordinate-based auto-text only)
;;   handle-reference = Owner that provides calculation data (always reference object, never leader)
;;                      - For direct path: owner = reference (same object)
;;                      - For leader path: owner = leader, reference = alignment/pipe/surface
;;
;; Leader Path Example:
;;   User places NE bubble in paper space, clicks alignment for StaOff auto-text.
;;   - handle-owner = leader handle (we track leader arrowhead moves)
;;   - handle-reference = alignment handle (we get station/offset from alignment)
;;   When leader moves, we recalculate StaOff using NEW leader position on SAME alignment.
;;
;; Example:
;;   (hcnm-ldrblk-reactor-add-auto 
;;     data "1D235" "62880" "NOTETXT1" "StaOff" "1D235"
;;   )
;;==============================================================================
(defun hcnm-ldrblk-reactor-add-auto (data handle-owner handle-bubble tag auto-type handle-reference)
  (haws_nested_list_update
    data
    (list "HCNM-BUBBLE" handle-owner handle-bubble tag auto-type)
    handle-reference
  )
)

;;==============================================================================
;; hcnm-ldrblk-assure-auto-text-has-reactor
;;==============================================================================
;; Purpose:
;;   Ensures a bubble's auto-text field is tracked by the persistent reactor system.
;;   Creates reactor if needed, attaches owner objects, updates data structure.
;;   This is called when user creates new auto-text via insertion or editing.
;;
;; Arguments:
;;   objref - VLA-OBJECT of reference (alignment/pipe/surface) or NIL for N/E/NE
;;   ename-bubble - Entity name of bubble block
;;   ename-leader - Entity name of leader (or NIL if direct bubble without leader)
;;   tag - Attribute tag to track (e.g., "NOTETXT1")
;;   auto-type - Auto-text type (e.g., "StaOff", "Dia", "N", "E")
;;
;; Terminology:
;;   owner = Any object attached to reactor (reference OR leader)
;;   reference = Object that provides calculation data (alignment/pipe/surface, never leader)
;;   For direct path: owner = reference (track alignment changes)
;;   For leader path: owners = [reference, leader] (track both alignment AND leader moves)
;;
;; Call Flow:
;;   User action → insertion/editing → auto-dispatch → THIS FUNCTION → reactor-add-auto (builds data)
;;
;; Reactor Architecture (ONE Persistent Reactor Per Drawing):
;;   CNM uses a SINGLE persistent VLR-OBJECT-REACTOR per drawing (not per bubble).
;;   This reactor tracks multiple owner objects (alignments, pipes, surfaces, leaders).
;;   Data structure maps: owner → bubble → tag → auto-type → reference-handle
;;   When ANY tracked owner changes, callback fires and updates dependent bubbles.
;;
;; Owner Attachment Logic (What to Track):
;;   1. If objref is NIL (N/E/NE only): attach leader only
;;      - Leader arrowhead position provides coordinates
;;   2. If coordinate-dependent (Sta/Off/StaOff/N/E/NE/Z): attach BOTH reference AND leader
;;      - Reference provides calculation context (alignment for station/offset)
;;      - Leader provides arrowhead position (what changed?)
;;   3. Otherwise: attach reference only
;;      - Leader position irrelevant (e.g., Dia/Slope/Length are object properties)
;;
;; Data Structure Symmetry (Critical for Leader Path):
;;   For coordinate-dependent types, BOTH paths must store SAME reference-handle:
;;     Alignment path: ("1D235" (("62880" (("NOTETXT1" (("StaOff" "1D235")))))))
;;     Leader path:    ("6287B" (("62880" (("NOTETXT1" (("StaOff" "1D235")))))))
;;                                                                  ^^^^^^ same reference!
;;   Why? When leader moves (notifier = "6287B"), callback drills down to find reference "1D235"
;;   and recalculates StaOff using NEW leader position on SAME alignment.
;;
;; Performance & Safety:
;;   - Filters reactor list once (vl-remove-if-not) to find HCNM-BUBBLE reactor
;;   - Fails loudly if multiple reactors found (corruption check - should never happen)
;;   - Cleans up stale bubble entries before adding new data (garbage collection)
;;   - Debug output shows final data structure (useful for troubleshooting)
;;
;; Side Effects:
;;   - Creates persistent reactor via vlr-pers if none exists (one-time per drawing)
;;   - Attaches owners to reactor via vlr-owner-add (tells AutoCAD what to watch)
;;   - Updates reactor data structure via vlr-data-set (tracks bubble dependencies)
;;   - Prints debug output to command line (can be disabled via config)
;;
;; Error Handling:
;;   - Multiple reactors: ALERT + abort (programming error, must fix)
;;   - Missing bubble: No-op (graceful - bubble may have been erased)
;;   - NIL objref for non-coordinate types: Alert (invalid use case)
;;
;; Example (Alignment-based StaOff with Leader):
;;   User places bubble in paper space, clicks alignment for StaOff auto-text.
;;   (hcnm-ldrblk-assure-auto-text-has-reactor 
;;     vla-alignment     ; objref - provides station/offset calculation
;;     ename-bubble      ; bubble to update
;;     ename-leader      ; leader - tracks arrowhead position
;;     "NOTETXT1"        ; tag to populate
;;     "StaOff"          ; auto-type
;;   )
;;   Result: Reactor tracks BOTH alignment (geometry changes) AND leader (position changes)
;;
;; Example (Coordinate-only NE without Reference):
;;   User places bubble, clicks NE button (no reference object).
;;   (hcnm-ldrblk-assure-auto-text-has-reactor 
;;     nil               ; objref - no reference object for N/E
;;     ename-bubble      ; bubble to update
;;     ename-leader      ; leader - provides arrowhead coordinates
;;     "NOTETXT1"        ; tag to populate
;;     "NE"              ; auto-type
;;   )
;;   Result: Reactor tracks leader only (N/E calculated from arrowhead position)
;;==============================================================================
(defun hcnm-ldrblk-assure-auto-text-has-reactor (objref ename-bubble
                                             ename-leader tag auto-type
                                             / callbacks data data-old
                                             reactor handle-bubble
                                             handle-reference handle-leader
                                             keys keys-leader
                                             key-app reactor-old
                                             reactors-old owner owners
                                             object-leader hcnm-reactors
                                             reactor-count
                                            )
  (setq
    callbacks
     '((:vlr-modified . hcnm-ldrblk-reactor-callback)
       ;; vlr-trace-reaction IS A CANNED CALLBACK PROVIDED BY AUTODESK FOR TESTING. IT PRINTS A MESSAGE. IT CAN BE REMOVED.
       ;; (:vlr-modified . vlr-trace-reaction)
      )
    reactors-old
     (cdar (vlr-reactors :vlr-object-reactor))
    object-leader
     (cond
       (ename-leader
        (vlax-ename->vla-object ename-leader)
       )
     )
    owners
     (cond
       ;; If OBJREF is NIL (for N/E/NE), only attach to leader
       ((and (not objref) object-leader) (list object-leader))
       ;; If auto-type requires coordinates (leader position matters), attach both objref and leader
       ((and object-leader (hcnm-ldrblk-auto-type-requires-coordinates-p auto-type))
        (list objref object-leader)
       )
       ;; Otherwise only attach objref (leader position doesn't matter for this auto-type)
       (t (list objref))
     )
    key-app "HCNM-BUBBLE"
    handle-reference
     (cond
       (objref (vla-get-handle objref))
       (t "")
     )
    handle-leader
     (cond
       (ename-leader
        (cdr (assoc 5 (entget ename-leader)))
       )
       (t nil)
     )
    handle-bubble
     (cdr (assoc 5 (entget ename-bubble)))
    keys
     ;; Path: ("HCNM-BUBBLE" reference-handle bubble-handle tag auto-type) -> reference-handle
     (list "HCNM-BUBBLE" handle-reference handle-bubble tag auto-type)
    keys-leader
     (cond
       ;; Only create leader keys if leader is coordinate-dependent
       ;; Path: ("HCNM-BUBBLE" leader-handle bubble-handle tag auto-type) -> reference-handle
       ((and handle-leader (hcnm-ldrblk-auto-type-requires-coordinates-p auto-type))
        (list "HCNM-BUBBLE" handle-leader handle-bubble tag auto-type)
       )
       (t nil)
     )
    reactor-old
     nil                                ; Initialize to nil
  )
  ;; Get the single HCNM-BUBBLE reactor (should be 0 or 1)
  (setq
    hcnm-reactors
     (vl-remove-if-not
       '(lambda (r)
          (and
            (listp (vlr-data r))
            (assoc key-app (vlr-data r))
          )
        )
       (cdar (vlr-reactors :vlr-object-reactor))
     )
    reactor-count
     (length hcnm-reactors)
    reactor-old
     (car hcnm-reactors)
  )
  ;; FAIL LOUDLY if multiple reactors (should never happen after CNM load cleanup)
  (if (> reactor-count 1)
    (alert
      (princ
        (strcat
          "\n*** PROGRAMMING ERROR ***\n\n"
          "Found "
          (itoa reactor-count)
          " HCNM-BUBBLE reactors.\n"
          "This should never happen!\n\n"
          "Please report this to GitHub with steps to reproduce.\n"
        )
      )
    )
  )
  ;; Clean up stale data entries before adding new ones
  (cond
    (reactor-old
     (vlr-data-set
       reactor-old
       (hcnm-ldrblk-cleanup-reactor-data reactor-old)
     )
    )
  )
  ;; Now handle reactor attachment/creation based on reactor-old
  (cond
    (reactor-old
     ;; ATTACH THIS OWNER NOTIFIER IF NOT ALREADY ATTACHED.
     (foreach
        owner owners
       (cond
         ((not (member owner (vlr-owners reactor-old)))
          (haws-debug
            (list
              "Adding owner: "
              (vla-get-handle owner)
              " to reactor"
            )
          )
          (vlr-owner-add reactor-old owner)
         )
         (t
          (haws-debug
            (list
              "Owner already attached: "
              (vla-get-handle owner)
            )
          )
         )
       )
     )
     ;; UPDATE THE DATA
     (vlr-data-set
       reactor-old
       (setq
         data
          (hcnm-ldrblk-reactor-add-auto
            (vlr-data reactor-old)
            handle-reference
            handle-bubble
            tag
            auto-type
            handle-reference
          )
       )
     )
     ;; If leader is coordinate-dependent, also add leader to data structure
     (cond
       (keys-leader
        (vlr-data-set
          reactor-old
          (setq
            data
             (hcnm-ldrblk-reactor-add-auto
               data
               handle-leader
               handle-bubble
               tag
               auto-type
               handle-reference
             )
          )
        )
       )
     )
    )
    (t
     ;; ELSE MAKE REACTOR AND MAKE IT PERSISTENT
     (setq
       data
        (hcnm-ldrblk-reactor-add-auto
          nil
          handle-reference
          handle-bubble
          tag
          auto-type
          handle-reference
        )
     )
     ;; If leader is coordinate-dependent, also add leader to data structure
     (cond
       (keys-leader
        (setq
          data
           (hcnm-ldrblk-reactor-add-auto
             data
             handle-leader
             handle-bubble
             tag
             auto-type
             handle-reference
           )
        )
       )
     )
     (setq
       reactor
        (vlr-object-reactor
          owners                        ; ATTACHED OWNERS OF REACTOR
          data
          callbacks
        )
       reactor
        (vlr-pers reactor)
     )
    )
  )
  ;; Debug output (can be disabled by commenting out function body)
  (hcnm-ldrblk-debug-reactor-attachment
    auto-type
    handle-reference
    handle-leader
    handle-bubble
    keys-leader
    owners
    data
  )
)
;;==============================================================================
;; REACTOR GATEWAY CHECKS
;;==============================================================================
;; Gateway functions determine if reactor callback should process updates.
;; Each returns T if gate is OPEN (allow processing), NIL if BLOCKED.
;;==============================================================================

;;==============================================================================
;; hcnm-ldrblk-reactor-callback
;;==============================================================================
;; Purpose:
;;   Main VLR-OBJECT-REACTOR callback - fires when any tracked reference object
;;   (alignment, pipe, surface) or leader is modified. Updates all dependent
;;   bubble notes with fresh auto-text.
;;
;; CRITICAL - AUTOLOADER STUB:
;;   This function has an autoloader stub defined in cnmloader.lsp that ensures
;;   cnm.lsp loads before first callback fires. The stub intercepts the call,
;;   loads cnm.lsp, then calls this real function. This solves the problem of
;;   persistent reactors firing before cnm.lsp autoloads via user commands.
;;
;; Arguments:
;;   obj-notifier - VLA-OBJECT that was modified (trigger object)
;;   obj-reactor - The persistent reactor object (contains data structure)
;;   parameter-list - Event parameters (unused)
;;
;; Call Flow:
;;   AutoCAD event → (stub in cnmloader.lsp) → THIS FUNCTION → notifier-update → bubble-update → update-bubble-tag
;;
;; Data Structure (stored in reactor):
;;   ("HCNM-BUBBLE" (
;;     (handle-owner (              ;; Any owner: reference object OR leader
;;       (handle-bubble (
;;         (tag (
;;           ("auto-type" "handle-reference")  ;; Leaf: always reference object, never leader
;;           ...
;;         ))
;;       ))
;;     ))
;;   ))
;;
;; Terminology (Semantic Hierarchy):
;;   OWNER = General term for any object attached to reactor (reference OR leader)
;;   NOTIFIER = Specific owner that triggered THIS callback (obj-notifier parameter)
;;   REFERENCE = Specific owner that provides calculation data (always reference object, never leader)
;;
;; Lookup Pattern:
;;   1. Search owner-list to find notifier-entry (which owner triggered this event?)
;;   2. Drill down through bubbles/tags to find reference handles (where's the data?)
;;
;; Performance:
;;   - Uses continue-p pattern for early exits (fail-fast)
;;   - Direct assoc lookup for notifier (O(1) instead of foreach)
;;   - Blocks nested callbacks to prevent dangerous recursion (Autodesk guideline)
;;
;; Gateways (must all pass to continue):
;;   1. BlockReactors = "0" (normal operation, blocking disabled for various reasons)
;;   2. Object not erased (error-safe check via vl-catch-all-apply)
;;   3. Notifier found in reactor data (valid tracked object)
;;
;;      GATEWAY 1 EXPLANATION: General-purpose reactor blocking flag.
;;      
;;      USAGE SCENARIOS where BlockReactors="1" blocks callbacks:
;;      1. NESTED CALLBACKS: Inside this callback when modifying bubble attributes
;;         - Problem: Modifying bubble triggers :vlr-modified on associated leader
;;         - Solution: Set flag="1" at callback entry, restore="0" at exit
;;         - Prevents infinite recursion (Autodesk guideline violation)
;;      
;;      2. ARROWHEAD STYLE CHANGES: During hcnm-ldrblk-change-arrowhead operations
;;         - Problem: Changing leader arrowhead style triggers callbacks during lattribs update
;;         - Solution: Block reactors during arrowhead property changes
;;         - Prevents stale XDATA from overwriting newly saved lattribs
;;      
;;      3. FUTURE USES: Any operation that needs to modify owners without triggering callbacks
;;      
;;      AUTODESK GUIDELINE: Do NOT modify reactor-monitored objects inside callbacks.
;;      Nested callbacks can cause infinite recursion, unpredictable ordering, and crashes.
;;      
;;      SOLUTION: Set BlockReactors="1" at callback entry (before any modifications).
;;      Nested callbacks see flag="1" and exit immediately at Gateway 1.
;;      Restore to "0" at callback end (allows next user action to process normally).
;;      
;;      This implements "outsmart the danger" pattern - we prevent recursion by
;;      temporarily disabling nested reactors during our update operation.
;;
;;      GATEWAY 3 EXPLANATION: This is a defensive/debug check for data integrity.
;;      
;;      EXPECTED BEHAVIOR: The notifier IS in our data structure because we added
;;      it when user created auto-text via hcnm-ldrblk-assure-auto-text-has-reactor.
;;      
;;      WHEN THIS CHECK FAILS (should be rare):
;;      - Data structure corruption (programming bugs)
;;      - Race conditions during undo/redo operations
;;      - Manual reactor data manipulation (advanced debugging)
;;      - Reactor ownership cleanup left orphaned callback registrations
;;      
;;      RESPONSE: Print warning (for debugging) and skip update gracefully.
;;      We DON'T crash AutoCAD over potentially transient state issues.
;;      
;;      This is "fail gracefully on UX convenience" not "fail loudly on data
;;      corruption" because the reactor system is self-healing (cleanup runs
;;      on next successful update).
;;
;; Side Effects:
;;   - Sets BlockReactors="1" to prevent nested callbacks during updates
;;   - Calls notifier-update which modifies bubble attributes
;;   - May remove notifier from reactor if no bubbles remain
;;   - Updates reactor data if structure changes
;;   - Restores BlockReactors to original value at callback end
;;
;; Error Handling:
;;   - Wraps vla-get-handle in error handler (erased objects throw errors)
;;   - Prints warning if notifier not in data (should never happen - see Gateway 3)
;;   - Silently skips if reactors blocked or object erased
;;   - haws-core-stperr also restores BlockReactors="0" on any CNM command error
;;
;; Gotchas:
;;   - BlockReactors="1" during space transitions (prevents spurious events)
;;   - BlockReactors="1" during this callback (prevents nested recursion)
;;   - Reactor data may have stale entries (cleanup removes them)
;;   - Object erasure triggers callback (must check before accessing)
;;
;; Example Event:
;;   User stretches alignment → obj-notifier = alignment VLA-OBJECT
;;   → Finds alignment handle in data structure
;;   → Updates all bubbles with "StaOff" auto-text from that alignment
;;==============================================================================
(defun hcnm-ldrblk-reactor-callback (obj-notifier obj-reactor parameter-list / 
                                     key-app data-old data handle-notifier 
                                     owner-list notifier-entry block-reactors-current
                                     profile-start
                                    ) 
  ;;===========================================================================
  ;; PROFILING: Start timing reactor callback
  ;;===========================================================================
  (setq profile-start (haws-profile-start "reactor-callback"))
  ;;===========================================================================
  ;; CRITICAL: Prevent nested/recursive callbacks (Autodesk guideline)
  ;; Problem: Bubble updates within callback trigger :vlr-modified on leader
  ;; Solution: Block all nested callbacks by setting flag at entry, restore at exit
  ;; Save current blocker state to honor parent-level blocks
  (setq block-reactors-current (hcnm-config-getvar "BlockReactors"))
  
  ;; DEBUG: Show callback entry with blocker state
  (haws-debug (list "[CALLBACK START] BlockReactors=" block-reactors-current))

  ;; Check all gateways - early exit if any blocked
  (cond
    ;; Gateway 1: Honor parent-level reactor blocker (prevents nested callbacks)
    ((= block-reactors-current "1") 
      (haws-debug 
        "[REACTOR BLOCKED] Gateway1=1 (nested callback blocked by parent)"
      )
    )
    
    ;; All other gates must pass to proceed with update
    ((and
       ;; Gateway 2: Notifier object still exists (not erased)
       (not (vl-catch-all-error-p 
              (vl-catch-all-apply 'vla-get-handle (list obj-notifier))))
     )
     ;; BLOCK NESTED CALLBACKS: Set flag to prevent recursion
     ;; Any bubble modifications below will trigger leader :vlr-modified
     ;; But nested callbacks will hit Gateway 1 and exit immediately
     (hcnm-config-setvar "BlockReactors" "1")
     ;; Extract reactor data and find notifier entry
     (setq 
       key-app "HCNM-BUBBLE"
       data-old (vlr-data obj-reactor)
       data data-old
       handle-notifier (vla-get-handle obj-notifier)
       owner-list (cadr (assoc key-app data))
       notifier-entry (assoc handle-notifier owner-list)
     )
     ;; DEBUG: Show what triggered this callback
     (haws-debug (list "[REACTOR FIRED] Notifier handle: " handle-notifier 
                      " Type: " (vla-get-objectname obj-notifier)))
     
     ;; Gateway 3: Verify notifier exists in data structure (defensive check)
     (cond
       (notifier-entry
        ;; All gates passed - update all bubbles dependent on this notifier
        ;; Returns cleaned notifier-entry (may have removed deleted bubbles)
        (setq notifier-entry 
          (hcnm-ldrblk-reactor-notifier-update notifier-entry handle-notifier))
        
        ;; Rebuild owner-list with cleaned notifier-entry
        (setq owner-list
          (subst notifier-entry (assoc handle-notifier owner-list) owner-list))
        
        ;; Update data structure
        (setq data (list (list key-app owner-list)))
        
        ;; POST-PROCESSING: Cleanup and maintenance
        (cond 
          ;; If notifier has no remaining bubbles, remove it as owner
          ((not (cadr notifier-entry))
           (vlr-owner-remove obj-reactor obj-notifier)
          )
        )
        (cond
          ;; If data structure changed, persist the changes
          ((not (equal data data-old))
           (vlr-data-set obj-reactor data)
          )
        )
       )
       (t
        ;; Gateway 3 failed - data corruption (should never happen)
        (princ 
          (strcat 
            "\nWarning: Notifier "
            handle-notifier
            " not found in reactor data"
            "\nReactor data structure: "
            (vl-prin1-to-string data)
          )
        )
       )
     )
    )
    ;; Gateway 2 blocked (object erased)
    (t
       (haws-debug (list "[REACTOR BLOCKED] Gateway2 failed (object erased or inaccessible)"))  
    )
  )
  
  ;; SELF-HEALING: Always restore blocker to parent state
  ;; Rationale: If Gateway 1 blocked (nested callback), we just exit cleanly
  ;;            If we processed updates, we set flag="1" to block children
  ;;            Now restore to parent's original state for next user action
  (haws-debug (list "[CALLBACK END] Restoring BlockReactors=" block-reactors-current))
  (hcnm-config-setvar "BlockReactors" block-reactors-current)
  ;;===========================================================================
  ;; PROFILING: End timing reactor callback
  ;;===========================================================================
  (haws-profile-end "reactor-callback" profile-start)
)
;;==============================================================================
;; hcnm-ldrblk-reactor-notifier-update
;;==============================================================================
;; Purpose:
;;   Updates all bubbles that depend on the notifier (the specific owner that triggered callback).
;;   Called by reactor callback when an alignment, pipe, surface, or leader changes.
;;
;; Arguments:
;;   notifier-entry - Data structure entry for the notifier (specific owner that triggered event)
;;                    Format: (handle-notifier ((handle-bubble tag-list) ...))
;;   handle-notifier - Handle string of the notifier (specific owner, for consistency check)
;;
;; Terminology:
;;   notifier = The specific owner that triggered THIS callback event
;;   owner = General term (any object attached to reactor: reference OR leader)
;;
;; Call Flow:
;;   reactor-callback → THIS FUNCTION → bubble-update → update-bubble-tag
;;
;; Performance Notes:
;;   - Parent callback already set BlockReactors="1" (nested callbacks blocked)
;;   - Processes ALL bubbles for this notifier in one pass
;;   - Each bubble gets ONE combined attribute update (not per-tag)
;;
;; Side Effects:
;;   - Modifies bubble attributes via bubble-update function
;;   - No flag management (parent callback handles BlockReactors lifecycle)
;;
;; Data Structure:
;;   notifier-entry = (handle-notifier bubble-list)
;;   bubble-list = ((handle-bubble tag-list) ...)
;;   tag-list = ((tag auto-list) ...)
;;
;; Example:
;;   (hcnm-ldrblk-reactor-notifier-update 
;;     '("1D235" (("62880" (("NOTETXT1" (("StaOff" "1D235")))))))
;;     "1D235"
;;   )
;;==============================================================================
(defun hcnm-ldrblk-reactor-notifier-update (notifier-entry handle-notifier / 
                                            bubble-list handle-bubble tag-list updated-any
                                            deleted-handles update-result cleaned-bubble-list)
  ;; Extract data from notifier entry
  (setq
    handle-notifier (car notifier-entry)
    bubble-list (cadr notifier-entry)
    updated-any nil  ; Track if we actually updated any bubble
    deleted-handles nil  ; Track bubbles that no longer exist
  )
  
  ;; NOTE: No flag setting here - parent callback already set BlockReactors="1"
  ;; This blocks ALL nested callbacks (leader modifications triggered by attribute updates)
  
  ;; Process each bubble that depends on this notifier
  (foreach
     bubble bubble-list
    (setq
      handle-bubble (car bubble)
      tag-list (cadr bubble)
    )
    ;; Update this bubble
    ;; Returns: T (modified), nil (skipped), "DELETED" (erased)
    (setq update-result (hcnm-ldrblk-reactor-bubble-update handle-bubble handle-notifier tag-list))
    (cond
      ((equal update-result "DELETED")
       ;; Bubble erased - add to cleanup list
       (setq deleted-handles (cons handle-bubble deleted-handles)))
      (update-result
       ;; Bubble modified successfully
       (setq updated-any T))
    )
  )
  
  ;; CLEANUP: Remove deleted bubbles from notifier entry by rebuilding list
  (cond
    (deleted-handles
     (princ (strcat "\n[NOTIFIER-UPDATE] Removing " (itoa (length deleted-handles)) " deleted bubble(s) from reactor data"))
     ;; Filter out deleted bubbles - returns new cleaned list
     (setq cleaned-bubble-list
       (vl-remove-if
         '(lambda (bubble) (member (car bubble) deleted-handles))
         bubble-list))
     ;; Return cleaned notifier entry (handle . cleaned-bubble-list)
     ;; Caller will use this to rebuild owner-list
     (list handle-notifier cleaned-bubble-list)
    )
    ;; No cleanup needed - return original
    (t notifier-entry)
  )
)
;;==============================================================================
;; hcnm-ldrblk-reactor-bubble-update
;;==============================================================================
;; Purpose:
;;   Updates all auto-text fields in one bubble after the notifier (specific owner)
;;   triggered a callback. Processes multiple tags and multiple auto-texts per tag.
;;
;; Arguments:
;;   handle-bubble - Handle string of bubble to update
;;   handle-notifier - Handle of the notifier (specific owner that triggered callback)
;;                     Used only for legacy format migration (fallback reference)
;;   tag-list - List of tags with auto-text entries to update
;;              Format: ((tag auto-list) ...)
;;              where auto-list = (("auto-type" "handle-reference") ...) [NEW]
;;              or auto-list = "auto-type" [OLD - deprecated]
;;
;; Call Flow:
;;   reactor-callback → notifier-update → THIS FUNCTION → update-bubble-tag (per auto-text)
;;
;; Terminology:
;;   notifier = Specific owner that triggered callback (alignment/pipe/leader)
;;   reference = Object that provides calculation data (stored in leaf, always alignment/pipe, never leader)
;;   For most updates: notifier = reference (same object)
;;   For leader-based updates: notifier = leader, reference = alignment/pipe (extracted from data)
;;
;; Algorithm:
;;   1. Skip if bubble erased (early exit)
;;   2. Foreach tag in tag-list:
;;      a. Check format (new list vs old string)
;;      b. Foreach auto-entry in auto-list:
;;         - Extract auto-type and handle-reference
;;         - Call update-bubble-tag to regenerate auto-text
;;
;; Data Format Migration:
;;   NEW: auto-list = (("StaOff" "1D235") ("Dia" "1D235"))  ← Multiple auto-texts per tag
;;   OLD: auto-list = "StaOff"  ← Single string, fallback uses handle-notifier as reference
;;        (Prints warning, continues with degraded functionality for leader-based bubbles)
;;
;; Performance Issues (KNOWN - documented in Section 1.2.1.6):
;;   - Each update-bubble-tag writes XDATA + attributes independently (non-atomic)
;;   - FUTURE OPTIMIZATION: Accumulate lattribs changes, write once per bubble after all loops
;;     Solution: Pass accumulator alist to update-bubble-tag, return updated accumulator,
;;     write complete lattribs + XDATA atomically after foreach loops complete
;;
;; Side Effects:
;;   - Calls update-bubble-tag which modifies bubble XDATA and attributes (per auto-text!)
;;   - Prints warnings for old format or unexpected data structure
;;
;; Example (new format):
;;   (hcnm-ldrblk-reactor-bubble-update 
;;     "62880"                                    ; bubble handle
;;     "1D235"                                    ; notifier handle (alignment)
;;     '(("NOTETXT1" (("StaOff" "1D235")          ; tag with auto-list
;;                    ("Dia" "1D235"))))          ; multiple auto-texts per tag
;;   )
;;==============================================================================
(defun hcnm-ldrblk-reactor-bubble-update (handle-bubble handle-notifier tag-list / tag auto-list auto-entry auto-type handle-reference updated-any ename-bubble)
  ;; Check if bubble is active (not erased)
  ;; Active bubble: (entget (handent handle)) returns entity data
  ;; Erased bubble: (entget (handent handle)) returns nil
  (setq ename-bubble (handent handle-bubble))
  (cond
    ;; If entget returns nil, bubble was erased - return "DELETED" for cleanup
    ((not (entget ename-bubble))
     (haws-debug (list "[REACTOR CLEANUP] Bubble handle " handle-bubble " erased (removing from reactor data)"))
     "DELETED"
    )
    ;; Bubble is active - process updates
    (t
     (setq updated-any nil)  ; Track if we actually modified anything
     (foreach
        tag-data tag-list
       (setq
         tag (car tag-data)
         auto-list (cadr tag-data)  ; List of (auto-type reference-handle) 2-element lists
       )
       ;; Check if this is old format (string) or new format (list of 2-element lists)
       (cond
         ;; New format: list of 2-element lists (auto-type reference-handle)
         ((and (listp auto-list) (listp (car auto-list)))
          (foreach
             auto-entry auto-list
            (setq
              auto-type (car auto-entry)
              handle-reference (cadr auto-entry)  ; Second element, not cdr
            )
            ;; FUTURE OPTIMIZATION (see Section 1.2.1.6): Accumulate updates, write once per bubble
            ;; Solution: Return updated lattribs instead of writing. Accumulate across
            ;; all auto-entries, then write once after foreach loops complete.
            (if (hcnm-ldrblk-update-bubble-tag
                  handle-bubble
                  tag
                  auto-type
                  handle-reference
                )
              (setq updated-any T)  ; Tag was updated
            )
          )
         )
         ;; Old format: just auto-type string, use handle-notifier as reference
         ((atom auto-list)
          (princ "\nWARNING: Old reactor data format detected - please re-insert bubble")
          (if (hcnm-ldrblk-update-bubble-tag
                handle-bubble
                tag
                auto-list  ; auto-type
                handle-notifier  ; Use notifier as reference (may be wrong for leaders)
              )
            (setq updated-any T)
          )
         )
         (t
          (princ (strcat "\nERROR: Unexpected reactor data format for tag " tag))
         )
       )
     )
     updated-any  ; Return T if anything was modified, NIL otherwise
    )
  )
)

;#endregion

;#region Smart Replace & Update Helpers
;;==============================================================================
;; Helper functions for reactor update path: extract old auto-text, generate new,
;; smart-replace in user text, write XDATA + attributes.
;; These functions preserve user edits while updating auto-text fields.
;;==============================================================================

;; Helper function to check if a bubble has a specific leader
(defun hcnm-ldrblk-bubble-has-leader
   (handle-bubble handle-leader / ename-bubble ename-leader)
  (setq ename-bubble (handent handle-bubble))
  (cond
    (ename-bubble
     (setq ename-leader (hcnm-ldrblk-bubble-leader ename-bubble))
     (cond
       (ename-leader
        (= handle-leader (cdr (assoc 5 (entget ename-leader))))
       )
       (t nil)
     )
    )
    (t nil)
  )
)
;; Helper function to check if entity is on the "Model" tab
(defun hcnm-ldrblk-is-on-model-tab (ename / layout-name)
  (setq layout-name (cdr (assoc 410 (entget ename))))
  (= (strcase layout-name) "MODEL")
)

;;==============================================================================
;; hcnm-ldrblk-extract-old-auto-text
;;==============================================================================
;; Purpose:
;;   Extracts the old auto-text value from bubble's XDATA for a specific tag
;;   and reference handle. This is the "search needle" for smart replace.
;;
;; Arguments:
;;   ename-bubble - Entity name of bubble
;;   tag - Attribute tag (e.g., "NOTETXT1")
;;   auto-type - Auto-type string (e.g., "StaOff", "Dia")
;;   handle-reference - Handle of reference object (alignment/pipe) or ""
;;
;; Returns:
;;   String: Old auto-text value, or NIL if not found
;;
;; Data Format (COMPOSITE-KEY ONLY):
;;   Handle-based XDATA: (("TAG" ((composite-key . "auto") ...)) ...)
;;   Where composite-key = (cons auto-type handle-reference)
;;
;; ARCHITECTURAL NOTE (2025-11-06):
;;   Simple format DEPRECATED. Only composite-key format supported.
;;   If simple string format found, FAIL LOUDLY (data corruption).
;;
;; Why This Matters:
;;   Users can edit bubble text directly in AutoCAD. We store verbatim auto-text
;;   in XDATA so we can find and replace it without corrupting user edits.
;;
;; Example:
;;   User text: "Storm STA 10+25.50 RT"
;;   XDATA: "STA 10+25.50"
;;   New auto: "STA 11+00.00"
;;   Result: "Storm STA 11+00.00 RT" (user prefix/postfix preserved)
;;==============================================================================
(defun hcnm-ldrblk-extract-old-auto-text (ename-bubble tag auto-type handle-reference / xdata-alist tag-xdata composite-key)
  (setq xdata-alist (hcnm-xdata-read ename-bubble))
  (setq tag-xdata (cdr (assoc tag xdata-alist)))
  (cond
    ;; Expected: Composite-key XDATA format
    ((and tag-xdata (listp tag-xdata) (listp (car tag-xdata)))
     (setq composite-key (cons auto-type handle-reference))
     (cdr (assoc composite-key tag-xdata))
    )
    ;; FAIL LOUDLY: Simple string format is data corruption
    ((and tag-xdata (atom tag-xdata))
     (alert
       (princ
         (strcat
           "\nDATA CORRUPTION: Simple format XDATA found in bubble!"
           "\n"
           "\nTag: " tag
           "\nValue: " (vl-prin1-to-string tag-xdata)
           "\n"
           "\nExpected composite-key format: ((composite-key . \"auto\") ...)"
           "\nFound simple format (deprecated): \"string\""
           "\n"
           "\nThis should not happen. All bubbles must use composite-key format."
           "\nPlease report this error to the developer."
         )
       )
     )
     nil  ; Return nil, don't use corrupted data
    )
    ;; No XDATA found (normal for bubbles without auto-text)
    (t nil)
  )
)

;;==============================================================================
;; hcnm-ldrblk-xdata-update-one
;;==============================================================================
;; Purpose:
;;   Updates a single auto-text entry in bubble's XDATA without affecting other entries.
;;   Used by reactor callbacks to update one auto-text field at a time.
;;
;; Arguments:
;;   ename-bubble - Entity name of bubble
;;   tag - Attribute tag (e.g., "NOTETXT1")
;;   auto-type - Auto-type string (e.g., "StaOff", "Dia")
;;   handle-reference - Handle of reference object or "" for coordinates
;;   auto-text - New auto-text value to store
;;
;; Returns:
;;   T if successful, NIL otherwise
;;
;; Call Pattern:
;;   Reactor callback → update-bubble-tag → THIS FUNCTION (one per auto-text field)
;;   Dialog save → hcnm-ldrblk-xdata-save (writes entire semi-global at once)
;;
;; ARCHITECTURAL NOTE (2025-11-06):
;;   This function ALWAYS writes composite-key format.
;;   Simple format DEPRECATED - all bubbles use composite-key format.
;;
;; Example:
;;   (hcnm-ldrblk-xdata-update-one ename-bubble "NOTETXT1" "StaOff" "ABC123" "STA 10+25.50")
;;==============================================================================
(defun hcnm-ldrblk-xdata-update-one (ename-bubble tag auto-type handle-reference auto-text / 
                                     xdata-alist tag-xdata composite-key composite-entry tag-entry
                                     profile-start
                                    )
  ;;===========================================================================
  ;; PROFILING: Start timing XDATA write (hot path, inherently slow)
  ;;===========================================================================
  (setq profile-start (haws-profile-start "reactor-xdata-write"))
  (setq xdata-alist (hcnm-xdata-read ename-bubble))
  (setq tag-entry (assoc tag xdata-alist))
  (setq tag-xdata (cdr tag-entry))
  
  ;; Build composite key
  (setq composite-key (cons auto-type handle-reference))
  
  (cond
    ;; Tag exists with composite key format
    ((and tag-xdata (listp tag-xdata) (listp (car tag-xdata)))
     (setq composite-entry (assoc composite-key tag-xdata))
     (cond
       ;; Update existing composite key
       (composite-entry
        (setq tag-xdata (subst (cons composite-key auto-text) composite-entry tag-xdata))
       )
       ;; Add new composite key
       (t
        (setq tag-xdata (append tag-xdata (list (cons composite-key auto-text))))
       )
     )
     ;; Replace tag in alist - use (cons tag tag-xdata) for dotted pair
     ;; (cdr) will extract tag-xdata directly without extra nesting
     (setq xdata-alist (subst (cons tag tag-xdata) tag-entry xdata-alist))
    )
    ;; Tag doesn't exist or is simple format - create composite key format
    (t
     (setq tag-xdata (list (cons composite-key auto-text)))
     (cond
       (tag-entry
        ;; Replace existing simple format with composite key format
        ;; Use (cons tag tag-xdata) for dotted pair
        (setq xdata-alist (subst (cons tag tag-xdata) tag-entry xdata-alist))
       )
       (t
        ;; Add new tag - use (cons tag tag-xdata) for dotted pair
        (setq xdata-alist (append xdata-alist (list (cons tag tag-xdata))))
       )
     )
    )
  )
  
  ;; Write updated XDATA
  (hcnm-xdata-set-autotext ename-bubble xdata-alist)
  ;;===========================================================================
  ;; PROFILING: End timing XDATA write
  ;;===========================================================================
  (haws-profile-end "reactor-xdata-write" profile-start)
  T
)

;;==============================================================================
;; hcnm-ldrblk-generate-new-auto-text
;;==============================================================================
;; Purpose:
;;   Generates fresh auto-text by calling auto-dispatch with reference object.
;;   Returns updated lattribs with new auto-text value (plain, no format codes).
;;
;; Arguments:
;;   ename-bubble - Entity name of bubble
;;   ename-reference - Entity name of reference object (or NIL for N/E/NE)
;;   lattribs - Current lattribs (2-element format)
;;   tag - Attribute tag to update
;;   auto-type - Auto-type string (e.g., "StaOff", "Dia", "N")
;;
;; Returns:
;;   Updated lattribs with new auto-text value in specified tag
;;
;; Special Cases:
;;   - For N/E/NE (no reference), passes T as sentinel to auto-dispatch
;;   - For reactor updates, auto-dispatch knows not to prompt user
;;
;; Side Effects:
;;   - May read viewport transform data from XDATA
;;   - Does NOT modify drawing (returns data only)
;;==============================================================================
(defun hcnm-ldrblk-generate-new-auto-text (ename-bubble ename-reference lattribs tag auto-type / objref)
  ;; Convert entity to VLA object, or use T sentinel for coordinate-only types
  (setq
    objref
     (cond
       (ename-reference (vlax-ename->vla-object ename-reference))
       (t t)  ; Sentinel for N/E/NE reactor updates (no reference object)
     )
  )
  ;; Call auto-dispatch to generate new auto-text
  (hcnm-ldrblk-auto-dispatch ename-bubble lattribs tag auto-type objref)
)

;;==============================================================================
;; hcnm-ldrblk-update-bubble-tag
;;==============================================================================
;; Purpose:
;;   Updates ONE specific auto-text field in a bubble after reference object changes.
;;   Core reactor update function - regenerates auto-text and smart-replaces in existing text.
;;
;; Arguments:
;;   handle-bubble - Handle string of bubble to update
;;   tag - Attribute tag to update (e.g., "NOTETXT1", "NOTETXT2")
;;   auto-type - Auto-type string specifying calculation (e.g., "StaOff", "Dia", "N", "E")
;;   handle-reference - Handle of reference object (alignment/pipe/surface) or "" for N/E/NE
;;
;; Terminology:
;;   reference = Object that provides calculation data (alignment, pipe, surface)
;;   For coordinate-based (N/E/NE): handle-reference = "" (uses leader arrowhead position)
;;   For alignment-based: handle-reference = alignment handle
;;   For pipe-based: handle-reference = pipe handle
;;
;; Call Flow:
;;   reactor-callback → notifier-update → bubble-update → THIS FUNCTION (once per auto-text field)
;;
;; Algorithm (Smart Replace to Preserve User Edits):
;;   1. Read current attribute text (may include user prefix/postfix edits)
;;   2. Extract old auto-text from XDATA (the "search needle")
;;   3. Generate new auto-text via auto-dispatch (recalculate from reference)
;;   4. Smart replace: find old auto-text in current text, replace with new
;;   5. Write XDATA (store updated auto-text as new search needle)
;;   6. Write attributes (save full text with format codes applied)
;;
;; Smart Replace Example (User Edits Preserved):
;;   Current attribute text: "Storm STA 10+25.50 RT"  ← User added prefix/postfix
;;   Old auto-text (XDATA): "STA 10+25.50"            ← Search needle
;;   New auto-text (calculated): "STA 11+00.00"       ← Alignment changed
;;   Result: "Storm STA 11+00.00 RT"                  ← User text preserved!
;;
;; Performance Issues (KNOWN - documented in Section 1.2.1.6):
;;   - Called ONCE PER AUTO-TEXT FIELD (can be 5+ times per bubble if user has N, E, StaOff, Dia, etc.)
;;   - Each call writes XDATA + attributes independently (NON-ATOMIC - BAD DESIGN!)
;;   - FUTURE OPTIMIZATION: Refactor to accumulate updates, write once per bubble
;;     Solution: Change signature to (update-bubble-tag ... lattribs-accumulator),
;;     return updated accumulator instead of writing. Move write to bubble-update
;;     after all foreach loops complete. This ensures atomic update of all tags.
;;
;; Side Effects:
;;   - Writes XDATA via hcnm-ldrblk-xdata-update-one (stores new search needle)
;;   - Writes formatted attributes via hcnm-set-attributes (applies format codes)
;;   - May alert if bubble not found (defensive error message)
;;
;; Edge Cases & Gotchas:
;;   - If user manually corrupts auto-text in attribute, smart replace fails silently (by design)
;;   - Multiple identical auto-texts in same tag: only updates first occurrence (known limitation)
;;   - If reference object erased: auto-dispatch returns NIL, smart replace no-ops gracefully
;;   - If auto-dispatch fails: attribute unchanged, XDATA unchanged (safe failure mode)
;;
;; Example:
;;   (hcnm-ldrblk-update-bubble-tag 
;;     "62880"      ; bubble handle
;;     "NOTETXT1"   ; attribute tag
;;     "StaOff"     ; auto-type
;;     "1D235"      ; alignment handle
;;   )
;;==============================================================================
(defun hcnm-ldrblk-update-bubble-tag (handle-bubble tag auto-type handle-reference / 
                                      ename-bubble ename-reference lattribs lattribs-old
                                      attr current-text old-auto-text auto-new new-text
                                      profile-start result
                                     )
  ;;===========================================================================
  ;; PROFILING: Start timing tag update (hot path)
  ;;===========================================================================
  (setq profile-start (haws-profile-start "reactor-update-tag"))
  (setq ename-bubble (handent handle-bubble))
  
  ;; Execute main logic and capture result
  (setq result
  (cond
    ((not ename-bubble)
     (princ
       (strcat
         "\n[REACTOR CLEANUP] Bubble handle "
         handle-bubble
         " not found - will be removed from reactor data"
       )
     )
     nil  ; Return nil - parent function detects via handent check
    )
    
    ;; Gateway 6: Entity read check - skip if entity can't be read
    ((not (entget ename-bubble))
     (princ
       (strcat
         "\n[REACTOR SKIPPED] Bubble handle "
         handle-bubble
         " Gateway 6 (entity-readable): CLOSED [entget returned nil]"
         "\n  This can happen during UNDO/REDO or rapid consecutive modifications."
         "\n  Skipping this update to prevent corruption."
       )
     )
     nil  ; Skip this update
    )
    
    ;; MAIN PROCESSING: Bubble exists and is readable, proceed with update
    (t
     ;; STEP 1: Read current state
     (setq
       ename-reference
        (cond
          ((= handle-reference "") nil)
          (t (handent handle-reference))
        )
       lattribs-old (hcnm-ldrblk-dwg-to-lattribs ename-bubble t)
       lattribs lattribs-old
       attr (assoc tag lattribs)
       current-text (if attr (cadr attr) "")
     )
     
     ;; STEP 2: Extract old auto-text from XDATA (search needle)
     (setq old-auto-text 
       (hcnm-ldrblk-extract-old-auto-text ename-bubble tag auto-type handle-reference)
     )
     ;; DEBUG OUTPUT
    (haws-debug (list "=== DEBUG update-bubble-tag ==="
                  "\n  Tag: " tag
                  "\n  Auto-type: " auto-type
                  "\n  Handle-reference: " (if (= handle-reference "") "(empty string)" handle-reference)
                  "\n  Old auto-text: " (if old-auto-text old-auto-text "NIL")
                  "\n  XDATA alist: " (vl-prin1-to-string (hcnm-xdata-read ename-bubble))))
     ;; STEP 3: Generate new auto-text via auto-dispatch
     (setq lattribs 
       (hcnm-ldrblk-generate-new-auto-text 
         ename-bubble ename-reference lattribs tag auto-type
       )
     )
     
     ;; STEP 4: Extract generated auto-text (plain, no format codes)
     (setq
       attr (assoc tag lattribs)
       auto-new (if attr (cadr attr) "")
     )
     
     ;; STEP 5: Smart replace - preserve user edits around auto-text
     (setq new-text
       (hcnm-ldrblk-smart-replace-auto current-text old-auto-text auto-new)
     )
     ;; DEBUG OUTPUT
     (haws-debug 
       (list "  Current text: " current-text "\n  New auto: " auto-new 
              "\n  Result text: " new-text))
     ;; STEP 6: Update lattribs with smartly-replaced text
     (setq lattribs
       (cond
         (attr (subst (list tag new-text) attr lattribs))
         (t (append lattribs (list (list tag new-text))))
       )
     )
     
     ;; STEP 7: Write to drawing if changed
     ;; PERFORMANCE NOTE: This writes XDATA + attributes for EACH call (BAD DESIGN)
     ;; FUTURE OPTIMIZATION (see Section 1.2.1.6): Accumulate changes at bubble level, write once
     ;; Solution: Remove this write block. Return lattribs instead. Let bubble-update
     ;; collect all returned lattribs and write once after processing all auto-entries.
     (cond
       ((/= lattribs lattribs-old)
        ;; Update XDATA for this specific composite key (preserves other auto-text entries)
        ;; This maintains composite key format for reactor updates
        (hcnm-ldrblk-xdata-update-one ename-bubble tag auto-type handle-reference auto-new)
        ;; Format for display (adds underline/overline codes)
        (setq lattribs (hcnm-ldrblk-underover-add lattribs))
        ;; Write formatted attributes (uses VLA methods, not entmod)
        (hcnm-set-attributes ename-bubble lattribs)
        T  ; Return T to indicate modification occurred
       )
       (t
        nil  ; Return nil if no changes
       )
     )
    )
  )
  )
  ;;===========================================================================
  ;; PROFILING: End timing tag update (always called, all exit paths)
  ;;===========================================================================
  (haws-profile-end "reactor-update-tag" profile-start)
  result  ; Return the result from main logic
)

;#endregion

;#region Bubble note editor dialog
(defun c:hcnm-edit-bubbles ()
  (haws-core-init 337)
  (princ "\nCNM version: ")
  (princ (haws-unified-version))
  (princ " [XDATA-FIX-18]")             ; Issue progress tracker
  (if (not haws-editall)
    (load "editall")
  )
  (haws-editall t)
  (haws-core-restore)
)
;;==============================================================================
;; hcnm-ldrblk-eb-init-auto-handles
;;==============================================================================
;; Purpose:
;;   Initialize semi-global auto-handles from existing XDATA when dialog opens.
;;   Extracts only composite-key format entries (lists), filters out simple strings.
;;
;; Arguments:
;;   ename-bubble - Entity name of bubble
;;
;; Returns:
;;   Alist in format: (("TAG" ((composite-key . "auto-text") ...)) ...)
;;
;; Why:
;;   Smart replace needs existing auto-text as "search needle". Without this,
;;   editing a bubble that already has StaOff would have no old-auto-text to replace.
;;
;; ARCHITECTURAL NOTE (2025-11-06):
;;   Simple format DEPRECATED. All bubbles now use composite-key format.
;;   No filtering needed - hcnm-xdata-read returns composite-key format only.
;;   If simple format found, hcnm-xdata-read will FAIL LOUDLY (data corruption).
;;
;; Example:
;;   XDATA has: (("NOTETXT1" ((("StaOff" . "ABC123") . "STA 10+25.50"))))
;;   Returns:   (("NOTETXT1" ((("StaOff" . "ABC123") . "STA 10+25.50"))))
;;==============================================================================
(defun hcnm-ldrblk-eb-init-auto-handles (ename-bubble)
  ;; Simply return XDATA directly - no filtering needed
  ;; If simple format exists, hcnm-xdata-read will alert (data corruption)
  (hcnm-xdata-read ename-bubble)
)

(defun hcnm-edit-bubble (ename-bubble / bubble-data dclfile ename-leader
                     hcnm-ldrblk-eb-lattribs hcnm-ldrblk-eb-auto-handles
                     notetextradiocolumn return-list tag done-code 
                    )
  (haws-debug (list "=== DEBUG: Entering hcnm-edit-bubble"))
  (setq
    ename-leader
     (hcnm-ldrblk-bubble-leader ename-bubble)
    ;; Semi-global variable. Global to the hcnm-ldrblk-eb- functions called from here.
    ;; Read attributes and XDATA to get prefix/auto/postfix structure
    hcnm-ldrblk-eb-lattribs
     (hcnm-ldrblk-dwg-to-lattribs ename-bubble t)
    ;; Semi-global: Track handle associations for auto-texts during editing
    ;; Format: (("TAG" ((composite-key . "auto-text") ...)) ...)
    ;; where composite-key = (cons auto-type handle-reference)
    ;; Initialize from XDATA to preserve existing auto-texts for smart replace
    hcnm-ldrblk-eb-auto-handles
     (hcnm-ldrblk-eb-init-auto-handles ename-bubble)
    notetextradiocolumn "RadioNOTETXT1"
    dclfile
     (load_dialog "cnm.dcl")
    done-code 2
  )
  (haws-debug
    (list
          "=== DEBUG: lattribs read, count="
          (itoa (length hcnm-ldrblk-eb-lattribs))
          "\n=== DEBUG: dclfile="
          (if dclfile
            (itoa dclfile)
            "FAILED"
          )
    )
  )
  ;; Validate lattribs before proceeding
  (cond
    ((not hcnm-ldrblk-eb-lattribs)
     (alert (princ "\nERROR: Failed to read bubble attributes"))
     (haws-core-restore)
     (princ)
    )
    ((not dclfile)
     (alert
       (princ "\nERROR: Failed to load cnm.dcl dialog file")
     )
     (haws-core-restore)
     (princ)
    )
    (t
     ;; Continue with dialog
     (haws-debug (list "=== DEBUG: Showing tip..."))
     ;; Show tip about auto-text editing expectations
     (haws-tip
       3                                ; Unique tip ID for auto-text editing explanation
       (strcat
         "About Editing with Auto Text and Delimiters\n\n"
         "CNM does its best to keep your existing auto text straight if you don't touch it. It adds new auto text at the end of your free form text unless you indicate the desired insertion location with \"```\" (three backquotes usually on the same key as ~ tilde).\n\n"
       )
     )
     (haws-tip
       5                                ; Unique tip ID for auto-text editing explanation
       (strcat
         "Understanding each other: CNM gives you free-form user edits with the following reasonable expectations:\n\n"
         "  - CNM keeps a separate hidden copy of your auto text in your bubble note's XDATA and uses it to identify your auto text for updates in the event your reference object or your arrowhead changes.\n"
         "  - Any text you add remains intact, and any auto text you respect updates correctly.\n"
         "  - If you change CNM Project settings that affect auto text format, the next update reflects those changes as long as you do not change individual auto text manually.\n"
         "  - If you completely delete (or fat-finger-corrupt) auto text or change its format (eg. adding prefixes/suffixes), it does not get acted on or restored at the next update.\n"
         "  - You can't have multiple auto text fields with identical values in the same bubble note line and have them all update correctly."
        )
     )
     (haws-debug (list "=== DEBUG: Entering dialog loop..."))
     (while (> done-code -1)
       (cond
         ((= done-code 0) (setq done-code (hcnm-edit-bubble-cancel)))
         ((= done-code 1)
          (setq done-code (hcnm-ldrblk-eb-save ename-bubble))
         )
         ((= done-code 2)
          ;; Show the CNM Bubble Note Editor dialog with the requested text line's radio button selected.
          (setq
            return-list
             (hcnm-ldrblk-eb-show
               dclfile
               notetextradiocolumn
               ename-bubble
             )
            done-code
             (car return-list)
            notetextradiocolumn
             (cadr return-list)
            tag
             (substr notetextradiocolumn 6)
          )
         )
         ((= done-code 29)
          ;; Change View button - clear viewport transformation data and immediately prompt for new association
          (hcnm-ldrblk-clear-viewport-transform-xdata ename-bubble)
          (princ
            "\nViewport association cleared. Please select the new target viewport."
          )
          ;; Use super-clearance path to bypass all gateways and force prompt
          (hcnm-ldrblk-gateways-to-viewport-selection-prompt
            ename-bubble "Sta"          ; Representative coordinate type for warning message
            nil                         ; No input object
            nil                         ; No object reference status needed for super-clearance
            "LINK-VIEWPORT"
           )                            ; Super-clearance - bypass all gateways
          (setq done-code 2)            ; Return to dialog
         )
         (t
          ;; Process clicked action tile (button) other than cancel or save.
          ;; bubble-data-update: This is start point 2 of 2 of the bubble data logic. This one is for the bubble note editing process.
          ;; this is called whenever a dialog auto-text button is clicked.
          (hcnm-ldrblk-eb-get-text ename-bubble done-code tag)
          (setq done-code 2)
         )
       )
     )
     ;; Change its arrowhead if needed.
     (hcnm-ldrblk-change-arrowhead ename-leader)
     (haws-debug (list "=== DEBUG: Dialog loop complete, cleaning up..."))
    )
  )
  ;; Close the validation cond
  
  ;; DEFENSIVE: Reset BlockReactors flag after dialog completes
  ;; Ensures stuck flags don't persist across user actions
  (hcnm-config-setvar "BlockReactors" "0")
  
  (haws-core-restore)
  (princ)
)

;;; Extract handle from reactor data for a specific tag
;;; Used to track handle associations during dialog editing
;;; Returns: handle string or "" if not found/not applicable
(defun hcnm-ldrblk-get-reactor-handle-for-tag (ename-bubble tag /
                                           hcnm-reactors reactor
                                           reactor-data handle-bubble
                                           handle-ref bubble-entries
                                           bubble-entry bubble-handle
                                           tag-entries found-handle
                                          )
  (setq
    handle-bubble
     (cdr (assoc 5 (entget ename-bubble)))
    found-handle nil
  )                                     ; Initialize return value
  ;; Find the HCNM-BUBBLE reactor
  (setq
    hcnm-reactors
     (vl-remove-if-not
       '(lambda (r)
          (and
            (listp (vlr-data r))
            (assoc "HCNM-BUBBLE" (vlr-data r))
          )
        )
       (cdar (vlr-reactors :vlr-object-reactor))
     )
    reactor
     (car hcnm-reactors)
  )
  (cond
    (reactor
     (setq reactor-data (vlr-data reactor))
     ;; Navigate nested structure: ("HCNM-BUBBLE" (handle-ref (handle-bubble (tag . auto-type))))
     ;; We need to find the entry where handle-bubble and tag match, then extract handle-ref
     (setq reactor-data (cdr (assoc "HCNM-BUBBLE" reactor-data)))
     ;; Now reactor-data is: ((handle-ref1 (handle-bubble1 (tag1 . auto-type1))) ...)
     (foreach
        handle-entry reactor-data
       (setq
         handle-ref
          (car handle-entry)
         bubble-entries
          (cdr handle-entry)
       )
       ;; bubble-entries is: ((handle-bubble1 (tag1 . auto-type1)) ...)
       (foreach
          bubble-entry bubble-entries
         (setq
           bubble-handle
            (car bubble-entry)
           tag-entries
            (cdr bubble-entry)
         )
         ;; Check if this is our bubble
         (cond
           ((= bubble-handle handle-bubble)
            ;; tag-entries is: ((tag1 . auto-type1) ...)
            (cond
              ((assoc tag tag-entries)
               ;; Found our tag! Save the handle-ref from outer loop
               (setq found-handle handle-ref)
              )
            )
           )
         )
       )
     )
     ;; Return the found handle (or "" if not found)
     (if found-handle
       found-handle
       ""
     )
    )
    (t "")                              ; No reactor found - return empty string
  )
)

(defun hcnm-ldrblk-eb-get-text (ename-bubble done-code tag / auto-string
                            auto-type attr current-text old-auto-text
                            new-text handle-ref tag-handles composite-key
                            existing-entry
                           )
  (setq
    auto-type
     (cadr (assoc done-code (hcnm-edit-bubble-done-codes)))
  )
  (cond
    ;; Handle ClearAuto button (code 28)
    ((= done-code 28)
     ;; Clear auto-text: remove XDATA and keep only user text
     ;; NOTE: In 2-element architecture, auto-text is stored in XDATA
     ;; Dialog shows full concatenated text, XDATA stores search needles
     ;; Clearing auto means: keep display text as-is, remove XDATA
     (setq attr (assoc tag hcnm-ldrblk-eb-lattribs))
     ;; For now, just keep the text as-is (XDATA will be cleared on save)
     ;; IMPLEMENTATION NOTE: XDATA clearing handled by model layer on save
    )
    ;; Handle auto-text generation buttons (only if auto-type is valid)
    ((and auto-type (not (= auto-type "")))
     ;; STEP 1: Save current text BEFORE auto-dispatch modifies it
     (setq attr (assoc tag hcnm-ldrblk-eb-lattribs))
     (setq
       current-text
        (if attr
          (cadr attr)
          ""
        )
     )
     ;; STEP 2: Get old auto-text from XDATA using composite key
     ;; First need handle-ref to build composite key
     (setq
       handle-ref
        (hcnm-ldrblk-get-reactor-handle-for-tag
          ename-bubble
          tag
        )
     )
     (cond
       ((not handle-ref) (setq handle-ref "")) ; Safety check
       ((not (= (type handle-ref) 'str)) (setq handle-ref ""))
     )
     ;; Now search semi-global for existing auto-text with this composite key
     (setq composite-key (cons auto-type handle-ref))
     (setq tag-handles (cdr (assoc tag hcnm-ldrblk-eb-auto-handles)))
     (setq existing-entry (assoc composite-key tag-handles))
     (setq old-auto-text (if existing-entry (cdr existing-entry) nil))
     
     ;; STEP 3: Generate new auto-text via auto-dispatch
     ;; NOTE: auto-dispatch REPLACES the entire tag value with just auto-text
     ;; bubble-data-update: this is called from command line and from edit box to get string as requested by user.
     (setq
       hcnm-ldrblk-eb-lattribs
        (hcnm-ldrblk-auto-dispatch
          ename-bubble hcnm-ldrblk-eb-lattribs tag auto-type nil
                                        ; nil = initial creation of auto text, not reactor update
         )
     )
     ;; STEP 4: Extract just the auto-text that was generated (plain text, no format codes)
     (setq attr (assoc tag hcnm-ldrblk-eb-lattribs))
     (setq
       auto-string
        (if attr
          (cadr attr)
          ""
        )
     )
     ;; STEP 4.5: Store in semi-global using composite key
     ;; Replace existing entry or append new one
     (setq composite-key (cons auto-type handle-ref))
     (setq tag-handles (cdr (assoc tag hcnm-ldrblk-eb-auto-handles)))
     (cond
       (existing-entry  ; Found - REPLACE
        (setq tag-handles (subst (cons composite-key auto-string) existing-entry tag-handles)))
       (t  ; Not found - APPEND
        (setq tag-handles (append tag-handles (list (cons composite-key auto-string))))))
     
     (setq
       hcnm-ldrblk-eb-auto-handles
        (cond
          ((assoc tag hcnm-ldrblk-eb-auto-handles)
           (subst
             (cons tag tag-handles)
             (assoc tag hcnm-ldrblk-eb-auto-handles)
             hcnm-ldrblk-eb-auto-handles
           )
          )
          (t
           (append
             hcnm-ldrblk-eb-auto-handles
             (list (cons tag tag-handles))
           )
          )
        )
     )
     ;; STEP 5: Do smart search/replace using shared function
     (setq
       new-text
        (hcnm-ldrblk-smart-replace-auto
          current-text
          old-auto-text
          auto-string
        )
     )
     ;; STEP 6: Update lattribs with the combined CLEAN text (format codes will be added by lattribs-to-dlg)
     (setq
       hcnm-ldrblk-eb-lattribs
        (subst
          (list tag new-text)
          attr
          hcnm-ldrblk-eb-lattribs
        )
     )
    )
    ;; Invalid done-code - just ignore
    (t
     (princ
       (strcat "\nWarning: Invalid button code " (itoa done-code))
     )
    )
  )
)

(defun hcnm-edit-bubble-cancel () -1)
;;; Remove delimiters from lattribs before saving
;;; Concatenates prefix+auto+postfix into plain text
(defun hcnm-ldrblk-eb-remove-delimiters (lattribs / result)
  (setq result '())
  (foreach
     attr lattribs
    (setq
      result
       (append
         result
         (list
           (list
             (car attr)                 ; TAG
             (hcnm-ldrblk-eb-flatten-value (cadr attr))
                                        ; Remove delimiters from VALUE
           )
         )
       )
    )
  )
  result
)
;;; Flatten a 3-element list (prefix auto postfix) to plain concatenated text.
;;; Concatenate parts with spaces between non-empty parts.
(defun hcnm-ldrblk-eb-flatten-value (value / prefix auto postfix result)
  (cond
    ((not value) "")
    ((atom value) value)                ; If it's a string, return as-is
    (t
     ;; It's a list: (prefix auto postfix)
     (setq
       prefix
        (nth 0 value)
       auto
        (nth 1 value)
       postfix
        (nth 2 value)
       result ""
     )
     ;; Add prefix
     (if (and prefix (/= prefix ""))
       (setq result prefix)
     )
     ;; Add auto with space if needed
     (if (and auto (/= auto ""))
       (setq
         result
          (if (= result "")
            auto
            (strcat result " " auto)
          )
       )
     )
     ;; Add postfix with space if needed
     (if (and postfix (/= postfix ""))
       (setq
         result
          (if (= result "")
            postfix
            (strcat result " " postfix)
          )
       )
     )
     result
    )
  )
)
;; Save auto-text to XDATA for each attribute tag
(defun hcnm-ldrblk-eb-save (ename-bubble /)
  ;; Save attributes (concatenated) and XDATA (auto text only)
  (hcnm-ldrblk-lattribs-to-dwg
    ename-bubble
    hcnm-ldrblk-eb-lattribs
  )
  ;; Save XDATA (FIX: was missing, causing reactor updates to fail)
  (hcnm-ldrblk-xdata-save
    ename-bubble
    hcnm-ldrblk-eb-lattribs
  )
  -1
)
(defun hcnm-edit-bubble-done-codes (/ eb-done)
  (setq eb-done t)
  '((11 "LF" eb-done)
    (12 "SF" eb-done)
    (13 "SY" eb-done)
    (14 "Sta" eb-done)
    (15 "Off" eb-done)
    (16 "StaOff" eb-done)
    (17 "AlName" eb-done)
    (18 "StaName" eb-done)
    (19 "N" eb-done)
    (20 "E" eb-done)
    (21 "NE" eb-done)
    (22 "Z" eb-done)
    (23 "Text" eb-done)
    (25 "Dia" eb-done)
    (26 "Slope" eb-done)
    (27 "L" eb-done)
   )
)

;; ACTION_TILE callback: Update text value in lattribs when user types (2-element)
;; User typing replaces the entire text value
;; Update text value when user types in dialog field
(defun hcnm-ldrblk-eb-update-text (tag new-value / attr)
  (setq attr (assoc tag hcnm-ldrblk-eb-lattribs))
  (setq
    hcnm-ldrblk-eb-lattribs
     (subst
       (list tag new-value)             ; Replace with new text value (2-element lattribs)
       attr
       hcnm-ldrblk-eb-lattribs
     )
  )
)

;;; DEPRECATED - No postfix field in 2-element architecture
;;; Postfix was only needed in 4-element (prefix auto postfix) structure
(defun hcnm-ldrblk-eb-update-postfix (tag new-postfix / attr)
  (alert
    (princ
      "\nDEPRECATED: eb-update-postfix called - no postfix in 2-element lattribs"
    )
  )
  ;; Do nothing - no postfix field exists
)

(defun hcnm-ldrblk-eb-show (dclfile notetextradiocolumn ename-bubble / tag
                        value parts prefix auto postfix on-model-tab-p
                        lst-dlg-attributes 
                       )
  (haws-debug (list "=== DEBUG: hcnm-ldrblk-eb-show ENTRY"))
  (new_dialog "HCNMEditBubble" dclfile)
  (haws-debug (list "=== DEBUG: new_dialog successful"))
  (set_tile "Title" "Edit CNM Bubble Note")
  ;; Check if bubble is in paper space
  (setq
    on-model-tab-p
     (or (not ename-bubble)
         (hcnm-ldrblk-is-on-model-tab ename-bubble)
     )
  )
  ;; Show delimiter tip and paper space warning if applicable
  ;; EXECUTIVE: The general disclaimer in the edit dialog is sufficient.
  ;; When user actually adds coordinate auto-text (Sta/Off/etc), they get
  ;; the detailed dismissable tip via hcnm-ldrblk-tip-warn-pspace-no-react.
  (set_tile
    "Message"
    (strcat
      "Tip: Use ``` (triple backtick) to mark where auto-text should insert."
      (haws_evangel_msg)
    )
  )
  (mode_tile "ChgView" 0)               ; Always enable
  (haws-debug (list "=== DEBUG: About to call lattribs-to-dlg..."))
  ;; ARCHITECTURE: Transform clean lattribs to dialog display format (with format codes)
  ;; This is the ONLY place we transform for display
  (setq
    lst-dlg-attributes
     (hcnm-ldrblk-lattribs-to-dlg
       hcnm-ldrblk-eb-lattribs
     )
  )
  (haws-debug
    (list
      "=== DEBUG: lattribs-to-dlg returned "
      (itoa (length lst-dlg-attributes))
      " items"
    )
  )
  ;; Note attribute edit boxes - use formatted display strings (2-element lattribs)
  (haws-debug (list "=== DEBUG: Setting dialog tiles..."))
  (foreach
     attribute lst-dlg-attributes
    (setq
      tag   (car attribute)
      value (cadr attribute)
    )                                   ; Just one text value in 2-element architecture
    (haws-debug (list "=== DEBUG: Setting tiles for " tag))
    ;; Set text field (single-column DCL with free-form editing)
    (set_tile tag value)
    (action_tile
      tag
      (strcat "(hcnm-ldrblk-eb-update-text \"" tag "\" $value)")
    )
  )
  ;;Radio buttons
  (set_tile "NoteTextRadioColumn" notetextradiocolumn)
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
    (hcnm-edit-bubble-done-codes)
  )
  ;; Clear Auto Text button
  (action_tile "ClearAuto" "(DONE_DIALOG 28)")
  ;; Change View button (paper space only)
  (action_tile "ChgView" "(DONE_DIALOG 29)")
  (action_tile "accept" "(DONE_DIALOG 1)")
  (action_tile "cancel" "(DONE_DIALOG 0)")
  (list (start_dialog) notetextradiocolumn)
)

;; Split string on delimiter
;; Keep this in case users decide to go to a free-form single-field editor later.
;; Returns list of substrings split on DELIM
(defun hcnm-ldrblk-eb-split-string (str delim / pos result)
  (setq result '())
  (while (setq pos (vl-string-search delim str))
    (setq
      result
       (append result (list (substr str 1 pos)))
      str
       (substr str (+ pos 2))
    )
  )
  (append result (list str))
)

;; Concatenate prefix, auto, and postfix into a 3-element list structure.
;; This replaces the old CHR(160) delimiter approach with clean list structure.
(defun hcnm-ldrblk-eb-concat-parts (prefix auto postfix /)
  (list
    (if prefix
      prefix
      ""
    )
    (if auto
      auto
      ""
    )
    (if postfix
      postfix
      ""
    )
  )
)
;#endregion
;#endregion
;#endregion
;#region CNM Options dialog
(defun c:hcnm-cnmoptions (/ cnmdcl done-code retn)
  (haws-core-init 210)
  (hcnm-projinit)
  (hcnm-proj)
  ;; Load Dialog
  (setq cnmdcl (load_dialog "cnm.dcl"))
  (setq done-code 2)
  (while (> done-code -1)
    (setq
      done-code
       (cond
         ((= done-code 0) (hcnm-dcl-options-cancel))
         ((= done-code 1) (hcnm-dcl-options-save))
         ((= done-code 2) (hcnm-dcl-options-show cnmdcl))
         ((= done-code 11) (hcnm-dcl-general-show cnmdcl))
         ((= done-code 12) (hcnm-dcl-bubble-show cnmdcl))
         ((= done-code 13) (hcnm-dcl-key-show cnmdcl))
         ((= done-code 14) (hcnm-dcl-qt-show cnmdcl))
       )
    )
  )
  (haws-core-restore)
  (princ)
)

(defun hcnm-dcl-options-cancel ()
  (hcnm-config-temp-clear)
  -1
)

;; Saves, then passes control to temp var clear function.
(defun hcnm-dcl-options-save () (hcnm-config-temp-save) 0)

(defun hcnm-dcl-options-show (cnmdcl)
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

(defun hcnm-dcl-general-show (cnmdcl)
  (new_dialog "HCNMGeneral" cnmdcl)
  ;; Dialog Actions
  (set_tile "Title" "CNM General Options")
  (hcnm-config-set-action-tile "DoCurrentTabOnly")
  (hcnm-config-dcl-list "InsertTablePhases")
  (hcnm-config-set-action-tile "PhaseAlias1")
  (hcnm-config-set-action-tile "PhaseAlias2")
  (hcnm-config-set-action-tile "PhaseAlias3")
  (hcnm-config-set-action-tile "PhaseAlias4")
  (hcnm-config-set-action-tile "PhaseAlias5")
  (hcnm-config-set-action-tile "PhaseAlias6")
  (hcnm-config-set-action-tile "PhaseAlias7")
  (hcnm-config-set-action-tile "PhaseAlias8")
  (hcnm-config-set-action-tile "PhaseAlias9")
  (hcnm-config-set-action-tile "NotesKeyTableDimstyle")
  (hcnm-config-set-action-tile "NotesLeaderDimstyle")
  (set_tile
    "ProjectFolder"
    (strcat
      "Project folder "
      (hcnm-shorten-path (hcnm-proj) 100)
    )
  )
  (hcnm-config-set-action-tile "ProjectNotes")
  (action_tile
    "ProjectNotesBrowse"
    "(hcnm-config-temp-setvar \"ProjectNotes\"(hcnm-GETPROJNOTES))(SET_TILE \"ProjectNotes\" (hcnm-config-temp-getvar \"ProjectNotes\"))"
  )
  (hcnm-config-dcl-list "LayersEditor")
  (hcnm-config-dcl-list "ProjectNotesEditor")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm-dcl-bubble-show (cnmdcl)
  (new_dialog "HCNMBubble" cnmdcl)
  (set_tile "Title" "CNM Bubble Options")
  (hcnm-config-set-action-tile "BubbleHooks")
  (hcnm-config-set-action-tile "BubbleMtext")
  (hcnm-config-set-action-tile "BubbleAreaIntegral")
  (hcnm-config-set-action-tile "NoteTypes")
  (hcnm-config-set-action-tile "BubbleTextLine1PromptP")
  (hcnm-config-set-action-tile "BubbleTextLine2PromptP")
  (hcnm-config-set-action-tile "BubbleTextLine3PromptP")
  (hcnm-config-set-action-tile "BubbleTextLine4PromptP")
  (hcnm-config-set-action-tile "BubbleTextLine5PromptP")
  (hcnm-config-set-action-tile "BubbleTextLine6PromptP")
  (hcnm-config-set-action-tile "BubbleTextLine0PromptP")
  (hcnm-config-set-action-tile "BubbleSkipEntryPrompt")
  (hcnm-config-set-action-tile "BubbleOffsetDropSign")
  (hcnm-config-set-action-tile "BubbleTextPrefixLF")
  (hcnm-config-set-action-tile "BubbleTextPrefixSF")
  (hcnm-config-set-action-tile "BubbleTextPrefixSY")
  (hcnm-config-set-action-tile "BubbleTextPrefixSta")
  (hcnm-config-set-action-tile "BubbleTextPrefixOff+")
  (hcnm-config-set-action-tile "BubbleTextPrefixOff-")
  (hcnm-config-set-action-tile "BubbleTextPrefixN")
  (hcnm-config-set-action-tile "BubbleTextPrefixE")
  (hcnm-config-set-action-tile "BubbleTextPrefixZ")
  (hcnm-config-set-action-tile "BubbleTextPostfixLF")
  (hcnm-config-set-action-tile "BubbleTextPostfixSF")
  (hcnm-config-set-action-tile "BubbleTextPostfixSY")
  (hcnm-config-set-action-tile "BubbleTextPostfixSta")
  (hcnm-config-set-action-tile "BubbleTextPostfixOff+")
  (hcnm-config-set-action-tile "BubbleTextPostfixOff-")
  (hcnm-config-set-action-tile "BubbleTextPostfixN")
  (hcnm-config-set-action-tile "BubbleTextPostfixE")
  (hcnm-config-set-action-tile "BubbleTextPostfixZ")
  (hcnm-config-set-action-tile "BubbleTextJoinDelSta")
  (hcnm-config-set-action-tile "BubbleTextJoinDelN")
  (hcnm-config-set-action-tile "BubbleTextPrecisionLF")
  (hcnm-config-set-action-tile "BubbleTextPrecisionSF")
  (hcnm-config-set-action-tile "BubbleTextPrecisionSY")
  (hcnm-config-set-action-tile "BubbleTextPrecisionOff+")
  (hcnm-config-set-action-tile "BubbleTextPrecisionN")
  (hcnm-config-set-action-tile "BubbleTextPrecisionE")
  (hcnm-config-set-action-tile "BubbleTextPrecisionZ")
  (hcnm-config-set-action-tile "BubbleTextPrefixPipeDia")
  (hcnm-config-set-action-tile "BubbleTextPostfixPipeDia")
  (hcnm-config-set-action-tile "BubbleTextPrecisionPipeDia")
  (hcnm-config-set-action-tile "BubbleTextPrefixPipeSlope")
  (hcnm-config-set-action-tile "BubbleTextPostfixPipeSlope")
  (hcnm-config-set-action-tile "BubbleTextPrecisionPipeSlope")
  (hcnm-config-set-action-tile "BubbleTextPrefixPipeLength")
  (hcnm-config-set-action-tile "BubbleTextPostfixPipeLength")
  (hcnm-config-set-action-tile
    "BubbleTextPrecisionPipeLength"
  )
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm-dcl-key-show (cnmdcl)
  (new_dialog "HCNMKey" cnmdcl)
  ;; Dialog Actions
  (set_tile "Title" "CNM Key Notes Table Options")
  (hcnm-config-set-action-tile "DescriptionWrap")
  (hcnm-config-set-action-tile "LineSpacing")
  (hcnm-config-set-action-tile "NoteSpacing")
  (hcnm-config-set-action-tile "ShowKeyTableTitleShapes")
  (hcnm-config-set-action-tile "ShowKeyTableQuantities")
  (hcnm-config-set-action-tile "ShowKeyTableGrid")
  (hcnm-config-set-action-tile "TableWidth")
  (hcnm-config-set-action-tile "PhaseWidthAdd")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm-dcl-qt-show (cnmdcl)
  (new_dialog "HCNMQT" cnmdcl)
  ;; Dialog Actions
  (set_tile "Title" "CNM Quantity Take-off Table Options")
  (hcnm-config-set-action-tile "NumberToDescriptionWidth")
  (hcnm-config-set-action-tile "DescriptionToQuantityWidth")
  (hcnm-config-set-action-tile "QuantityToQuantityWidth")
  (hcnm-config-set-action-tile "QuantityToUnitsWidth")
  (action_tile "close" "(DONE_DIALOG 2)")
  (start_dialog)
)

(defun hcnm-options-list-data ()
  '(("ProjectNotesEditor"
     (("text" "System Text Editor")
      ("csv" "System CSV (spreadsheet)")
      ("cnm" "CNM Pro Editor")
     )
    )
    ("LayersEditor"
     (("notepad" "Notepad") ("cnm" "CNM Pro Editor"))
    )
    ("InsertTablePhases"
     (("No" "No")
      ("1" "1")
      ("2" "2")
      ("3" "3")
      ("4" "4")
      ("5" "5")
      ("6" "6")
      ("7" "7")
      ("8" "8")
      ("9" "9")
      ("10" "10")
     )
    )
   )
)
(defun hcnm-config-set-action-tile (var)
  (set_tile var (hcnm-config-temp-getvar var))
  (action_tile
    var
    (strcat "(hcnm-config-temp-setvar \"" var "\" $value)")
  )
)
(defun hcnm-config-dcl-list (key /)
  (hcnm-set-tile-list
    key
    (mapcar
      '(lambda (x) (cadr x))
      (cadr (assoc key (hcnm-options-list-data)))
    )
    (cadr
      (assoc
        (hcnm-config-getvar key)
        (cadr (assoc key (hcnm-options-list-data)))
      )
    )
  )
  (action_tile
    key
    "(hcnm-config-dcl-list-callback $key $value)"
  )
)
(defun hcnm-set-tile-list (key options selected / item)
  (start_list key 3)
  (mapcar 'add_list options)
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
(defun hcnm-config-dcl-list-callback (key value /)
  (hcnm-config-temp-setvar
    key
    (car
      (nth
        (read value)
        (cadr (assoc key (hcnm-options-list-data)))
      )
    )
  )
)

;#region Debug Utilities
;;==============================================================================
;; DEBUG UTILITIES - Discovery Testing Helpers
;;==============================================================================
;; These functions help diagnose issues during development and testing.
;; They pretty-print complex data structures and validate bubble note integrity.
;;
;; USAGE DURING TESTING:
;; 1. Insert bubble with auto-text
;; 2. Call (hcnm-debug-show-bubble) to see full state
;; 3. Modify reference object (stretch, move)
;; 4. Call again to see if reactor updated correctly
;;==============================================================================

;; Main diagnostic command - shows everything about selected bubble
(defun c:hcnm-debug-bubble (/ en)
  (princ "\nSelect a bubble note: ")
  (setq en (car (entsel)))
  (if en
    (progn
      (hcnm-debug-show-lattribs en)
      (hcnm-debug-show-xdata en)
      (hcnm-debug-show-reactor-info en)
      (hcnm-debug-validate-bubble en)
    )
    (princ "\nNo entity selected.")
  )
  (princ)
)

;; Show lattribs structure (parsed from attributes)
(defun hcnm-debug-show-lattribs (ename-bubble / lattribs)
  (princ "\n\n=== LATTRIBS (from attributes) ===")
  (setq lattribs (hcnm-ldrblk-dwg-to-lattribs ename-bubble))
  (if lattribs
    (foreach
       attr-data lattribs
      (princ
        (strcat
          "\n  "
          (car attr-data)
          ": "
          "\""
          (cadr attr-data)
          "\" | "
          "\""
          (caddr attr-data)
          "\" | "
          "\""
          (cadddr attr-data)
          "\""
        )
      )
    )
    (princ "\n  ERROR: Could not parse lattribs!")
  )
  (princ "\n")
)

;; Show XDATA structure (extended entity data)
(defun hcnm-debug-show-xdata
   (ename-bubble / xdata-sections vptrans autotext avport)
  (princ "\n=== XDATA (extended entity data) ===")
  (setq xdata-sections (hcnm-xdata-read ename-bubble))
  (setq vptrans (cdr (assoc 'vptrans xdata-sections)))
  (princ "\n  VPTRANS (viewport transform): ")
  (if vptrans
    (princ (vl-prin1-to-string vptrans))
    (princ "NONE")
  )
  (setq avport (cdr (assoc 'avport xdata-sections)))
  (princ "\n  AVPORT (viewport number): ")
  (if avport
    (princ avport)
    (princ "NONE")
  )
  (setq autotext (cdr (assoc 'autotext xdata-sections)))
  (princ "\n  AUTO-TEXT: ")
  (if autotext
    (foreach
       pair autotext
      (princ (strcat "\n    " (car pair) " = \"" (cdr pair) "\""))
    )
    (princ "NONE")
  )
  (princ "\n")
)

;; Show reactor information for this bubble
(defun hcnm-debug-show-reactor-info (ename-bubble / reactors reactor data
                                 handle-bubble bubble-entry
                                )
  (princ "\n=== REACTOR INFO ===")
  (setq
    handle-bubble
     (cdr (assoc 5 (entget ename-bubble)))
    reactors
     (cdar (vlr-reactors :vlr-object-reactor))
  )
  (princ (strcat "\n  Bubble handle: " handle-bubble))
  (princ
    (strcat
      "\n  Total object reactors: "
      (itoa (length reactors))
    )
  )
  ;; Find HCNM reactor
  (setq reactor nil)
  (foreach
     r reactors
    (setq data (vlr-data r))
    (if (and (listp data) (assoc "HCNM-BUBBLE" data))
      (setq reactor r)
    )
  )
  (if reactor
    (progn
      (princ "\n  HCNM-BUBBLE reactor: FOUND")
      (setq
        bubble-entry
         (assoc
           handle-bubble
           (cdr (assoc "HCNM-BUBBLE" (vlr-data reactor)))
         )
      )
      (if bubble-entry
        (progn
          (princ "\n  This bubble in reactor data: YES")
          (princ "\n  Reference objects for this bubble:")
          (foreach
             ref (cdr bubble-entry)
            (princ
              (strcat
                "\n    "
                (car ref)
                " → tags: "
                (vl-prin1-to-string (cdr ref))
              )
            )
          )
        )
        (princ
          "\n  This bubble in reactor data: NO (reactor won't update this bubble!)"
        )
      )
    )
    (princ
      "\n  HCNM-BUBBLE reactor: NOT FOUND (reactors disabled or not attached)"
    )
  )
  (princ "\n")
)

;; Validate bubble structure and report issues
(defun hcnm-debug-validate-bubble (ename-bubble / lattribs issues)
  (princ "\n=== VALIDATION ===")
  (setq issues '())
  ;; Test 1: Can we parse lattribs?
  (setq lattribs (hcnm-ldrblk-dwg-to-lattribs ename-bubble))
  (if (not lattribs)
    (setq
      issues
       (cons
         "CRITICAL: Cannot parse lattribs from attributes"
         issues
       )
    )
  )
  ;; Test 2: Lattribs schema validation
  (if lattribs
    (progn
      (if (vl-catch-all-error-p
            (vl-catch-all-apply
              'hcnm-ldrblk-lattribs-validate
              (list lattribs)
            )
          )
        (setq issues (cons "ERROR: lattribs validation failed" issues))
      )
    )
  )
  ;; Test 3: XDATA readable?
  (if (vl-catch-all-error-p
        (vl-catch-all-apply 'hcnm-xdata-read (list ename-bubble))
      )
    (setq issues (cons "ERROR: Cannot read XDATA" issues))
  )
  ;; Test 4: Has leader?
  (if (not (hcnm-ldrblk-bubble-leader ename-bubble))
    (setq issues (cons "WARNING: No leader found" issues))
  )
  ;; Report
  (if issues
    (progn
      (princ "\n  ISSUES FOUND:")
      (foreach issue issues (princ (strcat "\n    ❌ " issue)))
    )
    (princ "\n  ✅ All validations PASSED")
  )
  (princ "\n")
)

;; Trace reactor callbacks (add to callback for debugging)
(defun hcnm-debug-trace-reactor (event-name obj-notifier / handle-notifier)
  (setq
    handle-notifier
      (if (vl-catch-all-error-p
            (vl-catch-all-apply
              'vla-get-handle
              (list obj-notifier)
            )
          )
        "ERASED"
        (vla-get-handle obj-notifier)
      )
  )
  (haws-debug
    (list
      "[REACTOR] "
      event-name
      " on object "
      handle-notifier
    )
  )
)

;; Compare expected vs actual auto-text after update
(defun hcnm-debug-compare-autotext (ename-bubble expected-alist /
                                actual-xdata actual-alist differences
                               )
  (princ "\n=== AUTO-TEXT COMPARISON ===")
  (setq
    actual-xdata
     (hcnm-xdata-get-autotext ename-bubble)
    actual-alist
     (if actual-xdata
       actual-xdata
       '()
     )
    differences
     '()
  )
  (foreach
     expected-pair expected-alist
    (setq
      tag
       (car expected-pair)
      expected-value
       (cdr expected-pair)
      actual-pair
       (assoc tag actual-alist)
      actual-value
       (if actual-pair
         (cdr actual-pair)
         nil
       )
    )
    (if (not (equal expected-value actual-value))
      (setq
        differences
         (cons
           (list tag expected-value actual-value)
           differences
         )
      )
    )
  )
  (if differences
    (progn
      (princ "\n  MISMATCHES FOUND:")
      (foreach
         diff differences
        (princ (strcat "\n    " (car diff) ":"))
        (princ (strcat "\n      Expected: \"" (cadr diff) "\""))
        (princ (strcat "\n      Actual:   \"" (caddr diff) "\""))
      )
    )
    (princ "\n  ✅ All auto-text values match expected")
  )
  (princ "\n")
  differences
)

;#endregion

;;------------------------------------------------------------------------------
;; LEGACY DEBUG FUNCTION (kept for backward compatibility)
;;------------------------------------------------------------------------------
;; This predates the Debug Utilities region and uses a different display format
;; (alert dialogs instead of command line output). Kept because users may have
;; it in their scripts or muscle memory.

;; Shows reactor, its data, and XDATA of a selected bubble note
(defun hcnm-dsbr ()
  (hcnm-debug-show-bubble-reactor-xdata)
  (princ)
)
(defun hcnm-debug-show-bubble-reactor-xdata (/ en reactors reactor data
                                         handle-bubble reactor-count
                                         hcnm-reactor
                                        )
  (vl-load-com)
  (princ "\nSelect a bubble note: ")
  (setq en (car (entsel)))
  (if en
    (progn
      (setq
        handle-bubble
         (cdr (assoc 5 (entget en)))
        reactors
         (cdar (vlr-reactors :vlr-object-reactor))
        reactor-count 0
        hcnm-reactor nil
      )
      ;; Find THE ONE HCNM reactor (there should be only one)
      (foreach
         reactor reactors
        (setq data (vlr-data reactor))
        (if (and (listp data) (assoc "HCNM-BUBBLE" data))
          (progn
            (setq
              reactor-count
               (1+ reactor-count)
              hcnm-reactor reactor
            )
          )
        )
      )
      (alert
        (princ
          (cond
            ((= reactor-count 0)
             "ERROR: No HCNM-BUBBLE reactor found!"
            )
            ((> reactor-count 1)
             (strcat
               "ERROR: Reactor proliferation! Found "
               (itoa reactor-count)
               " reactors.\nThere should be only ONE reactor for all bubbles."
             )
            )
            (t
             (strcat
               "ONE HCNM-BUBBLE Reactor (correct)\n\n"
               "Reactor object:\n"
               (vl-prin1-to-string hcnm-reactor)
               "\n\n"
               "Reactor data (nested list for all bubbles):\n"
               (vl-prin1-to-string (vlr-data hcnm-reactor))
               "\n\n"
               "Reactor owners count: "
               (itoa (length (vlr-owners hcnm-reactor)))
               "\n\n"
               "Selected bubble handle: "
               handle-bubble
               "\n\n"
               "Selected bubble XDATA:\n"
               (vl-prin1-to-string
                 (assoc -3 (entget en '("HCNM-BUBBLE")))
               )
             )
            )
          )
        )
      )
    )
    (alert (princ "No entity selected."))
  )
  (princ)
)
;#endregion
;#region CNM Initialization
;;; Register CNM with HAWS-CONFIG system (Issue #11)
;;; This allows CNM config to work independently when CNM is loaded
(if haws-config-register-app
  (haws-config-register-app "CNM" (hcnm-config-definitions))
)

;;; Clean up reactor proliferation on CNM load (Issue #X)
;;; Persistent reactors can accumulate across drawing saves/loads
;;; This ensures we start with a clean slate
(hcnm-check-reactor-proliferation)

(load "ini-edit")
;#endregion

 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 1 1 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
