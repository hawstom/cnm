;;; CONSTRUCTION NOTES MANAGER
;;;
;;; PHASING
;;; NOTES allows up to 9 phases named 1 through 9.
;;; To use phases, first build the table block NOTEQTY using TBLQTY1 through TBLQTY9 TGH
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
(VL-LOAD-COM)

(DEFUN
   HAWS-GETPHASELISTFROMTBLQTY (/ EL EN I DSCTAG NOTEQTYONDISK OLDTAGS
                                PHASEALIAS PHASELIST
                               )
  ;;Check for phasing in qty table block.  Phasing is controlled by presence of TBLQTY? attributes.
  ;;Construct phaselist as '((phase1 1 alias1)(phase2 2 alias2)(phasei i aliasi)).
  ;;Alias=phase for changing later by CNM.INI.
  ;;Phases are numbered in order they appear in block. (This number could be very unstable, but it is the key to phase order on this sheet.)
  ;;
  ;;Insert table line NOTEQTY block if not exist
  (SETQ
    J (IF (= (HCNM-GETVAR "InsertTablePhases") "No")
        ""
        (HCNM-GETVAR "InsertTablePhases")
      )
  )
  (COND
    ((NOT (TBLSEARCH "BLOCK" "NOTEQTY"))
     (COMMAND "._insert" (STRCAT "NOTEQTY=NOTEQTY" J))
     (COMMAND)
    )
  )
  ;;Check how many phases are in current block.
  (SETQ
    EN (CDR (ASSOC -2 (TBLSEARCH "BLOCK" "NOTEQTY")))
    I  0
  )
  (WHILE EN
    (SETQ EL (ENTGET EN))
    (IF (AND
          (= "ATTDEF" (CDR (ASSOC 0 EL)))
          (WCMATCH (CDR (ASSOC 2 EL)) "TBLQTY?")
        )
      (SETQ I (1+ I))
    )
    (SETQ EN (ENTNEXT EN))
  )
  ;;Redefine it if wrong number of phases
  (COND
    ((AND
       (/= J "")                        ;Inserting phases requested
       (SETQ J (ATOI J))
       (/= J I)                         ;Wrong number of phases currently inserted
     )
     (COMMAND "._insert" (STRCAT "noteqty=noteqty" (ITOA J)))
     (COMMAND)
    )
  )
  (SETQ
    EN (CDR (ASSOC -2 (TBLSEARCH "BLOCK" "NOTEQTY")))
    I  1
  )
  (WHILE EN
    (SETQ EL (ENTGET EN))
    (COND
      ((AND
         (= "ATTDEF" (CDR (ASSOC 0 EL)))
         (WCMATCH (CDR (ASSOC 2 EL)) "TBLQTY?")
       )
       (SETQ
         PHASELIST
          (CONS
            (LIST
              (SUBSTR (CDR (ASSOC 2 EL)) 7 1)
              I
              (SUBSTR (CDR (ASSOC 2 EL)) 7 1)
            )
            PHASELIST
          )
         I (1+ I)
       )
      )
      ((AND
         (= "ATTDEF" (CDR (ASSOC 0 EL)))
         (= "NOTETYPE" (CDR (ASSOC 2 EL)))
       )
       (SETQ OLDTAGS T)
      )
      ((AND
         (= "ATTDEF" (CDR (ASSOC 0 EL)))
         (= "TBLDSC" (CDR (ASSOC 2 EL)))
       )
       (SETQ DSCTAG T)
      )
    )
    (SETQ EN (ENTNEXT EN))
  )
  (SETQ PHASELIST (REVERSE PHASELIST))
  (IF (NOT PHASELIST)
    (SETQ PHASELIST '(("" 1 "")))
  )
  ;;Add phasealias settings to the phaselist.
  (MAPCAR
    '(LAMBDA (PHASE)
       (SETQ
         PHASELIST
          (SUBST
            (REVERSE
              (CONS
                (HCNM-GETVAR (CADR PHASE))
                (CDR (ASSOC (ITOA (CAR PHASE)) PHASELIST))
              )
            )
            (ASSOC (ITOA (CAR PHASE)) PHASELIST)
            PHASELIST
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
  (COND
    ((OR OLDTAGS (NOT DSCTAG))
     (COMMAND
       "._insert"
       (STRCAT
         "noteqty="
         (SETQ
           NOTEQTYONDISK
            (FINDFILE
              (STRCAT
                "noteqty"
                (IF (= (CAAR PHASELIST) "")
                  "0"
                  (ITOA (LENGTH PHASELIST))
                )
                ".dwg"
              )
            )
         )
       )
     )
     (COMMAND)
     (ALERT
       (PRINC
         (STRCAT
           "The NOTEQTY block in this drawing had the wrong attributes for this version of Construction Notes Manager."
           "\nor it was missing the description text attribute, TBLDSC."
           "\n\nConstruction Notes Manager tried to fix the problem by inserting\n"
           NOTEQTYONDISK " from disk."
           "\n\nIf results are still not satisfactory, please edit the drawing\n"
           NOTEQTYONDISK
           "\nto meet your needs and include the following attributes (in order):"
           "\n\nTBLTYPE\nTBLNUM\nTBLDSC\nTBLQTY\nTBLUNT"
          )
       )
     )
    )
  )
  PHASELIST
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
(DEFUN
   HAWS-SEARCHANDSAVE (DN PROJNOTES / ALIASLIST AT AV BLKI BLKSS COUNT
                       CTABONLY EL EN ET I J MVPORT MVSSET N NFNAME
                       NOTEFND NOTEI NOTELINES NOTELIST NOTENUM
                       NOTEPHASE NOTEQTY NOTETXT NOTETYPE NOTNUM NOTTYP
                       PHASE PHASELIST QTYOPT SKIPPEDPHASES USRVAR
                       VPLAYERS X
                      )
;;;
;;; Section 1.  Make an empty NOTELIST from tblqty and constnot.txt.  TGH I can use this section for Tally, except there is a conflict in the way they do PHASELIST.
;;;
  (SETQ
    PHASELIST
     (HAWS-GETPHASELISTFROMTBLQTY)
    CTABONLY
     (= "1" (HCNM-GETVAR "DoCurrentTabOnly"))
    NOTTYP ""
  )
  (FOREACH
     ENTRY *HCNM-CNMPROJECTNOTES*
    (COND
      ;;If it's a note
      ((= 3 (CAR ENTRY))
       ;;make a new type entry if necessary
       (COND
         ((/= NOTTYP (SETQ NOTTYP (CADR ENTRY)))
          (SETQ NOTELIST (CONS (LIST NOTTYP) NOTELIST))
         )
       )
       ;;and add the note to the list.
       (SETQ
         NOTNUM
          (CADDR ENTRY)
         QTYOPT
          (NTH 4 ENTRY)
         I 0
         NOTELINES
          (LENGTH (NTH 5 ENTRY))
       )
       (SETQ
         NOTELIST
          (SUBST
            (REVERSE
              (CONS
                (APPEND
                  (LIST NOTNUM NOTELINES QTYOPT)
                  (MAPCAR '(LAMBDA (PHASE) NIL) PHASELIST)
                                        ;Add a nil for each phase
                )
                (REVERSE (ASSOC NOTTYP NOTELIST))
              )
            )
            (ASSOC NOTTYP NOTELIST)
            NOTELIST
          )
       )
      )
    )
  )
  (SETQ NOTELIST (APPEND (LIST PHASELIST) (LIST NOTELIST)))
;;;
;;; Section 2.  Get quantities from bubble notes and save to file
;;;
  ;;Make a list of all layers frozen in current viewport.
  (SETQ
    COUNT 0
    MVSSET
     (SSGET "X" (LIST (CONS 0 "VIEWPORT")))
  )
  (IF MVSSET
    (WHILE (SETQ MVPORT (SSNAME MVSSET COUNT))
      (SETQ MVPORT (ENTGET MVPORT '("ACAD")))
      (COND
        ((AND
           (= (GETVAR "CTAB") (CDR (ASSOC 410 MVPORT)))
                                        ;Viewport is on current tab
           (= (GETVAR "Cvport") (CDR (ASSOC 69 MVPORT)))
                                        ;Viewport has current viewport number
           (ASSOC 1003 (CDADR (ASSOC -3 MVPORT)))
                                        ;Viewport has vp frozen layers
         )
         (FOREACH
            DXFGROUP (CDADR (ASSOC -3 MVPORT))
           (COND
             ((= 1003 (CAR DXFGROUP))
              (SETQ VPLAYERS (CONS (CDR DXFGROUP) VPLAYERS))
             )
           )
         )
        )
      )
      (SETQ COUNT (1+ COUNT))
    )
  )
  ;;Get bubbles selection set
  (SETQ
    BLKSS
     (SSGET "X" (LIST (CONS 0 "INSERT")))
    I -1
  )
  ;;Remove frozen and off blocks, frozen in current viewport,
  ;;xrefs, and xref dependent blocks from the set
  ;;Remove all blocks not in current space if CTABONLY = 1.
  (WHILE (AND BLKSS (SETQ BLKI (SSNAME BLKSS (SETQ I (1+ I)))))
    (IF
      (OR
        (= 1
           (LOGAND
             1
             (CDR
               (ASSOC
                 70
                 (TBLSEARCH "LAYER" (CDR (ASSOC 8 (ENTGET BLKI))))
               )
             )
           )
        )
        (MINUSP
          (CDR
            (ASSOC 62 (TBLSEARCH "LAYER" (CDR (ASSOC 8 (ENTGET BLKI)))))
          )
        )
        (= 4
           (LOGAND
             4
             (CDR
               (ASSOC
                 70
                 (TBLSEARCH "BLOCK" (CDR (ASSOC 2 (ENTGET BLKI))))
               )
             )
           )
        )
        (= 16
           (LOGAND
             16
             (CDR
               (ASSOC
                 70
                 (TBLSEARCH "BLOCK" (CDR (ASSOC 2 (ENTGET BLKI))))
               )
             )
           )
        )
        ;;On a layer frozen in the current viewport
        (MEMBER (CDR (ASSOC 8 (ENTGET BLKI))) VPLAYERS)
        ;;Not in space of current tab if only doing ctab
        (AND
          CTABONLY
          (/= (CDR (ASSOC 410 (ENTGET BLKI))) (GETVAR "CTAB"))
        )
      )
       (SETQ
         BLKSS
          (SSDEL BLKI BLKSS)
         I (1- I)
       )
    )
  )
  ;;Search through bubble notes and add their quantities to NOTELIST
  (SETQ
    I -1
    ALIASLIST
     (MAPCAR '(LAMBDA (PHASE) (REVERSE PHASE)) (CAR NOTELIST))
  )
  (WHILE (AND BLKSS (SETQ BLKI (SSNAME BLKSS (SETQ I (1+ I)))))
    (SETQ
      EN BLKI
      NOTETYPE NIL
      NOTENUM NIL
      NOTETXT
       '(0 1 2 3 4 5 6 7 8 9)
      NOTEPHASE ""
    )
    ;;Substitute the value of each NOTETXT attribute for its respective member of the pre-filled NOTETXT list.
    (WHILE
      (AND
        (SETQ EN (ENTNEXT EN))
        (= "ATTRIB" (SETQ ET (CDR (ASSOC 0 (SETQ EL (ENTGET EN))))))
      )
       (SETQ
         AT (CDR (ASSOC 2 EL))
         AV (CDR (ASSOC 1 EL))
       )
       (COND
         ((= AT "NOTETYPE") (SETQ NOTETYPE AV))
         ((= AT "NOTENUM") (SETQ NOTENUM AV))
         ((WCMATCH AT "NOTETXT#")
          (SETQ NOTETXT (SUBST AV (ATOI (SUBSTR AT 8 1)) NOTETXT))
         )
         ((= AT "NOTEPHASE") (SETQ NOTEPHASE AV))
       )
    )
    (SETQ NOTEI (ASSOC NOTENUM (CDR (ASSOC NOTETYPE (CADR NOTELIST)))))
    (COND
      ;;If there is such a note and phase, or no phasing is being used.
      ((AND
         NOTEI
         (SETQ
           N (IF (= (CAAR ALIASLIST) "") ;If no phasing
               1                        ;the quantity will be 4th atom in list '(notei txtlines qtyopt qty1) (N=3)
               (CADR (ASSOC NOTEPHASE ALIASLIST))
             )
         )
       )
       ;;get the quantity as instructed
       (SETQ
         N (+ N 2)
         QTYOPT
          (CADDR NOTEI)
         NOTEQTY
          (COND
            ((NTH N NOTEI))             ;If there's already a quantity growing, use it.
            (0.0)                       ;Otherwise add a real zero.
          )
       )
       (COND
         ((= QTYOPT "COUNT") (SETQ NOTEQTY (1+ NOTEQTY)))
         ((WCMATCH QTYOPT "LINE#")
          (SETQ
            NOTEQTY
             (+ NOTEQTY
                (ATOF
                  (CADR
                    (HAWS-EXTRACT
                      (COND
                        ((NTH (ATOI (SUBSTR QTYOPT 5 1)) NOTETXT)
                        )
                        ("0")
                      )
                    )
                  )
                )
             )
          )
         )
         ((= QTYOPT "") (SETQ NOTEQTY ""))
       )
       ;;Add quantity to notelist
       (SETQ J -1)
       (SETQ
         NOTELIST
          (LIST
            (CAR NOTELIST)
            (SUBST
              (SUBST
                (MAPCAR                 ;Substitute NOTEQTY for the Nth of NOTEI
                  '(LAMBDA (X)
                     (IF (= (SETQ J (1+ J)) N)
                       NOTEQTY
                       X
                     )
                   )
                  NOTEI
                )
                NOTEI
                (ASSOC NOTETYPE (CADR NOTELIST))
              )
              (ASSOC NOTETYPE (CADR NOTELIST))
              (CADR NOTELIST)
            )
          )
       )
      )
      ;;If there isn't such a phase, note it in SKIPPEDPHASES
      (NOTEI
       (IF (OR (NOT NOTEPHASE) (= NOTEPHASE ""))
         (SETQ NOTEPHASE "<none>")
       )
       (IF (NOT SKIPPEDPHASES)
         (SETQ SKIPPEDPHASES '(0 ""))
       )
       (SETQ
         SKIPPEDPHASES
          (LIST
            (1+ (CAR SKIPPEDPHASES))
            (IF (NOT
                  (WCMATCH NOTEPHASE (CADR SKIPPEDPHASES))
                )
              (COND
                ((= "" (CADR SKIPPEDPHASES)) NOTEPHASE)
                (T
                 (STRCAT
                   (CADR SKIPPEDPHASES)
                   ","
                   NOTEPHASE
                 )
                )
              )
              (CADR SKIPPEDPHASES)
            )
          )
       )
      )
    )
  )
  ;;After searching bubbles for presence and quantities,
  ;;get quantities from qty table if no counting instructions
  (SETQ
    BLKSS
     (SSGET "X" (LIST (CONS 2 "NOTEQTY")))
    I -1
  )
  (WHILE (AND BLKSS (SETQ BLKI (SSNAME BLKSS (SETQ I (1+ I)))))
    (COND
      (;;If only doing ctab and table block is not in current tab, do nothing
       (AND
         CTABONLY
         (/= (CDR (ASSOC 410 (ENTGET BLKI))) (GETVAR "CTAB"))
       )
      )
      (T
       (SETQ EN BLKI)
       ;;Get the table quantities from each tblqty attribute,
       ;;and put them into the NOTEQTY variable
       ;;as '(("phase" "qty")...)
       (SETQ NOTEQTY NIL)
       (WHILE
         (AND
           (SETQ EN (ENTNEXT EN))
           (= "ATTRIB" (SETQ ET (CDR (ASSOC 0 (SETQ EL (ENTGET EN))))))
         )
          (SETQ
            AT (CDR (ASSOC 2 EL))
            AV (CDR (ASSOC 1 EL))
          )
          (COND
            ((= AT "TBLTYPE") (SETQ NOTETYPE AV))
            ((= AT "TBLNUM") (SETQ NOTENUM AV))
            ((WCMATCH AT "TBLQTY*")
             (SETQ NOTEQTY (CONS (LIST (SUBSTR AT 7 1) AV) NOTEQTY))
            )
          )
       )
       (SETQ
         NOTEI
          (ASSOC NOTENUM (CDR (ASSOC NOTETYPE (CADR NOTELIST))))
       )
       ;;If there aren't any counting instructions given for note, check if it was found
       ;;then put its quantities for all phases in NOTELIST.
       (COND
         ((AND
            ;;No counting instructions for note
            (= "" (CADDR NOTEI))
            ;;and note found at least once in allowed phases
            (PROGN
              (FOREACH
                 PHASE ALIASLIST
                (IF (NTH (+ 2 (CADR PHASE)) NOTEI)
                  (SETQ NOTEFND T)
                )
              )
              NOTEFND
            )
          )
          ;;Reset the found flag
          (SETQ NOTEFND NIL)
          ;;Insert the quantities from NOTEQTY into NOTELIST
          (SETQ
            NOTELIST
             (LIST
               (CAR NOTELIST)           ;Phaselist
               (SUBST
                 (SUBST
                   (CONS
                     (CAR NOTEI)
                     (CONS
                       (CADR NOTEI)
                       (CONS
                         (CADDR NOTEI)
                         ;;Get the table quantities stored in NOTEQTY
                         (MAPCAR
                           '(LAMBDA (X)
                              (COND
                                ((CADR
                                   (ASSOC (CADDR X) NOTEQTY)
                                 )
                                )
                                (0.0)
                              )
                            )
                           (CAR NOTELIST)
                         )
                       )
                     )
                   )
                   (ASSOC
                     NOTENUM
                     (CDR (ASSOC NOTETYPE (CADR NOTELIST)))
                   )
                   (ASSOC NOTETYPE (CADR NOTELIST))
                 )
                 (ASSOC NOTETYPE (CADR NOTELIST))
                 (CADR NOTELIST)
               )
             )
          )
         )
       )
      )
    )
  )
  ;;Now that NOTELIST is filled, alert user if any phases in bubbles were skipped.
  (IF SKIPPEDPHASES
    (ALERT
      (PRINC
        (STRCAT
          "\nThese unexpected phase names: \""
          (CADR SKIPPEDPHASES)
          "\" were found and skipped.\n"
          (ITOA (CAR SKIPPEDPHASES))
          " blocks in total were not counted.\n\nClick OK to continue."
        )
      )
    )
  )
  ;;Save notelist to file
  (SETQ
    NFNAME
     (STRCAT
       DN
       (IF CTABONLY
         (STRCAT "-" (GETVAR "CTAB"))
         ""
       )
       ".not"
     )
    F2 (OPEN NFNAME "w")
  )
  ;;Write NOTELIST to the work file
  (PRINC "(" F2)
  (PRIN1 (CAR NOTELIST) F2)
  (PRINC "(" F2)
  (FOREACH
     NOTTYP (CADR NOTELIST)
    (PRINC "(" F2)
    (PRIN1 (CAR NOTTYP) F2)
    (FOREACH
       NOTNUM (CDR NOTTYP)
      (PRINC "(" F2)
      (PRIN1 (CAR NOTNUM) F2)
      (PRIN1 (CADR NOTNUM) F2)
      (PRIN1 (CADDR NOTNUM) F2)
      (FOREACH
         NOTEQTY (CDDDR NOTNUM)
        (COND
          ((= (TYPE NOTEQTY) 'STR) (PRIN1 NOTEQTY F2))
          (NOTEQTY (PRIN1 (RTOS NOTEQTY 2 8) F2))
          ((PRINC "nil " F2))
        )
        F2
      )
      (PRINC ")" F2)                    ;End of notnum
    )
    (PRINC ")" F2)                      ;End of nottyp
  )
  (PRINC "))" F2)                       ;End of (cadr noteqty) and noteqty
  (SETQ
    F2 (CLOSE F2)
       ;;Close notes file for this drawing (program work file)
  )
  (PRINC
    (STRCAT
      "\nUsed Project Notes at " PROJNOTES
      "\nSaved notes and quantities in " NFNAME "."
     )
  )
)
;;MAKENOTETABLE reads NOTELIST from file nfname and makes a table of notes and quantities.
;;Uses the qty block.
;;Puts table at qtypt.
;; TGH to use this for TALLY, maybe I just need to read NOTELIST as an argument instead of from a file in this function.
(DEFUN
   HAWS-MAKENOTETABLE (NFSOURCE QTYPT QTYSET DN TXTHT / CTABONLY ICOL
                       IPHASE IROW FIRSTLINE JROW NFNAME NOTDSC NOTELIST
                       NOTESMAXROWS NOTETITLES NOTNUM NOTQTY NOTTYP
                       NOTUNT NUMFND PHASELIST PROMPTEACHCOL QTY QTYPT1
                       RDLIN TXTHTTEMP TYPFND USRVAR
                      )
  (SETQ PHASELIST (HAWS-GETPHASELISTFROMTBLQTY))
  (SETVAR "osmode" 0)
  (SETVAR "attreq" 1)
  (INITGET "Prompt")
  (SETQ
    NOTESMAXROWS
     (HAWS-GETINTX
       "Maximum rows of notes"          ; or [Prompt for each column]"
       NOTESMAXROWS
       99
     )
  )
  (COND
    ((= NOTESMAXROWS "Prompt")
     (SETQ
       PROMPTEACHCOL T
       NOTESMAXROWS 99
     )
     (ALERT
       "The option to prompt for each column is not yet operational."
     )
    )
  )
  (SETQ CTABONLY (= (HCNM-GETVAR "DoCurrentTabOnly") "1"))
  (IF (= NFSOURCE "E")
    (SETQ
      NFNAME
       (COND
         (CTABONLY
          (FINDFILE (STRCAT DN "-" (GETVAR "CTAB") ".not"))
         )
         (T (FINDFILE (STRCAT DN ".not")))
       )
    )
    (SETQ NFNAME (GETFILED "Select Drawing and Layout" "" "NOT" 0))
  )
  (SETQ
    F1 (OPEN NFNAME "r")
    NOTELIST
     (READ (READ-LINE F1))
    F1 (CLOSE F1)
  )
  ;;Check that we got a valid NOTELIST from file.
  (IF (/= (TYPE NOTELIST) 'LIST)
    (ALERT
      (STRCAT
        "\nThe file"
        NFNAME
        "appears to be out of date.\nIt doesn't have valid information to make a notes table.\n\nPlease search and save notes again."
      )
    )
  )
  ;;All prompts done.  Let's make table!
  (HCNM-PROJINIT)                       ;Initialize project after user pauses
  (HCNM-READCF (HCNM-PROJNOTES))
  (SETQ
    LINSPC
     (ATOF (HCNM-GETVAR "LineSpacing"))
    NOTSPC
     (ATOF (HCNM-GETVAR "NoteSpacing"))
    TBLWID
     (ATOF (HCNM-GETVAR "TableWidth"))
    PHASEWID
     (ATOF (HCNM-GETVAR "PhaseWidthAdd"))
    IROW 0
    ICOL 1
    IPHASE 1
    QTYPT1 QTYPT
  )
  ;;Check that the right NOTEQTY block is inserted.
  (IF (OR (/= (LENGTH PHASELIST) (LENGTH (CAR NOTELIST)))
                                        ;Wrong number of current phases
          (AND                          ;or counted 1 phase, but current block has no phasing
            (= 1 (LENGTH (CAR NOTELIST)))
            (= 1 (ATOF (CAAAR NOTELIST)))
            (/= 1 (ATOF (CAAR PHASELIST)))
          )
          (AND                          ;or counted no phasing, but current block has it
            (= 1 (LENGTH (CAR NOTELIST)))
            (/= 1 (ATOF (CAAAR NOTELIST)))
            (= 1 (ATOF (CAAR PHASELIST)))
          )
      )
    (PROGN
      (COMMAND
        "._insert"
        (STRCAT
          "noteqty=noteqty"
          (CAR (NTH NOTELIST (1- (LENGTH NOTELIST))))
        )
      )
      (COMMAND)
    )
  )
  (COMMAND "._undo" "group")
  (IF QTYSET
    (COMMAND "._erase" QTYSET "")
  )
  (FOREACH
     ENTRY *HCNM-CNMPROJECTNOTES*
    (COND
      ;;If it's a variable setting, set it.
      ((= 1 (CAR ENTRY))
       (SETQ USRVAR (CADR ENTRY))
       (COND
         ((AND (= "TXTHT" USRVAR) (SETQ USRVAR (CADDR ENTRY)))
          (SETQ
            TXTHT
             (* (GETVAR "DIMSCALE")
                (COND
                  ((DISTOF USRVAR))
                  ((GETVAR "dimtxt"))
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
      ((= 2 (CAR ENTRY))
       (SETQ
         NOTETITLES
          (CONS
            (LIST TXTHT (CADDR ENTRY))
            ;; If clear titles flag (a note came between this title and the last),
            ;; start titles fresh.
            (IF (= 0 (CAR NOTETITLES))
              NIL
              NOTETITLES
            )
          )
         NOTTYP
          (CADR ENTRY)
       )
      )
      ;;If it's a note number,
      ;;flag any NOTETITLES as complete with a 0.
      ;;If it is found in NOTELIST, write it with quantities
      ;;and any pending titles to the table.
      ((AND
         (= 3 (CAR ENTRY))
         (IF (AND NOTETITLES (/= 0 (CAR NOTETITLES)))
           (SETQ NOTETITLES (CONS 0 NOTETITLES))
           T
         )
         (SETQ
           NOTNUM
            (ASSOC
              (CADDR ENTRY)
              (CDR
                (ASSOC (SETQ NOTTYP (CADR ENTRY)) (CADR NOTELIST))
              )
            )
         )
         (PROGN
           (SETQ NUMFND NIL)
           (FOREACH
              PHASE (CDDDR NOTNUM)
             (IF PHASE
               (SETQ NUMFND T)
             )
           )
           NUMFND
         )
       )
       (SETQ
         NOTUNT
          (CADDDR ENTRY)
         JROW
          (CADR NOTNUM)
         NOTQTY
          ;;Convert quantities to strings, preserving input precision for all quantities
          ;;Trim extra zeros from quantities
          (MAPCAR
            '(LAMBDA (QTY)
               (WHILE (WCMATCH QTY "*.*0,*.")
                 (SETQ QTY (SUBSTR QTY 1 (1- (STRLEN QTY))))
               )
               QTY
             )
            ;;Turn quantities into strings
            (MAPCAR
              '(LAMBDA (QTY)
                 (COND
                   ((= (TYPE QTY) 'STR) QTY)
                   ((= (TYPE QTY) 'REAL) (RTOS QTY 2 8))
                   (T "")
                 )
               )
              (CDDDR NOTNUM)
            )
          )
         NOTETITLES
          (CDR NOTETITLES)              ;If note was found, unflag and write titles.
       )
       (IF NOTETITLES
         (SETQ JROW (+ JROW (LENGTH NOTETITLES)))
       )
       (IF (> (+ JROW IROW) NOTESMAXROWS)
                                        ;If these titles and note won't fit in col.
         (IF (/= IROW 0)                ;If column won't be left empty,
           (SETQ                        ;go to next column
             ICOL
              (1+ ICOL)
             IROW 0
             QTYPT
              (POLAR
                QTYPT1
                0
                (* (1- ICOL)
                   (+ (* TXTHT TBLWID)
                      (* TXTHT PHASEWID (1- (LENGTH PHASELIST)))
                   )
                )
              )
           )
         )
       )
       (COND
         (NOTETITLES
          (SETQ TXTHTTEMP TXTHT)
          (FOREACH
             NOTETITLE (REVERSE NOTETITLES)
            (SETQ TXTHT (CAR NOTETITLE))
            (COMMAND
              "._insert"
              "NOTEQTY"
              QTYPT
              TXTHT
              ""
              "0"
              ""
              ""
              (CADR NOTETITLE)
            )
            (FOREACH X NOTQTY (COMMAND ""))
            (COMMAND "")
            (SETQ
              QTYPT
               (POLAR QTYPT (* PI -0.5) (* TXTHT LINSPC))
              IROW
               (1+ IROW)
              JROW
               (1- JROW)
            )
          )
          (SETQ
            QTYPT
             (POLAR QTYPT (* PI -0.5) (* TXTHT (- NOTSPC LINSPC)))
            TXTHT TXTHTTEMP
          )
         )
       )
       ;;Split note from titles only if titles were needed to avoid empty column.
       (IF (AND
             (> (+ JROW IROW) NOTESMAXROWS)
                                        ;If this note won't fit in col
             (= IROW (LENGTH NOTETITLES))
                                        ;and these titles were placed alone in the col
           )
         (IF (/= IROW 0)                ;If column won't be left empty,
           (SETQ                        ;go to next column
             ICOL
              (1+ ICOL)
             IROW 0
             QTYPT
              (POLAR
                QTYPT1
                0
                (* (1- ICOL)
                   (+ (* TXTHT TBLWID)
                      (* TXTHT PHASEWID (LENGTH PHASELIST))
                   )
                )
              )
           )
         )
       )
       (COMMAND
         "._insert"
         (STRCAT "cnm" NOTTYP)
         QTYPT
         TXTHT
         ""
         "0"
       )
       (SETQ FIRSTLINE T)
;;;    (setvar "attreq" 0)
       (FOREACH
          NOTDSC (NTH 5 ENTRY)
;;;All this stuff is to make the attribute order insensitive.
;;;       (COMMAND
;;;  "._insert" "NOTEQTY" "non"   
;;;  QTYPT
;;;  TXTHT
;;;  ""
;;;  "0"
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
         (COMMAND
           "._insert"
           "NOTEQTY"
           QTYPT
           TXTHT
           ""
           "0"
           (IF FIRSTLINE
             NOTTYP
             ""
           )
           (IF FIRSTLINE
             (CAR NOTNUM)
             ""
           )
           NOTDSC
         )
         (FOREACH
            X NOTQTY
           (COMMAND
             (IF FIRSTLINE
               X
               ""
             )
           )
         )
         (COMMAND
           (IF FIRSTLINE
             NOTUNT
             ""
           )
         )
         (SETQ
           QTYPT
            (POLAR QTYPT (* PI -0.5) (* TXTHT LINSPC))
           IROW
            (1+ IROW)
           NOTETITLES NIL
           FIRSTLINE NIL
         )
       )
       (SETQ
         QTYPT
          (POLAR QTYPT (* PI -0.5) (* TXTHT (- NOTSPC LINSPC)))
       )
      )
    )
  )
  ;;Apply ini table display settings.  If no settings (legacy), show both.
  (MAPCAR
    '(LAMBDA (LAYERKEY / LAYERSHOW LAYERLIST)
       (SETQ
         LAYERLIST
          (TBLSEARCH
            "LAYER"
            (CAR (HAWS-GETLAYR (CAR LAYERKEY)))
          )
         LAYERSHOW
          (/= "0" (HCNM-GETVAR (CADR LAYERKEY)))
       )
       ;;If layer exists and is showing in drawing, freeze it if setting says.
       (IF (AND
             (CDR (ASSOC 70 LAYERLIST))
             (/= 1 (LOGAND 1 (CDR (ASSOC 70 LAYERLIST))))
             (< 0 (CDR (ASSOC 62 LAYERLIST)))
           )
         (IF (NOT LAYERSHOW)
           (COMMAND-S "._layer" "_f" (CDR (ASSOC 2 LAYERLIST)) "")
         )
         ;;Otherwise it's not there or not showing.  Thaw and on it if registry says.
         (IF LAYERSHOW
           (COMMAND-S
             "._layer"
             "_t"
             (CDR (ASSOC 2 LAYERLIST))
             "_on"
             (CDR (ASSOC 2 LAYERLIST))
             ""
           )
         )
       )
     )
    '(("NOTESKEYGRID" "ShowKeyTableGrid")
      ("NOTESKEYQTYS" "ShowKeyTableQuantities")
     )
  )
  (COMMAND "._undo" "end")
)
;;CNM-SEARCH
;;In the NOTES strategy, this routine is first of three main routines.
;;Gets project info from CONSTNOT.TXT
;;Gets drawing info from bubbles or table.
;;Saves all in .NOT file for other two routines
(DEFUN
   CNM-SEARCH (DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID / EL EN I
               NOTELIST QTYPT QTYSET TABLESPACE
              )
  (SETQ
    QTYSET
     (SSGET "X" (LIST (CONS 8 (CAR (HAWS-MKLAYR "NOTESEXP")))))
  )
  (COND
    (QTYSET
     (SETQ
       I (IF (HAWS-ICAD-P)
           1
           (SSLENGTH QTYSET)
         )
     )
     (WHILE (SETQ EN (SSNAME QTYSET (SETQ I (1- I))))
       (SETQ EL (ENTGET EN))
       (COND
         ((OR (= (GETVAR "CTAB") (SETQ TABLESPACE (CDR (ASSOC 410 EL))))
              (AND (= "Model" TABLESPACE) (< 1 (GETVAR "cvport")))
              (HAWS-ICAD-P)             ;If we are in intellicad, which doesn't have the tab information in the entget data
          )
          (IF (NOT QTYPT)
            (SETQ
              QTYPT
               (TRANS
                 (CDR
                   (COND
                     ((ASSOC 11 EL))
                     ((ASSOC 10 EL))
                   )
                 )
                 0
                 1
               )
            )
          )
         )
         (T (SSDEL (CDR (ASSOC -1 EL)) QTYSET))
       )
     )
    )
  )
  (IF (NOT QTYPT)
    (SETQ QTYPT (GETPOINT "\nStart point for key notes table: "))
  )
  (HAWS-SEARCHANDSAVE DN PROJNOTES)
  ;;Make a new notes table
  (HAWS-MAKENOTETABLE "E" QTYPT QTYSET DN TXTHT)
)
;;CNM-IMPORT
;;In the NOTES strategy, this routine is second of three main routines.
;;Reads from .NOT file, created by CNM-SEARCH, everything necessary and creates a table. 
(DEFUN
   CNM-IMPORT (DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID / EL EN I QTYPT
               QTYSET TABLESPACE
              )
  (SETQ
    QTYSET
     (SSGET "X" (LIST (CONS 8 (CAR (HAWS-MKLAYR "NOTESIMP")))))
  )
  (COND
    (QTYSET
     (SETQ
       I (IF (HAWS-ICAD-P)
           1
           (SSLENGTH QTYSET)
         )
     )
     (WHILE (SETQ EN (SSNAME QTYSET (SETQ I (1- I))))
       (SETQ EL (ENTGET EN))
       (COND
         ((OR (= (GETVAR "CTAB") (SETQ TABLESPACE (CDR (ASSOC 410 EL))))
              (AND (= "Model" TABLESPACE) (< 1 (GETVAR "cvport")))
              (HAWS-ICAD-P)             ;If we are in intellicad, which doesn't have the tab information in the entget data
          )
          (IF (NOT QTYPT)
            (SETQ
              QTYPT
               (TRANS
                 (CDR
                   (COND
                     ((ASSOC 11 EL))
                     ((ASSOC 10 EL))
                   )
                 )
                 0
                 1
               )
            )
          )
         )
         (T (SSDEL (CDR (ASSOC -1 EL)) QTYSET))
       )
     )
    )
  )
  (IF (NOT QTYPT)
    (SETQ
      QTYPT
       (GETPOINT "\nStart point for imported key notes table: ")
    )
  )
  ;;Make a new notes table after erasing qtyset
  (HAWS-MAKENOTETABLE "I" QTYPT QTYSET DN TXTHT)
)
;;CNM-TALLY
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
(DEFUN
   CNM-TALLY (DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID / ALLNOT COL1X
              COLUMN DQWID DWGFIL EL F1 F2 FLSPEC I LSTFIL NDWID NFNAME
              NOTDSC NOTELIST NOTFIL NOTNUM NOTQTY NOTTYP NOTUNT NUMFND
              NUMLIST PGPMOD PHASE PHASELIST PHASENUMI PT1Z Q QQWID
              QTYLIST QTYPT1 QTYSET QUWID RDLIN RDLN1 ROW1Y SHTLST
              TABLESPACE TEMP TOTAL TNWID TYPFND USRVAR USERS1OLD
              USERS2OLD USERS3OLD USERS4OLD WRITELIST X Y Z
             )
;;;
;;;  Section 1.
;;;  Determine list of drawings to tally.
;;;
  (COND
    ;; The VBA years.
    ((and (< (ATOF (GETVAR "acadver")) 18)(>= (ATOF (GETVAR "acadver")) 15))
     (AND (HAWS-VLISP-P) (NOT (HAWS-ICAD-P)))
     (SETQ
       USERS1OLD
        (GETVAR "users1")
       USERS2OLD
        (GETVAR "users2")
       USERS3OLD
        (GETVAR "users3")
       USERS4OLD
        (GETVAR "users4")
     )
     (SETVAR
       "users1"
       (COND
         (*HCNM-QTLISTFILE*)
         ((STRCAT DN ".lst"))
       )
     )
     (SETVAR "users2" "CNM Sheet Quantities Files (*.not)|*.not")
     (SETVAR
       "users3"
       "File(s) listed below will be included in the quantity take-off shown in this drawing:"
     )
     (SETVAR
       "users4"
       "When you make a key notes table from bubble notes, Construction Notes Manager saves the quantities in a CNM Sheet Quantities file.  You list here the files for which you want to show the quantity take-off in this drawing."
     )
     (COMMAND "-vbarun" "fselect.dvb!modFileSelect.FileSelect")
     (SETQ
       LSTFIL
        (GETVAR "users1")
       *HCNM-QTLISTFILE* LSTFIL
     )
     (IF (/= (GETVAR "users2") "OK")
       (SETQ TEMP T)
     )
     (SETVAR "users1" USERS1OLD)
     (SETVAR "users2" USERS2OLD)
     (SETVAR "users3" USERS3OLD)
     (SETVAR "users4" USERS4OLD)
     (IF TEMP
       (EXIT)
     )
    )
    ;;Non-VBA selection methods
    ((AND
       (OR (SETQ LSTFIL (FINDFILE (STRCAT DN ".lst")))
           (SETQ
             LSTFIL
              (FINDFILE (STRCAT (GETVAR "DWGPREFIX") "tally.lst"))
           )
       )
       (= "Yes"
          (PROGN
            (INITGET 1 "Yes No")
            (GETKWORD
              (STRCAT
                "\nKeep and use existing list file, \""
                LSTFIL
                "\"? <Yes/No>: "
              )
            )
          )
       )
     )
    )
    (T
     (PROMPT
       (STRCAT
         "\n\nHow will you specify drawings to tally?"
         "\nUse a text file you have prepared with a List of drawings, "
         "\n(CNM will automatically use tally.lst if present), "
         "\nenter Wildcards (eg. * or grad\\unit1*), "
         "\nor Select drawings one at a time from a dialogue box?"
       )
     )
     (INITGET 1 "List Wildcards Select")
     (SETQ
       TEMP
        (GETKWORD "\n<List file/Wildcards/Select one at a time>: ")
     )
     (COND
       ((= TEMP "List")
        (SETQ LSTFIL (GETFILED "Select a List File" "" "LST" 0))
       )
       ((= TEMP "Wildcards")
        ;;Add function to user's ACAD.PGP to shell and wait for attrib command to finish.
        (SETQ
          LSTFIL
           (STRCAT DN ".lst")
          TEMP
           (FINDFILE "acad.pgp")
          F1 (OPEN TEMP "r")
        )
        (WHILE (SETQ DWGFIL (READ-LINE F1))
          (IF (= "RUN," (SUBSTR DWGFIL 1 4))
            (SETQ PGPMOD T)
          )
          (IF (= "SH," (SUBSTR DWGFIL 1 3))
            (SETQ
              SHTLST
               (CONS
                 ";The following line was added by HawsEDC Construction Notes Manager for file processing by Wildcard."
                 SHTLST
               )
              SHTLST
               (CONS
                 "RUN,       cmd /c,         0,*Batch file to run: ,"
                 SHTLST
               )
            )
          )
          (SETQ SHTLST (CONS DWGFIL SHTLST))
        )
        (SETQ F1 (CLOSE F1))
        (IF (NOT PGPMOD)
          (PROGN
            (SETQ
              F1     (OPEN TEMP "w")
              SHTLST (REVERSE SHTLST)
            )
            (FOREACH DWGFIL SHTLST (WRITE-LINE DWGFIL F1))
            (SETQ F1 (CLOSE F1))
            (SETVAR "re-init" 16)
          )
        )
        (WHILE (NOT COLUMN)
          (SETQ
            FLSPEC
             (GETSTRING
               T
               "\nFiles to tally using OS wildcards (eg. * or grad\\*): "
             )
          )
          (COMMAND
            "run"
            (STRCAT "attrib \"" FLSPEC ".not\" > " LSTFIL)
          )
          (SETQ
            F1 (OPEN LSTFIL "r")
            DWGFIL
             (READ-LINE F1)
            COLUMN
             (STRLEN DWGFIL)
          )
          (COND
            ((WCMATCH DWGFIL "* not found *")
             (SETQ
               COLUMN NIL
               F1 (CLOSE F1)
             )
             (ALERT
               (PRINC
                 (STRCAT
                   "The operating system could not find\nany files found matching the wildcard:\n\n "
                   FLSPEC
                   ".not\n\nPlease try again."
                 )
               )
             )
            )
            (T
             (WHILE (NOT
                      (AND
                        (WCMATCH
                          (STRCASE (SUBSTR DWGFIL COLUMN))
                          (STRCASE (STRCAT FLSPEC "`.NOT"))
                        )
                        (OR (= "\\" (SUBSTR DWGFIL (1- COLUMN) 1))
                            (= "\\" (SUBSTR DWGFIL COLUMN 1))
                            (= ":" (SUBSTR DWGFIL (1+ COLUMN) 1))
                        )
                      )
                    )
               (SETQ COLUMN (1- COLUMN))
             )
             (SETQ
               F1     (CLOSE F1)
               F1     (OPEN LSTFIL "r")
               SHTLST NIL
             )
             (WHILE (SETQ DWGFIL (READ-LINE F1))
               (SETQ
                 DWGFIL
                  (SUBSTR DWGFIL COLUMN)
                 SHTLST
                  (CONS
                    (SUBSTR DWGFIL 1 (- (STRLEN DWGFIL) 4))
                    SHTLST
                  )
               )
             )
             (SETQ
               F1 (CLOSE F1)
               F1 (OPEN LSTFIL "w")
             )
             (SETQ SHTLST (REVERSE SHTLST))
             (FOREACH DWGFIL SHTLST (WRITE-LINE DWGFIL F1))
             (SETQ F1 (CLOSE F1))
            )
          )
        )
       )
       ((= TEMP "Select")
        (SETQ
          LSTFIL
           (STRCAT DN ".lst")
          F1 (OPEN LSTFIL "w")
        )
        (WHILE (SETQ
                 DWGFIL
                  (GETFILED
                    "File to tally (Cancel when Finished)"
                    ""
                    "NOT"
                    6
                  )
               )
          (WRITE-LINE (SUBSTR DWGFIL 1 (- (STRLEN DWGFIL) 4)) F1)
        )
        (SETQ F1 (CLOSE F1))
       )
     ) ;_ end cond
    )
  )
  ;;Build an empty phase list of (phase number alias).  
  ;;The reason we do this instead of just adding the
  ;;new phases as they come is to avoid sorting the list when we're done.
  ;;In other words, this list is nothing but a definition of the presentation order for phases.
  (SETQ
    PHASELIST
     '(("" 0 NIL)
       ("1" 1 NIL)
       ("2" 2 NIL)
       ("3" 3 NIL)
       ("4" 4 NIL)
       ("5" 5 NIL)
       ("6" 6 NIL)
       ("7" 7 NIL)
       ("8" 8 NIL)
       ("9" 9 NIL)
       ("A" 10 NIL)
       ("B" 11 NIL)
       ("C" 12 NIL)
       ("D" 13 NIL)
       ("E" 14 NIL)
       ("F" 15 NIL)
       ("G" 16 NIL)
       ("H" 17 NIL)
       ("I" 18 NIL)
       ("J" 19 NIL)
       ("K" 20 NIL)
       ("L" 21 NIL)
       ("M" 22 NIL)
       ("N" 23 NIL)
       ("O" 24 NIL)
       ("P" 25 NIL)
       ("Q" 26 NIL)
       ("R" 27 NIL)
       ("S" 28 NIL)
       ("T" 29 NIL)
       ("U" 30 NIL)
       ("V" 31 NIL)
       ("W" 32 NIL)
       ("X" 33 NIL)
       ("Y" 34 NIL)
       ("Z" 35 NIL)
      )
  )
;;;
;;;  Section 2.
;;;  Read all .NOT's into a master QTYLIST
;;;  Add phases from all .NOTs to the list if not already there.  And if aliases in conflict, alert user.
;;;  
  (SETQ F1 (OPEN LSTFIL "r"))
  (PRINC "\n")
  (WHILE (AND (SETQ NOTFIL (READ-LINE F1)) (/= "" NOTFIL))
    ;;Read in this sheet's notelist '( ((alias number phase)) ((type1 (notenum txtlines countmethod qty1...))))
    ;;Alert user of possible incompatibility with old-style list.
    (SETQ
      NFNAME
       (COND
         ((FINDFILE NOTFIL))
         ((FINDFILE (STRCAT NOTFIL ".not")))
         (T
          (ALERT
            (PRINC
              (STRCAT
                "The file \"" NOTFIL "\" listed in \"" LSTFIL
                "\" cannot be found.\nConstruction Notes Manager cannot continue."
               )
            )
          )
         )
       )
      F2 (OPEN NFNAME "r")
      NOTELIST
       (READ (READ-LINE F2))
      QTYLIST
       (CONS (CONS NOTFIL NOTELIST) QTYLIST)
    )
    (IF (READ-LINE F2)
      (ALERT
        (PRINC
          (STRCAT
            "Error:  Sheet quantities file for "
            NOTFIL
            " is out of date.\nPlease search and save quantities again."
          )
        )
      )
    )
    (SETQ F2 (CLOSE F2))
    ;;Set all phases discovered.
    ;;In .NOT files, phases are ("alias" order "number"), but here they are ("number" order "alias")
    (FOREACH
       PHASE (CAR NOTELIST)
      (COND
        ;;If its alias is not yet in PHASELIST, add the phase.
        ;;The reason we substitute instead of just adding the
        ;;new phases as they come is to avoid sorting the list when we're done.
        ((NOT (CADDR (ASSOC (CADDR PHASE) PHASELIST)))
         (SETQ
           PHASELIST
            (SUBST
              ;;Set the alias.
              (SUBST
                (CAR PHASE)
                NIL
                (ASSOC (CADDR PHASE) PHASELIST)
              )
              (ASSOC (CADDR PHASE) PHASELIST)
              PHASELIST
            )
         )
        )
        ;;If alias in phaselist isn't same as alias in this sheet, alert user.
        ((/= (CADDR (ASSOC (CAR PHASE) PHASELIST)) (CADDR PHASE))
         (ALERT
           (PRINC
             (STRCAT
               NOTFIL
               " is trying to assign alias \""
               (CADDR PHASE)
               "\" to phase \""
               (CAR PHASE)
               "\", which already has alias \""
               (CADDR (ASSOC (CAR PHASE) PHASELIST))
               "\".\n\nGrouping alias \""
               (CADDR PHASE)
               "\" on this sheet with phase \""
               (CAR PHASE)
               "\", alias \""
               (CADDR (ASSOC (CAR PHASE) PHASELIST))
               "."
             )
           )
         )
        )
      )
    )
  )
  (SETQ F1 (CLOSE F1))
  ;;Condense list to standard phaselist format: '((phasej j aliasj)...)
  ;;and renumber
  (SETQ I 0)
  (FOREACH
     PHASE PHASELIST
    (IF (CADDR PHASE)
      (SETQ X (CONS (LIST (CAR PHASE) (SETQ I (1+ I)) (CADDR PHASE)) X))
    )
  )
  (SETQ PHASELIST (REVERSE X))
;;;
;;;  Section 3.
;;;  Write requested totals to drawing and sheet-by-sheet quantities to dwg.csv.
;;;
  (INITGET "All Used")
  (SETQ
    ALLNOT
     (= (GETKWORD
          "\nList which notes from CONSTNOT.TXT? All/Used: "
        )
        "All"
     )
    QTYPT1
     (COND
       ((AND
          (SETQ
            QTYSET
             (SSGET
               "X"
               (LIST
                 (CONS 8 (CAR (HAWS-MKLAYR "NOTESTAL")))
               )
             )
          )
          (SETQ
            EL (ENTGET
                 (SSNAME
                   QTYSET
                   (IF (HAWS-ICAD-P)
                     0
                     (1- (SSLENGTH QTYSET))
                   )
                 )
               )
          )
          (OR (= (GETVAR "CTAB")
                 (SETQ TABLESPACE (CDR (ASSOC 410 EL)))
              )
              (AND (< 1 (GETVAR "cvport")) (= TABLESPACE "Model"))
              (HAWS-ICAD-P)
          )
        )
        (TRANS
          (CDR
            (COND
              ((ASSOC 11 EL))
              ((ASSOC 10 EL))
            )
          )
          0
          1
        )
       )
       (T
        (GETPOINT "\nStart point for quantity take-off table: ")
       )
     )
  )
  (HCNM-PROJINIT)                       ;Initialize project after user pauses
  (HCNM-READCF (HCNM-PROJNOTES))
  (SETQ
    LINSPC
     (ATOF (HCNM-GETVAR "LineSpacing"))
    NOTSPC
     (ATOF (HCNM-GETVAR "NoteSpacing"))
    TBLWID
     (ATOF (HCNM-GETVAR "TableWidth"))
    PHASEWID
     (ATOF (HCNM-GETVAR "PhaseWidthAdd"))
    COL1X
     (CAR QTYPT1)
    ROW1Y
     (CADR QTYPT1)
    PT1Z
     (CADDR QTYPT1)
    X COL1X
    Y ROW1Y
    Z PT1Z
    ;;width from middle of number to left point of description text
    NDWID
     (ATOF (HCNM-GETVAR "NumberToDescriptionWidth"))
    ;;width from left point of description text to right point of quantity
    DQWID
     (ATOF (HCNM-GETVAR "DescriptionToQuantityWidth"))
    ;;width from right point of one quantity phase to right point of next quantity phase
    QQWID
     (ATOF (HCNM-GETVAR "QuantityToQuantityWidth"))
    ;;width from right point of quantity to left point of unit
    QUWID
     (ATOF (HCNM-GETVAR "QuantityToUnitsWidth"))
  )
  (SETVAR "osmode" 0)
  ;;Write column headings to the file
  (WHILE (NOT (SETQ F2 (OPEN (STRCAT DN ".csv") "w")))
    (ALERT
      (PRINC
        (STRCAT
          "Couldn't write to "
          DN
          ".csv.\nPlease close file, then click ok."
        )
      )
    )
  )
  (SETQ TEMP "")
  (PRINC "TYPE,NO,ITEM,UNIT," F2)
  (SETQ TEMP "")
  (FOREACH
     SHTLST QTYLIST
    (FOREACH
       PHASE PHASELIST
      (SETQ
        TEMP
         (STRCAT
           TEMP
           (HAWS-MKFLD
             (STRCAT
               (STRCASE (CAR SHTLST))
               (IF (= (CAR PHASE) "")
                 " (SINGLE PHASE)"
                 " PHASE "
               )
               (CADDR PHASE)
             )
             ","
           )
         )
      )
    )
  )
  (PRINC TEMP F2)
  (FOREACH
     PHASE PHASELIST
    (PRINC
      (STRCAT
        "TOTAL"
        (IF (= (CAR PHASE) "")
          " (SINGLE PHASE)"
          " PHASE "
        )
        (CADDR PHASE)
        ","
      )
      F2
    )
  )
  (WRITE-LINE "" F2)
  (IF QTYSET
    (COMMAND "._erase" QTYSET "")
  )
  ;;For each line in project file
  (FOREACH
     ENTRY *HCNM-CNMPROJECTNOTES*
    (COND
      ;;If it's a variable setting, set it.
      ((= 1 (CAR ENTRY))
       (SETQ USRVAR (CADR ENTRY))
       (COND
         ((AND (= "TXTHT" USRVAR) (SETQ USRVAR (CADDR ENTRY)))
          (SETQ
            TXTHT
             (* (GETVAR "DIMSCALE")
                (COND
                  ((DISTOF USRVAR))
                  ((GETVAR "dimtxt"))
                )
             )
          )
         )
       )
      )
      ;;If its a title, save it for future use.
      ;;If a number intervened since last titles, clear them first.
      ((= 2 (CAR ENTRY))
       (SETQ
         NOTETITLES
          (CONS
            (LIST TXTHT (CADDR ENTRY))
            ;; If clear titles flag (a note came between this title and the last)
            ;; or nottyp has changed, clear titles.
            (IF (= 0 (CAR NOTETITLES))
              NIL
              NOTETITLES
            )
          )
         NOTTYP
          (CADR ENTRY)
       )
      )
      ;;If it's a note number,
      ;;flag the NOTETITLES as complete with a 0.
      ;;If it is found in the qty lst,
      ;;get and add the quantities from qty list
      ;;and add the note with quantities to the table.
      ((AND
         (= 3 (CAR ENTRY))
         (IF (AND NOTETITLES (/= 0 (CAR NOTETITLES)))
           (SETQ NOTETITLES (CONS 0 NOTETITLES))
           T
         )
         (SETQ
           NOTTYP
            (CADR ENTRY)
           NOTNUM
            (CADDR ENTRY)
         )
         (OR ALLNOT
             (SETQ
               NUMFND NIL
               NUMFND
                (FOREACH
                   SHTLST QTYLIST
                  (FOREACH
                     PHASEI (CDDDR
                              (ASSOC
                                NOTNUM
                                (CDR (ASSOC NOTTYP (CADDR SHTLST)))
                              )
                            )
                    (IF PHASEI
                      (SETQ NUMFND NOTNUM)
                    )
                  )
                  NUMFND
                )
             )
         )
       )
       ;;If note was found, unflag and write titles.
       (COND
         (NOTETITLES
          (SETQ TXTHTTEMP TXTHT)
          (FOREACH
             NOTETITLE (REVERSE (CDR NOTETITLES))
            (SETQ TXTHT (CAR NOTETITLE))
            (SETQ X COL1X)
            (IF (/= (CADR NOTETITLE) "")
              (HAWS-MKTEXT "ML" (LIST X Y Z) TXTHT 0 (CADR NOTETITLE))
            )
            (SETQ Y (- Y (* TXTHT LINSPC)))
            (WRITE-LINE (CADR NOTETITLE) F2)
          )
          (SETQ
            Y         (- Y (* TXTHT (- NOTSPC LINSPC)))
            TXTHT TXTHTTEMP
          )
         )
       )
       ;;Print most note info to both drawing and file.
       ;;Print unit to file before quantities (because lots of columns), but wait in drawing 'til after quantities.
       ;;
       ;;Insert shape block
       (SETQ X COL1X)
       (SETQ Y (- Y (/ (* TXTHT LINSPC) 2)))
       (COMMAND
         "._insert"
         (STRCAT "cnm" NOTTYP)
         (LIST X Y Z)
         TXTHT
         ""
         "0"
       )
       ;;Make number text
       (HAWS-MKTEXT "M" (LIST X Y Z) TXTHT 0 NOTNUM)
       (SETQ
         NOTETITLES NIL
         NOTUNT
          (CADDDR ENTRY)
       )
       ;;Print the quantity for each phase from each sheet to file, and increment the total.
       (SETQ
         X (+ X (* TXTHT (- (+ NDWID DQWID) QQWID)))
         WRITELIST
          (LIST NOTTYP NOTNUM (NTH 5 ENTRY) NOTUNT)
         ;;Initialize running totals for each phase
         NOTQTY
          (MAPCAR '(LAMBDA (X) 0) PHASELIST)
       )
       (FOREACH
          SHTLST QTYLIST
         (SETQ
           NOTQTY
            (MAPCAR
              '(LAMBDA (X)
                 (SETQ
                   TOTAL
                    (NTH (1- (CADR X)) NOTQTY)
                   ;;Get the current total from notqty
                   Q
                    (COND
                      ((AND
                         ;;If the current sheet has the current phase
                         (SETQ
                           PHASENUMI
                            (CADR
                              (ASSOC
                                (CAR X)
                                (CADR SHTLST)
                              )
                            )
                         )
                         ;;and if the current sheet has the current note
                         (SETQ
                           NUMLIST
                            (ASSOC
                              NOTNUM
                              (CDR
                                (ASSOC
                                  NOTTYP
                                  (CADDR SHTLST)
                                )
                              )
                            )
                         )
                         ;;and if the quantity isn't nil,
                         (SETQ Q (NTH (+ 2 PHASENUMI) NUMLIST))
                       )
                       ;; use its numeric conversion
                       (ATOF Q)
                      )
                      (0)
                    )
                   TOTAL
                    (+ TOTAL Q)
                 )
                 (SETQ
                   WRITELIST
                    (REVERSE
                      (CONS
                        (HAWS-PRIN1-TO-STRING Q)
                        (REVERSE WRITELIST)
                      )
                    )
                 )
                 TOTAL
               )
              PHASELIST
            )
         )
       )
       ;;convert quantities to strings, preserving input precision.
       (SETQ
         NOTQTY
          (MAPCAR
            '(LAMBDA (PHASE / TEMP)
               (SETQ
                 TEMP
                  (RTOS (NTH (1- (CADR PHASE)) NOTQTY) 2 8)
               )
               (WHILE (WCMATCH TEMP "*.*0,*.")
                 (SETQ TEMP (SUBSTR TEMP 1 (1- (STRLEN TEMP))))
               )
               TEMP
             )
            PHASELIST
          )
       )
       ;;Print totals to drawing and file.
       (MAPCAR
         '(LAMBDA (PHASE)
            (SETQ X (+ X (* TXTHT QQWID)))
            (HAWS-MKTEXT
              "MR"
              (LIST X Y Z)
              TXTHT
              0
              (NTH (1- (CADR PHASE)) NOTQTY)
            )
            (SETQ
              WRITELIST
               (REVERSE
                 (CONS
                   (HAWS-PRIN1-TO-STRING
                     (NTH (1- (CADR PHASE)) NOTQTY)
                   )
                   (REVERSE WRITELIST)
                 )
               )
            )
          )
         PHASELIST
       )
       ;;Write unit to drawing
       (SETQ X (+ X (* TXTHT QUWID)))
       (IF (/= NOTUNT "")
         (HAWS-MKTEXT "ML" (LIST X Y Z) TXTHT 0 NOTUNT)
       )
       (SETQ
         X         (+ COL1X (* TXTHT NDWID))
         FIRSTLINE T
       )
       (FOREACH
          NOTDSC (NTH 5 ENTRY)
         (IF (/= NOTDSC "")
           (HAWS-MKTEXT "ML" (LIST X Y Z) TXTHT 0 NOTDSC)
         )
         (SETQ Y (- Y (* TXTHT LINSPC)))
       )
       (SETQ Y (- Y (* TXTHT (- NOTSPC LINSPC))))
       ;;Write note to file.
       (FOREACH
          X WRITELIST
         (IF (= (TYPE X) 'STR)
           (PRINC (STRCAT X ",") F2)
           (PROGN
             (SETQ TEMP "")
             (FOREACH Y X (SETQ TEMP (STRCAT TEMP "\n" Y)))
             (PRINC (HAWS-MKFLD (SUBSTR TEMP 2) ",") F2)
           )
         )
       )
       (SETQ WRITELIST NIL)
       (WRITE-LINE "" F2)
      )
    )
  )
  (SETQ F2 (CLOSE F2))
  (PROMPT
    (STRCAT "\nUsed project notes file found at " PROJNOTES)
  )
)
;;CNM main commands
(DEFUN C:HCNM-CNM () (HCNM-CNM NIL))
(DEFUN C:HCNM-CNMKT () (HCNM-CNM "Search"))
(DEFUN C:HCNM-CNMKTI () (HCNM-CNM "Import"))
(DEFUN C:HCNM-CNMQT () (HCNM-CNM "Tally"))
;;CNM main function
(DEFUN
   HCNM-CNM (OPT / CFNAME DN LINSPC PHASEWID TBLWID TXTHT)
  ;;Main function
  (HAWS-ERDF$@ 1)
  (HAWS-VSAVE
    '("attdia" "attreq" "cmdecho" "clayer" "osmode")
  )
  (SETVAR "attdia" 0)
  (SETVAR "cmdecho" 0)
  (COND
    ((NOT OPT)
     (PROMPT
       "\nConstruction Notes Manager searches, saves, and lists notes and quantities from attributed bubble notes."
     )
     (PROMPT
       "\nConstruction Notes Manager can also import the notes and quantities list into this or another tab or drawing."
     )
     (PROMPT
       "\nConstruction Notes Manager tallies quantities from several drawings previously searched and saved."
     )
     (PROMPT "\nSee www.ConstructionNotesManager.com")
     (INITGET "Search Import Tally")
     (SETQ
       OPT
        (GETKWORD
          "\nSearch notes and make table/Import table/Tally drawings: "
        )
     )
    )
  )
  (HCNM-PROJINIT)                       ;Initialize after pauses
  ;;Set any desired dimstyle.
  (HCNM-SET-DIMSTYLE "NotesKeyTableDimstyle")
  (SETQ
    DN (HAWS-GETDNPATH)
    PROJNOTES
     (HCNM-PROJNOTES)
    TXTHT
     (* (GETVAR "dimtxt") (GETVAR "dimscale"))
    ;;Column and line spacing widths (half width for middle justified columns)
    ;;line spacing
    LINSPC
     (ATOF (HCNM-GETVAR "LineSpacing"))
    ;;width of single sheet table with only one phase
    TBLWID
     (ATOF (HCNM-GETVAR "TableWidth"))
    ;;width for each extra phase on single sheet table.
    PHASEWID
     (ATOF (HCNM-GETVAR "PhaseWidthAdd"))
  )
  (HCNM-READCF PROJNOTES)
  (COND
    ((= OPT "Search")
     (CNM-SEARCH DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID)
    )
    ((= OPT "Import")
     (CNM-IMPORT DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID)
    )
    ((= OPT "Tally")
     (CNM-TALLY DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID)
    )
  )
  ;;Restore old dimstyle
  (HCNM-RESTORE-DIMSTYLE)
  (HAWS-VRSTOR)
  (HAWS-ERRRST)
  (PRINC)
)
;;;
;;;End of CNM
;;;

;;;================================================================================================================
;;;
;;; Begin Project Management functions
;;;
;;;================================================================================================================

;;
;;HCNM-PROJINIT initializes the CNM project variables
;;because there is good reason to believe they need to
;;be checked again (a pause for user input or a new user command)
;;All the functions assume if they are present they are valid.
;;
(DEFUN
   HCNM-PROJINIT ()
  (SETQ
    *HCNM-CNMSETTINGS* NIL
    *HCNM-CNMPROJECTROOT* NIL
    *HCNM-CNMPROJECTNOTES* NIL
  )
)


;;Does nothing but strcat, since the existence of the file
;;is validated by (HCNM-PROJ)
(DEFUN HCNM-INI (PROJ) (STRCAT PROJ "\\cnm.ini"))

;; HCNM-PROJ gets a valid project root folder for this drawing's folder.
;; While it returns the folder only, that folder is qualified to have CNM.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\projroot" path to other functions.
(DEFUN
   HCNM-PROJ (/ I DWGDIR PROJROOT PROJTXT SIMPLECNM)
  (SETQ
    DWGDIR
     (HAWS-FILENAME-DIRECTORY (GETVAR "dwgprefix"))
    *HCNM-CNMPROJECTROOT*
     (COND
       ;;If project is already defined this session, use it.
       ;;(Assume it's valid.  Calling function should init project if there's been a chance of change or loss by user.)
       (*HCNM-CNMPROJECTROOT*)
       ;;Else Well-formed simple (single-folder) projects. CNM.INI is here.
       ((SETQ
          SIMPLECNM
           (FINDFILE
             (STRCAT DWGDIR "\\cnm.ini")
           )
        )
        (HAWS-FILENAME-DIRECTORY SIMPLECNM)
       )
       ;;Well-formed complex (multi-folder) projects.  CNMPROJ.TXT is here and
       ;;we'll make sure it really points to a CNM.INI.
       ((AND
          (FINDFILE (SETQ PROJTXT (STRCAT DWGDIR "\\cnmproj.txt")))
          (SETQ F1 (OPEN PROJTXT "r"))
          (PROGN
            (WHILE (SETQ RDLIN (READ-LINE F1))
              ;;Bricscad option
              (COND
                ((HAWS-VLISP-P)
                 (IF (VL-FILE-DIRECTORY-P RDLIN)
                   (SETQ PROJROOT RDLIN)
                 )
                )
                (T
                 (IF (/= ";" (SUBSTR RDLIN 1 1))
                   (SETQ PROJROOT RDLIN)
                 )
                )
              )
            )
            (SETQ F1 (CLOSE F1))
            PROJROOT
          )
        )
         (IF (NOT (FINDFILE (HCNM-INI PROJROOT)))
           (HCNM-GETINIDEFAULTS PROJROOT)
         )
         (PRINC
           (STRCAT
             "Using project settings in CNM.INI located at "
             PROJROOT
             " as directed by CNMPROJ.TXT in this drawing's folder."
           )
         )
         PROJROOT
       )
       ;;Make a project in this drawing's folder.
       (T
        (ALERT
          (PRINC
            (STRCAT
              "This drawing's folder is new to CNM."
            )
          )
        )
        (HCNM-GETINIDEFAULTS DWGDIR)
        DWGDIR
       )
     )
  )
)

;;as posted the autodesk discussion customization group by Tony Tanzillo
(DEFUN
   ALE_BROWSEFORFOLDER
   (PRMSTR IOPTNS DEFFLD / SHLOBJ FOLDER FLDOBJ OUTVAL)
  (SETQ
    SHLOBJ
     (VLA-GETINTERFACEOBJECT
       (VLAX-GET-ACAD-OBJECT)
       "Shell.Application"
     )
    FOLDER
     (VLAX-INVOKE-METHOD
       SHLOBJ 'BROWSEFORFOLDER 0 PRMSTR IOPTNS DEFFLD
      )
  )
  (VLAX-RELEASE-OBJECT SHLOBJ)
  (IF FOLDER
    (PROGN
      (SETQ
        FLDOBJ
         (VLAX-GET-PROPERTY FOLDER 'SELF)
        OUTVAL
         (VLAX-GET-PROPERTY FLDOBJ 'PATH)
      )
      (VLAX-RELEASE-OBJECT FOLDER)
      (VLAX-RELEASE-OBJECT FLDOBJ)
      OUTVAL
    )
  )
)

;;Prompts user for a Project Root folder and links to it by creating
;;or modifying this drawing's folder's cnmproj.txt
;;returns project root
(DEFUN C:HCNM-LINKPROJ () (HCNM-LINKPROJ) (PRINC))
(DEFUN
   HCNM-LINKPROJ (/ DWGDIR)             ;  (ARXLOAD "winapi.arx")
  (IF (NOT *HCNM-CNMPROJECTROOT*)
    (HCNM-PROJ)
  )
  (SETQ DWGDIR (HAWS-FILENAME-DIRECTORY (GETVAR "dwgprefix")))
  (SETQ
    PROJ
     (COND
       ((HAWS-VLISP-P)
        (ALE_BROWSEFORFOLDER
          (COND
            ((< (STRLEN *HCNM-CNMPROJECTROOT*) 50)
             *HCNM-CNMPROJECTROOT*
            )
            ((STRCAT
               "Cancel to keep current Project Folder:\n"
               (SUBSTR *HCNM-CNMPROJECTROOT* 1 3)
               "..."
               (HAWS-ENDSTR *HCNM-CNMPROJECTROOT* 47 47)
             )
            )
          )
          48
          ""
        )
       )
       (T
        (HAWS-FILENAME-DIRECTORY
          (GETFILED "Select any file in Project Folder" "" "" 0)
        )
       )
     )
  )
  (COND
    (PROJ
     (SETQ *HCNM-CNMPROJECTROOT* PROJ)
     (COND
       ((= PROJ DWGDIR) DWGDIR)
       (PROJ
        (HCNM-MAKEPROJTXT PROJ DWGDIR)
        (IF (NOT (FINDFILE (HCNM-INI PROJ)))
          (HCNM-GETINIDEFAULTS PROJ)
        )
        PROJ
       )
     )
    )
    (*HCNM-CNMPROJECTROOT*
     (ALERT
       (STRCAT
         "Project Folder\n"
         *HCNM-CNMPROJECTROOT*
         "\nnot changed."
       )
     )
    )
  )
  (PRINC)
)

;;Makes a project root reference file CNMPROJ.TXT in this drawing's folder
;;Returns nil.
(DEFUN
   HCNM-MAKEPROJTXT (PROJDIR DWGDIR)
  (SETQ F2 (OPEN (STRCAT DWGDIR "\\cnmproj.txt") "w"))
  (PRINC
    (STRCAT
      ";For simple projects, all project drawings are in one folder, 
;and Construction Notes Manager keeps settings (CNM.INI) 
;in that folder with the drawings.
;
;For complex projects (ones that that have drawings in
;multiple folders all using the same Project Notes file and settings), 
;CNMPROJ.TXT (this file) points from each folder to 
;the Project Root Folder, given below:
"     PROJDIR
    )
    F2
  )
  (SETQ F2 (CLOSE F2))
)


;;;================================================================================================================
;;;
;;; Begin Settings functions
;;;
;;;================================================================================================================
(DEFUN
   HCNM-GETSETTINGS ()
  (SETQ
    *HCNM-CNMSETTINGS*
     (INI_READSECTION (HCNM-INI (HCNM-PROJ)) "CNM")
  )
)

;;;Sets a variable in a temporary global lisp list
(DEFUN
   HCNM-SETVARTEMP (VAR VAL)
  (COND
    ((ASSOC VAR *HCNM-CNMTEMPSETTINGS*)
     (SETQ
       *HCNM-CNMTEMPSETTINGS*
        (SUBST
          (LIST VAR VAL)
          (ASSOC VAR *HCNM-CNMTEMPSETTINGS*)
          *HCNM-CNMTEMPSETTINGS*
        )
     )
    )
    (T
     (SETQ
       *HCNM-CNMTEMPSETTINGS*
        (CONS
          (LIST VAR VAL)
          *HCNM-CNMTEMPSETTINGS*
        )
     )
    )
  )
)

;;;Gets a variable in a temporary global lisp list
;;;If it's not present there, gets real value.
(DEFUN
   HCNM-GETVARTEMP (VAR)
  (COND
    ((CADR (ASSOC VAR *HCNM-CNMTEMPSETTINGS*)))
    (T (HCNM-GETVAR VAR))
  )
)

;;;Saves the global list of temporary settings in the real global lisp list and in CNM.INI
(DEFUN
   HCNM-SAVETEMPSETTINGS ()
  (FOREACH
     SETTING *HCNM-CNMTEMPSETTINGS*
    (HCNM-SETVAR (CAR SETTING) (CADR SETTING))
  )
)

;;;Sets a variable in the global lisp list and in CNM.INI
(DEFUN
   HCNM-SETVAR (VAR VAL)
  (SETQ
    *HCNM-CNMSETTINGS*
     (COND
       ((ASSOC VAR *HCNM-CNMSETTINGS*)
        (SUBST
          (LIST VAR VAL)
          (ASSOC VAR *HCNM-CNMSETTINGS*)
          *HCNM-CNMSETTINGS*
        )
       )
       (T (CONS (LIST VAR VAL) *HCNM-CNMSETTINGS*))
     )
  )
  (INI_WRITEENTRY (HCNM-INI (HCNM-PROJ)) "CNM" VAR VAL)
  VAL
)

;;; HCNM-GETVAR
(DEFUN
   HCNM-GETVAR (VAR / ADDTOLIST ADDTOPROJ DIR INI PROJROOT VAL)
;;; Var is case sensitive
  (SETQ
    ADDTOLIST T
    ADDTOPROJ T
  )
  ;;Get list if no list
  (IF (NOT *HCNM-CNMSETTINGS*)
    (HCNM-GETSETTINGS)
  )
  (COND
    ;;Try getting from list
    ((SETQ VAL (CADR (ASSOC VAR *HCNM-CNMSETTINGS*)))
     (SETQ
       ADDTOLIST NIL
       ADDTOPROJ NIL
     )
    )
    ;;Try getting from project ini.
    ((AND
       (SETQ DIR (HCNM-PROJ))
       (SETQ INI (FINDFILE (STRCAT DIR "\\" "cnm.ini")))
       (SETQ
         VAL
          (INI_READENTRY INI "CNM" SETTING)
         ADDTOPROJ NIL
       )
     )
    )
    ;;Get from app ini if not.
    ((AND
       (SETQ DIR (FINDFILE "cnm.mnl"))
       (SETQ
         ;;Behavior change 20080414 TGH
         ;;Made it get the random CNM.INI in path instead of app folder.
         INI
          (FINDFILE "cnm.ini")
       )
       (SETQ VAL (INI_READENTRY INI "CNM" SETTING))
     )
    )
    ;;Use default if there is one
    ((SETQ VAL (HCNM-VARTODEFAULTVAL VAR)))
    ;;Otherwise fail.
    (T
     (ALERT
       (STRCAT
         "Fatal error in CNM:\nCould not initialize the variable\n"
         (HAWS-PRIN1-TO-STRING VAR)
       )
     )
     (SETQ
       ADDTOLIST NIL
       ADDTOPROJ NIL
     )
    )
  )
  (IF ADDTOLIST
    (SETQ
      *HCNM-CNMSETTINGS*
       (CONS (LIST SETTING VAL) *HCNM-CNMSETTINGS*)
    )
  )
  (IF ADDTOPROJ
    (INI_WRITEENTRY (HCNM-INI (HCNM-PROJ)) "CNM" SETTING VAL)
  )
  VAL
)

(DEFUN
   HCNM-VARTODEFAULTVAL (VAR)
  (CADR (ASSOC VAR (HCNM-VARDEFAULTS)))
)

(DEFUN
   HCNM-VARDEFAULTS ()
  '(("ProjectNotes" "constnot.txt")
    ("ProjectNotesEditor" "notepad.exe")
    ("NoteTypes" "BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI")
    ("DoCurrentTabOnly" "0")
    ("PhaseAlias1" "1")
    ("PhaseAlias2" "2")
    ("PhaseAlias3" "3")
    ("PhaseAlias4" "4")
    ("PhaseAlias5" "5")
    ("PhaseAlias6" "6")
    ("PhaseAlias7" "7")
    ("PhaseAlias8" "8")
    ("PhaseAlias9" "9")
    ("InsertTablePhases" "No")
    ("TableWidth" "65")
    ("PhaseWidthAdd" "9")
    ("LineSpacing" "1.5")
    ("NoteSpacing" "3")
    ("NumberToDescriptionWidth" "2.5")
    ("DescriptionToQuantityWidth" "56")
    ("QuantityToQuantityWidth" "9")
    ("QuantityToUnitsWidth" "1")
    ("ShowKeyTableGrid" "0")
    ("ShowKeyTableQuantities" "1")
    ("BubbleHooks" "0")
    ("BubbleLeaderConnectOsnap" "mid,end")
    ("ImportLayerSettings" "No")
    ("LayersEditor" "notepad.exe")
    ("NotesLeaderDimstyle" "")
    ("NotesKeyTableDimstyle" "")
    ("TCGLeaderDimstyle" "TCG Leader")
   )
)

;;Gets an entire ini file (per CNM forum) from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(DEFUN
   HCNM-GETINIDEFAULTS (PROJ / APP APPINI PROJINI)
  (SETQ PROJINI (STRCAT PROJ "\\" "cnm.ini"))
  (COND
    ((AND
       (SETQ APP (HAWS-FILENAME-DIRECTORY (FINDFILE "cnm.mnl")))
       (SETQ APPINI (FINDFILE (STRCAT APP "\\" "cnm.ini")))
     )
     (ALERT
       (PRINC
         (STRCAT
           "CNM is copying settings found in\n" APPINI "\nto\n" PROJINI
           "\nfor this project."
          )
       )
     )
     (HAWS-FILE-COPY APPINI PROJINI)
     (WHILE (NOT (FINDFILE PROJINI)))
     PROJINI
    )
    (T
     (ALERT
       (PRINC
         (STRCAT
           "CNM could not find a settings file in\n" APP
           "\n\nPutting hard-coded defaults in\n" PROJINI
           "\nfor this project."
          )
       )
     )
     (SETQ F2 (OPEN PROJINI "w"))
     (PRINC "[CNM]" F2)
     (SETQ F2 (CLOSE F2))
     (SETQ *HCNM-CNMSETTINGS* (HCNM-VARDEFAULTS))
     (HCNM-SAVECNMTOINI PROJ)
     PROJINI
    )
  )
)


;;Saves *hcnm-cnmsettings* to this project's ini
(DEFUN
   HCNM-SAVECNMTOINI (PROJ)
  (INI_WRITESECTION
    (HCNM-INI
      (COND
        (PROJ)
        ((HCNM-PROJ))
      )
    )
    "CNM"
    *HCNM-CNMSETTINGS*
  )
)

(DEFUN
   HCNM-SET-DIMSTYLE (KEY / DSTY)
  ;;Set dimstyle as requested by calling function and set by user
  ;;First, get dimstyle name
  (SETQ DSTY (HCNM-GETVAR KEY))
  ;;Second, if the style is TCGLeader and doesn't already exist, set the _DotSmall ldrblk.
  (COND
    ((AND
       (= KEY "TCGLeaderDimstyle")
       (NOT (TBLSEARCH "DIMSTYLE" DSTY))
     )
     (COMMAND "._dim1" "_dimldrblk" "_DotSmall")
    )
  )
  ;;Third, if the desired style exists, save current style for later, then restore the desired style.
  (COND
    ((AND (/= KEY "")(TBLSEARCH "DIMSTYLE" DSTY))
	(ALERT (VL-PRIN1-TO-STRING  (TBLSEARCH "DIMSTYLE" DSTY)))
     (SETQ *HCNM-DIMSTYLEOLD* (GETVAR "dimstyle"))
     (COMMAND "._dimstyle" "_restore" DSTY)
    )
  )
)
(DEFUN
   HCNM-RESTORE-DIMSTYLE ()
  ;;Third, if the desired style exists, save current style for later, then restore the desired style.
  (COND
    (*HCNM-DIMSTYLEOLD*
     (COMMAND "._dimstyle" "_restore" *HCNM-DIMSTYLEOLD*)
    )
  )
)

;;;============================================================================
;;;
;;; Begin Project Notes functions
;;;
;;;============================================================================

;; HCNM-PROJNOTES gets a valid project notes file
;; It should resolve all errors and user conditions.
;; and return a "drive:\\...\\projroot\\pnname" filename to other functions.
(DEFUN
   HCNM-PROJNOTES (/ APP APPPN OPT1 PNNAME PROJNOTES)
  (SETQ PNNAME (HCNM-GETVAR "ProjectNotes"))
  (IF (= PNNAME "")
    (HCNM-SETVAR "ProjectNotes" (SETQ PNNAME "constnot.txt"))
  )
  (HAWS-MILEPOST
    (STRCAT
      "HCNM-PROJNOTES is beginning with ProjectNotes="
      PNNAME
    )
  )
  (COND
    ;;First, if there is a directory given, try to find project notes there.
    ((AND
       (/= "" (HAWS-FILENAME-DIRECTORY PNNAME))
       (SETQ PROJNOTES (FINDFILE PNNAME))
     )
     PROJNOTES
    )
    ;;Second, try to find the pnname (ProjectNotes=) given in CNM.INI
    ;;in the project folder ignoring any directory in the name.
    ((FINDFILE
       (SETQ
         PROJNOTES
          (STRCAT
            (HCNM-PROJ)
            "\\"
            (HAWS-FILENAME-BASE PNNAME)
            (HAWS-FILENAME-EXTENSION PNNAME)
          )
       )
     )
     ;;Record the find in the INI
     (HCNM-SETVAR "ProjectNotes" PROJNOTES)
    )
    ;;Third choice, we couldn't find the Project Notes specified,
    ;;so try to get Project Notes from the app folder (where CNM.MNL is)
    ;;and put it in the location tried above.
    ;;To pamper new users, get constnot.csv if CNMEdit.exe is current.
    ;;The CFREAD functions will later evaluate the necessity of changing the file
    ;;format and name.
    ((AND
       (SETQ APP (FINDFILE "cnm.mnl"))
       (SETQ
         APPPN
          (FINDFILE
            (STRCAT
              (HAWS-FILENAME-DIRECTORY APP)
              "\\"
              (IF (WCMATCH
                    (STRCASE (HCNM-READNOTESEDITOR))
                    "*CNMEDIT.EXE"
                  )
                "constnot.csv"
                "constnot.txt"
              )
            )
          )
       )
     )
     ;;If CONSTNOT.TXT was found in the app folder,
     ;;try to copy it to this project.
     (HAWS-FILE-COPY APPPN PROJNOTES)
     ;;Record the find in the INI
     (HCNM-SETVAR "ProjectNotes" PROJNOTES)
    )
    ;;Third and last choice, fail with alert.
    (T
     (ALERT
       (PRINC
         (STRCAT
           "Fatal error in CNM:\nCouldn't find or create Project Notes.\n\nPlease create project notes at "
           PROJNOTES
           "\nor change the current Project Notes or Project Folder."
         )
       )
     )
    )
  )
)
(DEFUN
   C:HCNM-CHANGEPROJNOTES ()
  (HCNM-CHANGEPROJNOTES)
  (PRINC)
)

(DEFUN
   HCNM-CHANGEPROJNOTES ()
  (HCNM-SETVAR "ProjectNotes" (HCNM-GETPROJNOTES))
)

(DEFUN
   HCNM-GETPROJNOTES (/ DPNAME OLDPROJNOTES PROJNOTES)
  (HCNM-PROJINIT)                       ;Initialize variables in case any files changed.
  (SETQ OLDPROJNOTES (HCNM-PROJNOTES))
  (SETQ DPNAME (STRCAT (GETVAR "dwgprefix") "constnot.txt"))
  (SETQ
    PROJNOTES
     (GETFILED
       "Select Project Notes Filename"
       (HCNM-GETVAR "ProjectNotes")
       ""
       37
     )
  )
  ;;Remove path if project notes is in project folder.
  (COND
    ((= (HAWS-FILENAME-DIRECTORY PROJNOTES) (HCNM-PROJ))
     (SETQ
       PROJNOTES
        (STRCAT
          (HAWS-FILENAME-BASE PROJNOTES)
          (HAWS-FILENAME-EXTENSION PROJNOTES)
        )
     )
    )
  )
  PROJNOTES
)

;; HCNM-READCF
;; Reads any acceptable Project Notes file format to a *HCNM-CNMPROJECTNOTES* list of the following format
;; '((0 . "comment")(1 "var" "val1" "val2")(2 . "title")(3 "type" "num" "unit" "count" "text"))
;; The acceptable file formats are:
;; TXT1 Fixed field ;Comment\nSET VAR VAL\nNUM (comment)\nBOX (type)\nTITLE \n1    Text thru column 67...UNTCOUNT\n     Cont. text.
;; TXT2 White space delimited ;Comment\n
;; Excel CSV
;; Doesn't do project management except to write txt2 settings to cnm.ini in the same folder as projnotes.
(DEFUN
   HCNM-READCF (PROJNOTES / BAKPROJNOTES PNFORMAT)
  ;;Do a file read to figure out what the file format is.
  ;;For now, assume that a file that has any of the shape keys followed by a comma ("BOX,", etc.) is CSV
  ;;any other file is TXT2
  (HAWS-MILEPOST
    (STRCAT
      "HCNM-READCF is deciphering the format of "
      PROJNOTES
      "\nand evaluating the need for format conversion."
    )
  )
  (SETQ F1 (OPEN PROJNOTES "r"))
  (WHILE (AND (NOT PNFORMAT) (SETQ RDLIN (READ-LINE F1)))
    (COND
      ((WCMATCH
         (SUBSTR RDLIN 1 4)
         "BOX`,,CIR`,,DIA`,,ELL`,,HEX`,,OCT`,,PEN`,,REC`,,SST`,,TRI`,"
       )
       (SETQ PNFORMAT "csv")
      )
      ((WCMATCH (SUBSTR RDLIN 1 3) (HCNM-GETVAR "NoteTypes"))
       (SETQ PNFORMAT "txt2")
      )
    )
  )
  (SETQ F1 (CLOSE F1))
  (COND
    ((= PNFORMAT "txt2")
     (HCNM-READCFTXT2 PROJNOTES)
     (COND
       ((WCMATCH (STRCASE (HCNM-READNOTESEDITOR)) "*CNMEDIT.EXE")
        (SETQ BAKPROJNOTES PROJNOTES)
        (HAWS-FILE-COPY
          PROJNOTES
          (PROGN
            (WHILE (FINDFILE
                     (SETQ
                       BAKPROJNOTES
                        (STRCAT
                          (HAWS-FILENAME-DIRECTORY BAKPROJNOTES)
                          "\\"
                          (HAWS-FILENAME-BASE BAKPROJNOTES)
                          "0"
                          (HAWS-FILENAME-EXTENSION BAKPROJNOTES)
                        )
                     )
                   )
            )
            BAKPROJNOTES
          )
        )
        (ALERT
          (PRINC
            (STRCAT
              "CNM needs to convert\n"
              PROJNOTES
              "\nto comma-separated (csv) format.\n\nCurrent version backed up as\n"
              BAKPROJNOTES
            )
          )
        )
        (HCNM-WRITECFCSV PROJNOTES)
       )
     )
    )
    ((= PNFORMAT "csv")
     (HCNM-READCFCSV PROJNOTES)
     (COND
       ((NOT
          (WCMATCH (STRCASE (HCNM-READNOTESEDITOR)) "*CNMEDIT.EXE")
        )
        (SETQ BAKPROJNOTES PROJNOTES)
        (HAWS-FILE-COPY
          PROJNOTES
          (PROGN
            (WHILE (FINDFILE
                     (SETQ
                       BAKPROJNOTES
                        (STRCAT
                          (HAWS-FILENAME-DIRECTORY BAKPROJNOTES)
                          "\\"
                          (HAWS-FILENAME-BASE BAKPROJNOTES)
                          "0"
                          (HAWS-FILENAME-EXTENSION BAKPROJNOTES)
                        )
                     )
                   )
            )
            BAKPROJNOTES
          )
        )
        (ALERT
          (PRINC
            (STRCAT
              "CNM needs to convert\n"
              PROJNOTES
              "\nto traditional text format.\n\nCurrent version backed up as\n"
              BAKPROJNOTES
            )
          )
        )
        (HCNM-WRITECFTXT2 PROJNOTES)
       )
     )
    )
    ((NOT PNFORMAT)
     (ALERT
       (PRINC
         (STRCAT
           "Current Project Notes file\n"
           PROJNOTES
           "\ndoes not contain recognizable project notes.\n\nPlease correct the file and try again."
         )
       )
     )
     (EXIT)
    )
  )
)

(DEFUN
   HCNM-READCFTXT2 (PROJNOTES / ALERTNOTE ALERTTITLE CFITEM CFLIST
                    CFLIST2 COMMENTBEGIN FILEV42 ILINE ININAME NOTTYP
                    RDLIN VAL1 VAL2 VAR VARLIST
                   )
  (SETQ
    TYPWC
     (HCNM-GETVAR "NoteTypes")          ; Get typwc (which may open f1) before opening f1
    F1 (OPEN PROJNOTES "r")
  )
  (WHILE (SETQ RDLIN (READ-LINE F1))
    (COND
      ;;Comment
      ((= ";" (SUBSTR RDLIN 1 1))
       (SETQ CFLIST (CONS (CONS 0 (SUBSTR RDLIN 2)) CFLIST))
      )
      ;;Variable setting
      ((= "SET" (HAWS-RDFLD 1 RDLIN "W" 1))
       (SETQ
         VAR  (HAWS-RDFLD 2 RDLIN "W" 1)
         VAL1 (HAWS-RDFLD 3 RDLIN "W" 1)
       )
       (COND
         ;;CNMVERSION greater than 4.1 triggers ignoring SET variables other than TXTHT
         ((AND (= VAR "CNMVERSION") (< 4.1 (ATOF VAL1)))
          (SETQ
            FILEV42 T
            CFLIST
             (CONS (LIST 1 VAR VAL1) CFLIST)
          )
         )
         ;;TXTHT gets added to CFLIST
         ((= VAR "TXTHT")
          (SETQ CFLIST (CONS (LIST 1 VAR VAL1) CFLIST))
         )
         ;;If file hasn't been converted yet (settings put in ini)
         ;;All others (unless deprecated) get put in CNM.INI
         ;;and left in with a note for backward compatibility.
         ((NOT FILEV42)
          (SETQ
            VAR
             (COND
               ((= VAR "LINSPC") "LineSpacing")
               ((= VAR "TBLWID") "TableWidth")
               ((= VAR "PHASEWID") "PhaseWidthAdd")
               ((= VAR "NDWID") "NumberToDescriptionWidth")
               ((= VAR "DQWID") "DescriptionToQuantityWidth")
               ((= VAR "QQWID") "QuantityToQuantityWidth")
               ((= VAR "QUWID") "QuantityToUnitsWidth")
               ((= VAR "PHASES") "InsertTablePhases")
               ((= VAR "CTABONLY") "DoCurrentTabOnly")
               ((= VAR "PHASEALIAS") "PhaseAlias")
               (T NIL)                  ;Don't use unlisted/deprecated variables
             )
          )
          (COND
            ((= VAR "PhaseAlias")
             (SETQ
               VAL2 (HAWS-RDFLD 4 RDLIN "W" 1)
               VAR  (STRCAT VAR VAL1)
               VAL1 VAL2
             )
            )
          )
          (IF VAR
            (SETQ VARLIST (CONS (LIST VAR VAL1) VARLIST))
          )
          (IF (= VAR "LineSpacing")
            (SETQ VARLIST (CONS (LIST "NoteSpacing" VAL1) VARLIST))
          )
         )
       )
      )
      ;;TXT2 header.  Turn into comment.
      ((= "NUM" (SUBSTR RDLIN 1 3))
       (SETQ
         CFLIST
          (CONS (CONS 0 (STRCAT "NUM" (SUBSTR RDLIN 5))) CFLIST)
       )
      )
      ;;Note type/shape heading.
      ((WCMATCH (SUBSTR RDLIN 1 3) TYPWC)
       (SETQ NOTTYP (SUBSTR RDLIN 1 3))
      )
      ;;Title.
      ((= "TITLE" (SUBSTR RDLIN 1 5))
       (COND
         (NOTTYP
          (SETQ
            CFLIST
             (CONS
               (LIST
                 2
                 NOTTYP
                 (HAWS-RDFLD 1 (SUBSTR RDLIN 6 62) 62 1)
               )
               CFLIST
             )
          )
         )
         (T (SETQ ALERTTITLE T))
       )
      )
      ;;Note number.
      ((/= "" (SETQ NOTNUM (HAWS-RDFLD 1 RDLIN 5 1)))
       (COND
         (NOTTYP
          (SETQ
            CFLIST
             (CONS
               (LIST
                 3
                 NOTTYP
                 NOTNUM
                 (HAWS-RDFLD 1 (SUBSTR RDLIN 68 3) 3 1)
                 (HAWS-RDFLD 15 RDLIN 5 1)
                 (LIST (HAWS-RDFLD 1 (SUBSTR RDLIN 6 62) 62 1))
               )
               CFLIST
             )
          )
         )
         (T (SETQ ALERTNOTE T))
       )
      )
      ;;Additional note description.
      ((= "" (HAWS-RDFLD 1 RDLIN 5 1))
       (SETQ N -1)
       ;;If there's no CFLIST, educate the user.
       (COND
         ((NOT CFLIST)
          (ALERT
            (PRINC
              (STRCAT
                "A note continuation line was found in "
                PROJNOTES
                " before any notes.\nPlease edit the file to correct the problem.\n(Note continuations are empty or begin with five spaces.\nFile comments must begin with a semi-colon \";\".)"
              )
            )
          )
          (EXIT)
         )
         (T
          ;;Find first note in list (there may be comments before it).
          (WHILE (AND
                   (/= 3 (CAR (NTH (SETQ N (1+ N)) CFLIST)))
                   (< N (LENGTH CFLIST))
                 )
          )
          (IF (/= N (LENGTH CFLIST))
            (SETQ
              TEMP   (NTH 5 (NTH N CFLIST))
              CFLIST (CONS
                       (SUBST
                         (REVERSE
                           (CONS
                             (HAWS-RDFLD 2 RDLIN 5 3)
                             (COND
                               ((= TEMP '("")) NIL)
                               ((REVERSE TEMP))
                             )
                           )
                         )
                         TEMP
                         (NTH N CFLIST)
                       )
                       (CDR CFLIST)
                     )
            )
          )
         )
       )
      )
    )
  )
  (SETQ F1 (CLOSE F1))
  (IF ALERTTITLE
    (ALERT
      (PRINC
        (STRCAT
          "Title(s) were found in"
          PROJNOTES
          "\nthat came before any shape.\n\nThe title(s) will never be printed."
        )
      )
    )
  )
  (IF ALERTNOTE
    (ALERT
      (PRINC
        (STRCAT
          "Note(s) were found in "
          PROJNOTES
          "\nthat came before any shape.\n\nThe note(s) will never be found or printed."
        )
      )
    )
  )
  ;;Put settings from v4 Project Notes into CNM.INI and alert.
  (COND
    ((AND (NOT FILEV42) VARLIST)
     (ALERT
       (PRINC
         "\nCNM is moving project settings from version 4.1 Project Notes to CNM.INI."
       )
     )
     (SETQ *HCNM-CNMSETTINGS* VARLIST)
     (HCNM-SAVECNMTOINI NIL)
    )
  )
  (SETQ *HCNM-CNMPROJECTNOTES* (REVERSE CFLIST))
  (HAWS-MILEPOST
    (STRCAT
      "HCNM-READCFTXT2 read "
      (ITOA (LENGTH *HCNM-CNMPROJECTNOTES*))
      " lines from "
      PROJNOTES
      "."
    )
  )
  ;;Add comments and version number to old file.
  (COND
    ((NOT FILEV42)
     (SETQ
       *HCNM-CNMPROJECTNOTES*
        (CONS
          (LIST 1 "CNMVERSION" "4.2")
          (REVERSE CFLIST)
        )
     )
     (ALERT
       (PRINC
         (STRCAT
           "\nCNM is converting "
           PROJNOTES
           " to a version 4.2 file.\n\nNote: The meaning of the TXTHT setting has changed\nfrom \"Text height in AutoCAD units\"\nto \"Plotted text height\"\n\nCNM is using the current value of DIMSCALE to convert text heights."
         )
       )
     )
     (SETQ
       F1 (OPEN PROJNOTES "r")
       CFLIST NIL
       ILINE 0
     )
     (WHILE (SETQ RDLIN (READ-LINE F1))
       (SETQ ILINE (1+ ILINE))
       ;;If the line is recognizable as a vestige of version 4.1 variable settings,
       ;;make a note of it for adding a comment.
       (IF (OR (AND
                 (NOT COMMENTBEGIN)
                 (= (SUBSTR RDLIN 1 3) "SET")
                 (/= (HAWS-RDFLD 2 RDLIN "W" 1) "TXTHT")
               )
               (= (SUBSTR RDLIN 1 5) ";SET ")
           )
         (SETQ COMMENTBEGIN (1- ILINE))
       )
       (IF
         (=
           RDLIN
           ";This section shows how to override program size/scale defaults"
         )
          (SETQ COMMENTBEGIN (- ILINE 2))
       )
       (SETQ CFLIST (CONS RDLIN CFLIST))
     )
     (SETQ
       F1    (CLOSE F1)
       F2    (OPEN PROJNOTES "w")
       ILINE 0
     )
     (WRITE-LINE "SET CNMVERSION 4.2" F2)
     (FOREACH
        CFITEM (REVERSE CFLIST)
       (IF (= ILINE COMMENTBEGIN)
         (WRITE-LINE
           ";The variable settings section below is not used by CNM 4.2.\n;All variables except TXTHT (optional) and CNMVERSION are in CNM.INI.\n;You can use TXTHT to vary text heights from one line to the next.\n;CNM uses the current DIMTXT for the whole table if TXTHT is omitted,\n;."
           F2
         )
       )
       (SETQ ILINE (1+ ILINE))
       (WRITE-LINE CFITEM F2)
     )
     (SETQ F2 (CLOSE F2))
    )
  )
)


(DEFUN
   HCNM-READCFCSV (PROJNOTES / CFLIST I IPREV NOTDSCLST NOTDSCSTR NOTTYP
                   RDLIN
                  )
  (SETQ
    TYPWC
     (HCNM-GETVAR "NoteTypes")          ; Get typwc (which may open f1) before opening f1
    F1 (OPEN PROJNOTES "r")
  )
  (WHILE (SETQ RDLIN (READ-LINE F1))
    (COND
      ;;Comment
      ((= ";" (SUBSTR RDLIN 1 1))
       (SETQ CFLIST (CONS (CONS 0 (HAWS-RDFLD 3 RDLIN "," 1)) CFLIST))
      )
      ;;Variable setting
      ((= "SET" (HAWS-RDFLD 1 RDLIN "," 1))
       (SETQ
         CFLIST
          (CONS
            (LIST
              1
              (HAWS-RDFLD 2 RDLIN "," 1)
              (HAWS-RDFLD 3 RDLIN "," 1)
            )
            CFLIST
          )
       )
      )
      ;;Note or title
      ((WCMATCH (SETQ NOTTYP (SUBSTR RDLIN 1 3)) TYPWC)
       (COND
         ((= "TITLE" (HAWS-RDFLD 2 RDLIN "," 1))
          (SETQ
            CFLIST
             (CONS
               (LIST 2 NOTTYP (HAWS-RDFLD 3 RDLIN "," 1))
               CFLIST
             )
          )
         )
         (T
          (SETQ
            NOTDSCSTR
             (HAWS-RDFLD 3 RDLIN "," 1)
            NOTDSCLST NIL
            IPREV 1
            I 0
          )
          (WHILE (/= "" (SUBSTR NOTDSCSTR (SETQ I (1+ I)) 1))
            (IF (= "\\n" (SUBSTR NOTDSCSTR I 2))
              (SETQ
                NOTDSCLST
                 (CONS (LIST IPREV (- I IPREV)) NOTDSCLST)
                IPREV
                 (+ I 2)
              )
            )
          )
          (SETQ
            NOTDSCLST
             (MAPCAR
               '(LAMBDA (I)
                  (SUBSTR NOTDSCSTR (CAR I) (CADR I))
                )
               NOTDSCLST
             )
          )
          (SETQ
            CFLIST
             (CONS
               (LIST
                 3
                 NOTTYP
                 (HAWS-RDFLD 2 RDLIN "," 1)
                 (HAWS-RDFLD 4 RDLIN "," 1)
                 (HAWS-RDFLD 5 RDLIN "," 1)
                 (REVERSE
                   (CONS (SUBSTR NOTDSCSTR IPREV I) NOTDSCLST)
                 )
               )
               CFLIST
             )
          )
         )
       )
      )
    )
  )
  (SETQ F1 (CLOSE F1))
  (SETQ *HCNM-CNMPROJECTNOTES* (REVERSE CFLIST))
)

(DEFUN
   HCNM-WRITECFTXT2 (PROJNOTES / I ITEM NOTTYP NOTTXT NOTTXTNEW)
  (ALERT
    (PRINC
      (STRCAT
        "CNM is converting\n"
        PROJNOTES
        "\nto traditional text format."
      )
    )
  )
  (SETQ F2 (OPEN PROJNOTES "w"))
  (FOREACH
     ITEM *HCNM-CNMPROJECTNOTES*
    (COND
      ;;Comment
      ((= 0 (CAR ITEM)) (WRITE-LINE (STRCAT ";" (CDR ITEM)) F2))
      ;;Set variable (TXTHT only at this time)
      ((= 1 (CAR ITEM))
       (WRITE-LINE (STRCAT "SET " (CADR ITEM) " " (CADDR ITEM)) F2)
      )
      ;;Title
      ((= 2 (CAR ITEM))
       (IF (/= NOTTYP (SETQ NOTTYP (CADR ITEM)))
         (WRITE-LINE NOTTYP F2)
       )
       (WRITE-LINE (STRCAT "TITLE " (CADDR ITEM)) F2)
      )
      ;;Note
      ((= 3 (CAR ITEM))
       (IF (/= NOTTYP (SETQ NOTTYP (CADR ITEM)))
         (WRITE-LINE NOTTYP F2)
       )
       (PRINC
         (STRCAT
           (HAWS-MKFLD (CADDR ITEM) 5)
           (HAWS-MKFLD (CAR (NTH 5 ITEM)) 62)
           (HAWS-MKFLD (CADDDR ITEM) 3)
           (NTH 4 ITEM)
           "\n"
         )
         F2
       )
       (FOREACH
          ITEM (CDR (NTH 5 ITEM))
         (PRINC (STRCAT "     " ITEM "\n") F2)
       )
      )
    )
  )
  (SETQ F2 (CLOSE F2))
  *HCNM-CNMPROJECTNOTES*
)

(DEFUN
   HCNM-WRITECFCSV (PROJNOTES / DESC DESCLINE ITEM NOTTYP)
  (ALERT
    (PRINC
      (STRCAT
        "CNM is converting\n"
        PROJNOTES
        "\nto comma-separated format."
      )
    )
  )
  (SETQ F2 (OPEN PROJNOTES "w"))
  (FOREACH
     ITEM *HCNM-CNMPROJECTNOTES*
    (COND
      ((= 0 (CAR ITEM))
       (WRITE-LINE
         (STRCAT
           ";"
           (COND
             (NOTTYP)
             ("CIR")
           )
           ",N/A,"
           (HAWS-MKFLD (CDR ITEM) ",")
         )
         F2
       )
      )
      ((= 1 (CAR ITEM))
       (WRITE-LINE (STRCAT "SET," (CADR ITEM) "," (CADDR ITEM)) F2)
      )
      ((= 2 (CAR ITEM))
       (SETQ NOTTYP (CADR ITEM))
       (WRITE-LINE (STRCAT NOTTYP ",TITLE," (CADDR ITEM) ",,") F2)
      )
      ((= 3 (CAR ITEM))
       (SETQ
         DESC ""
         NOTTYP
          (CADR ITEM)
       )
       (FOREACH
          DESCLINE (NTH 5 ITEM)
         (SETQ DESC (STRCAT DESC "\\n" DESCLINE))
       )
       (SETQ DESC (SUBSTR DESC 3))
       (WRITE-LINE
         (STRCAT
           NOTTYP
           ","
           (CADDR ITEM)
           ","
           (HAWS-MKFLD DESC ",")
           (CADDDR ITEM)
           ","
           (NTH 4 ITEM)
         )
         F2
       )
      )
    )
  )
  (SETQ F2 (CLOSE F2))
  *HCNM-CNMPROJECTNOTES*
)

;;;================================================================================================================
;;;
;;; Begin Project Notes Editor functions section
;;;
;;;================================================================================================================
(DEFUN C:HCNM-NOTESEDITOR () (HCNM-NOTESEDITOR))

(DEFUN
   HCNM-NOTESEDITOR (/ NEWEDITOR)
  (SETQ NEWEDITOR (HCNM-GETNOTESEDITOR))
  (HCNM-WRITENOTESEDITOR NEWEDITOR)
)

(DEFUN
   HCNM-GETNOTESEDITOR (/ OLDEDITOR NEWEDITOR)
  (SETQ
    OLDEDITOR
     (HCNM-READNOTESEDITOR)
    NEWEDITOR
     (GETFILED "Select editor" OLDEDITOR "" 0)
  )
)

(DEFUN
   HCNM-READNOTESEDITOR (/ NOTESEDITOR)
  (SETQ
    NOTESEDITOR
     (COND
       ((HAWS-VLISP-P)
        (VL-REGISTRY-READ
          "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
          "NotesEditor"
        )
       )
       (T (HCNM-GETVAR "ProjectNotesEditor"))
     )
  )
  (COND
    (NOTESEDITOR)
    ("notepad.exe")
  )
)

(DEFUN
   HCNM-WRITENOTESEDITOR (NEWEDITOR)
  (COND
    ((OR (NOT NEWEDITOR) (NOT (FINDFILE NEWEDITOR)))
     (ALERT "File not found.  Using notepad.exe.")
     (SETQ NEWEDITOR "notepad.exe")
    )
  )
  (COND
    ((HAWS-VLISP-P)
     (VL-REGISTRY-WRITE
       "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
       "NotesEditor"
       NEWEDITOR
     )
    )
  )
  (HCNM-SETVAR "ProjectNotesEditor" NEWEDITOR)
  (ALERT
    (PRINC
      (STRCAT "Editor for project notes is now \n" NEWEDITOR)
    )
  )
  NEWEDITOR
)




(DEFUN
   C:HCNM-NOTESEDIT
   (/ CNMEDITP NOTESEDITOR PROJROOT I RDLIN TFNAME WSHSHELL)
  (SETQ NOTESEDITOR (HCNM-READNOTESEDITOR))
  (IF (= (STRCASE (SUBSTR NOTESEDITOR (- (STRLEN NOTESEDITOR) 10)))
         "CNMEDIT.EXE"
      )
    (SETQ CNMEDITP T)
  )
  (IF CNMEDITP
    (HAWS-ERRDEF 2)
    (HAWS-ERRDEF 1)
  )
  ;;Since this is a user command, possibly after deletion of project root files,
  ;;refresh project root at beginning.
  (HCNM-PROJINIT)                       ;Initialize project after user pauses
  (HCNM-READCF (HCNM-PROJNOTES))        ;Read to convert project notes if necessary before editing
  ;; The code below replaced because it locked the drawings until project notes editor closed.
  ;;(SETQ WSHSHELL (VLAX-CREATE-OBJECT "WScript.Shell"))
  ;;(SETQ
  ;;  OEXEC
  ;;   (VLAX-INVOKE-METHOD
  ;;     WSHSHELL
  ;;     'EXEC
  ;;     (STRCAT "\"" NOTESEDITOR "\" \"" (HCNM-PROJNOTES) "\"" (IF CNMEDITP (STRCAT " \"" (HCNM-PROJ) "\\cnm.ini\"") ""))
  ;;   )
  ;;)
  ;;(princ "\nOpening Project Notes for editing.  Please close editor before saving drawings.")
  (PRINC (STRCAT "\nEditing " (HCNM-PROJNOTES) "."))
  (STARTAPP
    (STRCAT "\"" NOTESEDITOR "\"")
    (STRCAT
      "\""
      (HCNM-PROJNOTES)
      "\""
      (IF CNMEDITP
        (STRCAT " \"" (HCNM-PROJ) "\\cnm.ini\"")
        ""
      )
    )
  )
)

;;;================================================================================================================
;;;
;;; Begin Layers Editor functions section
;;;
;;;================================================================================================================
;; Change layer defaults editor
(DEFUN C:HCNM-LAYEREDITOR () (HCNM-LAYEREDITOR))

(DEFUN
   HCNM-LAYEREDITOR (/ OLDEDITOR NEWEDITOR)
  (SETQ
    OLDEDITOR
     (HCNM-READLAYEREDITOR)
    NEWEDITOR
     (GETFILED "Select editor" OLDEDITOR "" 0)
  )
  (HCNM-WRITELAYEREDITOR NEWEDITOR)
)

(DEFUN
   HCNM-READLAYEREDITOR (/ LAYEREDITOR)
  (SETQ
    LAYEREDITOR
     (COND
       ((HAWS-VLISP-P)
        (VL-REGISTRY-READ
          "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
          "LayersEditor"
        )
       )
       (T (HCNM-GETVAR "LayersEditor"))
     )
  )
  (COND
    (LAYEREDITOR)
    ("notepad.exe")
  )
)

(DEFUN
   HCNM-WRITELAYEREDITOR (NEWEDITOR)
  (COND
    ((OR (NOT NEWEDITOR) (NOT (FINDFILE NEWEDITOR)))
     (ALERT "File not found.  Using notepad.exe.")
     (SETQ NEWEDITOR "notepad.exe")
    )
  )
  (COND
    ((HAWS-VLISP-P)
     (VL-REGISTRY-WRITE
       "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
       "LayersEditor"
       NEWEDITOR
     )
    )
  )
  (HCNM-SETVAR "LayersEditor" NEWEDITOR)
  (ALERT
    (PRINC
      (STRCAT
        "Editor for layer defaults file is now \n"
        NEWEDITOR
      )
    )
  )
  NEWEDITOR
)

;; Edit layer defaults
(DEFUN
   C:HCNM-CNMLAYER (/ LAYERSEDITOR LAYERSFILE WSHSHELL)
  (SETQ *HAWS:LAYERS* NIL)
  (SETQ
    LAYERSEDITOR
     (HCNM-READLAYEREDITOR)
    LAYERSFILE
     (FINDFILE "layers.dat")
  )
  ;;The code below used before because I couldn't guarantee editor would get focus.
  ;;(SETQ WSHSHELL (VLAX-CREATE-OBJECT "WScript.Shell"))
  ;;(SETQ
  ;;  OEXEC
  ;;   (VLAX-INVOKE-METHOD
  ;;     WSHSHELL
  ;;     'EXEC
  ;;     (STRCAT "\"" LAYERSEDITOR "\" \"" LAYERSFILE "\"")
  ;;   )
  ;;)
  ;;The code below replaced before because I couldn't guarantee editor would get focus.
  (STARTAPP
    (STRCAT "\"" LAYERSEDITOR "\" \"" LAYERSFILE "\"")
  )
  (ALERT
    (STRCAT
      "Click OK to import layer settings after editing and saving."
    )
  )
  (HAWS-GETLAYR "NOTES-EXPORT")         ;Get a layer to renew *HAWS:LAYERS*
  (COMMAND "._layer")
  (FOREACH
     LAYER *HAWS:LAYERS*
    (COMMAND "_n" (CADR LAYER))
    (IF (/= (CADDR LAYER) "")
      (COMMAND "_c" (CADDR LAYER) (CADR LAYER))
    )
    (COMMAND
      "_lt"
      (IF (TBLSEARCH "LTYPE" (CADDDR LAYER))
        (CADDDR LAYER)
        ""
      )
      (CADR LAYER)
    )
  )
  (COMMAND "")
  (PRINC)
)

;;;================================================================================================================
;;;
;;; Begin Bubble Notes commands
;;;
;;;================================================================================================================
;;; SETNOTESBUBBLESTYLE
;;; Saves the users preferred Notes Bubble Style to the registry
(DEFUN
   C:HCNM-SETNOTESBUBBLESTYLE (/ BUBBLEHOOKS)
  (INITGET "Yes No")
  (SETQ
    BUBBLEHOOKS
     (GETKWORD
       "\nInsert bubble notes with hooks? <Yes/No>: "
     )
  )
  (IF BUBBLEHOOKS
    (HCNM-SETVAR "BubbleHooks" (COND((= BUBBLEHOOKS "Yes") "1")("0")))
  )
  (PRINC)
)
;;; Global edit of bubble note phases
(DEFUN
   C:HAWS-PHASEEDIT (/ NEWPHASE OLDPHASE)
  (SETQ
    OLDPHASE
     (GETSTRING "\nEnter phase to change: ")
    NEWPHASE
     (GETSTRING "\nEnter new phase: ")
  )
  (COMMAND
    "._attedit" "_n" "_n" "note???l,note???r" "notephase" "*" OLDPHASE
    NEWPHASE
   )
  (GRAPHSCR)
  (PRINC)
)
;;; Put attributes on NOPLOT layer
(DEFUN
   C:HCNM-ATTNOPLOT ()
  (HCNM-ATTLAYER "NOTESNOPLOT")
  (COMMAND
    "._layer"
    "_Plot"
    "_No"
    (HAWS-GETLAYR "NOTESNOPLOT")
    ""
  )
)
(DEFUN C:HCNM-ATTPLOT () (HCNM-ATTLAYER "0"))
(DEFUN
   HCNM-ATTLAYER (LAYER / AT EL EN ET NPLAYER NPLIST SSET SSLEN)
  (HAWS-ERDF$@ 1)
  (HAWS-VSAVE '("CLAYER"))
  (COMMAND "._undo" "_g")
  (SETQ NPLAYER (CAR (HAWS-GETLAYR LAYER)))
  (IF (NOT (TBLSEARCH "LAYER" NPLAYER))
    (HAWS-MKLAYR LAYER)
  )
  (PROMPT "\nBlocks to change: ")
  (SETQ SSET (SSGET '((0 . "INSERT"))))
  (IF (NOT SSET)
    (PROGN (PROMPT "\nNone found.") (EXIT))
    (PROGN
      (WHILE (SETQ
               EN (CAR
                    (NENTSEL
                      (STRCAT
                        "\nAttributes to change to layer "
                        NPLAYER
                        " by example/<enter when finished>: "
                      )
                    )
                  )
             )
        (IF (= "ATTRIB" (CDR (ASSOC 0 (ENTGET EN))))
          (PROGN
            (REDRAW EN 3)
            (SETQ
              NPLIST
               (CONS (LIST (CDR (ASSOC 2 (ENTGET EN))) EN) NPLIST)
            )
          )
        )
      )
      (FOREACH EN NPLIST (REDRAW (CADR EN) 4))
      ;; Change all of the entities in the selection set.
      (PROMPT
        (STRCAT "\nPutting attributes on " NPLAYER " layer...")
      )
      (SETQ SSLEN (SSLENGTH SSET))
      (WHILE (> SSLEN 0)
        (SETQ EN (SSNAME SSET (SETQ SSLEN (1- SSLEN))))
        (WHILE (AND
                 (SETQ EN (ENTNEXT EN))
                 (/= "SEQEND"
                     (SETQ ET (CDR (ASSOC 0 (SETQ EL (ENTGET EN)))))
                 )
               )
          (COND
            ((AND
               (= ET "ATTRIB")
               (SETQ AT (CDR (ASSOC 2 EL)))
               (ASSOC AT NPLIST)
             )
             (ENTMOD (SUBST (CONS 8 NPLAYER) (ASSOC 8 EL) EL))
             (ENTUPD EN)
            )
          )
        )
      )
      (PROMPT "done.")
    )
  )
  (COMMAND "._undo" "_e")
  (HAWS-VRSTOR)
  (HAWS-ERRRST)
  (PRINC)
)
(DEFUN
   C:HAWS-BUBREDEF (/ SS1)
  (HAWS-ERDF$@ 0)
  (IF (NOT (FINDFILE "attredef.lsp"))
    (ALERT
      (PRINC
        "\nThe AutoDESK support application\nATTREDEF.LSP could not be found.\nFor support see www.hawsedc.com."
      )
    )
    (LOAD "attredef")
  )
  (HAWS-VSAVE '("attreq" "clayer" "regenmode" "osmode"))
  (COMMAND "._undo" "_g")
  (HAWS-MKLAYR "NOTESLDR")
  (COMMAND
    "._view" "_s" "bubredef" "._zoom" "_w" "-10,-10" "10,10"
   )
  (SETVAR "regenmode" 0)
  (IF (SETQ SS1 (SSGET "X" '((0 . "ATTDEF") (2 . "NOTE*"))))
    (COMMAND "._erase" SS1 "")
  )
  (SETQ F1 (OPEN (STRCAT (GETVAR "dwgprefix") "bubredef.scr") "w"))
  (WRITE-LINE "attreq 0" F1)
  (FOREACH
     BLOCK '("box" "cir" "dia" "ell" "hex" "oct" "pen" "rec" "sst")
    (FOREACH
       SIDE '("l" "r")
      (FOREACH
         STYLE '("" "1" "2")
        (COND
          ((SSGET
             "X"
             (LIST
               (CONS 0 "INSERT")
               (CONS 2 (STRCAT "note" BLOCK SIDE STYLE))
             )
           )
           (WRITE-LINE
             "(if (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) \"ACAD_GROUP\"))) \"BUBREDEF\")(command \"._group\" \"_explode\" \"bubredef\"))"
             F1
           )
           (WRITE-LINE
             (STRCAT
               "insert note"
               BLOCK
               SIDE
               STYLE
               "="
               (IF (= STYLE "")
                 (STRCAT "note" BLOCK SIDE "1")
                 ""
               )
               " 0,0   "
             )
             F1
           )
           (WRITE-LINE
             "explode l (setq *hcnm-bubredefss* (ssget \"p\")) select"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTENUM\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTEPHASE\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETYPE\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTEGAP\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT1\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT2\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT3\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT4\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT5\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT6\")))"
             F1
           )
           (WRITE-LINE
             "(ssget \"X\" '((0 . \"ATTDEF\")(2 . \"NOTETXT0\")))"
             F1
           )
           (WRITE-LINE "!*hcnm-bubredefss* " F1)
           (WRITE-LINE "group c bubredef " F1)
           (WRITE-LINE "p " F1)
           (WRITE-LINE "attredef" F1)
           (WRITE-LINE (STRCAT "note" BLOCK SIDE STYLE) F1)
           (WRITE-LINE "group bubredef " F1)
           (WRITE-LINE "0,0" F1)
          )
          ((TBLSEARCH "BLOCK" (STRCAT "note" BLOCK SIDE STYLE))
           (COMMAND
             "._insert"
             (STRCAT
               "note"
               BLOCK
               SIDE
               STYLE
               "="
               (IF (= STYLE "")
                 (STRCAT "note" BLOCK SIDE "1")
                 ""
               )
             )
           )
           (COMMAND)
          )
        )
      )
    )
  )
  (WRITE-LINE
    "view r bubredef view d bubredef attreq 1 ._undo e"
    F1
  )
  (SETQ F1 (CLOSE F1))
  (COMMAND
    "._script"
    (STRCAT (GETVAR "dwgprefix") "bubredef")
  )
  (IF (HAWS-VLISP-P)
    (VL-FILE-DELETE
      (STRCAT (GETVAR "dwgprefix") "bubredef.scr")
    )
  )
  (HAWS-VRSTOR)
  (HAWS-ERRRST)
)

;;;================================================================================================================================================================
;;;
;;; Begin Miscellaneous commands
;;;
;;;================================================================================================================================================================
;;;

;;SETNOTEPHASES
;;Sets the number of phases for this drawing or this folder.
(DEFUN
   C:HAWS-SETNOTEPHASES (/ CFLIST OPT1 PHASES RDLIN)
  (INITGET 1 "Drawing Project")
  (SETQ
    OPT1
     (GETKWORD
       "\nSet number of phases for this drawing only or this project <Drawing/Project>: "
     )
  )
  (INITGET 1 "None")
  (SETQ
    PHASES
     (GETINT
       "\nNumber of phases to use (or None to ignore bubble note phases): "
     )
  )
  (IF (= PHASES "None")
    (SETQ PHASES "0")
    (SETQ PHASES (ITOA PHASES))
  )
  (COMMAND "._insert" (STRCAT "noteqty=noteqty" PHASES))
  (COMMAND)
  (COND
    ((= OPT1 "Drawing")
     (PROMPT
       (STRCAT
         (FINDFILE (STRCAT "noteqty" PHASES ".dwg"))
         " inserted to drawing as noteqty."
       )
     )
    )
    ((= OPT1 "Project")
     (SETQ
       F1     (OPEN (HCNM-PROJNOTES) "r")
       CFLIST NIL
     )
     (WHILE (SETQ RDLIN (READ-LINE F1))
       (COND
         ;;If there is a SET PHASES line already, remove it.
         ((AND
            (= (HAWS-RDFLD 1 RDLIN "W" 1) "SET")
            (= (HAWS-RDFLD 2 RDLIN "W" 1) "PHASES")
          )
         )
         ;;Otherwise regurgitate any other line
         ((SETQ CFLIST (CONS RDLIN CFLIST)))
       )
     )
     (SETQ
       F1 (CLOSE F1)
       CFLIST
        ;;Put the SET PHASES line at the beginning of the file.
        (CONS (STRCAT "SET PHASES " PHASES) (REVERSE CFLIST))
       F1 (OPEN (HCNM-PROJNOTES) "W")
     )
     (FOREACH RDLIN CFLIST (WRITE-LINE RDLIN F1))
     (SETQ F1 (CLOSE F1))
    )
  )
  (PRINC)
)

(DEFUN
   C:HAWS-CNMMENU ()
  (COMMAND "._menuunload" "cnm" "._menuload" "cnm.mnu")
)

(DEFUN
   C:HCNM-ABOUT ()
  (ALERT
    (STRCAT
      "Construction Notes Manager version "
      (HAWS-UNIFIED-VERSION)
      " Thomas Gail Haws\nhttp://www.constructionnotesmanager.com"
    )
  )
)

(DEFUN
   C:HAWS-CNMSETUP (/ ACADPATHPREFIX ACADPATHSUFFIX I OLDACADPATH
                    OLDPROGRAMFOLDER PROGRAMFOLDER
                   )
  (SETQ
    PROGRAMFOLDER
     (GETVAR "dwgprefix")
    PROGRAMFOLDER
     (SUBSTR
       PROGRAMFOLDER
       10
       (1- (STRLEN PROGRAMFOLDER (GETVAR "dwgprefix")))
     )
  )
  (SETQ
    OLDPROGRAMFOLDER
     (VL-REGISTRY-READ
       "HKEY_LOCAL_MACHINE\\Software\\HawsEDC\\CNM"
       "ProgramFolder"
     )
    OLDACADPATH
     (GETVAR "acadprefix")
    ACADPATHPREFIX ""
    ACADPATHSUFFIX OLDACADPATH
  )
  ;;If the old program folder is still in the ACAD path, remove it.
  (IF (AND
        OLDPROGRAMFOLDER
        (WCMATCH OLDACADPATH (STRCAT "*" OLDPROGRAMFOLDER "*"))
      )
    (PROGN
      (WHILE (< (SET
                  MATCHLENGTH
                  (VL-STRING-MISMATCH
                    OLDACADPATH
                    OLDPROGRAMFOLDER
                    (SETQ
                      I (IF I
                          (1+ I)
                          0
                        )
                    )
                  )
                )
                (STRLEN OLDPROGRAMFOLDER)
             )
      )
      (SETQ
        ACADPATHPREFIX
         (SUBSTR OLDACADPATH 1 I)
        ACADPATHSUFFIX
         (SUBSTR
           OLDACADPATH
           (+ I 1 (STRLEN OLDPROGRAMFOLDER))
         )
      )
    )
  )
  (ALERT
    (STRCAT
      "Construction Notes Manager Setup will now add\n"
      PROGRAMFOLDER
      "\nto the current user profile's\nAutoCAD Support Files Search Path\nand load the CNM menu."
    )
  )
  (VL-REGISTRY-WRITE
    "HKEY_LOCAL_MACHINE\\Software\\HawsEDC\\CNM"
    "ProgramFolder"
    PROGRAMFOLDER
  )
  (VL-REGISTRY-WRITE
    (STRCAT
      "HKEY_CURRENT_USER\\"
      (VLAX-PRODUCT-KEY)
      "\\Profiles\\"
      (GETVAR "CPROFILE")
      "\\General"
    )
    "ACAD"
    (STRCAT ACADPATHPREFIX PROGRAMFOLDER ";" ACADPATHSUFFIX)
  )
  (COMMAND "._menuunload" "cnm" "._menuload" "cnm")
  (VL-FILE-DELETE (STRCAT PROGRAMFOLDER "\\acaddoc.lsp"))
  (ALERT
    "Construction Notes Manager setup is done.\n\nYou may now explore the CNM menus and toolbar\nafter restarting AutoCAD."
  )
)
(DEFUN
   C:HAWS-NTPURGE (/ OL PL PLSS)
  (SETQ
    OL (GETVAR "clayer")
    PL (CAR (HAWS-GETLAYR "NOTESEXP"))
  )
  (COMMAND "erase" (SSGET "X" (LIST (CONS 8 PL))) "")
  (SETVAR "clayer" OL)
  (COMMAND "._purge" "_b" "noteqty*,cnm*" "_n")
  (IF (SETQ PLSS (SSGET "X" (LIST (CONS 8 PL))))
    (ALERT
      (STRCAT
        "All entities on the "
        PL
        " layer\nhave been erased from the current tab.\n\n"
        (ITOA (SSLENGTH PLSS))
        " objects were not in the current tab\nand must still be erased."
      )
    )
  )
  (PRINC)
)

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
(DEFUN
   C:HAWS-BOXL ()
  (HAWS-LDRBLK
    "noteboxl" "noteboxr" "noteboxd" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-CIRL ()
  (HAWS-LDRBLK
    "notecirl" "notecirr" "notecird" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-DIAL ()
  (HAWS-LDRBLK
    "notedial" "notediar" "notediad" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-ELLL ()
  (HAWS-LDRBLK
    "noteelll" "noteellr" "noteelld" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-HEXL ()
  (HAWS-LDRBLK
    "notehexl" "notehexr" "notehexd" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-OCTL ()
  (HAWS-LDRBLK
    "noteoctl" "noteoctr" "noteoctd" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-PENL ()
  (HAWS-LDRBLK
    "notepenl" "notepenr" "notepend" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-RECL ()
  (HAWS-LDRBLK
    "noterecl" "noterecr" "noterecd" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-SSTL ()
  (HAWS-LDRBLK
    "notesstl" "notesstr" "notesstd" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-TRIL ()
  (HAWS-LDRBLK
    "notetril" "notetrir" "notetrid" "NOTESLDR" "NotesLeader"
   )
)
(DEFUN
   C:HAWS-TCG ()
  (HAWS-LDRBLK
    "ldrtcgl" "ldrtcgr" "ldrtcgd" "TCGLDR" "TCGLeader"
   )
)
(DEFUN
   C:HAWS-TXTL ()
  (HAWS-LDRBLK
    "ldrtxtl" "ldrtxtr" "ldrtxtd" "NOTESLDR" "NotesLeader"
   )
)

(DEFUN
   HAWS-LDRBLK (BLLEFT BLRGHT BLDRAG BLLAY BLDSTY / APOLD AS ANG AUOLD
                BLGF BLLINE BLK BUBBLEHOOKS BUBBLELEADERCONNECTOSNAP
                DSTY DSTYOLD DTOLD EL EN ENBLK ENDRAG FIXHOOK FIXPHASE
                FIXTXT3 I P1 P2 P3 P4 P5 P6 P7 P8 PFOUND R1 DS TS LEFT
                NUM TXT1 TXT2 ANG1 ANG2 FIXORDER OSMOLD
               )
  (HAWS-ERDF$@ 1)
  (HAWS-VSAVE
    '("aperture" "attdia" "attreq" "aunits" "clayer" "cmdecho" "osmode"
      "plinegen" "regenmode"
     )
  )
  (COMMAND "._undo" "_g")
  (HCNM-SET-DIMSTYLE (STRCAT BLDSTY "Dimstyle"))
  (SETVAR "osmode" 0)
  (HCNM-PROJINIT)
  ;;Find out what bubble style to use
  (SETQ
    BUBBLEHOOKS
     (COND
       ((= (SUBSTR BLLEFT 1 3) "ldr") "1")
       ((= (HCNM-GETVAR "BubbleHooks") "0") "2")
       ("1")
     )
    BUBBLELEADERCONNECTOSNAP
     (HCNM-GETVAR "BubbleLeaderConnectOsnap")
    BLLEFT
     (STRCAT BLLEFT BUBBLEHOOKS)
    BLRGHT
     (STRCAT BLRGHT BUBBLEHOOKS)
    BLDRAG
     (STRCAT BLDRAG BUBBLEHOOKS)
  )
  (HAWS-MKLAYR BLLAY)
  (SETQ
    P1 (GETPOINT "\nStart point for leader:")
    DS (GETVAR "dimscale")
    TS (* DS (GETVAR "dimtxt"))
    AS (* DS (GETVAR "dimasz"))
  )
  ;;Redefine bubble note blocks if necessary to work with current routine
  (COND
    ((WCMATCH (STRCASE BLLEFT) "NOTE*")
     (FOREACH
        BL (LIST BLLEFT BLRGHT)
       (IF (NOT (TBLSEARCH "BLOCK" BL))
         (PROGN (COMMAND "._insert" BL) (COMMAND))
       )
       (SETQ
         EN (CDR (ASSOC -2 (TBLSEARCH "BLOCK" BL)))
         FIXPHASE T
         FIXORDER T
       )
       (WHILE EN
         (SETQ EL (ENTGET EN))
         (IF (AND
               (= "ATTDEF" (CDR (ASSOC 0 EL)))
               (= "NOTETXT6" (CDR (ASSOC 2 EL)))
             )
           (SETQ FIXORDER NIL)
         )
         (IF (AND
               (= "ATTDEF" (CDR (ASSOC 0 EL)))
               (= "NOTEPHASE" (CDR (ASSOC 2 EL)))
             )
           (SETQ FIXPHASE NIL)
         )
         (IF (AND
               (= "INSERT" (CDR (ASSOC 0 EL)))
               (= "NOTEHK" (SUBSTR (CDR (ASSOC 2 EL)) 1 6))
             )
           (SETQ FIXHOOK T)
         )
         (SETQ EN (ENTNEXT EN))
       )
       (IF (OR FIXHOOK FIXPHASE FIXTXT3 FIXORDER)
         (PROGN (COMMAND "._insert" (STRCAT BL "=")) (COMMAND))
       )
     )
    )
  )
  (COMMAND
    "._insert"
    BLDRAG
    "s"
    TS
    P1
    (ANGTOS (GETVAR "snapang"))
  )
  (SETQ EN (ENTLAST))
  (COND
    ((= BUBBLEHOOKS "2")
     (PROMPT "\nLocation for bubble: ")
     (COMMAND "._move" EN "" P1 PAUSE)
     (SETQ
       P2   (TRANS (CDR (ASSOC 10 (ENTGET EN))) EN 1)
       ANG1 (ANGLE P1 P2)
       R1   1.5
       I    -1
     )
     (WHILE (AND (NOT PFOUND) (< (SETQ I (1+ I)) 20))
       (SETQ
         P3 (POLAR
              P2
              (+ PI ANG1)
              (* TS (+ R1 (* 0.1 (- I (REM I 2)) (- (REM I 2) 0.5))))
            )
       )
       ;;Get candidate osnaps with block present
       (SETQ P4 (OSNAP P3 BUBBLELEADERCONNECTOSNAP))
       (HAWS-MKLINE P1 P2)
       (SETQ P5 (OSNAP P3 "int"))       ;int with block and line present
       ;;Get comparison int osnap with just block gone
       (ENTDEL EN)                      ;delete block
       (SETQ P6 (OSNAP P3 "int"))
       ;;Get comparison int osnap with just line gone
       (ENTDEL (ENTLAST))               ;delete line
       (ENTDEL EN)                      ;bring back block
       (SETQ P7 (OSNAP P3 "int"))
       ;;Get comparison multi osnap without either present
       (ENTDEL EN)                      ;delete block
       (SETQ P8 (OSNAP P3 BUBBLELEADERCONNECTOSNAP))
       (ENTDEL EN)                      ;bring back block
       ;;Decide whether there is a point worth using as endpoint of leader
       (SETQ
         PFOUND
          (NOT
            (OR (AND (NOT P4) (NOT P5)) ;neither candidate point found
                (AND P4 (EQUAL P8 P4))  ;multi found didn't depend on block
                (EQUAL P6 P5)           ;int didn't depend on block
                                        ;       (EQUAL P7 P5)  ;int didn't depend on line
            )
          )
       )
     )
     (SETQ
       P3     (COND
                ((AND PFOUND P4) P4)    ;Use multi if found good
                ((AND PFOUND P5) P5)    ;Use int otherwise if found good
                (T (POLAR P2 (+ PI ANG1) (* TS R1)))
              )
       ANG2   (ANGLE P1 P3)
       LEFT   (MINUSP (COS (- ANG1 (GETVAR "snapang"))))
       P4     (POLAR P1 ANG2 AS)
       ENDRAG (ENTLAST)
     )
     (SETVAR "attdia" 0)
     (SETVAR "attreq" 1)
     (COND
       ((>= (ATOF (GETVAR "acadver")) 14)
        (COMMAND "._leader" P1 P3 "" "" "_block")
       )
     )
     (SETQ AUOLD (GETVAR "aunits"))
     (SETVAR "aunits" 3)
     (COMMAND
       (IF LEFT
         BLLEFT
         BLRGHT
       )
       P2
       TS
       TS
       (GETVAR "snapang")
     )
     (SETVAR "aunits" AUOLD)
    )
    (T
     (PROMPT "\nEnd point for leader: ")
     (COMMAND "._move" EN "" P1 PAUSE)
     (SETQ
       P2     (TRANS (CDR (ASSOC 10 (ENTGET (ENTLAST)))) (ENTLAST) 1)
       ANG    (ANGLE P1 P2)
       LEFT   (MINUSP (COS (+ ANG (GETVAR "snapang"))))
       P3     (POLAR P1 ANG AS)
       P4     (POLAR
                P2
                (IF LEFT
                  PI
                  0
                )
                AS
              )
       P5     (POLAR P4 (/ PI 2) (* TS 3.0))
       P6     (POLAR
                (POLAR
                  P5
                  (IF LEFT
                    PI
                    0
                  )
                  (* TS 10)
                )
                (/ PI -2)
                (* TS 6.0)
              )
       ENDRAG (ENTLAST)
     )
     (COND
       ((WCMATCH (STRCASE BLLEFT) "NOTE*")
        (COMMAND
          "._insert"
          (IF LEFT
            "notehkl"
            "notehkr"
          )
          P2
          TS
          TS
          0
        )
       )
     )
     (SETVAR "attdia" 0)
     (SETVAR "attreq" 1)
     (COND
       ((>= (ATOF (GETVAR "acadver")) 14)
        (COMMAND "._leader" P1 P2 "" "" "_block")
       )
     )
     (SETQ AUOLD (GETVAR "aunits"))
     (SETVAR "aunits" 3)
     (COMMAND
       (IF LEFT
         BLLEFT
         BLRGHT
       )
       P2
       TS
       TS
       (GETVAR "snapang")
     )
     (SETVAR "aunits" AUOLD)
    )
  )
  (COND
    ((WCMATCH (STRCASE BLLEFT) "NOTE*")
     (SETQ
       NUM  (GETSTRING "\nNote number <XX>: ")
       TXT1 (GETSTRING 1 "Line 1 text: ")
       TXT2 (GETSTRING 1 "Line 2 text: ")
     )
     (COMMAND
       NUM
       (IF (OR (/= TXT1 "") (/= TXT2 ""))
         "%%u "
         ""
       )
       (IF (= TXT1 "")
         ""
         (STRCAT "%%u" TXT1)
       )
       (IF (= TXT2 "")
         ""
         (STRCAT "%%o" TXT2)
       )
     )
    )
  )
  (SETVAR "cmdecho" 1)
  (WHILE (= 1 (LOGAND (GETVAR "cmdactive") 1))
    (COMMAND PAUSE)
  )
  (SETVAR "cmdecho" 0)
  (COMMAND "._erase" ENDRAG "")
  (IF (NOT (ENTNEXT (ENTLAST)))
    (COMMAND
      "._explode"
      "_l"
      "._change"
      "_p"
      ""
      "_p"
      "_la"
      (GETVAR "clayer")
      ""
    )
  )
  (HCNM-RESTORE-DIMSTYLE)
  (HAWS-VRSTOR)
  (COMMAND "._undo" "_e")
  (HAWS-ERRRST)
  (IF (OR FIXHOOK FIXPHASE FIXTXT3 FIXORDER)
    (PROMPT
      (STRCAT
        "\nBecause "
        (COND
          (FIXHOOK
           "leader hook was inside (it should be out for user manipulation)"
          )
          (FIXPHASE "it didn't have a phase attribute")
          (FIXTXT3
           "the NOTETXT3 attribute was preset (flags should be normal)"
          )
          (FIXORDER
           "the attributes were assumed to be in the wrong order (old style)"
          )
        )
        ",\nbubble note block was redefined."
      )
    )
  )
  (PRINC)
)
;;end of LDRBLK

;;Playing with reactors
;;(defun hcnm-bubblesmoving ()
;;  ()
;;)
;;(defun hcnm-bubblesmoved ()
;;  ( )
;;)

(DEFUN
   C:HCNM-CNMOPTIONS (/ CNMDCL RETN LIST1)
  ;; Load Dialog
  (SETQ CNMDCL (LOAD_DIALOG "cnm.dcl"))
  (NEW_DIALOG "CNMOptions" CNMDCL)
  ;; Dialog Actions
  (SET_TILE "ProjectNotes" (HCNM-GETVAR "ProjectNotes"))
  (ACTION_TILE
    "ProjectNotes"
    "(HCNM-SETVARTEMP \"ProjectNotes\" (get_tile \"ProjectNotes\"))"
  )
  (ACTION_TILE
    "ProjectNotesBrowse"
    "(HCNM-SETVARTEMP \"ProjectNotes\"(HCNM-changeprojnotes))(set_tile \"ProjectNotes\" (hcnm-getvartemp \"ProjectNotes\"))"
  )
  (SET_TILE
    "ProjectNotesEditor"
    (HCNM-GETVAR "ProjectNotesEditor")
  )
  (ACTION_TILE
    "ProjectNotesEditor"
    "(HCNM-SETVARTEMP \"ProjectNotesEditor\" (HCNM-WRITENOTESEDITOR (get_tile \"ProjectNotesEditor\")))(set_tile \"ProjectNotesEditor\" (HCNM-GETVARTEMP \"ProjectNotesEditor\"))"
  )
  (ACTION_TILE
    "ProjectNotesEditorBrowse"
    "(HCNM-SETVARTEMP \"ProjectNotesEditor\"(HCNM-noteseditor))(set_tile \"ProjectNotesEditor\" (hcnm-getvartemp \"ProjectNotesEditor\"))"
  )
  (SET_TILE
    "LayersEditor"
    (HCNM-GETVAR "LayersEditor")
  )
  (ACTION_TILE
    "ProjectNotesEditor"
    "(HCNM-SETVARTEMP \"LayersEditor\" (HCNM-WRITELAYEREDITOR (get_tile \"LayersEditor\")))(set_tile \"LayersEditor\" (HCNM-GETVARTEMP \"LayersEditor\"))"
  )
  (ACTION_TILE
    "LayersEditorBrowse"
    "(HCNM-SETVARTEMP \"LayersEditor\"(HCNM-layereditor))(set_tile \"LayersEditor\" (hcnm-getvartemp \"LayersEditor\"))"
  )
  (HCNM-VAR-SET-ACTION-TILE "NoteTypes")
  (SETQ LIST1 '("No" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
  (HAWS-SET_TILE_LIST
    "InsertTablePhases"
    LIST1
    (HCNM-GETVAR "InsertTablePhases")
  )
  (ACTION_TILE
    "InsertTablePhases"
    "(HCNM-SETVARTEMP \"InsertTablePhases\" (nth (read $value) list1))"
  )
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias1")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias2")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias3")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias4")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias5")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias6")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias7")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias8")
  (HCNM-VAR-SET-ACTION-TILE "PhaseAlias9")
  (HCNM-VAR-SET-ACTION-TILE "BubbleHooks")
  (HCNM-VAR-SET-ACTION-TILE "DoCurrentTabOnly")
  (HCNM-VAR-SET-ACTION-TILE "BubbleLeaderConnectOsnap")
  (HCNM-VAR-SET-ACTION-TILE "LineSpacing")
  (HCNM-VAR-SET-ACTION-TILE "NoteSpacing")
  (HCNM-VAR-SET-ACTION-TILE "ShowKeyTableQuantities")
  (HCNM-VAR-SET-ACTION-TILE "ShowKeyTableGrid")
  (HCNM-VAR-SET-ACTION-TILE "TableWidth")
  (HCNM-VAR-SET-ACTION-TILE "PhaseWidthAdd")
  (HCNM-VAR-SET-ACTION-TILE "NumberToDescriptionWidth")
  (HCNM-VAR-SET-ACTION-TILE "DescriptionToQuantityWidth")
  (HCNM-VAR-SET-ACTION-TILE "QuantityToQuantityWidth")
  (HCNM-VAR-SET-ACTION-TILE "QuantityToUnitsWidth")
  (ACTION_TILE "accept" "(done_dialog 1)")
  (ACTION_TILE "cancel" "(done_dialog 0)")
  (SETQ RETN (START_DIALOG))
  (UNLOAD_DIALOG CNMDCL)
  (COND ((= RETN 1) (HCNM-SAVETEMPSETTINGS)))
  (PRINC)
)

(DEFUN
   HCNM-VAR-SET-ACTION-TILE (VAR)
  (SET_TILE VAR (HCNM-GETVAR VAR))
  (ACTION_TILE
    VAR
    (STRCAT "(hcnm-setvartemp \"" VAR "\" $value)")
  )
)

(LOAD "ini-edit")
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
