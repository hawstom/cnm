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
(DEFUN HCNM_GETPHASELISTFROMTBLQTY (/ EL EN I DSCTAG NOTEQTYONDISK OLDTAGS
                                PHASEALIAS PHASELIST
                               )
  ;;Check for phasing in qty table block.  Phasing is controlled by presence of TBLQTY? attributes.
  ;;Construct phaselist as '((phase1 1 alias1)(phase2 2 alias2)(phasei i aliasi)).
  ;;Alias=phase for changing later by CNM.INI.
  ;;Phases are numbered in order they appear in block. (This number could be very unstable, but it is the key to phase order on this sheet.)
  ;;
  ;;Insert table line NOTEQTY block if not exist
  (SETQ
    J (IF (= (c:hcnm-config-getvar "InsertTablePhases") "No")
        ""
        (c:hcnm-config-getvar "InsertTablePhases")
      )
  )
  (COND
    ((NOT (TBLSEARCH "BLOCK" "NOTEQTY"))
     (setvar "attreq" 0)
     (VL-CMDF "._insert" (STRCAT "NOTEQTY=NOTEQTY" J) "_Scale" "1" "_Rotate" "0" "0,0")
     (setvar "attreq" 1)
     (entdel (entlast))
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
     (VL-CMDF "._insert" (STRCAT "noteqty=noteqty" (ITOA J)))
     (VL-CMDF)
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
  ;;Add phasealias configs to the phaselist.
  (MAPCAR
    '(LAMBDA (PHASE)
       (SETQ
         PHASELIST
          (SUBST
            (REVERSE
              (CONS
                (c:hcnm-config-getvar (CADR PHASE))
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
     (VL-CMDF
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
     (VL-CMDF)
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
(DEFUN HCNM_KEY_TABLE_SEARCHANDSAVE (DN PROJNOTES / ALIASLIST AT ATTRIBUTES AV BLKI
                                   BLKSS COUNT CTABONLY EL EN ET I J
                                   MVPORT MVSSET N NFNAME NOTEFND NOTEI
                                   NOTELINES NOTELIST NOTENUM NOTEPHASE
                                   NOTEQTY NOTETXT NOTETYPE NOTNUM
                                   NOTTYP PHASE PHASELIST QTYOPT
                                   SKIPPEDPHASES USRVAR VPLAYERS X
                                  )
  ;;
  ;; Section 1.  Make an empty NOTELIST from tblqty and constnot.txt.  TGHI can use this section for Tally, except there is a conflict in the way they do PHASELIST.
  ;;
  (SETQ
    PHASELIST
     (HCNM_GETPHASELISTFROMTBLQTY)
    CTABONLY
     (= "1" (c:hcnm-config-getvar "DoCurrentTabOnly"))
    NOTTYP ""
  )
  (FOREACH
     ENTRY *HCNM_CNMPROJECTNOTES*
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
          (LENGTH (NTH 6 ENTRY))
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
  ;;
  ;; Section 2.  Get quantities from bubble notes and save to file
  ;;
  ;; Make a list of all layers frozen in current viewport.
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
      NOTETYPE
       (COND
         ((LM:GETDYNPROPVALUE
            (VLAX-ENAME->VLA-OBJECT EN)
            "Shape"
          )
         )
         (T NIL)
       )
      NOTENUM NIL
      NOTETXT
       '(0 1 2 3 4 5 6 7 8 9)
      NOTEPHASE ""
      ATTRIBUTES (HCNM_GET_ATTRIBUTES EN NIL)
    )
    ;;Substitute the value of each NOTETXT attribute for its respective member of the pre-filled NOTETXT list.
    (SETQ
      NOTENUM
       (CADR (ASSOC "NOTENUM" ATTRIBUTES))
      NOTETXT
       (MAPCAR
         '(LAMBDA (I)
            (CADR (ASSOC (STRCAT "NOTETXT" (ITOA I)) ATTRIBUTES))
          )
         '(0 1 2 3 4 5 6 7 8 9)
       )
      NOTEPHASE
       (CADR (ASSOC "NOTEPHASE" ATTRIBUTES))
      NOTEI
       (ASSOC NOTENUM (CDR (ASSOC NOTETYPE (CADR NOTELIST))))
    )
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
(DEFUN HCNM_KEY_TABLE_MAKE (NFSOURCE QTYPT QTYSET DN TXTHT / CTABONLY ICOL
                        IPHASE COLUMN_HEIGHT NOTE_FIRST_LINE_P
                        COLUMN_HEIGHT_PENDING NFNAME NOTDSC NOTELIST
                        NOTESMAXHEIGHT NOTETITLES NOTNUM NOTQTY NOTTYP
                        NOTUNT NUMFND PHASELIST PROMPTEACHCOL QTY QTYPT1
                        RDLIN TXTHTTEMP TYPFND USRVAR
                       )
  (SETQ PHASELIST (HCNM_GETPHASELISTFROMTBLQTY))
  (SETVAR "osmode" 0)
  (SETVAR "attreq" 1)
  (INITGET "Prompt")
  (SETQ
    NOTESMAXHEIGHT
     (HAWS-GETDISTX
       QTYPT
       "Maximum height of each notes column"
                                        ; or [Prompt for each column]"
       NOTESMAXHEIGHT
       9999.0
     )
  )
  (COND
    ((= NOTESMAXHEIGHT "Prompt")
     (SETQ
       PROMPTEACHCOL T
       NOTESMAXHEIGHT 9999.0
     )
     (ALERT
       "The option to prompt for each column is not yet operational."
     )
    )
  )
  (SETQ CTABONLY (= (C:HCNM-CONFIG-GETVAR "DoCurrentTabOnly") "1"))
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
  (HCNM_PROJINIT)                       ;Initialize project after user pauses
  (HCNM_READCF (HCNM_PROJNOTES))
  (SETQ
    LINSPC
     (ATOF (C:HCNM-CONFIG-GETVAR "LineSpacing"))
    NOTSPC
     (ATOF (C:HCNM-CONFIG-GETVAR "NoteSpacing"))
    TBLWID
     (ATOF (C:HCNM-CONFIG-GETVAR "TableWidth"))
    PHASEWID
     (ATOF (C:HCNM-CONFIG-GETVAR "PhaseWidthAdd"))
    ICOL 1
    COLUMN_HEIGHT 0
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
      (VL-CMDF
        "._insert"
        (STRCAT
          "noteqty=noteqty"
          (CAR (NTH NOTELIST (1- (LENGTH NOTELIST))))
        )
      )
      (VL-CMDF)
    )
  )
  (VL-CMDF "._undo" "_group")
  (IF QTYSET
    (VL-CMDF "._erase" QTYSET "")
  )
  (FOREACH
     ENTRY *HCNM_CNMPROJECTNOTES*
    (COND
      ;;If it's a variable config, set it.
      ((= 1 (CAR ENTRY))
       (SETQ USRVAR (CADR ENTRY))
       (COND
         ((AND (= "TXTHT" USRVAR) (SETQ USRVAR (CADDR ENTRY)))
          (SETQ
            TXTHT
             (* (HAWS-DWGSCALE)
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
      ;;
      ;; STYLE GUIDE
      ;; LINSPC is the spacing/height of a line from its own top to bottom.
      ;; NOTSPC is the spacing above each note or group of titles.
      ;; (- NOTSPC LINSPC) is the additional space above a note or group of titles.
      ;; Insertion point is vertically (y coordinate) at the middle of each line.
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
         COLUMN_HEIGHT_PENDING 0
         NOTUNT
          (CADDDR ENTRY)
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
       ;; Calculate height of titles plus a paragraph space
       (COND
         (NOTETITLES
          (SETQ
            TXTHTTEMP TXTHT
            I_TITLE 0
          )
          (FOREACH
             NOTETITLE (REVERSE NOTETITLES)
            (SETQ TXTHT (CAR NOTETITLE))
            (COND
              ((= I_TITLE 0)
               (COND
                 ;; At top, rewind before first title
                 ((= COLUMN_HEIGHT 0)
                  (SETQ
                    COLUMN_HEIGHT_PENDING
                     (+ COLUMN_HEIGHT_PENDING
                        (* -0.5 TXTHT LINSPC)
                     )
                  )
                 )
                 ;; Else add a paragraph space before first title
                 (T
                  (SETQ
                    COLUMN_HEIGHT_PENDING
                     (+ COLUMN_HEIGHT_PENDING
                        (* TXTHT (- NOTSPC LINSPC))
                     )
                  )
                 )
               )
              )
            )
            ;; Space for each title
            (SETQ
              COLUMN_HEIGHT_PENDING
               (+ COLUMN_HEIGHT_PENDING
                  (* TXTHT LINSPC)
               )
              I_TITLE
               (1+ I_TITLE)
            )
          )
          (SETQ TXTHT TXTHTTEMP)
         )
       )
       ;; Calculate height of note
       (COND
         ;; At top, rewind before note
         ((AND (NOT NOTETITLES) (= COLUMN_HEIGHT 0))
          (SETQ
            COLUMN_HEIGHT_PENDING
             (+ COLUMN_HEIGHT_PENDING
                (* -0.5 TXTHT LINSPC)
             )
          )
         )
         ;; Else add a paragraph space before note
         (T
          (SETQ
            COLUMN_HEIGHT_PENDING
             (+ COLUMN_HEIGHT_PENDING
                (* TXTHT (- NOTSPC LINSPC))
             )
          )
         )
       )
       (SETQ
         COLUMN_HEIGHT_PENDING
          ;; Add note height
          (+ COLUMN_HEIGHT_PENDING
             (* (CADR NOTNUM) (* TXTHT LINSPC))
          )
       )
       ;; Add titles and note
       ;; If titles _and note_ won't fit and column isn't empty, advance to new column
       (COND
         ((AND
            (> (+ COLUMN_HEIGHT_PENDING COLUMN_HEIGHT) NOTESMAXHEIGHT)
                                        ; Won't fit
            (/= COLUMN_HEIGHT 0)        ; Not first note in column
          )
          (HCNM_KEY_TABLE_ADVANCE_COLUMN)
         )
       )
       ;; Add any titles
       (COND
         (NOTETITLES
          (SETQ
            TXTHTTEMP TXTHT
            I_TITLE 0
          )
          (FOREACH
             NOTETITLE (REVERSE NOTETITLES)
            (SETQ TXTHT (CAR NOTETITLE))
            ;; If not first note, space appropriately
            (COND
              ((/= COLUMN_HEIGHT 0)     ; Not first note in column
               (COND
                 ;; Add a paragraph space above first title based on its height
                 ((= I_TITLE 0)
                  (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 (- NOTSPC LINSPC)))
                 )
               )
               (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 LINSPC))
              )
            )
            (COND
              ((= (C:HCNM-CONFIG-GETVAR "ShowKeyTableTitleShapes") "1")
               (HCNM_KEY_TABLE_INSERT_SHAPE)
              )
            )
            (SETQ NOTDSC (CADR NOTETITLE))
            (HCNM_KEY_TABLE_INSERT_TEXT)
            (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 LINSPC))
          )
          (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 (- NOTSPC LINSPC)))
          (SETQ TXTHT TXTHTTEMP)
         )
       )
       ;; If note won't fit in new column with titles, advance column again.
       (COND
         (;; Titles were added
          (/= COLUMN_HEIGHT 0)
          (COND
            (;; Note won't fit after titles.
             (> (+ COLUMN_HEIGHT_PENDING COLUMN_HEIGHT) NOTESMAXHEIGHT)
             (HCNM_KEY_TABLE_ADVANCE_COLUMN)
            )
            (;; Note will fit after titles.
             T
             ;; Paragraph spacing
             (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 (- NOTSPC LINSPC)))
             ;; Down to middle of first note
             (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 LINSPC))
            )
          )
         )
       )
       ;; Now add note
       (SETQ NOTE_FIRST_LINE_P T)
       (HCNM_KEY_TABLE_INSERT_SHAPE)
       (HCNM_KEY_TABLE_ADVANCE_DOWN (* -0.5 LINSPC))
       (FOREACH
          NOTDSC (NTH 6 ENTRY)
         (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 LINSPC))
         (HCNM_KEY_TABLE_INSERT_TEXT)
         (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 LINSPC))
         (SETQ
           NOTETITLES NIL
           NOTE_FIRST_LINE_P NIL
         )
       )
       (HCNM_KEY_TABLE_ADVANCE_DOWN (* 0.5 (- NOTSPC LINSPC)))
      )
    )
  )
  ;;Apply table display configs from ini.  If no configs (legacy), show both.
  (MAPCAR
    '(LAMBDA (LAYERKEY / LAYERSHOW LAYERLIST)
       (SETQ LAYERSHOW (/= "0" (C:HCNM-CONFIG-GETVAR (CADR LAYERKEY))))
       (COND
         (LAYERSHOW (HAWS-MKLAYR (CAR LAYERKEY)))
         (T
          (SETQ
            LAYERLIST
             (TBLSEARCH
               "LAYER"
               (CAR (HAWS-GETLAYR (CAR LAYERKEY)))
             )
          )
          ;; If thawed and on, freeze
          (IF (AND
                (CDR (ASSOC 70 LAYERLIST))
                (/= 1 (LOGAND 1 (CDR (ASSOC 70 LAYERLIST))))
                (< 0 (CDR (ASSOC 62 LAYERLIST)))
              )
	            (VL-CMDF "._layer" "_f" (CDR (ASSOC 2 LAYERLIST)) "")
          )
         )
       )
     )
    '(("NOTESKEYGRID" "ShowKeyTableGrid")
      ("NOTESKEYQTYS" "ShowKeyTableQuantities")
     )
  )
  (VL-CMDF "._undo" "_end")
)




(DEFUN HCNM_KEY_TABLE_ADVANCE_COLUMN ()
  (SETQ
    COLUMN_HEIGHT 0
    ICOL
     (1+ ICOL)
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

(DEFUN HCNM_KEY_TABLE_ADVANCE_DOWN (SPACE / DOWN_HEIGHT)
  (SETQ
    DOWN_HEIGHT (* SPACE TXTHT)
    QTYPT
     (POLAR QTYPT (* PI -0.5) DOWN_HEIGHT)
    COLUMN_HEIGHT
     (+ COLUMN_HEIGHT DOWN_HEIGHT)
    COLUMN_HEIGHT_PENDING
     (- COLUMN_HEIGHT_PENDING DOWN_HEIGHT)
  )
)

(DEFUN HCNM_KEY_TABLE_INSERT_SHAPE ()
  (VL-CMDF
    "._insert"
    (STRCAT "cnm" NOTTYP)
    "_Scale"
    TXTHT
    "_Rotate"
    "0"
    QTYPT
  )
)

(DEFUN HCNM_KEY_TABLE_INSERT_TEXT ()
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
  (VL-CMDF
    "._insert"
    "NOTEQTY"
    "_Scale"
    TXTHT
    "_Rotate"
    "0"
    QTYPT
    (IF NOTE_FIRST_LINE_P
      NOTTYP
      ""
    )
    (IF NOTE_FIRST_LINE_P
      (CAR NOTNUM)
      ""
    )
    NOTDSC
  )
  (FOREACH
     X NOTQTY
    (VL-CMDF
      (IF NOTE_FIRST_LINE_P
        X
        ""
      )
    )
  )
  (VL-CMDF
    (IF NOTE_FIRST_LINE_P
      NOTUNT
      ""
    )
  )
)

;;HCNM_KEY_TABLE_FROM_SEARCH
;;In the NOTES strategy, this routine is first of three main routines.
;;Gets project info from CONSTNOT.TXT
;;Gets drawing info from bubbles or table.
;;Saves all in .NOT file for other two routines
(DEFUN HCNM_KEY_TABLE_FROM_SEARCH (DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID /
                           EL EN I NOTELIST QTYPT QTYSET TABLESPACE
                          )
  (SETQ
    QTYSET
     (SSGET "X" (LIST (CONS 8 (CAR (HAWS-MKLAYR "NOTESEXP")))))
  )
  (COND
    (QTYSET
     (SETQ
       I (IF (c:haws-icad-p)
           1
           (SSLENGTH QTYSET)
         )
     )
     (WHILE (SETQ EN (SSNAME QTYSET (SETQ I (1- I))))
       (SETQ EL (ENTGET EN))
       (COND
         ((OR (= (GETVAR "CTAB") (SETQ TABLESPACE (CDR (ASSOC 410 EL))))
              (AND (= "Model" TABLESPACE) (< 1 (GETVAR "cvport")))
              (c:haws-icad-p)             ;If we are in intellicad, which doesn't have the tab information in the entget data
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
  (HCNM_KEY_TABLE_SEARCHANDSAVE DN PROJNOTES)
  ;;Make a new notes table
  (HCNM_KEY_TABLE_MAKE "E" QTYPT QTYSET DN TXTHT)
)
;#endregion
;#region Table from import
;;HCNM_IMPORT
;;In the NOTES strategy, this routine is second of three main routines.
;;Reads from .NOT file, created by HCNM_KEY_TABLE_FROM_SEARCH, everything necessary and creates a table. 
(DEFUN HCNM_IMPORT (DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID / EL EN I QTYPT
               QTYSET TABLESPACE
              )
  (SETQ
    QTYSET
     (SSGET "X" (LIST (CONS 8 (CAR (HAWS-MKLAYR "NOTESIMP")))))
  )
  (COND
    (QTYSET
     (SETQ
       I (IF (c:haws-icad-p)
           1
           (SSLENGTH QTYSET)
         )
     )
     (WHILE (SETQ EN (SSNAME QTYSET (SETQ I (1- I))))
       (SETQ EL (ENTGET EN))
       (COND
         ((OR (= (GETVAR "CTAB") (SETQ TABLESPACE (CDR (ASSOC 410 EL))))
              (AND (= "Model" TABLESPACE) (< 1 (GETVAR "cvport")))
              (c:haws-icad-p)             ;If we are in intellicad, which doesn't have the tab information in the entget data
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
  (HCNM_KEY_TABLE_MAKE "I" QTYPT QTYSET DN TXTHT)
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
(DEFUN HCNM_TALLY (DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID / ALLNOT
                  ALL_SHEETS_QUANTITIES COL1X COLUMN DQWID EL
                  FLSPEC I INPUT NDWID NOTDESC NOTETITLES NOTE_FIRST_LINE_P
                  NOTNUM NOTPRICE NOTQTY NOTSPC NOTTYP NOTUNT NUMFND
                  NUMLIST PGP_DEFINES_RUN PGP_FILENAME PGP_FILE_CONTENTS
                  PGP_FILE_LINE PHASE PHASENUMI PHASES_DEFINITION PT1Z Q
                  QQWID QTYPT1 QTYSET QUWID ROW1Y SHEET_FILENAME
                  SHEET_FILENAMES SHEET_FILE_NAME SHEET_HEADINGS SHEET_LIST_FILENAME
                  SHEET_LIST_LINE SHEET_QUANTITIES TABLESPACE TOTAL
                  TXTHTTEMP USRVAR WRITELIST X Y Z
                 )
;;;
;;;  Section 1.
;;;  Determine list of drawings to tally.
;;;
  (COND
    ((AND
       (OR (SETQ SHEET_LIST_FILENAME (FINDFILE (STRCAT DN ".lst")))
           (SETQ
             SHEET_LIST_FILENAME
              (FINDFILE (STRCAT (GETVAR "DWGPREFIX") "tally.lst"))
           )
       )
       (= "Yes"
          (PROGN
            (INITGET 1 "Yes No")
            (GETKWORD
              (STRCAT
                "\nKeep and use existing list file, \""
                SHEET_LIST_FILENAME
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
       INPUT
        (GETKWORD "\n[List file/Wildcards/Select one at a time]: ")
     )
     (COND
       ((= INPUT "List")
        (SETQ SHEET_LIST_FILENAME (GETFILED "Select a List File" DN "LST" 0))
       )
       ((= INPUT "Wildcards")
        ;;Add function to user's ACAD.PGP to shell and wait for attrib command to finish.
        (SETQ
          SHEET_LIST_FILENAME
           (STRCAT DN ".lst")
          PGP_FILENAME
           (FINDFILE "acad.pgp")
          F1 (OPEN PGP_FILENAME "r")
        )
        (WHILE (SETQ PGP_FILE_LINE (READ-LINE F1))
          (IF (= "RUN," (SUBSTR PGP_FILE_LINE 1 4))
            (SETQ PGP_DEFINES_RUN T)
          )
          (IF (= "SH," (SUBSTR PGP_FILE_LINE 1 3))
            (SETQ
              PGP_FILE_CONTENTS
               (CONS
                 "RUN,       cmd /c,         0,*Batch file to run: ,"
                 PGP_FILE_CONTENTS
               )
            )
          )
          (SETQ PGP_FILE_CONTENTS (CONS PGP_FILE_LINE PGP_FILE_CONTENTS))
        )
        (SETQ F1 (CLOSE F1))
        (IF (NOT PGP_DEFINES_RUN)
          (PROGN
            (SETQ
              F1     (OPEN PGP_FILENAME "w")
              PGP_FILE_CONTENTS (REVERSE PGP_FILE_CONTENTS)
            )
            (FOREACH PGP_FILE_LINE PGP_FILE_CONTENTS (WRITE-LINE PGP_FILE_LINE F1))
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
          (VL-CMDF
            "run"
            (STRCAT "attrib \"" FLSPEC ".not\" > \"" SHEET_LIST_FILENAME "\"")
          )
          (SETQ
            F1 (OPEN SHEET_LIST_FILENAME "r")
            SHEET_FILENAME
             (READ-LINE F1)
            COLUMN
             (STRLEN SHEET_FILENAME)
          )
          (COND
            ((WCMATCH SHEET_FILENAME "* not found *")
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
                          (STRCASE (SUBSTR SHEET_FILENAME COLUMN))
                          (STRCASE (STRCAT FLSPEC "`.NOT"))
                        )
                        (OR (= "\\" (SUBSTR SHEET_FILENAME (1- COLUMN) 1))
                            (= "\\" (SUBSTR SHEET_FILENAME COLUMN 1))
                            (= ":" (SUBSTR SHEET_FILENAME (1+ COLUMN) 1))
                        )
                      )
                    )
               (SETQ COLUMN (1- COLUMN))
             )
             (SETQ
               F1     (CLOSE F1)
               F1     (OPEN SHEET_LIST_FILENAME "r")
               SHEET_FILENAMES NIL
             )
             (WHILE (SETQ SHEET_FILENAME (READ-LINE F1))
               (SETQ
                 SHEET_FILENAME
                  (SUBSTR SHEET_FILENAME COLUMN)
                 SHEET_FILENAMES
                  (CONS
                    (SUBSTR SHEET_FILENAME 1 (- (STRLEN SHEET_FILENAME) 4))
                    SHEET_FILENAMES
                  )
               )
             )
             (SETQ
               F1 (CLOSE F1)
               F1 (OPEN SHEET_LIST_FILENAME "w")
             )
             (SETQ SHEET_FILENAMES (REVERSE SHEET_FILENAMES))
             (FOREACH SHEET_FILENAME SHEET_FILENAMES (WRITE-LINE SHEET_FILENAME F1))
             (SETQ F1 (CLOSE F1))
            )
          )
        )
       )
       ((= INPUT "Select")
        (SETQ
          SHEET_LIST_FILENAME
           (STRCAT DN ".lst")
          F1 (OPEN SHEET_LIST_FILENAME "w")
        )
        (WHILE (SETQ
                 SHEET_FILENAME
                  (GETFILED
                    "File to tally (Cancel when Finished)"
                    ""
                    "NOT"
                    6
                  )
               )
          (WRITE-LINE (SUBSTR SHEET_FILENAME 1 (- (STRLEN SHEET_FILENAME) 4)) F1)
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
    PHASES_DEFINITION
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
;;;  Read all .NOT's into a master ALL_SHEETS_QUANTITIES
;;;  Add phases from all .NOTs to the list if not already there.  And if aliases in conflict, alert user.
;;;  
  (SETQ F1 (OPEN SHEET_LIST_FILENAME "r"))
  (PRINC "\n")
  (WHILE (AND (SETQ SHEET_LIST_LINE (READ-LINE F1)) (/= "" SHEET_LIST_LINE))
    ;;Read in this sheet's notelist '( ((alias number phase)) ((type1 (notenum txtlines countmethod qty1...))))
    ;;Alert user of possible incompatibility with old-style list.
    (SETQ
      SHEET_FILE_NAME
       (COND
         ((FINDFILE SHEET_LIST_LINE))
         ((FINDFILE (STRCAT SHEET_LIST_LINE ".not")))
         (T
          (ALERT
            (PRINC
              (STRCAT
                "The file \"" SHEET_LIST_LINE "\" listed in \"" SHEET_LIST_FILENAME
                "\" cannot be found.\nConstruction Notes Manager cannot continue."
               )
            )
          )
         )
       )
      F2 (OPEN SHEET_FILE_NAME "r")
      SHEET_QUANTITIES
       (READ (READ-LINE F2))
      ALL_SHEETS_QUANTITIES
       (CONS (CONS SHEET_FILE_NAME SHEET_QUANTITIES) ALL_SHEETS_QUANTITIES)
    )
    (IF (READ-LINE F2)
      (ALERT
        (PRINC
          (STRCAT
            "Error:  Sheet quantities file for "
            SHEET_FILE_NAME
            " is out of date.\nPlease search and save quantities again."
          )
        )
      )
    )
    (SETQ F2 (CLOSE F2))
    ;;Set all phases discovered.
    ;;In .NOT files, phases are ("alias" order "number"), but here they are ("number" order "alias")
    (FOREACH
       PHASE (CAR SHEET_QUANTITIES)
      (COND
        ;;If its alias is not yet in PHASES_DEFINITION, add the phase.
        ;;The reason we substitute instead of just adding the
        ;;new phases as they come is to avoid sorting the list when we're done.
        ((NOT (CADDR (ASSOC (CADDR PHASE) PHASES_DEFINITION)))
         (SETQ
           PHASES_DEFINITION
            (SUBST
              ;;Substitute the alias for the nil.
              (SUBST
                (CAR PHASE)
                NIL
                (ASSOC (CADDR PHASE) PHASES_DEFINITION)
              )
              (ASSOC (CADDR PHASE) PHASES_DEFINITION)
              PHASES_DEFINITION
            )
         )
        )
        ;;If alias in PHASES_DEFINITION isn't same as alias in this sheet, alert user.
        ((/= (CADDR (ASSOC (CAR PHASE) PHASES_DEFINITION)) (CADDR PHASE))
         (ALERT
           (PRINC
             (STRCAT
               SHEET_QUANTITIES
               " is trying to assign alias \""
               (CADDR PHASE)
               "\" to phase \""
               (CAR PHASE)
               "\", which already has alias \""
               (CADDR (ASSOC (CAR PHASE) PHASES_DEFINITION))
               "\".\n\nGrouping alias \""
               (CADDR PHASE)
               "\" on this sheet with phase \""
               (CAR PHASE)
               "\", alias \""
               (CADDR (ASSOC (CAR PHASE) PHASES_DEFINITION))
               "."
             )
           )
         )
        )
      )
    )
  )
  (SETQ F1 (CLOSE F1))
  ;;Condense list to standard PHASES_DEFINITION format: '((phasej j aliasj)...)
  ;;and renumber for only sheets being tallied.
  (SETQ I 0)
  (FOREACH
     PHASE PHASES_DEFINITION
    (IF (CADDR PHASE)
      (SETQ X (CONS (LIST (CAR PHASE) (SETQ I (1+ I)) (CADDR PHASE)) X))
    )
  )
  (SETQ PHASES_DEFINITION (REVERSE X))
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
                   (IF (c:haws-icad-p)
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
              (c:haws-icad-p)
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
  (HCNM_PROJINIT)                       ;Initialize project after user pauses
  (HCNM_READCF (HCNM_PROJNOTES))
  (SETQ
    LINSPC
     (ATOF (c:hcnm-config-getvar "LineSpacing"))
    NOTSPC
     (ATOF (c:hcnm-config-getvar "NoteSpacing"))
    TBLWID
     (ATOF (c:hcnm-config-getvar "TableWidth"))
    PHASEWID
     (ATOF (c:hcnm-config-getvar "PhaseWidthAdd"))
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
     (ATOF (c:hcnm-config-getvar "NumberToDescriptionWidth"))
    ;;width from left point of description text to right point of quantity
    DQWID
     (ATOF (c:hcnm-config-getvar "DescriptionToQuantityWidth"))
    ;;width from right point of one quantity phase to right point of next quantity phase
    QQWID
     (ATOF (c:hcnm-config-getvar "QuantityToQuantityWidth"))
    ;;width from right point of quantity to left point of unit
    QUWID
     (ATOF (c:hcnm-config-getvar "QuantityToUnitsWidth"))
  )
  (SETVAR "osmode" 0)
  ;;Write column headings to the file
  (SETQ F2 (HAWS-FILE-OPEN (STRCAT DN ".csv") "w"))
  (PRINC "TYPE,NO,ITEM,UNIT,PRICE," F2) ;; Price and cost
  (SETQ SHEET_HEADINGS "")
  (FOREACH
     SHEET_QUANTITIES ALL_SHEETS_QUANTITIES
    (FOREACH
       PHASE PHASES_DEFINITION
      (SETQ
        SHEET_HEADINGS
         (STRCAT
           SHEET_HEADINGS
           (HAWS-MKFLD
             (STRCAT
               (STRCASE (CAR SHEET_QUANTITIES))
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
  (PRINC SHEET_HEADINGS F2)
  (FOREACH
     PHASE PHASES_DEFINITION
    (PRINC
      (STRCAT
        "TOTAL"
        (IF (= (CAR PHASE) "")
          " (SINGLE PHASE)"
          " PHASE "
        )
        (CADDR PHASE)
        ",COST"
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
    (VL-CMDF "._erase" QTYSET "")
  )
  ;;For each line in project file
  (FOREACH
     ENTRY *HCNM_CNMPROJECTNOTES*
    (COND
      ;;If it's a config setting, set it.
      ((= 1 (CAR ENTRY))
       (SETQ USRVAR (CADR ENTRY))
       (COND
         ((AND (= "TXTHT" USRVAR) (SETQ USRVAR (CADDR ENTRY)))
          (SETQ
            TXTHT
             (* (HAWS-DWGSCALE)
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
           ;; Price and cost
           NOTPRICE
            (NTH 5 ENTRY)
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
                   SHEET_QUANTITIES ALL_SHEETS_QUANTITIES
                  (FOREACH
                     PHASEI (CDDDR
                              (ASSOC
                                NOTNUM
                                (CDR (ASSOC NOTTYP (CADDR SHEET_QUANTITIES)))
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
            Y     (- Y (* TXTHT (- NOTSPC LINSPC)))
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
       (VL-CMDF
         "._insert"
         (STRCAT "cnm" NOTTYP)
         "_Scale"
         TXTHT
         "_Rotate"
         "0"
         (LIST X Y Z)
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
          (LIST NOTTYP NOTNUM (NTH 6 ENTRY) NOTUNT NOTPRICE)
         ;;Initialize running totals for each phase '(qty price)
         NOTQTY
          (MAPCAR '(LAMBDA (X) (LIST 0 0)) PHASES_DEFINITION)
       )
       (FOREACH
          SHEET_QUANTITIES ALL_SHEETS_QUANTITIES
         (SETQ
           NOTQTY
            (MAPCAR
              '(LAMBDA (X)
                 (SETQ
                   TOTAL
                    (CAR (NTH (1- (CADR X)) NOTQTY))
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
                                (CADR SHEET_QUANTITIES)
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
                                  (CADDR SHEET_QUANTITIES)
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
                 (LIST TOTAL (* TOTAL (ATOF NOTPRICE))) ;Price and cost 2020-12
               )
              PHASES_DEFINITION
            )
         )
       )
       ;;convert quantities and costs to strings, preserving quantities input precision.
       (SETQ
         NOTQTY ; List of qty and price for each phase.
          (MAPCAR
            '(LAMBDA (PHASE / QTY_STRING)
               (SETQ
                 QTY_STRING
                  (RTOS (CAR (NTH (1- (CADR PHASE)) NOTQTY)) 2 8) ;Price and cost 2020-12
               )
               (WHILE (WCMATCH QTY_STRING "*.*0,*.")
                 (SETQ QTY_STRING (SUBSTR QTY_STRING 1 (1- (STRLEN QTY_STRING))))
               )
               (LIST QTY_STRING (RTOS (CADR (NTH (1- (CADR PHASE)) NOTQTY)) 2 2)) ;Price and cost 2020-12
             )
            PHASES_DEFINITION
          )
       )
       ;;Print totals to drawing and file.
       (MAPCAR
         '(LAMBDA (PHASE)
            (SETQ X (+ X (* TXTHT QQWID)))
            ;; Quantity total for phase
            (HAWS-MKTEXT
              "MR"
              (LIST X Y Z)
              TXTHT
              0
              (CAR (NTH (1- (CADR PHASE)) NOTQTY))
            )
            (SETQ
              WRITELIST
               (REVERSE
                 (CONS
                   (CAR (NTH (1- (CADR PHASE)) NOTQTY))
                   (REVERSE WRITELIST)
                 )
               )
            )
            ;; Cost total for phase
            (SETQ
              WRITELIST
               (REVERSE
                 (CONS
                   (CADR (NTH (1- (CADR PHASE)) NOTQTY))
                   (REVERSE WRITELIST)
                 )
               )
            )
          )
         PHASES_DEFINITION
       )
       ;;Write unit to drawing
       (SETQ X (+ X (* TXTHT QUWID)))
       (IF (/= NOTUNT "")
         (HAWS-MKTEXT "ML" (LIST X Y Z) TXTHT 0 NOTUNT)
       )
       (SETQ
         X         (+ COL1X (* TXTHT NDWID))
         NOTE_FIRST_LINE_P T
       )
       (FOREACH
          NOTDSC (NTH 6 ENTRY)
         (IF (/= NOTDSC "")
           (HAWS-MKTEXT "ML" (LIST X Y Z) TXTHT 0 NOTDSC)
         )
         (SETQ Y (- Y (* TXTHT LINSPC)))
       )
       (SETQ Y (- Y (* TXTHT (- NOTSPC LINSPC))))
       ;;Write note to file.
       (FOREACH
          X WRITELIST
         (IF (= (TYPE X) 'LIST)
           (PROGN
             (SETQ NOTDESC "")
             (FOREACH Y X (SETQ NOTDESC (STRCAT NOTDESC "\n" Y)))
             (PRINC (HAWS-MKFLD (SUBSTR NOTDESC 2) ",") F2)
           )
           (PRINC (STRCAT X ",") F2)
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
;#endregion
;#region CNM Main
;;CNM main commands
(DEFUN C:HCNM-CNM ()
(haws-core-init 179) (HCNM_CNM NIL)(haws-core-restore))
(DEFUN C:HCNM-CNMKT ()
(haws-core-init 180) (HCNM_CNM "Search")(haws-core-restore))
(DEFUN C:HCNM-CNMKTI ()
(haws-core-init 181) (HCNM_CNM "Import")(haws-core-restore))
(DEFUN C:HCNM-CNMQT ()
(haws-core-init 338) (HCNM_CNM "Tally")(haws-core-restore))
;;CNM main function
(DEFUN HCNM_CNM (OPT / CFNAME DN LINSPC PHASEWID TBLWID TXTHT)
  ;;Main function
  (HAWS-VSAVE
    '("attdia" "attreq" "clayer" "osmode")
  )
  (SETVAR "attdia" 0)
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
  (HCNM_PROJINIT)                       ;Initialize after pauses
  ;;Set user's desired dimstyle.
  (HCNM_SET_DIMSTYLE "NotesKeyTableDimstyle")
  (SETQ
    DN (HAWS-GETDNPATH)
    PROJNOTES
     (HCNM_PROJNOTES)
    TXTHT
     (* (GETVAR "dimtxt") (HAWS-DWGSCALE))
    ;;Column and line spacing widths (half width for middle justified columns)
    ;;line spacing
    LINSPC
     (ATOF (c:hcnm-config-getvar "LineSpacing"))
    ;;width of single sheet table with only one phase
    TBLWID
     (ATOF (c:hcnm-config-getvar "TableWidth"))
    ;;width for each extra phase on single sheet table.
    PHASEWID
     (ATOF (c:hcnm-config-getvar "PhaseWidthAdd"))
  )
  (HCNM_READCF PROJNOTES)
  (COND
    ((= OPT "Search")
     (HCNM_KEY_TABLE_FROM_SEARCH
       DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID
      )
    )
    ((= OPT "Import")
     (HCNM_IMPORT DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID)
    )
    ((= OPT "Tally")
     (HCNM_TALLY DN PROJNOTES TXTHT LINSPC TBLWID PHASEWID)
    )
  )
  ;;Restore old dimstyle
  (HCNM_RESTORE_DIMSTYLE)
  (HAWS-VRSTOR)
  (haws-core-restore)
  (PRINC)
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
(DEFUN HCNM_PROJINIT ()
  (SETQ
    *HCNM_CONFIG* NIL
    *HCNM_CNMPROJECTROOT* NIL
    *HCNM_CNMPROJECTNOTES* NIL
  )
)


;;Does nothing but strcat, since the existence of the file
;;is validated by (HCNM_PROJ)
(DEFUN HCNM_INI_NAME (PROJ)
  (HCNM_PROJECT_FOLDER_TO_INI PROJ)
)

;; HCNM_PROJ gets a valid project root folder for this drawing's folder.
;; While it returns the folder only, that folder is qualified to have CNM.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\projroot" path to other functions.
(DEFUN HCNM_PROJ (/ DWGDIR LINKED_PROJECT_FOLDER LINKED_PROJECT_MARKER
              LOCAL_PROJECT_FOLDER LOCAL_PROJECT_MARKER
             )
  (SETQ
    DWGDIR
     (HAWS-FILENAME-DIRECTORY (GETVAR "dwgprefix"))
    LOCAL_PROJECT_MARKER
     (HCNM_LOCAL_PROJECT_MARKER DWGDIR)
    LINKED_PROJECT_MARKER
     (HCNM_LINKED_PROJECT_MARKER DWGDIR)
  )
  (COND
    (LOCAL_PROJECT_MARKER
     (SETQ
       LOCAL_PROJECT_FOLDER
        (HCNM_ASSURE_LOCAL_PROJECT
          LOCAL_PROJECT_MARKER
        )
     )
    )
  )
  (COND
    (LINKED_PROJECT_MARKER
     (SETQ
       LINKED_PROJECT_FOLDER
        (HCNM_ASSURE_LINKED_PROJECT
          LINKED_PROJECT_MARKER
        )
     )
    )
  )
  (SETQ
    *HCNM_CNMPROJECTROOT*
     (COND
       ;;If project is already defined this session, use it.
       ;;(Assume it's valid.  Calling function should init project if there's been a chance of change or loss by user.)
       (*HCNM_CNMPROJECTROOT*)
       ((AND
          LOCAL_PROJECT_MARKER
          LINKED_PROJECT_MARKER
        )
        (HCNM_ERROR_AMBIGUOUS_PROJECT_MARKERS
          LOCAL_PROJECT_FOLDER
          LINKED_PROJECT_FOLDER
        )
       )
       ;;Else well-formed simple (single-folder) projects. CNM.INI is here.
       (LOCAL_PROJECT_MARKER
        (HCNM_ASSURE_LOCAL_PROJECT
          LOCAL_PROJECT_MARKER
        )
       )
       ;;Well-formed complex (multi-folder) projects.  CNMPROJ.TXT is here and
       ;;we'll make sure it really points to a CNM.INI.
       (LINKED_PROJECT_MARKER
        (HCNM_ASSURE_LINKED_PROJECT
          LINKED_PROJECT_MARKER
        )
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
        (HCNM_INITIALIZE_PROJECT DWGDIR)
        DWGDIR
       )
     )
  )
)

(DEFUN HCNM_LOCAL_PROJECT_MARKER (DWGDIR)
  (FINDFILE (HCNM_PROJECT_FOLDER_TO_INI DWGDIR))
)

(DEFUN HCNM_LINKED_PROJECT_MARKER (DWGDIR)
  (FINDFILE (HCNM_PROJECT_FOLDER_TO_LINK DWGDIR))
)

(DEFUN HCNM_ERROR_NOT_WRITEABLE
   ()
  (ALERT
    (PRINC
      (STRCAT
        "Fatal error:\n\nThis drawing must be saved before CNM can be used."
        "\nCNM cannot continue."
       )
    )
  )
  (EXIT)
)

(DEFUN HCNM_ERROR_AMBIGUOUS_PROJECT_MARKERS
   (LOCAL_PROJECT_FOLDER LINKED_PROJECT_FOLDER)
  (ALERT
    (PRINC
      (STRCAT
        "Error:\nThis drawing's folder\n" LOCAL_PROJECT_FOLDER
        "\nhas both its own project settings (CNM.ini) and a link (in CNMPROJ.TXT) to a project in another folder:\n"
        LINKED_PROJECT_FOLDER
        "\n\nCNM cannot continue. File names will be printed to the command history for your use resolving the ambiguity."
       )
    )
  )
  (PRINC
    (STRCAT
      "\nLocal project: "
      (HCNM_PROJECT_FOLDER_TO_INI LOCAL_PROJECT_FOLDER)
    )
  )
  (PRINC
    (STRCAT
      "\nLink to another project: "
      (HCNM_PROJECT_FOLDER_TO_LINK LOCAL_PROJECT_FOLDER)
    )
  )
  (EXIT)
)


(DEFUN HCNM_ASSURE_LOCAL_PROJECT (LOCAL_MARKER_FILE)
  (HCNM_CHECK_MOVED_PROJECT LOCAL_MARKER_FILE)
  (HAWS-FILENAME-DIRECTORY LOCAL_MARKER_FILE)
)

(DEFUN HCNM_ASSURE_LINKED_PROJECT (LINK_MARKER / PROJROOT RDLIN)
  (COND
    ((AND
       (SETQ F1 (OPEN LINK_MARKER "r"))
       (PROGN
         (WHILE (AND (SETQ RDLIN (READ-LINE F1)) (NOT PROJROOT))
           (COND
             ((HAWS-VLISP-P)
              (IF (VL-FILE-DIRECTORY-P RDLIN)
                (SETQ PROJROOT RDLIN)
              )
             )
             ;;Bricscad option
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
    )
  )
  (IF (NOT (FINDFILE (HCNM_INI_NAME PROJROOT)))
    (HCNM_INITIALIZE_PROJECT PROJROOT)
  )
  (HCNM_CHECK_MOVED_PROJECT
    (HCNM_PROJECT_FOLDER_TO_INI PROJROOT)
  )
  (PRINC
    (STRCAT
      "\nUsing project settings from another folder as directed by CNMPROJ.TXT in this drawing's folder."
      "Using project settings from another folder located at "
      PROJROOT
      "\\CNM.INI as directed by CNMPROJ.TXT in this drawing's folder."
    )
  )
  PROJROOT
)

(DEFUN HCNM_CHECK_MOVED_PROJECT
   (PROJECT_FILE_NAME / INPUT1 PNNAME THISFILE_VALUE)
  (COND
    ((AND
       (SETQ
         THISFILE_VALUE
          (INI_READENTRY
            PROJECT_FILE_NAME
            "CNM"
            "ThisFile"
          )
       )
       (SETQ
         PNNAME
          (INI_READENTRY PROJECT_FILE_NAME "CNM" "ProjectNotes")
       )
       (/= THISFILE_VALUE "")
       (/= THISFILE_VALUE PROJECT_FILE_NAME)
     )
     (ALERT
       (PRINC
         (STRCAT
           "Warning!\nYou are using these project notes:\n\n"
           PNNAME
           "\n\nand the CNM.ini for this folder says \n\"ThisFile=\""
           THISFILE_VALUE
           "\n\nIt appears it may have been copied from another project."
           "\nYou may be about to edit the wrong Project Notes file."
         )
       )
     )
     (INITGET "Yes No")
     (SETQ INPUT1 (GETKWORD "\nContinue with this file? [Yes/No]: "))
     (COND
       ((= INPUT1 "Yes")(INI_WRITEENTRY PROJECT_FILE_NAME "CNM" "ThisFile" ""))
       (T (EXIT))
     )
    )
  )
)


(DEFUN HCNM_PROJECT_INI_NAME () "cnm.ini")
(DEFUN HCNM_PROJECT_LINK_NAME () "cnmproj.txt")

(DEFUN HCNM_PROJECT_FOLDER_TO_INI (PROJECT_FOLDER)
  (STRCAT PROJECT_FOLDER "\\" (HCNM_PROJECT_INI_NAME))
)
(DEFUN HCNM_PROJECT_FOLDER_TO_LINK (PROJECT_FOLDER)
  (STRCAT PROJECT_FOLDER "\\" (HCNM_PROJECT_LINK_NAME))
)


;;as posted the autodesk discussion customization group by Tony Tanzillo
(DEFUN ALE_BROWSEFORFOLDER
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
(DEFUN C:HCNM-LINKPROJ ()
(haws-core-init 183) (HCNM_LINKPROJ NIL) (haws-core-restore)(PRINC))

;; Sets the CNM project to the given folder. Includes wizards, alerts, and error checks.
(DEFUN HCNM_LINKPROJ (PROJ / DWGDIR LOCALPROJ LOCALPROJBAK OLDLINK)
  (SETQ
    DWGDIR
     (HAWS-FILENAME-DIRECTORY (GETVAR "dwgprefix"))
    *HCNM_CNMPROJECTROOT*
     (COND
       (*HCNM_CNMPROJECTROOT*)
       (DWGDIR)
     )
  )
  (COND ((NOT PROJ)(SETQ PROJ (HCNM_BROWSEPROJ *HCNM_CNMPROJECTROOT*))))
  (COND
    (PROJ
     (SETQ *HCNM_CNMPROJECTROOT* PROJ)
     (COND
       ((= PROJ DWGDIR)
        (COND
          ((SETQ OLDLINK (FINDFILE (HCNM_PROJECT_FOLDER_TO_LINK PROJ)))
           (ALERT
             (PRINC
               "Setting project to this drawing's folder by deleting an existing link to another folder."
             )
           )
           (VL-FILE-DELETE OLDLINK)
          )
          (T
           (ALERT
             (STRCAT
               "Project Folder\n"
               *HCNM_CNMPROJECTROOT*
               "\nnot changed."
             )
           )
          )
        )
       )
       (PROJ
        (HCNM_MAKEPROJTXT PROJ DWGDIR)
        (ALERT
          (PRINC
            (STRCAT
              "Created link in this drawing's folder to CNM project settings in\n"
              PROJ
            )
          )
        )
        (COND
          ((SETQ
             LOCALPROJ
              (FINDFILE (HCNM_PROJECT_FOLDER_TO_INI DWGDIR))
           )
           (SETQ LOCALPROJBAK (STRCAT LOCALPROJ ".bak"))
           (ALERT
             (PRINC
               (STRCAT
                 "Note: CNM renamed the existing\n" LOCALPROJ "\nto\n"
                 LOCALPROJBAK
                 "\nbecause you linked to a project in another folder."
                )
             )
           )
           (VL-FILE-RENAME LOCALPROJ LOCALPROJBAK)
          )
        )
       )
     )
    )
    (*HCNM_CNMPROJECTROOT*
     (ALERT
       (STRCAT
         "Project Folder\n"
         *HCNM_CNMPROJECTROOT*
         "\nnot changed."
       )
     )
    )
  )
)

(DEFUN HCNM_BROWSEPROJ (OLDPROJ)
  (COND
    ((HAWS-VLISP-P)
     (ALE_BROWSEFORFOLDER
       (HCNM_SHORTEN_PATH OLDPROJ 50)
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

(DEFUN HCNM_SHORTEN_PATH (PATH NSHORT)
  (COND
    ((< (STRLEN PATH) NSHORT) PATH)
    ((STRCAT
       "Cancel to keep current Project Folder:\n"
       (SUBSTR PATH 1 3)
       "..."
       (HAWS-ENDSTR PATH (- NSHORT 3) (- NSHORT 3))
     )
    )
  )
)


;;Makes a project root reference file CNMPROJ.TXT in this drawing's folder
;;Returns nil.
(DEFUN HCNM_MAKEPROJTXT (PROJDIR DWGDIR)
  (SETQ F2 (OPEN (HCNM_PROJECT_FOLDER_TO_LINK DWGDIR) "w"))
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
(DEFUN C:TESTSET ()
(haws-core-init 184)
  (hcnm_concept_TESTSETVAR
    (GETSTRING "\nVariable name: ")
    (GETSTRING "\nValue: ")
  )
 (haws-core-restore)
)
(DEFUN C:TESTGET ()
(haws-core-init 185)
  (hcnm_concept_TESTGETVAR (GETSTRING "\nVariable name: "))
  (haws-core-restore)
)
(DEFUN hcnm_concept_TESTSETVAR (VAR VAL)
  (hcnm_concept_SETVAR
    ;; variable
    VAR
    ;;value
    VAL
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
(DEFUN hcnm_concept_TESTGETVAR (VAR)
  (hcnm_concept_GETVAR
    ;;variable
    VAR
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
(DEFUN hcnm_concept_INIFILE (APP SCOPE TESTFILE / INIFILE ASSUMEDINIFOLDER
                 ASSUMEDINIFILE
                )
  (COND ((NOT TESTFILE) (SETQ TESTFILE (STRCAT APP ".cui"))))
  (COND
    ;; Project already defined this session
    ;; (Assume it's valid.  Calling function should init project
    ;; if there's been a chance of loss.)
    ((CDR
       (ASSOC
         0
         (CDR (ASSOC APP (CDR (ASSOC SCOPE *hcnm_concept_SETTINGS*))))
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
    ((AND
       (SETQ ASSUMEDINIFOLDER (FINDFILE TESTFILE))
       (SETQ
         ASSUMEDINIFOLDER
          (hcnm_concept_FILENAME_DIRECTORY ASSUMEDINIFOLDER)
       )
       (SETQ
         ASSUMEDINIFILE
          (FINDFILE (STRCAT ASSUMEDINIFOLDER APP ".ini"))
       )
     )
     ASSUMEDINIFILE
    )
    ;;Or make an ini file in folder with testfile in it or in proj folder
    ((hcnm_concept_GETINIDEFAULTS ASSUMEDINIFOLDER) ASSUMEDINIFOLDER)
  )
)


;;Gets all settings from an inifile if it can.
(DEFUN hcnm_concept_GETSETTINGS (INIFILE TESTFILE APPORPROJ)
  (SETQ
    *hcnm_concept_SETTINGS*
     (INI_READINI
       (hcnm_concept_INI (hcnm_concept_INIFOLDER INIFILE TESTFILE))
     )
  )
)

;;Sets a variable in the global lisp list and in HAWSEDC.INI
(DEFUN hcnm_concept_SETVAR (INIFILE INISECTION VAR VAL / SETTING)
  ;; Call GETVAR before setting var.  Why?  To populate *hcnm_concept_SETTINGS*?
  (hcnm_concept_GETVAR
    VAR INISECTION INIFILE TESTFILE APPORPROJ DEFAULTS
   )
  (hcnm_concept_ADDVARTOLIST VAR VAL INISECTION INIFILE)
  (INI_WRITEENTRY
    (hcnm_concept_INI (hcnm_concept_INIFOLDER))
    INISECTION
    SETTING
    VAL
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
(DEFUN hcnm_concept_GETVAR (VAR SECT APP SCOPE TESTFILE DEFAULTS / ADDTOLIST
                ADDTOINI DIR INI SETTING VAL
               )
  (SETQ
    ;; Does the variable need to be added to the *hcnm_concept_SETTINGS* list? Assume yes initially.
    ADDTOLIST
     T
    ;; Does the variable need to be added to the appropriate ini file? Assume yes initially
    ADDTOINI
     T
  )
  ;;Get var list if no var list
  (IF (NOT *hcnm_concept_SETTINGS*)
    (hcnm_concept_GETSETTINGS)
  )
  (COND
    ;;Try getting from list
    ((SETQ
       VAL
        (hcnm_concept_VARTOVAL
          VAR
          (CADR (ASSOC SECT (CADDR (ASSOC APP *hcnm_concept_SETTINGS*))))
        )
     )
     (SETQ
       ADDTOLIST NIL
       ADDTOINI NIL
     )
    )
    ;;Try getting from ini.
    ((SETQ
       VAL
        (INI_READENTRY APP SECT SETTING)
       ADDTOINI NIL
     )
    )
    ;;Get from app ini if not.
    ((AND
       (SETQ DIR (FINDFILE TESTFILE))
       (SETQ
         INI
          (FINDFILE (STRCAT (hcnm_concept_FILENAME_DIRECTORY DIR) "\\" APP))
       )
       (SETQ VAL (INI_READENTRY INI SECT SETTING))
     )
    )
    ;;Use default if there is one
    ((SETQ VAL (hcnm_concept_VARTOVAL VAR DEFAULTS)))
    ;;Otherwise fail.
    (T
     (ALERT
       (STRCAT
         "Fatal error in "
         SECT
         ":\nCould not initialize the variable\n"
         VAR
       )
     )
     (SETQ
       ADDTOLIST NIL
       ADDTOINI NIL
     )
    )
  )
  (IF ADDTOLIST
    (hcnm_concept_ADDVARTOLIST VAR VAL SECT APP)
  )
  (IF ADDTOINI
    (INI_WRITEENTRY (hcnm_concept_INI (hcnm_concept_INIFOLDER)) APP SETTING VAL)
  )
  VAL
)

(DEFUN hcnm_concept_ADDVARTOLIST (VAR VAL INISECTION INIFILE)
  (SETQ
    SETTING
     (hcnm_concept_VARTOSETTING VAR)
    *hcnm_concept_SETTINGS*
     (SUBST
       (SUBST
         (SUBST
           (LIST SETTING VAL)
           (ASSOC
             INISETTING
             (ASSOC
               INISECTION
               (ASSOC INIFILE *hcnm_concept_SETTINGS*)
             )
           )
           (ASSOC
             INISECTION
             (ASSOC INIFILE *hcnm_concept_SETTINGS*)
           )
         )
         (ASSOC INISECTION (ASSOC FILE *hcnm_concept_SETTINGS*))
         (ASSOC INIFILE *hcnm_concept_SETTINGS*)
       )
       (ASSOC INIFILE *hcnm_concept_SETTINGS*)
       *hcnm_concept_SETTINGS*
     )
  )
)


;;Gets an entire ini file from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(DEFUN hcnm_concept_GETINIDEFAULTS (PROJ / APP APPINI PROJINI)
  (ALERT
    (PRINC
      (STRCAT
        "Note: Program settings not found in program folder\n"
        PROJ
        "\n\nUsing default settings."
      )
    )
  )
  (SETQ PROJINI (STRCAT PROJ "\\" "cnm.ini"))
  (COND
    ((AND
       (SETQ APP (C:HCNM-CONFIG-GETVAR "AppFolder"))
       (SETQ
         APPINI
          (FINDFILE
            (STRCAT (hcnm_concept_FILENAME_DIRECTORY APP) "\\" "cnm.ini")
          )
       )
       (hcnm_concept_FILE_COPY APPINI PROJINI)
     )
     (WHILE (NOT (FINDFILE PROJINI)))
     PROJINI
    )
    (T
     (SETQ F2 (OPEN PROJINI "w"))
     (PRINC
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
"      F2
     )
     (SETQ F2 (CLOSE F2))
     PROJINI
    )
  )
)

;;Saves *hcnm_concept_SETTINGS* to the requested inifile
(DEFUN hcnm_concept_SAVESETTINGSTOINI (INIFILE TESTFILE APPORPROJ)
  (INI_WRITESECTION
    (hcnm_concept_INI (hcnm_concept_INIFOLDER INIFILE TESTFILE))
    INIFILE
    *hcnm_concept_SETTINGS*
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
(DEFUN HCNM_CONFIG_DEFINITIONS (/)
  (LIST
    (LIST "Scope"
     (LIST "Session" 0)
     (LIST "Drawing" 1) ; Does not work yet?
     (LIST "Project" 2)
     (LIST "App" 3) ; Does not work yet?
     (LIST "User" 4)
    )
    (LIST "Var"
     (LIST "ProjectFolder" "" 1)
     (LIST "AppFolder"  (HAWS-FILENAME-DIRECTORY (findfile "cnm.mnl")) 0)
     (LIST "LXXListMode" "yes" 4)
     (LIST "CNMAliasActivation" "0" 4)
     (LIST "ProjectNotesEditor" (HCNM_CONFIG_DEFAULT_PROJECTNOTESEDITOR) 2) ; text, csv, or cnm
     (LIST "LayersEditor" "notepad" 4) ; notepad or cnm
     (LIST "ProjectNotes" "constnot.csv" 2)
     (LIST "ThisFile" "" 2)
     (LIST "ImportLayerSettings" "No" 2)
     (LIST "NoteTypes" "BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI" 2)
     (LIST "DoCurrentTabOnly" "0" 2)
     (LIST "PhaseAlias1" "1" 2)
     (LIST "PhaseAlias2" "2" 2)
     (LIST "PhaseAlias3" "3" 2)
     (LIST "PhaseAlias4" "4" 2)
     (LIST "PhaseAlias5" "5" 2)
     (LIST "PhaseAlias6" "6" 2)
     (LIST "PhaseAlias7" "7" 2)
     (LIST "PhaseAlias8" "8" 2)
     (LIST "PhaseAlias9" "9" 2)
     (LIST "InsertTablePhases" "No" 2)
     (LIST "TableWidth" "65" 2)
     (LIST "PhaseWidthAdd" "9" 2)
     (LIST "DescriptionWrap" "9999" 2)
     (LIST "LineSpacing" "1.5" 2)
     (LIST "NoteSpacing" "3" 2)
     (LIST "NumberToDescriptionWidth" "2.5" 2)
     (LIST "DescriptionToQuantityWidth" "56" 2)
     (LIST "QuantityToQuantityWidth" "9" 2)
     (LIST "QuantityToUnitsWidth" "1" 2)
     (LIST "ShowKeyTableTitleShapes" "1" 2)
     (LIST "ShowKeyTableGrid" "0" 2)
     (LIST "ShowKeyTableQuantities" "1" 2)
     (LIST "BubbleHooks" "0" 2)
     (LIST "BubbleMtext" "0" 2)
     (LIST "BubbleAreaIntegral" "0" 2)
     (LIST "NotesLeaderDimstyle" "" 2)
     (LIST "NotesKeyTableDimstyle" "" 2)
     (LIST "TCGLeaderDimstyle" "TCG Leader" 2)
     (LIST "BubbleTextLine1PromptP" "1" 4)
     (LIST "BubbleTextLine2PromptP" "1" 4)
     (LIST "BubbleTextLine3PromptP" "0" 4)
     (LIST "BubbleTextLine4PromptP" "0" 4)
     (LIST "BubbleTextLine5PromptP" "0" 4)
     (LIST "BubbleTextLine6PromptP" "0" 4)
     (LIST "BubbleTextLine0PromptP" "0" 4)
     (LIST "BubbleSkipEntryPrompt" "0" 4)
     (LIST "BubbleOffsetDropSign" "1" 2)
     (LIST "BubbleTextPrefixLF" "" 2)
     (LIST "BubbleTextPrefixSF" "" 2)
     (LIST "BubbleTextPrefixSY" "" 2)
     (LIST "BubbleTextPrefixSta" "STA " 2)
     (LIST "BubbleTextPrefixOff+" "" 2)
     (LIST "BubbleTextPrefixOff-" "" 2)
     (LIST "BubbleTextPrefixN" "N " 2)
     (LIST "BubbleTextPrefixE" "E " 2)
     (LIST "BubbleTextPrefixZ" "" 2)
     (LIST "BubbleTextPostfixLF" " LF" 2)
     (LIST "BubbleTextPostfixSF" " SF" 2)
     (LIST "BubbleTextPostfixSY" " SY" 2)
     (LIST "BubbleTextPostfixSta" "" 2)
     (LIST "BubbleTextPostfixOff+" " RT" 2)
     (LIST "BubbleTextPostfixOff-" " LT" 2)
     (LIST "BubbleTextPostfixN" "" 2)
     (LIST "BubbleTextPostfixE" "" 2)
     (LIST "BubbleTextPostfixZ" "" 2)
     (LIST "BubbleTextJoinDelSta" ", " 2)
     (LIST "BubbleTextJoinDelN" ", " 2)
     (LIST "BubbleTextPrecisionLF" "0" 4)
     (LIST "BubbleTextPrecisionSF" "0" 4)
     (LIST "BubbleTextPrecisionSY" "0" 4)
     (LIST "BubbleTextPrecisionOff+" "2" 4)
     (LIST "BubbleTextPrecisionN" "2" 4)
     (LIST "BubbleTextPrecisionE" "2" 4)
     (LIST "BubbleTextPrecisionZ" "2" 4)
     (LIST "BubbleCurrentAlignment" T 0)
     (LIST "BubbleArrowIntegralPending" "0" 0)
    )
   )
)

(DEFUN HCNM_CONFIG_DEFAULT_PROJECTNOTESEDITOR (/ REG_VAL)
  (COND
    ((AND
       (HAWS-VLISP-P)
       (SETQ
         REG_VAL
          (VL-REGISTRY-READ
            "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
            "ProjectNoteseditor"
          )
       )
       (WCMATCH (STRCASE REG_VAL) "*CNMEDIT*")
     )
     "cnm"
    )
    (T "csv")
  )
)

;; Strips scope stuff and returns just defaults list
(DEFUN HCNM_CONFIG_DEFAULTS ()
  (COND
    (*HCNM_CONFIG_DEFAULTS*)
    ((MAPCAR
       '(LAMBDA (VAR) (HCNM_CONFIG_ENTRY_STRIP_SCOPE VAR))
       (CDR (ASSOC "Var" (HCNM_CONFIG_DEFINITIONS)))
     )
    )
  )
)

(DEFUN HCNM_CONFIG_ENTRY_STRIP_SCOPE (ENTRY)
  (REVERSE (CDR (REVERSE ENTRY)))
)

(DEFUN HCNM_CONFIG_DEFAULTS_SINGLE_SCOPE
   (SCOPE_KEY / SCOPE_CODE SCOPE_LIST)
  (SETQ SCOPE_CODE (HCNM_CONFIG_SCOPE_CODE SCOPE_KEY))
  (FOREACH
     ENTRY (CDR (ASSOC "Var" (HCNM_CONFIG_DEFINITIONS)))
    (COND
      ((= (HCNM_CONFIG_ENTRY_SCOPE_CODE ENTRY) SCOPE_CODE)
       (SETQ
         SCOPE_LIST
          (CONS
            (HCNM_CONFIG_ENTRY_STRIP_SCOPE ENTRY)
            SCOPE_LIST
          )
       )
      )
    )
  )
  (REVERSE SCOPE_LIST)
)

(DEFUN HCNM_CONFIG_SCOPE_CODE (SCOPE_KEY)
  (CADR
    (ASSOC
      SCOPE_KEY
      (CDR (ASSOC "Scope" (HCNM_CONFIG_DEFINITIONS)))
    )
  )
)

(DEFUN HCNM_CONFIG_SCOPE_EQ (VAR SCOPE_KEY)
  (= (HCNM_CONFIG_ENTRY_SCOPE_CODE
       (ASSOC VAR (CDR (ASSOC "Var" (HCNM_CONFIG_DEFINITIONS))))
     )
     (HCNM_CONFIG_SCOPE_CODE SCOPE_KEY)
  )
)

(DEFUN HCNM_CONFIG_ENTRY_VAR (ENTRY) (CAR ENTRY))

(DEFUN HCNM_CONFIG_ENTRY_VAL (ENTRY) (CADR ENTRY))

(DEFUN HCNM_CONFIG_ENTRY_SCOPE_CODE (ENTRY) (CADDR ENTRY))

(DEFUN HCNM_CONFIG_GET_DEFAULT (VAR)
  (HCNM_CONFIG_ENTRY_VAL (ASSOC VAR (HCNM_CONFIG_DEFAULTS)))
)

(DEFUN HCNM_CONFIG_READ_ALL (/ INI_CONFIGS)
  (APPEND
    (HCNM_CONFIG_READ_ALL_USER)
    (HCNM_CONFIG_READ_ALL_PROJECT)
  )
)
(DEFUN HCNM_CONFIG_READ_ALL_USER (/ )
  ;; This function doesn't need to setq an ini_configs since it does HCNM_CONFIG_READ_USER var by var.
  (MAPCAR
    '(LAMBDA (ENTRY / VAR VAL)
       (SETQ
         VAR (HCNM_CONFIG_ENTRY_VAR ENTRY)
         VAL (HCNM_CONFIG_READ_USER VAR)
       )
       (LIST VAR VAL)
     )
    (HCNM_CONFIG_DEFAULTS_SINGLE_SCOPE "User")
  )
)

(DEFUN HCNM_CONFIG_READ_ALL_PROJECT (/ INI_CONFIGS)
  (SETQ INI_CONFIGS (INI_READSECTION (HCNM_INI_NAME (HCNM_PROJ)) "CNM"))
  (MAPCAR
    '(LAMBDA (ENTRY / VAR VAL)
       (SETQ
         VAR (HCNM_CONFIG_ENTRY_VAR ENTRY)
         VAL (HCNM_CONFIG_ENTRY_VAL (ASSOC VAR INI_CONFIGS))
       )
       (LIST VAR VAL)
     )
    (HCNM_CONFIG_DEFAULTS_SINGLE_SCOPE "Project")
  )
)

(DEFUN HCNM_CONFIG_READ_ALL_SESSION (/ INI_CONFIGS)
  ;; Maybe this function can do what HCNM_CONFIG_READ_ALL_USER does; it doesn't need to setq an ini_configs since it does HCNM_CONFIG_READ_USER var by var.
  (SETQ INI_CONFIGS *HCNM_CONFIG_SESSION*)
  (MAPCAR
    '(LAMBDA (ENTRY / VAR VAL)
       (SETQ
         VAR (HCNM_CONFIG_ENTRY_VAR ENTRY)
         VAL (HCNM_CONFIG_ENTRY_VAL (ASSOC VAR INI_CONFIGS))
       )
       (LIST VAR VAL)
     )
    (HCNM_CONFIG_DEFAULTS_SINGLE_SCOPE "Session")
  )
)

;;;Sets a variable in a temporary global lisp list
(DEFUN HCNM_CONFIG_TEMP_SETVAR (VAR VAL)
  (COND
    ((ASSOC VAR *HCNM_CONFIG_TEMP*)
     (SETQ
       *HCNM_CONFIG_TEMP*
        (SUBST
          (LIST VAR VAL)
          (ASSOC VAR *HCNM_CONFIG_TEMP*)
          *HCNM_CONFIG_TEMP*
        )
     )
    )
    (T
     (SETQ *HCNM_CONFIG_TEMP* (CONS (LIST VAR VAL) *HCNM_CONFIG_TEMP*))
    )
  )
)

;;;Gets a variable in a temporary global lisp list
;;;If it's not present there, gets real value.
(DEFUN HCNM_CONFIG_TEMP_GETVAR (VAR)
  (COND
    ((CADR (ASSOC VAR *HCNM_CONFIG_TEMP*)))
    (T (C:HCNM-CONFIG-GETVAR VAR))
  )
)


;;;Saves the global list of temporary configs in the real global lisp list and in CNM.INI
(DEFUN HCNM_CONFIG_TEMP_SAVE ()
  (FOREACH
     ENTRY *HCNM_CONFIG_TEMP*
    (c:hcnm-config-setvar
      (HCNM_CONFIG_ENTRY_VAR ENTRY)
      (HCNM_CONFIG_ENTRY_VAL ENTRY)
    )
  )
)

;;;Saves the global list of temporary configs in the real global lisp list and in CNM.INI
(DEFUN HCNM_CONFIG_TEMP_CLEAR ()
  (SETQ *HCNM_CONFIG_TEMP* NIL)
)

;;;Sets a variable in the global lisp list and in CNM.INI
(DEFUN C:HCNM-CONFIG-SETVAR (VAR VAL)
  (SETQ
    *HCNM_CONFIG*
     (COND
       ((ASSOC VAR *HCNM_CONFIG*)
        (SUBST
          (LIST VAR VAL)
          (ASSOC VAR *HCNM_CONFIG*)
          *HCNM_CONFIG*
        )
       )
       (T (CONS (LIST VAR VAL) *HCNM_CONFIG*))
     )
  )
  (COND
    ((HCNM_CONFIG_SCOPE_EQ VAR "User")
     (HCNM_CONFIG_WRITE_USER VAR VAL)
    )
    ((HCNM_CONFIG_SCOPE_EQ VAR "Project")
     (INI_WRITEENTRY (HCNM_INI_NAME (HCNM_PROJ)) "CNM" VAR VAL)
    )
    ((HCNM_CONFIG_SCOPE_EQ VAR "Session")
     (HCNM_CONFIG_WRITE_SESSION VAR VAL)
    )
  )
  VAL
)


;;; c:hcnm-config-getvar
;;; Var is case sensitive
(DEFUN C:HCNM-CONFIG-GETVAR
   (VAR / SETVAR_P DEFINE_CONFIGS DIR INI PROJROOT CONFIG VAL)
  (SETQ SETVAR_P T)
  (COND
    ;; Initialize configs as needed
    ((NOT (ASSOC VAR *HCNM_CONFIG*))
     (COND
       ((HCNM_CONFIG_SCOPE_EQ VAR "Project")
        (SETQ
          *HCNM_CONFIG*
           (APPEND
             *HCNM_CONFIG*
             ;; If one project var is missing, all project vars are missing
             (HCNM_CONFIG_READ_ALL_PROJECT)
           )
        )
       )
       ((HCNM_CONFIG_SCOPE_EQ VAR "User")
        (SETQ
          *HCNM_CONFIG*
           (APPEND
             *HCNM_CONFIG*
             ;; If one user var is missing, all user vars are missing
             (HCNM_CONFIG_READ_ALL_USER)
           )
        )
       )
       ((HCNM_CONFIG_SCOPE_EQ VAR "Session")
        (SETQ
          *HCNM_CONFIG*
           (APPEND
             *HCNM_CONFIG*
             ;; If one session var is missing, all session vars are missing
             (HCNM_CONFIG_READ_ALL_SESSION)
           )
        )
       )       
     )
    )
  )
  (COND
    ;;Try getting from list
    ((SETQ VAL (CADR (ASSOC VAR *HCNM_CONFIG*)))
     (SETQ SETVAR_P NIL)
    )
    ;;Use default if there is one
    ((SETQ VAL (HCNM_CONFIG_GET_DEFAULT VAR)))
    ;;Otherwise fail.
    (T
     (ALERT
       (STRCAT
         "Fatal error in CNM:\nCould not initialize the variable\n"
         (HAWS-PRIN1-TO-STRING VAR)
       )
     )
     (SETQ SETVAR_P NIL)
    )
  )
  (IF SETVAR_P
    (C:HCNM-CONFIG-SETVAR VAR VAL)
  )
  VAL
)

(DEFUN HCNM_CONFIG_READ_USER (VAR / )
  (COND
    ((HAWS-VLISP-P)
     (VL-REGISTRY-READ
       "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
       VAR
     )
    )
    (T NIL)
  )
)

(DEFUN HCNM_CONFIG_WRITE_USER (VAR VAL)
  (COND
    ((HAWS-VLISP-P)
     (VL-REGISTRY-WRITE
       "HKEY_CURRENT_USER\\Software\\HawsEDC\\CNM"
       VAR
       VAL
     )
    )
  )
)

(DEFUN HCNM_CONFIG_READ_SESSION (VAR / )
    (CADR (ASSOC VAR *HCNM_CONFIG_SESSION*))
)

(DEFUN HCNM_CONFIG_WRITE_SESSION (VAR VAL)
  (SETQ
    *HCNM_CONFIG_SESSION*
     (COND
       ((ASSOC VAR *HCNM_CONFIG_SESSION*)
        (SUBST
          (LIST VAR VAL)
          (ASSOC VAR *HCNM_CONFIG_SESSION*)
          *HCNM_CONFIG_SESSION*
        )
       )
       (T (CONS (LIST VAR VAL) *HCNM_CONFIG_SESSION*))
     )
  )
)

;;Gets an entire ini file (per CNM forum) from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(DEFUN HCNM_INITIALIZE_PROJECT (PROJ / APP APPINI PROJINI MARK_FILE_P)
  (SETQ PROJINI (HCNM_PROJECT_FOLDER_TO_INI PROJ))
  (COND
    ((AND
       (SETQ APP (C:HCNM-CONFIG-GETVAR "AppFolder"))
       (SETQ APPINI (FINDFILE (HCNM_PROJECT_FOLDER_TO_INI APP)))
     )
     (IF (NOT (HAWS-FILE-COPY APPINI PROJINI)) (HCNM_ERROR_NOT_WRITEABLE)) 
     (ALERT
       (PRINC
         (STRCAT
           "CNM is copying settings found in\n" APPINI "\nto\n" PROJINI
           "\nfor this project."
          )
       )
     )
     (WHILE (NOT (FINDFILE PROJINI)))
     (SETQ MARK_FILE_P T)
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
     ;|(SETQ F2 (OPEN PROJINI "w"))
     (PRINC "[CNM]" F2) ; TODO TEST AND REMOVE
     (SETQ F2 (CLOSE F2))|;
     (SETQ *HCNM_CONFIG* (HCNM_CONFIG_DEFAULTS_SINGLE_SCOPE "Project"))
     (HCNM_CONFIG_WRITE_PROJECT PROJ)
     (SETQ *HCNM_CONFIG* (HCNM_CONFIG_DEFAULTS))
     (SETQ MARK_FILE_P T)
     PROJINI
    )
  )
  (COND
    (MARK_FILE_P
     (INI_WRITEENTRY PROJINI "CNM" "ThisFile" PROJINI)
    )
  )
)

;;Saves *HCNM_CONFIG* to this project's ini
(DEFUN HCNM_CONFIG_WRITE_PROJECT (PROJ)
  (INI_WRITESECTION
    (HCNM_INI_NAME
      (COND
        (PROJ)
        ((HCNM_PROJ))
      )
    )
    "CNM"
    *HCNM_CONFIG*
  )
)

(DEFUN HCNM_SET_DIMSTYLE (KEY / DSTY)
  ;;Set dimstyle as requested by calling function and set by user
  ;;First, get dimstyle name
  (SETQ DSTY (c:hcnm-config-getvar KEY))
  ;;Second, if the style is TCGLeader and doesn't already exist, set the _DotSmall ldrblk.
  (COND
    ((AND
       (= KEY "TCGLeaderDimstyle")
       (NOT (TBLSEARCH "DIMSTYLE" DSTY))
     )
     (VL-CMDF "._dim1" "_dimldrblk" "_DotSmall")
    )
  )
  ;;Third, if the desired style exists, save current style for later, then restore the desired style.
  (COND
    ((AND (/= KEY "") (TBLSEARCH "DIMSTYLE" DSTY))
     (SETQ *HCNM_DIMSTYLEOLD* (GETVAR "dimstyle"))
     (VL-CMDF "._dimstyle" "_restore" DSTY)
    )
  )
)
(DEFUN HCNM_RESTORE_DIMSTYLE ()
  (COND
    (*HCNM_DIMSTYLEOLD*
     (VL-CMDF "._dimstyle" "_restore" *HCNM_DIMSTYLEOLD*)
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
(DEFUN HCNM_PROJNOTES (/ APP APPPN format OPT1 PNNAME PROJNOTES)
  (SETQ PNNAME (c:hcnm-config-getvar "ProjectNotes"))
  (IF (= PNNAME "")
    (c:hcnm-config-setvar
      "ProjectNotes"
      (SETQ PNNAME "constnot.txt")
    )
  )
  (HAWS-MILEPOST
    (STRCAT
      "HCNM_PROJNOTES is beginning with ProjectNotes="
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
            (HCNM_PROJ)
            "\\"
            (HAWS-FILENAME-BASE PNNAME)
            (HAWS-FILENAME-EXTENSION PNNAME)
          )
       )
     )
     ;;Record the find in the INI
     (c:hcnm-config-setvar "ProjectNotes" PROJNOTES)
    )
    ;;Third choice, we couldn't find the Project Notes specified,
    ;;so try to get the appropriate style Project Notes from the app folder
    ;;and put it in the location tried above.
    ;;The CFREAD functions will later evaluate the necessity of changing the file
    ;;format and name.
    ((AND
       (SETQ APP (C:HCNM-CONFIG-GETVAR "AppFolder"))
       (SETQ
         format (HCNM_CONFIG_PROJECT_NOTES_FORMAT)
         APPPN
          (FINDFILE
            (STRCAT
              APP
              "\\"
              (COND
                ((= format "txt2") "constnot-default.txt")
                ((= format "csv") "constnot-default.csv")
                (T (alert(princ "\nUnexpected Project Notes format. CNM cannot continue. Contact developer."))(exit))
              )
            )
          )
       )
     )
     ;;If CONSTNOT.TXT was found in the app folder,
     ;;try to copy it to this project.
     (HAWS-FILE-COPY APPPN PROJNOTES)
     ;;Record the find in the INI
     (c:hcnm-config-setvar "ProjectNotes" PROJNOTES)
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

(DEFUN HCNM_GETPROJNOTES (/ DPNAME OLDPROJNOTES PROJNOTES)
  (HCNM_PROJINIT)                       ;Initialize variables in case any files changed.
  (SETQ OLDPROJNOTES (HCNM_PROJNOTES))
  (SETQ DPNAME (STRCAT (GETVAR "dwgprefix") "constnot.txt"))
  (SETQ
    PROJNOTES
     (GETFILED
       "Select Project Notes Filename"
       (c:hcnm-config-getvar "ProjectNotes")
       ""
       37
     )
  )
  ;;Remove path if project notes is in project folder.
  (COND
    ((AND PROJNOTES (= (HAWS-FILENAME-DIRECTORY PROJNOTES) (HCNM_PROJ)))
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

;; HCNM_READCF
;; Reads any acceptable Project Notes file format to a *HCNM_CNMPROJECTNOTES* list of the following format
;; '((0 . "comment")(1 "var" "val1" "val2")(2 . "title")(3 "type" "num" "unit" "count" "text"))
;; The acceptable file formats are:
;; TXT1 Fixed field ;Comment\nSET VAR VAL\nNUM (comment)\nBOX (type)\nTITLE \n1    Text thru column 67...UNTCOUNT\n     Cont. text.
;; TXT2 White space delimited ;Comment\n
;; Excel CSV
;; Doesn't do project management except to write txt2 configs to cnm.ini in the same folder as projnotes.
(DEFUN HCNM_READCF (PROJNOTES / BAKPROJNOTES PNFORMAT RDLIN REQUESTED_FORMAT)
  ;;Do a file read to figure out what the file format is.
  ;;For now, assume that a file that has any of the shape keys followed by a comma ("BOX,", etc.) is CSV
  ;;any other file is TXT2
  (HAWS-MILEPOST
    (STRCAT
      "HCNM_READCF is deciphering the format of "
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
      ((WCMATCH
         (SUBSTR RDLIN 1 3)
         (c:hcnm-config-getvar "NoteTypes")
       )
       (SETQ PNFORMAT "txt2")
      )
    )
  )
  (SETQ F1 (CLOSE F1)
    REQUESTED_FORMAT (HCNM_CONFIG_PROJECT_NOTES_FORMAT)
  )
  (COND
    ((= PNFORMAT "txt2")
     (HCNM_READCFTXT2 PROJNOTES)
     (COND
       ((= REQUESTED_FORMAT "csv")
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
        (HCNM_WRITECFCSV PROJNOTES)
       )
     )
    )
    ((= PNFORMAT "csv")
     (HCNM_READCFCSV PROJNOTES)
     (COND
       ((= REQUESTED_FORMAT "txt2")
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
        (HCNM_WRITECFTXT2 PROJNOTES)
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

(DEFUN HCNM_READCFTXT2 (PROJNOTES / ALERTNOTE ALERTTITLE CFITEM CFLIST
                    CFLIST2 COMMENTBEGIN FILEV42 ILINE ININAME NOTTYP
                    RDLIN VAL1 VAL2 VAR VARLIST N NOTDESC NOTNUM TYPWC
                   )
  (SETQ
    TYPWC
     (c:hcnm-config-getvar "NoteTypes")   ; Get typwc (which may open f1) before opening f1
    F1 (OPEN PROJNOTES "r")
  )
  (WHILE (SETQ RDLIN (READ-LINE F1))
    (COND
      ;;Comment
      ((= ";" (SUBSTR RDLIN 1 1))
       (SETQ CFLIST (CONS (CONS 0 (SUBSTR RDLIN 2)) CFLIST))
      )
      ;;Config setting
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
         ;;If file hasn't been converted yet (configs put in ini)
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
                 (HAWS-RDFLD 1 (SUBSTR RDLIN 77) 12 3) ;Price
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
          ;;Find first note in list (there may be comments before it, or in other words, after it in the file).
          (WHILE (AND
                   (/= 3 (CAR (NTH (SETQ N (1+ N)) CFLIST)))
                   (< N (LENGTH CFLIST))
                 )
          )
          (IF (/= N (LENGTH CFLIST))
            (SETQ
              NOTDESC   (NTH 6 (NTH N CFLIST))
              CFLIST (CONS
                       (SUBST
                         (REVERSE
                           (CONS
                             (HAWS-RDFLD 1 (SUBSTR RDLIN 6 62) 62 1)
                             (COND
                               ((= NOTDESC '("")) NIL)
                               ((REVERSE NOTDESC))
                             )
                           )
                         )
                         NOTDESC
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
  ;;Put configs from v4 Project Notes into CNM.INI and alert.
  (COND
    ((AND (NOT FILEV42) VARLIST)
     (ALERT
       (PRINC
         "\nCNM is moving project settings from version 4.1 Project Notes to CNM.INI."
       )
     )
     (FOREACH
        ENTRY VARLIST
       (c:hcnm-config-setvar
         (HCNM_CONFIG_ENTRY_VAR ENTRY)
         (HCNM_CONFIG_ENTRY_VAL ENTRY)
       )
     )
    )
  )
  (SETQ *HCNM_CNMPROJECTNOTES* (REVERSE CFLIST))
  (HAWS-MILEPOST
    (STRCAT
      "HCNM_READCFTXT2 read "
      (ITOA (LENGTH *HCNM_CNMPROJECTNOTES*))
      " lines from "
      PROJNOTES
      "."
    )
  )
  ;;Add comments and version number to old file.
  (COND
    ((NOT FILEV42)
     (SETQ
       *HCNM_CNMPROJECTNOTES*
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
       ;;If the line is recognizable as a vestige of version 4.1 config settings,
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


(DEFUN HCNM_READCFCSV (PROJNOTES / CFLIST NOTDSCSTR NOTTYP RDLIN TYPWC VAL VAR WRAP
                  )
  (SETQ
    WRAP (ATOI (C:HCNM-CONFIG-GETVAR "DescriptionWrap"))
    TYPWC
     (c:hcnm-config-getvar "NoteTypes")   ; Get typwc (which may open f1) before opening f1
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
              (SETQ VAR (HAWS-RDFLD 2 RDLIN "," 1))
              (SETQ VAL (HAWS-RDFLD 3 RDLIN "," 1))
            )
            CFLIST
          )
       )
       (COND ((= (STRCAT VAR) "WRAP")(SETQ WRAP (ATOI VAL))))
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
            CFLIST 
             (CONS
               (LIST
                 3
                 NOTTYP
                 (COND ((HAWS-RDFLD 2 RDLIN "," 1)) (""))
                 (COND ((HAWS-RDFLD 4 RDLIN "," 1)) (""))
                 (COND ((HAWS-RDFLD 5 RDLIN "," 1)) (""))
                 (COND ((HAWS-RDFLD 6 RDLIN "," 1)) ("")); Price
                 (HCNM_WRAP_DESCRIPTION (COND ((HAWS-RDFLD 3 RDLIN "," 1)) ("")) WRAP)
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
  (SETQ *HCNM_CNMPROJECTNOTES* (REVERSE CFLIST))
)

(DEFUN HCNM_WRAP_DESCRIPTION (NOTDSCSTR WRAP / CHARACTER_I I I_ENDLINE I_NEWLINE_PREV
                          I_NEWWORD_PREV INWORD_P NEED_WRAP_P NOTDSCLST
                          WORD_PROVIDED_P WRAP_EXCEEDED_P
                         )
  (SETQ
    NOTDSCLST NIL
    I_NEWLINE_PREV 1
    I_NEWWORD_PREV 1
    INWORD_P T
    I 0
  )
  (WHILE (<= (SETQ I (1+ I)) (1+ (STRLEN NOTDSCSTR)))
    (SETQ
      CHARACTER_I
       (SUBSTR NOTDSCSTR I 1)
      WRAP_EXCEEDED_P
       (>= (- I I_NEWLINE_PREV) WRAP)
      WORD_PROVIDED_P
       (> I_NEWWORD_PREV I_NEWLINE_PREV)
    )
    (COND ((OR (= CHARACTER_I "") (AND WRAP_EXCEEDED_P WORD_PROVIDED_P)) (SETQ NEED_WRAP_P T)))
    (COND
      ((= "\\n" (SUBSTR NOTDSCSTR I 2))
       (SETQ
         NOTDSCLST
          (CONS
            (LIST I_NEWLINE_PREV (- I I_NEWLINE_PREV))
            NOTDSCLST
          )
         I_NEWLINE_PREV
          (+ I 2)
         I_NEWWORD_PREV
          (+ I 2)
         INWORD_P T
         NEED_WRAP_P NIL
       )
      )
      ((WCMATCH CHARACTER_I " ,\t")
       (SETQ INWORD_P NIL)
      )
      (T
       (COND
         ((AND (/= CHARACTER_I "")(NOT INWORD_P))
          (SETQ
            I_NEWWORD_PREV I
            INWORD_P T
          )
         )
       )
       (COND
         (NEED_WRAP_P
          (SETQ
            I_NEWLINE
             (COND
               ((= CHARACTER_I "") I)
               (T I_NEWWORD_PREV)
             )
            NOTDSCLST
             (CONS (LIST I_NEWLINE_PREV (- I_NEWLINE I_NEWLINE_PREV)) NOTDSCLST)
            I_NEWLINE_PREV
             I_NEWLINE
            NEED_WRAP_P NIL
          )
         )
       )
      )
    )
  )
  (SETQ
    NOTDSCLST
     (MAPCAR
       '(LAMBDA (I) (SUBSTR NOTDSCSTR (CAR I) (CADR I)))
       NOTDSCLST
     )
  )
  (REVERSE NOTDSCLST)
)

;|
(DEFUN HCNM_WRAP_DESCRIPTION_TEST ( / ERRORSTRING NOTDSCSTR WRAP)
  (SETQ
    NOTDSCSTR "A23456789 B23456789 C23456789"
    WRAP 2
    ERRORSTRING "List of assertions violated:"
    ERRORSTRING
     (STRCAT
       ERRORSTRING
       (COND
         ((/= (CAR (HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP))
              "A23456789 "
          )
          "\nMust leave at least one word on each line"
         )
         ("")
       )
     )
    WRAP 11
    ERRORSTRING
     (STRCAT
       ERRORSTRING
       (COND
         ((/= (CAR (HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP))
              "A23456789 "
          )
          "\nMust wrap word 2 to line 3 if it exceeds by many."
         )
         ("")
       )
     )
    WRAP 28
    ERRORSTRING
     (STRCAT
       ERRORSTRING
       (COND
         ((/= (CAR (HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP))
              "A23456789 B23456789 "
          )
          "\nMust wrap word 3 to line 3 if it exceeds by one."
         )
         ("")
       )
     )
    WRAP 21
    ERRORSTRING
     (STRCAT
       ERRORSTRING
       (COND
         ((/= (CAR (HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP))
              "A23456789 B23456789 "
          )
          "\nMust wrap word 3 to line two if it exceeds by many."
         )
         ("")
       )
     )
    WRAP 18
    ERRORSTRING
     (STRCAT
       ERRORSTRING
       (COND
         ((/= (CAR (HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP))
              "A23456789 "
          )
          "\nMust wrap word 2 to line two if it exceeds by one."
         )
         ("")
       )
     )
  )
  ;;(HCNM_WRAP_DESCRIPTION NOTDSCSTR WRAP)
  ERRORSTRING
)
|;

(DEFUN HCNM_WRITECFTXT2 (PROJNOTES / I ITEM NOTTYP NOTTXT NOTTXTNEW)
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
     ITEM *HCNM_CNMPROJECTNOTES*
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
  *HCNM_CNMPROJECTNOTES*
)

(DEFUN HCNM_WRITECFCSV (PROJNOTES / DESC DESCLINE ITEM NOTTYP)
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
     ITEM *HCNM_CNMPROJECTNOTES*
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
  *HCNM_CNMPROJECTNOTES*
)

(DEFUN HCNM_CONFIG_PROJECT_NOTES_FORMAT (/ EDITOR FORMAT VALID_EDITORS)
  (SETQ
    VALID_EDITORS
     (LIST
       (LIST "text" "txt2")
       (LIST "csv" "csv")
       (LIST "cnm" "csv")
     )
    EDITOR
     (C:HCNM-CONFIG-GETVAR "ProjectNotesEditor")
    FORMAT
     (CADR (ASSOC EDITOR VALID_EDITORS))
  )
  (COND
    ((NOT FORMAT)
     (ALERT
       (PRINC
         (STRCAT
           "\nInvalid ProjectNotesEditor. CNM cannot continue.\nUse HCNM-CNMOptions to select your desired editor.\n\nFound ProjectNotesEditor="
           EDITOR
           "\n\nExpected one of these: "
           (apply 'strcat (mapcar '(lambda (x) (strcat "\n" (car x)))valid_editors))
         )
       )
     )
     (EXIT)
    )
  )
  FORMAT
)

;#endregion
;#region Project Notes Editor
;;;================================================================================================================
;;;
;;; Begin Project Notes Editor functions section
;;;
;;;================================================================================================================
(DEFUN C:HCNM-NOTESEDIT (/ CNMEDIT_P NOTESEDITOR PNNAME)
  (SETQ
    NOTESEDITOR (C:HCNM-CONFIG-GETVAR "ProjectNotesEditor")
    ;; wcmatch is legacy hack to be removed when 4.2.29 is deprecated and replaced with translation/conversion on getvar.
    CNMEDIT_P (wcmatch (strcase NOTESEDITOR) "*CNM*")
  )
  (IF CNMEDIT_P
    (haws-core-init 335)
    (haws-core-init 188)
  )
  ;; Since this is a user command, possibly after deletion of project root files,
  ;; refresh project root at beginning.
  (HCNM_PROJINIT)
  ;; Read to convert project notes if necessary before editing
  (SETQ PNNAME (HCNM_PROJNOTES))
  (HCNM_READCF PNNAME)
  (SETQ PNNAME (HCNM_PROJNOTES_MATCH_EXTENSION PNNAME NOTESEDITOR))
  (PRINC (STRCAT "\nEditing " (HCNM_PROJNOTES) "."))
  (COND
    (CNMEDIT_P
     (STARTAPP
       (STRCAT
         "\""
         (C:HCNM-CONFIG-GETVAR "AppFolder")
         "\\CNMEdit.exe"
         "\" "
         "\""
         PNNAME
         "\" "
         "\""
         (HCNM_PROJ)
         "\\cnm.ini\""
       )
     )
    )
    (T (VL-CMDF "._SH"  (STRCAT "\"" PNNAME "\"")))
  )
  (haws-core-restore)
  (PRINC)
)

(DEFUN HCNM_PROJNOTES_MATCH_EXTENSION (PROJNOTES NOTESEDITOR)
  (cond
    ((= NOTESEDITOR "text")(HCNM_CHANGE_FILENAME_EXTENSION PROJNOTES "txt"))
    (T(HCNM_CHANGE_FILENAME_EXTENSION PROJNOTES "csv"))
  )
)

(DEFUN HCNM_CHANGE_FILENAME_EXTENSION
   (OLD_FILENAME NEW_EXTENSION / NEW_FILENAME)
  (COND
    ((/= (HAWS-FILENAME-EXTENSION OLD_FILENAME) NEW_EXTENSION)
     (SETQ
       NEW_FILENAME
        (STRCAT
          (HAWS-FILENAME-DIRECTORY OLD_FILENAME)
          "\\"
          (HAWS-FILENAME-BASE OLD_FILENAME)
          "."
          NEW_EXTENSION
        )
     )
     (VL-FILE-RENAME OLD_FILENAME NEW_FILENAME)
     (C:HCNM-CONFIG-SETVAR "ProjectNotes" NEW_FILENAME)
    )
  )
  NEW_FILENAME
)

;#endregion
;#region Layers Editor
;;;================================================================================================================
;;;
;;; Begin Layers Editor functions section
;;;
;;;================================================================================================================
;; Edit layer defaults
(DEFUN C:HCNM-CNMLAYER (/ LAYERSEDITOR LAYERSFILE WSHSHELL)
(haws-core-init 189)
  (SETQ
    *HAWS:LAYERS* NIL
    LAYERSEDITOR
     ;; wcmatch is legacy hack to be removed when 4.2.29 is deprecated and replaced with translation/conversion on getvar.
     (COND
       ((WCMATCH
          (STRCASE (C:HCNM-CONFIG-GETVAR "LayersEditor"))
          "*CNM*"
        )
        (STRCAT (C:HCNM-CONFIG-GETVAR "AppFolder") "\\CNMLayer.exe")
       )
       (T "notepad.exe")
     )
    LAYERSFILE
     (FINDFILE "layers.dat")
  )
  (STARTAPP
    (STRCAT "\"" LAYERSEDITOR "\" " "\"" LAYERSFILE "\" ")
  )
  (ALERT
    (STRCAT
      "Click OK to import layer settings after editing and saving."
    )
  )
  ;;Get a layer to renew *HAWS:LAYERS*  
  (HAWS-GETLAYR "NOTES-EXPORT")
  (VL-CMDF "._layer")
  (FOREACH
     LAYER *HAWS:LAYERS*
    (VL-CMDF "_n" (CADR LAYER))
    (IF (/= (CADDR LAYER) "")
      (VL-CMDF "_c" (CADDR LAYER) (CADR LAYER))
    )
    (VL-CMDF
      "_lt"
      (IF (TBLSEARCH "LTYPE" (CADDDR LAYER))
        (CADDDR LAYER)
        ""
      )
      (CADR LAYER)
    )
  )
  (VL-CMDF "")
  (haws-core-restore)
  (PRINC)
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
(DEFUN C:HAWS-SETNOTEPHASES (/ CFLIST OPT1 PHASES RDLIN)
(haws-core-init 194)
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
  (VL-CMDF "._insert" (STRCAT "noteqty=noteqty" PHASES))
  (VL-CMDF)
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
       F1     (OPEN (HCNM_PROJNOTES) "r")
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
       F1 (OPEN (HCNM_PROJNOTES) "W")
     )
     (FOREACH RDLIN CFLIST (WRITE-LINE RDLIN F1))
     (SETQ F1 (CLOSE F1))
    )
  )
  (haws-core-restore)
  (PRINC)
)

(DEFUN C:HAWS-CNMMENU ()
(haws-core-init 195)
  (VL-CMDF "._menuunload" "cnm" "._menuload" "cnm.mnu")
  (haws-core-restore)
)

(DEFUN C:HAWS-CNMSETUP (/ ACADPATHPREFIX ACADPATHSUFFIX I OLDACADPATH
                    OLDPROGRAMFOLDER PROGRAMFOLDER MATCHLENGTH
                   )
(haws-core-init 196)
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
  (VL-CMDF "._menuunload" "cnm" "._menuload" "cnm")
  (VL-FILE-DELETE (STRCAT PROGRAMFOLDER "\\acaddoc.lsp"))
  (ALERT
    "Construction Notes Manager setup is done.\n\nYou may now explore the CNM menus and toolbar\nafter restarting AutoCAD."
  )
  (haws-core-restore)
)
(DEFUN C:HAWS-NTPURGE (/ OL PL PLSS)
(haws-core-init 197)
  (SETQ
    OL (GETVAR "clayer")
    PL (CAR (HAWS-GETLAYR "NOTESEXP"))
  )
  (VL-CMDF "._erase" (SSGET "X" (LIST (CONS 8 PL))) "")
  (SETVAR "clayer" OL)
  (VL-CMDF "._purge" "_b" "noteqty*,cnm*" "_n")
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
  (haws-core-restore)
  (PRINC)
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
(DEFUN C:HCNM-SETNOTESBUBBLESTYLE (/ BUBBLEHOOKS)
(haws-core-init 190)
  (INITGET "Yes No")
  (SETQ
    BUBBLEHOOKS
     (GETKWORD
       "\nInsert bubble notes with hooks? [Yes/No]: "
     )
  )
  (IF BUBBLEHOOKS
    (c:hcnm-config-setvar
      "BubbleHooks"
      (COND
        ((= BUBBLEHOOKS "Yes") "1")
        ("0")
      )
    )
  )
  (haws-core-restore)
  (PRINC)
)
;;; Global edit of bubble note phases
(DEFUN C:HAWS-PHASEEDIT (/ NEWPHASE OLDPHASE)
(haws-core-init 191)
  (SETQ
    OLDPHASE
     (GETSTRING "\nEnter phase to change: ")
    NEWPHASE
     (GETSTRING "\nEnter new phase: ")
  )
  (VL-CMDF
    "._attedit" "_n" "_n" "note???l,note???r" "notephase" "*" OLDPHASE
    NEWPHASE
   )
  (GRAPHSCR)
  (haws-core-restore)
  (PRINC)
)
;;; Put attributes on NOPLOT layer
(DEFUN C:HCNM-ATTNOPLOT ()
(haws-core-init 192)
  (HCNM_ATTLAYER "NOTESNOPLOT")
  (VL-CMDF
    "._layer"
    "_Plot"
    "_No"
    (HAWS-GETLAYR "NOTESNOPLOT")
    ""
  )
  (haws-core-restore)
)
(DEFUN C:HCNM-ATTPLOT () (HCNM_ATTLAYER "0"))
(DEFUN HCNM_ATTLAYER (LAYER / AT EL EN ET NPLAYER NPLIST SSET SSLEN)
  (haws-core-init 193)
  (HAWS-VSAVE '("CLAYER"))
  (VL-CMDF "._undo" "_g")
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
  (VL-CMDF "._undo" "_e")
  (HAWS-VRSTOR)
  (haws-core-restore)
  (PRINC)
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
(DEFUN C:HAWS-TCG ()
(haws-core-init 208)
  (HAWS-LDRBLK
    "ldrtcgl" "ldrtcgr" "ldrtcgd" "TCGLDR" "TCGLeader"
   )
  (haws-core-restore)
)
(DEFUN C:HAWS-TXTL ()
  (HAWS-LDRBLK
    "ldrtxtl" "ldrtxtr" "ldrtxtd" "NOTESLDR" "NotesLeader"
   )
)

(DEFUN HAWS-LDRBLK (BLLEFT BLRGHT BLDRAG BLLAY BLDSTY / APOLD AS ASSOCIATE_P ANG AUOLD
                BLGF BLLINE BLK DSTY DSTYOLD DTOLD EL EN ENBLK ENDRAG
                FIXHOOK FIXPHASE FIXTXT3 I P1 P2 P3 P4 P5 P6 P7 P8
                PFOUND R1 DS TS LEFT NUM TXT1 TXT2 ANG1 ANG2 FIXORDER
                OSMOLD
               )
  (haws-core-init 209)
  (HAWS-VSAVE
    '("aperture" "attdia" "attreq" "aunits" "clayer" "cmdecho" "osmode"
      "plinegen" "regenmode"
     )
  )
  (VL-CMDF "._undo" "_g")
  ;; Block isn't annotative. Can't associate with annotative leader.
  (SETQ
    ASSOCIATE_P
     (COND
       ((= (GETVAR "DIMANNO") 1) NIL)
       (T)
     )
  )
  (HCNM_PROJINIT)
  (HCNM_SET_DIMSTYLE (STRCAT BLDSTY "Dimstyle"))
  (SETVAR "osmode" 0)
  (HAWS-MKLAYR BLLAY)
  (SETQ
    P1 (GETPOINT "\nStart point for leader:")
  )
  (SETQ
    DS (HAWS-DWGSCALE)
    TS (* DS (GETVAR "dimtxt"))
    AS (* DS (GETVAR "dimasz"))
  )
  (VL-CMDF
    "._insert"
    BLDRAG
    "_Scale"
    TS
    "_Rotate"
    (ANGTOS (GETVAR "snapang"))
    P1
  )
  (SETQ EN (ENTLAST))
  (PROMPT "\nEnd point for leader: ")
  (VL-CMDF "._move" EN "" P1 PAUSE)
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
  (SETVAR "attdia" 0)
  (SETVAR "attreq" 1)
  (COND
    ((>= (ATOF (GETVAR "acadver")) 14)
     (VL-CMDF "._leader" P1 P2 "" "")
     (COND
       (ASSOCIATE_P (VL-CMDF "_block"))
       (T (VL-CMDF "_none" "._INSERT"))
     )
    )
  )
  (SETQ AUOLD (GETVAR "aunits"))
  (SETVAR "aunits" 3)
  (VL-CMDF
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
  (VL-CMDF "._erase" ENDRAG "")
  (IF (NOT (ENTNEXT (ENTLAST)))
    (VL-CMDF
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
  (HCNM_RESTORE_DIMSTYLE)
  (HAWS-VRSTOR)
  (VL-CMDF "._undo" "_e")
  (haws-core-restore)
  (PRINC)
)
;;end of LDRBLK

;#endregion
;#region Bubble insertion and editing

(DEFUN C:HAWS-BOXL () (haws-core-init 198) (HCNM_LDRBLK_DYNAMIC "BOX")(haws-core-restore))
(DEFUN C:HAWS-CIRL () (haws-core-init 199) (HCNM_LDRBLK_DYNAMIC "CIR")(haws-core-restore))
(DEFUN C:HAWS-DIAL () (haws-core-init 200) (HCNM_LDRBLK_DYNAMIC "DIA")(haws-core-restore))
(DEFUN C:HAWS-ELLL () (haws-core-init 201) (HCNM_LDRBLK_DYNAMIC "ELL")(haws-core-restore))
(DEFUN C:HAWS-HEXL () (haws-core-init 202) (HCNM_LDRBLK_DYNAMIC "HEX")(haws-core-restore))
(DEFUN C:HAWS-OCTL () (haws-core-init 203) (HCNM_LDRBLK_DYNAMIC "OCT")(haws-core-restore))
(DEFUN C:HAWS-PENL () (haws-core-init 204) (HCNM_LDRBLK_DYNAMIC "PEN")(haws-core-restore))
(DEFUN C:HAWS-RECL () (haws-core-init 205) (HCNM_LDRBLK_DYNAMIC "REC")(haws-core-restore))
(DEFUN C:HAWS-SSTL () (haws-core-init 206) (HCNM_LDRBLK_DYNAMIC "SST")(haws-core-restore))
(DEFUN C:HAWS-TRIL () (haws-core-init 207) (HCNM_LDRBLK_DYNAMIC "TRI")(haws-core-restore))
(DEFUN C:HCNM-REPLACE-BUBBLE () (haws-core-init 338) (HCNM_LDRBLK_DYNAMIC NIL))

(DEFUN HCNM_LDRBLK_DYNAMIC (NOTETYPE / BLOCKNAME BUBBLE_DATA BUBBLEHOOKS
                        ENAME_BLOCK_OLD P1_DATA P2_DATA REPLACE_BLOCK_P
                        TH
                       )
  (HAWS-VSAVE '("attreq" "aunits" "clayer" "cmdecho"))
  (COND
    ((AND (GETVAR "wipeoutframe") (/= (GETVAR "wipeoutframe") 2))
     (ALERT "Setting WIPEOUTFRAME to 2 to show but not plot")
     (SETVAR "wipeoutframe" 2)
    )
  )
  (COND
    ((= (GETVAR "ANNOALLVISIBLE") 0)
     (INITGET "Yes No")
     (COND
       ((/=
          (GETKWORD
            "ANNOALLVISIBLE is 0. Set to 1 so that bubble notes appear at all scales? [Yes/No] <Yes>: "
          )
          "No"
        )
        (SETVAR "ANNOALLVISIBLE" 1)
       )
     )
    )
  )
  (VL-CMDF "._undo" "_g")
  (HCNM_PROJINIT)
  (HCNM_SET_DIMSTYLE "NotesLeaderDimstyle")
  (SETQ
    BUBBLEHOOKS
     (C:HCNM-CONFIG-GETVAR "BubbleHooks")
    BLOCKNAME
     (STRCAT
       "cnm-bubble-"
       (HCNM_LDRBLK_GET_MTEXT_STRING)
       (COND
         ((= (STRCASE BUBBLEHOOKS) "YES") "1")
         ((= (STRCASE BUBBLEHOOKS) "NO") "0")
         (T BUBBLEHOOKS)
       )
     )
    TH (* (GETVAR "dimtxt")
          (IF (GETVAR "DIMANNO")
            1
            (GETVAR "DIMTXT")
          )
       )
  )
  (HAWS-MKLAYR "NOTESLDR")
  (SETVAR "attreq" 0)
  (SETQ
    REPLACE_BLOCK_P
     (NOT NOTETYPE)
    ENAME_BLOCK_OLD
     (HCNM_LDRBLK_GET_ENAME_BLOCK_OLD REPLACE_BLOCK_P)
    P1_DATA
     (HCNM_LDRBLK_GET_P1_DATA ENAME_BLOCK_OLD)
    NOTETYPE
     (COND
       (NOTETYPE)
       ((LM:GETDYNPROPVALUE
          (VLAX-ENAME->VLA-OBJECT (CADR P1_DATA))
          "Shape"
        )
       )
     )
    P2_DATA
     (HCNM_LDRBLK_GET_P2_DATA P1_DATA TH BLOCKNAME NOTETYPE)
    ;; bubble-data-update: Refactored to Draw bubble, get data, then finish bubble. 
    ;; Hopefully this is more readable.
  )
  (HCNM_LDRBLK_DRAW_BUBBLE
    P1_DATA P2_DATA BUBBLE_DATA TH NOTETYPE BLOCKNAME
   )
  (SETQ BUBBLE_DATA (HCNM_LDRBLK_GET_BUBBLE_DATA P1_DATA))
  (HCNM_LDRBLK_FINISH_BUBBLE P1_DATA BUBBLE_DATA NOTETYPE)
  (PRINC "\nUse the ATTIPEDIT command to edit bubble note.")
  (HCNM_RESTORE_DIMSTYLE)
  (HAWS-VRSTOR)
  (VL-CMDF "._undo" "_e")
  (HAWS-CORE-RESTORE)
  (PRINC)
)
(DEFUN HCNM_LDRBLK_GET_ENAME_BLOCK_OLD (REPLACE_BLOCK_P / ELIST_BLOCK_OLD ENAME_BLOCK_OLD)
  (COND
    (REPLACE_BLOCK_P
     ;; Prompt and check for old block.
     (WHILE (OR (NOT
                  (SETQ
                    ENAME_BLOCK_OLD
                     (CAR (ENTSEL "\nSelect bubble note: "))
                  )
                )
                (NOT (SETQ ELIST_BLOCK_OLD (ENTGET ENAME_BLOCK_OLD)))
                (NOT
                  (AND
                    (= (CDR (ASSOC 0 ELIST_BLOCK_OLD)) "INSERT")
                    (WCMATCH
                      (STRCASE
                        (VLA-GET-EFFECTIVENAME
                          (VLAX-ENAME->VLA-OBJECT ENAME_BLOCK_OLD)
                        )
                      )
                      "CNM-BUBBLE-*"
                    )
                  )
                )
            )
       (PRINC "\nSelected entity is not a CNM bubble note.")
     )
     ENAME_BLOCK_OLD
    )
    (T NIL)
  )
)
(DEFUN HCNM_LDRBLK_GET_P1_DATA (ENAME_BLOCK_OLD / ELIST_BLOCK_OLD ENAME_330 ENAME_LEADER_OLD P1_DATA P1_ENTRY REPLACE_BLOCK_P)
  (COND
    (ENAME_BLOCK_OLD
     (SETQ ELIST_BLOCK_OLD (ENTGET ENAME_BLOCK_OLD)
       REPLACE_BLOCK_P T
     )
     ;; Get start point
     ;; Find associated leader.
     (WHILE ;; Check all 330 groups
            (AND
              (NOT ENAME_LEADER_OLD)
              (SETQ ENAME_330 (CDR (ASSOC 330 ELIST_BLOCK_OLD)))
            )
       ;; Use the one that refers back to this block. Or move to the next one.
       (COND
         ((EQ (CDR (ASSOC 340 (ENTGET ENAME_330))) ENAME_BLOCK_OLD)
          (SETQ ENAME_LEADER_OLD ENAME_330)
         )
         (T
          (SETQ
            ELIST_BLOCK_OLD
             (CDR
               (MEMBER
                 (ASSOC 330 ELIST_BLOCK_OLD)
                 ELIST_BLOCK_OLD
               )
             )
            ENAME_LEADER_OLD NIL
          )
         )
       )
     )
     (SETQ
       P1_ENTRY (COND
            (ENAME_LEADER_OLD
             (CDR (ASSOC 10 (ENTGET ENAME_LEADER_OLD)))
            )
            (T (CDR (ASSOC 10 ELIST_BLOCK_OLD)))
          )
     )
    )
    (T 
       (SETQ P1_ENTRY (GETPOINT "\nStart point for leader:"))
    )
  )
  (SETQ P1_DATA (LIST P1_ENTRY ENAME_BLOCK_OLD ENAME_LEADER_OLD REPLACE_BLOCK_P))
)
(DEFUN HCNM_LDRBLK_GET_P2_DATA
   (P1_DATA TH BLOCKNAME NOTETYPE / BLOCK_DATA ENAME_BLOCK P1_ENTRY P2 P2_DATA SS1 VLAOBJ)
  (SETQ
    P1_ENTRY (CAR P1_DATA)
    SS1 (SSADD)
  )
  (FOREACH
     FLIPSTATE '("right" "left")
    (VL-CMDF
      "._insert"
      (STRCAT BLOCKNAME "-" FLIPSTATE)
      "_Scale"
      TH
      "_Rotate"
      (ANGTOS (GETVAR "snapang"))
      P1_ENTRY
    )
    (SETQ
      ENAME_BLOCK
       (ENTLAST)
      VLAOBJ
       (VLAX-ENAME->VLA-OBJECT ENAME_BLOCK)
    )
    (LM:SETDYNPROPVALUE VLAOBJ "Shape" NOTETYPE)
    (SSADD ENAME_BLOCK SS1)
  )
  (PROMPT "\nLocation for bubble: ")
  (VL-CMDF "._MOVE" SS1 "" P1_ENTRY PAUSE)
  (SETQ
    P2     (TRANS (CDR (ASSOC 10 (ENTGET ENAME_BLOCK))) ENAME_BLOCK 1)
  )
  (VL-CMDF "._erase" SS1 "")
  (SETQ P2_DATA (LIST P2))
)
(DEFUN HCNM_LDRBLK_DRAW_BUBBLE (P1_DATA P2_DATA ATTRIBUTE_LIST TH NOTETYPE BLOCKNAME / ANG1
                     ASSOCIATE_P ATTRIBUTES_OLD AUOLD BLOCK_DATA ELIST_LEADER_OLD
                     ENAME_BLOCK_NEW ENAME_BLOCK_OLD ENAME_LEADER
                     ENAME_LEADER_OLD ETYPE FLIPSTATE INPUT1 P1_ENTRY P2
                     REPLACE_BLOCK_P
                    )
  (SETQ
    P1_ENTRY
     (CAR P1_DATA)
    ENAME_BLOCK_OLD
     (CADR P1_DATA)
    ENAME_LEADER_OLD
     (CADDR P1_DATA)
    REPLACE_BLOCK_P
     (CADDDR P1_DATA)
    P2 (CAR P2_DATA)
    ANG1
     (- (ANGLE P1_ENTRY P2) (GETVAR "snapang"))
    FLIPSTATE
     (COND
       ((MINUSP (COS ANG1)) "left")
       (T "right")
     )
  )
  (COND
    ;; If it's not a new insertion, don't draw a leader.
    (ENAME_BLOCK_OLD
     (SETQ AUOLD (GETVAR "aunits"))
     (SETVAR "aunits" 3)
     (VL-CMDF
       "._insert"
       (STRCAT BLOCKNAME "-" FLIPSTATE)
       "_Scale"
       TH
       "_Rotate"
       (GETVAR "snapang")
       P2
     )
     (SETVAR "aunits" AUOLD)
     ;; If there is an old leader, stretch it and associate it.
     (COND
       (ENAME_LEADER_OLD
        (SETQ ELIST_LEADER_OLD (ENTGET ENAME_LEADER_OLD))
        ;; Change its arrowhead if needed.
        (HCNM_LDRBLK_CHANGE_ARROWHEAD ENAME_LEADER_OLD)
        ;; Stretch it.
        (ENTMOD
          (SUBST
            (CONS 10 P2)
            (ASSOC
              10
              (CDR
                (MEMBER (ASSOC 10 ELIST_LEADER_OLD) ELIST_LEADER_OLD)
              )
            )
            ELIST_LEADER_OLD
          )
        )
        ;; Associate it.
        (VL-CMDF
          "._qldetachset"
          ENAME_LEADER_OLD
          ""
          "._qlattach"
          ENAME_LEADER_OLD
          (ENTLAST)
          "._draworder"
          (ENTLAST)
          ""
          "_front"
        )
       )
     )
    )
    (T
     (SETQ
       ASSOCIATE_P
        (COND
          ((= (GETVAR "DIMANNO") 1) T)
          (NIL)
        )
     )
     (COND
       ((AND
          (NOT ASSOCIATE_P)
          (GETVAR "CANNOSCALEVALUE")
          (/= (GETVAR "DIMSCALE") (/ 1.0 (GETVAR "CANNOSCALEVALUE")))
        )
        (ALERT
          (PRINC
            (STRCAT
              "\nDimension scale ("
              (RTOS (GETVAR "DIMSCALE") 2 2)
              ") and\nAnnotation scale ("
              (RTOS (/ 1.0 (GETVAR "CANNOSCALEVALUE")) 2 2)
              ")\nare not equal.\nCNM recommends setting dimension scale to match annotation scale."
            )
          )
        )
        (INITGET 1 "Yes No")
        (SETQ
          INPUT1
           (GETKWORD
             "\nSet dimension scale to match annotation scale? [Yes/No]: "
           )
        )
        (COND
          ((= INPUT1 "Yes")
           (SETVAR "DIMSCALE" (/ 1.0 (GETVAR "CANNOSCALEVALUE")))
          )
        )
       )
     )
     (SETQ
       ANG1      (- (ANGLE P1_ENTRY P2) (GETVAR "snapang"))
       FLIPSTATE (COND
                   ((MINUSP (COS ANG1)) "left")
                   (T "right")
                 )
     )
     (SETQ ENAME_LEADER (ENTLAST))
     ;;Start insertion
     (COND
       ((>= (ATOF (GETVAR "acadver")) 14)
        (VL-CMDF "._leader" P1_ENTRY P2 "_Annotation" "")
        (COND
          (ASSOCIATE_P (VL-CMDF "_block"))
          (T (VL-CMDF "_none" "._INSERT"))
        )
       )
       (T
        (ALERT
          (PRINC
            "\nThe bubble notes inserter in CNM 4.2.3 and higher is not compatible with AutoCAD pre-R14."
          )
        )
       )
     )
     (SETQ AUOLD (GETVAR "aunits"))
     (SETVAR "aunits" 3)
     (VL-CMDF
       (STRCAT BLOCKNAME "-" FLIPSTATE)
       "_Scale"
       TH
       P2
       (GETVAR "snapang")
     )
     (SETVAR "aunits" AUOLD)
    )
  )
)
(DEFUN HCNM_LDRBLK_GET_BUBBLE_DATA (P1_DATA / ATTRIBUTE_LIST BLOCK_DATA ENAME_BLOCK_OLD INDEX NUM P1_ENTRY)
  (SETQ
    P1_ENTRY
     (CAR P1_DATA)
    ENAME_BLOCK_OLD
     (CADR P1_DATA)
  )
  (COND
    (ENAME_BLOCK_OLD
     (SETQ ATTRIBUTE_LIST (HCNM_GET_ATTRIBUTES ENAME_BLOCK_OLD T))
    )
    (T
     (INITGET 128 "Copy")
     (SETQ NUM (GETKWORD "\nNote number or [Copy note]: "))
     (COND
       ((= NUM "Copy")
        (SETQ
          ATTRIBUTE_LIST
           (HCNM_GET_ATTRIBUTES
             (SETQ ENAME_BLOCK_OLD (CAR (ENTSEL)))
             T
           )
        )
       )
       (T
        (SETQ ATTRIBUTE_LIST (HCNM_LDRBLK_INITIALIZE_ATTRIBUTE_LIST))
        ;; This is start point 1 of 2 of the bubble data logic. This start point is for the bubble note creation process.
        ;; This is one place where ATTRIBUTE_LIST gets created. The other is when a bubble note to be edited or copied has its attributes read in HCNM_GET_ATTRIBUTES.
        (MAPCAR
          '(LAMBDA (INDEX)
             (SETQ
               ATTRIBUTE_LIST
                (HCNM_LDRBLK_GET_TEXT_ENTRY
                  ENAME_BLOCK_OLD
                  P1_ENTRY
                  INDEX
                  ATTRIBUTE_LIST
                )
             )
           )
          '(1 2 3 4 5 6 0)
        )
       )
     )
    )
  )
  (HCNM_LDRBLK_ADJUST_FORMATS ATTRIBUTE_LIST)
)
(DEFUN HCNM_LDRBLK_FINISH_BUBBLE (P1_DATA ATTRIBUTE_LIST NOTETYPE
                              / ENAME_BLOCK_NEW
                              ENAME_BLOCK_OLD ENAME_LEADER ETYPE
                             )
  (SETQ
    ENAME_BLOCK_OLD
     (CADR P1_DATA)
    REPLACE_BLOCK_P
     (CADDDR P1_DATA)
    ENAME_BLOCK_NEW
     (ENTLAST)
  )
  (HCNM_LDRBLK_SET_DYNPROPS
    ENAME_BLOCK_NEW
    ENAME_BLOCK_OLD
    NOTETYPE
    REPLACE_BLOCK_P
  )
  (IF REPLACE_BLOCK_P
    (ENTDEL ENAME_BLOCK_OLD)
  )
  (HCNM_SET_ATTRIBUTES ENAME_BLOCK_NEW ATTRIBUTE_LIST)
  ;; Change leader arrowhead if needed.
  (WHILE
    (AND
      (= (C:HCNM-CONFIG-GETVAR "BubbleArrowIntegralPending") "1")
      (/= "LEADER"
          (CDR
            (ASSOC 0 (ENTGET (SETQ ENAME_LEADER (ENTNEXT ENAME_LEADER))))
          )
      )
    )
  )
  (HCNM_LDRBLK_CHANGE_ARROWHEAD ENAME_LEADER)
)
(DEFUN HCNM_LDRBLK_GET_MTEXT_STRING ()
  (COND
    ((= (C:HCNM-CONFIG-GETVAR "BubbleMtext") "1") "m-")
    (T "")
  )
)
(DEFUN HCNM_LDRBLK_CHANGE_ARROWHEAD (ENAME_LEADER)
  (COND
    ((= (C:HCNM-CONFIG-GETVAR "BubbleArrowIntegralPending") "1")
     ;; 18 is "Integral" arrowhead type.
     (VLA-PUT-ARROWHEADTYPE
       (VLAX-ENAME->VLA-OBJECT ENAME_LEADER)
       18
     )
     (C:HCNM-CONFIG-SETVAR "BubbleArrowIntegralPending" "0")
    )
  )
)
(DEFUN HCNM_LDRBLK_SET_DYNPROPS (ENAME_BLOCK_NEW ENAME_BLOCK_OLD NOTETYPE REPLACE_BLOCK_P /  DYN_PROPS_OLD DYN_PROPS_OLD_I VLAOBJ_BLOCK_NEW VLAOBJ_BLOCK_OLD)
  (SETQ
    VLAOBJ_BLOCK_NEW
     (VLAX-ENAME->VLA-OBJECT ENAME_BLOCK_NEW)
  )
  (COND
    (ENAME_BLOCK_OLD
     (SETQ
       VLAOBJ_BLOCK_OLD
        (VLAX-ENAME->VLA-OBJECT ENAME_BLOCK_OLD)
       DYN_PROPS_OLD
        (MAPCAR
          '(LAMBDA (X)
             (LIST
               (VLAX-GET-PROPERTY X 'PROPERTYNAME)
               (VLAX-GET-PROPERTY X 'VALUE)
               X
             )
           )
          (VLAX-INVOKE
            VLAOBJ_BLOCK_OLD
            'GETDYNAMICBLOCKPROPERTIES
          )
        )
     )
     (FOREACH
        VLAOBJ_PROPERTY_NEW
        (VLAX-INVOKE VLAOBJ_BLOCK_NEW 'GETDYNAMICBLOCKPROPERTIES)
       (IF (AND
             (SETQ
               DYN_PROPS_OLD_I
                (ASSOC
                  (VLAX-GET-PROPERTY
                    VLAOBJ_PROPERTY_NEW
                    'PROPERTYNAME
                  )
                  DYN_PROPS_OLD
                )
             )
             (/= (VLAX-GET-PROPERTY VLAOBJ_PROPERTY_NEW 'READONLY)
                 :VLAX-TRUE
             )
           )
         (VLAX-PUT-PROPERTY
           VLAOBJ_PROPERTY_NEW
           'VALUE
           (CADR DYN_PROPS_OLD_I)
         )
       )
     )
    )
    (T (LM:SETDYNPROPVALUE VLAOBJ_BLOCK_NEW "Shape" NOTETYPE))
  )
)
(DEFUN HCNM_LDRBLK_INITIALIZE_ATTRIBUTE_LIST (/ ATTRIBUTE_LIST)
  (SETQ
    ATTRIBUTE_LIST
     (MAPCAR
       '(LAMBDA (INDEX)
          (LIST
            (STRCAT "NOTETXT" (ITOA INDEX))
            ""
          )
        )
       '(1 2 3 4 5 6 0)
     )
    ATTRIBUTE_LIST
     (CONS (LIST "NOTEDATA" "") ATTRIBUTE_LIST)
    ATTRIBUTE_LIST
     (CONS (LIST "NOTEGAP" "") ATTRIBUTE_LIST)
    ATTRIBUTE_LIST
     (CONS (LIST "NOTENUM" NUM) ATTRIBUTE_LIST)
  )
  ATTRIBUTE_LIST
)
(DEFUN HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST (TAG VALUE ATTRIBUTE_LIST / )
 (SUBST (LIST TAG VALUE) (ASSOC TAG ATTRIBUTE_LIST) ATTRIBUTE_LIST)
)
;; Adds object to notedata attribute's list if it's not already in the list.
;; Returns ATTRIBUTE_LIST
;; THIS FUNCTION LIKELY NOT NEEDED SINCE IT WAS FOR REACTORS AND WE ARE PUTTING ALL THE SAME INFO IN REACTOR DATA. LEARNING PROCESS
(DEFUN HCNM_LDRBLK_SAVE_OBJECT_REFERENCE_TO_NOTEDATA (LINE_TAG OBJECT DATA_KEY ATTRIBUTE_LIST / DATA_KEY_OLD HANDLE HANDLE_OLD LINE_DATA LINE_DATA_OLD LINE_KEY LINE_TAG NOTEDATA)
  (SETQ
    LINE_KEY (SUBSTR LINE_TAG 8 1)
    HANDLE (VLA-GET-HANDLE OBJECT)
    LINE_DATA (LIST LINE_KEY HANDLE DATA_KEY)
    NOTEDATA (READ (CADR(ASSOC "NOTEDATA" ATTRIBUTE_LIST)))
    LINE_DATA_OLD (ASSOC LINE_KEY NOTEDATA)
    HANDLE_OLD (CADR LINE_DATA_OLD)
    DATA_KEY_OLD (CADDR LINE_DATA_OLD)
  )
  (COND 
    ((NOT LINE_DATA_OLD) (SETQ NOTEDATA (CONS LINE_DATA NOTEDATA)))
    ((OR (/= HANDLE_OLD HANDLE) (/= DATA_KEY_OLD DATA_KEY))(SETQ NOTEDATA (SUBST LINE_DATA LINE_DATA_OLD NOTEDATA)))
  )
  (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST "NOTEDATA" (VL-PRIN1-TO-STRING NOTEDATA) ATTRIBUTE_LIST)
)
(DEFUN HCNM_LDRBLK_ADJUST_FORMATS (ATTRIBUTE_LIST / BUBBLEMTEXT TXT1 TXT2
                               GAP OVERLINE UNDERLINE
                              )
  ;; Adjust underlining and overlining
  (SETQ
    BUBBLEMTEXT
     (HCNM_LDRBLK_GET_MTEXT_STRING)
    UNDERLINE
     (COND
       ((= BUBBLEMTEXT "") "%%u")
       (T "\\L")
     )
    OVERLINE
     (COND
       ((= BUBBLEMTEXT "") "%%o")
       (T "\\O")
     )
    TXT1
     (HCNM_LDRBLK_ADJUST_FORMAT
       (CADR (ASSOC "NOTETXT1" ATTRIBUTE_LIST))
       UNDERLINE
     )
    TXT2
     (HCNM_LDRBLK_ADJUST_FORMAT
       (CADR (ASSOC "NOTETXT2" ATTRIBUTE_LIST))
       OVERLINE
     )
    GAP
     (COND
       ((= TXT1 TXT2 "") "")
       (T "%%u ")
     )
    ATTRIBUTE_LIST
     (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST
       "NOTETXT1"
       TXT1
       ATTRIBUTE_LIST
     )
    ATTRIBUTE_LIST
     (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST
       "NOTETXT2"
       TXT2
       ATTRIBUTE_LIST
     )
    ATTRIBUTE_LIST
     (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST
       "NOTEGAP"
       GAP
       ATTRIBUTE_LIST
     )
  )
  ;; Strip mtext codes if not using mtext
  ;; Disabled because LM:Unformat breaks field codes
  ;|(COND
    ((= BUBBLEMTEXT "")
     (SETQ
       ATTRIBUTE_LIST
        (MAPCAR
          '(LAMBDA (ATTRIBUTE)
             (COND
               ((WCMATCH (CADR ATTRIBUTE) "*\\*")
                (LIST
                  (CAR ATTRIBUTE)
                  (LM:UNFORMAT (CADR ATTRIBUTE) NIL)
                )
               )
               (ATTRIBUTE)
             )
           )
          ATTRIBUTE_LIST
        )
     )
    )
  )
  |;
)
;;; Underline or overline string unless it's empty.
(DEFUN HCNM_LDRBLK_ADJUST_FORMAT (STRING CODE)
  (COND
    ;; If empty, do nothing
    ((= STRING "") STRING)
    ;; If already underlined or overlined, strip that. Assumes that's the first code.
    ((WCMATCH STRING "\\*") (STRCAT CODE (SUBSTR STRING 3)))
    ((WCMATCH STRING "%%*") (STRCAT CODE (SUBSTR STRING 4)))
    ;; Otherwise just underline or overline.
    (T (STRCAT CODE STRING))
  )
)
(DEFUN HCNM_LDRBLK_GET_TEXT_ENTRY (ENAME_BLOCK P1_ENTRY LINE_NUMBER ATTRIBUTE_LIST /
                               SKIP_ENTRY_P INPUT LOOP-P PROMPT-P STRING TAG
                              )
  (SETQ
    LOOP-P T
    PROMPT-P
     (= (C:HCNM-CONFIG-GETVAR
          (STRCAT "BubbleTextLine" (ITOA LINE_NUMBER) "PromptP")
        )
        "1"
     )
    SKIP_ENTRY_P
     (= (C:HCNM-CONFIG-GETVAR "BubbleSkipEntryPrompt") "1")
    STRING ""
    TAG
     (STRCAT "NOTETXT" (ITOA LINE_NUMBER))
  )
  (WHILE (AND PROMPT-P LOOP-P)
    (COND
      ((OR SKIP_ENTRY_P
           (= (SETQ
                INPUT
                 (GETSTRING
                   1
                   (STRCAT
                     "\nLine "
                     (ITOA LINE_NUMBER)
                     " text or . for automatic text: "
                   )
                 )
              )
              "."
           )
       )
       (SETQ
         ATTRIBUTE_LIST
          (HCNM_LDRBLK_GET_AUTO_TYPE
            ENAME_BLOCK
            P1_ENTRY
            LINE_NUMBER
            TAG
            ATTRIBUTE_LIST
          )
         STRING
          (CADR (ASSOC TAG ATTRIBUTE_LIST))
       )
      )
      (T
       (SETQ
         STRING INPUT
         ATTRIBUTE_LIST
          (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST
            TAG
            STRING
            ATTRIBUTE_LIST
          )
       )
      )
    )
    (SETQ
      SKIP_ENTRY_P
       (AND SKIP_ENTRY_P (/= STRING "ENtry"))
      LOOP-P
       (OR (NOT STRING) (= STRING "ENtry"))
    )
  )
  ATTRIBUTE_LIST
)
;;; bubble-data-update: I believe that this needs to also define any data reference types.
(DEFUN HCNM_LDRBLK_GET_AUTO_TYPE_KEYS ()
  ;; Input Key Reference_type
  '(
    ("Lf" "LF" nil)
    ("SF" "SF" nil)
    ("SY" "SY" nil)
    ("STa" "Sta" "AL")
    ("Off" "Off" "AL")
    ("stAoff" "StaOff" "AL")
    ("N" "N" nil)
    ("E" "E" nil)
    ("Z" "Z" nil)
    ("Text" "Text" nil)
    ("ENtry" "ENtry" nil)
  )
)
(DEFUN HCNM_LDRBLK_GET_AUTO_TYPE (ENAME_BLOCK P1_ENTRY LINE_NUMBER TAG ATTRIBUTE_LIST /
                              CVPORT_OLD HAWS-QT-NEW INPUT SPACE STRING
                             )
  (INITGET
    (SUBSTR
      (APPLY
        'STRCAT
        (MAPCAR
          '(LAMBDA (X) (STRCAT " " (CAR X)))
          (HCNM_LDRBLK_GET_AUTO_TYPE_KEYS)
        )
      )
      2
    )
  )
  (SETQ
    INPUT
     (GETKWORD
       (STRCAT
         "\nLine "
         (ITOA LINE_NUMBER)
         " automatic text. Enter an option ["
         (SUBSTR
           (APPLY
             'STRCAT
             (MAPCAR
               '(LAMBDA (X) (STRCAT "/" (CAR X)))
               (HCNM_LDRBLK_GET_AUTO_TYPE_KEYS)
             )
           )
           2
         )
         "] <"
         (CAR (LAST (HCNM_LDRBLK_GET_AUTO_TYPE_KEYS)))
         ">: "
       )
     )
  )
  (COND
    ((OR (NOT INPUT) (= INPUT "ENtry"))
      (SETQ
        ATTRIBUTE_LIST
         (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST
           TAG
           "ENtry"
           ATTRIBUTE_LIST
         )
      )
    )
    (T
     (SETQ
       ATTRIBUTE_LIST
        (HCNM_LDRBLK_AUTO_DISPATCH
          ENAME_BLOCK
          ATTRIBUTE_LIST
          (STRCAT "NOTETXT" (ITOA LINE_NUMBER))
          (CADR
            (ASSOC INPUT (HCNM_LDRBLK_GET_AUTO_TYPE_KEYS))
          )
          P1_ENTRY
          NIL
        )
     )
    )
  )
  ;; bubble-data-update: This is broken because the calling function expects "ENtry" to be returned sometimes.
  ;; I feel like I really should find a way to abstract the interface from this other business logic. There may be a good example in my recent subdivision tools.
  ATTRIBUTE_LIST
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
;; NOTEDATA attribute value needs to look something like this:
;; Object table                   Object references as needed
;; "((line_key_1 object_handle type/key)(line_key_i object_handle type/key))"
;; Since what has to be done on update varies widely, we pass the attribute list to each sub-function and let it decided what to do with it. I think we also need to pass in an indication of whether this is a refresh or a user prompt.
;; We return the modified attribute_list.
;; INPUT IS THE OBJECT (ENAME OR VLA-OBJECT; COULD BE STANDARDIZED) OR STRING WE NEED TO EXAMINE IF WE AREN'T ASKING THE USER FOR IT OR NIL IF WE NEED TO GET IT.
;; Returns ATTRIBUTE_LIST with the requested auto data added.
(DEFUN HCNM_LDRBLK_AUTO_DISPATCH (ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT / DATA HANDLE STRING)
    ;; bubble-data-update: all of these have to take and return ATTRIBUTE_LIST instead of just string. That's because they could need to get NOTEDATA info from ATTRIBUTE_LIST
  (SETQ ATTRIBUTE_LIST
     (COND
       ((= KEY "Text") (HCNM_LDRBLK_AUTO_ES ENAME_BLOCK ATTRIBUTE_LIST TAG KEY INPUT))
       ((= KEY "LF") (HCNM_LDRBLK_AUTO_QTY ENAME_BLOCK ATTRIBUTE_LIST TAG KEY "Length" "1" INPUT))
       ((= KEY "SF") (HCNM_LDRBLK_AUTO_QTY ENAME_BLOCK ATTRIBUTE_LIST TAG KEY "Area" "1" INPUT))
       ((= KEY "SY")
        (HCNM_LDRBLK_AUTO_QTY ENAME_BLOCK ATTRIBUTE_LIST TAG KEY "Area" "0.11111111" INPUT)
       )
       ((= KEY "Sta") (HCNM_LDRBLK_AUTO_AL ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT))
       ((= KEY "Off") (HCNM_LDRBLK_AUTO_AL ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT))
       ((= KEY "StaOff")
        (HCNM_LDRBLK_AUTO_AL ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT)
       )
       ((= KEY "N") (HCNM_LDRBLK_AUTO_NE ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT))
       ((= KEY "E") (HCNM_LDRBLK_AUTO_NE ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT))
       ((= KEY "NE") (HCNM_LDRBLK_AUTO_NE ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT))
       ((= KEY "Z") (HCNM_LDRBLK_AUTO_SU ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT))
     )
  )
)

(DEFUN HCNM_LDRBLK_AUTO_ES (ENAME_BLOCK ATTRIBUTE_LIST TAG KEY INPUT / ENAME) 
  (SETQ
    ENAME
    (COND
      (INPUT)
      (T (CAR (NENTSEL (STRCAT "\nSelect object with " KEY ": "))))
    )     
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (SETQ  
    ATTRIBUTE_LIST 
     (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST 
       TAG
       (COND 
         (ENAME (CDR (ASSOC 1 (ENTGET ENAME))))
         (T "")
       )
       ATTRIBUTE_LIST
     )
  )
)
(DEFUN HCNM_LDRBLK_AUTO_QTY (ENAME_BLOCK ATTRIBUTE_LIST TAG KEY AUTO_TYPE FACTOR INPUT / STR_BACKSLASH INPUT1 PSPACE_BUBBLE_P
                         SS-P STRING
                        )
  (COND
    ((SETQ STRING INPUT))
    (T  
      (COND
        ((AND
          (= AUTO_TYPE "Area")
          (= (C:HCNM-CONFIG-GETVAR "BubbleAreaIntegral") "1")
          )
        (C:HCNM-CONFIG-SETVAR "BubbleArrowIntegralPending" "1")
        )
      )
      (SETQ PSPACE_BUBBLE_P (HCNM_LDRBLK_SPACE_SET_MODEL))
      (INITGET "Selection")
      (SETQ
        INPUT1
        (NENTSEL
          (STRCAT
            "\nSelect object to link dynamically or [Selection set (not dynamic) including AECC_PIPEs] <Selection set>: "
          )
        )
        SS-P
        (OR (NOT INPUT1) (= INPUT1 "Selection"))
        STRING
        (COND
          (SS-P
            (IF (NOT HAWS-QT-NEW)
              (LOAD "HAWS-QT")
            )
            (HAWS-QT-NEW "ldrblk")
            (HAWS-QT-SET-PROPERTY "ldrblk" "type" (STRCASE AUTO_TYPE T))
            (HAWS-QT-SET-PROPERTY "ldrblk" "factor" (READ FACTOR))
            (HAWS-QT-SET-PROPERTY
              "ldrblk"
              "postfix"
              (C:HCNM-CONFIG-GETVAR
                (STRCAT "BubbleTextPostfix" KEY)
              )
            )
            (HAWS-QT-STRING "ldrblk")
            (HAWS-QT-GET-PROPERTY "ldrblk" "string")
          )
          (T
            (STRCAT
              (C:HCNM-CONFIG-GETVAR
                (STRCAT "BubbleTextPrefix" KEY)
              )
              "%<\\AcObjProp Object(%<\\_ObjId "
              (VLA-GETOBJECTIDSTRING
                (VLA-GET-UTILITY
                  (VLA-GET-ACTIVEDOCUMENT (VLAX-GET-ACAD-OBJECT))
                )
                (VLAX-ENAME->VLA-OBJECT (CAR INPUT1))
                :VLAX-FALSE
              )
              ">%)."
              AUTO_TYPE
              " \\f \"%lu2%pr"
              (C:HCNM-CONFIG-GETVAR
                (STRCAT "BubbleTextPrecision" KEY)
              )
              "%ct8["
              FACTOR
              "]\">%"
              (C:HCNM-CONFIG-GETVAR
                (STRCAT "BubbleTextPostfix" KEY)
              )
            )
          )
        )
      )
      (HCNM_LDRBLK_SPACE_RESTORE PSPACE_BUBBLE_P)
    )
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST 
    TAG
    STRING
    ATTRIBUTE_LIST
  )
)
;; NOT USED
(DEFUN HCNM_LDRBLK_MTEXTATRIBUTE_P (EN)
  (SETQ OBJ (VLAX-ENAME->VLA-OBJECT EN))
  (AND
    (VLAX-PROPERTY-AVAILABLE-P OBJ 'MTEXTATTRIBUTE)
    (VLA-GET-MTEXTATTRIBUTE OBJ)
  )
)
(DEFUN HCNM_LDRBLK_SPACE_SET_MODEL ()
  (COND ((= (GETVAR "CVPORT") 1) (VL-CMDF "._MSPACE") T))
)
(DEFUN HCNM_LDRBLK_SPACE_RESTORE (PSPACE_BUBBLE_P)
  (COND (PSPACE_BUBBLE_P (VL-CMDF "._PSPACE")))
)
;; bubble-data-update: This has to be split into
;; 1. HCNM_LDRBLK_AUTO_AL_GET_OBJECT that returns object
;; 2. HCNM_LDRBLK_AUTO_AL_GET_STRING that returns staoff string of given object and point so that this function can be used to update string.
(DEFUN HCNM_LDRBLK_AUTO_AL (ENAME_BLOCK ATTRIBUTE_LIST TAG KEY P1_ENTRY INPUT / DRAWSTATION NAME OBJALIGN OFF PSPACE_BUBBLE_P STA STRING)
  (COND
    ((SETQ OBJALIGN INPUT))
    (T
      (SETQ
        PSPACE_BUBBLE_P
        (HCNM_LDRBLK_SPACE_SET_MODEL)
        OBJALIGN
        (HCNM_LDRBLK_AUTO_AL_GET_ALIGNMENT)
        P1_WORLD (HCNM_LDRBLK_TRANS_TO_WORLD P1_ENTRY PSPACE_BUBBLE_P)
      )
      (HCNM_LDRBLK_SPACE_RESTORE PSPACE_BUBBLE_P)
    )
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (COND 
    ((= (TYPE OBJALIGN) 'VLA-OBJECT)
      ;; http://docs.autodesk.com/CIV3D/2012/ENU/API_Reference_Guide/com/AeccXLandLib__IAeccAlignment__StationOffset@[in]_double@[in]_double@[out]_double_@[out]_double_.htm
      (VLAX-INVOKE-METHOD 
        OBJALIGN
        'STATIONOFFSET
        (VLAX-MAKE-VARIANT 
          (CAR P1_WORLD)
          VLAX-VBDOUBLE
        )
        (VLAX-MAKE-VARIANT 
          (CADR P1_WORLD)
          VLAX-VBDOUBLE
        )
        'DRAWSTATION
        'OFF
      )
      (SETQ 
        ATTRIBUTE_LIST
        (HCNM_LDRBLK_SAVE_OBJECT_REFERENCE_TO_NOTEDATA TAG OBJALIGN KEY ATTRIBUTE_LIST)
        NAME 
        (VLAX-GET-PROPERTY OBJALIGN 'NAME)             
        STA  
        (STRCAT 
          (C:HCNM-CONFIG-GETVAR "BubbleTextPrefixSta")
          (VLAX-INVOKE-METHOD 
            OBJALIGN
            'GETSTATIONSTRINGWITHEQUATIONS
            DRAWSTATION
          )
          (C:HCNM-CONFIG-GETVAR "BubbleTextPostfixSta")
        )
        OFF  
        (STRCAT 
          (COND 
            ((MINUSP OFF)
              (C:HCNM-CONFIG-GETVAR "BubbleTextPrefixOff-")
            )
            (T (C:HCNM-CONFIG-GETVAR "BubbleTextPrefixOff+"))
          )
          (RTOS 
            (COND 
              ((= (C:HCNM-CONFIG-GETVAR "BubbleOffsetDropSign") "1")
                (ABS OFF)
              )
              (T OFF)
            )
            2
            (ATOI (C:HCNM-CONFIG-GETVAR "BubbleTextPrecisionOff+"))
          )
          (COND 
            ((MINUSP OFF)
              (C:HCNM-CONFIG-GETVAR "BubbleTextPostfixOff-")
            )
            (T (C:HCNM-CONFIG-GETVAR "BubbleTextPostfixOff+"))
          )
        )
        STRING
        (COND 
          ((= KEY "Sta") STA)
          ((= KEY "Off") OFF)
          ((= KEY "StaOff")
          (STRCAT 
            STA
            (C:HCNM-CONFIG-GETVAR "BubbleTextJoinDelSta")
            OFF
          )
          )
        )
      )
    )
    (T (SETQ STRING "N/A"))
  )
  (HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR OBJALIGN ENAME_BLOCK TAG KEY)
  (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST 
    TAG
    STRING
    ATTRIBUTE_LIST
  )
)
(DEFUN HCNM_LDRBLK_AUTO_AL_GET_ALIGNMENT
   (/ EALIGN NAME OBJALIGN OBJALIGN_OLD PSPACE_BUBBLE_P)
  (SETQ
    OBJALIGN_OLD
     (C:HCNM-CONFIG-GETVAR "BubbleCurrentAlignment")
    NAME
     (COND
       ((= (TYPE OBJALIGN_OLD) 'VLA-OBJECT)
        (VLAX-GET-PROPERTY OBJALIGN_OLD 'NAME)
       )
       (T (SETQ OBJALIGN_OLD NIL) "")
     )
    EALIGN
     (NENTSEL
       (STRCAT
         "\nSelect alignment"
         (COND
           ((= NAME "") ": ")
           (T (STRCAT " or <" NAME ">: "))
         )
       )
     )
  )
  (COND
    ((AND
       EALIGN
       (= (CDR (ASSOC 0 (ENTGET (CAR EALIGN)))) "AECC_ALIGNMENT")
     )
     (SETQ OBJALIGN (VLAX-ENAME->VLA-OBJECT (CAR EALIGN)))
     (C:HCNM-CONFIG-SETVAR "BubbleCurrentAlignment" OBJALIGN)
    )
    (EALIGN (alert (princ "\nSelected object is not an alignment. Keeping previous alignment."))(SETQ OBJALIGN OBJALIGN_OLD))
    (T (princ "\nNo object selected. Keeping previous alignment.")(SETQ OBJALIGN OBJALIGN_OLD))
  )
)
(DEFUN HCNM_LDRBLK_AUTO_NE (ENAME_BLOCK ATTRIBUTE_LIST TAG KEY AUTO_TYPE P1_ENTRY INPUT / E N NE P1_WORLD STRING)
  (COND
    ((SETQ INPUT STRING))
    (T
      (SETQ
        PSPACE_BUBBLE_P
        (HCNM_LDRBLK_SPACE_SET_MODEL)
        P1_WORLD (HCNM_LDRBLK_TRANS_TO_WORLD P1_ENTRY PSPACE_BUBBLE_P)
        N  (HCNM_LDRBLK_AUTO_RTOS (CADR P1_WORLD) "N")
        E  (HCNM_LDRBLK_AUTO_RTOS (CAR P1_WORLD) "E")
        NE (STRCAT
            N
            (C:HCNM-CONFIG-GETVAR (STRCAT "BubbleTextJoinDel" "N"))
            E
          )
      )
      (HCNM_LDRBLK_SPACE_RESTORE PSPACE_BUBBLE_P)
      (SETQ STRING
        (COND
          ((= AUTO_TYPE "N") N)
          ((= AUTO_TYPE "E") E)
          ((= AUTO_TYPE "NE") NE)
        )
      )
    )
  )
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST 
    TAG
    STRING
    ATTRIBUTE_LIST
  )
)
(DEFUN HCNM_LDRBLK_AUTO_RTOS (NUMBER KEY)
  (STRCAT
    (C:HCNM-CONFIG-GETVAR (STRCAT "BubbleTextPrefix" KEY))
    (RTOS
      NUMBER
      2
      (atoi(C:HCNM-CONFIG-GETVAR (STRCAT "BubbleTextPrecision" KEY)))
    )
    (C:HCNM-CONFIG-GETVAR (STRCAT "BubbleTextPostfix" KEY))
  )
)
(DEFUN HCNM_LDRBLK_AUTO_SU (ENAME_BLOCK ATTRIBUTE_LIST TAG KEY AUTO_TYPE P1_ENTRY STRING)
  ;; END HCNM_LDRBLK_AUTO_GET_INPUT SUBFUNCTION
  ;; START HCNM_LDRBLK_AUTO_UPDATE SUBFUNCTION
  (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST 
    TAG
    (HCNM_LDRBLK_AUTO_APOLOGY AUTO_TYPE)
    ATTRIBUTE_LIST
  )
)
;; Translates from current user or paper coordinate system to world.
;; Assumes that the entry coordinate system was the same as the current.
(DEFUN HCNM_LDRBLK_TRANS_TO_WORLD (P1_ENTRY PSPACE_BUBBLE_P)
  (COND
    ((= (GETVAR "CVPORT") 1) (ALERT (PRINC "\nProgramming error: HCNM_LDRBLK_TRANS_TO_WORLD cannot be called from paper space.")))
    (PSPACE_BUBBLE_P (TRANS (TRANS P1_ENTRY 3 2) 2 0))
    (T (TRANS P1_ENTRY 1 0))
  )
)
(DEFUN HCNM_LDRBLK_AUTO_APOLOGY (AUTO_TYPE)
  (ALERT (PRINC (STRCAT "Sorry. Selection of " AUTO_TYPE " is not fully programmed yet and is not anticipated to be dynamic once programmed.\n\nPlease let Tom Haws <tom.haws@gmail.com> know if you are eager for this as static text.")))
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
(DEFUN HCNM_LDRBLK_ADJUST_NOTEDATA (DATA ATTRIBUTE_LIST / TAG VALUE)
  (SETQ
    TAG   (CAR DATA)
    VALUE (CADDDR DATA)
  )
  (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST TAG VALUE ATTRIBUTE_LIST)
)
(DEFUN C:HCNM-EDIT-BUBBLES ()
  (haws-core-init 337)
  (if (not haws-editall)(load "editall"))
  (haws-editall T)
  (haws-core-restore)
)
(DEFUN HCNM_EDIT_BUBBLE (ENAME_BLOCK / P1_DATA DCLFILE P1_ENTRY
                     ENAME_LEADER_OLD HCNM_EB:ATTRIBUTE_LIST
                     NOTETEXTRADIOCOLUMN REPLACE_BLOCK_P RETURN_LIST
                    )
  (SETQ
    P1_DATA
     (HCNM_LDRBLK_GET_P1_DATA ENAME_BLOCK)
    P1_ENTRY
     (CAR P1_DATA)
    ENAME_LEADER_OLD
     (CADDR P1_DATA)
    ;; Semi-global variable. Global to the HCNM-EB: functions called from here.
    HCNM_EB:ATTRIBUTE_LIST
     (HCNM_GET_ATTRIBUTES ENAME_BLOCK T)
    NOTETEXTRADIOCOLUMN "RadioNOTETXT1"
    DCLFILE
     (LOAD_DIALOG "cnm.dcl")
    DONE_CODE 2
  )
  (WHILE (> DONE_CODE -1)
    (COND
      ((= DONE_CODE 0) (SETQ DONE_CODE (HCNM_EDIT_BUBBLE_CANCEL)))
      ((= DONE_CODE 1)
       (SETQ DONE_CODE (HCNM_EB:SAVE ENAME_BLOCK))
      )
      ((= DONE_CODE 2)
       ;; Show the CNM Bubble Note Editor dialog with the requested text line's radio button selected.
       (SETQ
         RETURN_LIST
          (HCNM_EB:SHOW DCLFILE NOTETEXTRADIOCOLUMN P1_DATA)
         DONE_CODE
          (CAR RETURN_LIST)
         NOTETEXTRADIOCOLUMN
          (CADR RETURN_LIST)
         TAG
          (SUBSTR NOTETEXTRADIOCOLUMN 6)
       )
      )
      (T
       ;; Process clicked action tile (button) other than cancel or save.
       ;; bubble-data-update: This is start point 2 of 2 of the bubble data logic. This one is for the bubble note editing process.
       ;; this is called whenever a dialog auto-text button is clicked.
       (HCNM_EB:GET_TEXT ENAME_BLOCK DONE_CODE TAG P1_ENTRY)
       (SETQ DONE_CODE 2)
      )
    )
  )
  ;; Change its arrowhead if needed.
  (HCNM_LDRBLK_CHANGE_ARROWHEAD ENAME_LEADER_OLD)
  (HAWS-CORE-RESTORE)
  (PRINC)
)
;;; bubble-data-update: this or something below it needs to populate the NOTEDATA attribute.
(DEFUN HCNM_EB:GET_TEXT (ENAME_BLOCK DONE_CODE TAG P1_ENTRY / AUTO_STRING AUTO_TYPE)
  (SETQ
    AUTO_TYPE
     (CADR (ASSOC DONE_CODE (HCNM_EDIT_BUBBLE_DONE_CODES)))
    ;; bubble-data-update: this is called from command line and from edit box to get string as requested by user.
    HCNM_EB:ATTRIBUTE_LIST
     (HCNM_LDRBLK_ADJUST_FORMATS
       (HCNM_LDRBLK_AUTO_DISPATCH
         ENAME_BLOCK
         HCNM_EB:ATTRIBUTE_LIST
         TAG
         AUTO_TYPE
         P1_ENTRY
         NIL
       )
     )
  )
)

(defun HCNM_EDIT_BUBBLE_CANCEL ()
 -1
)
(defun HCNM_EB:SAVE (ENAME_BLOCK)
  (HCNM_SET_ATTRIBUTES ENAME_BLOCK HCNM_EB:ATTRIBUTE_LIST)
 -1
) 
(DEFUN HCNM_EDIT_BUBBLE_DONE_CODES ( / EB_DONE)
  (SETQ EB_DONE T)
  '((11 "LF" EB_DONE)
    (12 "SF" EB_DONE)
    (13 "SY" EB_DONE)
    (14 "Sta" EB_DONE)
    (15 "Off" EB_DONE)
    (16 "StaOff" EB_DONE)
    (17 "N" EB_DONE)
    (18 "E" EB_DONE)
    (19 "NE" EB_DONE)
    (20 "Z" EB_DONE)
    (21 "Text" EB_DONE)
   )
)
(DEFUN HCNM_EB:SHOW
   (DCLFILE NOTETEXTRADIOCOLUMN P1_DATA / )
  (SETQ P1_ENTRY (CAR P1_DATA))
  (NEW_DIALOG "HCNMEditBubble" DCLFILE)
  (SET_TILE "Title" "Edit CNM Bubble Note")
  ;; Note attribute edit boxes
  (FOREACH
     ATTRIBUTE HCNM_EB:ATTRIBUTE_LIST
    (SET_TILE (STRCAT "Edit" (CAR ATTRIBUTE)) (CADR ATTRIBUTE))
    (ACTION_TILE
      (STRCAT "Edit" (CAR ATTRIBUTE))
      (STRCAT
        "(HCNM_EB:SAVE_EDIT_BOX \""
        (CAR ATTRIBUTE)
        "\" $value)"
      )
    )
  )
  ;;Radio buttons
  (SET_TILE
    "NoteTextRadioColumn"
    NOTETEXTRADIOCOLUMN
  )
  (ACTION_TILE
    "NoteTextRadioColumn"
    "(SETQ NoteTextRadioColumn $value)"
  )
  ;;Auto text buttons
  (MAPCAR
    '(LAMBDA (CODE)
       (ACTION_TILE
         (CADR CODE)
         (STRCAT "(DONE_DIALOG " (ITOA (CAR CODE)) ")")
       )
     )
    (HCNM_EDIT_BUBBLE_DONE_CODES)
  )
  (ACTION_TILE "accept" "(DONE_DIALOG 1)")
  (ACTION_TILE "cancel" "(DONE_DIALOG 0)")
  (LIST (START_DIALOG) NOTETEXTRADIOCOLUMN)
)
(DEFUN HCNM_EB:SAVE_EDIT_BOX (TAG INPUT)
  (SETQ
    HCNM_EB:ATTRIBUTE_LIST
     (HCNM_LDRBLK_ADJUST_FORMATS
       (HCNM_LDRBLK_SAVE_ATTRIBUTE_TO_LIST
         TAG
         INPUT
         HCNM_EB:ATTRIBUTE_LIST
       )
     )
  )
)
;; FIELD_CODE_P NIL SIMPLIFIES PROCESSING WHEN BLOCKS LIKE NOTEQTY ARE KNOWN NOT TO HAVE FIELD CODES IN THEM
(DEFUN HCNM_GET_ATTRIBUTES (ENAME_BLOCK FIELD_CODE_P / ATTRIBUTE_LIST ELIST ENAME_NEXT ETYPE FIELD_CODE OBJ_NEXT)
  (SETQ ENAME_NEXT ENAME_BLOCK)
  (WHILE (AND
           (SETQ ENAME_NEXT (ENTNEXT ENAME_NEXT))
           (/= "SEQEND"
               (SETQ ETYPE (CDR (ASSOC 0 (SETQ ELIST (ENTGET ENAME_NEXT)))))
           )
         )
    (COND
      ((= ETYPE "ATTRIB")
       (SETQ
         OBJ_NEXT (VLAX-ENAME->VLA-OBJECT ENAME_NEXT)
         ATTRIBUTE_LIST
          (CONS
            (LIST 
              (CDR (ASSOC 2 ELIST)) 
              (COND 
                ((AND FIELD_CODE_P (SETQ FIELD_CODE (LM:FieldCode ENAME_NEXT))) FIELD_CODE)
                (T (VLA-GET-TEXTSTRING OBJ_NEXT)))
            )
            ATTRIBUTE_LIST
          )
       )
      ) ;_ end of and
    )
  )
  ATTRIBUTE_LIST
)
(defun LM:FieldCode ( en / fd id )
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

(DEFUN HCNM_SET_ATTRIBUTES (ENAME_BLOCK ATTRIBUTE_LIST / ATAG ELIST ENAME_NEXT ETYPE OBJ_NEXT)
  (SETQ ENAME_NEXT ENAME_BLOCK)
  (WHILE (AND
           (SETQ ENAME_NEXT (ENTNEXT ENAME_NEXT))
           (/= "SEQEND"
               (SETQ ETYPE (CDR (ASSOC 0 (SETQ ELIST (ENTGET ENAME_NEXT)))))
           )
         )
    (COND
      ((AND
         (= ETYPE "ATTRIB")
         (SETQ ATAG (CDR (ASSOC 2 ELIST)))
         (ASSOC ATAG ATTRIBUTE_LIST)
       ) ;_ end of and
        (SETQ OBJ_NEXT (VLAX-ENAME->VLA-OBJECT ENAME_NEXT))
        (VLA-PUT-TEXTSTRING
          OBJ_NEXT
          (CADR (ASSOC ATAG ATTRIBUTE_LIST))
        )
        (COND ((= (HCNM_LDRBLK_GET_MTEXT_STRING) "")(VL-CMDF "._updatefield" ENAME_NEXT "")))
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
(DEFUN HCNM_LDRBLK_LIST_REACTORS ( / REACTORS)
  (SETQ REACTORS (CDAR(VLR-REACTORS :VLR-OBJECT-REACTOR)))
  (FOREACH REACTOR REACTORS
    ;;(vlax-dump-object REACTOR)
    (PRINT (VL-PRIN1-TO-STRING (VLR-DATA REACTOR)))
    (PRINT (VL-PRIN1-TO-STRING (VLR-REACTIONS REACTOR)))
    ;; (vlr-remove REACTOR)
  )
)
;; (DEFUN HCNM_LDRBLK_ASSURE_REACTOR_FOR_ON_OBJECT (/ OBJECT OWNERS DATA CALLBACKS REACTOR)
;; )

(DEFUN HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR (NOTIFIER ENAME_BUBBLE TAG KEY / CALLBACKS DATA REACTOR REACTOR_OLD REACTORS_OLD OWNERS)
  (SETQ
    CALLBACKS 
     '((:vlr-modified . HCNM_LDRBLK_REACTOR_CALLBACK)(:vlr-modified . vlr-trace-reaction))
    REACTORS_OLD 
     (CDAR (VLR-REACTORS :VLR-OBJECT-REACTOR))
  )
  (FOREACH REACTOR REACTORS_OLD
    (COND
      ((= (CAR (VLR-DATA REACTOR)) "CNM-BUBBLE")
       (SETQ REACTOR_OLD REACTOR)
      )
    )
  )
  (COND
    (REACTOR_OLD
      ;; IF THIS OWNER IS ALREADY ATTACHED, JUST UPDATE THE DATA
      (COND 
        ((MEMBER OBJECT (VLR-OWNERS REACTOR_OLD))
         (VLR-DATA-SET REACTOR_OLD (HCNM_LDRBLK_UPDATE_DATA (VLR-DATA REACTOR_OLD) ENAME_BUBBLE TAG KEY))
        )
      )
    )
  )
  (SETQ
  ;; MAKE REACTOR
    REACTOR (VLR-OBJECT-REACTOR OWNERS DATA CALLBACKS)
  ;; MAKE IT PERSISTENT
    REACTOR (VLR-PERS REACTOR)    
  )
)
(DEFUN HCNM_LDRBLK_UPDATE_DATA (DATA ENAME_BUBBLE TAG KEY)
  (SETQ 
    HANDLE_BUBBLE (HANDENT ENAME_BUBBLE)
    BUBBLE (LIST HANDLE_BUBBLE (LIST TAG KEY))
  )
  (COND 
    ;; ADD BUBBLE IF NOT IN DATA
    ((NOT (ASSOC HANDLE_BUBBLE (CDR DATA))) 
      (SETQ DATA (REVERSE (CONS (LIST HANDLE_BUBBLE (LIST TAG KEY)) (REVERSE DATA))))
    )
    ;; ADD ATTRIBUTE IF NOT IN BUBBLE
    ((NOT (ASSOC TAG (CDR (ASSOC HANDLE_BUBBLE (CDR DATA))))) 
      (SETQ DATA (SUBST 
                   (CONS HANDLE_BUBBLE 
                         (CONS (LIST TAG KEY) (CDR (ASSOC HANDLE_BUBBLE (CDR DATA))))
                   )
                   (ASSOC HANDLE_BUBBLE (CDR DATA))
                   DATA
                 )
      )
    )
    ;; CHANGE KEY IF NEEDED
    ((/= (ASSOC TAG (CDR (ASSOC HANDLE_BUBBLE (CDR DATA)))) KEY)
      (SETQ DATA (SUBST 
                   (CONS HANDLE_BUBBLE 
                         (CONS (LIST TAG KEY) (CDR (ASSOC HANDLE_BUBBLE (CDR DATA))))
                   )
                   (SUBST
                     (LIST TAG KEY)
                     (ASSOC TAG (CDR (ASSOC HANDLE_BUBBLE (CDR DATA))))
                     (ASSOC HANDLE_BUBBLE (CDR DATA))
                   )
                   DATA
                 )
      )
    )
  )
)
(DEFUN HCNM_LDRBLK_REACTOR_CALLBACK (NOTIFIER-OBJECT REACTOR-OBJECT PARAMETER-LIST / VERTICES)
  (HCNM_LDRBLK_UPDATE_NOTIFIER NOTIFIER-OBJECT REACTOR-OBJECT)
)
;;; NEED TO DEFINE THE REACTOR DATA FOR THIS APPLICATION. PROBABLY SOMETHING THAT MAKES IT EASY FOR THE CALLBACK TO PROCESS WHAT'S REQUIRED AND FOR THE REACTOR ASSURER TO KNOW WHETHER A GIVEN REACTOR IS FOR THIS ATTRIBUTE OF THIS BLOCK OF THIS APPLICATION AND WHAT IS THE DATA TYPE. PROBABLY THIS LIST 
;;; '("CNM-BUBBLE" 
;;;   (BUBBLE1_ENAME 
;;;     (ATTRIBUTE_TAG_1 TYPE1) 
;;;     (ATTRIBUTE_TAG_I TYPEI)
;;;   )
;;;   (BUBBLEI_ENAME 
;;;     (ATTRIBUTE_TAG_1 TYPE1)
;;;     (ATTRIBUTE_TAG_I TYPEI)
;;;   )
;;; )
;;; 
(DEFUN HCNM_LDRBLK_UPDATE_NOTIFIER (NOTIFIER REACTOR / BUBBLE DATA DATA_OLD) 
  (SETQ DATA_OLD (VLR-DATA REACTOR)
        DATA     DATA_OLD
  )
  (PRINT DATA)
  (COND 
    ((/= (CAR DATA) "CNM-BUBBLE") (ALERT "\nFatal error in reactor. Data is not for CNM-BUBBLE") (EXIT))
  )
  (CONS 
    (CAR DATA)
    (MAPCAR 
      '(LAMBDA (BUBBLE) 
         (HCNM_LDRBLK_UPDATE_BUBBLE BUBBLE NOTIFIER)
       )
      (CDR DATA)
    )
  )
  (COND 
    ;; DETACH THE NOTIFIER IF USER REMOVED ALL ITS DEPENDENT AUTO-TEXT FROM ALL BUBBLES.
    ((NOT (CDR DATA))
     (VLR-OWNER-REMOVE REACTOR NOTIFIER)
    )
    ;; UPDATE THE REACTOR DATA IF USER REMOVED SOME OF ITS DEPENDENT AUTO-TEXT
    ((/= DATA DATA_OLD)
     (VLR-DATA-SET REACTOR DATA)
    )
  )
)
;; UPDATES A BUBBLE INSERTION. RETURNS BUBBLE WITH ATTRIBUTES REMOVED IF USER REMOVED THEM (SINCE WE DON'T YET HAVE A STRUCTURED INTERFACE THAT WOULD LET THE USER REMOVE IN REAL TIME. MAYBE IT WOULD BE EASIEST TO JUST HAVE A WAY TO DISABLE EDITING AUTO ATTRIBUTES UNLESS USER CLICKS A "MANUAL" BUTTON. I LOVE THAT. EDIT BUBBLE COULD READ THE REACTOR DATA OR EACH BUBBLE COULD HAVE A LIST IN NOTEDATA)
(DEFUN HCNM_LDRBLK_UPDATE_BUBBLE (BUBBLE NOTIFIER / ATTRIBUTE_LIST ATTRIBUTE_LIST_OLD ENAME_BUBBLE PT_LEADER_START) 
  (SETQ ENAME_BUBBLE       (HANDENT (CAR BUBBLE))
        PT_LEADER_START    (CDR (ASSOC 10 (ENTGET ENAME_BUBBLE)))
        ATTRIBUTE_LIST_OLD (HCNM_GET_ATTRIBUTES ENAME_BUBBLE T)
        ATTRIBUTE_LIST     ATTRIBUTE_LIST_OLD
  )
  (FOREACH ATTRIBUTE BUBBLE 
    (COND 
      ;; AT THE MOMENT, ONLY TOTAL ATTRIBUTE DELETION CANCELS A REACTOR.
      ((= (CADR (ASSOC (CAR ATTRIBUTE) ATTRIBUTE_LIST)) "")
       ;; I AM CURIOUS WHETHER MODIFYING BUBBLE WITHIN FOREACH WILL CAUSE A BUG. I THINK NOT SINCE IT ITERATES OVER A COPY.
       (SETQ BUBBLE (VL-REMOVE ATTRIBUTE BUBBLE))
      )
      (T
       (SETQ ATTRIBUTE_LIST (HCNM_LDRBLK_AUTO_DISPATCH 
                              ENAME_BUBBLE
                              ATTRIBUTE_LIST
                              (CAR ATTRIBUTE) ; TAG
                              (CADR ATTRIBUTE) ; KEY
                              PT_LEADER_START
                              NOTIFIER ; INPUT
                            )
       )
      )
    )
  )
  (COND 
    ((/= ATTRIBUTE_LIST ATTRIBUTE_LIST_OLD)
     ;; UPDATE BLOCK INSERTION
     (HCNM_SET_ATTRIBUTES 
       ENAME_BUBBLE
       (HCNM_LDRBLK_ADJUST_FORMATS 
         ATTRIBUTE_LIST
       )
     )
    )
  )
  BUBBLE
)
;#endregion
;#region CNM Options dialog
(DEFUN C:HCNM-CNMOPTIONS (/ CNMDCL DONE_CODE RETN)
  (haws-core-init 210)
  (HCNM_PROJINIT)
  (HCNM_PROJ)
 ;; Load Dialog
  (SETQ CNMDCL (LOAD_DIALOG "cnm.dcl"))
  (setq DONE_CODE 2)
  (while (> DONE_CODE -1)
    (setq DONE_CODE
      (cond
        ((= DONE_CODE 0)(HCNM_DCL_OPTIONS_CANCEL))
        ((= DONE_CODE 1)(HCNM_DCL_OPTIONS_SAVE))
        ((= DONE_CODE 2)(HCNM_DCL_OPTIONS_SHOW CNMDCL))
        ((= DONE_CODE 11)(HCNM_DCL_GENERAL_SHOW CNMDCL))
        ((= DONE_CODE 12)(HCNM_DCL_BUBBLE_SHOW CNMDCL))
        ((= DONE_CODE 13)(HCNM_DCL_KEY_SHOW CNMDCL))
        ((= DONE_CODE 14)(HCNM_DCL_QT_SHOW CNMDCL))
      )
    )
  )
 (haws-core-restore)  
 (PRINC)
)

(defun HCNM_DCL_OPTIONS_CANCEL()
 (HCNM_CONFIG_TEMP_CLEAR)
 -1
)

;; Saves, then passes control to temp var clear function.
(defun HCNM_DCL_OPTIONS_SAVE()
 (HCNM_CONFIG_TEMP_SAVE)
 0
)

(DEFUN HCNM_DCL_OPTIONS_SHOW (CNMDCL)
  (NEW_DIALOG "HCNMOptions" CNMDCL)
  (SET_TILE "Title" "CNM Options")
  (ACTION_TILE "General" "(DONE_DIALOG 11)")
  (ACTION_TILE "Bubble" "(DONE_DIALOG 12)")
  (ACTION_TILE "Key" "(DONE_DIALOG 13)")
  (ACTION_TILE "QT" "(DONE_DIALOG 14)")
  (ACTION_TILE "accept" "(DONE_DIALOG 1)")
  (ACTION_TILE "cancel" "(DONE_DIALOG 0)")
  (START_DIALOG)
)

(DEFUN HCNM_DCL_GENERAL_SHOW (CNMDCL)
  (NEW_DIALOG "HCNMGeneral" CNMDCL)
  ;; Dialog Actions
  (SET_TILE "Title" "CNM General Options")
  (HCNM_CONFIG_SET_ACTION_TILE "DoCurrentTabOnly")
  (HCNM_CONFIG_DCL_LIST "InsertTablePhases")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias1")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias2")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias3")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias4")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias5")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias6")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias7")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias8")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseAlias9")
  (HCNM_CONFIG_SET_ACTION_TILE "NotesKeyTableDimstyle")
  (HCNM_CONFIG_SET_ACTION_TILE "NotesLeaderDimstyle")
  (SET_TILE
    "ProjectFolder"
    (STRCAT
      "Project folder "
      (HCNM_SHORTEN_PATH (HCNM_PROJ) 100)
    )
  )
  (HCNM_CONFIG_SET_ACTION_TILE "ProjectNotes")
  (ACTION_TILE
    "ProjectNotesBrowse"
    "(HCNM_CONFIG_TEMP_SETVAR \"ProjectNotes\"(HCNM_GETPROJNOTES))(SET_TILE \"ProjectNotes\" (HCNM_CONFIG_TEMP_GETVAR \"ProjectNotes\"))"
  )
  (HCNM_CONFIG_DCL_LIST "LayersEditor")
  (HCNM_CONFIG_DCL_LIST "ProjectNotesEditor")
  (ACTION_TILE "close" "(DONE_DIALOG 2)")
  (START_DIALOG)
)

(DEFUN HCNM_DCL_BUBBLE_SHOW (CNMDCL)
  (NEW_DIALOG "HCNMBubble" CNMDCL)
  (SET_TILE "Title" "CNM Bubble Options")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleHooks")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleMtext")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleAreaIntegral")
  (HCNM_CONFIG_SET_ACTION_TILE "NoteTypes")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine1PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine2PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine3PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine4PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine5PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine6PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextLine0PromptP")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleSkipEntryPrompt")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleOffsetDropSign")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixLF")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixSF")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixSY")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixSta")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixOff+")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixOff-")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixN")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixE")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrefixZ")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixLF")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixSF")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixSY")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixSta")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixOff+")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixOff-")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixN")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixE")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPostfixZ")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextJoinDelSta")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextJoinDelN")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionLF")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionSF")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionSY")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionOff+")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionN")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionE")
  (HCNM_CONFIG_SET_ACTION_TILE "BubbleTextPrecisionZ")
  (ACTION_TILE "close" "(DONE_DIALOG 2)")
  (START_DIALOG)
)

(defun HCNM_DCL_KEY_SHOW(CNMDCL)
  (NEW_DIALOG "HCNMKey" CNMDCL)
  ;; Dialog Actions
  (SET_TILE "Title" "CNM Key Notes Table Options")
  (HCNM_CONFIG_SET_ACTION_TILE "DescriptionWrap")
  (HCNM_CONFIG_SET_ACTION_TILE "LineSpacing")
  (HCNM_CONFIG_SET_ACTION_TILE "NoteSpacing")
  (HCNM_CONFIG_SET_ACTION_TILE "ShowKeyTableTitleShapes")
  (HCNM_CONFIG_SET_ACTION_TILE "ShowKeyTableQuantities")
  (HCNM_CONFIG_SET_ACTION_TILE "ShowKeyTableGrid")
  (HCNM_CONFIG_SET_ACTION_TILE "TableWidth")
  (HCNM_CONFIG_SET_ACTION_TILE "PhaseWidthAdd")
  (ACTION_TILE "close" "(DONE_DIALOG 2)")
  (START_DIALOG)
)

(defun HCNM_DCL_QT_SHOW(CNMDCL)
  (NEW_DIALOG "HCNMQT" CNMDCL)
  ;; Dialog Actions
  (SET_TILE "Title" "CNM Quantity Take-off Table Options")
  (HCNM_CONFIG_SET_ACTION_TILE "NumberToDescriptionWidth")
  (HCNM_CONFIG_SET_ACTION_TILE "DescriptionToQuantityWidth")
  (HCNM_CONFIG_SET_ACTION_TILE "QuantityToQuantityWidth")
  (HCNM_CONFIG_SET_ACTION_TILE "QuantityToUnitsWidth")
  (ACTION_TILE "close" "(DONE_DIALOG 2)")
  (START_DIALOG)
)

(DEFUN HCNM_OPTIONS_LIST_DATA ()
  '(
    ("ProjectNotesEditor" (("text" "System Text Editor") ("csv" "System CSV (spreadsheet)") ("cnm" "CNM Pro Editor")))
    ("LayersEditor" (("notepad" "Notepad") ("cnm" "CNM Pro Editor")))
    ("InsertTablePhases" (("No" "No")("1" "1")("2" "2")("3" "3")("4" "4")("5" "5")("6" "6")("7" "7")("8" "8")("9" "9")("10" "10")))
  )
)
(DEFUN HCNM_CONFIG_SET_ACTION_TILE (VAR)
  (SET_TILE VAR (HCNM_CONFIG_TEMP_GETVAR VAR))
  (ACTION_TILE
    VAR
    (STRCAT "(HCNM_CONFIG_TEMP_SETVAR \"" VAR "\" $value)")
  )
)
(DEFUN HCNM_CONFIG_DCL_LIST (KEY /)
  (HCNM_SET_TILE_LIST
    KEY
    (MAPCAR
      '(LAMBDA (X) (CADR X))
      (CADR (ASSOC KEY (HCNM_OPTIONS_LIST_DATA)))
    )
    (CADR
      (ASSOC
        (C:HCNM-CONFIG-GETVAR KEY)
        (CADR (ASSOC KEY (HCNM_OPTIONS_LIST_DATA)))
      )
    )
  )
  (ACTION_TILE
    KEY
    "(HCNM_CONFIG_DCL_LIST_CALLBACK $key $value)"
  )
)
(DEFUN HCNM_SET_TILE_LIST (KEY OPTIONS SELECTED / ITEM)
  (START_LIST KEY 3)
  (MAPCAR 'ADD_LIST OPTIONS)
  (END_LIST)
  (FOREACH
     ITEM (IF (LISTP SELECTED)
            SELECTED
            (LIST SELECTED)
          )
    (IF (MEMBER ITEM OPTIONS)
      (SET_TILE
        KEY
        (ITOA (- (LENGTH OPTIONS) (LENGTH (MEMBER ITEM OPTIONS))))
      )
    )
  )
)
(DEFUN HCNM_CONFIG_DCL_LIST_CALLBACK (KEY VALUE /)
  (HCNM_CONFIG_TEMP_SETVAR
    KEY
    (CAR (NTH (READ VALUE) (CADR (ASSOC KEY (HCNM_OPTIONS_LIST_DATA)))))
  )
)

;#endregion

(LOAD "ini-edit")
;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 100 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
