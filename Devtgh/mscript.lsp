;;(C) Copyright 1997 by Thomas Gail Haws
;;Rev. 2011-10-21 to limit call to VBA to the VBA versions.
;;Rev. 7/22/99
;;Runs a script file on multiple drawings.
(DEFUN C:HAWS-MSCR () (C:HAWS-MSCRIPT))
(DEFUN
   C:HAWS-MSCRIPT (/ TEMP MSNAMELIST MSNAMESHORT PLTCMD FLSPEC RDSCR
                   RPTSCR RSNAME USERS1OLD USERS2OLD USERS3OLD USERS4OLD
                   DWGFIL COLUMN DEL
                  )
  (SETQ TEMP "MSCRIPT.LSP version 1.22 by Thomas Gail Haws")
  (IF (NOT
        (SETQ
          F1 (OPEN
               (STRCAT
                 (SETQ
                   MSNAMESHORT
                    (STRCAT
                      (GETVAR "dwgprefix")
                      "mscr"
                      (SUBSTR (GETVAR "loginname") 1 3)
                    )
                 )
                 ".scr"
               )
               "a"
             ) ;_ end of open
        ) ;_ end of setq
      ) ;_ end of not
    (SETQ MSNAMESHORT (STRCAT (GETVAR "dwgprefix") "mscr"))
    (SETQ F1 (CLOSE F1))
  ) ;_ end of if
  (SETQ
    MSNAMELIST
     (COND
       (*HAWS-MSCRIPTLISTFILE*)
       ((STRCAT MSNAMESHORT ".lst"))
     )
    DN (HAWS-GETDNPATH)
  )
  (haws-core-borrow 0)                       ;requires library function
  (TEXTPAGE)
  (PROMPT TEMP)
  (PROMPT
    "\n\nMSCRIPT must have the following conditions to run trouble-free."
  )
  (PROMPT
    "\n  1. Any needed font files, xref files, etc. must be accessible."
  )
  (PROMPT
    "\n  2. Script commands can not call dialogue boxes."
  )
  (COND
    ((AND
       (< (ATOF (GETVAR "acadver")) 18)
       (>= (ATOF (GETVAR "acadver")) 15)
     )
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
     (SETVAR "users1" (STRCAT MSNAMELIST))
     (SETVAR "users2" "Drawings (*.dwg)|*.dwg")
     (SETVAR
       "users3"
       "File(s) listed below will be processed with the script selected in the next step:"
     )
     (SETVAR
       "users4"
       "Multiple Script Processor runs a script repeatedly on multiple drawings for automation, uniformity, and speed.  It is wise to check the script on a single drawing first and to back up all drawings before processing."
     )
     (COMMAND "-vbarun" "fselect.dvb!modFileSelect.FileSelect")
     (SETQ
       F1 (SETQ MSNAMELIST (GETVAR "users1"))
       *HAWS-MSCRIPTLISTFILE* MSNAMELIST
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
    ((AND
       (OR (SETQ LSTFIL (FINDFILE (STRCAT DN ".lst")))
           (SETQ
             LSTFIL
              (FINDFILE (STRCAT (GETVAR "DWGPREFIX") "mscript.lst"))
           )
       )
       (= "Yes"
          (PROGN
            (INITGET 1 "Yes No")
            (GETKWORD
              (STRCAT
                "\nKeep and use existing list file, \""
                LSTFIL
                "\"? [Yes/No]: "
              )
            )
          )
       )
     )
    )
    (T
     (PROMPT
       (STRCAT
         "\n\nHow will you specify drawings to process?"
         "\nUse a text file you have prepared with a List of drawings"
         "\n(MSCRIPT will automatically use mscript.lst if present), "
         "\nenter Wildcards (eg. * or grad\\unit1*), "
         "\nor Select drawings one at a time from a dialogue box?"
        )
     )
     (INITGET 1 "List Wildcards Select")
     (SETQ
       TEMP
        (GETKWORD "\n[List file/Wildcards/Select one at a time]: ")
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
               "\nFiles to process using OS wildcards (eg. * or grad\\*): "
             )
          )
          (COMMAND
            "run"
            (STRCAT "attrib \"" FLSPEC ".dwg\" > " LSTFIL)
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
                          (STRCASE (STRCAT FLSPEC "`.DWG"))
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
                    "File to process (Cancel when Finished)"
                    ""
                    "DWG"
                    6
                  )
               )
          (WRITE-LINE (SUBSTR DWGFIL 1 (- (STRLEN DWGFIL) 4)) F1)
        )
        (SETQ F1 (CLOSE F1))
       )
     )
    )
  )
  (SETQ
    RSNAME
     (GETFILED
       "Select script to repeat on each drawing"
       ""
       "SCR"
       2
     )
    F1 (OPEN LSTFIL "r")
    F2 (OPEN (STRCAT MSNAMESHORT ".scr") "w")
    DWGFIL
     (READ-LINE F1)
  ) ;_ end of setq
  (SETQ COLUMN 0)
  (IF (WCMATCH DWGFIL "*:\\*,*\\\\*")
    (WHILE (NOT
             (WCMATCH
               (SUBSTR DWGFIL (SETQ COLUMN (1+ COLUMN)))
               "?:\\*,\\\\*"
             )
           )
    )
    (SETQ COLUMN 1)
  ) ;_ end of if
  (SETQ DWGFIL (SUBSTR DWGFIL COLUMN))
  ;;  Begin writing script file.
  ;;  First script line sets pre R-12 commands and opens first drawing.
  (IF (AND
        (<= 15.0 (ATOF (GETVAR "acadver")))
        (= 0 (GETVAR "sdi"))
      )
    (PRINC "sdi 1 " F2)
  ) ;_ end of if
  (WRITE-LINE (STRCAT "qsave open \"" DWGFIL "\"") F2)
  ;;  Write script lines for each drawing.
  (WHILE DWGFIL
    ;;  Script line sets newly opened drawing to pre R-12 commands.
    ;;  Other commands for each dwg. may be added or deleted after "cmddia 0".
    (WRITE-LINE "filedia 0 cmddia 0" F2)
    ;;  Embed selected script file.
    (SETQ RPTSCR (OPEN (FINDFILE RSNAME) "r"))
    (WHILE (SETQ RDSCR (READ-LINE RPTSCR))
      (WRITE-LINE RDSCR F2)
    )
    (CLOSE RPTSCR)
    ;;  Save each drawing.
    (PRINC "filedia 1 cmddia 1\nqsave " F2)
    (SETQ DWGFIL (READ-LINE F1))
    ;;  If there is another file to process, open it.
    (IF DWGFIL
      (SETQ DWGFIL (SUBSTR DWGFIL COLUMN))
    )
    (IF (AND
          DWGFIL
          (OR (FINDFILE DWGFIL) (FINDFILE (STRCAT DWGFIL ".dwg")))
        )
      (WRITE-LINE (STRCAT "open \"" DWGFIL "\"") F2)
      (SETQ DWGFIL NIL)
    ) ;_ end of if
  ) ;_ end while
  (IF (AND
        (<= 15.0 (ATOF (GETVAR "acadver")))
        (= 0 (GETVAR "sdi"))
      )
    (WRITE-LINE "sdi 0" F2)
  ) ;_ end of if
  (CLOSE F2)
  (CLOSE F1)
  (IF DEL
    (COMMAND "sh" (STRCAT "del \"" MSNAMELIST "\""))
  ) ;_ end of if
  (IF (AND
        (<= 15.0 (ATOF (GETVAR "acadver")))
        (= 0 (GETVAR "sdi"))
      )
    (ALERT
      (STRCAT
        "MSCRIPT is now ready to process drawings, but must return to Single Drawing Interface (pre-2000) first."
        "\n\nAfter closing this message, please verify that all drawing windows"
        "\nexcept the current drawing are closed"
        "\nbefore letting MSCRIPT process drawings."
        "\n\nShould your script fail for any reason,"
        "\nyou will need to turn SDI off (SDI 0) before working with multiple drawings again."
       ) ;_ end of STRCAT
    ) ;_ end of ALERT
  ) ;_ end of if
  (INITGET "Yes No")
  (IF (= (GETKWORD
           "\nProcess drawings now after saving current dwg?[Yes/No]:"
         )
         "Yes"
      )
    (COMMAND "script" MSNAMESHORT)
    (PROMPT
      (STRCAT
        "Done.  Run script file '"
        MSNAMESHORT
        "' to process."
      )
    )
  ) ;_ end of if
  (haws-core-return)                         ;requires library function
  (PRINC)
) ;_ end of DEFUN

;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
