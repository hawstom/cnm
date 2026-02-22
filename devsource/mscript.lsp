;;(C) Copyright 1997 by Thomas Gail Haws
;;Rev. 2011-10-21 to limit call to VBA to the VBA versions.
;;Rev. 7/22/99
;;Runs a script file on multiple drawings.
(defun c:haws-mscr ()
(haws-core-init 263) (c:haws-mscript))
(defun c:haws-mscript (/ f1 f2 temp msnamelist msnameshort pltcmd flspec rdscr
                   rptscr rsname users1old users2old users3old users4old
                   dwgfil column del
                  )
  (setq temp "MSCRIPT.LSP version 1.22 by Thomas Gail Haws")
  (if (not
        (setq
          f1 (haws-open
               (strcat
                 (setq
                   msnameshort
                    (strcat
                      (getvar "dwgprefix")
                      "mscr"
                      (substr (getvar "loginname") 1 3)
                    )
                 )
                 ".scr"
               )
               "a"
             ) ;_ end of open
        ) ;_ end of setq
      ) ;_ end of not
    (setq msnameshort (strcat (getvar "dwgprefix") "mscr"))
    (setq f1 (haws-close f1))
  ) ;_ end of if
  (setq
    msnamelist
     (cond
       (*haws-mscriptlistfile*)
       ((strcat msnameshort ".lst"))
     )
    dn (haws-getdnpath)
  )
  (haws-core-init 264)                       ;requires library function
  (textpage)
  (prompt temp)
  (prompt
    "\n\nMSCRIPT must have the following conditions to run trouble-free."
  )
  (prompt
    "\n  1. Any needed font files, xref files, etc. must be accessible."
  )
  (prompt
    "\n  2. Script commands can not call dialogue boxes."
  )
  (cond
    ((and
       (< (atof (getvar "acadver")) 18)
       (>= (atof (getvar "acadver")) 15)
     )
     (setq
       users1old
        (getvar "users1")
       users2old
        (getvar "users2")
       users3old
        (getvar "users3")
       users4old
        (getvar "users4")
     )
     (setvar "users1" (strcat msnamelist))
     (setvar "users2" "Drawings (*.dwg)|*.dwg")
     (setvar
       "users3"
       "File(s) listed below will be processed with the script selected in the next step:"
     )
     (setvar
       "users4"
       "Multiple Script Processor runs a script repeatedly on multiple drawings for automation, uniformity, and speed.  It is wise to check the script on a single drawing first and to back up all drawings before processing."
     )
     (vl-cmdf "-vbarun" "fselect.dvb!modFileSelect.FileSelect")
     (setq
       f1 (setq msnamelist (getvar "users1"))
       *haws-mscriptlistfile* msnamelist
     )
     (if (/= (getvar "users2") "OK")
       (setq temp t)
     )
     (setvar "users1" users1old)
     (setvar "users2" users2old)
     (setvar "users3" users3old)
     (setvar "users4" users4old)
     (if temp
       (exit)
     )
    )
    ((and
       (or (setq lstfil (findfile (strcat dn ".lst")))
           (setq
             lstfil
              (findfile (strcat (getvar "DWGPREFIX") "mscript.lst"))
           )
       )
       (= "Yes"
          (progn
            (initget 1 "Yes No")
            (getkword
              (strcat
                "\nKeep and use existing list file, \""
                lstfil
                "\"? [Yes/No]: "
              )
            )
          )
       )
     )
    )
    (t
     (prompt
       (strcat
         "\n\nHow will you specify drawings to process?"
         "\nUse a text file you have prepared with a List of drawings"
         "\n(MSCRIPT will automatically use mscript.lst if present), "
         "\nenter Wildcards (eg. * or grad\\unit1*), "
         "\nor Select drawings one at a time from a dialogue box?"
        )
     )
     (initget 1 "List Wildcards Select")
     (setq
       temp
        (getkword "\n[List file/Wildcards/Select one at a time]: ")
     )
     (cond
       ((= temp "List")
        (setq lstfil (getfiled "Select a List File" "" "LST" 0))
       )
       ((= temp "Wildcards")
        ;;Add function to user's ACAD.PGP to shell and wait for attrib command to finish.
        (setq
          lstfil
           (strcat dn ".lst")
          temp
           (findfile "acad.pgp")
          f1 (haws-open temp "r")
        )
        (while (setq dwgfil (read-line f1))
          (if (= "RUN," (substr dwgfil 1 4))
            (setq pgpmod t)
          )
          (if (= "SH," (substr dwgfil 1 3))
            (setq
              shtlst
               (cons
                 ";The following line was added by HawsEDC Construction Notes Manager for file processing by Wildcard."
                 shtlst
               )
              shtlst
               (cons
                 "RUN,       cmd /c,         0,*Batch file to run: ,"
                 shtlst
               )
            )
          )
          (setq shtlst (cons dwgfil shtlst))
        )
        (setq f1 (haws-close f1))
        (if (not pgpmod)
          (progn
            (setq
              f1     (haws-open temp "w")
              shtlst (reverse shtlst)
            )
            (foreach dwgfil shtlst (write-line dwgfil f1))
            (setq f1 (haws-close f1))
            (setvar "re-init" 16)
          )
        )
        (while (not column)
          (setq
            flspec
             (getstring
               t
               "\nFiles to process using OS wildcards (eg. * or grad\\*): "
             )
          )
          (vl-cmdf
            "run"
            (strcat "attrib \"" flspec ".dwg\" > " lstfil)
          )
          (setq
            f1 (haws-open lstfil "r")
            dwgfil
             (read-line f1)
            column
             (strlen dwgfil)
          )
          (cond
            ((wcmatch dwgfil "* not found *")
             (setq
               column nil
               f1 (haws-close f1)
             )
             (alert
               (princ
                 (strcat
                   "The operating system could not find\nany files found matching the wildcard:\n\n "
                   flspec
                   "\n\nPlease try again."
                 )
               )
             )
            )
            (t
             (while (not
                      (and
                        (wcmatch
                          (strcase (substr dwgfil column))
                          (strcase (strcat flspec "`.DWG"))
                        )
                        (or (= "\\" (substr dwgfil (1- column) 1))
                            (= "\\" (substr dwgfil column 1))
                            (= ":" (substr dwgfil (1+ column) 1))
                        )
                      )
                    )
               (setq column (1- column))
             )
             (setq
               f1     (haws-close f1)
               f1     (haws-open lstfil "r")
               shtlst nil
             )
             (while (setq dwgfil (read-line f1))
               (setq
                 dwgfil
                  (substr dwgfil column)
                 shtlst
                  (cons
                    (substr dwgfil 1 (- (strlen dwgfil) 4))
                    shtlst
                  )
               )
             )
             (setq
               f1 (haws-close f1)
               f1 (haws-open lstfil "w")
             )
             (setq shtlst (reverse shtlst))
             (foreach dwgfil shtlst (write-line dwgfil f1))
             (setq f1 (haws-close f1))
            )
          )
        )
       )
       ((= temp "Select")
        (setq
          lstfil
           (strcat dn ".lst")
          f1 (haws-open lstfil "w")
        )
        (while (setq
                 dwgfil
                  (getfiled
                    "File to process (Cancel when Finished)"
                    ""
                    "DWG"
                    6
                  )
               )
          (write-line (substr dwgfil 1 (- (strlen dwgfil) 4)) f1)
        )
        (setq f1 (haws-close f1))
       )
     )
    )
  )
  (setq
    rsname
     (getfiled
       "Select script to repeat on each drawing"
       ""
       "SCR"
       2
     )
    f1 (haws-open lstfil "r")
    f2 (haws-open (strcat msnameshort ".scr") "w")
    dwgfil
     (read-line f1)
  ) ;_ end of setq
  (setq column 0)
  (if (wcmatch dwgfil "*:\\*,*\\\\*")
    (while (not
             (wcmatch
               (substr dwgfil (setq column (1+ column)))
               "?:\\*,\\\\*"
             )
           )
    )
    (setq column 1)
  ) ;_ end of if
  (setq dwgfil (substr dwgfil column))
  ;;  Begin writing script file.
  ;;  First script line sets pre R-12 commands and opens first drawing.
  (if (and
        (<= 15.0 (atof (getvar "acadver")))
        (= 0 (getvar "sdi"))
      )
    (princ "sdi 1 " f2)
  ) ;_ end of if
  (write-line (strcat "qsave open \"" dwgfil "\"") f2)
  ;;  Write script lines for each drawing.
  (while dwgfil
    ;;  Script line sets newly opened drawing to pre R-12 commands.
    ;;  Other commands for each dwg. may be added or deleted after "cmddia 0".
    (write-line "filedia 0 cmddia 0" f2)
    ;;  Embed selected script file.
    (setq rptscr (haws-open (findfile rsname) "r"))
    (while (setq rdscr (read-line rptscr))
      (write-line rdscr f2)
    )
    (setq rptscr (haws-close rptscr))
    ;;  Save each drawing.
    (princ "filedia 1 cmddia 1\nqsave " f2)
    (setq dwgfil (read-line f1))
    ;;  If there is another file to process, open it.
    (if dwgfil
      (setq dwgfil (substr dwgfil column))
    )
    (if (and
          dwgfil
          (or (findfile dwgfil) (findfile (strcat dwgfil ".dwg")))
        )
      (write-line (strcat "open \"" dwgfil "\"") f2)
      (setq dwgfil nil)
    ) ;_ end of if
  ) ;_ end while
  (if (and
        (<= 15.0 (atof (getvar "acadver")))
        (= 0 (getvar "sdi"))
      )
    (write-line "sdi 0" f2)
  ) ;_ end of if
  (setq f2 (haws-close f2))
  (setq f1 (haws-close f1))
  (if del
    (vl-cmdf "sh" (strcat "del \"" msnamelist "\""))
  ) ;_ end of if
  (if (and
        (<= 15.0 (atof (getvar "acadver")))
        (= 0 (getvar "sdi"))
      )
    (alert
      (strcat
        "MSCRIPT is now ready to process drawings, but must return to Single Drawing Interface (pre-2000) first."
        "\n\nAfter closing this message, please verify that all drawing windows"
        "\nexcept the current drawing are closed"
        "\nbefore letting MSCRIPT process drawings."
        "\n\nShould your script fail for any reason,"
        "\nyou will need to turn SDI off (SDI 0) before working with multiple drawings again."
       ) ;_ end of STRCAT
    ) ;_ end of ALERT
  ) ;_ end of if
  (initget "Yes No")
  (if (= (getkword
           "\nProcess drawings now after saving current dwg?[Yes/No]:"
         )
         "Yes"
      )
    (vl-cmdf "._script" msnameshort)
    (prompt
      (strcat
        "Done.  Run script file '"
        msnameshort
        "' to process."
      )
    )
  ) ;_ end of if
  (haws-core-restore)                         ;requires library function
  (princ)
) ;_ end of DEFUN

;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
