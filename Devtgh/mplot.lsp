;(C) Copyright 1997 by Thomas Gail Haws
(defun c:haws-MPLOT (/ temp msname pt1 pt2 pltcmd flspec wrtscr rdscr rptscr rsname
  dwgfil column del)
  (setq temp "MPLOT.LSP version 4.31 by Thomas Gail Haws")
  (if
    (not
      (setq
        f1
        (open
          (strcat
            (setq
              msname
              (strcat
                (getvar"dwgprefix")
                "mplot"
                (substr(getvar"loginname")1 3)
              )
            )
            ".scr"
          )
          "w"
        )
      )
    )
    (setq msname(strcat(getvar"dwgprefix")"mplot"))
    (setq f1 (close f1))
  )
  ;Rev. 7/22/99
  ;Plots multiple drawings.
  (haws-borrow 0);requires library function
  (textpage)
  (prompt temp)
  (prompt "\n\nMPLOT must have the following conditions to run trouble-free.")
  (prompt "\n  1) If plotting to a file, any previous plot files must be erased.")
  (prompt "\n  2) Any needed font files, xref files, etc. must be accessible.")
  (prompt "\n  3) A preplot script, if used, can not call dialogue boxes.")
  (setq temp (strcat
      "\n\nHow will you specify drawings to plot?"
      "\nUse an existing text file with a List of drawings, "
      "enter a Filespec with DOS wildcards (eg. \\dwg\\door*), "
      "or Select drawings one at a time from a dialogue box?"
    "\n<List/Filespec/Select>:")
  )
  (initget 1 "L F S")
  (setq listfil (getkword temp))
  (cond
    (
      (= listfil "L")
      (setq f1 (GETFILED "Select a List File" "" "LST" 6))
    )
    (
      (= listfil "F")
      (setq
        f1 (strcat "\"" msname ".lst\"")
        flspec (strcat (getstring "\nEnter filespec:haws-") ".dwg")
      )
      (command "shell" (strcat "attrib \"" flspec "\" > " f1))
      (setq del T)
    )
    (
      (= listfil "S")
      (setq f1 (open (strcat msname ".lst") "w"))
      (while (setq dwgfil
          (GETFILED "File to Plot (Cancel when Finished)" "" "DWG" 6)
        )
        (write-line dwgfil f1)
      )
      (setq f1 (close f1) f1 (strcat msname ".lst"))
    )
  );end cond
  (initget 1 "Yes No")
  (setq rsname
    (if
      (=
        (getkword "\nRun a standard script file before plotting each drawing?<Y/N>")
        "Yes"
      )
      (GETFILED "Select script to repeat before each plot" "" "SCR" 2)
    )
  )
  (setq
    f1 (open (findfile f1) "r")
    f2 (open (strcat msname ".scr") "w")
    dwgfil (read-line f1)
  )
  (initget 1 "Display Extents Limits View Window")
  (setq pltcmd
    (strcat
      "plot "
      (setq temp (getkword "\nPlot area?<Display/Extents/Limits/View/Window>:"))
      (cond
        ( (= temp "Window")
          (strcat
            " "
            (rtos (car(setq pt1(getpoint"\nFirst corner:"))) 2)
            ","
            (rtos (cadr pt1) 2 8)
            " "
            (rtos (car(setq pt2(getcorner pt1"Other corner:"))) 2)
            ","
            (rtos (cadr pt2) 2 8)
        ) )
        ( (= temp "View")
          (strcat " "(getstring "View name:"))
        )
        (T "")
      )
      (if (<= 14 (atof (getvar "acadver"))) "  " "   ")
    )
  )
  (setq column 0)
  (if (wcmatch dwgfil "*:\\*,*\\\\*")(while (not (wcmatch (substr dwgfil (setq column (1+ column))) "?:\\*,\\\\*")))(setq column 1))
  (setq dwgfil (substr dwgfil column))
  ;  Begin writing script file.
  ;  First script line sets pre R-12 commands and opens first drawing.
  (write-line (strcat "filedia 0 cmddia 0 qsave open \"" dwgfil "\"") f2)
  ;  Write script lines for each drawing.
  (while dwgfil
    ;  Script line sets newly opened drawing to pre R-12 commands.
    ;  Other commands for each dwg. may be added or deleted after "cmddia 0".
    (write-line "filedia 0 cmddia 0" f2)
    ;  Embed another script file (if given) for pre-plot execution.
    (if
      rsname
      (progn
        (setq rdscr nil  rptscr (open (findfile rsname) "r"))
        (while (setq rdscr (read-line rptscr))
          (write-line rdscr f2)
        )
        (close rptscr)
      )
    )
    ;  Plot each drawing. Save.
    (princ (strcat pltcmd "filedia 1 cmddia 1 qsave") f2)
    (setq dwgfil (read-line f1))
    ;  If there is another file to plot, open it.
    (if dwgfil
      (progn
        (setq dwgfil (substr dwgfil column))
        (write-line (strcat " open \"" dwgfil "\"") f2)
      )
    )
  );end while
  (close f2) (close f1)
  (if del (command "sh" (strcat "del \""msname".lst\"")))
  (initget "Yes No")
  (if
    (= (getkword "\nPlot drawings now after saving current dwg?<Y/N>:") "Yes")
    (progn (command "script" msname))
    (prompt (strcat "Done.  Run script file '"msname"' to plot."))
  )
  (haws-return);requires library function
  (princ)
);end MPLOT
