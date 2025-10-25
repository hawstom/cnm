; TIP964.LSP: LP.LSP   Layer List to File   (c)1994, Michael L. Jenkins

(defun c:haws-laprn (/ status lyr file lyr_list)
(haws-core-init 79)

  (defun _today ()
    (strcat
      (substr (rtos (getvar "cdate") 2 0) 5 2)
      "/"
      (substr (rtos (getvar "cdate") 2 0) 7 2)
      "/"
      (substr (rtos (getvar "cdate") 2 0) 3 2)
    )
  )

  (defun _time ()
    (strcat
      (substr (rtos (getvar "CDATE") 2 6) 10 2)
      ":"
      (substr (rtos (getvar "CDATE") 2 6) 12 2)
      ":"
      (substr (rtos (getvar "CDATE") 2 6) 14 2)
    )
  )

  (defun _midstr (string pos c)
    (if (> pos 1)
      (strcat (substr string 1 (1- pos)) c (substr string (+ pos (strlen c))))
      (strcat c (substr string (+ pos (strlen c))))
    )
  )

  (setq
    lyr (tblnext "LAYER" t)
    lyr_list nil
  )
  (while lyr
    (setq
      lyr_list (append lyr_list (list (cdr (assoc 2 lyr))))
      lyr (tblnext "LAYER")
    )
  )
  (setq lyr_list (acad_strlsort lyr_list))
  (prompt "\nGenerating report...")
  (setq file (open (strcat (haws-getdnpath) ".TXT") "w"))
  (write-line (strcat "Current Date: "(_today)) file)
  (write-line (strcat "Current Time: "(_time)) file)
  (write-line (strcat "Current User: "(strcase(getvar"loginname"))) file)
  (write-line (strcat "Drawing Name: "(haws-getdnpath)) file)
  (write-line (strcat "Total Layers: " (itoa (length lyr_list))) file)
  (write-line "Layer Name                                     Of Fr Lk Color Linetype         " file)
  (write-line "-----------------------------------------------------------------------------" file)
  (foreach
    lyr
    lyr_list
    (setq
      lyr (tblsearch "LAYER" (strcase lyr))
      status "                                                                              "
      status (_midstr status 1 (cdr (assoc 2 lyr)))
      state (cdr (assoc 70 lyr))
    )
    (if
      (< (cdr (assoc 62 lyr)) 0)
      (setq status (_midstr status 48 "X"))
    )
    (if
      (not (/= 1 (logand 1 state)))
      (setq status (_midstr status 51 "X"))
    )
    (if
      (= 4 (logand 4 state))
      (setq status (_midstr status 54 "X"))
    )
    (setq
      status (_midstr status 58 (itoa (abs (cdr (assoc 62 lyr)))))
      status (_midstr status 63 (cdr (assoc 6 lyr)))
    )
    (write-line status file)
  )
  (close file)
  (prompt "\rGenerating report...DONE!")
  (princ)
); end lp.lsp

