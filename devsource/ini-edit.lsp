;;; INI-EDIT.LSP contains functions to create and edit 
;;; Windows-Style INI files, and to retrieve data from 
;;; this files.
;;;
;;; INI-EDIT is freeware by
;;; Thomas Berger
;;; imago mundi CAD Systemhaus GmbH
;;;
;;; You may use and misuse this file and the included functions as you like.
;;; You may copy and distribute this file in whole or in parts freely.
;;; No warranties of course!
;;;
;;; If you like you may send me an email and tell me where you included 
;;; this functions.
;;; 
;;; CIS: Thomas Berger, 100326,2312
;;; Internet: imago@compuserve.com
;;; http://ourworld.compuserve.com/homepages/imago
;;;
;;;
;;; FUNCTIONS
;;;
;;; (INI_WRITEENTRY inifile section variable value)
;;;     inifile   - string,  a valid filename
;;;     section   - string,  a section name ("[" and "]" brackets will be added automatically!)
;;;     variable  - string,  a name of a variable in a INI file section
;;;     value     - string,  the value for the variable
;;;
;;;     This function creates a new or modifies an existing section entry. If the INI
;;;     file does not exist it will be created automatically.
;;;       [section]
;;;       variable=value
;;;
;;;     Example:
;;;       (ini_writeentry "c:/test.ini" "NEWSECTION" "NEWVALUE" "100")
;;;     creates a new ini file with content:
;;;       [NEWSECTION]
;;;       NEWVALUE=100
;;;
;;;
;;; Function added by TGH 2/22/05
;;; (INI_WRITESECTION inifile section entrylist)
;;;     inifile   - string,  a valid filename
;;;     section   - string,  a section name ("[" and "]" brackets will be added automatically)
;;;     entrylist - list,    a list of entries '((variable value))
;;;     variable  - string,  a name of a variable in the section
;;;     value     - string,  the value for the variable
;;;
;;;     This function creates a new or modifies an entries for multiple vvairables in a section.
;;;     If the INI file does not exist it will be created automatically.
;;;       [section]
;;;       variable=value
;;;
;;;     Example:
;;;       (ini_writeentry "c:/test.ini" "NEWSECTION" '(("NEWVALUE1" "100")("NEWVALUE2" "200")))
;;;     creates a new ini file with content:
;;;       [NEWSECTION]
;;;       NEWVALUE1=100
;;;       NEWVALUE2=200
;;;
;;;
;;; (INI_READENTRY inifile section variable)
;;;     inifile   - string,  a valid filename
;;;     section   - string,  a section name ("[" and "]" brackets will be added automatically!)
;;;     variable  - string,  a name of an variable in a INI file section
;;;
;;;     This function returns the value of an existing section entry as a string:
;;;
;;;     Example:
;;;       (ini_readentry "c:/test.ini" "NEWSECTION" "NEWVALUE")
;;;     returns "100"
;;;
;;;
;;; (INI_READSECTION inifile section)
;;;     inifile   - string,  a valid filename
;;;     section   - string,  a section name ("[" and "]" brackets will be added automatically!)
;;;
;;;     This function returns an assoc list with all variables and values of a complete
;;;     ini file section
;;;
;;;     Example:
;;;       (ini_readsection "c:/test.ini" "NEWSECTION")
;;;     returns (("NEWVALUE" "100") ("NEXTVALUE" "SAMPLE"))
;;;
;;;
;;; (INI_READINI inifile)
;;;     inifile   - string,  a valid filename
;;;
;;;     This function returns an assoc list of the complete ini file. 
;;;
;;;     Example:
;;;       (ini_readini "c:/test.ini")
;;;     returns (("[NEWSECTION]" ("NEWVALUE" "100") ("NEXTVALUE" "SAMPLE")) ("[NEXTSECTION]" ("INIPATH" "c:\\TEST.INI")))
;;;
;;;
;;; (INI_INILINE line separator)
;;;     line      - a string as delivered by (read-line ..)
;;;     separator - a string of a single char
;;;
;;;     Example:
;;;       (ini_iniline "NEWVALUE=100" "=")
;;;     returns ("NEWVALUE" "100")



(defun ini_readentry (inifile section entry )
(if (and (= 'STR (type section)) (/= "[" (substr section 1 1))) (setq section (strcat "[" section "]")))
(setq section (ini_readsection inifile section))
(cadr (assoc entry section))
)

(defun ini_writeentry (inifile section entry val / ofile ini sec)
(if (not (findfile inifile)) (progn (setq ofile (haws-open inifile "w")) (haws-close ofile)))
(if (and (= 'STR (type section)) (/= "[" (substr section 1 1))) (setq section (strcat "[" section "]")))
(if (setq ofile (findfile inifile))
	(progn
		(setq ini  (ini_readini inifile))
		(cond
			((setq sec (assoc section ini))
				(if (assoc entry (cdr sec))
					(setq sec (cons section (subst (list entry val) (assoc entry (cdr sec)) (cdr sec))))
					(setq sec (cons section (reverse (cons (list entry val) (reverse (cdr sec))))))
				)
				(setq ini (subst sec (assoc section ini) ini))
				(setq ofile (haws-open ofile "w"))
				(if ofile (progn
					(mapcar
						'(lambda (x)
							(write-line (car x) ofile)
							(mapcar
								'(lambda (x)
									(write-line (strcat (car x) "=" (cadr x)) ofile)
								)
								(cdr x)
							)
							(write-line "" ofile)
						)
						ini
					)
					(haws-close ofile)
				))
			)
			(t
				(setq ofile (haws-open ofile "a"))
				(if ofile
					(progn
						(write-line "" ofile);Revised formatting bug by TGH 2/17/05
					        (write-line section ofile)
					        (princ (strcat entry "=" val) ofile)
						(haws-close ofile)
					)
				)
			)

		)
	)
)

)

(defun ini_writesection (inifile section entrylist / entry ofile ini sec)
(if (not (findfile inifile)) (progn (setq ofile (haws-open inifile "w")) (haws-close ofile)))
(if (and (= 'STR (type section)) (/= "[" (substr section 1 1))) (setq section (strcat "[" section "]")))
(if (setq ofile (findfile inifile))
	(progn
		(setq ini  (ini_readini inifile))
		(cond
			((setq sec (assoc section ini))
			 (foreach entry entrylist
				(if (assoc (car entry) (cdr sec))
					(setq sec (cons section (subst entry (assoc (car entry) (cdr sec)) (cdr sec))))
					(setq sec (cons section (reverse (cons entry (reverse (cdr sec))))))
				)
			   )
				(setq ini (subst sec (assoc section ini) ini))
				(setq ofile (haws-open ofile "w"))
				(if ofile (progn
					(mapcar
						'(lambda (x)
							(write-line (car x) ofile)
							(mapcar
								'(lambda (x)
									(write-line (strcat (car x) "=" (cadr x)) ofile)
								)
								(cdr x)
							)
							(write-line "" ofile)
						)
						ini
					)
					(haws-close ofile)
				))

			)
			(t
				(setq ofile (haws-open ofile "a"))
				(if ofile
					(progn
						(if ini (write-line "" ofile));Revised formatting bug by TGH 2/17/05
					        (write-line section ofile)
						(foreach entry entrylist
					        (write-line (strcat (car entry) "=" (cadr entry)) ofile)
						)
						(haws-close ofile)
					)
				)
			)

		)
	)
)

)

(defun ini_readsection (inifile section / ofile line result )
(if (and (= 'STR (type section)) (/= "[" (substr section 1 1))) (setq section (strcat "[" section "]")))
(if (findfile inifile)
	(cdr (assoc section (ini_readini (findfile inifile))))
	(alert (princ(strcat inifile " not found.")))
)
)



(defun ini_readini (inifile / ofile line section result)
(if (findfile inifile)
	(progn
		(setq ofile (haws-open (findfile inifile) "r"))
		(if ofile (progn
			(while (and (setq line (read-line ofile)) (/= "[" (substr line 1 1))))
			(while (and line (= "[" (substr line 1 1)))
				(setq section (list line))
				(while (and (setq line (read-line ofile)) (/= "[" (substr line 1 1)))
					(if (and (/= ";"  (substr line 1 1)) (/= "" line))
						(setq section (cons (ini_iniline line "=") section))
					)
				)
				(setq result (cons (reverse section) result))
			)
			(haws-close ofile)
		))
	)
	(alert (princ (strcat inifile "\nnot found.")))
)
(reverse result)
)

(defun ini_iniline (line sep / str1 str2 )
	(if (= 'STR (type line))
		(progn
			(setq str1 "" str2 "")
			(while (and (/= "" line) (/= sep (substr line 1 1)))
				(setq str1 (strcat str1  (substr line 1 1)) line (substr line 2))
			)
			(if (= sep (substr line 1 1))
				(setq str2 (substr line 2))
			)
		)
	)
(list str1 str2)
)
