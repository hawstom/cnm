;#region HEAD
;;;
;;; ICAD compatibility issues:
;;;
;;; Can't compile code to make licensing scheme secret.  Must use
;;; different
;;; scheme.
;;;
;;; About version control
;;; 20170907 4.2.30 TGH See Git. Revamped bubble notes. Removed PGP. Reworked installer to replace AcadInst.exe. Improved CNM project mgt. Command spreadsheet audit/enhance.
;;; 20151001 4.2.29 TGH Added BIOS date registry location for Windows 10.
;;; 20150921 4.2.28 TGH Added LWP and LWPX.
;;; 20150916 4.2.27 TGH Fixed (vl-cmdf) incompatibility with v2015+ in lambdas.  Using (vl-cmdf-s).
;;; 20111021 4.2.21 TGH Made MSCRIPT use VBA only for releases 15 through 17 (2000 through 2009).  CNM QT had already been fixed that way.
;;; 20090923 4.2.20 TGH Changed authorization scheme in many ways to fix bugs.  Works with setcfg and still gets old stuff from registry.
;;; 20090923 4.2.19 TGH Changed authorization scheme (for Jared Cox) to use only HKCU section.
;;; 20090623 4.2.18 TGH Further fixed bug in CNM that caused no count (manual) notes to list quantities from other tabs in CTABONLY mode.  Also txtht didn't work after TITLE. 
;;; 20090622 4.2.17 TGH Fixed NOTEFND bug in CNM that caused notes to list even if not found if previously in table. 
;;; 20090309 4.2.16 TGH Fixed capitalization bug in CNM vars.  Vars remain case sensitive. 
;;; 20090205 4.2.15 TGH Fix bugs in (HCNM-PROJ) that were wiping Project Notes file clean if ProjectNotes= path was invalid. 
;;; 20081020 4.2.13 TGH Made CNM Bubbles follow a dimstyle stored in  CNM.INI. 
;;; 20080906 4.2.11 TGH Changed layers toward AIA/NCS.  Fixed CNM bubble hook bug. 
;;; 20080520 4.2.07 TGH Added non-nagging expiration mode and made nag mode a random 20% chance nag.
;;; 20080508 4.2.05 TGH Customized copy protection for XP, Vista, and ICAD.  Enhanced GETINTX, GETDISTX, and GETREALX.  Fixed CNM Table Grid INI var mismatch.
;;; 20080111 4.2.04 TGH Added phases A-Z to CNM for Danny Shahenian at DEI.
;;; 20071228 4.2.03 TGH Fixed a bug in timed trial reporting. Eased copy protection to use only (setcfg).
;;; 20070926 4.2.02 TGH Removed HKLM wssh `RegWrite and HKLM regwrite.  Swapped all to (setcfg) and (setenv) for Vista and ICAD compatibility.
;;; 20070818 4.2.01 TGH Fixed several bugs throughout incl stacl, layersav.  Made several globals local. Fixed HKLM wssh `RegWrite for Vista.
;;; 20050831 1.05   TGH Changed CNM to version 4.2.00.  Recompiled
;;; (legacy)
;;; lisputil.lsp
(prompt "\nHawsEDC library functions...")
(load "haws-tip")
;;;This is the current version of HawsEDC and CNM
(defun haws-unified-version ()
  "5.5.18"
)
(defun haws-copyright ()
  "Copyright 2025 Thomas Gail Haws"
)
;;;(SETQ *HAWS-ICADMODE* T);For testing icad mode in acad.
(setq *haws-debuglevel* 0)
;;This function returns the current setting of nagmode.
;;Elizabeth asked me to give her a version with no nag mode (direct to fail).
(defun haws-nagmode-p () t)
;; Returns a random CNM evangelism message for tips/prompts (sharing-focused)
(defun haws_evangel_msg (/ big_date msgs idx)
  (setq msgs (list
    "\nCNM is open source! Share it far and wide."
    "\nShare CNM with your colleagues and help it grow! https://github.com/hawstom/cnm"
    "\nSpread the word: CNM is open source."
    "\nIf you can describe it, we can make it happen."
    "\nLeverage modern AI plus 30 years of human experience to make AutoCAD work for you!"
    "\nShare CNM, discuss CNM, and contribute to CNM at https://github.com/hawstom/cnm"
    "\nCNM Bubble notes now offer auto text that reacts to leader and reference object changes."
    "\nDid you see LABEL (HAWS_LABEL), our new utility research pipe labeler? Edit haws-label-settings.lsp."
    "\nYou can open a suggestion or bug issue at at https://github.com/hawstom/cnm by registering at Github."
    "\nThe latest upgrades to CNM were made possible by AI! (VS Code IDE > Copilot AI agent > Claude Sonnet 4.5 model)"
    "\nGive back to CNM. Contact Tom to learn how you can be an AI-supported programmer."
    "\nGet involved with CNM at https://github.com/hawstom/cnm"
    "\nTake CNM with you wherever you work. Share it, share your ideas, and help it grow!"
    "\nCNM is a community project! Make a difference by sharing it and making it better."
    "\nContribute to CNM and report issues at https://github.com/hawstom/cnm"
  ))
  (setq big_date (* (getvar "DATE") 100000000))
  (setq idx  (rem (fix (* 10000 (- big_date (fix big_date)))) (length msgs)))
  (nth idx msgs)
)
(defun c:hcnm-about ()
(haws-core-init 216) (c:haws-about))
(defun c:haws-about (/ licensereport)
(haws-core-init 217)
  (alert
    (princ
      (strcat
        "Construction Notes Manager version "
        (haws-unified-version)
        "\n\n" (haws-copyright) "\nhttp://constructionnotesmanager.com\nhttp://hawsedc.com\n"
        (apply
          'STRCAT
          (mapcar
            '(lambda (package)
               (if (setq
                     licensereport
                      (haws-packagelicensereport
                        (car package)
                      )
                   )
                 (strcat
                   "\n"
                   (cadr package)
                   " authorized on this computer. Key: "
                   licensereport
                 )
                 ""
               )
             )
            *haws-edcmodules*
          )
        )
      )
    )
  )
  (princ)
)

;#endregion;
;#region ERROR AND PACKAGING
;;=================================================================
;;
;;                Error Handler and Licensing functions
;;
;;=================================================================
;;
;; The HawsEDC authorization routine
;;
;; Data forms:
;; Orderlist    '(pkgid numauths compname biosmonth biosday biosyear
;; startdate)

;;App groups are:
;;  0 HawsEDC catch-all misc apps
;;  1 NOTES apps
;;  2 CNMEdit.exe
;;Marketing packages (called "Modules" where stored) are:
;;  0 Full HawsEDC package
;;  1 NOTES package standard
;;  2 NOTES package with CNMEdit.exe
;;  3 Full HawsEDC with CNMEdit.exe (CNM Pro plus HawsEDC tools)
;;  4 Layer tools
;; Don't forget to add new packages to HAWS-CHECKSTOREDSTRINGS function at
;; bottom
;; of this
;; file.
(setq
  *haws-edcmodules*
   '((0 "CNM Lite") (3 "CNM Pro"))
  *haws-edcappgroups*
   '(;; App group -1 is free of protection
     (-1)
     ;; App group 0 is included in package 0 and 3
     (0 0 3)
     ;; App group 1 is included in packages 0, 1, and 2
     (1 0 1 2 3)
     ;; App group 2 is included in packages 2 and 3
     (2 2 3)
     ;; App group 128 (flag for showing all packages) is included in all packages
     (128 0 1 2 3)
    )
  ;; This sets the sales tracking flag for this executable compilation.
  *haws-originator*
   "TGH"
  *haws-salesid*
   (cond
     ((= *haws-originator* "TGH") 0)
     ((= *haws-originator* "PCW") 1)
     ((= *haws-originator* "DOW") 2)
   )
)

;;;HawsEDC general function handler
;;;Includes banner, error handler, and validator.
;;; Internal error handler function.  Call haws-core-init at the
;;; beginning
;;; of a
;;; routine.
;;;Call errrst at the end to restore old *error* handler.
;;;To restore previous UCS, set a global symbol 'ucsp to non-nil.
;;; To restore another previous UCS, set a global symbol 'ucspp to
;;; non-nil.
(defun haws-core-init (command-id / appgroup validated)
  (setq appgroup (cadr (assoc command-id *haws-edccommands*)))
  ;; If computer already has authorization,
  (cond
    ((or
         ;; No more nag mode.
         t
         ;; this computer has authorization
         (haws-validateappgroup appgroup)
         ;; or we successfully get it from the user,
         (haws-authorizeappgroup appgroup)
         ;; or we are allowing run after nag,
         (haws-nagmode-p)
     )
     (setq validated t)
    )
    (t
     (alert
       (princ
         "Application must be authorized to run.\n\nClosing application."
       )
     )
     (exit)
    )
  )
  (haws-use-log-local command-id)
  (setq
    olderr *error*
    *error* haws-core-stperr
  )
  ;;Versional housekeeping
  (if (= 'subr (type *push-error-using-command*)) (*push-error-using-command*))
  validated
)

;;Stperr replaces the standard error function.
;;It sets everything back in case of an error.
(defun haws-core-stperr (s)
  ;; Restore old *error* handler
  (if olderr
    (setq
      *error* olderr
      olderr nil
    )
  )
  (cond
    ((/= s "Function cancelled")
     (princ (strcat "\nTrapped error: " s))
    )
  )
  (while (< 0 (getvar "cmdactive"))
    (vl-cmdf)
  )
  (if (= (type f1) (quote file))
    (setq f1 (close f1))
  )
  ;; Close files
  (if (= (type f2) (quote file))
    (setq f2 (close f2))
  )
  (if (= (type f3) (quote file))
    (setq f3 (close f3))
  )
  ;;Versional housekeeping
  (if (/= 'subr (type command-s)) (setq command-s command))
  (if (= 8 (logand (getvar "undoctl") 8))
    (vl-cmdf "._undo" "end")
  )
  ;; End undo group
  (if vstr
    (haws-vrstor)
  )
  ;; Restore variables to previous values
  (if ucsp
    (vl-cmdf "._UCS" "_P")
  )
  ;; Restore previous UCS
  (if ucspp
    (vl-cmdf "._UCS" "_P")
  )
  ;; Restore previous UCS
  (if enm
    (redraw enm)
  )
  ;; Redraw work entity
  (if errosm
    (setvar "osmode" errosm)
  )
  (setq
    ucsp nil
    ucspp nil
    enm nil
  )
  (princ)
)
(defun haws-core-restore ()
  (setq
    ucsp nil
    ucspp nil
    enm nil
    f1 nil
    f2 nil
    *error* olderr
    olderr nil
  )
)
;;END ERROR HANDLER

;;VALIDATEAPPGROUP looks for authorization to run an application.
;;Looks for any package that
;;includes the requested application group
;;and has authorization.
;;First checks for a stored applicable package authorization.
;; If no applicable stand-alone package authorizations found, but a
;; network
;; license,
;;takes an appgroup token from the network server.
(defun haws-validateappgroup (appgroup / authdatedaysahead
                          userstringdatediff authorizedpermanent
                          authstring orderstring package trialdaysleft
                         )
  ;;To begin with, assume there is no free trial left.
  (setq trialdaysleft -1)
  ;;Check whether any of the packages that include the requested appgroup
  ;;are authorized to run.
  (foreach
     package (cdr (assoc appgroup *haws-edcappgroups*))
    (haws-milepost
      (strcat
        ";;Package "
        (itoa package)
        " listed as including this appgroup."
      )
    )
    ;;If
    (cond
      ((and
         ;;Permanent authorization hasn't already been found in some package
         (not authorizedpermanent)
         ;;There is an orderstring found for this package,
         (setq orderstring (haws-readordercode package))
         ;;and there is an authstring found for this package,
         (setq authstring (haws-readauthcode package))
         ;;And the order and authorization strings match (return a date difference)
         ;;(machine matching should have already been checked by HAWS-CHECKSTOREDSTRINGS on loadup)
         (setq
           userstringdatediff
            (haws-userstringdatediff
              orderstring
              authstring
            )
         )
       )
       (haws-milepost
         (strcat
           "Auth match found. P:"
           (itoa package)
           " O:"
           orderstring
           ", A:"
           authstring
           ", Diff:"
           (itoa userstringdatediff)
         )
       )
       ;;Then
       ;;Decide whether this package is authorized.
       (haws-milepost
         (strcat
           "Current trial stands at:"
           (itoa (fix trialdaysleft))
         )
       )
       (cond
         ;; If unlimited stand-alone auth is found for this appgroup
         ;; (auth code date is at least 1000000 days before order code date)
         ;; in this package, authorize immediately.
         ((<= userstringdatediff -1000000)
          (setq authorizedpermanent t)
         )
         ;;Else if this authdate extends the current trial length, let it.
         (t
          (setq
            trialdaysleft
             (max
               trialdaysleft
               (- (car (haws-authtolist authstring))
                  (fix (getvar "date"))
               )
             )
          )
         )
       )
      )
    )
  )
  (cond
    ;;If authorized permanently, return T
    ((or authorizedpermanent (= appgroup -1)) t)
    ;;Else if there are positive days left in a trial, print a note and return T
    ((not (minusp trialdaysleft))
     (princ
       (strcat
         "\nYou have "
         (itoa trialdaysleft)
         " days left in your trial period."
       )
     )
     t
    )
  )
)

;;; AUTHORIZEAPPGROUP gives user order code, prompts user for matching
;;; authorization
;;; code,
;;; and records authorization for future reference.
;;;
;;; This is the nag routine.
(defun haws-authorizeappgroup (appgroup)
  ;;Choose nag mode or expire mode prompt.
  (cond
    ;;If nag mode is active
    ((haws-nagmode-p)
     ;;Then
     (cond
       ;;If it's about 1/100th of the time.
       ;;(This expression holds true everytime (getvar "date") rolls around to a 00 in the 1/100,000,000 of a day place.)
       ((=
          (substr (rtos (rem (getvar "date") 1) 2 8) 9)
          "00"
        )
        (alert
          (princ
            (strcat
              "\nThis application is running in unlicensed mode."
              "\n\nLicense it or extend the expired trial mode"
              "\nby emailing the order code generated at the following command line prompts."
            )
          )
        )
        (haws-orderpackage appgroup)
       )
       (t (princ "\nContinuing in evaluation mode.") t)
     )
    )
    ;;Else expire mode is active
    (t
     (alert
       (princ
         (strcat
           "\nThis application has not been authorized."
           "\n\nYou must order trial or paid authorization"
           "\nby emailing the order code generated at the following command line prompts."
         )
       )
     )
     (haws-orderpackage appgroup)
    )
  )
)

(defun haws-packagelicensereport (package / datediff)
  (setq
    authstring
     (haws-readauthcode package)
    datediff
     (haws-packageuserstringdatediff package)
  )
  (cond
    ((not (and datediff authstring)) nil)
    ((<= datediff -1000000) (strcat authstring " (unlimited)"))
    ((>= datediff 31)
     (strcat authstring " (" (itoa datediff) " day trial)")
    )
    (t nil)
  )
)

(defun haws-packageuserstringdatediff
   (package / authstring orderstring userstringdatediff)
  (cond
    ((and
       ;;There is an orderstring found for this package,
       (setq orderstring (haws-readordercode package))
       ;;and there is an authstring found for this package,
       (setq authstring (haws-readauthcode package))
       ;;And the order and authorization strings match (return a date difference)
       ;;(machine matching should have already been checked by HAWS-CHECKSTOREDSTRINGS on loadup)
       (setq
         userstringdatediff
          (haws-userstringdatediff
            orderstring
            authstring
          )
       )
     )
     (haws-milepost
       (strcat
         "Auth match found. P:"
         (itoa package)
         " O:"
         orderstring
         ", A:"
         authstring
         ", Diff:"
         (itoa userstringdatediff)
       )
     )
     (haws-userstringdatediff orderstring authstring)
    )
  )
)

;;; USERSTRINGDATEDIFF compares an authorization string and a user
;;; string.
;;; Returns a date difference if the strings match.
;;; Returns nil if strings don't match.
(defun haws-userstringdatediff
   (orderstring authstring / acaddate authlist orderlist)
  (cond
    ;; If AUTHSTRING isn't the same length as ORDERSTRING,
    ;; comparison fails.  Return nil.
    ((/= (strlen authstring) (strlen orderstring)) nil)
    ;; If any mismatches besides date, comparison fails.
    ;; Return nil.
    ((not
       (equal
         (cdr (setq orderlist (haws-ordertolist orderstring)))
         (cdr (setq authlist (haws-authtolist authstring)))
       )
     )
     nil
    )
    ;;Else return the difference.
    (t (- (car authlist) (car orderlist)))
  )
)

(defun haws-readordercode (package)
  (haws-readpackagecode package "OrderString")
)

(defun haws-readauthcode (package)
  (haws-readpackagecode package "AuthString")
)

;;;(HAWS-READPACKAGECODE 0 "AuthString")
;;;(HAWS-READPACKAGECODE 0 "OrderString")


;;; Returns stored string or nil.
;;; (getcfg) returns nil if the section doesn't exist.  Returns "" if the param doesn't exist.
;;; Since this is executed by (HAWS-CHECKSTOREDSTRINGS) on load, we can use this function to transfer stored info from the registry
;;; to the new storage location, and we can depend on (getcfg) returning nil the first time.
(defun haws-readpackagecode (package stringname / storedstring)
  (haws-milepost
    (strcat
      "Beginning READPACKAGECODE for package "
      (itoa package)
      " "
      stringname
    )
  )
  (haws-readcfg
    (list "HawsEDC" "Modules" (itoa package) stringname)
  )
)

;;; HAWS-READCFG gets the value of a setting from the favorite HAWS-location.
;;; (getcfg) returns nil if the section doesn't exist.  Returns "" if the param doesn't exist.
(defun haws-readcfg (locationlist / moverequired returnvalue storedstring)
  (cond
    ;;If the requested value is in (getcfg), use it.
    ;;Intentionally here, we are using "".
    ;;This means we will only try the registry on a virgin install of this logic.
    ((setq
       storedstring
        (getcfg
          ;;CFG section
          (haws-locationtocfgsection locationlist)
        )
     )
    )
    ;;Else if the value is set in the HKCU registry section, use it and write to (setcfg).
    (;;Read from the HKCU registry section
     (setq
       storedstring
        (haws-registry-read
          ;;Registry path
          (haws-locationtoregistrypath
            locationlist
            "HKEY_CURRENT_USER"
          )
          ;;Registry key
          (car (reverse locationlist))
        )
     )
     ;;Flag to move value
     (setq moverequired t)
    )
    ;;Else try reading it from the HKLM section
    ((setq
       storedstring
        (haws-registry-read
          ;;Registry path
          (haws-locationtoregistrypath
            locationlist
            "HKEY_LOCAL_MACHINE"
          )
          ;;Registry key
          (car (reverse locationlist))
        )
     )
     ;;Flag to move value
     (setq moverequired t)
    )
  )
  (cond
    ;;Write to preferred location if flagged and return stored string
    ((and storedstring moverequired)
     (setcfg
       ;;CFG section
       (haws-locationtocfgsection locationlist)
       ;;Value
       storedstring
     )
    )
  )
  storedstring
)


;;;(HAWS-WRITEPACKAGECODE 0 "AuthString")
;;;(HAWS-WRITEPACKAGECODE 0 "OrderString")


;;; Stores string and returns stored string or nil.
;;; (getcfg) returns nil if the section doesn't exist.  Returns "" if the param doesn't exist.
(defun haws-writepackagecode
   (package stringname stringvalue / storedstring)
  (haws-milepost
    (strcat
      "Beginning WRITEPACKAGECODE for package "
      (itoa package)
      " "
      stringname
    )
  )
  (haws-writecfg
    (list "HawsEDC" "Modules" (itoa package) stringname)
    stringvalue
  )
)


;;; HAWS-WRITECFG sets the value of a setting in the favorite HAWS- location.
(defun haws-writecfg (locationlist inputstring / storedstring)
  (setq
    storedstring
    (setcfg
      ;;CFG section
      (haws-locationtocfgsection locationlist)
      ;;Value
      inputstring
   )
  )
)

(defun haws-locationtocfgsection (locationlist)
  (strcat
    ;;Start path with the AppData/ prefix per AutoCAD help.
    "AppData/"
    ;;Add the rest of the location provided.
    (haws-lsttostr locationlist "/" "\"")
  )
)

(defun haws-locationtoregistrypath (locationlist root)
  (strcat
    ;;Start with the prefix to get into the software area
    root
    "\\Software\\"
    ;;Add all but the last element of the location provided 
    (haws-lsttostr
      (reverse (cdr (reverse locationlist)))
      "\\"
      "\""
    )
  )
)

;;; ORDERPACKAGE is called when the user opts to order.
;;; It generates an order string and prompts for a matching authorization string.
;;; If authorization is provided correctly, it writes both to the storage location.,
;;; Returns T if successful, nil if not successful.

(defun haws-orderpackage (appgroup / authmatchsuccess authstring
                      authwritesuccess nagmatchsuccess nagstrings
                      orderlist orderstring package writestrings
                     )
  (setq
    orderlist
     (append
       (list (fix (getvar "date")) *haws-salesid*)
       (setq package (haws-getpackage appgroup))
       (list (haws-getshortcomputername))
       (haws-getbiosdate)
     )
    orderstring
     (haws-binarytouser
       (haws-encryptorderstring
         (haws-listtobinary orderlist)
       )
     )
  )
  (alert
    (princ
      (strcat
        "\n\nThe order code for the package you ordered will be shown below."
        "\n\nIf you already ordered an authorization key, please enter it at the following prompt."
        "\nTo order an authorization key, please close this message,"
        "\nthen copy and paste the order code shown into the contact form at www.HawsEDC.com."
      )
    )
  )
  (setq
    authstring
     (getstring
       (strcat
         "\nOrder code is: "
         orderstring
         "\nEnter authorization key or <continue>: "
       )
     )
  )
  ;;Act on input
  (cond
    ;; If a matching authorization was supplied,
    ;; flag to write the authorization and flag success,
    ((haws-userstringdatediff orderstring authstring)
     (setq writestrings t)
     (setq authmatchsuccess t)
    )
  )
  (cond
    ;; If flagged, write both strings to storage and flag success.
    (writestrings
     (haws-writecfg
       (list
         "HawsEDC"
         "Modules"
         (itoa (car package))
         "OrderString"
       )
       orderstring
     )
     (haws-writecfg
       (list "HawsEDC" "Modules" (itoa (car package)) "AuthString")
       authstring
     )
     (if (and
           (= (haws-readpackagecode (car package) "AuthString")
              authstring
           )
           (= (haws-readpackagecode (car package) "OrderString")
              orderstring
           )
         )
       (setq authwritesuccess t)
     )
    )
  )
  ;;Report to user
  (cond
    ((and authmatchsuccess authwritesuccess)
     (alert (princ "Authorization was successful."))
     t
    )
    (nagmatchsuccess t)
    (t
     (alert
       (princ
         (strcat
           "Authorization was not successful."
           (if (not authmatchsuccess)
             (strcat
               "\nOrder code\n\"" orderstring
               "\"\nand authorization key\n\"" authstring
               "\"\ndon't match."
              )
             ""
           )
         )
       )
     )
     nil
    )
  )
)


;;; HAWS-ORDERLICENSES gets an order code from the command line
(defun c:haws-orderlicenses ()
(haws-core-init 218)
  (haws-orderpackage 128)
  (princ)
)
;;GETPACKAGE displays a user form to get an order for the package
;;to authorize.
;;Returns a list of package number and number of authorizations.
;; 0 authorizations means authorization is for single user unlimited
;; use.
(defun haws-getpackage (appgroup / packages string)
;;;Begin user choice section
  (textpage)
  (prompt
    (strcat
      "\n\n==========================================================================="
      (cond
        ((= appgroup 128)
         "\n\nAuthorization is available for the following packages:\n"
        )
        ("\n\nThe tool you want to use is included in the following packages:\n"
        )
      )
    )
  )
  (setq packages (cdr (assoc appgroup *haws-edcappgroups*)))
  (cond
    ((member 0 packages)
     (prompt "\nCNM Lite (no custom Project Notes editor) .......1")
    )
  )
  (cond
    ((member 3 packages)
     (prompt "\nCNM Pro (includes custom Project Notes editor) ..2")
    )
  )
  (initget "1 2")
  (setq
    string
     (getkword "\n\nSelect a package
   [1/2]
   <1>:
   ")
  )
  ;;  "1" Full HawsEDC package
  ;;  "2" NOTES package standard
  ;;  "3" NOTES package with CNMEdit.exe
  ;;  "4" Full HawsEDC with CNMEdit.exe (CNM Pro plus HawsEDC tools)
  (setq
    string
     (cond
       ((= string "1") "1")
       ((= string "2") "4")
       (t "1")
     )
  )
;;;End user choice section
;;;=====================================================
;;;Begin Construction Notes Manager Pro only section
;;;  (PROMPT "\nGetting single seat authorization for Construction Notes Manager Pro.") ; prompt
;;;  (SETQ STRING "2")
;;;End Construction Notes Manager only section
  (list (1- (atoi string)) 0)
)

;;GetBiosDate uses BIOSDATE.EXE to return system bios date as a
;;list in the form '(mm dd yy).
(defun haws-getbiosdate (/ biosdatefull x)
  (setq
    biosdatefull
     (cond
       (*haws-biosdatefull*)
       ;; Win 10
       ((haws-registry-read
          "HKEY_LOCAL_MACHINE\\HARDWARE\\DESCRIPTION\\System\\BIOS"
          "BIOSReleaseDate"
        )
       )
       ;; Win NT 4.0 and Win 10
       ((haws-registry-read
          "HKEY_LOCAL_MACHINE\\HARDWARE\\DESCRIPTION\\System"
          "SystemBiosDate"
        )
       )
       ;; Win 2000 and XP
       ((haws-registry-read
          "HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Biosinfo"
          "SystemBiosDate"
        )
       )
       ;; Win 95/98/Me
       ((haws-registry-read
          "HKEY_LOCAL_MACHINE\\Enum\\Root\\*PNP0C01\\0000"
          "BIOSDate"
        )
       )
       (t "01\\01\\01")
     )
  )
  (setq *haws-biosdatefull* biosdatefull)
  (list
    (atoi (substr biosdatefull 1 2))
    (atoi (substr biosdatefull 4 2))
    (atoi (substr biosdatefull 7 2))
  )
)

;;; GetComputerName gets the computer name from the registry if
;;; possible.
;;; If not, returns "".
(defun haws-getcomputername ()
  (setq
    *haws-computername*
     (cond
       (*haws-computername*)
       ((haws-registry-read
          (strcat
            "HKEY_LOCAL_MACHINE\\System\\CurrentControlSet"
            "\\Control\\ComputerName\\ComputerName"
          )
          "ComputerName"
        )
       )
       ("O NO NAME FOUND O")
     )
  )
)




;;; GETSHORTCOMPUTERNAME gets the first and last characters of the computer name
;;; if possible.
;;; If not, returns "".
(defun haws-getshortcomputername (/ computername)
  (setq computername (haws-getcomputername))
  (strcat
    (substr computername 1 1)
    (haws-endstr computername 1 1)
  )
)

;;; LISTTOBINARY converts a list
;;; Orderlist
;;;  '(startdate salesid pkgid numauths compname biosmonth biosday biosyear)
;;; to a 72 bit binary string as follows:
;;; Bits 2-4,6-8,10-12,14-26 22-bit binary number of the AutoCAD date
;;; Bits 1,5,9,13 4-bit binary sales ID#
;;; Bits 27-30 4-bit binary package ID#
;;; Bits 31-40 10-bit binary number of authorizations
;;; Bits 41-56 16-bit binary ASCII for the first and last characters of the licensed computer name
;;; Bits 57-60 4-bit binary number of the BIOS date month
;;; Bits 61-65 5-bit binary number of the BIOS date day
;;; Bits 66-72 7-bit binary number of the BIOS date year
;;; to an order list
(defun haws-listtobinary
   (orderlist / binarydate binarysales binarystring i)
  ;;Pad BINARYSTRING from 68 to 72 bits to make 12 6-bit bytes.
  (setq
    binarydate
     ;;Get the binary value of the AutoCAD date
     (haws-endstr
       (strcat
         "0000000"
         (haws-convertdecimalto (car orderlist) 2)
       )
       22
       22
     )
    binarysales
     ;;Get the binary value of the sales ID
     (haws-endstr
       (strcat
         "0000000"
         (haws-convertdecimalto (cadr orderlist) 2)
       )
       4
       4
     )
    binarystring
     (strcat
       (substr binarysales 1 1)
       (substr binarydate 1 3)
       (substr binarysales 2 1)
       (substr binarydate 4 3)
       (substr binarysales 3 1)
       (substr binarydate 7 3)
       (substr binarysales 4 1)
       (substr binarydate 10)
       ;;Get the binary value of the package id #.
       (haws-endstr
         (strcat
           "0000"
           (haws-convertdecimalto (caddr orderlist) 2)
         )
         4
         4
       )
       ;;Get the binary value of the number of authorizations.
       (haws-endstr
         (strcat
           "0000000000"
           (haws-convertdecimalto (cadddr orderlist) 2)
         )
         10
         10
       )
     )
  )
  ;;Get the binary value of the computer name fragment
  (setq i 1)
  (repeat 2
    (setq
      binarystring
       (strcat
         binarystring
         (haws-endstr
           (strcat
             "00000000"
             (haws-convertdecimalto
               (ascii (substr (nth 4 orderlist) i 1))
               2
             )
           )
           8
           8
         )
       )
    )
    (setq i (1+ i))
  )
  ;;Get the binary value of the bios date.
  (setq
    binarystring
     (strcat
       binarystring
       (haws-endstr
         (strcat
           "0000"
           (haws-convertdecimalto (nth 5 orderlist) 2)
         )
         4
         4
       )
       (haws-endstr
         (strcat
           "00000"
           (haws-convertdecimalto (nth 6 orderlist) 2)
         )
         5
         5
       )
       (haws-endstr
         (strcat
           "0000000"
           (haws-convertdecimalto (nth 7 orderlist) 2)
         )
         7
         7
       )
     )
  )
)

;;; BINARYTOLIST converts a 72 character binary string as follows:
;;; Bits 2-4,6-8,10-12,14-26 22-bit binary number of the AutoCAD date
;;; Bits 1,5,9,13,27-30 8-bit binary sales originator ID#
;;; Bits 27-30 8-bit binary package ID#
;;; Bits 31-40 10-bit binary number of authorizations
;;; Bits 41-56 16-bit binary ASCII for the first and last characters of the licensed computer name
;;; Bits 57-60 4-bit binary number of the BIOS date month
;;; Bits 61-65 5-bit binary number of the BIOS date day
;;; Bits 66-72 7-bit binary number of the BIOS date year
;;; sdddsdddsdddsdddddddddddddppppaaaaaaaaaaccccccccccccccccmmmmdddddyyyyyyy
;;; to an order list
;;; Orderlist    '(pkgid numauths compname biosmonth biosday biosyear
;;; startdate)
(defun haws-binarytolist (binarystring / dec i)
  (list
    ;;Get the AutoCAD date
    (haws-converttodecimal
      (strcat
        (substr binarystring 2 3)
        (substr binarystring 6 3)
        (substr binarystring 10 3)
        (substr binarystring 14 13)
      )
      2
    )
    ;;Get the sales id #.
    (haws-converttodecimal
      (strcat
        (substr binarystring 1 1)
        (substr binarystring 5 1)
        (substr binarystring 9 1)
        (substr binarystring 13 1)
      )
      2
    )
    ;;Get the package id #.
    (haws-converttodecimal (substr binarystring 27 4) 2)
    ;;Get the number of authorizations.
    (haws-converttodecimal (substr binarystring 31 10) 2)
    ;;Get the computer name fragment
    (strcat
      (chr (haws-converttodecimal (substr binarystring 41 8) 2))
      (chr (haws-converttodecimal (substr binarystring 49 8) 2))
    )
    ;;Get the bios month.
    (haws-converttodecimal (substr binarystring 57 4) 2)
    ;;Get the bios day.
    (haws-converttodecimal (substr binarystring 61 5) 2)
    ;;Get the bios year.
    (haws-converttodecimal (substr binarystring 66 7) 2)
  )
)

;;; ENCRYPTAUTHSTRING encrypts a padded binarystring
;;; for translation to a user authorization string
(defun haws-encryptauthstring (s / si sj x)
  ;;Make list 8x9 of characters
  (repeat 8
    (repeat 9
      (setq
        si (cons (substr s 1 1) si)
        s  (substr s 2)
      )
    )
    (setq
      sj (cons si sj)
      si nil
    )
  )
  (apply
    'STRCAT
    (mapcar
      '(lambda (x) (apply 'STRCAT x))
      (apply 'MAPCAR (cons 'LIST sj))
    )
  )
)

;;; DECRYPTAUTHSTRING decrypts a padded authorization binarystring
;;; for translation to an order list
(defun haws-decryptauthstring (s / si sj)
  ;;Make list 9x8 of characters
  (repeat 9
    (repeat 8
      (setq
        si (cons (substr s 1 1) si)
        s  (substr s 2)
      )
    )
    (setq
      sj (cons si sj)
      si nil
    )
  )
  (apply
    'STRCAT
    (mapcar
      '(lambda (x) (apply 'STRCAT x))
      (apply 'MAPCAR (cons 'LIST sj))
    )
  )
)

;;; ENCRYPTORDERSTRING encrypts a padded binarystring
;;; for translation to a user order string
(defun haws-encryptorderstring (s / si sj)
  ;;Make list 12x6 of characters
  (repeat 12
    (repeat 6
      (setq
        si (cons (substr s 1 1) si)
        s  (substr s 2)
      )
    )
    (setq
      sj (cons si sj)
      si nil
    )
  )
  (apply
    'STRCAT
    (mapcar
      '(lambda (x) (apply 'STRCAT x))
      (apply 'MAPCAR (cons 'LIST sj))
    )
  )
)

;;; DECRYPTORDERSTRING decrypts a padded order binarystring
;;; for translation to an order list
(defun haws-decryptorderstring (s / si sj)
  ;;Make list 6x12 of characters
  (repeat 6
    (repeat 12
      (setq
        si (cons (substr s 1 1) si)
        s  (substr s 2)
      )
    )
    (setq
      sj (cons si sj)
      si nil
    )
  )
  (apply
    'STRCAT
    (mapcar
      '(lambda (x) (apply 'STRCAT x))
      (apply 'MAPCAR (cons 'LIST sj))
    )
  )
)

;;BINARYTOUSER
;; translates a 68 character encrypted binary license string into a
;; 12 character user string for e-mailing from or to user.
;; The user string consists only of upper and lower case letters plus
;; @ and &. Eg.  aR3YjMmhi@57
(defun haws-binarytouser (binarystring / dec i userstring)
  (setq
    userstring ""
    i 0
  )
  (repeat 12
    (setq
      dec
       (haws-converttodecimal
         (substr binarystring (1+ (* i 6)) 6)
         2
       )                                ; _
      userstring
       (strcat
         userstring
         (chr
           (cond
             ((<= 0 dec 25) (+ dec 65))
             ((<= 26 dec 51) (+ dec 71))
             ((<= 52 dec 61) (- dec 4))
             ((= dec 62) 38)
             ((= dec 63) 64)
           )
         )
       )
      i (1+ i)
    )
  )
  userstring
)

;; USERTOBINARY translates the 12-character user string generated by
;; BINARYTOUSER
;;into a 68 character (still encrypted) binary string as follows:
(defun haws-usertobinary (userstring / binarystring dec i temp)
  (setq
    binarystring ""
    i 0
  )
  (repeat 12
    (setq
      dec          (ascii (substr userstring (setq i (1+ i)) 1))
      dec          (cond
                     ((<= 65 dec 90) (- dec 65))
                     ((<= 97 dec 122) (- dec 71))
                     ((<= 48 dec 57) (+ dec 4))
                     ((= dec 38) 62)
                     ((= dec 64) 63)
                   )
      binarystring (strcat
                     binarystring
                     (haws-endstr
                       (strcat "000000" (haws-convertdecimalto dec 2))
                       6
                       6
                     )
                   )
    )
  )
)


;;ConvertDecimalTo converts a decimal number to another integer base.
;;Returns a string
(defun haws-convertdecimalto (num newbase / atomi done returnstring)
  (setq
    returnstring ""
    atomi 0
  )
  (while (not done)
    (setq
      atomi
       (rem num newbase)
      returnstring
       (strcat
         (if (< atomi 10)
           (itoa atomi)
           (chr (+ atomi 87))
         )
         returnstring
       )
      num
       (/ num newbase)
      done
       (= num 0)
    ) ;_ end of setq
  ) ;_ end of while
  returnstring
)
;;ConvertToDecimal converts a string representing a number in another base
;;to a decimal integer
(defun haws-converttodecimal (string base / chari decimal m)
  (setq
    decimal 0
    m 1
  ) ;_ end of setq
  (while (> (strlen string) 0)
    (setq
      chari
       (haws-endstr string 1 1)
      decimal
       (+ decimal
          (* m
             (if (< (ascii chari) 58)
               (atoi chari)
               (- (ascii (strcase chari)) 55)
             )
          )
       )
      string
       (substr string 1 (1- (strlen string)))
      m (* m base)
    ) ;_ end of setq
  ) ;_ end of while
  decimal
) ;_ end of defun

;;; Misc functions for developer and sales use.

;; ORDERTOAUTH
;; translates a user order string and a number of trial days
;; into a user authorization string
;; TRIALDAYS = -1000000 for unlimited authorization
(defun haws-ordertoauth (orderstring trialdays / orderlist)
  (setq
    orderlist
     (haws-binarytolist
       (haws-decryptorderstring
         (haws-usertobinary orderstring)
       )
     )
  )
  (haws-binarytouser
    (haws-encryptauthstring
      (haws-listtobinary
        (cons (+ (car orderlist) trialdays) (cdr orderlist))
      )
    )
  )
)
;; ORDERTOTRIAL
;; translates a user order string into a timed trial
;; authorization
(defun haws-ordertotrial (orderstring trialdays / orderlist)
  (haws-ordertoauth orderstring trialdays)
)
;; ORDERTOUNLIMITED
;; translates a user order string into an unlimited
;; authorization
(defun haws-ordertounlimited (orderstring / orderlist)
  (haws-ordertoauth orderstring -1000000)
)

;;ORDERTOLIST translates a user order string into an order list.
(defun haws-ordertolist (orderstring)
  (haws-binarytolist
    (haws-decryptorderstring (haws-usertobinary orderstring))
  )
)
;; AUTHTOLIST translates a user authorization string into an order
;; list.
(defun haws-authtolist (authstring)
  (haws-binarytolist
    (haws-decryptauthstring (haws-usertobinary authstring))
  )
)
;;LISTTOORDER translates a an order list into a user order string.
(defun haws-listtoorder (orderlist)
  (haws-binarytouser
    (haws-encryptorderstring (haws-listtobinary orderlist))
  )
)
;; LISTTOAUTH translates a an order list into a user authorization
;; string.
(defun haws-listtoauth (authlist)
  (haws-binarytouser
    (haws-encryptauthstring (haws-listtobinary authlist))
  )
)
;#endregion
;#region USE LOG
;;; ======================================================================
;;;
;;;                 Usage logging functions
;;;
;;; ======================================================================
;;; This list would be better if populated automatically from the web on load.
;;; Stored locally somewhere. Maybe a LSP file.
;;; id appgroup name
(setq
  *haws-edccommands*
   '((0 -1 "haws-adl")
     (1 -1 "haws-aar")
     (2 -1 "haws-attredef")
     (3 -1 "haws-at")
     (4 -1 "haws-bb")
     (5 -1 "haws-xda")
     (6 -1 "haws-xra")
     (7 -1 "haws-brk")
     (8 -1 "haws-bm")
     (9 -1 "haws-clean")
     (10 -1 "haws-contelev")
     (11 -1 "haws-copyrot")
     (12 -1 "haws-copyrotdrag")
     (13 -1 "haws-md")
     (14 -1 "haws-dimsty")
     (15 -1 "haws-d1")
     (16 -1 "haws-d2")
     (17 -1 "haws-dp")
     (18 -1 "haws-du")
     (19 -1 "haws-dv")
     (20 -1 "haws-ht")
     (21 -1 "haws-te")
     (22 -1 "haws-xx")
     (23 -1 "haws-c2")
     (24 -1 "haws-ct")
     (25 -1 "haws-dd")
     (26 -1 "haws-p0")
     (27 -1 "haws-ee")
     (28 -1 "haws-bf")
     (29 -1 "haws-copy")
     (30 -1 "haws-cb")
     (31 -1 "haws-mp")
     (32 -1 "haws-pj")
     (33 -1 "haws-r1")
     (34 -1 "haws-r2")
     (35 -1 "haws-r4")
     (36 -1 "haws-r9")
     (37 -1 "haws-s")
     (38 -1 "haws-ub")
     (39 -1 "haws-um")
     (40 -1 "haws-vb")
     (41 -1 "haws-facnum")
     (42 -1 "haws-funky")
     (43 -1 "haws-imp_exp")
     (44 -1 "haws-incnum")
     (45 -1 "haws-xin")
     (46 -1 "haws-xout")
     (47 -1 "haws-eg")
     (48 -1 "haws-egn")
     (54 -1 "haws-gb")
     (55 -1 "haws-gc")
     (56 -1 "haws-invl")
     (57 -1 "haws-invr")
     (58 -1 "haws-lotel")
     (59 -1 "haws-pad")
     (60 -1 "haws-rev")
     (61 -1 "haws-secb")
     (62 -1 "haws-secl")
     (63 -1 "haws-secr")
     (64 -1 "haws-sect")
     (65 -1 "haws-sll")
     (66 -1 "haws-slope")
     (67 -1 "haws-slr")
     (68 -1 "haws-spotel")
     (69 -1 "haws-tc")
     (70 -1 "haws-tcelev")
     (71 -1 "haws-tcelevl")
     (72 -1 "haws-tcelevr")
     (73 -1 "haws-l0")
     (74 -1 "haws-lk0")
     (75 -1 "haws-lka")
     (76 -1 "haws-lki")
     (77 -1 "haws-ofi")
     (78 -1 "haws-ula")
     (79 -1 "haws-laprn")
     (80 -1 "haws-ldr")
     (81 -1 "haws-led")
     (82 -1 "haws-lengthen")
     (83 -1 "haws-lm")
     (84 -1 "haws-loadandrun")
     (85 -1 "haws-m40")
     (86 -1 "haws-m42")
     (87 -1 "haws-mc2033")
     (88 -1 "haws-ffa")
     (89 -1 "haws-hawsalias")
     (90 -1 "haws-pgpedit")
     (91 -1 "haws-user")
     (92 -1 "haws-oo")
     (93 -1 "haws-offsetx")
     (94 -1 "haws-qs14")
     (95 -1 "haws-qs2000")
     (96 -1 "haws-qs2004")
     (97 -1 "haws-pjl")
     (98 -1 "haws-polarset")
     (99 -1 "haws-polaroff")
     (100 -1 "haws-0")
     (101 -1 "haws-1")
     (102 -1 "haws-aa")
     (103 -1 "haws-adt")
     (104 -1 "haws-cet")
     (105 -1 "haws-cmd")
     (106 -1 "haws-dia")
     (107 -1 "haws-fdt")
     (108 -1 "haws-mbt")
     (109 -1 "haws-qt")
     (110 -1 "haws-il")
     (111 -1 "haws-io")
     (112 -1 "haws-ir")
     (113 -1 "haws-it")
     (114 -1 "haws-llt")
     (115 -1 "haws-mvl")
     (116 -1 "haws-mvu")
     (117 -1 "haws-ose")
     (118 -1 "haws-osi")
     (119 -1 "haws-osm")
     (120 -1 "haws-osn")
     (121 -1 "haws-pslt")
     (122 -1 "haws-proto")
     (123 -1 "haws-protox")
     (124 -1 "haws-rga")
     (125 -1 "haws-uf")
     (126 -1 "haws-uf0")
     (127 -1 "haws-uf1")
     (128 -1 "haws-vsr")
     (129 -1 "haws-10")
     (130 -1 "haws-12")
     (131 -1 "haws-setdim10")
     (132 -1 "haws-setdim12")
     (133 -1 "haws-setup")
     (134 0 "haws-sheet")
     (135 -1 "haws-sel")
     (136 -1 "haws-ser")
     (137 -1 "haws-ssx")
     (138 -1 "haws-swap")
     (139 -1 "haws-th")
     (140 -1 "haws-2x")
     (141 -1 "haws-5x")
     (142 -1 "haws-9x")
     (143 -1 "haws-twz")
     (144 -1 "haws-x2")
     (145 -1 "haws-zw")
     (146 -1 "haws-z0")
     (147 -1 "haws-za")
     (148 -1 "haws-ze")
     (149 -1 "haws-zi")
     (150 -1 "haws-zo")
     (151 -1 "haws-zv")
     (152 -1 "haws-zz")
     (153 0 "haws-2l")
     (154 1 "haws-add")
     (155 -1 "haws-aee")
     (156 -1 "haws-acres")
     (157 -1 "haws-sf")
     (158 -1 "haws-aet")
     (159 -1 "haws-sm")
     (160 -1 "haws-sy")
     (161 -1 "haws-a2t")
     (162 -1 "haws-att2txt")
     (163 0 "haws-bdl")
     (164 0 "haws-bdp")
     (165 0 "haws-berm")
     (166 0 "haws-bl0")
     (167 0 "haws-bw")
     (168 1 "haws-ca")
     (169 0 "haws-chattrib")
     (170 0 "haws-chcoord")
     (171 -1 "haws-chdim")
     (172 -1 "haws-chm")
     (173 -1 "haws-chnum")
     (174 -1 "haws-chgtext")
     (175 -1 "haws-cht")
     (176 -1 "haws-cl")
     (177 0 "haws-cmpro")
     (178 0 "haws-cmt")
     (179 1 "hcnm-cnm")
     (180 1 "hcnm-cnmkt")
     (181 1 "hcnm-cnmkti")
     ;; 336 is "hcnm-cnmqt" due to a programming bug that made 182 the hcnm-cnm sub-function
     (183 1 "hcnm-linkproj")
     (184 1 "testset")
     (185 1 "testget")
     (188 1 "hcnm-notesedit")
     (189 1 "hcnm-cnmlayer")
     (190 1 "hcnm-setnotesbubblestyle")
     (191 1 "haws-phaseedit")
     (192 1 "hcnm-attnoplot")
     (193 1 "hcnm-attplot")
     (194 1 "haws-setnotephases")
     (195 1 "haws-cnmmenu")
     (196 1 "haws-cnmsetup")
     (197 1 "haws-ntpurge")
     (198 1 "haws-boxl")
     (199 1 "haws-cirl")
     (200 1 "haws-dial")
     (201 1 "haws-elll")
     (202 1 "haws-hexl")
     (203 1 "haws-octl")
     (204 1 "haws-penl")
     (205 1 "haws-recl")
     (206 1 "haws-sstl")
     (207 1 "haws-tril")
     (208 1 "haws-tcg")
     (209 1 "haws-txtl")
     (210 1 "hcnm-cnmoptions")
     (211 0 "haws-contvol")
     (212 0 "haws-contxt")
     (213 1 "haws-cs")
     (214 -1 "haws-curve")
     (215 0 "haws-dw")
     (216 -1 "hcnm-about")
     (217 -1 "haws-about")
     (218 -1 "haws-orderlicenses")
     (221 -1 "haws-eop")
     (222 -1 "haws-geodata")
     (223 0 "haws-goto")
     (224 -1 "haws-incatt")
     (226 -1 "haws-ffi")
     (228 0 "haws-istan")
     (229 -1 "haws-ff")
     (230 -1 "haws-lk")
     (231 1 "haws-off")
     (232 -1 "haws-ffx")
     (233 1 "haws-offx")
     (234 -1 "haws-uff")
     (235 -1 "haws-uffx")
     (236 -1 "haws-uoff")
     (237 -1 "haws-uoffx")
     (238 0 "haws-las")
     (239 0 "haws-lar")
     (240 -1 "haws-lcp")
     (241 -1 "haws-lcpx")
     (242 -1 "haws-loop")
     (243 -1 "haws-tilde")
     (244 -1 "haws-dot")
     (245 0 "haws-none")
     (246 -1 "haws-letter")
     (247 -1 "haws-lotnum")
     (248 -1 "haws-ltc")
     (249 -1 "haws-ltb")
     (250 -1 "haws-lth")
     (251 -1 "haws-ltp")
     (252 -1 "haws-ltpx")
     (253 -1 "haws-lwp")
     (254 -1 "haws-lwpx")
     (255 -1 "haws-lx")
     (256 -1 "haws-lxx")
     (257 -1 "haws-mf")
     (258 0 "haws-mfillet")
     (259 -1 "haws-mof")
     (260 0 "haws-moffset")
     (261 -1 "haws-mren")
     (262 0 "haws-mrename")
     (263 -1 "haws-mscr")
     (264 0 "haws-mscript")
     (265 -1 "haws-mv")
     (266 0 "haws-ne")
     (267 0 "haws-na")
     (268 0 "haws-newscale")
     (269 -1 "haws-num")
     (270 0 "haws-pipe")
     (271 -1 "haws-plt")
     (272 -1 "haws-presuf")
     (273 0 "haws-propipe")
     (274 0 "haws-prosup")
     (275 -1 "haws-pc")
     (276 0 "haws-procb")
     (277 -1 "haws-pm")
     (278 0 "haws-promh")
     (279 -1 "haws-pred")
     (280 0 "haws-proe")
     (281 0 "haws-pldr")
     (282 -1 "haws-newpro")
     (283 0 "haws-profc")
     (284 -1 "haws-pro")
     (285 0 "haws-tgh2_pro")
     (286 -1 "haws-lst")
     (287 0 "haws-ellabel")
     (288 0 "haws-stalabel")
     (289 -1 "haws-elv")
     (290 -1 "haws-grd")
     (291 -1 "haws-grc")
     (292 -1 "haws-grb")
     (293 -1 "haws-pall")
     (294 -1 "haws-l80")
     (295 -1 "haws-l100")
     (296 -1 "haws-l120")
     (297 -1 "haws-l140")
     (298 -1 "haws-l175")
     (299 -1 "haws-l200")
     (300 -1 "haws-l240")
     (301 -1 "haws-l290")
     (302 -1 "haws-l350")
     (303 -1 "haws-l500")
     (304 0 "haws-rescale")
     (305 0 "haws-romans")
     (306 -1 "haws-rotatebase")
     (307 -1 "haws-round")
     (308 -1 "haws-ssxpro")
     (309 0 "haws-stacl")
     (310 0 "haws-dm")
     (311 0 "haws-dm12")
     (312 0 "haws-tap")
     (313 0 "haws-tapinv")
     (316 1 "haws-to")
     (317 1 "haws-tu")
     (318 0 "haws-tw")
     (319 -1 "haws-txtsum")
     (320 -1 "haws-u0")
     (321 -1 "haws-u1")
     (322 -1 "haws-u2")
     (323 -1 "haws-u3")
     (324 -1 "haws-u8")
     (325 -1 "haws-us")
     (326 0 "haws-ut")
     (327 0 "haws-wall")
     (328 0 "haws-ws")
     (329 -1 "haws-wl")
     (330 -1 "haws-xd")
     (331 -1 "haws-xro")
     (332 0 "haws-xroffset")
     (333 -1 "haws-xu")
     (334 0 "haws-xy")
     (335 2 "hcnm-notesedit-pro") ; Not really the command name. Disambiguating from regular use.
     (336 1 "hcnm-cnmqt")
     (337 1 "hcnm-edit-bubbles")
     (338 1 "hcnm-replace-bubble")
     (339 -1 "haws_label")
     (1000 1 "untracked")
   )
)

(defun haws-use-local-location ()
  (list "HawsEDC" "UseLog" "UseString")
)

(defun haws-use-get-local-log-string ()
  (cond
    ((haws-readcfg (haws-use-local-location)))
    ("")
  )
)

(defun haws-use-log-local (command-id / log-string)
  (haws-writecfg
    (haws-use-local-location)
    (haws-use-command-id-to-log-string command-id (haws-use-get-local-log-string))
  )
)

;;; The ASCII code of each character of the log string (one for each command) represents the number of times the command has been used this session. This implies that we count only up to 255 uses per session.
(defun haws-use-initialize-log-string ( / i max-id)
  (setq i -1 log-string "" max-id (caar (reverse *haws-edccommands*))) (while (< (setq i (1+ i)) max-id) (setq log-string (strcat log-string (chr 1))))
  log-string
)

(defun haws-use-command-id-to-log-string (command-id log-string / max-id)
  (cond
    ((or (not log-string) (= log-string ""))
     (haws-use-initialize-log-string)
    )
  )
  (setq log-string (strcat (substr log-string 1 command-id) (chr (1+ (ascii (substr log-string (1+ command-id) 1)))) (substr log-string (+ command-id 2))))
)

(defun haws-use-log-remote (/ url http bios-date log-data)
  (setq
    url  "http://www.constructionnotesmanager.com/cnm_log.php"
    http (vlax-create-object "MSXML2.XMLHTTP")
    bios-date (haws-getbiosdate)
    log-data
     (strcat
       "computer_name="
       (haws-getcomputername)
       "&loginname="
       (getvar "loginname")
       "&cnm_version="
       (haws-unified-version)
       "&command_log="
       (haws-use-get-local-log-string)
     )
  )
  (vlax-invoke-method http 'OPEN "post" url :vlax-true)
  (vlax-invoke-method http 'setRequestHeader "Content-type" "application/x-www-form-urlencoded")
  (cond
    ((vl-catch-all-error-p
       (vl-catch-all-apply 'VLAX-INVOKE (list http 'SEND log-data))
     )
     (princ (strcat "\nInvalid request: " url))
    )
    (t (haws-writecfg (haws-use-local-location) (haws-use-initialize-log-string)))
  )
  (vlax-release-object http)
  (princ)
)

;#endregion
;#region GETTERS
(defun haws-getanglex (gx-startingpoint gx-prompt gx-defaultvalue
                   gx-initialvalue / gx-input
                  )
  (setq
    gx-defaultvalue
     (cond
       (gx-defaultvalue)
       (gx-initialvalue)
       (0.0)
     )
  )
  (cond
    ((and
       gx-startingpoint
       (setq
         gx-input
          (getangle
            gx-startingpoint
            (strcat
              "\n"
              gx-prompt
              " <"
              (angtos gx-defaultvalue)
              ">: "
            )
          )
       )
     )
     gx-input
    )
    ((and
       (not gx-startingpoint)
       (setq
         gx-input
          (getangle
            (strcat
              "\n"
              gx-prompt
              " <"
              (angtos gx-defaultvalue)
              ">: "
            )
          )
       )
     )
     gx-input
    )
    (gx-defaultvalue)
  )
)

;;;HAWS-GETDISTX
;;;Returns a distance
(vl-acad-defun 'HAWS-GETDISTX)
(defun haws-getdistx (gx-point1 gx-prompt gx-defaultvalue gx-initialvalue / gx-arcmode)
  (car
    (haws-getdistpoint
      gx-point1
      gx-prompt
      gx-defaultvalue
      gx-initialvalue
      (setq gx-arcmode-p nil)
    )
  )
)
;;;HAWS-GETDISTPOINT
;;;Returns a distance, the endpoint of the distance, and the bulge used for the distance.
;;;'(distance endpoint bulge)
(vl-acad-defun 'HAWS-GETDISTPOINT)
(defun haws-getdistpoint (gx-point1 gx-prompt gx-defaultvalue
                      gx-initialvalue gx-arcmode-p / gx-point2 gx-point3 gx-bulge
                      gx-distance
                     )
  (setq
    gx-defaultvalue
     (cond
       (gx-defaultvalue)
       (gx-initialvalue)
     )
  )
  ;;If starting point wasn't provided, get it.
  (cond
    ((not gx-point1)
     (setq gx-point1 (getpoint "\nSpecify first point: "))
    )
  )
  ;;If there is now a starting point, get the second point or Arc keyword.
  (cond
    (gx-point1
     (initget "Arc")
     (setq
       gx-point2
        (getpoint
          gx-point1
          (strcat
            "\n"
            gx-prompt
            (cond (arcmode-p " [Arc]") (t ""))
            (if gx-defaultvalue
              (strcat " <" (rtos gx-defaultvalue) ">")
              ""
            )
            ": "
          )
        )
     )
    )
  )
  (setq
    gx-distance
     (cond
       ;;If
       (;;point2 was not entered
        (not gx-point2)
        ;;then return the default value.
        gx-defaultvalue
       )
       ;;Else if point2 isn't "Arc"
       ((or (not arcmode-p) (/= "Arc" gx-point2))
        ;;then return the distance between point1 and point2
        (distance gx-point1 gx-point2)
       )
       ;;Else enter arc mode.
       (t
        ;;Prompt for the second and third arc points
        (setq
          gx-point2
           (getpoint
             (strcat "\nSpecify point on arc: ")
           )
          gx-point3
           (getpoint
             (strcat "\nSpecify end point of arc: ")
           )
        )
        (cond
          ;;If the second and third arc point were provided, then
          ((and gx-point2 gx-point3)
           ;;Return the length of the arc.
           (haws-segment-length
             gx-point1
             gx-point3
             (setq
               gx-bulge
                (haws-3pttobulge
                  (haws-flatten gx-point1)
                  (haws-flatten gx-point2)
                  (haws-flatten gx-point3)
                )
             )
           )
          )
        )
       )
     )
  )
  (list
    gx-distance
    (cond
      (gx-point3)
      (gx-point2)
    )
    gx-bulge
  )
)

(vl-acad-defun 'HAWS-GETDN)
(defun haws-getdn (/ dn)
  (setq dn (getvar "dwgname"))
  (if (wcmatch (strcase dn) "*`.DWG")
    (setq dn (substr dn 1 (- (strlen dn) 4)))
    (if (wcmatch dn "*\\*")
      (setq dn (substr dn (1+ (strlen (getvar "dwgprefix")))))
    )
  )
  dn
)

(vl-acad-defun 'HAWS-GETDNPATH)
(defun haws-getdnpath (/ dnpath)
  (setq dnpath (getvar "dwgname"))
  (if (wcmatch (strcase dnpath) "*`.DWG")
    (setq
      dnpath
       (strcat (getvar "dwgprefix") dnpath)
      dnpath
       (substr dnpath 1 (- (strlen dnpath) 4))
    )
  )
  dnpath
)

(defun haws-getfil (fprmpt fdflt ftype fext / fname fninp)
  (while (not f1)
    (setq
      fninp
       (haws-getstringx fprmpt fninp fdflt)
      fname
       (strcat fninp "." fext)
    )
    (cond
      ((and (= (strcase ftype) "W") (findfile fname))
       (initget "Yes No")
       (if (= (getkword "File already exists.  Overwrite? [Y/N]:")
              "Yes"
           )
         (setq f1 (open fname ftype))
       )
      )
      (t (setq f1 (open fname ftype)))
    )
    (if (not f1)
      (prompt
        (strcat "Invalid path or filename.  Please try again.\n")
      )
    )
  )
  (list f1 fname)
)

;;HAWS-GETINTX
;;Provided for legacy compatability and user experience.
(vl-acad-defun 'HAWS-GETINTX)
(defun haws-getintx (gx-prompt gx-defaultvalue gx-initialvalue)
  (haws-getintxx gx-prompt gx-defaultvalue gx-initialvalue 0)
)
;;;HAWS-GETINTXX
;;;Extended (getint) with default value and drawing text selection
;;;Three modes:
;;;1. If a default or initial value is supplied, GETINTX prompts with it and allows user to enter Select from drawing text mode.
;;;2. If no default is supplied and MODE is 0, the first prompt is for standard input, with fallback to selecting value from drawing text.
;;;3. If no default is supplied and MODE is 1, the first prompt is for drawing text selection, with fallback to standard input.
;;;Returns an INT or nil if nothing provided.
(vl-acad-defun 'HAWS-GETINTXX)
(defun haws-getintxx (gx-prompt gx-defaultvalue gx-initialvalue
                  gx-promptmode / gx-response
                 )
  ;;Log all calls to this function.
  (haws-log
    (strcat
      "HAWS-GETINTX GX-PROMPT="
      gx-prompt
      " GX-DEFAULT="
      (if gx-defaultvalue
        (itoa gx-defaultvalue)
        "nil"
      )
      " GX-INITIALVALUE="
      (if gx-initialvalue
        (itoa gx-initialvalue)
        "nil"
      )
    )
  )
  (setq
    gx-defaultvalue
     (cond
       (gx-defaultvalue)
       (gx-initialvalue)
     )
  )
  ;;First prompt
  (cond
    ;;If a default value was supplied, prompt with it and allow user to enter Select from drawing text mode.
    (gx-defaultvalue
     (initget "Select")
     (setq
       gx-response
        (getint
          (strcat
            "\n"
            gx-prompt
            " or [Select from drawing] <"
            (itoa gx-defaultvalue)
            ">: "
          )
        )
     )
    )
    ;;Else if mode is 0, prompt for standard input
    ((= gx-promptmode 0)
     (setq
       gx-response
        (getint
          (strcat
            "\n"
            gx-prompt
            " or <Select from drawing>: "
          )
        )
     )
    )
    ;;Else if mode is 1, prompt for object select
    ((= gx-promptmode 1)
     (setq
       gx-response
        (nentsel
          (strcat
            "\nSelect object with "
            gx-prompt
            " or <enter manually>: "
          )
        )
     )
    )
  )
  ;;Second prompt if necessary
  (cond
    ;;If
    ((and
       ;;no response
       (not gx-response)
       ;;and there's a default value,
       gx-defaultvalue
     )
     ;;No second prompt
     nil
    )
    ;;Else if
    ((or (and
           ;;no response
           (not gx-response)
           ;;and mode is 0,
           (= gx-promptmode 0)
         )
         ;;or response was Select
         (= gx-response "Select")
     )
     ;;Prompt for object select
     (setq
       gx-response
        (nentsel
          (strcat "\nSelect object with " gx-prompt ": ")
        )
     )
    )
    ;;Else if
    ((and
       ;; no response
       (not gx-response)
       ;;and mode is 1,
       (= gx-promptmode 1)
     )
     ;;Prompt for standard input
     (setq gx-response (getint (strcat "\n" gx-prompt ": ")))
    )
  )
  ;;Return the integer if provided
  (cond
    ;;If
    ((and
       ;;there's a response
       gx-response
       ;;and it's an integer,
       (= (type gx-response) 'INT)
     )
     ;;Then return it
     gx-response
    )
    ;;Else if
    ((and
       ;;there's a response
       gx-response
       ;;and it's an entsel,
       (= (type gx-response) 'LIST)
     )
     ;;Then return it
     ;;Then convert it to an integer
     (atoi
       (cadr
         (haws-extract (cdr (assoc 1 (entget (car gx-response)))))
       )
     )
    )
    ;;Else
    (t
     ;;Return the default
     gx-defaultvalue
    )
  )
)

;;HAWS-GETREALX
;;Provided for legacy compatability and user experience.
(vl-acad-defun 'HAWS-GETREALX)
(defun haws-getrealx (gx-prompt gx-defaultvalue gx-initialvalue)
  (haws-getrealxx gx-prompt gx-defaultvalue gx-initialvalue 0)
)
;;;HAWS-GETREALXX
;;;Extended (getreal) with default value and drawing text selection
;;;Three modes:
;;;1. If a default or initial value is supplied, GETREALX prompts with it and allows user to enter Select from drawing text mode.
;;;2. If no default is supplied and MODE is 0, the first prompt is for standard input, with fallback to selecting value from drawing text.
;;;3. If no default is supplied and MODE is 1, the first prompt is for drawing text selection, with fallback to standard input.
;;;Returns an REAL or nil if nothing provided.
(vl-acad-defun 'HAWS-GETREALXX)
(defun haws-getrealxx (gx-prompt gx-defaultvalue gx-initialvalue
                   gx-promptmode / gx-response
                  )
  ;;Log all calls to this function.
  (haws-log
    (strcat
      "HAWS-GETREALX GX-PROMPT="
      gx-prompt
      " GX-DEFAULT="
      (if gx-defaultvalue
        (rtos gx-defaultvalue 2)
        "nil"
      )
      " GX-INITIALVALUE="
      (if gx-initialvalue
        (rtos gx-initialvalue 2)
        "nil"
      )
    )
  )
  (setq
    gx-defaultvalue
     (cond
       (gx-defaultvalue)
       (gx-initialvalue)
     )
  )
  ;;First prompt
  (cond
    ;;If a default value was supplied, prompt with it and allow user to enter Select from drawing text mode.
    (gx-defaultvalue
     (initget "Select")
     (setq
       gx-response
        (getreal
          (strcat
            "\n"
            gx-prompt
            " or [Select from drawing] <"
            (rtos gx-defaultvalue 2)
            ">: "
          )
        )
     )
    )
    ;;Else if mode is 0, prompt for standard input
    ((= gx-promptmode 0)
     (setq
       gx-response
        (getreal
          (strcat
            "\n"
            gx-prompt
            " or <Select from drawing>: "
          )
        )
     )
    )
    ;;Else if mode is 1, prompt for object select
    ((= gx-promptmode 1)
     (setq
       gx-response
        (nentsel
          (strcat
            "\nSelect object with "
            gx-prompt
            " or <enter manually>: "
          )
        )
     )
    )
  )
  ;;Second prompt if necessary
  (cond
    ;;If
    ((and
       ;;no response
       (not gx-response)
       ;;and there's a default value,
       gx-defaultvalue
     )
     ;;No second prompt
     nil
    )
    ;;Else if
    ((or (and
           ;;no response
           (not gx-response)
           ;;and mode is 0,
           (= gx-promptmode 0)
         )
         ;;or response was Select
         (= gx-response "Select")
     )
     ;;Prompt for object select
     (setq
       gx-response
        (nentsel
          (strcat "\nSelect object with " gx-prompt ": ")
        )
     )
    )
    ;;Else if
    ((and
       ;; no response
       (not gx-response)
       ;;and mode is 1,
       (= gx-promptmode 1)
     )
     ;;Prompt for standard input
     (setq gx-response (getreal (strcat "\n" gx-prompt ": ")))
    )
  )
  ;;Return the real number if provided
  (cond
    ;;If
    ((and
       ;;there's a response
       gx-response
       ;;and it's an integer,
       (= (type gx-response) 'REAL)
     )
     ;;Then return it
     gx-response
    )
    ;;Else if
    ((and
       ;;there's a response
       gx-response
       ;;and it's an entsel,
       (= (type gx-response) 'LIST)
     )
     ;;Then return it
     ;;Then convert it to an real
     (atof
       (cadr
         (haws-extract (cdr (assoc 1 (entget (car gx-response)))))
       )
     )
    )
    ;;Else
    (t
     ;;Return the default
     gx-defaultvalue
    )
  )
)

;;;HAWSGETPOINTX
(vl-acad-defun 'HAWS-GETPOINTX)
(defun haws-getpointx (gx-startingpoint gx-prompt gx-defaultvalue
                   gx-initialvalue / gx-input
                  )
  (setq
    gx-defaultvalue
     (cond
       (gx-defaultvalue)
       (gx-initialvalue)
       ('(0.0 0.0 0.0))
     )
  )
  (setq
    gx-prompt
     (strcat
       gx-prompt
       " <"
       (rtos (car gx-defaultvalue))
       ","
       (rtos (cadr gx-defaultvalue))
       ","
       (rtos (caddr gx-defaultvalue))
       ">: "
     )
  )
  (setq
    gx-input
     (if gx-startingpoint
       (getpoint gx-startingpoint gx-prompt)
       (getpoint gx-prompt)
     )
  )
  (if (not gx-input)
    gx-defaultvalue
    gx-input
  )
)

(vl-acad-defun 'HAWS-GETSTRINGX)
(defun haws-getstringx
   (gx-prompt gx-defaultvalue gx-initialvalue / gx-input)
  (setq
    gx-defaultvalue
     (cond
       (gx-defaultvalue)
       (gx-initialvalue)
       ("")
     )
  )
  (cond
    ((/= ""
         (setq
           gx-input
            (getstring
              (strcat "\n" gx-prompt " <" gx-defaultvalue ">: ")
            )
         )
     )
     gx-input
    )
    (gx-defaultvalue)
  )
)

;#endregion
;#region STRING EXTRACTION
;; Atofx extracts a real number from a text string when text before
;; or
;; after
;;the number matches a give wild card spec.  Requires EXTRACTX.
;;Type 0 tries to match the wild cards with text preceding a number.
;;Type 1 tries to match the wild cards with text following a number
;;Returns 0.0 if search unsuccesful
(vl-acad-defun 'HAWS-ATOFX)
(defun haws-atofx (s wc opt / x)
  (setq x (cadr (haws-extractx s wc opt)))
  (if x
    (atof x)
    0.0
  )
)

;;; Distofx extracts a real number from a text string when text before
;;; or
;;; after
;;the number matches a give wild card spec.  Requires EXTRACTX.
;;Type 0 tries to match the wild cards with text preceding a number.
;;Type 1 tries to match the wild cards with text following a number
;;Returns nil if search unsuccesful
(vl-acad-defun 'HAWS-DISTOFX)
(defun haws-distofx (s wc opt / x)
  (setq x (cadr (haws-extractx s wc opt)))
  (if x
    (distof x)
    nil
  )
)

(defun haws-dxf (gcode entlst) (cdr (assoc gcode entlst)))

;; Endstr returns a substring of s starting with the ith to last
;; character
;;and continuing l characters.
(vl-acad-defun 'HAWS-ENDSTR)
(defun haws-endstr (s i l)
  (substr s (1+ (- (max (strlen s) 1) i)) l)
)

;;Extract used to extract numerical info from a text string.
;;Ignores commas in numbers.  Not international compatible.
(vl-acad-defun 'HAWS-EXTRACT)
(defun haws-extract (s / c i prefix number suffix)
  (setq
    i 0
    prefix ""
    number ""
    suffix ""
  )
  (repeat (strlen s)
    (setq c (substr s (setq i (1+ i)) 1))
    (cond
      ((and (wcmatch c "#") (eq suffix ""))
       (setq number (strcat number c))
      )
      ((and
         (eq c "-")
         (= suffix number "")
         (wcmatch (substr s (1+ i) 1) "#")
       )
       (setq number (strcat number c))
      )
      ((and
         (eq c ".")
         (= suffix "")
         (wcmatch (substr s (1+ i) 1) "#")
       )
       (setq number (strcat number c))
      )
      ;;Swallow commas inside numbers.  Not int'l compatible.
      ((and
         (eq c ",")
         (= suffix "")
         (wcmatch (substr s (1+ i) 1) "#")
       )
      )
      ((eq number "") (setq prefix (strcat prefix c)))
      (t (setq suffix (strcat suffix c)))
    )
  )
  (list prefix number suffix)
)

;; Extractx used to extract numerical info from a text string with
;; extended
;; options.
(vl-acad-defun 'HAWS-EXTRACTX)
(defun haws-extractx (s wc opt / c done i pre prei number suf sufi)
  (setq
    i (if (= opt 0)
        0
        (1+ (strlen s))
      )
    pre ""
    number ""
    suf ""
  )
  (repeat (strlen s)
    (setq
      c    (substr
             s
             (setq
               i (if (= opt 0)
                   (1+ i)
                   (1- i)
                 )
             )
             1
           )
      prei (substr s 1 (1- i))
      sufi (substr s (1+ i))
    )
    (cond
      ((not
         (wcmatch
           (if (= opt 0)
             prei
             sufi
           )
           wc
         )
       )
       (if (= opt 0)
         (setq pre (strcat pre c))
         (setq suf (strcat c suf))
       )
      )
      ((and (wcmatch c "#") (not done))
       (setq
         number
          (if (= opt 0)
            (strcat number c)
            (strcat c number)
          )
       )
      )
      ((and
         (eq c "-")
         (= number "")
         (not done)
         (wcmatch (substr s (1+ i) 1) "#")
       )
       (setq
         number
          (if (= opt 0)
            (strcat number c)
            (strcat c number)
          )
       )
      )
      ((and
         (eq c ".")
         (not done)
         (wcmatch (substr s (1+ i) 1) "#")
       )
       (setq
         number
          (if (= opt 0)
            (strcat number c)
            (strcat c number)
          )
       )
      )
      ((eq number "")
       (if (= opt 0)
         (setq pre (strcat pre c))
         (setq suf (strcat c suf))
       )
      )
      (t
       (setq done t)
       (if (= opt 0)
         (setq suf (strcat suf c))
         (setq pre (strcat c pre))
       )
      )
    )
  )
  (if (not (zerop (strlen number)))
    (list pre number suf)
  )
)

;#endregion
;#region ICAD SUBSTITUTES
;;;
;;; c:haws-icad-p
;;;
;;;Tests whether intellicad behavior is current.
;;;Bricscad has advanced to the point we no longer have to use a special mode for it.
(vl-acad-defun 'C:haws-icad-p)
(defun c:haws-icad-p ()
  (or *haws-icadmode*
      (setq *haws-icadmode* (wcmatch (getvar "acadver") "*i"))
  )
)

;;
;; HAWS-FILE-COPY
;;
;; Intellicad substitute for VL-FILE-COPY
;;
;; CAUTION: May not return the same value as vl-file-copy.
;;
(vl-acad-defun 'HAWS-FILE-COPY)
(defun haws-file-copy (source destination / rdlin return)
  (setq return t)
  (cond
    ((and (= (substr (getvar "DWGNAME") 1 7) "Drawing") (wcmatch destination (strcat (getvar "DWGPREFIX") "*")) (wcmatch (getvar "ACADVER") "*BricsCAD"))
      (alert "BricsCAD may crash if this drawing is not in a writable folder.")
      (initget "Yes No")
      (if (/= (getkword "\nBricsCAD may crash. Continue anyway? [Yes/No] <No>: ") "Yes")(exit))
    )
  )
  (cond
    ((haws-vlisp-p) 
     (vl-file-copy source destination))
    (t
     (if (not(setq f1 (open source "r")))
       (setq return nil)
     )
     (if (not (and return (setq f2 (open destination "w"))))
       (setq return nil)
     )
     (while (setq rdlin (read-line f1)) (write-line rdlin f2))
     (setq f1 (close f1))
     (setq f2 (close f2))
     return
    )
  )
)
;;
;;HAWS-FILENAME-BASE
;;
;; Intellicad substitute for vl-filename-base.
;;
(vl-acad-defun 'HAWS-FILENAME-BASE)
(defun haws-filename-base (filename / base)
  (cond
    ((haws-vlisp-p) (vl-filename-base filename))
    (t
     ;;Trim off the directory.
     (setq base (strcat " " filename))  ;Pad with a space
     (while (wcmatch
              (setq
                base
                 (substr base 2)        ;Trim first character.
              )
              "*[\\/]*"                 ; and do again if slashes in name.
            )
     )
     ;;Trim the extension off one character at a time
     (if (wcmatch base "*`.*")          ; If there are any dots in remaining name.
       (progn
         (while (/= (haws-endstr base 1 1) ".")
                                        ; While the last character isn't a dot.
           (setq
             base
              (substr base 1 (1- (strlen base))) ;Trim it.
           )
         )
         (substr base 1 (1- (strlen base)))
       )
       base
     )
    )
  )
)


;;
;;HAWS-FILENAME-DIRECTORY
;;
;; Intellicad substitute for vl-filename-directory. 
;;
(vl-acad-defun 'HAWS-FILENAME-DIRECTORY)
(defun haws-filename-directory (filename / directory)
  (cond
    ((haws-vlisp-p) (vl-filename-directory filename))
    (t
     (cond
       ((wcmatch filename "*[\\/]*")    ;If file has any directories.
        (setq directory (strcat filename " "))
        (while (not
                 (wcmatch
                   (setq
                     directory
                      (substr directory 1 (1- (strlen directory)))
                                        ;Trim last character
                   )
                   "*[\\/]"             ; and do again if the last character is not a slash.
                 )
               )
        )
        (substr directory 1 (1- (strlen directory))) ;Trim slash
       )
       (t "")
     )
    )
  )
)

;;;
;;;HAWS-FILENAME-EXTENSION
;;;
;;; Intellicad substitute for vl-filename-extension.
;;; Trims everything up to but excluding last dot from file name.
;;;
;;; Returns extension including dot.
(vl-acad-defun 'HAWS-FILENAME-EXTENSION)
(defun haws-filename-extension (filename / extension)
  (cond
    ((and
       (haws-vlisp-p)
       (setq extension (vl-filename-extension filename))
     )
     extension
    )
    ;;Trim off the directory.
    (t
     (setq extension (strcat " " filename)) ;Pad with a space
     (while (wcmatch
              (setq
                extension
                 (substr extension 2)   ;Trim first character.
              )
              "*[\\/]*"                 ; and do again if slashes in name.
            )
     )
     ;;Trim off the base name.
     (if (wcmatch extension "*`.*")     ; If there are any dots in remaining name.
       (progn
         (setq extension (strcat " " extension))
         (while (wcmatch
                  (setq
                    extension
                     (substr extension 2) ;Trim first character
                  )
                  "*`.*"                ; and do again if there is still a dot.
                )
         )
         extension
       )
       ""
     )
    )
  )
)
;#endregion;;
;#region DEVELOP
;; HAWS_UNIT_TEST EXAMPLE
;|
(haws_unit_test 
  '+ 
  '(
    ((1 0 0) 1)
    ((2 0 0) 2)
    ((3 0 0) 3)
    ((4 0 0) 1)
    ((5 0 0) 5)
    ((6 0 0) 6)
  )
)
(haws_unit_test
  'HAWS_NESTED_LIST_UPDATE
  '(
    (((1 . ((11 . "A")(12 . ((121 . "B")(122 . ((1221 . "C")))))))) (1 12 1221 "1")
    ((1 . ((11 . "A")(12 . ((121 . "B")(122 . ((1221 . "1"))))))))
    )
  )
)
|;
(defun haws_unit_test (f assertions / answer args continue_p i result) 
  (setq i          0
        continue_p t
  )
  (mapcar 
    '(lambda (assertion) 
       (and 
         continue_p
         (setq args   (car assertion)
               answer (cadr assertion)
         )
         (setq continue_p (equal 
                            (setq result (apply f args))
                            answer
                          )
         )
         (setq i (1+ i))
         (princ (strcat "\nSuccess on test " (itoa i)))
       )
       (and
         (not continue_p)
         (print result)
       )
     )
    assertions
  )
  i
)
;;; HAWS-LOG
;;; Writes a message to a log file including the username and timestamp
(vl-acad-defun 'HAWS-LOG)
(defun haws-log (message)
            ;|
  (setq
    f1 (open
         (strcat
           (haws-filename-directory (findfile "hawsedc.mnu"))
           "\\haws-log.txt"
         )
         "a"
       )
  )
  (write-line
    (strcat
      (haws-getcomputername)
      " - "
      (rtos (getvar "cdate") 2 6)
      " - "
      message
    )
    f1
  )
  (setq f1 (close f1))
  |;
  (princ)
)
;;; HAWS-MILEPOST
;;; Echos a progress message depending on debug level.
(vl-acad-defun 'HAWS-MILEPOST)
(defun haws-milepost (messagestring)
  (if (> *haws-debuglevel* 0)
    (princ (strcat "\nHawsEDC debug message: " messagestring))
    messagestring
  )
)

;#endregion
;#region MISC
;; ======================================================================
;;
;;                 Miscellaneous Utility functions
;;
;; ======================================================================

;; Function to read a value at a nested path (returns nil if not found)
;; Usage: (HAWS_NESTED_LIST_GET DATA '(1 13 132 1323))
;; Credit to Grok AI 2025-10-17
(defun haws_nested_list_get (alist keys / key rest pair val)
  (cond
    ((null keys) nil)  ; Invalid empty path
    (t
      (setq key (car keys)
            rest (cdr keys)
            pair (assoc key alist))
      (cond
        ((null pair) nil)
        (t
          (setq val (cadr pair))  ; Changed from (CDR PAIR) to (CADR PAIR) for uniform LIST structure
          (cond
            ((null rest)
             val  ; Return value directly - no need to check if it's a branch
            )
            (t
              (cond
                ((listp val) (haws_nested_list_get val rest))  ; Recurse into sub-alist - removed (CAR VAL)
                (t nil)  ; Cannot go deeper
              )
            )
          )
        )
      )
    )
  )
)

;; Function to update or create a value at a nested path
;; Uses subst to preserve original list order, returns modified list
;; Usage: (HAWS_NESTED_LIST_UPDATE DATA '(1 13 132 1323) "2")
;; Credit to Grok AI 2025-10-17
(defun haws_nested_list_update (alist keys val / key rest pair sub new_sub_alist new_pair)
  (cond
    ((null keys) alist)  ; Nothing to do for empty path
    (t
      (setq key (car keys)
            rest (cdr keys))
      (setq pair (assoc key alist))
      (cond
        ((null rest)
          (setq new_pair (list key val))  ; Changed from (CONS KEY VAL) to (LIST KEY VAL) for uniform structure
          (cond
            (pair (subst new_pair pair alist))
            (t (append alist (list new_pair)))
          )
        )
        (t
          (setq sub (cadr pair))  ; Changed from (CDR PAIR) then (CAR SUB) to just (CADR PAIR)
          (setq new_sub_alist (haws_nested_list_update sub rest val))
          (setq new_pair (list key new_sub_alist))  ; Changed from (CONS KEY (LIST ...)) to (LIST KEY ...)
          (cond
            (pair (subst new_pair pair alist))
            (t (append alist (list new_pair)))
          )
        )
      )
    )
  )
)

;; Function to delete a key at a nested path
;; Uses subst to preserve original list order, prunes empty branches, returns modified list
;; Usage: (HAWS_NESTED_LIST_DELETE DATA '(1 13 132 1323))
;; Credit to Grok AI 2025-10-17
(defun haws_nested_list_delete (alist keys / key rest pair sub new_sub_alist new_pair)
  (cond
    ((null keys) alist)  ; Nothing to do
    (t
      (setq key (car keys)
            rest (cdr keys))
      (setq pair (assoc key alist))
      (cond
        ((null pair) alist)  ; Nothing to delete
        ((null rest) (vl-remove pair alist))  ; Remove leaf or branch
        (t
          (setq sub (cadr pair))  ; Changed from (CDR PAIR) then (CAR SUB) to just (CADR PAIR)
          (cond
            ((not (listp sub)) alist)  ; Cannot delete deeper into leaf
            (t
              (setq new_sub_alist (haws_nested_list_delete sub rest))
              (cond
                ((null new_sub_alist) (vl-remove pair alist))  ; Prune empty branch
                (t
                  (setq new_pair (list key new_sub_alist))  ; Changed from (CONS KEY (LIST ...))
                  (subst new_pair pair alist)
                )
              )
            )
          )
        )
      )
    )
  )
)

;; Function to test nested list operations
;; Usage: (HAWS_NESTED_LIST_TEST)
;; Credit to Grok AI 2025-10-17, updated 2025-10-20 for uniform LIST structure
(defun haws_nested_list_test (/ data)
  (setq data nil)
  ;; Test long path creation
  (setq data (haws_nested_list_update data '(1 12 121) "121A"))
  (princ "\nExpecting ((1 ((12 ((121 \"121A\")))))) we get ")
  (print data)
  (princ "\nExpecting \"121A\" we get ")
  (print (haws_nested_list_get data '(1 12 121)))
  ;; Test NIL value creation
  (setq data (haws_nested_list_update data '(1 11 111) nil))
  (princ "\nExpecting nil we get ")
  (print (haws_nested_list_get data '(1 11 111)))
  (princ "\nExpecting ((1 ((12 ((121 \"121A\"))) (11 ((111 nil)))))) we get ")
  (print data)
  ;; Test deletion
  (setq data (haws_nested_list_update data '(1 13 131) "131A"))
  (princ "\nAfter adding (1 13 131) \"131A\", expecting ((1 ((12 ((121 \"121A\"))) (11 ((111 nil))) (13 ((131 \"131A\")))))) we get ")
  (print data)
  (setq data (haws_nested_list_delete data '(1 13 131)))
  (princ "\nAfter deleting (1 13 131), expecting ((1 ((12 ((121 \"121A\"))) (11 ((111 nil)))))) we get ")
  (print data)
  ;; Test list value storage (like ATTRIBUTE_LIST)
  (setq data (haws_nested_list_update data '("ATTRIBUTES") '(("TAG1" "value1") ("TAG2" "value2"))))
  (princ "\nAfter adding ATTRIBUTES list, we get ")
  (print data)
  (princ "\nExpecting ((\"TAG1\" \"value1\") (\"TAG2\" \"value2\")) we get ")
  (print (haws_nested_list_get data '("ATTRIBUTES")))
  (princ)
);; HAWS-FILE-OPEN
;;
;; If a write directive file is locked, allows user to provide an alternate filename to open.
;;
;;
(defun haws-file-open (filename mode / errobj fp input)
  (setq errobj (vl-catch-all-apply 'OPEN (list filename mode)))
  (cond
    ((vl-catch-all-error-p errobj)
     (alert
       (princ
         (strcat
           "Couldn't write to "
           filename
           "\nPlease close if possible and follow command prompts."
         )
       )
     )
     (initget "Continue Specify")
     (setq
       input
        (getkword
          "\n[Continue with file closed/Specify another filename] <Continue>: "
        )
     )
     (cond
       ((= input "Continue"))
       ((= input "Specify")
        (setq filename (getfiled "Specify filename" filename "" 1))
        (setq fp (haws-file-open filename mode))
       )
     )
    )
    (t (setq fp errobj))
  )
)
;;;  HAWS-FLATTEN
(vl-acad-defun 'HAWS-FLATTEN)
(defun haws-flatten (pnt)
;;;Returns flattened coordinates of a 3d point
  (list (car pnt) (cadr pnt) 0.0)
)
;;; HAWS-LSTTOSTR
;;;Assembles a list of fields into a delimited string.
;;;Usage: (haws-lsttostr
;;;         [InputList containing fields]
;;;         [FieldSeparator field delimiter]
;;;         not used yet [TextDelimiter text delimiter character]
;;;       )
;;;Avoid cleverness.
;;;Human readability trumps elegance and economy and cleverness here.
;;;This should be readable to a programmer familiar with any language.
;;;In this function, I'm trying to honor readability in a new (2008) way.
;;;And I am trying a new commenting style.
;;;Tests
;;;(HAWS-LSTTOSTR '("1" "2" "3") "," "\"")
;;;(HAWS-LSTTOSTR '("1" "2\""" "3") "," "\"")
(vl-acad-defun 'HAWS-LSTTOSTR)

(defun haws-lsttostr (inputlist fieldseparator textdelimiter / currentfield
                  outputstring
                 )
  ;;Initialize values of variables
  (setq outputstring "")
  ;;Step through list making each element into a string field
  (while (setq currentfield (car inputlist))
    (cond
      ((= (type currentfield) 'STR)
       ;;Alert that text delimiter isn't working yet.
       (if (wcmatch currentfield (strcat "*" textdelimiter "*"))
         (alert
           "Text delimiter processing in HAWS-LstToStr isn't implemented yet."
         )
       )
       (setq
         outputstring
          (strcat outputstring fieldseparator currentfield)
       )
      )
    )
    (setq inputlist (cdr inputlist))
  )
  ;;Remove gratuitous first delimiter
  (if (/= outputstring "")
    (setq
      outputstring
       (substr outputstring (1+ (strlen fieldseparator)))
    )
  )
  outputstring
)

;;; HAWS-LOAD-FROM-APP-DIR
;;; loads a vlx, fas, or lsp, in that preferred order (AutoLISP
;;; default),
;;; from the folder that contains cnm.mnl
;;;
(defun c:haws-load-from-app-dir (filename / file-path)
  (princ "\nLoading ")
  (cond
    ((vl-catch-all-error-p
       (vl-catch-all-apply
         'LOAD
         (list (princ (strcat (c:hcnm-config-getvar "AppFolder") "\\" filename)))
       )
     )
     (princ
       " ... not found in app folder. Searching in support files search path."
     )
     (load filename)
    )
  )
  (princ)
)

;;MKFLD sub-function makes a field string out of a string.
;;If format
;;Usage: (mkfld
;;         [string to place into a field]
;;         [uniform field width or field delimiter character]
;;       )
(vl-acad-defun 'HAWS-MKFLD)
(defun haws-mkfld (string format / char i mkfld_field mkfld_literal)
  (cond
    ((= (type format) 'STR)
     (setq
       i 0
       mkfld_field ""
     )
     (cond
       ((wcmatch string (strcat "*`" format "*,*\"*,*\n*"))
        (setq
          mkfld_literal t
          mkfld_field "\""
        )
       )
     )
     (while (<= (setq i (1+ i)) (strlen string))
       (setq
         mkfld_field
          (strcat
            mkfld_field
            (cond
              ((= (setq char (substr string i 1)) "\"")
               "\"\""
              )
              (t char)
            )
          )
       )
     )
     (if mkfld_literal
       (setq mkfld_field (strcat mkfld_field "\""))
     )
     (setq mkfld_field (strcat mkfld_field format))
    )
    (t
     (setq mkfld_field string)
     (while
       (< (strlen (setq mkfld_field (substr mkfld_field 1 format)))
          format
       )
        (setq mkfld_field (strcat mkfld_field " "))
     )
    )
  )
  mkfld_field
)
;; MKLAYR sub-function defines and makes current a layer for another routine.
;; Usage: (haws-mklayr (list "laname" "lacolr" "laltyp"))
;; Use empty quotes for default color and linetype (eg. (mklay (list "AZ" "" ""))
(defun haws-getusl (/ i rdlin temp)
  (setq temp (findfile "layers.dat"))
  (cond
    (temp
     (prompt "\nReading layer settings from ")
     (princ temp)
     (princ "\n)")
    )
    ((prompt "\nLayer settings file not found.") (exit))
  )
  (setq
    f3 (open temp "r")
    i  0
  )
  (while (setq rdlin (read-line f3))
    (princ "\rReading line ")
    (princ (setq i (1+ i)))
    (if (= 'LIST (type (setq temp (read rdlin))))
      (setq *haws:layers* (cons temp *haws:layers*))
    )
  )
  (setq f3 (close f3))
)
(vl-acad-defun 'HAWS-GETLAYR)
(defun haws-getlayr (key / temp)
  (if (or (not *haws:layers*)
          (cond
            ((= (c:hcnm-config-getvar "ImportLayerSettings") "Yes")
             (c:hcnm-config-setvar "ImportLayerSettings" "No")
             t
            )
          )
      )
    (haws-getusl)
  )
  (cond
    ((cdr (assoc key *haws:layers*)))
    (t
     (prompt
       (strcat
         "\nSettings for \""
         key
         "\" not found in LAYERS.DAT.  Using current layer."
       )
     )
     (list (getvar "clayer") "" "")
    )
  )
)
(vl-acad-defun 'HAWS-MKLAYR)
(defun haws-mklayr (laopt / laname lacolr laltyp ltfile ltfiles temp)
  ;;(princ "\nHAWS-MKLAYR in edclib")
  (if (= 'STR (type laopt))
    (setq
      laopt
       (cond
         ((haws-getlayr laopt))
         ('("" "" ""))
       )
    )
  )
  (setq
    laname
     (car laopt)
    lacolr
     (cadr laopt)
    laltyp
     (caddr laopt)
  )
  (haws-load-linetype laltyp)
  (while (and (/= laltyp "") (not (tblsearch "LTYPE" laltyp)))
    (alert
      (strcat
        "\nLinetype "
        laltyp
        " is still not loaded.\nPlease follow prompts to try a different linetype or file."
      )
    )
    (setq
      temp
       (haws-getstringx
         "\nEnter substitute linetype name or <try another file>"
         laltyp
         laltyp
       )
    )
    (cond
      ((/= temp laltyp)
       (setq laltyp temp)
       (haws-load-linetype laltyp)
      )
    )
    (cond
      ((not (tblsearch "LTYPE" laltyp))
       (setq
         ltfile
          (getfiled
            (strcat "File for " laltyp " Linetype")
            ""
            "LIN"
            6
          )
       )
      )
    )
    (vl-cmdf "._linetype" "_l" laltyp ltfile "")
  )
  (haws-milepost "Finished assuring linetype.")
  (if (not (tblsearch "LAYER" laname))
    (vl-cmdf "._layer" "_m" laname "")
    (vl-cmdf "._layer" "_t" laname "_on" laname "_u" laname "_s" laname "")
  )
  (if (/= lacolr "")
    (vl-cmdf "._layer" "_c" lacolr "" "")
  )
  (if (/= laltyp "")
    (vl-cmdf "._layer" "_lt" laltyp "" "")
  )
  (haws-milepost "Finished making layer.")
  laopt
)

(defun haws-load-linetype (ltype / i ltfiles)
  (setq
    ltfiles
     (list "acad" "hawsedc" "default")
    i -1
  )
  (while (and
           (/= laltyp "")
           (not (tblsearch "LTYPE" ltype))
           (setq ltfile (nth (setq i (1+ i)) ltfiles))
         )
    (princ
      (strcat
        "\nLinetype " ltype " is not loaded. Attempting to load from "
        ltfile ".lin..."
       )
    )
    (vl-cmdf "._linetype" "_l" ltype ltfile "")
  )
  (haws-milepost
    (strcat
      "Finished trying to load linetype "
      ltype
      " from acad.lin, default.lin (Bricscad), and hawsedc.lin."
    )
  )
)


;;; ======================================================================
;;;
;;;                 Text creation and scale functions
;;;
;;; ======================================================================


(vl-acad-defun 'HAWS-DWGSCALE)
(defun haws-dwgscale ()
  (cond
    ((or (= (getvar "DIMANNO") 1) (= (getvar "DIMSCALE") 0))
     (/ 1 (getvar "CANNOSCALEVALUE"))
    )
    ((getvar "DIMSCALE"))
  )
)

(defun haws-text-height-paper ()
  (getvar "DIMTXT")
)

(defun haws-text-height-model ()
  (* (getvar "DIMTXT") (haws-dwgscale))
)

(vl-acad-defun 'HAWS-MKTEXT)
(defun haws-mktext (j i h r s / ent jx jy)
  (setq
    i  (trans
         (if (= 2 (length i))
           (append i '(0.0))
           i
         )
         1
         0
       )
    j  (if (= j nil)
         "L"
         (strcase j)
       )
    jx (cond
         ((wcmatch j "L,BL*,ML*,TL*") 0)
         ((wcmatch j "C*,?C*") 1)
         ((wcmatch j "R*,?R*") 2)
         ((wcmatch j "A*") 3)
         ((wcmatch j "M*") 4)
         ((wcmatch j "F*") 5)
       )
    jy (cond
         ((wcmatch j "L,C*,R*,A*,F*") 0)
         ((wcmatch j "B*") 1)
         ((wcmatch j "M*") 2)
         ((wcmatch j "T*") 3)
       )
  )
  (entmake
    (list
      (cons 0 "TEXT")
      (cons
        1
        (cond
          (s)
          ("This text created by HAWS-MKTEXT")
        )
      )
      (cons 7 (getvar "textstyle"))
      (append '(10) i)
      ;; Simple entmake doesn't create annotative text.
      (cons
        40
        (cond
          (h)
          (t
            (haws-text-height-model)
          )
        )
      )
      (assoc 41 (tblsearch "STYLE" (getvar "textstyle")))
      (cons 50 (+ r (angle '(0.0 0.0 0.0) (getvar "ucsxdir"))))
      (cons
        51
        (cdr (assoc 50 (tblsearch "STYLE" (getvar "textstyle"))))
      )
      (cons 72 jx)
      (cons 73 jy)
    )
  )
  (setq
    ent (entget (entlast))
    ent (subst (cons 11 i) (assoc 11 ent) ent)
  )
  (entmod ent)
)

(defun haws-make-mtext (i j h w s masked-p / ename-mtext)
  ;; creates annotative text if style is annotative.
  (setq h
    (cond
      (h)
      ((lm:isannotative (getvar "textstyle"))(haws-text-height-paper))
      (t (haws-text-height-model))
    )
  )
  (vl-cmdf "._mtext" i "_j" (strcat "_" j) "_h" h "_w" w s "")
  (cond
    (masked-p
     (setq ename-mtext (entlast))
     (entmod
       (append
         (entget ename-mtext)
         '((90 . 3) (63 . 256) (45 . 1.1) (441 . 0))
       )
     )
    )
  )
)

(vl-acad-defun 'HAWS-MKLINE)
(defun haws-mkline (pt1 pt2)
  (setq
    pt1 (if (= 2 (length pt1))
          (append pt1 '(0.0))
          pt1
        )
    pt2 (if (= 2 (length pt2))
          (append pt2 '(0.0))
          pt2
        )
  )
  (entmake
    (list
      (cons 0 "LINE")
      (append '(10) (trans pt1 1 0))
      (append '(11) (trans pt2 1 0))
    )
  )
)

;;
;; HAWS-PATH-CHOP-TRUNK
;; Chops the common initial elements "trunk" off lists provided,
;; leaving only the unique branches.
;; Useful for relating paths.
;;
(vl-acad-defun 'HAWS-PATH-CHOP-TRUNK)
(defun haws-path-chop-trunk (trees case-sensitive-p / length-common)
  (setq
    tree-common
     (car trees)
    length-common
     (length tree-common)
  )
  ;; Find common trunk length
  (mapcar
    '(lambda (tree / i)
       (setq i -1)
       (while (and
                (nth (setq i (1+ i)) tree)
                (if case-sensitive-p (= (nth i tree) (nth i tree-common)) (= (strcase (nth i tree)) (strcase (nth i tree-common))))
              )
       )
       (if (< i length-common)
         (setq length-common i)
       )
     )
    trees
  )
  ;; Chop off common trunk from each tree.
  (mapcar
    '(lambda (tree / i)
       (setq i -1)
       (while (< (setq i (1+ i)) length-common)
         (setq tree (cdr tree))
       )
       tree
     )
    trees
  )
)

;;
;;HAWS-PATH-RELATE
;;
;; Converts an absolute path to a relative path if possible
;; given path and comparison path, both including filename.
;;
(vl-acad-defun 'HAWS-PATH-RELATE)
(defun haws-path-relate (path-absolute path-compare case-sensitive-p / branch-absolute branch-compare branches list-absolute list-compare relative-path)
  (setq
    ;; Parse to lists.
    list-absolute
     (haws-strtolst path-absolute "\\" "\"" t)
    list-compare
     (haws-strtolst path-compare "\\" "\"" t)
    branches
     (haws-path-chop-trunk
       (list
         ;; remove filenames.
         (reverse (cdr (reverse list-absolute)))
         (reverse (cdr (reverse list-compare)))
       )
       case-sensitive-p
     )
    branch-absolute
     (car branches)
    branch-compare
     (cadr branches)
  )
  (setq
    relative-path
     (strcat
       (cond
         ((= (length branch-compare) 0) ".\\")
         ((/= (substr (car branch-compare) 2 1) ":")
          (apply
            'STRCAT
            (mapcar '(lambda (x) "..\\") branch-compare)
          )
         )
         (t "")
       )
       (haws-lsttostr (reverse (cons (car (reverse list-absolute)) (reverse branch-absolute))) "\\" "\"")
     )
  )
)

;;
;;HAWS-PATH-UNRELATE
;;
;; Converts a relative path to an absolute path
;; given path and comparison path, both including filename.
;;
(vl-acad-defun 'HAWS-PATH-UNRELATE)
(defun haws-path-unrelate
   (path-relative path-compare / list-compare list-relative)
  (setq
    ;; Parse to lists.
    list-relative
     (haws-strtolst path-relative "\\" "\"" t)
    ;; Reverse and remove filename
    list-compare
     (cdr
       (reverse (haws-strtolst path-compare "\\" "\"" t))
     )
  )
  (cond
    ;; If really relative, process.
    ((= (substr (car list-relative) 1 1) ".")
      (foreach
         node list-relative
        (cond
          ((= (substr node 1 1) ".")
           (setq list-relative (cdr list-relative))
          )
        )
        (cond ((= node "..\\") (setq list-compare (cdr list-compare))))
      )
      (haws-lsttostr (append (reverse list-compare) list-relative) "\\" "\"")
    )
    ;; If not really relative, return provided path.
    (t path-relative)
  )
)

;;
;;HAWS-PRIN1-TO-STRING
;;
;; For Intellicad, a cheap (and dirty, leaves a file on disk)
;; VL-PRIN1-TO-STRING
;; substitute
;;
(vl-acad-defun 'HAWS-PRIN1-TO-STRING)
(defun haws-prin1-to-string (atomx / f1 f2 string)
  (cond
    ((haws-vlisp-p) (vl-prin1-to-string atomx))
    (t
     (setq f2 (open "hawsprin1.tmp" "w"))
     (prin1 atomx f2)
     (setq f2 (close f2))
     (setq f1 (open "hawsprin1.tmp" "r"))
     (setq string (read-line f1))
     (setq f1 (close f1))
     string
    )
  )
)

;;;  HAWS-3PTTOBULGE
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                                             
;;;                 /\                          
;;;                /  \                         
;;;               /    \                        
;;;              /      \                       
;;;             /        \                      
;;;            /          \                     
;;;           /            R                    
;;;          /              \                   
;;;         /                \                  
;;;        /                  \                 
;;;       /                    \                
;;;      /                      \               
;;;     /       __chord___ ----- 1
;;;    3 ------                 *
;;;       .    ANG2        _.* 
;;;           - 2 - .__..-
;;;
;;;
;;;
(vl-acad-defun 'HAWS-3PTTOBULGE)
(defun haws-3pttobulge
   (pnt1 pnt2 pnt3 / ang1 ang2 ang3 bulge chord delta delta1 r)
;;;Returns the bulge of an arc defined by three points, PNT1, PNT2, and PNT3
;;;If point 2 nil, returns 0.
;;;In geometry triangle terms, R=a/(2*sin(A)) for any of the three points
;;;The sum of angles 1 and 3 is delta
  (cond
    ((not pnt2) 0)
    (t
     (setq
       chord
        (distance pnt1 pnt3)
       ang2
        (- (angle pnt2 pnt1) (angle pnt2 pnt3))
       ;;CHORD / SIN(ANG2) is 
       r
        (/ chord (* 2 (sin ang2)))
       delta1
        (* 2 (haws-asin (/ chord (* 2 r))))
       ;;If sin(ang1) is negative, bulge is negative.
       ;;Since AutoCAD always returns a positive angle,
       ;;if the quadrant of the second
       ang1
        (abs (- (angle pnt1 pnt3) (angle pnt1 pnt2)))
       ang1
        (abs
          (if (> ang1 pi)
            (- ang1 (* 2 pi))
            ang1
          )
        )
       ang3
        (abs (- (angle pnt3 pnt1) (angle pnt3 pnt2)))
       ang3
        (abs
          (if (> ang3 pi)
            (- ang3 (* 2 pi))
            ang3
          )
        )
       delta
        (* 2 (+ ang1 ang3))
       bulge
        (* (if (minusp r)
             -1
             1
           )
           (haws-tan (/ delta 4.0))
        )
     )
    )
  )
)


;;;  HAWS-SEGMENT-LENGTH
(vl-acad-defun 'HAWS-SEGMENT-LENGTH)
(defun haws-segment-length
;;;  Returns curve or straight length of a segment.
                       (2dpnt1 2dpnt2 bulge / d delta dover2 l r)
  (setq
    ;;Make sure points are truly 2d
    2dpnt1
     (haws-flatten 2dpnt1)
    2dpnt2
     (haws-flatten 2dpnt2)
    d (/ (distance 2dpnt1 2dpnt2) 2)
  ) ;_ end of setq
  (cond
    ((/= 0 bulge)
     (setq
       dover2
        (abs (* 2 (atan bulge)))
       delta
        (* 2 dover2)
       r (/ d (sin dover2))
     ) ;_ end of setq
     (* delta r)
    )
    (t (* d 2))
  ) ;_ end of cond
)

;;;HAWS-STRTOLST
;;;Parses a string into a list of fields.
;;;Usage: (haws-strtolst
;;;         [InputString containing fields]
;;;         [FieldSeparatorWC field delimiter wildcard string
;;;          Use "`," for comma and " ,\t,\n" for white space]
;;;         [TextDelimiter text delimiter character.]
;;;         [EmptyFieldsDoCount flag.
;;;           If nil, consecutive field delimiters are ignored.
;;;           Nil is good for word (white space) delimited strings.
;;;         ]
;;;       )
;;;Avoid cleverness.
;;;Human readability trumps elegance and economy and cleverness here.
;;;This should be readable to a programmer familiar with any language.
;;;In this function, I'm trying to honor readability in a new (2008) way.
;;;And I am trying a new commenting style.
;;;Tests
;;;(alert (apply 'strcat (mapcar '(lambda (x) (strcat "\n----\n" x)) (haws-strtolst "1 John,\"2 2\"\" pipe,\nheated\",3 the end,,,,," "," "\"" nil))))
;;;(alert (apply 'strcat (mapcar '(lambda (x) (strcat "\n----\n" x)) (haws-strtolst "1 John,\"2 2\"\" pipe,\nheated\",3 the end,,,,," "," "\"" T))))
(vl-acad-defun 'HAWS-STRTOLST)
(defun haws-strtolst (inputstring fieldseparatorwc textdelimiter
                  emptyfieldsdocount / charactercounter conversionisdone
                  currentcharacter currentfield currentfieldisdone
                  previouscharacter returnlist textmodeison textpairisopen
                 )
  ;;Initialize the variables for clarity's sake
  (setq
    charactercounter 0
    previouscharacter ""
    currentcharacter ""
    currentfield ""
    currentfieldisdone nil
    textmodeison nil
    conversionisdone nil
    returnlist nil
  )
  ;;Make sure that the FieldSeparatorWC is not empty.
  (cond
    ;;If an empty string matches the FieldSeparatorWC, then
    ((wcmatch "" fieldseparatorwc)
     ;;1. Give an alert about the problem.
     (alert
       ;;Include princ to allow user to see and copy error
       ;;after dismissing alert box.
       (princ
         (strcat
           "\n\""
           fieldseparatorwc
           "\" is not a valid field delimiter."
         )
       )
     )
     ;;2. Exit with error.
     (exit)
    )
  )
  ;;Start the main character-by-character InputString examination loop.
  (while (not conversionisdone)
    (setq
      ;;Save CurrentCharacter as PreviousCharacter.
      previouscharacter
       currentcharacter
      ;;CharacterCounter starts at 0 above.  Increment it.
      charactercounter
       (1+ charactercounter)
      ;;Get new CurrentCharacter from InputString.
      currentcharacter
       (substr inputstring charactercounter 1)
    )
    ;;Decide what to do with CurrentCharacter.
    (cond
      ;;If CurrentCharacter is a TextDelimiter, then
      ((= currentcharacter textdelimiter)
       ;;1.  Toggle the TextModeIsOn flag
       (if textmodeison
         (setq textmodeison nil)
         (setq textmodeison t)
       )
       ;;2.  Use and toggle the TextPairIsOpen flag.
       (cond
         (textpairisopen
           ;;Output it to CurrentField.
           (setq currentfield (strcat currentfield currentcharacter))
           (setq textpairisopen nil)
         )
         (t
           (setq textpairisopen t)
         )
       )
      )
      ;;Else if CurrentCharacter is a FieldDelimiter wildcard match, then
      ((wcmatch currentcharacter fieldseparatorwc)
       (cond
         ;;If TextModeIsOn = True, then 
         ((= textmodeison t)
          ;;Output CurrentCharacter to CurrentField.
          (setq currentfield (strcat currentfield currentcharacter))
         )
         ;;Else if
         ((or ;;EmptyFieldsDoCount, or
              (= emptyfieldsdocount t)
              ;;the CurrentField isn't empty,
              (/= "" currentfield)
          )
          ;;Then
          ;;Set the CurrentFieldIsDone flag to true.
          (setq currentfieldisdone t)
         )
         (t
          ;;Else do nothing
          ;;Do not flag the CurrentFieldDone,
          ;;nor output the CurrentCharacter.
          nil
         )
       )
      )
      ;;Else if CurrentCharacter is empty, then
      ((= currentcharacter "")
       ;;We are at the end of the string.
       ;;1.  Flag ConversionIsDone.
       (setq conversionisdone t)
       ;;2.  If
       (if (or ;;EmptyFieldsDoCount, or
               emptyfieldsdocount
               ;;the PreviousCharacter wasn't a FieldSeparatorWC, or
               (not (wcmatch previouscharacter fieldseparatorwc))
               ;;the ReturnList is still nil due to only empty non-counting fields in string,
               ;;(This check is a bug fix added 2008-02-18 TGH)
               (= returnlist nil)
           )
         ;;Then flag the CurrentFieldIsDone to wrap up the last field.
         (setq currentfieldisdone t)
       )
      )
      ;;Else (CurrentCharacter is something else),
      (t
       ;;Output CurrentCharacter to CurrentField.
       (setq currentfield (strcat currentfield currentcharacter))
       (setq textpairisopen nil)
      )
    )
    ;;If CurrentFieldIsDone, then
    (if currentfieldisdone
      ;;Output it to the front of ReturnList.
      (setq
        returnlist
         (cons currentfield returnlist)
        ;;Start a new CurrentField.
        currentfield
         ""
        currentfieldisdone nil
      )
    )
    ;;End the main character-by-character InputString examination loop.
  )
  ;;Reverse the backwards return list and we are done.
  (reverse returnlist)
)


;;; Read fields from a text string delimited by a field width or a delimiter character.
;;;Usage: (haws-rdfld
;;;         [field number]
;;;         [string containing fields]
;;;         [uniform field width, field delimiter character, or "W" for words separated by one or more spaces]
;;;         [sum of options: 1 (non-numerical character field) 2 (unlimited length field at end of string)]
;;;       )
;;;Tests
;;;(haws-rdfld 3 "1 John,\"2 2\"\" pipe,\nheated\",3 the end,,,,," "," 1))))
(vl-acad-defun 'HAWS-RDFLD)
(defun haws-rdfld (fieldno inputstring fieldseparator opt / atomcounter
               atomy atomx emptyfieldsdocount ischrislong parsedlist
               textdelimiter fieldseparatorwc ischr islong
              )
  (setq
    ischr
     (= 1 (logand 1 opt))
    islong
     (= 2 (logand 2 opt))
    textdelimiter "\""
    emptyfieldsdocount t
    fieldseparatorwc fieldseparator
  )
  (cond
    ;;If the field delimiter is a comma ",", then
    ((= fieldseparatorwc ",")
     ;;Replace it with an AutoCAD escaped comma wildcard.
     (setq fieldseparatorwc "`,")
    )
    ;;If the field delimiter is "W" (for word or whitespace), then
    ((= fieldseparatorwc "W")
     (setq
       ;;1. Replace it with a white space wild card.
       fieldseparatorwc
        " ,\t,\n"
       ;;2.  Set EmptyFieldsDoCount to nil
       emptyfieldsdocount
        nil
     )
    )
  )
  (cond
    ;;If fielddelimiter is a number, then do a fixed width field extraction.
    ((= (type fieldseparatorwc) 'INT)
     (setq
       atomx
        (substr
          inputstring
          (1+ (* (1- fieldno) fieldseparatorwc))
          (if islong
            1000
            fieldseparatorwc
          )
        )
     )
     (if (and ischr (not islong))
       (setq atomx (haws-rdfld-unpad atomx))
     )
    )
    ;;Else do a character delimiter field extraction.
    (t
     (setq
       parsedlist
        (haws-strtolst
          inputstring
          fieldseparatorwc
          textdelimiter
          emptyfieldsdocount
        )
     )
     (setq atomx (nth (1- fieldno) parsedlist))
     ;;If the IsLong flag is set, add any subsequent fields to the output string.
     (cond
       (islong
        (setq atomcounter (1- fieldno))
        (while (setq
                 atomy
                  (nth (setq atomcounter (1+ atomcounter)) parsedlist)
               )
          (setq atomx (strcat atomx fieldseparator atomy))
        )
       )
     )
    )
  )
  (setq
    atomx
     (if ischr
       atomx
       (distof atomx)
     )
  )
)

;;Strip white space from beginning and end of a string
(vl-acad-defun 'HAWS-RDFLD-UNPAD)
(defun haws-rdfld-unpad (str)
  (while (wcmatch (substr str 1 1) " ,\t")
    (setq str (substr str 2))
  )
  (while (wcmatch (haws-endstr str 1 1) " ,\t")
    (setq str (substr str 1 (1- (strlen str))))
  )
  str
)

;;Returns nil if in ICAD mode
(vl-acad-defun 'HAWS-REGISTRY-READ)
(defun haws-registry-read (reg-key val-name)
  (cond
    ((c:haws-icad-p) nil)
    ((vl-registry-read reg-key val-name))
  )
)

;;Returns nil if in ICAD mode
(vl-acad-defun 'HAWS-REGISTRY-WRITE)
(defun haws-registry-write (reg-key val-name val-data)
  (cond
    ((c:haws-icad-p) nil)
    ((vl-registry-write reg-key val-name val-data))
  )
)


;;Remove an element from a list
(vl-acad-defun 'HAWS-REMOVE)
(defun haws-remove (element lst)
  (append
    (reverse (cdr (member element (reverse lst))))
    (cdr (member element lst))
  )
)

;;Convert a radian angle to a presentation quality bearing.
(vl-acad-defun 'HAWS-RTOB)
(defun haws-rtob (rad au / b i)
  (setq b (angtos rad au))
  (if (wcmatch b "*d*")
    (progn
      (setq i 0)
      (while (/= "d" (substr b (setq i (1+ i)) 1)))
      (setq b (strcat (substr b 1 (1- i)) "%%d" (substr b (1+ i))))
    )
  )
  (if (wcmatch b "*d#[`.']*")
    (progn
      (setq i 0)
      (while (/= "d" (substr b (setq i (1+ i)) 1)))
      (setq b (strcat (substr b 1 i) "0" (substr b (1+ i))))
    )
  )
  (if (wcmatch b "*'#[`.\"]*")
    (progn
      (setq i 0)
      (while (/= "'" (substr b (setq i (1+ i)) 1)))
      (setq b (strcat (substr b 1 i) "0" (substr b (1+ i))))
    )
  )
  (setq
    b (cond
        ((= b "N") "NORTH")
        ((= b "S") "SOUTH")
        ((= b "E") "EAST")
        ((= b "W") "WEST")
        (b)
      )
  )
)

;; RTOSTA sub-function converts a real number to a base 100 road
;; station.
(vl-acad-defun 'HAWS-RTOSTA)
(defun haws-rtosta (sta lup / isneg after before)
  (setq
    lup
     (cond
       (lup)
       ((getvar "luprec"))
     )
    isneg
     (minusp sta)
    sta
     (rtos (abs sta) 2 lup)
  )
  (while (< (strlen sta)
            (if (= lup 0)
              3
              (+ lup 4)
            )
         )
    (setq sta (strcat "0" sta))
  )
  (setq
    after
     (if (= lup 0)
       (- (strlen sta) 1)
       (- (strlen sta) lup 2)
     )
    before
     (substr sta 1 (1- after))
    after
     (substr sta after)
  )
  (if isneg
    (setq
      before
       (strcat "-(" before)
      after
       (strcat after ")")
    )
  )
  (strcat before "+" after)
)

;;;  Trig functions not included with AutoLISP
(vl-acad-defun 'HAWS-ASIN)
(defun haws-asin (x) (atan x (sqrt (- 1 (* x x)))))
(vl-acad-defun 'HAWS-ACOS)
(defun haws-acos (x) (atan (sqrt (- 1 (* x x))) x))
(vl-acad-defun 'TAN)
(defun haws-tan (x) (/ (sin x) (cos x)))
(vl-acad-defun 'HAWS-VSET)
(defun haws-vset (vlst)
  (foreach
     v vlst
    (if (getvar (car v))
      (setvar (car v) (cadr v))
    )
  )
)

(vl-acad-defun 'HAWS-VTOG)
(defun haws-vtog (vlst)
  (foreach
     v vlst
    (princ (strcat "\n" v " toggled to "))
    (setvar
      v
      (princ
        (if (= (getvar v) 0)
          1
          0
        )
      )
    )
  )
  (princ)
)

(vl-acad-defun 'HAWS-VSAVE)
(defun haws-vsave (vlst)
  (setq *haws-vstr* (mapcar '(lambda (v) (list v (getvar v))) vlst))
)

(vl-acad-defun 'HAWS-VRSTOR)
(defun haws-vrstor ()
  (mapcar '(lambda (v) (setvar (car v) (cadr v))) *haws-vstr*)
)

;; This function does a word wrap on a string by cutting the string
;; into
;;pieces no more than "maxlen" characters long after places where
;; "char" character is matched.  Leading and trailng spaces and the
;; used
;; break
;;characters are stripped.
;;Example: (wrap "Go home, eat dinner, comb, brush, sleep" 15 ",")
;;Returns  ("Go home" "eat dinner" "comb, brush" "sleep")
(vl-acad-defun 'HAWS-WRAP)
(defun haws-wrap (strng1 maxlen char / first i lstrni stripc strips strng2
              strngi temp wlist
             )
  (setq
    i 1
    char
     (strcat "`" char)
    first t
    wlist nil
    strng2 ""
    strngi ""
    lstrni 0
  )
  ;;Break strng1 at every break point
  (while (/= "" (substr strng1 i))
    (cond
      (;;For every break or at end of string
       (or (wcmatch (substr strng1 1 i) (strcat "*" char))
           (= i (strlen strng1))
       )
       (setq
         strngi
          (substr strng1 1 i)
         strips strngi
         stripc strngi
         strng1
          (substr strng1 (1+ i))
         i 1
       )
       ;; Strip leading spaces from all but first piece.  Save as
       ;; strips.
       (if (not first)
         (while (= (substr strips 1 1) " ")
           (setq strips (substr strips 2))
         )
       )
       ;;Strip break character.  Save as stripc
       (if (wcmatch stripc (strcat "*" char))
         (setq stripc (substr stripc 1 (1- (strlen stripc))))
       )
       ;; Add strngi to strng2 if possible, otherwise, call strng2
       ;; full.
       (cond
         ;;If strng2 is empty set to strips
         ((= "" strng2) (setq strng2 strips))
         ;;else add strngi to strng2 if it fits stripped.
         ((<= (strlen (setq temp (strcat strng2 stripc))) maxlen)
          (setq strng2 (strcat strng2 strngi))
         )
         ((if (wcmatch strng2 (strcat "*" char))
            (setq strng2 (substr strng2 1 (1- (strlen strng2))))
          )
          (setq
            wlist
             (cons strng2 wlist)
            strng2 strips
          )
         )
       )
       (setq first nil)
      )
      (t (setq i (1+ i)))
    )
  )
  (reverse (cons strng2 wlist))
)

;;Functions for oo, selstyle, and le

;;Selcerob--Selects a certain type of object. Returns entsel list.
(vl-acad-defun 'HAWS-SELCEROB)
(defun haws-selcerob (prmpt serch / e elst enm ok)
  (while (not ok)
    (while (not (setq e (entsel prmpt))))
    (setq elst (entget (setq enm (car e))))
    (if (/= (cdr (assoc 0 elst)) serch)
      (princ (strcat "**Not a " serch ", try again**"))
      (setq ok t)
    )
  )
  e
)

;;
;;HAWS-TXLEN
;;
;; For Intellicad compatibility
;;
(vl-acad-defun 'HAWS-TXLEN)
(defun haws-txlen (string height)
  (if (c:haws-icad-p)
    (* height (strlen string) 0.80)
    (caadr (textbox (list (cons 1 string) (cons 40 height))))
  )
)

;;
;; HAWS-VLISP-P
;;
;;Tests whether visual lisp functions are available.
(vl-acad-defun 'HAWS-VLISP-P)
(defun haws-vlisp-p ()
  (not (< (atof (getvar "acadver")) 15))
)


;;end sub-functions

;#endregion
;#region USE_LOG
;;; Verify/Check values of stored authorization strings at
;;; load-time
;;; against computer name and bios date.
;;; Delete registry entry if invalid for this computer.
;;; New scheme 2007-09:
;;; Getting a little more lax.
;;; If (HAWS-READCFG "/HawsEDC/Modules/package/OrderString") "",
;;; we assume quite trustingly that the application has never yet been tried.
(defun haws-checkstoredstrings (/ authlist authstring deleteall temp)
  (haws-milepost "Entering HAWS-CHECKSTOREDSTRINGS")
  (foreach
     package '(0 3)
    (setq authstring (haws-readauthcode package))
    (cond
      (;;If
       (and
         ;;Authstring is present
         authstring
         (/= authstring "aaaaaaaaaaaa")
         (haws-milepost
           "Authstring is present and isn't the dummy string.  Now checking that it's for the right computer to delete it if not."
         )
         ;;and
         (or ;; it either is invalid,
             (/= (strlen authstring) 12)
             ;;for the wrong computer,
             (not
               (equal
                 (cdr
                   (cdddr (setq authlist (haws-authtolist authstring)))
                 )
                 (cons (haws-getshortcomputername) (haws-getbiosdate))
               )
             )
             ;;or
             ;;it's for the wrong package,
             (/= package (caddr authlist))
         )
       )
       ;;Then flag to delete all order strings and auth strings from storage.
       (haws-milepost
         (strcat "Package " (itoa package) "codes are wrong")
       )
       (setq deleteall t)
      )
      ((not authstring)
       (haws-milepost
         ";;Else if there is no stored string
       "
       )
       ;|
      (t
       (haws-milepost ";;Always give a new free trial (for testing only)
       ;; by storing order and auth strings for a trial
       ")
      |;
       (haws-milepost
         ";;Assume it's a virgin installation,
       ;; and give a free trial by storing order and auth strings
       ;; for
       ;; a 30 day trial
       "
       )
       (haws-writepackagecode
         package
         "OrderString"
         (haws-binarytouser
           (haws-encryptorderstring
             (haws-listtobinary
               (setq
                 temp
                  (cons
                    (fix (getvar "date"))
                    (cons
                      0
                      (cons
                        package
                        (cons
                          0
                          (cons
                            (haws-getshortcomputername)
                            (haws-getbiosdate)
                          )
                        )
                      )
                    )
                  )
               )
             )
           )
         )
       )
       (haws-writepackagecode
         package
         "AuthString"
         (haws-binarytouser
           (haws-encryptauthstring
             ;;Trial length
             (haws-listtobinary (cons (+ (car temp) 30) (cdr temp)))
           )
         )
       )
      )
    )
  )
  (cond
    (deleteall
     (foreach
        package '(0 3)
       (haws-writepackagecode package "OrderString" "aaaaaaaaaaaa")
       (haws-writepackagecode package "AuthString" "aaaaaaaaaaaa")
     )
    )
  )
  (haws-milepost "Finished HAWS-CHECKSTOREDSTRINGS")
)
(haws-checkstoredstrings)
(if (/=(haws-use-get-local-log-string)(haws-use-initialize-log-string))(haws-use-log-remote))

;#endregion(PROMPT "loaded.")
 ;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
