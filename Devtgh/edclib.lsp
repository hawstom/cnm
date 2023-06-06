(PROMPT "\nHawsEDC library functions...")

;;;This is the current version of HawsEDC and CNM
(DEFUN
   HAWS-UNIFIED-VERSION ()
  "5.4.09"
)
(DEFUN
   HAWS-COPYRIGHT ()
  "Copyright 2023 Thomas Gail Haws"
)
;;;(SETQ *HAWS-ICADMODE* T);For testing icad mode in acad.
(SETQ *HAWS-DEBUGLEVEL* 0)
;;This function returns the current setting of nagmode.
;;Elizabeth asked me to give her a version with no nag mode (direct to fail).
(DEFUN HAWS-NAGMODE-P () T)

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
;;; 20150916 4.2.27 TGH Fixed (command) incompatibility with v2015+ in lambdas.  Using (command-s).
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
(DEFUN C:HCNM-ABOUT ()
(haws-core-init 216) (C:HAWS-ABOUT))
(DEFUN
   C:HAWS-ABOUT (/ LICENSEREPORT)
(haws-core-init 217)
  (ALERT
    (PRINC
      (STRCAT
        "Construction Notes Manager version "
        (HAWS-UNIFIED-VERSION)
        "\n\n" (HAWS-COPYRIGHT) "\nhttp://constructionnotesmanager.com\nhttp://hawsedc.com\n"
        (APPLY
          'STRCAT
          (MAPCAR
            '(LAMBDA (PACKAGE)
               (IF (SETQ
                     LICENSEREPORT
                      (HAWS-PACKAGELICENSEREPORT
                        (CAR PACKAGE)
                      )
                   )
                 (STRCAT
                   "\n"
                   (CADR PACKAGE)
                   " authorized on this computer. Key: "
                   LICENSEREPORT
                 )
                 ""
               )
             )
            *HAWS-EDCMODULES*
          )
        )
      )
    )
  )
  (PRINC)
)
;;;=================================================================
;;;
;;;                Error Handler and Licensing functions
;;;
;;;=================================================================
;;;
;;; The HawsEDC authorization routine
;;;
;;; Data forms:
;;; Orderlist    '(pkgid numauths compname biosmonth biosday biosyear
;;; startdate)

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
(SETQ
  *HAWS-EDCMODULES*
   '((0 "CNM Lite") (3 "CNM Pro"))
  *HAWS-EDCAPPGROUPS*
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
  *HAWS-ORIGINATOR*
   "TGH"
  *HAWS-SALESID*
   (COND
     ((= *HAWS-ORIGINATOR* "TGH") 0)
     ((= *HAWS-ORIGINATOR* "PCW") 1)
     ((= *HAWS-ORIGINATOR* "DOW") 2)
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
(DEFUN
   haws-core-init (COMMAND-ID / APPGROUP VALIDATED)
  (setq APPGROUP (cadr (assoc command-id *haws-edccommands*)))
  ;; If computer already has authorization,
  (COND
    ((OR
         ;; No more nag mode.
         T
         ;; this computer has authorization
         (HAWS-VALIDATEAPPGROUP APPGROUP)
         ;; or we successfully get it from the user,
         (HAWS-AUTHORIZEAPPGROUP APPGROUP)
         ;; or we are allowing run after nag,
         (HAWS-NAGMODE-P)
     )
     (SETQ VALIDATED T)
    )
    (T
     (ALERT
       (PRINC
         "Application must be authorized to run.\n\nClosing application."
       )
     )
     (EXIT)
    )
  )
  (HAWS-USE-LOG-LOCAL COMMAND-ID)
  (SETQ
    OLDERR *ERROR*
    *ERROR* HAWS-CORE-STPERR
  )
  ;;Versional housekeeping
  (if (= 'subr (type *push-error-using-command*)) (*push-error-using-command*))
  VALIDATED
)

;;Stperr replaces the standard error function.
;;It sets everything back in case of an error.
(DEFUN
   HAWS-CORE-STPERR (S)
  ;; Restore old *error* handler
  (IF OLDERR
    (SETQ
      *ERROR* OLDERR
      OLDERR NIL
    )
  )
  (COND
    ((/= S "Function cancelled")
     (PRINC (STRCAT "\nTrapped error: " S))
    )
  )
  (WHILE (< 0 (GETVAR "cmdactive"))
    (COMMAND)
  )
  (IF (= (TYPE F1) (QUOTE FILE))
    (SETQ F1 (CLOSE F1))
  )
  ;; Close files
  (IF (= (TYPE F2) (QUOTE FILE))
    (SETQ F2 (CLOSE F2))
  )
  (IF (= (TYPE F3) (QUOTE FILE))
    (SETQ F3 (CLOSE F3))
  )
  ;;Versional housekeeping
  (if (/= 'subr (type command-s)) (setq command-s command))
  (IF (= 8 (LOGAND (GETVAR "undoctl") 8))
    (COMMAND-S "._undo" "end")
  )
  ;; End undo group
  (IF VSTR
    (HAWS-VRSTOR)
  )
  ;; Restore variables to previous values
  (IF UCSP
    (COMMAND-S "._UCS" "_P")
  )
  ;; Restore previous UCS
  (IF UCSPP
    (COMMAND-S "._UCS" "_P")
  )
  ;; Restore previous UCS
  (IF ENM
    (REDRAW ENM)
  )
  ;; Redraw work entity
  (IF ERROSM
    (SETVAR "osmode" ERROSM)
  )
  (SETQ
    UCSP NIL
    UCSPP NIL
    ENM NIL
  )
  (PRINC)
)
(DEFUN
   haws-core-restore ()
  (SETQ
    UCSP NIL
    UCSPP NIL
    ENM NIL
    F1 NIL
    F2 NIL
    *ERROR* OLDERR
    OLDERR NIL
  )
  ;;Versional housekeeping
  (if (= 'subr (type *pop-error-mode*)) (*pop-error-mode*))
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
(DEFUN
   HAWS-VALIDATEAPPGROUP (APPGROUP / AUTHDATEDAYSAHEAD
                          USERSTRINGDATEDIFF AUTHORIZEDPERMANENT
                          AUTHSTRING ORDERSTRING PACKAGE TRIALDAYSLEFT
                         )
  ;;To begin with, assume there is no free trial left.
  (SETQ TRIALDAYSLEFT -1)
  ;;Check whether any of the packages that include the requested appgroup
  ;;are authorized to run.
  (FOREACH
     PACKAGE (CDR (ASSOC APPGROUP *HAWS-EDCAPPGROUPS*))
    (HAWS-MILEPOST
      (STRCAT
        ";;Package "
        (ITOA PACKAGE)
        " listed as including this appgroup."
      )
    )
    ;;If
    (COND
      ((AND
         ;;Permanent authorization hasn't already been found in some package
         (NOT AUTHORIZEDPERMANENT)
         ;;There is an orderstring found for this package,
         (SETQ ORDERSTRING (HAWS-READORDERCODE PACKAGE))
         ;;and there is an authstring found for this package,
         (SETQ AUTHSTRING (HAWS-READAUTHCODE PACKAGE))
         ;;And the order and authorization strings match (return a date difference)
         ;;(machine matching should have already been checked by HAWS-CHECKSTOREDSTRINGS on loadup)
         (SETQ
           USERSTRINGDATEDIFF
            (HAWS-USERSTRINGDATEDIFF
              ORDERSTRING
              AUTHSTRING
            )
         )
       )
       (HAWS-MILEPOST
         (STRCAT
           "Auth match found. P:"
           (ITOA PACKAGE)
           " O:"
           ORDERSTRING
           ", A:"
           AUTHSTRING
           ", Diff:"
           (ITOA USERSTRINGDATEDIFF)
         )
       )
       ;;Then
       ;;Decide whether this package is authorized.
       (HAWS-MILEPOST
         (STRCAT
           "Current trial stands at:"
           (ITOA (FIX TRIALDAYSLEFT))
         )
       )
       (COND
         ;; If unlimited stand-alone auth is found for this appgroup
         ;; (auth code date is at least 1000000 days before order code date)
         ;; in this package, authorize immediately.
         ((<= USERSTRINGDATEDIFF -1000000)
          (SETQ AUTHORIZEDPERMANENT T)
         )
         ;;Else if this authdate extends the current trial length, let it.
         (T
          (SETQ
            TRIALDAYSLEFT
             (MAX
               TRIALDAYSLEFT
               (- (CAR (HAWS-AUTHTOLIST AUTHSTRING))
                  (FIX (GETVAR "date"))
               )
             )
          )
         )
       )
      )
    )
  )
  (COND
    ;;If authorized permanently, return T
    ((OR AUTHORIZEDPERMANENT (= APPGROUP -1)) T)
    ;;Else if there are positive days left in a trial, print a note and return T
    ((NOT (MINUSP TRIALDAYSLEFT))
     (PRINC
       (STRCAT
         "\nYou have "
         (ITOA TRIALDAYSLEFT)
         " days left in your trial period."
       )
     )
     T
    )
  )
)

;;; AUTHORIZEAPPGROUP gives user order code, prompts user for matching
;;; authorization
;;; code,
;;; and records authorization for future reference.
;;;
;;; This is the nag routine.
(DEFUN
   HAWS-AUTHORIZEAPPGROUP (APPGROUP)
  ;;Choose nag mode or expire mode prompt.
  (COND
    ;;If nag mode is active
    ((HAWS-NAGMODE-P)
     ;;Then
     (COND
       ;;If it's about 1/100th of the time.
       ;;(This expression holds true everytime (getvar "date") rolls around to a 00 in the 1/100,000,000 of a day place.)
       ((=
          (SUBSTR (RTOS (REM (GETVAR "date") 1) 2 8) 9)
          "00"
        )
        (ALERT
          (PRINC
            (STRCAT
              "\nThis application is running in unlicensed mode."
              "\n\nLicense it or extend the expired trial mode"
              "\nby emailing the order code generated at the following command line prompts."
            )
          )
        )
        (HAWS-ORDERPACKAGE APPGROUP)
       )
       (T (PRINC "\nContinuing in evaluation mode.") T)
     )
    )
    ;;Else expire mode is active
    (T
     (ALERT
       (PRINC
         (STRCAT
           "\nThis application has not been authorized."
           "\n\nYou must order trial or paid authorization"
           "\nby emailing the order code generated at the following command line prompts."
         )
       )
     )
     (HAWS-ORDERPACKAGE APPGROUP)
    )
  )
)

(DEFUN
   HAWS-PACKAGELICENSEREPORT (PACKAGE / DATEDIFF)
  (SETQ
    AUTHSTRING
     (HAWS-READAUTHCODE PACKAGE)
    DATEDIFF
     (HAWS-PACKAGEUSERSTRINGDATEDIFF PACKAGE)
  )
  (COND
    ((NOT (AND DATEDIFF AUTHSTRING)) NIL)
    ((<= DATEDIFF -1000000) (STRCAT AUTHSTRING " (unlimited)"))
    ((>= DATEDIFF 31)
     (STRCAT AUTHSTRING " (" (ITOA DATEDIFF) " day trial)")
    )
    (T NIL)
  )
)

(DEFUN
   HAWS-PACKAGEUSERSTRINGDATEDIFF
   (PACKAGE / AUTHSTRING ORDERSTRING USERSTRINGDATEDIFF)
  (COND
    ((AND
       ;;There is an orderstring found for this package,
       (SETQ ORDERSTRING (HAWS-READORDERCODE PACKAGE))
       ;;and there is an authstring found for this package,
       (SETQ AUTHSTRING (HAWS-READAUTHCODE PACKAGE))
       ;;And the order and authorization strings match (return a date difference)
       ;;(machine matching should have already been checked by HAWS-CHECKSTOREDSTRINGS on loadup)
       (SETQ
         USERSTRINGDATEDIFF
          (HAWS-USERSTRINGDATEDIFF
            ORDERSTRING
            AUTHSTRING
          )
       )
     )
     (HAWS-MILEPOST
       (STRCAT
         "Auth match found. P:"
         (ITOA PACKAGE)
         " O:"
         ORDERSTRING
         ", A:"
         AUTHSTRING
         ", Diff:"
         (ITOA USERSTRINGDATEDIFF)
       )
     )
     (HAWS-USERSTRINGDATEDIFF ORDERSTRING AUTHSTRING)
    )
  )
)

;;; USERSTRINGDATEDIFF compares an authorization string and a user
;;; string.
;;; Returns a date difference if the strings match.
;;; Returns nil if strings don't match.
(DEFUN
   HAWS-USERSTRINGDATEDIFF
   (ORDERSTRING AUTHSTRING / ACADDATE AUTHLIST ORDERLIST)
  (COND
    ;; If AUTHSTRING isn't the same length as ORDERSTRING,
    ;; comparison fails.  Return nil.
    ((/= (STRLEN AUTHSTRING) (STRLEN ORDERSTRING)) NIL)
    ;; If any mismatches besides date, comparison fails.
    ;; Return nil.
    ((NOT
       (EQUAL
         (CDR (SETQ ORDERLIST (HAWS-ORDERTOLIST ORDERSTRING)))
         (CDR (SETQ AUTHLIST (HAWS-AUTHTOLIST AUTHSTRING)))
       )
     )
     NIL
    )
    ;;Else return the difference.
    (T (- (CAR AUTHLIST) (CAR ORDERLIST)))
  )
)

(DEFUN
   HAWS-READORDERCODE (PACKAGE)
  (HAWS-READPACKAGECODE PACKAGE "OrderString")
)

(DEFUN
   HAWS-READAUTHCODE (PACKAGE)
  (HAWS-READPACKAGECODE PACKAGE "AuthString")
)

;;;(HAWS-READPACKAGECODE 0 "AuthString")
;;;(HAWS-READPACKAGECODE 0 "OrderString")


;;; Returns stored string or nil.
;;; (getcfg) returns nil if the section doesn't exist.  Returns "" if the param doesn't exist.
;;; Since this is executed by (HAWS-CHECKSTOREDSTRINGS) on load, we can use this function to transfer stored info from the registry
;;; to the new storage location, and we can depend on (getcfg) returning nil the first time.
(DEFUN
   HAWS-READPACKAGECODE (PACKAGE STRINGNAME / STOREDSTRING)
  (HAWS-MILEPOST
    (STRCAT
      "Beginning READPACKAGECODE for package "
      (ITOA PACKAGE)
      " "
      STRINGNAME
    )
  )
  (HAWS-READCFG
    (LIST "HawsEDC" "Modules" (ITOA PACKAGE) STRINGNAME)
  )
)

;;; HAWS-READCFG gets the value of a setting from the favorite HAWS-location.
;;; (getcfg) returns nil if the section doesn't exist.  Returns "" if the param doesn't exist.
(DEFUN
   HAWS-READCFG (LOCATIONLIST / MOVEREQUIRED RETURNVALUE STOREDSTRING)
  (COND
    ;;If the requested value is in (getcfg), use it.
    ;;Intentionally here, we are using "".
    ;;This means we will only try the registry on a virgin install of this logic.
    ((SETQ
       STOREDSTRING
        (GETCFG
          ;;CFG section
          (HAWS-LOCATIONTOCFGSECTION LOCATIONLIST)
        )
     )
    )
    ;;Else if the value is set in the HKCU registry section, use it and write to (setcfg).
    (;;Read from the HKCU registry section
     (SETQ
       STOREDSTRING
        (HAWS-REGISTRY-READ
          ;;Registry path
          (HAWS-LOCATIONTOREGISTRYPATH
            LOCATIONLIST
            "HKEY_CURRENT_USER"
          )
          ;;Registry key
          (CAR (REVERSE LOCATIONLIST))
        )
     )
     ;;Flag to move value
     (SETQ MOVEREQUIRED T)
    )
    ;;Else try reading it from the HKLM section
    ((SETQ
       STOREDSTRING
        (HAWS-REGISTRY-READ
          ;;Registry path
          (HAWS-LOCATIONTOREGISTRYPATH
            LOCATIONLIST
            "HKEY_LOCAL_MACHINE"
          )
          ;;Registry key
          (CAR (REVERSE LOCATIONLIST))
        )
     )
     ;;Flag to move value
     (SETQ MOVEREQUIRED T)
    )
  )
  (COND
    ;;Write to preferred location if flagged and return stored string
    ((AND STOREDSTRING MOVEREQUIRED)
     (SETCFG
       ;;CFG section
       (HAWS-LOCATIONTOCFGSECTION LOCATIONLIST)
       ;;Value
       STOREDSTRING
     )
    )
  )
  STOREDSTRING
)


;;;(HAWS-WRITEPACKAGECODE 0 "AuthString")
;;;(HAWS-WRITEPACKAGECODE 0 "OrderString")


;;; Stores string and returns stored string or nil.
;;; (getcfg) returns nil if the section doesn't exist.  Returns "" if the param doesn't exist.
(DEFUN
   HAWS-WRITEPACKAGECODE
   (PACKAGE STRINGNAME STRINGVALUE / STOREDSTRING)
  (HAWS-MILEPOST
    (STRCAT
      "Beginning WRITEPACKAGECODE for package "
      (ITOA PACKAGE)
      " "
      STRINGNAME
    )
  )
  (HAWS-WRITECFG
    (LIST "HawsEDC" "Modules" (ITOA PACKAGE) STRINGNAME)
    STRINGVALUE
  )
)


;;; HAWS-WRITECFG sets the value of a setting in the favorite HAWS- location.
(DEFUN
   HAWS-WRITECFG (LOCATIONLIST INPUTSTRING / STOREDSTRING)
  (SETQ
    STOREDSTRING
    (SETCFG
      ;;CFG section
      (HAWS-LOCATIONTOCFGSECTION LOCATIONLIST)
      ;;Value
      INPUTSTRING
   )
  )
)

(DEFUN
   HAWS-LOCATIONTOCFGSECTION (LOCATIONLIST)
  (STRCAT
    ;;Start path with the AppData/ prefix per AutoCAD help.
    "AppData/"
    ;;Add the rest of the location provided.
    (HAWS-LSTTOSTR LOCATIONLIST "/" "\"")
  )
)

(DEFUN
   HAWS-LOCATIONTOREGISTRYPATH (LOCATIONLIST ROOT)
  (STRCAT
    ;;Start with the prefix to get into the software area
    ROOT
    "\\Software\\"
    ;;Add all but the last element of the location provided 
    (HAWS-LSTTOSTR
      (REVERSE (CDR (REVERSE LOCATIONLIST)))
      "\\"
      "\""
    )
  )
)

;;; ORDERPACKAGE is called when the user opts to order.
;;; It generates an order string and prompts for a matching authorization string.
;;; If authorization is provided correctly, it writes both to the storage location.,
;;; Returns T if successful, nil if not successful.

(DEFUN
   HAWS-ORDERPACKAGE (APPGROUP / AUTHMATCHSUCCESS AUTHSTRING
                      AUTHWRITESUCCESS NAGMATCHSUCCESS NAGSTRINGS
                      ORDERLIST ORDERSTRING PACKAGE WRITESTRINGS
                     )
  (SETQ
    ORDERLIST
     (APPEND
       (LIST (FIX (GETVAR "date")) *HAWS-SALESID*)
       (SETQ PACKAGE (HAWS-GETPACKAGE APPGROUP))
       (LIST (HAWS-GETSHORTCOMPUTERNAME))
       (HAWS-GETBIOSDATE)
     )
    ORDERSTRING
     (HAWS-BINARYTOUSER
       (HAWS-ENCRYPTORDERSTRING
         (HAWS-LISTTOBINARY ORDERLIST)
       )
     )
  )
  (ALERT
    (PRINC
      (STRCAT
        "\n\nThe order code for the package you ordered will be shown below."
        "\n\nIf you already ordered an authorization key, please enter it at the following prompt."
        "\nTo order an authorization key, please close this message,"
        "\nthen copy and paste the order code shown into the contact form at www.HawsEDC.com."
      )
    )
  )
  (SETQ
    AUTHSTRING
     (GETSTRING
       (STRCAT
         "\nOrder code is: "
         ORDERSTRING
         "\nEnter authorization key or <continue>: "
       )
     )
  )
  ;;Act on input
  (COND
    ;; If a matching authorization was supplied,
    ;; flag to write the authorization and flag success,
    ((HAWS-USERSTRINGDATEDIFF ORDERSTRING AUTHSTRING)
     (SETQ WRITESTRINGS T)
     (SETQ AUTHMATCHSUCCESS T)
    )
  )
  (COND
    ;; If flagged, write both strings to storage and flag success.
    (WRITESTRINGS
     (HAWS-WRITECFG
       (LIST
         "HawsEDC"
         "Modules"
         (ITOA (CAR PACKAGE))
         "OrderString"
       )
       ORDERSTRING
     )
     (HAWS-WRITECFG
       (LIST "HawsEDC" "Modules" (ITOA (CAR PACKAGE)) "AuthString")
       AUTHSTRING
     )
     (IF (AND
           (= (HAWS-READPACKAGECODE (CAR PACKAGE) "AuthString")
              AUTHSTRING
           )
           (= (HAWS-READPACKAGECODE (CAR PACKAGE) "OrderString")
              ORDERSTRING
           )
         )
       (SETQ AUTHWRITESUCCESS T)
     )
    )
  )
  ;;Report to user
  (COND
    ((AND AUTHMATCHSUCCESS AUTHWRITESUCCESS)
     (ALERT (PRINC "Authorization was successful."))
     T
    )
    (NAGMATCHSUCCESS T)
    (T
     (ALERT
       (PRINC
         (STRCAT
           "Authorization was not successful."
           (IF (NOT AUTHMATCHSUCCESS)
             (STRCAT
               "\nOrder code\n\"" ORDERSTRING
               "\"\nand authorization key\n\"" AUTHSTRING
               "\"\ndon't match."
              )
             ""
           )
         )
       )
     )
     NIL
    )
  )
)


;;; HAWS-ORDERLICENSES gets an order code from the command line
(DEFUN
   C:HAWS-ORDERLICENSES ()
(haws-core-init 218)
  (HAWS-ORDERPACKAGE 128)
  (PRINC)
)
;;GETPACKAGE displays a user form to get an order for the package
;;to authorize.
;;Returns a list of package number and number of authorizations.
;; 0 authorizations means authorization is for single user unlimited
;; use.
(DEFUN
   HAWS-GETPACKAGE (APPGROUP / PACKAGES STRING)
;;;Begin user choice section
  (TEXTPAGE)
  (PROMPT
    (STRCAT
      "\n\n==========================================================================="
      (COND
        ((= APPGROUP 128)
         "\n\nAuthorization is available for the following packages:\n"
        )
        ("\n\nThe tool you want to use is included in the following packages:\n"
        )
      )
    )
  )
  (SETQ PACKAGES (CDR (ASSOC APPGROUP *HAWS-EDCAPPGROUPS*)))
  (COND
    ((MEMBER 0 PACKAGES)
     (PROMPT "\nCNM Lite (no custom Project Notes editor) .......1")
    )
  )
  (COND
    ((MEMBER 3 PACKAGES)
     (PROMPT "\nCNM Pro (includes custom Project Notes editor) ..2")
    )
  )
  (INITGET "1 2")
  (SETQ
    STRING
     (GETKWORD "\n\nSelect a package
   [1/2]
   <1>:
   ")
  )
  ;;  "1" Full HawsEDC package
  ;;  "2" NOTES package standard
  ;;  "3" NOTES package with CNMEdit.exe
  ;;  "4" Full HawsEDC with CNMEdit.exe (CNM Pro plus HawsEDC tools)
  (SETQ
    STRING
     (COND
       ((= STRING "1") "1")
       ((= STRING "2") "4")
       (T "1")
     )
  )
;;;End user choice section
;;;=====================================================
;;;Begin Construction Notes Manager Pro only section
;;;  (PROMPT "\nGetting single seat authorization for Construction Notes Manager Pro.") ; prompt
;;;  (SETQ STRING "2")
;;;End Construction Notes Manager only section
  (LIST (1- (ATOI STRING)) 0)
)

;;GetBiosDate uses BIOSDATE.EXE to return system bios date as a
;;list in the form '(mm dd yy).
(DEFUN
   HAWS-GETBIOSDATE (/ BIOSDATEFULL X)
  (SETQ
    BIOSDATEFULL
     (COND
       (*HAWS-BIOSDATEFULL*)
       ;; Win 10
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\HARDWARE\\DESCRIPTION\\System\\BIOS"
          "BIOSReleaseDate"
        )
       )
       ;; Win NT 4.0 and Win 10
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\HARDWARE\\DESCRIPTION\\System"
          "SystemBiosDate"
        )
       )
       ;; Win 2000 and XP
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Biosinfo"
          "SystemBiosDate"
        )
       )
       ;; Win 95/98/Me
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\Enum\\Root\\*PNP0C01\\0000"
          "BIOSDate"
        )
       )
       (T "01\\01\\01")
     )
  )
  (SETQ *HAWS-BIOSDATEFULL* BIOSDATEFULL)
  (LIST
    (ATOI (SUBSTR BIOSDATEFULL 1 2))
    (ATOI (SUBSTR BIOSDATEFULL 4 2))
    (ATOI (SUBSTR BIOSDATEFULL 7 2))
  )
)

;;; GetComputerName gets the computer name from the registry if
;;; possible.
;;; If not, returns "".
(DEFUN
   HAWS-GETCOMPUTERNAME ()
  (SETQ
    *HAWS-COMPUTERNAME*
     (COND
       (*HAWS-COMPUTERNAME*)
       ((HAWS-REGISTRY-READ
          (STRCAT
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
(DEFUN
   HAWS-GETSHORTCOMPUTERNAME (/ COMPUTERNAME)
  (SETQ COMPUTERNAME (HAWS-GETCOMPUTERNAME))
  (STRCAT
    (SUBSTR COMPUTERNAME 1 1)
    (HAWS-ENDSTR COMPUTERNAME 1 1)
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
(DEFUN
   HAWS-LISTTOBINARY
   (ORDERLIST / BINARYDATE BINARYSALES BINARYSTRING I)
  ;;Pad BINARYSTRING from 68 to 72 bits to make 12 6-bit bytes.
  (SETQ
    BINARYDATE
     ;;Get the binary value of the AutoCAD date
     (HAWS-ENDSTR
       (STRCAT
         "0000000"
         (HAWS-CONVERTDECIMALTO (CAR ORDERLIST) 2)
       )
       22
       22
     )
    BINARYSALES
     ;;Get the binary value of the sales ID
     (HAWS-ENDSTR
       (STRCAT
         "0000000"
         (HAWS-CONVERTDECIMALTO (CADR ORDERLIST) 2)
       )
       4
       4
     )
    BINARYSTRING
     (STRCAT
       (SUBSTR BINARYSALES 1 1)
       (SUBSTR BINARYDATE 1 3)
       (SUBSTR BINARYSALES 2 1)
       (SUBSTR BINARYDATE 4 3)
       (SUBSTR BINARYSALES 3 1)
       (SUBSTR BINARYDATE 7 3)
       (SUBSTR BINARYSALES 4 1)
       (SUBSTR BINARYDATE 10)
       ;;Get the binary value of the package id #.
       (HAWS-ENDSTR
         (STRCAT
           "0000"
           (HAWS-CONVERTDECIMALTO (CADDR ORDERLIST) 2)
         )
         4
         4
       )
       ;;Get the binary value of the number of authorizations.
       (HAWS-ENDSTR
         (STRCAT
           "0000000000"
           (HAWS-CONVERTDECIMALTO (CADDDR ORDERLIST) 2)
         )
         10
         10
       )
     )
  )
  ;;Get the binary value of the computer name fragment
  (SETQ I 1)
  (REPEAT 2
    (SETQ
      BINARYSTRING
       (STRCAT
         BINARYSTRING
         (HAWS-ENDSTR
           (STRCAT
             "00000000"
             (HAWS-CONVERTDECIMALTO
               (ASCII (SUBSTR (NTH 4 ORDERLIST) I 1))
               2
             )
           )
           8
           8
         )
       )
    )
    (SETQ I (1+ I))
  )
  ;;Get the binary value of the bios date.
  (SETQ
    BINARYSTRING
     (STRCAT
       BINARYSTRING
       (HAWS-ENDSTR
         (STRCAT
           "0000"
           (HAWS-CONVERTDECIMALTO (NTH 5 ORDERLIST) 2)
         )
         4
         4
       )
       (HAWS-ENDSTR
         (STRCAT
           "00000"
           (HAWS-CONVERTDECIMALTO (NTH 6 ORDERLIST) 2)
         )
         5
         5
       )
       (HAWS-ENDSTR
         (STRCAT
           "0000000"
           (HAWS-CONVERTDECIMALTO (NTH 7 ORDERLIST) 2)
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
(DEFUN
   HAWS-BINARYTOLIST (BINARYSTRING / DEC I)
  (LIST
    ;;Get the AutoCAD date
    (HAWS-CONVERTTODECIMAL
      (STRCAT
        (SUBSTR BINARYSTRING 2 3)
        (SUBSTR BINARYSTRING 6 3)
        (SUBSTR BINARYSTRING 10 3)
        (SUBSTR BINARYSTRING 14 13)
      )
      2
    )
    ;;Get the sales id #.
    (HAWS-CONVERTTODECIMAL
      (STRCAT
        (SUBSTR BINARYSTRING 1 1)
        (SUBSTR BINARYSTRING 5 1)
        (SUBSTR BINARYSTRING 9 1)
        (SUBSTR BINARYSTRING 13 1)
      )
      2
    )
    ;;Get the package id #.
    (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 27 4) 2)
    ;;Get the number of authorizations.
    (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 31 10) 2)
    ;;Get the computer name fragment
    (STRCAT
      (CHR (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 41 8) 2))
      (CHR (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 49 8) 2))
    )
    ;;Get the bios month.
    (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 57 4) 2)
    ;;Get the bios day.
    (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 61 5) 2)
    ;;Get the bios year.
    (HAWS-CONVERTTODECIMAL (SUBSTR BINARYSTRING 66 7) 2)
  )
)

;;; ENCRYPTAUTHSTRING encrypts a padded binarystring
;;; for translation to a user authorization string
(DEFUN
   HAWS-ENCRYPTAUTHSTRING (S / SI SJ X)
  ;;Make list 8x9 of characters
  (REPEAT 8
    (REPEAT 9
      (SETQ
        SI (CONS (SUBSTR S 1 1) SI)
        S  (SUBSTR S 2)
      )
    )
    (SETQ
      SJ (CONS SI SJ)
      SI NIL
    )
  )
  (APPLY
    'STRCAT
    (MAPCAR
      '(LAMBDA (X) (APPLY 'STRCAT X))
      (APPLY 'MAPCAR (CONS 'LIST SJ))
    )
  )
)

;;; DECRYPTAUTHSTRING decrypts a padded authorization binarystring
;;; for translation to an order list
(DEFUN
   HAWS-DECRYPTAUTHSTRING (S / SI SJ)
  ;;Make list 9x8 of characters
  (REPEAT 9
    (REPEAT 8
      (SETQ
        SI (CONS (SUBSTR S 1 1) SI)
        S  (SUBSTR S 2)
      )
    )
    (SETQ
      SJ (CONS SI SJ)
      SI NIL
    )
  )
  (APPLY
    'STRCAT
    (MAPCAR
      '(LAMBDA (X) (APPLY 'STRCAT X))
      (APPLY 'MAPCAR (CONS 'LIST SJ))
    )
  )
)

;;; ENCRYPTORDERSTRING encrypts a padded binarystring
;;; for translation to a user order string
(DEFUN
   HAWS-ENCRYPTORDERSTRING (S / SI SJ)
  ;;Make list 12x6 of characters
  (REPEAT 12
    (REPEAT 6
      (SETQ
        SI (CONS (SUBSTR S 1 1) SI)
        S  (SUBSTR S 2)
      )
    )
    (SETQ
      SJ (CONS SI SJ)
      SI NIL
    )
  )
  (APPLY
    'STRCAT
    (MAPCAR
      '(LAMBDA (X) (APPLY 'STRCAT X))
      (APPLY 'MAPCAR (CONS 'LIST SJ))
    )
  )
)

;;; DECRYPTORDERSTRING decrypts a padded order binarystring
;;; for translation to an order list
(DEFUN
   HAWS-DECRYPTORDERSTRING (S / SI SJ)
  ;;Make list 6x12 of characters
  (REPEAT 6
    (REPEAT 12
      (SETQ
        SI (CONS (SUBSTR S 1 1) SI)
        S  (SUBSTR S 2)
      )
    )
    (SETQ
      SJ (CONS SI SJ)
      SI NIL
    )
  )
  (APPLY
    'STRCAT
    (MAPCAR
      '(LAMBDA (X) (APPLY 'STRCAT X))
      (APPLY 'MAPCAR (CONS 'LIST SJ))
    )
  )
)

;;BINARYTOUSER
;; translates a 68 character encrypted binary license string into a
;; 12 character user string for e-mailing from or to user.
;; The user string consists only of upper and lower case letters plus
;; @ and &. Eg.  aR3YjMmhi@57
(DEFUN
   HAWS-BINARYTOUSER (BINARYSTRING / DEC I USERSTRING)
  (SETQ
    USERSTRING ""
    I 0
  )
  (REPEAT 12
    (SETQ
      DEC
       (HAWS-CONVERTTODECIMAL
         (SUBSTR BINARYSTRING (1+ (* I 6)) 6)
         2
       )                                ; _
      USERSTRING
       (STRCAT
         USERSTRING
         (CHR
           (COND
             ((<= 0 DEC 25) (+ DEC 65))
             ((<= 26 DEC 51) (+ DEC 71))
             ((<= 52 DEC 61) (- DEC 4))
             ((= DEC 62) 38)
             ((= DEC 63) 64)
           )
         )
       )
      I (1+ I)
    )
  )
  USERSTRING
)

;; USERTOBINARY translates the 12-character user string generated by
;; BINARYTOUSER
;;into a 68 character (still encrypted) binary string as follows:
(DEFUN
   HAWS-USERTOBINARY (USERSTRING / BINARYSTRING DEC I TEMP)
  (SETQ
    BINARYSTRING ""
    I 0
  )
  (REPEAT 12
    (SETQ
      DEC          (ASCII (SUBSTR USERSTRING (SETQ I (1+ I)) 1))
      DEC          (COND
                     ((<= 65 DEC 90) (- DEC 65))
                     ((<= 97 DEC 122) (- DEC 71))
                     ((<= 48 DEC 57) (+ DEC 4))
                     ((= DEC 38) 62)
                     ((= DEC 64) 63)
                   )
      BINARYSTRING (STRCAT
                     BINARYSTRING
                     (HAWS-ENDSTR
                       (STRCAT "000000" (HAWS-CONVERTDECIMALTO DEC 2))
                       6
                       6
                     )
                   )
    )
  )
)


;;ConvertDecimalTo converts a decimal number to another integer base.
;;Returns a string
(DEFUN
   HAWS-CONVERTDECIMALTO (NUM NEWBASE / ATOMI DONE RETURNSTRING)
  (SETQ
    RETURNSTRING ""
    ATOMI 0
  )
  (WHILE (NOT DONE)
    (SETQ
      ATOMI
       (REM NUM NEWBASE)
      RETURNSTRING
       (STRCAT
         (IF (< ATOMI 10)
           (ITOA ATOMI)
           (CHR (+ ATOMI 87))
         )
         RETURNSTRING
       )
      NUM
       (/ NUM NEWBASE)
      DONE
       (= NUM 0)
    ) ;_ end of setq
  ) ;_ end of while
  RETURNSTRING
)
;;ConvertToDecimal converts a string representing a number in another base
;;to a decimal integer
(DEFUN
   HAWS-CONVERTTODECIMAL (STRING BASE / CHARI DECIMAL M)
  (SETQ
    DECIMAL 0
    M 1
  ) ;_ end of setq
  (WHILE (> (STRLEN STRING) 0)
    (SETQ
      CHARI
       (HAWS-ENDSTR STRING 1 1)
      DECIMAL
       (+ DECIMAL
          (* M
             (IF (< (ASCII CHARI) 58)
               (ATOI CHARI)
               (- (ASCII (STRCASE CHARI)) 55)
             )
          )
       )
      STRING
       (SUBSTR STRING 1 (1- (STRLEN STRING)))
      M (* M BASE)
    ) ;_ end of setq
  ) ;_ end of while
  DECIMAL
) ;_ end of defun

;;; Misc functions for developer and sales use.

;; ORDERTOAUTH
;; translates a user order string and a number of trial days
;; into a user authorization string
;; TRIALDAYS = -1000000 for unlimited authorization
(DEFUN
   HAWS-ORDERTOAUTH (ORDERSTRING TRIALDAYS / ORDERLIST)
  (SETQ
    ORDERLIST
     (HAWS-BINARYTOLIST
       (HAWS-DECRYPTORDERSTRING
         (HAWS-USERTOBINARY ORDERSTRING)
       )
     )
  )
  (HAWS-BINARYTOUSER
    (HAWS-ENCRYPTAUTHSTRING
      (HAWS-LISTTOBINARY
        (CONS (+ (CAR ORDERLIST) TRIALDAYS) (CDR ORDERLIST))
      )
    )
  )
)
;; ORDERTOTRIAL
;; translates a user order string into a timed trial
;; authorization
(DEFUN
   HAWS-ORDERTOTRIAL (ORDERSTRING TRIALDAYS / ORDERLIST)
  (HAWS-ORDERTOAUTH ORDERSTRING TRIALDAYS)
)
;; ORDERTOUNLIMITED
;; translates a user order string into an unlimited
;; authorization
(DEFUN
   HAWS-ORDERTOUNLIMITED (ORDERSTRING / ORDERLIST)
  (HAWS-ORDERTOAUTH ORDERSTRING -1000000)
)

;;ORDERTOLIST translates a user order string into an order list.
(DEFUN
   HAWS-ORDERTOLIST (ORDERSTRING)
  (HAWS-BINARYTOLIST
    (HAWS-DECRYPTORDERSTRING (HAWS-USERTOBINARY ORDERSTRING))
  )
)
;; AUTHTOLIST translates a user authorization string into an order
;; list.
(DEFUN
   HAWS-AUTHTOLIST (AUTHSTRING)
  (HAWS-BINARYTOLIST
    (HAWS-DECRYPTAUTHSTRING (HAWS-USERTOBINARY AUTHSTRING))
  )
)
;;LISTTOORDER translates a an order list into a user order string.
(DEFUN
   HAWS-LISTTOORDER (ORDERLIST)
  (HAWS-BINARYTOUSER
    (HAWS-ENCRYPTORDERSTRING (HAWS-LISTTOBINARY ORDERLIST))
  )
)
;; LISTTOAUTH translates a an order list into a user authorization
;; string.
(DEFUN
   HAWS-LISTTOAUTH (AUTHLIST)
  (HAWS-BINARYTOUSER
    (HAWS-ENCRYPTAUTHSTRING (HAWS-LISTTOBINARY AUTHLIST))
  )
)

;;; ======================================================================
;;;
;;;                 Usage logging functions
;;;
;;; ======================================================================
;;; This list should be populated automatically from the web on load.
;;; Stored locally somewhere. Maybe a LSP file.
;;; id appgroup name
(setq
  *HAWS-EDCCOMMANDS*
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
     (335 2 "hcnm-notesedit")
     (336 1 "hcnm-cnmqt")
    )
)

(DEFUN HAWS-USE-LOCAL-LOCATION ()
  (list "HawsEDC" "UseLog" "UseString")
)

(DEFUN HAWS-USE-GET-LOCAL-LOG-STRING ()
  (COND
    ((HAWS-READCFG (HAWS-USE-LOCAL-LOCATION)))
    ("")
  )
)

(DEFUN HAWS-USE-LOG-LOCAL (COMMAND-ID / LOG-STRING)
  (HAWS-WRITECFG
    (HAWS-USE-LOCAL-LOCATION)
    (HAWS-USE-COMMAND-ID-TO-LOG-STRING COMMAND-ID (HAWS-USE-GET-LOCAL-LOG-STRING))
  )
)

(DEFUN HAWS-USE-INITIALIZE-LOG-STRING ( / I MAX-ID)
  (SETQ I -1 LOG-STRING "" MAX-ID (CAAR (REVERSE *HAWS-EDCCOMMANDS*))) (WHILE (< (SETQ I (1+ I)) MAX-ID) (SETQ LOG-STRING (STRCAT LOG-STRING (CHR 1))))
  LOG-STRING
)

(DEFUN HAWS-USE-COMMAND-ID-TO-LOG-STRING (COMMAND-ID LOG-STRING / MAX-ID)
  (COND
    ((OR (NOT LOG-STRING) (= LOG-STRING ""))
     (HAWS-USE-INITIALIZE-LOG-STRING)
    )
  )
  (SETQ LOG-STRING (STRCAT (SUBSTR LOG-STRING 1 COMMAND-ID) (CHR (1+ (ASCII (SUBSTR LOG-STRING (1+ COMMAND-ID) 1)))) (SUBSTR LOG-STRING (+ COMMAND-ID 2))))
)

(DEFUN
   HAWS-USE-LOG-REMOTE (/ URL HTTP BIOS-DATE LOG-DATA)
  (SETQ
    URL  "http://www.constructionnotesmanager.com/cnm_log.php"
    HTTP (VLAX-CREATE-OBJECT "MSXML2.XMLHTTP")
    BIOS-DATE (HAWS-GETBIOSDATE)
    LOG-DATA
     (STRCAT
       "computer_name="
       (HAWS-GETCOMPUTERNAME)
       "&bios_date="
       *HAWS-BIOSDATEFULL*
       "&cnm_version="
       (HAWS-UNIFIED-VERSION)
       "&command_log="
       (HAWS-USE-GET-LOCAL-LOG-STRING)
     )
  )
  (VLAX-INVOKE-METHOD HTTP 'OPEN "post" URL :VLAX-TRUE)
  (VLAX-INVOKE-METHOD HTTP 'setRequestHeader "Content-type" "application/x-www-form-urlencoded")
  (COND
    ((VL-CATCH-ALL-ERROR-P
       (VL-CATCH-ALL-APPLY 'VLAX-INVOKE (LIST HTTP 'SEND LOG-DATA))
     )
     (PRINC (STRCAT "\nInvalid request: " URL))
    )
    (T (HAWS-WRITECFG (HAWS-USE-LOCAL-LOCATION) (HAWS-USE-INITIALIZE-LOG-STRING)))
  )
  (VLAX-RELEASE-OBJECT HTTP)
  (princ)
)

;;; ======================================================================
;;;
;;;                 Miscellaneous Utility functions
;;;
;;; ======================================================================

;; Atofx extracts a real number from a text string when text before
;; or
;; after
;;the number matches a give wild card spec.  Requires EXTRACTX.
;;Type 0 tries to match the wild cards with text preceding a number.
;;Type 1 tries to match the wild cards with text following a number
;;Returns 0.0 if search unsuccesful
(VL-ACAD-DEFUN 'HAWS-ATOFX)
(DEFUN
   HAWS-ATOFX (S WC OPT / X)
  (SETQ X (CADR (HAWS-EXTRACTX S WC OPT)))
  (IF X
    (ATOF X)
    0.0
  )
)


;;;
;;; c:haws-icad-p
;;;
;;;Tests whether intellicad behavior is current.
;;;Bricscad has advanced to the point we no longer have to use a special mode for it.
(VL-ACAD-DEFUN 'C:HAWS-ICAD-P)
(DEFUN
   C:HAWS-ICAD-P ()
  (OR *HAWS-ICADMODE*
      (SETQ *HAWS-ICADMODE* (WCMATCH (GETVAR "acadver") "*i"))
  )
)

;;; Distofx extracts a real number from a text string when text before
;;; or
;;; after
;;the number matches a give wild card spec.  Requires EXTRACTX.
;;Type 0 tries to match the wild cards with text preceding a number.
;;Type 1 tries to match the wild cards with text following a number
;;Returns nil if search unsuccesful
(VL-ACAD-DEFUN 'HAWS-DISTOFX)
(DEFUN
   HAWS-DISTOFX (S WC OPT / X)
  (SETQ X (CADR (HAWS-EXTRACTX S WC OPT)))
  (IF X
    (DISTOF X)
    NIL
  )
)

(DEFUN HAWS-DXF (GCODE ENTLST) (CDR (ASSOC GCODE ENTLST)))

;; Endstr returns a substring of s starting with the ith to last
;; character
;;and continuing l characters.
(VL-ACAD-DEFUN 'HAWS-ENDSTR)
(DEFUN
   HAWS-ENDSTR (S I L)
  (SUBSTR S (1+ (- (MAX (STRLEN S) 1) I)) L)
)

;;Extract used to extract numerical info from a text string.
;;Ignores commas in numbers.  Not international compatible.
(VL-ACAD-DEFUN 'HAWS-EXTRACT)
(DEFUN
   HAWS-EXTRACT (S / C I PREFIX NUMBER SUFFIX)
  (SETQ
    I 0
    PREFIX ""
    NUMBER ""
    SUFFIX ""
  )
  (REPEAT (STRLEN S)
    (SETQ C (SUBSTR S (SETQ I (1+ I)) 1))
    (COND
      ((AND (WCMATCH C "#") (EQ SUFFIX ""))
       (SETQ NUMBER (STRCAT NUMBER C))
      )
      ((AND
         (EQ C "-")
         (= SUFFIX NUMBER "")
         (WCMATCH (SUBSTR S (1+ I) 1) "#")
       )
       (SETQ NUMBER (STRCAT NUMBER C))
      )
      ((AND
         (EQ C ".")
         (= SUFFIX "")
         (WCMATCH (SUBSTR S (1+ I) 1) "#")
       )
       (SETQ NUMBER (STRCAT NUMBER C))
      )
      ;;Swallow commas inside numbers.  Not int'l compatible.
      ((AND
         (EQ C ",")
         (= SUFFIX "")
         (WCMATCH (SUBSTR S (1+ I) 1) "#")
       )
      )
      ((EQ NUMBER "") (SETQ PREFIX (STRCAT PREFIX C)))
      (T (SETQ SUFFIX (STRCAT SUFFIX C)))
    )
  )
  (LIST PREFIX NUMBER SUFFIX)
)

;; Extractx used to extract numerical info from a text string with
;; extended
;; options.
(VL-ACAD-DEFUN 'HAWS-EXTRACTX)
(DEFUN
   HAWS-EXTRACTX (S WC OPT / C DONE I PRE PREI NUMBER SUF SUFI)
  (SETQ
    I (IF (= OPT 0)
        0
        (1+ (STRLEN S))
      )
    PRE ""
    NUMBER ""
    SUF ""
  )
  (REPEAT (STRLEN S)
    (SETQ
      C    (SUBSTR
             S
             (SETQ
               I (IF (= OPT 0)
                   (1+ I)
                   (1- I)
                 )
             )
             1
           )
      PREI (SUBSTR S 1 (1- I))
      SUFI (SUBSTR S (1+ I))
    )
    (COND
      ((NOT
         (WCMATCH
           (IF (= OPT 0)
             PREI
             SUFI
           )
           WC
         )
       )
       (IF (= OPT 0)
         (SETQ PRE (STRCAT PRE C))
         (SETQ SUF (STRCAT C SUF))
       )
      )
      ((AND (WCMATCH C "#") (NOT DONE))
       (SETQ
         NUMBER
          (IF (= OPT 0)
            (STRCAT NUMBER C)
            (STRCAT C NUMBER)
          )
       )
      )
      ((AND
         (EQ C "-")
         (= NUMBER "")
         (NOT DONE)
         (WCMATCH (SUBSTR S (1+ I) 1) "#")
       )
       (SETQ
         NUMBER
          (IF (= OPT 0)
            (STRCAT NUMBER C)
            (STRCAT C NUMBER)
          )
       )
      )
      ((AND
         (EQ C ".")
         (NOT DONE)
         (WCMATCH (SUBSTR S (1+ I) 1) "#")
       )
       (SETQ
         NUMBER
          (IF (= OPT 0)
            (STRCAT NUMBER C)
            (STRCAT C NUMBER)
          )
       )
      )
      ((EQ NUMBER "")
       (IF (= OPT 0)
         (SETQ PRE (STRCAT PRE C))
         (SETQ SUF (STRCAT C SUF))
       )
      )
      (T
       (SETQ DONE T)
       (IF (= OPT 0)
         (SETQ SUF (STRCAT SUF C))
         (SETQ PRE (STRCAT C PRE))
       )
      )
    )
  )
  (IF (NOT (ZEROP (STRLEN NUMBER)))
    (LIST PRE NUMBER SUF)
  )
)

;;
;; HAWS-FILE-COPY
;;
;; Intellicad substitute for VL-FILE-COPY
;;
;; CAUTION: May not return the same value as vl-file-copy.
;;
(VL-ACAD-DEFUN 'HAWS-FILE-COPY)
(DEFUN
   HAWS-FILE-COPY (SOURCE DESTINATION / RDLIN RETURN)
  (SETQ RETURN T)
  (COND
    ((AND (= (SUBSTR (GETVAR "DWGNAME") 1 7) "Drawing") (WCMATCH DESTINATION (STRCAT (GETVAR "DWGPREFIX") "*")) (WCMATCH (getvar "ACADVER") "*BricsCAD"))
      (ALERT "BricsCAD may crash if this drawing is not in a writable folder.")
      (INITGET "Yes No")
      (IF (/= (GETKWORD "\nBricsCAD may crash. Continue anyway? [Yes/No] <No>: ") "Yes")(exit))
    )
  )
  (COND
    ((HAWS-VLISP-P) 
     (VL-FILE-COPY SOURCE DESTINATION))
    (T
     (IF (NOT(SETQ F1 (OPEN SOURCE "r")))
       (SETQ RETURN nil)
     )
     (IF (NOT (AND RETURN (SETQ F2 (OPEN DESTINATION "w"))))
       (SETQ RETURN nil)
     )
     (WHILE (SETQ RDLIN (READ-LINE F1)) (WRITE-LINE RDLIN F2))
     (SETQ F1 (CLOSE F1))
     (SETQ F2 (CLOSE F2))
     RETURN
    )
  )
)


;;
;;HAWS-FILENAME-BASE
;;
;; Intellicad substitute for vl-filename-base.
;;
(VL-ACAD-DEFUN 'HAWS-FILENAME-BASE)
(DEFUN
   HAWS-FILENAME-BASE (FILENAME / BASE)
  (COND
    ((HAWS-VLISP-P) (VL-FILENAME-BASE FILENAME))
    (T
     ;;Trim off the directory.
     (SETQ BASE (STRCAT " " FILENAME))  ;Pad with a space
     (WHILE (WCMATCH
              (SETQ
                BASE
                 (SUBSTR BASE 2)        ;Trim first character.
              )
              "*[\\/]*"                 ; and do again if slashes in name.
            )
     )
     ;;Trim the extension off one character at a time
     (IF (WCMATCH BASE "*`.*")          ; If there are any dots in remaining name.
       (PROGN
         (WHILE (/= (HAWS-ENDSTR BASE 1 1) ".")
                                        ; While the last character isn't a dot.
           (SETQ
             BASE
              (SUBSTR BASE 1 (1- (STRLEN BASE))) ;Trim it.
           )
         )
         (SUBSTR BASE 1 (1- (STRLEN BASE)))
       )
       BASE
     )
    )
  )
)


;;
;;HAWS-FILENAME-DIRECTORY
;;
;; Intellicad substitute for vl-filename-directory. 
;;
(VL-ACAD-DEFUN 'HAWS-FILENAME-DIRECTORY)
(DEFUN
   HAWS-FILENAME-DIRECTORY (FILENAME / DIRECTORY)
  (COND
    ((HAWS-VLISP-P) (VL-FILENAME-DIRECTORY FILENAME))
    (T
     (COND
       ((WCMATCH FILENAME "*[\\/]*")    ;If file has any directories.
        (SETQ DIRECTORY (STRCAT FILENAME " "))
        (WHILE (NOT
                 (WCMATCH
                   (SETQ
                     DIRECTORY
                      (SUBSTR DIRECTORY 1 (1- (STRLEN DIRECTORY)))
                                        ;Trim last character
                   )
                   "*[\\/]"             ; and do again if the last character is not a slash.
                 )
               )
        )
        (SUBSTR DIRECTORY 1 (1- (STRLEN DIRECTORY))) ;Trim slash
       )
       (T "")
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
(VL-ACAD-DEFUN 'HAWS-FILENAME-EXTENSION)
(DEFUN
   HAWS-FILENAME-EXTENSION (FILENAME / EXTENSION)
  (COND
    ((AND
       (HAWS-VLISP-P)
       (SETQ EXTENSION (VL-FILENAME-EXTENSION FILENAME))
     )
     EXTENSION
    )
    ;;Trim off the directory.
    (T
     (SETQ EXTENSION (STRCAT " " FILENAME)) ;Pad with a space
     (WHILE (WCMATCH
              (SETQ
                EXTENSION
                 (SUBSTR EXTENSION 2)   ;Trim first character.
              )
              "*[\\/]*"                 ; and do again if slashes in name.
            )
     )
     ;;Trim off the base name.
     (IF (WCMATCH EXTENSION "*`.*")     ; If there are any dots in remaining name.
       (PROGN
         (SETQ EXTENSION (STRCAT " " EXTENSION))
         (WHILE (WCMATCH
                  (SETQ
                    EXTENSION
                     (SUBSTR EXTENSION 2) ;Trim first character
                  )
                  "*`.*"                ; and do again if there is still a dot.
                )
         )
         EXTENSION
       )
       ""
     )
    )
  )
)
;;;  HAWS-FLATTEN
(VL-ACAD-DEFUN 'HAWS-FLATTEN)
(DEFUN
   HAWS-FLATTEN (PNT)
;;;Returns flattened coordinates of a 3d point
  (LIST (CAR PNT) (CADR PNT) 0.0)
)

(DEFUN
   HAWS-GETANGLEX (GX-STARTINGPOINT GX-PROMPT GX-DEFAULTVALUE
                   GX-INITIALVALUE / GX-INPUT
                  )
  (SETQ
    GX-DEFAULTVALUE
     (COND
       (GX-DEFAULTVALUE)
       (GX-INITIALVALUE)
       (0.0)
     )
  )
  (COND
    ((AND
       GX-STARTINGPOINT
       (SETQ
         GX-INPUT
          (GETANGLE
            GX-STARTINGPOINT
            (STRCAT
              "\n"
              GX-PROMPT
              " <"
              (ANGTOS GX-DEFAULTVALUE)
              ">: "
            )
          )
       )
     )
     GX-INPUT
    )
    ((AND
       (NOT GX-STARTINGPOINT)
       (SETQ
         GX-INPUT
          (GETANGLE
            (STRCAT
              "\n"
              GX-PROMPT
              " <"
              (ANGTOS GX-DEFAULTVALUE)
              ">: "
            )
          )
       )
     )
     GX-INPUT
    )
    (GX-DEFAULTVALUE)
  )
)

;;;HAWS-GETDISTX
;;;Returns a distance
(VL-ACAD-DEFUN 'HAWS-GETDISTX)
(DEFUN
   HAWS-GETDISTX (GX-POINT1 GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE / GX-ARCMODE)
  (CAR
    (HAWS-GETDISTPOINT
      GX-POINT1
      GX-PROMPT
      GX-DEFAULTVALUE
      GX-INITIALVALUE
      (SETQ GX-ARCMODE-P NIL)
    )
  )
)
;;;HAWS-GETDISTPOINT
;;;Returns a distance, the endpoint of the distance, and the bulge used for the distance.
;;;'(distance endpoint bulge)
(VL-ACAD-DEFUN 'HAWS-GETDISTPOINT)
(DEFUN
   HAWS-GETDISTPOINT (GX-POINT1 GX-PROMPT GX-DEFAULTVALUE
                      GX-INITIALVALUE GX-ARCMODE-P / GX-POINT2 GX-POINT3 GX-BULGE
                      GX-DISTANCE
                     )
  (SETQ
    GX-DEFAULTVALUE
     (COND
       (GX-DEFAULTVALUE)
       (GX-INITIALVALUE)
     )
  )
  ;;If starting point wasn't provided, get it.
  (COND
    ((NOT GX-POINT1)
     (SETQ GX-POINT1 (GETPOINT "\nSpecify first point: "))
    )
  )
  ;;If there is now a starting point, get the second point or Arc keyword.
  (COND
    (GX-POINT1
     (INITGET "Arc")
     (SETQ
       GX-POINT2
        (GETPOINT
          GX-POINT1
          (STRCAT
            "\n"
            GX-PROMPT
            (COND (ARCMODE-P " [Arc]") (T ""))
            (IF GX-DEFAULTVALUE
              (STRCAT " <" (RTOS GX-DEFAULTVALUE) ">")
              ""
            )
            ": "
          )
        )
     )
    )
  )
  (SETQ
    GX-DISTANCE
     (COND
       ;;If
       (;;point2 was not entered
        (NOT GX-POINT2)
        ;;then return the default value.
        GX-DEFAULTVALUE
       )
       ;;Else if point2 isn't "Arc"
       ((OR (NOT ARCMODE-P) (/= "Arc" GX-POINT2))
        ;;then return the distance between point1 and point2
        (DISTANCE GX-POINT1 GX-POINT2)
       )
       ;;Else enter arc mode.
       (T
        ;;Prompt for the second and third arc points
        (SETQ
          GX-POINT2
           (GETPOINT
             (STRCAT "\nSpecify point on arc: ")
           )
          GX-POINT3
           (GETPOINT
             (STRCAT "\nSpecify end point of arc: ")
           )
        )
        (COND
          ;;If the second and third arc point were provided, then
          ((AND GX-POINT2 GX-POINT3)
           ;;Return the length of the arc.
           (HAWS-SEGMENT-LENGTH
             GX-POINT1
             GX-POINT3
             (SETQ
               GX-BULGE
                (HAWS-3PTTOBULGE
                  (HAWS-FLATTEN GX-POINT1)
                  (HAWS-FLATTEN GX-POINT2)
                  (HAWS-FLATTEN GX-POINT3)
                )
             )
           )
          )
        )
       )
     )
  )
  (LIST
    GX-DISTANCE
    (COND
      (GX-POINT3)
      (GX-POINT2)
    )
    GX-BULGE
  )
)

(VL-ACAD-DEFUN 'HAWS-GETDN)
(DEFUN
   HAWS-GETDN (/ DN)
  (SETQ DN (GETVAR "dwgname"))
  (IF (WCMATCH (STRCASE DN) "*`.DWG")
    (SETQ DN (SUBSTR DN 1 (- (STRLEN DN) 4)))
    (IF (WCMATCH DN "*\\*")
      (SETQ DN (SUBSTR DN (1+ (STRLEN (GETVAR "dwgprefix")))))
    )
  )
  DN
)

(VL-ACAD-DEFUN 'HAWS-GETDNPATH)
(DEFUN
   HAWS-GETDNPATH (/ DNPATH)
  (SETQ DNPATH (GETVAR "dwgname"))
  (IF (WCMATCH (STRCASE DNPATH) "*`.DWG")
    (SETQ
      DNPATH
       (STRCAT (GETVAR "dwgprefix") DNPATH)
      DNPATH
       (SUBSTR DNPATH 1 (- (STRLEN DNPATH) 4))
    )
  )
  DNPATH
)

(DEFUN
   HAWS-GETFIL (FPRMPT FDFLT FTYPE FEXT / FNAME FNINP)
  (WHILE (NOT F1)
    (SETQ
      FNINP
       (HAWS-GETSTRINGX FPRMPT FNINP FDFLT)
      FNAME
       (STRCAT FNINP "." FEXT)
    )
    (COND
      ((AND (= (STRCASE FTYPE) "W") (FINDFILE FNAME))
       (INITGET "Yes No")
       (IF (= (GETKWORD "File already exists.  Overwrite? [Y/N]:")
              "Yes"
           )
         (SETQ F1 (OPEN FNAME FTYPE))
       )
      )
      (T (SETQ F1 (OPEN FNAME FTYPE)))
    )
    (IF (NOT F1)
      (PROMPT
        (STRCAT "Invalid path or filename.  Please try again.\n")
      )
    )
  )
  (LIST F1 FNAME)
)

;;HAWS-GETINTX
;;Provided for legacy compatability and user experience.
(VL-ACAD-DEFUN 'HAWS-GETINTX)
(DEFUN
   HAWS-GETINTX (GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE)
  (HAWS-GETINTXX GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE 0)
)
;;;HAWS-GETINTXX
;;;Extended (getint) with default value and drawing text selection
;;;Three modes:
;;;1. If a default or initial value is supplied, GETINTX prompts with it and allows user to enter Select from drawing text mode.
;;;2. If no default is supplied and MODE is 0, the first prompt is for standard input, with fallback to selecting value from drawing text.
;;;3. If no default is supplied and MODE is 1, the first prompt is for drawing text selection, with fallback to standard input.
;;;Returns an INT or nil if nothing provided.
(VL-ACAD-DEFUN 'HAWS-GETINTXX)
(DEFUN
   HAWS-GETINTXX (GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE
                  GX-PROMPTMODE / GX-RESPONSE
                 )
  ;;Log all calls to this function.
  (HAWS-LOG
    (STRCAT
      "HAWS-GETINTX GX-PROMPT="
      GX-PROMPT
      " GX-DEFAULT="
      (IF GX-DEFAULTVALUE
        (ITOA GX-DEFAULTVALUE)
        "nil"
      )
      " GX-INITIALVALUE="
      (IF GX-INITIALVALUE
        (ITOA GX-INITIALVALUE)
        "nil"
      )
    )
  )
  (SETQ
    GX-DEFAULTVALUE
     (COND
       (GX-DEFAULTVALUE)
       (GX-INITIALVALUE)
     )
  )
  ;;First prompt
  (COND
    ;;If a default value was supplied, prompt with it and allow user to enter Select from drawing text mode.
    (GX-DEFAULTVALUE
     (INITGET "Select")
     (SETQ
       GX-RESPONSE
        (GETINT
          (STRCAT
            "\n"
            GX-PROMPT
            " or [Select from drawing] <"
            (ITOA GX-DEFAULTVALUE)
            ">: "
          )
        )
     )
    )
    ;;Else if mode is 0, prompt for standard input
    ((= GX-PROMPTMODE 0)
     (SETQ
       GX-RESPONSE
        (GETINT
          (STRCAT
            "\n"
            GX-PROMPT
            " or <Select from drawing>: "
          )
        )
     )
    )
    ;;Else if mode is 1, prompt for object select
    ((= GX-PROMPTMODE 1)
     (SETQ
       GX-RESPONSE
        (NENTSEL
          (STRCAT
            "\nSelect object with "
            GX-PROMPT
            " or <enter manually>: "
          )
        )
     )
    )
  )
  ;;Second prompt if necessary
  (COND
    ;;If
    ((AND
       ;;no response
       (NOT GX-RESPONSE)
       ;;and there's a default value,
       GX-DEFAULTVALUE
     )
     ;;No second prompt
     NIL
    )
    ;;Else if
    ((OR (AND
           ;;no response
           (NOT GX-RESPONSE)
           ;;and mode is 0,
           (= GX-PROMPTMODE 0)
         )
         ;;or response was Select
         (= GX-RESPONSE "Select")
     )
     ;;Prompt for object select
     (SETQ
       GX-RESPONSE
        (NENTSEL
          (STRCAT "\nSelect object with " GX-PROMPT ": ")
        )
     )
    )
    ;;Else if
    ((AND
       ;; no response
       (NOT GX-RESPONSE)
       ;;and mode is 1,
       (= GX-PROMPTMODE 1)
     )
     ;;Prompt for standard input
     (SETQ GX-RESPONSE (GETINT (STRCAT "\n" GX-PROMPT ": ")))
    )
  )
  ;;Return the integer if provided
  (COND
    ;;If
    ((AND
       ;;there's a response
       GX-RESPONSE
       ;;and it's an integer,
       (= (TYPE GX-RESPONSE) 'INT)
     )
     ;;Then return it
     GX-RESPONSE
    )
    ;;Else if
    ((AND
       ;;there's a response
       GX-RESPONSE
       ;;and it's an entsel,
       (= (TYPE GX-RESPONSE) 'LIST)
     )
     ;;Then return it
     ;;Then convert it to an integer
     (ATOI
       (CADR
         (HAWS-EXTRACT (CDR (ASSOC 1 (ENTGET (CAR GX-RESPONSE)))))
       )
     )
    )
    ;;Else
    (T
     ;;Return the default
     GX-DEFAULTVALUE
    )
  )
)

;;HAWS-GETREALX
;;Provided for legacy compatability and user experience.
(VL-ACAD-DEFUN 'HAWS-GETREALX)
(DEFUN
   HAWS-GETREALX (GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE)
  (HAWS-GETREALXX GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE 0)
)
;;;HAWS-GETREALXX
;;;Extended (getreal) with default value and drawing text selection
;;;Three modes:
;;;1. If a default or initial value is supplied, GETREALX prompts with it and allows user to enter Select from drawing text mode.
;;;2. If no default is supplied and MODE is 0, the first prompt is for standard input, with fallback to selecting value from drawing text.
;;;3. If no default is supplied and MODE is 1, the first prompt is for drawing text selection, with fallback to standard input.
;;;Returns an REAL or nil if nothing provided.
(VL-ACAD-DEFUN 'HAWS-GETREALXX)
(DEFUN
   HAWS-GETREALXX (GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE
                   GX-PROMPTMODE / GX-RESPONSE
                  )
  ;;Log all calls to this function.
  (HAWS-LOG
    (STRCAT
      "HAWS-GETREALX GX-PROMPT="
      GX-PROMPT
      " GX-DEFAULT="
      (IF GX-DEFAULTVALUE
        (RTOS GX-DEFAULTVALUE 2)
        "nil"
      )
      " GX-INITIALVALUE="
      (IF GX-INITIALVALUE
        (RTOS GX-INITIALVALUE 2)
        "nil"
      )
    )
  )
  (SETQ
    GX-DEFAULTVALUE
     (COND
       (GX-DEFAULTVALUE)
       (GX-INITIALVALUE)
     )
  )
  ;;First prompt
  (COND
    ;;If a default value was supplied, prompt with it and allow user to enter Select from drawing text mode.
    (GX-DEFAULTVALUE
     (INITGET "Select")
     (SETQ
       GX-RESPONSE
        (GETREAL
          (STRCAT
            "\n"
            GX-PROMPT
            " or [Select from drawing] <"
            (RTOS GX-DEFAULTVALUE 2)
            ">: "
          )
        )
     )
    )
    ;;Else if mode is 0, prompt for standard input
    ((= GX-PROMPTMODE 0)
     (SETQ
       GX-RESPONSE
        (GETREAL
          (STRCAT
            "\n"
            GX-PROMPT
            " or <Select from drawing>: "
          )
        )
     )
    )
    ;;Else if mode is 1, prompt for object select
    ((= GX-PROMPTMODE 1)
     (SETQ
       GX-RESPONSE
        (NENTSEL
          (STRCAT
            "\nSelect object with "
            GX-PROMPT
            " or <enter manually>: "
          )
        )
     )
    )
  )
  ;;Second prompt if necessary
  (COND
    ;;If
    ((AND
       ;;no response
       (NOT GX-RESPONSE)
       ;;and there's a default value,
       GX-DEFAULTVALUE
     )
     ;;No second prompt
     NIL
    )
    ;;Else if
    ((OR (AND
           ;;no response
           (NOT GX-RESPONSE)
           ;;and mode is 0,
           (= GX-PROMPTMODE 0)
         )
         ;;or response was Select
         (= GX-RESPONSE "Select")
     )
     ;;Prompt for object select
     (SETQ
       GX-RESPONSE
        (NENTSEL
          (STRCAT "\nSelect object with " GX-PROMPT ": ")
        )
     )
    )
    ;;Else if
    ((AND
       ;; no response
       (NOT GX-RESPONSE)
       ;;and mode is 1,
       (= GX-PROMPTMODE 1)
     )
     ;;Prompt for standard input
     (SETQ GX-RESPONSE (GETREAL (STRCAT "\n" GX-PROMPT ": ")))
    )
  )
  ;;Return the real number if provided
  (COND
    ;;If
    ((AND
       ;;there's a response
       GX-RESPONSE
       ;;and it's an integer,
       (= (TYPE GX-RESPONSE) 'REAL)
     )
     ;;Then return it
     GX-RESPONSE
    )
    ;;Else if
    ((AND
       ;;there's a response
       GX-RESPONSE
       ;;and it's an entsel,
       (= (TYPE GX-RESPONSE) 'LIST)
     )
     ;;Then return it
     ;;Then convert it to an real
     (ATOF
       (CADR
         (HAWS-EXTRACT (CDR (ASSOC 1 (ENTGET (CAR GX-RESPONSE)))))
       )
     )
    )
    ;;Else
    (T
     ;;Return the default
     GX-DEFAULTVALUE
    )
  )
)

;;;HAWSGETPOINTX
(VL-ACAD-DEFUN 'HAWS-GETPOINTX)
(DEFUN
   HAWS-GETPOINTX (GX-STARTINGPOINT GX-PROMPT GX-DEFAULTVALUE
                   GX-INITIALVALUE / GX-INPUT
                  )
  (SETQ
    GX-DEFAULTVALUE
     (COND
       (GX-DEFAULTVALUE)
       (GX-INITIALVALUE)
       ('(0.0 0.0 0.0))
     )
  )
  (SETQ
    GX-PROMPT
     (STRCAT
       GX-PROMPT
       " <"
       (RTOS (CAR GX-DEFAULTVALUE))
       ","
       (RTOS (CADR GX-DEFAULTVALUE))
       ","
       (RTOS (CADDR GX-DEFAULTVALUE))
       ">: "
     )
  )
  (SETQ
    GX-INPUT
     (IF GX-STARTINGPOINT
       (GETPOINT GX-STARTINGPOINT GX-PROMPT)
       (GETPOINT GX-PROMPT)
     )
  )
  (IF (NOT GX-INPUT)
    GX-DEFAULTVALUE
    GX-INPUT
  )
)

(VL-ACAD-DEFUN 'HAWS-GETSTRINGX)
(DEFUN
   HAWS-GETSTRINGX
   (GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE / GX-INPUT)
  (SETQ
    GX-DEFAULTVALUE
     (COND
       (GX-DEFAULTVALUE)
       (GX-INITIALVALUE)
       ("")
     )
  )
  (COND
    ((/= ""
         (SETQ
           GX-INPUT
            (GETSTRING
              (STRCAT "\n" GX-PROMPT " <" GX-DEFAULTVALUE ">: ")
            )
         )
     )
     GX-INPUT
    )
    (GX-DEFAULTVALUE)
  )
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
(VL-ACAD-DEFUN 'HAWS-LSTTOSTR)

(DEFUN
   HAWS-LSTTOSTR (INPUTLIST FIELDSEPARATOR TEXTDELIMITER / CURRENTFIELD
                  OUTPUTSTRING
                 )
  ;;Initialize values of variables
  (SETQ OUTPUTSTRING "")
  ;;Step through list making each element into a string field
  (WHILE (SETQ CURRENTFIELD (CAR INPUTLIST))
    (COND
      ((= (TYPE CURRENTFIELD) 'STR)
       ;;Alert that text delimiter isn't working yet.
       (IF (WCMATCH CURRENTFIELD (STRCAT "*" TEXTDELIMITER "*"))
         (ALERT
           "Text delimiter processing in HAWS-LstToStr isn't implemented yet."
         )
       )
       (SETQ
         OUTPUTSTRING
          (STRCAT OUTPUTSTRING FIELDSEPARATOR CURRENTFIELD)
       )
      )
    )
    (SETQ INPUTLIST (CDR INPUTLIST))
  )
  ;;Remove gratuitous first delimiter
  (IF (/= OUTPUTSTRING "")
    (SETQ
      OUTPUTSTRING
       (SUBSTR OUTPUTSTRING (1+ (STRLEN FIELDSEPARATOR)))
    )
  )
  OUTPUTSTRING
)

;;; HAWS-LOAD-FROM-APP-DIR
;;; loads a vlx, fas, or lsp, in that preferred order (AutoLISP
;;; default),
;;; from the folder that contains cnm.mnl
;;;
(DEFUN
   C:HAWS-LOAD-FROM-APP-DIR (FILENAME / FILE-PATH)
  (PRINC "\nLoading ")
  (COND
    ((VL-CATCH-ALL-ERROR-P
       (VL-CATCH-ALL-APPLY
         'LOAD
         (LIST (PRINC (STRCAT (C:HCNM-CONFIG-GETVAR "AppFolder") "\\" FILENAME)))
       )
     )
     (PRINC
       " ... not found in app folder. Searching in support files search path."
     )
     (LOAD FILENAME)
    )
  )
  (PRINC)
)

;;; HAWS-LOG
;;; Writes a message to a log file including the username and timestamp
(VL-ACAD-DEFUN 'HAWS-LOG)
(DEFUN
   HAWS-LOG (MESSAGE)
            ;|
  (SETQ
    F1 (OPEN
         (STRCAT
           (HAWS-FILENAME-DIRECTORY (FINDFILE "hawsedc.mnu"))
           "\\haws-log.txt"
         )
         "a"
       )
  )
  (WRITE-LINE
    (STRCAT
      (HAWS-GETCOMPUTERNAME)
      " - "
      (RTOS (GETVAR "cdate") 2 6)
      " - "
      MESSAGE
    )
    F1
  )
  (SETQ F1 (CLOSE F1))
  |;
  (PRINC)
)
;;; HAWS-MILEPOST
;;; Echos a progress message depending on debug level.
(VL-ACAD-DEFUN 'HAWS-MILEPOST)
(DEFUN
   HAWS-MILEPOST (MESSAGESTRING)
  (IF (> *HAWS-DEBUGLEVEL* 0)
    (PRINC (STRCAT "\nHawsEDC debug message: " MESSAGESTRING))
    MESSAGESTRING
  )
)

;;MKFLD sub-function makes a field string out of a string.
;;If format
;;Usage: (mkfld
;;         [string to place into a field]
;;         [uniform field width or field delimiter character]
;;       )
(VL-ACAD-DEFUN 'HAWS-MKFLD)
(DEFUN
   HAWS-MKFLD (STRING FORMAT / CHAR I MKFLD_FIELD MKFLD_LITERAL)
  (COND
    ((= (TYPE FORMAT) 'STR)
     (SETQ
       I 0
       MKFLD_FIELD ""
     )
     (COND
       ((WCMATCH STRING (STRCAT "*`" FORMAT "*,*\"*,*\n*"))
        (SETQ
          MKFLD_LITERAL T
          MKFLD_FIELD "\""
        )
       )
     )
     (WHILE (<= (SETQ I (1+ I)) (STRLEN STRING))
       (SETQ
         MKFLD_FIELD
          (STRCAT
            MKFLD_FIELD
            (COND
              ((= (SETQ CHAR (SUBSTR STRING I 1)) "\"")
               "\"\""
              )
              (T CHAR)
            )
          )
       )
     )
     (IF MKFLD_LITERAL
       (SETQ MKFLD_FIELD (STRCAT MKFLD_FIELD "\""))
     )
     (SETQ MKFLD_FIELD (STRCAT MKFLD_FIELD FORMAT))
    )
    (T
     (SETQ MKFLD_FIELD STRING)
     (WHILE
       (< (STRLEN (SETQ MKFLD_FIELD (SUBSTR MKFLD_FIELD 1 FORMAT)))
          FORMAT
       )
        (SETQ MKFLD_FIELD (STRCAT MKFLD_FIELD " "))
     )
    )
  )
  MKFLD_FIELD
)
;; MKLAYR sub-function defines and makes current a layer for another routine.
;; Usage: (haws-mklayr (list "laname" "lacolr" "laltyp"))
;; Use empty quotes for default color and linetype (eg. (mklay (list "AZ" "" ""))
(DEFUN
   HAWS-GETUSL (/ I RDLIN TEMP)
  (SETQ TEMP (FINDFILE "layers.dat"))
  (COND
    (TEMP
     (PROMPT "\nReading layer settings from ")
     (PRINC TEMP)
     (PRINC "\n)")
    )
    ((PROMPT "\nLayer settings file not found.") (EXIT))
  )
  (SETQ
    F3 (OPEN TEMP "r")
    I  0
  )
  (WHILE (SETQ RDLIN (READ-LINE F3))
    (PRINC "\rReading line ")
    (PRINC (SETQ I (1+ I)))
    (IF (= 'LIST (TYPE (SETQ TEMP (READ RDLIN))))
      (SETQ *HAWS:LAYERS* (CONS TEMP *HAWS:LAYERS*))
    )
  )
  (SETQ F3 (CLOSE F3))
)
(VL-ACAD-DEFUN 'HAWS-GETLAYR)
(DEFUN
   HAWS-GETLAYR (KEY / TEMP)
  (IF (OR (NOT *HAWS:LAYERS*)
          (COND
            ((= (C:HCNM-CONFIG-GETVAR "ImportLayerSettings") "Yes")
             (C:HCNM-CONFIG-SETVAR "ImportLayerSettings" "No")
             T
            )
          )
      )
    (HAWS-GETUSL)
  )
  (COND
    ((CDR (ASSOC KEY *HAWS:LAYERS*)))
    (T
     (PROMPT
       (STRCAT
         "\nSettings for \""
         KEY
         "\" not found in LAYERS.DAT.  Using current layer."
       )
     )
     (LIST (GETVAR "clayer") "" "")
    )
  )
)
(VL-ACAD-DEFUN 'HAWS-MKLAYR)
(DEFUN
   HAWS-MKLAYR (LAOPT / LANAME LACOLR LALTYP LTFILE LTFILES TEMP)
  ;;(princ "\nHAWS-MKLAYR in edclib")
  (IF (= 'STR (TYPE LAOPT))
    (SETQ
      LAOPT
       (COND
         ((HAWS-GETLAYR LAOPT))
         ('("" "" ""))
       )
    )
  )
  (SETQ
    LANAME
     (CAR LAOPT)
    LACOLR
     (CADR LAOPT)
    LALTYP
     (CADDR LAOPT)
  )
  (HAWS-LOAD-LINETYPE LALTYP)
  (WHILE (AND (/= LALTYP "") (NOT (TBLSEARCH "LTYPE" LALTYP)))
    (ALERT
      (STRCAT
        "\nLinetype "
        LALTYP
        " is still not loaded.\nPlease follow prompts to try a different linetype or file."
      )
    )
    (SETQ
      TEMP
       (HAWS-GETSTRINGX
         "\nEnter substitute linetype name or enter to try another file"
         LALTYP
         LALTYP
       )
    )
    (COND
      ((/= TEMP LALTYP)
       (SETQ LALTYP TEMP)
       (HAWS-LOAD-LINETYPE LALTYP)
      )
    )
    (COND
      ((NOT (TBLSEARCH "LTYPE" LALTYP))
       (SETQ
         LTFILE
          (GETFILED
            (STRCAT "File for " LALTYP " Linetype")
            ""
            "LIN"
            6
          )
       )
      )
    )
    (COMMAND-S "._linetype" "_l" LALTYP LTFILE "")
  )
  (HAWS-MILEPOST "Finished assuring linetype.")
  (IF (NOT (TBLSEARCH "LAYER" LANAME))
    (COMMAND-S "._layer" "_m" LANAME "")
    (COMMAND-S "._layer" "_t" LANAME "_on" LANAME "_u" LANAME "_s" LANAME "")
  )
  (IF (/= LACOLR "")
    (COMMAND-S "._layer" "_c" LACOLR "" "")
  )
  (IF (/= LALTYP "")
    (COMMAND-S "._layer" "_lt" LALTYP "" "")
  )
  (HAWS-MILEPOST "Finished making layer.")
  LAOPT
)

(DEFUN
   HAWS-LOAD-LINETYPE (LTYPE / I LTFILES)
  (SETQ
    LTFILES
     (LIST "acad" "hawsedc" "default")
    I -1
  )
  (WHILE (AND
           (/= LALTYP "")
           (NOT (TBLSEARCH "LTYPE" LTYPE))
           (SETQ LTFILE (NTH (SETQ I (1+ I)) LTFILES))
         )
    (PRINC
      (STRCAT
        "\nLinetype " LTYPE " is not loaded. Attempting to load from "
        LTFILE ".lin..."
       )
    )
    (COMMAND-S "._linetype" "_l" LTYPE LTFILE "")
  )
  (HAWS-MILEPOST
    (STRCAT
      "Finished trying to load linetype "
      LTYPE
      " from acad.lin, default.lin (Bricscad), and hawsedc.lin."
    )
  )
)

;;; ======================================================================
;;;
;;;                 Text creation and scale functions
;;;
;;; ======================================================================


(VL-ACAD-DEFUN 'HAWS-DWGSCALE)
(DEFUN
   HAWS-DWGSCALE ()
  (COND
    ((OR (= (GETVAR "DIMANNO") 1) (= (GETVAR "DIMSCALE") 0))
     (/ 1 (GETVAR "CANNOSCALEVALUE"))
    )
    ((GETVAR "DIMSCALE"))
  )
)

(DEFUN
   HAWS-TEXT-HEIGHT-PAPER ()
  (GETVAR "DIMTXT")
)

(DEFUN
   HAWS-TEXT-HEIGHT-MODEL ()
  (* (GETVAR "DIMTXT") (HAWS-DWGSCALE))
)

(VL-ACAD-DEFUN 'HAWS-MKTEXT)
(DEFUN
   HAWS-MKTEXT (J I H R S / ENT JX JY)
  (SETQ
    I  (TRANS
         (IF (= 2 (LENGTH I))
           (APPEND I '(0.0))
           I
         )
         1
         0
       )
    J  (IF (= J NIL)
         "L"
         (STRCASE J)
       )
    JX (COND
         ((WCMATCH J "L,BL*,ML*,TL*") 0)
         ((WCMATCH J "C*,?C*") 1)
         ((WCMATCH J "R*,?R*") 2)
         ((WCMATCH J "A*") 3)
         ((WCMATCH J "M*") 4)
         ((WCMATCH J "F*") 5)
       )
    JY (COND
         ((WCMATCH J "L,C*,R*,A*,F*") 0)
         ((WCMATCH J "B*") 1)
         ((WCMATCH J "M*") 2)
         ((WCMATCH J "T*") 3)
       )
  )
  (ENTMAKE
    (LIST
      (CONS 0 "TEXT")
      (CONS
        1
        (COND
          (S)
          ("This text created by HAWS-MKTEXT")
        )
      )
      (CONS 7 (GETVAR "textstyle"))
      (APPEND '(10) I)
      ;; Simple entmake doesn't create annotative text.
      (CONS
        40
        (COND
          (H)
          (T
            (haws-text-height-model)
          )
        )
      )
      (ASSOC 41 (TBLSEARCH "STYLE" (GETVAR "textstyle")))
      (CONS 50 (+ R (ANGLE '(0.0 0.0 0.0) (GETVAR "ucsxdir"))))
      (CONS
        51
        (CDR (ASSOC 50 (TBLSEARCH "STYLE" (GETVAR "textstyle"))))
      )
      (CONS 72 JX)
      (CONS 73 JY)
    )
  )
  (SETQ
    ENT (ENTGET (ENTLAST))
    ENT (SUBST (CONS 11 I) (ASSOC 11 ENT) ENT)
  )
  (ENTMOD ENT)
)

(defun
   haws-make-mtext (i j h w s masked-p / ename-mtext)
  ;; creates annotative text if style is annotative.
  (setq h
    (cond
      (h)
      ((LM:isAnnotative (getvar "textstyle"))(haws-text-height-paper))
      (T (haws-text-height-model))
    )
  )
  (command "._mtext" i "_j" (strcat "_" j) "_h" h "_w" w s "")
  (COND
    (MASKED-P
     (SETQ ENAME-MTEXT (ENTLAST))
     (ENTMOD
       (APPEND
         (ENTGET ENAME-MTEXT)
         '((90 . 3) (63 . 256) (45 . 1.1) (441 . 0))
       )
     )
    )
  )
)

(VL-ACAD-DEFUN 'HAWS-MKLINE)
(DEFUN
   HAWS-MKLINE (PT1 PT2)
  (SETQ
    PT1 (IF (= 2 (LENGTH PT1))
          (APPEND PT1 '(0.0))
          PT1
        )
    PT2 (IF (= 2 (LENGTH PT2))
          (APPEND PT2 '(0.0))
          PT2
        )
  )
  (ENTMAKE
    (LIST
      (CONS 0 "LINE")
      (APPEND '(10) (TRANS PT1 1 0))
      (APPEND '(11) (TRANS PT2 1 0))
    )
  )
)

;;
;; HAWS-PATH-CHOP-TRUNK
;; Chops the common initial elements "trunk" off lists provided,
;; leaving only the unique branches.
;; Useful for relating paths.
;;
(VL-ACAD-DEFUN 'HAWS-PATH-CHOP-TRUNK)
(DEFUN
   HAWS-PATH-CHOP-TRUNK (TREES CASE-SENSITIVE-P / LENGTH-COMMON)
  (SETQ
    TREE-COMMON
     (CAR TREES)
    LENGTH-COMMON
     (LENGTH TREE-COMMON)
  )
  ;; Find common trunk length
  (MAPCAR
    '(LAMBDA (TREE / I)
       (SETQ I -1)
       (WHILE (AND
                (NTH (SETQ I (1+ I)) TREE)
                (IF CASE-SENSITIVE-P (= (NTH I TREE) (NTH I TREE-COMMON)) (= (STRCASE (NTH I TREE)) (STRCASE (NTH I TREE-COMMON))))
              )
       )
       (IF (< I LENGTH-COMMON)
         (SETQ LENGTH-COMMON I)
       )
     )
    TREES
  )
  ;; Chop off common trunk from each tree.
  (MAPCAR
    '(LAMBDA (TREE / I)
       (SETQ I -1)
       (WHILE (< (SETQ I (1+ I)) LENGTH-COMMON)
         (SETQ TREE (CDR TREE))
       )
       TREE
     )
    TREES
  )
)

;;
;;HAWS-PATH-RELATE
;;
;; Converts an absolute path to a relative path if possible
;; given path and comparison path, both including filename.
;;
(VL-ACAD-DEFUN 'HAWS-PATH-RELATE)
(DEFUN
   HAWS-PATH-RELATE (PATH-ABSOLUTE PATH-COMPARE CASE-SENSITIVE-P / BRANCH-ABSOLUTE BRANCH-COMPARE BRANCHES LIST-ABSOLUTE LIST-COMPARE RELATIVE-PATH)
  (SETQ
    ;; Parse to lists.
    LIST-ABSOLUTE
     (HAWS-STRTOLST PATH-ABSOLUTE "\\" "\"" T)
    LIST-COMPARE
     (HAWS-STRTOLST PATH-COMPARE "\\" "\"" T)
    BRANCHES
     (HAWS-PATH-CHOP-TRUNK
       (LIST
         ;; remove filenames.
         (REVERSE (CDR (REVERSE LIST-ABSOLUTE)))
         (REVERSE (CDR (REVERSE LIST-COMPARE)))
       )
       CASE-SENSITIVE-P
     )
    BRANCH-ABSOLUTE
     (CAR BRANCHES)
    BRANCH-COMPARE
     (CADR BRANCHES)
  )
  (SETQ
    RELATIVE-PATH
     (STRCAT
       (COND
         ((= (LENGTH BRANCH-COMPARE) 0) ".\\")
         ((/= (SUBSTR (CAR BRANCH-COMPARE) 2 1) ":")
          (APPLY
            'STRCAT
            (MAPCAR '(LAMBDA (X) "..\\") BRANCH-COMPARE)
          )
         )
         (T "")
       )
       (HAWS-LSTTOSTR (REVERSE (CONS (CAR (REVERSE LIST-ABSOLUTE)) (REVERSE BRANCH-ABSOLUTE))) "\\" "\"")
     )
  )
)

;;
;;HAWS-PATH-UNRELATE
;;
;; Converts a relative path to an absolute path
;; given path and comparison path, both including filename.
;;
(VL-ACAD-DEFUN 'HAWS-PATH-UNRELATE)
(DEFUN
   HAWS-PATH-UNRELATE
   (PATH-RELATIVE PATH-COMPARE / LIST-COMPARE LIST-RELATIVE)
  (SETQ
    ;; Parse to lists.
    LIST-RELATIVE
     (HAWS-STRTOLST PATH-RELATIVE "\\" "\"" T)
    ;; Reverse and remove filename
    LIST-COMPARE
     (CDR
       (REVERSE (HAWS-STRTOLST PATH-COMPARE "\\" "\"" T))
     )
  )
  (COND
    ;; If really relative, process.
    ((= (SUBSTR (CAR LIST-RELATIVE) 1 1) ".")
      (FOREACH
         NODE LIST-RELATIVE
        (COND
          ((= (SUBSTR NODE 1 1) ".")
           (SETQ LIST-RELATIVE (CDR LIST-RELATIVE))
          )
        )
        (COND ((= NODE "..\\") (SETQ LIST-COMPARE (CDR LIST-COMPARE))))
      )
      (HAWS-LSTTOSTR (APPEND (REVERSE LIST-COMPARE) LIST-RELATIVE) "\\" "\"")
    )
    ;; If not really relative, return provided path.
    (T PATH-RELATIVE)
  )
)

;;
;;HAWS-PRIN1-TO-STRING
;;
;; For Intellicad, a cheap (and dirty, leaves a file on disk)
;; VL-PRIN1-TO-STRING
;; substitute
;;
(VL-ACAD-DEFUN 'HAWS-PRIN1-TO-STRING)
(DEFUN
   HAWS-PRIN1-TO-STRING (ATOMX / F1 F2 STRING)
  (COND
    ((HAWS-VLISP-P) (VL-PRIN1-TO-STRING ATOMX))
    (T
     (SETQ F2 (OPEN "hawsprin1.tmp" "w"))
     (PRIN1 ATOMX F2)
     (SETQ F2 (CLOSE F2))
     (SETQ F1 (OPEN "hawsprin1.tmp" "r"))
     (SETQ STRING (READ-LINE F1))
     (SETQ F1 (CLOSE F1))
     STRING
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
(VL-ACAD-DEFUN 'HAWS-3PTTOBULGE)
(DEFUN
   HAWS-3PTTOBULGE
   (PNT1 PNT2 PNT3 / ANG1 ANG2 ANG3 BULGE CHORD DELTA DELTA1 R)
;;;Returns the bulge of an arc defined by three points, PNT1, PNT2, and PNT3
;;;If point 2 nil, returns 0.
;;;In geometry triangle terms, R=a/(2*sin(A)) for any of the three points
;;;The sum of angles 1 and 3 is delta
  (COND
    ((NOT PNT2) 0)
    (T
     (SETQ
       CHORD
        (DISTANCE PNT1 PNT3)
       ANG2
        (- (ANGLE PNT2 PNT1) (ANGLE PNT2 PNT3))
       ;;CHORD / SIN(ANG2) is 
       R
        (/ CHORD (* 2 (SIN ANG2)))
       DELTA1
        (* 2 (HAWS-ASIN (/ CHORD (* 2 R))))
       ;;If sin(ang1) is negative, bulge is negative.
       ;;Since AutoCAD always returns a positive angle,
       ;;if the quadrant of the second
       ANG1
        (ABS (- (ANGLE PNT1 PNT3) (ANGLE PNT1 PNT2)))
       ANG1
        (ABS
          (IF (> ANG1 PI)
            (- ANG1 (* 2 PI))
            ANG1
          )
        )
       ANG3
        (ABS (- (ANGLE PNT3 PNT1) (ANGLE PNT3 PNT2)))
       ANG3
        (ABS
          (IF (> ANG3 PI)
            (- ANG3 (* 2 PI))
            ANG3
          )
        )
       DELTA
        (* 2 (+ ANG1 ANG3))
       BULGE
        (* (IF (MINUSP R)
             -1
             1
           )
           (HAWS-TAN (/ DELTA 4.0))
        )
     )
    )
  )
)


;;;  HAWS-SEGMENT-LENGTH
(VL-ACAD-DEFUN 'HAWS-SEGMENT-LENGTH)
(DEFUN
   HAWS-SEGMENT-LENGTH
;;;  Returns curve or straight length of a segment.
                       (2DPNT1 2DPNT2 BULGE / D DELTA DOVER2 L R)
  (SETQ
    ;;Make sure points are truly 2d
    2DPNT1
     (HAWS-FLATTEN 2DPNT1)
    2DPNT2
     (HAWS-FLATTEN 2DPNT2)
    D (/ (DISTANCE 2DPNT1 2DPNT2) 2)
  ) ;_ end of setq
  (COND
    ((/= 0 BULGE)
     (SETQ
       DOVER2
        (ABS (* 2 (ATAN BULGE)))
       DELTA
        (* 2 DOVER2)
       R (/ D (SIN DOVER2))
     ) ;_ end of setq
     (* DELTA R)
    )
    (T (* D 2))
  ) ;_ end of cond
)

;;;HAWS-STRTOLST
;;;Parses a string into a list of fields.
;;;Usage: (haws-strtolst
;;;         [InputString containing fields]
;;;         [FieldSeparatorWC field delimiter wildcard string]
;;;         [TextDelimiter text delimiter character.
;;;          Use "`," for comma and " ,\t,\n" for white space]
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
(VL-ACAD-DEFUN 'HAWS-STRTOLST)
(DEFUN
   HAWS-STRTOLST (INPUTSTRING FIELDSEPARATORWC TEXTDELIMITER
                  EMPTYFIELDSDOCOUNT / CHARACTERCOUNTER CONVERSIONISDONE
                  CURRENTCHARACTER CURRENTFIELD CURRENTFIELDISDONE
                  PREVIOUSCHARACTER RETURNLIST TEXTMODEISON TextPairIsOpen
                 )
  ;;Initialize the variables for clarity's sake
  (SETQ
    CHARACTERCOUNTER 0
    PREVIOUSCHARACTER ""
    CURRENTCHARACTER ""
    CURRENTFIELD ""
    CURRENTFIELDISDONE NIL
    TEXTMODEISON NIL
    CONVERSIONISDONE NIL
    RETURNLIST NIL
  )
  ;;Make sure that the FieldSeparatorWC is not empty.
  (COND
    ;;If an empty string matches the FieldSeparatorWC, then
    ((WCMATCH "" FIELDSEPARATORWC)
     ;;1. Give an alert about the problem.
     (ALERT
       ;;Include princ to allow user to see and copy error
       ;;after dismissing alert box.
       (PRINC
         (STRCAT
           "\n\""
           FIELDSEPARATORWC
           "\" is not a valid field delimiter."
         )
       )
     )
     ;;2. Exit with error.
     (EXIT)
    )
  )
  ;;Start the main character-by-character InputString examination loop.
  (WHILE (NOT CONVERSIONISDONE)
    (SETQ
      ;;Save CurrentCharacter as PreviousCharacter.
      PREVIOUSCHARACTER
       CURRENTCHARACTER
      ;;CharacterCounter starts at 0 above.  Increment it.
      CHARACTERCOUNTER
       (1+ CHARACTERCOUNTER)
      ;;Get new CurrentCharacter from InputString.
      CURRENTCHARACTER
       (SUBSTR INPUTSTRING CHARACTERCOUNTER 1)
    )
    ;;Decide what to do with CurrentCharacter.
    (COND
      ;;If CurrentCharacter is a TextDelimiter, then
      ((= CURRENTCHARACTER TEXTDELIMITER)
       ;;1.  Toggle the TextModeIsOn flag
       (IF TEXTMODEISON
         (SETQ TEXTMODEISON nil)
         (SETQ TEXTMODEISON T)
       )
       ;;2.  Use and toggle the TextPairIsOpen flag.
       (COND
         (TextPairIsOpen
           ;;Output it to CurrentField.
           (SETQ CURRENTFIELD (STRCAT CURRENTFIELD CURRENTCHARACTER))
           (SETQ TextPairIsOpen nil)
         )
         (T
           (SETQ TextPairIsOpen T)
         )
       )
      )
      ;;Else if CurrentCharacter is a FieldDelimiter wildcard match, then
      ((WCMATCH CURRENTCHARACTER FIELDSEPARATORWC)
       (COND
         ;;If TextModeIsOn = True, then 
         ((= TEXTMODEISON T)
          ;;Output CurrentCharacter to CurrentField.
          (SETQ CURRENTFIELD (STRCAT CURRENTFIELD CURRENTCHARACTER))
         )
         ;;Else if
         ((OR ;;EmptyFieldsDoCount, or
              (= EMPTYFIELDSDOCOUNT T)
              ;;the CurrentField isn't empty,
              (/= "" CURRENTFIELD)
          )
          ;;Then
          ;;Set the CurrentFieldIsDone flag to true.
          (SETQ CURRENTFIELDISDONE T)
         )
         (T
          ;;Else do nothing
          ;;Do not flag the CurrentFieldDone,
          ;;nor output the CurrentCharacter.
          NIL
         )
       )
      )
      ;;Else if CurrentCharacter is empty, then
      ((= CURRENTCHARACTER "")
       ;;We are at the end of the string.
       ;;1.  Flag ConversionIsDone.
       (SETQ CONVERSIONISDONE T)
       ;;2.  If
       (IF (OR ;;EmptyFieldsDoCount, or
               EMPTYFIELDSDOCOUNT
               ;;the PreviousCharacter wasn't a FieldSeparatorWC, or
               (NOT (WCMATCH PREVIOUSCHARACTER FIELDSEPARATORWC))
               ;;the ReturnList is still nil due to only empty non-counting fields in string,
               ;;(This check is a bug fix added 2008-02-18 TGH)
               (= RETURNLIST NIL)
           )
         ;;Then flag the CurrentFieldIsDone to wrap up the last field.
         (SETQ CURRENTFIELDISDONE T)
       )
      )
      ;;Else (CurrentCharacter is something else),
      (T
       ;;Output CurrentCharacter to CurrentField.
       (SETQ CURRENTFIELD (STRCAT CURRENTFIELD CURRENTCHARACTER))
       (SETQ TextPairIsOpen nil)
      )
    )
    ;;If CurrentFieldIsDone, then
    (IF CURRENTFIELDISDONE
      ;;Output it to the front of ReturnList.
      (SETQ
        RETURNLIST
         (CONS CURRENTFIELD RETURNLIST)
        ;;Start a new CurrentField.
        CURRENTFIELD
         ""
        CURRENTFIELDISDONE NIL
      )
    )
    ;;End the main character-by-character InputString examination loop.
  )
  ;;Reverse the backwards return list and we are done.
  (REVERSE RETURNLIST)
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
(VL-ACAD-DEFUN 'HAWS-RDFLD)
(DEFUN
   HAWS-RDFLD (FIELDNO INPUTSTRING FIELDSEPARATOR OPT / ATOMCOUNTER
               ATOMY ATOMX EMPTYFIELDSDOCOUNT ISCHRISLONG PARSEDLIST
               TEXTDELIMITER FIELDSEPARATORWC ISCHR ISLONG
              )
  (SETQ
    ISCHR
     (= 1 (LOGAND 1 OPT))
    ISLONG
     (= 2 (LOGAND 2 OPT))
    TEXTDELIMITER "\""
    EMPTYFIELDSDOCOUNT T
    FIELDSEPARATORWC FIELDSEPARATOR
  )
  (COND
    ;;If the field delimiter is a comma ",", then
    ((= FIELDSEPARATORWC ",")
     ;;Replace it with an AutoCAD escaped comma wildcard.
     (SETQ FIELDSEPARATORWC "`,")
    )
    ;;If the field delimiter is "W" (for word or whitespace), then
    ((= FIELDSEPARATORWC "W")
     (SETQ
       ;;1. Replace it with a white space wild card.
       FIELDSEPARATORWC
        " ,\t,\n"
       ;;2.  Set EmptyFieldsDoCount to nil
       EMPTYFIELDSDOCOUNT
        NIL
     )
    )
  )
  (COND
    ;;If fielddelimiter is a number, then do a fixed width field extraction.
    ((= (TYPE FIELDSEPARATORWC) 'INT)
     (SETQ
       ATOMX
        (SUBSTR
          INPUTSTRING
          (1+ (* (1- FIELDNO) FIELDSEPARATORWC))
          (IF ISLONG
            1000
            FIELDSEPARATORWC
          )
        )
     )
     (IF (AND ISCHR (NOT ISLONG))
       (SETQ ATOMX (HAWS-RDFLD-UNPAD ATOMX))
     )
    )
    ;;Else do a character delimiter field extraction.
    (T
     (SETQ
       PARSEDLIST
        (HAWS-STRTOLST
          INPUTSTRING
          FIELDSEPARATORWC
          TEXTDELIMITER
          EMPTYFIELDSDOCOUNT
        )
     )
     (SETQ ATOMX (NTH (1- FIELDNO) PARSEDLIST))
     ;;If the IsLong flag is set, add any subsequent fields to the output string.
     (COND
       (ISLONG
        (SETQ ATOMCOUNTER (1- FIELDNO))
        (WHILE (SETQ
                 ATOMY
                  (NTH (SETQ ATOMCOUNTER (1+ ATOMCOUNTER)) PARSEDLIST)
               )
          (SETQ ATOMX (STRCAT ATOMX FIELDSEPARATOR ATOMY))
        )
       )
     )
    )
  )
  (SETQ
    ATOMX
     (IF ISCHR
       ATOMX
       (DISTOF ATOMX)
     )
  )
)

;;Strip white space from beginning and end of a string
(VL-ACAD-DEFUN 'HAWS-RDFLD-UNPAD)
(DEFUN
   HAWS-RDFLD-UNPAD (STR)
  (WHILE (WCMATCH (SUBSTR STR 1 1) " ,\t")
    (SETQ STR (SUBSTR STR 2))
  )
  (WHILE (WCMATCH (HAWS-ENDSTR STR 1 1) " ,\t")
    (SETQ STR (SUBSTR STR 1 (1- (STRLEN STR))))
  )
  STR
)

;;Returns nil if in ICAD mode
(VL-ACAD-DEFUN 'HAWS-REGISTRY-READ)
(DEFUN
   HAWS-REGISTRY-READ (REG-KEY VAL-NAME)
  (COND
    ((C:HAWS-ICAD-P) NIL)
    ((VL-REGISTRY-READ REG-KEY VAL-NAME))
  )
)

;;Returns nil if in ICAD mode
(VL-ACAD-DEFUN 'HAWS-REGISTRY-WRITE)
(DEFUN
   HAWS-REGISTRY-WRITE (REG-KEY VAL-NAME VAL-DATA)
  (COND
    ((C:HAWS-ICAD-P) NIL)
    ((VL-REGISTRY-WRITE REG-KEY VAL-NAME VAL-DATA))
  )
)


;;Remove an element from a list
(VL-ACAD-DEFUN 'HAWS-REMOVE)
(DEFUN
   HAWS-REMOVE (ELEMENT LST)
  (APPEND
    (REVERSE (CDR (MEMBER ELEMENT (REVERSE LST))))
    (CDR (MEMBER ELEMENT LST))
  )
)

;;Convert a radian angle to a presentation quality bearing.
(VL-ACAD-DEFUN 'HAWS-RTOB)
(DEFUN
   HAWS-RTOB (RAD AU / B I)
  (SETQ B (ANGTOS RAD AU))
  (IF (WCMATCH B "*d*")
    (PROGN
      (SETQ I 0)
      (WHILE (/= "d" (SUBSTR B (SETQ I (1+ I)) 1)))
      (SETQ B (STRCAT (SUBSTR B 1 (1- I)) "%%d" (SUBSTR B (1+ I))))
    )
  )
  (IF (WCMATCH B "*d#[`.']*")
    (PROGN
      (SETQ I 0)
      (WHILE (/= "d" (SUBSTR B (SETQ I (1+ I)) 1)))
      (SETQ B (STRCAT (SUBSTR B 1 I) "0" (SUBSTR B (1+ I))))
    )
  )
  (IF (WCMATCH B "*'#[`.\"]*")
    (PROGN
      (SETQ I 0)
      (WHILE (/= "'" (SUBSTR B (SETQ I (1+ I)) 1)))
      (SETQ B (STRCAT (SUBSTR B 1 I) "0" (SUBSTR B (1+ I))))
    )
  )
  (SETQ
    B (COND
        ((= B "N") "NORTH")
        ((= B "S") "SOUTH")
        ((= B "E") "EAST")
        ((= B "W") "WEST")
        (B)
      )
  )
)

;; RTOSTA sub-function converts a real number to a base 100 road
;; station.
(VL-ACAD-DEFUN 'HAWS-RTOSTA)
(DEFUN
   HAWS-RTOSTA (STA LUP / ISNEG AFTER BEFORE)
  (SETQ
    LUP
     (COND
       (LUP)
       ((GETVAR "luprec"))
     )
    ISNEG
     (MINUSP STA)
    STA
     (RTOS (ABS STA) 2 LUP)
  )
  (WHILE (< (STRLEN STA)
            (IF (= LUP 0)
              3
              (+ LUP 4)
            )
         )
    (SETQ STA (STRCAT "0" STA))
  )
  (SETQ
    AFTER
     (IF (= LUP 0)
       (- (STRLEN STA) 1)
       (- (STRLEN STA) LUP 2)
     )
    BEFORE
     (SUBSTR STA 1 (1- AFTER))
    AFTER
     (SUBSTR STA AFTER)
  )
  (IF ISNEG
    (SETQ
      BEFORE
       (STRCAT "-(" BEFORE)
      AFTER
       (STRCAT AFTER ")")
    )
  )
  (STRCAT BEFORE "+" AFTER)
)

;;;  Trig functions not included with AutoLISP
(VL-ACAD-DEFUN 'HAWS-ASIN)
(DEFUN HAWS-ASIN (X) (ATAN X (SQRT (- 1 (* X X)))))
(VL-ACAD-DEFUN 'HAWS-ACOS)
(DEFUN HAWS-ACOS (X) (ATAN (SQRT (- 1 (* X X))) X))
(VL-ACAD-DEFUN 'TAN)
(DEFUN HAWS-TAN (X) (/ (SIN X) (COS X)))
(VL-ACAD-DEFUN 'HAWS-VSET)
(DEFUN
   HAWS-VSET (VLST)
  (FOREACH
     V VLST
    (IF (GETVAR (CAR V))
      (SETVAR (CAR V) (CADR V))
    )
  )
)

(VL-ACAD-DEFUN 'HAWS-VTOG)
(DEFUN
   HAWS-VTOG (VLST)
  (FOREACH
     V VLST
    (PRINC (STRCAT "\n" V " toggled to "))
    (SETVAR
      V
      (PRINC
        (IF (= (GETVAR V) 0)
          1
          0
        )
      )
    )
  )
  (PRINC)
)

(VL-ACAD-DEFUN 'HAWS-VSAVE)
(DEFUN
   HAWS-VSAVE (VLST)
  (SETQ *HAWS-VSTR* (MAPCAR '(LAMBDA (V) (LIST V (GETVAR V))) VLST))
)

(VL-ACAD-DEFUN 'HAWS-VRSTOR)
(DEFUN
   HAWS-VRSTOR ()
  (MAPCAR '(LAMBDA (V) (SETVAR (CAR V) (CADR V))) *HAWS-VSTR*)
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
(VL-ACAD-DEFUN 'HAWS-WRAP)
(DEFUN
   HAWS-WRAP (STRNG1 MAXLEN CHAR / FIRST I LSTRNI STRIPC STRIPS STRNG2
              STRNGI TEMP WLIST
             )
  (SETQ
    I 1
    CHAR
     (STRCAT "`" CHAR)
    FIRST T
    WLIST NIL
    STRNG2 ""
    STRNGI ""
    LSTRNI 0
  )
  ;;Break strng1 at every break point
  (WHILE (/= "" (SUBSTR STRNG1 I))
    (COND
      (;;For every break or at end of string
       (OR (WCMATCH (SUBSTR STRNG1 1 I) (STRCAT "*" CHAR))
           (= I (STRLEN STRNG1))
       )
       (SETQ
         STRNGI
          (SUBSTR STRNG1 1 I)
         STRIPS STRNGI
         STRIPC STRNGI
         STRNG1
          (SUBSTR STRNG1 (1+ I))
         I 1
       )
       ;; Strip leading spaces from all but first piece.  Save as
       ;; strips.
       (IF (NOT FIRST)
         (WHILE (= (SUBSTR STRIPS 1 1) " ")
           (SETQ STRIPS (SUBSTR STRIPS 2))
         )
       )
       ;;Strip break character.  Save as stripc
       (IF (WCMATCH STRIPC (STRCAT "*" CHAR))
         (SETQ STRIPC (SUBSTR STRIPC 1 (1- (STRLEN STRIPC))))
       )
       ;; Add strngi to strng2 if possible, otherwise, call strng2
       ;; full.
       (COND
         ;;If strng2 is empty set to strips
         ((= "" STRNG2) (SETQ STRNG2 STRIPS))
         ;;else add strngi to strng2 if it fits stripped.
         ((<= (STRLEN (SETQ TEMP (STRCAT STRNG2 STRIPC))) MAXLEN)
          (SETQ STRNG2 (STRCAT STRNG2 STRNGI))
         )
         ((IF (WCMATCH STRNG2 (STRCAT "*" CHAR))
            (SETQ STRNG2 (SUBSTR STRNG2 1 (1- (STRLEN STRNG2))))
          )
          (SETQ
            WLIST
             (CONS STRNG2 WLIST)
            STRNG2 STRIPS
          )
         )
       )
       (SETQ FIRST NIL)
      )
      (T (SETQ I (1+ I)))
    )
  )
  (REVERSE (CONS STRNG2 WLIST))
)

;;Functions for oo, selstyle, and le

;;Selcerob--Selects a certain type of object. Returns entsel list.
(VL-ACAD-DEFUN 'HAWS-SELCEROB)
(DEFUN
   HAWS-SELCEROB (PRMPT SERCH / E ELST ENM OK)
  (WHILE (NOT OK)
    (WHILE (NOT (SETQ E (ENTSEL PRMPT))))
    (SETQ ELST (ENTGET (SETQ ENM (CAR E))))
    (IF (/= (CDR (ASSOC 0 ELST)) SERCH)
      (PRINC (STRCAT "**Not a " SERCH ", try again**"))
      (SETQ OK T)
    )
  )
  E
)

;;
;;HAWS-TXLEN
;;
;; For Intellicad compatibility
;;
(VL-ACAD-DEFUN 'HAWS-TXLEN)
(DEFUN
   HAWS-TXLEN (STRING HEIGHT)
  (IF (C:HAWS-ICAD-P)
    (* HEIGHT (STRLEN STRING) 0.80)
    (CAADR (TEXTBOX (LIST (CONS 1 STRING) (CONS 40 HEIGHT))))
  )
)

;;
;; HAWS-VLISP-P
;;
;;Tests whether visual lisp functions are available.
(VL-ACAD-DEFUN 'HAWS-VLISP-P)
(DEFUN
   HAWS-VLISP-P ()
  (NOT (< (ATOF (GETVAR "acadver")) 15))
)


;;end sub-functions

;;; Verify/Check values of stored authorization strings at
;;; load-time
;;; against computer name and bios date.
;;; Delete registry entry if invalid for this computer.
;;; New scheme 2007-09:
;;; Getting a little more lax.
;;; If (HAWS-READCFG "/HawsEDC/Modules/package/OrderString") "",
;;; we assume quite trustingly that the application has never yet been tried.
(DEFUN
   HAWS-CHECKSTOREDSTRINGS (/ AUTHLIST AUTHSTRING DELETEALL TEMP)
  (HAWS-MILEPOST "Entering HAWS-CHECKSTOREDSTRINGS")
  (FOREACH
     PACKAGE '(0 3)
    (SETQ AUTHSTRING (HAWS-READAUTHCODE PACKAGE))
    (COND
      (;;If
       (AND
         ;;Authstring is present
         AUTHSTRING
         (/= AUTHSTRING "aaaaaaaaaaaa")
         (HAWS-MILEPOST
           "Authstring is present and isn't the dummy string.  Now checking that it's for the right computer to delete it if not."
         )
         ;;and
         (OR ;; it either is invalid,
             (/= (STRLEN AUTHSTRING) 12)
             ;;for the wrong computer,
             (NOT
               (EQUAL
                 (CDR
                   (CDDDR (SETQ AUTHLIST (HAWS-AUTHTOLIST AUTHSTRING)))
                 )
                 (CONS (HAWS-GETSHORTCOMPUTERNAME) (HAWS-GETBIOSDATE))
               )
             )
             ;;or
             ;;it's for the wrong package,
             (/= PACKAGE (CADDR AUTHLIST))
         )
       )
       ;;Then flag to delete all order strings and auth strings from storage.
       (HAWS-MILEPOST
         (STRCAT "Package " (ITOA PACKAGE) "codes are wrong")
       )
       (SETQ DELETEALL T)
      )
      ((NOT AUTHSTRING)
       (HAWS-MILEPOST
         ";;Else if there is no stored string
       "
       )
       ;|
      (T
       (HAWS-MILEPOST ";;Always give a new free trial (for testing only)
       ;; by storing order and auth strings for a trial
       ")
      |;
       (HAWS-MILEPOST
         ";;Assume it's a virgin installation,
       ;; and give a free trial by storing order and auth strings
       ;; for
       ;; a 30 day trial
       "
       )
       (HAWS-WRITEPACKAGECODE
         PACKAGE
         "OrderString"
         (HAWS-BINARYTOUSER
           (HAWS-ENCRYPTORDERSTRING
             (HAWS-LISTTOBINARY
               (SETQ
                 TEMP
                  (CONS
                    (FIX (GETVAR "date"))
                    (CONS
                      0
                      (CONS
                        PACKAGE
                        (CONS
                          0
                          (CONS
                            (HAWS-GETSHORTCOMPUTERNAME)
                            (HAWS-GETBIOSDATE)
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
       (HAWS-WRITEPACKAGECODE
         PACKAGE
         "AuthString"
         (HAWS-BINARYTOUSER
           (HAWS-ENCRYPTAUTHSTRING
             ;;Trial length
             (HAWS-LISTTOBINARY (CONS (+ (CAR TEMP) 30) (CDR TEMP)))
           )
         )
       )
      )
    )
  )
  (COND
    (DELETEALL
     (FOREACH
        PACKAGE '(0 3)
       (HAWS-WRITEPACKAGECODE PACKAGE "OrderString" "aaaaaaaaaaaa")
       (HAWS-WRITEPACKAGECODE PACKAGE "AuthString" "aaaaaaaaaaaa")
     )
    )
  )
  (HAWS-MILEPOST "Finished HAWS-CHECKSTOREDSTRINGS")
)
(HAWS-CHECKSTOREDSTRINGS)
(IF (/=(HAWS-USE-GET-LOCAL-LOG-STRING)(HAWS-USE-INITIALIZE-LOG-STRING))(HAWS-USE-LOG-REMOTE))
(PROMPT "loaded.")
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
