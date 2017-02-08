(PROMPT "\nHawsEDC library functions...")

;;;This is the current version of HawsEDC and CNM
(DEFUN HAWS-UNIFIED-VERSION () "4.2.19")
;;;(SETQ *HAWS-ICADMODE* T);For testing icad mode in acad.
(SETQ *HAWS-DEBUGLEVEL* 0)
;;This function returns the current setting of nagmode.
;;Elizabeth asked me to give her a version with no nag mode.
(DEFUN HAWS-NAGMODE-P () T)

;;;
;;; ICAD compatibility issues:
;;;
;;; (autoload) doesn't work, so I wrote (haws-autoload)
;;; Visual LISP functions (registry access, vl-file-*, vl-filename-*,
;;; vl-prin1-to-string)
;;; (textbox) requires an actual text entity, not just a string and
;;; height
;;; ssget creates in reverse order (such as for CNM key notes table
;;; refresh)
;;; Can't compile code to make licensing scheme secret.  Must use
;;; different
;;; scheme.
;;;
;;; About version control
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
(DEFUN
   C:HAWS-ABOUT ()
  (ALERT
    (STRCAT
      "HawsEDC Tools version "
      (HAWS-UNIFIED-VERSION)
      "\n\nCopyright 2008 Thomas Gail Haws\nhttp://www.hawsedc.com"
    )
  )
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
;; Don't forget to add new packages to checkregistry function at
;; bottom
;; of this
;; file.
(SETQ
  *HAWS-EDCAPPGROUPS*
   ;; App group 0 is included in package 0 and 3
   '
    ((0 0 3)
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

(VL-LOAD-COM)

;;;HawsEDC general function handler
;;;Includes banner, error handler, and validator.
;;; Internal error handler function.  Call HAWS-ERDF$@ at the
;;; beginning
;;; of a
;;; routine.
;;;Call errrst at the end to restore old *error* handler.
;;;To restore previous UCS, set a global symbol 'ucsp to non-nil.
;;; To restore another previous UCS, set a global symbol 'ucspp to
;;; non-nil.
(DEFUN HAWS-ERRDEF (APPGROUP) (HAWS-ERDF$@ APPGROUP))
(DEFUN
   HAWS-ERDF$@ (APPGROUP / VALIDATED)
  ;; If computer already has authorization,
  (IF (OR (HAWS-VALIDATEAPPGROUP APPGROUP)
        ;; or we successfully get it from the user,
          (HAWS-AUTHORIZEAPPGROUP APPGROUP)
      )
    (SETQ VALIDATED T)
    (COND
      ((NOT (HAWS-NAGMODE-P))
       (ALERT
         (PRINC
           "Application must be authorized to run.\n\nClosing application."
         )
       )
       (EXIT)
      )
      (T (ALERT (PRINC "Continuing in evaluation mode.")))
    )
  )
  (SETQ
    OLDERR *ERROR*
    *ERROR* HAWS-STPERR
  )
  VALIDATED
)

;;Stperr replaces the standard error function.
;;It sets everything back in case of an error.
(DEFUN
   HAWS-STPERR (S)
  (COND
    ((/= S "Function cancelled")
     (PRINC (STRCAT "\nTrapped error: " S))
    )
  )
  (COMMAND)
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
  (IF (= 8 (LOGAND (GETVAR "undoctl") 8))
    (COMMAND "._undo" "end")
  )
  ;; End undo group
  (IF VSTR
    (HAWS-VRSTOR)
  )
  ;; Restore variables to previous values
  (IF UCSP
    (COMMAND "._UCS" "P")
  )
  ;; Restore previous UCS
  (IF UCSPP
    (COMMAND "._UCS" "P")
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
  (IF OLDERR
    (SETQ
      *ERROR* OLDERR
      OLDERR NIL
    )
  )
  ;; Restore old *error* handler
  (PRINC)
)
(DEFUN
   HAWS-ERRRST ()
  (SETQ
    UCSP NIL
    UCSPP NIL
    ENM NIL
    F1 NIL
    F2 NIL
    *ERROR* OLDERR
    OLDERR NIL
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
(DEFUN
   HAWS-VALIDATEAPPGROUP (APPGROUP / AUTHRESULT AUTHORIZEDPERMANENT
                          AUTHSTRING ORDERSTRING PACKAGE TRIALDAYSLEFT
                         )
  ;;To begin with, assume there is no free trial left.
  (SETQ TRIALDAYSLEFT -1)
  ;;Check whether any of the packages that include the requested appgroup
  ;;are authorized to run.
  (FOREACH
     ;;For every package listed as including this appgroup,
     PACKAGE (CDR (ASSOC APPGROUP *HAWS-EDCAPPGROUPS*))
    ;;If
    (IF (AND
          ;;Permanent authorization hasn't already been found in some package
          (NOT AUTHORIZEDPERMANENT)
          ;;and there is an orderstring found for this package,
          (SETQ ORDERSTRING (HAWS-READORDERCODE PACKAGE))
          ;;and there is an authstring found for this package,
          (SETQ AUTHSTRING (HAWS-READAUTHCODE PACKAGE))
          ;;And the order and authorization strings match (return a date difference)
          ;;(matching should have already been checked by checkregistry on loadup)
          (SETQ
            AUTHRESULT
             (HAWS-COMPAREUSERSTRINGS ORDERSTRING AUTHSTRING)
          )
        )
      ;;Then
      ;;Decide whether this package is authorized.
      (COND
        ;; If unlimited stand-alone auth is found for this appgroup
        ;; (order code date is over a million days past auth code date)
        ;; in this package, authorize immediately.
        ((< AUTHRESULT -1000000) (SETQ AUTHORIZEDPERMANENT T))
        ;;Else if today's date is at least a day before auth code date
        ;;a timed trial is in force.
        ;;Bump trial note its remaining time
        ((> (SETQ
              AUTHRESULT
               (- (CAR (HAWS-AUTHTOLIST AUTHSTRING))
                  (GETVAR "date")
               )
            )
            0
         )
         (SETQ TRIALDAYSLEFT (MAX TRIALDAYSLEFT AUTHRESULT))
        )
      )
    )
  )
  (COND
    ;;If authorized permanently, return T
    (AUTHORIZEDPERMANENT)
    ;;Else if there are positive days left in a trial, print a note and return T
    ((NOT (MINUSP TRIALDAYSLEFT))
     (PRINC
       (STRCAT
         "\nEvaluation period expires in "
         (ITOA (FIX TRIALDAYSLEFT))
         " days."
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
       ;;If it's about 1/10th of the time.
       ;;(This expression holds true everytime (getvar "date") rolls around to a 1 in the 1/100,000,000 of a day place.)
       ((WCMATCH
          (SUBSTR (RTOS (REM (GETVAR "date") 1) 2 8) 10)
          "1,2"
        )
        (ALERT
          (PRINC
            (STRCAT
              "\nThis application has not been authorized,\nbut will continue in full-featured evaluation mode until authorized."
              "\n\nAfter a brief evaluation, order trial or paid authorization"
              "\nby providing the order code generated at the following command line prompts."
            )
          )
        )
        (HAWS-ORDERPACKAGE APPGROUP)
       )
     )
    )
    ;;Else expire mode is active
    (T
     (ALERT
       (PRINC
         (STRCAT
           "\nThis application has not been authorized."
           "\n\nYou must order trial or paid authorization"
           "\nby providing the order code generated at the following command line prompts."
         )
       )
     )
     (HAWS-ORDERPACKAGE APPGROUP)
    )
  )
)








;; COMPAREUSERSTRINGS compares an authorization string and a user
;; string.
;;Returns T use is ok
;;Prompts with date if a timed trial is in force
;;Deletes registry authorization if there was a date violation
(DEFUN
   HAWS-COMPAREUSERSTRINGS
   (ORDERSTRING AUTHSTRING / ACADDATE AUTHLIST ORDERLIST)
  (COND
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
    ;;Else if authdate is before orderdate, there is unlimited authorization.
    ;;Return the difference.
    ((< (CAR AUTHLIST) (CAR ORDERLIST))
     (- (CAR AUTHLIST) (CAR ORDERLIST))
    )
    ;;Else if authdate is on or after today's date,
    ;;a trial is in force.
    ;;Return number of days left.
    ((>= (CAR AUTHLIST) (SETQ ACADDATE (GETVAR "date")))
     (- (CAR AUTHLIST) (FIX ACADDATE))
    )
    ;;Else trial is expired.
    ;;Return nil
    (T NIL)
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
;;; Since this is executed by (checkregistry) on load, we can use this function to transfer stored info from the registry
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
(DEFUN
   HAWS-READCFG
   (LOCATIONLIST / HKCUWRITEREQUIRED RETURNVALUE STOREDSTRING)
  (SETQ
    STOREDSTRING
     (COND
       ;;If we are running ICAD (which could be under Linux),
       (*HAWS-ICADMODE*
        ;;Then use the getcfg function
        (GETCFG
          ;;CFG section
          (HAWS-LOCATIONTOCFGSECTION LOCATIONLIST)
        )
       )
       ;;Else if the value is set in the HKCU registry section, use it.
       (;;Read from the HKCU registry section
        ;;(AutoCAD can't write to HKEY_LOCAL_MACHINE under Vista)
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
       ;;Else
       (T
        ;;Flag to write to the HKCU registry section
        (SETQ HKCUWRITEREQUIRED T)
        ;;And read from the HKLM registry section
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
     )
  )
  (COND
    ;;If no stored string, return nothing.
    ((= STOREDSTRING "") NIL)
    ;;Else write to HKCU if flagged and return stored string
    (T
     (IF (AND STOREDSTRING HKCUWRITEREQUIRED)
       (HAWS-REGISTRY-WRITE
         ;;Registry path
         (HAWS-LOCATIONTOREGISTRYPATH
           LOCATIONLIST
           "HKEY_LOCAL_MACHINE"
         )
         ;;Registry key
         (CAR (REVERSE LOCATIONLIST))
         STOREDSTRING
       )
     )
     STOREDSTRING
    )
  )
)
















(DEFUN
   HAWS-WRITEORDERCODE (PACKAGE)
  (HAWS-WRITEPACKAGECODE PACKAGE "OrderString")
)

(DEFUN
   HAWS-WRITEAUTHSTRING (PACKAGE)
  (HAWS-WRITEPACKAGECODE PACKAGE "AuthString")
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
     (COND
       ;;If we are running ICAD (which could be under Linux),
       (*HAWS-ICADMODE*
        ;;Then use the setcfg function
        (SETCFG
          ;;CFG section
          (HAWS-LOCATIONTOCFGSECTION LOCATIONLIST)
          ;;Value
          INPUTSTRING
        )
       )
       ;;Else if we are running under Windows Vista
       (T
        ;;Write to the HKCU section of the registry
        ;;(AutoCAD can't write to HKEY_LOCAL_MACHINE under Vista)
        (HAWS-REGISTRY-WRITE
          ;;Registry path
          (HAWS-LOCATIONTOREGISTRYPATH
            LOCATIONLIST
            "HKEY_CURRENT_USER"
          )
          ;;Registry key
          (CAR (REVERSE LOCATIONLIST))
          ;;Registry value
          INPUTSTRING
        )
       )
     )
  )
  (IF (= STOREDSTRING "")
    NIL
    STOREDSTRING
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
;;; It writes an order string to the storage location,
;;; and asks user for an authorization code.
;;; Returns T if successful, nil if not successful.

(DEFUN
   HAWS-ORDERPACKAGE (APPGROUP / AUTHMATCHSUCCESS AUTHSTRING
                      AUTHWRITESUCCESS ORDERLIST ORDERSTRING PACKAGE
                      WSSH
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
        "\n\nIf you already ordered an authorization code, please enter it at the following prompt."
        "\nTo order an authorization code, please close this message,"
        "\nthen copy and paste the order code shown into the contact form at www.HawsEDC.com."
      )
    )
  )
  (IF (AND
        (SETQ
          AUTHSTRING
           (SUBSTR
             (STRCAT
               (GETSTRING
                 (STRCAT
                   "\nOrder code is: "
                   ORDERSTRING
                   "\nEnter authorization code or the word "
 "
                 )
               )
               "aaaaaaaaaaaa"
             )
             1
             12
           )
        )
        (HAWS-COMPAREUSERSTRINGS ORDERSTRING AUTHSTRING)
      )
    (PROGN
      (SETQ AUTHMATCHSUCCESS T)
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
  (IF (AND AUTHMATCHSUCCESS AUTHWRITESUCCESS)
    (PROGN (ALERT (PRINC "Authorization was successful.")) T)
    (PROGN
      (ALERT
        (PRINC
          (STRCAT
            "Authorization was not successful."
            (IF (NOT AUTHMATCHSUCCESS)
              (STRCAT
                "\nOrder code\n" ORDERSTRING
                "\nand authorization code\n" AUTHSTRING "\ndon't match."
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
        ("\n\nThe application you are trying to run is included in the following packages:\n"
        )
      )
    )
  )
  (SETQ PACKAGES (CDR (ASSOC APPGROUP *HAWS-EDCAPPGROUPS*)))
  (COND
    ((MEMBER 1 PACKAGES)
     (PROMPT
       "\nConstruction Notes Manager Lite alone (no custom editor) ....$200 .....1"
     )
    )
  )
  (COND
    ((MEMBER 2 PACKAGES)
     (PROMPT
       "\nConstruction Notes Manager Pro alone ........................$250 .....2"
     )
    )
  )
  (COND
    ((MEMBER 0 PACKAGES)
     (PROMPT
       "\nHawsEDC Tools including CNM Lite (no custom editor) .........$450 .....3"
     )
    )
  )
  (COND
    ((MEMBER 3 PACKAGES)
     (PROMPT
       "\nHawsEDC Tools including CNM Pro .............................$500 .....4"
     )
    )
  )
  (INITGET "1 2 3 4")(SETQ
    STRING
     (GETKWORD
       "\n\nSelect a package
   [1/2/3/4]
   <4>:
   " )
  )
  ;;  "1" Full HawsEDC package
  ;;  "2" NOTES package standard
  ;;  "3" NOTES package with CNMEdit.exe
  ;;  "4" Full HawsEDC with CNMEdit.exe (CNM Pro plus HawsEDC tools)
  (SETQ
    STRING
     (COND
       ((= STRING "1") "2")
       ((= STRING "2") "3")
       ((= STRING "3") "1")
       ((= STRING "4") "4")
       (T "4")
     )
  )
;;;End user choice section
;;;=====================================================
;;;Begin Construction Notes Manager Pro only section
;;;  (PROMPT "\nGetting single seat authorization for Construction Notes Manager Pro.") ; prompt
;;;  (SETQ STRING "3")
;;;End Construction Notes Manager only section
  (LIST (1- (ATOI STRING)) 0)
)

;;GetBiosDate uses BIOSDATE.EXE to return system bios date as a
;;list in the form '(mm dd yy).
;; In Win 95/98/Me, Bios Date is in
;; HKEY_LOCAL_MACHINE\Enum\Root\*PNP0C01\0000
;; In Win NT 4.0, Bios Date is in
;; HKEY_LOCAL_MACHINE\HARDWARE\DESCRIPTION\System
;; In Win 2000 and XP, Bios Date is in
;; HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Biosinfo
;; SystemBiosDate
(DEFUN
   HAWS-GETBIOSDATE (/ BIOSDATEFULL X)
  (SETQ
    BIOSDATEFULL
     (COND
       (*HAWS-BIOSDATEFULL*)
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Biosinfo"
          "SystemBiosDate"
        )
       )
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\Enum\\Root\\*PNP0C01\\0000"
          "BIOSDate"
        )
       )
       ((HAWS-REGISTRY-READ
          "HKEY_LOCAL_MACHINE\\HARDWARE\\DESCRIPTION\\System"
          "SystemBiosDate"
        )
       )
       (T
        (HAWS-VSAVE '("cmdecho"))
        (SETVAR "cmdecho" 0)
        (COMMAND
          "sh"
          (STRCAT
            (SETQ
              X (SUBSTR
                  (SETQ X (FINDFILE "cnmloader.lsp"))
                  1
                  (- (STRLEN X) 13)
                )
            )
            "biosdate.exe>"
            X
            "biosdate.txt"
          )
        )
        (WHILE
          (NOT (SETQ F1 (OPEN (STRCAT X "biosdate.txt") "r")))
          (COMMAND "delay" "100")
        )
        (READ-LINE F1)
        (SETQ
          BIOSDATEFULL
           (READ-LINE F1)
          F1 (CLOSE F1)
        )
        (VL-FILE-DELETE (STRCAT X "biosdate.txt"))
        (HAWS-VRSTOR)
        BIOSDATEFULL
       )
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
  (COND
    ((HAWS-REGISTRY-READ
       (STRCAT
         "HKEY_LOCAL_MACHINE\\System\\CurrentControlSet"
         "\\Control\\ComputerName\\ComputerName"
       )
       "ComputerName"
     )
    )
    ("ONO NAME FOUNDO")
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

;; ORDERTOAUTH translates a user order string into an unlimited
;; authorization
;;string to give to user for authorization.
(DEFUN
   HAWS-ORDERTOAUTH (ORDERSTRING / ORDERLIST)
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
        (CONS (- (CAR ORDERLIST) 1000001) (CDR ORDERLIST))
      )
    )
  )
)

;; ORDERTOTRIAL translates a user order string into a timed trial
;; authorization
;;string to give to user for authorization.
(DEFUN
   HAWS-ORDERTOTRIAL (ORDERSTRING TRIALDAYS / ORDERLIST)
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

;;; ===================================================================
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
;;;      "u" USER settings are currently stored in ACAD.CGF with (setcfg)/(getcfg) or the Windows registry
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
(DEFUN
   C:TESTSET ()
  (HAWS-TESTSETVAR
    (GETSTRING "\nVariable name: ")
    (GETSTRING "\nValue: ")
  )
)
(DEFUN
   C:TESTGET ()
  (HAWS-TESTGETVAR (GETSTRING "\nVariable name: "))
)
(DEFUN
   HAWS-TESTSETVAR (VAR VAL)
  (HAWS-SETVAR
    ;; variable
    VAR
    ;;value
    VAL
    ;; application name for its section in *HAWS-SETTINGS*
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
;;to call HAWS-GETVAR.
(DEFUN
   HAWS-TESTGETVAR (VAR)
  (HAWS-GETVAR
    ;;variable
    VAR
    ;; application name for its section in *HAWS-SETTINGS*
    "test"
    ;; indicator file name for default location of ini or vanilla ini
    ;; (indicator file can be optionally a fully qualified path)
    "hawsedc.mnl"
    ;; ini section
    "Test"
    ;; Scope of setting.  Can be "a" (application)  "u" (user)  "p" (project) or "d" (drawing).  Determines where to store and get setting.
    "a"
    ;;Fallback defaults
    '
     (("1" "1val")
      ("2" "2val")
     )
   )
)

;; HAWS-INI
;; Finds INI file
;; Returns a fully qualified path, that folder is qualified to have
;; HAWSEDC.INI in it.
;; It should resolve all errors and user conditions
;; and return a "drive:\\...\\INIFOLDER" path to other functions.


;;; HAWS-INIFOLDER gets a valid INI folder.
;;; This function is wrong because there isn't a single *HAWS-INIFOLDER* that this function
;;; can throw around globally.
(DEFUN
   HAWS-INIFILE (APP SCOPE TESTFILE / INIFILE ASSUMEDINIFOLDER
                 ASSUMEDINIFILE
                )
  (cond
    ((not testfile)(setq testfile (strcat app ".cui")))
  )
  (COND
    ;; Project already defined this session
    ;; (Assume it's valid.  Calling function should init project
    ;; if there's been a chance of loss.)
    ((CDR
       (ASSOC
         0
         (CDR (ASSOC APP (CDR (ASSOC SCOPE *HAWS-SETTINGS*))))
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
          (HAWS-FILENAME-DIRECTORY ASSUMEDINIFOLDER)
       )
       (SETQ
         ASSUMEDINIFILE
          (FINDFILE (STRCAT ASSUMEDINIFOLDER APP ".ini"))
       )
     )
     ASSUMEDINIFILE
    )
    ;;Or make an ini file in folder with testfile in it or in proj folder
    ((HAWS-GETINIDEFAULTS ASSUMEDINIFOLDER) ASSUMEDINIFOLDER)
  )
)


;;Gets all settings from an inifile if it can.
(DEFUN
   HAWS-GETSETTINGS (INIFILE TESTFILE APPORPROJ)
  (SETQ
    *HAWS-SETTINGS*
     (INI_READINI
       (HAWS-INI (HAWS-INIFOLDER INIFILE TESTFILE))
     )
  )
)

;;Sets a variable in the global lisp list and in HAWSEDC.INI
(DEFUN
   HAWS-SETVAR (INIFILE INISECTION VAR VAL / SETTING)
  ;; Call GETVAR before setting var.  Why?  To populate *HAWS-SETTINGS*?
  (HAWS-GETVAR
    VAR INISECTION INIFILE TESTFILE APPORPROJ DEFAULTS
   )
  (HAWS-ADDVARTOLIST VAR VAL INISECTION INIFILE)
  (INI_WRITEENTRY
    (HAWS-INI (HAWS-INIFOLDER))
    INISECTION
    SETTING
    VAL
  )                                     ; _
)

;; HAWS-GETVAR
;; HAWS-GETVAR is called by wrapper functions like HCNM-GETVAR or HAWS-EDCGETVAR
;; It gets a variable without opening a file if it can.
;; (Higher calling functions may use functions like HCNM-PROJINIT or
;; HAWS-REMOVESETTINGS to remove settings and force a file
;; read.)
;; HAWS-GETVAR gets a program setting from
;; 1. The global *HAWS-SETTINGS* list if found
;; 2. An ini file or other location
;; 3. reverts to a default value without fail
;;
;; INIFILE is an ini filename. Ini file might not be used for a given scope in the current strategy.  
;; If there is no such ini file found and is needed, haws-getvar creates it.
;; If haws-getvar can't create the file, it sends an alert.
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
(DEFUN
   HAWS-GETVAR (VAR SECT APP SCOPE TESTFILE DEFAULTS /
                ADDTOLIST ADDTOINI DIR INI SETTING VAL
               )
  (SETQ
    ;; Does the variable need to be added to the *HAWS-SETTINGS* list? Assume yes initially.
    ADDTOLIST
     T
    ;; Does the variable need to be added to the appropriate ini file? Assume yes initially
    ADDTOINI
     T
  )
  ;;Get var list if no var list
  (IF (NOT *HAWS-SETTINGS*)
    (HAWS-GETSETTINGS)
  )
  (COND
    ;;Try getting from list
    ((SETQ
       VAL
        (HAWS-VARTOVAL
          VAR
          (CADR
            (ASSOC
              SECT
              (CADDR (ASSOC APP *HAWS-SETTINGS*))
            )
          )
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
          (FINDFILE
            (STRCAT (HAWS-FILENAME-DIRECTORY DIR) "\\" APP)
          )
       )
       (SETQ VAL (INI_READENTRY INI SECT SETTING))
     )
    )
    ;;Use default if there is one
    ((SETQ VAL (HAWS-VARTOVAL VAR DEFAULTS)))
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
    (HAWS-ADDVARTOLIST VAR VAL SECT APP)
  )
  (IF ADDTOINI
    (INI_WRITEENTRY
      (HAWS-INI (HAWS-INIFOLDER))
      APP
      SETTING
      VAL
    )
  )
  VAL
)

(DEFUN
   HAWS-ADDVARTOLIST (VAR VAL INISECTION INIFILE)
  (SETQ
    SETTING
     (HAWS-VARTOSETTING VAR)
    *HAWS-SETTINGS*
     (SUBST
       (SUBST
         (SUBST
           (LIST SETTING VAL)
           (ASSOC
             INISETTING
             (ASSOC
               INISECTION
               (ASSOC INIFILE *HAWS-SETTINGS*)
             )
           )
           (ASSOC
             INISECTION
             (ASSOC INIFILE *HAWS-SETTINGS*)
           )
         )
         (ASSOC INISECTION (ASSOC FILE *HAWS-SETTINGS*))
         (ASSOC INIFILE *HAWS-SETTINGS*)
       )
       (ASSOC INIFILE *HAWS-SETTINGS*)
       *HAWS-SETTINGS*
     )
  )
)


;;Gets an entire ini file from app folder
;;or else writes defaults to a fresh ini.
;;Doesn't add to existing ini.
;;Returns ini file name.
(DEFUN
   HAWS-GETINIDEFAULTS (PROJ / APP APPINI PROJINI)
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
       (SETQ APP (FINDFILE "cnm.mnl"))
       (SETQ
         APPINI
          (FINDFILE
            (STRCAT (HAWS-FILENAME-DIRECTORY APP) "\\" "cnm.ini")
          )
       )
       (HAWS-FILE-COPY APPINI PROJINI)
     )
     (WHILE (NOT (FINDFILE PROJINI)))
     PROJINI
    )
    (T
     (SETQ F2 (OPEN PROJINI "w"))
     (PRINC
       "[CNM]
ProjectNotes=constnot.txt
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
LineSpacing=1.5
NoteSpacing=3
NumberToDescriptionWidth=2.5
DescriptionToQuantityWidth=56
QuantityToQuantityWidth=9
QuantityToUnitsWidth=1
ShowKeyTableGrid=0
ShowKeyTableQuantities=1
BubbleHooks=Yes
BubbleLeaderConnectOsnap=mid,end
ImportLayerSettings=No
"      F2
     )
     (SETQ F2 (CLOSE F2))
     PROJINI
    )
  )
)

;;Saves *HAWS-SETTINGS* to the requested inifile
(DEFUN
   HAWS-SAVESETTINGSTOINI (INIFILE TESTFILE APPORPROJ)
  (INI_WRITESECTION
    (HAWS-INI (HAWS-INIFOLDER INIFILE TESTFILE))
    INIFILE
    *HAWS-SETTINGS*
  )
)


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


;;Does nothing but strcat, since the existence of the file is
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
       ;;Project already defined this session
       ;;(Assume it's valid.  Calling function should init project if there's been a chance of loss.)
       (*HCNM-CNMPROJECTROOT*)
       ;;Well-formed simple (single-folder) projects. CNM.INI is here.
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
          (FINDFILE
            (SETQ
              PROJTXT
               (STRCAT
                 DWGDIR
                 "\\cnmproj.txt"
               )
            )
          )
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
        PROJROOT
       )
       ;;Old style projects.
       ((SETQ PROJROOT (HCNM-PROJ4))
        (HCNM-GETINIDEFAULTS PROJROOT)
        PROJROOT
       )
       ;;Make a project in this drawing's folder.
       ((HCNM-GETINIDEFAULTS DWGDIR) DWGDIR)
     )
  )
)

;;Converts settings from version 4.1 project management scheme.
;;Returns project root if found.  Otherwise nil.
;;Doesn't set *hcnm-cnmsettings* or *HCNM-CNMPROJECTNOTES*
(DEFUN
   HCNM-PROJ4 (/ PROJNOTES DWGPREF I PROJROOT RDLIN STRTEMP TFNAME)
  (SETQ
    PROJROOT
     (COND
       ;;First choice is DWGPREFIX, whether there be an actual CONSTNOT.TXT
       ;;or a pointer file
       ((SETQ
          F1 (OPEN
               (SETQ
                 TFNAME
                  (STRCAT
                    (SETQ DWGPREF (GETVAR "dwgprefix"))
                    "constnot.txt"
                  )
               )
               "r"
             )
        )
        (SETQ I 0)
        (WHILE (AND
                 (< (SETQ I (1+ I)) 2)
                 (SETQ RDLIN (READ-LINE F1))
               )
          (IF (SETQ PROJNOTES (FINDFILE RDLIN))
            (SETQ I 10)
          )
        )
        (SETQ F1 (CLOSE F1))
        ;;If it had a filepath in it, that's the projnotes, otherwise the file itself is it.
        (COND
          (PROJNOTES
           (HCNM-MAKEPROJTXT
             (SETQ PROJROOT (HAWS-FILENAME-DIRECTORY PROJNOTES))
             DWGPREF
           )
           PROJROOT
          )
          (TFNAME (HAWS-FILENAME-DIRECTORY TFNAME))
        )
       )
       ;;Next choice is to look upward in the path.
       ;;If found, use.
       ((WHILE (AND
                 (< 1
                    (STRLEN
                      (COND
                        (STRTEMP)
                        ((SETQ STRTEMP DWGPREF))
                      )
                    )
                 )
                 (NOT PROJNOTES)
               )
          (SETQ
            STRTEMP
             (SUBSTR STRTEMP 1 (1- (STRLEN STRTEMP)))
            TFNAME
             (STRCAT STRTEMP "constnot.txt")
          )
          (SETQ PROJNOTES (FINDFILE TFNAME))
        )
        (COND
          (PROJNOTES
           (HCNM-MAKEPROJTXT
             (SETQ PROJROOT (HAWS-FILENAME-DIRECTORY PROJNOTES))
             (HAWS-FILENAME-DIRECTORY DWGPREF)
           )
           PROJROOT
          )
        )
       )
     )
  )
  PROJROOT
)

;;as posted the autodesk discussion customization group by Tony Tanzillo
(DEFUN
   ALE_BROWSEFORFOLDER
   (PRMSTR IOPTNS DEFFLD / SHLOBJ FOLDER FLDOBJ OUTVAL)
  (VL-LOAD-COM)
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
                                        ;  (SETQ PROJ (API_BROWSE "Select CNM Project Folder" DWGDIR))
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
(DEFUN
   HAWS-ATOFX (S WC OPT / X)
  (SETQ X (CADR (HAWS-EXTRACTX S WC OPT)))
  (IF X
    (ATOF X)
    0.0
  )
)


;;
;; HAWS-icad-P
;;
;;Tests whether intellicad behavior is current.
(DEFUN
   HAWS-ICAD-P ()
  (OR *HAWS-ICADMODE* (SETQ *HAWS-ICADMODE* (WCMATCH (GETVAR "acadver") "*i,*Bricscad")))
)

;;; Distofx extracts a real number from a text string when text before
;;; or
;;; after
;;the number matches a give wild card spec.  Requires EXTRACTX.
;;Type 0 tries to match the wild cards with text preceding a number.
;;Type 1 tries to match the wild cards with text following a number
;;Returns nil if search unsuccesful
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
(DEFUN
   HAWS-ENDSTR (S I L)
  (SUBSTR S (1+ (- (MAX (STRLEN S) 1) I)) L)
)

;;Extract used to extract numerical info from a text string.
;;Ignores commas in numbers.  Not international compatible.
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
(DEFUN
   HAWS-FILE-COPY (SOURCE DESTINATION / RDLIN RETURN)
  (COND
    ((HAWS-VLISP-P) (VL-FILE-COPY SOURCE DESTINATION))
    (T
     (IF (SETQ F1 (OPEN SOURCE "r"))
       (SETQ RETURN 1)
     )
     (IF (AND RETURN (SETQ F2 (OPEN DESTINATION "w")))
       (SETQ RETURN 2)
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
(DEFUN HAWS-GETDISTX (GX-POINT1 GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE)
  (CAR (HAWS-GETDISTPOINT GX-POINT1 GX-PROMPT GX-DEFAULTVALUE GX-INITIALVALUE))
)
;;;HAWS-GETDISTPOINT
;;;Returns a distance, the endpoint of the distance, and the bulge used for the distance.
;;;'(distance endpoint bulge)
(DEFUN
   HAWS-GETDISTPOINT (GX-POINT1 GX-PROMPT GX-DEFAULTVALUE
                      GX-INITIALVALUE / GX-POINT2 GX-POINT3 GX-BULGE
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
            " [Arc]"
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
       (  ;;point2 was not entered
          (NOT GX-POINT2)
        ;;then return the default value.
        GX-DEFAULTVALUE
       )
       ;;Else if point2 isn't "Arc"
       ((/= "Arc" GX-POINT2)
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
       (IF (= (GETKWORD "File already exists.  Overwrite?<Y/N>:")
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

;;; HAWS-LOAD
;;; loads a vlx, fas, or lsp, in that preferred order (AutoLISP
;;; default),
;;; from the folder that contains this file edclib.lsp
;;;
(DEFUN
   HAWS-LOAD (FILENAME)
  ;;Make sure app folder is set.
  (IF (NOT *HAWS-APPFOLDER*)
    (SETQ
      *HAWS-APPFOLDER*
       (HAWS-FILENAME-DIRECTORY
         (FINDFILE "cnm.mnl")
       )
    )
  )
  (IF (= "failed"
         (LOAD (STRCAT *HAWS-APPFOLDER* "\\" FILENAME) "failed")
      )
    (LOAD FILENAME)
  )
)

;;; HAWS-LOG
;;; Writes a message to a log file including the username and timestamp
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
(DEFUN
   HAWS-MILEPOST (MESSAGESTRING)
  (IF (> *HAWS-DEBUGLEVEL* 0)
    (PRINC (STRCAT "\nHawsEDC debug message: " MESSAGESTRING))
  )
)

;;MKFLD sub-function makes a field string out of a string.
;;If format
;;Usage: (mkfld
;;         [string to place into a field]
;;         [uniform field width or field delimiter character]
;;       )
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
;; Usage: (mklayr (list "laname" "lacolr" "laltyp"))
;; Use empty quotes for default color and linetype (eg. (mklay (list "AZ" "" ""))
(DEFUN
   HAWS-GETLAYR (KEY / TEMP)
  (DEFUN
     HAWS-GETUSL (/ RDLIN TEMP)
    (SETQ TEMP (FINDFILE "layers.dat"))
    (IF (NOT TEMP)
      ((PROMPT "\nLayer settings file not found.") (EXIT))
    )
    (SETQ F3 (OPEN TEMP "r"))
    (WHILE (SETQ RDLIN (READ-LINE F3))
      (IF (= 'LIST (TYPE (SETQ TEMP (READ RDLIN))))
        (SETQ *HAWS:LAYERS* (CONS TEMP *HAWS:LAYERS*))
      )
    )
    (SETQ F3 (CLOSE F3))
  )
  (IF (OR (NOT *HAWS:LAYERS*)
          (COND
            ((= (HCNM-GETVAR "ImportLayerSettings") "Yes")
             (HCNM-SETVAR "ImportLayerSettings" "No")
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
(DEFUN
   HAWS-MKLAYR (LAOPT / LANAME LACOLR LALTYP LTFILE)
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
    LTFILE "acad"
  )
  (IF (NOT (OR (= LALTYP "") (TBLSEARCH "LTYPE" LALTYP)))
    (PROGN
      (COMMAND "._linetype" "l" LALTYP "acad")
      (COMMAND)
      (COMMAND "._linetype" "l" LALTYP "hawsedc")
      (COMMAND)
    )
  )
  (WHILE (NOT (OR (= LALTYP "") (TBLSEARCH "LTYPE" LALTYP)))
    (ALERT
      (STRCAT
        "AutoCAD could not find "
        LALTYP
        " linetype in the specified file.\nPlease follow prompts to try a different linetype or file."
      )
    )
    (SETQ
      LALTYP
       (HAWS-GETSTRINGX
         "\nEnter substitute linetype name or enter to try another file"
         LALTYP
         LALTYP
       )
    )
    (COMMAND "._linetype" "l" LALTYP LTFILE)
    (COMMAND)
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
    (COMMAND "._linetype" "l" LALTYP LTFILE)
    (COMMAND)
  )
  (COMMAND "._layer")
  (IF (NOT (TBLSEARCH "LAYER" LANAME))
    (COMMAND "m" LANAME)
    (COMMAND "t" LANAME "on" LANAME "u" LANAME "s" LANAME)
  )
  (IF (/= LACOLR "")
    (COMMAND "c" LACOLR "")
  )
  (IF (/= LALTYP "")
    (COMMAND "lt" LALTYP "")
  )
  (COMMAND "")
  LAOPT
)

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
      (CONS
        40
        (COND
          (H)
          ((* (GETVAR "dimscale") (GETVAR "dimtxt")))
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
;;HAWS-PRIN1-TO-STRING
;;
;; For Intellicad, a cheap (and dirty, leaves a file on disk)
;; VL-PRIN1-TO-STRING
;; substitute
;;
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


(defun haws-set_tile_list (KeyName$ ListName@ Selected / Item)
  (start_list KeyName$ 3)
  (mapcar 'add_list ListName@)
  (end_list)
  (foreach Item (if (listp Selected) Selected (list Selected))
   (if (member Item ListName@)
     (set_tile KeyName$ (itoa (- (length ListName@) (length (member Item ListName@)))))
   );if
  );foreach
);defun set_tile_list

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
(DEFUN
   HAWS-STRTOLST (INPUTSTRING FIELDSEPARATORWC TEXTDELIMITER
                  EMPTYFIELDSDOCOUNT / CHARACTERCOUNTER CONVERSIONISDONE
                  CURRENTCHARACTER CURRENTFIELD CURRENTFIELDISDONE
                  PREVIOUSCHARACTER RETURNLIST TEXTMODEISON
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
       (IF (NOT TEXTMODEISON)
         (SETQ TEXTMODEISON T)
         (SETQ TEXTMODEISON NIL)
       )
       ;;2.  If this is the second consecutive TextDelimiter character, then
       (IF (= PREVIOUSCHARACTER TEXTDELIMITER)
         ;;Output it to CurrentField.
         (SETQ CURRENTFIELD (STRCAT CURRENTFIELD CURRENTCHARACTER))
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
(DEFUN
   HAWS-REGISTRY-READ (REG-KEY VAL-NAME)
  (COND
    ((HAWS-ICAD-P) NIL)
    ((VL-REGISTRY-READ REG-KEY VAL-NAME))
  )
)

;;Returns nil if in ICAD mode
(DEFUN
   HAWS-REGISTRY-WRITE (REG-KEY VAL-NAME VAL-DATA)
  (COND
    ((HAWS-ICAD-P) NIL)
    ((VL-REGISTRY-WRITE REG-KEY VAL-NAME VAL-DATA))
  )
)


;;Remove an element from a list
(DEFUN
   HAWS-REMOVE (ELEMENT LST)
  (APPEND
    (REVERSE (CDR (MEMBER ELEMENT (REVERSE LST))))
    (CDR (MEMBER ELEMENT LST))
  )
)

;;Convert a radian angle to a presentation quality bearing.
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
(DEFUN HAWS-ASIN (X) (ATAN X (SQRT (- 1 (* X X)))))
(DEFUN HAWS-ACOS (X) (ATAN (SQRT (- 1 (* X X))) X))
(DEFUN HAWS-TAN (X) (/ (SIN X) (COS X)))

(DEFUN
   HAWS-VSET (VLST)
  (FOREACH
     V VLST
    (IF (GETVAR (CAR V))
      (SETVAR (CAR V) (CADR V))
    )
  )
)

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

(DEFUN
   HAWS-VSAVE (VLST)
  (SETQ *HAWS-VSTR* '())
  (REPEAT (LENGTH VLST)
    (SETQ
      *HAWS-VSTR*
       (APPEND
         *HAWS-VSTR*
         (LIST (LIST (CAR VLST) (GETVAR (CAR VLST))))
       )
      VLST
       (CDR VLST)
    )
  )
)

(DEFUN
   HAWS-VRSTOR ()
  (REPEAT (LENGTH *HAWS-VSTR*)
    (SETVAR (CAAR *HAWS-VSTR*) (CADAR *HAWS-VSTR*))
    (SETQ *HAWS-VSTR* (CDR *HAWS-VSTR*))
  )
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
(DEFUN
   HAWS-TXLEN (STRING HEIGHT)
  (IF (HAWS-ICAD-P)
    (* HEIGHT (STRLEN STRING) 0.80)
    (CAADR (TEXTBOX (LIST (CONS 1 STRING) (CONS 40 HEIGHT))))
  )
)




;;
;; HAWS-VLISP-P
;;
;;Tests whether visual lisp functions are available.
(DEFUN
   HAWS-VLISP-P ()
  (NOT
    (OR (WCMATCH (GETVAR "acadver") "*i")
        (< (ATOF (GETVAR "acadver")) 15)
        *HAWS-ICADMODE*
    )
  )
)


;;end sub-functions

;;; Verify/Check values of authorization strings in registry at
;;; load-time
;;; against computer name and bios date.
;;; Delete registry entry if invalid for this computer.
;;; New scheme 2007-09:
;;; Getting a little more lax.
;;; If (HAWS-READCFG "/HawsEDC/Modules/package/OrderString") "",
;;; we assume quite trustingly that the application has never yet been tried.
(DEFUN
   CHECKREGISTRY (/ AUTHLIST AUTHSTRING TEMP)
  (HAWS-MILEPOST "Entering CheckRegistry")
  (FOREACH
     PACKAGE '(0 1 2 3)
    (SETQ AUTHSTRING (HAWS-READAUTHCODE PACKAGE))
    (COND
      (;;If
       (AND
         ;;Authstring is present
         AUTHSTRING
         ;;and
         (OR ;; it either is for the wrong computer
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
       ;;Then delete the order string and the auth string from storage.
       (HAWS-MILEPOST "About to WRITEPACKAGECODE")
       (HAWS-WRITEPACKAGECODE PACKAGE "OrderString" "")
       (HAWS-WRITEPACKAGECODE PACKAGE "AuthString" "")
      )
      ;;Else if there is no stored string
      ((NOT AUTHSTRING)
       ;; Then
       ;; assume it's a virgin installation,
       ;; and give a free trial by storing order and auth strings
       ;; for
       ;; a 30 day trial
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
             (HAWS-LISTTOBINARY (CONS (+ (CAR TEMP) 30) (CDR TEMP)))
           )
         )
       )
      )
    )
  )
  (HAWS-MILEPOST "Finished CheckRegistry")
)
(CHECKREGISTRY)


;;; Load other utilities
;; LISPUTIL.LSP has library functions for legacy routines some legacy users have.
(IF (NOT MKLAYR)
  (HAWS-LOAD "lisputil")
)
;; CNM.LSP has the HCNM-GETVAR function that is being called by
;; HAWS-MKLAYR (This is a messy, sloppy workaround.)
(IF (NOT HCNM-GETVAR)
  (HAWS-LOAD "cnm")
)
;;Can't autoload AH.LSP the normal way.  Load here.
(IF (NOT AH)
  (HAWS-LOAD "ah")
)
(PROMPT "\nloaded.")
 ;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
