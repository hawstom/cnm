(SETQ
  *HCNM-ACTIVATION-PREFERENCE*
   (ATOI
     (C:HCNM-CONFIG-GETVAR
       "CNMAliasActivation"
     )
   )
)
 
(DEFUN
   HCNM-ALIAS-ACTIVATION-GROUPS ()
  ;;("Key" Flag "Message" "Prompt" "Heading")
  '
   (("Tools"
     1
     "custom tool"
     "Activate keyboard shortcuts for CNM tools including LX etc. (recommended)"
     ""
    )
    ("Custompgp"
     2
     "custom alias for AutoCAD command (change with CNMAlias)"
     "Activate CNM's custom LISP changes to PGP definitions including C Copy and CC circle (suggested; shown above in PGP format)"
     "Custom Command Aliases"
    )
    ("Standardpgp"
     4
     "verbatim alias for AutoCAD command (change with CNMAlias)"
     "Activate verbatim LISP duplications of stock PGP definitions (not recommended; shown above in PGP format)"
     "Stock AutoCAD Command Aliases"
    )
   )
)

(DEFUN
   HCNM-ALIAS-GROUP (KEY)
  (ASSOC KEY (HCNM-ALIAS-ACTIVATION-GROUPS))
)

(DEFUN HCNM-ALIAS-GROUP-FLAG (GROUP) (CADR GROUP))
(DEFUN
   HCNM-ALIAS-GROUP-MESSAGE (GROUP ALIAS)
  (STRCAT
    "CNM "
    (CADDR GROUP)
    ": "
    (STRCASE (CAR ALIAS))
    (IF (= (CADDDR ALIAS) "")
      ""
      (STRCAT "\n" (CADDDR ALIAS))
    )
  )
)

(DEFUN
   HCNM-DEFINE-LISP-ALIAS (ALIAS)
  (HCNM-DEFINE-ALIAS ALIAS "(c:" ")")
)
(DEFUN
   HCNM-DEFINE-COMMAND-ALIAS (ALIAS)
  (HCNM-DEFINE-ALIAS ALIAS "(command \"._" "\")")
  (SETQ *HCNM-PGP-ALIASES* (CONS ALIAS *HCNM-PGP-ALIASES*))
)

(DEFUN
   HCNM-DEFINE-ALIAS (ALIAS INVOCATION-PREFIX INVOCATION-SUFFIX /)
  (HCNM-DEFINE-ALIASES
    (LIST ALIAS)
    INVOCATION-PREFIX
    INVOCATION-SUFFIX
  )
)

(DEFUN
   HCNM-DEFINE-LISP-ALIASES ()
  (HCNM-DEFINE-ALIASES (HCNM-ALIASES) "(c:" ")")
)
(DEFUN
   HCNM-DEFINE-COMMAND-ALIASES ()
  (HCNM-DEFINE-ALIASES
    (HCNM-PGP-ALIASES)
    "(command \"._"
    "\")"
  )
)

(DEFUN
   HCNM-DEFINE-ALIASES (ALIAS-LIST INVOCATION-PREFIX INVOCATION-SUFFIX /
                        ACTIVATION-PREFERENCE ALIAS
                        ALIAS-ACTIVATION-FLAG ALIAS-ACTIVATION-GROUP
                       )
  (SETQ ACTIVATION-PREFERENCE *HCNM-ACTIVATION-PREFERENCE*)
  (COND
    ((> ACTIVATION-PREFERENCE 0)
     (FOREACH
        ALIAS ALIAS-LIST
       (SETQ
         ALIAS-ACTIVATION-GROUP
          (HCNM-ALIAS-GROUP (CADDR ALIAS))
         ALIAS-ACTIVATION-FLAG
          (HCNM-ALIAS-GROUP-FLAG
            ALIAS-ACTIVATION-GROUP
          )
       )
       (COND
         ((= (LOGAND ACTIVATION-PREFERENCE ALIAS-ACTIVATION-FLAG)
             ALIAS-ACTIVATION-FLAG
          )
          (EVAL
            (READ
              (STRCAT
                "(defun c:"
                (CADR ALIAS)
                " () (princ \""
                (HCNM-ALIAS-GROUP-MESSAGE ALIAS-ACTIVATION-GROUP ALIAS)
                "\")"
                INVOCATION-PREFIX
                (CAR ALIAS)
                INVOCATION-SUFFIX
                "(princ))"
              )
            )
          )
         )
       )
     )
    )
  )
)

(DEFUN C:CNMALIAS () (C:HAWS-ALIASMANAGE))
(DEFUN
   C:HAWS-ALIASMANAGE (/ ALIAS-ACTIVATION-FLAG ACTIVATION-PREFERENCE INPUT1)
  (TEXTPAGE)
  (PRINC
    (STRCAT
      "\n==================================================================================="
      "\nCNMAlias: Manage CNM Command Aliases"
      "\nStep 1. Choose which CNM alias groups to activate."
    )
  )
  (SETQ
    ACTIVATION-PREFERENCE
     (ATOI
       (C:HCNM-CONFIG-GETVAR "CNMAliasActivation")
     )
  )
  (FOREACH
     GROUP (HCNM-ALIAS-ACTIVATION-GROUPS)
    (HCNM-PRINT-PGP-STYLE-ALIASES GROUP)
    (INITGET "Yes No")
    (SETQ
      ALIAS-ACTIVATION-FLAG
       (CADR GROUP)
      INPUT1
       (GETKWORD
         (STRCAT
           "\n"
           (NTH 3 GROUP)
           "? [Yes/No] <"
           (COND
             ((= (LOGAND
                   ACTIVATION-PREFERENCE
                   ALIAS-ACTIVATION-FLAG
                 )
                 ALIAS-ACTIVATION-FLAG
              )
              "Yes"
             )
             (T "No")
           )
           ">: "
         )
       )
    )
    (COND
      (INPUT1
       (COND
         ((= (LOGAND ACTIVATION-PREFERENCE ALIAS-ACTIVATION-FLAG)
             ALIAS-ACTIVATION-FLAG
          )
          (SETQ
            ACTIVATION-PREFERENCE
             (- ACTIVATION-PREFERENCE
                ALIAS-ACTIVATION-FLAG
             )
          )
         )
       )
       (COND
         ((= INPUT1
             "Yes")
             (SETQ
               ACTIVATION-PREFERENCE
                (+ ACTIVATION-PREFERENCE
                   ALIAS-ACTIVATION-FLAG
                )
             )
          )
         
       )
       (C:HCNM-CONFIG-SETVAR
         "CNMAliasActivation"
         (ITOA ACTIVATION-PREFERENCE)
       )
      )
    )
  )
  (COND
    ((PROGN
       (INITGET 1 "Yes No")
       (=
         (GETKWORD
           "\nStep 2. Continue to edit individual CNMAlias.lsp aliases? [Yes/No]: "
         )
         "Yes"
       )
     )
     (STARTAPP
       (STRCAT "\"notepad\" \"" (FINDFILE "cnmalias.lsp") "\"")
     )
     (ALERT
       (STRCAT
         "CNMAlias.lsp has been opened in Notepad for you to edit. Follow the instructions inside.\n\nClick OK to load CNM aliases after editing and saving.\n\nAny previous aliases will remain active--overriding PGP aliases--until a new session is started."
       )
     )
    )
  )
  (LOAD "cnmalias")
  (PRINC)
)
(DEFUN
   HCNM-PRINT-PGP-STYLE-ALIASES (GROUP / PRINT-FLAG)
  (COND
    ((WCMATCH (CAR GROUP) "*pgp")
     (SETQ
       PRINT-FLAG
        (HCNM-ALIAS-GROUP-FLAG GROUP)
     )
     (PRINC
       (STRCAT
     "\n==================================================================================="
     "\n; "
     (NTH 4 GROUP)
         )
       )
     (FOREACH
        ALIAS *HCNM-PGP-ALIASES*
       (COND
         ((= (HCNM-ALIAS-GROUP-FLAG (HCNM-ALIAS-GROUP (CADDR ALIAS)))
             PRINT-FLAG
          )
          (PRINC (STRCAT "\n" (CAR ALIAS) ",\t*" (CADR ALIAS)))
         )
       )
     )
    )
  )
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
