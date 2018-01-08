(SETQ *HCNM-PGP-ALIASES* NIL)

(DEFUN
   HCNM-ALIAS-ACTIVATION-GROUPS ()
  ;;("Key" Flag "Message" "Prompt")
  '
   (("Tools" 1 "custom tool" "cnm Tools")
    ("Custompgp"
     2
     "custom alias for AutoCAD command (change with CNMAlias)"
     "Custom command aliases"
    )
    ("Standardpgp"
     4
     "verbatim alias for AutoCAD command (change with CNMAlias)"
     "Standard command aliases"
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
  (IF (NOT *HCNM-ACTIVATION-PREFERENCE*)
    (SETQ
      *HCNM-ACTIVATION-PREFERENCE*
       (ATOI
         (C:HCNM-CONFIG-GETVAR
           "CNMAliasActivation"
         )
       )
    )
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
                                        ; This is the one hard-coded alias. Not in the aliases list.
(DEFUN
   C:HAWS-ALIASMANAGE (/ ACTIVATION-PREFERENCE INPUT1)
  (TEXTPAGE)
  (PRINC "\n;Standard AutoCAD aliases (for reference only)")
  (HCNM-PRINT-PGP-STYLE-ALIASES "Standardpgp")
  (PRINC "\n;Custom CNM aliases (copy these)")
  (HCNM-PRINT-PGP-STYLE-ALIASES "Custompgp")
  (PRINC
    (STRCAT
      "\n==================================================================================="
      "\nStep 1. Choose which CNM alias groups to activate. You can also"
      "\ncopy any of the aliases above to your ACAD.PGP."
    )
  )
  (SETQ
    ACTIVATION-PREFERENCE
     (ATOI
       (C:HCNM-CONFIG-GETVAR "CNMAliasActivation")
     )
  )
  (WHILE (PROGN
           (INITGET
             (APPLY
               'STRCAT
               (MAPCAR
                 '(LAMBDA (GROUP) (STRCAT (CAR GROUP) " "))
                 (HCNM-ALIAS-ACTIVATION-GROUPS)
               )
             )
           )
           (SETQ
             INPUT1
              (GETKWORD
                (STRCAT
                  "\nToggle CNM alias groups to activate as LISP shortcuts ["
                  (SUBSTR
                    (APPLY
                      'STRCAT
                      (MAPCAR
                        '(LAMBDA (GROUP / ALIAS-ACTIVATION-FLAG)
                           (SETQ ALIAS-ACTIVATION-FLAG (CADR GROUP))
                           (STRCAT
                             "/"
                             (NTH 3 GROUP)
                             (COND
                               ((= (LOGAND
                                     ACTIVATION-PREFERENCE
                                     ALIAS-ACTIVATION-FLAG
                                   )
                                   ALIAS-ACTIVATION-FLAG
                                )
                                " (yes)"
                               )
                               (T " (no)")
                             )
                           )
                         )
                        (HCNM-ALIAS-ACTIVATION-GROUPS)
                      )
                    )
                    2
                  )
                  "] <continue>: "
                )
              )
           )
         )
    (SETQ
      TOGGLE-FLAG
       (CADR (ASSOC INPUT1 (HCNM-ALIAS-ACTIVATION-GROUPS)))
      ACTIVATION-PREFERENCE
       (COND
         ((= (LOGAND
               ACTIVATION-PREFERENCE
               TOGGLE-FLAG
             )
             TOGGLE-FLAG
          )
          (- ACTIVATION-PREFERENCE TOGGLE-FLAG)
         )
         (T (+ ACTIVATION-PREFERENCE TOGGLE-FLAG))
       )
    )
  )
  (C:HCNM-CONFIG-SETVAR
    "CNMAliasActivation"
    (ITOA ACTIVATION-PREFERENCE)
  )
  (GETSTRING
    "\nStep 2. <continue to edit individual CNMAlias.lsp aliases>: "
  )
  (STARTAPP
    (STRCAT "\"notepad\" \"" (FINDFILE "cnmalias.lsp") "\"")
  )
  (ALERT
    (STRCAT
      "CNMAlias.lsp has been opened in Notepad for you to edit.\n\nClick OK to load CNM aliases after editing and saving."
    )
  )
  (LOAD "cnmalias")
  (PRINC)
)
(DEFUN
   HCNM-PRINT-PGP-STYLE-ALIASES (PRINT-KEY / PRINT-FLAG)
  (SETQ PRINT-FLAG (HCNM-ALIAS-GROUP-FLAG (HCNM-ALIAS-GROUP PRINT-KEY)))
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
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
