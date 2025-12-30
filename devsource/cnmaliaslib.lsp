(setq
  *hcnm-activation-preference*
   (atoi
     (haws-getvar "CNMAliasActivation")
   )
)
 
(defun hcnm-alias-activation-groups ()
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
     "Activate CNM's custom LISP changes to PGP definitions including C Copy and CC circle (not recommended; shown above in PGP format)"
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

(defun hcnm-alias-group (key)
  (assoc key (hcnm-alias-activation-groups))
)

(defun hcnm-alias-group-flag (group) (cadr group))
(defun hcnm-alias-group-message (group alias)
  (strcat
    "CNM "
    (caddr group)
    ": "
    (strcase (car alias))
    (if (= (cadddr alias) "")
      ""
      (strcat "\n" (cadddr alias))
    )
  )
)

(defun hcnm-define-lisp-alias (alias)
  (hcnm-define-alias alias "(c:" ")")
)
(defun hcnm-define-command-alias (alias)
  (hcnm-define-alias alias "(vl-cmdf \"._" "\")")
  (setq *hcnm-pgp-aliases* (cons alias *hcnm-pgp-aliases*))
)

(defun hcnm-define-alias (alias invocation-prefix invocation-suffix /)
  (hcnm-define-aliases
    (list alias)
    invocation-prefix
    invocation-suffix
  )
)

(defun hcnm-define-lisp-aliases ()
  (hcnm-define-aliases (hcnm-aliases) "(c:" ")")
)
(defun hcnm-define-command-aliases ()
  (hcnm-define-aliases
    (hcnm-pgp-aliases)
    "(vl-cmdf \"._"
    "\")"
  )
)

(defun hcnm-define-aliases (alias-list invocation-prefix invocation-suffix /
                        activation-preference alias
                        alias-activation-flag alias-activation-group
                       )
  (setq activation-preference *hcnm-activation-preference*)
  (cond
    ((> activation-preference 0)
     (foreach
        alias alias-list
       (setq
         alias-activation-group
          (hcnm-alias-group (caddr alias))
         alias-activation-flag
          (hcnm-alias-group-flag
            alias-activation-group
          )
       )
       (cond
         ((= (logand activation-preference alias-activation-flag)
             alias-activation-flag
          )
          (eval
            (read
              (strcat
                "(defun c:"
                (cadr alias)
                " () (princ \""
                (hcnm-alias-group-message alias-activation-group alias)
                "\")"
                invocation-prefix
                (car alias)
                invocation-suffix
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

(defun c:cnmalias () (c:haws-aliasmanage))
(defun c:haws-aliasmanage (/ alias-activation-flag activation-preference input1)
  (textpage)
  (princ
    (strcat
      "\n==================================================================================="
      "\nCNMAlias: Manage CNM Command Aliases"
      "\nStep 1. Choose which CNM alias groups to activate."
    )
  )
  (setq
    activation-preference
     (atoi
       (haws-getvar "CNMAliasActivation")
     )
  )
  (foreach
     group (hcnm-alias-activation-groups)
    (hcnm-print-pgp-style-aliases group)
    (initget "Yes No")
    (setq
      alias-activation-flag
       (cadr group)
      input1
       (getkword
         (strcat
           "\n"
           (nth 3 group)
           "? [Yes/No] <"
           (cond
             ((= (logand
                   activation-preference
                   alias-activation-flag
                 )
                 alias-activation-flag
              )
              "Yes"
             )
             (t "No")
           )
           ">: "
         )
       )
    )
    (cond
      (input1
       (cond
         ((= (logand activation-preference alias-activation-flag)
             alias-activation-flag
          )
          (setq
            activation-preference
             (- activation-preference
                alias-activation-flag
             )
          )
         )
       )
       (cond
         ((= input1
             "Yes")
             (setq
               activation-preference
                (+ activation-preference
                   alias-activation-flag
                )
             )
          )
         
       )
       (haws-setvar "CNMAliasActivation" (itoa activation-preference))
      )
    )
  )
  (cond
    ((progn
       (initget 1 "Yes No")
       (=
         (getkword
           "\nStep 2. Continue to edit individual CNMAlias.lsp aliases? [Yes/No]: "
         )
         "Yes"
       )
     )
     (startapp
       (strcat "\"notepad\" \"" (findfile "cnmalias.lsp") "\"")
     )
     (alert
       (strcat
         "CNMAlias.lsp has been opened in Notepad for you to edit. Follow the instructions inside.\n\nClick OK to load CNM aliases after editing and saving.\n\nAny previous aliases will remain active--overriding PGP aliases--until a new session is started."
       )
     )
    )
  )
  (load "cnmalias")
  (princ)
)
(defun hcnm-print-pgp-style-aliases (group / print-flag)
  (cond
    ((wcmatch (car group) "*pgp")
     (setq
       print-flag
        (hcnm-alias-group-flag group)
     )
     (princ
       (strcat
     "\n==================================================================================="
     "\n; "
     (nth 4 group)
         )
       )
     (foreach
        alias *hcnm-pgp-aliases*
       (cond
         ((= (hcnm-alias-group-flag (hcnm-alias-group (caddr alias)))
             print-flag
          )
          (princ (strcat "\n" (car alias) ",\t*" (cadr alias)))
         )
       )
     )
    )
  )
)
;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
