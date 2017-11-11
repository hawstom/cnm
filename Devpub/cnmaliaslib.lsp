(defun
   hcnm-alias-activation-groups ()
  ;;("Key" Flag "Message" "Prompt")
  '
   (("Tools" 1 "custom tool" "cnm Tools")
    ("Custompgp" 2 "custom alias for AutoCAD command (change with CNMAlias)" "Custom command aliases")
    ("Standardpgp" 4 "verbatim alias for AutoCAD command (change with CNMAlias)" "Standard command aliases")
   )
)

(defun hcnm-alias-group (key) (assoc key (hcnm-alias-activation-groups)))
(defun hcnm-alias-group-flag (group) (cadr group))
(defun
   hcnm-alias-group-message (group alias)
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

(defun
   hcnm-define-lisp-alias (alias)
  (hcnm-define-alias (alias) "(c:" ")")
)
(defun
   hcnm-define-command-alias (alias)
  (hcnm-define-alias (alias) "(command \"._" "\")")
)

(defun
  hcnm-define-alias (alias invocation-prefix invocation-suffix /)
  (hcnm-define-aliases ((list alias) invocation-prefix invocation-suffix)
)

(defun
   hcnm-define-lisp-aliases ()
  (hcnm-define-aliases (hcnm-aliases) "(c:" ")")
)
(defun
   hcnm-define-command-aliases ()
  (hcnm-define-aliases (hcnm-pgp-aliases) "(command \"._" "\")")
)

(defun
   hcnm-define-aliases (alias-list invocation-prefix invocation-suffix / activation-preference alias alias-activation-flag alias-activation-group)
  (if (not *hcnm-activation-preference*) (setq *hcnm-activation-preference*(atoi (c:hcnm-config-getvar "CNMAliasActivation"))))
  (setq activation-preference *hcnm-activation-preference*)
  (cond
    ((> activation-preference 0)
     (foreach
        alias alias-list
       (setq
         alias-activation-group
          (hcnm-alias-group (caddr alias))
         alias-activation-flag
          (hcnm-alias-group-flag alias-activation-group)
       )
       (cond
         ((= (logand activation-preference alias-activation-flag) alias-activation-flag)
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

(defun c:cnmalias () (c:haws-aliasmanage)) ; This is the one hard-coded alias. Not in the aliases list.
(defun
   c:haws-aliasmanage (/ activation-preference input1)
  (textpage)
  (princ "\n;Standard AutoCAD aliases (for reference only)")
  (hcnm-print-pgp-style-aliases "Standardpgp")
  (princ "\n;Custom CNM aliases (copy these)")
  (hcnm-print-pgp-style-aliases "Custompgp")
  (princ
    (strcat
      "\n==================================================================================="
      "\nStep 1. Choose which CNM alias groups to activate. You can also"
      "\ncopy any of the aliases above to your ACAD.PGP."
    )
  )
  (setq activation-preference (atoi (c:hcnm-config-getvar "CNMAliasActivation")))
  (while (progn
             (initget
    (apply 'strcat (mapcar '(lambda (group) (strcat (car group) " ")) (hcnm-alias-activation-groups)))
  )
(setq
           input1
            (getkword
              (strcat
                "\nToggle CNM alias groups to activate as LISP shortcuts ["
                (substr
                  (apply
                    'strcat
                    (mapcar
                      '(lambda (group / alias-activation-flag)
                         (setq alias-activation-flag (cadr group))
                         (strcat
                           "/"
                           (nth 3 group)
                           (cond
                             ((= (logand activation-preference alias-activation-flag) alias-activation-flag) " (yes)")
                             (t " (no)")
                           )
                         )
                       )
                      (hcnm-alias-activation-groups)
                    )
                  )
                  2
                )
                "] <continue>: "
              )
            )
         ))
    (setq toggle-flag (cadr (assoc input1 (hcnm-alias-activation-groups)))
    activation-preference
    (cond
      ((= (logand activation-preference alias-activation-flag) alias-activation-flag)
       (- activation-preference toggle-flag)
      )
      (t (+ activation-preference toggle-flag))
    ))
  )
  (c:hcnm-config-setvar "CNMAliasActivation" (itoa activation-preference))
  (getstring "\nStep 2. <continue to edit individual CNMAlias.lsp aliases>: ")
  (startapp (strcat "\"notepad\" \"" (findfile "cnmalias.lsp") "\""))
  (alert
    (strcat
      "CNMAlias.lsp has been opened in Notepad for you to edit.\n\nClick OK to load CNM aliases after editing and saving."
    )
  )
  (load "cnmalias")
  (princ)
)
(defun hcnm-print-pgp-style-aliases (print-key / print-flag)
  (setq print-flag (hcnm-alias-group-flag (hcnm-alias-group print-key)))
  (foreach
     alias (hcnm-pgp-aliases)
    (cond ((= (hcnm-alias-group-flag (hcnm-alias-group (caddr alias))) print-flag) (princ (strcat "\n" (car alias) ",\t*" (cadr alias)))))
  )
)
