(DEFUN C:HAWS-ADL () (HAWS-ADL 1 "LF"))
(DEFUN
   HAWS-ADL (FACTOR LABEL / E SS LEN I HAWS-QT-INSTANCE)
  (HAWS-CORE-INIT 0)
  (SETQ HAWS-QT-INSTANCE (HAWS-QT-NEW "haws-adl"))
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "type" "length")
  (SETQ
    SS  (SSGET)
    LEN (HAWS-QT-SS-TOTAL HAWS-QT-INSTANCE SS)
  )
  (COND
    (LEN
     (SETQ
       LEN  (STRCAT (RTOS (* LEN FACTOR) 2 (GETVAR "luprec")) " " LABEL)
       TXPT (GETPOINT "\nMiddle point for text:")
       TS   (* (GETVAR "dimscale") (GETVAR "dimtxt"))
     )
     (IF TXPT
       (HAWS-MKTEXT "m" TXPT NIL 0 LEN)
     )
     (PRINC (STRCAT "\n" LEN))
    )
  )
  (PRINC)
)

;; Constructs an instance of the HAWS-QT object
;; NAME is the string that identifies this instance.
;; type can be "area", "length"
;; destination can be "return", "prompt", "drawing", "text", "attribute"
;; action can be "replace", "prepend", "append"
(DEFUN
   HAWS-QT-NEW (NAME)
  (SETQ
    INSTANCE
     (LIST
       NAME
       (CONS "name" NAME)
       (CONS "type" "area")
       (CONS "factor" 1)
       (CONS "underline_string" "%%u")
       (CONS "underline_p" NIL)
       (CONS "overline_string" "%%o")
       (CONS "overline_p" NIL)
       (CONS "prefix" "")
       (CONS "postfix" "")
       (CONS "precision" 0)
       (CONS "destination" "prompt")
       (CONS "position" "replace")
     )
  )
  (COND
    ((NOT (ASSOC NAME *HAWS-QT-INSTANCES*))
     (SETQ *HAWS-QT-INSTANCES* (CONS INSTANCE *HAWS-QT-INSTANCES*))
    )
  )
  NAME
)

(DEFUN
   HAWS-QT-SET-PROPERTY (INSTANCE PROPERTY VALUE)
  (SETQ
    *HAWS-QT-INSTANCES*
     (SUBST
       (SUBST
         (cons property value)
         (assoc property (cdr (ASSOC INSTANCE *HAWS-QT-INSTANCES*)))
         (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
       )
       (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
       *HAWS-QT-INSTANCES*
     )
  )
)

(DEFUN HAWS-QT-GET-PROPERTY (INSTANCE PROPERTY)
  (CDR (ASSOC PROPERTY (CDR (ASSOC INSTANCE *HAWS-QT-INSTANCES*))))
)

;; Gives user custom prompt for selection.
;; Follows options for formatting: factor, underline, overline, prefix, postfix, precision
;; Returns formatted quantity text
(DEFUN HAWS-QT-STRING (INSTANCE / SS TOTAL)
  (SETQ
    SS  (SSGET)
    TOTAL (HAWS-QT-SS-TOTAL SS INSTANCE)
  )
 (strcat
   (cond
     ((HAWS-QT-GET-PROPERTY INSTANCE "underline_p")(HAWS-QT-GET-PROPERTY INSTANCE "underline_string"))
     ((HAWS-QT-GET-PROPERTY INSTANCE "overline_p")(HAWS-QT-GET-PROPERTY INSTANCE "overline_string"))
     ("")
   )
   (HAWS-QT-GET-PROPERTY INSTANCE "prefix")
   (rtos (* total (HAWS-QT-GET-PROPERTY INSTANCE "factor")) 2 (HAWS-QT-GET-PROPERTY INSTANCE "precision"))
   (HAWS-QT-GET-PROPERTY INSTANCE "postfix")
 )
  
)
;; Returns a real number in native units.
;; Valid types: "length", "area"
(DEFUN
   HAWS-QT-SS-TOTAL (INSTANCE SS / E I Q QI QTYPE)
  (SETQ
    Q 0.0
    QTYPE
     (HAWS-QT-GET-PROPERTY INSTANCE "type")
  )
  (COND
    (SS
     (SETQ I (SSLENGTH SS))
     (WHILE (>= (SETQ I (1- I)) 0)
       (SETQ
         E  (SSNAME SS I)
         QI (COND
              ((= QTYPE "length")
               (VLAX-CURVE-GETDISTATPARAM E (VLAX-CURVE-GETENDPARAM E))
              )
              ((= QTYPE "area") (VLAX-CURVE-GETAREA E))
            )
         Q  (COND
              (QI (+ Q QI))
              (Q)
            )
       )
     )
    )
  )
  Q
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
