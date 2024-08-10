;;; Written by Thomas Gail Haws
;;; AET section
(DEFUN C:HAWS-ACRES ()
(haws-core-init 156) (HAWS-AET (/ 1.0 43560) " AC"))
(DEFUN C:HAWS-SF ()
(haws-core-init 157) (HAWS-AET 1 " SF"))
(DEFUN C:HAWS-AET ()
(haws-core-init 158) (HAWS-AET 1 ""))
(DEFUN C:HAWS-SM ()
(haws-core-init 159) (HAWS-AET (/ 1.0 27878400) " SQ. MI."))
(DEFUN C:HAWS-SY ()
(haws-core-init 160) (HAWS-AET (/ 1.0 9) " SY"))
(DEFUN
   HAWS-AET (FACTOR LABEL / AREA TS TXPT)
  (SETQ HAWS-QT-INSTANCE (HAWS-QT-NEW "haws-aet"))
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "type" "area")
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "factor" factor)
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "postfix" label)
  (HAWS-QT-SET-PROPERTY
    HAWS-QT-INSTANCE
    "precision"
    (GETVAR "luprec")
  )
  (HAWS-QT-STRING HAWS-QT-INSTANCE)
  (HAWS-QT-ADD-DRAWING-TEXT HAWS-QT-INSTANCE)
  (PRINC)
)
;;; ADL section
(DEFUN C:HAWS-ADL () (HAWS-ADL 1 "LF"))
(DEFUN
   HAWS-ADL (FACTOR LABEL / E SS LEN I HAWS-QT-INSTANCE)
  (HAWS-CORE-INIT 0)
  (SETQ HAWS-QT-INSTANCE (HAWS-QT-NEW "haws-adl"))
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "type" "length")
  (HAWS-QT-SET-PROPERTY
    HAWS-QT-INSTANCE
    "precision"
    (GETVAR "luprec")
  )
  (HAWS-QT-STRING HAWS-QT-INSTANCE)
  (HAWS-QT-ADD-DRAWING-TEXT HAWS-QT-INSTANCE)
  (PRINC)
)
;;; HAWS-QT "Class" library section

;; Constructs an instance of the HAWS-QT object if not already present
;; NAME is the string that identifies this instance.
;; type can be "area", "length"
;; destination can be "return", "prompt", "drawing", "text", "attribute"
;; action can be "replace", "prepend", "append"
(DEFUN
   HAWS-QT-NEW (NAME / INSTANCE)
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
       (CONS "string" "")
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
       (COND
         ((ASSOC
            PROPERTY
            (CDR
              (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
            )
          )
          ;; Update it if it exists
          (SUBST
            (CONS PROPERTY VALUE)
            (ASSOC
              PROPERTY
              (CDR
                (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
              )
            )
            (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
          )
         )
         (T
          ;; Add it if it doesn't exist
          (REVERSE
            (CONS
              (CONS PROPERTY VALUE)
              (REVERSE
                (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
              )
            )
          )
         )
       )
       (ASSOC INSTANCE *HAWS-QT-INSTANCES*)
       *HAWS-QT-INSTANCES*
     )
  )
)

(DEFUN
   HAWS-QT-GET-PROPERTY (INSTANCE PROPERTY)
  (CDR
    (ASSOC PROPERTY (CDR (ASSOC INSTANCE *HAWS-QT-INSTANCES*)))
  )
)

;; Gives user custom prompt for selection.
;; Follows HAWS-QT instance properties for formatting: factor, underline, overline, prefix, postfix, precision
;; Sets HAWS-QT instance string property
(DEFUN
   HAWS-QT-STRING (INSTANCE / SS TOTAL)
  (SETQ
    SS    (SSGET)
    TOTAL (HAWS-QT-SS-TOTAL INSTANCE SS)
  )
  (HAWS-QT-SET-PROPERTY
    INSTANCE
    "string"
    (STRCAT
      (COND
        ((HAWS-QT-GET-PROPERTY INSTANCE "underline_p")
         (HAWS-QT-GET-PROPERTY INSTANCE "underline_string")
        )
        ((HAWS-QT-GET-PROPERTY INSTANCE "overline_p")
         (HAWS-QT-GET-PROPERTY INSTANCE "overline_string")
        )
        ("")
      )
      (HAWS-QT-GET-PROPERTY INSTANCE "prefix")
      (RTOS
        (* TOTAL (HAWS-QT-GET-PROPERTY INSTANCE "factor"))
        2
        (HAWS-QT-GET-PROPERTY INSTANCE "precision")
      )
      (HAWS-QT-GET-PROPERTY INSTANCE "postfix")
    )
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
               (COND
                 ((= (CDR (ASSOC 0 (ENTGET E))) "AECC_PIPE")
                  (PRINC "\nGetting 3D length of AECC_PIPE object.")
                  (VLAX-GET-PROPERTY (VLAX-ENAME->VLA-OBJECT E) 'Length3D)
                 )
                 (T
                  (VLAX-CURVE-GETDISTATPARAM
                    E
                    (VLAX-CURVE-GETENDPARAM E)
                  )
                 )
               )
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

(DEFUN
   HAWS-QT-ADD-DRAWING-TEXT (INSTANCE / QT-STRING TS TXPT)
  (PRINC (STRCAT "\n"))
  (SETQ
    TXPT (GETPOINT
           (STRCAT
             "\nMiddle point for text \""
             (SETQ QT-STRING (HAWS-QT-GET-PROPERTY INSTANCE "string"))
             "\": "
           )
         )
    TS   (* (GETVAR "dimscale") (GETVAR "dimtxt"))
  )
  (IF TXPT
    (HAWS-MKTEXT "m" TXPT NIL 0 QT-STRING)
  )
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
