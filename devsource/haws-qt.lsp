;;; Written by Thomas Gail Haws
;;; AET section
(DEFUN C:HAWS-ACRES ()
  (HAWS-CORE-INIT 156)
  (HAWS-AET (/ 1.0 43560) " AC")
)
(DEFUN C:HAWS-SF () (HAWS-CORE-INIT 157) (HAWS-AET 1 " SF"))
(DEFUN HAWS_EVANGEL_MSG_PROMPT ()
  (PROMPT (STRCAT "\n" (HAWS_EVANGEL_MSG)))
)

(DEFUN C:HAWS-AET () (HAWS-CORE-INIT 158) (HAWS_EVANGEL_MSG_PROMPT) (HAWS-AET 1 ""))
(DEFUN C:HAWS-SM ()
  (HAWS-CORE-INIT 159)
  (HAWS-AET (/ 1.0 27878400) " SQ. MI.")
)
(DEFUN C:HAWS-SY ()
  (HAWS-CORE-INIT 160)
  (HAWS-AET (/ 1.0 9) " SY")
)
(DEFUN HAWS-AET (FACTOR LABEL / AREA TS TXPT)
  (SETQ HAWS-QT-INSTANCE (HAWS-QT-NEW "haws-aet"))
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "type" "area")
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "factor" FACTOR)
  (HAWS-QT-SET-PROPERTY HAWS-QT-INSTANCE "postfix" LABEL)
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
(DEFUN HAWS-ADL (FACTOR LABEL / E SS LEN I HAWS-QT-INSTANCE)
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
;; mode can be "ss", "nes"
;; destination can be "return", "prompt", "drawing", "text", "attribute"
;; action can be "replace", "prepend", "append"
;; space can be "mspace" or "pspace"
(DEFUN HAWS-QT-NEW (NAME / INSTANCE)
  (SETQ
    INSTANCE
     (LIST
       NAME
       (CONS "name" NAME)
       (CONS "mode" "ss")
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
       (CONS "space" NIL)
     )
  )
  (COND
    ((NOT (ASSOC NAME *HAWS-QT-INSTANCES*))
     (SETQ *HAWS-QT-INSTANCES* (CONS INSTANCE *HAWS-QT-INSTANCES*))
    )
  )
  NAME
)

(DEFUN HAWS-QT-SET-PROPERTY (INSTANCE PROPERTY VALUE)
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
  VALUE
)

(DEFUN HAWS-QT-GET-PROPERTY (INSTANCE PROPERTY)
  (CDR
    (ASSOC PROPERTY (CDR (ASSOC INSTANCE *HAWS-QT-INSTANCES*)))
  )
)

;; Gives user custom prompt for selection.
;; Follows HAWS-QT instance properties for formatting: factor, underline, overline, prefix, postfix, precision
;; Sets HAWS-QT instance string property
(DEFUN HAWS-QT-STRING (INSTANCE / SS TOTAL)
  (SETQ TOTAL (HAWS-QT-SS-TOTAL INSTANCE))
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
(DEFUN HAWS-QT-SS-TOTAL (INSTANCE / E ENAMELIST I Q QI QTYPE SMODE)
  (SETQ
    Q 0.0
    QTYPE
     (HAWS-QT-GET-PROPERTY INSTANCE "type")
    ENAMELIST
     (HAWS-QT-SELECT)
  )
  (COND
    (ENAMELIST
     (FOREACH E ENAMELIST (SETQ Q (HAWS-QT-ADD-ENT E Q QTYPE)))
    )
  )
  Q
)
(DEFUN HAWS-QT-SELECT (/ CONTINUE-P ENAMELIST INPUT-RESULTS LAYERLIST SS-P)
  (SETQ ENAMELIST '())
  (WHILE (NOT CONTINUE-P)
    (SETQ
      INPUT-RESULTS
       (COND
         (SS-P (HAWS-QT-SELECT-SSGET))
         (T (HAWS-QT-SELECT-MULTI-NENTSEL))
       )
      SS-P
       (CADR INPUT-RESULTS)
      CONTINUE-P
       (CADDR INPUT-RESULTS)
    )
    (COND
      ((CAR INPUT-RESULTS)
       (SETQ ENAMELIST (APPEND ENAMELIST (CAR INPUT-RESULTS)))
      )
    )
  )
  ENAMELIST
)
(DEFUN HAWS-QT-SELECT-SSGET (/ CONTINUE-P ENAMELIST INPUT1 SS-P)
  (SETQ
    INPUT1
     (SSGET)
    CONTINUE-P
     (NOT INPUT1)
    SS-P NIL
    ENAMELIST
     (HAWS-QT-SELECT-SS-TO-LIST INPUT1)
  )
  (LIST ENAMELIST SS-P CONTINUE-P)
)
(DEFUN HAWS-QT-SELECT-SS-TO-LIST (INPUT1 / EN I SS1 SSLIST)
  (COND
    ((AND INPUT1 (= (TYPE INPUT1) 'PICKSET))
     (SETQ
       SS1 INPUT1
       I   -1
     )
     (WHILE (SETQ EN (SSNAME SS1 (SETQ I (1+ I))))
       (SETQ SSLIST (CONS EN SSLIST))
     )
     SSLIST
    )
  )
)
(DEFUN HAWS-QT-SELECT-MULTI-NENTSEL
   (/ CONTINUE-P ENAMELIST INPUT1 SS-P NENTSEL-RESULTS)
  (INITGET "Selection Continue")
  (SETQ
    INPUT1
     (NENTSEL
       (STRCAT
         "\nSelect nested object or [Selection set/Continue] <Continue>: "
       )
     )
    CONTINUE-P
     (NOT INPUT1)
    SS-P
     (= INPUT1 "Selection")
    ENAMELIST
     (COND
       ((AND INPUT1 (= (TYPE INPUT1) 'LIST))
        (PRINC (STRCAT "\n" (CDR(ASSOC 0 (ENTGET (CAR INPUT1)))) " was selected."))
        (LIST (CAR INPUT1))
       )
       (T NIL)
     )
  )
  (LIST ENAMELIST SS-P CONTINUE-P)
)
(DEFUN HAWS-QT-ADD-ENT (E Q QTYPE / OBJ QI)
  (IF (AND
        (= (CDR (ASSOC 0 (ENTGET E))) "VERTEX")
        (ASSOC 330 (ENTGET E))
      )
    (SETQ E (CDR (ASSOC 330 (ENTGET E))))
  )
  (SETQ
    OBJ
     (VLAX-ENAME->VLA-OBJECT E)
    ERROBJ
     (VL-CATCH-ALL-APPLY 'VLAX-CURVE-GETSTARTPOINT (list OBJ))
    QI (COND
         ((VL-CATCH-ALL-ERROR-P ERROBJ)
          (PRINC
            (STRCAT
              "\nCan't get quantity from a "
              (CDR (ASSOC 0 (ENTGET E)))
              "."
            )
          )
          0
         )
         ((= QTYPE "length")
          (COND
            ((= (CDR (ASSOC 0 (ENTGET E))) "AECC_PIPE")
             (PRINC "\nGetting 3D length of AECC_PIPE object.")
             (VLAX-GET-PROPERTY OBJ 'LENGTH3D)
            )
            (T
             (VLAX-CURVE-GETDISTATPARAM
               OBJ
               (VLAX-CURVE-GETENDPARAM OBJ)
             )
            )
          )
         )
         ((= QTYPE "area") (VLAX-CURVE-GETAREA OBJ))
       )
    Q (COND
        (QI (+ Q QI))
        (Q)
      )
  )
  Q
)
(DEFUN HAWS-QT-ADD-DRAWING-TEXT (INSTANCE / QT-STRING TS TXPT)
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
;*** DO NOT add text below the comment! ***|
