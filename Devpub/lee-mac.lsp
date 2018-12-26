;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; block-object-1 - [vla] VLA Dynamic Block Reference object
;; dynamic-property-name - [str] Dynamic Block property name (case-insensitive)

(DEFUN
   LM:GETDYNPROPVALUE (BLOCK-OBJECT-1
                       DYNAMIC-PROPERTY-NAME
                       /
                       DYNAMIC-PROPERTY-NAME-UPPER
                      )
  (SETQ DYNAMIC-PROPERTY-NAME-UPPER (STRCASE DYNAMIC-PROPERTY-NAME))
  (VL-SOME
    '(LAMBDA (X)
       (IF (= DYNAMIC-PROPERTY-NAME-UPPER
              (STRCASE (VLAX-GET X 'PROPERTYNAME))
           )
         (VLAX-GET X 'VALUE)
       )
     )
    (VLAX-INVOKE BLOCK-OBJECT-1 'GETDYNAMICBLOCKPROPERTIES)
  )
)


;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; block-object-1 - [vla] VLA Dynamic Block Reference object
;; dynamic-property-name - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(DEFUN
   LM:SETDYNPROPVALUE (BLOCK-OBJECT-1 DYNAMIC-PROPERTY-NAME VAL /
                       DYNAMIC-PROPERTY-NAME-UPPER
                      )
  (SETQ DYNAMIC-PROPERTY-NAME-UPPER (STRCASE DYNAMIC-PROPERTY-NAME))
  (VL-SOME
    '(LAMBDA (X)
       (IF (EQ DYNAMIC-PROPERTY-NAME-UPPER
               (STRCASE (VLAX-GET X 'PROPERTYNAME))
           )
         (PROGN
           (VLA-PUT-VALUE
             X
             (VLAX-MAKE-VARIANT
               VAL
               (VLAX-VARIANT-TYPE (VLA-GET-VALUE X))
             )
           )
           (COND
             (VAL)
             (T)
           )
         )
       )
     )
    (VLAX-INVOKE BLOCK-OBJECT-1 'GETDYNAMICBLOCKPROPERTIES)
  )
)

(defun LM:isAnnotative ( style / object annotx )
  (and
    (setq object (tblobjname "STYLE" style))
    (setq annotx (cadr (assoc -3 (entget object '("AcadAnnotative")))))
    (= 1 (cdr (assoc 1070 (reverse annotx))))
  )
)
