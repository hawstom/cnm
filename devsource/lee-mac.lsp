;; Get Dynamic Block Property Value  -  Lee Mac
;; Returns the value of a Dynamic Block property (if present)
;; block-object-1 - [vla] VLA Dynamic Block Reference object
;; dynamic-property-name - [str] Dynamic Block property name (case-insensitive)

(defun lm:getdynpropvalue (block-object-1
                       dynamic-property-name
                       /
                       dynamic-property-name-upper
                      )
  (setq dynamic-property-name-upper (strcase dynamic-property-name))
  (vl-some
    '(lambda (x)
       (if (= dynamic-property-name-upper
              (strcase (vlax-get x 'PROPERTYNAME))
           )
         (vlax-get x 'VALUE)
       )
     )
    (vlax-invoke block-object-1 'GETDYNAMICBLOCKPROPERTIES)
  )
)


;; Set Dynamic Block Property Value  -  Lee Mac
;; Modifies the value of a Dynamic Block property (if present)
;; block-object-1 - [vla] VLA Dynamic Block Reference object
;; dynamic-property-name - [str] Dynamic Block property name (case-insensitive)
;; val - [any] New value for property
;; Returns: [any] New value if successful, else nil

(defun lm:setdynpropvalue (block-object-1 dynamic-property-name val /
                       dynamic-property-name-upper
                      )
  (setq dynamic-property-name-upper (strcase dynamic-property-name))
  (vl-some
    '(lambda (x)
       (if (eq dynamic-property-name-upper
               (strcase (vlax-get x 'PROPERTYNAME))
           )
         (progn
           (vla-put-value
             x
             (vlax-make-variant
               val
               (vlax-variant-type (vla-get-value x))
             )
           )
           (cond
             (val)
             (t)
           )
         )
       )
     )
    (vlax-invoke block-object-1 'GETDYNAMICBLOCKPROPERTIES)
  )
)

(defun lm:isannotative ( style / object annotx )
  (and
    (setq object (tblobjname "STYLE" style))
    (setq annotx (cadr (assoc -3 (entget object '("AcadAnnotative")))))
    (= 1 (cdr (assoc 1070 (reverse annotx))))
  )
)

;;-------------------=={ UnFormat String }==------------------;;
;;                                                            ;;
;;  Returns a string with all MText formatting codes removed. ;;
;;------------------------------------------------------------;;
;;  Author: Lee Mac, Copyright © 2011 - www.lee-mac.com       ;;
;;------------------------------------------------------------;;
;;  Arguments:                                                ;;
;;  str - String to Process                                   ;;
;;  mtx - MText Flag (T if string is for use in MText)        ;;
;;------------------------------------------------------------;;
;;  Returns:  String with formatting codes removed            ;;
;;------------------------------------------------------------;;

(defun lm:unformat ( str mtx / _replace rx )

    (defun _replace ( new old str )
        (vlax-put-property rx 'pattern old)
        (vlax-invoke rx 'replace str new)
    )
    (if (setq rx (vlax-get-or-create-object "VBScript.RegExp"))
        (progn
            (setq str
                (vl-catch-all-apply
                    (function
                        (lambda ( )
                            (vlax-put-property rx 'global     actrue)
                            (vlax-put-property rx 'multiline  actrue)
                            (vlax-put-property rx 'ignorecase acfalse) 
                            (foreach pair
                               '(
                                    ("\032"    . "\\\\\\\\")
                                    (" "       . "\\\\P|\\n|\\t")
                                    ("$1"      . "\\\\(\\\\[ACcFfHLlOopQTW])|\\\\[ACcFfHLlOopQTW][^\\\\;]*;|\\\\[ACcFfHLlOopQTW]")
                                    ("$1$2/$3" . "([^\\\\])\\\\S([^;]*)[/#\\^]([^;]*);")
                                    ("$1$2"    . "\\\\(\\\\S)|[\\\\](})|}")
                                    ("$1"      . "[\\\\]({)|{")
                                )
                                (setq str (_replace (car pair) (cdr pair) str))
                            )
                            (if mtx
                                (_replace "\\\\" "\032" (_replace "\\$1$2$3" "(\\\\[ACcFfHLlOoPpQSTW])|({)|(})" str))
                                (_replace "\\"   "\032" str)
                            )
                        )
                    )
                )
            )
            (vlax-release-object rx)
            (if (null (vl-catch-all-error-p str))
                str
            )
        )
    )
)
