;;; CNMInstall
;;; Installs the CNM menus and commands to AutoCAD
;;; Copyright 2017 Thomas Gail Haws
;;;
;;;
;;;
;;; This file licensed to the public under the terms of the GNU General Public License
;;; This file is Free Software. For more info read the license at fsf.org.
(VL-LOAD-COM)

(DEFUN
   HAWS-UPDATE-SUPPORTPATHS (PATHS / PATH)
  ;;Remove paths
  (FOREACH PATH PATHS (HAWS-REMOVE-SUPPORTPATH PATH))
  (FOREACH PATH PATHS (HAWS-ADD-SUPPORTPATH PATH))
)

(DEFUN
   HAWS-UPDATE-TRUSTEDPATHS (PATHS / PATH)
  ;;Remove paths
  (FOREACH PATH PATHS (HAWS-REMOVE-TRUSTEDPATH PATH))
  (FOREACH PATH PATHS (HAWS-ADD-TRUSTEDPATH PATH))
)

(DEFUN
   HAWS-RELOAD-MENUS (MODE / COUNTER ISALLMENUSREQUESTED ISMENUREMOVED MENUS-DONE MENUS-DONE-STRING MENUS-TO-DO MENUS-TO-DO-TEMP NMENUS
                      USERINPUT
                     )
  (SETQ
    MENUS-TO-DO
     '(
       ;;("File/group name" "Prompt")
       ("CNM" "Core CNM") 
     )
  )
  (COND
    ((SETQ MENUS-DONE (HAWS-REMOVE-MENUS MENUS-TO-DO))
     (SETQ MENUS-DONE-STRING (APPLY 'STRCAT (MAPCAR '(LAMBDA (X) (STRCAT "\n" (CAR X))) MENUS-DONE)))
     (ALERT
       (STRCAT "The following previously loaded CNM menus were found and unloaded.\n" MENUS-DONE-STRING)
     )
    )
  )
  (FOREACH
     GROUP MENUS-TO-DO
    (COND
      ((OR (= MODE "auto")
           (PROGN
             (INITGET "Yes No")
             (/= "No"
                 (SETQ
                   USERINPUT
                    (GETKWORD
                      (STRCAT
                        "\nLoad "
                        (CADR GROUP)
                        " menu? [Yes/No] <Yes>: "
                      )
                    )
                 )
             )
           )
       )
       ;; Load the menu.
       (VLAX-INVOKE-METHOD
         (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS)
         'LOAD
         (FINDFILE (STRCAT (CAR GROUP) ".cuix"))
       )
      )
    )
  )
)

(DEFUN
   HAWS-REMOVE-MENUS (MENUS-TO-DO / COUNTER ISMENUREMOVED MENUS-DONE NMENUS)
  (FOREACH
     GROUP MENUS-TO-DO
    (SETQ
      COUNTER -1
      ISMENUREMOVED NIL
      NMENUS
       (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS) 'COUNT)
    )
    ;;Loop through loaded menus to find and unload this menu.
    (WHILE (AND (< (SETQ COUNTER (1+ COUNTER)) NMENUS) (NOT ISMENUREMOVED))
      (COND
        ((= (STRCASE
              (VLAX-GET-PROPERTY
                (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS) 'ITEM COUNTER)
                'NAME
              )
            )
            (STRCASE (CAR GROUP))
         )
         (VLAX-INVOKE-METHOD
           (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS) 'ITEM COUNTER)
           'UNLOAD
         )
         (SETQ
           ISMENUREMOVED T
           NMENUS (1- NMENUS)
           MENUS-DONE
            (CONS GROUP MENUS-DONE)
         )
        )
      )
    )
  )
  MENUS-DONE
)


(DEFUN
   HAWS-ACAD-OBJECT ()
  (COND
    (*ACAD-OBJECT*)
    (T (SETQ *ACAD-OBJECT* (VLAX-GET-ACAD-OBJECT)))
  )
)

(DEFUN
   HAWS-ADD-SUPPORTPATH (NEWSTRING / FILES)
  (SETQ FILES (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'PREFERENCES) 'FILES))
  (VLAX-PUT-PROPERTY FILES 'SUPPORTPATH (STRCAT NEWSTRING ";" (VLAX-GET-PROPERTY FILES 'SUPPORTPATH)))
)

(DEFUN
   HAWS-REMOVE-SUPPORTPATH (STRING / FILES NEWSUPPORTPATH OLDSUPPORTPATH POSITION)
  (SETQ
    FILES
     (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'PREFERENCES) 'FILES)
    OLDSUPPORTPATH
     (VLAX-GET-PROPERTY FILES 'SUPPORTPATH)
    NEWSUPPORTPATH
     (VL-STRING-SUBST "" (STRCASE STRING) (STRCASE OLDSUPPORTPATH))
  )
  (COND
    ((= NEWSUPPORTPATH (STRCASE OLDSUPPORTPATH))
     NIL
     )
    (T (VLAX-PUT-PROPERTY
         FILES
         'SUPPORTPATH
         (VL-STRING-SUBST "" STRING OLDSUPPORTPATH)
       )
    )
  )
)

(DEFUN
   HAWS-ADD-TRUSTEDPATH (NEWSTRING / FILES)
  (SETVAR "TRUSTEDPATHS" (STRCAT NEWSTRING ";" (GETVAR "TRUSTEDPATHS")))
)

(DEFUN
   HAWS-REMOVE-TRUSTEDPATH (STRING / OLDTRUSTEDPATH)
  (SETQ OLDTRUSTEDPATH (GETVAR "TRUSTEDPATHS"))
  (SETVAR "TRUSTEDPATHS" (VL-STRING-SUBST "" STRING OLDTRUSTEDPATH))
)

(DEFUN
  HAWS-INSTALL-PENDING-READ ()
  (VL-REGISTRY-READ "HKEY_CURRENT_USER\\Software\\HawsEDC" "InstallPending")
)

(DEFUN
  HAWS-INSTALL-PENDING-DELETE ()
  (VL-REGISTRY-DELETE "HKEY_CURRENT_USER\\Software\\HawsEDC" "InstallPending")
)

(DEFUN
   HAWS-CNMINSTALL (CNMPATH / PATHS)
  ;; Add paths in reverse order (top path at bottom)
  (SETQ
    PATHS (LIST CNMPATH)
    ;; MODE can be "auto" or "manual"
    MODE "manual"
  )
  (COND
    ((OR (= MODE "manual") (HAWS-INSTALL-PENDING-READ))
     (HAWS-UPDATE-SUPPORTPATHS PATHS)
     (HAWS-UPDATE-TRUSTEDPATHS PATHS)
     (HAWS-RELOAD-MENUS "auto")
     (LOAD "cnmloader")
     (C:HAWS-ALIASMANAGE)
     (ALERT (PRINC "\nPlease read:\n\n1. Use the CAM command to activate CNM keyboard aliases.\n2. If you moved CNM from a previous location, you must restart AutoCAD."))
     (COND ((= MODE "auto") (HAWS-INSTALL-PENDING-DELETE)))
    )
  )
  (PRINC)
)
;|«Visual LISP© Format Options»
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil T)
;*** DO NOT add text below the comment! ***|;
