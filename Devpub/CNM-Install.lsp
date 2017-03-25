;;; CNMInstall
;;; Installs the CNM menus and commands to AutoCAD
;;; Copyright 2017 Thomas Gail Haws
;;;
;;;
;;;
;;; This file licensed to the public under the terms of the GNU General Public License
;;; This file is Free Software. For more info read the license at fsf.org.
(VL-LOAD-COM)
;; Use the snippet below to explore the AutoCAD object model
;; (SETQ ACADAPP (VLAX-GET-ACAD-OBJECT))
;; (VLAX-DUMP-OBJECT (VLAX-GET-PROPERTY ACADAPP 'PREFERENCES) T)
;; (setq aeccapp (vla-getinterfaceobject ACADAPP "AeccXUiLand.AeccApplication.6.0"))
;; (VLAX-DUMP-OBJECT AECCAPP T)

(DEFUN
   HAWS-UPDATE-SUPPORTPATHS (PATHS / PATH)
  ;;Remove paths
  (FOREACH PATH PATHS 
    (HAWS-REMOVE-SUPPORTPATH PATH)
  )
  (FOREACH PATH PATHS 
    (HAWS-ADD-SUPPORTPATH PATH)
  )
)

(DEFUN
   HAWS-UPDATE-TRUSTEDPATHS (PATHS / PATH)
  ;;Remove paths
  (FOREACH PATH PATHS 
    (HAWS-REMOVE-TRUSTEDPATH PATH)
  )
  (FOREACH PATH PATHS 
    (HAWS-ADD-TRUSTEDPATH PATH)
  )
)

(DEFUN
   HAWS-RELOAD-MENUS (/ ISALLMENUSREQUESTED ISMENUREMOVED USERINPUT
                         NMENUS CNMMENUS COUNTER GROUP
                        )
  (SETQ
    CNMMENUS
     '("CNM" "FunKy")
    ISALLMENUSREQUESTED NIL
  )
  (FOREACH
     GROUP CNMMENUS
    (SETQ
      COUNTER -1
      ISMENUREMOVED NIL
      NMENUS
       (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS) 'COUNT)
    )
    (COND
      ;;If user gives permission
      ((OR ISALLMENUSREQUESTED
           (PROGN
             (INITGET "Yes No All")
             (/= "No"
                 (SETQ
                   USERINPUT
                    (GETKWORD
                      (STRCAT
                        "\nLoad "
                        GROUP
                        " menu? [Yes/No/All] <Yes>: "
                      )
                    )
                 )
             )
           )
       )
       (COND ((= USERINPUT "All") (SETQ ISALLMENUSREQUESTED T)))
       ;;1.  Unload the menu if present
       ;;Loop through load menus to find and unload this menu.
       (WHILE (AND
                (< (SETQ COUNTER (1+ COUNTER)) NMENUS)
                (NOT ISMENUREMOVED)
              )
         (COND
           ((= (STRCASE
                 (VLAX-GET-PROPERTY
                   (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS) 'ITEM COUNTER)
				   'NAME
                 )
               )
               (STRCASE GROUP)
            )
            (VLAX-INVOKE-METHOD
              (VLAX-INVOKE-METHOD (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS) 'ITEM COUNTER)
			  'UNLOAD
            )
            (SETQ ISMENUREMOVED T)
           )
         )
       )
       ;;2.  Load the menu.
       (VLAX-INVOKE-METHOD 
         (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'MENUGROUPS)
		 'LOAD
         (FINDFILE (STRCAT GROUP ".mnu"))
       )
      )
    )
  )
)

(DEFUN HAWS-ALIASES-WIZARD ( / input-1)
  (INITGET "Yes No")
  (setq input-1 (getkword "\n(Recommended) Learn about activating legacy Haws PGP keyboard shortcuts and customizing CNM/HawsEDC command aliases? [Yes/No] <Yes>: "))
  (cond ((/= input-1 "No") (alert(princ "\nYou can do this later by using the Haws-AliasEdit (HAE) command. You can find this tip later near the top of README.TXT in the CNM folder.")) (load "hawsalias.lsp") (c:haws-aliasedit)))
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
  (VLAX-PUT-PROPERTY
    FILES
    'SUPPORTPATH
    (STRCAT NEWSTRING ";" (VLAX-GET-PROPERTY FILES 'SUPPORTPATH))
  )
)

(DEFUN
   HAWS-REMOVE-SUPPORTPATH (STRING / FILES OLDSUPPORTPATH POSITION)
  (SETQ
    FILES (VLAX-GET-PROPERTY (VLAX-GET-PROPERTY (HAWS-ACAD-OBJECT) 'PREFERENCES) 'FILES)
    OLDSUPPORTPATH
     (VLAX-GET-PROPERTY FILES 'SUPPORTPATH)
  )
  (VLAX-PUT-PROPERTY FILES 'SUPPORTPATH
    (VL-STRING-SUBST "" STRING OLDSUPPORTPATH)
  )
)

(DEFUN
   HAWS-ADD-TRUSTEDPATH (NEWSTRING / FILES)
  (SETVAR "TRUSTEDPATHS"
    (STRCAT NEWSTRING ";" (GETVAR "TRUSTEDPATHS"))
  )
)

(DEFUN
   HAWS-REMOVE-TRUSTEDPATH (STRING / FILES OLDSUPPORTPATH POSITION)
  (SETQ
    OLDTRUSTEDPATH
     (GETVAR "TRUSTEDPATHS")
  )
  (SETVAR "TRUSTEDPATHS"
    (VL-STRING-SUBST "" STRING OLDTRUSTEDPATH)
  )
)

(DEFUN
   HAWS-CNMINSTALL (CNMPATH / PATHS)
   ;; Add paths in reverse order (top path at bottom)
  (SETQ PATHS
    (LIST
      CNMPATH
    )
  )
  (HAWS-UPDATE-SUPPORTPATHS PATHS)
  (HAWS-UPDATE-TRUSTEDPATHS PATHS)
  (HAWS-RELOAD-MENUS)
  (LOAD "cnmloader")
  (HAWS-ALIASES-WIZARD)
)