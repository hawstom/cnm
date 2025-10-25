;;; CNMInstall
;;; Installs the CNM menus and commands to AutoCAD
;;; Copyright 2017 Thomas Gail Haws
;;;
;;;
;;;
;;; This file licensed to the public under the terms of the GNU General Public License
;;; This file is Free Software. For more info read the license at fsf.org.
(vl-load-com)

(defun haws-update-supportpaths (paths / path)
  ;;Remove paths
  (foreach path paths (haws-remove-supportpath path))
  (foreach path paths (haws-add-supportpath path))
)

(defun haws-update-trustedpaths (paths / path)
  (cond
    ((getvar "TRUSTEDPATHS")
      (foreach path paths (haws-remove-trustedpath path))
      (foreach path paths (haws-add-trustedpath path))
    )
  )
)

(defun haws-reload-menus (mode / counter isallmenusrequested ismenuremoved menus-done menus-done-string menus-to-do menus-to-do-temp nmenus
                      userinput
                     )
  (setq
    menus-to-do
     '(
       ;;("File/group name" "Prompt")
       ("CNM" "Core CNM") 
     )
  )
  (cond
    ((setq menus-done (haws-remove-menus menus-to-do))
     (setq menus-done-string (apply 'STRCAT (mapcar '(lambda (x) (strcat "\n" (car x))) menus-done)))
     (alert
       (strcat "The following previously loaded CNM menus were found and unloaded.\n" menus-done-string)
     )
    )
  )
  (foreach
     group menus-to-do
    (cond
      ((or (= mode "auto")
           (progn
             (initget "Yes No")
             (/= "No"
                 (setq
                   userinput
                    (getkword
                      (strcat
                        "\nLoad "
                        (cadr group)
                        " menu? [Yes/No] <Yes>: "
                      )
                    )
                 )
             )
           )
       )
       ;; Load the menu.
       (vlax-invoke-method
         (vlax-get-property (haws-acad-object) 'MENUGROUPS)
         'LOAD
         (findfile (strcat (car group) ".cuix"))
       )
      )
    )
  )
)

(defun haws-remove-menus (menus-to-do / counter ismenuremoved menus-done nmenus)
  (foreach
     group menus-to-do
    (setq
      counter -1
      ismenuremoved nil
      nmenus
       (vlax-get-property (vlax-get-property (haws-acad-object) 'MENUGROUPS) 'COUNT)
    )
    ;;Loop through loaded menus to find and unload this menu.
    (while (and (< (setq counter (1+ counter)) nmenus) (not ismenuremoved))
      (cond
        ((= (strcase
              (vlax-get-property
                (vlax-invoke-method (vlax-get-property (haws-acad-object) 'MENUGROUPS) 'ITEM counter)
                'NAME
              )
            )
            (strcase (car group))
         )
         (vlax-invoke-method
           (vlax-invoke-method (vlax-get-property (haws-acad-object) 'MENUGROUPS) 'ITEM counter)
           'UNLOAD
         )
         (setq
           ismenuremoved t
           nmenus (1- nmenus)
           menus-done
            (cons group menus-done)
         )
        )
      )
    )
  )
  menus-done
)


(defun haws-acad-object ()
  (cond
    (*acad-object*)
    (t (setq *acad-object* (vlax-get-acad-object)))
  )
)

(defun haws-add-supportpath (newstring / files)
  (setq files (vlax-get-property (vlax-get-property (haws-acad-object) 'PREFERENCES) 'FILES))
  (vlax-put-property files 'SUPPORTPATH (strcat newstring ";" (vlax-get-property files 'SUPPORTPATH)))
)

(defun haws-remove-supportpath (string / files newsupportpath oldsupportpath position)
  (setq
    files
     (vlax-get-property (vlax-get-property (haws-acad-object) 'PREFERENCES) 'FILES)
    oldsupportpath
     (vlax-get-property files 'SUPPORTPATH)
    newsupportpath
     (vl-string-subst "" (strcase string) (strcase oldsupportpath))
  )
  (cond
    ((= newsupportpath (strcase oldsupportpath))
     nil
     )
    (t (vlax-put-property
         files
         'SUPPORTPATH
         (vl-string-subst "" string oldsupportpath)
       )
    )
  )
)

(defun haws-add-trustedpath (newstring / files)
  (setvar "TRUSTEDPATHS" (strcat newstring ";" (getvar "TRUSTEDPATHS")))
)

(defun haws-remove-trustedpath (string / oldtrustedpath)
  (setq oldtrustedpath (getvar "TRUSTEDPATHS"))
  (setvar "TRUSTEDPATHS" (vl-string-subst "" string oldtrustedpath))
)

(defun haws-install-pending-read ()
  (vl-registry-read "HKEY_CURRENT_USER\\Software\\HawsEDC" "InstallPending")
)

(defun haws-install-pending-delete ()
  (vl-registry-delete "HKEY_CURRENT_USER\\Software\\HawsEDC" "InstallPending")
)

(defun haws-cnminstall (cnmpath / paths)
  ;; Add paths in reverse order (top path at bottom)
  (setq
    paths (list cnmpath)
    ;; MODE can be "auto" or "manual"
    mode "manual"
  )
  (cond
    ((or (= mode "manual") (haws-install-pending-read))
     (haws-update-supportpaths paths)
     (haws-update-trustedpaths paths)
     (haws-reload-menus "auto")
     (load "cnmloader")
     ;;(C:HAWS-ALIASMANAGE)
     (alert (princ "\nPlease read:\n\n1. Use the CNMALIAS command to activate CNM keyboard aliases.\n2. If you moved CNM from a previous location, you must restart AutoCAD."))
     (cond ((= mode "auto") (haws-install-pending-delete)))
    )
  )
  (princ)
)
;|�Visual LISP� Format Options�
(72 2 40 2 nil "end of " 60 2 2 2 1 nil nil nil t)
;*** DO NOT add text below the comment! ***|;
