;;-------------------------EXTERNAL-------------------------
(defun c:haws-ffa ()
(haws-core-init 88) (princ(findfile (getstring "\nAutoCAD support file to find: ")))(princ))
(defun c:haws-pgpedit ()
(haws-core-init 90) (princ "\nEditing ")(vl-cmdf "._notepad" (princ(findfile "acad.pgp"))) (alert (princ "\nClick OK to reinit PGP file after editing.")) (setvar "re-init" 16) (princ))
(defun c:haws-user ()
(haws-core-init 91) (princ "\n")(cond((princ (findfile "user.lsp"))(princ "\nloaded.")(load "user"))(T (princ " <-- user.lsp not found.")))(princ))
;;-------------------------END EXTERNAL-------------------------
