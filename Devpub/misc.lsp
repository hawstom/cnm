;;-------------------------EXTERNAL-------------------------
(defun c:haws-ffa ()
(haws-core-init 88) (princ(findfile (getstring "\nAutoCAD support file to find: ")))(princ))
(defun c:haws-hawsalias ()
(haws-core-init 89) (princ "\nLoading ")(load (princ (findfile "hawsalias.lsp")))(princ))
(defun c:haws-pgpedit ()
(haws-core-init 90) (princ "\nEditing ")(command "._notepad" (princ(findfile "acad.pgp"))) (alert (princ "\nClick OK to reinit PGP file after editing.")) (setvar "re-init" 16) (princ))
(defun c:haws-user ()
(haws-core-init 91) (princ "\n")(cond((princ (findfile "user.lsp"))(princ "\nloaded.")(load "user"))(T (princ " <-- user.lsp not found.")))(princ))
;;-------------------------END EXTERNAL-------------------------
