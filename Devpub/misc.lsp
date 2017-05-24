;;-------------------------EXTERNAL-------------------------
(defun c:haws-ffa () (princ(findfile (getstring "\nAutoCAD support file to find: ")))(princ))
(defun c:haws-hawsalias () (princ "\nLoading ")(load (princ (findfile "hawsalias.lsp")))(princ))
(defun c:haws-pgpedit () (princ "\nEditing ")(command "._notepad" (princ(findfile "acad.pgp")))(princ))
(defun c:haws-user () (princ "\n")(cond((princ (findfile "user.lsp"))(princ "\nloaded.")(load "user"))(T (princ " <-- user.lsp not found.")))(princ))
;;-------------------------END EXTERNAL-------------------------
