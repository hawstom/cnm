;;-------------------------EXTERNAL-------------------------
(defun c:haws-ffa () (princ(findfile (getstring "\nAutoCAD support file to find: ")))(princ))
(defun c:haws-user () (load "user")(princ))
;;-------------------------END EXTERNAL-------------------------
