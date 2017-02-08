;Modified for AutoCAD 2000 by Thomas Gail Haws.  6/5/2000
(defun c:haws-LoadAndRun (/ funcname filename func)
  ;-----
  ;Get function name from user
  ;-----
  (setq funcname (strcase(getstring
    "\nEnter AutoLISP file/function name: "))
    filename (strcat funcname ".LSP")
    func  (read(strcat "c:" funcname))
  )
  ;-----
  ;Load the function if not already loaded
  ;-----
  ;  (if(not(caddr (eval func)))
  (if(findfile filename)
    (progn
      (princ(strcat "\nLoading " filename "..."))
      (load funcname)
      (princ(strcat "\n" filename " loaded."))
    )
    (princ(strcat "\nError: " filename
    " is not on the AutoCAD library path."))
  )
  ;  )
  ;-----
  ;Execute the function if it exists
  ;-----
  ;  (if(caddr(eval func))
    (eval(list func))
    ;else
  ;  (princ(strcat "\nError: c:haws-" funcname " is not a valid AutoLISP function"))
  ;)
  (princ)
);end LL
