(DEFUN
   C:HAWS-CLEAN ( / inp)
  (initget "Yes No")
  (setq inp (getkword "\nClean uses the wlock * command, which does not preserve Civil 3D styles.  Continue? [Yes/No]: "))
  (cond 
    ((= inp "Yes")
      (SETVAR "expert" 0)
      (COMMAND "._WBLOCK" (HAWS-GETDNPATH) "_Y" "*")
      (cond
        ((= (GETVAR "sdi") 1)
        (COMMAND "._OPEN" "_Y" (HAWS-GETDNPATH))
         )
        (T
         (alert (princ "Drawing has been cleaned and saved.\nClean must now close drawing.\nPlease re-open from disk."))
        (COMMAND "._CLOSE" "_Y")
         )
      )
    )
  )
)