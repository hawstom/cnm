(DEFUN
   C:HAWS-CLEAN ( / inp)
(haws-core-init 9)
  (initget "Yes No")
  (setq inp (getkword "\nClean uses the wlock * command, which does not preserve Civil 3D styles.  Continue? [Yes/No]: "))
  (cond 
    ((= inp "Yes")
      (SETVAR "expert" 0)
      (vl-cmdf "._WBLOCK" (HAWS-GETDNPATH) "_Y" "*")
      (cond
        ((= (GETVAR "sdi") 1)
        (vl-cmdf "._OPEN" "_Y" (HAWS-GETDNPATH))
         )
        (T
         (alert (princ "Drawing has been cleaned and saved.\nClean must now close drawing.\nPlease re-open from disk."))
        (vl-cmdf "._CLOSE" "_Y")
         )
      )
    )
  )
)