;(C) Copyright 1997 by Thomas Gail Haws
;MULTIREN.LSP--Multiple Autocad rename
;Thomas Gail Haws, Feb. 1996
(defun c:haws-mren ()
(haws-core-init 261)(c:haws-mrename))
(defun c:haws-mrename ( / opt entry)
  (haws-core-init 262)
  (setq opt (haws-mren_opt) entry nil)
  (while (setq entry (tblnext (car opt) (not entry)))
    (setq entry (cdr (assoc 2 entry)))
    (haws-mren_chg opt entry)
) )

(defun haws-mren_opt ( / entset incexc remove add addlen)
  (initget 1 "Block Dimstyle LAyer LType Style Ucs VIew VPort")
  (setq rntype(getkword "\nBlock/Dimstyle/LAyer/LType/Style/Ucs/VIew/VPort?:"))
  (initget 1 "Allqxz Select")
  (setq entset (getkword "\nChange All or Selective? [All/Select]:"))
  (cond ((= entset "Select")
      (initget 1 "Exclude Include")
      (setq incexc(getkword "\nSelect which to Exclude or Include? <Ex/In>:"))
      (setq entset
        (strcase
          (getstring
            (strcat "\n" rntype "s to " incexc " using wildcards (*,?,etc.):")
  ))  ) ) )
  (initget 1 "Yes No")
  (setq remove (=(getkword "\nRemove characters at beginning? [Yes/No]:") "Yes"))
  (if remove (setq remove (getint "\nRemove how many characters?:")))
  (initget 1 "Yes No")
  (setq add (=(getkword "\nAdd new characters at beginning? [Yes/No]:") "Yes"))
  (cond
    (add (setq add (getstring 1 "Enter prefix to add:"))
      (setq addlen (strlen add))
  ) )
  (list (strcase rntype) entset incexc remove add addlen)
)

(defun haws-mren_chg (opt entry / entold)
  (setq
    entold entry rntype (car opt) entset (cadr opt) incexc (caddr opt)
    remove (cadddr opt) add (nth 4 opt)
    addlen (nth 5 opt)
  )
  (if
    (and
      (/= "0" entry)
      (if (= incexc "Exclude") (wcmatch entry (strcat "~" entset)) t)
      (or
        (= entset "Allqxz")
        (if (= incexc "Include") (wcmatch entry entset) t)
      )
    )
    (progn
      (if remove (setq entry (substr entry (1+ remove))))
      (if add (setq entry (strcat add entry)))
      (if (and(not(tblsearch rntype entry)) (not(=(strlen entry) 0)))
        (vl-cmdf "._rename" (strcat "_" rntype) entold entry)
) ) ) )

