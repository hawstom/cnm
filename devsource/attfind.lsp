;ATTFIND
;(C) Copyright 1997 by Thomas Gail Haws
;Usage:
; (attfind [blockwc] [list of criteria lists] [return tag list] [stop flag])
;Example:
;(ATTFIND
;  "*NOTE*"
;  '(("NOTETYPE" "HEX")("NOTENUM" "1A"))
;  '("NOTETEXT" "NOTETXT2")
;  NIL)
;)
;Searches through all blocks named *NOTE* and returns the block entity name
;and a list of the requested attribute entity names for blocks that match all the
;given criteria.  List is in the following format.
;( ( <block1ename> (<att1aename> <att1bename>))
;  ( <block2ename> (<att2aename> <att2bename>))
;)
;If the stop flag is not nil, ATTFIND stops after finding the first match.
(defun haws-attfind
  (blkwc crtl0 tagl0 stop /
  at av blki blkm blkss crti crtli el en et i blklst tagli)
  (setq
    crtl0 (mapcar '(lambda (crtli) (mapcar '(lambda (crti) (strcase crti)) crtli)) crtl0)
    tagl0 (mapcar '(lambda (tagi) (strcase tagi)) tagl0)
    blkss (ssget "X" (list (cons 2 blkwc)))
    i -1
  )
  (while
    (and
      blkss
      (not(and stop blkm))
      (setq blki (ssname blkss (setq i (1+ i))))
    )
    (setq en blki tagli nil crtli nil)
    (while
      (and
        (setq en(entnext en))
        (= "ATTRIB"(setq et(cdr(assoc 0(setq el(entget en))))))
      )
      (if
        (assoc (setq at (cdr (assoc 2 el))) crtl0)
        (setq crtli (cons (list at (setq av(cdr(assoc 1 el)))) crtli))
      )
      (if (member at tagl0)(setq tagli (cons en tagli)))
    )
    (setq blkm t)
    (foreach crt0 crtl0
      (if
        (or
          (not blkm)
          (not (setq crti(assoc (car crt0) crtli)))
          (not (wcmatch (cadr crti) (cadr crt0)))
        )
        (setq blkm nil)
      )
    )
    (if blkm (setq blklst (cons (list blki(reverse tagli)) blklst)))
  )
  (reverse blklst)
)
