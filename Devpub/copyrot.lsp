;;;COPY AND ROTATE AN OBJECT
(defun c:haws-cr ()(c:haws-copyrot))
(defun c:haws-COPYROT (/ objs pt1)
  (while (not objs) (setq objs (ssget)) )
  (initget 9)
  (setq pt1 (getpoint "\nPoint of rotation: "))
  (command "._undo" "g" "._COPY" objs "" pt1 pt1)
  (princ "\n<rotation angle>/Reference: ")
  (command "._ROTATE" objs "" pt1)
  (while (= 1 (logand (getvar "cmdactive" )1 ))
    (command pause)
  )
  (command "._undo" "e")
  (princ)
);end CR
