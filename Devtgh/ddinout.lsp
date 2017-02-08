;;(C) Copyright 1997 by Thomas Gail Haws
;; Calling Procedure -
;;  (ddinout <list> <flag>)
;;    <list> of strings.
;;    <flag> nil if all un-selected,
;;           'T if all selected at start.
;;-----------------------------------------------
(defun DDINOUT (TITLE LST FLG / DH RES X)
  ;;
  ;;find dialog file DCL and load it.
  (setq DH (load_dialog
             (findfile "DDINOUT.DCL")))
  ;;
  ;;did it get loaded ok?
  (if (and DH (new_dialog "DDINOUT" DH))
   (progn
     (setq LST ;;concat \t (TAB) to strings with
       (mapcar ;;with select or space marks.
         '(lambda (X)
            (strcat
               (if FLG "\t" " \t")
               X)) LST))
     ;;
     (start_list "MSL")  ;;put in MSL list box
     (mapcar 'add_list LST) ;;add to buffer
     (end_list)
     ;;
     (set_tile "TITLE" title)
     ;;
     (action_tile "MSL" ;;establish call back
                  "(DDINOUT_MSL $value)")
     ;;
     (action_tile "SEL" ;;select all button
                  "(DDINOUT_SEL)")
     ;;
     (action_tile "UNS" ;;un-select all button
                  "(DDINOUT_UNS)")
     ;;
     (if (= (start_dialog) 1) ;;OK pressed?
         (foreach X LST ;;find which are selected
            (if (= (substr X 1 1) "")
               (setq RES ;;result list
                  (cons  ;;add to front, remove
                     (substr X 3) ;;1st 2 chars.
                     RES)))))
     (unload_dialog DH)
     (reverse RES))))  ;;result list return.
;;-----------------------------------------------
(defun DDINOUT_MSL (what)
   ;;
   ;;what is $value string, convert it to a list
   ;;of integers using (READ).
   (setq WHAT (read (strcat "(" what ")")))
   ;;
   ;;for each element in WHAT, change  marker
   ;;at the (NTH) position in LST.
   (foreach X WHAT
      (setq LST
        (NTH_SUBST  ;;defined below
          X         ;;nth position
          (strcat   ;;new string
             (if (= "" ;;already selected?
                    (substr (nth X LST) 1 1))
                 " "   ;;unselect it
                 "")  ;;select it
             (substr (nth X LST) 2))
          LST)      ;;list
      )
      ;;
      ;;update item in list box buffer at
      ;;nth position X.
      (start_list "MSL" 1 X)
      (add_list (nth X LST))
      (end_list)
      ;;
      ;;set currently selected list member to
      ;;be the same as the one just updated.
      (set_tile "MSL" (itoa X))
   )
)
;;-----------------------------------------------
(defun DDINOUT_SEL ()
  ;;
  ;;force selected mark  onto all strings
  ;;in LST.
  (setq LST (mapcar '(lambda (X)
     (strcat "\t" (substr X 3))) LST))
  ;;
  ;;Update list box buffer with new strings
  (start_list "MSL")
  (mapcar 'add_list LST)
  (end_list)
)
;;-----------------------------------------------
(defun DDINOUT_UNS ()
  ;;force all members in LST to have no selected
  ;;marker.
  (setq LST (mapcar '(lambda (X)
     (strcat " \t" (substr X 3))) LST))
  ;;
  ;;Update list box buffer with new strings
  (start_list "MSL")
  (mapcar 'add_list LST)
  (end_list)
)
;;-----------------------------------------------
;; NTH_SUBST   replace NDX member [base 0] in L
;; with NEW and return modified list.
;;
(defun NTH_SUBST (NDX NEW L / TMP)
   (while (> NDX 0)
      (setq TMP (cons (car L) TMP)
            L (cdr L)
            NDX (1- NDX)
      )
   )
   (append (reverse TMP) (list NEW) (cdr L))
)
;;-----------------------------------------------
