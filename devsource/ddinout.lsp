;;(C) Copyright 1997 by Thomas Gail Haws
;; Calling Procedure -
;;  (ddinout <list> <flag>)
;;    <list> of strings.
;;    <flag> nil if all un-selected,
;;           'T if all selected at start.
;;-----------------------------------------------
(defun ddinout (title lst flg / dh res x)
  ;;
  ;;find dialog file DCL and load it.
  (setq dh (load_dialog
             (findfile "DDINOUT.DCL")))
  ;;
  ;;did it get loaded ok?
  (if (and dh (new_dialog "DDINOUT" dh))
   (progn
     (setq lst ;;concat \t (TAB) to strings with
       (mapcar ;;with select or space marks.
         '(lambda (x)
            (strcat
               (if flg "\t" " \t")
               x)) lst))
     ;;
     (start_list "MSL")  ;;put in MSL list box
     (mapcar 'add_list lst) ;;add to buffer
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
         (foreach x lst ;;find which are selected
            (if (= (substr x 1 1) "")
               (setq res ;;result list
                  (cons  ;;add to front, remove
                     (substr x 3) ;;1st 2 chars.
                     res)))))
     (unload_dialog dh)
     (reverse res))))  ;;result list return.
;;-----------------------------------------------
(defun ddinout_msl (what)
   ;;
   ;;what is $value string, convert it to a list
   ;;of integers using (READ).
   (setq what (read (strcat "(" what ")")))
   ;;
   ;;for each element in WHAT, change  marker
   ;;at the (NTH) position in LST.
   (foreach x what
      (setq lst
        (nth_subst  ;;defined below
          x         ;;nth position
          (strcat   ;;new string
             (if (= "" ;;already selected?
                    (substr (nth x lst) 1 1))
                 " "   ;;unselect it
                 "")  ;;select it
             (substr (nth x lst) 2))
          lst)      ;;list
      )
      ;;
      ;;update item in list box buffer at
      ;;nth position X.
      (start_list "MSL" 1 x)
      (add_list (nth x lst))
      (end_list)
      ;;
      ;;set currently selected list member to
      ;;be the same as the one just updated.
      (set_tile "MSL" (itoa x))
   )
)
;;-----------------------------------------------
(defun ddinout_sel ()
  ;;
  ;;force selected mark  onto all strings
  ;;in LST.
  (setq lst (mapcar '(lambda (x)
     (strcat "\t" (substr x 3))) lst))
  ;;
  ;;Update list box buffer with new strings
  (start_list "MSL")
  (mapcar 'add_list lst)
  (end_list)
)
;;-----------------------------------------------
(defun ddinout_uns ()
  ;;force all members in LST to have no selected
  ;;marker.
  (setq lst (mapcar '(lambda (x)
     (strcat " \t" (substr x 3))) lst))
  ;;
  ;;Update list box buffer with new strings
  (start_list "MSL")
  (mapcar 'add_list lst)
  (end_list)
)
;;-----------------------------------------------
;; NTH_SUBST   replace NDX member [base 0] in L
;; with NEW and return modified list.
;;
(defun nth_subst (ndx new l / tmp)
   (while (> ndx 0)
      (setq tmp (cons (car l) tmp)
            l (cdr l)
            ndx (1- ndx)
      )
   )
   (append (reverse tmp) (list new) (cdr l))
)
;;-----------------------------------------------
