(defun c:haws-hawsedc () (alert "\nPlease use the EDCMENU command to load HawsEDC menus."))
(defun c:haws-edcmenu () (command "menuunload" "hawsedc" "menuload" "hawsedc.mnu"))

