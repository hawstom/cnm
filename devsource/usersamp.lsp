;To load commands automatically, remove the first semi-colon from the 
;desired lines with parentheses.  Then copy this file to a different 
;directory so it won't be overwritten.  
;Standard directory is c:haws-\user.

;The following lines make Autocad purge everything without prompts
;each time a drawing opens.
;Works only if stcommon is loaded.
;(if s::startup (setq s::STARTUP (append s::STARTUP '((purge"all"))))
;  (defun s::startup () (purge"all"))
;)
