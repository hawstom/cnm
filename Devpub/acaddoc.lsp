;;;The following line loads user.lsp if found.
(if (setq temp(findfile "user.lsp"))(load temp))
;;;You can put personally preferred routines in user.lsp.
;;;It is suggested that you keep a user.lsp in a reserved user support files folder
;;;added to AutoCAD's Support Files Search Path.
;;;Keep user.lsp out of the program folder or it may be deleted.
(princ "\nHawsEDC ACADDOC.LSP loaded.")(princ)
