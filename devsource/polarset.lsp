(defun c:haws-polarset ()
(haws-core-init 98)
 (setvar "polarang" (/ pi 12))
; (if (= (getvar "osmode") 0)(setvar "osmode" 8)); A running osnap not needed post ACAD 2000.  Bug fixed.
 (if (/= 8 (logand 8 (getvar "autosnap")))(setvar "autosnap" (+ (getvar "autosnap") 8))) ;Turn on polar tracking
; (if (/= 32 (logand 32 (getvar "autosnap")))(setvar "autosnap" (+ (getvar "autosnap") 32))) ;Turn on tool tips.
)
(defun c:haws-polaroff ()
(haws-core-init 99)
 (if (= 8(logand 8 (getvar "autosnap")))(setvar "autosnap" (- (getvar "autosnap") 8)))
 (if (= 32(logand 32 (getvar "autosnap")))(setvar "autosnap" (- (getvar "autosnap") 32)))
)
