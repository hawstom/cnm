;;; Unit tests for edclib / CNM functions that can be verified without drawing
;;; geometry. Loaded by cnm-unattended-tests.scr. Results go to haws-debug-log.md.
(defun haws-assert-equal (expected actual label / pass)
  (setq pass (equal expected actual))
  (haws-debug
    (cond
      (pass (strcat "PASS " label))
      (t (strcat "FAIL " label
                 "\n    expected: |" (vl-princ-to-string expected) "|"
                 "\n    actual:   |" (vl-princ-to-string actual) "|"))
    )
  )
  pass
)
(haws-debug "=== haws-mtext-unformat unit tests begin ===")
(haws-assert-equal
  ""
  (haws-mtext-unformat "")
  "empty string passthrough"
)
(haws-assert-equal
  "Hello World"
  (haws-mtext-unformat "Hello World")
  "plain text unchanged"
)
(haws-assert-equal
  "Hello"
  (haws-mtext-unformat "\\LHello\\l")
  "simple \\L...\\l stripped"
)
(haws-assert-equal
  "Date: %<\\AcVar Filename>%"
  (haws-mtext-unformat "Date: %<\\AcVar Filename>%")
  "field alone preserved"
)
(haws-assert-equal
  "Hello %<\\AcVar Filename>%"
  (haws-mtext-unformat "\\LHello\\l %<\\AcVar Filename>%")
  "codes stripped, field preserved (regression case)"
)
(haws-assert-equal
  "%<\\AcVar A>% x %<\\AcVar B>%"
  (haws-mtext-unformat "%<\\AcVar A>% x %<\\AcVar B>%")
  "multiple fields preserved"
)
(haws-assert-equal
  "CLICK HERE (```), THEN NE, THEN %<\\AcVar Filename>% OK."
  (haws-mtext-unformat "{\\LCLICK HERE\\l} (```), THEN NE, THEN %<\\AcVar Filename>% OK.")
  "production case from cnm-test.scr"
)
(haws-debug "=== haws-mtext-unformat unit tests end ===")
(haws-debug "=== haws-open/haws-close round trip tests begin ===")
(setq test-file-name (strcat (getvar "dwgprefix") "haws-open-test.csv"))
(setq test-f (haws-open test-file-name "w"))
(haws-debug (list "pointer string: |" (vl-princ-to-string test-f) "|"))
(haws-debug (list "OpenFiles after open: " (vl-prin1-to-string (haws-getvar "OpenFiles"))))
(haws-close test-f)
(haws-debug (list "OpenFiles after close: " (vl-prin1-to-string (haws-getvar "OpenFiles"))))
(setq test-f (haws-open test-file-name "w"))
(haws-assert-equal 'file (type test-f) "second haws-open for write after haws-close")
(if test-f (haws-close test-f))
(vl-file-delete test-file-name)
(haws-debug "=== haws-open/haws-close round trip tests end ===")
