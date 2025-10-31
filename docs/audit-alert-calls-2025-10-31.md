# Alert Call Audit - October 31, 2025

## Issue
User reported: "at **every alert** you MUST use the form (alert(princ)) so that we see the alert at the prompt history."

## Audit Results

### ❌ MISSING (alert (princ ...))
These need to be fixed:

1. **Line 5199** - WIPEOUTFRAME warning
   - Current: `(alert "Setting WIPEOUTFRAME to 2 to show but not plot")`
   - Fix: `(alert (princ "\nSetting WIPEOUTFRAME to 2 to show but not plot"))`

2. **Line 5939** - lattribs-put-element validation error
   - Current: `(alert (strcat "hcnm-ldrblk-lattribs-put-element: value must be 3-element list...`
   - Fix: `(alert (princ (strcat "hcnm-ldrblk-lattribs-put-element: value must be 3-element list...`
   - **This is the error the user hit!**

3. **Line 6136** - LATTRIBS schema validation error
   - Current: `(alert (strcat "LATTRIBS SCHEMA VALIDATION FAILED:\n\n"...`
   - Fix: `(alert (princ (strcat "LATTRIBS SCHEMA VALIDATION FAILED:\n\n"...`

4. **Line 7881** - Split attribute on XDATA error
   - Current: `(alert (strcat ...` (multi-line)
   - Fix: `(alert (princ (strcat ...`

### ✅ ALREADY CORRECT
These already use (alert (princ ...)):

- Line 5401 - DIMSCALE/Annotation scale warning ✅
- Line 5433 - AutoCAD R14 compatibility warning ✅
- Line 6930 - Alignment selection warning ✅
- Line 7382 - Auto-apology (not implemented) ✅
- Line 8449 - Some other alert ✅
- Line 9502 - Reactor debug function ✅
- Line 9522 - No entity selected ✅

## Additional Bug Found

**Line 5574-5577** - Wrong argument type passed to lattribs-put-element:
```autolisp
(hcnm-ldrblk-lattribs-put-element
  tag
  string    ; ❌ WRONG: This is just the input string (e.g., ",")
  lattribs
)
```

Should be:
```autolisp
(hcnm-ldrblk-lattribs-put-element
  tag
  (list string "" "")  ; ✅ CORRECT: 3-element list (prefix auto postfix)
  lattribs
)
```

This is why the user got the error: "expected a list and got ','".

## Fixes Required

1. Fix 4 missing `(alert (princ ...))` calls
2. Fix line 5576 to pass 3-element list instead of bare string
