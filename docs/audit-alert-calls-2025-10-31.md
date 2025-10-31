# Alert Call Audit - October 31, 2025

## Issue
User reported: "at **every alert** you MUST use the form (alert(princ)) so that we see the alert at the prompt history."

## Test Result
User hit an error: "expected a list and got ','"

## Root Cause Analysis

### Bug 1: Wrong Argument Type (CRITICAL - This caused the error)
**Line 5576** in `hcnm-ldrblk-get-data-prompts`:
```autolisp
;; BEFORE (WRONG):
(hcnm-ldrblk-lattribs-put-element
  tag
  string    ; ❌ Just a string like ","
  lattribs
)

;; AFTER (CORRECT):
(hcnm-ldrblk-lattribs-put-element
  tag
  (list string "" "")  ; ✅ 3-element list (prefix auto postfix)
  lattribs
)
```

**Why this caused the error:**
- User typed "," as line 1 text
- Code passed "," as `string` directly to `lattribs-put-element`
- That function expects a 3-element list `(prefix auto postfix)`
- Validation at line 5939 caught it and alerted (but without princ!)
- Then `(exit)` aborted the command

### Bug 2: Missing (princ) in Alert Calls
Four alert calls didn't use `(alert (princ ...))` pattern:

1. **Line 5199** - WIPEOUTFRAME warning
2. **Line 5939** - lattribs-put-element validation (the one user hit!)
3. **Line 6136** - LATTRIBS schema validation  
4. **Line 7881** - Split attribute on XDATA error

## Audit Results

### ❌ FIXED (Missing princ)

1. **Line 5199** - WIPEOUTFRAME warning
   - Before: `(alert "Setting WIPEOUTFRAME to 2 to show but not plot")`
   - After: `(alert (princ "\nSetting WIPEOUTFRAME to 2 to show but not plot"))`

2. **Line 5939** - lattribs-put-element validation error ⚠️ **User hit this one!**
   - Before: `(alert (strcat "hcnm-ldrblk-lattribs-put-element: value must be 3-element list...`
   - After: `(alert (princ (strcat "\nhcnm-ldrblk-lattribs-put-element: value must be 3-element list...`

3. **Line 6136** - LATTRIBS schema validation error
   - Before: `(alert (strcat "LATTRIBS SCHEMA VALIDATION FAILED:\n\n"...`
   - After: `(alert (princ (strcat "\nLATTRIBS SCHEMA VALIDATION FAILED:\n\n"...`

4. **Line 7881** - Split attribute on XDATA error
   - Before: `(alert (strcat "Message from the CNM..."...`
   - After: `(alert (princ (strcat "\nMessage from the CNM..."...`

### ✅ ALREADY CORRECT
These already use `(alert (princ ...))`:

- Line 5401 - DIMSCALE/Annotation scale warning ✅
- Line 5433 - AutoCAD R14 compatibility warning ✅
- Line 6930 - Alignment selection warning ✅
- Line 7382 - Auto-apology (not implemented) ✅
- Line 8449 - Various alerts ✅
- Line 9502 - Reactor debug function ✅
- Line 9522 - No entity selected ✅

## Fixes Applied (Commit 6fdbf8c)

### 1. Critical Bug Fix
✅ Line 5576: Pass 3-element list to `lattribs-put-element`, not bare string

### 2. Alert Pattern Fixes
✅ Line 5199: WIPEOUTFRAME warning - added princ
✅ Line 5939: lattribs-put-element validation - added princ
✅ Line 6136: LATTRIBS schema validation - added princ
✅ Line 7881: Split attribute XDATA error - added princ

All alerts now use `(alert (princ ...))` pattern to ensure messages appear in command history, not just dialog boxes.

## Testing
User should now be able to:
1. Insert bubble note
2. Type "," or any other text for line 1
3. See proper error message in command history if validation fails
4. **Most importantly**: Not hit the "expected list, got ','" error anymore

## Prevention
Going forward, all new alert calls must use:
```autolisp
(alert (princ "\n[your message here]"))
```

Never use bare `(alert "string")` - it won't appear in command history!

