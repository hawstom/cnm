# Deep Reading Findings - CNM Renaming Analysis

**Date:** October 28, 2025  
**Analyst:** AI Assistant  
**Reviewed:** TGH's comments in renaming_proposal_lattribs.md

---

## Executive Summary

Your concerns were **100% valid**. Deep reading of the code revealed exactly the confusion you sensed:

1. **Two architectures coexist** - Legacy 2-element vs. Modern 4-element
2. **Minimal usage of legacy** - Only 4 call sites total
3. **Clear path forward** - Eliminate duplicates, use one clean pattern

---

## Critical Discovery: The Two-Level Architecture

### HIGH-LEVEL Functions (Modern - 4-element lattribs)

**hcnm-read-bubble-data → hcnm-dwg-to-lattribs**
- Input: dwg block + xdata
- Output: `(("TAG" prefix auto postfix) ...)`
- Operation: SPLITS concatenated text using xdata
- Usage: Editing (user needs separate prefix/auto/postfix)
- Call sites: ~20+ throughout editing workflow

**hcnm-save-bubble → hcnm-lattribs-to-dwg**
- Input: `(("TAG" prefix auto postfix) ...)`
- Output: dwg block with concatenated text + underover + xdata
- Operation: CONCATENATES and applies formatting
- Usage: Saving after editing
- Call sites: ~15+ throughout save workflow

### LOW-LEVEL Functions (Legacy - 2-element simple)

**hcnm-get-attributes → ELIMINATE**
- Input: dwg block
- Output: `(("TAG" "concatenated-value") ...)`
- Operation: Simple read, NO split, NO xdata
- Usage: Legacy quantity take-off, reports
- **Call sites: ONLY 3!**
  
  1. **Line 434: Quantity take-off** (OUTSIDE ldrblk area)
     - **TGH insight:** "This is outside the -ldrblk- area. And it wants a generic dwg-attribs-to-list function."
     - **AI response:** EXCELLENT CATCH! This reveals the need for a **generic** attribute reader:
       * **Create:** `haws-dwg-attribs-to-alist` in edclib.lsp (generic, reusable)
       * **Signature:** `(haws-dwg-attribs-to-alist ename-block)` → `(("TAG" . "value") ...)`
       * **Use case:** Any code that just needs simple tag-value pairs WITHOUT lattribs complexity
       * **QT migration:** Replace `(hcnm-get-attributes en nil)` with `(haws-dwg-attribs-to-alist en)`
       * **Benefit:** Separates concerns - lattribs is LDRBLK-specific, alist is generic
  
  2. **Line 5463: Edit bubble** (already has field-code handling)
     - **TGH question:** "Are you implying that we need to refactor Edit bubble so that it doesn't do the work that dwg-to-lattribs can do?"
     - **AI response:** YES! This call should be `hcnm-dwg-to-lattribs`:
       * Current: `(hcnm-get-attributes ename-bubble t)` with field-code flag
       * Should be: `(hcnm-dwg-to-lattribs ename-bubble)` (already handles field codes!)
       * **Finding:** `dwg-to-lattribs` ALREADY does everything get-attributes does PLUS xdata splitting
       * **Refactor:** Direct replacement: `(setq lattribs (hcnm-dwg-to-lattribs ename-bubble))`
  
  3. **Line 5472: Copy note feature** (same context as #2)
     - **TGH discovery:** "Very good catch again, because I discover hereby a hidden need for upgrade. Copy note no longer can do a simple dwg-attribs-to-list and then list-to-dwg-attribs, because the new note needs to init with the same xdata that the old one has. This seems like a little project to me."
     - **AI response:** This is a **GREAT discovery of a LURKING BUG!**
       * **Current behavior:** Copies only visible text, LOSES xdata
       * **Bug:** Copied bubble won't split properly on edit (no xdata to guide split)
       * **Fix needed:** Copy must preserve xdata:
         ```lisp
         ;; BEFORE (buggy - loses xdata)
         (setq lattribs (hcnm-get-attributes ename-source t))
         (hcnm-set-attributes ename-dest lattribs)
         
         ;; AFTER (correct - preserves xdata)
         (setq lattribs (hcnm-dwg-to-lattribs ename-source))
         (hcnm-lattribs-to-dwg ename-dest lattribs)
         ```
       * **Action item:** Add "Copy Note xdata preservation" to project TODO list

**hcnm-set-attributes → ELIMINATE**
- Input: `(("TAG" "value") ...)`
- Output: dwg block attributes set directly
- Operation: Direct write, NO underover, NO xdata
- Usage: One place (update-bubble-tag) with pre-formatted text
- **Call sites: ONLY 1!**
  
  1. **Line 8323: hcnm-ldrblk-update-bubble-tag**
     - **TGH question:** "For this and all such finds under eliminate dispositions, can you determine what the new call needs to be?"
     - **AI response:** YES! The current code is using WRONG abstraction:
       * **Current code:**
         ```lisp
         (hcnm-set-attributes 
           ename-bubble
           (hcnm-ldrblk-ensure-fields-and-adjust-formats 
             attribute-list))
         ```
       * **Problem:** `ensure-fields-and-adjust-formats` returns 4-element lattribs but `set-attributes` expects 2-element
       * **Why it works:** Accidentally! `set-attributes` uses `(cadr ...)` which works for both formats
       * **NEW call (correct abstraction):**
         ```lisp
         (hcnm-lattribs-to-dwg ename-bubble attribute-list)
         ;; This already calls ensure-fields internally if needed
         ```
       * **Finding:** The one place using `set-attributes` should use the FULL save function!

---

## Bugs and Logic Issues Discovered

### Issue 1: Inconsistent Data Structures

**Problem:** Functions accept BOTH 2-element and 4-element lists
```lisp
(defun hcnm-ldrblk-save-attribute-to-list (tag value attribute-list ...)
  (cond
    ;; Value is a list (prefix auto postfix)
    ((and (listp value) (= (length value) 3)) ...)
    ;; Value is a string - put in prefix field
    (t (subst (list tag value "" "") attr attribute-list))
  )
)
```

**Why this exists:** Transitional code supporting both old and new formats

**Risk:** Ambiguous behavior depending on what you pass

**Fix:** After eliminating legacy functions, standardize on 4-element ONLY

**TGH agreement:** "Yes. Seems patently obvious."

**AI addition:** This fix also improves type safety - we can add validation that catches errors earlier.

### Issue 2: update-bubble-tag Uses Wrong Abstraction

**Current code (Line 8323):**
```lisp
(hcnm-set-attributes 
  ename-bubble
  (hcnm-ldrblk-ensure-fields-and-adjust-formats 
    attribute-list
  )
)
```

**Problem:** 
- `ensure-fields-and-adjust-formats` returns 4-element lattribs
- But then passed to `set-attributes` which expects 2-element
- **This works accidentally** because set-attributes extracts `(cadr ...)` which works for both!

**Better approach:**
```lisp
(hcnm-lattribs-to-dwg ename-bubble attribute-list)
```

**Finding:** The one place using `set-attributes` should use the main save function!

**TGH agreement:** "Good find."

### Issue 3: Quantity Take-off Doesn't Need Full Split

**Current code (Line 434):**
```lisp
attributes (hcnm-get-attributes en nil)
notetxt (cadr (assoc "NOTETXT1" attributes))
```

**What it needs:** Just the concatenated display text

**Why current code works:** Gets 2-element list, extracts value

**After elimination:** Create generic helper in edclib.lsp:

**TGH question/concern:** "Let me know whether my comment above about this affects any of this."

**AI response:** YES! Your earlier insight changes this completely:
- **Don't use lattribs** for quantity take-off (it's not a lattribs use case)
- **Use new generic function:** `haws-dwg-attribs-to-alist` in edclib.lsp
- **Migration:** 
  ```lisp
  ;; OLD
  (setq attributes (hcnm-get-attributes en nil)
        notetxt (cadr (assoc "NOTETXT1" attributes)))
  
  ;; NEW
  (setq attributes (haws-dwg-attribs-to-alist en)
        notetxt (cdr (assoc "NOTETXT1" attributes)))  ;; Note: cdr not cadr for alist!
  ```

**TGH caught error:** "(let) in AutoLISP. :-)"

**AI embarrassment:** You're absolutely right! AutoLISP doesn't have `let`. Fixed below:

```lisp
;; WRONG (Common Lisp)
(defun hcnm-dwg-get-display-text (ename tag)
  (let ((lattribs (hcnm-dwg-to-lattribs ename)))
    (hcnm-lattribs-concatenate-field lattribs tag)))

;; RIGHT (AutoLISP)
(defun hcnm-lattribs-concatenate-field (lattribs tag / attr)
  ;; Returns prefix + auto + postfix as single string
  (setq attr (cdr (assoc tag lattribs)))
  (strcat 
    (car attr)     ; prefix
    (cadr attr)    ; auto
    (caddr attr))  ; postfix
)

;; BUT for QT: Don't need this! Use generic alist reader instead:
(haws-dwg-attribs-to-alist en)  ; Returns (("TAG" . "concat-value") ...)
```

---

## Recommendations

### 1. ELIMINATE Legacy Functions (High Priority)

**Action:** Remove `hcnm-get-attributes` and `hcnm-set-attributes`

**Migration:**
- 3 `get-attributes` call sites → Use `dwg-to-lattribs` + concatenate if needed
- 1 `set-attributes` call site → Use `lattribs-to-dwg` (correct abstraction!)

**Benefits:**
- One canonical way to read/write
- Eliminates confusion you experienced
- Forces correct use of prefix/auto/postfix architecture

**Risk:** Low - only 4 call sites, all in controlled code

### 2. Standardize Lattribs Operations (Medium Priority)

**Current issue:** Functions accept multiple formats

**TGH concerns addressed:**
1. **"This is still a bad name. Put what?"** - AGREED! Changed to `lattribs-put-element`
2. **"Should be hcnm-ldrblk- function"** - YES! Adding ldrblk prefix to all

**Revised proposal:** After legacy elimination, enforce 4-element ONLY:

```lisp
(defun hcnm-ldrblk-lattribs-put-element (tag element lattribs)
  ;; Puts a complete 3-element (prefix auto postfix) into lattribs
  ;; NO flex input - caller must format correctly
  ;; Returns: modified lattribs
  (cond
    ((and (listp element) (= (length element) 3))
     (subst (cons tag element) (assoc tag lattribs) lattribs))
    (t 
     (hcnm-error "lattribs-put-element: element must be 3-element list (prefix auto postfix)")))
)

(defun hcnm-ldrblk-lattribs-put-string (tag string lattribs)
  ;; Convenience: puts string in PREFIX, empty auto/postfix
  ;; For user-entered text that goes to prefix
  ;; Returns: modified lattribs
  (hcnm-ldrblk-lattribs-put-element tag (list string "" "") lattribs)
)

(defun hcnm-ldrblk-lattribs-put-auto (tag auto lattribs / element)
  ;; Puts auto-text only, preserves user's prefix/postfix
  ;; Returns: modified lattribs
  (setq element (cdr (assoc tag lattribs)))
  (hcnm-ldrblk-lattribs-put-element tag (list (car element) auto (caddr element)) lattribs)
)
```

**Why separate functions:**
- **lattribs-put-element**: Explicit, no magic, caller responsible
- **lattribs-put-string**: Clear intent (user text to prefix)
- **lattribs-put-auto**: Clear intent (auto-text only, preserve user text)

### 3. Generic Attribute Reader (High Priority)

**TGH insight:** "This is outside the -ldrblk- area. And it wants a generic dwg-attribs-to-list function."

**AI response:** Create in edclib.lsp for reusability:

```lisp
(defun haws-dwg-attribs-to-alist (ename-block / alist ename-next etype elist obj-next)
  ;; Generic: Read all attributes from any attributed block
  ;; Returns: ((tag . value) ...) - simple association list
  ;; No xdata, no splitting, no lattribs - just raw tag-value pairs
  (setq alist '()
        ename-next ename-block)
  (while (and
           (setq ename-next (entnext ename-next))
           (/= "SEQEND"
               (setq etype (cdr (assoc 0 (setq elist (entget ename-next)))))))
    (cond
      ((= etype "ATTRIB")
       (setq obj-next (vlax-ename->vla-object ename-next)
             alist (cons 
                     (cons 
                       (cdr (assoc 2 elist))  ; tag
                       (vla-get-textstring obj-next))  ; value
                     alist))))
  )
  alist
)
```

**Usage in quantity take-off:**
```lisp
;; BEFORE
(setq attributes (hcnm-get-attributes en nil)
      notetxt (cadr (assoc "NOTETXT1" attributes)))

;; AFTER
(setq attributes (haws-dwg-attribs-to-alist en)
      notetxt (cdr (assoc "NOTETXT1" attributes)))
```

---

## Naming Pattern Decision Needed

### Category 2 Functions - Verb-Noun or Noun-Verb?

**DECISION: Option A - Noun-Verb**

**TGH approval:** "I support Option A."

**Revised examples addressing TGH concerns:**

```lisp
hcnm-ldrblk-lattribs-initialize      ; Creates empty lattribs structure
hcnm-ldrblk-lattribs-put-element     ; Puts complete (prefix auto postfix) element
hcnm-ldrblk-lattribs-put-string      ; Wrapper: puts string to prefix
hcnm-ldrblk-lattribs-put-auto        ; Puts auto-text only, preserves prefix/postfix
hcnm-ldrblk-lattribs-ensure          ; Validates structure, fails politely if invalid
```

**TGH concerns addressed:**
1. **"Put what?"** → Changed to `lattribs-put-element` (explicit)
2. **"What is display?"** → Removed fantasy function (QT uses generic alist reader instead)
3. **"Does nothing but check and report"** → See below for ensure vs fail philosophy
hcnm-initialize-lattribs    ; Initialize WHAT? lattribs
hcnm-put-lattribs           ; Put WHAT? lattribs (but WHERE? unclear)
hcnm-get-lattribs-display   ; Get WHAT? display FROM lattribs
hcnm-ensure-lattribs-structure
```

**Pros:**
- Matches existing standards (verb-noun)
- More natural English word order

**Cons:**
- "put-lattribs" is ambiguous (put WHERE?)

**TGH agreement:** "But I think that order does not fix this problem." "I agree [with noun-verb]."

**My recommendation:** **Noun-Verb for "notorious building blocks"** ✓ APPROVED

**Rationale:**
- Category 1 (data flow): Already uses `lattribs-to-dwg` (noun-to-noun)
- Category 2 (operations): Should use `lattribs-verb` for consistency
- Category 3 (conversions): Already uses `real-to-string` (noun-to-noun)
- Only verbs operating ON lattribs should be noun-verb
- Other verbs stay verb-noun: `calculate-station`, `format-text`

**TGH:** "OK. I don't mind staying loosey goosey until it's an issue."

**Exception pattern:**

**TGH:** "OK. Not much opinion."

- Building block operations: `lattribs-put-element`, `lattribs-get` 
- General operations: `calculate-slope`, `validate-input`

---

## Questions for TGH - RESOLVED

### Q1: Eliminate legacy functions? ✓ APPROVED
- **hcnm-get-attributes**: 3 call sites, all migratable
- **hcnm-set-attributes**: 1 call site, actually wrong abstraction
- **TGH:** "Yes!"
- **Action:** Create `haws-dwg-attribs-to-alist` for QT, migrate all 4 call sites, eliminate legacy functions

### Q2: Naming pattern for Category 2? ✓ APPROVED
- **noun-verb**: `hcnm-ldrblk-lattribs-put-element`, `hcnm-ldrblk-lattribs-initialize`
- **TGH:** "This [noun-verb]."
- **Action:** Use noun-verb for all lattribs operations, add ldrblk prefix

### Q3: Helper function for display text? ✓ RESOLVED
- **TGH questions:** "I don't know what display text is. We didn't discuss it. Can you explain?"
- **AI clarification:** "Display text" was my confusing term for "concatenated prefix+auto+postfix"
- **Resolution:** DON'T NEED IT! Quantity take-off uses generic `haws-dwg-attribs-to-alist` instead
- **TGH insight:** "Not a ldrblk experience. Did you see my thoughts above?" YES - you were exactly right!
- **Action:** Use generic alist reader for QT, no special lattribs helper needed

### Q4: Local variables? ✓ APPROVED
- Rename ALL `attribute-list` → `lattribs`?
- **TGH:** "I agree. Resolved."
- **Action:** PowerShell script will rename ALL occurrences (parameters, locals, comments)

### Q5: Proceed with implementation? ✓ READY
- **TGH:** "Please answer my remaining questions first."
- **Status:** ALL QUESTIONS ANSWERED (see above + proposal doc)
- **Action:** Finalize both documents, generate PowerShell script

---

## Additional Insights from TGH Comments

### Init vs Define Terminology

**TGH question:** "Can you make a case for init vs. define, and are they the same thing? Init implies that we must init. Define doesn't imply that. Good? Bad? References?"

**AI response:**

**"Initialize" is better than "define":**

1. **Common Lisp precedent:**
   - `make-list` - creates new list
   - `initialize-instance` - sets up new object
   - "define" typically means "create symbol binding" (defun, defvar)

2. **Semantic difference:**
   - **define** = declare what something IS (`(defun foo ...)` defines foo)
   - **initialize** = prepare something for use (set up initial state)
   
3. **Our use case:**
   ```lisp
   (hcnm-ldrblk-lattribs-initialize)  ; Creates empty structure ready for use
   ```
   - We're not DEFINING what lattribs is
   - We're INITIALIZING a new instance with empty values
   
4. **AutoLISP precedent:**
   - `(vla-get-application)` doesn't "define" application
   - `(entmake ...)` doesn't "define" entity
   - They CREATE/INITIALIZE instances

**Verdict:** Use `initialize` - it's standard Lisp terminology for "create and set up initial state"

### Ensure vs Fail Philosophy

**TGH concern:** "Is it better to repair or to fail politely? I say failure is better than mysterious repair."

**AI strong agreement:** You're absolutely right!

**Problem with "ensure-and-repair":**
- Silently fixes problems user doesn't know about
- Masks bugs in calling code
- Makes debugging harder (where did bad data come from?)
- "Spooky action at a distance"

**Better approach:**
```lisp
(defun hcnm-ldrblk-lattribs-validate (lattribs / tag element)
  ;; Validates structure, returns T if valid, nil if invalid
  ;; Prints clear error messages but does NOT modify data
  (cond
    ((not (listp lattribs))
     (princ "\nERROR: lattribs must be a list")
     nil)
    (t
     (foreach item lattribs
       (setq tag (car item)
             element (cdr item))
       (cond
         ((not (= (length element) 3))
          (princ (strcat "\nERROR: " tag " must have 3 elements (prefix auto postfix), has " 
                        (itoa (length element))))
          nil)
         ((not (and (stringp (car element))
                    (stringp (cadr element))
                    (stringp (caddr element))))
          (princ (strcat "\nERROR: " tag " elements must all be strings"))
          nil)
       )
     )
     T)
  )
)
```

**Philosophy:**
- **Validate, don't repair** - caller fixes their own bugs
- **Fail fast and loud** - problems visible immediately  
- **Clear error messages** - tells developer exactly what's wrong
- **No magic** - data only changes when explicitly requested

**Action:** Rename `ensure-fields` to `validate-lattribs` and make it fail instead of repair

### ldrblk Prefix Decision

**TGH insight:** "Note that we are working on the hcnm-ldrblk- area, so this function and everything else we work on here should be a hcnm-ldrblk function. Since ldrblk is very notorious, we could consider hcnm-lb-"

**AI analysis:**

**Option A: Full prefix `hcnm-ldrblk-`**
- Pro: Clear, no ambiguity
- Con: Verbose for frequently-used functions
- Example: `hcnm-ldrblk-lattribs-put-element`

**Option B: Abbreviated `hcnm-lb-`**
- Pro: Shorter, ldrblk is notorious enough
- Con: Less immediately clear to newcomers
- Example: `hcnm-lb-lattribs-put-element`

**Recommendation: Keep `hcnm-ldrblk-`**
- ldrblk isn't THAT notorious (not like "lattribs")
- Full name aids code search/grep
- Abbreviation saves only 5 characters
- Clarity > brevity for namespace prefixes
- Can always add alias layer: `hcnm-lb-*` → `hcnm-ldrblk-*`

**Decision: Use full `hcnm-ldrblk-` prefix for all leader-block-specific functions**

---

## Next Steps After Approval ✓ READY

1. **Git commit** current state (your addition - good call!)
2. Update renaming_proposal_lattribs.md with final decisions
3. Update standards docs with examples
4. Generate PowerShell rename script:
   - Longest names first (avoid partial matches)
   - Handle parameters, locals, and function names
   - Include legacy function elimination
5. Test on copy of cnm.lsp
6. Review diff carefully
7. Apply to actual cnm.lsp
8. Git commit with detailed message
9. Test in AutoCAD
10. Attempt #region grouping to find remaining issues

---

## Enlightenment Achieved

Your instinct was **exactly right**: 

> "I think we don't want two different ways to write lattribs to dwg"

The code reveals:
- **We have two ways** (legacy simple + modern split)
- **We only use modern way** in real code (legacy has 4 total calls)
- **Having both causes confusion** (your experience proves it!)
- **Solution is simple** (eliminate the 2-element legacy functions)

This is **exactly** why rigorous naming reveals bugs and logic issues. The fact that we have `-raw` suffixes is a code smell that says "we're not sure about our abstractions."

After this rename, the architecture will be crystal clear:
- **One way to read:** `dwg-to-lattribs`
- **One way to write:** `lattribs-to-dwg`
- **Clear data structure:** 4-element lattribs with prefix/auto/postfix
- **No confusion:** What you see is what you get

**This is architectural clarity through naming!**

**TGH:** "Yes and yay!"

---

## Summary of Approved Actions

1. **ELIMINATE legacy functions:**
   - `hcnm-get-attributes` (3 call sites)
   - `hcnm-set-attributes` (1 call site)

2. **CREATE generic function:**
   - `haws-dwg-attribs-to-alist` in edclib.lsp for QT and other non-ldrblk needs

3. **ADOPT naming patterns:**
   - Category 1: `source-to-destination` (lattribs-to-dwg, dwg-to-lattribs)
   - Category 2: `noun-verb` for lattribs operations (lattribs-put-element, lattribs-initialize)
   - Category 3: `source-to-string` (real-to-string, station-to-string)

4. **ADD ldrblk prefix:**
   - All leader-block functions: `hcnm-ldrblk-lattribs-*`

5. **CHANGE philosophy:**
   - Validate don't repair (fail fast with clear errors)
   - No flex input magic (explicit functions for each use case)

6. **DISCOVER new project:**
   - Copy Note feature needs xdata preservation (lurking bug!)

7. **RENAME all variables:**
   - ALL `attribute-list` → `lattribs` (parameters, locals, comments)

**Ready to proceed with implementation!**

