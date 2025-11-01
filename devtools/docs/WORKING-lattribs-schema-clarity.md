# WORKING DOCUMENT: lattribs Schema Clarity

**Status:** Working draft for feat-sunrise branch  
**Purpose:** Get clarity on lattribs schema to guide bug fixes  
**Lifecycle:** Delete, archive, or digest into standards after codebase is clear  
**Created:** 2025-11-01

---

## 1. CURRENT STATE ANALYSIS

### 1.1 What We Think lattribs Should Be

**Canonical Structure:** Association list of 4-element lists
```autolisp
'(("TAG" "prefix" "auto" "postfix")
  ("TAG" "prefix" "auto" "postfix")
  ...)
```

**Required Tags:**
- `NOTENUM`, `NOTEPHASE`, `NOTEGAP`
- `NOTEDATA` (legacy, may be removed)
- `NOTETXT0` through `NOTETXT6`

**Element Rules (what we think):**
1. Each element MUST be a 4-element list
2. Element [0] (tag) MUST be a string
3. Elements [1,2,3] (prefix, auto, postfix) MUST be strings (not nil)
4. Empty values should be `""` (empty string), never `nil`

### 1.2 What We Actually Have (Bugs Found)

**BUG 1: `hcnm-ldrblk-underover-add` creates 3-element NOTEGAP**
- Line 6337: `(list "NOTEGAP" gap-value "")` ← ONLY 3 ELEMENTS!
- Should be: `(list "NOTEGAP" gap-value "" "")`

**BUG 2: `hcnm-split-attribute-on-xdata` returns 3-element with nil**
- Returns: `(prefix auto postfix)` - 3-element "parts" list
- If any part is nil, it stays nil
- Later combined with tag: `(cons tag parts)` → 4-element with nil values

**BUG 3: `hcnm-ldrblk-dwg-to-lattribs` has NO validation**
- Line 7986: Returns `lattribs` directly without normalization
- Propagates nil values from `split-attribute-on-xdata`

**BUG 4: Most auto-text functions bypass validation**
- Lines 6486, 6564, 6660, 6886, 7018, 7324, 7358
- All call `bubble-data-set "ATTRIBUTES" lattribs` without validation

### 1.3 Two Competing Validators

**Validator 1: `hcnm-ldrblk-lattribs-validate` (line 6013)**
- **Philosophy:** NORMALIZER - tries to fix things
- **Fixes:** Converts 2-element → 4-element lists
- **Does NOT fix:** nil values in existing 4-element lists
- **Used at:** Line 5496 only (1 call site)

**Validator 2: `hcnm-ldrblk-lattribs-validate-schema` (line 6065)**
- **Philosophy:** STRICT - fails loudly with alert
- **Checks:** All required tags present, 4-element, all strings (no nil)
- **Used at:** Line 9447 only (debug function)

**CONFLICT:** Copilot instructions say "Fail loudly", but normalizer tries to fix.

---

## 2. QUESTIONS TO RESOLVE

### 2.1 Schema Questions

**Q1:** Should lattribs EVER contain 2-element lists?
- **Context:** Old format was `(tag value)` with CHR(160) delimiters
- **Current:** Validator converts 2→4 for backward compatibility
- **Decision needed:** Keep migration path or fail loudly?

**Q2:** Should lattribs EVER contain nil values?
- **Context:** `split-attribute-on-xdata` can return nil
- **Current:** No protection, nil propagates to `set_tile` → crash
- **Decision needed:** Fix at source or normalize later?

**Q3:** What about NOTEGAP element count?
- **Context:** `underover-add` creates 3-element NOTEGAP (bug)
- **Current:** Inconsistent with other attributes
- **Decision needed:** Fix bug or special-case NOTEGAP?

### 2.2 Architecture Questions

**Q4:** What is the "One True Validator"?
- **Option A:** Make `lattribs-validate` normalize everything (2→4, nil→"")
- **Option B:** Make `lattribs-validate` fail loudly (rename to validate-schema)
- **Option C:** Keep both - normalizer for migration, strict for bugs

**Q5:** Where should validation happen?
- **Option A:** At every entry point (dwg-to-lattribs, auto functions, etc.)
- **Option B:** At data layer boundaries only (to-dwg, to-dlg)
- **Option C:** Once at creation, trust thereafter

**Q6:** What about the nil → "" bandaid in eb-show?
- **Context:** Lines 9020-9022 convert nil to "" before set_tile
- **Current:** Masks underlying data integrity problem
- **Decision needed:** Keep safety net or remove and fix source?

---

## 3. PROPOSED DECISIONS (Draft)

### 3.1 Schema Rules (Strict)

**RULE 1:** lattribs MUST be association list of 4-element lists
- No 2-element lists allowed (migration complete)
- No 3-element lists allowed (fix NOTEGAP bug)
- No other structures allowed

**RULE 2:** All elements MUST be strings
- `nil` is NEVER allowed
- Empty values MUST be `""` (empty string)
- This is AutoLISP `set_tile` requirement, not arbitrary

**RULE 3:** Required tags MUST be present
- All bubble attributes must exist
- Missing tags = corruption or wrong block type

### 3.2 Validation Strategy (Defense in Depth)

**LAYER 1: Fix at Source**
- `split-attribute-on-xdata`: Always return `("" "" "")` never nil
- `underover-add`: Fix NOTEGAP to 4 elements
- `dwg-to-lattribs`: Add validation at END before return

**LAYER 2: Validate at Boundaries**
- `lattribs-to-dwg`: Validate at START (fail loudly if invalid)
- `lattribs-to-dlg`: Validate at START (fail loudly if invalid)
- All `bubble-data-set "ATTRIBUTES"`: Validate parameter

**LAYER 3: One True Validator**
- **Keep both validators:**
  - `lattribs-validate`: NORMALIZER for backward compatibility (2→4 only)
  - `lattribs-validate-schema`: STRICT for bug detection (fails loudly)
- Call normalizer first (handle old format)
- Call strict validator second (catch bugs)

**LAYER 4: Remove Safety Nets**
- Delete nil→"" bandaid in eb-show (lines 9020-9022)
- If nil reaches dialog, that's a bug we need to see
- Strict validator should catch it first

### 3.3 Migration Path

**Phase 1: Fix Source Bugs (Current)**
1. Fix `underover-add` NOTEGAP bug
2. Fix `split-attribute-on-xdata` to never return nil
3. Add validation to `dwg-to-lattribs` return

**Phase 2: Add Validation at Boundaries**
4. Add validation to `lattribs-to-dwg`
5. Add validation to `lattribs-to-dlg`
6. Add validation to all auto-text functions

**Phase 3: Test and Remove Safety Nets**
7. Test with strict validator enabled
8. Remove nil→"" bandaid from eb-show
9. Remove debug logging

**Phase 4: Document in Code**
10. Add schema comments to validator functions
11. Add schema assertions to creation functions
12. Delete or archive this working document

---

## 4. IMPLEMENTATION PLAN

### 4.1 Immediate Fixes (Fail Loudly)

```autolisp
;; FIX 1: split-attribute-on-xdata (line 7898, 7900)
;; OLD: (list str-attribute "" "")
;; NEW: (list (if str-attribute str-attribute "")
;;            (if str-xdata str-xdata "")
;;            (if postfix postfix ""))

;; FIX 2: underover-add NOTEGAP (line 6337)
;; OLD: (list "NOTEGAP" gap-value "")
;; NEW: (list "NOTEGAP" gap-value "" "")

;; FIX 3: dwg-to-lattribs (line 7986)
;; OLD: lattribs
;; NEW: (hcnm-ldrblk-lattribs-validate
;;       (hcnm-ldrblk-lattribs-validate-schema lattribs))
```

### 4.2 Validation Wrapper Pattern

```autolisp
;; Standard validation call at boundaries
(defun hcnm-validate-and-assert (lattribs context / )
  ;; Step 1: Normalize (backward compatibility)
  (setq lattribs (hcnm-ldrblk-lattribs-validate lattribs))
  
  ;; Step 2: Strict check (fail loudly)
  (if (not (hcnm-ldrblk-lattribs-validate-schema lattribs))
    (progn
      (alert (princ (strcat "\nCRITICAL: Invalid lattribs in " context)))
      (exit)))
  
  ;; Step 3: Return validated lattribs
  lattribs
)
```

### 4.3 Testing Checklist

- [ ] Insert bubble with no auto-text → should work
- [ ] Insert bubble with alignment auto-text → should work
- [ ] Edit bubble dialog opens → should work (no nil in set_tile)
- [ ] Stretch leader → auto-text updates → should work
- [ ] Open old drawing (2-element format) → should migrate → should work
- [ ] Corrupt bubble (missing tag) → should fail loudly → should alert user

---

## 5. DECISION LOG

### Decision 1: Keep Normalizer? (2025-11-01)
**Question:** Should we keep backward compatibility with 2-element format?  
**Decision:** TBD - need user input  
**Rationale:** Unknown if customers have old drawings needing migration

### Decision 2: Where to Validate? (2025-11-01)
**Question:** Validate at every entry or just boundaries?  
**Decision:** Defense in depth - both source fixes AND boundary checks  
**Rationale:** Fail loudly principle + graceful degradation

### Decision 3: Remove Safety Net? (2025-11-01)
**Question:** Keep nil→"" bandaid in eb-show?  
**Decision:** Remove AFTER source bugs fixed and tested  
**Rationale:** Safety net masks bugs we need to find

---

## 6. OPEN QUESTIONS FOR USER

1. **Do we need backward compatibility with 2-element lattribs?**
   - Or can we assume all drawings are already migrated?

2. **Should validator normalize or fail?**
   - Normalizer = graceful (user doesn't see errors)
   - Fail loudly = finds bugs (user sees alerts)

3. **What about NOTEDATA attribute?**
   - Currently in required tags list
   - Copilot instructions say "never saw it in git"
   - Should we remove it from schema?

---

## 7. NEXT STEPS

**Waiting for user input on:**
1. Philosophy: Normalize vs. Fail Loudly
2. Backward compatibility: Keep 2-element support?
3. NOTEDATA: Keep or remove from required tags?

**Ready to implement:**
- Fix 3-element NOTEGAP bug (clear bug)
- Fix nil returns from split-attribute (clear bug)
- Add validation to dwg-to-lattribs (architectural improvement)

---

**END OF WORKING DOCUMENT**
