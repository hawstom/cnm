HAWSEDC STANDARDS VOLUME 05: ARCHITECTURE
==========================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-11-12  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 Key Patterns
### 1.1.1 General
- **Central Core Library Functions:** edclib.lsp
- **Config Storage:** `hcnm-config-getvar`/`hcnm-config-setvar` now use the haws-config system. CNM project management (INI files, HCNM_PROJINIT) not yet migrated.

**Note:** S04 (Organization and Locations) answers "WHERE is everything?" while S05 (Architecture) answers "HOW does it work internally?"

### 1.1.2 CNM General
- **Config Storage:** `hcnm-config-getvar`/`hcnm-config-setvar` now use haws-config system. Project management (INI files, HCNM_PROJINIT) remains in CNM-specific code.
  - **Performance Decision:** HCNM_PROJINIT disabled by default. CNM Options dialog alerts users that settings changes made outside dialog require drawing restart.
  - **Business Decision:** Prioritized performance over instant reflection of external INI edits.
  - Session: AutoLISP global via haws-config
  - User: getcfg/setcfg via haws-config  
  - Project: INI file per folder (CNM-specific, not migrated)
  - App: INI file in app folder (CNM-specific, not migrated)
- **Project Notes:** There have been three versions of project notes file formatting. READCF, READCFTXT2, and READCFCSV. READCFCSV is current and is used for spreadsheet editing and the third party custom editor (see previous point). READCFTXT3 was never released. READCFTXT was the original pretty, but clunky for editing, file format edited directly without automatic line wrapping.
- **Key Notes Table:** CNM reads line by line from a Project Notes file and makes a Key Notes Table of the project notes that are found plus their total quantities for the sheet. When the a Key Notes Table is created for a sheet, a .not file is created with the notes and the quantities for that sheet.
- **Quantity Take-off (aka Tally):** Using the .not files created by Key Notes Table, CNM Quantity Take-off makes a total quantities list in the drawing and saves a sheet-by-sheet breakdown to a csv file.
- **Reactor System:** ONE VLR-OBJECT-REACTOR for all bubble notes (not one per bubble)
### 1.1.3 CNM Bubble Notes
- **Reactor System:** ONE VLR-OBJECT-REACTOR for all bubble notes (not one per bubble)
- **XDATA Storage:** XDATA stores AUTO-text values as TAG-VALUE pairs for search/parse on read
- **System-Controlled Attributes:** NOTENUM, NOTEPHASE, NOTEGAP always go to PREFIX field

#### 1.1.3.1 Data Flow Functions

**Design Principle:** All conversion functions are PURE (list → list). I/O wrappers handle AutoCAD entity reading/writing separately.

**Current State vs. Goal:**
- **CURRENT:** `dwg-to-lattribs` and `lattribs-to-dwg` mix conversion + I/O (not pure)
- **GOAL:** Separate into pure converters + I/O wrappers for elegance and testability

**Proposed Architecture:**

| Layer | Function (prefixed `hcnm-ldrblk-`)    | Input              | Output      | Notes                                    |
|-------|---------------------------------------|--------------------|-------------|------------------------------------------|
| **I/O** | `dwg-read`                          | ename              | alist       | Pure I/O: Read attributes → alist        |
| **Convert** | `dwg-to-lattribs`                 | alist + xdata      | lattribs    | Pure: Convert + strip format codes       |
| **Convert** | `lattribs-to-dwg`                 | lattribs           | alist       | Pure: Convert + add format codes         |
| **I/O** | `dwg-write`                         | ename + alist      | (side effect)| Pure I/O: Write alist → attributes      |
| **Convert** | `lattribs-to-dlg`                 | lattribs           | dlg-list    | Pure: Display format via underover-add   |
| **Convert** | `dlg-to-lattribs`                 | dlg-list           | lattribs    | Pure: Strip via underover-remove         |
| **Helper** | `lattribs-put-element`             | tag + parts + list | lattribs    | Puts (prefix auto postfix) element       |
| **Helper** | `lattribs-put-string`              | tag + string + list| lattribs    | User text → prefix, insertion safety check (won't fail in practice) |
| **Helper** | `lattribs-put-auto`                | tag + string + list| lattribs    | Auto-text only, preserves prefix/postfix |
| **Transform** | `lattribs-concat`               | lattribs           | lattribs-cat | Concatenates prefix+auto+postfix → single string per tag |
| **Transform** | `lattribs-split`                | lattribs-cat + xdata | lattribs  | Splits by searching for AUTO text from XDATA in concatenated strings |
| **Format** | `underover-add`                    | lattribs       | lattribs | Adds format codes (%%u, %%o) to prefix + sets NOTEGAP |
| **Format** | `underover-remove`                 | lattribs       | lattribs | Strips format codes (%%u, %%o) from prefix + clears NOTEGAP |

**Usage Pattern (Current - WORKING):**
```lisp
;; Dialog flow (CLEAN ARCHITECTURE ✓)
(setq lattribs (hcnm-get-attributes ename))                    ; Read structured
(setq dlg-lattribs (hcnm-ldrblk-lattribs-to-dlg lattribs))   ; Add format codes to prefix
;; ... user edits dialog (3 separate fields map to prefix/auto/postfix) ...
(setq lattribs (hcnm-ldrblk-dlg-to-lattribs dlg-lattribs))   ; Strip format codes from prefix
```

**Usage Pattern (Future - After dwg function refactoring):**
```lisp
;; Reading from drawing
(setq lattribs-cat (hcnm-ldrblk-dwg-read ename-bubble))       ; Read concatenated strings
(setq xdata (hcnm-ldrblk-xdata-get ename-bubble))             ; Read AUTO text values (TAG-VALUE pairs)
(setq lattribs (hcnm-ldrblk-lattribs-split lattribs-cat xdata)) ; Split by searching for AUTO text
(setq lattribs (hcnm-ldrblk-underover-remove lattribs))       ; Strip format codes

;; Writing to drawing
(setq lattribs (hcnm-ldrblk-underover-add lattribs))          ; Add format codes
(setq lattribs-cat (hcnm-ldrblk-lattribs-concat lattribs))    ; Concatenate parts
(hcnm-ldrblk-dwg-write ename-bubble lattribs-cat)             ; Write to attributes
(hcnm-ldrblk-xdata-save ename-bubble lattribs)                ; Save AUTO text as TAG-VALUE pairs
```

**Benefits of Pure Functions:**
- Testable without AutoCAD entities
- Composable (can chain transformations)
- Parallel to `lattribs-to-dlg` pattern
- Single Responsibility Principle

#### 1.1.3.2 Data Structure Terminology

**lattribs** (structured):
```lisp
'(("NOTETXT1" "prefix" "auto" "postfix")
  ("NOTETXT2" "prefix" "auto" "postfix")
  ("NOTEGAP" "" "" ""))
```

**lattribs-cat** (concatenated):
```lisp
'(("NOTETXT1" "prefixautopostfix")
  ("NOTETXT2" "prefixautopostfix")
  ("NOTEGAP" ""))
```

#### 1.1.3.3 Underover Logic (Format Codes)

Business logic operates on **structured lattribs**:
- **TXT1 not empty?** → add underline formatting (%%u for dtext, \L for mtext) to prefix
- **TXT2 not empty?** → add overline formatting (%%o for dtext, \O for mtext) to prefix  
- **Either not empty?** → NOTEGAP = underline + two spaces (`"%%u  "` or `"\L  "`), else `""` (hide gap)
- **Empty check:** `(= "" (strcat prefix auto postfix))`

**Critical:** Underover preserves structure - adds codes to prefix, keeps auto/postfix separate.

#### 1.1.3.4 Architecture Cleanup Needed

**COMPLETED (2025-10-30):**
1. ✅ **CREATED:** `lattribs-concat` - Pure transform: structured → concatenated (2-element lists)
2. ✅ **CREATED:** `lattribs-split` - Pure transform: concatenated + xdata → structured (4-element lists)
3. ✅ **REFACTORED:** `underover-add` - Now pure formatting (assumes input already concatenated)
4. ✅ **REFACTORED:** `underover-remove` - Pure code stripping (assumes input concatenated)
5. ✅ **DEPRECATED:** `underover` - Marked for deletion (not real business logic)
6. ✅ **REFACTORED:** `lattribs-to-dlg` - Now calls concat THEN underover-add (clean separation)
7. ✅ **REFACTORED:** `dlg-to-lattribs` - Returns lattribs-cat, not structured (user must split if needed)

**REMAINING WORK:**
1. **TODO:** Refactor `lattribs-to-dwg` to use concat + underover-add (currently duplicates logic)
2. **TODO:** Delete `hcnm-ldrblk-underover` function entirely
3. **TODO:** Fix `lattribs-validate-and-underover` to not call deleted function

### 1.1.4 HAWS_QT
- **Serves CNM:** Provides "third party" (in another file) length and area quantities to CNM Bubble Notes
- **Object-oriented-like:** Uses innovative "object-oriented" approach to serve "instances" of its functions so multiple apps can each have their own settings. Calling apps declare their "app" name when they "instantiate a new QT object"


---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume documents the core architectural patterns and systems used in HAWSEDC.

## 2.2 Scope
[AI: Content about overall system architecture]

---
<!-- #endregion -->

<!-- #region 3. CONFIGURATION SYSTEM -->
# 3. CONFIGURATION SYSTEM

## 3.1 Config System (Current Hybrid)

### 3.1.1 Overview
CNM uses a hybrid configuration system: core config functions migrated to haws-config, project management remains CNM-specific.

### 3.1.2 Migrated to haws-config
**Status:** COMPLETED

```lisp
(c:hcnm-config-getvar "BubbleTextPrefixSta")  ; Uses haws-config internally
(c:hcnm-config-setvar "BubbleTextPrefixSta" "STA ")  ; Uses haws-config internally
```

**Scopes handled by haws-config:**
- **Session:** In-memory AutoLISP global
- **User:** AutoCAD's getcfg/setcfg registry

### 3.1.3 Remaining CNM-Specific
**Status:** NOT MIGRATED (performance reasons)

**Project Management:**
- **INI Files:** Project-specific cnm.ini files
- **HCNM_PROJINIT:** Project scope refresh (disabled by default)
- **Performance Decision:** INI reads expensive, disabled for performance
- **User Alert:** CNM Options dialog warns "settings changes made outside dialog require drawing restart"

**Key Functions (CNM-specific):**
```lisp
HCNM_CONFIG_DEFINITIONS    ; Schema for 400+ settings
HCNM_PROJINIT             ; Project INI refresh (disabled by default)
```

### 3.1.4 Business Decision: Performance vs Responsiveness

**Trade-off:** External INI edits not reflected immediately vs performance.

**Chosen:** Performance (disabled HCNM_PROJINIT by default)

**Rationale:**
- Most users edit via CNM Options dialog (immediate reflection)
- External INI editing rare (legacy third-party editor deprecated)
- Drawing restart acceptable for rare external edits
- Performance improvement significant

## 3.2 haws-config System (Current Implementation)

### 3.2.1 Status
**COMPLETED:** Core config functions extracted into reusable haws-config library.

### 3.2.2 Current Architecture
```lisp
;; CNM functions now use haws-config internally
(c:hcnm-config-getvar "BubbleTextPrefixSta")   ; Wrapper to haws-config
(c:hcnm-config-setvar "BubbleTextPrefixSta" "STA ")  ; Wrapper to haws-config

;; Direct haws-config usage (for new code)
(haws-config-getvar "CNM" "BubbleTextPrefixSta")
(haws-config-setvar "CNM" "BubbleTextPrefixSta" "STA ")
```

### 3.2.3 Multi-App Support
**Implemented:** Each app gets separate namespace:
```lisp
;; App registration
(haws-config-register-app "CNM")
(haws-config-register-app "HAWS_QT")
```

### 3.2.4 Features Implemented
- ✅ **Type system:** Explicit integer/boolean/string validation
- ✅ **Multi-app support:** Separate namespaces per application
- ✅ **Session/User scopes:** In-memory and registry storage
- ✅ **Backward compatibility:** CNM wrapper functions maintain API
- ❌ **Project scope:** Remains in CNM-specific code (performance decision)

### 3.2.5 Benefits Realized
- **Code reuse:** haws-config used by multiple HawsEDC applications
- **Consistent UX:** Uniform config behavior across apps
- **Performance:** Optimized for session/user scopes
- **Maintainability:** Central config logic, not duplicated

---
<!-- #endregion -->

<!-- #region 4. CNM BUBBLE NOTES -->
# 4. CNM BUBBLE NOTES

## 4.1 Current Architecture (2025-11-13)

**Current production system** as of 2025-11-01 (fully updated architecture from November 2025 debugging).

### 4.1.1 lattribs Structure

**2-Element Format:** `'(("TAG" "full-text") ...)`
- Always 2-element lists: `("TAG" "string")`  
- Never nil: All values MUST be strings, use `""` for empty
- All required tags present: Missing tag = corruption
- **NO prefix/auto/postfix separation** - full text stored in single string

**Required Tags:**
- `NOTENUM` - Bubble number (user-controlled)
- `NOTEPHASE` - Construction phase (user-controlled)  
- `NOTEGAP` - Spacing between text lines (system-controlled)
- `NOTETXT0` through `NOTETXT6` - User/auto text lines (free-form)

**Schema Validation:** Strict validation with `hcnm-lb-lattribs-validate-schema` - fails loudly on violations.

### 4.1.2 XDATA Storage Patterns

**Two Storage Systems:**
1. **XDATA** - Auto-text composite keys (small, frequent access)
2. **XRECORD** - Viewport transforms (large, infrequent access)

**XDATA Format - Composite Key Storage**
```autolisp
;; (("TAG" (((auto-type . handle) . "value") ...)) ...)
(("NOTETXT1" ((("StaOff" . "ABC123") . "STA 10+25.00 OFF 5.0' RT")))
 ("NOTETXT2" ((("N" . "") . "N 123456.78") 
              (("E" . "") . "E 789012.34"))))
```

**XRECORD Format - Viewport Transform Storage**

**Storage location:** Extension dictionary → `"HCNM"` dict → `"VPTRANS"` xrecord

**Data structure:** `(cvport ref-ocs-1 ref-wcs-1 ref-ocs-2 ref-wcs-2 ref-ocs-3 ref-wcs-3)`
- cvport = viewport number (integer)
- Three 3D point pairs defining affine transformation

**Service Layer API:**
```autolisp
(hcnm-xdata-get-vptrans ename-bubble)         ; Returns viewport data or nil
(hcnm-xdata-set-vptrans ename-bubble data)    ; Writes to XRECORD
```

**When created:**
1. User places bubble in paper space with coordinate-based auto-text (N/E/NE)
2. User clicks alignment/pipe in paper space for station/offset/diameter
3. "Change View" button in edit dialog (explicit viewport reassociation)

**When used:** Every reactor update for paper space bubbles - transforms leader arrowhead position from paper space OCS to model space WCS.

**Migration note (2025-11-05):**
- ✅ VPTRANS moved from XDATA to XRECORD (frees ~200 bytes per bubble)
- ✅ Auto-text remains in XDATA (small, dynamic)
- ✅ NOTEDATA attribute deprecated (never used)

### 4.1.3 bubble-data Alist

**Purpose:** Pass ephemeral state during insertion/editing operations without modifying global variables.

**Structure:** Association list with typed accessors:
```autolisp
(setq bubble-data (hcnm-lb-bubble-data-def))  ; Create empty structure
(setq bubble-data (hcnm-lb-bubble-data-set bubble-data "NOTETYPE" "ELL"))
(setq notetype (hcnm-lb-bubble-data-get bubble-data "NOTETYPE"))
```

**Key Fields:**
- `"NOTETYPE"` - Bubble shape (BOX/CIR/ELL/etc.)
- `"ATTRIBUTES"` - lattribs structure
- `"ename-bubble"` - Entity name of bubble
- `"ename-leader"` - Entity name of leader
- `"p1-world"` - World coordinates for coordinate-based auto-text
- `"auto-metadata"` - Accumulated auto-text entries for reactor attachment

**Data Flow:** Insertion commands build bubble-data incrementally, passing between functions. Dialog operations work on lattribs directly (simpler, synchronous).

### 4.1.4 Edit Flow Architecture

**Key Insight from Nov 2025 debugging:** Editor system breaks VLA-OBJECT attachments while preserving data structure.

**Edit Dialog Flow:**
1. `hcnm-lb-eb-open` - Load lattribs + XDATA into dialog
2. `hcnm-lb-eb-auto-button` - Update dialog field (in-memory only)
3. `hcnm-lb-eb-save` - ATOMIC write: lattribs + XDATA + reactor rebuild
4. `hcnm-lb-eb-reactor-refresh` - Rebuild VLA-OBJECT attachments from XDATA

**Critical:** Step 4 was failing to properly re-attach VLA-OBJECTs to reactor, causing "all reactions killed by editing" bug discovered Nov 2025.

### 4.1.5 Auto-Text Dispatcher Architecture Flaw

**Problem:** `hcnm-lb-auto-dispatch` parameter `obj-target` serves dual purposes:
1. **Data source:** VLA-OBJECT for handle-based auto-text
2. **Path discriminator:** NIL (insertion) vs non-NIL (reactor)

**Breaks for handleless auto-text (N/E/NE):** Must use sentinel value `T` instead of reference object.

**Proposed Solution:** Add explicit `reactor-context-p` parameter:
```autolisp
;; Current (broken)
(hcnm-lb-auto-dispatch tag auto-type obj-target bubble-data)

;; Proposed (clean)  
(hcnm-lb-auto-dispatch tag auto-type obj-reference bubble-data reactor-context-p)
```

**Status:** Architectural revision required - treating symptoms, not root cause.

## 4.2 Reactive Auto-Text System

**Reference:** Based on architectural understanding gained during November 2025 debugging session.

### 4.2.1 System Overview

**Problem Solved:** Auto-text in bubble notes needs to update automatically when:
- Alignments change (station/offset updates)
- Pipes change (diameter/slope updates)  
- Leaders are moved (coordinate updates)
- Surfaces change (elevation updates)

**Solution:** Single persistent `VLR-OBJECT-REACTOR` per drawing with 5-level nested data structure tracking owner-bubble-tag-auto-type-reference relationships.

### 4.2.2 Two Orthogonal Auto-Text Classifications

**Handle-based vs Handleless:**
- **Handle-based:** StaOff/Dia/Slope (stores reference object handle in XDATA)
- **Handleless:** N/E/NE (empty handle `""`, coordinate-only)

**Coordinate-based vs Non-coordinate:**
- **Coordinate-based:** StaOff/N/E (needs viewport transform in paper space)
- **Non-coordinate:** Dia/Slope (no transform needed)

**Key Architectural Point:** Handleless auto-text STILL needs leader attachment for stretch updates. November 2025 fix ensures handleless auto-text always attached to leader regardless of coordinate requirements.

### 4.2. Reactor Data Structure (5-Level Hierarchy)

**Storage Location:** Reactor `:data` property

**Structure:**
```
"HCNM-BUBBLE"               ; Application namespace (fixed key)
  → owner-handle             ; Object being tracked (VLA-OBJECT handle)
    → bubble-handle          ; Bubble depending on this owner
      → tag                  ; Attribute tag ("NOTETXT1")
        → auto-type          ; Calculation type ("StaOff")
          → reference-handle ; LEAF: Object providing data
```

**Semantic Terminology (CRITICAL):**
- **OWNER** = Any object attached to reactor (reference OR leader)
- **NOTIFIER** = Specific owner that triggered THIS callback
- **REFERENCE** = Object providing calculation data (always reference object, never leader)

**Owner Attachment Logic:**
```autolisp
;; November 2025 Fix: Proper leader attachment for handleless auto-text
(setq keys-leader
  (cond
    ((= handle-reference "")
     ;; Handleless: Always attach to leader (for stretch updates)
     (list (list handle-leader handle-reference))
    )
    ((member auto-type coord-based-list)
     ;; Coordinate-based: Attach to both leader and reference  
     (list (list handle-leader handle-reference) 
           (list handle-reference handle-reference))
    )
    (t
     ;; Handle-based, non-coordinate: Reference only
     (list (list handle-reference handle-reference))
    )
  )
)
```

### 4.2. BlockReactors Flag Lifecycle

**Purpose:** Prevent infinite recursion when reactor callbacks modify reactor-monitored objects.

**Critical Pattern (ALWAYS use save/restore):**
```autolisp
(defun hcnm-ldrblk-reactor-callback (obj-notifier reactor event-list / saved-state ...)
  (setq saved-state (c:hcnm-config-getvar "BlockReactors"))
  (if (= saved-state "1")
    (progn  ; Gateway: Already blocked
      (haws-debug "=== REACTOR BLOCKED by BlockReactors flag ===")
      (c:hcnm-config-setvar "BlockReactors" saved-state)  ; Restore
      (princ))
    (progn  ; Normal processing
      (c:hcnm-config-setvar "BlockReactors" "1")          ; Block nested
      (hcnm-ldrblk-reactor-notifier-update ...)           ; Do work
      (c:hcnm-config-setvar "BlockReactors" saved-state)  ; Restore
    )
  )
)
```

**Why save/restore?** Nested callbacks must honor parent's blocking state. Philosophy: Better one extra update than permanently blocked updates.

### 4.2. Reactor Update Algorithm

**Flow:**
1. **Trigger:** Object change fires VLR-OBJECT-REACTOR callback
2. **Gateway:** Check BlockReactors flag (prevent infinite recursion)
3. **Lookup:** Find all bubbles depending on changed object in 5-level hierarchy
4. **Update:** For each bubble/tag/auto-type combination:
   - Extract old auto-text from XDATA (search needle)
   - Generate new auto-text via `hcnm-lb-auto-dispatch`
   - Smart replace: preserve user edits around auto-text
   - Update XDATA with new search needle
   - Write lattribs to drawing attributes
5. **Cleanup:** Remove deleted bubbles from reactor data (functional updates with `vl-remove-if`)

**Key Insight:** Use functional data structure updates during callbacks to avoid corruption from concurrent reactor firing.

### 4.2. Cleanup Pattern (Three-Tier)

**Tier 1: Immediate Detection**
- Function: `hcnm-ldrblk-reactor-bubble-update`
- When: Every reactor callback attempts to update each tracked bubble
- Check: `(not (entget ename-bubble))` → Return `"DELETED"` string

**Tier 2: Batch Cleanup**
- Function: `hcnm-ldrblk-reactor-notifier-update`
- When: After processing all bubbles for ONE notifier
- Algorithm: Accumulate deleted handles, filter using `vl-remove-if`

**Tier 3: Deep Scrub**
- Function: `hcnm-ldrblk-cleanup-reactor-data`
- When: Called explicitly via `(c:pretest)` or maintenance commands
- Purpose: Full scrub of entire reactor data structure

### 4.2. Edit Dialog Integration

**Critical Bug (November 2025):** Editor operations break VLA-OBJECT attachments while preserving data structure.

**Edit Flow:**
1. `hcnm-lb-eb-open` - Load lattribs + XDATA into dialog
2. `hcnm-lb-eb-auto-button` - Update dialog field (in-memory only)
3. `hcnm-lb-eb-save` - ATOMIC write: lattribs + XDATA + reactor rebuild
4. `hcnm-lb-eb-reactor-refresh` - Rebuild VLA-OBJECT attachments from XDATA

**Root Cause:** Step 4 was failing to properly re-attach VLA-OBJECTs to reactor.

### 4.2. Debugging Tools

**Standard Commands:**
```autolisp
;; View reactor data structure
(hcnm-lb-reactor-debug-dump)

;; Clean up all reactors and test bubbles  
(c:pretest)

;; Check XDATA format
(hcnm-xdata-read ename-bubble)

;; Check BlockReactors flag status
(hcnm-config-getvar "BlockReactors")
```

### 4.2. Performance Considerations

**Known Bottlenecks:**
- XDATA read/write on every reactor callback
- Nested loops in handle lookup
- Multiple reactor callbacks per leader move

**Optimization Strategy (Future):**
- Batch updates at bubble level instead of tag level
- Cache reactor handle lookups
- Reactor data reorganization: Move bubble to higher level in hierarchy

### 4.2. Maintenance Guidelines

**Do:**
- Understand owner vs notifier vs reference semantics before modifying
- Preserve BlockReactors save/restore pattern in ALL reactor callbacks
- Test both handleless (N/E/NE) and handle-based auto-text with leader stretch
- Use functional data structure updates (`vl-remove-if`) during callbacks

**Don't:**
- Add defensive error handlers that mask architectural problems
- Mutate data structures during callback iteration (corruption risk)
- Skip cleanup logic - leads to memory leaks and performance degradation

**Philosophy:** Clean architectural fixes are usually small and precise. The November 2025 handleless auto-text bug was fixed with 3 lines of code, not error handlers.

---
<!-- #endregion -->

<!-- #region 5. CNM PROJECT NOTES -->
# 5. CNM PROJECT NOTES

## 5.1 File Formats (Evolution)

### 5.1.1 Version 1: READCF (Original)
Pretty format with manual line wrapping. Clunky to edit directly.

**Status:** DEPRECATED

### 5.1.2 Version 2: READCFTXT2
Improved text format.

**Status:** DEPRECATED (never released publicly)

### 5.1.3 Version 3: READCFCSV (Current)
**FIRM:** CSV format for spreadsheet editing and third-party editor compatibility.

Location: Project folder, typically `constnot-default.csv`

Format:
```csv
NOTENUM,NOTETEXT,NOTEPHASE,NOTEGAP
1,Existing inlet to remain,A,
2,Remove and replace inlet,B,
```

### 5.1.4 Migration
Old formats automatically converted on first load.

## 5.2 Key Notes Table

### 5.2.1 Purpose
Reads project notes CSV and creates drawing table showing:
- Note numbers and text
- Quantities for current sheet only

### 5.2.2 .not Files
When Key Notes Table created, CNM saves `.not` file with:
- Notes found on sheet
- Quantities per note

Example: `Sheet-C-01.not`

## 5.3 Quantity Take-off (Tally)

### 5.3.1 Purpose
Aggregates quantities across all sheets.

### 5.3.2 Process
1. Read all `.not` files in project
2. Sum quantities per note
3. Create drawing table with totals
4. Save CSV with sheet-by-sheet breakdown

### 5.3.3 Output
CSV file: `project-quantity-takeoff.csv`

Format:
```csv
NOTENUM,NOTETEXT,SHEET1,SHEET2,TOTAL
1,Existing inlet,5,3,8
2,Replace inlet,2,1,3
```

---
<!-- #endregion -->

<!-- #region 6. TIP SYSTEM -->
# 6. TIP SYSTEM

## 6.1 Purpose
Show helpful tips to users, with "Don't show again" option.

## 6.2 Storage
**FIRM:** Uses AutoCAD getcfg/setcfg (not file I/O)

Location: `%APPDATA%/Autodesk/AutoCAD/[version]/HawsEDC/TipsHidden`

Format: `"(TIP1 TIP2 TIP3)"` - List of hidden tip IDs

## 6.3 Functions
```lisp
HAWS_TIP_HIDE_LIST       ; Read list of hidden tips
HAWS_TIP_SAVE_HIDE_LIST  ; Save list of hidden tips
```

## 6.4 Historical Migration
Old system used file: `haws-tip-hide.lsp`

**Status:** DEPRECATED - Migrated to getcfg/setcfg for reliability (no file permissions issues)

---
<!-- #endregion -->

<!-- #region 7. ARCHITECTURE GLOSSARY -->
# 7. ARCHITECTURE GLOSSARY

## 7.1 Core Building-Block Terms

### 7.1.1 Overview
These are the notorious, hard-working terms used throughout CNM's architecture. They appear frequently in function names and should be understood as foundational concepts.

### 7.1.2 Data Representations

#### lattribs
**Pronunciation:** "ela-tribs" (short for "list of attributes")

**Definition:** The in-memory association list representation of a bubble's attribute data during editing and processing.

**Structure:** Each element is `(TAG . VALUE)` where TAG is attribute name, VALUE is delimited string.

**Example:**
```lisp
(("NOTENUM" . "1§§")
 ("NOTETEXT" . "§STA 100+25.50§")
 ("NOTEPHASE" . "A§§"))
```

**Pros and Cons:**
- **Pro:** Short, memorable term for frequently-used concept
- **Pro:** Distinguishes from drawing representation (dwg) and dialog state (dlg)
- **Pro:** Lisp programmers recognize "l" prefix pattern (like lst-, el-)
- **Con:** Not immediately obvious to new developers (requires documentation)

**Related Functions:** `hcnm_lattribs_init`, `hcnm_lattribs_set`, `hcnm_lattribs_get`

#### dwg
**Definition:** The drawing representation of a bubble - the actual block entity in the AutoCAD drawing with its attributes.

**Example:** Entity name or entget list of a bubble block

**Pros and Cons:**
- **Pro:** Universal abbreviation for "drawing" in CAD context
- **Pro:** Very short for frequently-used concept
- **Con:** Could confuse with .dwg file format (but context usually clear)

**Related Functions:** `hcnm_lattribs_to_dwg`, `hcnm_dwg_to_lattribs`

#### xdata
**Definition:** Extended entity data attached to AutoCAD entities. Standard AutoCAD term.

**Usage:** Stores bubble metadata (linked object IDs, auto-text type, etc.)

**Note:** KEEP standard AutoCAD terminology - don't rename.

### 7.1.3 UI Components

#### dlg
**Definition:** Dialog-related functions, variables, or state.

**Example:** `hcnm_eb_dlg_action`, `hcnm_eb_dlg_state`

**Pros and Cons:**
- **Pro:** Standard abbreviation in UI programming
- **Pro:** Clearly distinguishes dialog state from data representations
- **Con:** None significant

### 7.1.4 Data Operations

#### underover
**Definition:** Underline and overline formatting codes applied to attribute text.

**Usage:** Describes the operation of applying format codes (prefix = underline, postfix = overline).

**Example:** `hcnm_lattribs_underover_gap` applies underline to GAP field

**Pros and Cons:**
- **Pro:** Precisely describes what operation does (not vague "format")
- **Pro:** Avoids confusion with string formatting/concatenation
- **Con:** Longer than "format"
- **Con:** Less familiar than "format"

**Replaces:** "format" and "adjust-formats" which were ambiguous

**Related Functions:** `hcnm_lattribs_underover_gap`, `hcnm_lattribs_underover_all`

### 7.1.5 Unchanged Terms

#### reactor
**Definition:** AutoLISP VLR-OBJECT-REACTOR for automatic updates.

**Note:** KEEP - standard AutoLISP term.

#### bubble-data
**Definition:** Complete conceptual unit of a bubble (not a building block).

**Note:** KEEP - descriptive compound term, not frequently-used enough to abbreviate.

## 7.2 Naming Patterns

### 7.2.1 Data Flow Functions
Functions that move data between representations use `source_to_destination` pattern:

```lisp
;; BEFORE (ambiguous direction)
hcnm_save_bubble          ; Save to where? From what?
hcnm_read_bubble_data     ; Read into what?

;; AFTER (explicit data flow)
hcnm_lattribs_to_dwg      ; lattribs → drawing
hcnm_dwg_to_lattribs      ; drawing → lattribs
hcnm_xdata_to_lattribs    ; xdata → lattribs
hcnm_dlg_to_lattribs      ; dialog → lattribs
```

**Pros and Cons:**
- **Pro:** Architecturally visible - shows data flow through system
- **Pro:** Unambiguous - always clear what's happening
- **Pro:** Scales well - can add new representations (file, database, etc.)
- **Con:** Slightly longer than single-verb names
- **Con:** Less familiar pattern initially

**See Also:** S03.7 (Function Naming - Data Flow Pattern)

## 7.3 Usage Notes

### 7.3.1 When to Use Short Terms
Use abbreviated terms (lattribs, dwg, dlg, underover) for:
- **Frequently-used concepts** appearing in many function names
- **Core building blocks** of the architecture
- **Type prefixes** when clarification needed

### 7.3.2 When to Use Full Terms
Use full descriptive terms for:
- **Composite concepts** (bubble-data, bubble-reactor)
- **Infrequently-used concepts** 
- **User-facing names** in dialogs or prompts

### 7.3.3 Pronunciation Guide
For team communication:
- **lattribs:** "ela-tribs" (not "lat-ribs")
- **dwg:** "drawing" or spell out "D-W-G"
- **dlg:** "dialog" or spell out "D-L-G"
- **underover:** "under-over" (two syllables each)

---
<!-- #endregion -->

<!-- #region 8. NOTES -->
# 8. NOTES

## 8.1 Future Architecture Topics

### 8.1.1 To Document
[AI: Content to add in future updates]
- Detailed reactor callback flow
- DCL dialog architecture patterns
- Error handling strategies
- Undo handling in bubble edits
- Space switching (model ↔ paper) patterns

---
<!-- #endregion -->

**End of Document**
