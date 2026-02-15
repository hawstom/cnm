HAWSEDC STANDARDS VOLUME 03: STYLE
==============================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-11-06  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1. Key Rules
### 1.1.1. Indentation and whitespace
  - **Indentation:** 2 spaces; no tabs; weakly discourage wide-car style and cond exception.
  - **Whitespace:** No whitespace on "in" side of braces (parentheses). Otherwise single space. Weakly discourage padding spaces for vertical alignment other than initial indentation.
  - **Closing brace:** On own line, aligned with opening brace (AutoLISP style) for defun, cond, loops, and long blocks (4+ lines). May be stacked on last line of code (Common LISP style) for closing a few (3-4) lines.
### 1.1.2. Names
  - **Case:** lowercase for code symbols. This means variables and functions. Schedule: Very aggressive
  - **Delimiter:** Hyphens (not underscores or colons). Exception: `c:` prefix for AutoCAD command functions is required by AutoLISP (e.g., `c:cnm`, `c:cnmedit`)
  - **Files:** same
  - **Prefixes:**
  - `obj-` for VLA-OBJECTS
  - `es-` for the results of (entsel)
  - `en-` for entity names
  - `el-` or `eg-` for the results of (entget en)
  - `lst-` for lists when clarification is needed
  - `-p` suffix for booleans.
### 1.1.3. Comments
  - **Hierarchy:** follow established practice for number of semicolons 
    - **Single semicolon:** for very minor comments to the right of code. Exception: ;#region and ;#endregion for VS Code folding
    - **Double semicolon:** for normal comments aligned to the indentation of the code below them.
    - **Triple semicolon:** for major comments aligned to left margin.
    - **Block:** for temporary disabling. Minimize use.
---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume defines naming conventions for all code elements: functions, variables, files, folders, and string constants.

## 2.2 Implementation Authority
**FIRM:** Anyone at any time has permission to do global search and replace of one symbol per commit to incrementally bring the codebase into uniformity.

## 2.3 Scope
This volume covers:
- Case conventions (lowercase for symbols)
- Delimiter style (hyphens)
- Type prefixing (obj-, en-, -p suffix)
- Function naming patterns (verb-noun)
- Name length guidelines
- File and folder naming


---
<!-- #endregion -->

<!-- #region 3. NOTES -->
# 3. CASE CONVENTIONS

## 3.1 Symbols and Functions

### 3.1.1 Standard
**FIRM:** lowercase for all AutoLISP symbols and functions

### 3.1.2 Pros and Cons
**Why lowercase (Pros):**
- Less typing confusion.
- Matches community convention (most AutoLISP forums and examples use lowercase)
- Matches file naming convention (lowercase-with-hyphens)
- Ends the debate - aligns with what everyone else does
- AutoLISP internally converts all code to uppercase anyway (cosmetic choice only)

**Why UPPERCASE (Cons):**
- More readable and larger
  - But doesn't match file naming (creates inconsistency)
  - But creates perpetual "why did you do that?" questions

**Decision:**
This project uses lowercase for everything possible for consistency with community practice and our file naming standard file naming. AutoLISP converts everything to uppercase internally anyway, so this is purely a source code cosmetic choice.

### 3.1.3 Examples
```lisp
;; CORRECT
(defun hcnm-ldrblk-auto-pipe (bubble-data tag auto-type)
  (setq obj-pipe (get-pipe-object))
)

;; AVOID (inconsistent with project)
(DEFUN HCNM_LDRBLK_AUTO_PIPE (BUBBLE_DATA TAG AUTO_TYPE)
  (SETQ OBJ_PIPE (GET_PIPE_OBJECT))
)

;; ACCEPTABLE (borrowed/vendored code - leave as-is)
(defun lee-mac:str->lst (str del / pos)
  ...
)
```

### 3.1.4 Exceptions
**Borrowed/Vendored Code:**
UPPERCASE or mixed case in borrowed/vendored code is acceptable. Don't force-convert external libraries.

**Strings, Comments, and Quoted Symbols:**
The following are intentionally left with their original case (often UPPERCASE):
- **String literals** - User-facing messages, prompts, file content (e.g., `"PERIMETER"`, `"NOTETEXT"`)
- **Comments** - Copyright notices, documentation, author names (e.g., `; Thomas Gail Haws`)
- **Quoted symbols** - VLA property names, type constants (e.g., `'ENAME`, `'STR`, `'InnerDiameterOrWidth`)
- **AutoCAD system variables** - Must match AutoCAD's naming (e.g., `"DIMSTYLE"`, `"AREA"`)

**Schedule:** Very conservative (see S01.3)

## 3.2 Files and Folders

### 3.2.1 Standard
**FIRM:** lowercase with hyphens

### 3.2.2 Current State
Files use hyphens (`haws-tip.lsp`), consistent with symbol naming convention.

---
<!-- #endregion -->

<!-- #region 4. DELIMITER STYLE -->
# 4. DELIMITER STYLE

## 4.1 Symbols and Functions

### 4.1.1 Standard
**FIRM:** Hyphens for all symbol delimiters

### 4.1.2 Pros and Cons
**In favor of hyphens (chosen):**
- Easiest to type (no shift key)
- Common Lisp standard convention
- Autodesk examples and symbols use hyphens
- AutoLISP community consensus practice
- Consistent with Autodesk's own function naming (e.g., `vl-string-trim`, `vlax-get-property`)

**In favor of underscores (rejected):**
1. In other languages, `-` means subtract
2. More editors including VS Code recognize `_` as symbol continuation (better autocomplete)
3. Provides visual disambiguation from AutoCAD native functions

**In favor of colons (rejected):**
- In Common Lisp, colons denote package boundaries (`package:symbol`)
  - but AutoLISP has no package system, so colons have no semantic role
  - so colons confuse readers who expect Common Lisp conventions
  - so they would be implemented inconsistently

### 4.1.3 Hierarchical Example
```lisp
hcnm-ldrblk-auto-pipe-format-diameter
└──┬─┘ └──┬──┘ └─┬─┘└─┬─┘ └────┬────┘
   │      │       │    │        └─ Operation
   │      │       │    └──────── Type
   │      │       └───────────── Feature
   │      └───────────────────── Subsystem
   └──────────────────────────── Namespace
```

### 4.1.4 Migration Status
Migration from underscores to hyphens should be complete. Any remaining underscore-delimited symbols should be renamed to use hyphens.

## 4.2 Files and Folders

### 4.2.1 Standard
**FIRM:** Hyphens (same as symbols for consistency)

---
<!-- #endregion -->

<!-- #region 5. TYPE PREFIXES -->
# 5. TYPE PREFIXES

## 5.1 General Principle

### 5.1.1 Standard
**FIRM:** Prefix specific AutoCAD data types, suffix booleans only. Don't normally prefix `'INT`, `'REAL`, `'STRING` unless helpful.

**Schedule:** Moderately aggressive (important for code clarity, apply to new code immediately)

### 5.1.2 Reference
Google's Common Lisp Style Guide suggests avoiding Hungarian notation (e.g., `int-foo`) unless the type is critical to understanding the symbol's purpose. AutoLISP deviates slightly due to its integration with AutoCAD's type-specific APIs.

## 5.2 Required Prefixes

### 5.2.1 VLA-OBJECT (Most Important)
Use `obj-` prefix to distinguish from entity names and lists:

```lisp
;; CORRECT
obj-pipe      ; VLA-OBJECT of pipe
obj-align     ; VLA-OBJECT of alignment
obj-surface   ; VLA-OBJECT of surface

;; AVOID (ambiguous type)
pipe          ; Could be entity, object, or string
alignment     ; Unclear what type
```

### 5.2.2 Entity Names
Use `en-` prefix:

```lisp
;; CORRECT
en-bubble
en-leader
en-block
```

### 5.2.3 Entsel Results
Use `es-` prefix:

```lisp
;; CORRECT
es-circle  ; The result of (entsel) for the circle
```

### 5.2.4 Entget Lists
Use `eg-` or `el-` prefix if stored/passed around:

```lisp
;; CORRECT
eg-bubble  ; (entget en-bubble)
el-bubble  ; (entget en-bubble)
```

### 5.2.5 Selection Sets
Use `ss-` prefix:

```lisp
;; CORRECT
ss-trees  ; A selection set of trees
ss1       ; Generic selection set
```

## 5.3 Boolean Suffix

### 5.3.1 Standard
Use `-p` suffix (Lisp predicate convention):

```lisp
;; CORRECT
found-p
pspace-p
modified-p
has-attributes-p

;; AVOID (doesn't follow Lisp convention)
is-found
pspace-flag
is-pspace-p    ; Redundant - just use pspace-p
was-modified-p ; Redundant - just use modified-p
```

## 5.4 Optional Prefixes

### 5.4.1 Generic Lists
Only prefix if type is ambiguous:

```lisp
;; CLEAR (no prefix needed)
attribute-list
phase-list
reactor-list
bubbles  ; A list of bubble items

;; ACCEPTABLE (when clarification helps)
lst-bubble-data
lst-bubbles
```

---
<!-- #endregion -->

<!-- #region 6. NAME LENGTH -->
# 6. NAME LENGTH

## 6.1 General Principle

### 6.1.1 Standard
**FIRM:** Favor clarity over brevity. Use shorter names ony for local variables or notorious concepts, longer names for unremarkable variables that appear in many scattered locations and functions.

**Schedule:** Moderately aggressive (see S01.3)

### 6.1.2 Guidelines
- Keep short only for readability. Lengthen liberally as needed to disambiguate or to document across many locations and functions
- Use full words for public API functions, configuration variables, complex domain concepts
- Abbreviations only when established domain convention or very local scope

## 6.2 Long Names (Preferred)

### 6.2.1 Use For
- Public API functions
- Configuration variables
- Complex domain concepts

### 6.2.2 Examples
```lisp
;; GOOD - clear at a glance
hcnm-ldrblk-auto-pipe-format-diameter
bubble-text-prefix-pipe-dia
hcnm-ldrblk-assure-auto-text-has-reactor
```

## 6.3 Short Names (Acceptable)

### 6.3.1 Use When
- Established domain convention (STA=station, DIA=diameter, OFF=offset)
- Very local scope (loop variables: I, N, X)
- Part of common pattern (OBJ_, EN_)

### 6.3.2 Examples
```lisp
;; ACCEPTABLE abbreviations
auto-type     ; "Automatic text type" - clear in context
sta           ; Universal civil engineering abbreviation
dia           ; Common diameter abbreviation
off           ; Offset (clear in context)

;; ACCEPTABLE loop variables
(foreach i list-items ...)
(setq n 0)
(while (< x 10) ...)
```

## 6.4 When to Shorten

### 6.4.1 Don't Shorten
DON'T shorten just to save typing - code is read 10x more than written.

### 6.4.2 Do Shorten When
Abbreviation is clearer than full word in domain context.

### 6.4.3 Examples
```lisp
;; GOOD - Domain abbreviation improves clarity
drawstation   ; Better than drawing-station-value

;; AVOID - Cryptic abbreviation reduces clarity
pspace-bbl-p  ; Worse than pspace-bubble-p
alref         ; Worse than align-reference
```

---
<!-- #endregion -->

<!-- #region 7. FUNCTION NAMING -->
# 7. FUNCTION NAMING

## 7.1 Pattern

### 7.1.1 Standard
**FIRM:** verb-noun or get/set/update/calculate pattern

**Schedule:** Moderately conservative (well-established pattern, apply to new functions, rename only when refactoring)

### 7.1.2 Examples
```lisp
;; CORRECT - Action verbs for clarity
hcnm-ldrblk-auto-pipe-get-object        ; Retrieves object
hcnm-ldrblk-auto-pipe-format-diameter   ; Formats value
hcnm-ldrblk-auto-pipe-calculate-slope   ; Performs calculation
hcnm-ldrblk-update-bubble-tag            ; Modifies existing
```

## 7.2 Verb Vocabulary

### 7.2.1 Standard Verbs
- **get-** - Retrieve data (may prompt user)
- **set-** - Store data
- **calculate-** - Perform computation (pure function)
- **format-** - Convert to display string
- **update-** - Modify existing data
- **assure-** - Check and create if missing
- **validate-** - Check validity

## 7.3 Data Flow Functions (Source-to-Destination Pattern)

### 7.3.1 Standard
**FIRM:** For functions that move data between representations, use `source-to-destination` pattern with NO verb.

**Schedule:** Moderately aggressive (apply to new data flow functions immediately, rename existing during refactoring)

### 7.3.2 Purpose
Make data flow architecturally visible. Shows exactly what data transformation is happening.

### 7.3.3 Pattern Structure
```
<namespace>-<source>-to-<destination>
```

Where:
- **source** - Where data comes from (lattribs, dwg, dlg, xdata, file, etc.)
- **destination** - Where data goes to (lattribs, dwg, dlg, xdata, file, etc.)

### 7.3.4 Core Terms (Building Blocks)
See S05.7 (Architecture Glossary) for full definitions:
- **lattribs** - In-memory association list of bubble attributes (pronounced "ela-tribs")
- **dwg** - Drawing representation (block entity with attributes)
- **dlg** - Dialog state
- **xdata** - Extended entity data

### 7.3.5 Examples

#### Before (Ambiguous Verbs)
```lisp
;; Unclear direction and destination
(defun hcnm-save-bubble (bubble-data)           ; Save to WHERE?
  ...
)

(defun hcnm-read-bubble-data (en-bubble)        ; Read into WHAT?
  ...
)

(defun hcnm-get-bubble-xdata (en-bubble)        ; Get into what format?
  ...
)
```

#### After (Explicit Data Flow)
```lisp
;; Crystal clear source and destination
(defun hcnm-lattribs-to-dwg (lattribs en-bubble)      ; lattribs → drawing
  ...
)

(defun hcnm-dwg-to-lattribs (en-bubble)               ; drawing → lattribs
  ...
)

(defun hcnm-xdata-to-lattribs (en-bubble lattribs)    ; xdata → lattribs
  ...
)

(defun hcnm-dlg-to-lattribs (lattribs)                ; dialog state → lattribs
  ...
)
```

### 7.3.6 Additional Examples
```lisp
;; File operations
hcnm-file-to-lattribs              ; Import from file
hcnm-lattribs-to-file              ; Export to file

;; Cross-format conversions
hcnm-lattribs-to-string            ; Serialize
hcnm-string-to-lattribs            ; Deserialize

;; Complex transformations
hcnm-lattribs-to-table-data        ; Convert for table display
hcnm-csv-to-lattribs-list          ; Bulk import
```

### 7.3.7 When to Use This Pattern

#### DO Use For
- Data transformations between representations
- Import/export operations
- Format conversions
- State synchronization

#### DON'T Use For
- Operations on single representation (use verb-noun instead)
- Calculations that don't transform representation
- UI actions with side effects

#### Examples
```lisp
;; CORRECT - Data flow between representations
hcnm-lattribs-to-dwg              ; Transformation between formats

;; CORRECT - Operation on single representation (verb-noun)
hcnm-lattribs-underover-gap       ; Modifies lattribs only
hcnm-lattribs-get                 ; Retrieves from lattribs
hcnm-lattribs-set                 ; Stores to lattribs
hcnm-dwg-update-display           ; Modifies drawing only

;; CORRECT - Calculation (verb-noun)
hcnm-calculate-station            ; Pure computation
hcnm-format-diameter              ; String formatting
```

### 7.3.8 Pros and Cons

**Pros:**
- **Architecturally visible** - Data flow obvious from function name
- **Unambiguous** - No confusion about direction or destination
- **Scalable** - Easy to add new representations (database, network, etc.)
- **Self-documenting** - Function name explains what it does
- **Refactoring-friendly** - Clear boundaries for data transformations

**Cons:**
- **Longer names** - More characters than single-verb alternatives
- **Less familiar** - Non-standard pattern initially
- **Requires discipline** - Must agree on core term vocabulary

### 7.3.9 Migration Strategy

#### For New Code
**FIRM:** Use source-to-destination pattern for all new data flow functions.

#### For Existing Code
- **Rename when refactoring** data flow operations
- **Document with [AI: TODO]** if rename would help but isn't urgent
- **Use scripting** for bulk renames (longest-first sorting)

#### Example Migration
```lisp
;; OLD (before refactoring)
(defun hcnm-save-bubble (attribute-list en-bubble)
  ; Saves attribute list to drawing entity
  ...
)

;; NEW (after refactoring)
(defun hcnm-lattribs-to-dwg (lattribs en-bubble)
  ; lattribs → drawing: Writes association list to block attributes
  ...
)
```

---
<!-- #endregion -->

<!-- #region 8. SPECIAL CASES -->
# 8. SPECIAL CASES

## 8.1 Constants

### 8.1.1 Standard
Use descriptive names, no special prefix:

```lisp
;; CORRECT
hcnm-error-not-found "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
hcnm-default-precision "2"
```

## 8.2 Global Variables

### 8.2.1 Standard
Use `*` wrapper if truly global (session lifetime):

```lisp
;; CORRECT - Global session variables
*hcnm-config*              ; Config cache
*haws-config-cache*        ; Multi-app config cache

;; CORRECT - Module-scoped (no asterisks)
hcnm-eb-attribute-list     ; Edit bubble dialog state
```

## 8.3 Temporary Variables

### 8.3.1 Standard
In small scopes, single letters acceptable:

```lisp
;; ACCEPTABLE in 5-line loop
(foreach i item-list
  (setq n (1+ n))
)

;; AVOID - Too cryptic if used across 50 lines
(setq a (get-align)
      s (calc-sta a p)
      o (calc-off a p)
)
```

---
<!-- #endregion -->

<!-- #region 9. MIGRATION STRATEGY -->
# 9. MIGRATION STRATEGY

## 9.1 For New Code

### 9.1.1 Rules
- **FIRM:** Apply all conventions immediately
- Use formal headers for public functions (see S04 for details)
- Follow organization pattern (see S04 for details)

## 9.2 For Existing Code

### 9.2.1 Don't Rename Without Good Reason
**AVOID** churn - don't rename working code just to match conventions

### 9.2.2 Do Rename When
- Refactoring a module anyway
- Name is actively causing confusion
- Small scope (5-10 occurrences)
- Add headers to functions being modified
- Document with `[AI: TODO - rename for consistency]` if rename would help but isn't urgent

### 9.2.3 Breaking Changes
- Save for major version bumps
- Use scripting for bulk renames
- Update all callers atomically

## 9.3 Example Migration Commit
```
refactor: rename hcnm_ldrblk_auto_pipe to hcnm-ldrblk-auto-pipe

- Updated function definition
- Updated all 7 call sites
- Per S03.4.1: moving to hyphen delimiters
```

---
<!-- #endregion -->

<!-- #region 10. EXAMPLES SUMMARY -->
# 10. EXAMPLES SUMMARY

## 10.1 Good Example
```lisp
;; CORRECT - Follows all conventions
(defun hcnm-ldrblk-auto-pipe-format-diameter (obj-pipe / dia-feet dia-inches)
  (setq dia-feet   (vlax-get-property obj-pipe 'InnerDiameterOrWidth)
        dia-inches (* dia-feet 12.0)
  )
  (strcat
    (c:hcnm-config-getvar "BubbleTextPrefixPipeDia")
    (rtos dia-inches 2 (atoi (c:hcnm-config-getvar "BubbleTextPrecisionPipeDia")))
    (c:hcnm-config-getvar "BubbleTextPostfixPipeDia")
  )
)
```

## 10.2 Poor Example
```lisp
;; AVOID - Multiple convention violations
(DEFUN FMTDIA (P / D)  ; UPPERCASE not project standard
  (SETQ D (VLAX-GET-PROPERTY P 'InnerDiameterOrWidth))  ; No obj- prefix, UPPERCASE
  (STRCAT (GETVAR "pfx") (RTOS (* D 12) 2 2) (GETVAR "sfx"))  ; Unclear abbreviations
)
```

---
<!-- #endregion -->

<!-- #region 11. NOTES -->
# 11. NOTES

## 11.1 General Principles
- **Consistency > perfection** - Following 80% of standards consistently is better than 100% inconsistently
- **Context matters** - Use judgment for edge cases
- **Document exceptions** - If you break a standard, add comment explaining why
- **Evolve prudently** - Standards can change, but do so deliberately with discussion

## 11.2 Blank Lines in Code
**FIRM:** Remove ALL blank lines

**Schedule:** Very aggressive (easy to apply, low risk, improves code density)

**Rules:**
- No blank lines between function definitions
- No blank lines within function bodies
- No blank lines after opening parenthesis or before closing parenthesis
- No blank lines between comment and code it describes
- No blank lines anywhere in code

**Pros and Cons:**
- **Pro:** Files are more compact; more code visible per screen
- **Pro:** Well-named functions and clear logic provide structure without needing whitespace
- **Con:** Some developers prefer more whitespace for "breathing room"

**Example:**
```lisp
;; CORRECT - No blank lines
(defun calculate-area (width height / area)
  (setq area (* width height))
  (if (> area 1000)
    (princ "\nLarge area")
    (princ "\nSmall area")
  )
  area
)
(defun calculate-volume (width height depth)
  (* width height depth)
)
;; AVOID - Any blank lines
(defun calculate-area (width height / area)

  (setq area (* width height))

  (if (> area 1000)
    (princ "\nLarge area")
    (princ "\nSmall area")
  )

  area
)

(defun calculate-volume (width height depth)
  (* width height depth)
)
```

---
<!-- #endregion -->

<!-- #region 12. DOTTED PAIRS VS LISTS -->
# 12. DOTTED PAIRS VS LISTS

## 12.1 The Problem

### 12.1.1 Two List Construction Primitives
AutoLISP has two list construction primitives with different behavior:
- `(cons 1 2)` → Dotted pair: `(1 . 2)`
- `(list 1 2)` → Proper list: `(1 2)` = `(1 . (2 . nil))`

```lisp
(cdr (cons 1 2))   ; Returns atom: 2
(cdr (list 1 2))   ; Returns list: (2)
```

Mixing in same data structure causes bugs (functions expecting atoms get lists or vice versa).

## 12.2 When to Use Dotted Pairs

### 12.2.1 Association Lists (alists)
Use for semantic key-value pairs:

```lisp
;; CORRECT
(setq config-alist '(("Name" . "John") ("Age" . 30)))
(cdr (assoc "Name" config-alist))  ; Returns atom: "John" ✅
```

### 12.2.2 DXF Code Pairs
AutoCAD entity data format:

```lisp
;; CORRECT
(cons 8 "LAYER-NAME")  ; Returns: (8 . "LAYER-NAME") ✅
```

## 12.3 When to Use Proper Lists

### 12.3.1 Collections to Iterate
```lisp
;; CORRECT
(setq tags '("NOTETXT1" "NOTETXT2" "NOTETXT3"))
(foreach tag tags ...)  ; Clean iteration ✅
```

### 12.3.2 Function Arguments
```lisp
;; CORRECT
(hcnm-some-function (list tag text auto))  ; Multiple values ✅
```

### 12.3.3 Nested Data Structures
```lisp
;; CORRECT
'(("NOTETXT1" "text") ("NOTETXT2" "text"))  ; lattribs ✅
```

## 12.4 CNM-Specific Patterns

### 12.4.1 Dotted Pairs (alists)
```lisp
;; XDATA (key-value pairs)
'(("NOTETXT1" . "auto-text") ("NOTETXT2" . "auto-text"))

;; Config settings
'(("BlockReactors" . "0") ("DebugReactors" . "0"))
```

### 12.4.2 Proper Lists
```lisp
;; lattribs (tag + text, 2-element lists)
'(("NOTETXT1" "text") ("NOTETXT2" "text"))

;; Reactor data (nested collections)
'(("HCNM-BUBBLE" (("owner-handle" (("bubble-handle" ...))))))
```

## 12.5 Common Pitfall

### 12.5.1 Cons vs List for 2-Element Lists
```lisp
;; WRONG: cons creates dotted pair, not 2-element list
(cons "TAG" "value")  ; ("TAG" . "value")
(cadr '("TAG" . "value"))  ; ERROR: cdr returns atom "value"!

;; CORRECT: Use list for 2-element lists
(list "TAG" "value")  ; ("TAG" "value")
(cadr '("TAG" "value"))  ; Returns: "value" ✅
```

## 12.6 Decision Tree

### 12.6.1 When to Choose
```
Is this key-value mapping with semantic meaning?
├─ YES → Use dotted pairs (alist)
│   Examples: XDATA, config, DXF codes
└─ NO → Use proper lists
    Is this collection to iterate?
    ├─ YES → Proper lists
    │   Examples: lattribs, tags list, bubble-list
    └─ NO → Still prefer proper lists (consistency)
```

### 12.6.2 Rule of Thumb
If `assoc` is primary accessor, use dotted pairs. Otherwise, use proper lists.

---
<!-- #endregion -->

**End of Document**
