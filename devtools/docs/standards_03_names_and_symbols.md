HAWSEDC STANDARDS VOLUME 03: NAMES AND SYMBOLS
==============================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-10-25  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 Key Rules
- **Case:** UPPERCASE for code symbols. This means variables and functions. Schedule: Very aggressive
- **Delimiter:** Underscores (not hyphens or colons). This means squash all hyphens and colons. Schedule: Moderately aggressive
  - **Exception:** `C:` prefix for AutoCAD command functions is required by AutoLISP (e.g., `C:CNM`, `C:CNMEDIT`)
- **Files:** lowercase with underscores. Schedule: Moderately conservative
- **Prefixes:**  Moderately aggressive.
  - `OBJ_` for VLA-OBJECTS
  - `ES_` for the results of (entsel)
  - `EN_` for entity names
  - `EL_` or `EG_` for the results of (entget en)
  - `LST_` for lists when clarification is needed
  - `_P` suffix for booleans.

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
- Case conventions (UPPERCASE for symbols)
- Delimiter style (underscores not hyphens)
- Type prefixing (OBJ_, EN_, _P suffix)
- Function naming patterns (VERB_NOUN)
- Name length guidelines
- File and folder naming


---
<!-- #endregion -->

<!-- #region 3. NOTES -->
# 3. CASE CONVENTIONS

## 3.1 Symbols and Functions

### 3.1.1 Standard
**FIRM:** ALL CAPS for all AutoLISP symbols and functions

### 3.1.2 Pros and Cons
**Pros:**
- AutoLISP reads all code as uppercase regardless of input
- Existing codebase is 95% uppercase already
- Matches AutoCAD native function style
- Consistency > aesthetics

**Cons:**
- Converting would be massive churn for aesthetic gain only
- Some developers prefer mixed case for readability

### 3.1.3 Examples
```lisp
;; CORRECT
(DEFUN HCNM_LDRBLK_AUTO_PIPE (BUBBLE_DATA TAG AUTO_TYPE)
  (SETQ OBJ_PIPE (GET_PIPE_OBJECT))
)

;; AVOID (inconsistent with project)
(defun hcnm_ldrblk_auto_pipe (bubble_data tag auto_type)
  (setq obj_pipe (get_pipe_object))
)

;; ACCEPTABLE (borrowed/vendored code - leave as-is)
(defun lee-mac:str->lst (str del / pos)
  ...
)
```

### 3.1.4 Exception
Mixed case in borrowed/vendored code is acceptable. Don't force-convert external libraries.

## 3.2 Files and Folders

### 3.2.1 Standard
**PLANNED:** lowercase with underscores (migrating from hyphens)

### 3.2.2 Current State
Existing files use hyphens (`haws-tip.lsp`). Migrating to underscores (`haws_tip.lsp`) for consistency with symbol naming.

### 3.2.3 Migration Strategy
- New files: Use underscores
- Existing files: Rename during refactoring (low priority)
- Don't force rename unless touching file anyway

---
<!-- #endregion -->

<!-- #region 4. DELIMITER STYLE -->
# 4. DELIMITER STYLE

## 4.1 Symbols and Functions

### 4.1.1 Standard
**FIRM:** Underscores for all symbol delimiters

### 4.1.2 Pros and Cons
**Why underscores (Pros):**
1. In other languages, `-` means subtract
2. More editors recognize `_` as symbol continuation (better autocomplete)
3. Provides visual disambiguation from AutoCAD native functions (which use hyphens)
4. Slightly more readable word boundaries
5. Consistency across symbols and files

**Why not hyphens (Cons):**
- See reasons 1-3 above

**Why not colons (Cons):**
- In Common Lisp, colons denote package boundaries (`package:symbol`)
- AutoLISP has no package system, so colons have no semantic role
- Colons confuse readers expecting Common Lisp conventions
- Non-idiomatic in AutoLISP (Autodesk documentation never uses colons in symbol examples)
- Reduces portability in mixed Lisp environments
- Would be implemented inconsistently (scattershot adoption)
- Common Lisp Style Guide warns against using colons outside package contexts

### 4.1.3 Hierarchical Example
```lisp
HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
└──┬─┘ └──┬──┘ └─┬─┘└─┬─┘ └────┬────┘
   │      │       │    │        └─ Operation
   │      │       │    └──────── Type
   │      │       └───────────── Feature
   │      └───────────────────── Subsystem
   └──────────────────────────── Namespace
```

### 4.1.4 Migration Strategy
- New code: Always use underscores
- Refactoring: Convert hyphens to underscores incrementally
- One symbol per commit for traceability
- Update all callers atomically (see S03.2.2)

## 4.2 Files and Folders

### 4.2.1 Standard
**PLANNED:** Underscores (same as symbols for consistency)

### 4.2.2 Pros and Cons
**Pros:**
- No technical reason to differ from symbol delimiter style
- Consistency across code and filenames

**Cons:**
- Requires renaming existing files (low priority churn)

---
<!-- #endregion -->

<!-- #region 5. TYPE PREFIXES -->
# 5. TYPE PREFIXES

## 5.1 General Principle

### 5.1.1 Standard
**FIRM:** Prefix specific AutoCAD data types, suffix booleans only. Don't normally prefix `'INT`, `'REAL`, `'STRING` unless helpful.

**Schedule:** Moderately aggressive (important for code clarity, apply to new code immediately)

### 5.1.2 Reference
Google's Common Lisp Style Guide suggests avoiding Hungarian notation (e.g., `int_foo`) unless the type is critical to understanding the symbol's purpose. AutoLISP deviates slightly due to its integration with AutoCAD's type-specific APIs.

## 5.2 Required Prefixes

### 5.2.1 VLA-OBJECT (Most Important)
Use `OBJ_` prefix to distinguish from entity names and lists:

```lisp
;; CORRECT
OBJ_PIPE      ; VLA-OBJECT of pipe
OBJ_ALIGN     ; VLA-OBJECT of alignment
OBJ_SURFACE   ; VLA-OBJECT of surface

;; AVOID (ambiguous type)
PIPE          ; Could be entity, object, or string
ALIGNMENT     ; Unclear what type
```

### 5.2.2 Entity Names
Use `EN_` prefix:

```lisp
;; CORRECT
EN_BUBBLE
EN_LEADER
EN_BLOCK
```

### 5.2.3 Entsel Results
Use `ES_` prefix:

```lisp
;; CORRECT
ES_CIRCLE  ; The result of (ENTSEL) for the circle
```

### 5.2.4 Entget Lists
Use `EG_` or `EL_` prefix if stored/passed around:

```lisp
;; CORRECT
EG_BUBBLE  ; (ENTGET EN_BUBBLE)
EL_BUBBLE  ; (ENTGET EN_BUBBLE)
```

### 5.2.5 Selection Sets
Use `SS_` prefix:

```lisp
;; CORRECT
SS_TREES  ; A selection set of trees
SS1       ; Generic selection set
```

## 5.3 Boolean Suffix

### 5.3.1 Standard
Use `_P` suffix (Lisp predicate convention):

```lisp
;; CORRECT
FOUND_P
PSPACE_P
MODIFIED_P
HAS_ATTRIBUTES_P

;; AVOID (doesn't follow Lisp convention)
IS_FOUND
PSPACE_FLAG
IS_PSPACE_P    ; Redundant - just use PSPACE_P
WAS_MODIFIED_P ; Redundant - just use MODIFIED_P
```

## 5.4 Optional Prefixes

### 5.4.1 Generic Lists
Only prefix if type is ambiguous:

```lisp
;; CLEAR (no prefix needed)
ATTRIBUTE_LIST
PHASE_LIST
REACTOR_LIST
BUBBLES  ; A list of BUBBLE items

;; ACCEPTABLE (when clarification helps)
LST_BUBBLE_DATA
LST_BUBBLES
```

---
<!-- #endregion -->

<!-- #region 6. NAME LENGTH -->
# 6. NAME LENGTH

## 6.1 General Principle

### 6.1.1 Standard
**FIRM:** Favor clarity over brevity. Use shorter names for local variables, longer names for variables that appear in many scattered locations and functions.

**Schedule:** Moderately conservative (apply judgment, don't force renames without good reason)

### 6.1.2 Guidelines
- Keep short except as needed to disambiguate or to document across many locations and functions
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
HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
BUBBLE_TEXT_PREFIX_PIPE_DIA
HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR
```

## 6.3 Short Names (Acceptable)

### 6.3.1 Use When
- Established domain convention (STA=station, DIA=diameter, OFF=offset)
- Very local scope (loop variables: I, N, X)
- Part of common pattern (OBJ_, EN_)

### 6.3.2 Examples
```lisp
;; ACCEPTABLE abbreviations
AUTO_TYPE     ; "Automatic text type" - clear in context
STA           ; Universal civil engineering abbreviation
DIA           ; Common diameter abbreviation
OFF           ; Offset (clear in context)

;; ACCEPTABLE loop variables
(FOREACH I LIST_ITEMS ...)
(SETQ N 0)
(WHILE (< X 10) ...)
```

## 6.4 When to Shorten

### 6.4.1 Don't Shorten
DON'T shorten just to save typing - code is read 10x more than written.

### 6.4.2 Do Shorten When
Abbreviation is clearer than full word in domain context.

### 6.4.3 Examples
```lisp
;; GOOD - Domain abbreviation improves clarity
DRAWSTATION   ; Better than DRAWING_STATION_VALUE

;; AVOID - Cryptic abbreviation reduces clarity
PSPACE_BBL_P  ; Worse than PSPACE_BUBBLE_P
ALREF         ; Worse than ALIGN_REFERENCE
```

---
<!-- #endregion -->

<!-- #region 7. FUNCTION NAMING -->
# 7. FUNCTION NAMING

## 7.1 Pattern

### 7.1.1 Standard
**FIRM:** VERB_NOUN or GET/SET/UPDATE/CALCULATE pattern

**Schedule:** Moderately conservative (well-established pattern, apply to new functions, rename only when refactoring)

### 7.1.2 Examples
```lisp
;; CORRECT - Action verbs for clarity
HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT        ; Retrieves object
HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER   ; Formats value
HCNM_LDRBLK_AUTO_PIPE_CALCULATE_SLOPE   ; Performs calculation
HCNM_LDRBLK_UPDATE_BUBBLE_TAG            ; Modifies existing
```

## 7.2 Verb Vocabulary

### 7.2.1 Standard Verbs
- **GET_** - Retrieve data (may prompt user)
- **SET_** - Store data
- **CALCULATE_** - Perform computation (pure function)
- **FORMAT_** - Convert to display string
- **UPDATE_** - Modify existing data
- **ASSURE_** - Check and create if missing
- **VALIDATE_** - Check validity

---
<!-- #endregion -->

<!-- #region 8. SPECIAL CASES -->
# 8. SPECIAL CASES

## 8.1 Constants

### 8.1.1 Standard
Use descriptive names, no special prefix:

```lisp
;; CORRECT
HCNM_ERROR_NOT_FOUND "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
HCNM_DEFAULT_PRECISION "2"
```

## 8.2 Global Variables

### 8.2.1 Standard
Use `*` wrapper if truly global (session lifetime):

```lisp
;; CORRECT - Global session variables
*HCNM_CONFIG*              ; Config cache
*HAWS_CONFIG_CACHE*        ; Multi-app config cache

;; CORRECT - Module-scoped (no asterisks)
HCNM_EB_ATTRIBUTE_LIST     ; Edit bubble dialog state
```

## 8.3 Temporary Variables

### 8.3.1 Standard
In small scopes, single letters acceptable:

```lisp
;; ACCEPTABLE in 5-line loop
(FOREACH I ITEM_LIST
  (SETQ N (1+ N))
)

;; AVOID - Too cryptic if used across 50 lines
(SETQ A (GET_ALIGN)
      S (CALC_STA A P)
      O (CALC_OFF A P)
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
refactor: rename HCNM-LDRBLK-AUTO-PIPE to HCNM_LDRBLK_AUTO_PIPE

- Updated function definition
- Updated all 7 call sites
- Per S03.4.1: moving to underscore delimiters
```

---
<!-- #endregion -->

<!-- #region 10. EXAMPLES SUMMARY -->
# 10. EXAMPLES SUMMARY

## 10.1 Good Example
```lisp
;; CORRECT - Follows all conventions
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER (OBJ_PIPE / DIA_FEET DIA_INCHES)
  (SETQ DIA_FEET   (VLAX-GET-PROPERTY OBJ_PIPE 'InnerDiameterOrWidth)
        DIA_INCHES (* DIA_FEET 12.0)
  )
  (STRCAT
    (C:HCNM-CONFIG-GETVAR "BubbleTextPrefixPipeDia")  ;; OLD code (uses hyphens)
    (RTOS DIA_INCHES 2 (ATOI (C:HCNM-CONFIG-GETVAR "BubbleTextPrecisionPipeDia")))
    (C:HCNM-CONFIG-GETVAR "BubbleTextPostfixPipeDia")
  )
)
```

[AI: Note - C:HCNM-CONFIG-GETVAR uses hyphens because that's the current code. Per S03.4.1, this will be renamed to C:HCNM_CONFIG_GETVAR during refactoring. See S06.5.2.2]

## 10.2 Poor Example
```lisp
;; AVOID - Multiple convention violations
(defun fmtdia (p / d)  ; Lowercase, cryptic names
  (setq d (vlax-get-property p 'InnerDiameterOrWidth))  ; No OBJ_ prefix
  (strcat (getvar "pfx") (rtos (* d 12) 2 2) (getvar "sfx"))  ; Unclear abbreviations
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
(DEFUN CALCULATE_AREA (WIDTH HEIGHT / AREA)
  (SETQ AREA (* WIDTH HEIGHT))
  (IF (> AREA 1000)
    (PRINC "\nLarge area")
    (PRINC "\nSmall area")
  )
  AREA
)
(DEFUN CALCULATE_VOLUME (WIDTH HEIGHT DEPTH)
  (* WIDTH HEIGHT DEPTH)
)
;; AVOID - Any blank lines
(DEFUN CALCULATE_AREA (WIDTH HEIGHT / AREA)

  (SETQ AREA (* WIDTH HEIGHT))

  (IF (> AREA 1000)
    (PRINC "\nLarge area")
    (PRINC "\nSmall area")
  )

  AREA
)

(DEFUN CALCULATE_VOLUME (WIDTH HEIGHT DEPTH)
  (* WIDTH HEIGHT DEPTH)
)
```

---
<!-- #endregion -->

**End of Document**
