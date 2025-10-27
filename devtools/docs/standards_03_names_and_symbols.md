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
- **Case:** lowercase for code symbols. This means variables and functions. Schedule: Very aggressive
- **Delimiter:** Underscores (not hyphens or colons). This means squash all hyphens and colons. Schedule: Moderately aggressive
  - **Exception:** `c:` prefix for AutoCAD command functions is required by AutoLISP (e.g., `c:cnm`, `c:cnmedit`)
- **Files:** lowercase with underscores. Schedule: Moderately conservative
- **Prefixes:**  Moderately aggressive.
  - `obj_` for VLA-OBJECTS
  - `es_` for the results of (entsel)
  - `en_` for entity names
  - `el_` or `eg_` for the results of (entget en)
  - `lst_` for lists when clarification is needed
  - `_p` suffix for booleans.

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
- Delimiter style (underscores not hyphens)
- Type prefixing (obj_, en_, _p suffix)
- Function naming patterns (verb_noun)
- Name length guidelines
- File and folder naming


---
<!-- #endregion -->

<!-- #region 3. NOTES -->
# 3. CASE CONVENTIONS

## 3.1 Symbols and Functions

### 3.1.1 Standard
**FIRM:** lowercase for all AutoLISP symbols and functions

### 3.1.2 Rationale
**Why lowercase:**
- Matches community convention (most AutoLISP forums and examples use lowercase)
- Matches file naming convention (lowercase_with_underscores)
- Ends the debate - aligns with what everyone else does
- AutoLISP internally converts all code to uppercase anyway (cosmetic choice only)

**Why NOT UPPERCASE:**
- Differs from community standard (looks unusual to other AutoLISP developers)
- Doesn't match file naming (creates inconsistency)
- Creates perpetual "why did you do that?" questions

**Decision:**
This project uses lowercase for consistency with community practice and file naming. AutoLISP converts everything to uppercase internally anyway, so this is purely a source code cosmetic choice.

### 3.1.3 Examples
```lisp
;; CORRECT
(defun hcnm_ldrblk_auto_pipe (bubble_data tag auto_type)
  (setq obj_pipe (get_pipe_object))
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
hcnm_ldrblk_auto_pipe_format_diameter
└──┬─┘ └──┬──┘ └─┬─┘└─┬─┘ └────┬────┘
   │      │       │    │        └─ Operation
   │      │       │    └──────── Type
   │      │       └───────────── Feature
   │      └───────────────────── Subsystem
   └──────────────────────────── Namespace
```

### 4.1.4 Migration Strategy
- New code: Always use underscores
- Refactoring: We aim for about half of our commits to be dedicated to refactoring one or a few symbols.
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
Use `obj_` prefix to distinguish from entity names and lists:

```lisp
;; CORRECT
obj_pipe      ; VLA-OBJECT of pipe
obj_align     ; VLA-OBJECT of alignment
obj_surface   ; VLA-OBJECT of surface

;; AVOID (ambiguous type)
pipe          ; Could be entity, object, or string
alignment     ; Unclear what type
```

### 5.2.2 Entity Names
Use `en_` prefix:

```lisp
;; CORRECT
en_bubble
en_leader
en_block
```

### 5.2.3 Entsel Results
Use `es_` prefix:

```lisp
;; CORRECT
es_circle  ; The result of (entsel) for the circle
```

### 5.2.4 Entget Lists
Use `eg_` or `el_` prefix if stored/passed around:

```lisp
;; CORRECT
eg_bubble  ; (entget en_bubble)
el_bubble  ; (entget en_bubble)
```

### 5.2.5 Selection Sets
Use `ss_` prefix:

```lisp
;; CORRECT
ss_trees  ; A selection set of trees
ss1       ; Generic selection set
```

## 5.3 Boolean Suffix

### 5.3.1 Standard
Use `_p` suffix (Lisp predicate convention):

```lisp
;; CORRECT
found_p
pspace_p
modified_p
has_attributes_p

;; AVOID (doesn't follow Lisp convention)
is_found
pspace_flag
is_pspace_p    ; Redundant - just use pspace_p
was_modified_p ; Redundant - just use modified_p
```

## 5.4 Optional Prefixes

### 5.4.1 Generic Lists
Only prefix if type is ambiguous:

```lisp
;; CLEAR (no prefix needed)
attribute_list
phase_list
reactor_list
bubbles  ; A list of bubble items

;; ACCEPTABLE (when clarification helps)
lst_bubble_data
lst_bubbles
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
hcnm_ldrblk_auto_pipe_format_diameter
bubble_text_prefix_pipe_dia
hcnm_ldrblk_assure_auto_text_has_reactor
```

## 6.3 Short Names (Acceptable)

### 6.3.1 Use When
- Established domain convention (STA=station, DIA=diameter, OFF=offset)
- Very local scope (loop variables: I, N, X)
- Part of common pattern (OBJ_, EN_)

### 6.3.2 Examples
```lisp
;; ACCEPTABLE abbreviations
auto_type     ; "Automatic text type" - clear in context
sta           ; Universal civil engineering abbreviation
dia           ; Common diameter abbreviation
off           ; Offset (clear in context)

;; ACCEPTABLE loop variables
(foreach i list_items ...)
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
drawstation   ; Better than drawing_station_value

;; AVOID - Cryptic abbreviation reduces clarity
pspace_bbl_p  ; Worse than pspace_bubble_p
alref         ; Worse than align_reference
```

---
<!-- #endregion -->

<!-- #region 7. FUNCTION NAMING -->
# 7. FUNCTION NAMING

## 7.1 Pattern

### 7.1.1 Standard
**FIRM:** verb_noun or get/set/update/calculate pattern

**Schedule:** Moderately conservative (well-established pattern, apply to new functions, rename only when refactoring)

### 7.1.2 Examples
```lisp
;; CORRECT - Action verbs for clarity
hcnm_ldrblk_auto_pipe_get_object        ; Retrieves object
hcnm_ldrblk_auto_pipe_format_diameter   ; Formats value
hcnm_ldrblk_auto_pipe_calculate_slope   ; Performs calculation
hcnm_ldrblk_update_bubble_tag            ; Modifies existing
```

## 7.2 Verb Vocabulary

### 7.2.1 Standard Verbs
- **get_** - Retrieve data (may prompt user)
- **set_** - Store data
- **calculate_** - Perform computation (pure function)
- **format_** - Convert to display string
- **update_** - Modify existing data
- **assure_** - Check and create if missing
- **validate_** - Check validity

---
<!-- #endregion -->

<!-- #region 8. SPECIAL CASES -->
# 8. SPECIAL CASES

## 8.1 Constants

### 8.1.1 Standard
Use descriptive names, no special prefix:

```lisp
;; CORRECT
hcnm_error_not_found "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
hcnm_default_precision "2"
```

## 8.2 Global Variables

### 8.2.1 Standard
Use `*` wrapper if truly global (session lifetime):

```lisp
;; CORRECT - Global session variables
*hcnm_config*              ; Config cache
*haws_config_cache*        ; Multi-app config cache

;; CORRECT - Module-scoped (no asterisks)
hcnm_eb_attribute_list     ; Edit bubble dialog state
```

## 8.3 Temporary Variables

### 8.3.1 Standard
In small scopes, single letters acceptable:

```lisp
;; ACCEPTABLE in 5-line loop
(foreach i item_list
  (setq n (1+ n))
)

;; AVOID - Too cryptic if used across 50 lines
(setq a (get_align)
      s (calc_sta a p)
      o (calc_off a p)
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
refactor: rename hcnm-ldrblk-auto-pipe to hcnm_ldrblk_auto_pipe

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
(defun hcnm_ldrblk_auto_pipe_format_diameter (obj_pipe / dia_feet dia_inches)
  (setq dia_feet   (vlax-get-property obj_pipe 'InnerDiameterOrWidth)
        dia_inches (* dia_feet 12.0)
  )
  (strcat
    (c:hcnm_config_getvar "BubbleTextPrefixPipeDia")
    (rtos dia_inches 2 (atoi (c:hcnm_config_getvar "BubbleTextPrecisionPipeDia")))
    (c:hcnm_config_getvar "BubbleTextPostfixPipeDia")
  )
)
```

## 10.2 Poor Example
```lisp
;; AVOID - Multiple convention violations
(DEFUN FMTDIA (P / D)  ; UPPERCASE not project standard
  (SETQ D (VLAX-GET-PROPERTY P 'InnerDiameterOrWidth))  ; No obj_ prefix, UPPERCASE
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
(defun calculate_area (width height / area)
  (setq area (* width height))
  (if (> area 1000)
    (princ "\nLarge area")
    (princ "\nSmall area")
  )
  area
)
(defun calculate_volume (width height depth)
  (* width height depth)
)
;; AVOID - Any blank lines
(defun calculate_area (width height / area)

  (setq area (* width height))

  (if (> area 1000)
    (princ "\nLarge area")
    (princ "\nSmall area")
  )

  area
)

(defun calculate_volume (width height depth)
  (* width height depth)
)
```

---
<!-- #endregion -->

**End of Document**
