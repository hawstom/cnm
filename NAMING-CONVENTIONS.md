# CNM Naming Conventions

## Purpose
Document agreed-upon naming standards for CNM codebase to ensure consistency and maintainability.

---

## 1. Case Convention

**Standard: ALL CAPS**

Rationale:
- AutoLISP reads all code as uppercase regardless of input
- Matches AutoCAD native function style
- Existing codebase is ALL CAPS
- Consistency > aesthetics

```lisp
;; ✅ Correct
(DEFUN HCNM_LDRBLK_AUTO_PIPE (BUBBLE_DATA TAG AUTO_TYPE)
  (SETQ OBJ_PIPE (GET_PIPE_OBJECT))
)

;; ❌ Avoid (fights AutoLISP nature)
(defun hcnm_ldrblk_auto_pipe (bubble_data tag auto_type)
  (setq obj_pipe (get_pipe_object))
)
```

---

## 2. Delimiter Style

**Standard: Underscores for functions, Colons for data modules**

### Functions: Use underscores
```lisp
HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
└──┬─┘ └──┬──┘ └─┬─┘└─┬─┘ └────┬────┘
   │      │       │    │        └─ Operation
   │      │       │    └──────── Type
   │      │       └───────────── Feature
   │      └───────────────────── Subsystem
   └──────────────────────────── Namespace
```

### Data Modules: Use colons
```lisp
HCNM_LB:BD_GET    ; "Leader-Bubble : Bubble-Data getter"
     └─┬─┘
       └─ Module namespace separator (colon)
```

**Note on hyphens:** While hyphens don't require shift key, underscores provide better consistency with existing codebase.

---

## 3. Type Prefixing

**Standard: Prefix critical types, suffix booleans**

### VLA-OBJECT (Most Important)
Use `OBJ_` prefix to distinguish from entity names and lists:

```lisp
;; ✅ Correct
OBJ_PIPE      ; VLA-OBJECT of pipe
OBJ_ALIGN     ; VLA-OBJECT of alignment
OBJ_SURFACE   ; VLA-OBJECT of surface

;; ❌ Avoid (ambiguous type)
PIPE          ; Could be entity, object, or string
ALIGNMENT     ; Unclear what type
```

### Entity Names
Use `ENAME_` prefix (established convention):

```lisp
;; ✅ Correct
ENAME_BUBBLE
ENAME_LEADER
ENAME_BLOCK

;; Alternative (also acceptable)
EN_BUBBLE     ; Shorter, matches ENTGET→EL convention
```

### Entget Lists
Context usually makes these clear. Prefix only if ambiguous:

```lisp
;; ✅ Acceptable without prefix (clear from context)
EL (ENTGET ENAME_BUBBLE)

;; ✅ Use prefix if stored/passed around
ELIST_BUBBLE (ENTGET ENAME_BUBBLE)
```

### Booleans
Use `_P` suffix (Lisp predicate convention):

```lisp
;; ✅ Correct
FOUND_P
IS_PSPACE_P
WAS_MODIFIED_P
HAS_ATTRIBUTES_P

;; ❌ Avoid (doesn't follow Lisp convention)
IS_FOUND
PSPACE_FLAG
```

### Generic Lists
Only prefix if type is ambiguous:

```lisp
;; ✅ Clear from context (no prefix needed)
ATTRIBUTE_LIST
PHASE_LIST
REACTOR_LIST

;; ❌ Unnecessary prefix
LIST_ATTRIBUTES
LS_PHASES
```

---

## 4. Name Length Guidelines

**Principle: Favor clarity over brevity**

### Long Names (Preferred)
Use full words for:
- Public API functions
- Configuration variables
- Complex domain concepts

```lisp
;; ✅ Good - clear at a glance
HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
BUBBLE_TEXT_PREFIX_PIPE_DIA
HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR
```

### Short Names (Acceptable)
Use abbreviations only when:
- Established domain convention (STA=station, DIA=diameter, OFF=offset)
- Very local scope (loop variables: I, N, X)
- Part of common pattern (OBJ_, ENAME_)

```lisp
;; ✅ Acceptable abbreviations
AUTO_TYPE     ; "Automatic text type" - clear in context
STA           ; Universal civil engineering abbreviation
DIA           ; Common diameter abbreviation
OFF           ; Offset (clear in context)

;; ✅ Acceptable loop variables
(FOREACH I LIST_ITEMS ...)
(SETQ N 0)
(WHILE (< X 10) ...)
```

### When to Shorten
**DON'T shorten just to save typing** - code is read 10x more than written.
**DO shorten when** abbreviation is clearer than full word in domain context.

```lisp
;; ✅ Domain abbreviation improves clarity
DRAWSTATION   ; Better than DRAWING_STATION_VALUE

;; ❌ Cryptic abbreviation reduces clarity
PSPACE_BBL_P  ; Worse than PSPACE_BUBBLE_P
ALREF         ; Worse than ALIGN_REFERENCE
```

---

## 5. Function Naming Pattern

**Standard: VERB_NOUN or GET/SET/UPDATE/CALCULATE pattern**

```lisp
;; ✅ Action verbs for clarity
HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT        ; Retrieves object
HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER   ; Formats value
HCNM_LDRBLK_AUTO_PIPE_CALCULATE_SLOPE   ; Performs calculation
HCNM_LDRBLK_UPDATE_BUBBLE_TAG            ; Modifies existing

;; Verb vocabulary:
;; GET_      - Retrieve data (may prompt user)
;; SET_      - Store data
;; CALCULATE_ - Perform computation (pure function)
;; FORMAT_   - Convert to display string
;; UPDATE_   - Modify existing data
;; ASSURE_   - Check and create if missing
;; VALIDATE_ - Check validity
```

---

## 6. Organization Within Files

**Standard: Public API first, then private helpers, alphabetically within sections**

```lisp
;;==============================================================================
;; PUBLIC API - Pipe Network Auto-Text
;;==============================================================================
;; Functions called by other modules

(DEFUN HCNM_LDRBLK_AUTO_PIPE ...)  ; Main entry point (alphabetically first)

;;==============================================================================
;; PRIVATE HELPERS - Pipe Network
;;==============================================================================
;; Implementation details, alphabetically ordered

(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER ...)  ; D comes before G
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_LENGTH ...)
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_SLOPE ...)
(DEFUN HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT ...)       ; G comes last
```

**Benefits:**
- Public API jumps out when scanning file
- Easy to find functions alphabetically
- Clear separation of concerns
- VS Code regions fold nicely

---

## 7. Formal Comment Headers

**Standard: Use structured headers for all public functions**

### Template
```lisp
;;==============================================================================
;; FUNCTION_NAME
;;==============================================================================
;; Purpose:
;;   One-sentence summary of what function does.
;;   Additional detail if needed.
;;
;; Arguments:
;;   ARG1 - Description (required/optional) [type if helpful]
;;   ARG2 - Description (required/optional)
;;
;; Returns:
;;   Description of return value and type
;;
;; Side Effects:
;;   - Any user prompts, file I/O, or state changes
;;   - Reactor attachments
;;   - Space switching
;;
;; Related:
;;   OTHER_FUNCTION_1
;;   OTHER_FUNCTION_2
;;
;; Example:
;;   (SETQ RESULT (FUNCTION_NAME ARG1 ARG2))
;;==============================================================================
```

### When to Use
- ✅ All public API functions (called by other modules)
- ✅ Complex private functions (>30 lines)
- ⚠️ Simple private helpers (optional - use judgment)

### Example
```lisp
;;==============================================================================
;; HCNM_LDRBLK_AUTO_PIPE
;;==============================================================================
;; Purpose:
;;   Main pipe network auto-text function. Orchestrates getting pipe object,
;;   extracting property, formatting, and attaching reactor for updates.
;;
;; Arguments:
;;   BUBBLE_DATA - Bubble data alist (required)
;;   TAG - Attribute tag to update (required)
;;   AUTO_TYPE - Property type: "Dia", "Slope", or "L" (required)
;;   INPUT - Pre-selected pipe VLA-OBJECT (optional, for reactor callbacks)
;;
;; Returns:
;;   Updated BUBBLE_DATA with new attribute value
;;
;; Side Effects:
;;   - Prompts user for pipe selection if INPUT is NIL
;;   - Attaches VLR-OBJECT-REACTOR to pipe for automatic updates
;;   - Switches to model space temporarily if bubble is in paper space
;;
;; Related:
;;   HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER
;;   HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT
;;   HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR
;;
;; Example:
;;   (SETQ BUBBLE_DATA
;;     (HCNM_LDRBLK_AUTO_PIPE BUBBLE_DATA "NOTETXT1" "Dia" NIL)
;;   )
;;==============================================================================
```

---

## 8. Special Cases

### Constants
Use descriptive names, no special prefix:

```lisp
;; ✅ Clear constant names
HCNM_ERROR_NOT_FOUND "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!"
HCNM_DEFAULT_PRECISION "2"
```

### Global Variables
Use `*` wrapper if truly global (session lifetime):

```lisp
;; ✅ Global session variables
*HCNM_CONFIG*              ; Config cache
*HAWS-CONFIG:CACHE*        ; Multi-app config cache

;; ✅ Module-scoped (no asterisks)
HCNM_EB:ATTRIBUTE_LIST     ; Edit bubble dialog state
```

### Temporary Variables
In small scopes, single letters acceptable:

```lisp
;; ✅ Acceptable in 5-line loop
(FOREACH I ITEM_LIST
  (SETQ N (1+ N))
)

;; ❌ Too cryptic if used across 50 lines
(SETQ A (GET_ALIGN)
      S (CALC_STA A P)
      O (CALC_OFF A P)
)
```

---

## 9. Migration Strategy

### For New Code
- ✅ Apply all conventions immediately
- ✅ Use formal headers for public functions
- ✅ Follow organization pattern (public/private/alphabetical)

### For Existing Code
- ⚠️ **Don't rename without good reason** (avoid churn)
- ✅ **Do rename when:**
  - Refactoring a module anyway
  - Name is actively causing confusion
  - Small scope (5-10 occurrences)
- ✅ **Add headers to** functions being modified
- ✅ **Document with TODO** if rename would help but isn't urgent

### Breaking Changes
- Save for major version bumps
- Use scripting for bulk renames
- Update all callers atomically

---

## 10. Examples Summary

```lisp
;; ✅ GOOD - Follows all conventions
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER (OBJ_PIPE / DIA_FEET DIA_INCHES)
  (SETQ DIA_FEET   (VLAX-GET-PROPERTY OBJ_PIPE 'InnerDiameterOrWidth)
        DIA_INCHES (* DIA_FEET 12.0)
  )
  (STRCAT
    (C:HCNM-CONFIG-GETVAR "BubbleTextPrefixPipeDia")
    (RTOS DIA_INCHES 2 (ATOI (C:HCNM-CONFIG-GETVAR "BubbleTextPrecisionPipeDia")))
    (C:HCNM-CONFIG-GETVAR "BubbleTextPostfixPipeDia")
  )
)

;; ❌ POOR - Multiple convention violations
(defun fmtdia (p / d)  ; Lowercase, cryptic names
  (setq d (vlax-get-property p 'InnerDiameterOrWidth))  ; No OBJ_ prefix
  (strcat (getvar "pfx") (rtos (* d 12) 2 2) (getvar "sfx"))  ; Unclear abbreviations
)
```

---

## Notes

- **Consistency > perfection** - following 80% of conventions consistently is better than 100% inconsistently
- **Context matters** - use judgment for edge cases
- **Document exceptions** - if you break a convention, add comment explaining why
- **Evolve prudently** - conventions can change, but do so deliberately with team discussion

---

**Last Updated:** 2025-10-21  
**Branch:** feat-rbd  
**Status:** Active - apply to all new code
