# CNM Bubble Note Auto-Text Refactoring Suggestions

## Executive Summary
This document outlines readability and maintainability improvements for the bubble note auto-text system, particularly the coordinate-based features recently enhanced for paper space support.

---

## 1. Function Naming Conventions

### Current Issues
- Mixed naming styles: `HCNM_LDRBLK_AUTO_AL` vs `HCNM_LB:BD_GET`
- Abbreviations not immediately clear: `AL` (alignment), `NE` (northing/easting), `SU` (surface)
- Unclear distinction between get/set/update operations

### Suggestions

#### A. Standardize Prefix Hierarchy
```
HCNM_                  = Application namespace
HCNM_LDRBLK_           = Leader Bubble subsystem
HCNM_LDRBLK_AUTO_      = Auto-text feature
HCNM_LDRBLK_AUTO_AL_   = Alignment-specific auto-text
```

#### B. Use Verb-Noun Pattern
**Current:** `HCNM_LDRBLK_AUTO_AL`
**Suggested:** `HCNM_LDRBLK_AUTO_ALIGNMENT_CALCULATE` or `HCNM_LDRBLK_AUTO_CALCULATE_ALIGNMENT`

**Current:** `HCNM_LDRBLK_AUTO_NE`  
**Suggested:** `HCNM_LDRBLK_AUTO_COORDINATES_CALCULATE` (handles N, E, NE)

#### C. Clarify Operation Type
- `_GET_` = Retrieve data (user input or existing value)
- `_CALCULATE_` = Perform computation
- `_FORMAT_` = Convert to display string
- `_UPDATE_` = Modify existing data
- `_ENSURE_` = Check and create if missing

---

## 2. Variable Naming

### Current Issues
- Single-letter variables: `E`, `N`, `P` (acceptable for coordinates in limited scope)
- Unclear abbreviations: `OBJALIGN`, `PSPACE_BUBBLE_P`
- Inconsistent boolean naming: `PSPACE_BUBBLE_P` vs `FOUND_P`

### Suggestions

#### A. Use Full Words for Key Entities
**Current:** `OBJALIGN`  
**Suggested:** `ALIGNMENT_OBJECT` or keep `OBJALIGN` but add comment explaining it's a VLA-OBJECT

**Current:** `ENAME_BUBBLE`  
**Good:** Clear it's an entity name. Consider `BUBBLE_ENTITY` for consistency with `ALIGNMENT_OBJECT`

#### B. Standardize Boolean Predicates
All boolean variables should end with `_P`:
- `IS_PAPER_SPACE_P` instead of just tracking state
- `HAS_COORDINATES_P` 
- `REQUIRES_VIEWPORT_P`

#### C. Group Related Variables
```lisp
;; Current - scattered declarations
ENAME_BUBBLE ENAME_LEADER P1_WORLD

;; Suggested - logical grouping with comments
;; Bubble entities
ENAME_BUBBLE
ENAME_LEADER

;; Coordinate data
P1_WORLD      ; Leader insertion point in WCS
P1_ENTRY      ; Leader insertion point in entry space
```

---

## 3. Function Decomposition

### HCNM_LDRBLK_AUTO_AL - Current Structure
This 80+ line function does 3 distinct jobs:
1. Get alignment from user or use provided INPUT
2. Calculate station/offset from alignment and P1_WORLD
3. Format station/offset string with prefixes/postfixes

### Suggested Refactoring

```lisp
;; Top-level orchestrator (10-15 lines)
(DEFUN HCNM_LDRBLK_AUTO_ALIGNMENT (BUBBLE_DATA TAG AUTO_TYPE INPUT)
  ;; 1. Get alignment (user selection or provided)
  ;; 2. Calculate station/offset
  ;; 3. Format as text
  ;; 4. Attach reactor
  ;; 5. Return updated BUBBLE_DATA
)

;; Extraction functions (20-30 lines each)
(DEFUN HCNM_LDRBLK_AUTO_ALIGNMENT_GET_OBJECT (ENAME_BUBBLE INPUT)
  ;; Handles user selection or validation of INPUT
  ;; Returns: ALIGNMENT_OBJECT or NIL
)

(DEFUN HCNM_LDRBLK_AUTO_ALIGNMENT_CALCULATE (ALIGNMENT_OBJECT P1_WORLD)
  ;; Pure calculation: takes alignment + point, returns (STA . OFF)
  ;; No side effects, easily testable
)

(DEFUN HCNM_LDRBLK_AUTO_ALIGNMENT_FORMAT (STA OFF AUTO_TYPE)
  ;; Format station/offset with config-based prefixes/postfixes
  ;; Returns: Formatted string ready for attribute
)
```

### Benefits
- Each function has single responsibility
- Easier to test individual pieces
- Calculation logic can be reused for reactor updates
- Clear separation of user interaction from business logic

### Organization Within Files

**✅ AGREED STANDARD: Public API first, private helpers second, alphabetically within sections**

```lisp
;;==============================================================================
;; PUBLIC API - Pipe Network Auto-Text
;;==============================================================================
;; Functions called by other modules. Main entry points.

(DEFUN HCNM_LDRBLK_AUTO_PIPE ...)  ; Main orchestrator (alphabetically first)

;;==============================================================================
;; PRIVATE HELPERS - Pipe Network  
;;==============================================================================
;; Implementation details. Not called directly by external modules.
;; Alphabetically ordered for easy lookup.

(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER ...)  ; A-Z: D before G before L
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_LENGTH ...)
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_SLOPE ...)
(DEFUN HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT ...)       ; G comes after F
```

**Benefits:**
- Public API immediately visible when opening file
- Easy to find functions via alphabetical scan
- Clear architectural boundaries
- VS Code regions fold beautifully

**Migration:** Apply to new modules immediately. Reorganize existing modules opportunistically during refactoring.

---

## 4. Paper Space Architecture

### Current State
Paper space handling is interwoven throughout multiple functions:
- Space switching: `HCNM_LDRBLK_SPACE_SET_MODEL` / `SPACE_RESTORE`
- Viewport detection: `HCNM_LDRBLK_IS_ON_MODEL_TAB`
- Viewport selection: `HCNM_LDRBLK_GET_TARGET_VPORT`
- Transform storage: `HCNM_LDRBLK_SET_VIEWPORT_TRANSFORM_XDATA`

### Suggested Module
Create a "Paper Space Coordinator" module:

```lisp
;;==============================================================================
;; PAPER SPACE COORDINATE TRANSFORMATION SUBSYSTEM
;;==============================================================================

;; High-level: Handles all paper space complexity
(DEFUN HCNM_LDRBLK_PSPACE_PREPARE_FOR_COORDINATES (ENAME_BUBBLE)
  ;; 1. Check if bubble is in paper space
  ;; 2. Show warning if coordinate-based
  ;; 3. Prompt for viewport if needed
  ;; 4. Switch to model space
  ;; Returns: Context object with (NEEDS_RESTORE_P VIEWPORT_ID)
)

(DEFUN HCNM_LDRBLK_PSPACE_CLEANUP (PSPACE_CONTEXT)
  ;; Restore space, re-enable reactors, store transform
)
```

### Benefits
- Centralized paper space logic
- Easier to track reactor suppression state
- Clear entry/exit points for paper space operations

---

## 5. Configuration Variable Naming

### Current Pattern
```lisp
"BubbleTextPrefixSta"
"BubbleTextPostfixOff+"
"BubbleOffsetDropSign"
```

### Observations
- Good: Hierarchical naming (Bubble → Text → Prefix → Sta)
- Good: Descriptive names
- Consider: Group documentation by config scope

### Suggestion
Create config documentation:

```lisp
;;==============================================================================
;; CONFIGURATION VARIABLES - BUBBLE AUTO-TEXT FORMATTING
;;==============================================================================
;; Scope: Application (CNM.INI)
;; 
;; Station Formatting:
;;   BubbleTextPrefixSta     - Text before station value (e.g., "STA ")
;;   BubbleTextPostfixSta    - Text after station value
;;   BubbleTextPrecisionSta  - Decimal places for station
;;
;; Offset Formatting:
;;   BubbleTextPrefixOff+    - Text before positive offset
;;   BubbleTextPrefixOff-    - Text before negative offset (e.g., "LT ")
;;   BubbleTextPrecisionOff+ - Decimal places for offset
;;   BubbleOffsetDropSign    - "1" = show abs value, "0" = show +/-
;;
;; [Additional groups...]
```

---

## 6. Reactor System Documentation

### Current Issue
Reactor data structure is complex nested alist:
```lisp
'("HCNM-BUBBLE" 
  (HANDLE_REFERENCE 
    ((HANDLE_BUBBLE ((TAG AUTO_TYPE) ...))
     ...)
  )
)
```

### Suggestion
Add executive-level comments explaining the architecture:

```lisp
;;==============================================================================
;; REACTOR SYSTEM ARCHITECTURE
;;==============================================================================
;; Purpose: Automatically update bubble auto-text when reference objects change
;;
;; How It Works:
;;   1. When user selects alignment/surface for auto-text, attach :vlr-modified reactor
;;   2. Reactor watches both the reference object (alignment) and leader (for movement)
;;   3. When either changes, reactor callback updates affected bubble attributes
;;
;; Data Structure: Single persistent reactor with nested alist
;;   Level 1: Application key "HCNM-BUBBLE"
;;   Level 2: Reference object handle → list of dependent bubbles
;;   Level 3: Bubble handle → list of (TAG AUTO_TYPE) pairs
;;   Level 4: Individual tag/type that needs updating
;;
;; Key Functions:
;;   HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR - Add/update reactor data
;;   HCNM_LDRBLK_REACTOR_CALLBACK             - Handle modification events
;;   HCNM_LDRBLK_UPDATE_BUBBLE_TAG            - Recalculate and update one tag
;;
;; Reactor Suppression:
;;   AllowReactors config variable prevents recursive triggers during:
;;     - Space transitions (MSPACE/PSPACE commands)
;;     - Reactor-initiated updates (prevents infinite loops)
```

---

## 7. Error Handling Patterns

### Current Approach
- Mix of COND checks and VL-CATCH-ALL-ERROR-P
- Some functions return error strings ("NOT FOUND!!!")
- Reactor has error handling for erased objects

### Suggestions

#### A. Standardize Error Returns
```lisp
;; Option 1: NIL for error, value for success
(DEFUN HCNM_LDRBLK_CALCULATE_STATION (ALIGNMENT P1_WORLD)
  (COND
    ((AND ALIGNMENT P1_WORLD)
     ;; Return (STA . OFF) on success
     (CALCULATE...)
    )
    (T NIL)  ; Clearly indicate failure
  )
)

;; Option 2: Result object pattern
(DEFUN HCNM_LDRBLK_CALCULATE_STATION (ALIGNMENT P1_WORLD)
  (COND
    ((AND ALIGNMENT P1_WORLD)
     (LIST 'SUCCESS (CONS STA OFF))
    )
    (T (LIST 'ERROR "Missing alignment or coordinates"))
  )
)
```

#### B. Centralize Error Messages
```lisp
(DEFUN HCNM_LDRBLK_ERROR_TEXT (ERROR_TYPE)
  (COND
    ((= ERROR_TYPE "COORDINATE_NOT_FOUND")
     "!!!!!!!!!!!!!!!!!NOT FOUND!!!!!!!!!!!!!!!!!!!!!!!")
    ((= ERROR_TYPE "ALIGNMENT_INVALID")
     "Invalid alignment selected")
    ;; ...more error types
  )
)
```

---

## 8. Testing Strategy

### Suggested Test Functions

```lisp
;; Unit test for coordinate calculation
(DEFUN TEST_HCNM_CALCULATE_NORTHING_EASTING ()
  (SETQ TEST_POINT '(1000.0 2000.0 0.0))
  (SETQ RESULT (HCNM_LDRBLK_AUTO_COORDINATES_CALCULATE TEST_POINT "NE"))
  (COND
    ((WCMATCH RESULT "*1000.00*2000.00*")
     (PRINC "\nTest PASSED: NE calculation")
    )
    (T
     (PRINC "\nTest FAILED: Expected N=2000, E=1000")
    )
  )
)

;; Integration test for paper space workflow
(DEFUN TEST_HCNM_PAPER_SPACE_WARNING ()
  ;; Create test bubble in paper space
  ;; Trigger coordinate auto-text
  ;; Verify warning appears
  ;; Verify viewport selection prompt
)
```

---

## 9. Priority Recommendations

### High Priority (Immediate Impact)
1. ✅ Add executive-level comments to coordinate functions (DONE)
2. Document reactor data structure with ASCII art diagram
3. Create config variable reference documentation

### Medium Priority (Next Refactoring Session)
4. Split HCNM_LDRBLK_AUTO_AL into GET/CALCULATE/FORMAT functions
5. Create paper space coordinator module
6. Standardize error handling pattern

### Low Priority (Long-term Maintenance)
7. Rename functions to verb-noun pattern (breaking change)
8. Create unit test suite
9. Extract configuration to separate documented file

---

## 10. Specific Variable Renaming Suggestions

| Current | Suggested | Reason |
|---------|-----------|--------|
| `OBJALIGN` | `ALIGNMENT_OBJECT` | Clarity |
| `PSPACE_BUBBLE_P` | `WAS_IN_PSPACE_P` | More descriptive of what it tracks |
| `AVPORT` | `ACTIVE_VIEWPORT_ID` | Distinguish from CVPORT |
| `CVPORT` | `CURRENT_VIEWPORT_ID` | Full name clearer |
| `AUTO_TYPE` | Keep or `AUTOTEXT_TYPE` | Current is acceptable |
| `TAG` | Keep | Standard CAD term |
| `P1_WORLD` | Keep | Convention established |
| `ENAME_*` | Keep | Standard AutoLISP convention |

---

## Notes
- Many of these are suggestions for future refactoring
- Don't implement all at once - incremental improvements
- Maintain backward compatibility where possible
- Add TODO comments for future refactoring opportunities
- Consider creating a HCNM_LDRBLK_CONSTANTS file for magic strings

---

Last Updated: 2025-10-21  
Prepared for: feat-rbd branch review

 
 