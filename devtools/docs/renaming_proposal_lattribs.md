# Naming Proposal: Moving Beyond Style to Meaning

**Date:** October 28, 2025  
**Status:** FINAL - All Questions Resolved  
**Goal:** Replace ambiguous terms with precise, architecture-revealing names

**Approved By:** TGH (2025-10-28)

## Philosophy

We've learned how to "say" names correctly (hyphens, prefixes). Now we choose names that reveal architecture and data flow. CNM is a 25-year system that will live another 25 years. Names matter.

## Core Terms - The Building Blocks

### Agreed Terminology

| Old Term | New Term | Pronunciation | Usage |
|----------|----------|---------------|-------|
| attribute-list | **lattribs** | "ela-tribs" | In-memory list of bubble text with prefix/auto/postfix structure |
| (varies) | **dwg** | "D-W-G" | Drawing representation (attributed block) |
| (varies) | **dlg** | "dialog" | DCL dialog field representation |
| XDATA | **xdata** | "ex-data" | Extended entity data (keep as-is) |
| format/adjust-format | **underover** | "under-over" | Apply/remove underline/overline codes |
| reactor | **reactor** | (keep as-is) | Object reactor for dynamic updates |
| bubble-data | **bubble-data** | (keep as-is) | Not a building block; clear as-is |

### Why These Terms?

**lattribs** (attribute-list):
- SHORT: notorious within CNM scope, used constantly
- CLEAR: distinguishes from generic AutoCAD attributes
- PRONOUNCEABLE: "ela-tribs" flows naturally

**dwg** (drawing):
- PRECISE: means "the drawing representation" 
- UNAMBIGUOUS: in context, `lattribs-to-dwg` = "write to drawing"
- FAMILIAR: standard CAD term

**dlg** (dialog):
- STANDARD: common abbreviation
- BUILDING BLOCK: used in many function names

**underover** (format/adjust-format):
- DESCRIPTIVE: exactly what it does (underline line 1, overline line 2)
- DUAL PURPOSE: works as verb and noun
- REPLACES: the misleading term "format" which confused with parsing/concatenation

**bubble-data** (keep):
- Not a building block (limited scope)
- More than a list (structured alist with specific keys)
- Already clear and satisfactory

## Naming Pattern: Data Flow Functions

Use `source-to-destination` pattern to make data flow explicit:

### Before (Ambiguous):
```lisp
(hcnm-save-bubble ename lattribs)          ; Save where?
(hcnm-read-bubble-data ename)              ; Read into what?
(hcnm-adjust-formats lattribs)             ; What kind of adjustment?
(hcnm-get-attributes ename)                ; Get from where? Into what format?
```

### After (Clear):
```lisp
(hcnm-lattribs-to-dwg ename lattribs)      ; Write lattribs to drawing
(hcnm-dwg-to-lattribs ename)               ; Read from drawing to lattribs
(hcnm-lattribs-underover lattribs)         ; Apply underline/overline codes
(hcnm-dwg-to-lattribs-raw ename)           ; Read without splitting auto-text
```

## The Rosetta Stone - Function Renames

### Category 1: Data Flow (Major Renames)

**Architecture Discovery:** Deep code reading revealed a two-level architecture:
- **Modern (4-element):** `(("TAG" prefix auto postfix) ...)` - Used throughout editing
- **Legacy (2-element):** `(("TAG" "value") ...)` - Only 4 call sites remain

**Decision: ELIMINATE legacy functions** and migrate all call sites.

#### Final Category 1 Table

| Old Name | New Name | Rationale |
|----------|----------|-----------|
| `hcnm-save-bubble` | `hcnm-ldrblk-lattribs-to-dwg` | Explicit: 4-element lattribs → dwg (with underover + xdata) |
| `hcnm-read-bubble-data` | `hcnm-ldrblk-dwg-to-lattribs` | Explicit: dwg + xdata → 4-element lattribs |
| `hcnm-save-bubble-xdata` | `hcnm-ldrblk-xdata-save` | Helper: Save xdata portion only |
| `hcnm-get-attributes` | **ELIMINATE** | Replace with generic `haws-dwg-attribs-to-alist` (3 call sites) |
| `hcnm-set-attributes` | **ELIMINATE** | Replace with `hcnm-ldrblk-lattribs-to-dwg` (1 call site) |

**New Generic Function:** Create `haws-dwg-attribs-to-alist` in edclib.lsp
- Purpose: Generic attribute reader for non-ldrblk code (quantity take-off, reports)
- Returns: Simple alist `(("TAG" . "value") ...)` without lattribs complexity
- Separates concerns: lattribs is ldrblk-specific, alist is universal

### Category 2: Lattribs Operations (Building Block)

**Naming Pattern:** Noun-verb (object-oriented) - approved by TGH

**Philosophy Changes:**
1. **No flex input:** Separate explicit functions instead of magic type checking
2. **Validate don't repair:** Fail fast with clear errors, don't silently fix problems
3. **Clear naming:** Each function name states exactly what it does

#### Final Category 2 Table

| Old Name | New Name | Rationale |
|----------|----------|-----------|
| `hcnm-ldrblk-initialize-attribute-list` | `hcnm-ldrblk-lattribs-initialize` | Creates empty lattribs structure |
| `hcnm-ldrblk-save-attribute-to-list` | `hcnm-ldrblk-lattribs-put-element` | Puts complete 3-element (prefix auto postfix) |
| (new) | `hcnm-ldrblk-lattribs-put-string` | Convenience: puts string to prefix, empty auto/postfix |
| `hcnm-ldrblk-save-auto-to-list` | `hcnm-ldrblk-lattribs-put-auto` | Puts auto-text only, preserves prefix/postfix |
| `hcnm-ldrblk-ensure-fields` | `hcnm-ldrblk-lattribs-validate` | Validates structure, FAILS if invalid (not repair) |
| `hcnm-ldrblk-ensure-fields-and-adjust-formats` | `hcnm-ldrblk-lattribs-validate-and-underover` | Validates + applies underover |

**Why "initialize" not "define":**
- Common Lisp uses `initialize-instance` for setting up new objects
- "define" means declare symbol binding (`defun`, `defvar`)
- "initialize" means prepare new instance with initial state

**Why separate put functions:**
- **`lattribs-put-element`**: Explicit, requires caller to format correctly
- **`lattribs-put-string`**: Clear intent (user text → prefix)
- **`lattribs-put-auto`**: Clear intent (auto-text only, preserve user's prefix/postfix)

### Category 3: Format → String (Semantic Fix)

**Key insight:** These functions convert numbers to formatted strings, NOT apply underover codes.

**Pattern:** `source-to-string` makes data flow explicit

#### Final Category 3 Table

| Old Name | New Name | Rationale |
|----------|----------|-----------|
| `hcnm-ldrblk-adjust-formats` | `hcnm-ldrblk-lattribs-underover` | Applies underover codes to TXT1/TXT2/GAP |
| `hcnm-ldrblk-adjust-format` | `hcnm-ldrblk-string-underover` | Generic: apply underover to any string |
| `hcnm-ldrblk-auto-alignment-format-station` | `hcnm-ldrblk-auto-al-station-to-string` | number → string (explicit!) |
| `hcnm-ldrblk-auto-alignment-format-offset` | `hcnm-ldrblk-auto-al-offset-to-string` | number → string |
| `hcnm-ldrblk-auto-pipe-format-diameter` | `hcnm-ldrblk-auto-pipe-dia-to-string` | number → string |
| `hcnm-ldrblk-auto-pipe-format-slope` | `hcnm-ldrblk-auto-pipe-slope-to-string` | number → string |
| `hcnm-ldrblk-auto-pipe-format-length` | `hcnm-ldrblk-auto-pipe-length-to-string` | number → string |

**Why "to-string" is better:**
- Explicit data flow: number → string
- Distinguishes from "underover" (which operates on existing strings)
- Standard Lisp pattern: `real-to-string`, `int-to-string`
- Matches community conventions

**Abbreviations justified:**
- **"al"** for alignment: Already established in codebase
- **"dia"** for diameter: Standard engineering abbreviation (external standard, not just programming)

**Underover consolidation:**
- Renamed from `adjust-formats` (confusing - "format" means many things)
- Now handles ALL underover application (TXT1, TXT2, GAP)
- Single function with conditionals (is it line1? line2? has content?)

### Category 4: Minor Parameter/Local Variable Renames

These are parameter and local variable renames (hundreds of instances):

| Old Name | New Name | Scope |
|----------|----------|-------|
| `attribute-list` | `lattribs` | Parameter/local variable |
| `attribute-data` | `lattribs` | Parameter/local variable |
| `attribute_list` | `lattribs` | Parameter/local variable (some use underscores) |

## Data Flow Diagram (With New Names)

```
USER INPUT
    ↓
[lattribs] ← hcnm-ldrblk-lattribs-initialize
    ↓
[lattribs] ← hcnm-ldrblk-lattribs-put-string (user text → prefix)
    ↓
[lattribs] ← hcnm-ldrblk-lattribs-put-auto (auto-text → auto field)
    ↓
[lattribs] ← hcnm-ldrblk-lattribs-validate
    ↓
    ├─→ hcnm-ldrblk-xdata-save → [xdata] (auto-text only)
    │
    ├─→ hcnm-ldrblk-lattribs-underover → [lattribs] (TXT1/TXT2/GAP codes)
    │
    └─→ hcnm-ldrblk-lattribs-to-dwg → [dwg] (concatenate + write)

EDITING:
[dwg] + [xdata]
    ↓
hcnm-ldrblk-dwg-to-lattribs (splits using xdata)
    ↓
[lattribs]
    ↓
(user edits via dialog)
    ↓
hcnm-ldrblk-lattribs-to-dwg
```

## Architecture Glossary (For standards_05_architecture.md)

```markdown
## Glossary - Core Data Structures and Interfaces

### In-Memory Working Data

**lattribs** (attribute-list)
- The core in-memory representation of bubble text
- Format: `(("TAG" prefix auto postfix) ...)`
- Example: `(("NOTETXT1" "Storm " "N=12345.67" " Inlet"))`
- Scope: Ubiquitous throughout bubble note operations
- Why "lattribs": Short, notorious, distinguishes from raw AutoCAD attributes

**bubble-data**
- Association list containing all bubble state during insertion/editing
- Contains: lattribs, ename-bubble, ename-leader, p1-world, AVPORT, etc.
- Scope: Only during active operations (not persisted)
- Format: `((key . value) ...)`
- Why keep this name: Limited scope, already clear, not a building block

### Data Sources and Destinations

**dwg** (drawing)
- AutoCAD attributed block representation
- Visible to user in the drawing
- Contains concatenated text with underover codes: `%%uStorm N=12345.67 Inlet`
- Why "dwg": Standard CAD term, unambiguous in function names like `lattribs-to-dwg`

**xdata** (extended entity data)
- Invisible storage attached to bubble block
- Stores auto-text values separately from display text
- Preserves editability: user text stays in prefix, auto-text retrievable
- Why keep: Standard AutoCAD term, universally understood

**dlg** (dialog)
- DCL dialog field representation during editing
- Temporary: exists only while dialog is open
- Contains formatted text with underover codes (user expectation)
- Why "dlg": Standard abbreviation, used in many function names

**user** (user input)
- Command-line prompts or direct user entry
- Source for manual note numbers, text, selections
- Ephemeral: exists only during the prompt/command
- Why: Makes data flow clear when user is the source: `user-to-lattribs`

**reactor**
- AutoCAD object reactor for dynamic updates
- Watches for changes to referenced objects (alignments, pipes, leaders)
- Triggers auto-text recalculation
- Why keep: Standard term, "react" looks like a verb

### Operations

**underover** (verb/noun)
- Apply or remove underline/overline format codes
- Line 1: underline (`%%u` or `\L`)
- Line 2: overline (`%%o` or `\O`)
- Replaces ambiguous term "format"
- Why: Precisely describes what it does, dual-purpose word

**to** (data flow indicator)
- Indicates direction in function names
- Pattern: `source-to-destination`
- Examples: `lattribs-to-dwg`, `dwg-to-lattribs`, `xdata-to-lattribs`, `real-to-string`
- Why: Makes data flow architecturally visible
```

## Validation Questions

All questions below have been **RESOLVED** and approved by TGH.

### 1. Clarity Check
**Question:** Can someone reading `hcnm-ldrblk-lattribs-to-dwg` understand it means "write lattribs to drawing"?

**Resolution:** YES - The pattern reads naturally once you understand the building blocks. The data flow is clear: `dwg → lattribs → underover → dwg`

### 2. Consistency Check
**Question:** Do all data-flow functions follow `source-to-destination` pattern? Are there any exceptions?

**Resolution:** YES with appropriate exceptions - Category 1 functions use source-to-destination pattern. Category 2 functions (operations ON lattribs, not transformations BETWEEN representations) correctly use noun-verb pattern instead.

### 3. Completeness Check
**Question:** Have we identified all functions that need renaming? Are there local variables that also need consistent naming?

**Resolution:** YES - All functions identified across 4 categories. Local variables will be renamed with care during implementation.

### 4. Conflict Check
**Question:** Do any new names conflict with existing AutoLISP or AutoCAD functions?

**Resolution:** NO conflicts - All names use `hcnm-ldrblk-` prefix which provides namespace isolation.

### 5. Naming Pattern Consistency
**Question:** Are naming patterns consistent across all categories?

**Resolution:** YES - Patterns validated:
- Category 1 (data flow): `source-to-destination` ✓
- Category 2 (operations): `noun-verb` (object-oriented style) ✓
- Category 3 (conversions): `source-to-string` pattern ✓

---

## Implementation Strategy

### Phase 1: Documentation and Validation
1. **Git commit current state** with message: "docs: finalize renaming proposal (canonical)"
2. **Review and validate** this proposal document (TGH review)
3. **Address any final concerns** before implementation

### Phase 2: Code Changes
4. **Generate PowerShell rename script** (longest-first, sorted)
5. **Test on copy** of cnm.lsp first
6. **Review diff carefully** to catch any unintended replacements
7. **Run on actual** cnm.lsp
8. **Git commit** with message: "refactor: rename lattribs functions for clarity"

### Phase 3: Testing
9. **Load in AutoCAD** to check for syntax errors
10. **Test create note** (basic functionality)
11. **Test edit note** (lattribs round-trip)
12. **Test copy note** (xdata preservation - currently buggy!)
13. **Test auto-text** (GAP, alignment, pipe integration)

### Phase 4: Architecture Improvements (Future Projects)
- Migrate 4 legacy call sites to new functions
- Fix copy note xdata preservation bug
- Eliminate `hcnm-get-attributes` and `hcnm-set-attributes`
- Add comprehensive error messages to `lattribs-validate`
- Attempt #region grouping to identify remaining naming issues

---

## Migration Guide for Legacy Functions

The following 4 call sites use legacy functions that will be eliminated after migration:

### Call Site 1: Quantity Take-off (Line 434)
**Current:**
```lisp
(hcnm-get-attributes ent)  ; Returns (("TAG" "value") ...)
```

**Migrate To:**
```lisp
(haws-dwg-attribs-to-alist ent)  ; Generic 2-element reader
```

**Reason:** Quantity take-off needs simple tag-value pairs, not leader-specific 4-element format.

### Call Site 2: Edit Bubble (Line 5463)
**Current:**
```lisp
(setq bdat (hcnm-get-attributes bubbleName))
```

**Migrate To:**
```lisp
(setq lattribs (hcnm-ldrblk-dwg-to-lattribs bubbleName))
```

**Reason:** Edit operations need full 4-element format with xdata splitting.

### Call Site 3: Copy Note (Line 5472) - **HAS BUG**
**Current:**
```lisp
(setq attList (hcnm-get-attributes entName))
```

**Migrate To:**
```lisp
(setq lattribs (hcnm-ldrblk-dwg-to-lattribs entName))
```

**Reason:** Must preserve xdata when copying. Current code loses auto-text xdata!

### Call Site 4: Update Tag (Line 8323)
**Current:**
```lisp
(hcnm-set-attributes entityName (list (list "TAG" value)))
```

**Migrate To:**
```lisp
(setq lattribs (hcnm-ldrblk-dwg-to-lattribs entityName))
(setq lattribs (hcnm-ldrblk-lattribs-put-string lattribs "TAG" value))
(hcnm-ldrblk-lattribs-to-dwg entityName lattribs)
```

**Reason:** Should use proper lattribs abstraction, not direct 2-element writes.

---

## Projects Discovered During Analysis

### Project: Fix Copy Note xdata Preservation
**Bug:** Copy Note feature (line 5472) uses `hcnm-get-attributes` which doesn't preserve xdata. When user copies a note with auto-text, the auto-text information is lost.

**Fix:** Migrate to `hcnm-ldrblk-dwg-to-lattribs` which reads xdata and splits concatenated attributes properly.

**Priority:** Medium - affects user workflow, but workaround exists (manually re-enter auto-text)

---

## Review Summary

All questions have been **RESOLVED** and approved by TGH (2025-10-28):

1. ✅ **lattribs**: Pronounceable, canonical term
2. ✅ **dwg**: Clear, no confusion with file format
3. ✅ **underover**: Best available term, precise and clear
4. ✅ **-to-string pattern**: Implemented throughout Category 3
5. ✅ **Scope**: ALL parameters and locals renamed for consistency
6. ✅ **Legacy functions**: Migrate 4 call sites, then eliminate
7. ✅ **#region grouping**: Next step after renaming to identify remaining issues

---

## Conclusion

This proposal represents a comprehensive architectural clarification of the leader block attribute system. The renaming moves from style-based conventions (hyphens, abbreviations) to meaning-based terminology that makes the data flow and operations explicit.

**Key Achievements:**
- **Unified terminology**: `lattribs` as canonical in-memory format
- **Clear data flow**: `source-to-destination` pattern (dwg↔lattribs↔xdata)
- **Explicit operations**: `noun-verb` pattern for lattribs manipulation
- **Consistent conversions**: `source-to-string` pattern for formatting
- **Architecture discovery**: Identified and will eliminate legacy 2-element system
- **Bug discovery**: Found xdata preservation issue in Copy Note feature

**Next Steps:**
1. TGH review and approval of this document
2. Generate PowerShell rename script
3. Execute renaming with careful testing
4. Migrate 4 legacy call sites
5. Attempt #region grouping to identify any remaining issues

**Philosophy:**
> "We are moving beyond style toward meaning."  
> — TGH, 2025-10-28

This renaming transforms implicit knowledge into explicit architecture, making the codebase more maintainable and revealing opportunities for improvement.

---

*Document Status: FINAL - All Questions Resolved*  
*Approved By: TGH (2025-10-28)*  
*Ready for Implementation*