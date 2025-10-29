# Naming Proposal: Moving Beyond Style to Meaning

**Date:** October 28, 2025  
**Status:** Draft for Review  
**Goal:** Replace ambiguous terms with precise, architecture-revealing names

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

**[AI INVESTIGATION COMPLETE]** After deep reading of the code, here's what I discovered:

#### The Two-Level Architecture (Critical Understanding!)

**HIGH-LEVEL (Prefix/Auto/Postfix - 4-element):**
- `hcnm-read-bubble-data` → `hcnm-dwg-to-lattribs` 
  - Reads dwg + xdata → 4-element lattribs: `(("TAG" prefix auto postfix) ...)`
  - SPLITS the concatenated dwg text using xdata to extract prefix/auto/postfix
  - Used for: EDITING (user needs to see/edit prefix vs auto separately)

- `hcnm-save-bubble` → `hcnm-lattribs-to-dwg`
  - Writes 4-element lattribs → dwg (concatenates + applies underover)
  - Also saves xdata separately so split can happen later
  - Used for: SAVING after editing

**LOW-LEVEL (Simple Tag-Value - 2-element):**
- `hcnm-get-attributes` → **ELIMINATE** or `hcnm-dwg-to-simple-alist`
  - Reads dwg → 2-element: `(("TAG" "concatenated-value") ...)`
  - NO split, NO xdata parsing
  - Used for: LEGACY code (quantity take-off, reports)
  - **FINDING: Only 3 call sites! Should we eliminate?**

- `hcnm-set-attributes` → **ELIMINATE** or `hcnm-simple-alist-to-dwg`
  - Writes 2-element simple alist → dwg
  - NO concatenation, NO underover, NO xdata
  - Used for: One place (update-bubble-tag) where we have already-formatted text
  - **FINDING: Only 1 call site! Should we eliminate?**

#### Recommendation: ELIMINATE the "raw" functions

**Reasons:**
1. **Minimal usage**: hcnm-get-attributes has 3 calls, hcnm-set-attributes has 1 call
2. **Legacy pattern**: These were created before prefix/auto/postfix architecture
3. **Confusing**: Having two ways to read/write causes exactly the confusion you experienced
4. **Easy migration**: 
   - For reads: Use `hcnm-dwg-to-lattribs`, then extract just the concatenated value if needed
   - For writes: Use `hcnm-lattribs-to-dwg` with empty prefix/postfix if you only have simple text

**Proposed Category 1 Table (SIMPLIFIED):**

| Old Name | New Name | Rationale |
|----------|----------|-----------|
| `hcnm-save-bubble` | `hcnm-lattribs-to-dwg` | Explicit: 4-element lattribs → dwg (with underover + xdata) |
| `hcnm-read-bubble-data` | `hcnm-dwg-to-lattribs` | Explicit: dwg + xdata → 4-element lattribs |
| `hcnm-save-bubble-xdata` | `hcnm-xdata-save` | Helper: Save xdata portion only |
| `hcnm-get-attributes` | **ELIMINATE** | Migrate 3 call sites to use dwg-to-lattribs |
| `hcnm-set-attributes` | **ELIMINATE** | Migrate 1 call site to use lattribs-to-dwg |

**Your concern was 100% valid** - we DON'T want two different ways. The "raw" functions are legacy cruft.

### Category 2: Lattribs Operations (Building Block)

**[AI RESPONSE TO TGH CONCERNS]**

Your concerns are spot-on. Let me clarify what these functions actually DO:

#### Understanding the Operations

**hcnm-ldrblk-save-attribute-to-list** - What does it really do?
- **INPUT**: tag, value (string OR 3-element list), lattribs
- **OPERATION**: Updates ONE attribute in the lattribs structure
- **SOURCE**: Value comes from USER INPUT or AUTO-TEXT calculation
- **OUTPUT**: Modified lattribs with that one attribute updated

**Better name analysis:**
- "save-attribute-to-list" is terrible - "save" from where?
- This is really: **"user-to-lattribs"** OR **"value-to-lattribs"** OR just **"lattribs-put"**

**hcnm-ldrblk-initialize-attribute-list** - What does it do?
- **INPUT**: Nothing (or bubble definition)
- **OPERATION**: Creates empty lattribs structure with all expected tags
- **OUTPUT**: Fresh lattribs: `(("NOTENUM" "" "" "") ("NOTETXT1" "" "" "") ...)`

**Better name analysis:**
- You're right - "init" is too short if we're being rigorous
- This is: **"lattribs-create-empty"** OR **"lattribs-initialize"**

#### Revised Category 2 (With Source-to-Destination Thinking)

| Old Name | New Name | Rationale |
|----------|----------|-----------|
| `hcnm-ldrblk-initialize-attribute-list` | `hcnm-lattribs-initialize` | Creates empty lattribs structure |
| `hcnm-ldrblk-save-attribute-to-list` | `hcnm-lattribs-put` | Puts value into lattribs (like assoc/subst) |
| `hcnm-ldrblk-save-auto-to-list` | `hcnm-lattribs-put-auto` | Puts auto-text specifically |
| `hcnm-ldrblk-ensure-fields` | `hcnm-lattribs-ensure-structure` | Validates/repairs lattribs structure |
| `hcnm-ldrblk-ensure-fields-and-adjust-formats` | `hcnm-lattribs-ensure-and-underover` | Validates + applies underover |

**Alternative consideration:** Should these be source-to-destination?
- `hcnm-value-to-lattribs` - Makes the data flow explicit
- `hcnm-auto-to-lattribs` - Shows auto-text is the source
- But: These aren't format conversions, they're operations ON lattribs

**Your vote:** You prefer full names like `initialize-lattribs`. Should we use:
- `hcnm-lattribs-initialize` (noun-verb, object-oriented style)
- `hcnm-initialize-lattribs` (verb-noun, procedural style)

**My recommendation:** `hcnm-lattribs-put` is clearer than `lattribs-set` because "put" suggests "put this value HERE in the structure" whereas "set" is ambiguous about what you're setting.

### Category 3: Format → String (Semantic Fix)

**[AI RESPONSE TO TGH]** You're absolutely right! Using "to-string" makes the data flow explicit.

These functions convert **numbers → formatted strings**:
- Input: Number (station value, diameter in feet, etc.)
- Operation: Format with precision, add prefix/postfix from config
- Output: Formatted string

**Examples:**
- `123.45` → `"STA 1+23.45"` (number to string with formatting)
- `2.0` (feet) → `"Ø24\""` (diameter to string with conversion + units)

| Old Name | New Name | Rationale |
|----------|----------|-----------|
| `hcnm-ldrblk-adjust-formats` | `hcnm-lattribs-underover-gap` | Only handles GAP now (applies underover codes) |
| `hcnm-ldrblk-adjust-format` | `hcnm-string-underover` | Generic: apply underover to any string |
| `hcnm-ldrblk-auto-alignment-format-station` | `hcnm-ldrblk-auto-al-station-to-string` | number → string (explicit!) |
| `hcnm-ldrblk-auto-alignment-format-offset` | `hcnm-ldrblk-auto-al-offset-to-string` | number → string |
| `hcnm-ldrblk-auto-pipe-format-diameter` | `hcnm-ldrblk-auto-pipe-dia-to-string` | number → string |
| `hcnm-ldrblk-auto-pipe-format-slope` | `hcnm-ldrblk-auto-pipe-slope-to-string` | number → string |
| `hcnm-ldrblk-auto-pipe-format-length` | `hcnm-ldrblk-auto-pipe-length-to-string` | number → string |

**Why "to-string" is better:**
- Explicit data flow: number → string
- Distinguishes from "underover" (which operates on existing strings)
- Standard pattern: `real-to-string`, `int-to-string`, `list-to-string`
- Matches Lisp conventions

**Note:** Also abbreviated "alignment" to "al" and "diameter" to "dia" for brevity in these frequently-called functions.

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
[lattribs] ← hcnm-lattribs-init
    ↓
[lattribs] ← hcnm-lattribs-set (user text)
    ↓
[lattribs] ← hcnm-lattribs-set-auto (auto-text)
    ↓
[lattribs] ← hcnm-lattribs-ensure-fields
    ↓
    ├─→ hcnm-lattribs-to-xdata → [xdata] (auto-text only)
    │
    ├─→ hcnm-lattribs-underover-gap → [lattribs] (add GAP)
    │
    └─→ hcnm-lattribs-to-dwg → [dwg] (with underover codes)

EDITING:
[dwg] + [xdata]
    ↓
hcnm-dwg-to-lattribs (splits using xdata)
    ↓
[lattribs]
    ↓
(user edits via dialog)
    ↓
hcnm-lattribs-to-dwg
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

Before proceeding with renaming, we need to validate:

### 1. Clarity Check
**[TGH: Good question. I think so.]**
**[AI: Agreed - the pattern reads naturally once you understand the building blocks.]**
- Can someone reading `hcnm-lattribs-to-dwg` understand it means "write lattribs to drawing"?
- Is the data flow clear: `dwg → lattribs → underover → dwg`?

### 2. Consistency Check
- Do all data-flow functions follow `source-to-destination` pattern?
- Are there any exceptions that need special handling?
**[AI: YES - Category 2 functions are operations ON lattribs, not transformations BETWEEN representations. That's why they use verb-noun or noun-verb patterns instead.]**

### 3. Completeness Check
- Have we identified all functions that need renaming?
- Are there local variables that also need consistent naming?

### 4. Conflict Check
**[TGH: Yes. See my comments above.]**
**[AI: RESOLVED - Eliminating the "raw" functions removes the conflicts. The 4 call sites can migrate to the main lattribs functions.]**
- Any name collisions with existing functions?

**[TGH: No. I think we are safe for that. If we want to be superstitiously safe, we can temporarily add silliness like xyzzy to the names during the rename operation.]**
**[AI: I like the xyzzy idea! We could do: old → old-xyzzy-temp → new. But since we're eliminating duplicates rather than creating them, probably not needed.]**
- Any ambiguities between old and new terms during transition?

### 5. Naming Pattern Consistency
**[NEW QUESTION BASED ON DEEP READING]**
- Category 1 (data flow): `source-to-destination` ✓
- Category 2 (operations): `noun-verb` or `verb-noun`? 
  - Current proposal: `lattribs-put` (noun-verb, object-oriented)
  - Alternative: `put-lattribs` (verb-noun, procedural)
  - **TGH preference?**
- Category 3 (conversions): `real-to-string` pattern ✓

## Implementation Strategy

0. **Git commit** with clear message  [TGH 2025-10-28 19:09:18: I added this.]
1. **Update documentation** (this file + standards docs)
2. **Review and validate** naming strategy
3. **Generate PowerShell script** (longest-first, sorted)
4. **Test on copy** of cnm.lsp first
5. **Run on actual** cnm.lsp
6. **Git commit** with clear message
7. **Test in AutoCAD** to confirm no breaks

## Questions for Review

1. **lattribs vs attribute-list**: Is "ela-tribs" pronounceable enough? Or too cryptic?
   **[TGH: I don't think we need to give a pronunciation guide. But I grant that I do intuitively account for mental "pronunciation" in my preferences. "elatrib" is kind of cute.]**
   **[AI: Agreed - removed pronunciation guides from glossary, kept them only in standards doc for team discussions.]**

2. **dwg**: Does this clearly mean "drawing representation"? Any confusion with file format (.dwg)?
   **[TGH: 100% sold. No reservations.]**
   **[AI: Perfect!]**

3. **underover**: Is this term clear? Better than "format"? Other options: "noteline", "linecode"?
   **[TGH: Not perfect. But crystal clear and better (more clear and precise) than all contenders so far.]**
   **[AI: Agreed - shipping it!]**

4. **"format" functions**: Should alignment/pipe formatting functions use "string" instead?
   **[TGH: Good catch. Yes! Better yet, auto-al-station-to-string]**
   **[AI: IMPLEMENTED - Changed to `-to-string` pattern throughout Category 3.]**
   - ~~`auto-al-station-string`~~ vs ~~`auto-al-format-station`~~
   - ✓ `auto-al-station-to-string` (explicit data flow!)

5. **Scope**: Should ALL `attribute-list` parameters become `lattribs`, or only public function parameters?
   **[TGH: I need examples.]**
   **[AI: Here are examples...]**
   
   **Public functions (definitely rename):**
   ```lisp
   (defun hcnm-lattribs-to-dwg (ename-bubble lattribs) ...)  ; Clear API
   ```
   
   **Helper functions (rename?):**
   ```lisp
   (defun hcnm-internal-helper (lattribs tag) ...)  ; Or keep "attribute-list" for clarity?
   ```
   
   **Local variables (rename?):**
   ```lisp
   (defun foo (/ lattribs en)  ; Short and clear in small scope
     (setq lattribs (hcnm-dwg-to-lattribs en))
     ...
   )
   ```
   
   **My recommendation:** Rename ALL parameters and locals for consistency. "lattribs" is now the canonical term.

6. **Missing anything**: Any other functions or terms that need renaming?
   **[TGH: Surely yes. But this is a start. After we do this, I will attempt again to group the functions into #region areas. If I fail, then we need to do even better.]**
   **[AI: Excellent plan! The #region grouping will reveal any remaining naming issues. This is iterative enlightenment!]**

7. **NEW: Eliminate "raw" functions?**
   **[AI PROPOSAL based on deep reading]**
   - `hcnm-get-attributes` has only 3 call sites (legacy quantity take-off code)
   - `hcnm-set-attributes` has only 1 call site (update-bubble-tag)
   - These are pre-prefix/auto/postfix architecture
   - Having two ways to read/write causes confusion (your exact concern!)
   - **Recommendation**: ELIMINATE both, migrate 4 call sites to use main functions
   - **Your approval needed**: Should we kill these legacy functions?

8. **NEW: Noun-verb or verb-noun for operations?**
   **[AI QUESTION]**
   - `hcnm-lattribs-put` (noun-verb, object-oriented feel)
   - `hcnm-put-lattribs` (verb-noun, procedural feel)
   - **Your preference?** The standards doc currently favors verb-noun, but noun-verb works for "notorious building blocks"

## Next Steps

- [ ] Review this proposal
- [ ] Update standards_05_architecture.md with glossary
- [ ] Update standards_03_names.md with data flow pattern
- [ ] Add comment blocks to cnm.lsp regions
- [ ] Generate rename script
- [ ] Test and deploy
