# GitHub Copilot Instructions for CNM Project

## 1. Quick Start Guide

### 1.1. Essential Instructions

#### 1.1.1. Human Command Keywords
- **ci** = This file (.github\copilot-instructions.md)
- **cinote:** = Immediately revise ci to include that information
- **cinote.** = You failed to execute previous cinote command
- **citruth.** = Tell the truth! Qualify statements with certainty estimates. Don't say "Perfect/Fixed/Done" when you mean "Please test"
- **cicleanup** = Audit repository for obsolete documentation and comments, mark candidates with `[deleteme]` or `[moveme]` tags for human review (see Section 1.6.1). When invoked, respond: "The cleanup is ready for your review. Search for `deleteme` and `moveme` in the repository awaiting your approval."

#### 1.1.2. AutoLISP-Specific Rules
- **MANDATORY FUNCTION VERIFICATION**: NEVER use any function without first searching ORIGINAL codebase with grep_search AND verifying with git history that function existed before recent AI changes. If function not found in original CNM code, check official Autodesk documentation with fetch_webpage. See Section 1.5 for complete protocol.
- **Use get_errors tool**: Check syntax after AutoLISP edits. Fix errors before reporting
- **AutoLISP Common Pitfalls**: See Section 1.4 for AutoLISP/Common Lisp differences and correct AutoLISP idioms.

#### 1.1.3. Workflow Constraints
- **DO NOT git commit**: Let the human commit. AI prepares files only.

#### 1.1.4. Architecture Checklist (MANDATORY before coding)

**ALWAYS ask these questions BEFORE writing code:**

1. **Who calls this?** (user command, script, callback, internal function)
   - User command → use `c:` prefix
   - Script-only → NO `c:` prefix (not a user command)
   - Callback → NO `c:` prefix (event-driven)
   - Internal → NO `c:` prefix (helper function)

2. **Where does it belong?** (layering check)
   - Core library (edclib) → MUST NOT depend on apps (CNM)
   - App (CNM) → CAN depend on core, CANNOT depend on test suite
   - Test suite → ISOLATED, minimal dependencies
   - Prefix must match location: `haws-` for edclib, `hcnm-` for CNM

3. **What's the scope?** (identity/lifetime)
   - Session-only → Scope 0
   - Project-wide → Scope 2
   - User preference → Scope 4
   - App must match usage: CNM settings in "CNM", HawsEDC-wide in "HAWS"

4. **Does this already exist?** (DRY check)
   - Search with `grep_search` for similar functionality
   - Reuse existing utilities (haws-debug, haws-clock-console-log, etc.)
   - Don't duplicate file I/O patterns

5. **Is this the simplest solution?**
   - Shortest path to working code
   - Reuse existing patterns
   - No premature abstraction

**VIOLATION CONSEQUENCES:** Architectural errors create technical debt, confuse future maintainers, and waste human cleanup time.

**When uncertain:** ASK the human these questions explicitly before proceeding.

### 1.2. Project Context

**What is CNM?** Civil engineering tool for managing construction notes on AutoCAD drawings.

**AI's role:** Help maintain 20-year-old codebase with strict data integrity requirements.

**Recent Milestones:**
- **2025-11-16**: obj-target refactoring complete - all auto-text functions now use obj-reference + reactor-context-p pattern
- **2025-11-16**: Multiple auto-texts per line working (Slope + Off + Sta on same tag)
- **2025-11-16**: Handle extraction from auto-metadata complete - edit dialog correctly stores alignment/pipe handles

### 1.3. Key Principles
- Don't break code or cause regressions
- Make it gradually more maintainable and readable
- Fail loudly (strict validation); never mask errors
- Respect user edits (users bypass our dialogs)

### 1.4. AutoLISP
AutoLISP is the primary programming language of CNM. AutoLISP and Common LISP are unrelated dialects of LISP with an ancient common ancestry. Do not assume that AutoLISP is similar to Common LISP.

#### 1.4.1. AutoLISP vs Common Lisp Differences

**AutoLISP ≠ Common Lisp:** AutoLISP lacks many Common Lisp functions. Use these AutoLISP idioms:

- **Function exists:** `(boundp 'function-name)` ✅ | `fboundp` ❌ (doesn't exist)
- **Load paths:** AutoLISP `(load)` function doesn't accept relative paths (e.g., `"../file.lsp"`). It searches AutoCAD's support paths automatically.
- **Symbolic constant idiom:** AutoLISP does not provide any benefits for user symbolic constants `'my-constant` ❌
- **Entity deletion check:** `(handent handle)` returns nil for user-erased entities ✅ (entdel is esoteric/temporary, ignore it)

#### 1.4.2. CRITICAL: Function Name Verification Protocol

**MANDATORY BEFORE USING ANY FUNCTION:**

1. **Search ORIGINAL codebase FIRST:** Use `grep_search` to find the function in devsource/*.lsp files
2. **Verify with git history:** Use `git log -S "function-name"` to confirm function existed BEFORE recent AI changes, not AI additions
3. **If not in original code:** Check official Autodesk AutoLISP documentation or established AutoLISP community resources using `fetch_webpage` tool
4. **If function not documented:** EXPLICITLY STATE "Function not found in original CNM codebase or Autodesk documentation" and do not use it
5. **NEVER assume function exists:** Just because it "should" exist doesn't mean it does
6. **When errors occur:** First suspect your own function names, not the environment

**Protocol violation consequences:** Function hallucination wastes human time and undermines trust. This is completely preventable by following the verification protocol above.

**Verified Visual LISP functions (found in CNM codebase):**
- `vl-consp`
- `vlax-ename->vla-object` - Used throughout CNM (line 438, 5472, 6007, etc.)
- `vlax-get-property` - Used in CNM (line 2608, 6674, etc.)
- `vlax-put-property` - Used in CNM (line 6703)
- `vlax-invoke` - Used in CNM (line 6679)
- `vlax-get-acad-object` - Used in CNM (line 2595)
- `vlax-release-object` - Used in CNM (line 2603)
- `vla-get-handle` - Used in CNM (line 6158)
- `vlr-owner-add` - Used in CNM (line 10777)
- `vlr-owners` - Used in CNM reactor code
- `vlr-data` - Used in CNM reactor code

#### 1.4.3. AI Hallucination Blacklist

**BANNED FUNCTIONS (do not exist in AutoLISP):**
- `vlax-object-p` - AI hallucination, does not exist
- `fboundp` - Common Lisp only, not AutoLISP
- `consp` - Common Lisp only, not AutoLISP. vl-consp is a function.

**ADD TO BLACKLIST:** When human corrects AI hallucination, immediately add function to this blacklist to prevent repeat arguments.

**Protocol violation consequences:** Function hallucination wastes human time and undermines trust. This is completely preventable by following the verification protocol above.

### 1.5. AutoCAD-Specific Pitfalls

**AutoCAD block behavior patterns:**

- **Anonymous blocks:** Dynamic blocks become anonymous when modified. Use EffectiveName property or XDATA for identification

**Auto-text type classification (two orthogonal properties - CRITICAL):**
- **Handle-based vs Handleless (affects XDATA storage):**
  - Handle-based: ALL alignment types (Sta/Off/StaOff/AlName/StaName) + Dia/Slope/L - stores reference object handle in XDATA (never empty)
  - Handleless: N/E/NE ONLY - empty handle `""` in XDATA, coordinate-only, no reference object
- **Coordinate-based vs Non-coordinate (affects paper space transform):**
  - Coordinate-based: Sta/Off/StaOff/StaName/N/E/NE - needs p1-world + VPTRANS for paper space
  - Non-coordinate: AlName/Dia/Slope/L - no transform needed, just reads object properties
- **Common misconception to avoid:**
  - ❌ WRONG: "Sta/Off have empty handles" (they store alignment handle!)
  - ✅ CORRECT: "N/E/NE have empty handles" (handleless coordinates)
- **Complete architecture:** See S05.4.2 (Reactive Auto-Text System) for VLR-OBJECT-REACTOR patterns, BlockReactors flag lifecycle, and debugging guidelines

### 1.6. AI Collaboration Workflow

See Section 5.2 for planning document workflow and [standards-02-ai-human-collaboration.md](../devtools/docs/standards/02-ai-human-collaboration.md) for general AI collaboration patterns.

#### 1.6.1. Cleanup Markers (deleteme / moveme)

AI agents are **encouraged** to identify obsolete comments, resolved issues, and long-disconnected documentation. When you spot content that should be removed or relocated, mark it with a lightweight tag so a human can review and approve:

- `[deleteme]...[/deleteme]` — Propose deletion (content is obsolete or resolved)
- `[moveme destination="ci 1.4" or "S05.2.3"]...[/moveme]` — Propose relocation to a canonical location

Keep the marker on a single line when possible:
```
[deleteme] This workaround note is obsolete after the 2025-11 refactor. [/deleteme]
```

For multi-line passages, wrap the entire block:
```
[moveme destination="S05.4.1"]
<long-form content that belongs in architecture docs>
[/moveme]
```

Human reviewers: inspect each marker, then either (A) apply the change and remove the marker, or (B) remove the marker if the content should stay. See S02.4.2.4 for full policy.

### 1.7. Read These Sections First

Essential sections for understanding CNM:
- **Section 2.2**: Core workflow (what engineers do with CNM)
- **S05.4.1**: Data models (lattribs structure, XDATA patterns, bubble-data)
- **Section 5.1**: Communication patterns (what human terms mean)
- **Section 5.2**: Planning document workflow
- **Section 5.3**: Document structure guidelines (how to write docs for AI)

### 1.8. For Future Debugging Efforts

When encountering complex issues in CNM:

1. **Start with S05** (devtools/docs/standards/05-architecture.md) to understand system architecture
   - **S05.3**: Config system (hcnm-config-getvar now uses haws-config)
   - **S05.4.1**: Current bubble architecture (lattribs, XDATA, bubble-data)
   - **S05.4.2**: Reactive auto-text system (VLR-OBJECT-REACTOR patterns)
2. **Check S04** (devtools/docs/standards/04-organization.md) to locate relevant files  
3. **Use S03** (devtools/docs/standards/03-style.md) for naming/style conventions
4. **Apply S02** (devtools/docs/standards/02-ai-human-collaboration.md) for AI collaboration patterns

**S05 provides the "30,000 foot view"** that prevents getting lost in implementation details while debugging complex multi-system interactions like reactor issues.

---

## 2. CNM Project Overview

### 2.1. What is CNM?

**CNM (Construction Notes Manager)** is an AutoCAD add-on written in AutoLISP that helps civil engineers manage construction notes on drawings using "bubble notes" (leader annotations with attribute text).

**20+ year history:** Mature codebase with strict backward compatibility requirements. Users have thousands of drawings relying on CNM's data formats.

### 2.2. Core Workflow

Engineers use CNM in this sequence:

#### 2.2.1. Edit Project Notes
- **Project Notes** = Template of standard construction notes
- Each note has: type (shape), number (alphanumeric), description, optional quantities, cost instructions
- Descriptions may reference "the plan (view)" for specifics (e.g., "Trench width per plan")
- Template auto-copied from CNM application folder to seed new projects

#### 2.2.2. Insert Bubble Notes
- **Bubble Notes** = Leader annotations with numbers keyed to Project Notes
- Each bubble has: shape, number, 7 text lines (including hidden line 0)
- If Project Notes specifies quantity line (e.g., LINE4), that bubble line accepts quantities (7 LF, 280 SY, 8 EA)
- CNM ignores units, reads only numeric value, uses units from Project Notes
- Built from attributed blocks (easily customizable)

**Customer Problem Solved:**
Civil engineers annotate drawings with dynamic data (station/offset along alignments, pipe diameters/slopes, coordinates, elevations). Manual entry is error-prone. Alignment changes break notes. Updating 50 notes individually is tedious. Copy-paste mistakes show wrong values.

**CNM Solution:**
- Engineer clicks toolbar, places leader, selects alignment/pipe/surface
- CNM queries Civil 3D automatically, calculates station/offset
- Stores reference in XDATA, attaches VLR reactor
- When alignment shifts, CNM updates all bubble notes automatically
- No user intervention needed

**Customer Impact (20+ years of feedback):**
- 80% reduction in note updating time
- Eliminates manual transcription errors
- Encourages design iteration (changes are cheap)
- "CNM paid for itself on the first project"

**Mix Auto + Manual Text:**
Engineers add context: "Storm Drain STA 10+25.50 RT" where "Storm Drain" and "RT" are user prefix/postfix, "STA 10+25.50" is auto-text. CNM stores auto separately in XDATA so updates don't corrupt user edits.

#### 2.2.3. Make Key Notes Table
- CNM reads Project Notes, generates Key Notes Table for current sheet
- Table includes every note with bubbles in search scope (visible only, possibly current tab only)
- Calculates quantity totals per note
- Saves findings to .not file alongside drawing (for later project-wide takeoff)
- Table built from noteqty.dwg block insertions (easily customizable)

#### 2.2.4. Make Quantity Takeoff Table
- CNM QT generates project-wide totals table from saved .not files
- Tabulates every sheet listed in csv matching current drawing's base name
- Built from text following current style/dimension settings (easily customizable)

### 2.3. Project Management

#### 2.3.1. cnm.ini = CNM Project
- A CNM Project is a cnm.ini file in a folder
- All .dwg files in that folder belong to that project
- Other folders can link via tiny cnmproj.TXT file pointing to main project folder

#### 2.3.2. Settings Storage
- Project settings stored in cnm.ini using standard INI format
- Template cnm.ini in application folder auto-copied to seed defaults
- CNM Options dialog provides GUI for editing settings

### 2.4. Loading Architecture

CNM loads automatically when AutoCAD starts:

1. **cnm.cuix** (AutoCAD menu file) - Entry point, loads with AutoCAD
2. **cnm.mnl** (menu LISP file) - Checks for `haws-setlayr`, loads CNMloader if not present
3. **CNMloader.lsp** (initialization) - Loads immediately:
   - Defines `haws-autoload` function (creates command stubs)
   - Loads core libraries immediately: `edclib.lsp`, `haws-tip.lsp`, `cnmalias.lsp`
   - Defines autoloader stub for `hcnm-ldrblk-reactor-callback` (persistent reactor support) [TGH 2025-11-12 22:55:44: Update this to new name.]
   - Sets up command autoloaders for `cnm.lsp` functions (lazy loading)
4. **cnm.lsp** - Main CNM functionality, lazy-loaded when user runs first CNM command

**Key points:**
- Core libraries load immediately via CNMloader.lsp
- CNM commands use `haws-autoload` pattern: stub loads file on first use
- Reactor callback has special autoloader stub (reactors fire before commands run)
- Do NOT load cnm.lsp eagerly - defeats lazy loading pattern

---

## 3. CNM Components Reference

### 3.1. Project Notes

*[Section stub - to be documented]*

Project Notes management, template system, standard notes format.

### 3.2. Bubble Notes

#### 3.2.1. Bubble Note Basics

##### 3.2.1.1. Minimum Definition
Bubble notes = Attributed blocks with dynamic shape selector (change shape without recreating insertion).

**Requirements for custom bubble blocks:**
- Dynamic block "Shape" property matching CNM Project settings types
- CNM includes 8 shapes: BOX, CIR, DIA, ELL, HEX, OCT, PEN, REC, SST
- Required attributes: NOTENUM, NOTEPHASE, NOTEGAP, NOTETXT0 through NOTETXT6
- Any block name you like

**NOTEDATA attribute:** Legacy development experiment, never used in production. XDATA serves auto-text purpose. Attribute still exists in block definitions for backward compatibility but always empty.

##### 3.2.1.2. Included Bubble Blocks
CNM ships with bubble notes using block name pattern: `cnm-bubble-[m?]#-[dir]`
- `[m?]` = "m-" for multi-text bubbles
- `#` = 0 (no landing/hook) or 1 (with landing/hook)
- `[dir]` = "l" (left) or "r" (right) leader direction
- Associated with AutoCAD Leaders

#### 3.2.2. Insertion and Editing Tools

##### 3.2.2.1. Current Functionality
**Insert Bubble Note:**
- Places bubble with requested shape and leader
- Prompts for number, line 1, line 2
- Auto-text options:
  - Length/Area from selected polylines
  - Drawing coordinates (Northing/Easting)
  - Civil 3D Alignments (station/offset)
  - Civil 3D Pipe Networks (diameter, slope, length)
  - Civil 3D Surfaces (elevations)

**Edit Bubble Note:**
- Opens dialog to edit bubble attributes
- Add automatic text via buttons

**Replace Bubble:**
- Command: `c:hcnm-replace-bubble` (also called via notetype=nil path in hcnm-ldrblk-insert)
- Copies existing bubble to new location using same block definition
- Preserves all attribute text from source bubble
- **Legacy mode:** Works with bubbles created before reactive auto-text system
- **Current limitation:** Does not update reactor data structures or XDATA/XRECORD for auto-text bubbles
- **Planned upgrade:** See `.ai-plans/copy-replace-bubble-upgrade.md` for reactor/XDATA handling

##### 3.2.2.2. Reactive Auto-Text System
**Status:** Implemented on feat-sunrise branch, debugging regressions

**What it does:** Auto-text stays synchronized with source objects and follows leader arrowheads via VLR reactors (AutoLISP object event listeners).

**Architecture:** Free-form 2-element lattribs with handle-based XDATA supporting multiple auto-texts per line.

**Current work:** Addressing regressions one-by-one for production readiness.

#### 3.2.3. Data Models

##### 3.2.3.1. Overview

**See S05.4 (CNM Bubble Notes Architecture)** for complete data structure specifications including:
- lattribs structure and validation rules
- XDATA/XRECORD storage patterns  
- bubble-data alist format
- Data flow transformations

**Key Quick Reference:**
- **lattribs:** Internal 2-element format `'(("TAG" "text") ...)`
- **XDATA:** Auto-text storage with composite keys
- **XRECORD:** Viewport transform data (VPTRANS)
- **bubble-data:** Alist for passing state during insertion/editing

**See S05.4.2 (XDATA Storage Patterns)** for complete XDATA specifications.

##### 3.2.3.2. XRECORD Format - Viewport Transform Storage

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

##### 3.2.3.3. Coordinate Transformation

**AutoCAD Coordinate Systems (Critical Understanding):**

AutoCAD uses multiple coordinate systems with specific canonical relationships:

1. **WCS (World Coordinate System)** - The canon for model space
   - All model space geometry ultimately references WCS
   - Never changes (absolute reference frame)

2. **DCS (Display Coordinate System)** - The canon for each layout
   - Paper space has its own DCS per layout
   - Independent of model space WCS

3. **UCS (User Coordinate System)** - Local working plane
   - Can be rotated/translated in model space OR paper space
   - `(getpoint)` returns coordinates in current UCS
   - Temporary convenience for user - NOT stored with entities

4. **OCS (Object Coordinate System)** - Entity storage coordinate system
   - **CRITICAL:** OCS follows the canon (WCS for model space, DCS for paper space)
   - **OCS ignores UCS!** Even with UCS active, entity DXF code 10 stores OCS coordinates
   - For model space entities: OCS = WCS (regardless of current UCS)
   - For paper space entities: OCS = DCS (regardless of current UCS)

**Key Insight:** When inserting in model space, `(getpoint)` returns UCS coordinates, but the leader's DXF code 10 stores WCS coordinates (OCS). Therefore `p1-ucs` ≠ `p1-ocs` when UCS is rotated/translated, but `p1-ocs` = `p1-world` always in model space.

**Problem for CNM:** Auto-text needs WCS coordinates to query Civil 3D objects (alignments/pipes/surfaces). Bubbles may be in paper space DCS or model space with non-standard UCS active.

**Solution:** For paper space bubbles referencing model space objects for coordinate-based auto-text, store 3-point correspondence in XRECORD:
- 3 reference points in OCS (viewport DCS space)
- 3 reference points in WCS (model space)
- Apply affine transformation on reactor updates to get WCS from current leader position

##### 3.2.3.4. Viewport Association

**Problem:** Coordinate transformation requires associating viewport with bubble note. Users expect paper space bubbles to "look through" specific viewports to see model space objects.

**Solution:** For paper space bubbles referencing model space objects for coordinate-based auto-text, store viewport number in VPTRANS data (first element of viewport-data list). On updates, correctly transform auto-text coordinates from paper space to model space.

#### 3.2.4. Reactor System

**Quick Reference:** CNM uses VLR-OBJECT-REACTOR for automatic bubble updates.

**Essential Concepts:**
- **BlockReactors flag:** Prevents infinite recursion (save/restore pattern CRITICAL)
- **5-level hierarchy:** "HCNM-BUBBLE" → owner → bubble → tag → auto-type → reference
- **Three-tier cleanup:** Immediate detection, batch cleanup, deep scrub
- **Orphaned owners:** VLA-OBJECTs in VLR but not in data (causes warnings after edit)

**Terminology (CRITICAL):**
- **OWNER** = Any object in VLR system (reference OR leader)
- **NOTIFIER** = Specific owner that triggered callback
- **REFERENCE** = Object providing calculation data (never leader)

**For Complete Documentation:** See S05.4.2 (Reactive Auto-Text System)
- Section 4.2.3: 5-level data structure hierarchy
- Section 4.2.4: BlockReactors flag lifecycle and save/restore pattern
- Section 4.2.7: Orphaned owner cleanup (5-step process)
- Section 4.2.8: Debugging reactor attachment issues
- Section 4.2.10: Reactor cleanup flow (all tiers)
- Section 7.1.6: Reactor system terms (glossary)

**Common Pitfalls:**
- ❌ Don't mutate data during callback iteration (use vl-remove-if)
- ❌ Don't skip BlockReactors save/restore pattern
- ❌ Don't panic over "Notifier not found" immediately after edit (expected VLR lag)
- ✅ Do use functional updates for concurrent safety
- ✅ Do follow 5-step orphaned owner cleanup when removing auto-text
- ✅ Do check S05.4.8 debugging decision tree before investigating warnings

#### 3.2.5. Reactive Auto-Text Implementation

**Status:** Implemented on feat-sunrise branch (November 2025).

**What it does:** Auto-text stays synchronized with source objects and follows leader arrowheads via VLR reactors.

**Architecture:** Free-form 2-element lattribs with handle-based XDATA supporting multiple auto-texts per line.

**For Complete Documentation:** See S05.4 (CNM Bubble Notes Architecture)
- Section 4.1.5: lattribs structure (2-element format, validation)
- Section 4.1.6: XDATA storage patterns (composite keys, auto-text format)
- Section 4.2: Reactive Auto-Text System (complete architecture)

**Key Technical Challenges:**
1. **Paper Space Complexity** - VPTRANS XRECORD for coordinate transformation (see S05.4.1.6)
2. **Free-Form User Edits** - XDATA search/replace preserves user text (see S05.4.2.5)
3. **Legacy Migration** - 20+ years of drawings, minimal breaking changes (see S05.4.1)

##### 3.2.5.1. Quick Reference

**Free-Form User Edits:**
- Users edit bubbles directly in AutoCAD (bypass dialogs)
- XDATA stores verbatim auto-text as search needles
- Search/replace preserves user prefix/postfix text
- See S05.4.2.5 for complete algorithm

**Auto-Text Insertion Delimiter:**
- Triple backtick ` ``` ` marks insertion point
- No automatic spacing (user controlled)
- Append if no delimiter (graceful UX)
- See S05.4.2.5 for complete philosophy

**MVC Architecture:**
- Dialog = IN-MEMORY until Save
- Save = ATOMIC write (lattribs + XDATA + reactors)
- Cancel = SAFE (no writes)
- See S05.4.2.6 for complete flow diagrams

##### 3.2.5.2. Code Examples

**Free-form edit scenario:**

1. User places bubble with auto-text: `"STA 10+25.50"`
2. XDATA stores: `'(("NOTETXT1" . "STA 10+25.50"))`
3. User manually edits in AutoCAD: `"Storm Drain STA 10+25.50 RT"`
4. XDATA unchanged: `'(("NOTETXT1" . "STA 10+25.50"))`
5. Alignment shifts, reactor fires:
   - Search for `"STA 10+25.50"` in `"Storm Drain STA 10+25.50 RT"`
   - Split: `("Storm Drain " "STA 10+25.50" " RT")`
   - Update: `("Storm Drain " "STA 11+00.00" " RT")`
   - Concatenate: `"Storm Drain STA 11+00.00 RT"`

**Result:** User's prefix/postfix preserved, auto-text updated ✅

**MVC call flow - Insertion:**
```autolisp
;; 1. VIEW: Auto-text button clicked
(hcnm-bn-insert-auto-button tag)
  ;; 2. CONTROLLER: Route to handler
  (hcnm-bn-controller-add-auto tag auto-type)
    ;; 3. MODEL: Generate, store XDATA, attach reactor
    (hcnm-bn-model-set-auto tag text auto-type)
```

**MVC call flow - Editing:**
```autolisp
;; Dialog operates on IN-MEMORY copy until Save
(hcnm-bn-eb-open ename)  ; Load lattribs + XDATA
(hcnm-bn-eb-auto-button tag auto-type)  ; Update dialog field only
(hcnm-bn-eb-save ename)  ; ATOMIC write: lattribs + XDATA + reactors
(hcnm-bn-eb-cancel)  ; Safe: no writes, entity unchanged
```

**Model layer functions:**
```autolisp
(defun hcnm-bn-model-set-auto (tag text auto-type)
  ;; 1. Update lattribs (in-memory)
  ;; 2. Store XDATA with verbatim value for search
  ;; 3. Attach reactor (if coords-based, store viewport/transform)
  ;; 4. Store reference object handle for reactor lookup
  )

(defun hcnm-bn-model-set-free (tag text)
  ;; 1. Update lattribs (in-memory)
  ;; 2. Clear XDATA for this tag
  ;; 3. Remove reactor (user text doesn't auto-update)
  )
```

### 3.3. Key Notes Tables

*[Section stub - to be documented]*

Key Notes Table generation, search scope, quantity calculation.

### 3.4. Quantity Take-off

*[Section stub - to be documented]*

Project-wide quantity totals, .not file processing, CSV integration.

### 3.5. CNM Plus (HawsEDC) Tools

*[Section stub - to be documented]*

Additional tools following command reference and cnmloader categories. Developer tools: haws-tip, haws-config, etc.

---

## 4. Development Standards

### 4.1. Function Naming Conventions

See [standards-03-style.md, Sections 3-7](../devtools/docs/standards/03-style.md) for comprehensive naming conventions including:
- Case conventions (lowercase)
- Delimiter style (hyphens)
- Type prefixes (obj-, en-, -p suffix)
- Function naming patterns (verb-noun)
- Data flow functions (source-to-destination pattern)

**CNM namespace architecture** (for quick reference):
```
hcnm-*                    Top-level CNM functions
├── hcnm-config-*         Configuration/settings
├── hcnm-projnotes-*      Project Notes management
├── hcnm-key-table-*      Key Notes Tables
├── hcnm-qt-*             Quantity Takeoff
└── hcnm-bubbles-*        Bubble Notes subsystem (cohesive, sandboxed)
    ├── hcnm-bubbles-insert-*      Insertion functions
    ├── hcnm-bubbles-eb-*          Edit box (dialog) functions
    ├── hcnm-bubbles-reactor-*     Reactor (auto-update) functions
    ├── hcnm-bubbles-xdata-*       XDATA management
    └── hcnm-bubbles-lattribs-*    Attribute list transforms
```

### 4.2. Code Style

#### 4.2.1. Indentation Standards

See [standards-03-style.md, Section 1.1.1](../devtools/docs/standards/03-style.md#111-indentation-and-whitespace) for complete indentation rules.

**Quick reference:** 2-space indentation, no tabs, weakly allow cond exception.

#### 4.2.2. AutoLISP Extension Usage

**CRITICAL:** Autodesk AutoLISP Extension (autodesk.autolispext) is installed in this workspace.

**HUMANS SEE ERRORS AUTOMATICALLY via syntax highlighting. YOU MUST USE `get_errors` TO ACCESS THE SAME INFORMATION.**

**Workflow for AutoLISP edits:**
1. **Before editing:** Run `get_errors` to check current state (MANDATORY)
2. **Make changes:** Edit using `replace_string_in_file`
3. **After editing:** Run `get_errors` immediately to validate (MANDATORY)
4. **Fix issues:** If errors found, fix before proceeding

**CRITICAL - Tell the truth:**
- ❌ NEVER say: "File loads successfully" (you cannot test in AutoCAD)
- ✅ INSTEAD say: "VS Code shows no errors" or "AutoLISP extension reports no diagnostics"
- ❌ NEVER claim you tested something you didn't
- ✅ ALWAYS be explicit about what you verified vs. what requires human testing

**Note:** Even with no VS Code errors, user should test in AutoCAD (extension may not catch all runtime issues).

#### 4.2.3. AutoLISP Idioms

See [standards-03-style.md, Sections 3-6](../devtools/docs/standards/03-style.md) for complete naming conventions, including:
- Lower case symbols (section 3.1)
- Hyphen delimiters (section 4.1)
- Type prefixes: obj-, en-, es-, eg-, lst-, -p suffix (section 5)
- Name length guidelines (section 6)

**Code style:**
- **Formatting:** Use AutoLISP Extension auto-formatter (VS Code command)
- **No closing comments:** Don't add `;end defun` or `;end cond`
- **Explicit local variables:** Always declare in `/ var1 var2` parameter list
- **No ad hoc globals:** Use `haws-config` system
- **AI editing:** Follow standards strictly. Verify with `get_errors` after edits.
- **Deep nesting (3+ levels):** AI has known limitation with parentheses - ask human to verify after editing
- **Always run `get_errors`** Don't claim code is fixed without verification

#### 4.2.4. Dotted Pairs vs Lists

See [standards-03-style.md, Section 12](../devtools/docs/standards/03-style.md) for complete guidance on when to use dotted pairs vs proper lists in AutoLISP.

**Quick reference:** Use dotted pairs for alists (key-value with `assoc`), proper lists for everything else.

### 4.3. Documentation Standards

**Function headers:**
- Explain purpose, parameters, return values

**Architecture comments:**
- Explain WHY, not just WHAT

**Data flow:**
- Document transformations clearly

**Alerts to history:**
- All alerts must enclose princ: `(alert (princ "Hello world"))`

### 4.4. Error Handling Principles

**Fail loudly:**
- Use `(alert ...)` for schema violations

**User-friendly:**
- Clear error messages with context

**Graceful degradation:**
- Don't crash AutoCAD

---

## 5. AI Collaboration Guide

### 5.1. Communication Patterns

#### When human says...

- **"Use the architecture"** → Refer to lattribs data model and XDATA patterns
- **"Fail loudly"** → Strict validation with clear alerts
- **"Obsolete architecture"** → Deprecated legacy formats
- **"The reactor"** → VLR-OBJECT-REACTOR for auto-updating bubble text
- **"Command [name]"** → Always means AutoLISP command function `c:[name]`, not regular function
- **"cinote:"** → Revise copilot-instructions.md to include that information

#### What helps

- Ask clarifying questions about Civil 3D API specifics
- Propose multiple options (strict vs. lenient, refactor vs. patch)
- Explain trade-offs clearly
- Show examples with real data structures

#### What doesn't help

- Assuming modern language features (no closures, limited list processing)
- Complex abstractions (keep it simple for AutoLISP)
- Breaking changes without migration path

### 5.2. Planning Document Workflow

#### When to Use Planning Documents

**ALWAYS create for:**
- GitHub issues (use `.ai-plans/issue-{number}-{short-name}.md`)
- Features requiring multiple steps or phases
- Complex refactorings affecting multiple files
- Anything needing human validation between steps

**DON'T create for:**
- Simple bug fixes (< 5 lines changed)
- Documentation-only changes
- Trivial formatting/style fixes
- One-shot exploratory tasks

#### Workflow Steps

**Step 1: Create Planning Document**
1. Copy `.ai-plans/TEMPLATE.md` to `.ai-plans/issue-{number}-{short-name}.md`
2. Fill in executive summary, phases, tasks
3. Ask for clarification if needed (Questions section)
4. Wait for human approval before proceeding

**Step 2: Execute Phases**
1. Mark phase as "in-progress"
2. Complete all tasks for that phase
3. Update progress log with timestamps
4. Mark phase as "completed"
5. ASK HUMAN to validate before next phase

**Step 3: Keep Planning Doc Current**
- ✅ Update task checkboxes as completed
- ✅ Log significant progress (timestamps)
- ✅ Document technical decisions
- ✅ Add new questions as they arise
- ✅ Update "Files Changed" lists
- ❌ Don't let planning doc get stale

**Step 4: Handle Interruptions**
- Planning doc preserves all context
- Human can say "Continue working on issue-{number}"
- Pick up where you left off

**Step 5: Feature Complete**
1. Complete test plan
2. Update status to "completed"
3. Ask human to review
4. Human attaches planning doc to GitHub issue
5. Human closes issue

#### Best Practices

**Phases should be:**
- Small enough to validate (30 min - 2 hours)
- Self-contained (validate independently)
- Ordered by dependencies

**Progress logs should:**
- Use ISO timestamps: `2025-10-31 14:30`
- Be concise but specific: "Completed lattribs-validate function" not "Made progress"
- Record human validation: "Phase 1 COMPLETE - validated by Tom"

**Questions should:**
- Be specific and actionable
- Include context (why asking)
- Propose options when possible
- Get marked "Resolved" with answer and date

#### Git Integration

**Commit strategy:**
- Commit after completing each phase
- Mention phase in message: "feat(bubbles): Phase 2 - Add reactor callbacks (issue #X)"
- Planning doc stays in `.ai-plans/` (gitignored)
- Attach to GitHub issue when feature complete

**Branch naming:**
- `feat-{short-name}` for features
- `fix-{short-name}` for bugs
- `refactor-{short-name}` for refactorings

#### Summary vs Planning Documents

**Planning documents** (`.ai-plans/issue-X-name.md`):
- Created at START of work
- Updated continuously during work
- Track phases, progress, questions
- Gitignored (attached to issue when done)

**Summary documents** (optional, in `docs/`):
- Created at END if requested
- Explain completed work for future reference
- Go into version control
- Examples: architecture decisions, reorganization summaries

**Default:** Create planning docs for GitHub issues. Only create summary docs if explicitly requested.

### 5.3. Document Structure Guidelines

See [standards-02-ai-human-collaboration.md](../devtools/docs/standards/02-ai-human-collaboration.md) for comprehensive documentation standards:
- Section 3: Document Standards (structure, numbering, naming)
- Section 5: AI Agent Instructions (reading docs, editing docs, embeddings)

**Key principle for CNM docs:** Use hierarchical numbering (1.2.3), clear headings, concrete examples, and section references ("See Section X.Y.Z") for AI comprehension.

### 5.4. Testing Strategy

#### No Unit Tests Yet

AutoLISP lacks standard testing frameworks. Current approach:
- **Smoke tests:** Pure Lisp expressions to verify transforms
- **Manual testing:** Load in AutoCAD, test with real drawings
- **Incremental:** Test small changes before large refactors

#### Pre-Test Cleanup Command

`(c:pretest)` - Removes all bubble reactors and test bubbles (prevents orphaned reactors)

### 5.5. Current Work Status

#### In Progress (feat-sunrise branch)

1. **Renaming complete:** lattribs functions have clear, consistent names
2. **XDATA refactoring:** Search-based parsing with robust auto-text handling
3. **Validator implementation:** Adding strict schema validation

#### Known Issues

- Legacy references removed, clean 2-element architecture implemented
- `lattribs-split` needs proper XDATA search implementation
- Reactor cleanup logic incomplete

#### Related Resources

- **Standards docs:** `standards_05_architecture.md`, `standards_06_lattribs_schema.md`
- **Issue tracker:** https://github.com/hawstom/cnm/issues
- **Project:** 20+ year-old codebase, actively maintained for civil engineering customers


