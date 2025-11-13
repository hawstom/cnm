# GitHub Copilot Instructions for CNM Project

## 1. Quick Start Guide

### 1.1. Essential Instructions

1. **ci** = This file (.github\copilot-instructions.md)
2. **cinote:** = Immediately revise ci to include that information. "cinote." = You failed to execute previous cinote command
3. **citruth.** = Tell the truth! Qualify statements with certainty estimates. Don't say "Perfect/Fixed/Done" when you mean "Please test"
4. **Use get_errors tool**: Check syntax after AutoLISP edits. Fix errors before reporting
5. **MANDATORY FUNCTION VERIFICATION**: NEVER use any function without first searching ORIGINAL codebase with grep_search AND verifying with git history that function existed before recent AI changes. If function not found in original CNM code, check official Autodesk documentation with fetch_webpage. See Section 1.5 for complete protocol.
6. **AutoLISP load paths**: Load expressions must use full paths or constructed paths like `(load (strcat (vl-filename-directory (vl-filename-directory (findfile "cnm.lsp"))) "\\devtools\\scripts\\" filename))` since relative paths are not legal for (load). Use nested vl-filename-directory calls to climb directory tree, not ".." paths.
7. **AutoLISP Common Pitfalls**: See Section 1.4 for AutoLISP/Common Lisp differences and correct AutoLISP idioms.

### 1.2. Project Context

**What is CNM?** Civil engineering tool for managing construction notes on AutoCAD drawings.

**AI's role:** Help maintain 20-year-old codebase with strict data integrity requirements.

### 1.3. Key Principles
- Don't break code or cause regressions
- Make it gradually more maintainable and readable
- Fail loudly (strict validation); never mask errors
- Respect user edits (users bypass our dialogs)

### 1.4. AutoLISP vs Common Lisp Differences

**AutoLISP ≠ Common Lisp:** AutoLISP lacks many Common Lisp functions. Use these AutoLISP idioms:

- **Function exists:** `(boundp 'function-name)` ✅ | `fboundp` ❌ (doesn't exist)
- **Load paths:** Nested `vl-filename-directory` calls ✅ | `".."` relative paths ❌ (unsupported)  
- **Symbolic constant idiom:** AutoLISP does not provide any benefits for user symbolic constants `'my-constant` ❌

### 1.5. CRITICAL: Function Name Verification Protocol

**MANDATORY BEFORE USING ANY FUNCTION:**

1. **Search ORIGINAL codebase FIRST:** Use `grep_search` to find the function in devsource/*.lsp files
2. **Verify with git history:** Use `git log -S "function-name"` to confirm function existed BEFORE recent AI changes, not AI additions
3. **If not in original code:** Check official Autodesk AutoLISP documentation or established AutoLISP community resources using `fetch_webpage` tool
4. **If function not documented:** EXPLICITLY STATE "Function not found in original CNM codebase or Autodesk documentation" and do not use it
5. **NEVER assume function exists:** Just because it "should" exist doesn't mean it does
6. **When errors occur:** First suspect your own function names, not the environment

**Protocol violation consequences:** Function hallucination wastes human time and undermines trust. This is completely preventable by following the verification protocol above.

**Verified Visual LISP functions (found in CNM codebase):**
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

### 1.6. AI Hallucination Blacklist

**BANNED FUNCTIONS (do not exist in AutoLISP):**
- `vlax-object-p` - AI hallucination, does not exist
- `fboundp` - Common Lisp only, not AutoLISP

**ADD TO BLACKLIST:** When human corrects AI hallucination, immediately add function to this blacklist to prevent repeat arguments.

**Protocol violation consequences:** Function hallucination wastes human time and undermines trust. This is completely preventable by following the verification protocol above.

### 1.5. AutoCAD-Specific Pitfalls

**AutoCAD block behavior patterns:**

- **Anonymous blocks:** Dynamic blocks become anonymous when modified. Use EffectiveName property or XDATA for identification
- **Auto-text classification:** Handle-based (StaOff/Dia/Slope use object handles) vs Handleless (N/E/NE use empty handle). Don't confuse with "coordinate-based" - both types may require coordinates.

**Auto-text type classification (two orthogonal properties):**
- **Handle-based**: StaOff/Dia/Slope (stores reference object handle) vs Handleless: N/E/NE (empty handle)  
- **Coordinate-based**: StaOff/N/E (needs VPTRANS for paper space) vs Non-coordinate: Dia/Slope (no transform needed)
- **Complete architecture:** See S05.4.2 (Reactive Auto-Text System) for VLR-OBJECT-REACTOR patterns, BlockReactors flag lifecycle, and debugging guidelines
- **Load expressions:** Must use full paths or constructed paths since relative paths are not legal for (load) [TGH 2025-11-12 22:52:09: this is redundant with information above. DRY]

### 1.6. AI Collaboration Workflow

See Section 5.2 for planning document workflow and [standards-02-ai-human-collaboration.md](../devtools/docs/standards/02-ai-human-collaboration.md) for general AI collaboration patterns.

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
2. **cnm.mnl** (menu LISP file) - Checks for `haws-mklayr`, loads CNMloader if not present
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

##### 3.2.3.3. Coordinate Transformation

**Problem:** AutoCAD supports multiple coordinate systems. World Coordinate System (WCS) is canonical, but users may encounter:
- User Coordinate Systems (UCS)
- Object Coordinate Systems (OCS)
- Paper space Display Coordinate Systems (DCS, represented as OCS)

For auto-text, CNM needs model space WCS coordinates, but bubbles may be in paper space OCS or model space with translated/rotated UCS.

**Solution:** For paper space bubbles referencing model space objects for coordinate-based auto-text, store 3-point correspondence in XRECORD:
- 3 reference points in OCS (viewport space)
- 3 reference points in WCS (model space)
- Apply affine transformation on reactor updates

##### 3.2.3.4. Viewport Association

**Problem:** Coordinate transformation requires associating viewport with bubble note. Users expect paper space bubbles to "look through" specific viewports to see model space objects.

**Solution:** For paper space bubbles referencing model space objects for coordinate-based auto-text, store viewport number in VPTRANS data (first element of viewport-data list). On updates, correctly transform auto-text coordinates from paper space to model space.

#### 3.2.4. Reactor System

##### 3.2.4.1. BlockReactors Flag

**Purpose:** Prevent infinite recursion when reactor callbacks modify reactor-monitored objects.

**Problem:** AutoCAD VLR reactors fire recursively when callback code modifies monitored objects. Modifying bubble attributes during callback triggers `:vlr-modified` on associated leader, causing nested callback. Without prevention: infinite recursion crashes AutoCAD.

**Autodesk guideline:** "Don't modify reactor-monitored objects inside reactor callbacks." But CNM's purpose IS to update bubbles when tracked objects change. We must modify monitored objects.

**Solution:** `BlockReactors` config flag (stored in cnm.ini, persists across sessions).

**Flag lifecycle:**
```
Default: "0" (normal operation)
Parent callback fires → Save state → Set "1" → Modify bubbles
  Nested callback fires → Gateway 1 checks flag → Sees "1" → Exits immediately
Parent continues → Restores saved state "0"
```

**Save/Restore Pattern (CRITICAL):**
```autolisp
(defun hcnm-ldrblk-reactor-callback (obj-notifier reactor event-list / saved-state ...)
  (setq saved-state (c:hcnm-config-getvar "BlockReactors"))
  (if (= saved-state "1")
    (progn  ; Gateway 1: Already blocked
      (haws-debug "=== REACTOR BLOCKED by BlockReactors flag ===")
      (c:hcnm-config-setvar "BlockReactors" saved-state)  ; Restore "1"
      (princ))
    (progn  ; Normal processing
      (c:hcnm-config-setvar "BlockReactors" "1")
      (hcnm-ldrblk-reactor-notifier-update ...)
      (c:hcnm-config-setvar "BlockReactors" saved-state))))  ; Restore "0"
```

**Why save/restore?** Nested callbacks must honor parent's blocking state. If nested restored to "0", parent's blocking would be defeated.

**Defensive resets:** At natural completion boundaries (insertion complete, dialog complete, error handler), reset to "0" to prevent stuck flags.

**Philosophy:** Better to allow one extra update than permanently block all updates.

**Flag semantics:**
- `"0"` = Normal operation (process all reactor events)
- `"1"` = Ignore next reactor event (self-clearing via save/restore) [TGH 2025-11-12 22:58:27: Ignore all.]

##### 3.2.4.2. Data Structure Hierarchy

**One Reactor Per Drawing (Not Per Bubble)**

CNM uses single persistent `VLR-OBJECT-REACTOR` per drawing:
- Tracks multiple owner objects (alignments, pipes, surfaces, leaders)
- When ANY tracked owner changes, callback fires and updates dependent bubbles
- Data structure stored in reactor's `:data` property

**5-Level Nested Hierarchy:**
```
"HCNM-BUBBLE"          ; Application namespace (fixed key)
  → owner-handle        ; Object being tracked
    → bubble-handle     ; Bubble depending on this owner
      → tag             ; Attribute tag (e.g., "NOTETXT1")
        → auto-type     ; Calculation type (e.g., "StaOff")
          → reference-handle  ; LEAF: Object providing data
```

**Semantic terminology (CRITICAL):**
- **OWNER** = Any object attached to reactor (reference OR leader)
- **NOTIFIER** = Specific owner that triggered THIS callback
- **REFERENCE** = Object providing calculation data (always reference object, never leader)

**Lookup pattern:**
1. Search owner-list to find notifier-entry (which owner triggered?)
2. Drill down through bubbles/tags to find reference handles (where's the data?)

**Direct vs Leader Paths:**

**Direct path** (no leader):
- owner = alignment, reference = alignment (same object)
- Update when alignment geometry changes

**Leader path** (coordinate-based with leader):
- owner = leader (track arrowhead), reference = alignment (different object)
- Update when leader moves OR when alignment geometry changes
- Stores TWO owners: leader and alignment

**Key insight:** Coordinate-based auto-texts (Sta/Off/StaOff/N/E/NE/Z) attach TWO owners. Non-coordinate auto-texts (Dia/SLope/L) attach only reference object.

##### 3.2.4.3. Cleanup Pattern

**Problem:** Users can erase bubbles anytime. Reactor data must track deletions to prevent memory leaks, performance degradation, error messages.

**Solution:** Three-tier cleanup with "DELETED" status signal.

**Tier 1: Immediate Detection**
- Function: `hcnm-ldrblk-reactor-bubble-update`
- When: Every reactor callback attempts to update each tracked bubble
- Check: `(not (entget ename-bubble))` → Return `"DELETED"` string

**Tier 2: Batch Cleanup**
- Function: `hcnm-ldrblk-reactor-notifier-update`
- When: After processing all bubbles for ONE notifier
- Algorithm: Accumulate deleted handles, filter using `vl-remove-if` (functional, immutable)

**Tier 3: Deep Scrub**
- Function: `hcnm-ldrblk-cleanup-reactor-data`
- When: Called explicitly via `(c:pretest)` or maintenance commands
- Purpose: Full scrub of entire reactor data structure

**Why functional updates?** Reactor callbacks can fire DURING data structure iteration. Mutating lists mid-iteration corrupts. `vl-remove-if` returns NEW list (safe for concurrent access).

**Status signal pattern:**
- `"DELETED"` = Bubble erased (string, not symbol `'deleted`)
- `T` = Bubble modified successfully
- `NIL` = Bubble skipped (no changes)

**Philosophy:**
- Fail fast: Detect immediately (Tier 1)
- Batch efficiently: Remove multiple deletions (Tier 2)
- Manual deep clean: Explicit maintenance (Tier 3)

##### 3.2.4.4. Architecture Flaw: Handleless Auto-Text Reactor Disambiguation

**CRITICAL ARCHITECTURAL FLAW DISCOVERED (2025-11-12):**

**Problem:** The architecture uses `obj-target` parameter to distinguish insertion path from reactor path, but handleless auto-text types (N/E/NE) have no reference object to pass, making this disambiguation impossible.

**Current Implementation:**
```autolisp
;; auto-dispatch signature
(defun hcnm-lb-auto-dispatch (tag auto-type obj-target bubble-data) 

;; Insertion path: obj-target = NIL (no object passed)
(hcnm-lb-auto-dispatch tag "N" nil bubble-data)

;; Reactor path: obj-target = T (sentinel for handleless)
(hcnm-lb-auto-dispatch tag "N" t bubble-data)
```

**How handleless auto-text detects reactor updates:**
```autolisp
;; In auto-ne function:
reactor-update-p
  (and
    obj-target
    (or (= obj-target t)           ; T sentinel for N/E/NE reactor updates
        (= (type obj-target) 'vla-object))  ; Reference object for other types
  )
```

**The Architecture Flaw:**
The `obj-target` parameter serves dual purposes:
1. **Data source:** VLA-OBJECT for handle-based auto-text (alignments, pipes)
2. **Path discriminator:** NIL vs non-NIL to distinguish insertion from reactor

This works for handle-based auto-text but fails for handleless types (N/E/NE):
- **Insertion:** `obj-target = NIL` (correct)
- **Reactor:** `obj-target = T` (sentinel value, not data source)

**Why This Causes Problems:**
1. **Semantic confusion:** Same parameter represents data AND control flow
2. **Special case handling:** Handleless types need sentinel value logic
3. **Future extensibility:** Adding new handleless types requires sentinel modifications
4. **Code complexity:** Every auto-text function must handle sentinel values

**PROPOSED ARCHITECTURE REVISION:**

**Option A: Add explicit `reactor-context-p` parameter**
```autolisp
;; Clean signature with explicit context
(defun hcnm-lb-auto-dispatch (tag auto-type obj-reference bubble-data reactor-context-p)

;; Insertion path
(hcnm-lb-auto-dispatch tag "N" nil bubble-data nil)

;; Reactor path  
(hcnm-lb-auto-dispatch tag "N" nil bubble-data t)

;; Handle-based types work naturally
(hcnm-lb-auto-dispatch tag "StaOff" obj-alignment bubble-data t)
```

**Option B: Add context to bubble-data structure**
```autolisp
;; Store context in bubble-data
(setq bubble-data (hcnm-lb-bubble-data-set bubble-data "reactor-context-p" t))
(hcnm-lb-auto-dispatch tag auto-type obj-reference bubble-data)
```

**Option C: Separate functions for insertion vs reactor**
```autolisp
;; Dedicated functions eliminate ambiguity
(hcnm-lb-auto-dispatch-insertion tag auto-type obj-reference bubble-data)
(hcnm-lb-auto-dispatch-reactor tag auto-type obj-reference bubble-data)
```

**RECOMMENDATION:** Option A (explicit parameter) provides:
- Clear semantic separation
- Minimal code changes  
- Future extensibility
- Type safety (boolean vs sentinel values)

**Implementation Impact:**
- Update all auto-dispatch call sites (insertion, editing, reactor)
- Update all auto-text functions to accept new parameter
- Remove sentinel value logic from handleless functions
- Update bubble-data-add-auto-metadata calls

**Status:** ARCHITECTURAL REVISION REQUIRED - Current fix attempts treating symptoms, not root cause.

##### 3.2.4.5. Performance Considerations

**Known bottlenecks:**
- XDATA read/write on every reactor callback
- Nested loops in handle lookup (`hcnm-ldrblk-get-reactor-handle-for-tag`)
- Multiple reactor callbacks per leader move
- Debug output removed from hot paths (2025-11-01)

**Optimization suggestions (future work):**
1. Reactor data reorganization: Move bubble to higher level in hierarchy
2. Batch updates: Build update list, apply all at once (reduce XDATA writes)
3. Cache reactor handle lookups
4. Profile operations before optimizing

**Status:** Debug output removed. Further optimization pending user feedback on production performance.

#### 3.2.5. Reactive Auto-Text

##### 3.2.5.1. Technical Challenges

**1. Paper Space Complexity**

Bubbles in paper space viewports require coordinate transformation from paper space DCS (represented as OCS) to model space WCS.

- **Implementation:** VPTRANS XRECORD stores 3-point correspondence for affine transformation
- **Status:** Implemented, testing in progress

**2. Free-form User Edits**

Users edit bubble attributes directly in AutoCAD, bypassing CNM dialogs. Must robustly separate auto-text from user edits.

- **Implementation:** XDATA stores verbatim auto-text as search needles for search-and-replace updates
- **Status:** Implemented, handling edge cases

**User expectations:**
- Any text they add remains intact while auto-text updates
- Text around auto-text preserved
- Format changes via CNM settings reflected in next update
- If they corrupt/delete auto-text, it doesn't get restored
- Multiple auto-text fields with identical values can't all update independently (known limitation)

**3. Legacy Migration**

20+ years of customer drawings with evolving data formats. Fortunate to have few migration challenges from user perspective. Free to improve internal data formats as long as we maintain compatibility with:
- Block attribute names and values
- Project Notes file format
- Key Notes Table format
- Quantity Takeoff format

**4. IgnoreReactorOnce Flag (OBSOLETE - replaced by BlockReactors)**

This flag was part of early reactor architecture but has been superseded by the more robust BlockReactors flag with save/restore pattern.

##### 3.2.5.2. Free-Form Architecture

**Problem:** Previous 4-element lattribs structure limited users to ONE auto-text per line.

**User request:** "I want `Storm Drain STA 10+25 RT, Ø24`" (TWO auto-texts: station AND diameter)

**Solution (implemented 2025-11-01):**
- lattribs: `(("TAG" "full-text") ...)` ← No structural limits
- XDATA: `(("TAG" (((auto-type . handle) . "value") ...)) ...)` ← Multiple auto-texts with handles
- Dialog: Single text field per line ← Natural UX like native AutoCAD
- Reactor: Search/replace each auto-text needle independently

**Key architectural decisions:**
- Auto-text buttons during dialog = IN-MEMORY only (no XDATA writes until Save)
- Save button = ATOMIC write (lattribs + XDATA + reactors all at once)
- Cancel button = SAFE (no writes happened, nothing to rollback)

**Auto-text insertion with delimiter:**

User delimiter: ` ``` ` (triple backtick) marks insertion point
- Why backticks? No shift key, visually distinct, won't accumulate confusingly
- If delimiter exists: Replace FIRST occurrence
- Otherwise: Append WITHOUT SPACE (user controls spacing)

**CRITICAL DESIGN: No Automatic Spacing**

CNM NEVER adds spaces around auto-text. Spacing fully user-controlled. This is atypical but appropriate because users expect fine-grained control.

Example:
```
User types: "N"
Click E button: "NE 878838.54"  ← NO SPACE between N and E
User should have typed: "N ``` " to get "N E 878838.54"
```

**Philosophy:**
- Fail gracefully in UX - Append if no delimiter (don't anger users)
- Fail loudly in data - Invalid lattribs = alert and exit
- Teach quietly - Appending shows delimiter would be better

**Handle-based XDATA:** Multiple auto-texts from same attribute line supported. Each associated with reference object handle (alignment, pipe) or empty string `""` for coordinates.

**Auto-text per reference limitation:** Current dotted pair format `(handle . "auto-text")` means ONE auto-text per reference object per tag. For multiple from same reference (e.g., station AND offset from same alignment), click auto-button once and CNM generates combined format controlled by auto-type ("Sta" vs "Off" vs "StaOff").

Future enhancement could support `(handle ("auto1" "auto2" ...))` format.

**Terminology - Semantic Hierarchy:**
- OWNER = Any object attached to reactor (reference OR leader)
- NOTIFIER = Specific owner that triggered callback
- REFERENCE = Object providing calculation data (always reference, never leader)

Lookup: Search owner-list for notifier → Drill to reference handles in leaf nodes.

**Code semantically correct - documentation clarified 2025-11-05.**

##### 3.2.5.3. Implementation Examples

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
(hcnm-bubbles-insert-auto-button tag)
  ;; 2. CONTROLLER: Route to handler
  (hcnm-bubbles-controller-add-auto tag auto-type)
    ;; 3. MODEL: Generate, store XDATA, attach reactor
    (hcnm-bubbles-model-set-auto tag text auto-type)
```

**MVC call flow - Editing:**
```autolisp
;; Dialog operates on IN-MEMORY copy until Save
(hcnm-bubbles-eb-open ename)  ; Load lattribs + XDATA
(hcnm-bubbles-eb-auto-button tag auto-type)  ; Update dialog field only
(hcnm-bubbles-eb-save ename)  ; ATOMIC write: lattribs + XDATA + reactors
(hcnm-bubbles-eb-cancel)  ; Safe: no writes, entity unchanged
```

**Model layer functions:**
```autolisp
(defun hcnm-bubbles-model-set-auto (tag text auto-type)
  ;; 1. Update lattribs (in-memory)
  ;; 2. Store XDATA with verbatim value for search
  ;; 3. Attach reactor (if coords-based, store viewport/transform)
  ;; 4. Store reference object handle for reactor lookup
  )

(defun hcnm-bubbles-model-set-free (tag text)
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


