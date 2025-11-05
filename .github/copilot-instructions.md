# 1. Quick Start for AI

## 1.1. General instructions
1. The alias name of this document (.github\copilot-instructions.md) is "ci".
2. When a human says "cinote:" it means to immediately revise .github\copilot-instructions.md to include that information. The single-word reminder "cinote." signals the AI failed to execute a previous cinote: command and should update the file now.
3. When a human says "citruth." it means you failed to follow this instruction. Tell the truth! Be transparent about what you know. Qualify your statements with clear certainty estimates, especially when it comes to statements about the state of the code or our work. Don't say things like "Perfect", "Fixed", or "Done" when what you really mean is "Please test" or "I added/revised/removed this. Please test it".
4. This project is primarily written in AutoLISP for AutoCAD. This development environment has the AutoLISP Extension installed. You have the get_errors tool. Use it to check for syntax errors after code changes. Fix errors before reporting.

**What is CNM?** Civil engineering tool for managing construction notes on AutoCAD drawings.

**My role as AI?** Help maintain 20-year-old codebase with strict data integrity requirements.

**Key principles:**
- Don't break code or cause regressions
- Make it gradually more maintainable and readable
- Fail loudly (strict validation); do not handle errors silently. It is better not to handle an error than to mask it.
- Respect user edits (users bypass our dialogs)

**AI Collaboration Workflow:**
- When working on GitHub issues, create/update planning doc in `.ai-plans/issue-{number}-{short-name}.md`
- Use phased approach: Plan → Execute Phase → Validate → Next Phase
- Update progress log as you work
- Ask clarifying questions in Questions section
- See Section 1.7 for full workflow

**Read these first:**
- Section 1.1.1: Core workflow (what engineers do)
- Section 1.2.1: Data model (lattribs structure) (for bubble notes area only)
- Section 1.6: Communication tips
- Section 1.7: AI collaboration workflow with planning documents
- Section 1.8: Document structure guidelines (how to write effective docs for AI)

---

# 1. GitHub Copilot Instructions for CNM Project

## 1.1. Project Context

**CNM (Construction Notes Manager)** is an AutoCAD add-on written in AutoLISP that helps civil engineers manage construction notes on drawings using "bubble notes" (leader annotations with attribute text).

### 1.1.1. Core Use Case

1. **You edit Project Notes**
    - CNM Project Notes have types (shapes), numbers (alphanumeric), descriptions, and can have quantities and cost instructions.
    - Descriptions may reference "the plan (view)" for specific information (eg. "Trench width per plan" or "Length per plan").
    - The Project Notes template is copied from the CNM application folder automatically for every new project. This lets you put your own standard notes in the application folder to seed all new projects.
2. **You insert Bubble Notes** with numbers keyed to the Project Notes.
    - CNM Bubble Notes have a shape, a number, and optional free form text on any of 7 lines including a hidden line (0) for location-specific expansion on the key note.
    - If Project Notes has a line number (eg. LINE4) in the quantity instructions column for a bubble type/shape and number, you can include quantities (eg. 7 LF, 280 SY, or 8 EA) in the corresponding line of that bubble note. CNM ignores the units, reads only the numeric quantity, and uses the units given in Project Notes.
    - CNM Bubble Notes are made of blocks with attributes so that you can easily customize their appearance or content or build your own blocks or inserters from scratch as long as they have the required attributes and standard names.
3. **You make a Key Notes Table** specific to the current sheet.
    - CNM reads your Project Notes and adds to your drawing a Key Notes Table with every note that has bubbles in your given search scope (visible only and possibly on current tab only).
    - CNM calculates totals for any quantities instructed in Project Notes and lists them for each note in the Key Notes Table.
    - CNM saves what it found in a .not file alongside your drawing so that later you can make a Quantity Takeoff Table in another drawing based on the sheets you choose to tabulate. 
    - The Key Notes Table is built of insertions of the noteqty.dwg block so that you can easily customize its appearance.
4. **Later, you make a Quantity Takeoff Table** for the entire project
    - CNM QT reads your Project Notes and adds to your drawing a totals Quantity Takeoff Table based on the saved .not files for your project sheets.
    - CNM QT tabulates every sheet in a csv of the current drawing's base name so that you can do quality control in a single table.
    - The Quantity Takeoff Table is built of text following the current style and dimension settings so that you can easily customize its appearance.

### 1.1.2. Projects

#### 1.1.2.1. Project Management
A CNM Project is a cnm.ini file. All drawings (.dwg) in the same folder with that file are in that CNM Project. Other folders can link to a CNM Project. CNM does this by creating a tiny cnmproj.TXT file in the linked folder that points to the main CNM Project folder.

#### 1.1.2.2. Settings
CNM Project settings are stored in the cnm.ini project file using standard INI format. A cnm.ini file is a CNM Project. There is a template cnm.ini in the CNM application folder that is automatically copied to seed a default Project and settings whenever CNM is used in a new folder. There is a CNM Options dialog that lets you edit project settings easily.

#### 1.1.2.3. Loading Architecture
CNM loads automatically when AutoCAD starts via this sequence:

1. **cnm.cuix** (AutoCAD menu file) - Entry point, loads with AutoCAD
2. **cnm.mnl** (menu LISP file) - Checks for `haws-mklayr`, loads CNMloader if not present
3. **CNMloader.lsp** (initialization file) - Loads immediately and:
   - Defines `haws-autoload` function (creates command stubs)
   - Loads core libraries immediately: `edclib.lsp`, `haws-tip.lsp`, `cnmalias.lsp`
   - Defines autoloader stub for `hcnm-ldrblk-reactor-callback` (persistent reactor support)
   - Sets up command autoloaders for `cnm.lsp` functions (lazy loading)
4. **cnm.lsp** - Main CNM functionality, lazy-loaded when user runs first CNM command

**Key Points:**
- Core libraries (edclib, haws-tip, cnmalias) load immediately via CNMloader.lsp
- CNM commands use `haws-autoload` pattern: stub loads file on first use
- Reactor callback has special autoloader stub (reactors fire before commands run)
- This is NOT the place to load cnm.lsp eagerly - that would defeat lazy loading pattern

### 1.1.3. Bubble Notes

#### 1.1.3.1. Minimum Definition of a Bubble Note
Bubble notes are simply attributed blocks with one twist: The shape is a dynamic selector so that you can change the shape without making a new insertion and copying information.

You can create your own bubble note blocks as long as they have 
- A dynamic block "Shape" property with values that match the types defined in CNM Project settings. CNM comes with 8 shapes: BOX, CIR, DIA, ELL, HEX, OCT, PEN, REC, SST.
- The required attributes: NOTENUM, NOTEPHASE, NOTEGAP, NOTETXT0 through NOTETXT6. (Note that NOTEDATA was an internal (development team) experiment with a way to replace auto text in free-form attribute text; fortunately, neither git nor any user ever saw a block with that attribute included. XDATA serves its intended purpose.)
- Any block name you like. 

#### 1.1.3.2. Included Bubble Notes
CNM comes with bubble notes. They use the block name cnm-bubble-[m?]#-[dir] where [m?] is "m-" multi-text bubbles, # is 0 (for no landing/hook on bubbles) or 1 (for landing/hook on bubbles), and [dir] is "l" for left or "r" for right leader direction. They have the required attributes and dynamic block shape property. They are associated with AutoCAD Leaders.

#### 1.1.3.3. Included Insertion and Editing Tools
CNM comes with insertion and editing tools for bubble notes.

##### 1.1.3.3.1. Current Functionality
- Insert Bubble Note: Places a bubble note of the requested shape with leader, prompts for number, line 1, and line 2. Includes prompt options for a few types of automatic text possibly including
- **Length/Area** from selected polylines
- **Drawing coordinates** (Northing/Easting)
- **Civil 3D Alignments** (station/offset coordinates)
- **Civil 3D Pipe Networks** (diameter, slope, length)
- **Civil 3D Surfaces** (elevations)
- Edit Bubble Note: Opens dialog to edit bubble note attributes and optionally add automatic text.

##### 1.1.3.3.2. Reactive Auto-Text Implementation (DEBUGGING)
- **Status:** Implemented on feat-sunrise branch, currently debugging regressions
- **What it does:** Auto text in bubble notes stays synchronized with source objects and follows leader arrowheads via VLR reactors (AutoLISP object event listeners)
- **Architecture:** Free-form 2-element lattribs with handle-based XDATA for multiple auto-texts per line
- **Current issues:** Some regressions preventing full functionality, addressing bugs one-by-one

###### 1.1.3.3.2.1. Key Technical Challenges (BEING ADDRESSED)

**1. Paper Space Complexity**

Bubbles in paper space viewports require coordinate transformation from paper space DCS (represented by paper space object OCS) to model space WCS.

- **Implementation:** VPTRANS XDATA stores 3-point correspondence for affine transformation
- **Status:** Implemented, testing in progress

**2. Free-form User Edits**

Users edit bubble note attributes directly in AutoCAD, bypassing CNM dialogs. Robust parsing separates auto-generated text from user edits. We provide free-form editing capabilities, so we must store auto-generated text in a way that allows us to reliably identify and update it without overwriting user edits.

- **Implementation:** XDATA stores verbatim auto-text as search needles for search-and-replace updates
- **Status:** Implemented, handling edge cases

Users can reasonably expect:
- Any text they add remains intact while auto text updates correctly
- Text added around auto text remains intact while auto text updates correctly
- If they change CNM Project settings that affect auto text format, the next update reflects those changes (as long as the auto text itself remains uncorrupted and identical to what we store)
- If they completely delete (or fat-finger-corrupt) auto text or change its format (e.g., adding prefixes/suffixes), it does not get acted on or restored at the next update
- They can't have multiple auto text fields with identical values in the same bubble note line and have them all update correctly (known limitation)

**3. Legacy Migration**

20+ years of customer drawings with evolving data formats. CNM is fortunate to have few migration challenges from the user perspective. From the programmer perspective, the code base is evolving. But we are free to improve the data formats used internally as long as we maintain compatibility with block attributes and their names as they exist in customer drawings.

**4. IgnoreReactorOnce Flag Self-Healing**

The `IgnoreReactorOnce` config flag can get stuck at "1" if user hits Escape or an error occurs between `hcnm-ldrblk-space-set-model` (sets "1" during MSPACE/PSPACE transitions) and `hcnm-ldrblk-space-restore` (sets "0" after). When stuck at "1", all reactor updates are blocked by Gateway 1.

**Solution (IMPLEMENTED):**
- **Reactor callback end** (cnm.lsp): Always sets `IgnoreReactorOnce="0"` after callback completes. Self-healing: if blocked by Gateway 1, next reactor event will work.
- **Error handler** (edclib.lsp `haws-core-stperr`): Restores `IgnoreReactorOnce="0"` on any CNM command error/cancel. Applies to all CNM commands using `haws-core-init`.

**Philosophy:** User (or CNM) just did something we ignored. If they do it again, we should NOT ignore it. Better to allow one extra update than to permanently block all updates.

**Semantics:** 
- "0" = Normal operation (process reactor events)
- "1" = Ignore next reactor event (self-clearing)
- Inverted from old AllowReactors ("0"=block, "1"=allow) for clearer intent

###### 1.1.3.3.2.2. Example: Free-form Edit Scenario

**User places bubble with auto text:**
- Attribute text: `"STA 10+25.50"`
- XDATA: `'(("NOTETXT1" . "STA 10+25.50"))`

**User manually edits attribute in AutoCAD:**
- Attribute text: `"Storm Drain STA 10+25.50 RT"`
- XDATA unchanged: `'(("NOTETXT1" . "STA 10+25.50"))`

**Alignment shifts, reactor fires:**
1. Search for `"STA 10+25.50"` in `"Storm Drain STA 10+25.50 RT"`
2. Split: `("Storm Drain " "STA 10+25.50" " RT")`
3. Update: `("Storm Drain " "STA 11+00.00" " RT")`
4. Concatenate: `"Storm Drain STA 11+00.00 RT"`

**Result:** User's prefix/postfix preserved, auto text updated ✅

###### 1.1.3.3.2.3. Architecture Decision: Free-Form 2-Element (IMPLEMENTED)

**PROBLEM:** Previous 4-element lattribs structure limited users to ONE auto-text per line.

**User request:** "I want `Storm Drain STA 10+25 RT, Ø24"` (TWO auto-texts: station AND diameter)

**Previous limitation:**
- lattribs: `(("TAG" "prefix" "auto" "postfix") ...)` ← Only ONE auto field
- Dialog: 3 separate fields (Prefix | Auto | Postfix) ← Awkward UX

**Implemented architecture (free-form 2-element):**
- lattribs: `(("TAG" "full-text") ...)` ← No structural limits
- XDATA: `(("TAG" ("auto1" "auto2" ...)) ...)` ← List of auto-texts
- Dialog: Single text field per line ← Natural UX like native AutoCAD
- Reactor: Search/replace each auto-text needle independently ✅

**Decision made:** 2025-11-01 - Pivoted to free-form 2-element architecture.
**Status:** Implemented, testing in progress on feat-sunrise branch.

###### 1.1.3.3.2.4. Free-Form 2-Element Architecture Implementation

**Data Model:**
- **lattribs:** `(("TAG" "full-text") ...)` - Single string per attribute
- **XDATA:** `(("TAG" ("auto1" "auto2" ...)) ...)` - List of auto-text values for search
- **Dialog:** Single text field per line (like native AutoCAD attribute editor)

**MVC Layer Separation:**

**MVC Call Flow Examples:**

**INSERTION (from command line):**
```autolisp
;; User runs CNM bubble insertion command, clicks auto-text button
;; 1. VIEW: Auto-text button clicked
(hcnm-bubbles-insert-auto-button tag)
  ;; 2. CONTROLLER: Route to appropriate auto-text handler
  (hcnm-bubbles-controller-add-auto tag auto-type)
    ;; 3. MODEL: Generate auto-text, store XDATA, attach reactor
    (hcnm-bubbles-model-set-auto tag text auto-type)
      ;; - Update lattribs (in-memory): (("TAG" "text-with-auto") ...)
      ;; - Store XDATA: Verbatim auto-text for search
      ;; - Attach reactor: If coords-based, store viewport/transform
```

**EDITING (from dialog):**
```autolisp
;; User opens existing bubble, modifies text, clicks Save
;; Dialog operates on IN-MEMORY copy until Save clicked

;; 1. Dialog opens - loads current state
(hcnm-bubbles-eb-open ename)
  ;; Loads lattribs + XDATA into dialog fields (read-only snapshot)

;; 2. User types free text or clicks auto-text button
;;    Auto-button only updates dialog field (no XDATA writes yet!)
(hcnm-bubbles-eb-auto-button tag auto-type)
  ;; Generates auto-text, inserts at ``` or appends
  ;; Updates dialog field only (in-memory)
  ;; Marks field as "pending auto-text write"

;; 3. User clicks SAVE - atomic write of ALL changes
(hcnm-bubbles-eb-save ename)
  ;; CONTROLLER: Process each modified field
  (foreach field modified-fields
    (if (field-has-auto-text? field)
      ;; MODEL: Write auto-text (lattribs + XDATA + reactor)
      (hcnm-bubbles-model-set-auto tag text auto-type)
      ;; MODEL: Write free text (lattribs only, clear XDATA)
      (hcnm-bubbles-model-set-free tag text)))
  ;; Atomically write all lattribs to block attributes
  (hcnm-set-attributes ename lattribs)

;; 4. User clicks CANCEL - discard all changes
(hcnm-bubbles-eb-cancel)
  ;; No XDATA writes happened (safe rollback)
  ;; No lattribs writes happened (safe rollback)
  ;; Dialog just closes, entity unchanged
```

**Key Architectural Decisions:**
- **Auto-text buttons during dialog = IN-MEMORY only** (no XDATA writes)
- **Save button = ATOMIC write** (lattribs + XDATA + reactors all at once)
- **Cancel button = SAFE** (no writes happened, nothing to rollback)
- **Auto-type keys**: "STa", "Off", "stAoff", "N", "E", "NE", "Dia", "SLope", "L", etc.
  (See `hcnm-ldrblk-get-auto-type-keys` for complete list)

**MODEL LAYER (Data Persistence):**
```autolisp
;; Used by both insertion and editing
(defun hcnm-bubbles-model-set-auto (tag text auto-type)
  ;; 1. Update lattribs (in-memory): (("TAG" "new-value") ...)
  ;; 2. Store XDATA: Mark as auto-text with verbatim value for search
  ;; 3. Attach reactor: If coords-based, also store viewport/transform
  ;; 4. Store reference: Object handle for reactor lookup
  )

(defun hcnm-bubbles-model-set-free (tag text)
  ;; 1. Update lattribs (in-memory): (("TAG" "new-value") ...)
  ;; 2. Clear XDATA: Remove auto-text markers for this tag
  ;; 3. Remove reactor: User text doesn't update automatically
  )
```

**Auto-Text Insertion with Delimiter:**

**User Delimiter:** ` ``` ` (triple backtick) marks insertion point
- **Why backticks?** No shift key on most keyboards, visually distinct, won't accumulate confusingly like `,,,` → `,,,,`
- **Alternatives considered:** `|||` (needs shift), `,,,` (can pile up), `[[[` (needs shift)

**Insertion Logic:**
```autolisp
(defun hcnm-insert-auto-text (text auto-value / pos)
  (cond
    ;; If delimiter exists, replace FIRST occurrence (user learns this intuitively)
    ((setq pos (vl-string-search "```" text))
     (strcat (substr text 1 pos)
             auto-value
             (substr text (+ pos 3))))  ; Skip "```"
    ;; Otherwise append WITHOUT SPACE (user controls spacing)
    ;; INTENTIONAL EXCEPTION TO "FAIL LOUDLY": This is UX convenience, not data corruption.
    ;; Fail loudly on DATA integrity, fail gracefully on UX convenience.
    (t (strcat text auto-value))))  ; Append directly - no space padding
```

**TIP for Users:** When clicking multiple auto-text buttons, CNM replaces the **first** delimiter found. This convention is intuitive: type multiple delimiters in order, click buttons in order, each replaces the next delimiter. If no delimiter is found, auto-text appends at the end without adding spaces.

**CRITICAL DESIGN DECISION - No Automatic Spacing:**

**CNM NEVER adds spaces around auto-text.** Spacing is fully user-controlled.

This is an **atypical design** that is appropriate for CNM because users expect fine-grained control over text formatting. Users must manually add spaces or use delimiters to control placement.

**Fallback behavior:**
- If delimiter(s) present: Insert auto-text at **first** delimiter
- Otherwise: Append auto-text **without space padding**

**Example workflows:**
```
User types: "Storm ``` , Sanitary ``` "
Click Station (Storm): "Storm STA 10+25.00 , Sanitary ``` "
Click Station (Sanitary): "Storm STA 10+25.00 , Sanitary STA 8+45.00"

User types: "N"
Click E button: "NE 878838.54"  ← NO SPACE between N and E
User should have typed: "N ``` " to get "N E 878838.54"
```

**Handle-Based XDATA Architecture:**

Multiple auto-texts per line are supported via handle-based associations:

**XDATA Format:** `(("TAG" ((handle1 . "auto1") (handle2 . "auto2"))) ...)`

Each auto-text is associated with its reference object handle:
- Alignment handle for station/offset auto-texts
- Pipe handle for diameter/slope/length auto-texts  
- Empty string `""` for coordinate-based (N/E/NE) auto-texts

**Example:**
```
Display: "Storm STA 10+25.00, Sanitary STA 8+45.00"
XDATA: (("NOTETXT1" ((handle-storm . "STA 10+25.00") 
                     (handle-sanitary . "STA 8+45.00"))))
```

This allows:
- Multiple auto-texts of same type in one line (e.g., two "Sta")
- Each linked to different reference objects (different alignments)
- Reactor updates only affect the specific auto-text that changed

**LIMITATION - One auto-text per reference per tag:**
Current architecture stores `(handle . "auto-text")` dotted pairs, which means you can only have **one auto-text from each reference object per attribute line**. For example:
- ✅ **Allowed**: "Storm STA 10+25.00, Sanitary STA 8+45.00" (two alignments, different handles)
- ❌ **Not supported**: "STA 10+25.00 OFF 5.0' RT" from same alignment (same handle, two auto-texts)

**TIP for Users:** If you need multiple auto-texts from the same reference (e.g., station AND offset from same alignment), click the auto-button once and CNM generates both in one auto-text field. The format is controlled by the auto-type ("Sta" vs "Off" vs "StaOff").

**Future Enhancement:** Could support multiple auto-texts per reference by changing `(handle . "auto")` to `(handle ("auto1" "auto2" ...))`. This would require XDATA reader/writer updates but is architecturally straightforward.

**Terminology - Semantic Hierarchy (CRITICAL):**

**OWNER** = General term for any object attached to reactor (reference OR leader)
**NOTIFIER** = Specific owner that triggered THIS callback event
**REFERENCE** = Specific owner that provides calculation data (always reference object, never leader)

**Lookup Pattern:**
1. Search **owner-list** to find **notifier-entry** (which owner triggered this event?)
2. Drill down through bubbles/tags to find **reference** handles (where's the data?)

**Why This Matters:**
- Data structure uses `handle-owner` as keys (any owner at rest)
- Callback receives `obj-notifier` parameter (specific owner in motion)
- Deep leaf nodes store `handle-reference` (always data source, never leader)

**Leader Path Example:**
- User places NE bubble, clicks alignment for StaOff auto-text
- `handle-owner` = leader (we track arrowhead moves)
- `handle-reference` = alignment (we get station/offset from here)
- When leader moves: recalculate StaOff using NEW position on SAME alignment

**Direct Path Example:**
- User places StaOff bubble, clicks alignment (no leader involved)
- `handle-owner` = alignment (we track alignment changes)
- `handle-reference` = alignment (same object - we get data from what we track)
- When alignment moves: recalculate StaOff using alignment geometry

**Code was already semantically correct - documentation now crystal clear (2025-11-05).**
    (t (strcat text " " auto-value))))
```

**User Workflow:**
1. Dialog shows single text field per line
2. Tip message: "Use ``` to mark where auto-text should insert"
3. User types: `"Storm Drain ``` RT, ```"`
4. User clicks Station button → finds first ` ``` ` → `"Storm Drain STA 10+25 RT, ```"`
5. User clicks Diameter button → finds second ` ``` ` → `"Storm Drain STA 10+25 RT, Ø24"`
6. If no ` ``` ` found, append with space (graceful, non-angry fallback)

**Future Enhancements:**
- Configurable delimiter choices (```, ..., +++, etc.)
- Cursor position detection (insert at cursor instead of delimiter)
- Multiple delimiter types (user preference per project)

**Philosophy:**
- **Fail gracefully in UX** - Append if no delimiter (don't anger users)
- **Fail loudly in data** - Invalid lattribs structure = alert and exit
- **Teach quietly** - Appending shows delimiter would be better

3. **Legacy Migration**: 20+ years of customer drawings with evolving data formats
CNM is fortunate to have few if any migration challenges from the user perspective. From the programmer perspective, the code base is evolving. But we are free to improve the data formats used internally to better approach long term maintainability as long as we maintain compatibility with elements of the greater CNM application (Project Notes, Key Notes Table, and Quantity Take-off) that are outside the scope of the Reactive auto text project. This means working with the legacy block attributes and their names as they exist in customer drawings.

## 1.2. Architecture Principles

### 1.2.1. Bubble Notes Insertion and Editing Tools

#### 1.2.1.1. Data Model: lattribs (Attribute List)

**NEW ARCHITECTURE (2025-11-01): Free-Form 2-Element**
See Section 1.1.3.3.2.1.3 for full architecture details.

**CRITICAL FOR AI: NO BACKWARD COMPATIBILITY REQUIRED**
- lattribs is purely internal (users never see it)
- No migration needed (old formats deprecated, not in production)
- **FAIL LOUDLY on data schema violations** - alert and exit
- **FAIL GRACEFULLY on UX issues** - append if no delimiter (teach quietly)

**Structure:** `'(("TAG" "full-text") ...)`
- **Always 2-element lists**: `("TAG" "string")`
- **Never nil**: All values MUST be strings, use `""` for empty
- **All required tags present**: Missing tag = corruption

**Required Tags**:
- `NOTENUM` - Bubble number (user-controlled)
- `NOTEPHASE` - Construction phase (user-controlled)
- `NOTEGAP` - Spacing between text lines (system-controlled)
- `NOTETXT0` through `NOTETXT6` - User/auto text lines (free-form)

**What Users Care About** (must maintain compatibility):
- Block attribute names and values (visible in AutoCAD)
- Project Notes file format
- Key Notes Table format
- Quantity Takeoff format

**What Users DON'T Care About** (internal, change freely):
- lattribs structure (internal list representation)
- XDATA format (hidden extended data)
- Reactor implementation (internal event system)
- NOTEDATA attribute (legacy experiment, never used)
- Dialog field names (DCL implementation detail)

#### 1.2.1.2. XDATA Storage (Extended Entity Data)
**Purpose**: Store auto-generated text separately from concatenated display text
**Format**: `'(("TAG1" . "auto-value1") ("TAG2" . "auto-value2") ...)`

**Why**: Users often edit attributes directly in AutoCAD (bypassing dialog), so we need search needles to re-parse concatenated strings. See Section 1.1.3.3.2.1.2.

**Two XDATA Sections** (must coexist):
1. **AVPORT**: The number of the viewport that the user wants this bubble note to look through to see model space.
2. **VPTRANS**: Viewport transformation matrix (paper space coordinate conversion)
3. **AUTO-TEXT**: Tag-value pairs for auto-generated text

#### 1.2.1.3. Coordinate Transformation
**Problem:** AutoCAD allows many coordinate systems: The World Coordinate System (WCS) is sole and canonical. But there may also be User Coordinate Systems, Object Coordinate Systems, and paper space layout Display Coordinate Systems. Some users for some projects may place bubbles in paper space including translated/rotated/scaled DCS (represented for us in AutoCAD by OCS) or in model space with translated/rotated UCS. But for auto text, we are interested in objects that live in model space, and we are interested in world (WCS) coordinates.

**Solution:** For any paper space bubble note that references a model space object for a coordinates-based auto text, we store the associated 3-point correspondence in XDATA:
- 3 reference points in OCS (viewport space)
- 3 reference points in WCS (model space)
- Apply affine transformation on reactor updates

#### 1.2.1.4. Bubble Note Associated Viewport
**Problem:** Coordinate transformation requires us to associate a viewport with a bubble note. The user expects that when they place a bubble note in paper space, it will look through a specific viewport to see model space objects. Therefore, if we need to add a reactor to an object for a coordinates-based auto text, we need to store the bubble note's associated viewport number in XDATA so that on updates, we can correctly transform auto text coordinates from paper space to model space.

**Solution:** For any paper space bubble note that references a model space object for a coordinates-based auto text, we store the associated viewport number in XDATA:
- Viewport number in XDATA under AVPORT tag

#### 1.2.1.5. Function Naming Conventions

**Namespace Architecture:** CNM uses prefixed namespaces to organize code into bounded contexts:

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

**Why `bubbles` subsystem?** Insertion, editing, and reactors are tightly coupled (share data model, XDATA format, coordinate transforms). Keeping them under `hcnm-bubbles-*` signals this is a bounded context within CNM.

**Pattern Suffixes:**
- `*-spec` - Schema definition (pure, no side effects)
- `*-validate` - Schema validation (strict, fails loudly)
- `*-to-*` - Data transformation (pure functions)

#### 1.2.1.6. Reactor Performance Considerations

**Known Performance Considerations:**
- **XDATA read/write operations**: Frequent XDATA access on every reactor callback
- **Nested loops in handle lookup**: `hcnm-ldrblk-get-reactor-handle-for-tag` has nested loops
- **Multiple reactor callbacks**: Each leader move can trigger multiple object reactors
- **Debug output removed**: All `princ` statements removed from hot paths for production

**Optimization Suggestions (Future Work):**
1. **Reactor data reorganization**:
   - Current: `(ref (bubble (tag . auto-type)))`
   - Proposed: `(bubble (ref (tag . auto-type)))` - Bubble at higher level
   - Benefits: Foreach bubble in list, for this reference, loop tags (may reduce searches)

2. **Batch updates instead of update-inside-loop**:
   - Current: Update bubble attributes immediately in loop (cnm.lsp line ~8626)
   - Proposed: Build list of updates, then apply all at once
   - Benefits: Reduce XDATA write operations, fewer AutoCAD entity modifications

3. **Cache reactor handle lookups**:
   - If same bubble/tag queried multiple times, cache result
   - Clear cache on reactor attachment/detachment

4. **Profile operations**:
   - Measure XDATA read/write frequency and duration
   - Measure handle lookup execution time
   - Identify actual bottlenecks before optimizing

**Status**: Debug output removed (2025-11-01). Further optimization pending user feedback on production performance.

### 1.2.2. Code Style

#### 1.2.2.1. Indentation Standards
**STRICT REQUIREMENT**: All AutoLISP code uses **2-space indentation** (universal standard, no exceptions).

**Common Issues:**
- ❌ Lazy indentation: Line doesn't align with parent scope
- ❌ Inconsistent spacing: Mixing tabs and spaces
- ❌ No conventional nuances: `cond`, `setq`, etc. follow same 2-space rule

**Example (correct):**
```autolisp
(defun example (x / y)
  (setq y (* x 2))  ; 2 spaces from defun
  (cond
    ((> y 10)       ; 4 spaces from defun (2 spaces from cond)
     (alert "Big")) ; 5 spaces from defun (1 space from condition)
    (t
     (alert "Small"))))
```

**When working on any function, audit its indentation and correct to 2-space standard.**

#### 1.2.2.2. Using VS Code's AutoLISP Extension
**CRITICAL**: The Autodesk AutoLISP Extension (autodesk.autolispext) is installed in this workspace.

**HUMANS SEE ERRORS AUTOMATICALLY via syntax highlighting colors (red parentheses, etc.). YOU MUST USE `get_errors` TO ACCESS THE SAME INFORMATION.**

**This gives you an ADVANTAGE over humans - you can check errors programmatically before making changes. Use it.**

**Workflow for AutoLISP edits:**
1. **Before editing**: Run `get_errors` to check current state (MANDATORY)
2. **Make changes**: Edit the file using `replace_string_in_file`
3. **After editing**: Run `get_errors` immediately to validate (MANDATORY)
4. **Fix issues**: If errors found, fix them before proceeding

**Example:**
```
<get_errors filePaths="['c:\\path\\to\\cnm.lsp']" />
// Make edits
<get_errors filePaths="['c:\\path\\to\\cnm.lsp']" />
```

**CRITICAL - Tell the truth:**
- ❌ **NEVER say**: "File loads successfully" (you cannot test in AutoCAD)
- ✅ **INSTEAD say**: "VS Code shows no errors" or "AutoLISP extension reports no diagnostics"
- ❌ **NEVER say**: "Parentheses are balanced" (without running `get_errors`)
- ✅ **INSTEAD say**: "Running get_errors to verify..." then report results
- ❌ **NEVER claim** you tested something you didn't
- ✅ **ALWAYS be explicit** about what you verified vs. what requires human testing

**Humans struggle with truth-telling. You must be better than humans at this.**

**Note**: Even with no VS Code errors, user should still test in AutoCAD, as the extension may not catch all runtime issues.

### 1.2.3. AutoLISP Conventions
- **Lower case symbols**: Function names, variables (except legacy UPPERCASE in strings/comments)
- **Hyphen-separated**: `hcnm-bubbles-auto-alignment` (not underscore or colon)
- **Explicit local variables**: Always declare all local variables in the `/ var1 var2` parameter list. Undeclared variables become global and cause subtle bugs (AutoLISP's dynamic scoping leak).
- **No ad hoc globals**: Use `haws-config` system for persistent state. Document any global with `*asterisk-naming*` and explanation comment only if absolutely necessary.
- **Dotted pairs vs lists**: Avoid mixing dotted pairs `(cons 1 2)` and proper lists `(list 1 2)` in the same data structure. `(cdr (cons 1 2))` returns atom `2`, but `(cdr (list 1 2))` returns list `(2)`. Prefer proper lists for consistency unless dotted pairs are specifically required (e.g., association lists where the distinction is semantically meaningful).

**Code Style:**
- **Formatting**: Use AutoLISP Extension auto-formatter (VS Code command)
- **No closing comments**: Don't add comments like `;end defun` or `;end cond`
- **AI editing**: Match existing indentation exactly when making small changes
- **Deep nesting (3+ levels)**: AI has known limitation with parentheses - ask human to verify after editing
- **Always run `get_errors`** before claiming code is fixed

### 1.2.4. Documentation
- **Function headers**: Explain purpose, parameters, return values
- **Architecture comments**: Explain WHY, not just WHAT
- **Data flow**: Document transformations clearly
- **Alerts to history**: All alerts must enclose a princ. `(alert (princ "Hello world"))`

### 1.2.5. Error Handling
- **Fail loudly**: Use `(alert ...)` for schema violations
- **User-friendly**: Clear error messages with context
- **Graceful degradation**: Don't crash AutoCAD

## 1.3. Current Architecture Challenges

### 1.3.1. In Progress (feat-sunrise branch)
1. **Renaming complete**: `lattribs` functions now have clear, consistent names
2. **XDATA refactoring**: Migrating from brittle CHR(160) delimiters to search-based parsing
3. **Validator implementation**: Adding strict schema validation (this task)

### 1.3.2. Known Issues
- Some editor functions still reference CHR(160) (deprecated)
- `lattribs-split` needs proper XDATA search implementation
- Reactor cleanup logic incomplete

## 1.4. Testing Strategy

### 1.4.1. No Unit Tests Yet
AutoLISP lacks standard testing frameworks. Current approach:
- **Smoke tests**: Pure Lisp expressions to verify transforms
- **Manual testing**: Load in AutoCAD, test with real drawings
- **Incremental**: Test small changes before large refactors

### 1.4.2. Pre-Test Cleanup Command
`(c:pretest)` - Removes all bubble reactors and test bubbles (prevents orphaned reactors)

## 1.5. Related Resources

- **Standards docs**: `standards_05_architecture.md`, `standards_06_lattribs_schema.md`
- **Issue tracker**: https://github.com/hawstom/cnm/issues
- **Project**: 20+ year-old codebase, actively maintained for civil engineering customers

## 1.6. Communication Tips for AI

### 1.6.1. When I say...
- **"Use the architecture"** → Refer to lattribs data model and XDATA patterns
- **"Fail loudly"** → Strict validation with clear alerts
- **"Obsolete architecture"** → CHR(160) delimiters, old 2-element lists
- **"The reactor"** → VLR-OBJECT-REACTOR for auto-updating bubble text
- **"Command [name]"** → Always means AutoLISP command function `c:[name]`, not regular function
- **"Please note"** → Revise this copilot-instructions.md file to include that information

### 1.6.2. What helps
- Ask clarifying questions about Civil 3D API specifics
- Propose multiple options (strict vs. lenient, refactor vs. patch)
- Explain trade-offs clearly
- Show examples with real data structures

### 1.6.3. What doesn't help
- Assuming modern language features (no closures, limited list processing)
- Complex abstractions (keep it simple for AutoLISP)
- Breaking changes without migration path

## 1.7. AI Collaboration Workflow with Planning Documents

### 1.7.1. When to Use Planning Documents

**ALWAYS create a planning document for:**
- GitHub issues (use `.ai-plans/issue-{number}-{short-name}.md`)
- Features requiring multiple steps or phases
- Complex refactorings affecting multiple files
- Anything that needs human validation between steps

**DON'T create planning documents for:**
- Simple bug fixes (< 5 lines changed)
- Documentation-only changes
- Trivial formatting/style fixes
- One-shot exploratory tasks

### 1.7.2. Planning Document Workflow

#### Step 1: Create Planning Document
When starting work on a GitHub issue:
1. Copy `.ai-plans/TEMPLATE.md` to `.ai-plans/issue-{number}-{short-name}.md`
2. Fill in executive summary, phases, and tasks
3. Ask human for clarification if needed (use Questions section)
4. Wait for human approval of plan before proceeding

#### Step 2: Execute Phases
For each phase:
1. Mark phase as "in-progress" in planning doc
2. Complete all tasks for that phase
3. Update progress log with timestamps
4. Mark phase as "completed"
5. **ASK HUMAN to validate phase completion** before proceeding to next phase

#### Step 3: Keep Planning Doc Current
As you work:
- ✅ Update task checkboxes as you complete them
- ✅ Log significant progress in Progress Log section
- ✅ Document technical decisions as they're made
- ✅ Add new questions as they arise
- ✅ Update "Files Changed" lists
- ❌ Don't let planning doc get stale

#### Step 4: Handle Interruptions
If conversation is interrupted or VS Code crashes:
- Planning doc preserves all context
- Human can say "Continue working on issue-{number}"
- You pick up where you left off

#### Step 5: Feature Complete
When all phases done:
1. Complete the test plan
2. Update status to "completed"
3. Ask human to review
4. Human will attach planning doc to GitHub issue
5. Human closes issue

### 1.7.3. Planning Document Best Practices

**Phases should be:**
- Small enough to validate (30 min - 2 hours of work)
- Self-contained (can be validated independently)
- Ordered by dependencies (Phase N requires Phase N-1)

**Progress logs should:**
- Use ISO timestamps: `2025-10-31 14:30`
- Be concise but specific: "Completed lattribs-validate function" not "Made progress"
- Record human validation: "Phase 1 COMPLETE - validated by Tom"

**Questions should:**
- Be specific and actionable
- Include context (why you're asking)
- Propose options when possible
- Get marked as "Resolved" with answer and date

### 1.7.4. Multiple Concurrent Features (Advanced)

For working on multiple features simultaneously:
- Each feature gets its own planning document
- Each feature gets its own git branch
- Optional: Use git worktrees for separate VS Code instances
- Planning docs let you context-switch cleanly

### 1.7.5. Integration with Git

**Commit strategy:**
- Commit after completing each phase
- Mention phase in commit message: "feat(bubbles): Phase 2 - Add reactor callbacks (issue #X)"
- Planning doc stays in `.ai-plans/` (gitignored)
- Planning doc attached to GitHub issue when feature complete

**Branch naming:**
- `feat-{short-name}` for features
- `fix-{short-name}` for bugs
- `refactor-{short-name}` for refactorings

### 1.7.6. Summary Documents vs Planning Documents

**Planning documents** (`.ai-plans/issue-X-name.md`):
- Created at START of work
- Updated continuously during work
- Track phases, progress, questions
- Gitignored (attached to issue when done)

**Summary documents** (optional, in `docs/`):
- Created at END of work if requested
- Explain completed work for future reference
- Go into version control
- Examples: architecture decisions, region reorganization summaries

**Default behavior**: Create planning docs for GitHub issues. Only create summary docs if explicitly requested.

---

## 1.8. Document Length and Structure Guidelines (For AI Performance)

**See Section 1.8 for details on effective document structure.**

### 1.8.1. Context Window and Attention

**Technical limits:**
- AI context window: ~1 million tokens (~750,000 words)
- Single request context: ~200,000 tokens
- This file (copilot-instructions.md): ~15,000 tokens

**Attention behavior:**
- **HIGH attention**: Start of documents, end of documents, recent conversation
- **LOWER attention**: Middle of long documents
- **HIGHEST attention**: Recent messages in current conversation

### 1.8.2. Structure Matters More Than Length

**Good structure (easy for AI):**
- ✅ Table of contents with section numbers
- ✅ Clear hierarchical headings (##, ###, ####)
- ✅ Executive summaries at document top
- ✅ Examples showing "good vs bad"
- ✅ Bullet points and short paragraphs
- ✅ Code blocks with syntax highlighting

**Poor structure (hard for AI):**
- ❌ Wall of text without breaks
- ❌ No headings or organization
- ❌ Important info buried in middle
- ❌ No examples or concrete illustrations

### 1.8.3. Optimal Document Lengths

**Single-purpose documents**: 100-500 lines
- Examples: issue-template.md, lattribs-schema.md
- Perfect length for quick reference

**Reference documents**: 500-2000 lines
- Examples: copilot-instructions.md, API documentation
- Still effective with good structure (like this file)

**Over 2000 lines**: Consider splitting
- Use multiple focused documents
- Link between them
- Or ensure exceptional organization

### 1.8.4. Writing for AI Readers

**Key principles:**

1. **Put critical info at top or bottom**
   - Executive summary at start
   - Quick reference at end
   - Don't bury essentials in middle

2. **Use section references**
   - "See Section 1.2.3" helps AI locate info
   - Better than "see above" or "as mentioned earlier"

3. **Repeat critical constraints**
   - If something must NOT break, say it multiple times
   - In executive summary, in relevant sections, in examples

4. **Provide concrete examples**
   - Show actual code, not just descriptions
   - Real examples from codebase (like Issue #11)
   - "Good vs Bad" comparisons

5. **Update as you learn**
   - This document evolves
   - Add lessons learned
   - Remove obsolete information

### 1.8.5. This Document's Structure (Meta)

This file (copilot-instructions.md) follows its own advice:

- **Quick Start** at top: Most important info in first 20 lines
- **Section numbers**: Easy to reference (1.1, 1.2.3, etc.)
- **Examples throughout**: Real code and scenarios
- **Meta-section at end**: Document about the document (you're reading it)
haws-ti