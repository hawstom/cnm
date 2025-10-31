# 1. GitHub Copilot Instructions for CNM Project

## 1.1. Project Context

**CNM (Construction Notes Manager)** is an AutoCAD add-on written in AutoLISP that helps civil engineers manage construction notes on drawings using "bubble notes" (leader annotations with dynamic text).

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

### 1.1.3. Bubble Notes

#### 1.1.3.1. Minimum Definition of a Bubble Note
Bubble notes are simply attributed blocks with one twist: The shape is a dynamic selector so that you can change the shape without making a new insertion and copying information.

You can create your own bubble note blocks as long as they have 
- A dynamic block "Shape" property with values that match the types defined in CNM Project settings. CNM comes with 8 shapes: BOX, CIR, DIA, ELL, HEX, OCT, PEN, REC, SST.
- The required attributes: NOTENUM, NOTEPHASE, NOTEGAP, NOTEDATA, NOTETXT0 through NOTETXT6.
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

##### 1.1.3.3.2. Functionality Under Development
- Reactive auto text. Associate auto text with its reference object where appropriate. Update auto text as appropriate when reference object or bubble arrowhead point changes. Any auto text in bubble notes stays synchronized with source objects and follows their own leader arrowheads via VLR reactors (AutoLISP object event listeners).

###### 1.1.3.3.2.1. Key Technical Challenges
1. **Paper Space Complexity**: Bubbles in paper space viewports require coordinate transformation from paper space DCS (represented by paper space object OCS) to model space WCS.
2. **Free-form User Edits**: Users can edit bubble note attributes directly in AutoCAD, bypassing CNM dialogs. Need robust parsing to separate auto-generated text from user edits.
- Users can reasonably expect:
        - that any text they add remains intact while auto text updates correctly.
        - that if they add text around auto text, it remains intact while auto text updates correctly.
        - that if they change CNM Project settings that affect auto text format, the next update reflects those changes as long as the auto text itself remains uncorrupted and thus identical to what we store for our search and replace on update.
        - that if they completely delete (or fat-finger-corrupt) auto text or change its format (eg. adding prefixes/suffixes), it does not get acted on or restored at the next update.
        - that they can't have multiple auto text fields with identical values in the same bubble note line and have them all update correctly.
2. **Data Persistence**: Auto-generated text must be stored separately (in XDATA) from user-editable prefix/postfix text
We provide users free-form editing capabilities, so we must store auto-generated text in a way that allows us to reliably identify and update it without overwriting user edits. This is achieved by storing verbatim auto text in bubble note XDATA and doing search-and-replace in the complete attribute text on updates.
3. **Legacy Migration**: 20+ years of customer drawings with evolving data formats
CNM is fortunate to have few if any migration challenges from the user perspective. From the programmer perspective, the code base is evolving. But we are free to improve the data formats used internally to better approach long term maintainability as long as we maintain compatibility with elements of the greater CNM application (Project Notes, Key Notes Table, and Quantity Take-off) that are outside the scope of the Reactive auto text project. This means working with the legacy block attributes and their names as they exist in customer drawings.

## 1.2. Architecture Principles

### 1.2.1. Bubble Notes Insertion and Editing Tools

#### 1.2.1.1. Data Model: lattribs (Attribute List)
**Structure**: `'(("TAG" "prefix" "auto" "postfix") ...)`
- **Always complete**: All required tags must be present (no partial structures)
- **4-element lists**: Even if auto/postfix empty, keep structure: `("TAG" "value" "" "")`
- **Validation**: Fail loudly on schema violations (strict validator)

**Required Tags**:
- `NOTENUM` - Bubble number (user-controlled)
- `NOTEPHASE` - Construction phase (user-controlled)
- `NOTEGAP` - Spacing between text lines (system-controlled)
- `NOTEDATA` - Reference object metadata (system-controlled)
- `NOTETXT0` through `NOTETXT6` - User/auto text lines

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
- `hcnm-ldrblk-*` - Leader block (bubble note) functions
- `hcnm-config-*` - Configuration/settings functions
- `hcnm-ldrblk-eb-*` - Edit box (dialog) functions [TGH 2025-10-31 01:27:47: I think this could be hcnm-lb-eb-*]
- `*-spec` - Schema definition (pure, no side effects)
- `*-validate` - Schema validation (strict, fails loudly)
- `*-to-*` - Data transformation (pure functions)

### 1.2.2. Code Style

### 1.2.3. AutoLISP Conventions
- **Lower case symbols**: Function names, variables (except legacy UPPERCASE in strings/comments)
- **Hyphen-separated**: `hcnm-ldrblk-auto-alignment` (not underscore)
- **Explicit parameters**: Don't rely on dynamic scoping [TGH 2025-10-31 01:27:47: What do we mean by this?]
- **Local variables**: Always declare all variables in `/ local1 local2` list unless we have a very good reason not to and we document that reason clearly both with an explanation comment and *global-variable* style. But this is not authorized since we have the hcnm-config system available to us. No ad hoc global variables.

### 1.2.4. Documentation
- **Function headers**: Explain purpose, parameters, return values
- **Architecture comments**: Explain WHY, not just WHAT
- **Data flow**: Document transformations clearly

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

### 1.6.2. What helps
- Ask clarifying questions about Civil 3D API specifics
- Propose multiple options (strict vs. lenient, refactor vs. patch)
- Explain trade-offs clearly
- Show examples with real data structures

### 1.6.3. What doesn't help
- Assuming modern language features (no closures, limited list processing)
- Complex abstractions (keep it simple for AutoLISP)
- Breaking changes without migration path
