HAWSEDC STANDARDS VOLUME 05: ARCHITECTURE
==========================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-10-25  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 Key Patterns
### 1.1.1 General
- **Central Core Library Functions:** edclib.lsp
- **Config Storage:** Upcoming project to abstract HCNM_CONFIG (see this section) to HAWS_CONFIG. Meanwhile, HAWS-READCFG/HAWS-WRITECFG (AutoCAD getcfg/setcfg) not file I/O.

**Note:** S04 (Organization and Locations) answers "WHERE is everything?" while S05 (Architecture) answers "HOW does it work internally?"

### 1.1.2 CNM General
- **Config Storage:** Complex system HCNM_CONFIG with multiple storage scope options. Migrating it to a new file and system HAWS_CONFIG.
  - Session: AutoLISP global
  - Drawing: Not programmed
  - User: getcfg/setcfg
  - Project: ini file per folder or pointer file to other folder
  - App: ini file in app folder copied to project folder
  - Fallback and refresh: Falls back to hard-coded defaults as necessary. HCNM_PROJINIT refreshes project scope variables any time user may have edited the project ini file. Many calls with ini reads could be avoided if we disclaimed user edits to the ini file. Point of history: a third-party developer received permission to edit our ini file for his custom Project Notes Editor, devsource\cnmedit.exe. He now depreciates that editor, and he suggests that we replace it with our own editor, so that could be a little upcoming project.
- **Project Notes:** There have been three versions of project notes file formatting. READCF, READCFTXT2, and READCFCSV. READCFCSV is current and is used for spreadsheet editing and the third party custom editor (see previous point). READCFTXT3 was never released. READCFTXT was the original pretty, but clunky for editing, file format edited directly without automatic line wrapping.
- **Key Notes Table:** CNM reads line by line from a Project Notes file and makes a Key Notes Table of the project notes that are found plus their total quantities for the sheet. When the a Key Notes Table is created for a sheet, a .not file is created with the notes and the quantities for that sheet.
- **Quantity Take-off (aka Tally):** Using the .not files created by Key Notes Table, CNM Quantity Take-off makes a total quantities list in the drawing and saves a sheet-by-sheet breakdown to a csv file.
- **Reactor System:** ONE VLR-OBJECT-REACTOR for all bubble notes (not one per bubble)
### 1.1.3 CNM Bubble Notes
- **Reactor System:** ONE VLR-OBJECT-REACTOR for all bubble notes (not one per bubble)
- **Delimiter System:** CHR 160 (non-breaking space) separates PREFIX§AUTO§POSTFIX
- **System-Controlled Attributes:** NOTENUM, NOTEPHASE, NOTEGAP always go to PREFIX field

### 1.1.4 HAWS_QT
- **Serves CNM:** Provides "third party" (in another file) length and area quantities to CNM Bubble Notes
- **Object-oriented-like:** Uses innovative "object-oriented" approach to serve "instances" of its functions so multiple apps can each have their own settings. Calling apps declare their "app" name when they "instantiate a new QT object"


---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume documents the core architectural patterns and systems used in HAWSEDC.

## 2.2 Scope
[AI: Content about overall system architecture]

---
<!-- #endregion -->

<!-- #region 3. CONFIGURATION SYSTEM -->
# 3. CONFIGURATION SYSTEM

## 3.1 HCNM_CONFIG (Current System)

### 3.1.1 Overview
CNM uses a multi-scope configuration system for managing hundreds of settings.

### 3.1.2 Scopes
Configuration values exist at different scope levels with fallback chain:

1. **Session (0):** In-memory AutoLISP global, lost on drawing close
2. **Drawing (1):** Not currently programmed
3. **Project (2):** INI file per project folder
4. **App (3):** INI file in app folder, copied to project
5. **User (4):** AutoCAD's getcfg/setcfg registry

**Fallback:** Session → INI file → Hard-coded defaults

### 3.1.3 Key Functions
```lisp
C:HCNM-CONFIG-GETVAR  ; Retrieves value with scope awareness (uses hyphens - current code)
C:HCNM-CONFIG-SETVAR  ; Saves value to appropriate scope (uses hyphens - current code)
HCNM_CONFIG_DEFINITIONS ; Centralized schema (400+ settings)
```

[AI: Note - These command functions currently use hyphens. Will be renamed to C:HCNM_CONFIG_GETVAR and C:HCNM_CONFIG_SETVAR per S03.4.1 during refactoring]

### 3.1.4 Global Cache
`*HCNM_CONFIG*` - Association list caching all settings in session

### 3.1.5 Project Refresh
`HCNM_PROJINIT` - Refreshes project scope variables after user edits INI file

Historical note: Third-party developer received permission to edit INI files for custom Project Notes Editor (devsource\cnmedit.exe). Developer now deprecates that editor and suggests we replace it with our own.

## 3.2 HAWS_CONFIG (Planned Refactoring)

### 3.2.1 Goal
Extract CNM's config system into reusable HAWS_CONFIG library for all HawsEDC apps.

**Status:** IN-DEV - Planned on haws-config branch

### 3.2.2 Multi-App Design
```lisp
;; App registration
(HAWS_CONFIG_REGISTER_APP "CNM" (HCNM_CONFIG_DEFINITIONS))
(HAWS_CONFIG_REGISTER_APP "HAWS_QT" (HAWSQT_CONFIG_DEFINITIONS))

;; Each app gets its own INI per scope
;; User:    %APPDATA%/HawsEDC/CNM-user.ini
;; Project: C:/Projects/ABC/CNM-project.ini
```

### 3.2.3 Enhanced Features
- **Type system:** Explicit integer/boolean/string declarations
- **Validation:** Register validators per setting
- **Change notifications:** Callbacks when config changes
- **Import/Export:** Templates for project defaults

### 3.2.4 Backward Compatibility
CNM wrapper functions maintain compatibility:
```lisp
;; OLD function (still works, uses hyphens - current code)
(C:HCNM-CONFIG-GETVAR "BubbleTextPrefixSta")

;; NEW function (uses underscores - planned refactoring)
(HAWS_CONFIG_GETVAR "CNM" "BubbleTextPrefixSta")
```

### 3.2.5 Benefits
- Code reuse across HawsEDC apps
- Consistent UX
- Standalone library for third parties
- Cleaner CNM code (smaller file)

**Full refactoring plan:** See `devtools/docs/haws_config_refactoring_reference.md`

---
<!-- #endregion -->

<!-- #region 4. CNM BUBBLE NOTES -->
# 4. CNM BUBBLE NOTES

## 4.1 Delimiter System

### 4.1.1 Structure
Every bubble attribute value has three parts:
- **PREFIX:** User-controlled text (appears before auto-text)
- **AUTO:** System-generated text (pipe diameter, station, etc.)
- **POSTFIX:** User-controlled text (appears after auto-text)

Delimiter: CHR 160 (non-breaking space) - `§` used in documentation

### 4.1.2 Storage Format
`"PREFIX§AUTO§POSTFIX"`

Examples:
- `"STA §100+25.50§ RT"` - Station with prefix/postfix
- `"Ø§24§\"` - Pipe diameter
- `"NOTE 3§§"` - Manual note (no auto-text)

### 4.1.3 Key Functions
```lisp
HCNM_EB_SPLIT_ON_NBSP     ; Splits "A§B§C" → (LIST "A" "B" "C")
HCNM_EB_CONCAT_PARTS      ; Joins (LIST "A" "B" "C") → "A§B§C"
HCNM_EB_EXPAND_VALUE_TO_DELIMITED ; Converts old single-value to delimited
```

### 4.1.4 System-Controlled Attributes
Three attributes ALWAYS put values in PREFIX (never AUTO):
- **NOTENUM:** Bubble number
- **NOTEPHASE:** Phase indicator
- **NOTEGAP:** Gap identifier

**Pros and Cons:**
- **Pro:** Users expect full control over numbering/organizing fields
- **Pro:** These are metadata, not data from Civil 3D objects
- **Con:** Creates mixed paradigm (some attributes auto, some not)

## 4.2 Reactor System

### 4.2.1 Design Principle
**FIRM:** ONE VLR-OBJECT-REACTOR for all bubbles (not one per bubble)

### 4.2.2 Pros and Cons
**Pros (single reactor):**
- Performance: Avoids memory bloat from thousands of reactors
- Cleanup: Single reactor to manage, not thousands
- Reliability: Less chance of orphaned reactors

**Cons (single reactor):**
- Slightly more complex callback logic (must find relevant bubbles)
- All bubbles share same callback function

### 4.2.3 Implementation
One reactor watches ALL Civil 3D objects (pipes, alignments, surfaces). When object changes, reactor:
1. Finds all bubbles linked to that object
2. Updates each bubble's auto-text
3. Refreshes display

### 4.2.4 Attaching Reactors
```lisp
HCNM_LDRBLK_ASSURE_AUTO_TEXT_HAS_REACTOR
```
- Checks if reactor exists
- Creates if missing
- Never creates duplicate reactors

### 4.2.5 Reactor Proliferation Bug (Fixed)
**Historical Issue:** Early implementation created one reactor per bubble, leading to thousands of reactors and performance degradation.

**Fix:** Centralized reactor system with cleanup on CNM load.

## 4.3 Auto-Text Sources

### 4.3.1 Civil 3D Pipes
Functions: `HCNM_LDRBLK_AUTO_PIPE*`

Properties extracted:
- **Diameter:** Inner diameter or width
- **Slope:** Calculated from pipe geometry
- **Length:** Pipe length

### 4.3.2 Civil 3D Alignments
Functions: `HCNM_LDRBLK_AUTO_ALIGN*`

Properties extracted:
- **Station:** Point location along alignment
- **Offset:** Perpendicular distance from alignment

### 4.3.3 Civil 3D Surfaces
Functions: `HCNM_LDRBLK_AUTO_SURF*`

Properties extracted:
- **Elevation:** Surface elevation at point

### 4.3.4 HAWS_QT Integration
Functions: `HCNM_LDRBLK_AUTO_QT*`

Uses HAWS_QT library to extract:
- **Length:** Polyline/line length
- **Area:** Closed polyline area

[AI: HAWS_QT object-oriented design documented in S05.1.1.4]

## 4.4 Edit Bubble Dialog

### 4.4.1 DCL File
Location: `devsource/cnm.dcl`

### 4.4.2 State Management
Global: `HCNM_EB_ATTRIBUTE_LIST`

Stores current bubble attributes for dialog callbacks.

### 4.4.3 Delimiter Handling
Dialog splits/joins delimiter values transparently. User sees three text boxes (PREFIX, AUTO, POSTFIX) but file stores single delimited string.

---
<!-- #endregion -->

<!-- #region 5. CNM PROJECT NOTES -->
# 5. CNM PROJECT NOTES

## 5.1 File Formats (Evolution)

### 5.1.1 Version 1: READCF (Original)
Pretty format with manual line wrapping. Clunky to edit directly.

**Status:** DEPRECATED

### 5.1.2 Version 2: READCFTXT2
Improved text format.

**Status:** DEPRECATED (never released publicly)

### 5.1.3 Version 3: READCFCSV (Current)
**FIRM:** CSV format for spreadsheet editing and third-party editor compatibility.

Location: Project folder, typically `constnot-default.csv`

Format:
```csv
NOTENUM,NOTETEXT,NOTEPHASE,NOTEGAP
1,Existing inlet to remain,A,
2,Remove and replace inlet,B,
```

### 5.1.4 Migration
Old formats automatically converted on first load.

## 5.2 Key Notes Table

### 5.2.1 Purpose
Reads project notes CSV and creates drawing table showing:
- Note numbers and text
- Quantities for current sheet only

### 5.2.2 .not Files
When Key Notes Table created, CNM saves `.not` file with:
- Notes found on sheet
- Quantities per note

Example: `Sheet-C-01.not`

## 5.3 Quantity Take-off (Tally)

### 5.3.1 Purpose
Aggregates quantities across all sheets.

### 5.3.2 Process
1. Read all `.not` files in project
2. Sum quantities per note
3. Create drawing table with totals
4. Save CSV with sheet-by-sheet breakdown

### 5.3.3 Output
CSV file: `project-quantity-takeoff.csv`

Format:
```csv
NOTENUM,NOTETEXT,SHEET1,SHEET2,TOTAL
1,Existing inlet,5,3,8
2,Replace inlet,2,1,3
```

---
<!-- #endregion -->

<!-- #region 6. TIP SYSTEM -->
# 6. TIP SYSTEM

## 6.1 Purpose
Show helpful tips to users, with "Don't show again" option.

## 6.2 Storage
**FIRM:** Uses AutoCAD getcfg/setcfg (not file I/O)

Location: `%APPDATA%/Autodesk/AutoCAD/[version]/HawsEDC/TipsHidden`

Format: `"(TIP1 TIP2 TIP3)"` - List of hidden tip IDs

## 6.3 Functions
```lisp
HAWS_TIP_HIDE_LIST       ; Read list of hidden tips
HAWS_TIP_SAVE_HIDE_LIST  ; Save list of hidden tips
```

## 6.4 Historical Migration
Old system used file: `haws-tip-hide.lsp`

**Status:** DEPRECATED - Migrated to getcfg/setcfg for reliability (no file permissions issues)

---
<!-- #endregion -->

<!-- #region 7. NOTES -->
# 7. NOTES

## 7.1 Future Architecture Topics

### 7.1.1 To Document
[AI: Content to add in future updates]
- Detailed reactor callback flow
- DCL dialog architecture patterns
- Error handling strategies
- Undo handling in bubble edits
- Space switching (model ↔ paper) patterns

---
<!-- #endregion -->

**End of Document**
