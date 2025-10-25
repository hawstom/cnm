HAWSEDC STANDARDS VOLUME 04: ORGANIZATION AND LOCATIONS
========================================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-10-25  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 Key Rules
- **devsource/**: Production code loaded by users
- **devtools/**: Developer-only files (FAS compiler, documentation)
- **devmenu/**: Menu resources (RC files, icons)
- **sandbox/**: Experimental code and tests

---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume answers all "where" questions: Where does code live? Where are functions arranged within files? Where are install files, distribution scripts, command references, autoloaders, and command aliases? How do we decide when to create, split, or merge files? In short: how does all this work and where is everything?

## 2.2 Scope
This volume covers:
- **Directory structure:** devsource/, devtools/, devmenu/, sandbox/, retired/, compile/
- **File organization:** How functions are arranged within files (public API first, then helpers)
- **Function location:** Where to put new functions (same file, new file, or shared library)
- **File splitting criteria:** When to split large files into smaller modules
- **File merging criteria:** When to consolidate small files
- **Build and distribution:** Where install files, distrib scripts, and compiled outputs live
- **Command infrastructure:** Where autoloaders, command aliases, and menu definitions live
- **Documentation location:** Where README files, API references, and standards live

**What S04 does NOT cover:**
- **How things work internally** (that's S05 Architecture)
- **Naming conventions** (that's S03 Names and Symbols)

---
<!-- #endregion -->

<!-- #region 3. DIRECTORY STRUCTURE -->
# 3. DIRECTORY STRUCTURE

## 3.1 Top Level Folders

### 3.1.1 devsource
**Production code loaded by end users**

Contains:
- All .lsp source files
- All .dcl dialog files
- Block definition .dwg files
- Support files (.lin, .csv, .ini)
- cnmloader.lsp (main entry point)

**FIRM:** This folder gets distributed to users. Nothing development-specific belongs here.

### 3.1.2 devtools
**Developer-only files**

Contains:
- FAS compiler (haws_dev_rebuild_fas.lsp)
- Distribution scripts
- Developer documentation (this standards series)
- cnmdevloader.lsp (dev mode entry point)

**FIRM:** Nothing in this folder is distributed to users.

### 3.1.3 devmenu
**Menu resources**

Contains:
- RC files (menu definitions)
- Icons and graphics
- CUIX files (ribbon interface)
- VS-dll folder (compiled menu resources)

### 3.1.4 sandbox
**Experimental code and tests**

Contains:
- Proof-of-concept code
- Testing scripts
- Throwaway experiments

**FIRM:** Nothing here affects production. Feel free to experiment.

### 3.1.5 retired
**Deprecated code kept for reference**

Contains:
- Old implementations replaced by better versions
- Historical code that may inform future decisions
- Not loaded, not distributed

## 3.2 Key Files

### 3.2.1 cnmloader.lsp
**Main entry point for users**

Location: `devsource/cnmloader.lsp`

Purpose:
- Detects if developer mode is available
- Loads cnmdevloader.lsp if found
- Otherwise loads production files
- Creates C:CNM command

### 3.2.2 cnmdevloader.lsp
**Developer mode entry point**

Location: `devtools/cnmdevloader.lsp`

Purpose:
- Prints `*** HawsEDC Developer Mode Active ***`
- Loads source .lsp files (not .fas)
- Enables developer commands (HAWS-REBUILD-FAS)

### 3.2.3 cnm.lsp
**Main CNM application**

Location: `devsource/cnm.lsp`

Size: ~8600 lines

Contains:
- Bubble note system
- Project notes management
- Configuration system (HCNM_CONFIG)
- Edit bubble dialog
- Reactor system

[AI: This file should be split (see S06 Roadmap)]

### 3.2.4 edclib.lsp
**Central core library**

Location: `devsource/edclib.lsp`

Contains:
- Shared utility functions
- HAWS-READCFG/HAWS-WRITECFG
- String manipulation
- Math helpers
- Drawing utilities

---
<!-- #endregion -->

<!-- #region 4. CODE ORGANIZATION -->
# 4. CODE ORGANIZATION

## 4.1 Within Files

### 4.1.1 Standard Structure
**FIRM:** Command functions and public API first, then private helpers

**Schedule:** Moderately conservative (apply to new files, reorganize during major refactoring)

```lisp
;;==============================================================================
;; PUBLIC API - Command Functions and Public Helpers
;;==============================================================================
;; User commands (C:*) or functions called by other modules

(DEFUN C:CNM ...)              ; User command
(DEFUN C:CNMEDIT ...)          ; User command

(DEFUN HCNM_EDIT_BUBBLE ...)   ; Public API function

;;==============================================================================
;; PRIVATE HELPERS - Pipe Network
;;==============================================================================
;; Implementation details, alphabetically ordered

(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_DIAMETER ...)
(DEFUN HCNM_LDRBLK_AUTO_PIPE_FORMAT_LENGTH ...)
(DEFUN HCNM_LDRBLK_AUTO_PIPE_GET_OBJECT ...)
```

### 4.1.2 Pros and Cons
**Pros:**
- User-facing functions are easy to find at top of file
- Public API clearly separated from implementation
- Alphabetical order within sections aids navigation
- VS Code regions fold sections cleanly

**Cons:**
- Requires discipline to maintain organization
- May need refactoring when adding new functions

### 4.1.3 File Splitting
**PLANNED:** Split long files when practical

**Schedule:** Very conservative (only when file becomes unmaintainable or during major refactoring)

Current state: cnm.lsp is ~8600 lines. Consider splitting into:
- cnm_core.lsp (commands and main logic)
- cnm_bubble.lsp (bubble note system)
- cnm_config.lsp (configuration management)
- cnm_reactor.lsp (reactor system)

## 4.2 Function Headers

### 4.2.1 Status
**NOT ADOPTED:** Formal structured headers for all public functions

### 4.2.2 Template (If Adopted)
```lisp
;;==============================================================================
;; FUNCTION_NAME
;;==============================================================================
;; Purpose:
;;   One-sentence summary of what function does.
;;
;; Arguments:
;;   ARG1 - Description (required/optional) [type if helpful]
;;
;; Returns:
;;   Description of return value and type
;;
;; Side Effects:
;;   - User prompts, file I/O, state changes
;;   - Reactor attachments
;;   - Space switching
;;
;; Related:
;;   OTHER_FUNCTION_1
;;==============================================================================
```

### 4.2.3 When to Use (If Adopted)
- All public API functions (called by other modules)
- Complex private functions (>30 lines)
- Optional for simple helpers (use judgment)

---
<!-- #endregion -->

<!-- #region 5. LOADING AND COMPILATION -->
# 5. LOADING AND COMPILATION

## 5.1 Load Order

### 5.1.1 Standard Sequence
1. cnmloader.lsp (entry point)
2. Check for cnmdevloader.lsp
3. Load edclib.lsp (core utilities)
4. Load module files (cnm.lsp, haws-qt.lsp, etc.)
5. Load support files (cnmalias.lsp, etc.)

### 5.1.2 Dependencies
**FIRM:** edclib.lsp must load before any module that uses its functions

Common dependencies:
- HAWS-READCFG / HAWS-WRITECFG (in edclib.lsp)
- String utilities (in edclib.lsp)
- Math helpers (in edclib.lsp)

## 5.2 FAS Compilation

### 5.2.1 Purpose
- Faster loading
- Better run-time performance
- Reduced memory usage
- Some IP protection (compiled bytecode)

### 5.2.2 What to Compile
**FIRM:** Compile all production .lsp files EXCEPT loaders

Compile:
- cnm.lsp → cnm.fas
- edclib.lsp → edclib.fas
- haws-qt.lsp → haws-qt.fas
- All module files

Do NOT compile:
- cnmloader.lsp (needs to detect dev mode)
- cnmdevloader.lsp (developer tool)
- Install scripts

### 5.2.3 Dynamic File List
**PLANNED:** FAS compiler should scan devsource/ and compile all .lsp files except exclusions

Exclusion list:
- *loader.lsp
- CNM-Install.lsp
- Sandbox files

---
<!-- #endregion -->

<!-- #region 6. DISTRIBUTION -->
# 6. DISTRIBUTION

## 6.1 Current Process

### 6.1.1 Batch File
Location: `C:\TGHFiles\programming\hawsedc\compile\distrib.bat`

Purpose: Copy files to distribution folder

**Additional documentation:** See `C:\TGHFiles\programming\hawsedc\compile\README.TXT` for detailed compile setup steps.

### 6.1.2 Files to Distribute
From devsource/:
- All .fas files (compiled from .lsp)
- All .lsp files (as source backup)
- All .dcl files
- All .dwg block files
- Support files (.lin, .csv, .ini)
- cnmloader.lsp

From devmenu/:
- Compiled menu resources
- Icons

Do NOT distribute:
- devtools/ folder
- sandbox/ folder
- retired/ folder
- Development documentation

## 6.2 Planned Improvements

### 6.2.1 HAWS-DISTRIB Command
**PLANNED:** Replace batch file with AutoLISP command

Benefits:
- Cross-platform (not Windows-only)
- Integrated with development workflow
- Can read exclusion lists from standards
- Automatic version numbering

---
<!-- #endregion -->

<!-- #region 7. DEVELOPMENT WORKFLOW -->
# 7. DEVELOPMENT WORKFLOW

## 7.1 Developer Mode Setup

### 7.1.1 Enable Developer Mode
1. Add devtools/ to AutoCAD Support File Search Path
2. Restart drawing session or reload CNM
3. Verify: Should see `*** HawsEDC Developer Mode Active ***` in command line

### 7.1.2 Benefits
- Loads source .lsp files (not compiled .fas)
- Easier debugging (line numbers match source)
- Developer commands available (HAWS-REBUILD-FAS)
- Changes take effect immediately on reload

## 7.2 Making Changes

### 7.2.1 Edit-Test Cycle
1. Edit .lsp file in VS Code
2. Save file
3. In AutoCAD: `(load "cnmloader")` to reload
4. Test changes
5. Repeat

### 7.2.2 Before Committing
1. Read relevant standards volumes (S02.5.1.1)
2. Follow naming conventions (S03)
3. Update function headers if changed
4. Test in both dev mode and production mode
5. Run HAWS-REBUILD-FAS to verify compilation

## 7.3 Git Workflow

### 7.3.1 Branch Strategy
- `master` - stable production code
- `haws-config` - HAWS_CONFIG refactoring
- Feature branches as needed

### 7.3.2 Commit Messages
Format: `type: brief description`

Types:
- `feat:` - New feature
- `fix:` - Bug fix
- `refactor:` - Code restructuring
- `docs:` - Documentation only
- `style:` - Formatting (S03 compliance)
- `test:` - Adding tests

Example: `refactor: rename HCNM-BUBBLE to HCNM_BUBBLE per S03.4.1`

---
<!-- #endregion -->

<!-- #region 8. NOTES -->
# 8. NOTES

## 8.1 Future Organization Tasks

### 8.1.1 File Splitting
Consider splitting cnm.lsp (~8600 lines) into focused modules. Balance maintainability against load complexity.

### 8.1.2 Documentation Location
**FIRM:** All standards documentation lives in devtools/docs/

**FIRM:** Do not create separate README files for each module. Reference standards volumes instead.

### 8.1.3 Autoloading Strategy
[AI: TODO - Document how command aliases and autoloading work. See cnmalias.lsp and cnmaliaslib.lsp]

---
<!-- #endregion -->

**End of Document**
