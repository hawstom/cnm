HAWSEDC STANDARDS VOLUME 06: ROADMAP
=====================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-10-25  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 Current Priorities
1. **Fix CNM Edit Bubble dialog chr(160) parsing bug** (IN-PROGRESS 2025-10-25)
2. **Complete standards volumes** (this document series)
3. **Clean up develop/ directory** (delete redundant files)
4. **Implement developer tools** (FAS compiler, dev mode loader)
5. **Refactor HAWS_CONFIG** (migrate INI files to getcfg/setcfg)

---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume tracks planned features, refactoring initiatives, and project milestones.

## 2.2 Scope
[AI: Content about project planning and evolution]

---
<!-- #endregion -->

<!-- #region 2.5. COMPLIANCE DASHBOARD -->
# 2.5 COMPLIANCE DASHBOARD

## 2.5.1 Very Aggressive Standards (Highest Priority)
| Standard | Description | Compliance | Next Action |
|----------|-------------|------------|-------------|
| S03.3.1.1 | UPPERCASE symbols | ~95% | Low priority - mostly done |
| S03.11.2 | No blank lines in code | ~5% | **HIGH PRIORITY** - Systematic cleanup needed |

## 2.5.2 Moderately Aggressive Standards
| Standard | Description | Compliance | Next Action |
|----------|-------------|------------|-------------|
| S03.4.1.1 | Hyphens (migrated from underscores) | ~100% | Migration should be complete |
| S03.5.1.1 | Type prefixes (OBJ_, EN_, _P) | ~60% | Apply to new code, refactor old during edits |
| S02.4.1.1 | [INITIALS:] comments | ~95% | Already adopted |

## 2.5.3 Moderately Conservative Standards
| Standard | Description | Compliance | Next Action |
|----------|-------------|------------|-------------|
| S03.6.1.1 | Name length (clarity over brevity) | ~70% | Apply judgment during refactoring |
| S03.7.1.1 | Function naming VERB_NOUN | ~80% | Well-established, rename during refactoring |
| S04.4.1.1 | File organization (public API first) | ~40% | Reorganize during major refactoring |

---
<!-- #endregion -->

<!-- #region 3. CURRENT WORK -->
# 3. CURRENT WORK

## 3.1 In Progress

### 3.1.1 CNM Edit Bubble Dialog Chr(160) Parsing Bug
**Status:** IN-PROGRESS (2025-10-25)

**Symptom:** Auto text (LF, SF) appears in prefix field in edit dialog, cannot be removed with "_" button

**Root Causes Identified:**
1. `hcnm_eb:concat_parts` conditionally omitting chr(160) delimiters
2. `hcnm_ldrblk_adjust_format` mixing parsing logic with formatting logic
3. Lack of field structure validation (ensuring exactly two chr(160) in all attributes)

**Refactoring Completed:**
- ✅ `hcnm_eb:concat_parts` - Always includes delimiters (minimum "§§")
- ✅ `hcnm_ldrblk_adjust_format` - Simplified to single responsibility (format codes only)
- ✅ `hcnm_ldrblk_adjust_formats` - Separated parsing from formatting
- ✅ `hcnm_ldrblk_ensure_fields` - New validation/normalization layer
- ✅ Documentation - Added comprehensive DATA FLOW comments

**Testing Needed:**
- ⚠️ Create bubble with LF/SF auto text
- ⚠️ Edit in dialog
- ⚠️ Verify auto text stays in auto field (not migrating to prefix)
- ⚠️ Verify "_" button removes auto text properly

**Design Principles Applied:**
- Single responsibility (one function = one job)
- Separation of concerns (parsing ≠ formatting)
- Always include delimiters for consistent structure
- Document data flow clearly

### 3.1.2 Standards Documentation
**Status:** IN-DEV

**Tasks:**
- ✅ Create multi-volume standards system (S01-S06)
- ✅ Migrate content from old haws-code-style-guidelines.md
- ✅ Update source file references to new standards
- ✅ Delete deprecated documentation files (HAWSEDC_DEVELOPER_GUIDE.md, refactoring-suggestions.md)
- ✅ Populate Volume 06 (this document)

### 3.1.3 HAWS_CONFIG Refactoring
**Status:** PAUSED (waiting for bug fix) - Branch: haws-config

**Goal:** Extract CNM's config system into reusable HAWS_CONFIG library for all HawsEDC apps

**Tasks:**
- ⚠️ Create haws-config.lsp skeleton
- ⚠️ Extract scope handler functions from cnm.lsp
- ⚠️ Add app-awareness to global cache
- ⚠️ Implement app registration system (HAWS_CONFIG_REGISTER_APP)
- ⚠️ Create CNM wrapper functions for backward compatibility
- ⚠️ Test with CNM (verify no behavior changes)
- ⚠️ Add type system and validators
- ⚠️ Create demo app using HAWS_CONFIG
- ⚠️ Write API documentation

**Reference:** See S05.3.2 for architecture details and devtools/docs/haws_config_refactoring_reference.md for detailed refactoring plan

---
<!-- #endregion -->

<!-- #region 4. COMPLETED MILESTONES -->
# 4. COMPLETED MILESTONES

## 4.1 October 2025

### 4.1.1 CNM Bubble Note Fixes
**FIRM:** All bugs fixed and tested

**Completed:**
- ✅ Fixed reactor proliferation bug (ONE reactor for all bubbles)
- ✅ Cleaned up XDATA mess (42 lines of abandoned planning code)
- ✅ Fixed SPLIT_ON_NBSP bug (values without delimiters go to PREFIX not AUTO)
- ✅ System-controlled attributes (NOTENUM, NOTEPHASE, NOTEGAP) always to PREFIX
- ✅ DCL UI updated (text elements instead of disabled radio buttons)

### 4.1.2 Tip System Migration
**FIRM:** Complete

**Completed:**
- ✅ Migrated from file I/O to getcfg/setcfg
- ✅ Removed all haws-tip-hide.lsp file dependencies
- ✅ Documented in S05.6

### 4.1.3 Case Convention Migration
**FIRM:** Complete (2025-10-25)

**Completed:**
- ✅ Converted entire codebase to lowercase
- ✅ Updated standards_03_names_and_symbols.md
- ✅ Created conversion script (convert_to_lowercase.ps1)
- ✅ Converted 133 .lsp files in devsource/
- ✅ Tested successfully - all tests passed
- ✅ Merged to master branch for colleague testing
- ✅ Generated uppercase reports (660 lines with 8+, 2,416 with 4+, 183 quoted symbols)
- ✅ Documented uppercase exceptions in S03.3.1.4 (strings, comments, quoted symbols)

### 4.1.4 Standards System
**FIRM:** Complete

**Completed:**
- ✅ Created 6-volume standards series with hierarchical numbering
- ✅ Migrated all content from old documentation
- ✅ Established S##.#.# reference format
- ✅ Changed status markers from emoji to text (FIRM, PLANNED, IN-DEV)
- ✅ Fixed AI comment attribution (AI uses [AI: ...] not [TGH: ...])

---
<!-- #endregion -->

<!-- #region 5. PLANNED FEATURES -->
# 5. PLANNED FEATURES

## 5.1 Developer Tools

### 5.1.1 FAS Compiler
**Status:** PLANNED

**Goal:** Replace manual FAS compilation with HAWS-REBUILD-FAS command

**Requirements:**
- Dynamic file list (scan devsource/ for *.lsp)
- Exclusion list (*loader.lsp, CNM-Install.lsp, sandbox files)
- Compile all .lsp → .fas in devsource/
- Report compilation errors clearly
- Verify all .fas files load successfully

**File:** devtools/haws_dev_rebuild_fas.lsp

### 5.1.2 Developer Mode Loader
**Status:** PLANNED

**Goal:** Automatic dev mode detection and activation

**Requirements:**
- cnmdevloader.lsp in devtools/
- cnmloader.lsp detects if devtools/ in search path
- Prints "*** HawsEDC Developer Mode Active ***" to command line
- Loads source .lsp files (not .fas)
- Enables developer commands

**Files:**
- devtools/cnmdevloader.lsp (new)
- devsource/cnmloader.lsp (modify)

### 5.1.3 Distribution Packager
**Status:** PLANNED

**Goal:** Replace Windows batch file with cross-platform HAWS-DISTRIB command

**Current:** C:\TGHFiles\programming\hawsedc\compile\distrib.bat

**Requirements:**
- Copy compiled .fas files from devsource/
- Copy support files (.dcl, .dwg, .lin, .csv, .ini)
- Copy menu resources from devmenu/
- Exclude devtools/, sandbox/, retired/
- Automatic version numbering
- Create distribution README

**File:** devtools/haws_dev_distrib.lsp

## 5.2 Code Refactoring

### 5.2.1 Split cnm.lsp
**Status:** DISCUSS - File is 8600+ lines

**Proposal:** Split into focused modules:
- cnm_core.lsp (commands and main logic)
- cnm_bubble.lsp (bubble note system)
- cnm_config.lsp (configuration management)
- cnm_reactor.lsp (reactor system)
- cnm_notes.lsp (project notes and quantity takeoff)

**Pros:**
- Easier to navigate and maintain
- Clearer module boundaries
- Faster to load individual modules during development

**Cons:**
- More files to manage in loader
- Risk of circular dependencies
- Load order complexity

**Decision:** [AI: Discuss pros/cons when decision is made - see S02.6.3.1 for decision documentation format]

### 5.2.2 Delimiter Migration (Completed)
**Status:** COMPLETED

Migration from underscores to hyphens is complete. Hyphens are the FIRM standard per S03.4.1, matching AutoLISP community convention.

## 5.3 Project Notes Editor

### 5.3.1 Replace Third-Party Editor
**Status:** PLANNED

**Background:**
- Third-party developer created cnmedit.exe
- Developer now deprecates that editor
- Suggests we create our own editor

**Requirements:**
- Edit CSV format (READCFCSV)
- Spreadsheet-like interface
- AutoLISP DCL or external tool?
- Support editing constnot-default.csv

**Decision:** [AI: DCL vs external tool? Python with pandas? Excel macros? - Needs discussion]

## 5.4 Future Enhancements

### 5.4.1 AutoLoading Documentation
**Status:** PLANNED

**Goal:** Document how command aliases and autoloading work

**Files to document:**
- cnmalias.lsp
- cnmaliaslib.lsp
- How C:CNM expands to full command
- Demand loading strategy

**Target:** Add to S04

### 5.4.2 Test Framework
**Status:** DISCUSS

**Goal:** Automated testing for critical functions

**Considerations:**
- AutoLISP testing libraries available?
- Unit tests for pure functions (CALCULATE, FORMAT)
- Integration tests for commands
- Regression test suite

---
<!-- #endregion -->

<!-- #region 6. TECHNICAL DEBT -->
# 6. TECHNICAL DEBT

## 6.1 Known Issues

### 6.1.1 No Issues Currently Tracked
All recent bugs have been fixed (see section 4.1.1)

## 6.2 Code Quality

### 6.2.1 Function Headers
**Status:** NOT ADOPTED (see S04.4.2)

**Issue:** Many functions lack formal headers documenting purpose, arguments, returns

**Proposal:** Add headers incrementally when refactoring

### 6.2.2 Comment Consistency
**Status:** IN-DEV

**Issue:** Mix of comment styles throughout codebase

**Goal:** Use [TGH: ...] and [AI: ...] format consistently in new code

---
<!-- #endregion -->

<!-- #region 7. BRANCH STRATEGY -->
# 7. BRANCH STRATEGY

## 7.1 Current Branches

### 7.1.1 master
**Purpose:** Stable production code

**Rules:**
- All code must be tested before merging
- Follow all standards (S01-S06)
- Commit messages follow format (see S04.7.3.2)

### 7.1.2 haws-config
**Purpose:** HAWS_CONFIG refactoring work

**Status:** IN-DEV (see section 3.1.2)

**Rules:**
- Focus only on config system extraction
- Maintain CNM backward compatibility
- Merge to master when fully tested

### 7.1.3 Feature Branches
**Convention:** `feature/brief-description`

**Examples:**
- feature/fas-compiler
- feature/dev-mode-loader
- feature/split-cnm-file

## 7.2 Merge Strategy

### 7.2.1 Standard Process
1. Create feature branch from master
2. Make changes following standards
3. Test thoroughly
4. Commit with descriptive messages
5. Merge to master when complete

### 7.2.2 Breaking Changes
- Save for major version bumps
- Document in migration guide
- Update all affected code atomically

---
<!-- #endregion -->

<!-- #region 8. VERSION HISTORY -->
# 8. VERSION HISTORY

## 8.1 Version Numbering
**PLANNED:** Establish semantic versioning

**Format:** MAJOR.MINOR.PATCH

**Rules:**
- MAJOR: Breaking changes
- MINOR: New features (backward compatible)
- PATCH: Bug fixes

## 8.2 Recent Versions

### 8.2.1 Current Development
**Version:** TBD (pre-1.0)

**Focus:** Standards establishment, bug fixes, architecture improvements

---
<!-- #endregion -->

**End of Document**
