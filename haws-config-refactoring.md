# HAWS-CONFIG: Generalizing the CNM Configuration System

## Executive Summary

Your goal to extract the CNM configuration system into a reusable HAWS-CONFIG library is **excellent and highly achievable**. The existing `HCNM_CONCEPT` code (~lines 2738-3016 in cnm.lsp) shows you've already been thinking in this direction. The current HCNM-CONFIG system is well-architected for extraction with minimal breaking changes.

**Bottom Line**: This refactoring is **strongly recommended** and will significantly improve code reusability, maintainability, and shareability across your HawsEDC ecosystem.

---

## Analysis of Current Architecture

### Current HCNM-CONFIG System Structure

**Strengths:**
1. **Clean separation of concerns**: Config definitions, storage, and retrieval are separate
2. **Multi-scope architecture**: Session (0), Drawing (1), Project (2), App (3), User (4)
3. **Fallback chain**: Memory → INI file → Defaults with graceful degradation
4. **Type-aware**: String, integer, boolean handling
5. **INI-based persistence**: Standard format for storage
6. **Centralized definitions**: Single `HCNM_CONFIG_DEFINITIONS` function

**Key Functions:**
- `C:HCNM-CONFIG-GETVAR` - Retrieves config values with scope awareness
- `C:HCNM-CONFIG-SETVAR` - Saves config values to appropriate scope
- `HCNM_CONFIG_DEFINITIONS` - Centralized config schema
- `HCNM_CONFIG_READ_ALL_*` - Scope-specific readers (Project, User, Session)
- `HCNM_CONFIG_WRITE_*` - Scope-specific writers

**Architecture Pattern:**
```
Application
    └─ Wrapper Functions (C:HCNM-CONFIG-GETVAR/SETVAR)
        └─ Generic Config System
            ├─ Definitions (schema)
            ├─ Scope Handlers (Session/Project/User/App/Drawing)
            ├─ Storage Backend (INI files)
            └─ In-Memory Cache (*HCNM_CONFIG*)
```

### HCNM_CONCEPT Analysis

Your `HCNM_CONCEPT` code (lines 2816-3016) represents an **earlier iteration** with similar goals:

**Conceptual Advances:**
- Multi-app awareness: `APP` parameter for different applications sharing config system
- Test file indicators: Using `.mnl` or `.cui` files to locate config directories
- Cleaner API: `hcnm_concept_GETVAR(VAR, SECT, APP, SCOPE, TESTFILE, DEFAULTS)`

**Challenges Identified (your own comments):**
- "This function is wrong because there isn't a single `*hcnm_concept_INIFOLDER*`" (line 2864)
- Scope-specific INI file location logic needs refinement

**Recommendation**: Don't abandon HCNM_CONCEPT - it has good ideas! Merge its multi-app awareness into the refactored HAWS-CONFIG.

---

## Refactoring Strategy

### Phase 1: Extract Core (Minimal Breaking Changes)

**Goal**: Create `haws-config.lsp` with generic functions while maintaining CNM compatibility.

**Steps:**

1. **Create New File Structure**
   ```
   devsource/
   ├── haws-config.lsp         (NEW - Generic config system)
   ├── cnm.lsp                  (Modified - CNM-specific wrapper)
   └── cnmloader.lsp            (Modified - Load haws-config.lsp first)
   ```

2. **Extract Generic Functions to haws-config.lsp**
   
   Rename with `HAWS-CONFIG:` namespace:
   ```lisp
   ;; Core API
   (DEFUN HAWS-CONFIG:GETVAR (APP VAR SCOPE DEFAULTS)
   (DEFUN HAWS-CONFIG:SETVAR (APP VAR VAL SCOPE)
   (DEFUN HAWS-CONFIG:REGISTER-APP (APP DEFINITIONS)
   
   ;; Scope handlers
   (DEFUN HAWS-CONFIG:READ-SCOPE (APP SCOPE)
   (DEFUN HAWS-CONFIG:WRITE-SCOPE (APP VAR VAL SCOPE)
   
   ;; Storage backend
   (DEFUN HAWS-CONFIG:INI-PATH (APP SCOPE)
   (DEFUN HAWS-CONFIG:CACHE-GET (APP VAR)
   (DEFUN HAWS-CONFIG:CACHE-SET (APP VAR VAL)
   ```

3. **Keep CNM Wrapper Functions**
   
   In `cnm.lsp`, maintain backward compatibility:
   ```lisp
   (DEFUN C:HCNM-CONFIG-GETVAR (VAR)
     (HAWS-CONFIG:GETVAR 
       "CNM"                    ; App identifier
       VAR 
       (HCNM_CONFIG_SCOPE_CODE VAR)
       (HCNM_CONFIG_GET_DEFAULT VAR)
     )
   )
   
   (DEFUN C:HCNM-CONFIG-SETVAR (VAR VAL)
     (HAWS-CONFIG:SETVAR "CNM" VAR VAL (HCNM_CONFIG_SCOPE_CODE VAR))
   )
   ```

4. **Global Variable Refactoring**
   ```lisp
   ;; OLD: *HCNM_CONFIG* (single-app alist)
   ;; NEW: *HAWS-CONFIG:CACHE* (multi-app nested alist)
   ;;   Structure: '(("CNM" (("VAR1" "VAL1") ...))
   ;;                ("HAWS-QT" (("VAR2" "VAL2") ...)))
   ```

### Phase 2: Multi-Application Support

**Goal**: Allow multiple apps to share the config system.

**Design:**

```lisp
;; App registration with schema
(HAWS-CONFIG:REGISTER-APP "CNM" (HCNM_CONFIG_DEFINITIONS))
(HAWS-CONFIG:REGISTER-APP "HAWS-QT" (HAWSQT_CONFIG_DEFINITIONS))

;; Each app gets its own INI file per scope
;; User scope:   %APPDATA%/HawsEDC/CNM-user.ini
;; Project scope: C:/Projects/ABC/CNM-project.ini
;; Session scope: In-memory only
```

**Benefits:**
- Multiple HawsEDC apps can coexist
- Each app maintains independent settings
- Shared infrastructure reduces duplication

### Phase 3: Enhanced Features

**Improvements over current system:**

1. **Type System**
   ```lisp
   ;; Current: Everything is strings, converted manually
   ;; Proposed: Type declarations in definitions
   (LIST "BubbleTextPrecisionOff+" "2" :TYPE 'integer :SCOPE "User")
   (LIST "ShowKeyTableGrid" "0" :TYPE 'boolean :SCOPE "Project")
   ```

2. **Validation**
   ```lisp
   (HAWS-CONFIG:REGISTER-VALIDATOR 
     "CNM" 
     "BubbleTextPrecisionOff+" 
     '(LAMBDA (VAL) (AND (>= VAL 0) (<= VAL 8)))
   )
   ```

3. **Change Notifications**
   ```lisp
   ;; Register callbacks for when config changes
   (HAWS-CONFIG:ON-CHANGE "CNM" "NotesLeaderDimstyle" 
     '(LAMBDA (OLD NEW) (HCNM_UPDATE_DIMSTYLE_CACHE NEW))
   )
   ```

4. **Import/Export**
   ```lisp
   (HAWS-CONFIG:EXPORT-SCOPE "CNM" "Project" "C:/Templates/cnm-defaults.ini")
   (HAWS-CONFIG:IMPORT-SCOPE "CNM" "Project" "C:/Templates/cnm-defaults.ini")
   ```

---

## API Design Proposal

### Minimal API (Phase 1)

```lisp
;; App Setup (once per app, in loader)
(HAWS-CONFIG:REGISTER-APP "MYAPP" 
  '(("Var"
     ("Setting1" "default1" 2)  ; Project scope
     ("Setting2" "default2" 4)  ; User scope
   ))
)

;; Get/Set (application code)
(SETQ VAL (HAWS-CONFIG:GETVAR "MYAPP" "Setting1"))
(HAWS-CONFIG:SETVAR "MYAPP" "Setting1" "newvalue")
```

### Enhanced API (Phase 3)

```lisp
;; Registration with types and validation
(HAWS-CONFIG:REGISTER-APP "MYAPP"
  :DEFINITIONS '(
    ("IntSetting" 42 :TYPE 'integer :SCOPE "User" :MIN 0 :MAX 100)
    ("BoolSetting" T :TYPE 'boolean :SCOPE "Project")
    ("StringSetting" "default" :TYPE 'string :SCOPE "User" :MAX-LENGTH 50)
  )
  :INI-SECTION "MyApp"
  :INDICATOR-FILE "myapp.mnl"
)
```

---

## Migration Path for CNM

### Before (Current CNM Code)
```lisp
(C:HCNM-CONFIG-GETVAR "BubbleTextPrefixSta")
(C:HCNM-CONFIG-SETVAR "BubbleTextPrefixSta" "STA ")
```

### After (Phase 1 - Backward Compatible)
```lisp
;; No changes needed! Wrapper functions remain
(C:HCNM-CONFIG-GETVAR "BubbleTextPrefixSta")  ; Works exactly the same
(C:HCNM-CONFIG-SETVAR "BubbleTextPrefixSta" "STA ")
```

### After (Phase 2 - Direct HAWS-CONFIG API)
```lisp
;; Optional migration to new API
(HAWS-CONFIG:GETVAR "CNM" "BubbleTextPrefixSta")
(HAWS-CONFIG:SETVAR "CNM" "BubbleTextPrefixSta" "STA ")
```

**Key Point**: CNM code doesn't need to change immediately. The wrapper functions provide a smooth transition path.

---

## Benefits of Refactoring

### For CNM Project
1. **Cleaner separation**: Config logic isolated from business logic
2. **Easier testing**: Config system can be unit tested independently
3. **Reduced size**: `cnm.lsp` becomes smaller and more focused
4. **Better maintainability**: Config changes don't affect CNM core

### For HawsEDC Ecosystem
1. **Code reuse**: Other apps (HAWS-QT, future tools) use same system
2. **Consistent UX**: All apps have same config behavior
3. **Shared development**: Improvements benefit all apps
4. **Cross-app integration**: Apps can read each other's safe settings if needed

### For Sharing/Distribution
1. **Standalone library**: Can be distributed independently
2. **Clear API**: Well-documented interface for third parties
3. **Examples included**: Demo app shows how to use it
4. **Version management**: Library can evolve independently of apps

---

## Comparison: HCNM_CONCEPT vs Current HCNM-CONFIG

| Feature | HCNM_CONCEPT | Current HCNM-CONFIG | Proposed HAWS-CONFIG |
|---------|--------------|---------------------|----------------------|
| Multi-app support | ✅ Yes (designed for it) | ❌ No (CNM-specific) | ✅ Yes |
| Scope system | ✅ Yes | ✅ Yes (more mature) | ✅ Yes (enhanced) |
| Type system | ❌ No | ❌ No (implicit) | ✅ Yes (explicit) |
| Validation | ❌ No | ❌ No | ✅ Yes |
| INI file per app | ✅ Yes | ❌ No (single HAWSEDC.INI) | ✅ Yes |
| Indicator files | ✅ Yes | ⚠️ Partial | ✅ Yes |
| Maturity | ⚠️ Concept only | ✅ Battle-tested | 🔄 Combines both |
| Documentation | ❌ No | ⚠️ Comments only | ✅ Full API docs |

**Recommendation**: Use current HCNM-CONFIG as base, add multi-app ideas from HCNM_CONCEPT.

---

## Example: Creating a New App with HAWS-CONFIG

Imagine you're creating a new tool "HAWS-CONTOURS" for contour labeling:

```lisp
;; In haws-contours.lsp

;; 1. Register app (in loader)
(HAWS-CONFIG:REGISTER-APP "HAWS-CONTOURS"
  :DEFINITIONS '(
    ("ContourInterval" "5" :TYPE 'real :SCOPE "Project" :MIN 0.1)
    ("LabelRotation" "1" :TYPE 'boolean :SCOPE "User")
    ("TextStyle" "Standard" :TYPE 'string :SCOPE "User")
  )
  :INI-SECTION "Contours"
  :INDICATOR-FILE "haws-contours.mnl"
)

;; 2. Use in commands
(DEFUN C:HAWS-CONTOURS-LABEL ( / INTERVAL ROTATE STYLE)
  (SETQ 
    INTERVAL (ATOF (HAWS-CONFIG:GETVAR "HAWS-CONTOURS" "ContourInterval"))
    ROTATE (= "1" (HAWS-CONFIG:GETVAR "HAWS-CONTOURS" "LabelRotation"))
    STYLE (HAWS-CONFIG:GETVAR "HAWS-CONTOURS" "TextStyle")
  )
  ;; ... labeling logic ...
)
```

**Result**: New app gets full config system with ~20 lines of code!

---

## Implementation Roadmap

### Milestone 1: Extraction (Week 1)
- [ ] Create `haws-config.lsp` skeleton
- [ ] Move scope handler functions
- [ ] Move INI read/write functions
- [ ] Add app-awareness to global cache
- [ ] Test with CNM (no functionality changes)

### Milestone 2: Multi-App Support (Week 2)
- [ ] Implement app registration system
- [ ] Add scope-specific INI file paths per app
- [ ] Create `HAWS-CONFIG:REGISTER-APP` function
- [ ] Update CNM to use registration
- [ ] Document API for other apps

### Milestone 3: Enhanced Features (Week 3)
- [ ] Add type system to definitions
- [ ] Implement validators
- [ ] Add import/export functions
- [ ] Create migration framework
- [ ] Write comprehensive examples

### Milestone 4: Release (Week 4)
- [ ] Create standalone demo app using HAWS-CONFIG
- [ ] Write API documentation
- [ ] Create tutorial/quickstart
- [ ] Package for distribution
- [ ] Publish to GitHub

---

## Recommended Next Steps

### Immediate (This Sprint)
1. ✅ **You are here**: Document refactoring plan
2. Create `haws-config.lsp` with extracted scope handlers
3. Test extraction with CNM (verify no behavior changes)
4. Commit as "Phase 1: Extract HAWS-CONFIG core"

### Near Term (Next Sprint)
5. Add app registration system
6. Convert CNM to use registration
7. Update loader to load haws-config first
8. Add comprehensive comments/docs

### Future (After CNM is Stable)
9. Create demo app using HAWS-CONFIG
10. Publish haws-config.lsp as standalone library
11. Write blog post / tutorial
12. Migrate HAWS-QT to use HAWS-CONFIG

---

## Conclusion

**Your instinct to generalize the config system is spot-on.** The current HCNM-CONFIG architecture is already 80% of the way there - it just needs app-awareness and better packaging.

**Key Success Factors:**
- ✅ Maintain backward compatibility via wrapper functions
- ✅ Use HCNM_CONCEPT's multi-app ideas
- ✅ Keep current scope system (it works well)
- ✅ Document API clearly for external users
- ✅ Create working example (demo app)

**Timeline Estimate**: 4 weeks for full-featured, documented, shareable library.

**Recommendation**: **Proceed with refactoring.** The benefits far outweigh the costs, and your architecture is already well-suited for extraction.

---

## Appendix: File Structure After Refactoring

```
hawsedc/develop/
├── devsource/
│   ├── haws-config.lsp          ⭐ NEW - Generic config system
│   ├── cnm.lsp                   Modified - CNM-specific logic
│   ├── cnmloader.lsp             Modified - Load order
│   ├── haws-qt.lsp               Future - Could migrate to HAWS-CONFIG
│   └── ...
├── examples/
│   └── haws-config-demo.lsp     ⭐ NEW - Standalone demo app
├── docs/
│   ├── HAWS-CONFIG-API.md       ⭐ NEW - API documentation
│   └── HAWS-CONFIG-TUTORIAL.md  ⭐ NEW - Getting started guide
└── tests/
    └── test-haws-config.lsp     ⭐ NEW - Unit tests
```

---

*Generated: 2025-10-21*  
*Based on analysis of: cnm.lsp (lines 2816-3509, HCNM_CONCEPT and HCNM-CONFIG systems)*
