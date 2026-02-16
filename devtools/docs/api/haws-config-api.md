# HAWS-CONFIG API Documentation

## Overview

HAWS-CONFIG is a generic multi-application configuration system for AutoLISP applications in the HawsEDC ecosystem. It provides centralized configuration management with support for multiple scopes (session, project, user registry) and multiple applications.

**Version**: 1.0  
**Created**: 2025-10-31  
**Extracted from**: CNM HCNM-CONFIG system

---

## Key Features

- **Multi-Application Support**: Multiple apps can use the same config infrastructure with isolated storage
- **Five Scope Levels**: Session (memory), Drawing (not yet implemented), Project (INI), App (not yet implemented), User (Registry)
- **Automatic Fallback Chain**: Cache → Storage → Defaults
- **Registry Integration**: Windows Registry support for user-level settings
- **INI File Support**: Project-level settings in standard INI format
- **Backward Compatible**: CNM maintains 100% compatibility through wrapper functions

---

## Architecture

### Global Variables

```lisp
*haws-config-cache*        ; Multi-app in-memory cache
*haws-app-config-definitions*  ; Registered app definitions
*haws-config-session*      ; Session-scope storage (scope 0)
```

### Data Structures

**Cache Structure**:
```lisp
'(("APP1" (("var1" "val1") ("var2" "val2")))
  ("APP2" (("varA" "valA") ("varB" "valB"))))
```

**Definitions Structure**:
```lisp
'(("APP1" . <definitions-list>)
  ("APP2" . <definitions-list>))
```

**Config Definition Format** (flat list of tuples):
```lisp
;; Each entry: (var-name default-value scope-code)
;; Scope codes: 0=Session, 1=Drawing, 2=Project, 3=App, 4=User
(list
  (list "AppFolder" (haws-filename-directory (findfile "cnm.mnl")) 0)
  (list "ProjectNotes" "constnot.csv" 2)
  (list "LXXListMode" "yes" 4))
```

---

## Public API Functions

### haws-config-register-app

Register an application with the config system.

**Signature**:
```lisp
(haws-config-register-app app definitions)
```

**Parameters**:
- `app` (string): Application identifier (e.g., "CNM", "HAWS", "HAWS-QT")
- `definitions` (list): Config definitions in standard format

**Returns**: `T` on success

**Side Effects**:
- Stores definitions in `*haws-app-config-definitions*`
- Initializes app entry in `*haws-config-cache*`
- Initializes app entry in `*haws-config-session*`

**Example**:
```lisp
(defun myapp-config-definitions ()
  (list
    (list "Version" "1.0.0" 0)           ; Session scope
    (list "ProjectPath" "" 2)            ; Project scope
    (list "UserPreference" "default" 4)  ; User scope
  ))

(haws-config-register-app "MYAPP" (myapp-config-definitions))
```

---

### haws-config-getvar

Get configuration variable value with automatic fallback.

**Signature**:
```lisp
(haws-config-getvar app var ini-path section)
```

**Parameters**:
- `app` (string): Application identifier
- `var` (string): Variable name
- `ini-path` (string or nil): Full path to INI file (required for Project scope 2, nil otherwise)
- `section` (string or nil): INI section name (required for Project scope 2, nil otherwise)

**Note**: Scope is auto-looked-up from the app's registered definitions. It is NOT passed as a parameter.

**Returns**: Variable value (string), or default if not found

**Fallback Chain**:
1. Check cache
2. If not in cache, read from scope storage
3. If not in storage, use default from definitions
4. Store in cache and return

**Examples**:
```lisp
;; Session scope - no INI needed (scope auto-detected from definitions)
(setq version (haws-config-getvar "MYAPP" "Version" nil nil))

;; Project scope - requires INI path and section
(setq path (haws-config-getvar "MYAPP" "ProjectPath" "C:/project/config.ini" "MYAPP"))

;; User scope - reads from Registry
(setq pref (haws-config-getvar "MYAPP" "UserPreference" nil nil))
```

---

### haws-config-setvar

Set configuration variable value with automatic persistence.

**Signature**:
```lisp
(haws-config-setvar app var val ini-path section)
```

**Parameters**:
- `app` (string): Application identifier
- `var` (string): Variable name
- `val` (string): Value to set
- `ini-path` (string or nil): Full path to INI file (required for Project scope 2, nil otherwise)
- `section` (string or nil): INI section name (required for Project scope 2, nil otherwise)

**Note**: Scope is auto-looked-up from the app's registered definitions. It is NOT passed as a parameter.

**Returns**: The value that was set

**Side Effects**:
- Updates cache
- Writes to appropriate scope storage:
  - Scope 0: Session memory only
  - Scope 2: INI file (persistent)
  - Scope 4: Windows Registry (persistent)

**Examples**:
```lisp
;; Session scope
(haws-config-setvar "MYAPP" "Version" "2.0.0" nil nil)

;; Project scope
(haws-config-setvar "MYAPP" "ProjectPath" "C:/myproject" "C:/project/config.ini" "MYAPP")

;; User scope
(haws-config-setvar "MYAPP" "UserPreference" "advanced" nil nil)
```

---

## Scope System

### Scope 0 - Session (Memory Only)

**Storage**: `*haws-config-session*` global variable  
**Persistence**: Lost when AutoCAD closes  
**Use Cases**: Temporary runtime values, calculated paths, version info

**Example Variables**:
- Application folder path
- Runtime version
- Temporary flags

### Scope 1 - Drawing (Not Yet Implemented)

**Storage**: Drawing file extended entity data  
**Persistence**: Saved with drawing  
**Use Cases**: Drawing-specific settings

### Scope 2 - Project (INI File)

**Storage**: INI file (e.g., `cnm.ini`, `project.ini`)  
**Persistence**: Project-level, shared across drawings  
**Use Cases**: Project settings, shared team preferences

**Registry Path**: N/A  
**INI Format**:
```ini
[MYAPP]
ProjectPath=C:\myproject
TemplateFile=standard.dwt
```

**Example Variables**:
- Project notes file path
- Default templates
- Project-specific preferences

### Scope 3 - App (Not Yet Implemented)

**Storage**: Application install folder  
**Persistence**: Application-level  
**Use Cases**: Installation defaults, read-only settings

### Scope 4 - User (Windows Registry)

**Storage**: `HKEY_CURRENT_USER\Software\HawsEDC\{APP}`  
**Persistence**: User profile, machine-specific  
**Use Cases**: User preferences, UI settings, per-user defaults

**Registry Path**: `HKCU\Software\HawsEDC\MYAPP\`  
**Example Variables**:
- UI preferences
- Command aliases activation
- User-specific defaults

---

## Helper Functions (Internal Use)

These functions are available but typically not needed by application code:

### Cache Management
- `haws-config-cache-get` - Get value from cache
- `haws-config-cache-set` - Set value in cache
- `haws-config-cache-get-app` - Get entire app cache

### Definition Helpers
- `haws-config-get-definitions` - Get definitions for app
- `haws-config-scope-code` - Convert scope name to code
- `haws-config-entry-var` - Extract variable name from entry
- `haws-config-entry-val` - Extract default value from entry
- `haws-config-entry-scope-code` - Extract scope code from entry

### Storage Functions
- `haws-config-read-user` - Read from Registry
- `haws-config-write-user` - Write to Registry
- `haws-config-read-session` - Read from session cache
- `haws-config-write-session` - Write to session cache
- `haws-config-read-all-project` - Read all Project-scope vars
- `haws-config-write-project` - Write to INI file

---

## Integration Guide

### Step 1: Create Config Definitions

Create a function that returns your app's config schema (flat list of tuples):

```lisp
(defun myapp-config-definitions ()
  (list
    ;; Session variables (scope 0)
    (list "AppFolder" "" 0)
    (list "Version" "1.0.0" 0)
    ;; Project variables (scope 2)
    (list "ProjectName" "Untitled" 2)
    (list "TemplateFile" "default.dwt" 2)
    ;; User variables (scope 4)
    (list "UserName" "" 4)
    (list "ShowTips" "YES" 4)
  ))
```

### Step 2: Register on Load

Register your app when your main file loads:

```lisp
;; At the end of your main .lsp file
(if (and haws-config-register-app 
         (not (assoc "MYAPP" *haws-app-config-definitions*)))
  (haws-config-register-app "MYAPP" (myapp-config-definitions))
)
```

### Step 3: Use Config Functions

Use getvar/setvar in your code:

```lisp
;; Get session variable (scope auto-detected, no INI needed)
(setq version (haws-config-getvar "MYAPP" "Version" nil nil))

;; Get project variable (INI path required)
(setq template (haws-config-getvar "MYAPP" "TemplateFile"
                  (strcat project-path "/config.ini") "MYAPP"))

;; Get user variable (no INI needed)
(setq show-tips (haws-config-getvar "MYAPP" "ShowTips" nil nil))

;; Set values
(haws-config-setvar "MYAPP" "ProjectName" "My Project"
                    (strcat project-path "/config.ini") "MYAPP")
(haws-config-setvar "MYAPP" "ShowTips" "NO" nil nil)
```

### Step 4: Optional - Create Wrapper Functions

For convenience, create wrapper functions like CNM does:

```lisp
(defun myapp-getvar (var / scope-code)
  ;; Check scope to determine if INI path is needed
  (setq scope-code (haws-config-get-scope "MYAPP" var))
  (haws-config-getvar
    "MYAPP" var
    (if (= scope-code 2) (myapp-get-project-ini) nil)
    (if (= scope-code 2) "MYAPP" nil)
  )
)

(defun myapp-setvar (var val / scope-code)
  (setq scope-code (haws-config-get-scope "MYAPP" var))
  (haws-config-setvar
    "MYAPP" var val
    (if (= scope-code 2) (myapp-get-project-ini) nil)
    (if (= scope-code 2) "MYAPP" nil)
  )
)
```

---

## Best Practices

### Variable Naming
- Use descriptive names: `ProjectNotesFile` not `PNF`
- Use PascalCase for multi-word names
- Group related variables with prefixes: `Bubble...`, `Table...`

### Scope Selection
- **Session (0)**: Calculated or temporary values
- **Project (2)**: Settings that should be shared in a project
- **User (4)**: Personal preferences that follow the user

### INI File Management
- Use one INI file per project
- Use your app name as the section name
- Store INI path in a consistent location
- Consider a project management function

### Error Handling
- Always provide reasonable defaults
- Handle missing INI files gracefully
- Cache values to minimize Registry/INI reads

### Performance
- Batch read project variables on initialization
- Cache frequently-accessed values
- Avoid repeated INI file operations

---

## Migration from Legacy Config Systems

If you have an existing config system, here's how to migrate:

### 1. Map Your Variables

Create a mapping of old variables to new format:

```lisp
;; Old system
(setq *my-config*
  '(("Var1" "value1")
    ("Var2" "value2")))

;; New system - add scope codes (flat list of tuples)
(list
  (list "Var1" "value1" 4)  ; Determine appropriate scope
  (list "Var2" "value2" 2))
```

### 2. Create Wrapper Layer

Maintain backward compatibility:

```lisp
;; Old function
(defun my-old-getvar (var)
  (cadr (assoc var *my-config*)))

;; New wrapper maintains API
(defun my-old-getvar (var)
  (myapp-getvar var))  ; Calls HAWS-CONFIG internally
```

### 3. Gradual Migration

- Phase 1: Add HAWS-CONFIG alongside old system
- Phase 2: Redirect old functions to new system
- Phase 3: Remove old system after testing

---

## Troubleshooting

### Variable Returns Nil
- Check app is registered: `(assoc "MYAPP" *haws-app-config-definitions*)`
- Verify variable exists in definitions
- Check scope code is correct
- For scope 2, ensure INI path is valid

### Value Not Persisting
- Scope 0 (Session) doesn't persist - by design
- Scope 2 (Project): Check INI file path and permissions
- Scope 4 (User): Check Registry permissions

### Registry Issues
- Verify Visual LISP is loaded: `(haws-vlisp-p)`
- Check Registry path: `HKCU\Software\HawsEDC\{APP}`
- Ensure user has Registry write permissions

### INI File Issues
- Verify INI file exists and is readable
- Check section name matches app registration
- Ensure `ini_readsection` and `ini_writeentry` functions are loaded (from edclib.lsp)

---

## Examples from CNM

CNM (Construction Notes Manager) was the first application to use HAWS-CONFIG. Here are real-world examples:

### CNM Config Definitions

```lisp
(defun hcnm-config-definitions ()
  (list
    ;; Session
    (list "AppFolder" (haws-filename-directory (findfile "cnm.mnl")) 0)
    ;; Project (stored in cnm.ini)
    (list "ProjectNotes" "constnot.csv" 2)
    (list "NoteTypes" "BOX,CIR,DIA,ELL,HEX,OCT,PEN,REC,SST,TRI" 2)
    (list "DoCurrentTabOnly" "0" 2)
    ;; ... ~50 total variables
    ;; User (stored in Windows Registry)
    (list "LXXListMode" "yes" 4)
    (list "CNMAliasActivation" "0" 4)
  ))
;; Registration at end of cnm.lsp:
(haws-config-register-app "CNM" (hcnm-config-definitions))
```

### CNM Wrapper Functions

```lisp
;; Scope check prevents circular dependency:
;; hcnm-proj → hcnm-initialize-project → hcnm-config-getvar("AppFolder")
;; AppFolder is scope 0, so we pass nil for ini-path (no hcnm-proj call needed)
(defun hcnm-config-getvar (var / scope-code)
  (setq scope-code (haws-config-get-scope "CNM" var))
  (haws-config-getvar
    "CNM" var
    (if (= scope-code 2) (hcnm-ini-name (hcnm-proj)) nil)
    "CNM"
  )
)

(defun hcnm-config-setvar (var val / scope-code)
  (setq scope-code (haws-config-get-scope "CNM" var))
  (haws-config-setvar
    "CNM" var val
    (if (= scope-code 2) (hcnm-ini-name (hcnm-proj)) nil)
    "CNM"
  )
)
```

### HAWS App (Shared HawsEDC Configs in edclib.lsp)

**Note:** `haws-app-config-definitions` name is ambiguous (see §3.3.1 in S05). Rename to `haws-app-config-definitions` is pending.

```lisp
(defun haws-app-config-definitions ()
  (list
    (list "AppFolder" (haws-filename-directory (findfile "cnm.mnl")) 0)  ; Session
    (list "ImportLayerSettings" "YES" 2)  ; Project
    (list "CNMAliasActivation" "2" 4)     ; User
    (list "DebugLevel" "0" 0)             ; Session
  ))
;; Registration in edclib.lsp:
(haws-config-register-app "HAWS" (haws-app-config-definitions))
```

---

## Version History

- **1.0.0** (2025-10-31): Initial release extracted from CNM HCNM-CONFIG system
  - Multi-app support
  - Five scope levels (0, 1, 2, 3, 4)
  - Registry and INI file support
  - Cache management
  - CNM backward compatibility maintained

---

## See Also

- [HAWS-CONFIG Tutorial](HAWS-CONFIG-TUTORIAL.md) - Step-by-step integration guide
- [GitHub Issue #11](https://github.com/hawstom/cnm/issues/11) - Original refactoring ticket
- `.github/copilot-instructions.md` - Project coding conventions

---

## Support

For questions, issues, or contributions:
- GitHub Issues: https://github.com/hawstom/cnm/issues
- Tag issues with `haws-config` label
