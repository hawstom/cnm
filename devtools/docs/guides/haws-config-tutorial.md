# HAWS-CONFIG Tutorial

## Introduction

This tutorial will walk you through integrating HAWS-CONFIG into your AutoLISP application. We'll build a simple example application called "MyDrawingTool" that demonstrates all the key features.

**Time Required**: 30 minutes  
**Prerequisites**: 
- Basic AutoLISP knowledge
- HawsEDC ecosystem (edclib.lsp loaded)
- Understanding of INI files and Windows Registry concepts

---

## What You'll Build

A simple drawing tool application with:
- Session-scope variables (version, app folder)
- Project-scope variables (template file, default layer)
- User-scope variables (user preferences, UI settings)
- Configuration dialog
- Persistent settings

---

## Step 1: Create Your Main File

Create `mydrawingtool.lsp`:

```lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MyDrawingTool - Example Application Using HAWS-CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prompt "\nLoading MyDrawingTool...")

;; Global variables
(setq *mdt-version* "1.0.0")
(setq *mdt-project-folder* nil)  ; Will be set by user

```

---

## Step 2: Define Your Config Schema

Add the config definitions function:

```lisp
;;; Define configuration schema
(defun mdt-config-definitions ()
  (list
    ;; Scope definitions (required)
    (list "Scope"
      (list "Session" 0)   ; Memory only
      (list "Drawing" 1)   ; Not implemented yet
      (list "Project" 2)   ; INI file
      (list "App" 3)       ; Not implemented yet
      (list "User" 4)      ; Windows Registry
    )
    
    ;; Variable definitions (required)
    (list "Var"
      ;; Session-scope variables (scope 0)
      ;; These are calculated at runtime and don't persist
      (list "AppFolder" "" 0)
      (list "Version" *mdt-version* 0)
      
      ;; Project-scope variables (scope 2)
      ;; These are stored in project INI file
      (list "ProjectFolder" "" 2)
      (list "TemplateFile" "standard.dwt" 2)
      (list "DefaultLayer" "0" 2)
      (list "UseMetric" "NO" 2)
      
      ;; User-scope variables (scope 4)
      ;; These are stored in Windows Registry per-user
      (list "UserName" "" 4)
      (list "ShowWelcome" "YES" 4)
      (list "AutoSave" "YES" 4)
      (list "RecentFiles" "" 4)
    )
  )
)
```

**Key Points**:
- Every config must have both "Scope" and "Var" sections
- Each variable has: `(name default-value scope-code)`
- Scope codes: 0=Session, 2=Project, 4=User (most common)

---

## Step 3: Register Your Application

Register with HAWS-CONFIG when your file loads:

```lisp
;;; Register with HAWS-CONFIG (do this at load time)
(if (and haws-config-register-app 
         (not (assoc "MDT" *haws-config-definitions*)))
  (progn
    (haws-config-register-app "MDT" (mdt-config-definitions))
    (prompt "\nMyDrawingTool registered with HAWS-CONFIG")
  )
)
```

**Important**: 
- Use a short, unique app identifier ("MDT" in this case)
- Check if already registered to avoid duplicates
- Register AFTER defining your config schema

---

## Step 4: Create Helper Functions

Create wrapper functions for easier config access:

```lisp
;;; Get project INI file path
(defun mdt-get-ini-path (/ proj-folder)
  (setq proj-folder (mdt-getvar "ProjectFolder"))
  (cond
    ((and proj-folder (/= proj-folder ""))
     (strcat proj-folder "/mydrawingtool.ini"))
    (t nil))
)

;;; Get variable scope code from definitions
(defun mdt-get-var-scope (var / definitions var-list entry)
  (setq definitions (haws-config-get-definitions "MDT"))
  (setq var-list (cdr (assoc "Var" definitions)))
  (setq entry (assoc var var-list))
  (cond
    (entry (caddr entry))  ; Return scope code
    (t 0))  ; Default to session if not found
)

;;; Simplified getvar - determines scope and INI path automatically
(defun mdt-getvar (var / scope ini-path)
  (setq scope (mdt-get-var-scope var))
  (cond
    ((= scope 2)  ; Project scope needs INI path
     (setq ini-path (mdt-get-ini-path))))
  
  (haws-config-getvar "MDT" var scope ini-path "MDT")
)

;;; Simplified setvar - determines scope and INI path automatically
(defun mdt-setvar (var val / scope ini-path)
  (setq scope (mdt-get-var-scope var))
  (cond
    ((= scope 2)  ; Project scope needs INI path
     (setq ini-path (mdt-get-ini-path))))
  
  (haws-config-setvar "MDT" var val scope ini-path "MDT")
)
```

**Why Wrappers?**
- Simpler API for your code
- Automatic scope detection
- Centralized INI path management
- Easier to maintain

---

## Step 5: Initialize Session Variables

Set up session-scope variables when app loads:

```lisp
;;; Initialize session variables (these don't persist)
(defun mdt-init-session (/ app-folder)
  ;; Set app folder to where this file was loaded from
  (setq app-folder (haws-filename-directory (findfile "mydrawingtool.lsp")))
  (mdt-setvar "AppFolder" app-folder)
  (mdt-setvar "Version" *mdt-version*)
  
  (prompt (strcat "\nMyDrawingTool " (mdt-getvar "Version") " initialized"))
  (prompt (strcat "\nApp folder: " (mdt-getvar "AppFolder")))
)

;; Call initialization
(mdt-init-session)
```

---

## Step 6: Create Project Setup Command

Create a command to set up a project:

```lisp
;;; Command: Set up a new project
(defun c:mdt-setup (/ folder template layer metric)
  (prompt "\n=== MyDrawingTool Project Setup ===")
  
  ;; Get project folder
  (setq folder (getstring T "\nProject folder path: "))
  (cond
    ((or (not folder) (= folder ""))
     (alert "Project folder is required!")
     (exit)))
  
  ;; Get template file
  (initget "Standard Custom")
  (setq template (getkword "\nTemplate [Standard/Custom] <Standard>: "))
  (cond
    ((not template) (setq template "Standard"))
    ((= template "Standard") (setq template "standard.dwt"))
    ((= template "Custom") 
     (setq template (getfiled "Select Template File" "" "dwt" 0))))
  
  ;; Get default layer
  (setq layer (getstring T "\nDefault layer name <0>: "))
  (cond
    ((or (not layer) (= layer ""))
     (setq layer "0")))
  
  ;; Get metric preference
  (initget "Yes No")
  (setq metric (getkword "\nUse metric units? [Yes/No] <No>: "))
  (cond
    ((not metric) (setq metric "No")))
  
  ;; Save project settings
  (mdt-setvar "ProjectFolder" folder)
  (mdt-setvar "TemplateFile" template)
  (mdt-setvar "DefaultLayer" layer)
  (mdt-setvar "UseMetric" metric)
  
  ;; Set global for current session
  (setq *mdt-project-folder* folder)
  
  (prompt "\n=== Project Setup Complete ===")
  (prompt (strcat "\nProject folder: " folder))
  (prompt (strcat "\nTemplate: " template))
  (prompt (strcat "\nDefault layer: " layer))
  (prompt (strcat "\nMetric: " metric))
  (prompt "\nSettings saved to: ")
  (prompt (mdt-get-ini-path))
  
  (princ)
)
```

**Test It**:
```
Command: MDT-SETUP
Project folder path: C:/MyProject
Template [Standard/Custom] <Standard>: Standard
Default layer name <0>: ARCH
Use metric units? [Yes/No] <No>: No
```

Check the INI file at `C:/MyProject/mydrawingtool.ini`:
```ini
[MDT]
ProjectFolder=C:/MyProject
TemplateFile=standard.dwt
DefaultLayer=ARCH
UseMetric=No
```

---

## Step 7: Create User Preferences Command

Create a command for user preferences:

```lisp
;;; Command: Set user preferences
(defun c:mdt-prefs (/ username welcome autosave)
  (prompt "\n=== MyDrawingTool User Preferences ===")
  
  ;; Get current values
  (setq username (mdt-getvar "UserName"))
  (setq welcome (mdt-getvar "ShowWelcome"))
  (setq autosave (mdt-getvar "AutoSave"))
  
  ;; Show current values
  (prompt (strcat "\nCurrent user: " username))
  (prompt (strcat "\nShow welcome: " welcome))
  (prompt (strcat "\nAuto-save: " autosave))
  
  ;; Get new user name
  (setq username (getstring T (strcat "\nYour name <" username ">: ")))
  (cond
    ((and username (/= username ""))
     (mdt-setvar "UserName" username)))
  
  ;; Toggle welcome message
  (initget "Yes No")
  (setq welcome (getkword (strcat "\nShow welcome message? [Yes/No] <" welcome ">: ")))
  (cond
    (welcome (mdt-setvar "ShowWelcome" welcome)))
  
  ;; Toggle auto-save
  (initget "Yes No")
  (setq autosave (getkword (strcat "\nAuto-save drawings? [Yes/No] <" autosave ">: ")))
  (cond
    (autosave (mdt-setvar "AutoSave" autosave)))
  
  (prompt "\n=== Preferences Saved ===")
  (prompt "\nSettings saved to Windows Registry:")
  (prompt "\nHKEY_CURRENT_USER\\Software\\HawsEDC\\MDT")
  
  (princ)
)
```

**Test It**:
```
Command: MDT-PREFS
Current user: 
Show welcome: YES
Auto-save: YES
Your name <>: John Smith
Show welcome message? [Yes/No] <YES>: Yes
Auto-save drawings? [Yes/No] <YES>: No
```

Check Windows Registry:
- Path: `HKEY_CURRENT_USER\Software\HawsEDC\MDT`
- Keys: UserName, ShowWelcome, AutoSave

---

## Step 8: Use Config in Your Commands

Now use your config in actual drawing commands:

```lisp
;;; Command: Create a new drawing with project defaults
(defun c:mdt-new (/ template layer username welcome)
  ;; Get project settings
  (setq template (mdt-getvar "TemplateFile"))
  (setq layer (mdt-getvar "DefaultLayer"))
  
  ;; Get user settings
  (setq username (mdt-getvar "UserName"))
  (setq welcome (mdt-getvar "ShowWelcome"))
  
  ;; Show welcome if enabled
  (cond
    ((= welcome "YES")
     (alert 
       (princ
         (strcat
           "Welcome to MyDrawingTool, " username "!\n\n"
           "Template: " template "\n"
           "Default Layer: " layer)))))
  
  ;; Create new drawing logic here
  (prompt "\nCreating new drawing...")
  (prompt (strcat "\nUsing template: " template))
  (prompt (strcat "\nDefault layer: " layer))
  
  ;; Your drawing creation code here
  
  (princ)
)

;;; Command: Show current configuration
(defun c:mdt-info (/ )
  (prompt "\n=== MyDrawingTool Configuration ===")
  (prompt "\n")
  (prompt "\nSESSION VARIABLES (Memory Only):")
  (prompt (strcat "\n  Version: " (mdt-getvar "Version")))
  (prompt (strcat "\n  AppFolder: " (mdt-getvar "AppFolder")))
  (prompt "\n")
  (prompt "\nPROJECT VARIABLES (INI File):")
  (prompt (strcat "\n  ProjectFolder: " (mdt-getvar "ProjectFolder")))
  (prompt (strcat "\n  TemplateFile: " (mdt-getvar "TemplateFile")))
  (prompt (strcat "\n  DefaultLayer: " (mdt-getvar "DefaultLayer")))
  (prompt (strcat "\n  UseMetric: " (mdt-getvar "UseMetric")))
  (prompt "\n")
  (prompt "\nUSER VARIABLES (Registry):")
  (prompt (strcat "\n  UserName: " (mdt-getvar "UserName")))
  (prompt (strcat "\n  ShowWelcome: " (mdt-getvar "ShowWelcome")))
  (prompt (strcat "\n  AutoSave: " (mdt-getvar "AutoSave")))
  (prompt "\n")
  (prompt "\nINI File: ")
  (prompt (mdt-get-ini-path))
  (prompt "\nRegistry: HKCU\\Software\\HawsEDC\\MDT")
  (prompt "\n")
  (princ)
)
```

---

## Step 9: Test Your Application

### Test Sequence:

1. **Load the application**:
   ```
   Command: (load "mydrawingtool.lsp")
   Loading MyDrawingTool...
   MyDrawingTool registered with HAWS-CONFIG
   MyDrawingTool 1.0.0 initialized
   App folder: C:/HawsEDC/
   ```

2. **Set up a project**:
   ```
   Command: MDT-SETUP
   [Follow prompts]
   ```

3. **Set user preferences**:
   ```
   Command: MDT-PREFS
   [Follow prompts]
   ```

4. **View configuration**:
   ```
   Command: MDT-INFO
   ```

5. **Test persistence**:
   - Close AutoCAD
   - Reopen AutoCAD
   - Load application again
   - Run `MDT-INFO`
   - User preferences should persist (Registry)
   - Project settings should persist if you're in the same project folder

---

## Step 10: Advanced Features

### Add Recent Files Tracking

```lisp
;;; Add file to recent files list
(defun mdt-add-recent-file (filepath / recent-list)
  (setq recent-list (mdt-getvar "RecentFiles"))
  
  ;; Parse existing list (comma-separated)
  (setq recent-list 
    (if (and recent-list (/= recent-list ""))
      (strcat filepath "," recent-list)
      filepath))
  
  ;; Keep only first 5 files
  (setq recent-list (mdt-limit-string-list recent-list "," 5))
  
  ;; Save back
  (mdt-setvar "RecentFiles" recent-list)
)

;;; Helper: Limit comma-separated string to N items
(defun mdt-limit-string-list (str delimiter max-items / items result)
  (setq items (haws-string-split str delimiter))
  (setq items (haws-list-take items max-items))
  (haws-string-join items delimiter)
)
```

### Add Config Validation

```lisp
;;; Validate project configuration
(defun mdt-validate-config (/ folder template valid)
  (setq valid t)
  
  ;; Check project folder
  (setq folder (mdt-getvar "ProjectFolder"))
  (cond
    ((or (not folder) (= folder ""))
     (prompt "\nERROR: Project folder not set. Run MDT-SETUP.")
     (setq valid nil))
    ((not (vl-file-directory-p folder))
     (prompt (strcat "\nERROR: Project folder does not exist: " folder))
     (setq valid nil)))
  
  ;; Check template file
  (setq template (mdt-getvar "TemplateFile"))
  (cond
    ((and template (not (findfile template)))
     (prompt (strcat "\nWARNING: Template file not found: " template))))
  
  valid
)

;;; Use validation in commands
(defun c:mdt-new (/ )
  (cond
    ((not (mdt-validate-config))
     (alert "Configuration invalid. Please run MDT-SETUP.")
     (exit)))
  
  ;; Proceed with command...
)
```

### Add Config Export/Import

```lisp
;;; Export configuration to file
(defun c:mdt-export (/ filename file-handle vars)
  (setq filename (getfiled "Export Config" "" "cfg" 1))
  (cond
    (filename
     (setq file-handle (open filename "w"))
     (write-line ";; MyDrawingTool Configuration Export" file-handle)
     (write-line (strcat ";; Date: " (rtos (getvar "DATE"))) file-handle)
     (write-line "" file-handle)
     
     ;; Export user variables
     (write-line "[User]" file-handle)
     (write-line (strcat "UserName=" (mdt-getvar "UserName")) file-handle)
     (write-line (strcat "ShowWelcome=" (mdt-getvar "ShowWelcome")) file-handle)
     (write-line (strcat "AutoSave=" (mdt-getvar "AutoSave")) file-handle)
     
     (close file-handle)
     (prompt (strcat "\nConfiguration exported to: " filename))))
  (princ)
)
```

---

## Common Patterns

### Pattern 1: Conditional Config Loading

```lisp
;;; Load config only if project is set up
(defun mdt-ensure-project (/ )
  (cond
    ((or (not (mdt-getvar "ProjectFolder"))
         (= (mdt-getvar "ProjectFolder") ""))
     (prompt "\nNo project configured. Running setup...")
     (c:mdt-setup)
     t)
    (t nil))
)
```

### Pattern 2: Config Migration

```lisp
;;; Migrate from old config format
(defun mdt-migrate-old-config (/ old-config)
  (cond
    ((setq old-config (findfile "oldconfig.dat"))
     (prompt "\nMigrating from old configuration...")
     ;; Read old format
     ;; ...
     ;; Write to new format
     (mdt-setvar "TemplateFile" old-template)
     (mdt-setvar "DefaultLayer" old-layer)
     (prompt "\nMigration complete!")))
)
```

### Pattern 3: Reset to Defaults

```lisp
;;; Reset all config to defaults
(defun c:mdt-reset (/ response)
  (initget "Yes No")
  (setq response (getkword "\nReset all settings to defaults? [Yes/No]: "))
  (cond
    ((= response "Yes")
     ;; Clear cache forces reload from defaults
     (setq *haws-config-cache* 
       (vl-remove (assoc "MDT" *haws-config-cache*) *haws-config-cache*))
     (prompt "\nSettings reset to defaults.")))
  (princ)
)
```

---

## Troubleshooting

### Issue: Variables Return Nil

**Check registration**:
```lisp
Command: (assoc "MDT" *haws-config-definitions*)
```
Should return: `("MDT" . <definitions>)`

**Check variable exists**:
```lisp
Command: (mdt-get-var-scope "TemplateFile")
```
Should return: `2` (or appropriate scope code)

### Issue: Project Settings Don't Persist

**Check INI file path**:
```lisp
Command: (mdt-get-ini-path)
```
Should return valid path, not nil.

**Check INI file was created**:
```
Command: MDT-INFO
```
Look at INI File path, verify file exists.

**Manually check INI file**:
Open the INI file in notepad, verify section and variables exist.

### Issue: User Settings Don't Persist

**Check Visual LISP loaded**:
```lisp
Command: (haws-vlisp-p)
```
Should return: `T`

**Check Registry manually**:
- Run `regedit`
- Navigate to: `HKEY_CURRENT_USER\Software\HawsEDC\MDT`
- Verify keys exist

---

## Next Steps

Now that you have a working application:

1. **Add more variables** as needed for your application
2. **Create dialog boxes** (DCL) for configuration
3. **Add config validation** and error handling
4. **Implement config import/export** for team sharing
5. **Add config versioning** for upgrades
6. **Create unit tests** for your config functions

---

## Complete Example File

The complete `mydrawingtool.lsp` with all features is available at:
`examples/mydrawingtool-complete.lsp`

---

## Additional Resources

- [HAWS-CONFIG API Documentation](HAWS-CONFIG-API.md)
- [CNM Source Code](../devsource/cnm.lsp) - Real-world example
- [HAWS Config Source](../devsource/haws-config.lsp) - Implementation details

---

## Getting Help

If you have questions or run into issues:
- Check the [API Documentation](HAWS-CONFIG-API.md)
- Review CNM's implementation as a reference
- Open an issue on GitHub: https://github.com/hawstom/cnm/issues
- Tag with `haws-config` label

---

**Congratulations!** You've successfully integrated HAWS-CONFIG into your application. Your settings now persist across sessions, projects, and users with minimal code.
