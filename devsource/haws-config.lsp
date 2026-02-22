(princ "\nHaws-config functions ... ")

;; Reset configuration cache on load (clean slate for testing)
(setq *haws-config-cache* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HAWS-CONFIG - Generic Multi-Application Configuration System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PURPOSE:
;;; Generalized configuration system extracted from CNM (HCNM-CONFIG).
;;; Supports multiple applications sharing a common config infrastructure with
;;; app-specific INI files and settings isolation.
;;;
;;; SCOPE SYSTEM (Built-in, no longer app-defined):
;;; 0 - Session:  In-memory only (current AutoCAD session)
;;; 1 - Drawing:  Stored in drawing file (NOT YET IMPLEMENTED)
;;; 2 - Project:  Project-level INI file (e.g., cnm.ini)
;;; 3 - App:      Application install folder (NOT YET IMPLEMENTED)
;;; 4 - User:     User profile folder (Windows Registry or %APPDATA%)
;;;
;;; See "SCOPE DEFINITIONS (CANONICAL REFERENCE)" section below for detailed
;;; explanations of when to use each scope.
;;;
;;; ARCHITECTURE:
;;; - Multi-app cache: *haws-config-cache* stores config for all registered apps
;;; - Fallback chain: Memory → INI file → Defaults
;;; - App registration: Each app registers its variable list on load
;;;
;;; PUBLIC API:
;;; (haws-config-register-app app var-list) - Register an application
;;; (haws-config-getvar app var ini-path section) - Get config value
;;; (haws-config-setvar app var val ini-path section) - Set config value
;;;
;;; USAGE EXAMPLE:
;;; ;; Register app with variable definitions only (no boilerplate!)
;;; (haws-config-register-app "MyApp"
;;;   '(("UserSetting1" "A" 4)      ; User scope (Registry)
;;;     ("AppVersion" "1.0" 0)))    ; Session scope (Memory)
;;;
;;; ;; Get/Set values (scope auto-looked-up from definitions)
;;; (setq val (haws-config-getvar "MyApp" "UserSetting1" nil nil))  ; User scope
;;; (haws-config-setvar "MyApp" "UserSetting1" "B" nil nil)         ; User scope
;;; (setq val (haws-config-getvar "MyApp" "ProjectVar" (my-ini-path) "MyApp"))  ; Project scope
;;;
;;; CONVENTIONS:
;;; - All system functions use haws-config- prefix (lowercase with hyphens)
;;; - Apps MUST NOT use the haws-config- prefix for their own functions
;;;   to avoid confusion with system functions. Use {app}-config-definitions
;;;   (e.g., hcnm-config-definitions, myapp-config-definitions).
;;; - All local variables explicitly declared in function parameter list
;;; - Follows .github/copilot-instructions.md Section 1.2.3
;;;
;;; HISTORY:
;;; 2025-10-31 - Initial extraction from cnm.lsp HCNM-CONFIG system (Issue #11)
;;; 2025-11-03 - Removed redundant "Scope" section, scope codes now built-in
;;; 2025-11-03 - Auto-lookup scope from definitions (eliminated scope parameter)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *haws-config* - Unified configuration data structure
;;; Structure:
;;;   '(("Definitions"  ; App variable schemas
;;;      ("APP1" (("var1" "default1" scope-code) ...))
;;;      ("APP2" (...)))
;;;     ("Cache"        ; Runtime value cache (all scopes)
;;;      ("APP1" (("var1" "value1") ...))
;;;      ("APP2" (...)))
;;;     ("Session"      ; Session-scope storage (scope=0 only)
;;;      ("APP1" (("sessionvar1" "value1") ...))
;;;      ("APP2" (...))))
;;;
;;; SCOPE CODES (use integer literals with comments):
;;;   0 = Session  (in-memory only, current AutoCAD session)
;;;   1 = Drawing  (stored in drawing file - NOT YET IMPLEMENTED)
;;;   2 = Project  (project-level INI file)
;;;   3 = App      (application install folder - NOT YET IMPLEMENTED)
;;;   4 = User     (user profile - Windows Registry or %APPDATA%)
;;;
;;; See "SCOPE DEFINITIONS (CANONICAL REFERENCE)" section below for detailed
;;; explanations of when to use each scope.
;;;
(if (not *haws-config*)
  (setq *haws-config*
    (list
      (list "Definitions")    ; App variable schemas
      (list "Cache")          ; Runtime cache
      (list "Session")        ; Session-scope storage
      (list "ProjectRoots"))) ; Per-app project folder cache: dotted pairs (app . folder)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCOPE DEFINITIONS (CANONICAL REFERENCE)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SCOPE 0 - SESSION (In-Memory Only)
;;;   Storage:     *haws-config* "Session" section (memory only)
;;;   Lifetime:    Current AutoCAD session only
;;;   Persistence: Lost when AutoCAD closes
;;;   Use cases:   - Runtime calculated values (app version, app folder path)
;;;                - Temporary flags or state
;;;                - Performance caches that don't need persistence
;;;   Example:     (list "AppVersion" "1.0.0" 0)
;;;   Notes:       Fastest access (no disk/registry I/O)
;;;
;;; SCOPE 1 - DRAWING (NOT YET IMPLEMENTED)
;;;   Storage:     Drawing file extended data (XDATA)
;;;   Lifetime:    Saved with drawing
;;;   Persistence: Travels with the .dwg file
;;;   Use cases:   - Drawing-specific settings
;;;                - Per-drawing layer/style preferences
;;;                - Settings that should follow the drawing
;;;   Status:      Not implemented, reserved for future use
;;;
;;; SCOPE 2 - PROJECT (INI File)
;;;   Storage:     Project INI file (e.g., cnm.ini in project folder)
;;;   Lifetime:    Project lifetime (shared across all project drawings)
;;;   Persistence: Saved to disk in project folder
;;;   Use cases:   - Project-wide settings (all drawings in project)
;;;                - Team-shared configuration (if project folder is shared)
;;;                - Template paths, default layers, project standards
;;;   Example:     (list "TemplateFile" "standard.dwt" 2)
;;;   Notes:       Requires ini-path and section parameters in getvar/setvar
;;;                Location managed by app-specific project system
;;;
;;; SCOPE 3 - APP (NOT YET IMPLEMENTED)
;;;   Storage:     Application install folder (read-only for users)
;;;   Lifetime:    Application installation lifetime
;;;   Persistence: Saved with application (typically read-only)
;;;   Use cases:   - Factory defaults
;;;                - Vendor-provided templates
;;;                - System-wide settings (all users on machine)
;;;   Status:      Not implemented, reserved for future use
;;;
;;; SCOPE 4 - USER (Windows Registry)
;;;   Storage:     HKEY_CURRENT_USER\Software\HawsEDC\{app}
;;;   Lifetime:    User profile lifetime (persists across sessions)
;;;   Persistence: Windows Registry (per-user, per-machine)
;;;   Use cases:   - User preferences (UI settings, defaults)
;;;                - Per-user customizations
;;;                - Settings that persist across all projects
;;;   Example:     (list "ShowWelcome" "YES" 4)
;;;   Notes:       Uses vl-registry-read/write functions
;;;                Separate from project/drawing settings
;;;
;;; CHOOSING A SCOPE:
;;;   Ask: "Should this setting persist?"
;;;     NO  → Session (0)  - Calculated at runtime, temporary state
;;;     YES → Continue...
;;;   
;;;   Ask: "Should this setting travel with the drawing file?"
;;;     YES → Drawing (1)  - NOT YET IMPLEMENTED
;;;     NO  → Continue...
;;;   
;;;   Ask: "Is this setting specific to one project?"
;;;     YES → Project (2)  - Shared by team, project-specific
;;;     NO  → Continue...
;;;   
;;;   Ask: "Is this a user preference or system-wide?"
;;;     USER   → User (4)     - Personal preference, all projects
;;;     SYSTEM → App (3)      - NOT YET IMPLEMENTED (factory defaults)
;;;
;;; STORAGE MECHANISM SUMMARY:
;;;   Scope 0 (Session):  *haws-config* global variable (memory)
;;;   Scope 1 (Drawing):  Not implemented (would use XDATA)
;;;   Scope 2 (Project):  INI file via ini_readsection/ini_writeentry
;;;   Scope 3 (App):      Not implemented (would use app folder INI)
;;;   Scope 4 (User):     Windows Registry via vl-registry-read/write
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SECTION ACCESS HELPERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-get-section - Get a top-level section from *haws-config*
;;; Arguments:
;;;   section-name - Section name: "Definitions", "Cache", or "Session"
;;; Returns:
;;;   Section contents (list of app entries), or nil if not found
(defun haws-config-get-section (section-name / )
  (cdr (assoc section-name *haws-config*))
)

;;; haws-config-set-section - Update a top-level section in *haws-config*
;;; Arguments:
;;;   section-name - Section name: "Definitions", "Cache", or "Session"
;;;   new-contents - New contents for the section (list of app entries)
;;; Returns:
;;;   The new contents
;;; Side Effects:
;;;   Updates *haws-config* global
(defun haws-config-set-section (section-name new-contents / old-section)
  (setq old-section (assoc section-name *haws-config*))
  (cond
    (old-section
     ;; Section exists, replace it
     (setq *haws-config*
       (subst
         (cons section-name new-contents)
         old-section
         *haws-config*))
    )
    (t
     ;; Section doesn't exist, add it
     (setq *haws-config*
       (cons
         (cons section-name new-contents)
         *haws-config*))
    )
  )
  new-contents
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHE MANAGEMENT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-cache-get - Get value from cache for specific app
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Value string if found, nil otherwise
(defun haws-config-cache-get (app var / cache app-cache)
  (setq cache (haws-config-get-section "Cache"))
  (setq app-cache (assoc app cache))
  (if app-cache
    (cadr (assoc var (cdr app-cache)))
    nil
  )
)

;;; haws-config-cache-set - Set value in cache for specific app
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   val - Value to store (string)
;;; Returns:
;;;   The value that was set
(defun haws-config-cache-set (app var val / cache app-cache var-entry new-app-cache new-cache)
  (setq cache (haws-config-get-section "Cache"))
  (setq app-cache (assoc app cache))
  (cond
    (app-cache
     ;; App exists in cache, update or add variable
     (setq var-entry (assoc var (cdr app-cache)))
     (setq
       new-app-cache
        (cons
          app
          (cond
            (var-entry
             ;; Variable exists, update it
             (subst (list var val) var-entry (cdr app-cache))
            )
            (t
             ;; Variable doesn't exist, add it
             (cons (list var val) (cdr app-cache))
            )
          )
        )
     )
     (setq new-cache (subst new-app-cache app-cache cache))
    )
    (t
     ;; App doesn't exist in cache, create it
     (setq new-cache (cons (list app (list var val)) cache))
    )
  )
  (haws-config-set-section "Cache" new-cache)
  val
)

;;; haws-config-cache-get-app - Get entire app cache
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   List of (var val) pairs for app, or nil
(defun haws-config-cache-get-app (app / cache app-cache)
  (setq cache (haws-config-get-section "Cache"))
  (setq app-cache (assoc app cache))
  (if app-cache
    (cdr app-cache)
    nil
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCOPE HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-get-definitions - Get variable definitions for specific app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   Variable list for app: '(("var1" "default1" scope-code) ...)
;;;   Returns nil if not registered
(defun haws-config-get-definitions (app / definitions)
  (setq definitions (haws-config-get-section "Definitions"))
  (cdr (assoc app definitions))
)

;;; haws-config-entry-var - Extract variable name from config entry
;;; Arguments:
;;;   entry - Config entry: (var-name default-value scope-code)
;;; Returns:
;;;   Variable name (string)
(defun haws-config-entry-var (entry / )
  (car entry)
)

;;; haws-config-entry-val - Extract default value from config entry
;;; Arguments:
;;;   entry - Config entry: (var-name default-value scope-code)
;;; Returns:
;;;   Default value (string)
(defun haws-config-entry-val (entry / )
  (cadr entry)
)

;;; haws-config-entry-scope-code - Extract scope code from config entry
;;; Arguments:
;;;   entry - Config entry: (var-name default-value scope-code)
;;; Returns:
;;;   Scope code (integer 0-4)
(defun haws-config-entry-scope-code (entry / )
  (caddr entry)
)

;;; haws-config-entry-strip-scope - Remove scope code from entry
;;; Arguments:
;;;   entry - Config entry: (var-name default-value scope-code)
;;; Returns:
;;;   Entry without scope: (var-name default-value)
(defun haws-config-entry-strip-scope (entry / )
  (reverse (cdr (reverse entry)))
)

;;; haws-config-scope-eq - Check if variable belongs to specific scope
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   scope-code - Numeric scope code (0-4)
;;; Returns:
;;;   T if variable is in specified scope, nil otherwise
(defun haws-config-scope-eq (app var scope-code / definitions var-entry)
  (setq definitions (haws-config-get-definitions app))
  (setq var-entry (assoc var definitions))
  (=
    (haws-config-entry-scope-code var-entry)
    scope-code
  )
)

;;; haws-config-get-default - Get default value for variable
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Default value (string) or nil
(defun haws-config-get-default (app var / definitions)
  (setq definitions (haws-config-get-definitions app))
  (haws-config-entry-val (assoc var definitions))
)

;;; haws-config-get-scope - Get scope code for variable from definitions
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Scope code (integer 0-4) or nil if not found
(defun haws-config-get-scope (app var / definitions var-entry)
  (setq definitions (haws-config-get-definitions app))
  (setq var-entry (assoc var definitions))
  (if var-entry
    (haws-config-entry-scope-code var-entry)
    nil
  )
)

;;; haws-config-defaults-single-scope - Get all defaults for one scope
;;; Arguments:
;;;   app - Application identifier (string)
;;;   scope-code - Numeric scope code (0-4)
;;; Returns:
;;;   List of (var default) pairs for specified scope
(defun haws-config-defaults-single-scope (app scope-code / scope-list definitions entry)
  (setq definitions (haws-config-get-definitions app))
  (foreach
    entry
     definitions
    (cond
      ((= (haws-config-entry-scope-code entry) scope-code)
       (setq
         scope-list
          (cons
            (haws-config-entry-strip-scope entry)
            scope-list
          )
       )
      )
    )
  )
  (reverse scope-list)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCOPE READ/WRITE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-read-user - Read variable from User scope (Windows Registry)
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Value from registry, or nil if not found
(defun haws-config-read-user (app var / registry-path)
  (setq registry-path (strcat "HKEY_CURRENT_USER\\Software\\HawsEDC\\" app))
  (cond
    ((haws-vlisp-p)
     (vl-registry-read registry-path var)
    )
    (t nil)
  )
)

;;; haws-config-write-user - Write variable to User scope (Windows Registry)
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   val - Value to write (string)
;;; Returns:
;;;   Value that was written
(defun haws-config-write-user (app var val / registry-path)
  (setq registry-path (strcat "HKEY_CURRENT_USER\\Software\\HawsEDC\\" app))
  (cond
    ((haws-vlisp-p)
     (vl-registry-write registry-path var val)
    )
  )
  val
)

;;; haws-config-read-session - Read variable from Session scope (in-memory)
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Value from session cache, or nil if not found
(defun haws-config-read-session (app var / session app-session)
  (setq session (haws-config-get-section "Session"))
  (setq app-session (assoc app session))
  (if app-session
    (cadr (assoc var (cdr app-session)))
    nil
  )
)

;;; haws-config-write-session - Write variable to Session scope (in-memory)
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   val - Value to write (string)
;;; Returns:
;;;   Value that was written
(defun haws-config-write-session (app var val / session app-session var-entry new-app-session new-session)
  (setq session (haws-config-get-section "Session"))
  (setq app-session (assoc app session))
  (cond
    (app-session
     ;; App exists in session, update or add variable
     (setq var-entry (assoc var (cdr app-session)))
     (setq
       new-app-session
        (cons
          app
          (cond
            (var-entry
             ;; Variable exists, update it
             (subst (list var val) var-entry (cdr app-session))
            )
            (t
             ;; Variable doesn't exist, add it
             (cons (list var val) (cdr app-session))
            )
          )
        )
     )
     (setq new-session (subst new-app-session app-session session))
    )
    (t
     ;; App doesn't exist in session, create it
     (setq new-session (cons (list app (list var val)) session))
    )
  )
  (haws-config-set-section "Session" new-session)
  val
)

;;; haws-config-read-all-user - Read all User-scope variables for an app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   List of (var val) pairs from User scope
(defun haws-config-read-all-user (app / entry var val)
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (haws-config-entry-var entry)
         val (haws-config-read-user app var)
       )
       (list var val)
     )
    (haws-config-defaults-single-scope app 4)  ; 4 = User scope
  )
)

;;; haws-config-read-all-session - Read all Session-scope variables for an app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   List of (var val) pairs from Session scope
(defun haws-config-read-all-session (app / entry var val)
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (haws-config-entry-var entry)
         val (haws-config-read-session app var)
       )
       (list var val)
     )
    (haws-config-defaults-single-scope app 0)  ; 0 = Session scope
  )
)

;;; haws-config-read-all-project - Read all Project-scope variables for an app
;;; NOTE: This function is app-specific and needs to be implemented by each app
;;; because project INI file location depends on app-specific project management.
;;; CNM will provide its own wrapper that calls ini_readsection with proper path.
;;;
;;; Arguments:
;;;   app - Application identifier (string)
;;;   ini-path - Full path to project INI file (string)
;;;   section - INI section name (string, typically same as app)
;;; Returns:
;;;   List of (var val) pairs from Project scope
(defun haws-config-read-all-project (app ini-path section / ini-configs entry var val file-io-start)
  (setq file-io-start (haws-clock-start "config-file-io"))
  (setq ini-configs (ini_readsection ini-path section))
  (haws-clock-end "config-file-io" file-io-start)
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (haws-config-entry-var entry)
         val (haws-config-entry-val (assoc var ini-configs))
       )
       (list var val)
     )
    (haws-config-defaults-single-scope app 2)  ; 2 = Project scope
  )
)

;;; haws-config-write-project - Write variable to Project scope (INI file)
;;; Arguments:
;;;   app - Application identifier (string)
;;;   ini-path - Full path to project INI file (string)
;;;   section - INI section name (string, typically same as app)
;;;   var - Variable name (string)
;;;   val - Value to write (string)
;;; Returns:
;;;   Value that was written
(defun haws-config-write-project (app ini-path section var val / )
  (ini_writeentry ini-path section var val)
  val
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PUBLIC API FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-register-app - Register an application with the config system
;;; Arguments:
;;;   app - Application identifier (string, e.g. "CNM", "HAWS-QT")
;;;   definitions - List of variable definitions: '(("var1" "default1" scope-code) ...)
;;;                 Each entry is (name default-value scope-code)
;;;                 Scope codes: 0=Session, 2=Project, 4=User
;;; Returns:
;;;   T on success, nil if already registered
;;; Side Effects:
;;;   Stores definitions in *haws-config* "Definitions" section
;;;   Initializes app entry in "Cache" and "Session" sections
;;;
;;; EXAMPLE:
;;;   (haws-config-register-app "MyApp"
;;;     '(("UserSetting1" "A" 4)      ; 4=User (Registry)
;;;       ("AppVersion" "1.0" 0)))    ; 0=Session (Memory)
;;;
;;; NOTE: Safe to call multiple times - checks if already registered
(defun haws-config-register-app (app definitions / defs-section cache-section session-section)
  ;; Check if already registered
  (setq defs-section (haws-config-get-section "Definitions"))
  (cond
    ((assoc app defs-section)
     ;; Already registered, return nil
     nil
    )
    (t
     ;; Not registered, proceed with registration
     (setq cache-section (haws-config-get-section "Cache"))
     (setq session-section (haws-config-get-section "Session"))
     
     ;; Store variable definitions
     (setq defs-section (cons (cons app definitions) defs-section))
     (haws-config-set-section "Definitions" defs-section)
     
     ;; Initialize app cache
     (setq cache-section (cons (list app) cache-section))
     (haws-config-set-section "Cache" cache-section)
     
     ;; Initialize app session cache
     (setq session-section (cons (list app) session-section))
     (haws-config-set-section "Session" session-section)
     
     T
    )
  )
)

;;; haws-config-getvar - Get configuration variable value
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   ini-path - For Project scope: full path to INI file (nil for Session/User scopes)
;;;   section - For Project scope: INI section name (nil for Session/User scopes)
;;; Returns:
;;;   Variable value (string), or default if not found
;;; Fallback Chain:
;;;   1. Check cache
;;;   2. If not in cache, read from scope storage
;;;   3. If not in storage, use default from definitions
;;;   4. Store in cache and return
;;; Notes:
;;;   Scope is auto-looked-up from variable definitions (no longer passed as parameter)
(defun haws-config-getvar (app var ini-path section / val setvar-p scope-code start cache-start)
  (setq start (haws-clock-start "config-getvar-total"))
  (setq setvar-p t)
  ;; Try cache first — no scope lookup needed on hit
  (setq cache-start (haws-clock-start "config-cache-check"))
  (setq val (haws-config-cache-get app var))
  (haws-clock-end "config-cache-check" cache-start)
  (cond
    ;; Cache hit: skip scope lookup entirely
    (val
     (setq setvar-p nil)
    )
    ;; Cache miss: auto-lookup scope and load from storage
    (t
     (setq scope-code (haws-config-get-scope app var))
     (if (not scope-code)
       (progn
         (alert
           (princ
             (strcat
               "Fatal error in HAWS-CONFIG-GETVAR:\nVariable not registered\n"
               "App: " app "\nVar: " var
             )
           )
         )
         (setq scope-code 0)  ; Fallback to Session to avoid crash
       )
     )
     (cond
       ;; Session scope
       ((= scope-code 0)
        (setq val (haws-config-read-session app var))
        (if (not val)
          ;; Not in session yet, use default
          (setq val (haws-config-get-default app var))
        )
       )
       ;; Project scope — batch load all project vars into cache
       ((= scope-code 2)
        (if (and ini-path section)
          (progn
            (foreach
              pair
              (haws-config-read-all-project app ini-path section)
              (haws-config-cache-set app (car pair) (cadr pair))
            )
            (setq val (haws-config-cache-get app var))
            (setq setvar-p nil)  ; batch already populated cache
          )
          ;; No INI path provided, use default
          (setq val (haws-config-get-default app var))
        )
       )
       ;; User scope — batch load all user vars into cache
       ((= scope-code 4)
        (foreach
          pair
          (haws-config-read-all-user app)
          (haws-config-cache-set app (car pair) (cadr pair))
        )
        (setq val (haws-config-cache-get app var))
        (setq setvar-p nil)  ; cache already populated by foreach
       )
       ;; Other scopes not yet implemented
       (t
        (setq val (haws-config-get-default app var))
       )
     )
    )
  )
  ;; If we don't have a value yet, use default
  (if (not val)
    (setq val (haws-config-get-default app var))
  )
  ;; If we still don't have a value, that's a fatal error
  (if (not val)
    (alert
      (princ
        (strcat
          "Fatal error in HAWS-CONFIG:\nCould not initialize variable\n"
          "App: " app "\nVar: " var
        )
      )
    )
  )
  ;; Store in cache if needed (only for non-batch paths)
  (if (and setvar-p val)
    (haws-config-cache-set app var val)
  )
  (haws-clock-end "config-getvar-total" start)
  val
)

;;; haws-config-setvar - Set configuration variable value
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   val - Value to set (string)
;;;   ini-path - For Project scope: full path to INI file (nil for Session/User scopes)
;;;   section - For Project scope: INI section name (nil for Session/User scopes)
;;; Returns:
;;;   The value that was set
;;; Side Effects:
;;;   Updates cache
;;;   Writes to appropriate scope storage (Registry, INI file, or session memory)
;;; Notes:
;;;   Scope is auto-looked-up from variable definitions (no longer passed as parameter)
(defun haws-config-setvar (app var val ini-path section / scope-code)
  ;; Auto-lookup scope from definitions
  (setq scope-code (haws-config-get-scope app var))
  (if (not scope-code)
    (progn
      (alert
        (princ
          (strcat
            "Fatal error in HAWS-CONFIG-SETVAR:\nVariable not registered\n"
            "App: " app "\nVar: " var
          )
        )
      )
      (setq scope-code 0)  ; Fallback to Session to avoid crash
    )
  )
  
  ;; Always update cache
  (haws-config-cache-set app var val)
  
  ;; Persist to appropriate scope storage
  (cond
    ;; Session scope - already handled by cache
    ((= scope-code 0)
     (haws-config-write-session app var val)
    )
    ;; Project scope - write to INI file
    ((= scope-code 2)
     (if (and ini-path section)
       (haws-config-write-project app ini-path section var val)
     )
    )
    ;; User scope - write to registry
    ((= scope-code 4)
     (haws-config-write-user app var val)
    )
    ;; Other scopes not yet implemented
  )
  
  val
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROJECT MANAGEMENT SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic project folder resolver for multi-app use.
;;;
;;; CONCEPTS:
;;;   Local project:  app.ini exists in drawing's folder → that folder is root
;;;   Linked project: appproj.txt exists → contains path to a different root folder
;;;   Ambiguous:      Both exist → error
;;;
;;; CONVENTION: Each app registered with haws-config-proj MUST define a
;;;   Session-scope variable named "AppFolder" in its definitions.
;;;
;;; *haws-config* "ProjectRoots" section holds dotted pairs (app . folder):
;;;   '(("CNM" . "C:\\projects\\foo") ("APP2" . "D:\\work\\bar"))
;;;

;;; haws-config-get-proj-root - Get cached project root for app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   Project root path (string) or nil
(defun haws-config-get-proj-root (app / roots)
  (setq roots (haws-config-get-section "ProjectRoots"))
  (cdr (assoc app roots))
)

;;; haws-config-set-proj-root - Cache project root for app
;;; Arguments:
;;;   app    - Application identifier (string)
;;;   folder - Project root path (string)
;;; Returns:
;;;   folder
(defun haws-config-set-proj-root (app folder / roots old-entry)
  (setq roots (haws-config-get-section "ProjectRoots"))
  (setq old-entry (assoc app roots))
  (cond
    (old-entry
     (setq roots (subst (cons app folder) old-entry roots))
    )
    (t
     (setq roots (cons (cons app folder) roots))
    )
  )
  (haws-config-set-section "ProjectRoots" roots)
  folder
)

;;; haws-config-project-ini-name - Get INI filename for app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   Filename string, e.g. "cnm.ini"
(defun haws-config-project-ini-name (app / )
  (strcat (strcase app T) ".ini")
)

;;; haws-config-project-link-name - Get link filename for app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   Filename string, e.g. "cnmproj.txt"
(defun haws-config-project-link-name (app / )
  (strcat (strcase app T) "proj.txt")
)

;;; haws-config-project-folder-to-ini - Build INI path from folder
;;; Arguments:
;;;   app    - Application identifier (string)
;;;   folder - Project folder path (string)
;;; Returns:
;;;   Full path to INI file (string)
(defun haws-config-project-folder-to-ini (app folder / )
  (strcat folder "\\" (haws-config-project-ini-name app))
)

;;; haws-config-project-folder-to-link - Build link file path from folder
;;; Arguments:
;;;   app    - Application identifier (string)
;;;   folder - Folder path (string)
;;; Returns:
;;;   Full path to link file (string)
(defun haws-config-project-folder-to-link (app folder / )
  (strcat folder "\\" (haws-config-project-link-name app))
)

;;; haws-config-local-project-marker - Check for local INI in a folder
;;; Arguments:
;;;   app    - Application identifier (string)
;;;   dwgdir - Drawing's folder path (string)
;;; Returns:
;;;   Full path to INI if found, nil otherwise
(defun haws-config-local-project-marker (app dwgdir / )
  (findfile (haws-config-project-folder-to-ini app dwgdir))
)

;;; haws-config-linked-project-marker - Check for link file in a folder
;;; Arguments:
;;;   app    - Application identifier (string)
;;;   dwgdir - Drawing's folder path (string)
;;; Returns:
;;;   Full path to link file if found, nil otherwise
(defun haws-config-linked-project-marker (app dwgdir / )
  (findfile (haws-config-project-folder-to-link app dwgdir))
)

;;; haws-config-check-moved-project - Warn if INI was copied from another project
;;; Arguments:
;;;   app              - Application identifier (string)
;;;   project-ini-file - Full path to app's INI file (string)
;;; Returns:
;;;   nil (may call exit on user refusal)
(defun haws-config-check-moved-project (app project-ini-file / input1 pnname thisfile-value)
  (cond
    ((and
       (setq
         thisfile-value
          (ini_readentry project-ini-file (strcase app) "ThisFile")
       )
       (setq
         pnname
          (ini_readentry project-ini-file (strcase app) "ProjectNotes")
       )
       (/= thisfile-value "")
       (/= thisfile-value project-ini-file)
     )
     (alert
       (princ
         (strcat
           "Warning!\nYou are using these project notes:\n\n" pnname
           "\n\nand the " (haws-config-project-ini-name app) " for this folder says \n\"ThisFile=\""
           thisfile-value
           "\n\nIt appears it may have been copied from another project."
           "\nYou may be about to edit the wrong Project Notes file."
          )
       )
     )
     (initget "Yes No")
     (setq input1 (getkword "\nContinue with this file? [Yes/No]: "))
     (cond
       ((= input1 "Yes")
        (ini_writeentry project-ini-file (strcase app) "ThisFile" "")
       )
       (t (exit))
     )
    )
  )
)

;;; haws-config-assure-local-project - Validate local project marker
;;; Arguments:
;;;   app          - Application identifier (string)
;;;   local-marker - Full path to local INI file (string)
;;; Returns:
;;;   Project folder path (string)
(defun haws-config-assure-local-project (app local-marker / )
  (haws-config-check-moved-project app local-marker)
  (haws-filename-directory local-marker)
)

;;; haws-config-assure-linked-project - Validate linked project marker
;;; Arguments:
;;;   app           - Application identifier (string)
;;;   linked-marker - Full path to link file (string)
;;; Returns:
;;;   Project folder path (string) or nil
(defun haws-config-assure-linked-project (app linked-marker / f1 projroot rdlin)
  (cond
    ((and
       (setq f1 (haws-open linked-marker "r"))
       (progn
         (while (and (setq rdlin (read-line f1)) (not projroot))
           (cond
             ((haws-vlisp-p)
              (if (vl-file-directory-p rdlin)
                (setq projroot rdlin)
              )
             )
             (t
              (if (/= ";" (substr rdlin 1 1))
                (setq projroot rdlin)
              )
             )
           )
         )
         (haws-close f1)
         projroot
       )
     )
    )
  )
  (if (not (findfile (haws-config-project-folder-to-ini app projroot)))
    (haws-config-initialize-project app projroot)
  )
  (haws-config-check-moved-project
    app
    (haws-config-project-folder-to-ini app projroot)
  )
  (princ
    (strcat
      "\nUsing project settings from another folder as directed by "
      (haws-config-project-link-name app)
      " in this drawing's folder."
      " Project settings located at "
      projroot
      "\\" (haws-config-project-ini-name app)
      "."
    )
  )
  projroot
)

;;; haws-config-initialize-project - Create project INI for a new project folder
;;; Arguments:
;;;   app  - Application identifier (string)
;;;   proj - Project folder path (string)
;;; Returns:
;;;   Full path to project INI file (string)
;;; Side effects:
;;;   Copies app-folder template INI, or writes hard-coded defaults
;;;   Always writes ThisFile entry to projini
(defun haws-config-initialize-project (app proj / appfolder appini projini mark-file-p)
  (setq projini (haws-config-project-folder-to-ini app proj))
  (setq appfolder (haws-config-getvar app "AppFolder" nil nil))
  (cond
    ((and
       appfolder
       (setq appini (findfile (haws-config-project-folder-to-ini app appfolder)))
     )
     (if (not (haws-file-copy appini projini))
       (progn
         (alert
           (princ
             (strcat
               "Fatal error:\n\nThis drawing must be saved before "
               app
               " can be used.\n" app " cannot continue."
             )
           )
         )
         (exit)
       )
     )
     (alert
       (princ
         (strcat
           app " is copying settings found in\n" appini "\nto\n" projini
           "\nfor this project."
          )
       )
     )
     (while (not (findfile projini)))
     (setq mark-file-p t)
     projini
    )
    (t
     (alert
       (princ
         (strcat
           app " could not find a settings file in\n" appfolder
           "\n\nPutting hard-coded defaults in\n" projini
           "\nfor this project."
          )
       )
     )
     (foreach entry (haws-config-defaults-single-scope app 2)
       (ini_writeentry projini (strcase app) (car entry) (cadr entry))
     )
     (setq mark-file-p t)
     projini
    )
  )
  (cond
    (mark-file-p
     (ini_writeentry projini (strcase app) "ThisFile" projini)
    )
  )
)

;;; haws-config-proj - Resolve the project root folder for the current drawing
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   Project root folder path (string)
;;; Side effects:
;;;   Caches result in *haws-config* ProjectRoots section
;;;   May create new project INI via haws-config-initialize-project
(defun haws-config-proj (app / dwgdir linked-project-folder linked-project-marker
                     local-project-folder local-project-marker
                    )
  (setq
    dwgdir
     (haws-filename-directory (getvar "dwgprefix"))
    local-project-marker
     (haws-config-local-project-marker app dwgdir)
    linked-project-marker
     (haws-config-linked-project-marker app dwgdir)
  )
  (cond
    (local-project-marker
     (setq
       local-project-folder
        (haws-config-assure-local-project app local-project-marker)
     )
    )
  )
  (cond
    (linked-project-marker
     (setq
       linked-project-folder
        (haws-config-assure-linked-project app linked-project-marker)
     )
    )
  )
  (haws-config-set-proj-root
    app
    (cond
      ;; If project root already cached this session, use it
      ((haws-config-get-proj-root app))
      ;; Ambiguous: both local INI and link file exist
      ((and local-project-marker linked-project-marker)
       (alert
         (princ
           (strcat
             "Error:\nThis drawing's folder\n" local-project-folder
             "\nhas both its own project settings ("
             (haws-config-project-ini-name app)
             ") and a link ("
             (haws-config-project-link-name app)
             ") to a project in another folder:\n"
             linked-project-folder
             "\n\n" app
             " cannot continue. File names will be printed to the command history."
            )
         )
       )
       (princ
         (strcat
           "\nLocal project: "
           (haws-config-project-folder-to-ini app local-project-folder)
         )
       )
       (princ
         (strcat
           "\nLink to another project: "
           (haws-config-project-folder-to-link app local-project-folder)
         )
       )
       (exit)
      )
      ;; Simple single-folder project: INI is in this folder
      (local-project-marker
       (haws-config-assure-local-project app local-project-marker)
      )
      ;; Multi-folder project: link file points to another folder
      (linked-project-marker
       (haws-config-assure-linked-project app linked-project-marker)
      )
      ;; New folder: initialize project here
      (t
       (alert
         (princ
           (strcat "This drawing's folder is new to " app ".")
         )
       )
       (haws-config-initialize-project app dwgdir)
       dwgdir
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TEMP CONFIG SYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Purpose: Buffer pending dialog changes without writing to INI/registry.
;;; Usage:   Dialog opens → temp-setvar → OK → temp-save; Cancel → temp-clear
;;;
;;; *haws-config-temp* structure:
;;;   '(("CNM" (("var1" "val1") ("var2" "val2") ...))
;;;     ("APP2" (...)) ...)
;;;
(if (not *haws-config-temp*)
  (setq *haws-config-temp* nil)
)

;;; haws-config-temp-setvar - Store a pending change for an app
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   val - Pending value (string)
;;; Returns:
;;;   val
(defun haws-config-temp-setvar (app var val / temp-app var-entry new-app new-temp)
  (setq temp-app (assoc app *haws-config-temp*))
  (cond
    (temp-app
     ;; App entry exists — update or add variable
     (setq var-entry (assoc var (cdr temp-app)))
     (setq
       new-app
        (cons
          app
          (cond
            (var-entry
             (subst (list var val) var-entry (cdr temp-app))
            )
            (t
             (cons (list var val) (cdr temp-app))
            )
          )
        )
     )
     (setq *haws-config-temp* (subst new-app temp-app *haws-config-temp*))
    )
    (t
     ;; No entry for this app yet — create one
     (setq *haws-config-temp*
       (cons (list app (list var val)) *haws-config-temp*)
     )
    )
  )
  val
)

;;; haws-config-temp-getvar - Get pending value, falling back to real value
;;; Arguments:
;;;   app      - Application identifier (string)
;;;   var      - Variable name (string)
;;;   ini-path - For Project scope: full path to INI file (nil otherwise)
;;;   section  - For Project scope: INI section name (nil otherwise)
;;; Returns:
;;;   Pending value if set, otherwise (haws-config-getvar app var ini-path section)
(defun haws-config-temp-getvar (app var ini-path section / temp-app)
  (setq temp-app (assoc app *haws-config-temp*))
  (cond
    ((and temp-app (cadr (assoc var (cdr temp-app))))
     (cadr (assoc var (cdr temp-app)))
    )
    (t
     (haws-config-getvar app var ini-path section)
    )
  )
)

;;; haws-config-temp-save - Write all pending changes for an app to real storage
;;; Arguments:
;;;   app      - Application identifier (string)
;;;   ini-path - For Project scope: full path to INI file
;;;   section  - For Project scope: INI section name
;;; Returns:
;;;   nil
(defun haws-config-temp-save (app ini-path section / temp-app)
  (setq temp-app (assoc app *haws-config-temp*))
  (if temp-app
    (foreach entry (cdr temp-app)
      (haws-config-setvar
        app
        (haws-config-entry-var entry)
        (haws-config-entry-val entry)
        ini-path
        section
      )
    )
  )
)

;;; haws-config-temp-clear - Discard all pending changes for an app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   nil
(defun haws-config-temp-clear (app / temp-app)
  (setq temp-app (assoc app *haws-config-temp*))
  (if temp-app
    (setq *haws-config-temp* (vl-remove temp-app *haws-config-temp*))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF HAWS-CONFIG LIBRARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "loaded.")
(princ)