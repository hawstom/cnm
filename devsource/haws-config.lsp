;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HAWS-CONFIG - Generic Multi-Application Configuration System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PURPOSE:
;;; Generalized configuration system extracted from CNM (HCNM-CONFIG).
;;; Supports multiple applications sharing a common config infrastructure with
;;; app-specific INI files and settings isolation.
;;;
;;; SCOPE SYSTEM:
;;; 0 - Session:  In-memory only (current AutoCAD session)
;;; 1 - Drawing:  Stored in drawing file (NOT YET IMPLEMENTED)
;;; 2 - Project:  Project-level INI file (e.g., cnm.ini)
;;; 3 - App:      Application install folder (NOT YET IMPLEMENTED)
;;; 4 - User:     User profile folder (Windows Registry or %APPDATA%)
;;;
;;; ARCHITECTURE:
;;; - Multi-app cache: *HAWS-CONFIG:CACHE* stores config for all registered apps
;;; - Fallback chain: Memory → INI file → Defaults
;;; - App registration: Each app registers its config schema on load
;;;
;;; PUBLIC API:
;;; (HAWS-CONFIG:REGISTER-APP app definitions) - Register an application
;;; (HAWS-CONFIG:GETVAR app var scope defaults) - Get config value
;;; (HAWS-CONFIG:SETVAR app var val scope) - Set config value
;;;
;;; USAGE EXAMPLE:
;;; ;; Register app
;;; (HAWS-CONFIG:REGISTER-APP "CNM" (hcnm-config-definitions))
;;;
;;; ;; Get/Set values
;;; (SETQ val (HAWS-CONFIG:GETVAR "CNM" "BubbleTextPrefixSta" 2 "STA "))
;;; (HAWS-CONFIG:SETVAR "CNM" "BubbleTextPrefixSta" "STATION " 2)
;;;
;;; CONVENTIONS:
;;; - All public functions use HAWS-CONFIG: namespace prefix
;;; - All private/helper functions use haws-config- prefix (lowercase)
;;; - All local variables explicitly declared in function parameter list
;;; - Follows .github/copilot-instructions.md Section 1.2.3
;;;
;;; HISTORY:
;;; 2025-10-31 - Initial extraction from cnm.lsp HCNM-CONFIG system (Issue #11)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; *HAWS-CONFIG:CACHE* - Multi-app configuration cache
;;; Structure: '(("APP1" (("var1" "val1") ("var2" "val2") ...))
;;;              ("APP2" (("varA" "valA") ...)))
(if (not *HAWS-CONFIG:CACHE*) (setq *HAWS-CONFIG:CACHE* '()))

;;; *HAWS-CONFIG:DEFINITIONS* - Registered app definitions
;;; Structure: '(("APP1" . <definitions-list>)
;;;              ("APP2" . <definitions-list>))
(if (not *HAWS-CONFIG:DEFINITIONS*) (setq *HAWS-CONFIG:DEFINITIONS* '()))

;;; *HAWS-CONFIG:SESSION* - Session-scope cache (scope 0)
;;; Structure: Same as CACHE, but for session-only variables
(if (not *HAWS-CONFIG:SESSION*) (setq *HAWS-CONFIG:SESSION* '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CACHE MANAGEMENT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-cache-get - Get value from cache for specific app
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Value string if found, nil otherwise
(defun haws-config-cache-get (app var / app-cache)
  (setq app-cache (assoc app *HAWS-CONFIG:CACHE*))
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
(defun haws-config-cache-set (app var val / app-cache var-entry new-app-cache)
  (setq app-cache (assoc app *HAWS-CONFIG:CACHE*))
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
     (setq
       *HAWS-CONFIG:CACHE*
        (subst new-app-cache app-cache *HAWS-CONFIG:CACHE*)
     )
    )
    (t
     ;; App doesn't exist in cache, create it
     (setq
       *HAWS-CONFIG:CACHE*
        (cons
          (list app (list var val))
          *HAWS-CONFIG:CACHE*
        )
     )
    )
  )
  val
)

;;; haws-config-cache-get-app - Get entire app cache
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   List of (var val) pairs for app, or nil
(defun haws-config-cache-get-app (app / app-cache)
  (setq app-cache (assoc app *HAWS-CONFIG:CACHE*))
  (if app-cache
    (cdr app-cache)
    nil
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCOPE HELPER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; haws-config-get-definitions - Get definitions for specific app
;;; Arguments:
;;;   app - Application identifier (string)
;;; Returns:
;;;   Definitions list for app, or nil if not registered
(defun haws-config-get-definitions (app / )
  (cdr (assoc app *HAWS-CONFIG:DEFINITIONS*))
)

;;; haws-config-scope-code - Convert scope name to numeric code
;;; Arguments:
;;;   app - Application identifier (string)
;;;   scope-key - Scope name: "Session" "Drawing" "Project" "App" "User"
;;; Returns:
;;;   Numeric scope code (0-4)
(defun haws-config-scope-code (app scope-key / definitions)
  (setq definitions (haws-config-get-definitions app))
  (cadr
    (assoc
      scope-key
      (cdr (assoc "Scope" definitions))
    )
  )
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
;;;   scope-key - Scope name: "Session" "Drawing" "Project" "App" "User"
;;; Returns:
;;;   T if variable is in specified scope, nil otherwise
(defun haws-config-scope-eq (app var scope-key / definitions var-entry)
  (setq definitions (haws-config-get-definitions app))
  (setq var-entry (assoc var (cdr (assoc "Var" definitions))))
  (=
    (haws-config-entry-scope-code var-entry)
    (haws-config-scope-code app scope-key)
  )
)

;;; haws-config-get-default - Get default value for variable
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;; Returns:
;;;   Default value (string) or nil
(defun haws-config-get-default (app var / definitions defaults)
  (setq definitions (haws-config-get-definitions app))
  (setq
    defaults
     (mapcar
       'haws-config-entry-strip-scope
       (cdr (assoc "Var" definitions))
     )
  )
  (haws-config-entry-val (assoc var defaults))
)

;;; haws-config-defaults-single-scope - Get all defaults for one scope
;;; Arguments:
;;;   app - Application identifier (string)
;;;   scope-key - Scope name: "Session" "Drawing" "Project" "App" "User"
;;; Returns:
;;;   List of (var default) pairs for specified scope
(defun haws-config-defaults-single-scope (app scope-key / scope-code scope-list definitions entry)
  (setq
    scope-code (haws-config-scope-code app scope-key)
    definitions (haws-config-get-definitions app)
  )
  (foreach
    entry
     (cdr (assoc "Var" definitions))
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
(defun haws-config-read-session (app var / app-session)
  (setq app-session (assoc app *HAWS-CONFIG:SESSION*))
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
(defun haws-config-write-session (app var val / app-session var-entry new-app-session)
  (setq app-session (assoc app *HAWS-CONFIG:SESSION*))
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
     (setq
       *HAWS-CONFIG:SESSION*
        (subst new-app-session app-session *HAWS-CONFIG:SESSION*)
     )
    )
    (t
     ;; App doesn't exist in session, create it
     (setq
       *HAWS-CONFIG:SESSION*
        (cons
          (list app (list var val))
          *HAWS-CONFIG:SESSION*
        )
     )
    )
  )
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
    (haws-config-defaults-single-scope app "User")
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
    (haws-config-defaults-single-scope app "Session")
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
(defun haws-config-read-all-project (app ini-path section / ini-configs entry var val)
  (setq ini-configs (ini_readsection ini-path section))
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (haws-config-entry-var entry)
         val (haws-config-entry-val (assoc var ini-configs))
       )
       (list var val)
     )
    (haws-config-defaults-single-scope app "Project")
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

;;; HAWS-CONFIG:REGISTER-APP - Register an application with the config system
;;; Arguments:
;;;   app - Application identifier (string, e.g. "CNM", "HAWS-QT")
;;;   definitions - Config definitions list (same format as hcnm-config-definitions)
;;; Returns:
;;;   T on success
;;; Side Effects:
;;;   Stores definitions in *HAWS-CONFIG:DEFINITIONS*
;;;   Initializes app entry in *HAWS-CONFIG:CACHE*
(defun HAWS-CONFIG:REGISTER-APP (app definitions / )
  ;; Store definitions
  (setq
    *HAWS-CONFIG:DEFINITIONS*
     (cons
       (cons app definitions)
       *HAWS-CONFIG:DEFINITIONS*
     )
  )
  ;; Initialize app cache if not exists
  (if (not (assoc app *HAWS-CONFIG:CACHE*))
    (setq
      *HAWS-CONFIG:CACHE*
       (cons
         (list app)  ; Empty var list initially
         *HAWS-CONFIG:CACHE*
       )
    )
  )
  ;; Initialize app session cache if not exists
  (if (not (assoc app *HAWS-CONFIG:SESSION*))
    (setq
      *HAWS-CONFIG:SESSION*
       (cons
         (list app)  ; Empty var list initially
         *HAWS-CONFIG:SESSION*
       )
    )
  )
  T
)

;;; HAWS-CONFIG:GETVAR - Get configuration variable value
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   scope-code - Scope code (0=Session, 1=Drawing, 2=Project, 3=App, 4=User)
;;;   ini-path - For Project scope: full path to INI file (optional for other scopes)
;;;   section - For Project scope: INI section name (optional for other scopes)
;;; Returns:
;;;   Variable value (string), or default if not found
;;; Fallback Chain:
;;;   1. Check cache
;;;   2. If not in cache, read from scope storage
;;;   3. If not in storage, use default from definitions
;;;   4. Store in cache and return
(defun HAWS-CONFIG:GETVAR (app var scope-code ini-path section / val setvar-p)
  (setq setvar-p t)
  
  ;; Try getting from cache first
  (setq val (haws-config-cache-get app var))
  
  (cond
    ;; If found in cache, use it
    (val
     (setq setvar-p nil)
    )
    ;; Not in cache - need to load from storage
    (t
     ;; Load all vars of this scope if cache is empty for this var
     (cond
       ;; Session scope
       ((= scope-code 0)
        (setq val (haws-config-read-session app var))
        (if (not val)
          ;; Not in session yet, use default
          (setq val (haws-config-get-default app var))
        )
       )
       ;; Project scope
       ((= scope-code 2)
        (if (and ini-path section)
          (progn
            ;; Load all project vars into cache (batch operation)
            (foreach
              pair
               (haws-config-read-all-project app ini-path section)
              (haws-config-cache-set app (car pair) (cadr pair))
            )
            ;; Now get our specific var from cache
            (setq val (haws-config-cache-get app var))
          )
          ;; No INI path provided, use default
          (setq val (haws-config-get-default app var))
        )
       )
       ;; User scope
       ((= scope-code 4)
        (setq val (haws-config-read-user app var))
        (if (not val)
          ;; Not in registry yet, use default
          (setq val (haws-config-get-default app var))
        )
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
  
  ;; Store in cache if needed
  (if (and setvar-p val)
    (haws-config-cache-set app var val)
  )
  
  val
)

;;; HAWS-CONFIG:SETVAR - Set configuration variable value
;;; Arguments:
;;;   app - Application identifier (string)
;;;   var - Variable name (string)
;;;   val - Value to set (string)
;;;   scope-code - Scope code (0=Session, 1=Drawing, 2=Project, 3=App, 4=User)
;;;   ini-path - For Project scope: full path to INI file (optional for other scopes)
;;;   section - For Project scope: INI section name (optional for other scopes)
;;; Returns:
;;;   The value that was set
;;; Side Effects:
;;;   Updates cache
;;;   Writes to appropriate scope storage (Registry, INI file, or session memory)
(defun HAWS-CONFIG:SETVAR (app var val scope-code ini-path section / )
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
;;; END OF HAWS-CONFIG LIBRARY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "\nHAWS-CONFIG library loaded")
(princ)
