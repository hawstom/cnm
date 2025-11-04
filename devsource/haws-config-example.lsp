;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HAWS-CONFIG Example - Demonstrates Simple Configuration Registration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This example shows how easy it is to use HAWS-CONFIG after removing
;;; the redundant "Scope" section boilerplate.
;;;
;;; UNIFIED DATA STRUCTURE:
;;;   *haws-config* = '(("Definitions" ...)  ; App variable schemas
;;;                     ("Cache" ...)        ; Runtime cache
;;;                     ("Session" ...))     ; Session storage
;;;
;;; SCOPE CODES (use integer literals with comments):
;;;   0 = Session  (in-memory only)
;;;   1 = Drawing  (not implemented)
;;;   2 = Project  (INI file)
;;;   3 = App      (not implemented)
;;;   4 = User     (Windows Registry)
;;;
;;; BEFORE (Old Way - Multiple Globals):
;;;   *haws-config-definitions* = '(...)
;;;   *haws-config-cache* = '(...)
;;;   *haws-config-session* = '(...)
;;;
;;; AFTER (New Way - Single Global):
;;;   *haws-config* = '(("Definitions" ...) ("Cache" ...) ("Session" ...))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "\nHaws-config-example loading...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define your app's configuration - Just variable definitions!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun myapp-config-definitions ()
  (list
   (list "UserSetting1" "A" 4)        ; 4=User (Registry)
   (list "UserSetting2" "B" 4)        ; 4=User (Registry)
   (list "ProjectPath" "" 2)          ; 2=Project (INI file)
   (list "AppVersion" "1.0.0" 0)      ; 0=Session (Memory)
   (list "LastUsed" "" 0)             ; 0=Session (Memory)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Register app (do this once at load time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if haws-config-register-app
  (haws-config-register-app "MyApp" (myapp-config-definitions))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for cleaner API (scope auto-looked-up)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun myapp-getvar (var)
  (haws-config-getvar "MyApp" var nil nil)  ; Scope auto-looked-up from definitions
)

(defun myapp-setvar (var val)
  (haws-config-setvar "MyApp" var val nil nil)  ; Scope auto-looked-up from definitions
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c:myapp-test-write ()
  (myapp-setvar "UserSetting1" "NewValue")
  (myapp-setvar "LastUsed" (rtos (getvar "DATE") 2 6))
  (prompt "\nSaved UserSetting1 = NewValue to Registry")
  (prompt "\nSaved LastUsed timestamp to session memory")
  (princ)
)

(defun c:myapp-test-read ()
  (prompt (strcat "\nUserSetting1 (Registry) = " (myapp-getvar "UserSetting1")))
  (prompt (strcat "\nUserSetting2 (Registry) = " (myapp-getvar "UserSetting2")))
  (prompt (strcat "\nAppVersion (Session) = " (myapp-getvar "AppVersion")))
  (prompt (strcat "\nLastUsed (Session) = " (myapp-getvar "LastUsed")))
  (princ)
)

(defun c:myapp-test-info ()
  (prompt "\n=== MyApp Configuration Info ===")
  (prompt "\nRegistry Path: HKEY_CURRENT_USER\\Software\\HawsEDC\\MyApp")
  (prompt "\n")
  (prompt "\nUser Settings (Persistent - Registry):")
  (prompt (strcat "\n  UserSetting1 = " (myapp-getvar "UserSetting1")))
  (prompt (strcat "\n  UserSetting2 = " (myapp-getvar "UserSetting2")))
  (prompt "\n")
  (prompt "\nSession Settings (Memory Only - Not Persistent):")
  (prompt (strcat "\n  AppVersion = " (myapp-getvar "AppVersion")))
  (prompt (strcat "\n  LastUsed = " (myapp-getvar "LastUsed")))
  (prompt "\n")
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. (load "haws-config-example.lsp")
;;; 2. MYAPP-TEST-INFO          ; See defaults
;;; 3. MYAPP-TEST-WRITE         ; Write new values
;;; 4. MYAPP-TEST-READ          ; Read values
;;; 5. Close AutoCAD, reopen
;;; 6. (load "haws-config-example.lsp")
;;; 7. MYAPP-TEST-READ          ; User settings persist, session settings reset!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ "\nHaws-config-example loaded. Commands: MYAPP-TEST-INFO, MYAPP-TEST-WRITE, MYAPP-TEST-READ")
(princ)
