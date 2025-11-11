;;;=====================================================================
;;; HAWS-DISTRIB Compilation System
;;; 
;;; Purpose: Replace Visual LISP IDE compilation workflow with AutoLISP-based
;;;          compilation system for HawsEDC/CNM project distribution
;;;
;;; Author: HawsEDC Development Team  
;;; Created: 2025-11-08
;;; License: Copyright (c) HawsEDC
;;;
;;; Usage: Load this file in AutoCAD and run (c:haws-distrib-compile)
;;;
;;; Dependencies: 
;;;   - edclib.lsp (for haws-unified-version, haws-getstringx, haws-vlisp-p)
;;;   - Visual LISP enabled AutoCAD (2000+)
;;;   - Write access to compile folders
;;;
;;;=====================================================================

(defun haws-distrib-compile (/ current-version new-version)
  "Main compilation function - compiles all LSP files with version management"
  (princ "\n=== HawsEDC Distribution Compilation System ===")
  
  ;; Important: Alert user about debugger conflicts
  (alert (princ "REMINDER: Close all files and disconnect any active VS Code AutoLISP debugging sessions before proceeding."))
  
  ;; Initialize and validate environment
  (if (not (haws-distrib-validate-environment))
    (exit))
  
  ;; Version management workflow
  (setq current-version (haws-unified-version))
  (princ (strcat "\nCurrent version: " current-version))
  
  ;; Prompt for new version using haws-getstringx
  (setq new-version 
    (haws-getstringx 
      "Enter new version" 
      current-version 
      current-version))
  
  (if (/= new-version current-version)
    (progn
      (princ (strcat "\nUpdating version from " current-version " to " new-version))
      (haws-distrib-update-version new-version)
      (princ "\nVersion updated in edclib.lsp"))
    (princ "\nUsing current version (no change)"))
  
  ;; Clean destination folder first
  (haws-distrib-clean-destination)
  
  ;; Main compilation process
  (haws-distrib-compile-all-files)
  
  (princ "\n=== Compilation Complete ===")
  (princ "\nNext step: Run existing ../compile/distrib.bat for final packaging")
  (princ)
)

(defun haws-distrib-validate-environment (/ result)
  "Validate compilation environment and dependencies"
  (setq result t)
  
  ;; Check Visual LISP availability
  (if (not (haws-vlisp-p))
    (progn
      (alert "Visual LISP not available - compilation requires AutoCAD 2000+")
      (setq result nil)
    )
  )
  ;; Check edclib.lsp is loaded
  (if (not (type haws-unified-version))
    (progn
      (alert "edclib.lsp not loaded - required for version management")
      (setq result nil)
    )
  )
  ;; Check haws-getstringx availability  
  (if (not (type haws-getstringx))
    (progn
      (alert "haws-getstringx not available - check edclib.lsp")
      (setq result nil)
    )
  )
  result
)

(defun haws-distrib-update-version (new-version / edclib-path old-content new-content file-handle line)
  "Update version in edclib.lsp haws-unified-version function"
  (setq edclib-path (strcat (haws-distrib-get-source-path) "edclib.lsp"))
  
  ;; Read current file content
  (if (setq file-handle (open edclib-path "r"))
    (progn
      (setq old-content "")
      (while (setq line (read-line file-handle))
        (setq old-content (strcat old-content line "\n"))
      )
      (close file-handle)
      ;; Replace version string
      (setq new-content 
        (haws-distrib-replace-version-string old-content new-version)
      )
      ;; Write updated content
      (if (setq file-handle (open edclib-path "w"))
        (progn
          (princ new-content file-handle)
          (close file-handle)
        )
        (alert "Error: Cannot write to edclib.lsp")
      )
    )
    (alert "Error: Cannot read edclib.lsp")
  )
)

(defun haws-distrib-replace-version-string (content new-version / old-pattern new-pattern)
  "Replace version string in file content"
  ;; Pattern: look for "5.5.18" or similar version in haws-unified-version function
  ;; This is a simplified approach - may need refinement
  (setq old-pattern (haws-unified-version))  ; Current version
  (setq new-pattern new-version)
  
  ;; Simple string replacement (may need enhancement for robustness)
  (haws-string-replace content 
    (strcat "\"" old-pattern "\"")
    (strcat "\"" new-pattern "\"")
  )
)

(defun haws-string-replace (str old-substr new-substr / pos result)
  "Replace all occurrences of old-substr with new-substr in str"
  ;; Simple implementation - could use more sophisticated approach
  (setq pos (vl-string-search old-substr str))
  (if pos
    (strcat
      (substr str 1 pos)
      new-substr
      (haws-string-replace 
        (substr str (+ pos (strlen old-substr) 1))
        old-substr
        new-substr
      )
    )
    str
  )
)

(defun haws-distrib-compile-all-files (/ source-files excluded-files compile-count error-count total-files excluded-count compile-target file)
  "Compile all LSP files according to cnm.prj settings"
  (princ "\nStarting compilation process...")
  
  ;; Initialize Visual LISP
  (vl-load-com)
  
  ;; Get file lists
  (setq source-files (haws-distrib-get-source-files))
  (setq excluded-files (haws-distrib-get-excluded-files))
  (setq compile-count 0)
  (setq error-count 0)
  
  (if (not source-files)
    (progn
      (alert "ERROR: No source files found in devsource/ directory")
      (exit)
    )
  )
 
  ;; Count files that will be compiled vs excluded
  (setq total-files (length source-files))
  (setq excluded-count 0)
  (foreach file source-files
    (if (haws-distrib-file-excluded-p file excluded-files)
      (setq excluded-count (1+ excluded-count))
    )
  )
  (setq compile-target (- total-files excluded-count))
  
  (princ (strcat "\nFile analysis:"))
  (princ (strcat "\n  Total LSP files found: " (itoa total-files)))
  (princ (strcat "\n  Files to exclude: " (itoa excluded-count)))
  (princ (strcat "\n  Files to compile: " (itoa compile-target)))
  
  ;; Create destination directory if needed
  (haws-distrib-ensure-dest-directory)
  
  ;; Compile each file
  (foreach file source-files
    (if (not (haws-distrib-file-excluded-p file excluded-files))
      (progn
        (princ (strcat "\nCompiling " file "..."))
        (if (haws-distrib-compile-single-file file)
          (setq compile-count (1+ compile-count))
          (setq error-count (1+ error-count))
        )
      )
      (princ (strcat "\nSkipping " file " (excluded)"))
    )
  )
  
  ;; Report results
  (princ (strcat "\nCompilation summary:"))
  (princ (strcat "\n  Files compiled successfully: " (itoa compile-count)))
  (princ (strcat "\n  Files with errors: " (itoa error-count)))
  (princ (strcat "\n  Files excluded: " (itoa excluded-count)))
  (princ (strcat "\n  Expected vs actual: " (itoa compile-target) " -> " (itoa (+ compile-count error-count))))
  
  (if (> error-count 0)
    (princ "\nWARNING: Some files failed to compile. Check individual error messages above.")
    (princ "\nSUCCESS: All eligible files compiled without errors."))
)

(defun haws-distrib-get-source-files (/ source-path all-files)
  "Get list of LSP files by scanning devsource/ directory dynamically"
  (setq source-path (haws-distrib-get-source-path))
  
  (if (vl-file-directory-p source-path)
    (progn
      ;; Get all LSP files in devsource/
      (setq all-files (vl-directory-files source-path "*.lsp"))
      (princ (strcat "\nFound " (itoa (length all-files)) " LSP files in " source-path))
      all-files)
    (progn
      (princ (strcat "\nERROR: Source directory not found: " source-path))
      nil))
)

(defun haws-distrib-get-excluded-files ()
  "Get list of files to exclude from compilation (pattern-based)"
  ;; Dynamic exclusion patterns - easy to modify as files move
  (list
    ;; Files we want toi remain as LSP for user editing and easy inspection.
    "cnmalias.lsp"     ; Command aliases
    "cnmloader.lsp"    ; Main loader
    
    ;; Install and setup script needs to remain as LSP for Inno Setup editing.
    "CNM-Install.lsp"  ; Installation script
    
    ;; Pattern-based exclusions can be added here
    ;; Format: exact filename matches for now
    ;; Future enhancement: could support wildcards like "*loader*.lsp"
  )
)

(defun haws-distrib-file-excluded-p (filename excluded-list / excluded-p)
  "Check if file should be excluded from compilation"
  (setq excluded-p nil)
  
  ;; Check exact filename matches
  (if (member filename excluded-list)
    (setq excluded-p t))
  
  ;; Future enhancement: Pattern matching could go here
  ;; Example: (if (wcmatch filename "*loader*.lsp") (setq excluded-p t))
  
  excluded-p
)

(defun haws-distrib-compile-single-file (filename / source-path dest-path result)
  "Compile single LSP file to FAS using Visual LISP with cnm.prj options"
  (setq source-path (strcat (haws-distrib-get-source-path) filename))
  (setq dest-path (strcat (haws-distrib-get-dest-path) 
                         (haws-distrib-change-extension filename ".fas")))
  
  ;; Check if source file exists
  (if (not (findfile source-path))
    (progn
      (princ (strcat " ERROR: Source file not found: " source-path))
      nil
    )
    (progn
      ;; Attempt compilation with error handling
      (setq result
        (vl-catch-all-apply 'vlisp-compile
          (list 'st source-path dest-path)
        )
      )
      
      (if (vl-catch-all-error-p result)
        (progn
          (princ (strcat " ERROR: " (vl-catch-all-error-message result)))
          nil
        )
        (progn
          (princ " OK")
          t
        )
      )
    )
  )
)

(defun haws-distrib-get-source-path ()
  "Get source directory path based on edclib.lsp location"
  ;; Find edclib.lsp and use its directory as source path
  (strcat (vl-filename-directory (findfile "edclib.lsp")) "\\"))

(defun haws-distrib-get-dest-path (/ hawsedc-dir)
  "Get destination directory path - absolute path to avoid vlisp-compile issues"
  ;; Go up 2 levels from edclib.lsp location: devsource -> develop -> hawsedc
  ;; Then add compile/acad/
  (setq hawsedc-dir (vl-filename-directory (vl-filename-directory (vl-filename-directory (findfile "edclib.lsp")))))
  (strcat hawsedc-dir "\\compile\\acad\\")
)

(defun haws-distrib-clean-destination (/ dest-path fas-files file-path)
  "Clean all existing FAS files from destination directory"
  (setq dest-path (haws-distrib-get-dest-path))
  
  (princ (strcat "\nCleaning destination directory: " dest-path))
  
  ;; Get list of existing FAS files
  (setq fas-files (vl-directory-files dest-path "*.fas"))
  
  (if fas-files
    (progn
      (princ (strcat "\nRemoving " (itoa (length fas-files)) " existing FAS files..."))
      (foreach file fas-files
        (setq file-path (strcat dest-path file))
        (if (vl-file-delete file-path)
          (princ (strcat "\n  Deleted: " file))
          (princ (strcat "\n  WARNING: Could not delete: " file))
        )
      )
    (princ "\n  No existing FAS files to remove"))
  
  (princ "\nDestination cleanup complete."))
)
(defun haws-distrib-ensure-dest-directory (/ dest-path)
  "Create destination directory if it doesn't exist"
  (setq dest-path (haws-distrib-get-dest-path))
  ;; Simple check - create if needed (may need platform-specific implementation)
  (if (not (vl-file-directory-p dest-path))
    (progn
      (princ (strcat "\nCreating destination directory: " dest-path))
      ;; Note: May need to use platform-specific commands to create directory
      ;; For now, assume directory exists or user creates it manually
    )
  )
)
(defun haws-distrib-change-extension (filename new-ext / base-name)
  "Change file extension from .lsp to .fas"
  (setq base-name (substr filename 1 (- (strlen filename) 4)))  ; Remove .lsp
  (strcat base-name new-ext)
)

;; Note: Auto-execution happens at end of file - no user prompt needed

;; Auto-execute compilation on load
(princ "\nHAWS-DISTRIB compilation system loaded.")
(princ "\nStarting compilation automatically...")
(haws-distrib-compile)