# CLAUDE.md - CNM Project Instructions

## Project Overview
- Languages: AutoLISP (is **not** Common LISP) with DCL. Includes an AutoCAD Script File test suite at devtools\scripts\test-suite.
- Functionality: ~400 AutoCAD productivity commands. Current efforts are focused mostly on cnm.lsp bubble note auto text creation and updates.

## Critical Rules

### No Superstitious Code (NON-NEGOTIABLE)
Never add "just in case" defensive code. Every line must have justification with specific evidence. No error handling without understanding what errors are possible. No validation checks without knowing what you're validating. Git saves us - we can always revert. See `copilot-instructions.md` Section 1.1.4 for full policy.

### AutoLISP Is NOT Common Lisp
AutoLISP and Common Lisp are unrelated dialects. Do not assume similarity.
- `fboundp` does NOT exist - use `(boundp 'function-name)`
- `consp` does NOT exist - use `vl-consp`
- `vlax-object-p` does NOT exist (AI hallucination)
- `(load)` does not accept relative paths - it searches AutoCAD's support paths
- Before using any function, verify it exists in the codebase with grep or in Autodesk docs

### Workflow Constraints
- **DO NOT git commit** unless explicitly asked - let the human commit
- **DO NOT claim "fixed" or "loads successfully"** - say "validation passes" or "no syntax errors detected" (Claude cannot test in AutoCAD without preparing an unattended script)
- **Run paren validation** after every .lsp edit: `powershell -File devtools\scripts\haws-lisp-paren-check.ps1 -FilePath "devsource\<file>.lsp"` and `powershell -File devtools\scripts\validate-lisp-syntax.ps1 -FilePath "devsource\<file>.lsp"`

## Code Style

### Formatting
- **2-space indentation**, no tabs
- **Narrow-style indentation** (not wide-style aligned with first arg)
- **Closing parens** on own line, aligned with opening expression (for defun/cond/loops/4+ line blocks). Can be stacked on same line for inner expressions.
- **No blank lines** anywhere in code files (FIRM, very aggressive schedule)
- **No closing comments** like `;end defun` or `;end cond`

### Naming
- **Case:** lowercase for all symbols and functions
- **Delimiters:** Hyphens (migration from underscores should be complete)
- **Type prefixes:** `obj-` for VLA-OBJECTs, `en-` for entity names, `es-` for entsel results, `eg-`/`el-` for entget lists, `ss-` for selection sets
- **Boolean suffix:** `-p` (e.g., `found-p`, `pspace-p`)
- **Function naming:** verb-noun pattern (`get-`, `set-`, `calculate-`, `format-`, `update-`, `assure-`, `validate-`)
- **Data flow functions:** `source-to-destination` pattern with NO verb (e.g., `hcnm-lattribs-to-dwg`)
- **Namespaces:** `haws-` for edclib, `hcnm-` for CNM, `c:` prefix only for user-invocable commands

### Variables
- **Always declare locals** in `(defun func (args / local1 local2) ...)`
- **No ad hoc globals** - use `haws-config` system
- **Globals use `*asterisks*`** (e.g., `*hcnm-config*`)
- See `.github/authorized-globals.md` for all authorized globals. Ask aggressively about weeding out globals where possible.

### Comments
- **Single `;`** for minor inline comments (and `; #region`/`; #endregion` for VS Code folding)
- **Double `;;`** for normal comments at code indentation level
- **Triple `;;;`** for major section comments at left margin
- **Alerts must enclose princ:** `(alert (princ "message"))`

### Data Structures
- **Dotted pairs** for alists (key-value with `assoc`): `'(("Name" . "John"))`
- **Proper lists** for everything else: `'(("TAG" "value"))` for lattribs
- Rule of thumb: if `assoc` is primary accessor, use dotted pairs

## Architecture Quick Reference

### Key Files
- `devsource/cnm.lsp` - Main CNM module (~8600+ lines)
- `devsource/edclib.lsp` - Core shared library (MUST NOT depend on CNM)
- `devsource/cnmloader.lsp` - Entry point, lazy-loads cnm.lsp
- `devsource/haws-config.lsp` - Multi-app configuration system

### Directory Structure
- `devsource/` - Production code distributed to users after compilation
- `devtools/` - Developer-only files (NOT distributed)
- `devmenu/` - Menu resources (RC files, icons, CUIX)
- `sandbox/` - Experimental code

### Architecture Checklist (MANDATORY before coding)
1. **Who calls this?** User command (`c:` prefix) vs internal function (no `c:`)
2. **Where does it belong?** edclib (core, no app deps) vs CNM (can depend on core)
3. **What's the scope of this setting?** Session (scope 0) vs Project (scope 2) vs User pref (scope 4)
4. **Does this already exist?** Search before writing
5. **Is this the simplest solution?** No premature abstraction

### Bubble Notes Data Model
- **lattribs:** `'(("TAG" "full-text") ...)` - 2-element lists, never nil, always strings
- **XDATA:** Auto-text composite keys `(("TAG" (((auto-type . handle) . "value") ...)) ...)`
- **XRECORD:** Viewport transforms for paper space bubbles
- **bubble-data:** Ephemeral alist passed during insertion/editing

### Auto-Text Classifications
- **Handle-based:** Sta/Off/StaOff/AlName/StaName/Dia/Slope/L (stores reference handle)
- **Handleless:** N/E/NE only (empty handle `""`, coordinate-only)
- **Field-based:** LF/SF/SY (AutoCAD field system, not CNM updater)
- **Coordinate-based:** Sta/Off/StaOff/StaName/N/E/NE (needs viewport transform in paper space)

### Baseline Reference
Git commit `63efc0d` (v5.5.17) is a good baseline for the auto-text system before the reactor was added and before AI assistance. None of the XDATA, VPTRANS, or updater data structures existed at that point.

## Standards Documentation

For deep dives, read the full standards volumes in `devtools/docs/standards/`:
- **S01** - Values and Practices (scheduling standards implementation)
- **S02** - AI-Human Collaboration (document standards, comment lifecycle, cleanup markers)
- **S03** - Style (naming, indentation, dotted pairs vs lists)
- **S04** - Organization and Locations (directory structure, file organization, loading)
- **S05** - Architecture (config system, bubble notes, auto-text updater, glossary)
- **S06** - Roadmap (priorities, compliance dashboard, planned features)

Also see:
- `.github/copilot-instructions.md` - Comprehensive project instructions with full context
- `.github/authorized-globals.md` - All authorized global variables
- `devtools/docs/api/haws-config-api.md` - Config system API reference

## Testing
- No unit test framework yet - manual testing in AutoCAD
- Test script: open `devtools/scripts/test-suite/cnm-test-start.dwg` and run `cnm-test.scr`
- `(c:pretest)` removes test bubbles and cleans up test state before testing
- Always validate syntax and never claim anything is fixed

## Planning Documents
For multi-step features/issues, create `.ai-plans/issue-{number}-{short-name}.md` from `.ai-plans/TEMPLATE.md`. See copilot-instructions.md Section 5.2 for workflow.