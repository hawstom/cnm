# CNM Authorized Global Variables

**Purpose:** Document all intentional global variables in the CNM codebase per AutoLISP scoping requirements.

**AutoLISP Scoping Rule:** Variables not declared in the `/ local1 local2` parameter list become global and persist across function calls. This document authorizes legitimate global variables and helps identify accidental leaks.

---

## Authorized Global Variables

### Configuration & State Management

**`*haws-config*`** *(haws-config.lsp)*
- **Purpose:** Unified configuration data structure for all registered apps
- **Type:** Nested alist with sections: Definitions / Cache / Session / ProjectRoots
- **Set in:** `haws-config-register-app`, `haws-config-set-section`, `haws-config-set-proj-root`
- **Read in:** Throughout haws-config.lsp; CNM accesses via `hcnm-config-getvar`
- **Justification:** Central store for multi-app config; eliminates `*hcnm-config*`, `*hcnm-cnmprojectroot*`
- **Lifetime:** AutoCAD session

**`*haws-config-cache*`** *(haws-config.lsp)*
- **Purpose:** Legacy cache variable (reset on load to ensure clean state)
- **Type:** nil on load
- **Set in:** Top-level `(setq *haws-config-cache* nil)` in haws-config.lsp
- **Justification:** Exists only to reset legacy state on reload

**`*haws-config-temp*`** *(haws-config.lsp)*
- **Purpose:** Per-app pending dialog changes, buffered before save/cancel
- **Type:** `'(("CNM" (("var1" "val1") ...)) ("APP2" ...) ...)`
- **Set in:** `haws-config-temp-setvar`
- **Cleared in:** `haws-config-temp-clear`
- **Justification:** Dialog workflow — accumulate changes, commit on OK or discard on Cancel
- **Lifetime:** Single dialog session

**`*hcnm-cnmprojectnotes*`**
- **Purpose:** Cached project notes from CSV/TXT file
- **Type:** Complex nested list `'((0 . "comment") (1 "var" "val1") (3 "type" "num" "unit" "count" "text"))`
- **Set in:** `hcnm-readcfcsv` (line 4256), `hcnm-readcftxt2` (line 4112)
- **Read in:** Key Notes Table, Quantity Takeoff, bubble insertion
- **Justification:** Performance - avoid repeated file parsing [TGH 2025-10-31 03:28:07: Would there be value in trying to incorporate this into `*hcnm-config*`?]
- **Lifetime:** AutoCAD session (cleared by `hcnm-projinit`)

**`*hcnm-dimstyleold*`**
- **Purpose:** Saved dimension style before CNM changes it
- **Type:** String (dimension style name)
- **Set in:** `hcnm-set-dimstyle` (line 3632)
- **Restored in:** `hcnm-restore-dimstyle`
- **Justification:** User environment restoration [TGH 2025-10-31 03:28:07: I think this is a little weasely. We need to steel-man the case for localizing this or incorporating it into `*hcnm-config*`]
- **Lifetime:** Single command execution

---

### Layer System

**`*haws:layers*`** *(edclib.lsp)*
- **Purpose:** Cached layer definitions loaded from `layers.dat` file
- **Type:** List of layer-data lists `'((layerkey layername color linetype ...) ...)`
- **Set in:** `haws-getlayr` (reads and parses `layers.dat` on demand), `c:hcnm-cnmlayer` (reset to nil before refresh)
- **Read in:** `haws-getlayr`, `haws-setlayr`, `c:hcnm-cnmlayer` (iterates via `foreach`)
- **Justification:** Performance cache — layers.dat is read once per session rather than on each layer lookup
- **Naming:** Uses `haws:` namespace with colon (edclib convention predating hyphens standard; do not change)
- **Lifetime:** AutoCAD session (persists until `c:hcnm-cnmlayer` refresh)

---

### Paper Space Management (Under Review)

⚠️ **`*hcnm-pspace-restore-needed*`**
- **Purpose:** Flag indicating need to restore paper space after model space operations
- **Type:** Boolean (T/nil)
- **Set in:** `hcnm-ldrblk-get-target-vport` (line 7642)
- **Cleared in:** `hcnm-ldrblk-insert` (line 5280)
- **Status:** ⚠️ **TEMPORARY** - Marked for refactoring (see TGH comment line 5277)
- **Issue:** Ad hoc global, should use bubble-data or config system
- **Recommendation:** Refactor to use bubble-data parameter passing

---

### Edit Dialog Semi-Globals

**`hcnm-bn-eb-state`**
- **Purpose:** Combined edit dialog state (lattribs, auto-handles, focused tag)
- **Type:** Alist with keys "LATTRIBS", "AUTO-HANDLES", "FOCUSED-TAG"
- **Set in:** `hcnm-edit-bubble` (initialized), `hcnm-bn-eb-get-text`, `hcnm-bn-eb-update-text` (modified)
- **Scope:** Edit dialog session only
- **Justification:** DCL dialog callback architecture requires semi-global state. Single variable replaces 4 unauthorized semi-globals.

**`hcnm-ldrblk-eb-lattribs`** *(legacy - old edit dialog, not bubble note editor)*
- **Purpose:** Bubble attributes being edited in dialog
- **Type:** lattribs structure `'(("TAG" "prefix" "auto" "postfix") ...)`
- **Set in:** Edit dialog functions (`hcnm-eb-*`)
- **Scope:** Edit dialog session only
- **Justification:** DCL dialog callback architecture requires semi-global state
- **Pattern:** Standard AutoLISP dialog pattern

**`hcnm-ldrblk-eb-ename-bubble`** *(legacy - old edit dialog, not bubble note editor)*
- **Purpose:** Entity name of bubble being edited
- **Type:** AutoCAD entity name
- **Set in:** `hcnm-edit-bubble`
- **Scope:** Edit dialog session only
- **Justification:** DCL dialog callback architecture [TGH 2025-10-31 03:28:07: I need to audit this.]

---

### Key Table Ephemeral Dynamic-Scope Variables

The following variables are declared as **locals in `hcnm-key-table-make`** (line ~786) but accessed by its helper functions via AutoLISP dynamic scoping:

| Variable | Type | Role |
|---|---|---|
| `column-height` | Real | Current column fill height |
| `column-height-pending` | Real | Remaining space in column |
| `icol` | Integer | Current column index |
| `note-first-line-p` | Boolean | Whether this is the first line of a note |
| `notdsc` | String | Note description text |
| `notnum` | List | Note number data |
| `notqty` | List | Note quantity data |
| `nottyp` | String | Note type key |
| `notunt` | String | Note unit text |
| `phaselist` | List | Phase configuration list |
| `phasewid` | Real | Phase column width |
| `qtypt` | Point | Current insertion point |
| `qtypt1` | Point | Column start point |
| `tblwid` | Real | Table column width |
| `txtht` | Real | Text height |

**Helper functions that access these (NO local declarations — by design):**
- `hcnm-key-table-advance-column` (line ~1171)
- `hcnm-key-table-advance-down` (line ~1189)
- `hcnm-key-table-insert-shape` (line ~1202)
- `hcnm-key-table-insert-text` (line ~1214)

**CRITICAL:** Do NOT add these to the helper function locals lists. That would shadow the caller's variables and break the table drawing logic. This is an intentional AutoLISP dynamic-scoping pattern, identical to how `hcnm-bn-eb-state` works for DCL dialogs.

**Refactor option:** Create a `table-state` alist passed as a parameter (parallel to `bubble-data`). High effort, not currently planned.

---

## Verification Notes (February 2026)

### `*hcnm-config-temp*` Wrapper Functions
The 4 wrapper functions (`hcnm-config-temp-setvar/getvar/save/clear`) were flagged in the 2026-02-20 globals report as setting `*hcnm-config-temp*`. This was the pre-migration state. Current code (post haws-config migration) delegates purely to `haws-config-temp-*` functions in haws-config.lsp, which use `*haws-config-temp*` internally. No direct reference to `*hcnm-config-temp*` remains in cnm.lsp.

### Checker False Positives
The globals checker (static analysis) produces false positives for lambda parameters. These are NOT globals:
- `x` in `hcnm-bn-get-auto-data` and `hcnm-config-project-notes-format` — lambda params in `'(lambda (x) ...)`
- `index` in `hcnm-bn-lattribs-spec` — lambda param in `'(lambda (index) ...)`
- `auto-type` in `hcnm-bn-eb-show` — lambda param in `'(lambda (auto-type) ...)`
- `msg` in `hcnm-bn-lattribs-validate-schema` — lambda param in `'(lambda (msg) ...)`
- `phase` in `hcnm-getphaselistfromtblqty` — lambda param in `'(lambda (phase) ...)`

---

## Naming Convention

All authorized globals use `*asterisk-naming*` or `hcnm-prefix-name` conventions:

- **`*hcnm-*`** - Session-scoped cached state (config, project data)
- **`hcnm-bn-eb-*`** - Bubble note edit dialog semi-globals (DCL callback pattern)
- **`hcnm-ldrblk-eb-*`** - Legacy edit dialog semi-globals (DCL callback pattern)

---

## Audit Results

### Functions with Local Variable Declarations Added (October 31, 2025)

The following functions previously lacked `/` declarations but have now been fixed:

1. ✅ `hcnm-ldrblk-bubble-data-set` - Added `/ ` (line 5700)
2. ✅ `hcnm-ldrblk-change-arrowhead` - Added `/ ` (line 5822)
3. ✅ `hcnm-ldrblk-lattribs-validate-and-underover` - Added `/ ` (line 5971)
4. ✅ `hcnm-ldrblk-lattribs-validate` - Added `/ ` (line 5988)
5. ✅ `hcnm-ldrblk-space-restore` - Added `/ ` (line 7417)
6. ✅ `hcnm-ldrblk-warn-pspace-coordinates` - Added `/ ` (line 7493)
7. ✅ `hcnm-ldrblk-eb-save` - Added `/ ` (line 8897)

**Note:** Some simple wrapper/pure functions intentionally omit `/` because they have no local variables and contain no `setq` statements. This is safe AutoLISP practice.

---

## Guidelines for New Code

1. **Declare all locals:** Always use `(defun func (args / local1 local2) ...)`
2. **No ad hoc globals:** Use authorized patterns only
3. **Document globals:** Add to this file with justification
4. **Use `*asterisks*`:** For session-scoped globals
5. **Prefer parameters:** Pass data via function arguments when possible

---

## References

- **copilot-instructions.md** - Section 1.2.3 "AutoLISP Conventions"
- **audit-bubble-region-2025-10-31.md** - Section 4 "Local Variable Declarations"

---

**Last Updated:** February 21, 2026
**Audit Status:** ✅ All bubble region functions reviewed and fixed; globals report 2026-02-20 reviewed and categorized
