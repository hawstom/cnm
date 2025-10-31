# CNM Authorized Global Variables

**Purpose:** Document all intentional global variables in the CNM codebase per AutoLISP scoping requirements.

**AutoLISP Scoping Rule:** Variables not declared in the `/ local1 local2` parameter list become global and persist across function calls. This document authorizes legitimate global variables and helps identify accidental leaks.

---

## Authorized Global Variables

### Configuration & State Management

**`*hcnm-config*`**
- **Purpose:** Cached configuration settings from CNM.INI
- **Type:** Association list `'((var . value) ...)`
- **Set in:** `hcnm-config-read-all-project` (line 3588, 3590)
- **Read in:** Throughout codebase via `c:hcnm-config-getvar`
- **Justification:** Performance - avoid repeated INI file reads
- **Lifetime:** AutoCAD session (cleared by `hcnm-projinit`)

**`*hcnm-config-temp*`**
- **Purpose:** Temporary configuration changes before save
- **Type:** Association list `'((var val) ...)`
- **Set in:** `hcnm-config-temp-setvar` (line 3380)
- **Cleared in:** `hcnm-config-temp-clear` (line 3408)
- **Justification:** Dialog workflow - accumulate changes, save or cancel
- **Lifetime:** Single dialog session

**`*hcnm-cnmprojectroot*`**
- **Purpose:** Cached project root folder path
- **Type:** String path
- **Set in:** `hcnm-proj` (line 2547)
- **Read in:** Project management functions
- **Justification:** Performance - avoid repeated folder resolution
- **Lifetime:** AutoCAD session (cleared by `hcnm-projinit`)

**`*hcnm-cnmprojectnotes*`**
- **Purpose:** Cached project notes from CSV/TXT file
- **Type:** Complex nested list `'((0 . "comment") (1 "var" "val1") (3 "type" "num" "unit" "count" "text"))`
- **Set in:** `hcnm-readcfcsv` (line 4256), `hcnm-readcftxt2` (line 4112)
- **Read in:** Key Notes Table, Quantity Takeoff, bubble insertion
- **Justification:** Performance - avoid repeated file parsing
- **Lifetime:** AutoCAD session (cleared by `hcnm-projinit`)

**`*hcnm-dimstyleold*`**
- **Purpose:** Saved dimension style before CNM changes it
- **Type:** String (dimension style name)
- **Set in:** `hcnm-set-dimstyle` (line 3632)
- **Restored in:** `hcnm-restore-dimstyle`
- **Justification:** User environment restoration
- **Lifetime:** Single command execution

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

**`hcnm-ldrblk-eb-lattribs`**
- **Purpose:** Bubble attributes being edited in dialog
- **Type:** lattribs structure `'(("TAG" "prefix" "auto" "postfix") ...)`
- **Set in:** Edit dialog functions (`hcnm-eb-*`)
- **Scope:** Edit dialog session only
- **Justification:** DCL dialog callback architecture requires semi-global state
- **Pattern:** Standard AutoLISP dialog pattern

**`hcnm-ldrblk-eb-ename-bubble`**
- **Purpose:** Entity name of bubble being edited
- **Type:** AutoCAD entity name
- **Set in:** `hcnm-edit-bubble`
- **Scope:** Edit dialog session only
- **Justification:** DCL dialog callback architecture

---

## Naming Convention

All authorized globals use `*asterisk-naming*` or `hcnm-prefix-name` conventions:

- **`*hcnm-*`** - Session-scoped cached state (config, project data)
- **`hcnm-ldrblk-eb-*`** - Dialog semi-globals (DCL callback pattern)

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

**Last Updated:** October 31, 2025  
**Audit Status:** ✅ All bubble region functions reviewed and fixed
