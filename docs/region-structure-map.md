# CNM Region Structure Map

**Purpose:** Visual reference for navigating the region organization in `cnm.lsp`

**Last Updated:** October 31, 2025  
**Total Lines:** ~9,400

---

## Complete Region Hierarchy

```
cnm.lsp (9,410 lines)
│
├─ ;#region Header comments (1-68)
│  └─ Copyright, revision history, phasing documentation
│
├─ ;#region Table from search (69-1312)
│  ├─ hcnm-getphaselistfromtblqty
│  ├─ hcnm-key-table-searchandsave
│  ├─ hcnm-key-table-make
│  └─ hcnm-key-table-from-search
│
├─ ;#region Table from import (1313-1369)
│  └─ hcnm-import
│
├─ ;#region Tally (1370-2154)
│  └─ hcnm-tally (Quantity Takeoff)
│
├─ ;#region CNM Main (2155-2240)
│  ├─ c:hcnm-cnm
│  ├─ c:hcnm-cnmkt (Key Notes Table)
│  ├─ c:hcnm-cnmkti (Import)
│  └─ c:hcnm-cnmqt (Quantity Takeoff)
│
├─ ;#region Project Management (2241-2665)
│  ├─ hcnm-projinit
│  ├─ hcnm-proj (gets/validates project root)
│  ├─ hcnm-linkproj
│  └─ hcnm-makeprojtxt
│
├─ ;#region Config (2666-3645)
│  ├─ c:pretest (reactor cleanup for testing)
│  ├─ hcnm-config-definitions
│  ├─ hcnm-config-read-all-*
│  ├─ hcnm-config-temp-* (temporary config)
│  ├─ c:hcnm-config-setvar
│  ├─ c:hcnm-config-getvar
│  └─ hcnm-set-dimstyle / hcnm-restore-dimstyle
│
├─ ;#region Project Notes (3646-4563)
│  ├─ hcnm-projnotes (gets/validates project notes file)
│  ├─ hcnm-readcf (reads any format)
│  ├─ hcnm-readcftxt2
│  ├─ hcnm-readcfcsv
│  ├─ hcnm-writecftxt2
│  ├─ hcnm-writecfcsv
│  └─ hcnm-config-project-notes-format
│
├─ ;#region Project Notes Editor (4564-4639)
│  └─ c:hcnm-notesedit
│
├─ ;#region Layers Editor (4640-4697)
│  └─ c:hcnm-cnmlayer
│
├─ ;#region Misc commands (4698-4889)
│  ├─ c:haws-setnotephases
│  ├─ c:haws-cnmmenu
│  ├─ c:haws-cnmsetup
│  └─ c:haws-ntpurge
│
├─ ;#region Bubble notes utility commands (4890-5019)
│  ├─ c:hcnm-setnotesbubblestyle
│  ├─ c:haws-phaseedit
│  ├─ c:hcnm-attnoplot
│  └─ hcnm-attlayer
│
├─ ;#region Legacy LDRBLK (not for CNM) (5020-5174)
│  ├─ c:haws-tcg
│  ├─ c:haws-txtl
│  └─ haws-ldrblk (legacy bubble insertion)
│
├─ ;#region Bubble insertion and editing (5175-9405) ⭐ MAIN FOCUS
│  │
│  ├─ ;#region Bubble note insertion commands (5176-5661)
│  │  ├─ c:haws-boxl / c:haws-cirl / c:haws-dial / etc. (10 shape commands)
│  │  ├─ c:hcnm-replace-bubble
│  │  ├─ hcnm-ldrblk-insert ⭐ (MAIN INSERTION FUNCTION)
│  │  ├─ hcnm-ldrblk-get-user-start-point
│  │  ├─ hcnm-ldrblk-get-p2-data
│  │  ├─ hcnm-ldrblk-draw-bubble
│  │  ├─ hcnm-ldrblk-get-bubble-data
│  │  ├─ hcnm-ldrblk-finish-bubble
│  │  ├─ hcnm-ldrblk-get-text-entry
│  │  └─ hcnm-ldrblk-get-auto-type
│  │
│  ├─ ;#region Bubble data module (5662-5709)
│  │  ├─ hcnm-ldrblk-bubble-data-def (creates alist structure)
│  │  ├─ hcnm-ldrblk-bubble-data-get
│  │  ├─ hcnm-ldrblk-bubble-data-set
│  │  ├─ hcnm-ldrblk-bubble-data-ensure-p1-world
│  │  ├─ hcnm-ldrblk-get-ename-bubble-old
│  │  ├─ hcnm-ldrblk-bubble-leader
│  │  ├─ hcnm-ldrblk-p1-ocs
│  │  ├─ hcnm-ldrblk-get-mtext-string
│  │  ├─ hcnm-ldrblk-change-arrowhead
│  │  ├─ hcnm-ldrblk-set-dynprops
│  │  ├─ hcnm-ldrblk-lattribs-spec ⭐ (defines schema)
│  │  ├─ hcnm-ldrblk-lattribs-put-element
│  │  ├─ hcnm-ldrblk-lattribs-put-auto
│  │  ├─ hcnm-ldrblk-lattribs-validate-and-underover
│  │  ├─ hcnm-ldrblk-lattribs-validate
│  │  ├─ hcnm-ldrblk-lattribs-validate-schema ⭐ (fail-loudly)
│  │  ├─ hcnm-ldrblk-attr-has-content-p
│  │  ├─ hcnm-ldrblk-lattribs-concat
│  │  ├─ hcnm-ldrblk-lattribs-split
│  │  ├─ hcnm-ldrblk-underover-add
│  │  ├─ hcnm-ldrblk-underover-remove
│  │  ├─ hcnm-ldrblk-underover (DEPRECATED)
│  │  ├─ hcnm-ldrblk-lattribs-to-dlg
│  │  └─ hcnm-ldrblk-dlg-to-lattribs
│  │
│  ├─ ;#region Auto text dispatcher and children (6467-8699)
│  │  ├─ hcnm-ldrblk-get-auto-type-keys
│  │  ├─ hcnm-ldrblk-auto-dispatch ⭐ (router)
│  │  ├─ hcnm-ldrblk-auto-es (empty string)
│  │  ├─ hcnm-ldrblk-auto-qty (length/area from polylines)
│  │  │
│  │  ├─ ;#region Auto alignment (6704-6980)
│  │  │  ├─ hcnm-ldrblk-auto-alignment-calculate (pure calc)
│  │  │  ├─ hcnm-ldrblk-auto-al-station-to-string
│  │  │  ├─ hcnm-ldrblk-auto-al-offset-to-string
│  │  │  ├─ hcnm-ldrblk-auto-al ⭐ (main orchestrator)
│  │  │  └─ hcnm-ldrblk-auto-al-get-alignment
│  │  │
│  │  ├─ ;#region Auto NE (6981-7061)
│  │  │  └─ hcnm-ldrblk-auto-ne (Northing/Easting coords)
│  │  │
│  │  ├─ ;#region Auto pipe (7062-7378)
│  │  │  ├─ hcnm-ldrblk-auto-pipe-get-object
│  │  │  ├─ hcnm-ldrblk-auto-pipe-dia-to-string
│  │  │  ├─ hcnm-ldrblk-auto-pipe-slope-to-string
│  │  │  ├─ hcnm-ldrblk-auto-pipe-length-to-string
│  │  │  ├─ hcnm-ldrblk-auto-pipe ⭐ (main orchestrator)
│  │  │  └─ hcnm-ldrblk-auto-rtos
│  │  │
│  │  ├─ ;#region Auto surface (7379-7416)
│  │  │  ├─ hcnm-ldrblk-auto-su (elevations - UNIMPLEMENTED)
│  │  │  └─ hcnm-ldrblk-auto-apology
│  │  │
│  │  ├─ ;#region Auto helpers (7417-7456)
│  │  │  ├─ hcnm-ldrblk-space-set-model
│  │  │  └─ hcnm-ldrblk-space-restore
│  │  │
│  │  ├─ ;#region Auto text user experience interruptions (7457-7538)
│  │  │  ├─ hcnm-ldrblk-auto-type-is-coordinate-p
│  │  │  ├─ hcnm-ldrblk-capture-viewport-transform
│  │  │  └─ hcnm-ldrblk-warn-pspace-coordinates
│  │  │
│  │  ├─ ;#region Associate viewport (7539-7870)
│  │  │  ├─ hcnm-ldrblk-gateways-to-viewport-selection-prompt ⭐
│  │  │  ├─ hcnm-ldrblk-get-target-vport
│  │  │  ├─ hcnm-ldrblk-apply-transform-matrix (affine transform)
│  │  │  ├─ hcnm-ldrblk-get-viewport-transform-xdata
│  │  │  ├─ hcnm-ldrblk-get-avport-xdata (DEPRECATED)
│  │  │  ├─ hcnm-ldrblk-set-viewport-transform-xdata ⭐
│  │  │  ├─ hcnm-ldrblk-clear-viewport-transform-xdata
│  │  │  ├─ hcnm-ldrblk-set-avport-xdata (DEPRECATED)
│  │  │  └─ hcnm-ldrblk-p1-world (OCS → WCS conversion)
│  │  │
│  │  └─ ;#region XDATA (7871-8309)
│  │     ├─ hcnm-xdata-found-in-attribute-p
│  │     ├─ hcnm-split-attribute-on-xdata
│  │     ├─ hcnm-ldrblk-strip-format-codes-from-parts
│  │     ├─ hcnm-ldrblk-dwg-to-lattribs ⭐ (read from drawing)
│  │     ├─ hcnm-get-attributes
│  │     ├─ lm:fieldcode
│  │     ├─ c:hcnm-debug-bubble-xdata
│  │     │
│  │     ├─ ;#region XDATA Service Layer (8114-8308) ⭐⭐⭐
│  │     │  ├─ hcnm-xdata-read ⭐ (parse both sections)
│  │     │  ├─ hcnm-xdata-write ⭐ (atomic write point)
│  │     │  ├─ hcnm-xdata-set-vptrans (update viewport, preserve autotext)
│  │     │  ├─ hcnm-xdata-set-autotext (update autotext, preserve viewport)
│  │     │  ├─ hcnm-xdata-get-vptrans
│  │     │  └─ hcnm-xdata-get-autotext
│  │     │
│  │     ├─ hcnm-ldrblk-xdata-save (legacy wrapper)
│  │     ├─ hcnm-ldrblk-lattribs-to-dwg ⭐ (write to drawing)
│  │     ├─ hcnm-set-attributes
│  │     ├─ hcnm-ldrblk-list-reactors
│  │     ├─ hcnm-ldrblk-assure-auto-text-has-reactor ⭐
│  │     ├─ hcnm-ldrblk-reactor-callback ⭐ (VLR callback)
│  │     ├─ hcnm-ldrblk-bubble-has-leader
│  │     ├─ hcnm-ldrblk-is-on-model-tab
│  │     └─ hcnm-ldrblk-update-bubble-tag ⭐ (reactor update)
│  │
│  ├─ ;#region Bubble note editor dialog (8700-9129)
│  │  ├─ c:hcnm-edit-bubbles
│  │  ├─ hcnm-eb-add-delimiters (CHR 160 - legacy migration)
│  │  ├─ hcnm-eb-expand-value-to-delimited (CHR 160)
│  │  ├─ hcnm-edit-bubble ⭐ (main dialog function)
│  │  ├─ hcnm-eb-get-text
│  │  ├─ hcnm-edit-bubble-cancel
│  │  ├─ hcnm-eb-remove-delimiters
│  │  ├─ hcnm-eb-flatten-value
│  │  ├─ hcnm-ldrblk-eb-save
│  │  ├─ hcnm-edit-bubble-done-codes
│  │  ├─ hcnm-ldrblk-eb-migrate-legacy-format (CHR 160)
│  │  ├─ hcnm-ldrblk-eb-update-prefix
│  │  ├─ hcnm-ldrblk-eb-update-postfix
│  │  ├─ hcnm-ldrblk-eb-show
│  │  ├─ hcnm-ldrblk-eb-split-string
│  │  └─ hcnm-ldrblk-eb-concat-parts (CHR 160)
│  │
│  └─ ;#region CNM Options dialog (9130-9403)
│     └─ (External functions loaded)
│
└─ (load "ini-edit") (9406)
```

---

## Key Landmarks (⭐)

### Entry Points
- **`hcnm-ldrblk-insert`** (5189) - Main bubble insertion command
- **`hcnm-edit-bubble`** (8787) - Main bubble editor dialog
- **`c:hcnm-cnm`** (2155) - Main CNM command

### Data Structures
- **`hcnm-ldrblk-lattribs-spec`** (5892) - Defines attribute schema
- **`hcnm-ldrblk-lattribs-validate-schema`** (6085) - Fail-loudly validator
- **`hcnm-ldrblk-bubble-data-def`** (5672) - Bubble state alist

### Core Transforms
- **`hcnm-ldrblk-dwg-to-lattribs`** (7959) - Read from drawing
- **`hcnm-ldrblk-lattribs-to-dwg`** (8320) - Write to drawing
- **`hcnm-ldrblk-auto-dispatch`** (6524) - Route auto-text requests

### XDATA Service Layer (⭐⭐⭐)
- **`hcnm-xdata-read`** (8135) - Parse both VPTRANS and AUTO-TEXT sections
- **`hcnm-xdata-write`** (8193) - Atomic write (only XDATA writer)
- **Design Pattern:** "Shared document" - preserve both sections on every update

### Reactors
- **`hcnm-ldrblk-assure-auto-text-has-reactor`** (8438) - Attach VLR
- **`hcnm-ldrblk-reactor-callback`** (8534) - Update on object change
- **`hcnm-ldrblk-update-bubble-tag`** (8651) - Recalculate auto-text

---

## Navigation Tips

### VS Code

1. **Outline View:**
   - Click "Outline" in Explorer sidebar
   - Shows all functions with line numbers
   - ⚠️ Doesn't show regions (AutoLISP not fully supported)

2. **Go to Symbol:**
   - `Ctrl+Shift+O` - Jump to function in current file
   - `Ctrl+T` - Search all functions in workspace

3. **Search Regions:**
   - `Ctrl+F` then search for `;#region` or `;#endregion`
   - Use Find Widget's "Match Whole Word" for cleaner results

4. **Breadcrumbs:**
   - Enable: `View > Show Breadcrumbs`
   - Shows current function path at top
   - Click to navigate hierarchy

5. **Region Folding:**
   - VS Code doesn't recognize AutoLISP regions for folding
   - Can fold at function level: Click minus signs in gutter

### Recommended Workflow

1. **Open this map** in a split pane for reference
2. **Use search** to find region markers
3. **Bookmark key functions** (right-click line number → "Add Bookmark")
4. **Use Git Lens** to see function history

---

## Region Management Best Practices

### Adding New Functions

1. **Determine logical region** from this map
2. **Add function before `;#endregion`** of target region
3. **Update this map** if adding new subregion
4. **Run region validation** (search for unmatched markers)

### Region Validation Command

```powershell
# Count region markers
Select-String -Path "devsource/cnm.lsp" -Pattern ";#region" | Measure-Object
Select-String -Path "devsource/cnm.lsp" -Pattern ";#endregion" | Measure-Object
# Should be equal!

# Show hierarchy (with line numbers)
Select-String -Path "devsource/cnm.lsp" -Pattern ";#region|;#endregion"
```

### Common Mistakes

❌ **Forgetting closing `#endregion`**  
✅ Always add both when creating new region

❌ **Nesting too deep** (more than 3 levels)  
✅ Keep flat structure when possible

❌ **Region doesn't match actual functions**  
✅ Update map when refactoring

---

## Related Documents

- **audit-bubble-region-2025-10-31.md** - Comprehensive code audit
- **copilot-instructions.md** - Architecture principles
- **authorized-globals.md** - Global variable inventory

---

**Tip:** Print this map or keep it open in a split pane while navigating cnm.lsp!
