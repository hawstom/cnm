# Region Reorganization - Bubble Notes Module
**Date:** January 17, 2025  
**Branch:** feat-sunrise  
**File:** `devsource/cnm.lsp`  

## Summary

Reorganized bubble notes regions to improve code navigation and match architectural intent:

### Key Changes

1. **Wrapped lattribs data model** (lines 5710-6419)
   - Gathered all lattribs functions (spec, validate, schema, concat, split, underover, etc.)
   - Added descriptive header explaining 4-element structure
   - Previously scattered "lonely and cold" - now cohesive module ✅

2. **Renamed auto-text dispatcher** (line 6421)
   - Old: `;#region Auto text dispatcher and children` 
   - New: `;#region Auto-text`
   - Shorter, cleaner, more intuitive ✅

3. **Organized auto-text children by dispatch order** (lines 6538-7374)
   - Added regions for Text/mtext and Quantity (LF/SF/SY)
   - Children now appear in same order as parent `cond`:
     1. Text/mtext (es)
     2. Quantity LF/SF/SY (qty)
     3. Alignment (al)
     4. NE coordinates (ne)
     5. Pipe networks (pipe)
     6. Surface elevations (su)
   - Helpers grouped beneath their children ✅

4. **Added Reactors region** (line 8268)
   - Wrapped reactor callback and update functions
   - Previously unwrapped, causing region imbalance ✅

5. **Fixed region imbalance**
   - Removed extra `#endregion` at line 9297
   - **Result: 32 opens, 32 closes - perfectly balanced** ✅

---

## Region Structure Tree

```
5175: ;#region Bubble insertion and editing
  5176: ;#region Bubble note insertion commands main and loops
  5662: ;#region Bubble data module
    5710: ;#region lattribs data model ⭐ NEW
  6421: ;#region Auto-text ⭐ RENAMED (was "Auto text dispatcher and children")
    6538: ;#region Auto text/mtext ⭐ NEW
    6565: ;#region Auto quantity (LF/SF/SY) ⭐ NEW
    6661: ;#region Auto alignment
    6938: ;#region Auto NE
    7019: ;#region Auto pipe
    7336: ;#region Auto surface
    7374: ;#region Auto helpers
    7414: ;#region Auto text user experience interruptions
  7497: ;#region Associate viewport
  7829: ;#region XDATA
    8072: ;#region XDATA Service Layer
  8268: ;#region Reactors ⭐ NEW
  8658: ;#region Bubble note editor dialog

9023: ;#region CNM Options dialog
```

---

## Visual Hierarchy

```
┌─────────────────────────────────────────────────────────────┐
│ Bubble insertion and editing (5175-9022)                    │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Insertion commands (5176-5661)                       │  │
│  │  • c:haws-boxl, c:haws-cirl, etc.                   │  │
│  │  • hcnm-ldrblk-insert (main dispatcher)             │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Bubble data module (5662-6420)                       │  │
│  │                                                       │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ lattribs data model (5710-6419) ⭐ NEW        │  │  │
│  │  │  • hcnm-bubbles-lattribs-spec                 │  │  │
│  │  │  • hcnm-bubbles-lattribs-validate             │  │  │
│  │  │  • hcnm-bubbles-lattribs-schema               │  │  │
│  │  │  • hcnm-bubbles-lattribs-concat               │  │  │
│  │  │  • hcnm-bubbles-lattribs-split                │  │  │
│  │  │  • hcnm-bubbles-lattribs-underover            │  │  │
│  │  │  • ...and more lattribs transforms            │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Auto-text (6421-7496) ⭐ RENAMED                    │  │
│  │                                                       │  │
│  │  • hcnm-ldrblk-auto-dispatch (cond router)          │  │
│  │                                                       │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ 1. Auto text/mtext (6538-6564) ⭐ NEW         │  │  │
│  │  │    • hcnm-ldrblk-auto-es                      │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ 2. Auto quantity LF/SF/SY (6565-6660) ⭐ NEW  │  │  │
│  │  │    • hcnm-ldrblk-auto-qty                     │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ 3. Auto alignment (6661-6937)                 │  │  │
│  │  │    • hcnm-ldrblk-auto-al (station/offset)     │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ 4. Auto NE (6938-7018)                        │  │  │
│  │  │    • hcnm-ldrblk-auto-ne (Northing/Easting)   │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ 5. Auto pipe (7019-7335)                      │  │  │
│  │  │    • hcnm-ldrblk-auto-pipe (Civil 3D pipes)   │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ 6. Auto surface (7336-7373)                   │  │  │
│  │  │    • hcnm-ldrblk-auto-su (elevations)         │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ Auto helpers (7374-7413)                      │  │  │
│  │  │    • Shared utilities for auto-text           │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ Auto UX interruptions (7414-7496)             │  │  │
│  │  │    • User prompts and warnings                │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Associate viewport (7497-7828)                       │  │
│  │  • hcnm-ldrblk-gateways-to-viewport-selection-prompt│  │
│  │  • Paper space viewport association logic           │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ XDATA (7829-8267)                                    │  │
│  │                                                       │  │
│  │  ┌────────────────────────────────────────────────┐  │  │
│  │  │ XDATA Service Layer (8072-8266)               │  │  │
│  │  │  • hcnm-xdata-get-* / hcnm-xdata-set-*        │  │  │
│  │  │  • Clean interface for XDATA operations       │  │  │
│  │  └────────────────────────────────────────────────┘  │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Reactors (8268-8657) ⭐ NEW                          │  │
│  │  • hcnm-ldrblk-reactor-callback                      │  │
│  │  • hcnm-ldrblk-update-bubble-tag                     │  │
│  │  • VLR-OBJECT-REACTOR management                     │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │ Bubble note editor dialog (8658-9022)                │  │
│  │  • c:hcnm-edit-bubbles                               │  │
│  │  • hcnm-ldrblk-eb-* (edit box functions)             │  │
│  │  • DCL dialog interaction                            │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│ CNM Options dialog (9023-9296)                              │
│  • c:hcnm-cnmoptions                                        │
│  • hcnm-config-dcl-* (configuration callbacks)              │
│  • hcnm-debug-show-bubble-reactor-xdata                     │
└─────────────────────────────────────────────────────────────┘
```

---

## Validation Results

```powershell
Opens:   32
Closes:  32
Balance: 0

✅ SUCCESS: All regions perfectly balanced!
```

---

## Benefits

1. **Better Navigation**: VS Code folding now accurately reflects module boundaries
2. **Architectural Clarity**: Code organization matches data flow and dependencies
3. **Maintainability**: Related functions grouped logically (lattribs together, auto-text children ordered)
4. **Discoverability**: Auto-text dispatch order matches child function order
5. **Aesthetic Improvement**: Clean, consistent naming and nesting

---

## Technical Notes

### lattribs Data Model
- **Lines 5710-6419**: Complete set of functions for manipulating attribute lists
- **Structure**: `'(("TAG" "prefix" "auto" "postfix") ...)`
- **Purpose**: Transform between concatenated display text and structured prefix/auto/postfix format
- **Key Functions**:
  - `hcnm-bubbles-lattribs-spec`: Schema definition (4-element lists)
  - `hcnm-bubbles-lattribs-validate`: Strict validation (fail loudly)
  - `hcnm-bubbles-lattribs-concat`: Join prefix + auto + postfix
  - `hcnm-bubbles-lattribs-split`: Parse concatenated text using XDATA search needles
  - `hcnm-bubbles-lattribs-underover`: Split multi-line text into separate attributes

### Auto-text Dispatcher
- **Line 6421**: Parent region for all auto-text functionality
- **Dispatch Order**: Children now match `cond` statement order in `hcnm-ldrblk-auto-dispatch`
- **Pattern**: Each auto-text type has its own region with helpers beneath
- **Integration**: All use lattribs data model for consistent attribute handling

### Reactors
- **Lines 8268-8657**: VLR-OBJECT-REACTOR callback and update logic
- **Purpose**: Auto-update bubble notes when reference objects change (alignments, pipes, surfaces)
- **Key Challenge**: Paper space coordinate transformation via XDATA viewport transforms
- **Status**: Under active development (feat-sunrise branch)

---

## Related Work

- **Lost Work Recovery**: Attempted to recover uncommitted changes from ~24 hours ago with `#region.*model` pattern
  - Searched: git reflog, 34 dangling commits, stashes, VS Code history
  - Result: Work permanently lost (no backups)
  - Solution: Recreated with improvements per user requirements ✅
  
- **User Requirements**:
  - ✅ "Gather lattribs functions and wrap them in tersely named region"
  - ✅ "Innovate shorter name for auto-text dispatcher"
  - ✅ "Ensure child functions same order as parent cond"
  - ✅ "Apply whatever aesthetic sense you can find"
  - ✅ "Make another pretty drawing like audit report"

---

## Git Commit Message

```
feat(bubbles): reorganize regions for better navigation and clarity

- Wrap lattribs data model in dedicated region (5710-6419)
- Rename "Auto text dispatcher" → "Auto-text" (shorter, cleaner)
- Add regions for Text/mtext and Quantity auto-text types
- Order auto-text children to match dispatch cond sequence
- Add Reactors region wrapper (8268-8657)
- Fix region imbalance (remove extra #endregion at 9297)

Result: 32 opens, 32 closes - perfectly balanced ✅

Benefits:
- Lattribs functions no longer "lonely and cold"
- VS Code folding matches architectural intent
- Auto-text children follow dispatch order
- Improved code navigation and maintainability
```
