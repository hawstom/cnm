# CNM Test Suite

**Purpose:** Comprehensive regression testing and performance benchmarking for CNM bubble notes system

**Created:** 2025-11-17  
**Status:** Foundation Setup Complete (Phase B)  
**Next:** Auto-Text Tests Implementation (Phase C)

---

## Quick Start

### For Human Testers

**Preparation (one-time):**
1. Open `cnm-test.dwg` in Civil 3D
2. Prepare Civil 3D objects as specified in "Drawing Preparation" section below
3. Save as template

**Running Tests:**
- **Method 1 (Drag & Drop):** Drag `cnm-test.scr` into AutoCAD drawing window
- **Method 2 (Command):** Type `cnm-test-script` at AutoCAD command line (future wrapper)

**Results:**
- Script generates `cnm-test-report.md` with pass/fail status
- MTEXT annotations on `C-ANNO-TEST-RESULTS` layer show detailed results
- Summary MTEXT at top of drawing shows overall status

**Human Review:**
- Copy report file contents
- Paste to AI for analysis
- AI identifies failures and suggests fixes

---

## Drawing Preparation Requirements

**Starting point:** Empty drawing in model space with World UCS current
- Remove all CNM-Demo objects (we're not using that script)
- Model space (not paper space)
- World UCS active (not rotated/translated)
- No existing objects

**Human must prepare `cnm-test.dwg` with the following Civil 3D objects:**

### Object Placement Strategy
**Goal:** Convenient grouped layout for visual inspection + zoom extents coverage

**Suggested layout (all objects visible in single zoom extents):**
- Group 1 (left): Polylines at (100,100) and (100,250)
- Group 2 (center): Alignment from (500,500) to (1500,500)
- Group 3 (lower left): Pipe from (300,300) to (600,300)
- Text: (50,50) - visible near polylines

**Zoom:** Script will use `zoom extents` - all objects should fit comfortably in view

### 1. Alignment: "OAK ST"
- **Geometry:** Perfectly horizontal, single tangent segment
- **Start:** (500, 500)
- **End:** (1500, 500)
- **Length:** 1000.0 feet exactly
- **Station Range:** 0+00 to 10+00
- **Purpose:** Predictable coordinates for Sta/Off/StaOff/AlName/StaName tests

### 2. Pipe Network: "Storm"
- **Geometry:** Perfectly horizontal single pipe
- **Start Manhole:** (300, 300, 100.0)
- **End Manhole:** (600, 300, 98.0)
- **Diameter:** 18 inches (1.5 feet)
- **Slope:** -2.0 feet over 300 feet = -0.67%
- **Purpose:** Dia/Slope/L auto-text tests
- **Note:** Reactor tests will move end manholes (not use STRETCH command)

### 3. Surface: "EG" (DEFERRED - Not Phase 1)
- **Status:** Z auto-text behavior not yet defined
- **Future:** Simple TIN surface when Z auto-text implemented
- **Phase 1:** Skip all Z elevation tests

### 4. Polylines for LF/SF/SY Tests
- **Rectangle (closed):** (100,100) → (200,100) → (200,200) → (100,200) → close
  - Dimensions: 100' × 100' = 10,000 SF = 1,111.11 SY
- **Line:** (100,250) → (200,250)
  - Length: 100.0 LF exactly
- **Purpose:** Static auto-text calculation tests

### 5. Text for ES (Copy Text) Tests
- **MTEXT:** Located at (50,50)
- **Justify:** Middle center (MC)
- **Height:** 10 (or any readable size - AI doesn't care, human readability helpful)
- **Content:** "TEST STRING"
- **Purpose:** ES (copy text) auto-text test

### 6. Paper Space Layout (RECOMMENDED - Phase E preparation)
**Layout name:** "TEST-LAYOUT"

**Viewport specifications:**
- **Corners:** (0.5, 0.5) to (35.5, 23.5) - standard paper space limits
- **Zoom:** `ZOOM Center 1300,700 1/100XP`
- **Locking:** Locked (prevents accidental pan/zoom)
- **Purpose:** Phase E viewport VPTRANS tests

**Model space to paper space mapping:**
- Model space alignment: (500,500) to (1500,500)
- Paper space alignment: (10,10) to (20,10)
- **How's that for nice and tidy?** ✓

**Note:** Model space tests (Phase C-D) don't need this, but having it ready helps Phase E
---

## Test Suite Architecture

### File Structure
```
test-suite/
├── README.md                      # This file
├── cnm-test.dwg                   # Prepped drawing with Civil 3D objects
├── cnm-test.scr                   # Main test script (drag into drawing)
├── cnm-test.pro                   # CNM project notes template
├── cnm-test-validation.lsp        # AutoLISP validation functions
├── cnm-test-report.md             # Generated test results (AI reads this)
└── test-results/                  # Future: Saved test run archives
```

### Test Categories (Phases)

**Phase C: Auto-Text Tests (17 types)**
- Alignment-based: Sta, Off, StaOff, AlName, StaName
- Pipe-based: Dia, Slope, L
- Coordinate-based: N, E, NE
- Surface-based: Z (deferred)
- Static calculation: LF, SF, SY, ES
- Manual entry: Text

**Phase D: Reactor Tests**
- Leader stretch triggers coordinate updates
- Alignment modification triggers Sta/Off updates
- Pipe movement triggers Dia/Slope updates
- Reactor attachment validation
- Reactor cleanup on bubble deletion

**Phase E: Viewport Tests**
- Paper space bubble insertion with VPTRANS
- Viewport stretch applies coordinate transform
- Multiple viewport handling
- VPTRANS cleanup validation

**Phase F: Performance Benchmarks**
- 50 bubble insertion timing (baseline)
- 20 reactor update timing (performance regression)
- Memory usage monitoring (future)

---

## AI Execution Model

### How AI Uses This Test Suite

1. **AI writes .scr scripts** that simulate user input at AutoCAD command line
2. **AI writes validation functions** in AutoLISP to query drawing state
3. **AI cannot see drawing directly** but can:
   - Query object properties via AutoLISP
   - Create MTEXT annotations with test results
   - Generate markdown report file for analysis
4. **Human runs tests**, AI reads report file and identifies issues

### Report File Pattern

**Script execution:**
```autolisp
; Test generates results during execution
(test-validate-station (entlast) "NOTETXT1" "Expected STA 3+00")
; Writes to cnm-test-report.md
```

**Human workflow:**
- "Drag cnm-test.scr into cnm-test.dwg and let me know when I can examine cnm-test-report.md"
- Or: "Invoke c:cnm-test-script. It will generate report file when complete."

**AI workflow:**
- Human: "Tests complete, examine report"
- AI reads `cnm-test-report.md`
- AI identifies failures and suggests fixes

---

## Configuration Setup

### Predictable Test Outcomes

Use `hcnm-config-setvar` to configure expected output format:

```autolisp
; Example: Configure pipe diameter format
(hcnm-config-setvar "BubbleTextPrefixPipeDia" "Ø")
(hcnm-config-setvar "BubbleTextPostfixPipeDia" "\"")
(hcnm-config-setvar "BubbleTextPrecisionPipeDia" "0")
; Result: "Ø18\""
```

**Multiple test scenarios:**
- Settings stored in `cnm.ini` in test project folder
- Can prepare multiple `cnm.ini` versions in subfolders
- Switch scenarios by copying appropriate `cnm.ini`

---

## Current Implementation Status

### ✅ Phase A: File Cleanup (COMPLETE)
- Archived debugging scripts
- Organized devtools/ structure
- Created reference folder

### ✅ Phase B: Foundation Setup (COMPLETE)
- Copied CNM-Demo files to test-suite/
- Created folder structure
- Created this README documentation
- Ready for test implementation

### ⏸️ Phase C: Auto-Text Tests (NEXT)
- NOT YET IMPLEMENTED
- Will create comprehensive auto-text validation
- 17 auto-text types with XDATA/reactor verification

### ⏸️ Phase D-G: Reactor/Viewport/Performance (FUTURE)
- Awaiting Phase C completion

---

## Development Notes

### Script Execution Behavior

**CRITICAL: Alerts and dialogs do NOT pause script execution**
- `(alert "message")` displays but script continues immediately
- DCL dialogs display but script continues immediately
- **No RESUME required** for alerts/dialogs (script never pauses)
- **RESUME only needed** when:
  - Unknown command error occurs
  - AutoLISP error/crash occurs
- **Performance testing implication:** Alerts/dialogs not acceptable (add time, don't pause)

**Blank lines in .scr files:**
- Blank lines = Enter commands (can break script flow)
- Remove all unnecessary blank lines
- Use comments (`;`) instead of blank lines for readability

### Script Modularity Philosophy

**Initial approach:** Single monolithic `cnm-test.scr` file (simplest)

**If script grows large:** Can evolve to programmatic assembly pattern
- Example: `mscript.lsp` builds .scr from snippets before execution
- Allows conditional test selection via menu
- Paradigm: Dynamic file-building from components, not static sub-scripts

### Test Annotation System

**Layer:** `C-ANNO-TEST-RESULTS`

**Color coding:**
- Green (3) = PASS
- Red (1) = FAIL
- Cyan (4) = INFO (performance reports)

**Regression detection:** Compare test results layer between runs

---

## Related Documentation

- **Planning:** `.ai-plans/test-suite-expansion-plan.md`
- **Architecture:** `devtools/docs/standards/05-architecture.md`
- **Performance:** `devtools/scripts/README-reactor-performance.md`

---

**Next Steps:** Implement Phase C (Auto-Text Tests) - estimated 2-3 hours
