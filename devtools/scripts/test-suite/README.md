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

**Human must prepare `cnm-test.dwg` with the following Civil 3D objects:**

### 1. Alignment: "OAK ST"
- **Geometry:** Perfectly horizontal, single tangent segment
- **Start:** (500, 500, 100.0)
- **End:** (1500, 500, 100.0)
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
- **Content:** "TEST STRING"
- **Purpose:** ES (copy text) auto-text test

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
