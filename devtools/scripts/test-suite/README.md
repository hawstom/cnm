# CNM Test Suite

**Purpose:** Comprehensive regression testing and performance benchmarking for CNM bubble notes system

**Created:** 2025-11-17  
**Status:** All Phases Complete (A-J + F)  
**Current:** 42 tests implemented and validated

---

## Quick Start

### For Human Testers

**Preparation:**
1. Optional: Delete `cnm.ini` and `constnot.csv` before starting (script auto-cleans at end)
2. Open `cnm-test.dwg` in Civil 3D

**Running Tests:**
- **Method 1 (Drag & Drop):** Drag `cnm-test.scr` into AutoCAD drawing window
- **Method 2 (Command):** Type `cnm-test-script` at AutoCAD command line (future wrapper)

**Results:**
- Script generates `test-results/cnm-test-report.md` with pass/fail status
- Script generates `test-results/cnm-test-run.log` with timing data
- MTEXT annotations on `C-ANNO-TEST-RESULTS` layer show detailed results
- Summary MTEXT at top of drawing shows overall status

**Human Review:**
- Human evaluates finished state of cnm-test.dwg. Did test finish completely? Are positional adjustments needed?

**AI Review:**
- AI analyzes test-results/cnm-test-report.md, identifies failures, and suggests fixes

### Object Placement Strategy
**See:** `.ai-plans/test-suite-layout-phase-c.md` for complete coordinate system specification

**Summary:**
- Coordinate offsets: X +10000, Y +20000 (civil engineering convention)
- Viewport: 36"×24" paper @ 1/20XP scale
- Bubble grid: 6 columns × 10 rows, symmetric from centerline
- Test objects: Alignment, pipe, line, rectangle with precise coordinates

### 1. Alignment: "OAK ST"
- **Geometry:** Perfectly horizontal, single tangent segment
- **Start:** (10040', 20240')
- **End:** (10680', 20240')
- **Length:** 640 feet exactly
- **Station Range:** 50+00 to 56+40 (civil engineering convention)
- **Purpose:** Predictable coordinates for Sta/Off/StaOff/AlName/StaName tests

### 2. Pipe Network: "18\" STORM"
- **Geometry:** Perfectly horizontal single pipe
- **Start Manhole:** (10040', 20200', 100.0)
- **End Manhole:** (10680', 20200', 98.0)
- **Length:** 640 feet
- **Diameter:** 18 inches (1.5 feet)
- **Start elevation:** 100.00'
- **End elevation:** 98.00'
- **Slope:** -0.31%
- **Purpose:** Dia/Slope/L auto-text tests

### 3. Surface: "EG" (DEFERRED - Not Phase 1)
- **Status:** Z auto-text behavior not yet defined
- **Future:** Simple TIN surface when Z auto-text implemented
- **Phase 1:** Skip all Z elevation tests

### 4. Polylines for LF/SF/SY Tests
- **Curb Line:** (10040', 20260') → (10680', 20260')
  - Length: 640.0 LF exactly
  - Offset: 20' north of alignment
- **Sidewalk Rectangle (closed):** (10040',20266') → (10680',20272')
  - Dimensions: 640' × 6' = 3,840 SF = 426.67 SY
  - Offset: 26' to 32' north of alignment
- **Purpose:** Static auto-text calculation tests (LF/SF/SY)

### 5. Test String for ES (Copy Text) Tests
- **MTEXT:** Located at (10360', 20220')
- **Justify:** Middle center (MC)
- **Height:** 2' (readable)
- **Content:** "OAK ST FROM MTEXT"
- **Purpose:** ES (copy text from string) auto-text test

### 6. Paper Space Layout (RECOMMENDED - Phase E preparation)
**Layout name:** "TEST-LAYOUT"

**Viewport specifications:**
- **Size:** 36" × 24" (ANSI D, full page)
- **Center (paper):** (18", 12")
- **Center (model):** (10360', 20240')
- **Scale:** 1/20XP (1" = 20')
- **Locking:** Locked (prevents accidental pan/zoom)
- **Purpose:** Phase E viewport VPTRANS tests

**Note:** Model space tests (Phase C-D) don't need this, but having it ready helps Phase E
## Bubble Note Placement Guidelines

**See:** `.ai-plans/test-suite-layout-phase-c.md` Section 4 for complete bubble grid system

**Summary:**
- Grid positions: 1L-3L (left), 1R-3R (right), 1U-3U (up), 1D-4D (down)
- Column spacing: 60' model = 3" paper @ 1/20 scale
- Row spacing: 10' model = 0.5" paper @ 1/20 scale
- Leader geometry: ±5' horizontal, ±10' vertical offset

**Example positioning:**
```
; Position 1L-1U (Column 1 Left, Row 1 Up)
HAWS-ELL
10340,20250    ; Arrow point
10335,20260    ; Bubble location (5' left, 10' up from arrow)
```

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
└── test-results/                  # Generated files (gitignored)
    ├── cnm-test-report.md         # Test results (AI reads this)
    └── cnm-test-run.log           # Timing log (START/END timestamps)
```

**Cleanup:** Script auto-deletes `cnm.ini`, `constnot.csv`, `cnm-test.csv` at end

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
- Insertion performance (20 bubbles with Station auto-text)
- Reactor update performance (20 leader stretches)
- Baseline timing for regression detection

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

### ✅ Phase C: Auto-Text Tests (COMPLETE)
- 8 model space tests (Sta, Dia, N, LF, Manual, AlName, SF, SY)
- 10 paper space tests (VPTRANS coordinate transform validation)
- XDATA and reactor attachment verification
- All 18 auto-text tests passing

### ✅ Phase D-G: Reactor/Viewport/Performance (COMPLETE)
- Reactor attachment validated
- VPTRANS viewport coordinate transforms working
- Performance benchmarks implemented (Phase F)
  - TEST 39: Insertion performance (20 bubbles, baseline timing)
  - TEST 40: Reactor performance (20 updates, callback overhead)
  - Establishes regression detection baseline

### ✅ Phase H: Key Notes Table (COMPLETE)
- Table generation validated
- .not file creation verified
- Quantity totals tested

### ✅ Phase I: Quantity Takeoff (COMPLETE)
- QT table generation validated
- Single-sheet test confirmed
- CSV integration working

### ✅ Phase J: CNM Plus Utilities (COMPLETE)
- haws-contvol (contour volume)
- haws-ut (utility function - 2 tests)
- haws-mof (multiple offset)
- haws-lotnum (lot numbering)
- haws-newpro (profile drafter)

---

## CNM Behavior Notes (Test Expectations)

### LF/SF/SY Auto-Text Uses Field Expressions (CORRECT)

**Behavior:** CNM creates AutoCAD field expressions for polyline Length/Area calculations
- **Field format:** `%<\AcObjProp Object(%<\_ObjId ...>%).Length \f "%lu2%pr0%ct8[1]">% LF`
- **Example:** Instead of static "640.00 LF", you see field code in attribute text
- **Why:** Fields update automatically when polyline geometry changes (dynamic values)
- **Test validation:** Check for field code presence with correct suffix (LF/SF/SY)
- **Not a bug:** This is intentional CNM design for maintaining accuracy

**Impact on test expectations:**
- TEST 4 (LF): Validates field code exists with "LF" suffix
- TEST 7 (SF): Validates field code exists with "SF" suffix  
- TEST 8 (SY): Validates field code exists with "SY" suffix
- Tests do NOT validate numeric values (fields evaluate at display time)

### StaOff and Slope Formatting Specified by CNM.ini

**Behavior:** Format controlled by CNM.ini configuration settings
- **StaOff format:** `BubbleTextFormatStaOff` setting (default: `"STA %s, %s %s"` = comma separator)
- **Slope format:** `BubbleTextFormatSlope` setting (default: `"%s%%"`)
- **Test expectations:** Match CNM.ini defaults in test drawing
  - TEST 11 StaOff: `"STA 51+20.00, 10.00 LT"` (comma separator)
  - TEST 14 Slope: `"0.31%"` (as calculated, no forced sign)

**Note:** Slope value reflects actual calculation (positive/negative based on elevation delta), not forced formatting

### LF/SF/SY Do NOT Use XDATA/Reactors (Field-Only)

**Behavior:** LF/SF/SY are field-based only, NOT part of CNM's reactive auto-text system
- **No XDATA:** Field expressions stored directly in attribute text, no XDATA composite keys
- **No reactors:** Fields update via AutoCAD's field system, not VLR-OBJECT-REACTOR callbacks
- **No VPTRANS:** Field references are object handles, not coordinates needing viewport transform
- **Test expectations:** Assert ABSENCE of XDATA for LF/SF/SY tags (PASS if no XDATA found)

**Architecture difference:**
- **Reactive auto-text (Sta/Off/Dia/etc.):** Uses XDATA + VLR reactors for dynamic updates
- **Field-only (LF/SF/SY):** Uses AutoCAD built-in field system (simpler, no CNM overhead)

---

## Performance Analysis

### TEST 39: Insertion Performance (Baseline: 1552 ms/bubble)

**Status:** BASELINE ESTABLISHED - Lower priority (user reaction delay mitigates)

**No dialog overhead:** CNM bubble insertion has NO dialog prompts - all parameters provided via script

**Suspected bottlenecks:**
1. **Block insertion overhead:** Could `entmake` be faster than `command` for block insertion?
2. **Civil 3D queries:** Alignment/pipe property retrieval (vlax-get-property)
3. **XDATA writes:** Extended entity data storage
4. **Reactor attachment:** VLR-OBJECT-REACTOR setup per bubble

**Profiling strategy needed:**
- Instrument insertion path with `(getvar "MILLISECS")` checkpoints
- Isolate each operation: block insert, XDATA write, reactor attach, Civil 3D query
- Compare `entmake` vs `command` for block insertion
- Identify which operation consumes most time

**Target:** <500ms excellent, 500-1000ms acceptable, >1000ms investigate

### TEST 40: Reactor Performance (Baseline: 773 ms/stretch, ~193 ms/callback)

**Status:** PRIORITY CONCERN - Must be "blinding speed" (happens in specious present)

**Critical requirement:** Reactor updates must feel instant (< 100ms perceived as instant)

**Current performance:** 193 ms/callback significantly exceeds instant threshold

**Suspected bottlenecks:**
1. **Civil 3D property queries:** Every reactor callback queries alignment/pipe properties
2. **XDATA reads/writes:** Searching and updating auto-text in XDATA
3. **Attribute updates:** Writing new values to block attributes
4. **String formatting:** Generating formatted auto-text (Sta/Off/Slope/etc.)

**Profiling strategy needed:**
- Instrument `hcnm-ldrblk-reactor-callback` with millisecond checkpoints
- Measure: Civil 3D query time, XDATA read, XDATA write, attribute update, formatting
- Identify dominant bottleneck
- Consider caching frequently-accessed object properties

**Optimization opportunities:**
- Cache Civil 3D object properties (alignment/pipe handles → property cache)
- Minimize XDATA parsing overhead
- Batch attribute updates if multiple auto-texts per bubble
- Profile each formatting function (Sta, Off, StaOff, Slope, etc.)

**Target:** <20ms fast, 20-50ms moderate, >50ms slow, >100ms noticeable lag

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
