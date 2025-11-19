# CNM Test Suite - Debugging Guide

**Created:** 2025-11-18  
**Purpose:** Help debug test script failures without pulling your hair out

---

## Quick Debugging Workflow

### 1. Run Tests One at a Time

**Don't run the whole script first!** Copy individual test blocks to AutoCAD command line.

**Example - TEST 1 only:**
```
HAWS-ELLL
850,600
800,500
1

.
sta
800,500
(test-validate-auto-text (entlast) "NOTETXT1" "STA 3+50.00" "TEST 1: Sta (Model)")
```

**What to watch:**
- Does HAWS-ELLL prompt correctly?
- Does bubble appear at expected location?
- Does auto-text show correct value?
- Does validation report PASS or FAIL?

### 2. Verify Prerequisites Before Any Tests

**Check layout:**
```
(getvar "CTAB")
; Should return: "TEST-LAYOUT"
```

**Check space:**
```
(getvar "CVPORT")
; For model space tests: Should be > 1 (viewport number)
; For paper space tests: Should be 1 (paper space)
```

**Check objects exist:**
```
; Alignment exists?
(setq ss (ssget "X" '((0 . "AECC_ALIGNMENT"))))
; Should return selection set with 1 alignment

; Pipe exists?
(setq ss (ssget "X" '((0 . "AECC_PIPE"))))
; Should return selection set with 1 pipe

; Polylines exist?
(setq ss (ssget "X" '((0 . "LWPOLYLINE"))))
; Should return selection set with 2+ polylines
```

### 3. Check Bubble Creation After Each Test

**After HAWS-ELLL command:**
```
(entlast)
; Should return: <Entity name: 7fffXXXXXXXX>

(cdr (assoc 0 (entget (entlast))))
; Should return: "INSERT" (block insertion)

(cdr (assoc 2 (entget (entlast))))
; Should return: "cnm-bubble-m-1-r" or similar
```

### 4. Check Auto-Text Value Manually

**After auto-text inserted:**
```
; Get attribute value
(setq en-bubble (entlast))
(setq attribs (hcnm-bn-dwg-to-lattribs en-bubble))
(cdr (assoc "NOTETXT1" attribs))
; Should return: "STA 3+50.00" (or expected value)
```

### 5. Common Failure Modes

#### Failure: "HAWS-ELLL not found"
**Cause:** CNM not loaded  
**Fix:** `(load "cnm.lsp")`

#### Failure: Bubble appears at wrong location
**Cause:** Wrong coordinates for model vs paper space  
**Fix:** 
- Model space: Large coordinates (800,600)
- Paper space: Small coordinates (15.5,16.0)

#### Failure: "Alignment not found" or empty auto-text
**Cause:** Selection coordinates miss the object  
**Fix:** 
- Paper space selections use MODEL coordinates
- Check alignment exists at expected location
- Try `(ssget "_C" '(800 500) '(800 500))` to test selection

#### Failure: CVPORT issues in paper space
**Cause:** Not in correct space  
**Fix:** 
- Start on TEST-LAYOUT: `(getvar "CTAB")` should be "TEST-LAYOUT"
- Use MSPACE for model space tests
- Use PSPACE for paper space tests
- Check CVPORT: >1 = viewport active, 1 = paper space

#### Failure: Viewport confirmation hangs
**Cause:** Script waiting for Enter on N/E/NE tests  
**Fix:** Blank line in script provides Enter key automatically

### 6. Incremental Testing Strategy

**Phase 1: Verify Setup**
```
(load "cnm-test-validation.lsp")
(c:test-setup-layers)
; Check layer created: (tblsearch "LAYER" "C-ANNO-TEST-RESULTS")
```

**Phase 2: Test Model Space Insertion (TEST 1)**
```
mspace
HAWS-ELLL
850,600
800,500
1

.
sta
800,500
; Manually inspect bubble - does it show "STA 3+50.00"?
```

**Phase 3: Test Paper Space Insertion (TEST 7)**
```
pspace
HAWS-ELLL
15.5,16.0
15.0,15.5
7

.
sta
800,500
; Manually inspect bubble - does it show "STA 5+50.00"?
```

**Phase 4: Test Coordinate-Based (TEST 13)**
```
pspace
HAWS-ELLL
15.5,13.0
15.0,12.5
13

.
n
[ENTER when prompted for viewport]
; Manually inspect bubble - does it show "N 800.00"?
```

**Phase 5: Run Full Script**
Only after individual tests work!

### 7. Expected Values Quick Reference

**Model Space Tests:**
- TEST 1 (Sta): Arrow (850,600), Alignment at y=500 → "STA 3+50.00"
- TEST 2 (Dia): Pipe 18" → "18\""
- TEST 3 (N): Arrow y=600 → "N 600.00"
- TEST 4 (LF): 100' line → "100.00 LF"
- TEST 5 (Manual): "MODEL MANUAL"
- TEST 6 (AlName): Alignment "OAK ST" → "OAK ST"

**Paper Space Tests (through 1/100XP viewport):**
- TEST 7 (Sta): Paper (15.5,16.0) → Model (1050,1100) → "STA 5+50.00"
- TEST 8 (Off): Paper (18.5,16.0) → Model (1350,1100) → "600.00 RT"
- TEST 9 (StaOff): Paper (21.5,16.0) → Model (1650,1100) → "STA 11+50.00  600.00 RT"
- TEST 10 (StaName): Paper (24.5,16.0) → Model (1950,1100) → "STA 14+50.00 OAK ST"
- TEST 11 (Dia): Pipe through viewport → "18\""
- TEST 12 (Slope): Pipe through viewport → "-0.67%"
- TEST 13 (N): Paper (15.5,13.0) → Model (1050,800) → "N 800.00"
- TEST 14 (E): Paper (18.5,13.0) → Model (1350,800) → "E 1350.00"
- TEST 15 (NE): Paper (21.5,13.0) → Model (1650,800) → "N 800.00, E 1650.00"
- TEST 16 (Manual): "PAPER MANUAL"

### 8. Validation Function Testing

**Test validation functions directly:**
```
; After creating a bubble with STA auto-text
(setq en (entlast))
(test-validate-auto-text en "NOTETXT1" "STA 3+50.00" "Manual Test")
; Should print: PASS in green or FAIL in red

(test-validate-xdata en "NOTETXT1" "Sta" "Manual Test")
; Should print: PASS if XDATA composite-key correct

(test-validate-reactor en "NOTETXT1" "Sta" "Manual Test")
; Should print: PASS if reactor attached
```

### 9. Emergency Recovery

**If script crashes AutoCAD:**
1. Save drawing immediately after crash
2. Check crash location (what line number?)
3. Comment out that test in script
4. Run remaining tests
5. Debug failed test separately

**If drawing gets corrupted:**
1. Close without saving
2. Reopen cnm-test.dwg
3. Fix script issue
4. Try again

**If you need to start over:**
```
; Clear all test bubbles
(setq ss (ssget "X" '((0 . "INSERT") (2 . "cnm-bubble-*"))))
(command "._ERASE" ss "")

; Reload validation functions
(load "cnm-test-validation.lsp")
```

---

## Remember

**Scripts are fragile!** One typo breaks everything. Debug incrementally, not all at once.

**Expected workflow:**
1. Run setup only
2. Run TEST 1 only (model space)
3. Run TEST 7 only (paper space)
4. Run TEST 13 only (coordinate-based paper space)
5. If all 4 work, try full script

**Good luck!** 🎯
