# CNM Performance Benchmarking

**Purpose:** Simple performance benchmarking for CNM development  
**Location:** `devtools/scripts/`  
**Status:** Production-ready, minimal system  

## Quick Start

```lisp
;; Load system
(load "c:\\TGHFiles\\programming\\hawsedc\\develop\\devtools\\scripts\\performance-benchmark.lsp")

;; Test config system (Phase 3 findings: 40ms bottleneck)
(c:benchmark-config)

;; View results anytime
(c:benchmark-report)

;; Test other components
(c:benchmark-xdata)    ; XDATA performance
(c:benchmark-smoke)    ; Verify system works
```

## Key Phase 3 Findings

**Config System Bottleneck:**
- **40ms per config read** in CNM wrapper (`c:hcnm-config-getvar`)
- **Root cause:** `hcnm-proj` project folder validation on every call
- **Optimization potential:** 97.5% improvement (40ms → 1ms) via session caching

## Custom Benchmarking

```lisp
;; Method 1: Instrument existing functions (modify source code)
(defun my-existing-function (param1 param2 / start result)  ; start localized
  (setq start (benchmark-start "my-existing-function"))
  ;; ... original function code ...
  (setq result (original-calculation param1 param2))
  (benchmark-end "my-existing-function" start)
  result  ; Return original result
)

;; Method 2: External testing (no source modification)
(defun test-my-slow-operation (/ start)
  (setq start (benchmark-start "slow-operation-test"))
  ;; Call the actual function you suspect is slow
  (my-potentially-slow-function "test-data")
  (benchmark-end "slow-operation-test" start)
)

;; Method 3: Test with realistic data (AI agent can help create)
(defun test-bubble-insertion (/ start test-alignment)
  (setq test-alignment (car (entsel "\nSelect alignment for test: ")))
  (setq start (benchmark-start "bubble-insertion"))
  (command "_CNMI")  ; Trigger actual CNM insertion
  (benchmark-end "bubble-insertion" start)
)

;; View accumulated data
(benchmark-report)
```

## Testing Approaches

**1. Built-in Tests (Ready to Use):**
- `BENCHMARK-CONFIG` - Already tests the known 40ms bottleneck
- `BENCHMARK-XDATA` - Tests XDATA read/write performance
- `BENCHMARK-SMOKE` - Verifies timing system works

**2. Instrument Existing Code (Modify Source):**
Add `benchmark-start/end` calls inside functions you suspect are slow. Good for production monitoring.

**3. External Testing (No Source Changes):**
Create test functions that call existing operations with realistic data. Good for investigating without modifying working code.

**4. AI-Assisted Analysis:**
When you suspect a bottleneck, ask AI agent to:
- Analyze the function and predict slow parts
- Create realistic test scenarios with appropriate data
- Design stress tests with multiple iterations
- Interpret timing results and suggest optimizations

## Files

**Essential (2 files only):**
- **`performance-benchmark.lsp`** - Complete benchmarking system (160 lines)
- **`README-performance.md`** - This documentation

## Commands

- **`BENCHMARK-CONFIG`** - Test config system bottleneck
- **`BENCHMARK-XDATA`** - Test XDATA performance  
- **`BENCHMARK-SMOKE`** - Verify system works
- **`BENCHMARK-REPORT`** - View results
- **`BENCHMARK-RESET`** - Clear data

## System Design

**Minimal:** 160 lines of code, 2 files total
**Zero overhead:** Only loads when explicitly requested
**Self-contained:** No dependencies beyond AutoLISP
**Focused:** Solves specific CNM performance analysis needs

---

**Total system size:** ~200 lines of code vs. previous 1000+ lines