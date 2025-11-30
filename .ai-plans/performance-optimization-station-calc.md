# Performance Optimization: Station Calculation Bottleneck

**Status:** Proposal  
**Created:** 2025-11-30  
**Bottleneck:** Station/offset calculations taking 1000-2000ms per reactor callback  
**Impact:** 20 stretch operations = 20-40 seconds of blocking UI during TEST 40

---

## Executive Summary

Profiling data shows `hcnm-bn-auto-al` (station/offset calculation) is the primary bottleneck:
- **Average:** 1000-2000ms per callback
- **Breakdown:** 95%+ time in "Generate" phase (Civil3D API calls)
- **Quick wins:** Config caching, coordinate caching, batch VLA queries
- **Big win:** Async calculation (complex, requires reactor architecture changes)

---

## Profiling Evidence

```
[PROFILE] Auto-dispatch (Sta): 1000-2000ms  ← BOTTLENECK
  [PROFILE BREAKDOWN] 
    Read state: 0-47ms (fast)
    XDATA read: 0-15ms (fast)
    Generate: 1000-2000ms (SLOW - Civil3D queries)  ← ROOT CAUSE
    Replace: 0-16ms (fast)

Compare to:
[PROFILE] Auto-dispatch (Dia): 200-300ms
[PROFILE] Auto-dispatch (Slope): 150-250ms
[PROFILE] XDATA write: 0-32ms
[PROFILE] Attribute write: 0-32ms
```

**Root cause:** `StationAtPoint` and `OffsetAtPoint` Civil3D API calls are expensive.

---

## Optimization Strategies (Ordered by Impact/Effort Ratio)

### QUICK WIN 1: Cache Config Reads (10-20ms savings per callback)
**Effort:** Low (1 hour)  
**Risk:** None  
**Impact:** Small but immediate

**Problem:** Every callback reads config values from registry/INI:
```autolisp
(hcnm-config-getvar "BubbleTextPrefixSta")   ; Registry read
(hcnm-config-getvar "BubbleTextPostfixOff+") ; Registry read
(hcnm-config-getvar "BubbleTextPrecisionOff+") ; Registry read
```

**Solution:** Session-scope cache for frequently-used format settings:
```autolisp
;; In haws-config-definitions (edclib.lsp), add derived variables:
(list "CachedStationFormat" "" 0)  ; Computed once per session
(list "CachedOffsetFormat" "" 0)   ; Computed once per session

;; On first use, build format string and cache:
(defun hcnm-bn-get-station-format ( / fmt)
  (setq fmt (haws-config-getvar "HawsEDC" "CachedStationFormat" nil nil))
  (if (= fmt "")
    (progn
      (setq fmt 
        (strcat
          (hcnm-config-getvar "BubbleTextPrefixSta")
          "%s"  ; Placeholder for station value
          (hcnm-config-getvar "BubbleTextPostfixSta")
        )
      )
      (haws-config-setvar "HawsEDC" "CachedStationFormat" fmt nil nil)
    )
  )
  fmt
)
```

**Benefit:** Eliminates 3-6 config reads per callback (registry I/O avoided).

---

### QUICK WIN 2: Cache p1-world Coordinate (50-100ms savings per callback)
**Effort:** Low (2 hours)  
**Risk:** Low (requires testing paper space bubbles)  
**Impact:** Moderate

**Problem:** Every reactor callback recalculates p1-world from leader arrowhead:
```autolisp
;; In hcnm-bn-update-bubble-tag (called on EVERY stretch):
(setq p1-world (hcnm-bn-ensure-p1-world ...))  ; Viewport transform every time
```

**Solution:** Store p1-world in bubble XRECORD, only recalculate when leader moves:
```autolisp
;; Structure: ("P1WORLD" x y z timestamp)
;; timestamp = (getvar "MILLISECS") when last calculated

(defun hcnm-bn-get-cached-p1-world (ename-bubble ename-leader / xrec data timestamp age)
  (setq xrec (hcnm-xrecord-get ename-bubble "P1WORLD"))
  (setq data (cdr xrec))
  (setq timestamp (nth 3 data))
  (setq age (- (getvar "MILLISECS") timestamp))
  (if (< age 100)  ; Cache valid for 100ms (reactor bursts)
    (list (car data) (cadr data) (caddr data))  ; Return cached point
    nil  ; Cache expired, recalculate
  )
)
```

**Benefit:** Avoids expensive viewport transform for rapid-fire callbacks (user dragging leader).

---

### MEDIUM WIN: Batch VLA Property Queries (100-200ms savings)
**Effort:** Medium (4 hours)  
**Risk:** Medium (requires understanding Civil3D object model)  
**Impact:** Moderate to High

**Problem:** Civil3D API calls happen one-at-a-time:
```autolisp
(vlax-get-property obj-align 'Length)           ; API call 1
(vlax-invoke obj-align 'StationAtPoint p1 p2)   ; API call 2
(vlax-invoke obj-align 'OffsetAtPoint p1 p2)    ; API call 3
```

**Solution:** Query all properties at once (if Civil3D supports):
```autolisp
;; Research: Does Civil3D support bulk property queries?
;; If yes, use VLA-GET-PROPERTIES or similar

;; Alternative: Cache alignment object properties in session:
(defun hcnm-bn-cache-alignment-props (handle-align / obj props)
  (setq obj (handent handle-align))
  (setq props
    (list
      (cons "Length" (vlax-get-property obj 'Length))
      (cons "Name" (vlax-get-property obj 'Name))
      ;; ... other properties
    )
  )
  (haws-config-setvar "HawsEDC" 
    (strcat "AlignCache_" handle-align) 
    (vl-prin1-to-string props)  ; Serialize to string
    nil nil
  )
)
```

**Benefit:** Reduces API round-trips for repeated queries on same alignment.

---

### BIG WIN: Async Calculation (500-1500ms savings)
**Effort:** High (2-3 days)  
**Risk:** High (requires reactor architecture redesign)  
**Impact:** Dramatic UX improvement

**Problem:** Reactor callbacks BLOCK AutoCAD UI while calculating:
```
User drags leader → Reactor fires → UI FROZEN for 1000ms → Update complete → UI unfrozen
```

**Solution:** Defer calculation to idle time:
```autolisp
;; Phase 1: Reactor fires, queue calculation, return immediately
(defun hcnm-bn-reactor-callback (obj-notifier obj-reactor parameter-list)
  (hcnm-bn-queue-update (vla-get-handle obj-notifier))
  ;; Return immediately (UI stays responsive)
)

;; Phase 2: Idle timer processes queue
(defun hcnm-bn-process-update-queue ( / queue-item handle-bubble)
  (while (setq queue-item (hcnm-bn-dequeue-update))
    (setq handle-bubble (car queue-item))
    (hcnm-bn-update-bubble-deferred handle-bubble)  ; Actual calculation
    ;; Yield every 50ms to keep UI responsive
    (if (> (- (getvar "MILLISECS") *last-yield*) 50)
      (command)  ; Yield to AutoCAD command processor
    )
  )
)

;; Start idle timer on CNM load:
(vl-load-reactor nil '((:vlr-idleBegin . hcnm-bn-process-update-queue)))
```

**Benefits:**
- User can continue dragging while calculation happens in background
- Multiple updates batch together (drag 5 times → 1 calculation)
- Perceived performance 10x better

**Risks:**
- Stale data visible briefly (user sees old station while calculating)
- Reactor architecture needs queue management
- Edge cases: What if user saves drawing while queue processing?

---

## Recommendation

**Phase 1 (Implement Now):**
1. Quick Win 1: Config caching (1 hour, 10-20ms savings)
2. Quick Win 2: Coordinate caching (2 hours, 50-100ms savings)

**Total effort:** 3 hours  
**Expected savings:** 60-120ms per callback (6-12% faster)

**Phase 2 (Research First):**
1. Medium Win: Batch VLA queries (need to verify Civil3D API supports this)
2. Big Win: Async calculation (requires design review with human)

**Phase 3 (Future):**
1. Profile other auto-types (Dia, Slope) for similar patterns
2. Consider caching Civil3D object handles session-wide
3. Investigate if Civil3D has event callbacks we can subscribe to (alignment modified → invalidate cache)

---

## Questions for Human

1. **Is 1000ms per station calc normal for Civil3D API?** Or is there a faster method?
2. **Do users drag leaders rapidly?** Or mostly one-at-a-time edits? (Affects value of caching)
3. **Async calculation:** Worth the complexity? Or is current performance acceptable?
4. **Testing availability:** Can you test performance patches on real projects with 100+ bubbles?

---

## Success Metrics

**Before optimization:**
- TEST 40: 20-40 seconds total (1000-2000ms per callback)

**After Phase 1 (Quick Wins):**
- TEST 40: 18-36 seconds total (900-1800ms per callback)
- Target: 10% faster, zero risk

**After Phase 2 (if pursued):**
- TEST 40: 10-20 seconds total (500-1000ms per callback)
- Target: 50% faster, acceptable risk

**After Phase 3 (if pursued):**
- TEST 40: 2-8 seconds total (100-400ms per callback)
- Target: 80% faster, high complexity

---

## Next Steps

1. Human reviews this proposal
2. Human answers questions above
3. AI implements Phase 1 (Quick Wins) if approved
4. AI profiles again, measures actual improvement
5. Human decides whether Phase 2 worth the effort
