# Reactor Performance Profiling Enhancement

**Date:** 2025-11-29  
**Status:** Ready for Testing  
**Related:** TEST 40 (baseline ~193ms/callback)

## Objective

Add fine-grained millisecond checkpoints to reactor callback to isolate performance bottlenecks.

## Baseline Performance

From TEST 40:
- Total time: 773ms for 4 stretch operations
- Average: **~193ms per callback**
- Target: **<100ms** for "instant" user perception

## Profiling Checkpoints Added

### 1. hcnm-bn-update-bubble-tag (Main Update Function)

**Added timing for:**
- **Read state** (t-read-state): Read lattribs and current attribute text
- **XDATA read** (t-xdata-read): Extract old auto-text from XDATA
- **Generate** (t-generate): Call auto-dispatch to regenerate auto-text
- **Replace** (t-replace): Smart-replace old auto-text with new
- **XDATA write**: Update XDATA with new verbatim value
- **Attribute write**: Write formatted attributes to drawing

**Output format:**
```
    [PROFILE] XDATA write: Xms
    [PROFILE] Attribute write: Yms
    [PROFILE BREAKDOWN] Read state: Ams, XDATA read: Bms, Generate: Cms, Replace: Dms
```

### 2. hcnm-bn-auto-dispatch (Routing Function)

**Added timing for:**
- Total time to call appropriate auto-type handler (Sta/Off/StaOff/Dia/Slope/N/E/NE/etc.)

**Output format:**
```
      [PROFILE] Auto-dispatch (StaOff): Xms
```

### 3. hcnm-bn-auto-pipe-dia-to-string (Civil 3D Property Query)

**Added timing for:**
- **Civil3D query** (t-civil3d-query): vlax-get-property call for pipe diameter
- **Config+format** (t-format): Config lookups + rtos formatting

**Output format:**
```
        [PROFILE Dia] Civil3D query: Xms, Config+format: Yms
```

### 4. hcnm-bn-auto-pipe-slope-to-string (Civil 3D Property Query)

**Added timing for:**
- **Civil3D query** (t-civil3d-query): vlax-get-property call for pipe slope
- **Config+format** (t-format): Config lookups + rtos formatting

**Output format:**
```
        [PROFILE Slope] Civil3D query: Xms, Config+format: Yms
```

## Expected Output Sample

When TEST 40 runs with profiling enabled:

```
[PROFILE] reactor-callback: 193ms
      [PROFILE] Auto-dispatch (Dia): 45ms
        [PROFILE Dia] Civil3D query: 35ms, Config+format: 10ms
    [PROFILE] XDATA write: 8ms
    [PROFILE] Attribute write: 12ms
    [PROFILE BREAKDOWN] Read state: 5ms, XDATA read: 3ms, Generate: 45ms, Replace: 2ms
```

## Analysis Strategy

### 1. Run TEST 40 with Enhanced Profiling
Capture all timing output to identify slowest operation(s).

### 2. Expected Bottlenecks (Hypotheses)

**Hypothesis 1: Civil 3D Property Queries Dominate**
- vlax-get-property calls are synchronous COM calls
- May require Civil 3D to recalculate alignment geometry
- **Mitigation:** Cache queries, batch requests, or optimize COM calls

**Hypothesis 2: XDATA Operations Are Slow**
- Multiple reads/writes per bubble update (non-atomic design)
- **Mitigation:** Accumulate updates, write once per bubble (architecture change)

**Hypothesis 3: String Formatting/Config Lookups**
- Multiple config-getvar calls per update (prefix/postfix/precision)
- rtos formatting
- **Mitigation:** Cache config values

**Hypothesis 4: Attribute Write Operations**
- VLA property updates trigger AutoCAD regeneration
- **Mitigation:** Batch attribute updates, defer regeneration

### 3. Prioritize Optimization

Focus on operation with highest cumulative time:
- If Civil3D queries dominate → Optimize COM calls
- If XDATA dominates → Refactor to atomic updates
- If config dominates → Add config caching layer
- If attributes dominate → Batch VLA operations

## Files Modified

- `devsource/cnm.lsp`:
  - `hcnm-bn-update-bubble-tag`: Added 6 timing checkpoints
  - `hcnm-bn-auto-dispatch`: Added 1 timing checkpoint
  - `hcnm-bn-auto-pipe-dia-to-string`: Added 2 timing checkpoints
  - `hcnm-bn-auto-pipe-slope-to-string`: Added 2 timing checkpoints

## Testing Instructions

1. Open cnm-test.dwg in Civil 3D
2. Ensure CNM is loaded
3. Run cnm-test.scr (includes TEST 40 - reactor performance)
   - Method: Drag cnm-test.scr into AutoCAD drawing window
   - Or use: SCRIPT command and select cnm-test.scr
4. Capture console output with timing details
4. Analyze timing breakdown to identify bottleneck
5. Report findings in this document

## Next Steps

After profiling results:
1. Document timing breakdown in this file
2. Identify primary bottleneck operation
3. Research optimization strategies for that operation
4. Create focused optimization plan
5. Implement and re-test

## Notes

- All profiling uses `(getvar "MILLISECS")` for consistency
- Profiling adds ~5-10ms overhead (acceptable for diagnosis)
- Can be disabled by commenting out princ statements once bottleneck identified
- Indentation in output shows call hierarchy (visual debugging aid)
