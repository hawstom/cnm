# CNM Reactor Performance Testing

## Overview

Testing suite for CNM bubble reactor callback performance. The reactor system automatically updates bubble notes when referenced objects (alignments, pipes, leaders) change.

## Quick Start

1. **Load Test System**
   ```autolisp
   (load "devtools/scripts/test-reactor-performance.lsp")
   (c:test-reactor-quick)
   ```

2. **Prepare Test Drawing**
   - Open drawing with Civil 3D alignments, pipes, or surfaces
   - Insert CNM bubble notes with auto-text (Station/Offset, coordinates, pipe diameter, etc.)
   - Verify bubbles have reactive auto-text (check for XDATA)

3. **Run Performance Tests**
   ```autolisp
   (c:benchmark-reactor)         ; Interactive test - 10 callbacks
   (c:benchmark-reactor-stress)  ; Stress test - 100 callbacks
   ```

## Test Types

### Interactive Test (`BENCHMARK-REACTOR`)
- **Purpose**: Quick performance check with user feedback
- **Method**: Moves leader 10 times, triggering reactor callbacks
- **Duration**: ~10-30 seconds
- **Good for**: Initial performance assessment

### Stress Test (`BENCHMARK-REACTOR-STRESS`)
- **Purpose**: Statistical analysis with many samples
- **Method**: 100 micro-movements triggering callbacks  
- **Duration**: 30-60 seconds
- **Good for**: Identifying performance bottlenecks, regression testing

## Performance Expectations

Based on CNM architecture and AutoLISP performance characteristics:

- **< 20ms per callback**: ✅ Excellent (typical for simple auto-text)
- **20-50ms per callback**: ⚡ Acceptable (complex calculations, multiple Civil 3D queries)
- **> 50ms per callback**: ⚠️  Investigate (possible bottlenecks in calculation pipeline)

## Common Performance Factors

### Fast Operations
- Coordinate auto-text (N/E/NE) with cached viewport transforms
- Simple station calculations on straight alignments
- Pipe diameter lookups (cached VLA properties)

### Slower Operations  
- Station/offset on complex alignments with many curves
- Surface elevation queries (TIN interpolation)
- Multiple auto-text fields per bubble (compound calculations)
- Paper space bubbles requiring coordinate transformations

### Bottlenecks to Check
- **XDATA read/write**: Each callback reads and writes bubble XDATA
- **VLA property access**: Civil 3D object property queries
- **Coordinate transformations**: Paper space to model space conversions
- **Config system calls**: `hcnm-config-getvar` calls within callback

## Test Results Analysis

The benchmark system provides automatic analysis:

```
REACTOR PERFORMANCE ANALYSIS
========================================
Callbacks triggered: 100
Average callback time: 25.4ms
Total reactor time: 2540.0ms

⚡ MODERATE: 20-50ms per callback
📊 Performance acceptable for typical use
```

## Integration with Phase 3 Optimizations

These reactor tests complement the Phase 3 config optimization:

- **Before Phase 3**: Config calls within callbacks added 40ms overhead
- **After Phase 3**: Config system optimized to <1ms per call
- **Net improvement**: Reactor callbacks should show dramatic improvement

## Troubleshooting

### No Reactive Bubbles Found
- Insert bubbles with auto-text options
- Verify bubbles have XDATA: `(entget bubble-entity '("HCNM-BUBBLE"))`
- Check reactor system status: `(c:test-reactor-validation)`

### Reactor Not Triggering
- Verify global reactor exists: `*hcnm-bubble-reactor*` 
- Check BlockReactors flag: `(hcnm-config-getvar "BlockReactors")` should be "0"
- Test manual trigger: Move leader with grips

### Poor Performance
- Profile individual operations within callback
- Check for config bottlenecks (should be resolved in Phase 3)
- Consider XDATA size - large XDATA slows reads/writes
- Review calculation complexity for auto-text types

## Files

- `performance-benchmark.lsp` - Core benchmarking system
- `test-reactor-performance.lsp` - Reactor-specific test setup
- `README-performance.md` - This documentation

## Development Notes

The reactor callback (`hcnm-ldrblk-reactor-callback`) already includes profiling hooks using `haws-profile-start/end`. The benchmark tests leverage this existing instrumentation to measure real-world performance.

Reactor callbacks are triggered by AutoCAD's VLR (VLA Reactor) system when monitored objects change. The callback must complete quickly to avoid blocking the AutoCAD UI.