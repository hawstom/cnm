CNM REACTIVE AUTO-TEXT TEST LOG
===============================

**Date:** November 12, 2025  
**Issue:** All reactions killed by editing  
**Status:** Bug in editor system, NOT insertion system

---

# TEST SEQUENCE AND RESULTS

## Phase 1: Initial Testing

### 1. Insert bubble with auto-text → Should work normally
**Result:** ✅ **Works well.** Delay is a bit much.

### 2. Stretch leader, alignment and pipe  
**Result:** ✅ **Works well.** Delay is a bit much.

### 3. Edit dialog → modify auto-text → Should register with reactor properly
**Result:** ✅ **Appears to work.**

### 4. Stretch leader → Should update handleless auto-text (AlName, LF, Slope)
**Result:** ❌ **CRASH.**

**Stack Trace:**
```
Line 8119
Exception has occurred.
bad argument type: VLA-OBJECT T

<1> :ERROR-BREAK (Unknown Source:0)
[2] (vla-get-Handle T) (Unknown Source:0)
[3] (HCNM-LB-AUTO-AL (...) "NOTETXT1" "AlName" T) (c:\TGHFiles\programming\hawsedc\develop\devsource\cnm.lsp:8119)
[4] (HCNM-LB-AUTO-DISPATCH "NOTETXT1" "AlName" T (...)) (c:\TGHFiles\programming\hawsedc\develop\devsource\cnm.lsp:7549)
[5] (HCNM-LB-GENERATE-NEW-AUTO-TEXT <Entity name: 23925276f50> nil (...)) (c:\TGHFiles\programming\hawsedc\develop\devsource\cnm.lsp:11545)
[6] (HCNM-LB-UPDATE-BUBBLE-TAG "29C5D" "NOTETXT1" "AlName" "") (c:\TGHFiles\programming\hawsedc\develop\devsource\cnm.lsp:11640)
[7-12] (reactor callback chain)
```

### 5. Change alignment/pipe → Should update handle-based auto-text
- **Pipe:** ❌ No reaction
- **Alignment:** ❌ No reaction

---

# DEBUG ANALYSIS

## Debug Function: `c:hcnm-debug-bubble`

### Before Edit (Handle: 29ECA)
```
=== LATTRIBS (from attributes) ===
  NOTETXT0: ""; NOTETXT6: ""; NOTETXT5: ""; NOTETXT4: ""; NOTETXT3: ""
  NOTETXT2: "STA 16+38.36, 23.17 LT"
  NOTETXT1: "2.25%"
  NOTEGAP: ""; NOTEPHASE: ""; NOTENUM: "1"

=== XDATA (extended entity data) ===
  VPTRANS (viewport transform): (2 (0.0 0.0 0.0) (677324.0 918391.0 0.0) (1.0 0.0 0.0) (677341.0 918401.0 0.0) (0.0 1.0 0.0) (677314.0 918408.0 0.0))
  AUTO-TEXT:
    NOTETXT1 = ((("Slope" . "2232D") . "2.25%"))
    NOTETXT2 = ((("StaOff" . "1FE8E") . "STA 16+38.36, 23.17 LT"))

=== REACTOR INFO ===
  Bubble handle: 29ECA
  Total object reactors: 1
  HCNM-BUBBLE reactor: FOUND
  This bubble in reactor data: NO (reactor won't update this bubble!)

=== VALIDATION ===
  ✅ All validations PASSED
```

### After Edit (Handle: 29C5D)
```
=== LATTRIBS (from attributes) ===
  NOTETXT0: ""; NOTETXT6: ""; NOTETXT5: ""; NOTETXT4: ""
  NOTETXT3: "STA 16+33.14 OAK ST"
  NOTETXT2: "STA 16+33.14, 33.40 LT OAK ST"  
  NOTETXT1: "2.52%"
  NOTEGAP: ""; NOTEPHASE: ""; NOTENUM: "1"

=== XDATA (extended entity data) ===
  VPTRANS (viewport transform): (2 (0.0 0.0 0.0) (677324.0 918391.0 0.0) (1.0 0.0 0.0) (677341.0 918401.0 0.0) (0.0 1.0 0.0) (677314.0 918408.0 0.0))
  AUTO-TEXT:
    NOTETXT1 = ((("Slope" . "2232D") . "2.52%") (("AlName" . "") . "OAK ST"))
    NOTETXT2 = ((("StaOff" . "1FE8E") . "STA 16+33.14, 33.40 LT") (("AlName" . "") . "OAK ST"))
    NOTETXT3 = ((("StaName" . "") . "STA 16+33.14 OAK ST"))

=== REACTOR INFO ===
  Bubble handle: 29C5D
  Total object reactors: 1
  HCNM-BUBBLE reactor: FOUND
  This bubble in reactor data: NO (reactor won't update this bubble!)

=== VALIDATION ===
  ✅ All validations PASSED
```

## Debug Function: `c:hcnm-debug-reactor-dump`

**Complete Reactor Data Structure:**
```
Total object reactors: 1
HCNM-BUBBLE reactor found:
  Reactor data structure:
(("HCNM-BUBBLE" 
  (("" 
    (("297D7" (("NOTETXT1" (("N" ""))) ("NOTETXT2" (("E" ""))) ("NOTETXT3" (("AlName" ""))) ("NOTETXT4" (("LF" ""))) ("NOTETXT5" (("Slope" ""))) ("NOTETXT6" (("Sta" "") ("Off" ""))))) 
     ("29C5D" (("NOTETXT1" (("AlName" ""))) ("NOTETXT2" (("AlName" ""))) ("NOTETXT3" (("StaName" "")))))))
  ("297D2" 
    (("297D7" (("NOTETXT1" (("N" ""))) ("NOTETXT2" (("E" ""))) ("NOTETXT3" (("AlName" ""))) ("NOTETXT4" (("LF" ""))) ("NOTETXT5" (("Slope" ""))) ("NOTETXT6" (("Sta" "") ("Off" "")))))))
  ("2232D" 
    (("29C5D" (("NOTETXT1" (("Slope" "2232D"))))) 
     ("29ECA" (("NOTETXT1" (("Slope" "2232D")))))))
  ("29C58" 
    (("29C5D" (("NOTETXT1" (("AlName" ""))) ("NOTETXT2" (("StaOff" "1FE8E") ("AlName" ""))) ("NOTETXT3" (("StaName" "")))))))
  ("1FE8E" 
    (("29C5D" (("NOTETXT2" (("StaOff" "1FE8E"))))) 
     ("29ECA" (("NOTETXT2" (("StaOff" "1FE8E")))))))
  ("29EC5" 
    (("29ECA" (("NOTETXT2" (("StaOff" "1FE8E")))))))
)))
```

---

# CRITICAL INSIGHTS

## Key Evidence:

1. **Insertion Phase:** ✅ All reactions work correctly
2. **Before Editing:** Both "Slope" and "StaOff" auto-text present and reactive
3. **After Editing:** Added "AlName" and "StaName" auto-text, but ALL reactions stop working
4. **Debug Function Bug:** Reports "NO" for reactor data presence when reactor dump shows bubble IS present
5. **Handle Change:** Bubble handle changed from 29ECA → 29C5D during editing process

## Architecture Contradiction:

- **Reactor data shows:** Bubble 29C5D IS registered under multiple owners (2232D, 29C58, 1FE8E)
- **Debug function reports:** "This bubble in reactor data: NO"
- **Reality:** No reactions occur despite data structure appearing correct

## The Real Problem:

**The editor system is breaking reactor functionality**, not the insertion system. The issue is likely:
1. **VLA-Object attachment problem** - Objects not properly attached to reactor despite correct data structure
2. **Debug function bug** - Misreporting reactor status, masking real issue
3. **Editor-specific corruption** - Some step in the edit process breaks the VLA-Object monitoring

---

# FOCUS AREAS FOR INVESTIGATION:

1. **Editor save process** - What breaks reactor VLA-Object attachments?
2. **Debug function accuracy** - Why does it report "NO" when data exists?
3. **VLA-Object monitoring** - Are the actual objects still attached after editing?

**NOT** insertion logic, fallback mechanisms, or classification changes - those work correctly.