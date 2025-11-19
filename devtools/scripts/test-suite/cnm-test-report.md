# CNM Test Suite Report

**Date:** 2025-50-18 22:50:56

**Status:** Running tests...

---

## Test Results

### TEST 1: Sta (Model)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "STA 53+00.00"
Actual:   "STA 53+00.00"
```

### TEST 1 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Sta"
XDATA composite-key: ("Sta" . "E0F")
XDATA auto-text: "STA 53+00.00"
```

### TEST 1 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Sta"
Reactor validation: SKIPPED (API unclear)
Handle: 23092
```

### TEST 2: Dia (Model)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "18""
Actual:   "18""
```

### TEST 2 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Dia"
XDATA composite-key: ("Dia" . "E97")
XDATA auto-text: "18""
```

### TEST 2 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Dia"
Reactor validation: SKIPPED (API unclear)
Handle: 232E6
```

### TEST 3: N (Model)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "N 20260.00"
Actual:   "N 20260.00"
```

### TEST 3 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "N"
XDATA composite-key: ("N" . "")
XDATA auto-text: "N 20260.00"
```

### TEST 3 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "N"
Reactor validation: SKIPPED (API unclear)
Handle: 2353A
```

### TEST 4: LF (Model)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected: "640.00 LF"
Actual:   "%<\AcObjProp Object(%<\_ObjId 1531949738736>%).Length \f "%lu2%pr0%ct8[1]">% LF"
```

### TEST 4 (XDATA)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "LF"
XDATA composite-key: nil
XDATA auto-text: "<<NIL>>"
```

### TEST 5: Manual (Model)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "MODEL MANUAL"
Actual:   "MODEL MANUAL"
```

### TEST 6: Name (Model)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "OAK ST"
Actual:   "OAK ST"
```

### TEST 6 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "AlName"
XDATA composite-key: ("AlName" . "E0F")
XDATA auto-text: "OAK ST"
```

### TEST 6 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "AlName"
Reactor validation: SKIPPED (API unclear)
Handle: 23C39
```

### TEST 7: SF (Model)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected: "3840.00 SF"
Actual:   "%<\AcObjProp Object(%<\_ObjId 1531949738480>%).Area \f "%lu2%pr0%ct8[1]">% SF"
```

### TEST 7 (XDATA)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "SF"
XDATA composite-key: nil
XDATA auto-text: "<<NIL>>"
```

### TEST 8: SY (Model)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected: "426.67 SY"
Actual:   "%<\AcObjProp Object(%<\_ObjId 1531949738480>%).Area \f "%lu2%pr0%ct8[0.11111111]">% SY"
```

### TEST 9: Sta (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "STA 50+00.00"
Actual:   "STA 50+00.00"
```

### TEST 9 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Sta"
XDATA composite-key: ("Sta" . "E0F")
XDATA auto-text: "STA 50+00.00"
```

### TEST 9 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Sta"
Reactor validation: SKIPPED (API unclear)
Handle: 2433B
```

### TEST 10: Off (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "20.00 LT"
Actual:   "20.00 LT"
```

### TEST 10 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Off"
XDATA composite-key: ("Off" . "E0F")
XDATA auto-text: "20.00 LT"
```

### TEST 10 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Off"
Reactor validation: SKIPPED (API unclear)
Handle: 24592
```

### TEST 11: StaOff (Paper)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected: "STA 51+20.00  10.00 LT"
Actual:   "STA 51+20.00, 10.00 LT"
```

### TEST 11 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "StaOff"
XDATA composite-key: ("StaOff" . "E0F")
XDATA auto-text: "STA 51+20.00, 10.00 LT"
```

### TEST 11 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "StaOff"
Reactor validation: SKIPPED (API unclear)
Handle: 247E7
```

### TEST 12: StaName (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "STA 56+40.00 OAK ST"
Actual:   "STA 56+40.00 OAK ST"
```

### TEST 12 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "StaName"
XDATA composite-key: ("StaName" . "E0F")
XDATA auto-text: "STA 56+40.00 OAK ST"
```

### TEST 12 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "StaName"
Reactor validation: SKIPPED (API unclear)
Handle: 24A3C
```

### TEST 13: Dia (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "18""
Actual:   "18""
```

### TEST 13 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Dia"
XDATA composite-key: ("Dia" . "E97")
XDATA auto-text: "18""
```

### TEST 13 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Dia"
Reactor validation: SKIPPED (API unclear)
Handle: 24C91
```

### TEST 14: Slope (Paper)

**Status:** FAIL

**Details:**
```
Tag: NOTETXT1
Expected: "-0.31%"
Actual:   "0.31%"
```

### TEST 14 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Slope"
XDATA composite-key: ("Slope" . "E97")
XDATA auto-text: "0.31%"
```

### TEST 14 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "Slope"
Reactor validation: SKIPPED (API unclear)
Handle: 24EE5
```

### TEST 15: N (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "N 20260.00"
Actual:   "N 20260.00"
```

### TEST 15 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "N"
XDATA composite-key: ("N" . "")
XDATA auto-text: "N 20260.00"
```

### TEST 15 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "N"
Reactor validation: SKIPPED (API unclear)
Handle: 25139
```

### TEST 16: E (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "E 10380.00"
Actual:   "E 10380.00"
```

### TEST 16 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "E"
XDATA composite-key: ("E" . "")
XDATA auto-text: "E 10380.00"
```

### TEST 16 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "E"
Reactor validation: SKIPPED (API unclear)
Handle: 2538E
```

### TEST 17: NE (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "N 20230.00, E 10440.00"
Actual:   "N 20230.00, E 10440.00"
```

### TEST 17 (XDATA)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "NE"
XDATA composite-key: ("NE" . "")
XDATA auto-text: "N 20230.00, E 10440.00"
```

### TEST 17 (Reactor)

**Status:** INFO

**Details:**
```
Tag: NOTETXT1
Expected auto-type: "NE"
Reactor validation: SKIPPED (API unclear)
Handle: 255E3
```

### TEST 18: Manual (Paper)

**Status:** PASS

**Details:**
```
Tag: NOTETXT1
Expected: "PAPER MANUAL"
Actual:   "PAPER MANUAL"
```


---

## Summary

- **Total Tests:** 18

- **Model Space Tests:** 8

- **Paper Space Tests:** 10


**Note:** Review test entries above for PASS/FAIL status.

Review drawing annotations on C-ANNO-TEST-RESULTS layer for visual results.

