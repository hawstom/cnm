# Archived Development Scripts

**Purpose:** Historical debugging and diagnostic scripts that served specific one-time purposes.

---

## Files in This Archive

### diagnose-reactor-xdata-bug.lsp
**Date:** November 2025  
**Purpose:** Single-purpose diagnostic for reactor XDATA storage bug  
**Status:** Bug fixed in current architecture (reactor system refactored)  
**Historical Value:** Shows debugging approach for XDATA-related reactor issues

### repair-mixed-handle-xdata.lsp
**Date:** November 2025  
**Purpose:** One-time repair script for mixed handle storage bug  
**Status:** Bug fixed, no users affected (pre-release)  
**Historical Value:** Documents data migration pattern for XDATA schema changes

### test-anonymous-detection.lsp
**Date:** November 2025  
**Purpose:** Verification of Lee Mac's anonymous block detection method  
**Status:** Concept proven and integrated into main codebase  
**Historical Value:** Shows anonymous block detection implementation research

---

## When to Archive Scripts

Scripts should be archived (not deleted) when:
- Bug is fixed and script no longer needed
- Concept proven and integrated into production code
- One-time data migration completed
- Historical value for understanding system evolution

Scripts should be deleted when:
- Temporary test with no historical value
- Hardcoded to specific scenario with no reusability
- Superseded by better implementation with no educational value

---

**Note:** Archived scripts are kept in version control for historical reference but are not loaded or maintained.
