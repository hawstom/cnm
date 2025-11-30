# HAWS-CONFIG Project Management Circular Dependency

**Created:** 2025-11-18  
**Issue:** Hidden bug in project initialization  
**Status:** Analysis Complete - Awaiting Review

---

## Executive Summary

**Problem:** `hcnm-proj` calls `hcnm-initialize-project`, which calls `hcnm-config-getvar "AppFolder"`, which calls `hcnm-proj`, creating infinite recursion when initializing a new project (no cnm.ini exists yet).

**Root Cause:** HAWS-CONFIG migration (commit dac2c4c) introduced dependency on project folder determination (`hcnm-proj`) for reading Project-scope variables, but project initialization itself needs to get AppFolder before project exists. [TGH 2025-11-18 01:48:34: It seems that there was an intention to forestall this by setting AppFolder early in edclib.lsp ]

**Solution:** AppFolder should be Session-scope (not Project-scope) to break circular dependency. [TGH 2025-11-18 01:50:17: This maybe can work. But it isn't the legacy solution. Is it wise to retool? Are you sure?]

---

## Technical Analysis

### 1. The Circular Call Chain

**Sequence when opening drawing in folder with no cnm.ini:**

```
1. User runs CNM command
2. CNM calls hcnm-proj (line 2428)
3. hcnm-proj finds no cnm.ini
4. hcnm-proj calls hcnm-initialize-project(dwgdir)
5. hcnm-initialize-project calls hcnm-config-getvar("AppFolder")  [LINE 3725]
6. hcnm-config-getvar calls haws-config-getvar(...)  [LINE 3662]
7. haws-config-getvar needs ini-path for Project scope
8. hcnm-config-getvar provides: (hcnm-ini-name (hcnm-proj))  [LINE 3662]
9. hcnm-proj tries to find project folder
10. No cnm.ini exists → back to step 3
11. INFINITE RECURSION
```

### 2. Legacy Behavior (Pre-Bug: 8bbcdac) ✅ CORRECTED ANALYSIS

**How legacy code ACTUALLY avoided the loop:**

**CRITICAL DISCOVERY** (2025-11-18): AI's initial analysis was WRONG. Legacy `hcnm-config-read-all-project` DID call `hcnm-proj` in its first expression, but still avoided recursion. Here's the actual mechanism:

**Legacy `c:hcnm-config-getvar`** (commit 8bbcdac):
```autolisp
(defun c:hcnm-config-getvar (var / ...)
  (cond
    ;; ✅ CHECK CACHE FIRST before calling any loaders!
    ((not (assoc var *hcnm-config*))
     (cond
       ((hcnm-config-scope-eq var "Project")
        (setq *hcnm-config*
          (append *hcnm-config*
            (hcnm-config-read-all-project)  ; ← Calls hcnm-proj
          )
        )
       )
       ;; ✅ Session-scope does NOT call hcnm-proj!
       ((hcnm-config-scope-eq var "Session")
        (setq *hcnm-config*
          (append *hcnm-config*
            (hcnm-config-read-all-session)  ; ← Reads from *hcnm-config-session*
          )
        )
       )
     )
    )
  )
  ;; Return cached value
  (cadr (assoc var *hcnm-config*))
)

(defun hcnm-config-read-all-session (/ ini_configs)
  (setq ini_configs *hcnm-config-session*)  ; ← NO hcnm-proj call!
  (mapcar
    '(lambda (entry / var val)
       (setq
         var (hcnm-config-entry-var entry)
         val (hcnm-config-entry-val (assoc var ini_configs))
       )
       (list var val)
     )
    (hcnm-config-defaults-single-scope "Session")
  )
)
```

**Why legacy worked (execution sequence):**

1. **edclib.lsp loads** → Sets AppFolder in `*hcnm-config-session*` (Session scope)
2. **User runs CNM command** → Calls `hcnm-proj` for first time
3. **hcnm-proj** checks `*hcnm-cnmprojectroot*` cache → nil, proceeds to initialize
4. **hcnm-initialize-project** calls `(c:hcnm-config-getvar "AppFolder")`
5. **Legacy c:hcnm-config-getvar**:
   - Checks cache: `(not (assoc "AppFolder" *hcnm-config*))` → TRUE (not cached yet)
   - Checks scope: `(hcnm-config-scope-eq "AppFolder" "Session")` → TRUE
   - Calls `hcnm-config-read-all-session`
   - Session loader reads from `*hcnm-config-session*` **WITHOUT calling hcnm-proj**
   - Returns AppFolder value
6. **NO RECURSION** because Session-scope loader doesn't need project path!

**Key insight:** Legacy checked **scope BEFORE deciding whether to call hcnm-proj**. Session-scope variables were loaded from `*hcnm-config-session*` without any project path dependency.

### 3. Current Buggy Behavior (dac2c4c)

**HAWS-CONFIG migration broke this:**

```autolisp
;; Current code (dac2c4c):
(defun hcnm-config-getvar (var / ...)
  (setq val
    (haws-config-getvar
      "CNM"
      var
      (hcnm-ini-name (hcnm-proj))  ; ❌ ALWAYS calls hcnm-proj, even for Session vars!
      "CNM"
    )
  )
)
```

**Problem:** Current code **evaluates `(hcnm-proj)` BEFORE checking scope or cache**. This breaks the legacy mechanism:

- Legacy: Check cache → Check scope → Call appropriate loader (Session loader doesn't need hcnm-proj)
- Current: **Blindly evaluate `(hcnm-proj)` in argument list** → Recursion before haws-config can check anything

**Why this causes recursion:**

1. `hcnm-initialize-project` calls `(hcnm-config-getvar "AppFolder")`
2. Current getvar **immediately evaluates** `(hcnm-ini-name (hcnm-proj))` as argument
3. This happens **BEFORE** haws-config can check if AppFolder is Session-scope or cached
4. `hcnm-proj` called → circular dependency

**Critical flaw:** Even though haws-config HAS AppFolder cached (from edclib.lsp), we never let it check because we force `hcnm-proj` evaluation in the function call's argument list.

### 4. Why AppFolder Should Be Session-Scope

**Current definition (edclib.lsp line 3406):**
```autolisp
(list "AppFolder" (haws-filename-directory (findfile "cnm.mnl")) 0)
;                                                                  ^ Scope 0 = Session
```

**Good news:** AppFolder is ALREADY defined as Session-scope (0) and IS cached by haws-config!

**Why it still breaks:** The problem isn't the configuration - it's that current `hcnm-config-getvar` calls `hcnm-proj` **BEFORE** haws-config can check scope or cache.

**Legacy solution:** Check scope in the WRAPPER, not deep inside haws-config. If Session-scope, don't provide ini-path argument.

---

## Proposed Solution (MATCHES LEGACY BEHAVIOR)

### Solution: Add Scope Check to hcnm-config-getvar

**Matches legacy architecture:** Check variable scope BEFORE calling `hcnm-proj`, just like legacy did.

```autolisp
(defun hcnm-config-getvar (var / val start scope-code)
  (setq start (haws-profile-start "cnm-config-getvar-wrapper"))
  
  ;; Get scope code for this variable (matches legacy scope check!)
  (setq scope-code (haws-config-get-scope "CNM" var))
  
  ;; Call haws-config with appropriate parameters
  (setq val
    (haws-config-getvar
      "CNM"
      var
      ;; Only call hcnm-proj for Project-scope variables (scope 2)
      ;; Session/Drawing/App/User scopes don't need project path
      (if (= scope-code 2)
        (hcnm-ini-name (hcnm-proj))
        nil
      )
      "CNM"
    )
  )
  
  (haws-profile-end "cnm-config-getvar-wrapper" start)
  val
)
```

**Why this matches legacy:**
- ✅ Checks scope BEFORE calling hcnm-proj (like legacy `hcnm-config-scope-eq` check)
- ✅ Session-scope variables bypass hcnm-proj (like legacy `hcnm-config-read-all-session`)
- ✅ Only Project-scope variables trigger project path discovery
- ✅ Preserves HAWS-CONFIG multi-app architecture
- ✅ No recursion when initializing new projects

**Benefits:**
- **Minimal change**: One function, ~5 lines added
- **Matches legacy behavior**: Scope check before hcnm-proj call
- **Performance**: No unnecessary file system calls for Session/Drawing/App/User scopes
- **Architecturally sound**: CNM wrapper knows its own scope requirements

**Execution flow after fix:**
1. User runs CNM command → `hcnm-proj` called
2. No project exists → `hcnm-initialize-project` called
3. Calls `(hcnm-config-getvar "AppFolder")`
4. **NEW: Check scope** → `(haws-config-get-scope "CNM" "AppFolder")` → 0 (Session)
5. **NEW: Scope ≠ 2** → Pass `nil` for ini-path (no hcnm-proj call!)
6. `haws-config-getvar` returns cached AppFolder from edclib.lsp
7. **NO RECURSION!**

---

## Implementation Plan

### Phase 1: Update hcnm-config-getvar (30 min)
1. Add scope check logic
2. Conditionally call hcnm-proj only for Project scope
3. Test with new project folder (no cnm.ini)
4. Verify no recursion

### Phase 2: Testing (15 min)
1. Create test folder without cnm.ini
2. Open drawing in that folder
3. Run CNM command (should initialize project)
4. Verify cnm.ini created with correct settings
5. Verify no infinite recursion errors

### Phase 3: Documentation (15 min)
1. Add comment explaining scope check
2. Document why this matches legacy behavior
3. Update HAWS-CONFIG usage guide if needed

---

## Questions for Review

[TGH 2025-11-18 01:42:25: It seems that there was an intention to forestall this by setting AppFolder early in edclib.lsp line 3406. Why isn't that working?]

**ANSWER**: AppFolder IS being set correctly in edclib.lsp and IS cached by haws-config! The problem is that current `hcnm-config-getvar` calls `(hcnm-proj)` as an argument BEFORE haws-config can check if the value is already cached or if it's Session-scope. The fix respects the early setting by checking scope first.

[TGH 2025-11-18 01:42:25: This maybe can work. But it isn't the legacy solution. Is it wise to retool? Are you sure?]

**ANSWER**: After deeper investigation, this IS the legacy solution! Legacy code checked `(hcnm-config-scope-eq var "Session")` BEFORE calling any loader that would call hcnm-proj. The proposed fix does exactly the same thing - check scope before calling hcnm-proj. This isn't "retooling", it's restoring the legacy guard that was lost during HAWS-CONFIG refactoring.

---

## Files Changed

- `devsource/cnm.lsp` (line ~3653): `hcnm-config-getvar` function
  - Add scope check before calling hcnm-proj
  - Conditionally provide ini-path only for Project scope

---

## Status

**Phase 1**: ✅ Analysis complete - Root cause found  
**Phase 2**: ✅ Fix implemented - Scope check added to both wrappers  
**Phase 3**: ✅ Documentation added to cnm.lsp

**Completion Date:** 2025-11-18

**Files Modified:**
- `devsource/cnm.lsp` (lines 3370-3410): Added comprehensive migration status documentation
- `devsource/cnm.lsp` (lines 3675-3695): Added detailed circular dependency prevention comments
- `devsource/cnm.lsp` (line 3678): Fixed `hcnm-config-setvar` - added scope check
- `devsource/cnm.lsp` (line 3713): Fixed `hcnm-config-getvar` - added scope check

**Fix Summary:**
Both wrappers now check scope BEFORE calling `hcnm-proj`, matching legacy behavior (commit 8bbcdac). Session/Drawing/App/User scopes (0,1,3,4) pass `nil` for ini-path. Only Project scope (2) triggers `hcnm-proj` call.

**Documentation Added:**
- Legacy architecture explanation (commit 8bbcdac)
- HAWS-CONFIG migration intent vs reality
- Current hybrid state with TODO list for completion
- Circular dependency prevention explanation
- Future cleanup roadmap

**Ready for:** Testing with new project folder (no cnm.ini)

```autolisp
;; In edclib.lsp after registering HAWS app:
(if haws-config-register-app
  (progn
    (haws-config-register-app "HAWS" (haws-config-definitions))
    ;; NEW: Pre-cache AppFolder (Session scope)
    (haws-config-setvar "HAWS" "AppFolder" 
      (haws-filename-directory (findfile "cnm.mnl"))
      nil nil)
  )
)
```

**Pros:**
- AppFolder available immediately
- No hcnm-proj calls needed for AppFolder

**Cons:**
- Doesn't solve general problem (other Session vars could have same issue)
- Still have unnecessary hcnm-proj calls for non-Project vars

---

## Recommendation

**Implement Option 1** (scope check in hcnm-config-getvar):

1. **Correct by design:** Only call `hcnm-proj` when actually needed (Project scope)
2. **Performance:** Avoid file system calls for Session/User scopes
3. **Minimal impact:** One function change, well-isolated
4. **Future-proof:** Prevents similar issues with other Session/User variables

---

## Implementation Plan

### Phase 1: Fix Immediate Bug
1. Modify `hcnm-config-getvar` to check scope before calling `hcnm-proj`
2. Test with new project folder (no cnm.ini)
3. Verify AppFolder retrieval works
4. Verify project initialization completes

### Phase 2: Full Project Management Migration (Future)
As noted in issue description: "The ultimate fix will be to implement project management properly in haws-config."

This means:
- Move project folder determination into HAWS-CONFIG
- Standardize project discovery across all HawsEDC apps
- Eliminate CNM-specific `hcnm-proj` in favor of generic HAWS-CONFIG mechanism

**Not part of this fix:** Phase 2 is larger architectural work for future.

---

## Testing Strategy

### Test Case 1: New Project Folder
1. Create new folder with test drawing
2. Ensure no cnm.ini or cnmproj.txt exists
3. Run CNM command
4. Verify: No infinite recursion
5. Verify: cnm.ini created successfully
6. Verify: AppFolder correctly set to CNM application folder

### Test Case 2: Existing Project
1. Use folder with existing cnm.ini
2. Run CNM command
3. Verify: No regression (project found correctly)
4. Verify: AppFolder still accessible

### Test Case 3: Session Scope Variables
1. Get Session-scope variable (AppFolder)
2. Verify: No hcnm-proj call made (check debug output)
3. Verify: Value retrieved from cache/default

### Test Case 4: User Scope Variables
1. Get User-scope variable (CNMAliasActivation)
2. Verify: No hcnm-proj call made
3. Verify: Value retrieved from Registry

---

## Files to Modify

1. **devsource/cnm.lsp** (line 3653)
   - Function: `hcnm-config-getvar`
   - Change: Add scope check before calling `hcnm-proj`

---

## Verification

After fix, this sequence should work:

```
1. User runs CNM in new folder (no cnm.ini)
2. hcnm-proj finds no cnm.ini
3. hcnm-proj calls hcnm-initialize-project(dwgdir)
4. hcnm-initialize-project calls hcnm-config-getvar("AppFolder")
5. hcnm-config-getvar checks scope → Session (0)
6. hcnm-config-getvar calls haws-config-getvar with ini-path=nil
7. haws-config-getvar reads from Session scope (no INI needed)
8. AppFolder value returned (from cache or default)
9. hcnm-initialize-project copies template cnm.ini
10. SUCCESS - no recursion
```

---

## Questions for Review

1. **Is Option 1 the correct approach?** Or should we pursue Option 2/3?
2. **Should we add debug output** to help diagnose future similar issues?
3. **Should we add guard anyway** (Option 2) as defensive programming?
4. **When to schedule Phase 2** (full HAWS-CONFIG project management)?

---

**Status:** Ready for human review and decision on implementation approach.
