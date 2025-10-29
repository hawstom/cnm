# Q2: AutoLISP Community Naming Patterns Research

**Question:** Noun-verb or verb-noun for Category 2 functions?

**Date:** October 28, 2025

---

## Community Survey Results

### Autodesk Native Functions

**Pattern:** Mixed, but **verb-noun dominates**

Examples:
- `getvar` (verb-noun) - get variable
- `setvar` (verb-noun) - set variable
- `getfiled` (verb-noun) - get filed name
- `getpoint` (verb-noun) - get point
- `getint` (verb-noun) - get integer
- `entget` (verb-noun) - entity get
- `entmake` (verb-noun) - entity make
- `entmod` (verb-noun) - entity modify
- **BUT:** `vla-get-property` / `vla-put-property` (subject-verb-object)

**Observation:** Autodesk uses verb-noun for simple functions, but VLA-OBJECT functions use `object-verb-property` pattern.

### Lee Mac Code Analysis

**Pattern:** Strongly **verb-noun** with namespace prefix

Examples from Lee Mac library:
- `LM:lst->str` - list to string
- `LM:str->lst` - string to list  
- `LM:startundo` - start undo
- `LM:endundo` - end undo
- `LM:getfiles` - get files
- `LM:removenth` - remove nth
- `LM:uniquelist` - unique list (adjective-noun!)

**Observation:** Lee Mac consistently uses verb-noun, occasionally adjective-noun for descriptive functions.

### Common Lisp Conventions

**Pattern:** **verb-noun** heavily favored

Standard functions:
- `make-string` - verb-noun
- `make-list` - verb-noun
- `remove-if` - verb-preposition
- `delete-duplicates` - verb-noun
- **BUT:** Accessors use noun-verb: `list-length`, `string-length`

**Observation:** Common Lisp uses verb-noun for actions, noun-verb for accessors/properties.

---

## Analysis: Our Use Case

### Category 1: Data Flow (source-to-destination)
```lisp
hcnm-lattribs-to-dwg    ; noun-to-noun (transformation)
hcnm-dwg-to-lattribs    ; noun-to-noun (transformation)
```

**Verdict:** Already decided - `source-to-destination` is neither verb-noun nor noun-verb!

### Category 2: Lattribs Operations

**Two sub-patterns emerged:**

#### 2a. Transformations/Conversions
```lisp
hcnm-ldrblk-auto-al-station-to-string   ; noun-to-noun
hcnm-ldrblk-auto-pipe-dia-to-string     ; noun-to-noun
```

**Verdict:** Use `source-to-destination` like Category 1

#### 2b. Operations on lattribs structure

**These are the ones in question:**

**Option A: noun-verb (object-oriented)**
```lisp
hcnm-lattribs-initialize   ; lattribs.initialize()
hcnm-lattribs-put         ; lattribs.put(value)
hcnm-lattribs-get         ; lattribs.get(tag)
```

**Option B: verb-noun (procedural)**
```lisp
hcnm-initialize-lattribs   ; initialize(lattribs)
hcnm-put-lattribs         ; put(lattribs) ← AMBIGUOUS!
hcnm-get-lattribs         ; get(lattribs) ← AMBIGUOUS!
```

---

## Recommendation: NOUN-VERB for Building Blocks

### Rationale

1. **Consistency with Category 1:**
   - Category 1 already uses `lattribs-to-dwg` (noun-first)
   - Category 2 should also put `lattribs` first: `lattribs-verb`

2. **Disambiguation:**
   - `lattribs-put` = put something INTO lattribs
   - `put-lattribs` = put lattribs WHERE? (unclear)
   - `lattribs-get` = get something FROM lattribs
   - `get-lattribs` = get lattribs from WHERE? (unclear)

3. **Grouping in listings:**
   - All lattribs functions sort together alphabetically
   - Easy to find all operations on that structure

4. **Object-oriented feel:**
   - Matches VLA functions: `vla-get-property`, `vla-put-property`
   - Feels like methods on an object: `lattribs.initialize()`

5. **"Notorious building block" exception:**
   - Lattribs is ubiquitous enough to deserve special treatment
   - Not a general-purpose utility, it's THE core structure
   - Worth creating a mini "class" of functions around it

### When to Use Each Pattern

| Pattern | Use For | Examples |
|---------|---------|----------|
| `source-to-destination` | Data transformations | `lattribs-to-dwg`, `real-to-string` |
| `noun-verb` | Operations on notorious building blocks | `lattribs-put`, `lattribs-get` |
| `verb-noun` | General utilities | `calculate-slope`, `validate-input` |

### Exception Pattern for Notorious Building Blocks

**Definition:** A "notorious building block" is a data structure that:
- Appears in many function names (10+ functions)
- Has specialized operations specific to it
- Is central to the architecture
- Benefits from grouping its operations together

**In CNM:**
- `lattribs` = YES (notorious building block) → use `lattribs-verb`
- `bubble-data` = MAYBE (limited scope) → use `verb-bubble-data`
- Generic structures = NO → use `verb-noun`

---

## Final Recommendation

**For Category 2b (operations on lattribs):**

Use **noun-verb** pattern:

```lisp
hcnm-ldrblk-lattribs-initialize   ; Creates empty structure
hcnm-ldrblk-lattribs-put          ; Puts element into structure  
hcnm-ldrblk-lattribs-put-auto     ; Puts auto-text specifically
hcnm-ldrblk-lattribs-get          ; Gets element from structure
hcnm-ldrblk-lattribs-ensure       ; Validates structure
```

**Justification:**
1. Community: VLA-OBJECT functions use similar pattern
2. Clarity: Unambiguous what's being operated on
3. Consistency: Matches `lattribs-to-dwg` noun-first pattern
4. Grouping: All lattribs functions sort together
5. Architecture: Reveals lattribs as a foundational structure

**Note:** This is an exception for notorious building blocks. General utilities still use verb-noun.

---

## Decision

**TGH approved noun-verb pattern in comments.**

Proceeding with:
- Category 1: `source-to-destination` (noun-to-noun)
- Category 2a: `source-to-destination` (noun-to-noun) 
- Category 2b: `noun-verb` for lattribs operations
- Other: `verb-noun` for general utilities

✓ RESOLVED
