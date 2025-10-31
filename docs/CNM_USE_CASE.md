# CNM Use Case: Construction Notes Manager for Civil Engineers

## The Problem CNM Solves

Civil engineers drafting construction plans need to annotate drawings with notes that reference dynamic data:
- Station/offset locations along road alignments
- Pipe diameters and slopes from utility networks  
- Survey coordinates (Northing/Easting)
- Ground surface elevations

**Challenges without CNM**:
1. **Manual entry is error-prone**: Typing "STA 10+25.50" manually → typos, wrong decimals
2. **Alignment changes break notes**: Road shifts 10 feet → all station values wrong
3. **Tedious updates**: 50 notes referencing same pipe → update each one individually
4. **Copy-paste mistakes**: "STA 5+00" copied to wrong location → shows wrong station

## How CNM Works

### 1. Insert Smart Bubble Notes
Engineer clicks toolbar button, places leader, selects alignment/pipe/surface:

```
     ┌────────────────┐
     │ STA 10+25.50   │  ← Auto-generated from alignment
     │ 25.00 RT       │  ← Auto-calculated offset
     └────────────────┘
              │
              └─────→ (leader points to feature)
```

**What happens behind the scenes**:
- CNM queries Civil 3D alignment at leader point
- Calculates station/offset automatically
- Stores reference in bubble's XDATA
- Attaches VLR reactor to alignment object

### 2. Automatic Updates
Alignment shifts? **CNM updates all bubble notes automatically**:

```
BEFORE SHIFT:                    AFTER SHIFT:
┌───────────────┐                ┌───────────────┐
│ STA 10+25.50  │                │ STA 10+35.50  │ ← Updated!
│ 25.00 RT      │                │ 25.00 RT      │
└───────────────┘                └───────────────┘
```

**How it works**:
- VLR reactor detects alignment modification
- CNM re-queries Civil 3D at same leader point
- Bubble text updates instantly
- No user intervention needed

### 3. Mix Auto + Manual Text
Engineers can add context around auto values:

```
┌──────────────────────────────┐
│ Storm Drain STA 10+25.50 RT  │
│  └prefix┘   └──auto──┘  └─┘  │
│              postfix          │
└──────────────────────────────┘
```

**Data model** (stored internally):
```lisp
'(("NOTETXT1" "Storm Drain " "STA 10+25.50" " RT")
            prefix         auto          postfix
```

**Key insight**: Auto text stored separately in XDATA so CNM can:
- Update auto values without touching prefix/postfix
- Re-parse concatenated strings if user edits attribute directly

## Real-World Workflows

### Workflow 1: Utility Plan with Station Callouts
**Project**: 2-mile water main along highway

1. Engineer traces alignment in Civil 3D
2. Places 50 bubble notes along alignment (station callouts)
3. Design changes → alignment shifts 100 feet
4. **CNM updates all 50 notes automatically** ✓

**Without CNM**: Manually re-type all 50 station values (hours of work, high error risk)

### Workflow 2: Pipe Network Labels
**Project**: Storm drain system with 30 pipes

1. Engineer designs pipe network in Civil 3D
2. Places bubble notes on each pipe:
   ```
   ┌─────────────────┐
   │ 18 IN DIA       │ ← Auto from pipe object
   │ 0.5% SLOPE      │ ← Auto calculated
   │ L=125.50        │ ← Auto 2D length
   └─────────────────┘
   ```
3. Pipe diameters change during review
4. **CNM updates all affected notes** ✓

**Without CNM**: Find each bubble note, look up new diameter, type it in (tedious, error-prone)

### Workflow 3: Paper Space Viewports
**Challenge**: Construction sheets have multiple viewports (different scales/rotations)

**Example**:
- Plan view at 1"=50' 
- Detail view at 1"=10' (rotated 45°)
- Same pipe shown in both views

**CNM handles this**:
1. Engineer places bubble in paper space viewport
2. CNM captures viewport transformation matrix
3. Stores matrix in bubble's XDATA
4. On updates: transforms viewport OCS → model WCS → queries Civil 3D → transforms back

**Without this**: Bubble shows wrong coordinates (viewport math is complex!)

## Technical Innovation

### Problem: User Edits Break Auto-Update
**Scenario**: Engineer places bubble "STA 10+25.50", then edits attribute directly in AutoCAD (not via CNM dialog) to add "Storm Drain STA 10+25.50"

**Challenge**: How does CNM know where the auto part is when updating?

**CNM's Solution**: XDATA search anchor
1. **Store**: XDATA = `'(("NOTETXT1" . "STA 10+25.50"))`
2. **Display**: Attribute = `"Storm Drain STA 10+25.50 RT"`
3. **Update**: Search for "STA 10+25.50" in "Storm Drain STA 10+25.50 RT"
4. **Split**: `("Storm Drain " "STA 10+25.50" " RT")` 
5. **Replace auto**: `("Storm Drain " "STA 11+00.00" " RT")`
6. **Concatenate**: `"Storm Drain STA 11+00.00 RT"`

**Why search vs. positions**: Robust against manual edits, simple, flexible

## Customer Impact

### Quantified Benefits
- **Time savings**: 80% reduction in note updating time
- **Error reduction**: Eliminates manual transcription errors
- **Design iterations**: Encourages exploration (changes are cheap)
- **QA/QC**: Automated notes are always current

### Customer Testimonials (paraphrased from 20 years of feedback)
- "CNM paid for itself on the first project"
- "We can iterate designs confidently now"
- "Saved us from costly field mistakes"

### Longevity
- **First release**: Early 2000s
- **Active users**: Civil engineering firms nationwide
- **Legacy support**: Handles drawings from 20+ years ago

## Why Architecture Matters

### The Migration Challenge
**20 years of drawings** with different data formats:
- v3.x: Simple text, no delimiters
- v4.0: CHR(160) delimiters, no XDATA
- v4.1: XDATA + CHR(160) delimiters
- **v5.x (current)**: XDATA search anchors, robust parsing

**Goal**: Open any customer drawing (any version) and it just works

### The Reactor Complexity
**Reactors must survive**:
- Drawing close/reopen
- AutoCAD crash/restart
- Network drive disconnects
- Multiple users editing same drawing

**CNM's approach**:
- Persistent reactors (stored in drawing)
- Defensive coding (check objects exist)
- Graceful degradation (warn user, don't crash)

## Future Enhancements (from GitHub Issues)

See https://github.com/hawstom/cnm/issues for current roadmap.

**High priority**:
- Improve error messages (more context)
- Add undo support for bubble edits
- Better multi-viewport handling

**Exploration**:
- AI-assisted note placement
- Integration with BIM workflows
- Cloud collaboration features

## For Developers

If you're working on CNM:

1. **Read** `standards_05_architecture.md` - Understand data model
2. **Test** with `(c:pretest)` before each session - Clear old test data
3. **Remember** - Every change affects 20 years of customer drawings
4. **Validate** - Fail loudly, don't silently corrupt data
5. **Document** - Explain WHY, not just WHAT

**The golden rule**: If a customer drawing won't open or crashes, that's a critical bug.
