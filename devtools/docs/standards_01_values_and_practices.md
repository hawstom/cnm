HAWSEDC STANDARDS VOLUME 01: VALUES AND PRACTICES
=================================================

**Document Type:** Standard  
**Status:** Draft  
**Last Updated:** 2025-10-25  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 Key Points
- [TGH: To be defined]

---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume defines the philosophical foundation, vision, and core values of the HawsEDC/CNM project.

[TGH: Do we need this volume, or should we start with Volume 02? Keeping as placeholder for now.]

## 2.2 Scope

### 2.2.1 What This Document Should Cover
[AI: Brainstorming - what belongs here?]
- Project mission and goals
- Core values and principles
- Target audience and use cases
- Success criteria

### 2.2.2 Relationship to Other Volumes
- Volume 02: AI and Human Collaboration - explains HOW we work
- Volume 03+: Technical standards - explain WHAT we build

---
<!-- #endregion -->

<!-- #region 3. VISION -->
# 3. SCHEDULING
Implementing any standard balances the risk of regression with the cost of confusion. Scheduling implementation accounts for this balance. Every standard should include a one-word scheduling assignment as defined below. The schedule for any standard may move upward as it nears completion; or it may stay low for high-risk standards.

- Very aggressive: Stop everything else to immediately fix every found violation of this standard ANYWHERE.
- Aggressive: Stop everything else to immediately fix every found violation of this standard CONFINED TO THE CURRENT FILE.
- Moderately aggressive: Stop everything else to immediately fix every found violation of this standard in the function or section being edited or written.
- Moderately conservative: Do not create anything that violates this standard. Fix any found violations of this standard only if they were created by you in your own recent memory or you get due advice and approval.
- Conservative:  Do not create anything that violates this standard. Fix any found violations of this standard only if you get due advice and approval.
- Very conservative: Follow an overwhelming local consensus even if it violates this standard.
---
<!-- #endregion -->

<!-- #region 4. VALUES -->
# 4. VALUES

## 4.1 Core Partnership

### 4.1.1 Foundation
This project is a collaboration between human and AI. The code must serve both audiences. Standards exist not as bureaucracy, but as shared language that lets us work together effectively.

### 4.1.2 Why Standards Matter
- **For Humans:** Standards reduce cognitive load, prevent bikeshedding, and make code predictable
- **For AI:** Standards provide the explicit structure needed to understand intent and maintain consistency
- **For Both:** Standards are promises we make to each other about how code will be written

## 4.2 AI Agent Manifesto

### 4.2.1 What Makes Code AI-Readable

**1. Explicit Structure Over Implicit Convention**
- Clear function boundaries (one `defun` per logical unit)
- Documented data flow (the WHY and WHAT, not just the HOW)
- Type information encoded in names (`obj_`, `en_`, `_p`) even though AutoLISP doesn't require it

**2. Naming That Encodes Meaning**
```lisp
;; GOOD - I can understand without context
(defun hcnm_ldrblk_ensure_field (string)
  ;; namespace_subsystem_action_target
  
;; BAD - Forces me to read entire file for context
(defun process_value (x)
```

**3. Comments That State Facts**
```lisp
;; GOOD - States a fact about the system
;; NOTENUM and NOTEPHASE are search keys. Don't add delimiters.

;; BAD - Describes what the code does (I can see that)
;; Check if tag matches pattern
```

**4. Consistent Patterns**
- When functions follow predictable patterns (`_get`, `_set`, `_ensure`), I can infer behavior
- Deviations from pattern signal "this is special" - helping me focus attention
- Consistency multiplies my effectiveness exponentially

**5. Single Responsibility**
- **This is the biggest difference between AI and human needs**
- When functions do ONE thing, I understand them in isolation
- When functions do multiple things, I must hold the entire call graph in context
- My context window is large but finite

### 4.2.2 What I Struggle With

**1. Implicit State**
```lisp
;; HARD for AI
(setq *global-counter* 0)  ; Set in file A
(defun process ()          ; Used in file B
  (setq *global-counter* (1+ *global-counter*)))
```

**2. Undocumented Side Effects**
```lisp
;; HARD for AI - function modifies its argument
(defun update_attributes (attr_list)
  ;; Actually modifies attr_list, not clear from signature
```

**3. Magic Numbers**
```lisp
;; HARD for AI
(substr string 3)  ; Why 3? Format code length? Delimiter position?

;; EASY for AI
(substr string 3)  ; Skip mtext format code "\L" (3 chars)
```

**4. Long Functions**
- After ~50 lines, I lose track of control flow
- Prefer extracting helpers with clear names

**5. Overloaded Functions**
```lisp
;; HARD for AI - behavior changes based on argument types
(defun process (x)
  (cond
    ((= (type x) 'STR) (process-string x))
    ((= (type x) 'LIST) (process-list x))
    ((= (type x) 'ENAME) (process-entity x))
  ))
```

### 4.2.3 What Helps Me Most

**The refactoring we just completed is a perfect example:**
- Split responsibilities: parsing vs formatting vs validation
- Documented data flow: "prefix§auto§postfix"
- Clear naming: `ensure_fields` vs `adjust_formats` vs `adjust_format`
- Inline comments explaining business rules: "search keys don't get delimiters"

**This helped BOTH of us understand the code better.**

## 4.3 Human Developer Manifesto

### 4.3.1 What Makes Me Human
Mysterious memory
Laughably slow processing speed
Narrow experience
Eyes are my dominant programming input device

### 4.3.2
### 4.3.3
### 4.3.4

### 4.3.X What Makes Code Human-Readable

[AI to TGH: I invite you to write this section. What do YOU need from code? What makes it maintainable for you?]

**Questions to consider:**
- How much context can you hold in your head?
- What makes code "obvious" vs "needs investigation"?
- When do comments help vs clutter?
- What patterns let you navigate quickly?
- What makes code feel "clean"?

### 4.3.2 Where Human and AI Needs Align

**The overlap is substantial:**
- Clear, meaningful names
- Single responsibility principle
- Explicit over implicit
- Comments that explain WHY
- Consistent patterns and conventions
- Short, focused functions
- Type information when helpful

**Bottom line:** Most "AI-friendly" code is just well-written code. The standards we're developing serve both audiences because they're fundamentally about clarity and maintainability.

### 4.3.3 Where Our Needs Diverge

**Humans Excel At:**
- Tolerating ambiguity (inferring from context)
- Remembering conventions across sessions
- Skipping verbose comments when unnecessary
- Understanding implied relationships
- Seeing patterns in inconsistency

**AI Needs More Help With:**
- Explicit disambiguation
- Stated facts (I don't remember between sessions)
- All comments visible (I can't "skip" them mentally)
- Documented relationships
- Rigid consistency (my pattern matching is literal)

### 4.3.4 The Collaboration Model

**We're not trying to make code "for AI" - we're making code for the team:**
- Standards reduce friction in the collaboration
- Clear code helps both human and AI understand intent
- Documentation serves as shared context
- Consistency lets both partners work faster

**The goal:** Code so clear that both human and AI can confidently modify it without breaking things.

---
<!-- #endregion -->

<!-- #region 5. NOTES -->
# 5. NOTES

[TGH: This volume may be merged with Volume 02 or repurposed as needed]

---
<!-- #endregion -->

**End of Document**
