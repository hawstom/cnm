HAWSEDC STANDARDS VOLUME 02: AI AND HUMAN COLLABORATION
========================================================

**Document Type:** Standard  
**Status:** Active  
**Last Updated:** 2025-11-29  
**Owner:** Tom Haws (TGH)

---

<!-- #region 1. QUICK START -->
# 1. QUICK START

## 1.1 For AI Agents Reading These Standards

### 1.1.1 Critical Instructions
- **ALWAYS** read relevant standards volumes before making code changes (see S02.5.1.1 for guidance on which volumes)
- **ALWAYS** audit document nested sequential outline numbering when editing (verify `# 1` → `## 1.1` → `### 1.1.1` → `#### 1.1.1.1`)
- **ALWAYS** update "Last Updated" date when editing
- **NEVER** invent new patterns - follow established conventions

### 1.1.2 Document Structure
- Sections use decimal numbering: `1.1.2.3`
- Comments use format: `[TGH: comment text]`
- Only remove comments marked with your initials. Request the commenter to review and remove their comment if you believe it is addressed.
- All headings are numbered (no unnumbered headings except title)
- Do not add fake headings. Keep this structure semantically clean instead of full of direct formatting for looks.

### 1.1.3 Key Concepts
- **Embeddings:** How AI represents and retrieves document chunks
- **Chunking:** Breaking documents into digestible pieces for AI context
- **Hierarchical numbering:** Helps AI navigate and reference specific sections
- **Explicit references:** "See section 2.3.1" is better than "see above"

## 1.2 For Human Developers

### 1.2.1 Key Benefits
- **Persistent knowledge:** Decisions documented, not lost in chat history
- **Clear references:** "Read section 3.2.1" instead of re-explaining
- **AI efficiency:** Structured docs improve AI comprehension and accuracy
- **Human efficiency:** Find answers faster than searching chat logs

### 1.2.2 Navigation
- **VS Code Outline View:** Shows document structure in left sidebar
- **Breadcrumbs:** Current section shown at top of editor
- **Go to Symbol:** Press `Ctrl+Shift+O` to jump to any heading
- **Search:** Section numbers make grep-friendly references

---
<!-- #endregion -->

<!-- #region 2. INTRODUCTION -->
# 2. INTRODUCTION

## 2.1 Purpose
This volume defines how AI agents and human developers collaborate effectively on the HawsEDC/CNM codebase.

## 2.2 Scope

### 2.2.1 What This Document Covers
- How AI agents should read and interpret these standards
- How humans should write documentation for AI consumption
- Communication patterns between AI and humans
- Decision persistence and knowledge management
- Comment and annotation standards

### 2.2.2 What This Document Does NOT Cover
- Code style standards (see Volume 03: Names and Symbols)
- Architecture patterns (see Volume 05: Architecture)
- Specific implementation details (see individual module docs)

## 2.3 Terminology

### 2.3.1 AI Agent
Any AI assistant (GitHub Copilot, Claude, Grok, etc.) that reads code and documentation to provide development assistance.

### 2.3.2 Human Developer
Any person writing or maintaining HawsEDC/CNM code. May be Tom Haws (TGH) or future contributors.

### 2.3.3 Embeddings
Vector representations of text that AI uses to understand semantic meaning and find relevant information.

**How AI processes these docs:** AI agents (like GitHub Copilot) automatically create embeddings when reading your workspace. The hierarchical structure (numbered sections, clear headings, focused topics) helps AI tools chunk and retrieve information accurately. You don't need to subscribe to additional embedding services - the structure we're creating serves both human navigation AND AI comprehension.

### 2.3.4 Chunking
Breaking large documents into smaller segments for AI context windows and embedding generation.

### 2.3.5 Context Window
The amount of text an AI agent can "see" at once when processing requests.

## 2.4 Relationship to Other Volumes

### 2.4.1 Volume 01: Vision and Values
Defines the philosophical foundation and goals of HawsEDC/CNM.

### 2.4.2 Volume 03: Names and Symbols
Defines naming conventions for code (functions, variables, files).

### 2.4.3 Volume 04: Organization and Locations
Defines file structure, folder organization, and where to find things.

### 2.4.4 Volume 05: Architecture
Defines major architectural patterns (reactors, config system, delimiters, etc.).

### 2.4.5 Volume 06: Roadmap
Defines planned features, refactoring goals, and version milestones.

---
<!-- #endregion -->

<!-- #region 3. DOCUMENT STANDARDS -->
# 3. DOCUMENT STANDARDS

## 3.1 Document Structure

### 3.1.1 Required Elements
Every standards document must include:
1. Title with `====` underline (no heading marker)
2. Metadata block (Document Type, Status, Last Updated, Owner)
3. Horizontal rule `---` before section 1
4. Section 1: QUICK START
5. Section 2: INTRODUCTION
6. All subsequent sections numbered sequentially

### 3.1.2 Title Format
```markdown
HAWSEDC STANDARDS VOLUME NN: DOCUMENT TITLE
============================================
```

### 3.1.3 Metadata Block Format
```markdown
**Document Type:** [Standard | Guide | Spec | RFC | Reference]  
**Status:** [Draft | Active | Deprecated]  
**Last Updated:** YYYY-MM-DD  
**Owner:** Name (Initials)
```

### 3.1.4 Section Numbering
- Use decimal notation: `1`, `1.1`, `1.1.1`, `1.1.1.1`
- Markdown levels: `# 1` → `## 1.1` → `### 1.1.1` → `#### 1.1.1.1`
- No limit on depth. Prefer 3-4 levels maximum for readability, but do not feel constrained if it would drive you to make ad hoc pseudo headings. If you can't handle the need with a bullet list or a table, use deeper headings.
- NEVER use unnumbered headings except document title

### 3.1.5 Section Separators
Use horizontal rules `---` between major sections (level 1 headings).

### 3.1.6 Folding Regions (Optional)
Wrap sections in VS Code folding markers:
```markdown
<!-- #region 3. SECTION NAME -->
# 3. SECTION NAME
...content...
<!-- #endregion -->
```

### 3.1.7 Blank Lines
**FIRM:** No policy for documentation (use judgment)

**Note:** Code has a strict no-blank-lines policy (see S03.11.2), but documentation is flexible.

## 3.2 Naming Conventions

### 3.2.1 File Names
- Format: `STANDARDS_NN_TITLE_IN_CAPS.md`
- Example: `STANDARDS_02_AI_HUMAN_COLLABORATION.md`
- Use Arabic numerals with leading zero: `01`, `02`, ..., `10`, `11`
- Use underscores as delimiters
- All lowercase for actual filenames: `standards_02_ai_human_collaboration.md`

### 3.2.2 Volume Numbering
- Start at 01 (not 00)
- Use leading zeros for proper sorting: `01`, `02`, ..., `10`
- Reserve earlier numbers for foundational content

### 3.2.3 Section Headings
- ALL CAPS for section titles in headings
- Brief and descriptive
- No articles (a, an, the) unless necessary
- Action-oriented when appropriate

---
<!-- #endregion -->

<!-- #region 4. COMMENT AND ANNOTATION STANDARDS -->
# 4. COMMENT AND ANNOTATION STANDARDS

## 4.1 Comment Format

### 4.1.1 Standard Format
**FIRM:** Use [INITIALS: text] format for all documentation comments

**Schedule:** Moderately aggressive (already adopted, enforce going forward)

```markdown
[INITIALS: comment text]
```

### 4.1.2 Examples
```markdown
[AI: This section needs expansion - add more examples of comment types]
[AI: Should we include code examples here?]
[AI: Added explanation of embeddings per request]
```

### 4.1.3 Initials Key
- `TGH` - Tom Haws (project owner)
- `AI` - AI agent (any AI assistant)
- Future contributors: Add your initials here

## 4.2 Comment Lifecycle

### 4.2.1 Adding Comments
Anyone (human or AI) can add comments using their initials.

### 4.2.2 Resolving Comments
- Only the person whose initials are on the comment may remove it
- Mark as resolved with edit: `[TGH: ~~This is resolved~~]` before removing
- AI agents: Never remove comments marked with human initials

### 4.2.3 Requesting Cleanup
Humans can request: "Remove all resolved [TGH: ...] comments"  
AIs can suggest: "I see 3 resolved comments marked for removal"

### 4.2.4 Resolved-passages marker (deletion-by-citation workflow)

When an inline passage is judged obsolete or proposed for deletion, follow this low-risk, human-reviewed workflow:

- Add a Resolved-passages marker block directly above the passage using the following syntax:

```
[delete]
Human approval needed for deletion: This passage is proposed for removal because <short rationale>.
Proposed-by: <name or AI id>
Date: YYYY-MM-DD
[/delete]
```

- If the passage contains long-form knowledge that should be preserved, migrate that content to an appropriate canonical location (`ci` or `devtools/docs/standards`) BEFORE removing the passage. Replace the original passage with a one-line citation to the canonical document and leave the `[delete]` marker for human review.

- If the passage is low-value historical noise (e.g., a now-resolved workaround instruction), the `[delete]` marker allows humans to validate and then remove it safely.

Human reviewer responsibilities:

- Inspect each `[delete]` marker and either (A) remove the marked passage and the marker, or (B) remove the marker if the passage should remain. Record the decision in the relevant standards or the commit message.
- For migrations to `ci` or `devtools/docs/standards`, update the target document's metadata (`Last Updated`) and record the migration rationale.

Rationale: This procedure gives AI agents a standardized, auditable way to propose broad cleanups while ensuring humans retain final control and canonical knowledge is preserved.

## 4.3 Types of Comments

### 4.3.1 Questions
```markdown
[AI: Should embeddings be explained in more detail?]
```

### 4.3.2 TODO Items
```markdown
[AI: TODO - Add code examples for section 5.2]
```

### 4.3.3 Clarifications
Example of human clarifying context:
```markdown
[TGH: This refers to the old config system, not HAWS_CONFIG]
```

### 4.3.4 AI Responses
Example of AI responding to a request:
```markdown
[AI: Added requested explanation of chunking strategies]
```

---
<!-- #endregion -->

<!-- #region 5. AI AGENT INSTRUCTIONS -->
# 5. AI AGENT INSTRUCTIONS

## 5.1 Reading Documentation

### 5.1.1 Before Making Code Changes
1. Read relevant standards volumes completely
2. Understand the architectural context
3. Check for related sections using references
4. Verify understanding with human if uncertain

[AI: "Relevant" means the volumes that apply to the task:
- **Always read:** S02 (this volume), S03 (Names), S04 (Organization)
- **For architecture changes:** S05 (Architecture)
- **For planning/priorities:** S06 (Roadmap)
- **When in doubt:** Ask which volumes to read for the specific task]

### 5.1.2 Context Gathering Strategy
When asked to implement a feature:
1. Read Volume 03 (Names and Symbols) for naming conventions
2. Read Volume 04 (Organization) for file structure
3. Read Volume 05 (Architecture) for patterns
4. Search existing code for similar implementations
5. Propose approach before implementing

### 5.1.3 Handling Ambiguity
When documentation is unclear or contradictory:
1. Add comment: `[AI: Found ambiguity in section X.Y.Z]`
2. Ask human for clarification
3. Do NOT invent new patterns
4. Do NOT proceed with assumptions

## 5.2 Editing Documentation

### 5.2.1 Numbering Audit
ALWAYS verify section numbering before completing edit:
1. Check all sections are sequential: `1`, `2`, `3` (no gaps)
2. Check all subsections are sequential: `1.1`, `1.2`, `1.3`
3. Check hierarchy is correct: `# 1` → `## 1.1` → `### 1.1.1`
4. Verify no unnumbered headings exist (except title)

### 5.2.2 Metadata Updates
When editing a document, update:
- **Last Updated:** Set to current date (YYYY-MM-DD)
- **Status:** Change if applicable (Draft → Active)

### 5.2.3 Adding Content
When adding new sections:
1. Place in logical hierarchy
2. Renumber ALL subsequent sections if needed
3. Update any cross-references to renumbered sections
4. Add to QUICK START if critical information

### 5.2.4 Removing Content
When removing sections:
1. Check for cross-references in other documents
2. Update or remove those references
3. Renumber subsequent sections
4. Add comment explaining removal: `[AI: Removed section X - merged into Y]`

## 5.3 Embeddings and Chunking

### 5.3.1 Why Hierarchical Numbering Helps AI
[AI: Explanation from AI perspective - why numbered sections improve retrieval:]

Hierarchical numbering provides several benefits for AI processing:

1. **Precise References:** "See section 3.2.1" is unambiguous and can be directly looked up
2. **Chunking Boundaries:** Section numbers create natural chunk boundaries for embeddings
3. **Context Preservation:** Numbers maintain hierarchy when chunks are processed separately
4. **Retrieval Accuracy:** More likely to retrieve correct section when searching embeddings
5. **Cross-Document Links:** Easy to reference across volumes: "See Volume 05, section 2.3"

### 5.3.2 Optimal Section Length for AI
Aim for sections that are:
- **Minimum:** 2-3 paragraphs (enough context for standalone comprehension)
- **Maximum:** 1-2 pages (fits within typical embedding chunk sizes)
- **Sweet spot:** 5-10 paragraphs with clear topic focus

If a section exceeds this, consider splitting into subsections.

### 5.3.3 Writing for AI Comprehension
- Use explicit references instead of "above" or "below" (AI may see chunks out of order)
- Define acronyms at first use in each major section
- Include brief context at start of each section
- USE CONSISTENT TERMINOLOGY throughout document. If terminology is not consistent, ask and fix to a standardized version.

---
<!-- #endregion -->

<!-- #region 6. HUMAN DEVELOPER INSTRUCTIONS -->
# 6. HUMAN DEVELOPER INSTRUCTIONS

## 6.1 Writing Documentation

### 6.1.1 Starting a New Volume
1. Copy template from section 3.1
2. Choose appropriate volume number
3. Fill in metadata block
4. Write QUICK START section (brief, actionable)
5. Write INTRODUCTION section (scope, terminology, relationships)
6. Add substantive content in subsequent sections

### 6.1.2 Updating Existing Documentation
1. Add comments `[TGH: ...]` for questions or TODOs
2. Make edits directly for clear improvements
3. Update "Last Updated" date
4. Verify section numbering is intact

### 6.1.3 Requesting AI Assistance
Effective requests:
- ✅ "Add section 4.3 explaining X, following the pattern in section 3.2"
- ✅ "Read S03.2.1 and verify code follows those conventions"
- ✅ "Audit section numbering in this document and fix any issues"

Less effective:
- ❌ "Make this better"
- ❌ "Add some examples"
- ❌ "Fix the formatting" (too vague)

## 6.2 Pointing AI to Specific Content

### 6.2.1 Precise References
Use section numbers for precise references:
```
"Read section 3.2.1 before implementing"
"Follow the pattern described in Volume 05, section 4.1.2"
"See section 2.3 for terminology"
```

**Standard shorthand formats:**
- **Within same volume:** `section 3.2.1` or `S3.2.1`
- **Across volumes:** `Standards 05.4.1.2` or `Std05.4.1.2` or `S05.4.1.2`
- **Full reference:** `Volume 05 (Architecture), section 4.1.2`

[AI: Recommendation - Use `S##.#.#` format for brevity in code comments, full text in documentation]

### 6.2.2 Volume References
When referencing other volumes:
```
"See Volume 03 (Names and Symbols), section 2.2.1"
```

### 6.2.3 Multi-Section References
When pointing to multiple related sections:
```
"Read sections 3.1 through 3.3 for complete context"
```

## 6.3 Decision Persistence

### 6.3.1 Capturing Decisions
When you and AI reach a decision:
1. Document it in appropriate volume immediately
2. Mark with status using text markers: **FIRM**, **PLANNED**, **IN-DEV**, **DISCUSS**
3. List pros and cons of the decision (avoid confirmation bias)
4. Add cross-references to related sections

[AI: Using text markers instead of emoji makes it easier for everyone to type. Status goes in bold at start of relevant paragraph or as a prefix to the decision statement.]

### 6.3.2 Avoiding Lost Conversations
Instead of letting chat decisions disappear:
1. Add to standards volumes during conversation
2. Reference documented decisions in future chats
3. Build knowledge incrementally

### 6.3.3 Example Workflow
```
You: "Should we use underscores or hyphens?"
AI: "Here are the trade-offs..."
You: "Let's use underscores. Document that in Volume 03."
AI: [Adds to Volume 03 with rationale]
You: "Great. Now use that when refactoring X."
```

---
<!-- #endregion -->

<!-- #region 7. VERSION CONTROL AND STATUS -->
# 7. VERSION CONTROL AND STATUS

## 7.1 Document Status Values

### 7.1.1 Draft
Document is actively being written. Content may change significantly. Not yet suitable for AI agents to follow as authoritative.

### 7.1.2 Active
Document is complete and authoritative. AI agents should follow as standard.

### 7.1.3 Deprecated
Document has been superseded by newer version or approach. Kept for historical reference only.

## 7.2 Status Indicators Within Content

### 7.2.1 Decision Status
Use text markers in bold for clarity:
- **FIRM** - Final decision, implement immediately
- **PLANNED** - Agreed upon, not yet implemented
- **IN-DEV** - Currently being worked on
- **DISCUSS** - Under discussion, not yet decided
- **REJECTED** - Tried and abandoned (explain why)

Example: **FIRM:** Use underscores not hyphens in function names (see S03.1.1)

### 7.2.2 Code Status
- **CORRECT** - Follows conventions
- **AVOID** - Anti-pattern, do not use
- **ACCEPTABLE** - Exception with documented reason

## 7.3 Change Tracking

### 7.3.1 Git Commits
When updating standards:
- Commit message: `docs: update Volume NN section X.Y - brief description`
- Keep changes atomic (one topic per commit)
- Include rationale in commit message if not obvious

### 7.3.2 Major Revisions
For significant restructuring:
1. Create new branch: `docs/volume-NN-refactor`
2. Make changes
3. Update all cross-references
4. Merge when complete

---
<!-- #endregion -->

<!-- #region 8. NOTES -->
# 8. NOTES

## 8.1 General Principles
- **Consistency > perfection** - Following 80% of standards consistently is better than 100% inconsistently
- **Context matters** - Use judgment for edge cases
- **Document exceptions** - If you break a standard, add comment explaining why
- **Evolve prudently** - Standards can change, but do so deliberately with discussion

## 8.2 Future Enhancements
[AI: What other AI/human collaboration patterns should we document?]

Potential topics for future sections:
- Pair programming patterns with AI
- Code review workflows
- Testing strategies with AI assistance
- Debugging collaboration patterns

---
<!-- #endregion -->

**End of Document**

*Remember: Read the entire document before making code changes!*
