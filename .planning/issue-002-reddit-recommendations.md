# Implementation Plan: Reddit u/almost_not_terrible AI Collaboration Recommendations

## Issue
https://github.com/hawstom/cnm/issues/2

## Source
Reddit comment: https://www.reddit.com/r/GithubCopilot/comments/1nwsxjs/comment/nhii0h6/

## Recommendations Summary

### 1. Phased Feature Planning Documents
**Recommendation**: For each well-defined ticket, create a phased "feature planning" document that includes:
- Phased breakdown of work
- Progress tracking section
- Questions for clarification section

**Storage**: `.gitignored` folder, named by ticket number

**Update copilot-instructions.md** to understand this workflow

### 2. Multiple Worktrees (Advanced)
**Recommendation**: Use git worktrees to work on multiple tickets simultaneously in different VS Code instances

**For CNM**: Start with 1 worktree (we're learning), scale up later

### 3. Token-Heavy Workflow
**Recommendation**: Rotate between planning docs, letting AI work through phases with validation checkpoints

**Key practices**:
- Regular staging/commits to manage progress
- No qualms about ditching changes and resetting conversation
- Interrupt without impact (planning doc preserves state)
- Use Windows-H (speech-to-text) to work at "speed of thought"

### 4. Ticket Completion Workflow
Once feature complete:
1. Attach .md to ticket
2. Write test plan
3. Pass to Q/A
4. Clean up after itself

---

## Implementation Plan for CNM

### Phase 1: Basic Setup (This Session) ✅
**Status**: COMPLETE

1. [x] Create `.github/copilot-instructions.md` - DONE
2. [x] Create `docs/CNM_USE_CASE.md` - Comprehensive context for AI
3. [x] Create this planning document

### Phase 2: Planning Document Workflow (Next Session)
**Goal**: Set up infrastructure for per-ticket planning docs

**Tasks**:
1. Create `.gitignore` entry for `.planning/` folder
2. Create `.planning/` folder structure
3. Update `.github/copilot-instructions.md` to reference planning workflow
4. Create template for planning documents

**Template Structure**:
```markdown
# Feature: [Issue #X - Title]

## Status
- [x] Phase 1: Planning
- [ ] Phase 2: Implementation
- [ ] Phase 3: Testing
- [ ] Phase 4: Documentation

## Current Phase
**Phase 1: Planning**

## Progress
- Created planning document
- ...

## Questions for Clarification
1. Question here?
2. Another question?

## Phase Details

### Phase 1: Planning
- [ ] Task 1
- [ ] Task 2

### Phase 2: Implementation
...

## Notes
...
```

### Phase 3: Worktree Learning (Future)
**Goal**: Learn git worktrees for parallel ticket work

**Prerequisites**:
- Comfortable with planning doc workflow
- Multiple tickets ready to work on
- Understand git worktrees conceptually

**Resources**:
- https://git-scm.com/docs/git-worktree
- Practice on non-critical tickets first

### Phase 4: Full Workflow Integration (Future)
**Goal**: Seamless ticket → plan → implement → test → close workflow

**Includes**:
- Automated planning doc creation from ticket
- Progress tracking synchronized with commits
- Test plan generation
- Cleanup scripts

---

## Benefits for CNM Project

### Immediate (Phase 1-2)
1. **Better context**: AI understands CNM domain deeply
2. **Clear scope**: Each ticket has detailed plan before coding
3. **Recoverable**: Can pause/resume work without losing context
4. **Documented**: Natural artifact of development process

### Medium-term (Phase 3-4)
1. **Parallel work**: Multiple features in progress simultaneously
2. **Faster iteration**: Planning docs enable quick pivots
3. **Better testing**: Test plans generated as part of workflow
4. **Historical record**: Planning docs show decision rationale

### Long-term
1. **Onboarding**: New developers (human or AI) understand project quickly
2. **Maintenance**: Decision history preserved
3. **Quality**: Systematic approach reduces bugs
4. **Velocity**: Less context switching, more focused work

---

## Current Session Progress

### Completed
✅ Git cleanup (stash cleared)
✅ Milestone recognized (#region organization)
✅ copilot-instructions.md created
✅ CNM_USE_CASE.md created
✅ Strict validator implemented (`hcnm-ldrblk-lattribs-validate-schema`)

### In Progress
- This planning document (captures the Reddit recommendations)

### Next Actions
1. Commit this planning document
2. Update `.github/copilot-instructions.md` to reference planning workflow
3. Create `.planning/` folder infrastructure
4. Start using planning docs for next ticket

---

## Questions for Clarification

### Q1: Planning Doc Location?
**Options**:
A. `.planning/` folder (gitignored, per Reddit)
B. `docs/planning/` folder (tracked in git for historical record)
C. Hybrid: Active in `.planning/`, archive completed to `docs/archive/`

**Your preference?**

### Q2: Ticket Naming Convention?
**Format for planning docs**:
- `issue-002-reddit-recommendations.md` (GitHub issue format)
- `GH-002-reddit-recommendations.md` (alternate)
- Other?

### Q3: Worktree Timing?
When do you want to learn worktrees?
- Now (ambitious)
- After comfortable with planning docs (recommended)
- Later when needed (conservative)

---

## Notes

### On "Fail Loudly"
We're applying this principle to:
- `lattribs-validate-schema` - Strict validation, clear error messages
- All data transformations - Don't silently fix bugs
- User-facing errors - Context-rich alerts

### On Documentation
Reddit comment emphasizes: "Documentation is good, and embracing that will make you a better software engineer."

CNM has been documentation-light in code (heavy in standards docs). This planning workflow adds:
- **Process documentation** (planning docs)
- **Decision documentation** (why, not just what)
- **Progress documentation** (state at each commit)

### On Token Usage
Reddit comment: "Yes it churns tokens. Yes it's worth it!"

For CNM: We're building a 20+ year codebase. Quality > speed. Extra tokens for planning = fewer bugs = happy customers.
