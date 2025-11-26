# [Issue Title Here]

## Planning
Create a PHASED "feature planning" document for this issue, to include a section on progress and another on questions for clarification. The resultant file goes into a .gitignored folder and is named in line with this issue number. I've updated the .github/copilot-instructions.md to understand all this.

## Execution
When the planning document has no more clarifying questions, proceed phase by phase in compliance with copilot-instructions. Let human agent validate completeness of the phase before proceeding to the next. Ask human agent to execute regular staging / commits to carefully manage your progress. In case of catastrophe, remind human to abandon changes in git, reset the conversation and say "we're working on XY-12345. [Further clarification]. Update the plans and continue".

#Documentation

Once the the plan is fully executed:

- Attach the .md to the ticket

- Write a test plan

- Pass the ticket to Q/A

- Clean up all temporary working files.

Find the original scope specification here to read, copy, edit, document, then delete in compliance with the above.
[path to any reference document, or delete this line if none]

---

## What Must NOT Break

- [Constraint 1: e.g., "Zero breaking changes to existing functionality"]
- [Constraint 2: e.g., "Must maintain file format compatibility"]  
- [Constraint 3: e.g., "Follow Section 1.2.3 naming conventions"]

## Files Involved

- [File to modify: e.g., "devsource/cnm.lsp"]
- [File to modify: e.g., "devsource/edclib.lsp"]
- [File to create (beyond internal docs): e.g., "devsource/haws-config.lsp"]

## Questions or Concerns

You are likely to have questions. Add them here or in the scope specification as

`[AI 2025-11-01 14:30: Your question here?]`

or

`[TGH 2025-11-01 14:30: Your question here?]`
