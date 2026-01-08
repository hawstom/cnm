# Planning Documents Folder

## Purpose
This folder contains **active** feature planning documents for work in progress. These are AI collaboration artifacts following the workflow from [Reddit u/almost_not_terrible](https://www.reddit.com/r/GithubCopilot/comments/1nwsxjs/comment/nhii0h6/).

## Why Gitignored?
Planning docs are:
- **Ephemeral**: Active during development, archived when complete
- **Conversational**: Include AI clarification questions and iterative thinking
- **Personal**: Reflect your current working state, not final project state

## Workflow
1. Create ticket in GitHub Issues
2. AI creates planning doc: `.planning/issue-NNN-short-name.md`
3. Work through phases with AI, updating progress
4. When complete: Attach to ticket, archive if desired

## Completed Planning Docs
Optionally archive completed planning docs to `docs/archive/planning/` for historical record.

## Current Convention
**Filename format**: `issue-NNN-kebab-case-description.md`
- `issue-002-reddit-recommendations.md`
- `issue-003-lattribs-validator.md`
- etc.
