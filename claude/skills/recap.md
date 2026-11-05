# recap

"What changed recently" or "catch me up", answered from git history and checked against
the project's own status notes.

## SKILL.md

```markdown
---
name: recap
description: Use when the user asks what changed recently, "catch me up", or "what did we do last session". Summarizes recent git history and cross-references it against the status notes to flag anything now stale.
---
```

## Procedure to write into the body

1. `git log --oneline -20` (longer if asked).
2. For commits whose message does not explain the why, read the full message or
   `git show --stat`.
3. Group commits into themes in plain language, not a flat list of subjects.
4. Open the status notes (a session handoff file, a project status page). If one calls
   something "not started" or "open" that the log shows is done, flag the mismatch instead
   of trusting either source.
5. Present a short themed list.

## Rules

- Read-only. If a status note is stale, say so and ask before changing it, unless it sits
  in a folder the assistant is allowed to edit freely.
