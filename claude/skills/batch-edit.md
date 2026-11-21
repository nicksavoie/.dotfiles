# batch-edit

For any change touching more than about ten files: a migration, an audit, a
cross-referencing pass. The rule in `CLAUDE.md` is one line; this is the full procedure.

## SKILL.md

```markdown
---
name: batch-edit
description: Use for any change touching more than ~10 files - a big migration, audit, or cross-referencing pass. Script-driven procedure with a dry run, and how to keep the main conversation's context small during heavy read-only passes.
---
```

## Procedure to write into the body

1. **Check the working tree is clean and a checkpoint commit exists.** If not, stop and
   say so. Do not start a batch against uncommitted work.
2. **Write a script that defaults to a dry run.** It writes a report: files matched, a
   count, five to ten sample diffs. No writes on this pass.
3. **Share the report and wait for explicit approval of the counts.** Never apply in the
   same turn you propose.
4. **Apply only after a yes.** Never reset, clean, restore or push; rollback is the human's
   call, even if the batch looks wrong afterwards.
5. **Report after applying:** files changed, the `git diff --stat` summary, anything
   skipped and why, plus the last line of `scripts/mess-report.sh` (new notes, shrunk notes, lost links).

## Keeping context small

Reading hundreds of files to cross-reference them burns context faster than the edits do.

- Hand the read-heavy comparison to a subagent so its row-by-row output stays out of the
  main thread and only the summary returns.
- Checkpoint between phases of a long job, so a compaction or a break does not lose the
  thread.
- Prefer the script's own output file over reading every matched file into context. The
  report's samples are for review, not a substitute for loading all N files.
