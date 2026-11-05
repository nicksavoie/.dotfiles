# CLAUDE.md patterns

`CLAUDE.md` is loaded into every session, so it holds rules that must always apply and
points at where the detail lives. Procedures go in skills.

## Hard rules worth having

- **Ask before changing files.** Propose the change, show the diff, wait for a yes. Carve
  out the folders the assistant may edit freely (its own notes, scratch space).
- **Never touch version-control internals or editor state** (`.git/`, `.obsidian/`).
- **Do not run `commit`, `push`, `reset`, `clean` or `restore` unless asked.** Rollback is
  the human's decision. Loosen this per project once you trust the workflow, for example
  allowing local checkpoint commits but never a push.
- **Flag ambiguity instead of resolving it silently.** Schema conflicts, unclear
  ownership, two sources that disagree: say so.

## Bulk changes

For more than about ten files, write a script, not a run of single edits. The script
defaults to a dry run that writes a report (files matched, count, five to ten sample
diffs). Apply only after the counts are approved. Check that the working tree is clean
and a checkpoint commit exists first. See [skills/batch-edit.md](skills/batch-edit.md).

## Writing style

If the output lands in documents, put the style rules in the file once: spelling variant,
no em-dashes, how to mark up links and footnotes, a direct tone with no hedging.

## Boundaries

State what must not be written down at all. For work on a personal machine that means
the line between life-relevant facts (pay, stress, commute) and the employer's internal
detail, which belongs on the work account. When in doubt, ask.

## Keep it honest

A rule that conflicts with a standing permission confuses the next session. When the
permission changes, edit the rule. Verified sources beat remembered claims, so say which
wins.
