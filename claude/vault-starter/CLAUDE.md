# Vault Rules (Claude Code)

This is an Obsidian vault organised as PARA (projects, areas, resources, archive). Before
any task, read:
- `Meta/AI Context/Session Bridge.md`: where the last session left off.
- `Meta/AI Context/User Profile.md`: how the user works.
- `Meta/Templates/`: the template for whatever note type you are touching.

## Hard rules
- Never create, edit, move, rename or delete a file without asking first. Propose the
  change, show the diff, wait for a yes. Exception: `.claude/` and `Meta/AI Context/` may be
  edited freely.
- Never touch `.git/`, `.trash/` or `.obsidian/`.
- Never run `git push`, `git reset`, `git clean` or `git restore`. Commit only when asked.
- Flag schema ambiguities and inconsistencies. Do not resolve them silently.

## Batch changes
- For any change touching more than about ten notes, write a script. Do not edit one by one.
- The script defaults to a dry run that writes a report: files matched, a count, five to ten
  sample diffs. Apply only after the counts are approved.
- Before applying, confirm `git status` is clean and a checkpoint commit exists. If not,
  stop and say so.
- After applying, report files changed, the `git diff --stat` summary and anything skipped.

## Writing style
- Spelling: <ask: e.g. Canadian, British, American>.
- <ask: punctuation or tone rules, e.g. no em-dashes, no hedging>.
- `[[Wikilinks]]` for internal notes; footnotes at the end of the file for external sources.

## Boundaries
- <ask: what must never be written here, e.g. an employer's internal detail, credentials>.
  When in doubt, ask rather than deciding silently.

## Schema
Frontmatter keys, note types and status values are defined in `Meta/Schema.md`. Follow its
key order exactly.
