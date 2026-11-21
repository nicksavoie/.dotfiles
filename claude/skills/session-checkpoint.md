# session-checkpoint

Writes a short summary of the session to a handoff file, so the next session, with no
memory of this one, can read one file and carry on. Run it before `/clear`, at the end of
a session, or on "wrap up".

The skill is named `session-checkpoint` rather than `checkpoint` because `/checkpoint`
is a reserved built-in command.

## SKILL.md

```markdown
---
name: session-checkpoint
description: Use before /clear, at the end of a session, or when the user says "checkpoint", "wrap up", or "bridge to next session". Consolidates what happened this session into the handoff file so the next session can pick up cold.
---
```

## Procedure to write into the body

The handoff file is `Meta/AI Context/Session Bridge.md` in the vault layout from
[../vault-setup.md](../vault-setup.md).

1. **Scope the session:** decisions made, facts learned, edits applied, and anything left
   open or deferred.
2. **Check what actually landed.** Read `git log --oneline -20` and compare it with the
   conversation. The summary reflects committed reality, not intentions.
3. **Route durable facts elsewhere first.** If a fact belongs in a standing note (a
   profile, a project status page), update that note and do not repeat it in the handoff.
   If the session finished work a standing note still calls open, fix the note now.
4. **Clean up scratch** whose job is done: staged files that were placed, finished audit
   reports, one-off scripts. Before deleting anything a durable note cites, grep for the
   name and fix the reference in the same pass.
5. **Log a lesson** if the session caught a process mistake worth remembering (a
   near-miss, a skill gap, a correction from the user). One short entry: what happened and
   the fix. Prune entries that have since become a rule or a skill.
6. **Optional project changelog.** If another repo moved this session, append a dated
   section to that project's changelog note: what changed and why, behaviour the user
   would notice, and what was tried and failed with the reason. Failed experiments are the
   part that gets lost. Restrict the silent edit to the named changelog file and append only.
7. **Overwrite the handoff file** (do not accumulate) with:
   - `## Last session (<date>)`: three to six bullets of what changed, what is open, what
     is next.
   - `## Suggested edits not yet made`: anything noticed that belongs in a real note but
     needs the user's sign-off. Name the file and the change.
8. **Report the one-line gist** back, so the user does not have to open the file.
9. **Mess report, then commit, with no further prompt.** First run `scripts/mess-report.sh` (see `mess-report.md`) and put its last line in the report; ask one plain question about a shrunk note or lost link the session did not intend. Then `git add -A`, then one local commit titled
   `Session checkpoint: <thread names>`. Say `Committed <hash>; ready to /clear.` Never
   push. The user's standing permission and the trigger phrase are the yes; do not ask.

Widen the skill's `description` so natural phrases trigger it: "goodnight", "going to
bed", "clearing", "clear context", "done for now", "heading out", "wrapping up", plus the
originals. Typing a trigger phrase is cheaper than typing "checkpoint" and "commit" as
two prompts every session.

## Safety net for a bare `/clear`

`/clear` is a built-in command the CLI handles itself, so the skill cannot run before it.
A `SessionEnd` hook can still commit any dirty work: see
[../hooks/auto-commit.sh](../hooks/auto-commit.sh) and the project-settings example in
[../settings.md](../settings.md). It cannot update the handoff file (that needs the
model), so say a trigger phrase first when the handoff should be current.

## Rules

- The handoff is a rolling bridge, not a log. Each run replaces the "Last session" section.
- Edit only the assistant's own folder without asking. Everything else goes under
  "Suggested edits".
- Keep it scannable: short bullets, no paragraphs.
