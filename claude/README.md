# Claude Code setup

How I configure Claude Code and the skills I rebuild first on a new machine. These are
recipes: each one says what the thing is for, where it lives and what goes in it, so it can
be rebuilt for a different project or a different person in a few minutes. Start with
[vault-setup.md](vault-setup.md) to build a whole vault.

| Guide | What it covers |
|---|---|
| [vault-setup.md](vault-setup.md) | Build a Git + Obsidian + Claude vault from scratch (with a [bootstrap script](vault-starter/bootstrap.sh)) |
| [skills-anatomy.md](skills-anatomy.md) | The shape of a skill: file layout, frontmatter, writing a description that triggers |
| [claude-md-patterns.md](claude-md-patterns.md) | The rules I put in a project `CLAUDE.md` and why |
| [settings.md](settings.md) | `settings.json`: effort, permissions, status line |
| [statusline/](statusline/) | The status line script and how it works |
| [skills/session-checkpoint.md](skills/session-checkpoint.md) | Handoff file so the next session starts warm |
| [skills/daily-weekly-horizon.md](skills/daily-weekly-horizon.md) | `today`, `weekly-review` and `horizon`: what now, this week, the long game |
| [skills/idea-parking.md](skills/idea-parking.md) | Park side ideas in one line mid-task, then drain them in short guided passes |
| [skills/batch-edit.md](skills/batch-edit.md) | Large changes by dry-run script, with a small context |
| [skills/mess-report.md](skills/mess-report.md) | Measure new notes, shrunk notes and lost links after a task |
| [skills/model-choice.md](skills/model-choice.md) | One-line nudge to use the stronger model for design work, and how to measure it |
| [skills/live-prototype.md](skills/live-prototype.md) | Prove a UI or config change live with commands before writing it into the system config |
| [skills/session-card.md](skills/session-card.md) | A `SessionStart` hook: a short "where you are" card and one rotating tip on the first reply ([hook](hooks/session-card.sh), [tips starter](tips-starter.md)) |
| [skills/recap.md](skills/recap.md) | "Catch me up" from git history, flagging stale notes |
| [skills/project-from-idea.md](skills/project-from-idea.md) | Turn a rambling idea into a scoped project note |
| [skills/downloads-triage.md](skills/downloads-triage.md) | Sort a folder of mixed files without touching the source |

The principle behind all of them: the model is fast and tireless, so the setup spends its
effort on keeping changes reviewable. Anything that writes in bulk gets a dry run and a
rollback point first, and anything ambiguous gets flagged instead of silently resolved.
