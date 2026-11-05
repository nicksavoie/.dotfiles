# Anatomy of a skill

A skill is a markdown file of instructions that Claude loads only when the task matches
its description. That keeps `CLAUDE.md` short: always-on rules live there, procedures
live in skills.

## Layout

```
.claude/skills/<name>/SKILL.md      # per project
~/.claude/skills/<name>/SKILL.md    # for every project
```

## File shape

```markdown
---
name: batch-edit
description: Use for any change touching more than ~10 files ...
---

# Batch Edit

Steps, rules, and notes.
```

## The description is the trigger

Claude decides whether to load a skill from the `description` alone, so it carries the
whole job of matching:

- Start with "Use when ..." and name the situations, including the phrasings you would
  actually type ("catch me up", "audit my downloads").
- Say what it is **not** for, so neighbouring skills do not collide.
- Keep it to one or two sentences. Detail belongs in the body.

## Body conventions that have worked

- Numbered steps for the procedure, a short "Notes" section for edge cases.
- A "rule zero" line when one constraint outranks everything (for example: never modify
  the source folder until the specific action is approved).
- Say when the skill is read-only, and what it must ask before changing.
- Record lessons from real failures inline, with the date, so the reason survives.
- Point at other skills by name instead of duplicating their steps.

## Testing one

Phrase a request the way you would naturally, in a fresh session, and check that the skill
loads. If it does not, fix the description before touching the body.
