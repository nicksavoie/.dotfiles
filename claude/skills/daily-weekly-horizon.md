# today, weekly-review and horizon

Three read-only skills that answer "what now" at three altitudes. They read notes other
skills maintain and never duplicate their logic.

| Skill | Altitude | Answers |
|---|---|---|
| `today` | A day | What is on for today, and what did the last session leave? |
| `weekly-review` | A week | How did the week go, and what are two or three candidate focuses for the next? |
| `horizon` | Months | What is the next phase, and what has sat unstarted too long? |

They assume the vault layout in [../vault-setup.md](../vault-setup.md): a handoff file, an
open-loops list, and a projects folder. Rename the paths to suit.

## today

```markdown
---
name: today
description: Use when the user asks "what's today", "morning brief", or "where are we at", or invokes /today. Composes the handoff file, recent git changes and only the dated or due-soon items into one short answer.
---
```

1. Run `date`; state the weekday and date.
2. Read the handoff file: pull "Last session" and "Suggested edits" in a line or two each.
3. Read `git log --since="2 days ago" --oneline` and `git status --short`. Flag any
   handoff item the log shows is already done (see [recap](recap.md)).
4. Scan the open-loops list and scratch notes for items with a date or weekday inside the
   next 14 days, or that name an unblock date. Convert relative dates to absolute first.
5. Surface a horizon item only if it is dated inside the window or has sat "not started"
   across several sessions.
6. Present at most about eight lines: dated items first, then last-session leftovers, then
   the rest. If nothing is dated, say so.

Optional `transit` argument: apply a filter for limited means (on a train, no computer):
three to six items that need only a decision or a message, nothing that needs a computer
or focus.

Do not surface routine chores or generic nags, or anything blocked on someone else with no
action left on the user's side (except the unblock date).

## weekly-review

```markdown
---
name: weekly-review
description: Use when the user asks for a weekly review, "how was my week", or on a Sunday or Monday when planning the next. Reads the week's log, open loops, git history and project movement, then gives a short review and two or three candidate focuses.
---
```

Arguments: none for the current ISO week to date (review the previous week instead if it
is early Monday), `last` for the previous completed week.

1. Pin the ISO week and state its Monday-to-Sunday dates.
2. Read whatever the week's log holds. Summarize only what is logged; blank means not
   mentioned, never zero. List days with no entry; they are the main data gap.
3. Group `git log --since=<Monday>` by theme, as in [recap](recap.md). Scan other repos
   that matter the same way, one line each.
4. From the open loops and projects: what closed, what opened, and what has not moved for
   three or more weeks (ask, do not decide). A project untouched for weeks is a prompt to
   mark it on hold, not a failure.
5. List items dated in the next 14 days.
6. Offer two or three candidate focuses for next week, shaped by the user's working
   profile: precision work early in the day, a lighter plan after a rough week, checklists
   instead of "remember to". Offer, never assign.
7. Present in about 25 lines. Offer, but do not write, a rollup entry for a trends note.

Read-only. Leave out routine chores and anything outside the boundaries in `CLAUDE.md`.

## horizon

```markdown
---
name: horizon
description: Use when the user asks about the long game, big picture, next project phase, or parked long-term tasks. Distinct from day-to-day open loops; surfaces multi-month items.
---
```

1. Read the main project note: current phase, what is done, what the next one or two
   phases cover.
2. Read the "parked" section of the open-loops list.
3. Check any note that tracks a phase of that project.
4. Check scratch notes for scoped-but-unstarted projects not yet in the phase list.
5. Present the current phase, what closes it, and what is queued, as short bullets.
   Call out anything "not started" for several sessions running; that is the signal.

Horizon is the opposite altitude from `transit` (small, immediate) and `recap` (backward).
