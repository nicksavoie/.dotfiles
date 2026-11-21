# Idea parking

Ideas arrive in the middle of other tasks, faster than they can be sorted. This recipe captures each one in a line without derailing the task, then drains the pile in short guided passes. The aim is to get the whole cloud written down cheaply and assemble it later, not to suppress ideas.

It needs a capture file (here `Inbox/Quick Capture.md`, with a `## Parked` section), the [inbox-triage](#inbox-triage-guided-mode) skill, and a one-line rule that loads every session (a memory or a `CLAUDE.md` line).

## The capture rule

Put this where it loads every session:

```markdown
When a prompt contains an item that is not the current task (an idea, a "what if", a
"for later", a side topic that is not an instruction), do not discuss, expand or build it.
Append one line to `Inbox/Quick Capture.md` under `## Parked`:
`- [ ] YYYY-MM-DD: <the user's own words, trimmed to one line>`
Then carry on with the task. The reply carries one line: `Parked 3: <short labels>`.
No evaluation, pros and cons, or follow-up questions about a parked item.
If it is unclear whether something is an instruction or an idea, park it and say so.
"Do it now" (or any clear redirect) turns a parked item into the current task using the
parked text, and the line is removed from the file.
During a build with a `Done when` list, anything outside that list is parked, not added.
Facts the user reports and direct instructions are not parked.
```

Keep the user's wording, with no tags and no destination guess; classification is triage's job.

## inbox-triage guided mode

Add a `guided` argument to the triage skill:

1. Take at most 8 items (oldest first) or 3 clusters. Say how many remain and stop.
2. Group related items into clusters so one click settles several.
3. Ask with a clickable question tool, at most 4 questions per call, recommended option first. Options: do next, make a project, open loop with a date, keep parked with a revisit date, drop.
4. Only one "do next" while a project slice is active.
5. An item kept through two passes is asked about dropping; the recommended option is drop. State lives in a suffix on the line, `[kept N, revisit YYYY-MM-DD]`, with no side file.
6. Apply each answer straight away: add to the destination first, then remove from the capture file.
7. Report `moved N, kept M, dropped K`.

## Surfacing

- The daily view (`today`) prints `Parked: N (oldest D days)` and offers a pass only in the evening (the user's productive window) when N is 10 or more or the oldest item is 14 days or older.
- The weekly review reports the count, the oldest age and how many arrived and left that week.

## Measure

After two weeks: the capture file never above 15 items, median age 14 days or less, any older suggestion list down to a quarter of its size. Report the count of long prompts but do not target it; it may not fall.

Related: [daily-weekly-horizon.md](daily-weekly-horizon.md), [../skills-anatomy.md](../skills-anatomy.md).
