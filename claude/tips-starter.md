# Tips starter

The rotation pool for the session card hook. Copy this file to the path set as `TIPS` in the hook, then replace the tips with your own hotkeys, skills and habits. The hook shows the first tip it has not shown yet and records it in `~/.cache/session-card-tip-state`; when every tip has been shown, the cycle restarts. Interleave the kinds so they rotate. Moving a line under `## Retired` removes it from the rotation. `Tips: off` stops the tip line.

```markdown
Tips: on

## Tips
- workflow | /clear between tasks; a long session re-sends its whole history every turn. | your note
- skill | Say "what's today" for the full morning brief. | your skill list
- hotkey | (one of your own hotkeys, under about 90 characters.) | your cheat sheet
- workflow | Say "checkpoint" before /clear so each thread's state is saved. | your note

## Retired
```

Format of a line: `- kind | tip | source`, where kind is `hotkey`, `skill` or `workflow`.
