# session-card

A `SessionStart` hook that gives the first reply of a new or cleared session a short "where you are" card and one rotating tip. It addresses two frictions: opening a session with "where was I" or "what's next", and hotkeys, skills and habits that slip out of working memory because nothing repeats them.

## What it does

`hooks/session-card.sh` runs on the `startup` and `clear` sources only, never on `resume` or `compact`, so the card does not repeat mid-work. It is shell only, makes no model call, reads the project without writing to it, writes only under `~/.cache/`, and always exits 0. It gathers:

- the hour, and from it a time band (before 12:00, 12:00 to 17:00, after 17:00);
- the last commit (hash, age) and the uncommitted file count;
- the top three rows of a thread index table;
- unchecked open-loop lines marked `(due YYYY-MM-DD)` that are overdue, due today or due tomorrow (an unmarked line never appears);
- the next tip from a Tips file.

It prints one JSON object whose `additionalContext` holds those facts and the rules for the card. The model writes the card in its first reply, then answers the user's message. The card is at most 3 lines before 12:00, 4 from 12:00 to 17:00 and 5 after 17:00, with no tables and lines under about 80 characters. It is skipped when the first message is a slash command, a goodbye or a check-in.

## Build it

1. Copy `hooks/session-card.sh` to `.claude/hooks/` and set the three paths at the top (`BRIDGE`, `LOOPS`, `TIPS`).
2. Copy `tips-starter.md` to the `TIPS` path and replace the tips with your own.
3. Add the entry to `.claude/settings.json`:

```json
{
  "hooks": {
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PROJECT_DIR/.claude/hooks/session-card.sh\"",
            "timeout": 5
          }
        ]
      }
    ]
  }
}
```

## Test it before trusting it

1. **Assumptions first.** Start with a hook that only appends `source` to the log and emits a fixed test sentence. Confirm that `startup` and `clear` log and reach the first reply, and that `resume` and `compact` log and stay silent. Then swap in the real script.
2. **Time bands.** Run `echo '{"source":"startup"}' | CARD_DRY=1 CARD_HOUR=8 bash .claude/hooks/session-card.sh | jq -r .hookSpecificOutput.additionalContext` for hours 8, 14 and 19. The three shapes differ.
3. **Due items.** Mark one line due yesterday, today, tomorrow and in three days, and leave one unmarked. Only the first three appear.
4. **Rotation.** Run it once per tip without `CARD_DRY`. Each tip appears once, then the cycle restarts.
5. **Safety.** Run it with `jq` missing and with a missing project directory. Both print nothing and exit 0. It runs in well under half a second.
6. **Live.** Start a session and say "hi": a card on the first reply, none on the second. Start another with "goodnight": no card.

## Known quirk

The hook cannot see the first message, so a session that skips the card still advances the tip rotation.
