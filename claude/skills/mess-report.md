# mess-report

A read-only script that measures what a task did to a notes folder kept in git, so the summary does not depend on the assistant's own count. It addresses two frictions: the assistant creates more notes than anyone can review, and it condenses existing notes and loses detail.

## Script

`scripts/mess-report.sh` compares the working tree with `HEAD` (or `HEAD~1..HEAD` with `--last`) and prints:

- new notes, with a budget of 5 per task;
- notes that shrank by 20% or more and at least 150 words;
- notes that lost wikilinks;
- Review List entries added (only if the vault has that file).

The last line is `New notes: N. Shrunk: N. Lost links: N.` Thresholds are variables at the top of the script.

## Where it runs

1. The checkpoint recipe runs it before the commit. An unintended shrink or lost link gets one plain question before committing; an intended move is stated and passes; an over-budget count is stated, not blocked.
2. The batch-edit recipe includes the last line in its report.
3. The assistant runs it at the end of any task that created or edited more than two notes.

## The rule to load every session

```markdown
End a task with the mess report; extend notes over creating new ones; condensing existing content needs before and after word counts and the user's yes.
```

A bulk import or research run that reports its own counts is exempt from the new-note budget.
