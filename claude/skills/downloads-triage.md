# downloads-triage

Sorts a folder of unknown, mixed files into buckets before anything is moved, edited or
deleted.

## SKILL.md

```markdown
---
name: downloads-triage
description: Use when the user asks to go through a folder of mixed files (Downloads, an export, a raw dump) and work out what belongs in the notes. Not for a single already-identified file.
---
```

## Procedure to write into the body

**Rule zero:** never modify, move or delete anything in the source folder until the user
has approved that specific action. Reading, and staging extracted text in a scratch folder,
is fine.

1. **List and skim** every file: names, sizes, the header row or first page. No deep
   reading yet.
2. **Sort into four buckets in one report file,** not a wall of chat:
   - Ready to use: small, clearly one thing. Stage the extracted text and ask one
     placement question per item.
   - Large batches: tens to hundreds of similar rows. Ask for a go-ahead before staging,
     not just before applying.
   - Sensitive or ambiguous: official documents, anything with a password, third-party
     data. Read only enough to classify, and flag it.
   - Not content: browser profiles, junk. Recommend skipping.
3. **No conversion tool installed?** Use a small standard-library script instead of
   installing anything permanently.
4. **After the answers,** place items under the normal ask-first rule, then delete only
   the specific source files the user confirmed. Never a blanket "clean up".
5. **Delete your staging copy in the same pass** once a file is confirmed placed. Staged
   copies left behind are the usual failure; keep one only if the user says it is a live
   working reference, and say so in the report.
6. **Large cross-referencing jobs** use the [batch-edit](batch-edit.md) approach.
