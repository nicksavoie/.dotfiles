# Schema

The note types and frontmatter this vault uses. `CLAUDE.md` points here; keep the detail in
this one note.

## Frontmatter key order

```yaml
---
title:
type:
status:
date:
tags:
---
```

Quote date values (`date: "2026"`) so YAML keeps them as strings.

## Note types

| `type` | Folder | Notes |
|---|---|---|
| Journal | `0 Journals/` | One note per day or week |
| Project | `1 Projects/` | Has an end; `status` and a target date |
| Area | `2 Areas/` | Ongoing; no end date |
| Reference | `3 Resources/` | Needs a `## Summary` section |

## Status values (projects)

`TODO`, `DOIN`, `DONE`, `HOLD`, `DROP`

Replace these with your own. Whatever you choose, list it here so the assistant flags a
mismatch instead of guessing.
