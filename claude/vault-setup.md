# Build a Git + Obsidian + Claude vault

A notes vault that Claude Code can read and edit safely: plain markdown in Obsidian, a
local git repository as the rollback net, and a `CLAUDE.md` plus skills that set the rules.
Use this to start a fresh vault (a new job, a second vault) or to hand the setup to someone
else. A vault built this way takes about ten minutes, plus time to write your own rules.

If you are Claude Code reading this to build one: follow the steps in order, run the
bootstrap script, then write the skills from the recipes. Ask the user for the target path
(default `~/Documents/Vault`) and for the answers marked **ask**.

## Why this shape

- **Plain markdown in a folder.** Obsidian is the editor and viewer; Claude reads and
  writes the same files. No export step, no database.
- **Git for rollback, not for hosting.** A local repository with no remote is enough. Every
  batch edit has a checkpoint to return to, and the history doubles as a log for the
  [recap](skills/recap.md) skill. Add a remote later only if you decide to.
- **Rules live in the vault.** `CLAUDE.md` holds the always-on rules; skills hold the
  procedures; a small `Meta/AI Context/` folder holds what Claude needs to remember between
  sessions. The assistant may edit that folder freely, which makes it the working memory.
- **Ask first everywhere else.** The assistant proposes, shows the diff and waits. That is
  slower than letting it write, and it is the reason the vault stays trustworthy.

## Layout

```
Vault/
  CLAUDE.md                 rules loaded into every session
  0 Journals/               dated entries
  1 Projects/               things with an end
  2 Areas/                  ongoing responsibilities, open-loops list
  3 Resources/              reference notes, media, people
  4 Archive/                finished or dormant
  Meta/
    AI Context/             Session Bridge, profile, lessons (assistant may edit freely)
    Templates/              one template per note type
  .claude/
    skills/                 one folder per skill, each with a SKILL.md
```

The numbered folders are PARA (projects, areas, resources, archive). Use other names if you
prefer; update `CLAUDE.md` and the skills to match.

## Steps

1. **Run the bootstrap.**

   ```sh
   bash vault-starter/bootstrap.sh ~/Documents/Vault
   ```

   It creates the folders, copies the starter files, and runs `git init` with a first
   commit. It refuses to touch a folder that already has files in it.

2. **Open the folder as a vault in Obsidian.** Install a linter plugin if you want
   frontmatter and formatting normalised on demand. Run it yourself; do not have Claude
   trigger it.

3. **Fill in `CLAUDE.md`.** **Ask** the user for: spelling variant, any style rules (for
   example no em-dashes), what must never be written down (employer detail, secrets), and
   the note types and frontmatter keys they want. Put the schema in one note under `Meta/`
   and point `CLAUDE.md` at it rather than pasting it in.

4. **Fill in `Meta/AI Context/User Profile.md`.** How the user works, what helps and what
   does not. Keep it short and factual.

5. **Build the skills.** Create `.claude/skills/<name>/SKILL.md` for each recipe you want,
   using the frontmatter and procedure in [skills/](skills/). Start with
   `session-checkpoint`, `recap` and `batch-edit`; add the rest as the need appears. Read
   [skills-anatomy.md](skills-anatomy.md) first. Adapt the paths in each recipe to the
   layout above.

6. **Set `settings.json` and the status line** from [settings.md](settings.md) and
   [statusline/](statusline/).

7. **Test.** Start `claude` in the vault, ask "catch me up", then make a small change and
   confirm it asks before writing. Run `/clear` after a checkpoint and confirm a fresh
   session can resume from the handoff file alone.

## Habits that make it work

- Checkpoint before `/clear` and at the end of every session. The handoff file is what
  makes a cold start cheap.
- Commit a checkpoint before any batch edit. Let the user do the rollback.
- Keep the vault's `CLAUDE.md` short. When it grows, move procedures into skills.
- Keep a `Lessons-Learned` note. A mistake written down once is a rule you do not have to
  re-derive.
- Keep project repos separate from the vault. Write what you learn about a project into a
  vault note and keep the repo's own `CLAUDE.md` for build commands and layout only.

## Sync

Git is the rollback net, not the sync. For several devices use Obsidian Sync or any file
sync, and keep the git history local to one machine. Do not run two sync mechanisms over the
same folder, and if you do add a git remote, decide which side wins before the first
conflict.
