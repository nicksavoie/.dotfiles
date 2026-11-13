# settings.json

`~/.claude/settings.json` holds the user-level defaults. On NixOS I manage it declaratively
(a home-manager module writes it), so a rebuild is the only way it changes. Elsewhere,
edit the file directly.

```json
{
  "model": "sonnet",
  "effortLevel": "high",
  "modelSettings": {
    "claude-haiku-5-5": { "effortLevel": "high" },
    "claude-opus-5-5": { "effortLevel": "high" },
    "claude-sonnet-5-5": { "effortLevel": "high" }
  },
  "permissions": { "defaultMode": "auto" },
  "statusLine": {
    "type": "command",
    "command": "bash ~/.claude/statusline-command.sh",
    "refreshInterval": 30
  },
  "maxProseWidth": 120,
  "theme": "auto"
}
```

## Notes

- **Effort per model:** set `effortLevel` globally and again under `modelSettings` for
  each model, so switching models does not silently change it. Confirm with `/status`
  after a restart.
- **Permissions:** `defaultMode` sets how often it asks. Pair a looser mode with the
  CLAUDE.md hard rules, not instead of them. Use the `fewer-permission-prompts` skill to
  build an allowlist from real usage rather than guessing.
- **Status line:** a script that reads JSON on stdin and prints one line. Keep it quiet;
  directory and git state on the focused line are enough.
- **Project overrides:** `.claude/settings.json` in a repo overrides these for that repo.

## Project hooks (auto-commit safety net)

A project-level `.claude/settings.json` can run a hook when a session ends. This one
commits any uncommitted work in the vault, so a bare `/clear` never leaves the history
behind the working tree. No rebuild is needed; the file is tracked with the vault.

```json
{
  "hooks": {
    "SessionEnd": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "bash \"$CLAUDE_PROJECT_DIR/.claude/hooks/auto-commit.sh\"",
            "timeout": 20
          }
        ]
      }
    ]
  }
}
```

Copy [hooks/auto-commit.sh](hooks/auto-commit.sh) to `.claude/hooks/` in the vault. It
only acts when the session's directory is inside a git repo that contains
`Meta/AI Context/Session Bridge.md`, skips during a merge or rebase, never touches a
remote, always exits 0, and logs each run to `~/.cache/vault-auto-commit.log`. It needs
`jq` and `git` on the PATH. Test it against a scratch repo before enabling it, and read
the log after the first real `/clear` to confirm `SessionEnd` fired.
