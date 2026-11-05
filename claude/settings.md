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
