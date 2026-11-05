# Status line

A one-line Claude Code status line: repo, branch and git state on the left, and Remote
Control marker, context use and rate-limit usage on the right. [statusline-command.sh](statusline-command.sh)
is the working script; this page explains the parts so it can be changed.

## Install

1. Copy the script to `~/.claude/statusline-command.sh`.
2. Point `settings.json` at it (see [../settings.md](../settings.md)):

   ```json
   "statusLine": { "type": "command", "command": "bash ~/.claude/statusline-command.sh", "refreshInterval": 30 }
   ```

Needs `bash`, `jq`, `git` and `awk`. Colours are 24-bit; swap the palette block at the top
to change theme (it is Catppuccin Mocha).

## How it works

- Claude Code runs the command and pipes a JSON blob to stdin: the workspace directory,
  session id, context-window use and rate-limit usage with reset times. The script prints
  one line. Anything you want to show has to be a field in that JSON.
- The left side is built from segments (`add_left`), the right from items (`add_right`).
  Each right item has a name so the least important can be dropped when the terminal is
  narrow: context first, then 7-day use, then the Remote Control marker.
- Colour is for signal only. Values stay plain until they cross a threshold, then turn
  yellow or red (`level_fg`).
- The pace warning keeps one sample per session in `/tmp` and extrapolates linearly to the
  window reset. It is purely local; no API call.
- It leaves a few columns of margin on the right, because Claude Code truncates a line
  that reaches the pane edge.

## Known limits

- The permission mode is not in the JSON, so it cannot be shown here.
- The Remote Control marker reads an undocumented field from `~/.claude/sessions/*.json`.
  Check it again after a Claude Code upgrade.

## Design rule worth keeping

Colour and weight on the line you are looking at; the rest stays quiet. Powerline shapes
and background colours belong on the focused line only. A status line that competes with
the prompt gets ignored.
