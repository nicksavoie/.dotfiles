# Engineering Workflow Configuration (.dotfiles)

A personal dotfiles repository used to explore tooling, aesthetics, and interaction models, with some components being production-useful and others intentionally experimental.

1. Watch someone on YouTube with a perfect tiling WM
2. Discover lsd, bat, fastfetch, some cursed terminal file manager
3. Install everything
4. Feel powerful
5. Six weeks later: “Why is any of this here”
6. ...?
7. Profit off the actually good tools that stuck around

## Scope

My main machines run NixOS and are configured declaratively from a separate flake, so this repository no longer carries a shell, terminal, or Emacs config. It is the portable subset: the setup I bring to a machine I do not control, currently a Windows 11 work laptop with VSCode and WSL.

Earlier versions of this repo held zsh, kitty, LunarVim, ranger and a Doom Emacs config. They were retired when the flake replaced them; the history is still in the git log.

## Contents

| Path | What it is | Install |
|---|---|---|
| `vscode/` | `settings.json` and an extension list: Catppuccin Mocha, JetBrains Mono, Vim keys with a Space leader, Canadian English spellcheck | `.\vscode\install.ps1` in PowerShell |
| `tmux/` | A tmux config for WSL that mirrors my NixOS one: same keys, theme and session persistence, plus a project sessionizer and a keybind cheatsheet | `stow tmux` from WSL, then `prefix + I` |
| `claude/` | How I configure Claude Code, and recipes for the skills I rebuild first | Read, then adapt |

### tmux notes

- The prefix is the default `C-b`. Splits are `|` and `-`, `prefix + f` opens the sessionizer, and `prefix + ?` shows the cheatsheet.
- The sessionizer needs `fzf`. Set `SESSIONIZER_ROOTS` (colon-separated) to change where it looks; the default is `~/code` and `~/projects`.
- Copying uses OSC 52, so it reaches the Windows clipboard through Windows Terminal. Paste with `Ctrl+Shift+V`.
- The NixOS file is the source. If the two disagree, port the NixOS change here.

## Design Goals

- **Reproducibility:** Minimize configuration drift across systems and reinstalls.
- **Low-Friction Onboarding:** Bootstrap a usable environment with minimal manual steps.
- **Systems Thinking:** Treat the development environment as an engineered system.
- **Conservative Defaults:** Prefer settings that fail safe and are easy to audit.
- **Comfortable Aesthetics:** A low-glare palette and readable type, tuned for long focus sessions.
- **Systemic Flexibility:** Able to bring my tastes and tools with me into other environments (Eg. VSCode on Windows)
