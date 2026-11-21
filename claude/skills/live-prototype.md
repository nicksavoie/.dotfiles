# live-prototype

For UI and config changes kept in a declarative system config (a flake, dotfiles): prove the change live with commands, then write it into the config once. It avoids the rebuild-and-look loop, where each wrong guess costs a full rebuild.

## The loop

1. Write the `Done when` as commands with expected output before touching anything, and show them.
2. Prototype through the tool's runtime channel, never in the managed config: a runtime keyword, a scratch config passed by flag, a reload signal. A prototype vanishes on reboot, so a failed try leaves nothing behind.
3. Run the commands and show the output. A failure means iterate on the prototype, not rebuild.
4. Port the smallest diff into the config only when the commands pass. Then run the config's check and a build that does not switch. The person runs the switch.
5. After the switch, re-run the same commands. The person's check covers looks and feel only.

Two rebuild rounds is the target; a third means stop and say what the prototype could not show.

## Probe table

| Tool | Runtime channel | Check |
|---|---|---|
| Hyprland | `hyprctl keyword`, `hyprctl reload` | `hyprctl binds`, `hyprctl getoption`, `hyprctl clients -j` |
| waybar | `waybar -c <scratch.json> -s <scratch.css>` | `waybar --version` |
| kitty | `kitty --config <scratch.conf>` | `kitty --version` |
| tmux | `tmux source-file`, `tmux set -g` | `tmux show-options -g`, `tmux capture-pane -p` |
| status line script | pipe sample JSON to a scratch copy | the printed line |
| fish | `fish -c` or `source` in a child shell | `fish -n <file>` |
| flake | none | `nix flake check`; `nix build --no-link .#nixosConfigurations.<host>.config.system.build.toplevel` |

Add a row when a new tool comes up. The flake host name is the name in the flake, not the machine's hostname.
