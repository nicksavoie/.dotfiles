#!/usr/bin/env bash
# Create a Git + Obsidian + Claude vault from this starter.
# Usage: bash bootstrap.sh [target-dir]    (default: ~/Documents/Vault)
#
# Refuses to run if the target already has files in it. Creates the PARA folders, copies
# the starter files, and runs `git init` (local only, no remote) with a first commit.
set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
target=${1:-$HOME/Documents/Vault}

if [ -d "$target" ] && [ -n "$(ls -A "$target")" ]; then
  echo "Refusing to continue: $target is not empty." >&2
  exit 1
fi

mkdir -p "$target"
for dir in "0 Journals" "1 Projects" "2 Areas" "3 Resources" "4 Archive" \
           "Meta/AI Context" "Meta/Templates" ".claude/skills"; do
  mkdir -p "$target/$dir"
done

# Copy the starter files (everything except this script), keeping dotfiles.
(cd "$here" && find . -type f ! -name bootstrap.sh -print0) |
  while IFS= read -r -d '' file; do
    mkdir -p "$target/$(dirname "$file")"
    cp "$here/$file" "$target/$file"
  done

# Git keeps no empty folders; leave a marker so the layout survives a clone.
for dir in "0 Journals" "1 Projects" "3 Resources" "4 Archive" ".claude/skills"; do
  touch "$target/$dir/.gitkeep"
done

cd "$target"
git init -q
git add -A
git commit -q -m "Initial vault from starter"

cat <<MSG
Vault created at $target (local git repository, no remote).

Next:
  1. Open the folder as a vault in Obsidian.
  2. Fill in CLAUDE.md (the <ask: ...> placeholders) and Meta/AI Context/User Profile.md.
  3. Start 'claude' in the vault and have it build the skills from claude/skills/.
MSG
