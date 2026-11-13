#!/usr/bin/env bash
# SessionEnd safety net for the vault (Project AGI, slice 2).
# If the session ends (including a bare /clear) with uncommitted work in the vault,
# commit it locally. Shell only, no model call. Never touches a remote. Always exits 0,
# so a failure here can never block /clear or exit. It does not update the Session
# Bridge (that needs the model); the next session's recap can read the git log.
#
# Evidence log: ~/.cache/vault-auto-commit.log (outside the vault, so logging never
# dirties the tree). Every invocation writes one line, which is how the assumption
# "SessionEnd fires on /clear" gets checked.

trap 'exit 0' EXIT

LOG="${XDG_CACHE_HOME:-$HOME/.cache}/vault-auto-commit.log"
mkdir -p "$(dirname "$LOG")" 2>/dev/null
log() { printf '%s %s\n' "$(date -Is)" "$*" >> "$LOG" 2>/dev/null; }

input=$(cat 2>/dev/null)
cwd=$(printf '%s' "$input" | jq -r '.cwd // empty' 2>/dev/null)
reason=$(printf '%s' "$input" | jq -r '.reason // empty' 2>/dev/null)
[ -n "$cwd" ] || cwd="$CLAUDE_PROJECT_DIR"
[ -n "$reason" ] || reason="unknown"
# Never fall back to the current directory: with no cwd supplied, do nothing.
[ -n "$cwd" ] || { log "skip: no cwd in input (reason=$reason)"; exit 0; }

top=$(git -C "$cwd" rev-parse --show-toplevel 2>/dev/null) || { log "skip: not a git repo: $cwd (reason=$reason)"; exit 0; }

# Only the vault: it is marked by the Session Bridge note (portable across machines).
[ -f "$top/Meta/AI Context/Session Bridge.md" ] || { log "skip: not the vault: $top (reason=$reason)"; exit 0; }

# Leave a merge, rebase, cherry-pick or revert in progress alone.
gitdir=$(git -C "$top" rev-parse --absolute-git-dir 2>/dev/null)
for marker in MERGE_HEAD CHERRY_PICK_HEAD REVERT_HEAD rebase-merge rebase-apply index.lock; do
  if [ -e "$gitdir/$marker" ]; then
    log "skip: git operation in progress ($marker) (reason=$reason)"
    exit 0
  fi
done

if [ -z "$(git -C "$top" status --porcelain 2>/dev/null)" ]; then
  log "clean: nothing to commit (reason=$reason)"
  exit 0
fi

if git -C "$top" add -A >/dev/null 2>&1 \
  && git -C "$top" commit -q \
    -m "Auto-commit at session end ($reason)" \
    -m "Made by .claude/hooks/auto-commit.sh, not by a checkpoint; see Project AGI Slice 2 Auto Checkpoint." \
    >/dev/null 2>&1; then
  log "committed $(git -C "$top" rev-parse --short HEAD 2>/dev/null) (reason=$reason)"
else
  log "commit failed (reason=$reason)"
fi
exit 0
