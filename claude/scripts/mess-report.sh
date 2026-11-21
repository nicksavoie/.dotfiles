#!/usr/bin/env bash
# Read-only mess report: new notes, shrunk notes, lost wikilinks, Review List entries.
# Usage: mess-report.sh [--last]   (default: working tree vs HEAD; --last: HEAD~1..HEAD)
# Thresholds: shrink >= 20% and >= 150 words; 5 new notes per task.
set -u
SHRINK_PCT=20; SHRINK_WORDS=150; NEW_BUDGET=5
cd "$(git rev-parse --show-toplevel)" || exit 1

if [ "${1:-}" = "--last" ]; then
  BASE=HEAD~1; AFTER=HEAD
else
  BASE=HEAD; AFTER=""
fi

before() { git show "$BASE:$1" 2>/dev/null; }
after()  { if [ -n "$AFTER" ]; then git show "$AFTER:$1" 2>/dev/null; else cat -- "$1" 2>/dev/null; fi; }
links()  { grep -o '\[\[[^]|#]*' | sed 's/^\[\[//' | sort -u; }

if [ -n "$AFTER" ]; then
  STATUS=$(git diff --name-status -M "$BASE" "$AFTER" -- '*.md')
else
  STATUS=$( { git diff --name-status -M "$BASE" -- '*.md'; git ls-files --others --exclude-standard -- '*.md' | sed 's/^/A\t/'; } )
fi

new=(); shrunk=(); lost=()
while IFS=$'\t' read -r st a b; do
  [ -z "${st:-}" ] && continue
  case "$st" in
    A) new+=("$a") ;;
    M|R*)
      old="$a"; cur="${b:-$a}"
      bw=$(before "$old" | wc -w); aw=$(after "$cur" | wc -w)
      drop=$((bw - aw))
      if [ "$bw" -gt 0 ] && [ "$drop" -ge "$SHRINK_WORDS" ] && [ $((drop * 100 / bw)) -ge "$SHRINK_PCT" ]; then
        shrunk+=("$cur: $bw -> $aw words (-$((drop * 100 / bw))%)")
      fi
      miss=$(comm -23 <(before "$old" | links) <(after "$cur" | links))
      if [ -n "$miss" ]; then
        n=$(printf '%s\n' "$miss" | wc -l)
        first=$(printf '%s\n' "$miss" | head -3 | paste -sd ';' -)
        lost+=("$cur: $n lost ($first)")
      fi ;;
  esac
done <<< "$STATUS"

echo "## Mess report ($([ -n "$AFTER" ] && echo "HEAD~1..HEAD" || echo "working tree vs HEAD"))"
echo
echo "New notes: ${#new[@]} (budget $NEW_BUDGET)"
if [ "${#new[@]}" -gt 0 ]; then
  for f in "${new[@]}"; do
    case "$f" in "+ Inbox/"*) d="+ Inbox";; "3 Resources/Media/"*) d="Media";; *) d="other";; esac
    echo "- [$d] $f"
  done
  [ "${#new[@]}" -gt "$NEW_BUDGET" ] && echo "OVER BUDGET: skip if this was a media-import or research-compile run (they report their own counts)."
fi
echo
echo "Shrunk notes: ${#shrunk[@]}"
for s in "${shrunk[@]+"${shrunk[@]}"}"; do echo "- $s"; done
echo
echo "Lost links: ${#lost[@]}"
for l in "${lost[@]+"${lost[@]}"}"; do echo "- $l"; done
echo
RL="+ Inbox/Review List.md"
if [ -n "$AFTER" ]; then
  added=$(git diff "$BASE" "$AFTER" -- "$RL" | grep -c '^+- \[ \]')
else
  added=$(git diff "$BASE" -- "$RL" | grep -c '^+- \[ \]')
fi
long=$(grep '^- \[ \]' "$RL" 2>/dev/null | awk 'length($0) > 400' | wc -l)
echo "Review List: $added entries added; $long open entries over 400 characters"
echo
echo "New notes: ${#new[@]}. Shrunk: ${#shrunk[@]}. Lost links: ${#lost[@]}."
