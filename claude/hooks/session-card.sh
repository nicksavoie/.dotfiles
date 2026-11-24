#!/usr/bin/env bash
# SessionStart card hook. Read-only on the project; writes only under ~/.cache/. Always exits 0.
# Emits a short "where you are" card as context for the first reply, with one rotating tip.
# Needs bash, git, jq. With no jq it emits nothing.
# Test knobs: CARD_HOUR=<0-23> overrides the hour; CARD_DRY=1 does not advance tip state.

# Paths relative to the project root; change these to match your layout.
BRIDGE="Session Bridge.md"        # a markdown table under "## Threads": | Thread | Last touched | Next | ... |
LOOPS="open-loops.md"             # unchecked lines "- [ ] text (due YYYY-MM-DD)"
TIPS="Tips.md"                    # see tips-starter.md

C="$HOME/.cache"
mkdir -p "$C" 2>/dev/null
in=$(cat 2>/dev/null)
command -v jq >/dev/null 2>&1 || exit 0
src=$(printf '%s' "$in" | jq -r '.source // "unknown"' 2>/dev/null)
[ -n "${CARD_SRC:-}" ] && src="$CARD_SRC"
printf '%s source=%s\n' "$(date -Is)" "$src" >> "$C/session-card.log" 2>/dev/null
case "$src" in startup|clear) ;; *) exit 0;; esac

cd "${CLAUDE_PROJECT_DIR:-$PWD}" 2>/dev/null || exit 0
hour=$(( 10#${CARD_HOUR:-$(date +%H)} ))
today=$(date +%F)
tomorrow=$(date -d tomorrow +%F 2>/dev/null)

if [ "$hour" -lt 12 ]; then band=morning
elif [ "$hour" -ge 17 ]; then band=evening
else band=afternoon; fi

last=$(git --no-optional-locks log -1 --format='%h %cr' 2>/dev/null)
dirty=$(git --no-optional-locks status --porcelain 2>/dev/null | wc -l | tr -d ' ')

# Top three bridge rows: thread, last touched, Next cut at 100 characters.
threads=$(awk -F'|' '/^## Threads/{t=1;next} t&&/^\| /&&$2!~/Thread|---/{
  n=$4; gsub(/^ +| +$/,"",n); if(length(n)>100)n=substr(n,1,100) "...";
  gsub(/^ +| +$/,"",$2); gsub(/^ +| +$/,"",$3);
  print "- " $2 " (" $3 "): " n; if(++c==3)exit}' "$BRIDGE" 2>/dev/null)

# Due items: unchecked lines marked (due YYYY-MM-DD).
due=""
while IFS= read -r line; do
  d=$(printf '%s' "$line" | grep -o '(due [0-9-]\{10\})' | head -1 | grep -o '[0-9-]\{10\}')
  [ -z "$d" ] && continue
  txt=$(printf '%s' "$line" | sed -e 's/^- \[ \] //' -e 's/ *(due [0-9-]*)//' | cut -c1-80)
  if [ "$d" \< "$today" ]; then due="${due}- overdue (${d}): ${txt}"$'\n'
  elif [ "$d" = "$today" ]; then due="${due}- due today: ${txt}"$'\n'
  elif [ "$d" = "$tomorrow" ]; then due="${due}- due tomorrow: ${txt}"$'\n'; fi
done < <(grep '^- \[ \].*(due [0-9-]\{10\})' "$LOOPS" 2>/dev/null)

# Next tip: first unshown tip above "## Retired"; the cycle restarts when all are shown.
tip=""; state="$C/session-card-tip-state"
if grep -q '^Tips: on' "$TIPS" 2>/dev/null; then
  pool=$(awk '/^## Retired/{exit} /^- (hotkey|skill|workflow) \|/' "$TIPS" 2>/dev/null)
  tip=$(printf '%s\n' "$pool" | grep -vxFf "$state" 2>/dev/null | head -1)
  if [ -z "$tip" ]; then
    : > "$state" 2>/dev/null
    tip=$(printf '%s\n' "$pool" | head -1)
  fi
  [ -n "$tip" ] && [ -z "${CARD_DRY:-}" ] && printf '%s\n' "$tip" >> "$state" 2>/dev/null
  tip=$(printf '%s' "$tip" | awk -F' [|] ' '{print $1 ": " $2}' | sed 's/^- //')
fi

case "$band" in
  morning) shape="At most 3 short lines: what is due or overdue today (or 'Nothing due'), then the tip. No tables." ;;
  afternoon) shape="At most 4 short lines: where I am (the top threads, one line each), what is due, then the tip." ;;
  evening) shape="At most 5 short lines: where I am (top threads), what is due, then the tip." ;;
esac

ctx="SESSION CARD. Facts gathered by the start hook (hour ${hour}, ${band}):
Last commit: ${last:-unknown}; uncommitted files: ${dirty:-?}.
Top threads:
${threads:-none}
Due items:
${due:-none}
Tip: ${tip:-none (tips off)}

Rules: begin your first reply with the card, then answer my message. ${shape} Each line under about 80 characters (shorten thread lines to fit), no tables, plain text. Show exactly one tip, worded as given. Say nothing about the uncommitted count unless it is above 0, and then only as 'N uncommitted files'. Skip the card entirely if my first message is a slash command, goodnight, clearing, wrapping up or a check-in. Never repeat the card later in the session. Never comment on when or how long I use Claude."

jq -n --arg c "$ctx" '{hookSpecificOutput:{hookEventName:"SessionStart",additionalContext:$c}}' 2>/dev/null
exit 0
