#!/usr/bin/env bash
# Claude Code status line (Catppuccin Mocha). Left: the repo name and branch (the
# open project) then the directory (where we are in it), in plain coloured text
# with a thin arrow between them, since this is the line that draws focus. Right, aligned to the
# terminal width: RC marker, CTX, 5h usage (reset countdown as H:MM in
# parentheses) and 7d usage.
# Labels are dim, values brighter, and a value turns yellow or red only when it
# matters. Model and effort are left out for now (still in the JSON as
# .model.display_name and .effort.level). Claude Code draws its own
# permission-mode line underneath; the mode is not in the status line JSON, so it
# cannot be moved in here.
input=$(cat)
jqr() { printf '%s' "$input" | jq -r "$1 // empty"; }
five=$(jqr '.rate_limits.five_hour.used_percentage')
five_reset=$(jqr '.rate_limits.five_hour.resets_at')
week=$(jqr '.rate_limits.seven_day.used_percentage')
ctx=$(jqr '.context_window.used_percentage')
session_id=$(jqr '.session_id')
session_id=${session_id:-nosession}
cwd=$(jqr '.workspace.current_dir')
cwd=${cwd:-$PWD}

now_epoch=$(date +%s)

# ---------- palette (r;g;b) ----------
DIVIDER='108;112;134'
SUB='147;153;178'; TEXT='205;214;244'
MAUVE='203;166;247'; YELLOW='249;226;175'; RED='243;139;168'
THIN_R=$'\xee\x82\xb1'    # U+E0B1
RST=$'\033[0m'
c() { printf '\033[38;2;%sm%s' "$1" "$2"; }  # coloured text, no reset

# Left segments: "fg|bold|text". Right items: preformatted text with inline
# colours, joined by two spaces. right_names tracks each right item so
# low-priority ones can be dropped when the terminal is narrow.
left=(); right=(); right_names=()
add_left()  { left+=("$1|$2|$3"); }
add_right() { right_names+=("$1"); right+=("$2"); }
drop_right() {
  local i
  for i in "${!right_names[@]}"; do
    if [ "${right_names[$i]}" = "$1" ]; then
      unset "right_names[$i]" "right[$i]"
      right_names=("${right_names[@]}"); right=("${right[@]}")
      return 0
    fi
  done
  return 1
}

strip() { sed -E 's/\x1b\[[0-9;]*m//g'; }
width() { printf '%s' "$1" | strip | LC_ALL=C.UTF-8 wc -m; }
bold_on() { if [ "$1" = 1 ]; then printf '\033[1m'; fi; }

# Left side: coloured text, a thin dim arrow between items.
render_left() {
  local n=${#left[@]} i out="" fg bold text
  for ((i = 0; i < n; i++)); do
    IFS='|' read -r fg bold text <<<"${left[$i]}"
    if [ "$i" -gt 0 ]; then out+=$(printf '\033[38;2;%sm %s ' "$DIVIDER" "$THIN_R"); fi
    out+=$(printf '\033[38;2;%sm%s%s\033[22m' "$fg" "$(bold_on "$bold")" "$text")
  done
  printf '%s%s' "$out" "$RST"
}

# Right side: items joined by two spaces, no trailing space.
render_right() {
  local n=${#right[@]} i out="" sep
  sep='  '
  for ((i = 0; i < n; i++)); do
    if [ "$i" -gt 0 ]; then out+="$sep"; fi
    out+="${right[$i]}"
  done
  printf '%s%s' "$out" "$RST"
}

level_fg() { # $1 = pct, $2 = yellow threshold, $3 = red threshold; plain text below yellow
  if [ "$1" -ge "$3" ]; then printf '%s' "$RED"
  elif [ "$1" -ge "$2" ]; then printf '%s' "$YELLOW"
  else printf '%s' "$TEXT"; fi
}

# ---------- left: git ----------
if git -C "$cwd" --no-optional-locks rev-parse --git-dir >/dev/null 2>&1; then
  branch=$(git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)
  [ -n "$branch" ] || branch=$(git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  staged=$(git -C "$cwd" --no-optional-locks diff --cached --numstat 2>/dev/null | wc -l)
  modified=$(git -C "$cwd" --no-optional-locks diff --numstat 2>/dev/null | wc -l)
  untracked=$(git -C "$cwd" --no-optional-locks ls-files --others --exclude-standard 2>/dev/null | wc -l)
  repo=$(basename "$(git -C "$cwd" --no-optional-locks rev-parse --show-toplevel 2>/dev/null)")
  state="$branch"
  if [ "$staged" -gt 0 ]; then state+=" +$staged"; fi
  if [ "$modified" -gt 0 ]; then state+=" ~$modified"; fi
  if [ "$untracked" -gt 0 ]; then state+=" ?$untracked"; fi
  add_left "$TEXT" 0 "$(c "$MAUVE" $'\033[1m'"$repo"$'\033[22m')$(c "$DIVIDER" ': ')$(c "$TEXT" "$state")"
  in_repo=1
fi

# ---------- left: directory ----------
dir=${cwd/#$HOME/\~}
IFS=/ read -ra parts <<<"$dir"
if [ "${#parts[@]}" -gt 3 ]; then
  dir="…/${parts[-2]}/${parts[-1]}"
fi
if [ -n "${in_repo:-}" ]; then add_left "$TEXT" 0 "$dir"; else add_left "$MAUVE" 1 "$dir"; fi

# ---------- right: Remote Control marker ----------
# The status line JSON has no Remote Control field, but Claude Code records
# a bridgeSessionId in ~/.claude/sessions/<pid>.json while RC is connected
# (undocumented; re-check if the marker stops appearing after an upgrade).
for f in "$HOME"/.claude/sessions/*.json; do
  [ -e "$f" ] || continue
  if jq -e --arg id "$session_id" '.sessionId == $id and (.bridgeSessionId // "") != ""' "$f" >/dev/null 2>&1; then
    add_right rc "$(c "$MAUVE" $'\033[1m\xe2\x97\x8fRC\033[22m')"
    break
  fi
done

# ---------- right: context window ----------
if [ -n "$ctx" ]; then
  ctx_pct=$(printf '%.0f' "$ctx")
  add_right ctx "$(c "$SUB" 'CTX:')$(c "$(level_fg "$ctx_pct" 60 80)" "${ctx_pct}%")"
fi

# ---------- right: rate-limit usage ----------
if [ -n "$five" ]; then
  five_pct=$(printf '%.0f' "$five")
  reset_txt=""
  if [ -n "$five_reset" ]; then
    left_s=$((five_reset - now_epoch))
    if [ "$left_s" -lt 0 ]; then left_s=0; fi
    if [ "$left_s" -ge 3600 ]; then
      reset_txt=$(printf '\xe2\x86\xbb%d:%02d' $((left_s / 3600)) $(((left_s % 3600) / 60)))
    else
      reset_txt=$(printf '\xe2\x86\xbb0:%02d' $((left_s / 60)))
    fi
  fi
  five_txt="$(c "$SUB" '5h:')$(c "$(level_fg "$five_pct" 50 80)" "${five_pct}%")"
  if [ -n "$reset_txt" ]; then five_txt+="$(c "$SUB" " ($reset_txt)")"; fi
  add_right five "$five_txt"
fi
if [ -n "$week" ]; then
  week_pct=$(printf '%.0f' "$week")
  add_right week "$(c "$SUB" '7d:')$(c "$(level_fg "$week_pct" 50 80)" "${week_pct}%")"
fi

# ---------- right: pace projection, purely local ----------
# Sample used_percentage + timestamp to a per-session cache file (no network or
# API call), compare to the previous sample, and extrapolate linearly to the
# reset time. Only warns if that projection would cross 100% before the window
# resets.
if [ -n "$five" ]; then
  remain=""
  if [ -n "$five_reset" ]; then remain=$((five_reset - now_epoch)); fi
  pace_file="/tmp/statusline-pace-$session_id"
  if [ -f "$pace_file" ] && [ -n "$remain" ]; then
    IFS='|' read -r prev_ts prev_pct < "$pace_file"
    dt=$((now_epoch - prev_ts))
    if [ -n "$prev_ts" ] && [ "$dt" -ge 60 ]; then
      proj_i=$(awk -v cur="$five" -v prev="$prev_pct" -v dt="$dt" -v rem="$remain" \
        'BEGIN { rate = (cur - prev) / dt; p = cur + rate * rem; if (p < 0) p = 0; printf "%.0f", p }')
      if [ "$proj_i" -gt 100 ]; then
        add_right warn "$(c "$RED" "$(printf '\033[1m\xe2\x9a\xa0 on pace %d%%\033[22m' "$proj_i")")"
      fi
    fi
  fi
  printf '%s|%s\n' "$now_epoch" "$five" > "$pace_file"
fi

# ---------- compose: left, padding, right ----------
# Claude Code truncates a line that reaches the pane edge (it cut a line of
# COLUMNS-2 columns), so keep a small margin and drop the least important
# right-hand items first if the line would not fit.
cols=${COLUMNS:-0}
margin=4
l=$(render_left)
while :; do
  r=$(render_right)
  pad=$((cols - $(width "$l") - $(width "$r") - margin))
  if [ "$cols" -le 0 ] || [ "$pad" -ge 1 ]; then break; fi
  drop_right ctx || drop_right week || drop_right rc || break
done
if [ "$cols" -gt 0 ] && [ "$pad" -ge 1 ]; then
  printf '%s%*s%s\n' "$l" "$pad" "" "$r"
else
  printf '%s %s\n' "$l" "$r"
fi
