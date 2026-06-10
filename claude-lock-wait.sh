#!/usr/bin/env bash
# PreToolUse hook: ask the running Emacs (over emacsclient) whether the tool's
# target file is locked by an unsaved buffer, and wait up to CLAUDE_LOCK_TIMEOUT
# for it to clear. Exit 0 = allow, exit 2 = blocked (reason on stderr -> Claude).
#
# All parsing/matching lives in claude-lock.el; this is just the sleep timer,
# because a blocking wait *inside* Emacs would freeze the UI. Fail-open (exit 0)
# whenever Emacs is unreachable -- if Emacs isn't running, nothing is locked.
# Symlinked into ~/.claude/hooks via M-x claude-lock-install-hook.

TIMEOUT="${CLAUDE_LOCK_TIMEOUT:-120}"
POLL="${CLAUDE_LOCK_POLL:-2}"
EMACSCLIENT="${CLAUDE_LOCK_EMACSCLIENT:-emacsclient}"
SOCKET=()
[ -n "${CLAUDE_LOCK_EMACS_SERVER:-}" ] && SOCKET=(-s "$CLAUDE_LOCK_EMACS_SERVER")

command -v "$EMACSCLIENT" >/dev/null 2>&1 || exit 0

tmp="$(mktemp "${TMPDIR:-/tmp}/claude-lock.XXXXXX")" || exit 0
trap 'rm -f "$tmp"' EXIT
cat > "$tmp"

eval_el() { "$EMACSCLIENT" "${SOCKET[@]}" -e "$1" 2>/dev/null; }

# Anything but a clear "locked" -> allow (covers released, no-target, server
# down, or claude-lock.el not loaded).
waited=0
while [[ "$(eval_el "(claude-lock-hook-status \"$tmp\")")" == *locked* ]]; do
  if [ "$waited" -ge "$TIMEOUT" ]; then
    target="$(eval_el "(claude-lock-hook-target \"$tmp\")" | sed 's/^"//; s/"$//')"
    echo "🔒 BLOCKED: '${target:-the target file}' is locked by an unsaved Emacs buffer." >&2
    echo "Waited ${TIMEOUT}s and it is still locked -- the user has unsaved changes there and hasn't saved or reverted." >&2
    echo "Stop here. Tell the user to save (or revert) that buffer before you retry; do not edit around the lock." >&2
    exit 2
  fi
  sleep "$POLL"
  waited=$((waited + POLL))
done

exit 0
