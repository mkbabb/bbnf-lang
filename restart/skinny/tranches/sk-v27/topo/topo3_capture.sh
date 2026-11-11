#!/usr/bin/env bash
# LW1.TOPO-3: read-only capture of a dirty tree (ruling X-6).
# usage: topo3_capture.sh <name> <worktree> <custody-dir>
# Writes <custody>/<name>/: HEAD, status.z, staged.patch, unstaged.patch, dirty.patch,
# untracked.tar.gz (non-ignored untracked files; symlinks kept as links; nested
# repos recorded, not entered), untracked.sha256, nested.tsv, symlinks.tsv, big.tsv,
# and copies of every untracked file over 5 MiB in <custody>/big/<sha256>.
# Nothing in the worktree, its index or its refs is written.
set -euo pipefail
name=$1; wt=$2; C=$3/$name; BIG=$3/big
mkdir -p "$C" "$BIG"
cd "$wt"
git rev-parse HEAD > "$C/HEAD"
git branch --show-current >> "$C/HEAD" || true
git status --porcelain=v1 -z --untracked-files=all > "$C/status.z"
git diff --binary --cached > "$C/staged.patch"
git diff --binary > "$C/unstaged.patch"
git diff --binary HEAD > "$C/dirty.patch"
: > "$C/nested.tsv"; : > "$C/symlinks.tsv"; : > "$C/big.tsv"; : > "$C/files.z"
while IFS= read -r -d '' p; do
  if [[ $p == */ ]]; then   # a nested repository: record its head, never enter it
    printf '%s\t%s\n' "${p%/}" "$(git -C "$p" rev-parse HEAD 2>/dev/null || echo none)" >> "$C/nested.tsv"; continue
  fi
  printf '%s\0' "$p" >> "$C/files.z"
  if [[ -L $p ]]; then printf '%s\t%s\n' "$p" "$(readlink "$p")" >> "$C/symlinks.tsv"; continue; fi
  sz=$(stat -f %z "$p")
  if (( sz > 5242880 )); then
    h=$(shasum -a 256 "$p" | cut -d' ' -f1)
    [[ -f $BIG/$h ]] || cp "$p" "$BIG/$h"
    printf '%s\t%s\t%s\n' "$p" "$h" "$sz" >> "$C/big.tsv"
  fi
done < <(git ls-files --others --exclude-standard -z | sort -z)   # a nested repo is listed once, as "dir/"
tar --null -T "$C/files.z" -czf "$C/untracked.tar.gz"
: > "$C/untracked.sha256"
[[ -s $C/files.z ]] && \
  tr '\0' '\n' < "$C/files.z" | while IFS= read -r p; do [[ -L $p ]] || printf '%s\0' "$p"; done \
  | xargs -0 shasum -a 256 > "$C/untracked.sha256"
echo "$name: $(tr -cd '\0' < "$C/files.z" | wc -c | tr -d ' ') untracked, $(wc -l < "$C/nested.tsv" | tr -d ' ') nested, $(wc -l < "$C/big.tsv" | tr -d ' ') big, dirty.patch $(wc -c < "$C/dirty.patch" | tr -d ' ') B"
