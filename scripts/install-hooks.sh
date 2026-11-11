#!/usr/bin/env bash
# Install bbnf-lang's git hooks into the repo's hooks directory.
#
# Hooks are not versioned by git; this copies every in-tree template under
# scripts/hooks/ into .git/hooks/ and marks it executable. Re-run after a fresh
# clone or when a template updates.
#
#   pre-commit  regen drift gate
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
hooks_dir="$(git rev-parse --git-path hooks)"
src_dir="${repo_root}/scripts/hooks"

if [ ! -d "${src_dir}" ]; then
    echo "error: hooks source dir not found at ${src_dir}" >&2
    exit 1
fi

mkdir -p "${hooks_dir}"
for src in "${src_dir}"/*; do
    [ -f "${src}" ] || continue
    name="$(basename "${src}")"
    install -m 0755 "${src}" "${hooks_dir}/${name}"
    echo "installed ${name} hook → ${hooks_dir}/${name}"
done
