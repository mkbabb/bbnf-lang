#!/usr/bin/env bash
# Install bbnf-lang's git pre-commit hook into the repo's hooks directory.
#
# Hooks are not versioned by git; this script copies the in-tree template at
# scripts/hooks/pre-commit into .git/hooks/pre-commit and marks it executable.
# Re-run after a fresh clone or when the template updates.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
hooks_dir="$(git rev-parse --git-path hooks)"
src="${repo_root}/scripts/hooks/pre-commit"
dst="${hooks_dir}/pre-commit"

if [ ! -f "${src}" ]; then
    echo "error: source hook not found at ${src}" >&2
    exit 1
fi

mkdir -p "${hooks_dir}"
install -m 0755 "${src}" "${dst}"
echo "installed pre-commit hook → ${dst}"
