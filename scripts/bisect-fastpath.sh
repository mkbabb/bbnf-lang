#!/usr/bin/env bash
# Bisect a regression with per-step log capture.
#
# Wraps `git bisect run` so each step's stdout+stderr is preserved at
# /tmp/bisect-<short-hash>.log and the offending commit can be re-inspected
# without re-running the bisect command.
#
# Usage:
#     scripts/bisect-fastpath.sh <good-commit> <bad-commit> [command]
#
# Example:
#     scripts/bisect-fastpath.sh v0.4.0 HEAD "cargo check -p gorgeous --lib"
#
# The default command is `cargo check -p gorgeous --lib`. Any non-zero exit
# from the command is treated as "bad", any zero exit as "good", and exit
# code 125 (cargo's "build failed before tests" sentinel) is honoured by
# `git bisect run` as "skip".

set -euo pipefail

# ── Argument parsing ────────────────────────────────────────────────────────

DEFAULT_CMD="cargo check -p gorgeous --lib"

print_help() {
    cat <<'EOF'
bisect-fastpath.sh — wrap `git bisect run` with per-step log capture.

Usage:
    scripts/bisect-fastpath.sh <good-commit> <bad-commit> [command]
    scripts/bisect-fastpath.sh --help

Arguments:
    good-commit   Revision known to be good (e.g. v0.4.0, abcd123).
    bad-commit    Revision known to be bad (e.g. HEAD).
    command       Optional shell command to run at each step.
                  Defaults to: cargo check -p gorgeous --lib

Behaviour:
    1. Aborts if the working tree is dirty (uncommitted changes).
    2. Aborts if a bisect is already in progress — run `git bisect reset`
       and retry.
    3. Runs `git bisect start <bad> <good>` and `git bisect run bash -c …`.
    4. Each step's combined stdout+stderr is written to
       /tmp/bisect-<short-hash>.log so any commit can be re-inspected
       after the bisect finishes.
    5. On completion, prints the offending commit and the path to its
       log file.

Examples:
    # Bisect a JSON FastPath panic, default command.
    scripts/bisect-fastpath.sh v0.4.0 HEAD

    # Bisect against a custom test target.
    scripts/bisect-fastpath.sh good-tag HEAD "cargo test -p bbnf-ir --lib"

EOF
}

if [[ $# -ge 1 && ( "$1" == "--help" || "$1" == "-h" ) ]]; then
    print_help
    exit 0
fi

if [[ $# -lt 2 ]]; then
    echo "error: <good-commit> and <bad-commit> are required" >&2
    echo "       run 'scripts/bisect-fastpath.sh --help' for usage" >&2
    exit 2
fi

GOOD_REV="$1"
BAD_REV="$2"
shift 2
COMMAND="${1:-$DEFAULT_CMD}"

# ── Pre-flight checks ───────────────────────────────────────────────────────

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "error: not inside a git work tree" >&2
    exit 2
fi

if [[ -d .git/BISECT_LOG || -f .git/BISECT_START ]]; then
    echo "error: a bisect is already in progress" >&2
    echo "       run 'git bisect reset' first, then retry" >&2
    exit 2
fi

if ! git diff --quiet || ! git diff --cached --quiet; then
    echo "error: working tree is dirty — commit or stash changes first" >&2
    git status --short >&2
    exit 2
fi

if ! git rev-parse --verify "$GOOD_REV^{commit}" >/dev/null 2>&1; then
    echo "error: good revision '$GOOD_REV' does not resolve to a commit" >&2
    exit 2
fi

if ! git rev-parse --verify "$BAD_REV^{commit}" >/dev/null 2>&1; then
    echo "error: bad revision '$BAD_REV' does not resolve to a commit" >&2
    exit 2
fi

# ── Bisect ──────────────────────────────────────────────────────────────────

# The runner is a small wrapper that captures combined output to a per-commit
# log file before propagating the underlying exit code back to git bisect.
RUNNER="$(mktemp -t bisect-runner.XXXXXX)"
trap 'rm -f "$RUNNER"' EXIT

cat >"$RUNNER" <<RUNNER_EOF
#!/usr/bin/env bash
set -uo pipefail
short=\$(git rev-parse --short HEAD)
log="/tmp/bisect-\${short}.log"
echo "── bisect step \${short} ──" >"\$log"
echo "command: ${COMMAND}" >>"\$log"
echo >>"\$log"
( ${COMMAND} ) >>"\$log" 2>&1
status=\$?
echo >>"\$log"
echo "exit: \${status}" >>"\$log"
exit \$status
RUNNER_EOF
chmod +x "$RUNNER"

echo "── bisect-fastpath ──"
echo "good:    $GOOD_REV"
echo "bad:     $BAD_REV"
echo "command: $COMMAND"
echo "logs:    /tmp/bisect-<short-hash>.log"
echo

git bisect start "$BAD_REV" "$GOOD_REV"

set +e
git bisect run "$RUNNER"
bisect_status=$?
set -e

# `git bisect run` writes the offending commit to BISECT_HEAD or prints it on
# the final line. We re-derive it from the bisect log so the report is stable.
offender=$(git rev-parse --short HEAD)
log_file="/tmp/bisect-${offender}.log"

echo
echo "── result ──"
if [[ $bisect_status -eq 0 ]]; then
    echo "first bad commit: $(git rev-parse HEAD)"
    echo "                  $(git log -1 --format='%s' HEAD)"
    if [[ -f "$log_file" ]]; then
        echo "log file:         $log_file"
    fi
else
    echo "bisect run exited non-zero ($bisect_status) — partial result above"
fi

git bisect reset
