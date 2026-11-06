#!/usr/bin/env bash
# scripts/sync-csp-solver-vendor.sh — bbnf-lang side of the csc411 canonical-source policy.
#
# csp-solver's source of truth is the csc411 monorepo (its `csp-solver/` subtree).
# bbnf-lang vendors a full copy at `crates/csp-solver`. This gate keeps the copy
# faithful to a pinned csc411 revision AND proves that the pinned code still
# compiles every bbnf consumer.
#
# Modes
# -----
#   --check            (default)  Byte-diff the vendored src/ (+ data/) against the
#                                 pinned csc411 rev, extracted with `git archive`.
#                                 Fails loud on any drift. Proves "the copy matches
#                                 the pin."  (constraint-trait-bound-spike.md §8 — the
#                                 text-diff provenance gate.)
#
#   --update [<rev>]              Re-vendor src/ + data/ + tests/ from csc411 <rev>
#                                 (default: csc411 HEAD), rewrite the pin in
#                                 Cargo.toml's description, and point the caller at
#                                 --verify. Reproducible: extracts from
#                                 `git archive <rev>`, never from the moving working
#                                 tree (a concurrent csc411 lane may be mid-edit).
#                                 One command; the final release-window re-vendor is
#                                 just `--update <release-rev>`.
#
#   --verify                      The enforced compile stage (spike §8.1) + structural
#                                 pre-build tripwires (§8.2). Compiles
#                                   {root:   bbnf, bbnf-ir, egraph}
#                                 ∪ {skinny: passes}          (a SEPARATE workspace,
#                                                              invisible to every root
#                                                              alias — the only command
#                                                              that catches the B2
#                                                              SolveConfig break)
#                                 ∪ {vendored csp-solver × (default, py)}
#                                                              (both cfg branches — the
#                                                              ThreadSafe fix is
#                                                              cfg-gated)
#                                 then runs the vendored lattice test suite (the
#                                 revise/cascade confluence practical check). Proves
#                                 "the pinned code still builds its consumers."
#
# The pre-push hook (scripts/hooks/pre-push) runs --check AND --verify.
#
# NEVER pushes: csc411's origin is a stale public crate identity; bbnf-lang's origin
# is under a standing never-push order. This script only READS the csc411 sibling
# (git archive / cat-file) and only WRITES inside bbnf-lang's own tree.
set -euo pipefail

BBNF_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VENDOR_DIR="$BBNF_ROOT/crates/csp-solver"
VENDOR_SRC="$VENDOR_DIR/src"
VENDOR_DATA="$VENDOR_DIR/data"
VENDOR_TESTS="$VENDOR_DIR/tests"
CARGO_TOML="$VENDOR_DIR/Cargo.toml"
FIELD_BASELINE="$BBNF_ROOT/scripts/.csp-solver-fields.baseline"
CSC411_ROOT="$(cd "$BBNF_ROOT/../csc411/CSC411_HW2_ProgrammingQuestion" 2>/dev/null && pwd || true)"

die()  { echo "ERROR: $*" >&2; exit 1; }
fail() { echo "FAIL: $*"  >&2; exit 1; }

# Temp-dir bookkeeping: one EXIT trap cleans every scratch dir. (A per-function
# `trap ... RETURN` leaks — it stays registered and re-fires on outer returns,
# tripping `set -u` on the then-out-of-scope var.)
_TMPDIRS=()
_cleanup() { [ "${#_TMPDIRS[@]}" -gt 0 ] && rm -rf "${_TMPDIRS[@]}" || true; }
trap _cleanup EXIT
mktmp() { local d; d="$(mktemp -d)"; _TMPDIRS+=("$d"); printf '%s' "$d"; }

# The pin lives in exactly one place: Cargo.toml's description field (a 40-hex sha).
read_pin() { grep -oE '[0-9a-f]{40}' "$CARGO_TOML" | head -1; }

require_sibling() {
    [ -n "$CSC411_ROOT" ] || die "csc411 sibling not found at ../csc411/CSC411_HW2_ProgrammingQuestion."
}

# Print the sorted field-ident set of a `pub struct <name> { ... }` in a rust file.
struct_fields() {
    awk -v s="$2" '
        $0 ~ "pub struct " s " \\{" { inb=1; next }
        inb && /^\}/ { inb=0 }
        inb {
            line=$0; sub(/\/\/.*/, "", line)          # strip line/doc comments
            if (match(line, /pub[ \t]+[a-z_][a-z0-9_]*[ \t]*:/)) {
                f=substr(line, RSTART, RLENGTH)
                gsub(/pub[ \t]+/, "", f); sub(/[ \t]*:$/, "", f)
                print f
            }
        }' "$1" | sort
}

# Path of the (non-py) source file defining a given struct in the vendored tree.
struct_file() { grep -rlE "pub struct $1 \{" "$VENDOR_SRC" | grep -v '/py/' | head -1; }

# ── structural pre-build tripwires (fast; run before the slow consumer builds) ──
verify_tripwires() {
    echo "── structural tripwires (pre-build) ─────────────────────────────────"

    # (1) Trait-surface allow-list {Debug, ThreadSafe}. The ONLY sanctioned
    #     Send/Sync-conferring supertrait on Constraint/Domain/LatticeDomain is the
    #     cfg-gated `ThreadSafe` marker; a raw `Send`/`Sync` re-added unconditionally
    #     resurrects the B1 break (csc411's RefConstraint is !Send). Fail loud.
    #     (Domain legitimately carries Clone/PartialEq/Debug — non-auto-traits,
    #     irrelevant here — so the grep keys strictly on Send/Sync.)
    if grep -REn 'pub trait (Constraint|Domain|LatticeDomain)[^:{]*:[^{]*\b(Send|Sync)\b' "$VENDOR_SRC"; then
        fail "trait-surface tripwire: raw Send/Sync back on a shared trait — scope it to \`py\` via the ThreadSafe marker (spike §8.2)."
    fi
    echo "OK(trait-surface): no raw Send/Sync on Constraint/Domain/LatticeDomain."

    # (2) SolveConfig/SolveStats field-set delta vs the recorded baseline. A new
    #     public field is the B2 class: every downstream exhaustive literal must
    #     then spread `..Default::default()`. Warns (does not fail) — the human
    #     sweeps the literals and refreshes the baseline.
    local cfg_file stats_file current
    cfg_file="$(struct_file SolveConfig)"
    stats_file="$(struct_file SolveStats)"
    [ -n "$cfg_file" ]   || fail "could not locate SolveConfig in the vendored tree."
    [ -n "$stats_file" ] || fail "could not locate SolveStats in the vendored tree."
    current="$( { echo "SolveConfig:"; struct_fields "$cfg_file" SolveConfig; \
                  echo "SolveStats:";  struct_fields "$stats_file" SolveStats; } )"
    if [ -f "$FIELD_BASELINE" ]; then
        if ! diff <(printf '%s\n' "$current") "$FIELD_BASELINE" >/tmp/csp-field-delta.txt 2>&1; then
            echo "WARN(field-set): SolveConfig/SolveStats fields differ from the recorded baseline:" >&2
            sed 's/^/    /' /tmp/csp-field-delta.txt >&2
            echo "    → every downstream exhaustive literal must spread ..Default::default()" >&2
            echo "      (skinny/crates/passes/src/decision_csp.rs); then refresh" >&2
            echo "      scripts/.csp-solver-fields.baseline to acknowledge." >&2
        else
            echo "OK(field-set): SolveConfig/SolveStats match scripts/.csp-solver-fields.baseline."
        fi
    else
        printf '%s\n' "$current" > "$FIELD_BASELINE"
        echo "NOTE(field-set): wrote initial baseline → scripts/.csp-solver-fields.baseline"
    fi
}

do_check() {
    require_sibling
    local pin; pin="$(read_pin)"
    [ -n "$pin" ] || die "no 40-hex pin found in $CARGO_TOML's description."
    git -C "$CSC411_ROOT" cat-file -e "${pin}:csp-solver/src" 2>/dev/null \
        || die "pin $pin not in csc411 history (csp-solver/src). Fetch the sibling or re-pin via --update."

    local tmp; tmp="$(mktmp)"
    # data/ has existed as a vendored asset only since the include_dir! rewrite; diff
    # it too when the pin carries it.
    local paths=(csp-solver/src)
    git -C "$CSC411_ROOT" cat-file -e "${pin}:csp-solver/data" 2>/dev/null && paths+=(csp-solver/data) || true
    git -C "$CSC411_ROOT" archive "$pin" "${paths[@]}" | tar -x -C "$tmp"

    local rc=0
    if ! diff -rq "$tmp/csp-solver/src" "$VENDOR_SRC" >/tmp/csp-vendor-src.diff 2>&1; then
        echo "FAIL: crates/csp-solver/src drifted from csc411@$pin:" >&2; cat /tmp/csp-vendor-src.diff >&2; rc=1
    fi
    if [ -d "$tmp/csp-solver/data" ]; then
        if ! diff -rq "$tmp/csp-solver/data" "$VENDOR_DATA" >/tmp/csp-vendor-data.diff 2>&1; then
            echo "FAIL: crates/csp-solver/data drifted from csc411@$pin:" >&2; cat /tmp/csp-vendor-data.diff >&2; rc=1
        fi
    fi
    [ "$rc" -eq 0 ] || exit 1
    echo "OK: crates/csp-solver/{src,data} match csc411@$pin byte-for-byte."
}

do_update() {
    require_sibling
    local rev="${1:-HEAD}" sha
    sha="$(git -C "$CSC411_ROOT" rev-parse "$rev^{commit}" 2>/dev/null)" \
        || die "rev '$rev' not found in the csc411 sibling."
    git -C "$CSC411_ROOT" cat-file -e "${sha}:csp-solver/src" 2>/dev/null \
        || die "${sha}:csp-solver/src missing in csc411."

    local tmp; tmp="$(mktmp)"
    local paths=(csp-solver/src csp-solver/tests)
    git -C "$CSC411_ROOT" cat-file -e "${sha}:csp-solver/data" 2>/dev/null && paths+=(csp-solver/data) || true
    git -C "$CSC411_ROOT" archive "$sha" "${paths[@]}" | tar -x -C "$tmp"

    rsync -a --delete "$tmp/csp-solver/src/" "$VENDOR_SRC/"
    if [ -d "$tmp/csp-solver/data" ]; then
        mkdir -p "$VENDOR_DATA"; rsync -a --delete "$tmp/csp-solver/data/" "$VENDOR_DATA/"
    fi
    # tests/: refresh ONLY the files bbnf already curates (rsync --existing) so the
    # csc411-only tests (assignment_proptest, cost_finite, …) that would drag in
    # proptest/hungarian dev-deps are never imported.
    [ -d "$tmp/csp-solver/tests" ] && rsync -a --existing "$tmp/csp-solver/tests/" "$VENDOR_TESTS/" || true

    local oldpin; oldpin="$(read_pin)"
    if [ -n "$oldpin" ] && [ "$oldpin" != "$sha" ]; then
        sed -i.bak "s/$oldpin/$sha/g" "$CARGO_TOML" && rm -f "$CARGO_TOML.bak"
    fi

    echo "Re-vendored crates/csp-solver/{src,data,tests(existing)} from csc411@$sha."
    echo "  pin: ${oldpin:-<none>} -> $sha"
    echo
    echo "Next steps (ENFORCED — the pre-push hook runs both):"
    echo "  1. ./scripts/sync-csp-solver-vendor.sh --verify"
    echo "       root{bbnf,bbnf-ir,egraph} ∪ skinny{passes} ∪ csp-solver×{default,py} + lattice tests"
    echo "  2. If --verify's field-set tripwire warns, sweep every downstream exhaustive"
    echo "     SolveConfig/SolveStats literal to spread ..Default::default()"
    echo "     (skinny/crates/passes/src/decision_csp.rs), then refresh"
    echo "     scripts/.csp-solver-fields.baseline."
    echo "  3. If the manifest's own deps changed csc411-side (new [dependencies]),"
    echo "     mirror them into $CARGO_TOML (the sync gate diffs src/+data/, not Cargo.toml)."
    echo "  4. Commit: chore(csp-solver/canonical-diff): re-vendor src/ from csc411@$sha"
}

# The PyO3 cfg branch is csc411-only and cannot live in bbnf's workspace graph:
# bbnf-core's `jiter` already links pyo3 0.23 (`links = "python"`), csc411's crate
# wants pyo3 0.29, and cargo forbids two `links="python"` packages in one graph.
# So compile that branch against the VENDORED src in a DETACHED single-crate
# workspace (empty `[workspace]` table) at pyo3 0.29 — the faithful realization of
# spike §8.1's "both cfg branches must build; the ThreadSafe fix is cfg-gated."
verify_py_isolated() {
    local tmp; tmp="$(mktmp)"
    cp -R "$VENDOR_SRC" "$tmp/src"
    [ -d "$VENDOR_DATA" ] && cp -R "$VENDOR_DATA" "$tmp/data" || true
    cat > "$tmp/Cargo.toml" <<'EOF'
[package]
name = "csp-solver"
version = "0.1.0"
edition = "2024"

# Empty table → detaches this throwaway crate from bbnf's workspace so pyo3 0.29
# never meets bbnf-core's jiter→pyo3 0.23 (links="python") conflict.
[workspace]

[features]
default = []
py = ["dep:pyo3"]

[dependencies]
include_dir = "0.7.4"
# Promoted csc411-side from a dev-dependency to a real lib dependency at the
# 0.3.0 substrate excision (Kuhn-Munkres LAP call now in builder/assignment.rs);
# the isolated py crate compiles the same vendored src/, so it needs it too.
hungarian = "1"
pyo3 = { version = "0.29", features = ["extension-module"], optional = true }

[lib]
crate-type = ["lib"]
EOF
    # Forward-compat flag: pyo3 0.29's build script hard-rejects a Python newer
    # than its max supported; the local interpreter may be newer. This is
    # the exact env the spike used to compile the py branch (§6).
    ( cd "$tmp" && PYO3_USE_ABI3_FORWARD_COMPATIBILITY=1 cargo check --features py )
}

do_verify() {
    verify_tripwires
    echo "── compile stage ────────────────────────────────────────────────────"
    echo "[1/4] root workspace: bbnf + bbnf-ir + egraph (default features; py OFF)"
    ( cd "$BBNF_ROOT" && cargo check -p bbnf -p bbnf-ir -p egraph )
    echo "[2/4] skinny workspace: passes (separate workspace — the only B2 catcher)"
    ( cd "$BBNF_ROOT/skinny" && cargo check -p passes )
    echo "[3/4] vendored csp-solver: default features (bbnf's real branch)"
    ( cd "$VENDOR_DIR" && cargo check )
    echo "[4/4] vendored csp-solver: --features py IN ISOLATION (ThreadSafe ⟹ Csp: Send)"
    verify_py_isolated
    echo "── test stage ───────────────────────────────────────────────────────"
    echo "vendored lattice suite (monotonic-lattice revise/cascade confluence check)"
    ( cd "$VENDOR_DIR" && cargo test --test lattice )
    echo
    echo "OK: --verify green — root ∪ skinny ∪ csp-solver×{default,py} compile; lattice tests pass."
}

case "${1:---check}" in
    --check)  do_check ;;
    --update) shift || true; do_update "${1:-HEAD}" ;;
    --verify) do_verify ;;
    -h|--help) sed -n '2,60p' "$0" ;;
    *) die "usage: $0 [--check | --update [<rev>] | --verify]" ;;
esac
