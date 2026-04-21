#!/usr/bin/env bash
# Tiered workspace test runner.
#
# Three tiers of correctness coverage — the orchestrator picks the
# minimum tier required for the iteration. File-first discipline: each
# tier writes to `/tmp/test-tier-<tier>.txt`; callers grep the file.
#
# Tiers:
#   leaf     — pure-data + substrate crates (tape, bbnf-ir, egraph,
#              csp-solver, bbnf-ser). ~1 min cold, ~15 s warm.
#              Appropriate while iterating on mining / IR passes that
#              don't cross the emitter boundary.
#   grammar  — per-grammar tape parity + per-grammar shape emit. One
#              derive-Parser site per test binary. ~3-5 min cold.
#              Appropriate while iterating on shape emitters.
#   workspace — full `cargo test --workspace`. ~10-15 min cold.
#              Wave close only.
#
# Usage:
#   scripts/test-tier.sh leaf
#   scripts/test-tier.sh grammar
#   scripts/test-tier.sh workspace
#   scripts/test-tier.sh grammar --no-run   # compile-gate only
#
# Extra args after the tier name pass through to `cargo test`.

set -euo pipefail

TIER="${1:?usage: $0 <leaf|grammar|workspace> [extra cargo args]}"
shift || true

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# Always clear .bbnf-cache — the derive macro caches expansions and
# cargo clean does NOT clear them (see docs/instructions/README.md).
find . -name .bbnf-cache -type d -exec rm -rf {} + 2>/dev/null || true

OUT="/tmp/test-tier-$TIER.txt"
: > "$OUT"

case "$TIER" in
    leaf)
        # Leaf crates carry no derive-Parser sites; their rustc cost is
        # proportional to hand-written code only. Fastest tier.
        # Workspace crate names (see Cargo.toml / crates/*/Cargo.toml):
        # `tape`, `bbnf-ir`, `egraph`, `csp-solver`, `bbnf-ser`.
        cargo test -p tape -p bbnf-ir -p egraph -p csp-solver -p bbnf-ser \
            "$@" > "$OUT" 2>&1
        ;;
    grammar)
        # Each per-grammar test binary links exactly one derive-Parser
        # expansion. Bin list audited against crates/core/tests/ — stale
        # names (tape_parity_*, grammar_roundtrip) removed in B0.W2.a;
        # extend as further aggregates split.
        for bin in \
            bbnf_parity bbnf_ast_parity bbnf_self_parity \
            json_parity css_l4_parity sheets_parity \
            sheets_expr_parity sheets_self_parity \
            shape_dispatch_emission payload_layouts
        do
            if [[ -f "$ROOT/crates/core/tests/$bin.rs" ]] \
                || [[ -d "$ROOT/crates/core/tests/$bin" ]]; then
                echo "=== $bin ===" >> "$OUT"
                cargo test -p bbnf --test "$bin" "$@" >> "$OUT" 2>&1 || true
            fi
        done
        ;;
    workspace)
        cargo test --workspace "$@" > "$OUT" 2>&1
        ;;
    *)
        echo "unknown tier: $TIER (want leaf|grammar|workspace)" >&2
        exit 2
        ;;
esac

# Report.
echo "--- results ($OUT) ---"
grep -E '^test result|FAILED|error\[' "$OUT" || true
if grep -Eq 'FAILED|error\[' "$OUT"; then
    echo "FAIL: $OUT contains failures; grep the file for details." >&2
    exit 1
fi
echo "OK: $TIER tier green."
