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
#   workspace — full workspace `cargo nextest run`. ~10-15 min cold.
#              Wave close only.
#
# Usage:
#   scripts/test-tier.sh leaf
#   scripts/test-tier.sh grammar
#   scripts/test-tier.sh workspace
#   scripts/test-tier.sh leaf --profile ax-iter
#   scripts/test-tier.sh grammar --no-run   # compile-gate only
#
# Extra args after the tier name pass through to `cargo nextest run`.
# The cargo profile is selected via `--profile <name>` which nextest
# forwards as `--cargo-profile`; matches the alias surface in
# .cargo/config.toml (ax-iter for iteration, close for wave close).
#
# Cache discipline (B1 invariant 12): this script does NOT create or
# destroy target/.bbnf-cache/. The proc-macro cache lives where
# crates/derive writes it; cycle-2 bootstrap measurements depend on
# the cache surviving between iterations.

set -euo pipefail

TIER="${1:?usage: $0 <leaf|grammar|workspace> [extra cargo args]}"
shift || true

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

# Extract --profile <name> from the remaining args and forward it to
# nextest as --cargo-profile <name>. Preserves pass-through of any
# other flags (e.g. --no-run).
CARGO_PROFILE=""
EXTRA_ARGS=()
while (($#)); do
    case "$1" in
        --profile)
            CARGO_PROFILE="$2"
            shift 2
            ;;
        --profile=*)
            CARGO_PROFILE="${1#*=}"
            shift
            ;;
        *)
            EXTRA_ARGS+=("$1")
            shift
            ;;
    esac
done

NEXTEST_PROFILE_ARGS=()
if [[ -n "$CARGO_PROFILE" ]]; then
    NEXTEST_PROFILE_ARGS+=(--cargo-profile "$CARGO_PROFILE")
fi

OUT="/tmp/test-tier-$TIER.txt"
: > "$OUT"

case "$TIER" in
    leaf)
        # Leaf crates carry no derive-Parser sites; their rustc cost is
        # proportional to hand-written code only. Fastest tier.
        # Workspace crate names (see Cargo.toml / crates/*/Cargo.toml):
        # `tape`, `bbnf-ir`, `egraph`, `csp-solver`, `bbnf-ser`.
        cargo nextest run \
            "${NEXTEST_PROFILE_ARGS[@]}" \
            -p tape -p bbnf-ir -p egraph -p csp-solver -p bbnf-ser \
            "${EXTRA_ARGS[@]}" > "$OUT" 2>&1
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
                cargo nextest run \
                    "${NEXTEST_PROFILE_ARGS[@]}" \
                    -p bbnf --test "$bin" \
                    "${EXTRA_ARGS[@]}" >> "$OUT" 2>&1 || true
            fi
        done
        ;;
    workspace)
        cargo nextest run \
            "${NEXTEST_PROFILE_ARGS[@]}" \
            --workspace \
            "${EXTRA_ARGS[@]}" > "$OUT" 2>&1
        ;;
    *)
        echo "unknown tier: $TIER (want leaf|grammar|workspace)" >&2
        exit 2
        ;;
esac

# Report.
echo "--- results ($OUT) ---"
grep -E '^test result|FAILED|error\[|Summary' "$OUT" || true
if grep -Eq 'FAILED|error\[' "$OUT"; then
    echo "FAIL: $OUT contains failures; grep the file for details." >&2
    exit 1
fi
echo "OK: $TIER tier green."
