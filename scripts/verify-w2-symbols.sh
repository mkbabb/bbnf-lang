#!/usr/bin/env bash
# AW-IV.W2.2 — verify symbol-presence/absence in bench binaries.
#
# After running `cargo bench` over the parse-bench matrix, inspect each
# bench binary's `nm` output for the W2 invariant: zero hot-path helper
# symbols in the per-grammar walker; cold-path helpers (dispatch_one,
# try_branch) may remain in the AX replay surface.
#
# Background (tranche AW-IV.W2):
#   W2.1 inline-emits hot helper bodies into per-grammar walker arms.
#   W2.2 adds workspace bench-profile `lto = "fat", codegen-units = 1`
#   as belt-and-suspenders cover (Cargo.toml). After W2.1's inline
#   emission + W2.2's LTO, `nm` MUST show zero hot-path helper symbols
#   in the bench binary — either inlined at source level (W2.1) or
#   collapsed at link time (W2.2 LTO). If a helper symbol survives, the
#   function-call boundary is the new dispatcher; treat as a wave-close
#   failure.
#
#   The per-grammar walker entry symbol `dta_run_<grammar>` MUST remain
#   present — that IS the specialised hot path the tranche delivers.
#
# Usage:
#   BENCH_DIR=target/release/deps ./scripts/verify-w2-symbols.sh
#   REPORT=/tmp/aw4-w2.report ./scripts/verify-w2-symbols.sh
#
# Env vars:
#   BENCH_DIR   directory to scan for bench binaries (default: target/release/deps)
#   REPORT      optional path to tee the report into
#   STRICT      if "1", exit non-zero when any hot helper is PRESENT or
#               any cold helper is ABSENT; default reports only

set -euo pipefail

BENCH_DIR="${BENCH_DIR:-target/release/deps}"
REPORT="${REPORT:-}"
STRICT="${STRICT:-0}"

# Hot-path helpers W2.1 inline-emits — must be ABSENT from bench
# binaries after W2.1's inline-emit migration + W2.2's fat LTO cover.
# Each name is a fragment matched against `nm` output (grep -c).
HOT_HELPERS=(
    'emit_leaf'
    'reserve_compound'
    'push_compound_fused'
    'push_leaf_fused'
    'advance_or_pop_with'
    'advance_seq_fast'
    'close_compound'
    'trim_with_pattern'
    'first_ws_pattern'
    'handle_repeat_failure'
)

# PSI push: psi module's `pub fn push` → mangled as `...psi...push...`
# but also appears as `PayloadStream::push`; scan a broader pattern.
HOT_HELPERS_REGEX=(
    'psi.*push|PayloadStream.*push'
)

# Cold-path replay-surface symbols — AX retains these; PRESENT is fine.
COLD_HELPERS=(
    'dispatch_one'
    'try_branch'
)

# Per-grammar walker entry: `__dta_walker_inline::run` (mangled as
# `...dta_walker_inline3run`; the `3run` suffix is the rustc name
# length prefix that uniquely identifies the `run` entry inside the
# inline module). At least one must be PRESENT per bench — the
# specialised hot path. Matching only `3run` avoids the module-prefix
# false-positive where every generated helper nested inside the
# `___dta_walker_inline` module shares the prefix.
#
# The `dta_run_<grammar>` stable surface (per AW-IV plan §W1) is a
# thin wrapper around `__dta_walker_inline::run`; when emitted it
# also matches a non-length-prefixed form — rg the same symbol ends
# in `dta_run_<Grammar>` terminated by the grammar name identifier.
WALKER_ENTRY_LABEL='walker-entry (run/dta_run_<grammar>)'
WALKER_ENTRIES_PATTERN='dta_walker_inline3run|[0-9]+dta_run_[A-Z]'

emit() {
    if [[ -n "$REPORT" ]]; then
        # tee writes to both stdout and $REPORT; -a keeps append mode
        printf '%s\n' "$*" | tee -a "$REPORT"
    else
        printf '%s\n' "$*"
    fi
}

emit_line() {
    # Formatted emit used inside the per-binary loop — pass the
    # already-formatted line; unifies stdout + REPORT output.
    emit "$1"
}

[[ -n "$REPORT" ]] && : > "$REPORT"

emit "=== AW-IV.W2.2 bench-binary symbol ledger ==="
emit "bench dir: $BENCH_DIR"
emit "date:      $(date -u +%Y-%m-%dT%H:%M:%SZ)"
emit ""

overall_fail=0

for bench_name in 'json_monolithic' 'css_l4' 'google_sheets_monolithic' 'bbnf_monolithic'; do
    # Bench binary hashes differ per build; pick the newest non-artefact.
    bin="$(ls -t "$BENCH_DIR/${bench_name}-"* 2>/dev/null \
        | grep -vE '\.(d|o|dSYM|rmeta)$' \
        | head -1 || true)"
    if [[ -z "$bin" ]]; then
        emit "-- skip: $bench_name — no bench binary under $BENCH_DIR"
        emit ""
        continue
    fi

    emit "-- $bench_name  :: $(basename "$bin")"

    # Cache nm output per binary — cheaper than per-symbol.
    nm_out="$(nm "$bin" 2>/dev/null || true)"

    # Hot-path helpers (must be 0).
    for sym in "${HOT_HELPERS[@]}"; do
        n="$(printf '%s' "$nm_out" | grep -c "$sym" || true)"
        if [[ "$n" -eq 0 ]]; then
            status='OK'
        else
            status='PRESENT (hot-path helper — wave invariant failure)'
            [[ "$STRICT" == "1" ]] && overall_fail=1
        fi
        emit_line "$(printf '   hot   %-28s %4d  %s' "$sym" "$n" "$status")"
    done

    # Hot-path regex helpers.
    for pat in "${HOT_HELPERS_REGEX[@]}"; do
        n="$(printf '%s' "$nm_out" | grep -cE "$pat" || true)"
        if [[ "$n" -eq 0 ]]; then
            status='OK'
        else
            status='PRESENT (hot-path helper — wave invariant failure)'
            [[ "$STRICT" == "1" ]] && overall_fail=1
        fi
        emit_line "$(printf '   hot   %-28s %4d  %s' "$pat" "$n" "$status")"
    done

    # Cold-path replay-surface (PRESENT is fine; ABSENT means AX replay
    # path lost a helper — report but don't hard-fail unless STRICT=1).
    for sym in "${COLD_HELPERS[@]}"; do
        n="$(printf '%s' "$nm_out" | grep -c "$sym" || true)"
        if [[ "$n" -gt 0 ]]; then
            status='PRESENT (cold-path replay surface — expected)'
        else
            status='ABSENT (cold-path replay helper gone — AX replay broken?)'
            [[ "$STRICT" == "1" ]] && overall_fail=1
        fi
        emit_line "$(printf '   cold  %-28s %4d  %s' "$sym" "$n" "$status")"
    done

    # Walker entry (at least one must be PRESENT).
    n="$(printf '%s' "$nm_out" | grep -cE "$WALKER_ENTRIES_PATTERN" || true)"
    if [[ "$n" -gt 0 ]]; then
        status='PRESENT (per-grammar walker entry)'
    else
        status='ABSENT (per-grammar walker missing — tranche invariant failure)'
        [[ "$STRICT" == "1" ]] && overall_fail=1
    fi
    emit_line "$(printf '   entry %-28s %4d  %s' "$WALKER_ENTRY_LABEL" "$n" "$status")"

    emit ""
done

emit "=== end ledger ==="

if [[ "$STRICT" == "1" && "$overall_fail" -ne 0 ]]; then
    emit "STRICT mode: one or more invariants failed"
    exit 1
fi

exit 0
