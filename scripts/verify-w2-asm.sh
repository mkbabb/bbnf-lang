#!/usr/bin/env bash
# AW-IV.W2.2 — disasm scanner for hot-path helper-call instructions.
#
# Companion to `verify-w2-symbols.sh`. Where the nm-based script proves
# a helper *symbol* is absent from a bench binary, this script proves
# the *per-grammar walker body itself* contains no `bl` (aarch64) or
# `call` (x86_64) instructions targeting any hot-path helper. A symbol
# surviving elsewhere in the binary is harmless — what matters is
# whether the walker's own body still branches into a cross-crate
# helper at runtime.
#
# Procedure:
#   1. nm the bench binary; find every `__dta_walker_inline::run` and
#      `dta_run_<grammar>` symbol (walker entry points).
#   2. For each, disassemble just that symbol with `objdump -d
#      --disassemble-symbols=<mangled>`.
#   3. Scan the disassembly for `bl\s+0x...\s+<.*helper>` (aarch64) or
#      `callq\s+0x...\s+<.*helper>` (x86_64). Report any match.
#
# Usage:
#   BENCH_DIR=target/release/deps ./scripts/verify-w2-asm.sh
#   REPORT=/tmp/aw4-w2-asm.report ./scripts/verify-w2-asm.sh
#
# Env vars:
#   BENCH_DIR   directory holding bench binaries (default: target/release/deps)
#   REPORT      optional path to tee report into
#   STRICT      "1" → exit non-zero on any hot-helper call hit

set -euo pipefail

BENCH_DIR="${BENCH_DIR:-target/release/deps}"
REPORT="${REPORT:-}"
STRICT="${STRICT:-0}"

if ! command -v objdump >/dev/null 2>&1; then
    echo "error: objdump not found in PATH" >&2
    echo "       install binutils (llvm-objdump also works if aliased)" >&2
    exit 2
fi

ARCH="$(uname -m)"
case "$ARCH" in
    arm64|aarch64) CALL_MNEMONIC='bl' ;;
    x86_64|amd64)  CALL_MNEMONIC='callq' ;;
    *)
        echo "error: unsupported arch $ARCH (only arm64 / x86_64 supported)" >&2
        exit 2
        ;;
esac

# Hot-path helpers — any `bl`/`call` to these from the walker body is a
# W2 invariant failure. Regex-friendly; matches the mangled suffix.
HOT_HELPER_PATTERNS=(
    'emit_leaf'
    'emit_leaf_with_payload'
    'reserve_compound'
    'push_compound_fused'
    'push_leaf_fused'
    'advance_or_pop_with'
    'advance_seq_fast'
    'close_compound'
    'trim_with_pattern'
    'first_ws_pattern'
    'handle_repeat_failure'
    'handle_repeat_failure_bounded'
    # psi.rs push path — PayloadJob raw_vec::grow_one is the re-allocation
    # slow path; the steady-state push should be inlined as well.
    '3psi3push|PayloadJob.*grow_one'
)

# Build a single alternation regex from the hot-helper patterns for the
# call-site grep. Each pattern may itself contain alternation; we join
# with `|` producing one compound regex.
HOT_HELPER_ALT="$(IFS='|'; echo "${HOT_HELPER_PATTERNS[*]}")"

emit() {
    if [[ -n "$REPORT" ]]; then
        printf '%s\n' "$*" | tee -a "$REPORT"
    else
        printf '%s\n' "$*"
    fi
}

[[ -n "$REPORT" ]] && : > "$REPORT"

emit "=== AW-IV.W2.2 walker-disassembly helper-call ledger ==="
emit "bench dir: $BENCH_DIR"
emit "arch:      $ARCH  (call mnemonic: $CALL_MNEMONIC)"
emit "date:      $(date -u +%Y-%m-%dT%H:%M:%SZ)"
emit ""

overall_fail=0

for bench_name in 'json_monolithic' 'css_l4' 'google_sheets_monolithic' 'bbnf_monolithic'; do
    bin="$(ls -t "$BENCH_DIR/${bench_name}-"* 2>/dev/null \
        | grep -vE '\.(d|o|dSYM|rmeta)$' \
        | head -1 || true)"
    if [[ -z "$bin" ]]; then
        emit "-- skip: $bench_name — no bench binary under $BENCH_DIR"
        emit ""
        continue
    fi

    emit "-- $bench_name  :: $(basename "$bin")"

    # Collect walker entry symbols (demangled name contains one of
    # `dta_walker_inline::run` or `dta_run_<grammar>`).
    # Mangled symbols show `19___dta_walker_inline3run` or
    # `12dta_run_Json` etc.; match both.
    walkers="$(nm "$bin" 2>/dev/null \
        | awk '{print $NF}' \
        | grep -E 'dta_walker_inline.{0,6}run|dta_run_[A-Za-z]' \
        || true)"
    if [[ -z "$walkers" ]]; then
        emit "   no walker symbols found in binary"
        emit ""
        continue
    fi

    walker_count=0
    hit_count=0

    while IFS= read -r sym; do
        [[ -z "$sym" ]] && continue
        walker_count=$((walker_count + 1))

        # Disassemble just this symbol; scan for hot-helper call sites.
        hits="$(objdump -d --disassemble-symbols="$sym" "$bin" 2>/dev/null \
            | grep -E "^[[:space:]]*[0-9a-f]+:[[:space:]]+[0-9a-f ]+[[:space:]]+${CALL_MNEMONIC}[[:space:]]+.*<.*(${HOT_HELPER_ALT}).*>" \
            || true)"

        if [[ -n "$hits" ]]; then
            n="$(printf '%s\n' "$hits" | grep -c . || true)"
            hit_count=$((hit_count + n))
            [[ "$STRICT" == "1" ]] && overall_fail=1
            emit "   walker symbol: $sym"
            emit "       hits ($n):"
            while IFS= read -r hit; do
                [[ -z "$hit" ]] && continue
                # Normalise leading whitespace for a compact ledger line.
                emit "         $(echo "$hit" | sed -E 's/^[[:space:]]+//')"
            done <<<"$hits"
        fi
    done <<<"$walkers"

    if [[ "$hit_count" -eq 0 ]]; then
        emit "   walkers scanned: $walker_count — NO hot-helper calls (OK)"
    else
        emit "   walkers scanned: $walker_count — hot-helper call sites: $hit_count (FAILURE)"
    fi
    emit ""
done

emit "=== end ledger ==="

if [[ "$STRICT" == "1" && "$overall_fail" -ne 0 ]]; then
    emit "STRICT mode: walker body contains hot-helper calls — invariant failed"
    exit 1
fi

exit 0
