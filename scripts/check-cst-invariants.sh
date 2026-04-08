#!/usr/bin/env bash
# Verify the structural invariants of the CstSchema cleanup (Phases A–E).
#
# These invariants are the primary success metric for the schema-driven CST
# refactor — they enforce that all CST traversal/access goes through the
# generated schema helpers, and that no hand-coded walker duplication leaks
# back into the codebase.
#
# Exits non-zero on any violation.

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

fail=0

print_section() {
    echo
    echo "── $1 ──"
}

# ── Invariant 1 ───────────────────────────────────────────────────────────
# No recursive text/identifier unwrappers outside generated/schema code.
# `extract_span_text`, `extract_identifier_span`, `extract_path_text` were
# the v1 hand-coded walkers. They're replaced by `BbnfBootstrapEnum::*`
# schema-emitted accessors.
print_section "Invariant 1: no extract_*_text helpers"
matches=$(grep -rn 'fn extract_span_text\|fn extract_identifier_span\|fn extract_path_text' \
    crates/core/src crates/analysis/src crates/lsp/src 2>/dev/null \
    | grep -v 'crates/core/src/grammar/generated.rs' \
    | grep -v 'crates/core/src/grammar/schema/' || true)
if [[ -n "$matches" ]]; then
    echo "FAIL: hand-coded extract_*_text functions exist outside generated code:"
    echo "$matches"
    fail=1
else
    echo "OK: no extract_*_text helpers outside generated/schema code"
fi

# ── Invariant 2 ───────────────────────────────────────────────────────────
# `substitute_and_lower` was the parallel-walker for grammar closure
# beta-reduction. Phase C replaced it with the env stack on `LowerCtx`.
print_section "Invariant 2: substitute_and_lower deleted"
matches=$(grep -rn 'fn substitute_and_lower' crates/core/src crates/analysis/src crates/lsp/src 2>/dev/null || true)
if [[ -n "$matches" ]]; then
    echo "FAIL: substitute_and_lower function exists:"
    echo "$matches"
    fail=1
else
    echo "OK: substitute_and_lower deleted"
fi

# ── Invariant 3 ───────────────────────────────────────────────────────────
# The legacy `backend/rust/ir_visitor.rs` was the wrong-layer CST codegen.
# Phase E deletes it once the schema emitter reaches feature parity.
print_section "Invariant 3: ir_visitor.rs deleted"
if [[ -f crates/core/src/backend/rust/ir_visitor.rs ]]; then
    echo "FAIL: crates/core/src/backend/rust/ir_visitor.rs still exists"
    fail=1
else
    echo "OK: ir_visitor.rs deleted"
fi

# ── Invariant 4 ───────────────────────────────────────────────────────────
# No callers of the deleted `host::extract_*` facades remain.
print_section "Invariant 4: no callers of host::extract_*"
matches=$(grep -rn 'host::extract_span_text\|host::extract_identifier_span\|host::extract_path_text' \
    crates/ 2>/dev/null || true)
if [[ -n "$matches" ]]; then
    echo "FAIL: callers of removed host::extract_* helpers remain:"
    echo "$matches"
    fail=1
else
    echo "OK: no callers of host::extract_*"
fi

echo
if [[ $fail -eq 0 ]]; then
    echo "All CST invariants hold."
    exit 0
else
    echo "CST invariants FAILED."
    exit 1
fi
