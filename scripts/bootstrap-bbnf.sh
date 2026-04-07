#!/usr/bin/env bash
# Bootstrap the self-hosted BBNF grammar parser.
#
# Expands the proc-macro output from crates/bootstrap/ into a standalone
# generated.rs module for crates/core/src/grammar/.
#
# Usage: ./scripts/bootstrap-bbnf.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT="$ROOT_DIR/crates/core/src/grammar/generated.rs"

echo "Expanding bbnf-bootstrap..."
cd "$ROOT_DIR"
rm -rf target/.bbnf-cache/

{
    echo '#![allow(unused, non_snake_case, non_camel_case_types, non_upper_case_globals, clippy::all)]'
    echo '//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.'
    echo '//! Regenerate: scripts/bootstrap-bbnf.sh'
    echo ''
    cargo expand -p bbnf-bootstrap --lib 2>/dev/null \
        | grep -v '^#!\[' \
        | grep -v '^#\[prelude_import\]' \
        | grep -v '^use std::prelude::' \
        | grep -v '^extern crate' \
        | sed 's/use bbnf_derive::Parser;//' \
        | sed 's/#\[parser(.*)\]//' \
        | sed 's/^pub struct BbnfBootstrap;//'
} > "$OUTPUT"

echo "Generated: $OUTPUT ($(wc -l < "$OUTPUT" | tr -d ' ') lines)"
