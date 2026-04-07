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

TEMP="$(mktemp)"
cargo expand -p bbnf-bootstrap --lib 2>/dev/null > "$TEMP"

echo "Post-processing..."
python3 -c "
import re, sys

with open('$TEMP') as f:
    text = f.read()

# Strip crate-level attributes and prelude
text = re.sub(r'#!\[.*?\]\n', '', text)
text = re.sub(r'#\[prelude_import\]\nuse std::prelude::rust_2024::\*;\n', '', text)
text = re.sub(r'extern crate std;\n', '', text)

# Strip the derive macro attribute and struct declaration (we re-add it below)
text = re.sub(r'use bbnf_derive::Parser;\n', '', text)
text = re.sub(r'#\[parser\(.*?\)\]\n', '', text)
text = re.sub(r'pub struct BbnfBootstrap;\n', '', text)

# Replace the expanded Debug impl with a derive attribute.
# The expanded Debug uses fmt_helpers_for_derive which is unstable.
text = re.sub(
    r'#\[automatically_derived\]\nimpl<.a> ::core::fmt::Debug for BbnfBootstrapEnum<.a> \{.*?^\}\n',
    '',
    text,
    flags=re.DOTALL | re.MULTILINE
)
text = text.replace(
    'pub enum BbnfBootstrapEnum',
    '#[derive(Debug)]\npub enum BbnfBootstrapEnum'
)

# Also remove the context struct Debug impl
text = re.sub(
    r'#\[automatically_derived\]\nimpl ::core::fmt::Debug for __BbnfBootstrapEnumCtx \{.*?^\}\n',
    '',
    text,
    flags=re.DOTALL | re.MULTILINE
)

# Make context struct and with_capacity pub(crate) for grammar/mod.rs access
text = text.replace(
    'struct __BbnfBootstrapEnumCtx',
    'pub(crate) struct __BbnfBootstrapEnumCtx'
)
text = text.replace(
    '    fn with_capacity(n: usize)',
    '    pub(crate) fn with_capacity(n: usize)'
)

# Replace unstable panic_fmt with panic! macro.
# The expanded form is: ::core::panicking::panic_fmt(format_args!("..."),);
# We replace the entire block { panic_fmt(...); } with panic!("...");
text = re.sub(
    r'\{\s*::core::panicking::panic_fmt\(\s*format_args!\((.*?)\),?\s*\);?\s*\}',
    r'{ panic!(\1); }',
    text,
    flags=re.DOTALL
)

# Remove doc comments from bootstrap crate
lines = text.split('\n')
filtered = [line for line in lines if not line.strip().startswith('//!')]
text = '\n'.join(filtered)

# Clean up multiple blank lines
text = re.sub(r'\n{3,}', '\n\n', text)

header = '''//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

use ::parse_that::*;

pub struct BbnfBootstrap;

'''

# Strip the existing 'use ::parse_that::*;' since we put it in the header
text = text.replace('use ::parse_that::*;\n', '', 1)

print(header + text.strip() + '\n')
" > "$OUTPUT"

rm "$TEMP"
LINES=$(wc -l < "$OUTPUT" | tr -d ' ')
echo "Generated: $OUTPUT ($LINES lines)"
