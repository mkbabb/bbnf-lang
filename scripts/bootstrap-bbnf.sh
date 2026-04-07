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

# Strip the derive macro attribute and struct declaration (already in bootstrap crate)
text = re.sub(r'use bbnf_derive::Parser;\n', '', text)
text = re.sub(r'#\[parser\(.*?\)\]\n', '', text)
text = re.sub(r'pub struct BbnfBootstrap;\n', '', text)

# Replace the expanded Debug impl with a derive attribute
# The expanded Debug uses fmt_helpers_for_derive which is unstable.
# Remove the entire #[automatically_derived] impl Debug block.
text = re.sub(
    r'#\[automatically_derived\]\nimpl<.a> ::core::fmt::Debug for BbnfBootstrapEnum<.a> \{.*?^\}\n',
    '',
    text,
    flags=re.DOTALL | re.MULTILINE
)

# Add #[derive(Debug)] to the enum
text = text.replace(
    'pub enum BbnfBootstrapEnum',
    '#[derive(Debug)]\npub enum BbnfBootstrapEnum'
)

# Also handle the context struct Debug
text = re.sub(
    r'#\[automatically_derived\]\nimpl ::core::fmt::Debug for __BbnfBootstrapEnumCtx \{.*?^\}\n',
    '',
    text,
    flags=re.DOTALL | re.MULTILINE
)

# Remove doc comments from bootstrap crate
lines = text.split('\n')
filtered = []
skip_doc = False
for line in lines:
    stripped = line.strip()
    if stripped.startswith('//!'):
        continue
    filtered.append(line)
text = '\n'.join(filtered)

# Clean up multiple blank lines
text = re.sub(r'\n{3,}', '\n\n', text)

header = '''//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

'''

print(header + text.strip() + '\n')
" > "$OUTPUT"

rm "$TEMP"
LINES=$(wc -l < "$OUTPUT" | tr -d ' ')
echo "Generated: $OUTPUT ($LINES lines)"
