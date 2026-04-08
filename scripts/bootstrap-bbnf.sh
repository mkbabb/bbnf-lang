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
INPUT_FILE="$TEMP" python3 <<'PYEOF' > "$OUTPUT"
import os, re, sys

with open(os.environ['INPUT_FILE']) as f:
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

# Strip auto-generated trait impls inside the cst_directives module and
# add back #[derive(Clone, Copy)] above each struct.
#
# cargo expand expands ALL derives into unstable internals
# (fmt_helpers_for_derive, trivial_clone, derive_clone_copy_internals).
# We strip the impls, keep only the struct definitions, and re-add the
# stable derive macro above each one.
def find_module_body(s, header):
    start = s.find(header)
    if start < 0:
        return None
    open_brace = s.find('{', start)
    if open_brace < 0:
        return None
    depth = 1
    j = open_brace + 1
    while j < len(s) and depth > 0:
        if s[j] == '{':
            depth += 1
        elif s[j] == '}':
            depth -= 1
        j += 1
    return (start, open_brace + 1, j - 1, j)  # (header_start, body_start, body_end, after_close)

found = find_module_body(text, 'pub mod cst_directives ')
if found is not None:
    header_start, body_start, body_end, after_close = found
    body = text[body_start:body_end]
    # Strip every '#[automatically_derived]' impl/unsafe-impl block via brace
    # counting. The marker may be preceded by indentation; the impl that
    # follows (after attribute(s)) is what we strip.
    out_chars = []
    i = 0
    while i < len(body):
        marker = body.find('#[automatically_derived]', i)
        if marker < 0:
            out_chars.append(body[i:])
            break
        # Walk back over indentation on the marker's line.
        line_start = body.rfind('\n', 0, marker) + 1
        out_chars.append(body[i:line_start])
        # Skip any consecutive attribute lines + the impl/unsafe impl signature.
        k = marker
        while True:
            line_end = body.find('\n', k)
            if line_end < 0:
                line_end = len(body)
            line = body[k:line_end]
            stripped = line.lstrip()
            if stripped.startswith('#['):
                k = line_end + 1
                continue
            break
        # k now points at the line after attributes — should be `impl` or `unsafe impl`.
        impl_brace = body.find('{', k)
        if impl_brace < 0:
            out_chars.append(body[line_start:])
            break
        depth = 1
        j = impl_brace + 1
        while j < len(body) and depth > 0:
            if body[j] == '{':
                depth += 1
            elif body[j] == '}':
                depth -= 1
            j += 1
        # Skip the trailing newline if present.
        if j < len(body) and body[j] == '\n':
            j += 1
        i = j
    body = ''.join(out_chars)
    # Re-add #[derive(Clone, Copy)] above each struct definition.
    body = re.sub(
        r'(\n)(    )(pub struct [A-Za-z_]+<.a> \{)',
        r'\1\2#[derive(Clone, Copy)]\n\2\3',
        body,
    )
    text = text[:body_start] + body + text[body_end:]

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
PYEOF

rm "$TEMP"
LINES=$(wc -l < "$OUTPUT" | tr -d ' ')
echo "Generated: $OUTPUT ($LINES lines)"
