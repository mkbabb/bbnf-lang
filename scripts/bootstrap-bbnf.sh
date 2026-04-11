#!/usr/bin/env bash
# Bootstrap the self-hosted BBNF grammar parser.
#
# Expands the proc-macro output from crates/bootstrap/ into a standalone
# generated.rs module for crates/core/src/grammar/.
#
# Under AC.2 the expanded shape is tape-first: per-rule `<Name>View<'tape>`
# structs + an `impl ::bbnf::runtime::Root for BbnfBootstrap` binding + an
# `impl BbnfBootstrap` block carrying the rule functions and `parse` entry
# point. The old `BbnfBootstrapEnum<'a>` + `__BbnfBootstrapEnumCtx` surface
# is gone.
#
# Post-processing strips unstable-feature boilerplate that cargo expand
# injects (`#![feature(...)]`, `#[prelude_import]`, `::core::panicking::panic_fmt`)
# and replaces auto-derived `#[automatically_derived] impl Clone / Copy /
# Debug` blocks — which expand into unstable `derive_clone_copy`,
# `structural_match`, and `fmt_helpers_for_derive` internals — with stable
# `#[derive(Clone, Copy, Debug)]` attributes above the struct definitions.
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

# ── 1. Strip crate-level boilerplate ─────────────────────────────────
# cargo expand prepends `#![feature(...)]` lines, `#[prelude_import]`,
# and `extern crate std;`. None of these belong in a checked-in module.
# Crate-level attributes may span multiple lines (DOTALL) and be
# followed by optional whitespace — match both single-line and
# multi-line forms.
text = re.sub(r'#!\[[^]]*?\]\n', '', text, flags=re.DOTALL)
text = re.sub(r'#\[prelude_import\]\nuse std::prelude::rust_2024::\*;\n', '', text)
text = re.sub(r'extern crate std;\n', '', text)

# ── 2. Strip the derive macro attribute + source struct ──────────────
# The `pub struct BbnfBootstrap;` declaration is re-emitted in the
# hand-written header below. The `use bbnf_derive::Parser;` import is
# not needed once the macro is expanded. The `#[parser(...)]` attribute
# is a macro input, not Rust code.
text = re.sub(r'use bbnf_derive::Parser;\n', '', text)
text = re.sub(r'#\[parser\(.*?\)\]\n', '', text)
text = re.sub(r'pub struct BbnfBootstrap;\n', '', text)

# ── 3. Generic brace-matching helpers ─────────────────────────────────
# Find a `{ ... }` block starting at `open_brace` (index of the '{').
# Returns the index AFTER the matching '}' (or `len(s)` on unbalanced).
def skip_braces(s, open_brace):
    assert s[open_brace] == '{'
    depth = 1
    j = open_brace + 1
    while j < len(s) and depth > 0:
        c = s[j]
        if c == '{':
            depth += 1
        elif c == '}':
            depth -= 1
        j += 1
    return j

# Find the body of a module/block whose header contains `header_needle`.
# Returns (header_start, body_start, body_end, after_close) or None.
def find_module_body(s, header_needle):
    start = s.find(header_needle)
    if start < 0:
        return None
    open_brace = s.find('{', start)
    if open_brace < 0:
        return None
    after_close = skip_braces(s, open_brace)
    return (start, open_brace + 1, after_close - 1, after_close)

# Strip every `#[automatically_derived] impl ... { ... }` block from
# `body`. The marker may be preceded by indentation and by other
# attribute lines (`#[inline]`, `#[allow(...)]` etc.); we walk back
# over the whole attribute run so the resulting text has no dangling
# attributes. The impl signature may also be prefixed by `unsafe`.
def strip_auto_derived_impls(body):
    out = []
    i = 0
    while i < len(body):
        marker = body.find('#[automatically_derived]', i)
        if marker < 0:
            out.append(body[i:])
            break
        # Walk back to the start of the marker's line (preserve any
        # text before that line, which belongs to the previous item).
        line_start = body.rfind('\n', 0, marker) + 1
        out.append(body[i:line_start])

        # Consume all contiguous attribute lines beginning at line_start.
        k = line_start
        while True:
            line_end = body.find('\n', k)
            if line_end < 0:
                line_end = len(body)
            stripped = body[k:line_end].lstrip()
            if stripped.startswith('#['):
                k = line_end + 1
                continue
            break

        # `k` now points at the line after attributes, which must start
        # with `impl` or `unsafe impl`. Skip the block by brace-matching.
        impl_brace = body.find('{', k)
        if impl_brace < 0:
            out.append(body[line_start:])
            break
        after = skip_braces(body, impl_brace)
        # Swallow the trailing newline so we don't leave a blank.
        if after < len(body) and body[after] == '\n':
            after += 1
        i = after
    return ''.join(out)

# ── 4. Strip auto-derived impls globally ─────────────────────────────
# cargo expand expands every `#[derive(Clone)]`, `#[derive(Copy)]`,
# and `#[derive(Debug)]` into `#[automatically_derived] impl ...`
# blocks. For Clone/Copy these use the unstable
# `derive_clone_copy` / `coerce_pointee` internals, and for Debug
# the unstable `fmt_helpers_for_derive` macro. Stripping them and
# re-adding stable `#[derive(...)]` attributes produces buildable
# nightly code that compiles on stable toolchains too.
text = strip_auto_derived_impls(text)

# ── 5. Re-add stable `#[derive(Clone, Copy, Debug)]` above view structs ─
# The AC.2 view generator emits every view as `#[derive(Clone, Copy,
# Debug)]`, so we need one uniform re-add. Match the `pub struct
# <Name>View<'tape> {` header and splice the derive above it, preserving
# indentation. The `#[allow(non_camel_case_types)]` attribute survives
# expansion unchanged on the struct — only `#[derive(...)]` is split
# out into auto-derived impls, so we don't re-add that one.
def readd_view_derives(text):
    """Insert `#[derive(Clone, Copy, Debug)]` above every `pub struct
    <Name>View<'tape> { ... }` definition, preserving the original
    indentation and leaving any pre-existing attribute lines alone."""
    out = []
    pos = 0
    pat = re.compile(
        r'(?P<indent>^[ \t]*)pub struct (?P<name>[A-Za-z_][A-Za-z_0-9]*)View<\'tape> \{',
        re.MULTILINE,
    )
    for m in pat.finditer(text):
        out.append(text[pos:m.start()])
        indent = m.group('indent')
        name = m.group('name')
        # Idempotent: if the line immediately before the struct is
        # already our derive, don't re-add it.
        line_start = text.rfind('\n', 0, m.start()) + 1
        prev_line_end = line_start - 1
        prev_line_start = text.rfind('\n', 0, prev_line_end) + 1 if prev_line_end > 0 else 0
        prev_line = text[prev_line_start:prev_line_end].strip()
        if prev_line == '#[derive(Clone, Copy, Debug)]':
            out.append(text[m.start():m.end()])
        else:
            out.append(f'{indent}#[derive(Clone, Copy, Debug)]\n{indent}pub struct {name}View<\'tape> {{')
        pos = m.end()
    out.append(text[pos:])
    return ''.join(out)

text = readd_view_derives(text)

# ── 6. Re-add stable derives inside the `cst_directives` module ──────
# The AC.2 schema emitter may still emit `cst_directives` as a sub-
# module containing `pub struct FooDirective<'a> { ... }` definitions.
# These used `#[derive(Clone, Copy)]` in the schema emitter, so cargo
# expand produced auto-derived impls that we stripped in step 4. Walk
# the module body and re-add `#[derive(Clone, Copy)]` above each
# struct definition.
#
# If the AC.2 rewrite moves these structs up to the top level (or
# deletes them entirely), this block is a no-op because the module
# header won't be found.
def readd_cst_directive_derives(body):
    """Insert `#[derive(Clone, Copy)]` above each `pub struct Name {`
    in the `cst_directives` module body, idempotent under repeats."""
    out = []
    pos = 0
    pat = re.compile(
        r'(?P<indent>^[ \t]*)pub struct (?P<name>[A-Za-z_][A-Za-z_0-9]*)(?P<gen>(?:<[^>]*>)?) \{',
        re.MULTILINE,
    )
    for m in pat.finditer(body):
        out.append(body[pos:m.start()])
        indent = m.group('indent')
        # Check whether the previous non-empty line is already our derive.
        line_start = body.rfind('\n', 0, m.start()) + 1
        prev_end = line_start - 1
        prev_start = body.rfind('\n', 0, prev_end) + 1 if prev_end > 0 else 0
        prev = body[prev_start:prev_end].strip()
        if prev == '#[derive(Clone, Copy)]':
            out.append(body[m.start():m.end()])
        else:
            out.append(f'{indent}#[derive(Clone, Copy)]\n{indent}pub struct {m.group("name")}{m.group("gen")} {{')
        pos = m.end()
    out.append(body[pos:])
    return ''.join(out)

found = find_module_body(text, 'pub mod cst_directives ')
if found is not None:
    header_start, body_start, body_end, after_close = found
    body = text[body_start:body_end]
    body = readd_cst_directive_derives(body)
    text = text[:body_start] + body + text[body_end:]

# ── 7. Replace unstable `::core::panicking::panic_fmt` with `panic!` ─
# `panic!("...")` expands into a block calling
# `::core::panicking::panic_fmt(format_args!("..."))`. The stable
# form is simply `panic!(...)`. We match the expanded block shape
# and rewrite it back to the macro invocation.
text = re.sub(
    r'\{\s*::core::panicking::panic_fmt\(\s*format_args!\((.*?)\),?\s*\);?\s*\}',
    r'{ panic!(\1); }',
    text,
    flags=re.DOTALL,
)

# ── 8. Strip doc comments from bootstrap crate ────────────────────────
# The `//!` outer doc comment on bbnf-bootstrap's `lib.rs` survives
# expansion. We strip it because the checked-in file has its own
# auto-generated header.
lines = text.split('\n')
filtered = [line for line in lines if not line.strip().startswith('//!')]
text = '\n'.join(filtered)

# ── 9. Collapse runs of blank lines ───────────────────────────────────
text = re.sub(r'\n{3,}', '\n\n', text)

# ── 10. Emit header + body ────────────────────────────────────────────
# The header declares the `BbnfBootstrap` marker struct (stripped in
# step 2) and the `use` imports the generated code relies on. Under
# AC.2 the runtime types come from `::bbnf::runtime::*` (tape types
# and `Root` trait), while the legacy `parse_that::Span` path is
# kept for any surviving fields that still carry spans verbatim.
#
# If the expanded output contains import lines emitted inside
# `emit_grammar_impl` — or, on re-processing, the lines inserted by
# the header block below — strip them so the header below provides
# the single canonical prelude. Matches idempotent under re-runs.
text = re.sub(r'^\s*use ::parse_that::\*;\n', '', text, flags=re.MULTILINE)
text = re.sub(r'^\s*use ::parse_that::Span;\n', '', text, flags=re.MULTILINE)
text = re.sub(r'^\s*use ::bbnf::runtime::\*;\n', '', text, flags=re.MULTILINE)
text = re.sub(r'^\s*use ::bbnf::runtime::tape::\*;\n', '', text, flags=re.MULTILINE)
text = re.sub(
    r'^\s*use ::bbnf::runtime::\{[^}]*\};\n',
    '',
    text,
    flags=re.MULTILINE,
)
# Also strip the pre-existing `pub struct BbnfBootstrap;` declaration
# if re-processing an already-generated file — the header emits it.
text = re.sub(r'^\s*pub struct BbnfBootstrap;\n', '', text, flags=re.MULTILINE)

header = '''//! AUTO-GENERATED from grammar/bbnf/bbnf.bbnf — do not edit manually.
//! Regenerate: scripts/bootstrap-bbnf.sh

#![allow(
    dead_code,
    unused_variables,
    unused_mut,
    unused_parens,
    unused_assignments,
    non_camel_case_types,
    non_snake_case,
    non_upper_case_globals,
    clippy::all
)]

use ::bbnf::runtime::tape::*;
use ::bbnf::runtime::{Parsed, ParseErr, Root};
use ::parse_that::Span;

pub struct BbnfBootstrap;

'''

print(header + text.strip() + '\n')
PYEOF

rm "$TEMP"
LINES=$(wc -l < "$OUTPUT" | tr -d ' ')
echo "Generated: $OUTPUT ($LINES lines)"
