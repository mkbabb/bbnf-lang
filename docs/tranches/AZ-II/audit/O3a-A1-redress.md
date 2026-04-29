# O3a-A1 Redress Probe - Analysis, LSP, json-prototype, Bootstrap/JIT

Date: 2026-04-29
Worktree: `/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress`
Branch: `codex/azii-o3a-a1-redress`

## Scope Boundary

This agent did not edit source. Source and archive redress waits for
the O3a-A1 plan amendment. The only created file is this audit note.

No-shim disposition: do not add compatibility paths for tape-era or
derive-era surfaces. Live analysis/LSP failures should be repaired at
the analysis span model. Historical `json-prototype` should be deleted
or archived by its owning wave rather than fixture-seeded as a live
workspace test surface.

## Focused Reproduction Commands

All cargo probes used an isolated target directory:
`CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress`.

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress cargo nextest run --cargo-profile ax-iter -p bbnf-analysis -E 'test(import_directive_has_semantic_tokens)'
```

Result: reproduced. `bbnf-analysis::directives import_directive_has_semantic_tokens`
failed twice. Panic:
`crates/analysis/tests/directives.rs:232:5: should have semantic token for @import keyword`.

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress cargo nextest run --cargo-profile ax-iter -p bbnf-lsp -E 'test(test_hover_recover_keyword)'
```

Result: reproduced. `bbnf-lsp::integration test_hover_recover_keyword`
failed twice. LSP response was:
`{"jsonrpc":"2.0","result":null,"id":10}`.

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress cargo nextest run --cargo-profile ax-iter --manifest-path crates/core/benches/json-prototype/Cargo.toml -E 'test(parses_citm) | test(parses_data_s) | test(parses_data_xl) | test(parses_canada) | test(parses_twitter) | test(tape_visitor_twitter) | test(tape_visitor_data_s)'
```

Result: reproduced. All seven historical `json-prototype::corpus`
tests failed before parsing with `No such file or directory` for
`../../data/json/{data,twitter,citm_catalog,canada,data_xl}.json`.

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress cargo nextest run --cargo-profile ax-iter -p bbnf-analysis -E 'test(import_item_spans_correct) | test(import_directive_has_semantic_tokens)' --no-fail-fast
```

Result: `import_item_spans_correct` passed, while
`import_directive_has_semantic_tokens` failed. This separates item
span extraction from directive keyword token span handling.

```bash
CARGO_TARGET_DIR=/Users/mkbabb/Programming/bbnf-wt-azii-o3a-a1-redress/target/o3a-a1-redress cargo nextest run --cargo-profile ax-iter -p bbnf-lsp -E 'test(test_analyze_recover_directive)'
```

Result: passed. Recover directives are extracted into analysis state;
the failing LSP hover is specific to keyword hover/span handling.

## Scan Commands

```bash
rg -n "import_directive_has_semantic_tokens|test_hover_recover_keyword|parses_citm|tape_visitor_twitter|tape_visitor_data_s|json-prototype|bootstrap_parser|crates/gorgeous/src/jit.rs|jit.rs" .
rg -n "bootstrap_parser::parse|pub mod bootstrap_parser|BbnfBootstrap::parse|generated::BbnfBootstrap|grammar::parse\(|parse_with_state\(" crates docs/tranches/AZ-II docs/benchmarks/post-AZ-II.json docs/tranches/REMAINING-TRAJECTORY.md
rg -n "bbnf_derive|the proc-macro derive|generate_project|cargo xtask regen|prettify|GrammarParser|gorgeous-jit|crates/gorgeous/src/jit.rs|jit" crates/gorgeous/src docs/tranches/AZ-I/audit docs/tranches/AZ-II docs/tranches/AC/AC.md docs/tranches/AW/audit/full-codebase-prune.md
rg -n "json-prototype|benches/json-prototype|crates/json-prototype|json_value" Cargo.toml crates/core/Cargo.toml crates/core/benches/json-prototype/Cargo.toml docs/tranches/AZ-I docs/tranches/AZ-II docs/benchmarks/post-AZ-I.json docs/benchmarks/post-AZ-II.json .cargo/config.toml Makefile
ls -l data data/json crates/core/tests/fixtures/tape_golden/json
```

## Evidence

### Live analysis failure

`crates/analysis/src/directives/import.rs:26-44` generates the
`@import` keyword token from `imp.span.0..imp.span.0 + 7`.
`crates/analysis/tests/directives.rs:219-252` expects the token text
slice to equal `@import`. The companion test
`import_item_spans_correct` passes, so imported item spans are valid;
the failing surface is the directive keyword span.

Likely patch files:

- `crates/analysis/src/state/types.rs`
- `crates/analysis/src/directives/import.rs`
- `crates/analysis/tests/directives.rs`

Patch shape: add explicit keyword spans for import directives, or
normalize `ImportInfo::span` so it includes the `@import` keyword.
Do not add a token fallback that searches the whole document at use
time; make extraction own the span truth.

### Live LSP failure

`crates/lsp/tests/analyze.rs::test_analyze_recover_directive` passes,
so `@recover` directives reach analysis state. The integration hover
test still returns `result:null` over line 0 character 3.

`crates/analysis/src/directives/recover.rs:49-59` also assumes the
keyword token starts at `rec.span.0`, and
`crates/analysis/src/features/hover/directive.rs:6-14` gates hover on
`rec.span`. This matches the import failure pattern: directive data
exists, but keyword/directive span coverage is wrong for the keyword
position.

Likely patch files:

- `crates/analysis/src/state/types.rs`
- `crates/analysis/src/directives/recover.rs`
- `crates/analysis/src/features/hover/directive.rs`
- `crates/lsp/tests/integration.rs`

Patch shape: make recover keyword span explicit and use it for semantic
tokens and hover. Keep hover over the rule name delegated to rule hover.

### Historical json-prototype failures

The failing tests in `crates/core/benches/json-prototype/tests/corpus.rs`
load fixtures with `../../data/json`. That path was correct for the old
`crates/json-prototype` location but is stale after demotion to
`crates/core/benches/json-prototype`. Current `data/json` is a sibling
worktree seed symlink to `/Users/mkbabb/Programming/bbnf-lang/data/json/`;
small checked fixtures also exist under
`crates/core/tests/fixtures/tape_golden/json`.

`docs/benchmarks/post-AZ-I.json` records
`crates/json-prototype/ relocated to crates/core/benches/json-prototype/;
workspace member retired`. `crates/core/Cargo.toml` still has
`json-prototype = { path = "benches/json-prototype" }`, and
`.cargo/config.toml` still includes `--bench json_value` in
`bench-json`.

Likely delete/archive files if O5 consumes deletion:

- `crates/core/benches/json-prototype/**`
- `crates/core/Cargo.toml` entries for the `json-prototype` path
  dev-dependency and `json_value` bench target
- `.cargo/config.toml` `bench-json` reference to `--bench json_value`
- Any O6 benchmark text that still treats `json_value` as a live close
  surface

No-shim disposition: do not seed fixture path compatibility for this
prototype. If retained as a benchmark archaeology artifact, move it out
of nextest/workspace execution and out of tape deletion blockers.

### bootstrap_parser.rs disposition

`crates/core/src/grammar/bootstrap_parser.rs:1-19` says the file is a
hand-written AZ-II.cutover.G bootstrap parser that bypasses broken
`BbnfBootstrap::parse` self-hosting. `crates/core/src/grammar/mod.rs`
routes public `grammar::parse` through `bootstrap_parser::parse`, and
`crates/core/src/pipeline/directives.rs` routes pipeline directive
parsing through the same bridge.

The bridge is live, not dead code. `docs/benchmarks/post-AZ-II.json`
records the generated `BbnfBootstrap::parse` self-host failure and
names generated self-host repair as the real close condition.

Likely owner files:

- `crates/core/src/grammar/bootstrap_parser.rs`
- `crates/core/src/grammar/mod.rs`
- `crates/core/src/pipeline/directives.rs`
- `crates/core/benches/bbnf/monolithic.rs`
- `crates/bootstrap/src/bin/debug_parse.rs`

Disposition: keep only as a bounded bridge until O6/O7 proves generated
`BbnfBootstrap::parse` can parse the BBNF fixture corpus and regen can
self-host. Do not hide it as ambient legacy code, and do not add a
second parser shim.

### Gorgeous JIT disposition

`crates/gorgeous/src/main.rs` still exposes grammar-file mode through
`jit::format_grammar`. `crates/gorgeous/src/jit.rs` generates a
temporary Cargo project with published `bbnf_derive`, emits
`#[derive(Parser)]`, and compiles `gorgeous-jit`. This is derive-era
JIT substrate, not wired to `cargo xtask regen`.

Likely owner files:

- `crates/gorgeous/src/jit.rs`
- `crates/gorgeous/src/main.rs`
- `crates/gorgeous/Cargo.toml` if a feature/CLI surface is removed

Disposition: delete grammar-file JIT mode unless a product owner writes
a real `cargo xtask regen` backed replacement. Do not keep the published
derive path as compatibility.

## Halt / Ready Disposition

Halt: no source edits were made because A1 source/archive redress waits
for a plan amendment.

Ready for plan amendment:

1. Live analysis/LSP: repair directive keyword span ownership in
   `bbnf-analysis`, then re-run the two focused failing tests plus the
   recover analysis pass.
2. Historical json-prototype: O5 should delete/archive the prototype
   and remove its bench/dev-dependency hooks before deleting `crates/tape`.
3. Bootstrap: O6/O7 should add a generated self-host proof gate before
   retiring `bootstrap_parser.rs`.
4. Gorgeous JIT: O5/O7 should delete the derive-shaped JIT surface or
   replace it with a regen-backed implementation.
