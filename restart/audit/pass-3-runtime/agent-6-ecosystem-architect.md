# Agent 6 - Ecosystem Architect

## §1 Scope + framing

Lens: crate/module tree, CLI, language server, benches, fixtures, playground/extension boundaries, cookbook surfaces, and ecosystem packaging. PASS-3 owns the user-surface/runtime/ecosystem layer (`restart/README.md:100`, `restart/README.md:410`) and must produce module trees for `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `path-ts`, and `test-fixtures`.

Phase-1 parallel assumption: PASS-1/PASS-2 final reports are not consumed as authority under this dispatch. This report designs PASS-3 crates around the settled lock set and marks cross-pass requirements for SYNTHESIS.

## §2 Pro / Con / Explication / Challenge ledger

| Item | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| `bbnf` user runtime crate | Restart README names the top runtime and grammar-facing crate (`restart/README.md:31-38`, `restart/README.md:50-58`). | One obvious entrypoint for generated grammar users. | Can become a dumping ground. | Keep prelude/runtime traits in `bbnf`; generated grammar modules live below workspace metadata boundaries. | KEEP |
| `bbnf-cli` | Restart top layer includes CLI (`restart/README.md:100`). Old PASS-C had deferred CLI notes, but restart user-surface scope now includes it. | Essential for check/parse/bench/debug workflows. | CLI can force premature product scope. | Ship focused commands that exercise runtime contracts: `check`, `parse`, `path`, `debug`, `bench`, `metadata`. | REINVENT |
| `bbnf-language-server` consolidation | PASS-C recommended merging analysis, LSP, and DAP into one language-server crate (`restart-archive-2026-05-04/audit/passes/PASS-C.md:90-92`). | Simplifies packaging and aligns LSP/DAP/document-state needs. | Larger crate boundary. | Internal module split handles complexity; extension remains thin client. | KEEP |
| `bbnf-bench` SOTA harness | Locks demand performance gates name competitors (`restart/locks/14-LOCKS.md:207`), and README carries SOTA targets (`restart/README.md:358-361`). | Keeps claims honest and repeatable. | Bench work can sprawl. | Harness must cover only approved datasets and user surfaces. | KEEP |
| `test-fixtures` grammar-agnostic harness | PASS-C called `bbnf-test-fixtures` stable (`restart-archive-2026-05-04/audit/passes/PASS-C.md:185`), and Amendment 01 requires fixtures/benches to be grammar-agnostic (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:58-62`). | Prevents overfit and gives every generated grammar a standard conformance path. | Fixture metadata can become another registry. | Data-only grammar fixture manifests, no per-grammar Rust crates. | KEEP |
| Playground as primary app | Current playground is rich with panes, editor, diagnostics, and debug UI (`playground/src/views/playground/PlaygroundPage.vue:22-40`, `playground/src/views/playground/PlaygroundPage.vue:115-188`). | Useful for demos and language authoring. | Restart scope asks crate/module trees, not app rebuilds. | Treat playground as consumer of wasm/server APIs; do not design it as a core crate. | KEEP |
| Extension as thin client | Current extension launches language server and debug adapter (`extension/src/extension.ts:30-55`, `extension/src/extension.ts:80-100`). | Good packaging boundary. | Extension-specific logic can drift from server. | Keep extension thin; all language intelligence stays in `bbnf-language-server`. | KEEP |
| Per-grammar declaration crates | Amendment 01 rejects them (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-22`, `restart/inheritance/INDEX.md:62`). | None for ecosystem scale. | Rare host-specific integration may still need glue. | Rare escape only through explicit metadata-reviewed host adapters, not default crates. | DISCARD |

## §3 Proposed crate/module trees

### `bbnf`

```text
crates/bbnf/src/
  lib.rs
  prelude.rs
  grammar.rs
  parse.rs
  document.rs
  value.rs
  tape.rs
  visitor.rs
  path.rs
  diagnostics.rs
  host.rs
  layout.rs
  error.rs
  metadata.rs
```

`bbnf` exports user-facing traits and runtime types, not compiler internals. It owns the public `Grammar`, `DocumentView`, `ValueRef`, diagnostics, visitor traits, host ABI traits, and metadata schema.

### `bbnf-cli`

```text
crates/bbnf-cli/src/
  main.rs
  app.rs
  config.rs
  commands/check.rs
  commands/parse.rs
  commands/path.rs
  commands/debug.rs
  commands/bench.rs
  commands/metadata.rs
  output/human.rs
  output/json.rs
  output/trace.rs
```

CLI commands must exercise runtime contracts, not implement alternate parsers. `parse` emits value/tape/diagnostic output, `path` validates metadata projections, `debug` steps tape traces, and `bench` delegates to `bbnf-bench`.

### `bbnf-language-server`

```text
crates/bbnf-language-server/src/
  main.rs
  lib.rs
  document/service.rs
  document/snapshot.rs
  document/edits.rs
  analysis/diagnostics.rs
  analysis/hover.rs
  analysis/completion.rs
  analysis/semantic_tokens.rs
  analysis/imports.rs
  incremental/anchors.rs
  incremental/reparse_plan.rs
  lsp/server.rs
  lsp/protocol.rs
  dap/server.rs
  dap/session.rs
  dap/mapping.rs
  workspace/metadata.rs
```

This keeps current analysis/LSP/DAP concepts but removes the old split across crates (`crates/analysis/src/lib.rs:1-13`, `crates/lsp/src/server/mod.rs:22-31`, `crates/lsp/src/dap/mod.rs:1-9`).

### `bbnf-bench`

```text
crates/bbnf-bench/src/
  lib.rs
  datasets.rs
  competitors.rs
  report.rs
  harness/parse.rs
  harness/path.rs
  harness/visitor.rs
  harness/incremental.rs
  harness/debug.rs
benches/
  parse_json.rs
  parse_css.rs
  path_select.rs
  visitors.rs
  incremental.rs
```

Bench reports must name competitor baselines per Lock 14 (`restart/locks/14-LOCKS.md:207`) and track the README SOTA synthesis (`restart/README.md:358-361`).

### `bbnf-path-core`, `bbnf-path`, `bbnf-path-ts`

```text
crates/bbnf-path-core/src/
  lib.rs
  ast.rs
  lexer.rs
  parser.rs
  lower.rs
  validate.rs
  diagnostics.rs
  runtime/cursor.rs
  runtime/selector.rs
  schema.rs

crates/bbnf-path/src/
  lib.rs
  path_macro.rs
  select_macro.rs
  diagnostics.rs

crates/bbnf-path-ts/src/
  lib.rs
  template_tag.rs
  schema.rs
  bindings.rs
```

The old hardcoded registry and TS duplicate compiler are removed (`crates/bbnf-path/src/registry.rs:80-98`, `crates/bbnf-path-ts/src/compile.rs:1-12`).

### `bbnf-test-fixtures`

```text
crates/bbnf-test-fixtures/src/
  lib.rs
  manifest.rs
  metadata.rs
  corpus.rs
  expect.rs
  harness.rs
fixtures/
  json/
  css/
  bbnf/
  sheets/
```

Fixture directories contain data and manifests. Rust harness code is grammar-agnostic, preserving Amendment 01's fixture rule (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:58-62`).

## §4 Performance gate posture

The bench harness should track:

| Dataset | Competitors / source | PASS-3 surface under test |
| --- | --- | --- |
| `twitter.json` | sonic-rs and simd-json microbench references (`restart/corpora/SOTA.md:54-56`) | borrowed parse, tape cursor, direct root |
| `citm_catalog.json` | sonic/simd-json fixture set (`restart/corpora/SOTA.md:54-56`) | object traversal and `path!` |
| `canada.json` | sonic/simd-json fixture set (`restart/corpora/SOTA.md:54-56`) | array-heavy parse and selector scan |
| CSS bootstrap/animate | lightningcss visitor/parser evidence (`restart/corpora/SOTA.md:103-118`, `restart/corpora/SOTA.md:134-136`) | generated visitors, layout, diagnostics |
| BBNF grammar corpus | Lock 14 full grammar generalization (`restart/locks/14-LOCKS.md:60`) | no-overfit parser generation |

Bench output must show parse mode (`borrowed`, `arena`, `owned`), trace enabled/disabled, incremental fallback rate, and competitor baseline.

## §5 Cross-pass hand-offs

PASS-1 supplies tape ABI, snapshot IDs, span maps, recovery/layout flags, and traversal primitives. PASS-2 emits runtime modules, metadata descriptors, visitors, path schemas, diagnostics, and host ABI glue. PASS-3 consumes both; it does not fork a second runtime substrate.

SYNTHESIS must resolve stale prompt references: PASS-3 prompt names `ParseStream` as keystone (`restart/prompts/PASS-3-RUNTIME.md:79-81`), while dispatch authority and Lock 1 keep tape. README line `restart/README.md:473` is stale relative to earlier settled README extension language (`restart/README.md:123`, `restart/README.md:133-143`).

## §6 Risk + mitigation

Risk: crate layout recreates old workspace sprawl. Mitigation: no per-grammar declaration crates, consolidated language server, data-only fixtures, and one shared path core.

Risk: CLI and playground invent alternate APIs. Mitigation: CLI/playground call the same `bbnf`, `bbnf-path-core`, and language-server APIs.

Risk: benchmarks become marketing. Mitigation: require competitor names and raw mode labels in every report.

## §7 Verdict

KEEP `bbnf`, consolidated language server, bench harness, grammar-agnostic fixtures, playground/extension consumers, and thin ecosystem wrappers. REINVENT CLI and path crate split around restart runtime contracts. DISCARD per-grammar declaration crates, hardcoded fixture registries, duplicated TS path compilers, and any ecosystem package that bypasses tape/direct runtime identity.
