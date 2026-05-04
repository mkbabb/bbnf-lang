# PASS-3 - User Surface + Runtime + Ecosystem Synthesis

## §0 Orchestrator scope and conflict resolution

This PASS-3 synthesis covers Phase 1 target PASS-3 only: user runtime surfaces, path/select DSL, visitors, tape/direct runtime union, error recovery, incremental parsing, LSP/DAP/playground/CLI ecosystem, fixtures, and benchmarks. The six sub-agent reports are:

- `restart/audit/pass-3-runtime/agent-1-value-api-designer.md`
- `restart/audit/pass-3-runtime/agent-2-path-select-dsl-designer.md`
- `restart/audit/pass-3-runtime/agent-3-visitor-surface-designer.md`
- `restart/audit/pass-3-runtime/agent-4-tape-union-architect.md`
- `restart/audit/pass-3-runtime/agent-5-error-recovery-incremental-parsing.md`
- `restart/audit/pass-3-runtime/agent-6-ecosystem-architect.md`

PASS-1 and PASS-2 syntheses are not consumed as authority in this Phase 1 parallel run by design. This synthesis read the PASS-1/PASS-2 prompts, README, locks, inheritance index, corpus references, archived plans, BB W4/W5 materials, cookbooks, and current source references, then records assumptions and hand-offs for SYNTHESIS.

Settled authority: tape is the substrate and is properly unioned with direct-to-struct. It must not be renamed to `ParseStream`. Columnar SoA and parallel substrates are dead. BBNF extensions in this PASS are lookbehind, `@host fn`, multi-function chaining, generics, `@error`, and `@layout`; rewrite-mode is out; grammar-level Unicode-class algebra is deferred to parse-that/regex. No per-grammar declaration crates exist by default.

Conflicts surfaced:

- `restart/prompts/PASS-3-RUNTIME.md:3` and `restart/prompts/PASS-3-RUNTIME.md:79-81` name `ParseStream` as the PASS-3 keystone. That is stale; PASS-3 uses tape.
- `restart/inheritance/INDEX.md:65-66` says tape naming dies and structural insight survives as `ParseStream`. That is stale; structural insight survives as tape/direct union.
- `restart/README.md:473` contains stale extension and `ParseStream` phrasing. It conflicts with earlier settled README lines: lookbehind only, rewrite rejected, Unicode handled in regex (`restart/README.md:123`, `restart/README.md:133-143`) and tape/direct union (`restart/README.md:285-314`).
- PASS-1/PASS-2 prompt clauses that mention rewrite-mode, Unicode grammar algebra, or `ParseStream` are stale hand-off risks, not blockers (`restart/prompts/PASS-1-SUBSTRATE.md:3`, `restart/prompts/PASS-1-SUBSTRATE.md:31`, `restart/prompts/PASS-1-SUBSTRATE.md:66`, `restart/prompts/PASS-2-CODEGEN.md:33`, `restart/prompts/PASS-2-CODEGEN.md:81`).

## §1 Verdict ledger

| Surface | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| Value API | Generated grammars expose typed roots, `DocumentView`, and untyped `ValueRef` over shared tape/direct identity. | Clear for normal users; powerful for tools. | Lifetimes can be intimidating. | Keep `parse(&str)` simple and route advanced cases to cookbook diagnostics. | KEEP / REINVENT |
| Parse constructors | `parse`, `parse_in`, and `parse_owned` implement Lock 9 slice-borrow/default plus explicit arena/owned modes (`restart/locks/14-LOCKS.md:50`). | Covers low allocation, batch, and durable use cases. | Owned mode can hide copies. | Bench all modes separately. | KEEP |
| Tape/direct union | Tape is the single advanced substrate; direct structs remain the ergonomic default (`restart/README.md:285-314`, `restart/locks/14-LOCKS.md:34`). | Unifies path, visitors, debug, LSP, and benchmarks. | Requires careful identity invariants. | Every visible direct node carries tape identity. | REINVENT |
| `ParseStream` | Stale replacement name in prompts/inheritance. | None under current authority. | Would fork naming and contracts. | Mention only as stale conflict. | DISCARD |
| Path/select DSL | `path!` and `select!` validate against generated metadata and lower to runtime traversal plans. | Strong diagnostics and static typing. | Current registry is overfit. | Replace hardcoded fixtures with metadata. | REINVENT |
| Visitors | Generated visitors plus `VisitTypes` bitflag pruning are the only mutation channel. | Matches W5 and lightningcss-style traversal. | Large generated APIs need restraint. | Expose simple prelude plus advanced modules. | KEEP |
| Error/recovery/layout | `@error`, `@recover`, and `@layout` feed generated diagnostics, formatter, and recovery nodes. | Better author control and LSP output. | Can become semantic-action sprawl. | Keep declarative; no rewrite-mode. | KEEP / REINVENT |
| Incremental parsing | LSP retains range edit application but parse state becomes snapshot-based with reparse plans. | Moves beyond full reparse. | Needs PASS-1 anchors. | Report fallback-to-full-parse rates. | REINVENT |
| Per-grammar declaration crates | Amendment 01 rejects them (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:13-22`). | None for default design. | Rare host escape may be needed. | Only metadata-reviewed host adapters. | DISCARD |
| Rewrite-mode / grammar Unicode algebra | Settled out or deferred to regex (`restart/README.md:123`, `restart/README.md:133-143`). | Reduces grammar/runtime scope. | Old prompt text lingers. | Remove from generated APIs and docs. | DISCARD |

## §2 User runtime commitments

The default user experience is generated typed roots:

```rust
let doc = Json::parse(source)?;
let value = doc.root();
```

Advanced users get arena and owned modes:

```rust
let doc = Json::parse_in(source, &arena)?;
let owned = Json::parse_owned(source.to_owned())?;
```

This follows the W4 lifetime surface and cookbook guidance: borrowed parse first, arena for batch/high-throughput, owned for retention (`docs/tranches/BB/waves/W4.md:66-74`, `docs/tranches/BB/waves/W4a.md:16-19`, `docs/cookbook/lifetime-surfaces.md:27-41`, `docs/cookbook/lifetime-surfaces.md:45-65`). Diagnostics must preserve the cookbook's clear lifetime/bumpalo troubleshooting posture (`docs/cookbook/lifetime-surfaces.md:69-89`, `docs/cookbook/lifetime-surfaces.md:93-105`, `docs/cookbook/lifetime-surfaces.md:109-139`).

The common runtime shape:

```rust
pub trait Grammar {
    type Root<'arena, 'input>;
    type OwnedRoot;

    fn parse(input: &str) -> Result<Self::Root<'_, '_>, ErrorBundle>;
    fn parse_in<'arena, 'input>(
        input: &'input str,
        arena: &'arena Arena,
    ) -> Result<Self::Root<'arena, 'input>, ErrorBundle>;
    fn parse_owned(input: impl Into<String>) -> Result<Self::OwnedRoot, ErrorBundle>;
}

pub trait DocumentView<'input> {
    fn source(&self) -> &'input str;
    fn root_value(&self) -> ValueRef<'_, 'input, RootKind>;
    fn diagnostics(&self) -> &[Diagnostic];
}
```

Direct structs and tape are not competing products. Direct roots are what normal users author against. Tape-backed `ValueRef` is the shared cursor for `path!`, `select!`, visitors, debugger, CLI projections, LSP features, and playground inspection. This resolves the old SOTA recommendation against tape (`restart/corpora/SOTA.md:205-214`) by applying Lock 1 and the current dispatch authority.

## §3 Path, select, and visitor commitments

`path!` and `select!` survive, but their implementation is rebuilt. The current `bbnf-path` code demonstrates the macro direction (`crates/bbnf-path/src/lib.rs:1-22`, `crates/bbnf-path/src/path_macro.rs:146-210`) while also exposing two dead ends: hardcoded grammar marker registries (`crates/bbnf-path/src/registry.rs:80-98`, `crates/bbnf-path/src/registry.rs:125-138`) and placeholder terminal typing (`crates/bbnf-path/src/path_macro.rs:198-199`). The TypeScript mirror duplicates compiler logic because the proc-macro crate cannot be consumed directly (`crates/bbnf-path-ts/src/compile.rs:1-12`, `crates/bbnf-path-ts/src/compile.rs:41-65`).

PASS-3 commitment:

- `bbnf-path-core` owns parsing, lowering, validation, diagnostics, metadata schema, and runtime plans.
- `bbnf-path` owns Rust proc macros.
- `bbnf-path-ts` owns TS template tags and schema bindings.
- Generated grammar metadata replaces fixture registries.
- Both explicit type suffixes and implicit terminal inference are supported, matching the W5 pointer decision (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:20-27`, `docs/tranches/BB/audit/W5-pointer-syntax-decision.md:148-156`).

Visitors keep the W5 design: generated `Visitor` traits, `Visit`/walker support, and `VisitTypes` bitflag pruning (`docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:11-19`, `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:107-123`, `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:127-174`). Mutation happens through visitors/edit builders only, as required by the README (`restart/README.md:318`). Cookbook examples for visitor collection, pruning, mutation, and warnings remain the documentation backbone (`docs/cookbook/visitors.md:102-124`, `docs/cookbook/visitors.md:153-165`, `docs/cookbook/visitors.md:177-188`, `docs/cookbook/visitors.md:248`).

## §4 Tape/direct runtime architecture

PASS-3 requires these substrate-visible semantics from PASS-1:

- Stable document/snapshot identity.
- Stable node-kind IDs shared with codegen metadata.
- Compact spans and payload references.
- Child/sibling traversal and skip ranges.
- Recovery/layout/debug flags.
- Optional trace events for DAP/playground.

Illustrative shape:

```rust
pub struct Tape<'input> {
    source: &'input str,
    tokens: Box<[TapeToken]>,
    payloads: PayloadArena,
    diagnostics: Box<[Diagnostic]>,
}

pub struct TapeToken {
    kind: NodeKindId,
    flags: NodeFlags,
    start: u32,
    end: u32,
    payload: u32,
    sibling_skip: u32,
}

pub struct ValueRef<'doc, 'input, K = AnyKind> {
    tape: &'doc Tape<'input>,
    index: u32,
    _kind: PhantomData<K>,
}
```

This layout is not a PASS-1 mandate; it is a user-surface contract. PASS-1 may pack differently if these semantics remain true. PASS-2 may build direct structs first or tape first per grammar, but the externally visible invariant is stable: every public node has tape identity and every tape node can be projected through `ValueRef`.

Debug and DAP should reuse this identity. Existing DAP code already contains sessions, breakpoints, and stepping (`crates/lsp/src/dap/mod.rs:45-83`, `crates/lsp/src/dap/mod.rs:121-143`, `crates/lsp/src/dap/mapping.rs:41-92`), and the playground wasm API already exposes parse/LSP/debug concepts (`playground/src/composables/wasm/index.ts:233-256`, `playground/src/composables/wasm/index.ts:274-322`, `playground/src/composables/wasm/types.ts:166-183`). Restart should keep those user concepts while moving their internals onto tape snapshots.

## §5 Error recovery, incremental parsing, LSP, and DAP

`@error`, `@recover`, and `@layout` are in. Existing code already has recovery directive extraction and hover docs (`crates/analysis/src/directives/recover.rs:10-37`, `crates/analysis/src/directives/recover.rs:39-77`, `crates/analysis/src/features/hover/directive.rs:22-44`). PASS-3 keeps the authoring intent and rebuilds implementation on generated metadata.

Current LSP applies incremental text edits but reparses/reanalyzes the full document after updates (`crates/lsp/src/server/protocol.rs:82-109`, `crates/lsp/src/server/mod.rs:56-80`, `crates/analysis/src/state/mod.rs:55-83`). That is a useful entry shape, not the final incremental parse design. Restart target:

```rust
pub struct DocumentSnapshot {
    id: SnapshotId,
    text: Rope,
    tape: TapeOwned,
    diagnostics: DiagnosticSet,
    semantic: SemanticIndex,
}

pub enum ReparsePlan {
    Reuse { unchanged: Vec<TapeRange> },
    Reparse { dirty: Vec<TextRange>, anchors: Vec<TapeId> },
}
```

The server may fall back to full parse when anchors fail, but bench/dev output must report fallback rates. Users should see stable diagnostics, not implementation warnings. The language server should consolidate current analysis, LSP, and DAP surfaces, matching archived PASS-C guidance (`restart-archive-2026-05-04/audit/passes/PASS-C.md:90-92`, `restart-archive-2026-05-04/audit/passes/PASS-C.md:158-159`).

## §6 Crate and module tree

`bbnf`:

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
  metadata.rs
```

`bbnf-cli`:

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

`bbnf-language-server`:

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

`bbnf-bench`:

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

`bbnf-path-core`, `bbnf-path`, `bbnf-path-ts`:

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

`bbnf-test-fixtures`:

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

Fixture directories are data and manifests only. Rust fixture code is grammar-agnostic, preserving Amendment 01 (`restart-archive-2026-05-04/audit/master-plan/AMENDMENT-01-NO-PER-GRAMMAR-CRATES.md:58-62`) and Lock 14 no-overfit pressure (`restart/locks/14-LOCKS.md:60`).

## §7 Benchmark and SOTA gates

Every perf gate names a competitor per Lock 14 (`restart/locks/14-LOCKS.md:207`). The SOTA corpus compares sonic-rs, simdjson/simd-json, lightningcss, and tree-sitter style tradeoffs (`restart/corpora/SOTA.md:12-16`, `restart/corpora/SOTA.md:35-42`, `restart/corpora/SOTA.md:64-77`, `restart/corpora/SOTA.md:103-118`). PASS-3 gates must report parse mode, trace mode, and surface under test.

| Dataset | Baseline citation | PASS-3 gate |
| --- | --- | --- |
| `twitter.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | borrowed parse, tape cursor, direct root |
| `citm_catalog.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | object traversal and `path!` |
| `canada.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | selector scan and array-heavy parse |
| CSS bootstrap/animate | lightningcss visitor and perf evidence (`restart/corpora/SOTA.md:103-118`, `restart/corpora/SOTA.md:134-136`) | generated visitor pruning and layout metadata |
| BBNF grammar corpus | restart full-grammar generalization (`restart/locks/14-LOCKS.md:60`) | no per-grammar overfit |

PASS-3 recommends bench reports include: borrowed/arena/owned timings, tape/direct projection timings, `path!` and `select!` traversal timings, visitor pruning win/loss, incremental fallback rate, and DAP trace overhead.

## §8 Cross-pass hand-offs

To PASS-1:

- Define tape token packing, payload arenas, span widths, child/sibling traversal, recovery/layout/debug flags, and snapshot identity.
- Keep tape as the substrate name and contract; do not expose `ParseStream`.
- Avoid columnar SoA and parallel substrates.
- Provide enough anchor identity for incremental parse reuse and DAP snapshots.

To PASS-2:

- Emit typed roots, three parse constructors, `DocumentView`, `ValueRef` projections, generated visitors, `VisitTypes`, recovery/error/layout metadata, path schemas, host-function metadata, and fixture/bench metadata.
- Do not emit per-grammar declaration crates by default.
- Do not generate rewrite-mode hooks or grammar-level Unicode class algebra APIs.
- Ensure direct structs and tape round-trip identity is tested.

To SYNTHESIS:

- Normalize stale prompt text across PASS-1/PASS-2/PASS-3.
- Decide final crate names if the restart workspace chooses unprefixed `path`/`path-core` module names versus crate package names like `bbnf-path-core`.
- Reconcile old PASS-C CLI deferral notes with the current PASS-3 top-layer ownership.
- Ensure performance gates are integrated with PASS-1/PASS-2 outputs before implementation sequencing.

## §9 KEEP / REINVENT / DISCARD summary

KEEP:

- `parse`, `parse_in`, `parse_owned`.
- Typed generated roots and grammar-specific visitors.
- Untyped `ValueRef`/generic value for tools.
- `path!`, `select!`, explicit and implicit pointer forms.
- `@host fn`, multi-function chaining, generics, `@error`, `@recover`, `@layout`.
- Consolidated language server with LSP and DAP.
- Data-only grammar fixtures and competitor-named benches.

REINVENT:

- Tape as the single advanced substrate unioned with direct-to-struct.
- Current path registries into generated metadata validation.
- Current TypeScript path compiler into schema bindings over shared core.
- Current full-reparse document state into snapshot-based incremental parse.
- CLI as a focused runtime exercise surface.
- Visitor cohorts and diagnostics around restart metadata.

DISCARD:

- Public `ParseStream` name.
- Columnar SoA and parallel substrates.
- Per-grammar declaration crates by default.
- Rewrite-mode.
- Grammar-level Unicode-class algebra.
- Fixture-bound hardcoded registries.
- `TypedPath<..., ()>` terminal placeholders.
- Arbitrary mutable `Value` setters outside visitor/edit-builder flows.

## §10 Unresolved punch-list

1. SYNTHESIS must reconcile stale `ParseStream` and extension clauses in prompt/README/inheritance materials.
2. PASS-1 must confirm tape ABI, node IDs, span widths, snapshot IDs, and incremental anchors.
3. PASS-2 must confirm generated metadata schema for paths, visitors, host functions, diagnostics, and fixtures.
4. Final workspace naming must choose package names for `path`, `path-core`, and `path-ts`.
5. Bench harness must codify exact target numbers and machine profiles after PASS-1/PASS-2 designs land.
6. Rare host adapter escape-valve policy must be written so it cannot become per-grammar declaration crates by another name.

## §11 Final posture

PASS-3 keeps the user-facing promise simple: generated grammars parse into typed roots, tools can project through a common value/tape cursor, visitors own mutation, paths are validated against generated metadata, language tooling is snapshot-aware, and benchmarks name the competitors they claim to beat. The architectural guardrail is equally simple: one tape/direct runtime identity, no `ParseStream` rename, no rewrite-mode, no grammar-level Unicode algebra, and no default per-grammar declaration crates.
