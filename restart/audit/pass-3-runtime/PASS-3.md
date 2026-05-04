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
| Path/select DSL | `pointer!` and `select!` validate against generated metadata and lower to runtime traversal plans. | Strong diagnostics and static typing. | Current registry is overfit. | Replace hardcoded fixtures with metadata. | REINVENT |
| Visitors | Generated visitors plus `VisitTypes` bitflag pruning are the only mutation channel. | Matches W5 and lightningcss-style traversal. | Large generated APIs need restraint. | Expose simple prelude plus advanced modules. | KEEP |
| Error/recovery/layout | `@error(recover = ...)` and `@layout` feed generated diagnostics, formatter, and recovery nodes. Legacy `@recover` may parse only as an alias during migration. | Better author control and LSP output. | Can become semantic-action sprawl. | Keep declarative; no rewrite-mode. | KEEP / REINVENT |
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

Direct structs and tape are not competing products. Direct roots are what normal users author against. Tape-backed `ValueRef` is the shared cursor for `pointer!`, `select!`, visitors, debugger, CLI projections, LSP features, and playground inspection. This resolves the old SOTA recommendation against tape (`restart/corpora/SOTA.md:205-214`) by applying Lock 1 and the current dispatch authority.

## §3 Path, select, and visitor commitments

`pointer!` and `select!` survive, but their implementation is rebuilt. The current `bbnf-path` code demonstrates the macro direction (`crates/bbnf-path/src/lib.rs:1-22`, `crates/bbnf-path/src/path_macro.rs:146-210`) while also exposing two dead ends: hardcoded grammar marker registries (`crates/bbnf-path/src/registry.rs:80-98`, `crates/bbnf-path/src/registry.rs:125-138`) and placeholder terminal typing (`crates/bbnf-path/src/path_macro.rs:198-199`). The TypeScript mirror duplicates compiler logic because the proc-macro crate cannot be consumed directly (`crates/bbnf-path-ts/src/compile.rs:1-12`, `crates/bbnf-path-ts/src/compile.rs:41-65`). These citations are legacy evidence only; restart package names are `path`, `path-core`, and `path-ts`.

PASS-3 commitment:

- `path-core` owns parsing, lowering, validation, diagnostics, metadata schema, and runtime plans.
- `path` owns Rust proc macros: `pointer!` and `select!`.
- `path-ts` owns TS template tags and schema bindings.
- Generated grammar metadata replaces fixture registries.
- Both explicit type suffixes and implicit terminal inference are supported, matching the W5 pointer decision (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:20-27`, `docs/tranches/BB/audit/W5-pointer-syntax-decision.md:148-156`).

**Registry deletion close gate.** Hardcoded grammar marker registries are not a deferral; they are a deletion item. Final close fails unless the following all hold:

```text
rg -n 'GRAMMAR_PATH_REGISTRY|GrammarMarkerRegistry|hardcoded_grammar_registry' \
   crates/path crates/path-core crates/path-ts crates/bbnf crates/codegen \
   crates/runtime crates/ir
=> zero outside generated data
```

Generated metadata produced by xtask-emitted descriptors is the only validation surface for `pointer!` and `select!`. Owner: PASS-3 amendment in coordination with SYNTHESIS deletion-gate ledger.

**Consumer acceptance gates.** PASS-3 surfaces are not closeable on prose-only hand-offs from PASS-2. The emission contract from PASS-2 must satisfy three executable consumer gates before PASS-3 closes:

| Consumer gate | Surface under test | Acceptance criterion |
|---|---|---|
| Emitted parse signatures compile under PASS-3 wrappers | `parse`, `parse_in`, `parse_owned` for every extant grammar plus yaml | Generated runtime crate compiles when `Json::parse(&str)`, `Json::parse_in(&str, &Arena)`, and `Json::parse_owned(String)` are called from a PASS-3 consumer smoke. |
| `DocumentView` metadata feeds visitors and selectors | `DocumentView::root_value`, `DocumentView::diagnostics`, generated `Visitor` trait, `pointer!`, `select!` | Generated `DocumentView::root_value()` projects to the same `ValueRef` index space the visitor walker, `pointer!` runtime plan, and `select!` traversal plan consume. |
| Materialisation cost tables generated and documented | Per-grammar runtime emission | Codegen emits a `materialisation_cost.toml` (or equivalent generated artefact) with field counts, payload arena bytes, and tape-token width per node kind; the cookbook references it. |

These gates appear as receiver/blocker/receiving-gate rows in §8.

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

`@error(recover = ...)` and `@layout` are in. Existing code already has recovery directive extraction and hover docs (`crates/analysis/src/directives/recover.rs:10-37`, `crates/analysis/src/directives/recover.rs:39-77`, `crates/analysis/src/features/hover/directive.rs:22-44`). PASS-3 keeps the authoring intent and rebuilds implementation on generated metadata. A standalone `@recover` token is a legacy alias only if SYNTHESIS keeps a migration parser; it is not a new V1 extension.

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

**Fallback-rate gates by dataset.** PASS-3 binds the incremental parse contract to dataset-level thresholds. A miss does not merely log; it blocks bench close.

| Edit corpus | Snapshot reuse target | Fallback-rate ceiling | Surface |
|---|---|---|---|
| JSON edit corpus (twitter, citm, canada with synthesized point edits) | >= 85 percent of token spans reused | <= 5 percent full-reparse fallback | `incremental/edit_anchor` bench. |
| CSS edit corpus (bootstrap, animate with selector/property edits) | >= 75 percent of rule spans reused | <= 10 percent full-reparse fallback | `incremental/edit_anchor` bench. |
| BBNF self-edit corpus (grammar source point edits) | >= 70 percent of rule spans reused | <= 15 percent full-reparse fallback | `incremental/edit_anchor` bench. |
| Large-paste corpus (10x-source paste) | reuse target N/A; full-reparse expected | report only; not a regression gate | `incremental/edit_anchor` bench. |

**LSP user-facing output policy.** Fallbacks are diagnostic noise to the user; they are signal to the engineer. Default LSP output is silent on fallback. A debug-only diagnostic channel reports fallback events with snapshot id and reason; the channel is disabled in shipped builds and enabled in development with `BBNF_LSP_DEBUG=1`. Fallback ledgers are written to bench output, never to LSP `Diagnostic` items, never to a `showMessage` notification. The HARDENING-CONSOLIDATED §4.36 policy "keep user-facing LSP output quiet unless policy says otherwise" is normative.

## §6 Crate and module tree

`bbnf`:

```text
crates/bbnf/src/
  lib.rs
  prelude.rs
  parse/
  document/
  query/
  visitor/
  diagnostics/
  metadata/
```

The public crate carries exactly 8 immediate children — `lib.rs`, `prelude.rs`, `parse/` (entry constructors), `document/` (root + view), `query/` (`pointer!`/`select!` adapters), `visitor/` (generated traits + `VisitTypes` pruning), `diagnostics/` (rendering + categories), and `metadata/` (workspace-metadata access, host route surface, layout descriptors) — per HARDENING-CONSOLIDATED §4.19 and Lock 13's 4-10 child-count rule. The substrate cursor (`tape/`) and the typed-root projection (`value/`) live under `runtime/src/tape/` and `runtime/src/value/` per Lock 1; the aggregator re-exports them through `prelude.rs` rather than carrying duplicate sibling directories. The grammar-specific surface (`Json`, `CssL4`, etc.) is generated under per-grammar runtime crates referenced from `metadata/`, not as a sibling directory in `bbnf/src/`. Host-function bindings, layout lowering hooks, and metadata sidecars all live under `metadata/`.

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

`path-core`, `path`, `path-ts`:

```text
crates/path-core/src/
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

crates/path/src/
  lib.rs
  pointer_macro.rs
  select_macro.rs
  diagnostics.rs

crates/path-ts/src/
  lib.rs
  template_tag.rs
  schema.rs
  bindings.rs
```

`test-fixtures`:

```text
crates/test-fixtures/src/
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

**Fixture separation.** Lock 14 onboarding admits exactly two surfaces: the grammar source file (`yaml.bbnf`) and one workspace-metadata block (`[workspace.metadata.bbnf.grammars.yaml]`). `fixtures/yaml/*` is *not* part of the onboarding allowance. The four `fixtures/<grammar>/` directories above are post-onboarding parity evidence — they appear after a grammar's runtime emission, visitor, `pointer!`/`select!` adapters, and host route are already proven. Adding yaml therefore proceeds in two phases:

1. **Onboarding phase** — `yaml.bbnf` plus the metadata block. Generated value API, `pointer!`, `select!`, visitor, host route, and bench manifest must all appear from metadata + codegen with zero Rust edits and zero entries under `fixtures/yaml/`.
2. **Parity phase** (separate gate) — `fixtures/yaml/` data files and a `fixtures/yaml/manifest.toml` may be added later to feed the parity bench cohort. This is a cookbook-level gate, never an onboarding gate.

The grep gate `rg -n 'fixtures/yaml' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` must return zero hits inside Lock 14 onboarding allowances; matches are confined to parity-phase prose.

## §6a Per-grammar feeder table

PASS-3 owns the column-feeder rows for the SYNTHESIS-owned 10x9 per-grammar table. Each row records the user-runtime surface PASS-3 must emit per grammar; SYNTHESIS composes these with PASS-1 substrate and PASS-2 emission columns to assemble the architecture-level table.

| Grammar | Typed root | `ValueRef` kind | Generated runtime files | Visitor + `VisitTypes` | Path schema | Fixture manifest | Host route |
|---|---|---|---|---|---|---|---|
| `bbnf` | `Bbnf` | `BbnfRoot` | `generated.rs`, `parser.rs`, `host.rs` | `BbnfVisitor`, `BbnfVisitTypes` | `bbnf.path-schema.toml` | `fixtures/bbnf/manifest.toml` | self-host primitives plus regen utilities |
| `bnf` | `Bnf` | `BnfRoot` | `generated.rs`, `parser.rs` | `BnfVisitor`, `BnfVisitTypes` | `bnf.path-schema.toml` | `fixtures/bnf/manifest.toml` | none (pure recogniser) |
| `csv` | `Csv` | `CsvRoot` | `generated.rs`, `parser.rs` | `CsvVisitor`, `CsvVisitTypes` | `csv.path-schema.toml` | `fixtures/csv/manifest.toml` | none |
| `css_l4` | `CssL4` | `CssL4Root` | `generated.rs`, `parser.rs`, `host.rs`, `layout.rs` | `CssL4Visitor`, `CssL4VisitTypes` | `css_l4.path-schema.toml` | `fixtures/css/manifest.toml` | colour-function host primitives plus length conversion |
| `css_pretty` | `CssPretty` | `CssPrettyRoot` | `generated.rs`, `parser.rs`, `layout.rs` | `CssPrettyVisitor`, `CssPrettyVisitTypes` | `css_pretty.path-schema.toml` | shares `fixtures/css/` corpus | none |
| `ebnf` | `Ebnf` | `EbnfRoot` | `generated.rs`, `parser.rs` | `EbnfVisitor`, `EbnfVisitTypes` | `ebnf.path-schema.toml` | `fixtures/ebnf/manifest.toml` | none |
| `google_sheets` | `GoogleSheets` | `GoogleSheetsRoot` | `generated.rs`, `parser.rs`, `host.rs` | `GoogleSheetsVisitor`, `GoogleSheetsVisitTypes` | `google_sheets.path-schema.toml` | `fixtures/sheets/manifest.toml` | range/date/array-literal host primitives |
| `json` | `Json` | `JsonRoot` | `generated.rs`, `parser.rs` | `JsonVisitor`, `JsonVisitTypes` | `json.path-schema.toml` | `fixtures/json/manifest.toml` | none |
| `math` | `Math` | `MathRoot` | `generated.rs`, `parser.rs` | `MathVisitor`, `MathVisitTypes` | `math.path-schema.toml` | `fixtures/math/manifest.toml` | none (Pratt-eligible operator chain only) |
| `yaml` (onboarding proof) | `Yaml` | `YamlRoot` | `generated.rs`, `parser.rs`, `host.rs` (if metadata declares host route) | `YamlVisitor`, `YamlVisitTypes` | `yaml.path-schema.toml` | parity-phase `fixtures/yaml/manifest.toml` | as declared in `[workspace.metadata.bbnf.grammars.yaml]` |

The yaml row exists at the onboarding boundary: every cell to the left of the parity-phase fixture manifest must be generated from `yaml.bbnf` plus the workspace-metadata block, with zero Rust edits and zero per-grammar match arms in any generic crate.

## §6b Compiler diagnostic ledger

PASS-3 owns the user-facing diagnostic strings for runtime, pointer/select, lifetime, layout, optimizer, host, and yaml-onboarding surfaces. Strings are committed verbatim; later prose may not soften them. Each diagnostic carries a stable code, the target user, the mental model the user holds at the point of failure, the confusion point the message resolves, and the artefact that closes the loop.

| Code | Verbatim text | Target user | Mental model | Confusion point | Artefact |
|---|---|---|---|---|---|
| `BBNF-LIFE001` | `error[BBNF-LIFE001]: borrowed value escapes parse scope; the source string `&str` was dropped before this projection. help: use `Json::parse_owned(input)` to retain the data, or hold `&input` alive for the duration of `doc`.` | Application author | "I parsed once and stored the result." | Default `parse(&str)` borrows. | Cookbook §lifetime-surfaces. |
| `BBNF-LIFE002` | `error[BBNF-LIFE002]: arena mismatch; root was parsed in arena #N but projected through arena #M. help: use the same `&Arena` for parse and projection.` | Arena user | "I'm batching parses through one bumpalo." | Two arenas in scope. | Cookbook arena chapter. |
| `BBNF-LAYOUT001` | `warning[BBNF-LAYOUT001]: @layout directive is unused by generated formatter; rule never reaches a layout-sensitive emit path.` | Grammar author | "@layout always shapes output." | Rule has no emitting use. | Layout cookbook. |
| `BBNF-LAYOUT002` | `error[BBNF-LAYOUT002]: rule `{rule}` has no resolvable layout; reason: {cause}. help: layout descriptors must derive from a leaf, an explicit `@layout(...)`, or an upstream rule with a known layout.` | Grammar author | "Lowering finds layout from context." | Layout chain underdetermined. | Layout cookbook §unresolved-layout. |
| `BBNF-OPT001` | `note[BBNF-OPT001]: Pratt was not applied to `{rule}`; reason: {cause}. The grammar still parses; performance fallback uses recursive-descent.` | Grammar author | "Auto-Pratt always fires for operator chains." | Cost model declined. | Cookbook §pratt-detection. |
| `BBNF-OPT002` | `note[BBNF-OPT002]: SIMD scanner was not selected for `{rule}`; reason: {cause}. The grammar still parses; performance fallback uses scalar scan.` | Grammar author | "SIMD is always faster." | Dispatch cost outweighs win. | Cookbook §simd-detection. |
| `BBNF-GRAMMAR001` | `error[BBNF-GRAMMAR001]: workspace metadata block missing for grammar `{name}`. help: add `[workspace.metadata.bbnf.grammars.{name}]` to your Cargo workspace metadata; the grammar source file alone is not sufficient.` | New-grammar author | "Source file is enough." | Lock 14 requires both surfaces. | Onboarding cookbook §two-surfaces. |
| `BBNF-POINTER001` | `error[BBNF-POINTER001]: unknown pointer segment `{segment}` in `{pointer_macro_input}`; rule has no field with that name.` | Application author | "Pointers traverse fields by name." | Field name typo or stale. | Pointer cookbook §validation. |
| `BBNF-POINTER002` | `error[BBNF-POINTER002]: pointer grammar inference failed; help: add an explicit grammar prefix like `pointer!(Json => "/...")`.` | Application author | "Implicit grammar always works." | Two grammars in scope. | Pointer cookbook §explicit-grammar. |
| `BBNF-POINTER003` | `error[BBNF-POINTER003]: terminal type for pointer `{path}` is not yet known to the macro; help: regenerate with `cargo xtask regen` so the schema is in sync.` | Application author | "Macro reads metadata at compile time." | Stale generated schema. | Pointer cookbook §regen. |
| `LookbehindWidth` | `error[BBNF-LIFE003]: lookbehind `|<` width is unbounded for `{rule}`; help: lookbehinds must be finite-width; use a bounded alternative or move the constraint into a regex with `(?<=...)`.` | Grammar author | "Lookbehind takes any pattern." | Unbounded width. | Grammar surface spec. |
| `HostSignature` | `error[BBNF-HOST001]: host function `{name}` cannot satisfy signature `{expected}`; argument {index} inferred `{actual}` at {span}.` | Host author | "@host fn body just runs." | Type flow mismatch. | Host cookbook §signatures. |
| `ChainStep` | `error[BBNF-HOST002]: chain step `{step}` does not accept `{input_type}` from previous step; the chain `-> f1 -> f2` requires `f2` to accept `f1`'s output.` | Host author | "Chains compose." | Step type fault. | Host cookbook §chains. |
| `WasmHost` | `error[BBNF-HOST003]: host chain `{chain}` cannot lower to WASM; reason: {cause}. The Rust backend continues to compile.` | TS/WASM author | "Hosts work everywhere." | Host primitive missing in WASM ABI. | WASM ABI cookbook. |
| `LowererImport` | `error[BBNF-CG001]: lowerer at `{path}` imports `bbnf_ir::grammar_ir`; only the BIR producer may consume Grammar IR. help: lower against `bbnf_ir::backend_ir`.` | Codegen author | "All IR is one IR." | Two-IR contract violation. | Architecture §7. |

These strings are part of the SYNTHESIS-owned diagnostic ledger; PASS-3 commits the text and cookbook receivers, the lowerer-import-deny code is mirrored from PASS-2 ownership, and the layout/lookbehind/host/chain codes are mirrored from PASS-1 ownership.

## §7 Benchmark and SOTA gates

Every perf gate names a competitor per Lock 14 (`restart/locks/14-LOCKS.md:207`). The SOTA corpus compares sonic-rs, simdjson/simd-json, lightningcss, and tree-sitter style tradeoffs (`restart/corpora/SOTA.md:12-16`, `restart/corpora/SOTA.md:35-42`, `restart/corpora/SOTA.md:64-77`, `restart/corpora/SOTA.md:103-118`). PASS-3 gates must report parse mode, trace mode, and surface under test.

| Dataset | Baseline citation | PASS-3 gate |
| --- | --- | --- |
| `twitter.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | borrowed parse, tape cursor, direct root |
| `citm_catalog.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | object traversal and `pointer!` |
| `canada.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | selector scan and array-heavy parse |
| CSS bootstrap/animate | lightningcss visitor and perf evidence (`restart/corpora/SOTA.md:103-118`, `restart/corpora/SOTA.md:134-136`) | generated visitor pruning and layout metadata |
| BBNF grammar corpus | restart full-grammar generalization (`restart/locks/14-LOCKS.md:60`) | no per-grammar overfit |

PASS-3 recommends bench reports include: borrowed/arena/owned timings, tape/direct projection timings, `pointer!` and `select!` traversal timings, visitor pruning win/loss, incremental fallback rate, and DAP trace overhead.

Exact PASS-3 benchmark rows:

| Row | Target | Surface under test |
|---|---|---|
| `json/twitter/borrowed` | <= 380us | `parse(&str)` plus direct root. |
| `json/twitter/tape_cursor` | <= borrowed + 10% | `ValueRef` cursor projection. |
| `json/citm/pointer` | <= 750us parse target plus reported selector time | `pointer!` object traversal. |
| `json/canada/array_scan` | <= 2.8ms | array-heavy parse and selector scan. |
| `css/bootstrap/visitor` | <= 3.0ms | generated visitor pruning over CSS. |
| `css/animate/layout` | <= 1.6ms | layout metadata plus parser surface. |
| `bbnf/self_host/internal` | <= 100 ms full self-parse + format roundtrip; non-Lock-8 internal gate; no SOTA peer claim attaches. | BBNF grammar parses itself through the public runtime. |
| `incremental/edit_anchor` | report fallback rate | LSP edit reparse plan. |
| `debug/trace_overhead` | report overhead | DAP/playground trace projection. |

Generated API budget:

| Surface | Budget gate |
|---|---|
| Visitor traits | Per-grammar generated visitor LOC reported separately; no handwritten visitor file over 500 LOC; per-grammar visitor LOC delta beyond W3 baseline carries a +2 percent ceiling per regen. |
| Path metadata (Rust) | Generated schema rows are counted against grammar runtime budget; `path-core` handwritten files obey Lock 13; per-grammar path-schema Rust budget <= 32 KB. |
| Path metadata (sidecar) | Generated `*.path-schema.toml` sidecar size <= 64 KB per grammar; bench manifest sidecar <= 8 KB per grammar. |
| Tape projections | Generated projection LOC counted with runtime module budget; per-grammar projection delta beyond W3 baseline carries a +2 percent ceiling per regen. |
| Tape identity field/method delta | Adding a tape identity field or `ValueRef` method costs <= 1 field plus 2 methods per regen; larger deltas open a named amendment. |
| Bench-report generation | Per-grammar bench-report markdown <= 16 KB; per-grammar bench-report JSON <= 8 KB; aggregate bench summary <= 64 KB. |
| Regen wall budget | `cargo xtask regen --check` <= 12 s on M1 Pro for the nine extant grammars; <= 14 s including yaml; over-budget regen blocks close. |
| Diagnostics | Generated code list is data; diagnostic rendering code remains shared and non-generated. |

## §8 Cross-pass hand-offs

| Contract | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Tape token packing, payload arenas, span widths, child/sibling traversal, recovery/layout/debug flags, and snapshot identity. | PASS-1 / Tranche B | PASS-3 cannot prove cursor, visitor, incremental, or DAP identity. | Runtime identity tests over direct root and `ValueRef`. |
| Tape as the substrate name; no public `ParseStream`. | PASS-1 / SYNTHESIS | Naming fork leaks into public APIs. | Conflict guard for `ParseStream` in public docs and code. |
| Typed roots, three parse constructors, `DocumentView`, `ValueRef`, visitors, `VisitTypes`, diagnostic metadata, path schemas, host metadata, and fixture/bench metadata. | PASS-2 / Tranche F | Generated runtime lacks consumer-facing metadata. | PASS-3 consumer smokes from generated runtime. |
| Consumer acceptance: emitted parse signatures compile under PASS-3 wrappers, `DocumentView` metadata feeds visitors and selectors, materialisation cost tables generated and documented. | PASS-2 / Tranche F + Tranche I | PASS-3 close on prose-only hand-off. | Three executable consumer gates pass on every extant grammar plus yaml. |
| No per-grammar declaration crates, rewrite-mode hooks, or grammar Unicode algebra APIs. | PASS-2 / SYNTHESIS | Generated surfaces reintroduce discarded extension scope. | Negative API and parser fixtures. |
| Final crate names: `path`, `path-core`, `path-ts`, and `test-fixtures`. | SYNTHESIS / Tranche A | Legacy package names survive into greenfield docs. | Workspace crate-name check. |
| Hardcoded grammar registry deletion. | SYNTHESIS / Tranche I close gate | Registry survives parallel to metadata. | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry' crates/` returns zero outside generated data. |
| CLI/LSP/DAP ownership. | SYNTHESIS / Tranche I | Old PASS-C CLI deferral leaves top-layer gap. | CLI and LSP diagnostics parity test. |
| Performance rows integrated with PASS-1/PASS-2 outputs. | SYNTHESIS / Tranche H/J | Bench gates become narrative only. | Exact benchmark rows above appear in master plan gates. |
| Incremental fallback gates by dataset and LSP user-facing silence policy. | PASS-1 / Tranche I | Fallbacks become an unreported workaround. | Dataset-level fallback ledger + LSP policy enforcement test. |
| Per-grammar feeder rows for typed root, `ValueRef`, runtime files, visitor, path schema, fixture manifest, host route. | SYNTHESIS / Architecture per-X table | All-grammar claims fall to prose. | 10-row table consumed verbatim by Architecture; columns match SYNTHESIS schema. |
| Compiler diagnostic ledger with committed strings. | SYNTHESIS + cookbook receivers | Diagnostics drift between PASS, cookbook, and runtime. | Every code in §6b appears in cookbook table-of-contents and runtime emit tests. |

## §9 KEEP / REINVENT / DISCARD summary

KEEP:

- `parse`, `parse_in`, `parse_owned`.
- Typed generated roots and grammar-specific visitors.
- Untyped `ValueRef`/generic value for tools.
- `pointer!`, `select!`, explicit and implicit pointer forms.
- `@host fn`, multi-function chaining, generics, `@error(recover = ...)`, `@layout`.
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

| Carry | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Stale `ParseStream` and extension clauses in prompt/README/inheritance materials. | SYNTHESIS input-normalization table | Stale text propagates into greenfield docs. | `rg -n 'ParseStream\|rewrite-mode\|Unicode class algebra'` against the SYNTHESIS trio classifies every match as stale-input or deleted surface. |
| Tape ABI: node IDs, span widths, snapshot IDs, and incremental anchors. | PASS-1 / Tranche B | PASS-3 cursor and visitor identity unproven. | PASS-1 publishes the ABI table; PASS-3 binds against it in identity tests. |
| Generated metadata schema for paths, visitors, host functions, diagnostics, and fixtures. | PASS-2 / Tranche F | PASS-3 cannot validate `pointer!`/`select!` or visitors at compile time. | Schema is enumerated and PASS-3 macros consume it without fixture registries. |
| Workspace naming: `path`, `path-core`, `path-ts`, `test-fixtures`. | SYNTHESIS / Tranche A | Prefixed names re-leak into greenfield. | `rg -n 'bbnf-path\|bbnf-test-fixtures' restart/` returns zero outside deletion archaeology. |
| Bench harness target numbers and machine profiles. | SYNTHESIS / Tranche H/J | Bench rows become aspirational. | Master/Architecture inline competitor + dataset + platform + bbnf number for every row. |
| Rare host adapter escape-valve policy. | SYNTHESIS / Architecture rare-escape form | Per-grammar declaration crates re-enter through naming. | Review form requires reason, owner, why metadata + `@host fn` fail, declaration location, deletion path, reviewer, receiving gate. |
| Hardcoded grammar registry deletion. | PASS-3 amendment + SYNTHESIS deletion-gate ledger | Registry survives parallel to metadata route. | `rg` close gate returns zero outside generated data. |
| Consumer acceptance gates for PASS-2 emission contract. | PASS-2 / Tranche F + Tranche I | PASS-3 closes on prose only. | Three executable consumer gates pass on every extant grammar plus yaml. |
| Diagnostic ledger committed strings. | SYNTHESIS + cookbook receivers | Strings drift across docs. | Every code in §6b appears verbatim in cookbook + runtime emit tests. |
| Per-grammar feeder rows. | SYNTHESIS / Architecture per-X table | "All grammars" claims rest on prose. | Architecture consumes the §6a table verbatim. |
| Fixture separation from Lock 14 onboarding. | SYNTHESIS / Architecture Lock 14 proof | yaml fixtures re-enter onboarding allowance. | `rg -n 'fixtures/yaml' restart/` returns zero hits inside Lock 14 onboarding allowance. |
| Incremental fallback dataset gates and LSP silence policy. | PASS-1 / Tranche I | Fallback becomes unreported workaround. | Dataset thresholds + LSP policy test + bench ledger row. |

## §11 Final posture

PASS-3 keeps the user-facing promise simple: generated grammars parse into typed roots, tools can project through a common value/tape cursor, visitors own mutation, paths are validated against generated metadata, language tooling is snapshot-aware, and benchmarks name the competitors they claim to beat. The architectural guardrail is equally simple: one tape/direct runtime identity, no `ParseStream` rename, no rewrite-mode, no grammar-level Unicode algebra, and no default per-grammar declaration crates.
