# PASS-3 - User Surface + Runtime + Ecosystem Synthesis

## §0 Orchestrator scope and conflict resolution

This PASS-3 synthesis covers Phase 1 target PASS-3 only: user runtime surfaces, path/select DSL, visitors, tape/direct runtime union, error recovery, incremental parsing, LSP/DAP/playground/CLI ecosystem, fixtures, and benchmarks. The six sub-agent reports are:

- `restart/audit/pass-3-runtime/agent-1-value-api-designer.md`
- `restart/audit/pass-3-runtime/agent-2-path-select-dsl-designer.md`
- `restart/audit/pass-3-runtime/agent-3-visitor-surface-designer.md`
- `restart/audit/pass-3-runtime/agent-4-tape-union-architect.md`
- `restart/audit/pass-3-runtime/agent-5-error-recovery-incremental-parsing.md`
- `restart/audit/pass-3-runtime/agent-6-ecosystem-architect.md`

PASS-1 and PASS-2 syntheses were not consumed as authority in the original Phase 1 parallel run by design. This synthesis read the then-current dispatch materials, README, locks, inheritance index, corpus references, archived plans, BB W4/W5 materials, cookbooks, and current source references, then recorded assumptions and hand-offs for SYNTHESIS. The retired dispatch prompts are archaeology only; current hardening uses the five-prompt orchestrator suite at `restart/prompts/`.

Settled authority: tape is the substrate and is properly unioned with direct-to-struct. It must not be renamed to `ParseStream`. Columnar SoA and parallel substrates are dead. The post-Phase-7.1 V1 BBNF surface formalises six directives — `@import`, `@host fn`, `@error(recover = ...)`, `@layout`, `@pretty`, `@token` — alongside lookbehind, multi-function chaining, generics, function values + lambda literals (`|x| body`), and closure capture by `&'i Tape<'i>` reference. Rewrite-mode is out; grammar-level Unicode-class algebra is deferred to `parse-that-regex` (the regex sub-crate of `parse-that`). No per-grammar declaration crates exist by default. The Rust path/select macro family is `path!` + `select!` (the `pointer!` spelling retires per audit #3); diagnostic codes carry the `BBNF-PATH-*` prefix.

Conflicts surfaced:

- Retired PASS-3 dispatch archaeology named `ParseStream` as the PASS-3 keystone. That is stale; PASS-3 uses tape.
- `restart/inheritance/INDEX.md:65-66` says tape naming dies and structural insight survives as `ParseStream`. That is stale; structural insight survives as tape/direct union.
- Stale extension and `ParseStream` phrasing conflicts with settled README lines: lookbehind only, rewrite rejected, Unicode handled in regex (`restart/README.md:123`, `restart/README.md:133-143`) and tape/direct union (`restart/README.md:285-314`).
- Retired PASS-1/PASS-2 dispatch clauses that mention rewrite-mode, Unicode grammar algebra, or `ParseStream` are stale hand-off risks, not blockers. Live synthesis authority is `restart/prompts/ORCHESTRATOR.md` plus the pass syntheses.

## §1 Verdict ledger

| Surface | Explication | Pro | Con | Challenge | Verdict |
| --- | --- | --- | --- | --- | --- |
| Value API | Generated grammars expose typed roots, `DocumentView`, and untyped `ValueRef` over shared tape/direct identity. | Clear for normal users; powerful for tools. | Lifetimes can be intimidating. | Keep `parse(&str)` simple and route advanced cases to cookbook diagnostics. | KEEP / REINVENT |
| Parse constructors | `parse`, `parse_in`, and `parse_owned` implement Lock 9 slice-borrow/default plus explicit arena/owned modes (`restart/locks/14-LOCKS.md:50`). | Covers low allocation, batch, and durable use cases. | Owned mode can hide copies. | Bench all modes separately. | KEEP |
| Tape/direct union | Tape is the single advanced substrate; direct structs remain the ergonomic default (`restart/README.md:285-314`, `restart/locks/14-LOCKS.md:34`). | Unifies path, visitors, debug, LSP, and benchmarks. | Requires careful identity invariants. | Every visible direct node carries tape identity. | REINVENT |
| `ParseStream` | Stale replacement name in prompts/inheritance. | None under current authority. | Would fork naming and contracts. | Mention only as stale conflict. | DISCARD |
| Path/select DSL | `path!` and `select!` validate against generated metadata and lower to runtime traversal plans. | Strong diagnostics and static typing. | Current registry is overfit. | Replace hardcoded fixtures with metadata. | REINVENT |
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
    fn format(&self) -> String;
}
```

The `format()` method is the public surface of the `@layout`-driven formatter. The engine itself is grammar-emitted: `@layout`, `@pretty`, and `@token` directives produce `LayoutFacts` consumed by per-grammar layout lowering; the public method walks tape identity against those facts and emits source. The legacy `gorgeous` engine is archived per Lock 12 and is not the runtime substrate; `format()` carries no separate engine. Per-grammar pretty options (e.g. `compact`, `group`, `indent`, `hardbreak`, `sep(...)`, `block`) are surfaced through metadata derived from `@pretty`, not through a `PrettyOptions` argument; advanced cases that need custom strategies route through grammar-level `@pretty` rules rather than runtime API knobs.

Direct structs and tape are not competing products. Direct roots are what normal users author against. Tape-backed `ValueRef` is the shared cursor for `path!`, `select!`, visitors, debugger, CLI projections, LSP features, and playground inspection. This resolves the old SOTA recommendation against tape (`restart/corpora/SOTA.md:205-214`) by applying Lock 1 and the current dispatch authority.

## §3 Path, select, and visitor commitments

`path!` and `select!` survive, but their implementation is rebuilt. The current `bbnf-path` code demonstrates the macro direction (`crates/bbnf-path/src/lib.rs:1-22`, `crates/bbnf-path/src/path_macro.rs:146-210`) while also exposing two dead ends: hardcoded grammar marker registries (`crates/bbnf-path/src/registry.rs:80-98`, `crates/bbnf-path/src/registry.rs:125-138`) and placeholder terminal typing (`crates/bbnf-path/src/path_macro.rs:198-199`). The TypeScript mirror duplicates compiler logic because the proc-macro crate cannot be consumed directly (`crates/bbnf-path-ts/src/compile.rs:1-12`, `crates/bbnf-path-ts/src/compile.rs:41-65`). These citations are legacy evidence only; restart package names are `path`, `path-core` (V1 J.W3), with `path-ts` deferred post-V1 alongside the TS-native parse+runtime fork.

PASS-3 commitment:

- `path-core` owns parsing, lowering, validation, diagnostics, metadata schema, and runtime plans. Compile-time validation against grammar metadata is shared across every macro callsite: `path!(Json, ["a", "b", 0])` and `select!(Css, "...")` both consume the same `path-schema.toml` sidecar.
- `path` owns Rust proc macros: `path!` and `select!`. The `path!` spelling matches the path-core/path/path-ts crate set per the `vec!`/`Vec` analogy; the legacy `pointer!` spelling retires.
- `path-ts` defers post-V1. J.W3 ships `path-core` + `path` (Rust) only via the `RustBackend: Backend` impl; the TS template-tag + schema-binding surface lands when `TsBackend: Backend` lands per Lock 5 amendment.
- Generated grammar metadata replaces fixture registries.
- Both explicit type suffixes and implicit terminal inference are supported, matching the W5 pointer decision (`docs/tranches/BB/audit/W5-pointer-syntax-decision.md:20-27`, `docs/tranches/BB/audit/W5-pointer-syntax-decision.md:148-156`).

**Path/select worked path.** A generated JSON runtime proves both query
surfaces against the same `json.path-schema.toml` sidecar:

```rust
let doc = Json::parse(source)?;
let root: ValueRef<'_, '_, JsonRoot> = doc.root_value();

let sku_path = path!(Json, ["orders", 0, "sku"]);
let sku_query = select!(
    Json,
    "object.member[name='orders'] > array > object.member[name='sku']"
);
```

Compile time: `path!` validates the `orders -> [0] -> sku` segment chain
against generated JSON metadata and fixes the terminal kind to a string value;
`select!` validates the structural query against the same node-kind and field
schema and lowers it to a traversal plan over the `ValueRef` cursor space.
Runtime: both plans start at `root`, walk tape node ids without re-parsing, and
return typed `ValueRef` projections over the same snapshot identity that
visitors, debugger, and LSP reuse. The `path!` path yields one
`ValueRef<_, _, JsonString>`; the `select!` plan yields the matching SKU
projection set.

Failure path: `path!(Json, ["orders", 0, "sku_code"])` emits
`BBNF-PATH-UNKNOWN-SEGMENT` because `sku_code` is not a generated field. An implicit form
with both `Json` and `Yaml` roots in scope emits `BBNF-PATH-GRAMMAR-MISMATCH` until the
caller supplies the explicit grammar marker. A stale `json.path-schema.toml`
emits `BBNF-PATH-UNKNOWN-TERMINAL` and routes the user to `cargo xtask regen`.

**Registry deletion close gate.** Hardcoded grammar marker registries are not a deferral; they are a deletion item. Final close fails unless the following all hold:

```text
rg -n 'GRAMMAR_PATH_REGISTRY|GrammarMarkerRegistry|hardcoded_grammar_registry' \
   crates/path crates/path-core crates/bbnf crates/codegen \
   crates/runtime crates/ir
=> zero outside generated data
```

The `crates/path-ts/` tree is post-V1 deferred per Lock 7 amendment; it does not contribute to the V1 deletion-gate scan. Generated metadata produced by xtask-emitted descriptors is the only validation surface for `path!` and `select!`. Owner: PASS-3 amendment in coordination with SYNTHESIS deletion-gate ledger.

**Consumer acceptance gates.** PASS-3 surfaces are not closeable on prose-only hand-offs from PASS-2. The emission contract from PASS-2 must satisfy three executable consumer gates before PASS-3 closes:

| Consumer gate | Surface under test | Acceptance criterion |
|---|---|---|
| Emitted parse signatures compile under PASS-3 wrappers | `parse`, `parse_in`, `parse_owned` for every extant grammar plus yaml | Generated runtime crate compiles when `Json::parse(&str)`, `Json::parse_in(&str, &Arena)`, and `Json::parse_owned(String)` are called from a PASS-3 consumer smoke. |
| `DocumentView` metadata feeds visitors and selectors | `DocumentView::root_value`, `DocumentView::diagnostics`, `DocumentView::format`, generated `Visitor` trait, `path!`, `select!` | Generated `DocumentView::root_value()` projects to the same `ValueRef` index space the visitor walker, `path!` runtime plan, and `select!` traversal plan consume; `DocumentView::format()` walks `LayoutFacts` against tape identity and emits `@layout`-driven source. |
| Materialisation cost tables generated and documented | Per-grammar runtime emission | Codegen emits a `materialisation_cost.toml` (or equivalent generated artefact) with field counts, payload arena bytes, tape-token width, `TapeShape` scalar-cache policy, string-normalization policy, repeated-access cost class, selected objective profile, scalarized score/objective vector, and domination reason per node kind; the cookbook references it. |

These gates appear as receiver/blocker/receiving-gate rows in §8.

Visitors keep the W5 design: generated `Visitor` traits, `Visit`/walker support, and `VisitTypes` bitflag pruning (`docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:11-19`, `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:107-123`, `docs/tranches/BB/audit/W5-visitor-bitflag-spec.md:127-174`). The generated `Visitor` trait shape and method-naming convention (`visit_object`, `visit_member`, etc.) explicitly mirror `syn::visit::Visit` / `VisitMut` per V8 §3 γ4 host-leverage; users transferring from `syn`-based code recognise the pattern without retraining. The `VisitTypes` bitflag pruning is bbnf-specific (the mask is grammar-derived from generated metadata, not a `syn` precedent). Mutation happens through visitors/edit builders only, as required by the README (`restart/README.md:318`). Cookbook examples for visitor collection, pruning, mutation, and warnings remain the documentation backbone (`docs/cookbook/visitors.md:102-124`, `docs/cookbook/visitors.md:153-165`, `docs/cookbook/visitors.md:177-188`, `docs/cookbook/visitors.md:248`). Visitor diagnostics emitted by the runtime carry `BBNF-VISITOR-*` codes (rows in §6b); the visitor cookbook table-of-contents indexes each code so authors land on the relevant chapter from the diagnostic alone.

## §4 Tape/direct runtime architecture

PASS-3 requires these substrate-visible semantics from PASS-1:

- Snapshot-scoped document identity and `TapeId` identity.
- Stable node-kind IDs shared with codegen metadata.
- Compact spans, payload references, and payload classes.
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

This layout is not a PASS-1 mandate; it is a user-surface contract. PASS-1 may pack differently if these semantics remain true. PASS-2 may build direct structs first or tape first per grammar, but build order is not semantic. The externally visible invariant is stable: every public node has snapshot-scoped tape identity, every tape node can be projected through `ValueRef`, every generated direct field traces to one `(TapeId, node id, payload class)`, and cross-snapshot identity exists only through a `ReparsePlan` reuse map. `TapeShape` declares token kind, span class, payload class, traversal skip policy, scalar-cache policy, and string-normalization policy; `ValueShape` declares generated typed projections over the same node id. Red-like cursor views, direct typed roots, and AST adapters are transient projections over the green-like owning tape; none owns independent parse identity or recovery state.

Debug and DAP must reuse this identity. Existing DAP code already contains sessions, breakpoints, and stepping (`crates/lsp/src/dap/mod.rs:45-83`, `crates/lsp/src/dap/mod.rs:121-143`, `crates/lsp/src/dap/mapping.rs:41-92`), and the playground wasm API already exposes parse/LSP/debug concepts (`playground/src/composables/wasm/index.ts:233-256`, `playground/src/composables/wasm/index.ts:274-322`, `playground/src/composables/wasm/types.ts:166-183`); the playground wasm citation is legacy artefact, and the V1 surface defers WASM-specific lower-and-bench programme to V2 per Lock 5 amendment. Restart keeps those user concepts while moving their internals onto tape snapshots. The debug acceptance gate requires every breakpoint, step, hover, and playground trace event to carry `SnapshotId`, `TapeId`, node kind, and source span when the tape node exists. Span-only fallback is allowed only inside a parse-failed region before a stable recovery node exists; the fallback reason is reported on the debug-only channel and never as a user LSP diagnostic.

**Closure environment.** BBNF closures (`|x| body`) capture by `&'i Tape<'i>` reference only; closure environment frames are stack-allocated, never heap-escaping. The four committed closure sites — host-chain closure, map closure, predicate closure, recovery closure — each lower to a fixed BIR variant (`HostChain`, `ValueProject`, predicate-bool, `ErrorRecover`) per the Phase-7.1 Lock 4 amendment + ARCH §8.2 type-system commitment. None of the four sites materialises a runtime function-pointer table; each closure is monomorphised at its lambda's allocation site. Closure capture lifetime is bounded by `&'i`, so closures cannot escape the parse scope they were authored in; the lifetime parameter `'i` ties capture identity to tape identity. Function-value broadening beyond the four sites (first-class storage, return-from-rule, parameter-pass outside the host chain) defers to a Lock 1 reuse-map amendment that extends snapshot-scoped identity to closure environments. PASS-3 absorbs the narrow contract today; the broadening contract is a Lock 1 amendment surface.

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
    Reuse {
        unchanged: Vec<TapeRange>,
        reuse_map: Vec<(OldTapeId, NewTapeId)>,
    },
    Reparse {
        dirty: Vec<TextRange>,
        anchors: Vec<TapeId>,
        reuse_map: Vec<(OldTapeId, NewTapeId)>,
        fallback_reason: Option<FallbackReason>,
        invalidated_queries: QueryInvalidationSet,
    },
}
```

The reuse map is the only cross-snapshot identity bridge. Type-inference, cost-model, and e-graph query caches reuse only through `DocumentSnapshot`, `TapeId` reuse-map entries, and semantic facts; each invalidation records a query key and reason rather than silently surviving a parser edit.

The design language references existing precedents per V8 §3 γ7 host-leverage: salsa's revisions / queries / invalidation vocabulary informs the `(SnapshotId, query-key, invalidation-reason)` framing of `QueryInvalidationSet` and the cross-snapshot fact-cache contract; tree-sitter's incremental parse algorithm informs the `(OldTapeId, NewTapeId)` reuse-map computation and the dirty-range / anchor-set construction. The runtime mechanism is bbnf-built — salsa-queries are too coarse-grained for per-tape-range reuse, and tree-sitter's edit primitives do not compose with bbnf's recovery semantics — but the conceptual scaffolding is borrowed, not reinvented.

**Incremental recovery worked path.** Given a generated JSON rule with a
declarative recovery policy:

```bbnf
@error missing_member_value(recover = "," | "}" | "]")
rule member = string ":" value;
```

and snapshot `S42` for:

```json
{"orders":[{"sku":"A-1","qty":1}]}
```

an edit deleting `1` produces dirty range `TextRange(31..32)` inside the
`member` value. The language server builds a `ReparsePlan::Reparse` with
`dirty: [31..32]`, anchors `[orders-array, qty-member]`, a `reuse_map` for
proven unchanged tape ranges, and `invalidated_queries` for diagnostics whose
spans intersect the dirty range. The generated parser reaches
`@error(recover = ...)`, skips to the nearest sync token `}` without leaving the
array/object scopes, emits `BBNF-RECOVERY*` through `RecoveryFacts`, and
inserts a recovered member-value node flagged in tape as
`RecoveryKind::Substituted`. Recovered tape nodes carry
`RecoveryKind::{Error, Missing, Substituted}`, diagnostic code, sync token,
typed placeholder policy, and `VisitTypes::ERROR` behavior. The new snapshot
`S43` therefore keeps unchanged tape ranges for the `orders` array and `sku`
member, replaces only the `qty` value subtree, and gives visitors a recovery
node that `BBNF-VISITOR-RECOVERY-SKIP` can mention if the visitor opted out of
`VisitTypes::ERROR`.

Fallback path: if the same edit also deletes the closing `}]`, anchor matching
fails because the sync set cannot re-enter a balanced scope. The server falls
back to full parse with reason `anchor_miss_unbalanced_scope`, increments the
`incremental/edit_anchor` fallback ledger for the relevant corpus row, and keeps
default LSP output silent. With `BBNF_LSP_DEBUG=1`, the debug channel reports
`S42 -> S43`, the dirty range, the failed anchors, the empty reuse map, and the
fallback reason.

Yaml syntax-error fallback uses the same contract. During onboarding, a broken
indentation edit in `yaml.bbnf` still yields a typed `YamlRoot` projection when
recovery can place `RecoveryKind::Error` or `RecoveryKind::Missing` nodes under
the snapshot-scoped `TapeId`; if indentation destroys anchors across the edited
block, the LSP falls back silently, records
`fallback_reason: yaml_indent_anchor_miss`, and exposes details only through
`DocumentSnapshot` trace/debug output.

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

The public crate carries exactly 8 immediate children — `lib.rs`, `prelude.rs`, `parse/` (entry constructors), `document/` (root + view), `query/` (`path!`/`select!` adapters), `visitor/` (generated traits + `VisitTypes` pruning), `diagnostics/` (categories + rendering; verbatim strings of §6b lower to `thiserror::Error` derives and `miette::Diagnostic` rendering, no bbnf-invented diagnostic struct per V8 §3 γ3 host-leverage), and `metadata/` (workspace-metadata access, host route surface, layout descriptors) — per HARDENING-CONSOLIDATED §4.19 and Lock 13's 4-10 child-count rule. The substrate cursor (`tape/`) and the typed-root projection (`value/`) live under `runtime/src/tape/` and `runtime/src/value/` per Lock 1; the aggregator re-exports them through `prelude.rs` rather than carrying duplicate sibling directories. The grammar-specific surface (`Json`, `CssL4`, etc.) is generated under per-grammar runtime crates referenced from `metadata/`, not as a sibling directory in `bbnf/src/`. Host-function bindings, layout lowering hooks, and metadata sidecars all live under `metadata/`.

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

The `lsp/server.rs` + `lsp/protocol.rs` files are thin `tower-lsp` adapters per V8 §3 γ5 host-leverage; the `dap/server.rs` + `dap/session.rs` + `dap/mapping.rs` files are thin `dap-types` (or `debug-adapter-protocol` crate) adapters per V8 §3 γ6 host-leverage. The bbnf-specific surfaces are `analysis/` (diagnostics, hover, completion, semantic-tokens, imports) and `incremental/` (anchors, reparse plan); the protocol scaffolding is host-provided. The bbnf-language-server's invented surface shrinks to `analysis/` + `incremental/` only; LSP and DAP wire-formats are `tower-lsp` + `dap-types`.

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

`path-core`, `path` (V1 J.W3 stable cohort):

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
  path_macro.rs
  select_macro.rs
  diagnostics.rs
```

`crates/path-ts/` defers post-V1. The TS template-tag + schema-binding surface (`template_tag.rs`, `schema.rs`, `bindings.rs`) lands when `TsBackend: Backend` lands per Lock 5 amendment; the V1 `RustBackend` impl carries the entire path/select surface that V1 ships. The deferred tree shape:

```text
crates/path-ts/src/                  (deferred post-V1; TS-native parse+runtime fork)
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

**Fixture separation.** Lock 14 onboarding admits exactly two surfaces: the grammar source file (`yaml.bbnf`) and one workspace-metadata block (`[workspace.metadata.bbnf.grammars.yaml]`). `fixtures/yaml/*` is *not* part of the onboarding allowance. The four `fixtures/<grammar>/` directories above are post-onboarding parity evidence — they appear after a grammar's runtime emission, visitor, `path!`/`select!` adapters, and host route are already proven. Adding yaml therefore proceeds in two phases:

1. **Onboarding phase** — `yaml.bbnf` plus the metadata block. Generated value API, `path!`, `select!`, visitor, host route, and bench manifest must all appear from metadata + codegen with zero Rust edits and zero entries under `fixtures/yaml/`.
2. **Parity phase** (separate gate) — `fixtures/yaml/` data files and a `fixtures/yaml/manifest.toml` may be added later to feed the parity bench cohort. This is a cookbook-level gate, never an onboarding gate.

The grep gate `rg -n 'fixtures/yaml' restart/ARCHITECTURE.md restart/MASTER-PLAN.md restart/audit/pass-*` must return zero hits inside Lock 14 onboarding allowances; matches are confined to parity-phase prose.

## §6a Per-grammar feeder table

PASS-3 owns the column-feeder rows for the synthesis-owned per-grammar table. The local table carries 10 grammar rows across 8 PASS-3 columns; SYNTHESIS composes these with PASS-1 substrate and PASS-2 emission columns to assemble the architecture-level table.

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
| `yaml` (onboarding proof) | `Yaml` | `YamlRoot` | `generated.rs`, `parser.rs`, `host.rs` (if metadata declares host route) | `YamlVisitor`, `YamlVisitTypes` | `yaml.path-schema.toml` | parity-phase `fixtures/yaml/manifest.toml` | decomposed via `host::primitives` + `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from the onboarding two surfaces |

The yaml row exists at the onboarding boundary: every cell to the left of the parity-phase fixture manifest must be generated from `yaml.bbnf` plus the workspace-metadata block, with zero Rust edits and zero per-grammar match arms in any generic crate.

## §6b Compiler diagnostic ledger

PASS-3 owns the user-facing diagnostic strings for runtime, path/select, lifetime, layout, optimizer, host, and yaml-onboarding surfaces. Strings are committed verbatim; later prose may not soften them. Each diagnostic carries a stable code, the target user, the mental model the user holds at the point of failure, the confusion point the message resolves, and the artefact that closes the loop.

| Code | Verbatim text | Target user | Mental model | Confusion point | Artefact |
|---|---|---|---|---|---|
| `BBNF-LIFETIME-ESCAPE` | `error[BBNF-LIFETIME-ESCAPE]: borrowed value escapes parse scope; the source string `&str` was dropped before this projection. help: use `Json::parse_owned(input)` to retain the data, or hold `&input` alive for the duration of `doc`.` | Application author | "I parsed once and stored the result." | Default `parse(&str)` borrows. | Cookbook §lifetime-surfaces. |
| `BBNF-ARENA-MISMATCH` | `error[BBNF-ARENA-MISMATCH]: arena mismatch; root was parsed in arena #N but projected through arena #M. help: use the same `&Arena` for parse and projection.` | Arena user | "I'm batching parses through one bumpalo." | Two arenas in scope. | Cookbook arena chapter. |
| `BBNF-LAYOUT-UNUSED` | `warning[BBNF-LAYOUT-UNUSED]: @layout directive is unused by generated formatter; rule never reaches a layout-sensitive emit path.` | Grammar author | "@layout always shapes output." | Rule has no emitting use. | Layout cookbook. |
| `BBNF-LAYOUT-CONFLICT` | `error[BBNF-LAYOUT-CONFLICT]: rule `{rule}` has no resolvable layout; reason: {cause}. help: layout descriptors must derive from a leaf, an explicit `@layout(...)`, or an upstream rule with a known layout.` | Grammar author | "Lowering finds layout from context." | Layout chain underdetermined. | Layout cookbook §unresolved-layout. |
| `BBNF-PRATT-NOT-APPLIED` | `note[BBNF-PRATT-NOT-APPLIED]: Pratt was not applied to `{rule}` under profile `{profile}`; reason: {cause}. The grammar still parses; performance fallback uses recursive-descent with objective-profile evidence.` | Grammar author | "Auto-Pratt always fires for operator chains." | Cost model or objective profile declined. | Cookbook §pratt-detection. |
| `BBNF-SIMD-NOT-SELECTED` | `note[BBNF-SIMD-NOT-SELECTED]: SIMD scanner was not selected for `{rule}` under profile `{profile}`; reason: {cause}. Exact SIMD scans must prove scalar parity; prefilter scans must pass `RegexProgram`, DFA/VM, or scalar verifier before tape emission. The grammar still parses; fallback remains exact scalar scan.` | Grammar author | "SIMD is always faster." | Cost, Unicode semantics, scalar parity, or missing verifier route blocked selection. | Cookbook §simd-detection. |
| `BBNF-METADATA-MISSING-GRAMMAR` | `error[BBNF-METADATA-MISSING-GRAMMAR]: workspace metadata block missing for grammar `{name}`. help: add `[workspace.metadata.bbnf.grammars.{name}]` to your Cargo workspace metadata; the grammar source file alone is not sufficient.` | New-grammar author | "Source file is enough." | Lock 14 requires both surfaces. | Onboarding cookbook §two-surfaces. |
| `BBNF-PATH-UNKNOWN-SEGMENT` | `error[BBNF-PATH-UNKNOWN-SEGMENT]: unknown path segment `{segment}` in `{path_macro_input}`; rule has no field with that name.` | Application author | "Paths traverse fields by name." | Field name typo or stale. | Path cookbook §validation. |
| `BBNF-PATH-GRAMMAR-MISMATCH` | `error[BBNF-PATH-GRAMMAR-MISMATCH]: path grammar inference failed; help: add an explicit grammar prefix like `path!(Json => "/...")`.` | Application author | "Implicit grammar always works." | Two grammars in scope. | Path cookbook §explicit-grammar. |
| `BBNF-PATH-UNKNOWN-TERMINAL` | `error[BBNF-PATH-UNKNOWN-TERMINAL]: terminal type for path `{path}` is not yet known to the macro; help: regenerate with `cargo xtask regen` so the schema is in sync.` | Application author | "Macro reads metadata at compile time." | Stale generated schema. | Path cookbook §regen. |
| `BBNF-VISITOR-NO-MATCHING-KINDS` | `warning[BBNF-VISITOR-NO-MATCHING-KINDS]: visitor declares no matching node kinds. help: add the desired kind to `VisitTypes` or remove the visitor.` | Visitor author | "Empty `VisitTypes` walks every node." | Bitflag default vs. declared-kinds intent. | Visitor cookbook §pruning. |
| `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY` | `error[BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY]: borrowed parse tree cannot be mutated in place. help: use `parse_owned`, `parse_in` with a mutable arena document, or emit an edit plan via the edit builder.` | Visitor author | "Visitors always mutate." | Borrowed root is shared-immutable. | Visitor cookbook §mutation. |
| `BBNF-VISITOR-RECOVERY-SKIP` | `warning[BBNF-VISITOR-RECOVERY-SKIP]: recovery nodes skipped by this visitor. help: implement `visit_error` or enable `VisitTypes::ERROR`.` | Visitor author | "Default visitor sees every node." | Recovery nodes opted out by default. | Visitor cookbook §recovery. |
| `BBNF-RECOVERY*` | `error[BBNF-RECOVERY*]: recovered `{rule}` at {span}; skipped to {sync}. help: supply {expected} before {sync}.` | Grammar author and LSP user | "Recovery keeps the document usable." | The recovered node is real runtime state, not a parser warning. | Recovery cookbook §sync-sets. |
| `BBNF-LOOKBEHIND-WIDTH` / `LookbehindWidth` | `error[BBNF-LOOKBEHIND-WIDTH]: lookbehind operator width is unbounded for `{rule}`; help: lookbehinds must be finite-width; use a bounded alternative or move the constraint into a regex with `(?<=...)`.` | Grammar author | "Lookbehind takes any pattern." | Unbounded width. | PASS-1 diagnostic string; Grammar surface spec. |
| `TypeMismatch` / `BBNF-SUBSUMPTION-EDGE` | `error[BBNF-SUBSUMPTION-EDGE]: value projection `{projection}` expected `{expected}` from {expected_from}, but `{actual}` was synthesized from {actual_from}; ValueShape `{value_shape}` cannot satisfy {check_synth_cause}. help: fix the annotation, host signature, or generated value projection.` | Grammar author and host author | "The generated value shape follows my annotation." | Check/synth, coercion, or value-shape obligation failed. | Host/type cookbook §type-obligations. |
| `HostSignature` | `error[BBNF-HOST-SIGNATURE-MISMATCH]: host function `{name}` cannot satisfy signature `{expected}`; argument {index} inferred `{actual}` at {span}.` | Host author | "@host fn body just runs." | Type flow mismatch. | Host cookbook §signatures. |
| `ChainStep` | `error[BBNF-CHAIN-STEP]: chain step `{step}` does not accept `{input_type}` from previous step; the chain `-> f1 -> f2` requires `f2` to accept `f1`'s output.` | Host author | "Chains compose." | Step type fault. | Host cookbook §chains. |
| `WasmHost` | `error[BBNF-HOST-WASM-PRIMITIVE-MISSING]: host chain `{chain}` cannot lower to WASM; reason: {cause}. The Rust backend continues to compile; the WASM lower-and-bench programme defers post-V1 alongside the V2 `WasmBackend: Backend` impl per Lock 5 amendment, so keep the chain Rust-only or carry the broadening to V2.` | TS/WASM author | "Hosts work everywhere." | Host primitive missing in WASM ABI; V2 surface deferred. | WASM ABI cookbook (V2); host-primitive matrix routes through `WasmBackend` impl when V2 lands. |
| `BBNF-CODEGEN-IMPORT-DENY` / `LowererImport` | `error[BBNF-CODEGEN-IMPORT-DENY]: lowerer at `{path}` imports `ir::grammar_ir`; only the BIR producer may consume Grammar IR. help: lower against `ir::backend_ir`.` | Codegen author | "All IR is one IR." | Two-IR contract violation. | Architecture §7. |
| `BBNF-LOCAL-EQUALITY-ANNOTATION` | `error[BBNF-LOCAL-EQUALITY-ANNOTATION]: match arm at {span} introduces branch-local type equality {equality}; add or correct the refinement annotation `Pattern @ where T = U` so the OutsideIn(X) solver can discharge the wanted equality from the givens.` | Grammar / host author writing GADT-style match arms | "Branch-local equalities flow without annotation." | The wanted equality cannot be solved from the implicit givens; an explicit refinement is required. | Type-system cookbook §gadt-refinements; PASS-1 §3 type-system algorithm; ARCH §8.2 GADT V1 surface. |

These strings are part of the SYNTHESIS-owned diagnostic ledger; PASS-3 commits the text and cookbook receivers, the lowerer-import-deny code is mirrored from PASS-2 ownership, and the layout/lookbehind/host/chain/type codes are mirrored from PASS-1 ownership. PASS-3 emits the PASS-1-owned lookbehind diagnostic as one binding pairing alphabetic code `BBNF-LOOKBEHIND-WIDTH` with vocabulary kind `LookbehindWidth`; numeric aliases retire per the Phase 8.4 simplification fold (V8 §3 β1), and the mnemonic code is the single namespace. The `BBNF-LOCAL-EQUALITY-ANNOTATION` row above is V1-emitted (Phase 8.3.1 / Lock 4 amendment); the OutsideIn(X)-style implication-constraint solver at `passes/types/` discharges branch-local type equalities to `LayoutFacts` per ARCH §8.2 GADT V1 surface; reservation phrasing for the row retires. Type diagnostics expose check/synth stage, obligation provenance, and `ValueShape` projection cause to users without exposing `TypeFacts` or a public higher-rank type pass. The runtime rendering layer is host-provided: the verbatim strings of this ledger lower into `thiserror::Error` derives at `crates/bbnf/src/diagnostics/`, and cookbook receivers become `miette::Diagnostic::url` impls; bbnf invents no diagnostic struct.

Scanner diagnostics follow two runtime rules. In exact mode, a SIMD positive that disagrees with the scalar offset vector is a correctness failure before tape emission. In prefilter mode, a SIMD positive is only a candidate; `RegexProgram`, DFA/VM, or scalar verifier acceptance is required before any tape node is emitted, and rejected candidates remain debug evidence rather than user diagnostics.

**WASM host primitive route (V2 deferred).** The runtime/WASM path does not add grammar syntax — and the lower-and-bench programme itself defers post-V1 alongside the V2 `WasmBackend: Backend` impl per Lock 5 amendment. Grammar authors still use block-bodied `@host fn` definitions, host-chain calls, generic `host::primitives`, and workspace metadata; V1's `RustBackend` impl carries the entire host-chain surface in Rust. PASS-2 lowers that metadata into Backend IR `CallHost`/host-chain records; the WASM ABI descriptor surface lands when `WasmBackend` lands. The H.W3 / J.W3 WASM measurement rows defer to V2 — V1 SOTA close gates measure the Rust-line only at H.W3, H.W4, and H.W5 per Lock 8 amendment. A missing primitive emits `BBNF-HOST-WASM-PRIMITIVE-MISSING` reframed for V2 deferral; PASS-3 records no `{N}` or `{M}` latency/size numbers in V1.

## §7 Benchmark and SOTA gates

Every throughput SOTA gate that claims external performance names a competitor per Lock 8 (`restart/locks/14-LOCKS.md:48`). The SOTA corpus compares sonic-rs, simdjson/simd-json, lightningcss, and tree-sitter style tradeoffs (`restart/corpora/SOTA.md:12-16`, `restart/corpora/SOTA.md:35-42`, `restart/corpora/SOTA.md:64-77`, `restart/corpora/SOTA.md:103-118`). Non-throughput rows such as incremental fallback and debug trace overhead are report-only measurement gates until bench output records input hash, machine, compiler flags, warmup, sample count, validation mode, source ownership mode, direct/tape materialisation mode, scalar-cache policy, string-normalization policy, selected objective profile, trace mode, and surface under test.

| Dataset | Baseline citation | PASS-3 gate |
| --- | --- | --- |
| `twitter.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | borrowed parse, tape cursor, direct root |
| `citm_catalog.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | object traversal and `path!` |
| `canada.json` | sonic/simd-json fixture references (`restart/corpora/SOTA.md:54-56`) | selector scan and array-heavy parse |
| CSS bootstrap/animate | lightningcss visitor and perf evidence (`restart/corpora/SOTA.md:103-118`, `restart/corpora/SOTA.md:134-136`) | generated visitor pruning and layout metadata |
| BBNF grammar corpus | restart full-grammar generalization (`restart/locks/14-LOCKS.md:60`) | no per-grammar overfit |

PASS-3 recommends bench reports include: borrowed/arena/owned timings, `parse(&str)` prevalidation versus byte/file validation labels, source ownership mode, tape/direct projection timings, direct/tape materialisation mode, scalar-cache and string-normalization policy, selected objective profile, `path!` and `select!` traversal timings, visitor pruning win/loss, incremental fallback rate, and DAP trace overhead.

Exact PASS-3 benchmark rows. Competitor floor + Platform columns inline the per-row attribution mandated by Lock 8 (`restart/locks/14-LOCKS.md:48` and `restart/README.md:328-334`); cross-document carry to SYNTHESIS H/J at §10 remains as insurance for any post-PASS-3 ratification.

| Row | Target | Competitor floor | Platform | Surface under test |
|---|---|---|---|---|
| `json/twitter/borrowed` | <= 380us | sonic-rs 436 µs / simd-json 424 µs | M1 Pro | `parse(&str)` plus direct root. |
| `json/twitter/tape_cursor` | <= borrowed + 10% | (no Lock-8 claim; relative to bbnf borrowed row) | M1 Pro | `ValueRef` cursor projection. |
| `json/citm/path` | <= 750us parse target plus reported selector time | sonic-rs 854 µs / simd-json 831 µs | M1 Pro | `path!` object traversal. |
| `json/canada/array_scan` | <= 2.8ms | sonic-rs 3.144 ms | M1 Pro | array-heavy parse and selector scan. |
| `css/bootstrap/visitor` | <= 3.0ms | lightning-css ~4.16 ms | M1 Pro (PASS-3 §9 disclaim re: platform ratification per `restart/README.md:336`) | generated visitor pruning over CSS. |
| `css/animate/layout` | <= 1.6ms | lightning-css 1.97 ms | M1 Pro (same disclaim as bootstrap row) | layout metadata plus parser surface. |
| `bbnf/self_host/internal` | <= 100 ms full self-parse + format roundtrip; non-Lock-8 internal gate; no SOTA peer claim attaches. | (no SOTA peer claim attaches) | M1 Pro | BBNF grammar parses itself through the public runtime. |
| `incremental/edit_anchor` | report fallback rate | (no Lock-8 claim; non-throughput row) | M1 Pro | LSP edit reparse plan. |
| `debug/trace_overhead` | report overhead | (no Lock-8 claim; non-throughput row) | M1 Pro | DAP/playground trace projection. |

Generated API budget. The +2 percent regen ceiling rows below anchor against the W3 baseline LOC totals captured in `restart/audit/pass-2-codegen/PASS-2.md:380-392` (per-grammar generated_loc table observed against PASS-B audit): css_l4 ≈ 107,138 LOC, bbnf ≈ 21,503 LOC, google_sheets ≈ 14,088 LOC, css_pretty ≈ 9,021 LOC, ebnf ≈ 7,646 LOC, json ≈ 3,500 LOC, bnf ≈ 3,290 LOC, csv ≈ 1,693 LOC, math ≈ 871 LOC, total 168,750 LOC across the nine extant grammars; yaml provisional ≤ 4,000 LOC. The delta semantics ("+2 percent") gate against these anchors per regen.

| Surface | W3 baseline LOC | Budget gate |
|---|---|---|
| Visitor traits | css_l4 visitor ≤ 22 K LOC at W3 baseline (≈ 20 percent of 107 K); bbnf visitor ≤ 6 K LOC at W3 baseline (≈ 28 percent of 21 K); other-grammar visitor LOC scales with each grammar's `generated_loc` row in PASS-2.md §6. | Per-grammar generated visitor LOC reported separately; no handwritten visitor file over 500 LOC; per-grammar visitor LOC delta beyond the W3 baseline anchor carries a +2 percent ceiling per regen. |
| Path metadata (Rust) | Counted within each grammar's `generated_loc` PASS-2 row; no separate W3 anchor. | Generated schema rows are counted against grammar runtime budget; `path-core` handwritten files obey Lock 13; per-grammar path-schema Rust budget <= 32 KB. |
| Path metadata (sidecar) | Sidecar files are not Rust source; no W3 LOC anchor applies. | Generated `*.path-schema.toml` sidecar size <= 64 KB per grammar; bench manifest sidecar <= 8 KB per grammar. |
| Tape projections | css_l4 projection ≤ 35 K LOC at W3 baseline (≈ 33 percent of 107 K); bbnf projection ≤ 8 K LOC at W3 baseline (≈ 38 percent of 21 K); other-grammar projection LOC scales with PASS-2.md §6. | Generated projection LOC counted with runtime module budget; per-grammar projection delta beyond the W3 baseline anchor carries a +2 percent ceiling per regen. |
| Tape identity field/method delta | Field/method count, not LOC; the W3 anchor is the post-PASS-1 substrate definition (§4 above). | Adding a tape identity field or `ValueRef` method costs <= 1 field plus 2 methods per regen; larger deltas open a named amendment. |
| Bench-report generation | Generated artefacts; no W3 LOC anchor applies. | Per-grammar bench-report markdown <= 16 KB; per-grammar bench-report JSON <= 8 KB; aggregate bench summary <= 64 KB. |
| Regen wall budget | Wall time, not LOC; PASS-2.md §6 carries observed-vs-provisional baselines. | `cargo xtask regen --check` <= 12 s on M1 Pro for the nine extant grammars; <= 14 s including yaml; over-budget regen blocks close. |
| Diagnostics | Diagnostic-rendering code is non-generated; W3 anchor governs the per-grammar diagnostic data only. | Generated code list is data; diagnostic rendering code remains shared and non-generated. |

## §8 Cross-pass hand-offs

| Contract | Receiver | Blocker | Receiving gate |
|---|---|---|---|
| Tape token packing, payload arenas, span widths, child/sibling traversal, recovery/layout/debug flags, snapshot-scoped `TapeId`, red-like cursor views, and `ReparsePlan` reuse maps. | PASS-1 / Tranche B | PASS-3 cannot prove cursor, visitor, incremental, or DAP identity. | Runtime identity tests over direct root, `ValueRef`, `path!`, `select!`, visitor traversal, and debug trace for the same `(TapeId, node id)`. |
| Tape as the substrate name; no public `ParseStream`. | PASS-1 / SYNTHESIS | Naming fork leaks into public APIs. | Conflict guard for `ParseStream` in public docs and code. |
| Typed roots, three parse constructors, `DocumentView`, `ValueRef`, visitors, `VisitTypes`, diagnostic metadata, path schemas, host metadata, and fixture/bench metadata. | PASS-2 / Tranche F | Generated runtime lacks consumer-facing metadata. | PASS-3 consumer smokes from generated runtime. |
| Consumer acceptance: emitted parse signatures compile under PASS-3 wrappers, `DocumentView` metadata feeds visitors and selectors, materialisation cost tables generated and documented with `TapeShape`, `ValueShape`, scalar-cache policy, string-normalization policy, repeated-access cost, objective vector, selected profile, and domination reason. | PASS-2 / Tranche F + Tranche I | PASS-3 close on prose-only hand-off. | Three executable consumer gates pass on every extant grammar plus yaml. |
| WASM host primitive ABI descriptor (V2 deferred). | PASS-2 / V2 `WasmBackend: Backend` impl | Host primitives cannot become new grammar syntax or hand-written packaging glue; V1 ships Rust impl only per Lock 5 amendment. | V2 records exported function names, host-call shape, marshalling rule, primitive coverage, scalar/SIMD parity; V1 emits `BBNF-HOST-WASM-PRIMITIVE-MISSING` reframed for V2 deferral. |
| SIMD/DFA scanner verifier contract. | PASS-2 / Tranche F + H.W3 | A regex prefilter candidate could emit tape without semantic acceptance. | Exact scans prove scalar parity; prefilter candidates pass `RegexProgram`, DFA/VM, or scalar verifier before tape emission. |
| No per-grammar declaration crates, rewrite-mode hooks, or grammar Unicode algebra APIs. | PASS-2 / SYNTHESIS | Generated surfaces reintroduce discarded extension scope. | Negative API and parser fixtures. |
| Final crate names (V1): `path`, `path-core`, and `test-fixtures`. `path-ts` defers post-V1 alongside the TS-native parse+runtime fork per Lock 7 amendment. | SYNTHESIS / Tranche A | Legacy package names survive into greenfield docs. | Workspace crate-name check. |
| Hardcoded grammar registry deletion. | SYNTHESIS / Tranche I close gate | Registry survives parallel to metadata. | `rg -n 'GRAMMAR_PATH_REGISTRY\|GrammarMarkerRegistry' crates/` returns zero outside generated data. |
| CLI/LSP/DAP ownership. V1 entry surface (server start + diagnostics + hover + go-to-definition + LSP/DAP wire-format adapters over `tower-lsp` + `dap-types` + snapshot-identity binding through tape source spans) is V1 LOAD-BEARING; LSP completion + semantic-tokens + imports + incremental anchors + reparse-plan body + DAP server / session / mapping protocol implementation + `commands/debug.rs` CLI surface route to tranche I body per V8 §3 δ5 + δ6. | SYNTHESIS / Tranche I (V1 entry) + Tranche I body (completion / semantic-tokens / imports / incremental-anchors / reparse-plan / DAP protocol + CLI) | Old PASS-C CLI deferral leaves top-layer gap; downstream IDE polish without V1 binding contract creates orphan implementation. | V1 entry: CLI and LSP diagnostics parity test on JSON + CSS edits with diagnostic + hover output. Tranche I body close gate: DAP `bbnf-language-server` server starts under VSCode + emits one breakpoint event over a JSON parse — promotion-test, not landing-test. |
| Performance rows integrated with PASS-1/PASS-2 outputs. | SYNTHESIS / Tranche H/J | Bench gates become narrative only. | Exact benchmark rows above appear in master plan gates. |
| Incremental fallback gates by dataset, yaml syntax-error recovery, typed recovery placeholders, and LSP user-facing silence policy. V1 ships full-reparse-on-each-edit (correct, slow); reuse-map computation + `(OldTapeId, NewTapeId)` pair semantics + edit-anchor algorithm + dataset-level fallback-rate gates route to tranche I body per V8 §3 δ7 — recovery semantics + fallback-reason ledger + LSP silence policy are V1 LOAD-BEARING (the user-mandate is "LSP fault-tolerant fallback", which is recovery, not incremental). | PASS-1 / Tranche I (recovery + fallback-reason ledger + LSP silence — V1 LOAD-BEARING) + Tranche I body (reuse-map computation + edit-anchor algorithm + dataset-level fallback-rate gates) | Fallbacks become an unreported workaround; reuse-map computation without recovery binding orphans the incremental path. | V1 entry: LSP policy enforcement test with `RecoveryKind` and `fallback_reason` evidence on full-reparse path. Tranche I body close gate: dataset-level fallback ledger at §5 fallback-rate gates table with reuse-map evidence per row. |
| Per-grammar feeder rows for typed root, `ValueRef`, runtime files, visitor, path schema, fixture manifest, host route. | SYNTHESIS / Architecture per-X table | All-grammar claims fall to prose. | 10-row table consumed verbatim by Architecture; columns match SYNTHESIS schema. |
| Compiler diagnostic ledger with committed strings, objective-profile optimizer notes, scanner verifier routing, and `TypeMismatch` / value-shape causes. Verbatim §6b strings + cookbook URL stubs (every `BBNF-*` code resolves to `docs/cookbook/...md` from the diagnostic URL) are V1 LOAD-BEARING; full cookbook chapter content (visitor pruning chapters, format mode tables, path validation walk-throughs, recovery sync-set worked paths, type-system §gadt-refinements chapter) routes to tranche J body per V8 §3 δ7. | SYNTHESIS + cookbook receivers (verbatim strings + URL stubs — V1 LOAD-BEARING) + Tranche J body (cookbook chapter authoring waves) | Diagnostics drift between PASS, cookbook, and runtime; URL-resolves-to-empty-page is a worse user experience than no URL. | V1 entry: every code in §6b appears in cookbook table-of-contents and runtime emit tests without public `TypeFacts` or higher-rank type-pass leakage. Tranche J body close gate: every `BBNF-*` code in §6b has a published cookbook chapter at `docs/cookbook/...md` resolvable from the diagnostic URL. |

## §9 KEEP / REINVENT / DISCARD summary

KEEP:

- `parse`, `parse_in`, `parse_owned`.
- Typed generated roots and grammar-specific visitors.
- Untyped `ValueRef`/generic value for tools.
- `path!`, `select!`, explicit and implicit path forms.
- The six V1 directives — `@import`, `@host fn`, `@error(recover = ...)`, `@layout`, `@pretty`, `@token` — alongside lookbehind, multi-function chaining, generics, function values + lambda literals, and closure capture by `&'i`.
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
| Generated metadata schema for paths, visitors, host functions, diagnostics, and fixtures. | PASS-2 / Tranche F | PASS-3 cannot validate `path!`/`select!` or visitors at compile time. | Schema is enumerated and PASS-3 macros consume it without fixture registries. |
| Workspace naming (V1): `path`, `path-core`, `test-fixtures`. `path-ts` defers post-V1 per Lock 7 amendment. | SYNTHESIS / Tranche A | Prefixed names re-leak into greenfield. | `rg -n 'bbnf-path\|bbnf-test-fixtures' restart/` returns zero outside deletion archaeology. |
| Bench harness target numbers and machine profiles. | SYNTHESIS / Tranche H/J | Bench rows become aspirational. | Master/Architecture inline competitor + dataset + platform + bbnf number for every row. |
| Rare host adapter escape-valve policy. | SYNTHESIS / Architecture rare-escape form | Per-grammar declaration crates re-enter through naming. | Review form requires reason, scope, owner, why workspace metadata plus `host::primitives` plus block-bodied `@host fn` fail, declaration location, deletion path, reviewer, and receiving gate; Lock 11 incubation remains path-dep sister-crate policy, not a default per-grammar declaration-crate route. |
| Hardcoded grammar registry deletion. | PASS-3 amendment + SYNTHESIS deletion-gate ledger | Registry survives parallel to metadata route. | `rg` close gate returns zero outside generated data. |
| Consumer acceptance gates for PASS-2 emission contract. | PASS-2 / Tranche F + Tranche I | PASS-3 closes on prose only. | Three executable consumer gates pass on every extant grammar plus yaml. |
| Diagnostic ledger committed strings. | SYNTHESIS + cookbook receivers | Strings drift across docs. | Every code in §6b appears verbatim in cookbook + runtime emit tests. |
| Per-grammar feeder rows. | SYNTHESIS / Architecture per-X table | "All grammars" claims rest on prose. | Architecture consumes the §6a table verbatim. |
| Fixture separation from Lock 14 onboarding. | SYNTHESIS / Architecture Lock 14 proof | yaml fixtures re-enter onboarding allowance. | `rg -n 'fixtures/yaml' restart/` returns zero hits inside Lock 14 onboarding allowance. |
| Incremental fallback dataset gates and LSP silence policy. | PASS-1 / Tranche I | Fallback becomes unreported workaround. | Dataset thresholds + LSP policy test + bench ledger row. |

## §11 Final posture

PASS-3 keeps the user-facing promise simple: generated grammars parse into typed roots, tools can project through a common value/tape cursor, visitors own mutation, paths are validated against generated metadata, language tooling is snapshot-aware, and benchmarks name the competitors they claim to beat. The architectural guardrail is equally simple: one tape/direct runtime identity, no `ParseStream` rename, no rewrite-mode, no grammar-level Unicode algebra, and no default per-grammar declaration crates.
