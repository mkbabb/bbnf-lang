# Restart Architecture

This document is the Phase 2 architecture contract for the greenfield restart.
It binds the workspace shape, dependency DAG, public APIs, private internals,
Cargo metadata, IR contracts, BBNF surface, and migration-facing invariants.
It is written from the resolved authority set: `restart/README.md`, the 14
locks, the instruction precepts, the three PASS syntheses, and the current
dispatch authority.

## 0. Authority And Conflict Ledger

The current restart is not a continuation of the old tranche plan. The README
states that the new anchor starts from the post-interrogation answers, ffuzzy,
locks, precepts, and synthesis pass outputs rather than the legacy BA-BD plans
alone (`restart/README.md:3`). Legacy plans remain inheritance, not governing
truth; the inheritance index says BA/BB/BC/BD are to be mined and cited, not
resumed as-is (`restart/inheritance/INDEX.md:1-5`).

| Topic | Settled architecture | Superseded material | Resolution |
|---|---|---|---|
| Runtime substrate | Tape is the substrate and is unioned with direct-to-struct. | Older restart notes and inheritance rows that say tape dies or ParseStream replaces it. | The README names tape and direct-to-struct as a union (`restart/README.md:272-318`), Lock 1 repeats this (`restart/locks/14-LOCKS.md:34`), and PASS-3 resolves ParseStream mentions as stale (`restart/audit/pass-3-runtime/PASS-3.md:14-23`). |
| ParseStream term | Do not rename tape to ParseStream. | `restart/inheritance/INDEX.md:66` and old README remnants that mention ParseStream. | Keep the term `tape`. `ParseStream` only appears today as a `syn` macro parse type, not a runtime concept. |
| Columnar SoA / parallel substrate | Dead. | Old speculative substrate sketches. | Lock 1 rejects parallel substrates and OpenFrame ladders (`restart/locks/14-LOCKS.md:34`). |
| Rewrite-mode | Out of the BBNF surface. | ffuzzy transducer/rewrite ideas and stale README line language. | README says rewrite-mode is rejected (`restart/README.md:139-148`); PASS-1 discards it (`restart/audit/pass-1-substrate/PASS-1.md:5-20`). |
| Unicode class algebra | Deferred to `parse-that/regex`; not a grammar-level BBNF surface. | ffuzzy and stale extension lists that treat class algebra as a grammar feature. | README routes Unicode set work below BBNF (`restart/README.md:150-158`); PASS-1 says no BBNF-level Unicode class algebra (`restart/audit/pass-1-substrate/PASS-1.md:84-121`). |
| Lookbehind | In. | Old rejection of lookbehind. | README accepts lookbehind as a first-class extension (`restart/README.md:121-137`); PASS-1 includes `Lookbehind` in Grammar IR (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| Per-grammar declaration crates | Not default. A rare escape valve must be explicit and fenced. | Old plans that created declaration crates per grammar. | README says onboarding is `.bbnf` plus workspace metadata, with no Rust crate or match arms (`restart/README.md:11-25`); Lock 14 allows only rare optional declaration crates (`restart/locks/14-LOCKS.md:60`). |
| Generic grammar code | Mandatory. | Current hardcoded parser registries and grammar-name Rust modules. | CENSUS identifies grammar-name leaks in metadata, registries, path mirrors, and generated shims (`restart/corpora/CENSUS.md:103-122`). Lock 14 rejects generic crates with grammar switches, types, modules, or features (`restart/locks/14-LOCKS.md:60`). |
| IR boundary | Two IRs plus side tables. | Old backend walkers that emit from Grammar IR directly. | README requires Grammar IR and Backend IR (`restart/README.md:104-118`); Lock 5 forbids emitter walking grammar directly (`restart/locks/14-LOCKS.md:42`). |
| Optimization graph | CSP, egraph, miners, and cost model compose by output piping. | A fused global hypergraph. | README and Lock 4 require bridged sister crates rather than a fused graph (`restart/README.md:219-228`, `restart/locks/14-LOCKS.md:40`). |

## 1. Workspace Shape

The greenfield workspace has about 24 crates. Internal crates drop the `bbnf-`
prefix while user-facing crates keep it (`restart/README.md:29-60`). The crate
set below is authoritative for tranche planning and for migration disposition.

| Crate | Visibility | Role | Current inheritance |
|---|---:|---|---|
| `bbnf` | Public | User library facade: grammar loading, generated grammar handles, parse APIs, value views, path integration. | New facade from `core`, `runtime`, `analysis`, and old package root. |
| `bbnf-cli` | Public | Command-line frontend for parse, check, build, debug, metadata, bench handoff. | Current CLI fragments in `core`/`bootstrap`/`xtask`; new crate. |
| `bbnf-language-server` | Public | LSP, diagnostics, incremental parse, editor bridge, playground/DAP hooks. | Consolidates `crates/analysis` and `crates/lsp`; PASS-3 requires this consolidation (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| `bbnf-bench` | Public/dev | Benchmark harness, fixture runner, SOTA gate runner, corpus profiles. | From old benches, `test-fixtures`, and PASS-2/3 gate commitments. |
| `error` | Internal | Diagnostics, spans, recovery codes, host-call errors, report formatting types. | Extract from `core`, `analysis`, `lsp`. |
| `pipeline` | Internal | End-to-end orchestration from source to generated artifacts, pass scheduling, cache keys. | Replaces scattered core pipeline files; README defines pass order (`restart/README.md:188-207`). |
| `source` | Internal | Source files, includes/import graph, spans, checkpoints, text rope/snapshot primitives. | Extract from `core/src/imports`, source maps, LSP text state. |
| `grammar` | Internal | Parser for BBNF itself, grammar AST, semantic validation input. | Current generated bootstrap parser plus handwritten grammar logic. |
| `ir` | Internal | Grammar IR, Backend IR, side-table schemas, validation invariants. | Reinvented around two IRs; current `crates/ir` is mined (`restart/corpora/MODULES.md:264-505`). |
| `passes` | Internal | Type inference, shape mining, recognizer mining, normalization, egraph/CSP bridge, extraction. | Current `ir` passes and `core` lowerers split by responsibility. |
| `vm` | Internal | Backend IR interpreter, debug/replay, trace events, golden execution. | Current VM-ish code plus PASS-1 VM scope. |
| `codegen` | Internal | Lowerers and emitters for Rust, WASM V1, SIMD patterns, template rendering, regen equality. | Replaces current core backend walker; PASS-2 makes BIR-only lowerers mandatory (`restart/audit/pass-2-codegen/PASS-2.md:5-8`). |
| `runtime` | Internal/public support | Tape, direct-to-struct builder support, generated grammar modules, visitors, document views. | Replaces hand-written per-grammar runtime dirs and OpenFrame-heavy flow. |
| `host` | Internal | Generic host primitives, `@host fn` registry, host chain typing/runtime dispatch. | Replaces grammar-specific shims such as `css_types.rs` and hardcoded host tables. |
| `cost-model` | Internal | Cost facts, SOTA profiles, extraction scoring, generated LOC budgets. | PASS-1 keeps a real cost model with SOTA gates (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `path` | Public | Rust macro/front-facing path DSL: `pointer!`, `select!`, visitor selectors. | Renames current `bbnf-path`; README requires `path`, `path-core`, `path-ts` (`restart/README.md:47-53`). |
| `path-core` | Internal/shared | Path parser, typed segments, evaluator core, diagnostics shared by Rust and TS. | Extract from `bbnf-path` and `bbnf-path-ts`. |
| `path-ts` | Public | TypeScript path package generated over `path-core` semantics. | Renames/splits current `bbnf-path-ts`; Lock 7 names this split (`restart/locks/14-LOCKS.md:46`). |
| `egraph` | Internal/sister | Equality saturation core and bridge APIs. | Keep and harden current `crates/egraph` (`restart/corpora/MODULES.md:136-162`). |
| `egraph-derive` | Internal/sister | Derive support for egraph term declarations. | Keep with `egraph` (`restart/corpora/MODULES.md:136-162`). |
| `csp-solver` | Internal/sister | Generic CSP solver used by type inference, layout choices, and extraction facts. | Keep and harden current generic solver (`restart/corpora/MODULES.md:73-132`). |
| `parse-that` | Internal/sister | Regex and parser substrate utilities, including Unicode-class implementation below BBNF. | New extraction target for regex support. |
| `simd-scan` | Internal/sister | SIMD scanner kernels for AVX2, AVX512, NEON, WASM SIMD, and scalar fallback. | Keep current clean crate (`restart/corpora/MODULES.md:47-69`). |
| `test-fixtures` | Internal/dev | Shared fixtures, parity matrix, generated snapshots, perf corpora. | New crate from legacy fixture work and BD fixture specs. |

Crates not in this list do not survive as production crates. `ser` and
`gorgeous` are archived before the first implementation tranche, as Lock 12
requires (`restart/locks/14-LOCKS.md:56`). `bootstrap` is slimmed into
bootstrap artifacts and developer commands, not kept as a first-class runtime
crate.

## 2. Dependency DAG

The workspace DAG must remain acyclic and at most six hops deep from any public
entrypoint to a leaf dependency. The README sets that ceiling and says sister
optimization crates compose by output piping rather than fusion
(`restart/README.md:64-92`).

```text
bbnf-cli
  -> bbnf
  -> pipeline
  -> bbnf-bench

bbnf
  -> runtime
  -> path
  -> host
  -> error

bbnf-language-server
  -> pipeline
  -> grammar
  -> ir
  -> passes
  -> runtime
  -> error
  -> source

bbnf-bench
  -> pipeline
  -> runtime
  -> cost-model
  -> test-fixtures

pipeline
  -> source
  -> grammar
  -> ir
  -> passes
  -> codegen
  -> error

grammar
  -> source
  -> error

ir
  -> grammar
  -> source
  -> error

passes
  -> ir
  -> egraph
  -> csp-solver
  -> cost-model
  -> parse-that
  -> simd-scan
  -> error

codegen
  -> ir
  -> host
  -> runtime
  -> cost-model
  -> simd-scan
  -> error

runtime
  -> source
  -> error
  -> host
  -> path-core

path
  -> path-core
  -> runtime
  -> error

path-ts
  -> path-core

vm
  -> ir
  -> runtime
  -> host
  -> error

parse-that
  -> error

cost-model
  -> ir
  -> error
```

The edge rules are:

| Rule | Consequence |
|---|---|
| No public crate is a dependency of an internal crate except where the public crate is only a facade-free data crate. | `runtime` depends on `path-core`, not `path`; `passes` depends on `ir`, not `bbnf`. |
| `runtime` never depends on `codegen`. | Code generation emits files into `runtime/src/grammars/<name>/`; runtime remains buildable without codegen at library use time. |
| `codegen` never reads Grammar IR directly for emitter logic. | Lowerers consume Backend IR and side tables, honoring Lock 5 (`restart/locks/14-LOCKS.md:42`). |
| `parse-that` has no `bbnf` dependency. | Unicode-class algebra remains regex-layer machinery, not grammar-level BBNF syntax. |
| `path-ts` consumes shared semantics, not Rust macro internals. | `path-core` is the single semantics owner, matching the path split in Lock 7 (`restart/locks/14-LOCKS.md:46`). |
| `egraph` and `csp-solver` remain generic. | They can be published or path-dep incubated without grammar concepts (`restart/locks/14-LOCKS.md:52-56`). |

## 3. Public API Surfaces

Public APIs must be small, grammar-neutral, and stable before they become
documentation commitments. The current codebase violates this in several
places by naming concrete parsers in metadata, registries, path mirrors, and
generated host shims (`restart/corpora/CENSUS.md:103-122`).

### 3.1 `bbnf`

```rust
pub struct GrammarHandle<G: Grammar> { /* private */ }

pub trait Grammar {
    type View<'a>: DocumentView<'a>
    where
        Self: 'a;

    fn name(&self) -> GrammarName;
    fn parse<'a>(&self, input: &'a [u8]) -> Result<Self::View<'a>, ParseError>;
    fn parse_in<'a>(
        &self,
        input: &'a [u8],
        arena: &'a Arena,
    ) -> Result<Self::View<'a>, ParseError>;
    fn parse_owned(&self, input: Vec<u8>) -> Result<OwnedDocument<Self>, ParseError>;
}
```

PASS-3 sets the user runtime API around `parse`, `parse_in`, `parse_owned`, and
`DocumentView` (`restart/audit/pass-3-runtime/PASS-3.md:42-80`). `parse` is
slice-borrow primary, `parse_in` is arena-aware, and `parse_owned` owns input
bytes for longer-lived documents. Lock 9 confirms this API family
(`restart/locks/14-LOCKS.md:50`).

Public exports:

| Export | Purpose |
|---|---|
| `Grammar`, `GrammarHandle`, `GrammarName` | Grammar-neutral entrypoint and identity. |
| `DocumentView`, `OwnedDocument`, `NodeView`, `TokenView` | User-facing views over tape/direct structures. |
| `ParseError`, `Diagnostic`, `Span` | Diagnostics re-exported from `error`/`source`. |
| `Value`, `ValueRef`, `ValueOwned` | Hybrid value API over grammar-derived structures. |
| `load_workspace`, `load_grammar`, `compile_workspace` | Facade over pipeline actions. |
| `host`, `path` modules | Narrow re-exports of host registration and path APIs. |

Private:

| Private item | Reason |
|---|---|
| Tape storage layout details | Users interact through document views and visitors. |
| Generated module paths | Names are metadata-derived and not hand-coded API. |
| Backend IR | Compiler contract, not user API. |
| Runtime checkpoints | Perf machinery, not semantic API. |

### 3.2 `bbnf-cli`

Commands:

| Command | Contract |
|---|---|
| `bbnf check` | Parse and validate grammar plus metadata. |
| `bbnf build` | Generate committed Rust outputs. |
| `bbnf parse` | Parse input and print selected projection. |
| `bbnf path` | Evaluate a path expression against a parsed document. |
| `bbnf debug ir` | Print Grammar IR, Backend IR, side tables, and VM trace. |
| `bbnf bench` | Dispatch to `bbnf-bench` profiles and SOTA gates. |
| `bbnf metadata init` | Create a workspace metadata stanza, never Rust match arms. |

The CLI may expose grammar names as user input, but it must not encode those
names in Rust switches. Lock 14 forbids generic crates from containing grammar
switches or modules (`restart/locks/14-LOCKS.md:60`).

### 3.3 `bbnf-language-server`

Public server contracts:

| Export | Purpose |
|---|---|
| `LanguageServer` | LSP service. |
| `DocumentSnapshot` | Immutable source snapshot. |
| `ReparsePlan` | Incremental parse plan. |
| `DiagnosticSet` | Recoverable diagnostics for editor display. |
| `SemanticIndex` | Symbols, rules, captures, host calls, layout and error directives. |

PASS-3 makes incremental parsing opt-in for batch and always-on for LSP
(`restart/audit/pass-3-runtime/PASS-3.md:137-158`); README says the same
(`restart/README.md:344-348`).

### 3.4 `path`, `path-core`, `path-ts`

Public Rust macros:

```rust
pointer!("/rules/0/name")
select!("rule[name='expr'] > alt:nth(0)")
```

Public shared concepts:

| Export | Owner | Purpose |
|---|---|---|
| `PathExpr` | `path-core` | Parsed typed path expression. |
| `Segment` | `path-core` | Pointer/select segment. |
| `CompiledPath` | `path-core` | Validated evaluator plan. |
| `PathDiagnostic` | `path-core` | Shared diagnostics. |
| `pointer!`, `select!` | `path` | Rust compile-time syntax. |
| `compilePath`, `select` | `path-ts` | TypeScript API over the same semantics. |

README keeps `pointer!`, `select!`, JSONPath-style selection, and read-write
visitor mutation in the user surface (`restart/README.md:272-318`).

### 3.5 Complete Public API Matrix

Every crate has a narrow exported surface. A crate may expose additional test
helpers under `cfg(test)` or crate-local integration features, but those helpers
are not part of the public contract.

| Crate | Public exports | Explicitly not public |
|---|---|---|
| `bbnf` | `Grammar`, `GrammarHandle`, `DocumentView`, `OwnedDocument`, `Value`, `ParseError`, `load_workspace`, `compile_workspace`, prelude. | Generated module internals, tape storage, BIR, pass scheduler. |
| `bbnf-cli` | Binary commands and stable output formats. | Pipeline internals, generated template paths. |
| `bbnf-language-server` | `LanguageServer`, `DocumentSnapshot`, `ReparsePlan`, `DiagnosticSet`, protocol entrypoints. | Incremental cache internals, parser registries, generated grammar module paths. |
| `bbnf-bench` | Bench profile names, SOTA report schema, fixture profile loader. | Microbench harness internals and machine-local cache paths. |
| `error` | `Diagnostic`, `DiagnosticCode`, `Severity`, `Report`, `RecoveryHint`, `SourceLabel`. | Formatting scratch buffers, code allocation tables. |
| `source` | `SourceFile`, `SourceId`, `Span`, `ByteRange`, `LineCol`, `Snapshot`, include graph handles. | Rope chunks, include DFS state, file watcher glue. |
| `grammar` | AST nodes, metadata schema, parser entry, semantic validation result. | Bootstrap parser implementation, recovery heuristics. |
| `ir` | Grammar IR, Backend IR, IDs, side-table schemas, validation traits. | Builder mutation state, arena allocation details. |
| `passes` | Pass traits, pass outputs, typed pipeline facts. | Pass-local caches, heuristic thresholds except through `cost-model` profiles. |
| `pipeline` | Workspace compiler entrypoints, stage results, artifact manifest. | Stage scheduler internals, cache key derivation details. |
| `vm` | BIR interpreter, replay trace, debug event schema. | Interpreter stack layout, bytecode-like scratch representation. |
| `codegen` | Lowerer traits, emit requests, generated artifact manifest. | Template renderer internals, backend-specific scratch trees. |
| `runtime` | Tape/direct document APIs, builders needed by generated code, visitor traits. | Token storage layout and checkpoint internals. |
| `host` | Host registry, primitive signatures, chain typing, dispatch handles. | Function pointer tables and metadata normalization state. |
| `cost-model` | Cost facts, profiles, SOTA gate schema, generated LOC budget API. | Raw scorer tuning internals and platform cache. |
| `path` | `pointer!`, `select!`, typed path wrappers, visitor selector helpers. | Macro parser scratch AST. |
| `path-core` | Path AST, parser, typed evaluator, diagnostics. | Rust macro glue and TypeScript emitter details. |
| `path-ts` | TypeScript package generation schema and exported TS API definitions. | Rust path macro internals. |
| `egraph` | Generic egraph arena, rewrite, extraction, explanation APIs. | BBNF bridge terms. |
| `egraph-derive` | Derive macro entrypoints for generic egraph terms. | Expansion scratch state. |
| `csp-solver` | Generic variables, domains, constraints, solver, explanations. | BBNF-specific fact conversion. |
| `parse-that` | Regex parser/program APIs, DFA/literal helpers, Unicode data wrappers. | BBNF grammar parser state. |
| `simd-scan` | Scanner traits, scalar/NEON/AVX dispatch handles, feature detection. | Intrinsic-specific loop bodies not needed by callers. |
| `test-fixtures` | Fixture manifest, corpus loader, parity matrix schema. | Local fixture generation scratch files. |

### 3.6 API Leakage Rules

| Leakage class | Forbidden example | Allowed replacement |
|---|---|---|
| Grammar parser type | `JsonParser`, `CssL4Parser` in generic code. | Metadata-derived `GrammarName` and generated manifest lookup. |
| Runtime grammar module | `runtime::json::Document` in public facade code. | `GrammarHandle<G>` and `DocumentView`. |
| Host shim file | `css_types.rs` as a generic dependency. | `@host fn` plus `host::registry`. |
| Strategy registry | `PRODUCTION_MANIFEST_TABLE` or `bbnf-strategy`. | Workspace metadata and recognizer facts. |
| Path mirror | Hardcoded TS path registry per grammar. | `path-core` schema emitted from shape facts. |
| Backend syntax | Lowerer peeking at `GrammarIr::Alt`. | `BackendIr::DispatchAlt` or `BackendIr::SpeculativeAlt`. |

These rules are direct consequences of Lock 14 and the current generalization
audit (`restart/locks/14-LOCKS.md:60`, `restart/corpora/CENSUS.md:103-122`).

## 4. Private Internals By Crate

The following trees are contracts for implementation planning. PASS-1, PASS-2,
and PASS-3 supply the corresponding crate tree expectations
(`restart/audit/pass-1-substrate/PASS-1.md:46-61`,
`restart/audit/pass-2-codegen/PASS-2.md:137-258`,
`restart/audit/pass-3-runtime/PASS-3.md:160-289`).

### 4.1 Core Frontend Crates

```text
bbnf/src/
  lib.rs
  prelude.rs
  parse/
  document/
  query/
  visitor/
  diagnostics/
  metadata/

bbnf-cli/src/
  main.rs
  commands/
  output/
  workspace/
  debug/
  bench.rs

bbnf-language-server/src/
  lib.rs
  server/
  document/
  diagnostics/
  semantic/
  protocol/
  debug/

bbnf-bench/src/
  lib.rs
  profiles/
  corpus/
  gates/
  report/
```

The `bbnf` aggregator carries exactly 8 immediate children — `lib.rs`,
`prelude.rs`, `parse/`, `document/`, `query/`, `visitor/`, `diagnostics/`,
`metadata/` — which satisfies Lock 13's 4-10 child-count rule and the
HARDENING-CONSOLIDATED §4.19 fix for the prior 7-children divergence. `tape/`
and `value/` live under `runtime/src/{tape,value}/` per Lock 1; the aggregator
re-exports the substrate cursor types and the typed-root projection through
`prelude.rs` rather than carrying duplicate sibling directories. The
grammar-specific surface (`Json`, `CssL4`, etc.) is generated under
`runtime/src/grammars/<name>/` and referenced from `metadata/`, never as a
sibling of `parse/` or `document/`.

### 4.2 Compiler Pipeline Crates

```text
error/src/
  lib.rs
  diagnostic/
  codes/
  report/
  recovery/

source/src/
  lib.rs
  file/
  span/
  include/
  snapshot/
  rope/

grammar/src/
  lib.rs
  ast/
  parse/
  validate/
  metadata/
  bootstrap/

ir/src/
  lib.rs
  grammar_ir/
  backend_ir/
  side_tables/
  validate/
  pretty/

pipeline/src/
  lib.rs
  workspace/
  stages/
  cache/
  emit/
  verify/

passes/src/
  lib.rs
  normalize/
  types/
  shapes/
  recognizers/
  extract/
  bridge/

vm/src/
  lib.rs
  interp/
  trace/
  replay/
  debug/
```

### 4.3 Runtime And Backend Crates

```text
codegen/src/
  lib.rs
  lower/
  rust/
  wasm/
  simd/
  templates/
  verify/

runtime/src/
  lib.rs
  tape/
  document/
  builder/
  visitor/
  grammars/
  support/

host/src/
  lib.rs
  registry/
  primitives/
  chain/
  types/
  dispatch/

cost-model/src/
  lib.rs
  facts/
  profiles/
  score/
  gates/
  loc_budget/
```

### 4.4 Sister And Path Crates

```text
path-core/src/
  lib.rs
  ast/
  parse/
  typecheck/
  eval/
  diagnostic/

path/src/
  lib.rs
  macro_impl/
  typed/
  visitor/

path-ts/src/
  lib.rs
  schema/
  emit/
  tests/

egraph/src/
  lib.rs
  arena/
  rewrite/
  extract/
  explain/

egraph-derive/src/
  lib.rs

csp-solver/src/
  lib.rs
  domain/
  constraint/
  solve/
  explain/
  tests/

parse-that/src/
  lib.rs
  regex/
  dfa/
  unicode/
  literal/

simd-scan/src/
  lib.rs
  scalar/
  neon/
  avx2/
  avx512/
  wasm/
  dispatch/

test-fixtures/src/
  lib.rs
  corpus/
  matrix/
  snapshots/
  generated/
```

No directory may become a dumping ground. Lock 13 sets the 4-10 child target
and the 500 LOC handwritten file ceiling, generated files excepted
(`restart/locks/14-LOCKS.md:58`).

### 4.5 Complete Private Internals Matrix

| Crate | Private internals that must not leak through public API | Rationale |
|---|---|---|
| `bbnf` | Generated module paths, pipeline stage graph, tape token layout. | Facade stability and grammar neutrality. |
| `bbnf-cli` | Command implementation modules, workspace discovery heuristics. | CLI output is stable; internal command plumbing is not. |
| `bbnf-language-server` | Incremental cache, semantic index storage, transport glue. | Editor clients consume protocol results, not cache shapes. |
| `bbnf-bench` | Machine-local measurements, warmup state, raw profiler hooks. | Reports are portable; local bench state is not. |
| `error` | Diagnostic code allocator and formatter buffers. | Codes and reports are stable; allocation mechanics are not. |
| `source` | Rope chunks, include DFS work queue, filesystem watcher adapters. | Callers need spans and snapshots only. |
| `grammar` | Bootstrap parser tables and recovery fallbacks. | Grammar AST and metadata schema are the contract. |
| `ir` | Arena slots, builder cursors, validation scratch state. | IR values and side-table schemas are the boundary. |
| `passes` | Heuristic caches, intermediate mining worklists. | Pass outputs are durable; mining internals can change. |
| `pipeline` | Cache-key internals, scheduler topology, artifact temp paths. | Pipeline users consume stage results and manifests. |
| `vm` | Interpreter stack representation, trace buffer allocation. | Replay events are stable; execution machinery is not. |
| `codegen` | Template AST, emitter scratch files, backend work queues. | Generated artifacts and lowerer trait are the boundary. |
| `runtime` | Tape storage vectors, checkpoint frames, direct builder scratch slots. | Runtime users consume views and visitors. |
| `host` | Raw function-pointer table, metadata normalization maps. | Registry and typed dispatch handles are enough. |
| `cost-model` | Scorer coefficients before profile publication, raw sample cache. | Profiles and gate reports are stable. |
| `path` | Macro parser scratch tokens and hygiene helpers. | Users get macros and typed expressions. |
| `path-core` | Parser state machine and evaluator stack representation. | Path AST, diagnostics, and compiled plans are stable. |
| `path-ts` | Emitter templates and package staging directories. | TS API and schema are stable. |
| `egraph` | Arena compaction state and extraction work queues. | Generic API remains clean. |
| `egraph-derive` | Token expansion scratch modules. | Macro output is the visible contract. |
| `csp-solver` | Propagation queue internals and search heuristics. | Solver inputs, outputs, and explanations are stable. |
| `parse-that` | Unicode table generation scratch data and DFA builder state. | Regex program APIs are stable. |
| `simd-scan` | Intrinsic loop bodies and dispatch probe cache. | Scanner trait and dispatch handle are stable. |
| `test-fixtures` | Local fixture generation scratch state. | Fixture manifests and corpus loaders are stable. |

## 5. Cargo And Workspace Metadata

The current root metadata contains a grammar array plus a hardcoded
`bbnf-strategy` table with parser type names and builder paths. That pattern
requires code or metadata updates beyond adding a grammar, and the current file
even says a new grammar must be added to both the metadata and a production
manifest table (`Cargo.toml:18-29`, `Cargo.toml:41-56`). The restart replaces
that with exactly two onboarding surfaces:

1. A grammar source file, normally `grammars/<name>.bbnf`.
2. One metadata block: `[workspace.metadata.bbnf.grammars.<name>]`.

The README states this constraint directly: onboarding is `.bbnf` plus
workspace metadata, with no Rust crate, no per-grammar match arms, and no
manual registries (`restart/README.md:11-25`).

Canonical workspace skeleton:

```toml
[workspace]
resolver = "2"
members = [
  "crates/bbnf",
  "crates/bbnf-cli",
  "crates/bbnf-language-server",
  "crates/bbnf-bench",
  "crates/error",
  "crates/pipeline",
  "crates/source",
  "crates/grammar",
  "crates/ir",
  "crates/passes",
  "crates/vm",
  "crates/codegen",
  "crates/runtime",
  "crates/host",
  "crates/cost-model",
  "crates/path",
  "crates/path-core",
  "crates/path-ts",
  "crates/egraph",
  "crates/egraph-derive",
  "crates/csp-solver",
  "crates/parse-that",
  "crates/simd-scan",
  "crates/test-fixtures",
  "xtask",
]

[workspace.package]
edition = "2021"
license = "MIT OR Apache-2.0"
repository = "https://github.com/mkbabb/bbnf-lang"

[workspace.dependencies]
bbnf-error = { path = "crates/error", package = "error" }
bbnf-source = { path = "crates/source", package = "source" }
bbnf-grammar = { path = "crates/grammar", package = "grammar" }
bbnf-ir = { path = "crates/ir", package = "ir" }
bbnf-runtime = { path = "crates/runtime", package = "runtime" }
bbnf-host = { path = "crates/host", package = "host" }
```

Package names can be adjusted during implementation to satisfy Cargo naming
rules and publication policy, but the crate ownership and dependency graph do
not change without an explicit architecture amendment.

Canonical schema:

```toml
[workspace.metadata.bbnf]
generated_root = "crates/runtime/src/grammars"
fixture_root = "crates/test-fixtures/corpus"
profile = "balanced"
host_registry = "default"

[workspace.metadata.bbnf.recognizers]
pratt = "auto"
simd = "auto"
literal_trie = "auto"
regex_prefilter = "auto"

[workspace.metadata.bbnf.host_fns]
default_registry = "host::primitives"
allow_unregistered = false

[workspace.metadata.bbnf.grammars.json]
source = "grammars/json.bbnf"
package_name = "json"
features = []
output_dir = "crates/runtime/src/grammars/json"

[workspace.metadata.bbnf.grammars.json.runtime]
mode = "tape-direct"
document_view = true
owned_document = true

[workspace.metadata.bbnf.grammars.json.host]
registry = "default"
allow_declaration_crate = false
declaration_crate_reason = ""

[workspace.metadata.bbnf.grammars.json.optimization]
profile = "balanced"
recognizers = "auto"
pratt = "auto"
simd = "auto"
layout = "auto"
regex_prefilter = "auto"

[workspace.metadata.bbnf.grammars.json.codegen]
rust = true
wasm = false
generated_loc_budget = 1.02

[workspace.metadata.bbnf.grammars.json.fixtures]
valid = ["tests/fixtures/json/valid"]
invalid = ["tests/fixtures/json/invalid"]
perf = ["tests/perf/json"]
```

Schema rules:

| Rule | Enforcement |
|---|---|
| Metadata may name files, profiles, and feature flags. | `grammar::metadata` validates paths and profile names. |
| Metadata may not name Rust parser types, generated modules, or builder structs. | `pipeline::workspace` rejects Rust-looking paths and known old strategy keys. |
| `allow_declaration_crate = true` requires an explicit reason and review gate. | Lock 14 makes declaration crates rare escape valves (`restart/locks/14-LOCKS.md:60`). |
| `pratt`, `simd`, and recognizers default to `auto`. | Lock 10 says Pratt and SIMD are auto-detected, not directives (`restart/locks/14-LOCKS.md:52`). |
| `wasm = true` selects the WASM V1 lowerer. | PASS-2 defines WASM V1 as a wasm32 Rust binding path, not a separate hand emitter (`restart/audit/pass-2-codegen/PASS-2.md:32-49`). |
| Adding a grammar must not touch Rust source. | The future grammar test in this document makes that a hard gate. |

Metadata validation errors are normal diagnostics, not panics. They flow
through `error` so CLI and LSP report the same code for the same bad metadata.

Declaration-crate review form. Per HARDENING-CONSOLIDATED §4.15, every escape
valve carries the following eight fields. The exception table is empty for the
nine extant grammars (`bbnf`, `bnf`, `csv`, `css_l4`, `css_pretty`, `ebnf`,
`google_sheets`, `json`, `math`) and stays empty unless a metadata + `@host fn`
demonstration of insufficiency lands first.

| Field | Required content |
|---|---|
| Reason | The specific boundary requiring a per-grammar declaration crate (e.g., a trait-impl bridge that `host::primitives` cannot express, or a backend-specific FFI that `@host fn` bodies cannot express). State the boundary in mechanism terms, not in convenience terms. |
| Owner | The named human or team accountable for the declaration crate's source, regen discipline, and deletion path. |
| Why metadata fails | Specific demonstration that `[workspace.metadata.bbnf.grammars.<name>]` cannot describe the boundary. Cite metadata schema lines that would have to grow and explain why the growth contaminates the generic schema. |
| Why `@host fn` fails | Specific demonstration that a block-bodied `@host fn` decomposing into `host::primitives` cannot express the boundary. Cite the generic primitive set considered and the gap that survives. |
| Declaration location | Explicit path, normally `runtime/src/grammars/<name>/decl/` (sub-module of the per-grammar generated runtime). The declaration crate may not live at a workspace-level path that pollutes generic crate graphs. |
| No generic import | Proof that no generic crate (`bbnf`, `pipeline`, `passes`, `ir`, `codegen`, `runtime`, `host`, `path`, `path-core`, `egraph`, `csp-solver`, `parse-that`, `simd-scan`) imports the per-grammar declaration crate. The proof is a `rg` command in the review record. |
| Deletion path | Explicit named condition that retires the declaration crate (e.g., "deletes when `host::primitives::<name>` lands and the per-grammar trait moves to BBNF metadata"). The deletion path must terminate; "indefinite" is rejected. |
| Reviewer | Named human (architecture owner) plus the receiving tranche gate where the exception is reviewed (e.g., A.W4 metadata-schema gate, J.W2 close gate). The reviewer is distinct from the owner. |

Reified as TOML under `[workspace.metadata.bbnf.grammars.<name>.declaration_crate]`:

```toml
[workspace.metadata.bbnf.grammars.<name>.declaration_crate]
allow = false
reason = ""
owner = ""
why_metadata_fails = ""
why_host_fn_fails = ""
declaration_location = "runtime/src/grammars/<name>/decl/"
no_generic_import_proof = ""
deletion_path = ""
reviewer = ""
receiving_gate = ""
```

`allow = true` requires every other field to be populated; the metadata
validator rejects partial fences. `allow = false` (the default for all nine
extant grammars) treats the remaining keys as documentation slots and writes
nothing to the runtime. The fence is reviewed at A.W4 (metadata-schema close)
and re-verified at every receiving tranche named in `receiving_gate`.

## 6. Pipeline

The pipeline order is fixed:

```text
source load
  -> BBNF parse
  -> semantic validation
  -> type inference
  -> shape mining
  -> recognizer mining
  -> egraph rewrite
  -> CSP solve
  -> cost extraction
  -> Backend IR
  -> lowerers
  -> template emit
  -> regen equality
```

README names the core order as parse, validation, inference, shape mining,
egraph, cost extraction, Backend IR, lowerers, and regen equality
(`restart/README.md:188-207`). PASS-1 adds CSP/egraph bridge facts and keeps
the optimizer crates separate (`restart/audit/pass-1-substrate/PASS-1.md:46-61`).

The pipeline invariants are:

| Invariant | Owner |
|---|---|
| Source and metadata validation happen before Grammar IR construction. | `grammar`, `pipeline` |
| Type inference annotates Grammar IR; it does not mutate grammar syntax. | `passes::types` |
| Shape mining produces side tables. | `passes::shapes` |
| Recognizer mining produces side tables and BIR hints. | `passes::recognizers` |
| Egraph and CSP exchange facts through bridge tables. | `passes::bridge` |
| Cost extraction selects from alternatives; it does not introduce new grammar semantics. | `cost-model`, `passes::extract` |
| Backend IR is the only input to lowerers. | `codegen::lower` |
| Generated output is committed and equality-checked. | `codegen::verify`, `pipeline::verify` |

Cursor and skip gates:

| Lock 3 proof | Required test |
|---|---|
| Empty-path parse does not allocate a cursor path. | `__EAGER_EMPTY_PATH` regression fixture. |
| Byte-skipping is explicit and observable. | `CursorDecision::Skip` unit and VM replay fixture. |
| Scanner fast path preserves diagnostics. | skipped spans round-trip through tape and LSP diagnostics. |

## 7. IR Contract

There are two IRs plus side tables. README fixes that architecture
(`restart/README.md:104-118`), and PASS-1 gives the concrete starting shape
(`restart/audit/pass-1-substrate/PASS-1.md:24-42`).

### 7.1 Grammar IR

Grammar IR is semantic and close to the BBNF source. It keeps grammar-level
meaning, typed annotations, host references, layout/error directives, and
lookbehind.

Initial variants:

| Variant | Purpose |
|---|---|
| `Rule` | Named rule with generic parameters, attributes, type annotations, and body. |
| `Seq` | Ordered composition. |
| `Alt` | Alternatives. |
| `Repeat` | Repetition with min/max and separator metadata. |
| `Optional` | Optional expression. |
| `Literal` | Byte/string literal. |
| `Regex` | Regex expression owned by `parse-that/regex`. |
| `Ref` | Rule reference with type arguments. |
| `Predicate` | Lookahead and grammar predicate forms. |
| `Lookbehind` | Positive/negative bounded lookbehind. |
| `Map` | Semantic mapping expression. |
| `HostCall` | `@host fn` call or chain segment. |
| `LayoutDirective` | `@layout` policy. |
| `ErrorDirective` | `@error` recovery vocabulary hook. |
| `Annotation` | Explicit type, cost, profile, or docs annotation. |

Grammar IR payload and lowering matrix:

| Variant | Payload shape | Lower-time invariant | Main BIR consumer |
|---|---|---|---|
| `Rule` | Name, generics, signature, body ID, annotations. | Rule is typechecked and metadata-resolved. | `Entry`, `CallRule`. |
| `Seq` | Ordered expression IDs. | Empty sequence is normalized before BIR. | `Seq`. |
| `Alt` | Alternative IDs plus dispatch hints. | Byte-disjoint alts are marked before extraction. | `DispatchAlt`, `SpeculativeAlt`. |
| `Repeat` | Body ID, min/max, separator, greediness. | Nullable body is rejected or guarded. | `RepeatLoop`. |
| `Optional` | Body ID. | Lowered without changing capture shape. | `OptionalBranch`. |
| `Literal` | Byte string, case policy, span. | Encoding is known and stable. | `ByteLiteral`, `SimdScan`. |
| `Regex` | Regex program handle, flags, span. | Regex parsed by `parse-that/regex`. | `RegexProgram`, `SimdScan`. |
| `Ref` | Target rule ID, type args, call annotations. | Target resolves after generics instantiate. | `CallRule`. |
| `Predicate` | Kind and expression ID. | Predicate has no consuming side effects. | `DispatchAlt` hints or guard BIR. |
| `Lookbehind` | Kind, bounded body ID, width facts. | Width is proven bounded. | `SpeculativeAlt`, guard BIR. |
| `Map` | Source expression, target type/value expression. | Output type agrees with shape facts. | `ValueProject`, `DirectBuild`. |
| `HostCall` | Function ID, args, chain segment IDs. | Host signature and chain types compose. | `CallHost`, `HostChain`. |
| `LayoutDirective` | Policy ID, body/span. | Policy is scoped. | `LayoutPush`, `LayoutPop`. |
| `ErrorDirective` | Recovery code, sync rules, body/span. | Recovery code is registered. | `ErrorRecover`. |
| `Annotation` | Key/value typed metadata. | Annotation key is known or fenced. | Side tables and diagnostics. |

Grammar IR invariants:

| Invariant | Gate |
|---|---|
| No backend-specific node names. | `ir::validate::grammar_ir_has_no_backend_nodes`. |
| No grammar-level rewrite-mode node. | Parser rejects rewrite syntax; IR enum has no variant. |
| Regex Unicode classes are opaque regex data. | `Regex` stores parsed regex program handles, not BBNF class algebra. |
| Lookbehind is bounded before Backend IR. | Type/inference pass proves or rejects unbounded lookbehind. |
| Host calls resolve through generic primitives or declared `@host fn` signatures. | `host::types` and `passes::types`. |

### 7.2 Backend IR

Backend IR is executable and lowerer-facing. PASS-2 makes it the single backend
contract and supplies the final 23-variant shape (`restart/audit/pass-2-codegen/PASS-2.md:52-76`).

| Variant | Purpose |
|---|---|
| `Entry` | Backend function entrypoint. |
| `Seq` | Lowered ordered execution. |
| `DispatchAlt` | Predictive alternative dispatch. |
| `SpeculativeAlt` | Alternative with checkpoint/rollback. |
| `RepeatLoop` | Lowered repetition. |
| `OptionalBranch` | Lowered optional branch. |
| `ByteLiteral` | Byte/string literal check. |
| `RegexProgram` | Lowered regex program handle. |
| `SimdScan` | SIMD scanner operation. |
| `PrattSpine` | Pratt parser spine. |
| `CallRule` | Rule call. |
| `CallHost` | Single host call. |
| `HostChain` | Multi-function host chain. |
| `LayoutPush` | Enter layout policy. |
| `LayoutPop` | Exit layout policy. |
| `ErrorRecover` | Recovery site. |
| `SpanMark` | Span/tape marker. |
| `TapeEmit` | Emit tape token/event. |
| `DirectBuild` | Build typed direct view. |
| `ValueProject` | Project value shape. |
| `PathEval` | Path-evaluator hook. |
| `DebugMark` | VM/debug trace marker. |
| `Return` | Backend return from entry/rule. |

If an implementation keeps PASS-1's 22-variant table without `Return`, it must
prove equivalent control-flow closure before codegen. The architecture default
is PASS-2's final 23-variant table.

Backend IR payload and lowerer matrix:

| Variant | Payload shape | Rust lowerer | VM behavior | WASM/SIMD note |
|---|---|---|---|---|
| `Entry` | Symbol, input mode, output mode, body block. | Emits public/internal function. | Starts frame. | Exported only through WASM facade. |
| `Seq` | Ordered BIR node IDs. | Emits straight-line control flow. | Runs children in order. | No special handling. |
| `DispatchAlt` | Discriminator facts, alt targets. | Emits match/table dispatch. | Chooses deterministic alt. | SIMD may feed discriminator. |
| `SpeculativeAlt` | Alt targets, checkpoint policy. | Emits bounded checkpoint/rollback. | Saves and restores frame. | Must not clone OpenFrame stacks. |
| `RepeatLoop` | Body, min/max, exit guard. | Emits loop with progress guard. | Iterates with progress check. | SIMD may accelerate body prefix. |
| `OptionalBranch` | Body and empty branch shape. | Emits branch. | Runs or skips. | No special handling. |
| `ByteLiteral` | Bytes, case policy, span. | Emits byte compare. | Consumes on match. | SIMD may widen compare. |
| `RegexProgram` | Regex program handle. | Calls regex engine. | Executes regex VM/DFA. | Unicode stays below BBNF. |
| `SimdScan` | Scanner kind, needle/class, fallback. | Emits dispatch to `simd-scan`. | Calls scalar reference. | Main SIMD hook. |
| `PrattSpine` | Operators, precedence, associativity, atom rule. | Emits Pratt loop. | Executes Pratt interpreter. | Auto-detected only. |
| `CallRule` | Callee ID, args, result slot. | Emits function call. | Pushes rule frame. | No special handling. |
| `CallHost` | Host function ID, args, result slot. | Emits registry dispatch. | Calls host shim. | WASM requires ABI-safe wrapper. |
| `HostChain` | Ordered host call IDs. | Emits typed chain. | Runs chain left-to-right. | ABI-safe at boundary only. |
| `LayoutPush` | Layout policy ID. | Emits scoped policy push. | Pushes layout state. | No special handling. |
| `LayoutPop` | Layout policy ID. | Emits scoped pop. | Pops layout state. | No special handling. |
| `ErrorRecover` | Recovery code, sync set, resume target. | Emits diagnostic and recovery path. | Records diagnostic and resumes. | LSP consumes same code. |
| `SpanMark` | Start/end marker kind. | Emits span capture. | Records mark. | No special handling. |
| `TapeEmit` | Token/event kind, span/value refs. | Appends tape token. | Appends reference token. | No special handling. |
| `DirectBuild` | Shape ID, field slots, source refs. | Builds typed direct view. | Builds reference view. | Must share tape spans. |
| `ValueProject` | Shape ID, path/value projection. | Emits projection helper. | Evaluates projection. | Path typing consumes shape. |
| `PathEval` | Compiled path ID, input view. | Calls `path-core` evaluator. | Evaluates path. | TS uses same schema. |
| `DebugMark` | Event label, node ID, span. | Emits trace hook if enabled. | Emits trace event. | Debug-only in WASM unless enabled. |
| `Return` | Return value/control mode. | Emits return. | Pops frame. | WASM maps to ABI return. |

Example source-to-BIR coverage:

| BIR variant | Example source fragment or compiler source | Notes |
|---|---|---|
| `Entry` | `json = value;` | Grammar entry metadata selects exported entry. |
| `Seq` | `"a" "b"` | Ordered expression. |
| `DispatchAlt` | `"true" | "false" | "null"` | Byte-disjoint alternatives. |
| `SpeculativeAlt` | `ident | keyword` when prefix overlaps. | Requires checkpoint. |
| `RepeatLoop` | `digit+` | Progress guard required. |
| `OptionalBranch` | `sign? number` | Empty branch keeps shape. |
| `ByteLiteral` | `"{"` | Byte literal. |
| `RegexProgram` | `/[0-9]+/` | Regex program is opaque to BBNF. |
| `SimdScan` | Long literal set or regex prefilter. | Mined by recognizer pass. |
| `PrattSpine` | Expression grammar with precedence pattern. | Auto-detected, no directive. |
| `CallRule` | `value` inside another rule. | Rule reference. |
| `CallHost` | `@trim(text)` | Single host call. |
| `HostChain` | `@decode(x).normalize().intern()` | Chain semantics. |
| `LayoutPush` | `@layout indent { ... }` | Scoped policy. |
| `LayoutPop` | End of layout body. | Compiler-generated pair. |
| `ErrorRecover` | `@error missing_semicolon { ... }` | Recovery site. |
| `SpanMark` | Any captured rule. | Compiler-generated span boundaries. |
| `TapeEmit` | Any token/node event. | Compiler-generated. |
| `DirectBuild` | Rule with struct-like shape. | ShapeFacts consumer. |
| `ValueProject` | Map or public value view. | Value API consumer. |
| `PathEval` | Generated visitor/path hook. | Path crate consumer. |
| `DebugMark` | Debug profile enabled. | VM/replay consumer. |
| `Return` | End of rule entry. | Compiler-generated. |

Backend IR invariants:

| Invariant | Gate |
|---|---|
| Lowerers never inspect Grammar IR. | Compile-time module boundary, import-deny tests, and `ir::backend_ir` snapshots. |
| Tape and direct-to-struct are one materialization strategy. | `TapeEmit` and `DirectBuild` are scheduled together from side tables. |
| OpenFrame clone stacks are absent. | Generated code review plus perf gate. |
| SIMD and Pratt are mined, not syntax-directed. | `passes::recognizers` owns detection. |
| VM can replay all BIR variants. | `vm::replay` golden tests. |

### 7.3 Side Tables

The optimized IR is not a third core IR. README says optimized IR is side-table
data, not another central tree (`restart/README.md:104-118`). Per Lock 2 and
HARDENING-CONSOLIDATED §3 conflict #4, the layout-lowering pass is the public
surface; HM/CSP type checking is its internal subroutine. `TypeFacts` is an
internal scratch artefact of `passes::layout` (used by HM unification and CSP
constrained choice) and never appears as a public side table; downstream passes
read `LayoutFacts` instead. Public side tables are:

| Table | Producer | Consumer | Visibility |
|---|---|---|---|
| `LayoutFacts` | `passes::layout` (folds HM + bidirectional + CSP into layout decisions). | Backend IR builder (`LayoutPush`, `LayoutPop`), host registry, diagnostics. | Public. |
| `ShapeFacts` | Shape mining. | Direct builder, Value API, path typing. | Public. |
| `RecognizerFacts` | Recognizer mining. | BIR builder, SIMD/Pratt lowerers. | Public. |
| `EGraphFacts` | Egraph bridge. | Cost extraction. | Public. |
| `CspSolution` | CSP solver (when called by `passes::layout` or other clients). | Cost extraction, layout, host chain typing. | Public when produced for cost extraction; internal when produced inside layout lowering. |
| `CostFacts` | Cost model. | Backend IR extraction, benchmark report. | Public. |
| `RecoveryFacts` | Error pass. | `ErrorRecover`, LSP diagnostics. | Public. |
| `TypeFacts` | HM + bidirectional checker (internal to `passes::layout`). | `passes::layout` only. | Internal subroutine artefact; not exported across pass boundaries. |

## 8. BBNF Language Surface

The BBNF surface supports the extensions settled in README and PASS-1:
lookbehind, `@host fn`, multi-function chaining, generics, `@error`, and
`@layout` (`restart/README.md:121-182`,
`restart/audit/pass-1-substrate/PASS-1.md:84-121`). It does not contain
rewrite-mode or grammar-level Unicode class algebra.

### 8.1 Core Grammar Sketch

```ebnf
Grammar       ::= Item*
Item          ::= HostDecl | RuleDecl | LayoutDecl | ErrorDecl | Annotation

RuleDecl      ::= Ident GenericParams? TypeAnnotation? "=" Expr ";"
GenericParams ::= "<" GenericParam ("," GenericParam)* ">"
GenericParam  ::= Ident BoundList?
BoundList     ::= ":" Bound ("+" Bound)*

Expr          ::= Alt
Alt           ::= Seq ("|" Seq)*
Seq           ::= Prefix*
Prefix        ::= Lookahead | Lookbehind | Suffix
Lookahead     ::= "&" Suffix | "!" Suffix
Lookbehind    ::= "|<" Suffix | "|<!" Suffix
Suffix        ::= Primary Quantifier?
Quantifier    ::= "?" | "*" | "+" | "{" Number ("," Number?)? "}"
Primary       ::= Literal
                | Regex
                | Ref
                | HostCall
                | MapExpr
                | "(" Expr ")"

Ref           ::= Ident TypeArgs?
TypeArgs      ::= "<" Type ("," Type)* ">"
HostCall      ::= "@" Ident "(" HostArgs? ")" Chain*
Chain         ::= "." Ident "(" HostArgs? ")"
MapExpr       ::= Expr "=>" TypeExpr

HostDecl      ::= "@host" "fn" Ident GenericParams? "(" ParamList? ")" "->" Type ";"
LayoutDecl    ::= "@layout" Ident LayoutBody
ErrorDecl     ::= "@error" Ident ErrorBody
Annotation    ::= "@" Ident AnnotationBody?
Regex         ::= "/" RegexProgram "/"
```

`RegexProgram` is parsed by `parse-that/regex`. Unicode classes may exist
inside that regex syntax, but BBNF itself does not expose a set algebra surface.

Input-normalization deletions are explicit and named. Every surface listed below
is forbidden from the BBNF grammar and from any IR variant; the named substrate
is the only place the corresponding capability may live.

| Surface | Status at BBNF level | Routed substrate | Closing gate |
|---|---|---|---|
| Rewrite-mode (transducer/rewrite ladders). | Deleted; not parsed, not lowered, not represented in either IR. | None; rewrite-mode does not survive as a feature. | `rg "rewrite-mode\|RewriteMode\|@rewrite"` returns zero outside this deletion table. |
| Unicode class set algebra (`[[:alpha:]]&&[^a-z]`, `\p{L}--\p{Cyrillic}`). | Deleted from BBNF grammar surface. | `parse-that/regex` HIR may model class algebra inside a regex literal; BBNF never exposes it as productions. | Architecture §8.1 has no `CharClass`/`ClassExpr` production; `parse-that/regex` carries the algebra under HIR. |
| Grammar-level `(?<= ...)` lookbehind syntax. | Deleted in favor of `|<` and `|<!`. | `parse-that/regex` retains regex-internal `(?<= ...)`; BBNF parser rejects it outside regex literals. | PASS-1 spec uses `|<` only; lookbehind diagnostic carries `BBNF-LOOKBEHIND-WIDTH`. |
| Standalone `@recover` directive. | Folded into `@error(recover = ...)`; not a separate top-level form. | `error` crate keeps the recovery descriptor; grammar parser rejects bare `@recover`. | PASS-3 amendment; no production for `Recover ::= ...` outside `@error` body. |
| Per-grammar declaration crates. | Not default; allowed only via fenced metadata escape valve. | Workspace `[workspace.metadata.bbnf.grammars.<name>]` plus the §5.6 review form. | Lock 14 lint passes; the §5.6 fence supplies reason/scope/deletion path. |

### 8.2 Type System

The type system is Hindley-Milner plus bidirectional checks and CSP constraints.
README sets HM, bidirectional typing, CSP use, explicit annotations, generic
rules, subtyping/coercion, and lookbehind types as in-scope
(`restart/README.md:258-268`). PASS-1 adds host/chaining/generic constraints
to the type contract (`restart/audit/pass-1-substrate/PASS-1.md:24-42`).

Type rules:

| Rule | Contract |
|---|---|
| Inference is grammar-wide. | Rule references and host calls unify inside `passes::layout` through internal `TypeFacts`; downstream passes consume `LayoutFacts`, never `TypeFacts` directly. |
| Annotations narrow, not bypass, inferred types. | Contradictions are diagnostics. |
| Host functions are typed. | `@host fn` declarations and generic primitives share the same checker. |
| Chains compose left-to-right. | Output of one host call is input to the next. |
| Lookbehind must be bounded. | Unbounded lookbehind is rejected before Backend IR. |
| Layout and error directives are typed side effects. | They produce `LayoutFacts` and `RecoveryFacts`, not ad hoc codegen flags. |

### 8.3 Host Functions

Host functions decompose through:

1. Generic primitives in `host`.
2. Workspace metadata.
3. `@host fn` declarations.

Per-grammar declaration crates are not a default escape hatch. A declaration
crate can only be introduced when metadata and `@host fn` cannot represent the
host boundary; it must be fenced with an explicit reason and a deletion path.
This follows the current dispatch authority and Lock 14
(`restart/locks/14-LOCKS.md:60`).

### 8.4 Closure Semantics

Closure semantics are intentionally narrow. They exist to model host chains and
typed grammar mappings without turning BBNF into a general programming
language.

| Form | Captures | Type rule | Runtime rule |
|---|---|---|---|
| Host chain closure | Previous host result and explicit args only. | Output of segment N unifies with input of segment N+1. | Lowered to `HostChain`. |
| Map closure | Matched value, named captures, explicit annotations. | Map result unifies with rule output shape. | Lowered to `ValueProject` and/or `DirectBuild`. |
| Predicate closure | Read-only parser state and explicit expression. | Must return boolean-like predicate type. | No tape/direct side effects. |
| Recovery closure | Diagnostic context and sync facts. | Must produce registered recovery code or hint. | Lowered to `ErrorRecover`. |

Forbidden closure behavior:

| Forbidden behavior | Reason |
|---|---|
| Capturing arbitrary host process state through grammar syntax. | Host state belongs in `host` registry and metadata. |
| Mutating parse input. | Source snapshots are immutable. |
| Emitting runtime code directly from Grammar IR. | Backend IR is the lowerer boundary. |
| Encoding rewrite-mode as closure sugar. | Rewrite-mode is out. |

## 9. Runtime Architecture

Tape and direct-to-struct are a single substrate family. The README names the
runtime as tape plus direct-to-struct, with mutation through a read-write
visitor only (`restart/README.md:285-318`). PASS-3 gives the tape/direct
architecture and an illustrative token model (`restart/audit/pass-3-runtime/PASS-3.md:96-135`).

### 9.1 Tape

`runtime/src/tape` owns:

| Module | Purpose |
|---|---|
| `token` | Token/event representation. |
| `builder` | Append-only builder with bounded checkpoints. |
| `span` | Span and source slice linkage. |
| `view` | Borrowed read views. |
| `trace` | Debug/replay event hooks. |

Tape invariants:

| Invariant | Gate |
|---|---|
| Append-only after committed checkpoints. | Builder tests. |
| Rollback is bounded and does not clone OpenFrame stacks. | Perf tests and code review. |
| Tokens borrow source slices where possible. | Slice-borrow API tests. |
| Direct views can point into tape. | `DocumentView` tests. |

### 9.2 Direct-To-Struct Union

Direct builders do not bypass the tape. They are scheduled with `TapeEmit` and
`DirectBuild` from Backend IR and share spans, source slices, and diagnostics.
The direct value is a typed view/projection over the same parse event stream.

Generated per-grammar runtime lives under:

```text
runtime/src/grammars/<grammar>/
  mod.rs
  generated.rs
  view.rs
  value.rs
  visitor.rs
  host.rs
```

The files are template-emitted. They are not hand-written production crates.
PASS-2 requires this runtime template shape and says generated runtime modules
are emitted under `runtime/src/grammars/<name>` (`restart/audit/pass-2-codegen/PASS-2.md:98-116`).

## 10. Codegen And Lowerers

Code generation is Backend-IR-only. Lock 5 forbids source-emitting per backend
and forbids emitters walking grammar directly (`restart/locks/14-LOCKS.md:42`).
PASS-2 defines the `BackendLowerer` contract and final lowerer ownership
(`restart/audit/pass-2-codegen/PASS-2.md:80-96`).

Lowerers:

| Lowerer | V1 contract |
|---|---|
| Rust | Primary production lowerer. Emits runtime template, tape/direct builder, host chain calls, visitors, value projections. |
| WASM | V1 through wasm32 Rust binding and exported API surface. No independent hand emitter. |
| SIMD | Pattern lowerer fed by recognizer facts and `SimdScan` BIR. |
| VM | Executable interpreter for debug, replay, and golden equality. |

Generated source is committed. Lock 6 rejects a proc-macro facade and requires
`xtask`-style committed source generation (`restart/locks/14-LOCKS.md:44`).

## 11. Performance Targets

SOTA gates are explicit restart requirements. README names the target family:
twitter <= 380 us, canada <= 2.8 ms, citm <= 750 us, CSS bootstrap <= 3 ms,
animate <= 1.6 ms, and simdjson on-demand 7 GB/s-class x86 targets
(`restart/README.md:322-340`). Lock 8 lists competitor anchors: simdjson OD
7 GB/s, sonic-rs M1 twitter 436 us, and lightning-css Bootstrap 4.16 ms
(`restart/locks/14-LOCKS.md:48`). SOTA.md records the supporting competitor
benchmarks (`restart/corpora/SOTA.md:50-89`, `restart/corpora/SOTA.md:130-136`).

Gate owners:

| Gate | Owner |
|---|---|
| JSON twitter/citm/canada | `bbnf-bench`, `cost-model`, `runtime`, `simd-scan`. |
| CSS bootstrap/animate | `bbnf-bench`, `runtime`, `passes::recognizers`, `codegen::rust`. |
| simdjson-class throughput | `simd-scan`, `runtime::tape`, `codegen::simd`. |
| Generated LOC budget | `cost-model::loc_budget`, `codegen::verify`. |
| OpenFrame clone absence | `runtime`, `codegen`, `bbnf-bench`. |

PASS-2 sets a generated LOC budget baseline and allows a +2 percent ceiling
for emitted runtime source (`restart/audit/pass-2-codegen/PASS-2.md:293-310`).

Exact gate rows:

| Row | Competitor baseline | Restart target | Required metadata |
|---|---|---|---|
| `json/twitter` | sonic-rs 436us, simd-json 424us on M1 Pro. | <= 380us on M1 Pro. | CPU model, OS, compiler flags, input hash, parse mode, competitor version, bbnf commit, warmup, sample policy. |
| `json/citm` | sonic-rs 854us, simd-json 831us on M1 Pro. | <= 750us on M1 Pro. | CPU model, OS, compiler flags, input hash, selector mode, competitor version, bbnf commit, warmup, sample policy. |
| `json/canada` | sonic-rs 3.144ms on M1 Pro. | <= 2.8ms on M1 Pro. | CPU model, OS, compiler flags, input hash, array scan profile, competitor version, bbnf commit, warmup, sample policy. |
| `css/bootstrap` | lightning-css 4.16ms on M1 Pro. | <= 3.0ms on M1 Pro. | CSS fixture hash, layout mode, visitor mode, competitor version, bbnf commit, warmup, sample policy. |
| `css/animate` | lightning-css 1.97ms on M1 Pro. | <= 1.6ms on M1 Pro. | CSS fixture hash, layout mode, visitor mode, competitor version, bbnf commit, warmup, sample policy. |
| `simd/structural_scan` | simdjson On-Demand ~7 GB/s on x86 AVX2; ~5 GB/s on M-series NEON. | >= 5 GB/s on M-series, >= 7 GB/s on x86 AVX2; scalar parity hash matches. | ISA, CPU flags, kernel, scalar parity hash, competitor version, bbnf commit, warmup, sample policy. |

Generated LOC budget rows:

| Scope | Budget |
|---|---|
| Nine seed grammars total | PASS-2 baseline plus 2 percent. |
| Per-grammar generated runtime | PASS-2 table maximum for that grammar. |
| New yaml grammar | Reported separately until admitted as a seed grammar. |
| WASM/SIMD target-specific output | Attributed by target and justified in SOTA report. |

## 12. Future Grammar Onboarding Test

The future grammar test proves Lock 14. It is the greenfield version of “add a
grammar without touching Rust.”

Test grammar: `yaml.bbnf`.

Allowed changes:

```text
grammars/yaml.bbnf
Cargo.toml [workspace.metadata.bbnf.grammars.yaml]
```

Forbidden changes:

```text
crates/*/src/**/*.rs
crates/*/Cargo.toml package declarations
manual parser registry tables
manual path registry tables
manual host shim files
per-grammar declaration crates, unless the rare escape-valve process is invoked
```

Required commands:

```sh
cargo xtask bbnf check yaml
cargo xtask bbnf build yaml
cargo test -p bbnf --test future_grammar_yaml
cargo test -p bbnf-language-server yaml_metadata
git diff -- crates ':!crates/runtime/src/grammars/yaml'
```

The last command must show no handwritten crate source changes other than
generated runtime output under `runtime/src/grammars/yaml`.

### 12.1 Per-Grammar Authority Table

Lock 14 mandates per-X tables for "all nine seed grammars" claims. The
authoritative 10-row × 9-column matrix below covers the nine extant grammars
plus the `yaml` onboarding probe. It is fed by PASS-2 §3 runtime emission table
(`restart/audit/pass-2-codegen/PASS-2.md:461-475`), PASS-2 §6 LOC budget table
(`restart/audit/pass-2-codegen/PASS-2.md:380-392`), and PASS-3 §6a feeder table
(`restart/audit/pass-3-runtime/PASS-3.md:333-344`); architecture is the
authoritative consumer, the PASS surfaces remain the producer-side reference.
Every "all extant grammars" or "all nine seed grammars" claim elsewhere in this
document resolves against this table.

| Grammar | Typed root | `ValueRef` borrow shape | Runtime files emitted | Visitor + `VisitTypes` | Path schema | Fixture manifest | Host route | Generated LOC (current → max) | Declaration-crate status |
|---|---|---|---|---|---|---|---|---:|---|
| `bbnf` | `Bbnf` | `BbnfRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs`, layout, error, `PrattSpine` LUT | `BbnfVisitor`, `BbnfVisitTypes` | `bbnf.path-schema.toml` | `fixtures/bbnf/manifest.toml` | self-host primitives plus regen utilities; metadata + `@host fn` blocks in `bbnf.bbnf` | 21,503 → 21,933 | none (default; §5.6 fence empty) |
| `bnf` | `Bnf` | `BnfRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, layout, error | `BnfVisitor`, `BnfVisitTypes` | `bnf.path-schema.toml` | `fixtures/bnf/manifest.toml` | none (pure recogniser; metadata-only host stanza) | 3,290 → 3,356 | none |
| `csv` | `Csv` | `CsvRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, layout, error, `SimdScan` for delimiter alphabet | `CsvVisitor`, `CsvVisitTypes` | `csv.path-schema.toml` | `fixtures/csv/manifest.toml` | metadata + escape host fns from `host::primitives` | 1,693 → 1,727 | none |
| `css_l4` | `CssL4` | `CssL4Root` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs`, `layout.rs`, error, `SimdScan` for structural alphabet | `CssL4Visitor`, `CssL4VisitTypes` | `css_l4.path-schema.toml` | `fixtures/css/manifest.toml` | colour-function host primitives plus length conversion via `host::primitives`; metadata + `@host fn` blocks | 107,138 → 109,281 | none |
| `css_pretty` | `CssPretty` | `CssPrettyRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, `layout.rs`, error | `CssPrettyVisitor`, `CssPrettyVisitTypes` | `css_pretty.path-schema.toml` | shares `fixtures/css/` corpus | metadata + format host fns from `host::primitives` | 9,021 → 9,201 | none |
| `ebnf` | `Ebnf` | `EbnfRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, layout, error | `EbnfVisitor`, `EbnfVisitTypes` | `ebnf.path-schema.toml` | `fixtures/ebnf/manifest.toml` | none (metadata-only host stanza) | 7,646 → 7,799 | none |
| `google_sheets` | `GoogleSheets` | `GoogleSheetsRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs`, layout, error, `PrattSpine` for operator precedence | `GoogleSheetsVisitor`, `GoogleSheetsVisitTypes` | `google_sheets.path-schema.toml` | `fixtures/sheets/manifest.toml` | range/date/array-literal host primitives plus formula host chains | 14,088 → 14,370 | none |
| `json` | `Json` | `JsonRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, layout, error, `SimdScan` for structural alphabet (twitter/citm/canada hot path) | `JsonVisitor`, `JsonVisitTypes` | `json.path-schema.toml` | `fixtures/json/manifest.toml` | metadata + numeric/string host fns from `host::primitives` | 3,500 → 3,570 | none |
| `math` | `Math` | `MathRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, layout, error, `PrattSpine` for operator precedence | `MathVisitor`, `MathVisitTypes` | `math.path-schema.toml` | `fixtures/math/manifest.toml` | metadata + numeric host fns from `host::primitives` (Pratt-eligible operator chain only) | 871 → 888 | none |
| `yaml` (onboarding probe) | `Yaml` | `YamlRoot` over `&'i Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs` (if metadata declares host route), layout, error; Pratt/SIMD auto-detected from grammar shape | `YamlVisitor`, `YamlVisitTypes` | `yaml.path-schema.toml` | parity-phase `fixtures/yaml/manifest.toml` (post-onboarding gate, never an onboarding surface) | 0 → ≤ 4,000 (provisional; SYNTHESIS Wave-2 owner) | none (Lock 14 onboarding admits exactly two surfaces: `yaml.bbnf` plus `[workspace.metadata.bbnf.grammars.yaml]`; declaration crate is forbidden at onboarding) |

Column semantics:

| Column | Definition |
|---|---|
| Typed root | The generated direct-to-struct type returned by `parse(&'i str)` per PASS-3 §2. |
| `ValueRef` borrow shape | The untyped tape-cursor view over `&'i Tape<'i>` that backs `pointer!`, `select!`, visitors, and the debugger per Lock 1 and PASS-3 §4. |
| Runtime files emitted | Template-emitted files under `runtime/src/grammars/<name>/`; every cell is generated or data-only, hand-written runtime files are forbidden by Lock 14. |
| Visitor + `VisitTypes` | The generated `Visitor` trait and its bitflag-pruned visit-type set per PASS-3 §3. |
| Path schema | The generated path-schema sidecar consumed by `pointer!`/`select!` typing per PASS-3 §3. |
| Fixture manifest | The corpus manifest under `crates/test-fixtures/corpus/`; `yaml` carries a parity-phase manifest only. |
| Host route | The host-function decomposition source: `@host fn` blocks in the grammar, generic primitives in `host::primitives`, or workspace-metadata directives. Declaration crates are not part of the default host route. |
| Generated LOC | PASS-2 §6 baselines and +2% ceiling per grammar; LOC excludes `parser.rs` macros and `host.rs` shells per Lock 13's generated-file exemption. |
| Declaration-crate status | `none` (default; §5.6 fence empty) for every grammar in the seed set. Any future entry must populate the eight-field §5.6 review form. |

The "Until Wave 3 lands the full matrix" note that previously occupied this
section retires; the matrix above is the matrix.

## 13. File And Directory Discipline

Lock 13 applies to every handwritten crate source tree. Generated files are
budgeted separately, but they still need stable names and equality checks.
Current corpora show why: prior census found 23 handwritten files over 500 LOC
outside generated code (`restart/corpora/CENSUS.md:321-353`), and the module
corpus calls out split work across `core`, `ir`, and `csp-solver`
(`restart/corpora/MODULES.md:1295-1303`).

Rules:

| Rule | Gate |
|---|---|
| 4-10 children per source directory unless a short rationale is in the tranche doc. | `cargo xtask lint-tree`. |
| No handwritten Rust file over 500 LOC. | `cargo xtask lint-loc`. |
| Generated files carry headers and budget tags. | `codegen::verify`. |
| No grammar names in generic crate source. | `cargo xtask lint-grammar-generalization`. |
| No old strategy registries. | `cargo xtask lint-no-hardcoded-grammars`. |

Lock 13 exception ledger:

| Exception | Allowed? | Rationale |
|---|---|---|
| Generated grammar/runtime files over 500 LOC. | Yes, budgeted separately. | Lock 6 requires committed generation; budget gates replace handwritten LOC cap. |
| Generated data tables. | Yes, with headers and source metadata. | Scanner, Pratt, and diagnostic tables are data products. |
| Handwritten parser/lowerer/runtime files over 500 LOC. | No. | Split by concern before landing implementation. |
| More than 10 children in source directory. | Only with tranche-local rationale and lint allowlist. | Temporary migration shape must not become the default. |

## 14. Documentation And Voice

Implementation docs follow the precepts:

| Precept | Architecture effect |
|---|---|
| Economical and concrete writing. | No marketing prose, no filler, no future-only placeholders (`docs/precepts/instructions/STYLE.md:1-55`). |
| Same-wave consumer gates. | Every producer in a tranche has at least one consumer before close (`docs/precepts/instructions/LESSONS-LEARNED.md:1-34`). |
| Wave boundaries by dependency, not topic. | Tranches pair substrate and consumer work rather than isolated theoretical cleanup. |
| One writer per side effect. | Generated runtime, metadata schema, and public docs each have one owning wave (`docs/precepts/instructions/ORCHESTRATION.md:30-46`). |
| Path:line citations. | Concrete claims cite sources, as README requires (`restart/README.md:452`). |

## 15. Architecture Close

The restart architecture is:

1. A 24-crate, acyclic workspace with public facade crates and grammar-neutral
   internal crates.
2. A two-surface grammar onboarding contract: `.bbnf` plus workspace metadata.
3. Two IRs plus side tables, with Backend IR as the only lowerer input.
4. Tape unioned with direct-to-struct; no ParseStream rename and no parallel
   substrate.
5. BBNF extensions limited to the settled set: lookbehind, `@host fn`,
   chaining, generics, `@error`, and `@layout`.
6. Rewrite-mode out; Unicode class algebra below BBNF in `parse-that/regex`.
7. Generic host and runtime systems; no per-grammar declaration crates by
   default.
8. SOTA, generated LOC, tree-shape, and future-grammar tests as hard gates.

This is the architecture consumed by `restart/MIGRATION.md` and
`restart/MASTER-PLAN.md`.
