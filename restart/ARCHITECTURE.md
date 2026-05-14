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
| Unicode class algebra | Deferred to `parse-that-regex`; not a grammar-level BBNF surface. | ffuzzy and stale extension lists that treat class algebra as a grammar feature. | README routes Unicode set work below BBNF (`restart/README.md:150-158`); PASS-1 says no BBNF-level Unicode class algebra (`restart/audit/pass-1-substrate/PASS-1.md:84-121`). |
| Lookbehind | In. | Old rejection of lookbehind. | README accepts lookbehind as a first-class extension (`restart/README.md:121-137`); PASS-1 includes `Lookbehind` in Grammar IR (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| Per-grammar declaration crates | Not default. A rare escape valve must be explicit and fenced. | Old plans that created declaration crates per grammar. | README says onboarding is `.bbnf` plus workspace metadata, with no Rust crate or match arms (`restart/README.md:11-25`); Lock 14 allows only rare optional declaration crates (`restart/locks/14-LOCKS.md:60`). |
| Generic grammar code | Mandatory. | Current hardcoded parser registries and grammar-name Rust modules. | CENSUS identifies grammar-name leaks in metadata, registries, path mirrors, and generated shims (`restart/corpora/CENSUS.md:103-122`). Lock 14 rejects generic crates with grammar switches, types, modules, or features (`restart/locks/14-LOCKS.md:60`). |
| IR boundary | Two IRs plus side tables. | Old backend walkers that emit from Grammar IR directly. | README requires Grammar IR and Backend IR (`restart/README.md:104-118`); Lock 5 forbids emitter walking grammar directly (`restart/locks/14-LOCKS.md:42`). |
| Optimization graph | CSP, egraph, miners, and cost model compose by output piping. | A fused global hypergraph. | README and Lock 4 require bridged sister crates rather than a fused graph (`restart/README.md:219-228`, `restart/locks/14-LOCKS.md:40`). |

egglog-style Datalog/equality-saturation fusion is a known SOTA alternative,
not an omitted design. V1 keeps bridge tables because CSP, egraph, miners, and
cost model have separate crate APIs, diagnostic ownership, and stabilization
gates; fusion remains a post-V1 research comparison rather than the governing
substrate.

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
| `codegen` | Internal | Lowerers and emitters for Rust V1, deferred V2 backend proofs, SIMD patterns, template rendering, regen equality. | Replaces current core backend walker; PASS-2 makes BIR-only lowerers mandatory (`restart/audit/pass-2-codegen/PASS-2.md:5-8`). |
| `runtime` | Internal/public support | Tape, direct-to-struct builder support, generated grammar modules, visitors, document views. | Replaces hand-written per-grammar runtime dirs and OpenFrame-heavy flow. |
| `host` | Internal | Generic host primitives, `@host fn` registry, host chain typing/runtime dispatch. | Replaces grammar-specific shims such as `css_types.rs` and hardcoded host tables. |
| `cost-model` | Internal | Cost facts, SOTA profiles, extraction scoring, generated LOC budgets. | PASS-1 keeps a real cost model with SOTA gates (`restart/audit/pass-1-substrate/PASS-1.md:46-61`). |
| `path` | Public | Rust macro/front-facing path DSL: `path!`, `select!`, visitor selectors. | Renames current `bbnf-path`; README requires `path`, `path-core`, `path-ts` (`restart/README.md:47-53`). |
| `path-core` | Internal/shared | Path parser, typed segments, evaluator core, diagnostics shared by Rust and TS. | Extract from `bbnf-path` and `bbnf-path-ts`. |
| `path-ts` | V2 deferred | TypeScript path package generated over `path-core` semantics after `TsBackend: Backend` lands. | Legacy `bbnf-path-ts` is deferred; Lock 7 names the split and Lock 5 keeps TS post-V1 (`restart/locks/14-LOCKS.md:42`, `restart/locks/14-LOCKS.md:46`). |
| `egraph` | Internal/sister | Equality saturation core and bridge APIs. | Keep and harden current `crates/egraph` (`restart/corpora/MODULES.md:136-162`). |
| `egraph-derive` | Internal/sister | Derive support for egraph term declarations. | Keep with `egraph` (`restart/corpora/MODULES.md:136-162`). |
| `csp-solver` | Internal/sister | Generic CSP solver used by type inference, layout choices, and extraction facts. | Keep and harden current generic solver (`restart/corpora/MODULES.md:73-132`). |
| `parse-that` | Internal/sister | Regex and parser substrate utilities, including Unicode-class implementation below BBNF. | New extraction target for regex support. |
| `bbnf-simd` | Internal/sister | SIMD scanner kernels for AVX2, AVX512, NEON, WASM SIMD, and scalar fallback. | Keep current clean crate (`restart/corpora/MODULES.md:47-69`). |
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
  -> bbnf-simd
  -> error

codegen
  -> ir
  -> host
  -> runtime
  -> cost-model
  -> bbnf-simd
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

path-ts (V2)
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
| V2 `path-ts` consumes shared semantics, not Rust macro internals. | `path-core` is the single semantics owner, matching the path split in Lock 7 (`restart/locks/14-LOCKS.md:46`). |
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

This API family is the V1 Rust-line surface. V2 `WasmBackend` and `TsBackend`
may expose host-idiomatic allocation and GC forms without changing Backend IR:
WASM may shape entrypoints around linear-memory handles, while TS may return
GC-managed document objects. The shared contract remains grammar source +
workspace metadata + Backend IR; host entrypoint spelling is a backend API
choice, not a grammar-surface change.

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
path!(Bbnf => "/rules/0/name")
select!(Bbnf => "rule[name='expr'] > alt:nth(0)")
```

The grammar prefix is the canonical public macro shape. Legacy unqualified path
examples are migration archaeology; no generic path macro alias survives as a
public surface.

Public shared concepts:

| Export | Owner | Purpose |
|---|---|---|
| `PathExpr` | `path-core` | Parsed typed path expression. |
| `Segment` | `path-core` | Pointer/select segment. |
| `CompiledPath` | `path-core` | Validated evaluator plan. |
| `PathDiagnostic` | `path-core` | Shared diagnostics. |
| `path!`, `select!` | `path` | Rust compile-time syntax. |
| `compilePath`, `select` | `path-ts` (V2) | TypeScript API over the same semantics after `TsBackend: Backend` lands. |

README keeps `path!`, `select!`, JSONPath-style selection, and read-write
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
| `cost-model` | `CostDecision` facts, objective profiles, Pareto/frontier reports, SOTA gate schema, generated LOC budget API. | Raw scorer tuning internals, solver adapter scratch state, and platform cache. |
| `path` | `path!`, `select!`, typed path wrappers, visitor selector helpers. | Macro parser scratch AST. |
| `path-core` | Path AST, parser, typed evaluator, diagnostics. | Rust macro glue and TypeScript emitter details. |
| `path-ts` (V2) | TypeScript package generation schema and exported TS API definitions after `TsBackend: Backend` lands. | Rust path macro internals. |
| `egraph` | Generic egraph arena, rewrite, extraction, explanation APIs. | BBNF bridge terms. |
| `egraph-derive` | Derive macro entrypoints for generic egraph terms. | Expansion scratch state. |
| `csp-solver` | Generic variables, domains, constraints, solver, explanations. | BBNF-specific fact conversion. |
| `parse-that` | Regex HIR/program APIs, NFA/DFA/VM execution plans, prefilter contracts, literal helpers, Unicode data wrappers. | BBNF grammar parser state. |
| `bbnf-simd` | Scanner traits, scalar/NEON/AVX dispatch handles, feature detection. | Intrinsic-specific loop bodies not needed by callers. |
| `test-fixtures` | Fixture manifest, corpus loader, parity matrix schema. | Local fixture generation scratch files. |

### 3.6 API Leakage Rules

| Leakage class | Forbidden example | Allowed replacement |
|---|---|---|
| Grammar parser type | `JsonParser`, `CssL4Parser` in generic code. | Metadata-derived `GrammarName` and generated manifest lookup. |
| Runtime grammar module | `runtime::json::Document` in public facade code. | `GrammarHandle<G>` and `DocumentView`. |
| Host shim file | `css_types.rs` as a generic dependency. | `@host fn` plus `host::registry`. |
| Strategy registry | `PRODUCTION_MANIFEST_TABLE` or `bbnf-strategy`. | Workspace metadata and recognizer facts. |
| Path mirror | Hardcoded TS path registry per grammar. | `path-core` schema emitted from shape facts. |
| Backend syntax | Lowerer peeking at `GrammarIr::Alt`. | `BackendIr::Alt` (`mode: Dispatch | Speculative`). |

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
  layout/
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
  frontier/
  solve/
  evidence/
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
  regex/hir/
  regex/nfa/
  regex/dfa/
  regex/vm/
  regex/prefilter/
  unicode/
  literal/

bbnf-simd/src/
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
| `path-ts` (V2) | Emitter templates and package staging directories. | TS API and schema are stable after `TsBackend: Backend` lands. |
| `egraph` | Arena compaction state and extraction work queues. | Generic API remains clean. |
| `egraph-derive` | Token expansion scratch modules. | Macro output is the visible contract. |
| `csp-solver` | Propagation queue internals and search heuristics. | Solver inputs, outputs, and explanations are stable. |
| `parse-that` | Unicode table generation scratch data, HIR simplification caches, NFA/DFA builder state, lazy-DFA cache policy, and SIMD prefilter plans. | Regex program APIs and verifier contracts are stable. |
| `bbnf-simd` | Intrinsic loop bodies and dispatch probe cache. | Scanner trait and dispatch handle are stable. |
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
  "crates/egraph",
  "crates/egraph-derive",
  "crates/csp-solver",
  "crates/parse-that",
  "crates/bbnf-simd",
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
| `wasm = true` is invalid in V1 metadata and routes to the V2 `WasmBackend: Backend` receiver. | V1 ships `RustBackend` only; WASM lower-and-bench waits for the V2 backend impl (`restart/locks/14-LOCKS.md:42`, `restart/ARCHITECTURE.md:1095-1097`). |
| Adding a grammar must not touch Rust source. | The future grammar test in this document makes that a hard gate. |

Metadata validation errors are normal diagnostics, not panics. They flow
through `error` so CLI and LSP report the same code for the same bad metadata.

The metadata schema content above is host-agnostic; the V1 carrier is
Cargo.toml's `[workspace.metadata.bbnf]` block because Rust-line onboarding
is the V1 surface. The cross-host metadata-carrier work (a language-neutral
sidecar so future TS/WASM consumers do not re-invent the carrier) routes
to MASTER-PLAN §24 carry as tranche-body work, not a V2 amendment; the
schema fields above lock in at V1 regardless of which carrier file
delivers them.

### 5.6 Declaration-Crate Fence

Per HARDENING-CONSOLIDATED §4.15, every declaration-crate escape valve
carries the following eight-field review form. The fence is the
architectural gate through which the rare `allow_declaration_crate = true`
exception passes; the metadata validator rejects partial fences. The
exception table is empty for the nine extant grammars (`bbnf`, `bnf`,
`csv`, `css_l4`, `css_pretty`, `ebnf`, `google_sheets`, `json`, `math`)
and stays empty unless a metadata + `@host fn` demonstration of
insufficiency lands first.

| Field | Required content |
|---|---|
| Reason | The specific boundary requiring a per-grammar declaration crate (e.g., a trait-impl bridge that `host::primitives` cannot express, or a backend-specific FFI that `@host fn` bodies cannot express). State the boundary in mechanism terms, not in convenience terms. |
| Owner | The named human or team accountable for the declaration crate's source, regen discipline, and deletion path. |
| Why metadata fails | Specific demonstration that `[workspace.metadata.bbnf.grammars.<name>]` cannot describe the boundary. Cite metadata schema lines that would have to grow and explain why the growth contaminates the generic schema. |
| Why `@host fn` fails | Specific demonstration that a block-bodied `@host fn` decomposing into `host::primitives` cannot express the boundary. Cite the generic primitive set considered and the gap that survives. |
| Declaration location | Explicit path, normally `runtime/src/grammars/<name>/decl/` (sub-module of the per-grammar generated runtime). The declaration crate may not live at a workspace-level path that pollutes generic crate graphs. |
| No generic import | Proof that no generic crate (`bbnf`, `pipeline`, `passes`, `ir`, `codegen`, `runtime`, `host`, `path`, `path-core`, `egraph`, `csp-solver`, `parse-that`, `bbnf-simd`) imports the per-grammar declaration crate. The proof is a `rg` command in the review record. |
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
  -> global CSP extraction solve
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
The type/layout CSP subroutine runs inside `passes::layout`; the later global
CSP solve consumes public solved or narrowed facts for extraction-time legality
and optimization choices, not layout-internal `TypeFacts`.

The pipeline invariants are:

| Invariant | Owner |
|---|---|
| Source and metadata validation happen before Grammar IR construction. | `grammar`, `pipeline` |
| Type inference annotates Grammar IR; it does not mutate grammar syntax. | `passes::layout` (HM + bidirectional + CSP run as a subroutine inside layout lowering per Lock 2). |
| Shape mining produces side tables. | `passes::shapes` |
| Recognizer mining produces side tables and BIR hints. | `passes::recognizers` |
| Egraph and CSP exchange monotone facts through bridge tables; CSP search state remains inside `csp-solver`. | `passes::bridge` |
| Cost extraction selects from legal alternatives by `CostDecision` evidence; it does not introduce new grammar semantics. | `cost-model`, `passes::extract` |
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
| `Regex` | Regex expression owned by `parse-that-regex`. |
| `Ref` | Rule reference with type arguments. |
| `Predicate` | Lookahead and grammar predicate forms. |
| `Lookbehind` | Positive/negative bounded lookbehind. |
| `Call` (`kind: Map | Host`) | Semantic mapping expression or `@host fn` call/chain segment; the discriminator preserves syntactic origin. |
| `LayoutDirective` | `@layout` policy. |
| `ErrorDirective` | `@error` recovery vocabulary hook. |
| `Annotation` | Explicit type, cost, profile, or docs annotation. |

Grammar IR payload and lowering matrix:

| Variant | Payload shape | Lower-time invariant | Main BIR consumer |
|---|---|---|---|
| `Rule` | Name, generics, signature, body ID, annotations. | Rule is typechecked and metadata-resolved. | `Entry`, `CallRule`. |
| `Seq` | Ordered expression IDs. | Empty sequence is normalized before BIR. | `Seq`. |
| `Alt` | Alternative IDs plus dispatch hints. | Byte-disjoint alts are marked before extraction. | `Alt` (`mode: Dispatch | Speculative`). |
| `Repeat` | Body ID, min/max, separator, greediness. | Nullable body is rejected or guarded. | `RepeatLoop`. |
| `Optional` | Body ID. | Lowered without changing capture shape. | `OptionalBranch`. |
| `Literal` | Byte string, case policy, span. | Encoding is known and stable. | `ByteLiteral`, `SimdScan`. |
| `Regex` | Regex program handle, flags, span. | Regex parsed by `parse-that-regex`. | `RegexProgram`, `SimdScan`. |
| `Ref` | Target rule ID, type args, call annotations. | Target resolves after generics instantiate. | `CallRule`. |
| `Predicate` | Kind and expression ID. | Predicate has no consuming side effects. | `Alt` (`Dispatch`) hints or guard BIR. |
| `Lookbehind` | Kind, bounded body ID, width facts. | Width is proven bounded. | `Alt` (`Speculative`), guard BIR. |
| `Call` (`kind: Map | Host`) | Source expression or function ID, args, chain segment IDs, expected type. | Output type agrees with shape facts; host signatures and chain types compose. | `ValueProject`, `DirectBuild`, `CallHost` (chain lowers as `Seq` of `CallHost`). |
| `LayoutDirective` | Policy ID, body/span. | Policy is scoped. | `LayoutScope` (`kind: Push | Pop`). |
| `ErrorDirective` | Recovery code, sync rules, body/span. | Recovery code is registered. | `ErrorRecover`. |
| `Annotation` | Key/value typed metadata. | Annotation key is known or fenced. | Side tables and diagnostics. |

Grammar IR invariants:

| Invariant | Gate |
|---|---|
| No backend-specific node names. | `ir::validate::grammar_ir_has_no_backend_nodes`. |
| No grammar-level rewrite-mode node. | Parser rejects rewrite syntax; IR enum has no variant. |
| Regex Unicode classes are opaque regex data. | `Regex` stores parsed regex program handles, not BBNF class algebra. |
| Lookbehind is bounded before Backend IR. | Type/inference pass proves or rejects unbounded lookbehind. |
| Host calls resolve through generic primitives or declared `@host fn` signatures. | `host::types` and `passes::layout` (host signatures unify inside the layout-lowering subroutine per Lock 2). |

### 7.2 Backend IR

Backend IR is executable and lowerer-facing. PASS-2 makes it the single backend
contract; the post-Phase-8.4 fold consolidates three semantically-redundant
pairs from the original 22-variant alphabet (`restart/audit/pass-2-codegen/PASS-2.md:52-76`)
into the 20-variant shape below — `LayoutScope { kind: Push | Pop }`,
`Alt { mode: Dispatch | Speculative }`, and `CallHost` (multi-function host
chains express as `Seq` of `CallHost`, no separate `HostChain` variant). The
discriminator field carries every distinction the prior pair carried, with no
loss of lowering distinct-ness.

| Variant | Purpose |
|---|---|
| `Entry` | Backend function entrypoint. |
| `Seq` | Lowered ordered execution. |
| `Alt` | Predictive or speculative alternative dispatch (`mode: Dispatch | Speculative`); `Speculative` carries checkpoint/rollback. |
| `RepeatLoop` | Lowered repetition. |
| `OptionalBranch` | Lowered optional branch. |
| `ByteLiteral` | Byte/string literal check. |
| `RegexProgram` | Lowered regex program handle. |
| `SimdScan` | SIMD scanner operation. |
| `PrattSpine` | Pratt parser spine. |
| `CallRule` | Rule call. |
| `CallHost` | Host call; multi-function chains express as `Seq` of `CallHost`. |
| `LayoutScope` | Enter or exit layout policy (`kind: Push | Pop`). |
| `ErrorRecover` | Recovery site. |
| `SpanMark` | Span/tape marker. |
| `TapeEmit` | Emit tape token/event. |
| `DirectBuild` | Build typed direct view. |
| `ValueProject` | Project value shape. |
| `PathEval` | Path-evaluator hook. |
| `DebugMark` | VM/debug trace marker. |
| `Return` | Backend return from entry/rule. |

The 20-variant shape preserves the `Return` row PASS-2 added on top of the
original PASS-1 22-variant table; the three pair collapses (Layout, Alt,
host-call) net the alphabet to 19 semantic variants plus `Return`. PASS-1
retains the alphabet ownership; PASS-2 ratifies the variant payload tables;
the snapshot tests at `ir::backend_ir` consume the post-fold shape.

**2026-05-12 lowering amendment** (no IR addition): `Alt { mode: Dispatch }`
lowers through a materialization plan per `LayoutFacts.backend_shape[rule_id]`
(see §7.3 for the spec; §7.4 for the Rust implementation status — Wave 1 of
SK-V5 lands the enum, the field, and the per-shape lowerer). The access
pattern may be byte-position (`EagerTape`), typed-event cursor over retained
offsets (`OffsetTape`), typed-event cursor over retained event cells
(`EventTape`), direct sink with no retained document (`SinkOnly`), or
collapsed mask-state walk (`CollapsedStage`). The variant payload is
unchanged; only the lowerer emits differently. `backend_shape` is
cost-model-derived from Grammar IR facts per Lock 10 auto-detection, never a
user-visible directive.

Backend IR payload and lowerer matrix:

| Variant | Payload shape | Rust lowerer | VM behavior | WASM/SIMD note |
|---|---|---|---|---|
| `Entry` | Symbol, input mode, output mode, body block. | Emits public/internal function. | Starts frame. | Exported only through WASM facade. |
| `Seq` | Ordered BIR node IDs. | Emits straight-line control flow. | Runs children in order. | No special handling. |
| `Alt` | `mode: Dispatch | Speculative`; discriminator facts (Dispatch) or checkpoint policy (Speculative); alt targets. | Emits match/table dispatch (Dispatch) or bounded checkpoint/rollback (Speculative). | Chooses deterministic alt (Dispatch) or saves and restores frame (Speculative). | SIMD may feed Dispatch discriminator; Speculative must not clone parallel-substrate stacks. |
| `RepeatLoop` | Body, min/max, exit guard. | Emits loop with progress guard. | Iterates with progress check. | SIMD may accelerate body prefix. |
| `OptionalBranch` | Body and empty branch shape. | Emits branch. | Runs or skips. | No special handling. |
| `ByteLiteral` | Bytes, case policy, span. | Emits byte compare. | Consumes on match. | SIMD may widen compare. |
| `RegexProgram` | Regex program handle and execution plan. | Calls regex verifier. | Executes regex VM, lazy DFA, or full DFA plan. | Unicode stays below BBNF; `parse-that-regex` carries internal cross-engine parity (VM ↔ lazy DFA ↔ full DFA) per V1-FOLD-CANDIDATES Tier 3 #23, and no external regex oracle is consumed at V1. |
| `SimdScan` | `SimdScanMode::{Exact, Prefilter}`, needle/class, fallback, verifier route. | Emits dispatch to `bbnf-simd`. | Exact mode must match scalar offsets; prefilter mode emits candidates only. | Prefilter acceptance routes to `RegexProgram` or scalar verifier before tape/event emission. |
| `PrattSpine` | Operators, precedence, associativity, atom rule. | Emits Pratt loop. | Executes Pratt interpreter. | Auto-detected only. |
| `CallRule` | Callee ID, args, result slot. | Emits function call. | Pushes rule frame. | No special handling. |
| `CallHost` | Host function ID, args, result slot. | Emits registry dispatch. | Calls host shim. | WASM requires ABI-safe wrapper; multi-function chains lower as `Seq` of `CallHost`. |
| `LayoutScope` | `kind: Push | Pop`; layout policy ID. | Emits scoped policy push (Push) or pop (Pop). | Pushes or pops layout state. | No special handling. |
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
| `Alt` (Dispatch) | `"true" | "false" | "null"` | Byte-disjoint alternatives. |
| `Alt` (Speculative) | `ident | keyword` when prefix overlaps. | Requires checkpoint. |
| `RepeatLoop` | `digit+` | Progress guard required. |
| `OptionalBranch` | `sign? number` | Empty branch keeps shape. |
| `ByteLiteral` | `"{"` | Byte literal. |
| `RegexProgram` | `/[0-9]+/` | Regex program is the semantic verifier and is opaque to BBNF. |
| `SimdScan` | Long literal set, exact structural alphabet, or regex prefilter. | Exact scans require scalar parity; prefilters require verifier acceptance before tape emission. |
| `PrattSpine` | Expression grammar with precedence pattern. | Auto-detected, no directive. |
| `CallRule` | `value` inside another rule. | Rule reference. |
| `CallHost` | `@trim(text)` (single call); `@decode(x).normalize().intern()` (chain lowers as `Seq` of `CallHost`). | Chain semantics: each step's output unifies with the next step's input. |
| `LayoutScope` (Push) | `@layout indent { ... }` (entry). | Scoped policy push. |
| `LayoutScope` (Pop) | End of layout body. | Compiler-generated pair. |
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

`SimdScan` has two runtime products. A transient **mask stream** feeds typed
events during parse; an optional retained tape stores offsets, event cells, or
direct payload facts after parse. Grammar-specific parse indexes, such as JSON's
escape/control flags, may add columns only when the full parse row recovers the
extra cost. The structural-only gate must not accidentally pay for parser-only
columns. The mask stream itself is never a second substrate; if retained, it is
the tape projection.

### 7.3 Side Tables

The optimized IR is not a third core IR. README says optimized IR is side-table
data, not another central tree (`restart/README.md:104-118`). Per Lock 2 and
HARDENING-CONSOLIDATED §3 conflict #4, the layout-lowering pass is the public
surface; HM/CSP type checking is its internal subroutine. `TypeFacts` is an
internal scratch artefact of `passes::layout` (used by HM unification and CSP
constrained choice) and never appears as a public side table; downstream passes
read `LayoutFacts` instead. `TypeObligationLog` is retained only as internal
diagnostic provenance until `LayoutFacts` and `RecoveryFacts` are emitted. Side
tables and internal fact logs are:

| Table | Producer | Consumer | Visibility |
|---|---|---|---|
| `LayoutFacts` | `passes::layout` (folds HM + bidirectional + CSP into layout decisions); `passes::recognizers` extends with `backend_shape: HashMap<RuleId, BackendShape>` and `hot_call_graph: HashMap<RuleId, HotPathFact>` per Lock 10 + Lock 15. | Backend IR builder (`LayoutScope`), rust template lowerer (per-rule access-pattern + force-inline emission), host registry, diagnostics. | Public. |
| `ShapeFacts` | Shape mining. | Direct builder, Value API, path typing. | Public. |
| `RecognizerFacts` | Recognizer mining. | BIR builder, SIMD/Pratt lowerers. | Public. |
| `EGraphFacts` | Egraph bridge. | Cost extraction. | Public; keys stable e-class/node facts, not chosen representatives. |
| `BridgeJustification` | `passes::bridge`, with egraph and CSP explanation refs. | Cost extraction, diagnostics, bridge tests. | Public proof reference; does not expose pass-local bridge terms from generic crates. |
| `CspSolution` | CSP solver (when called by `passes::layout` or other clients). | Cost extraction, layout, host chain typing. | Public when produced for extraction legality; internal when produced inside layout lowering. |
| `CostFacts` | Cost model. | Backend IR extraction, benchmark report. | Public; stores `CostDecision` records, objective vectors, Pareto/frontier membership, scalarization profile, selected alternative, rejected alternatives, dominated alternatives, and extraction method. |
| `RecoveryFacts` | Error pass. | `ErrorRecover`, LSP diagnostics. | Public. |
| `TypeFacts` | HM + bidirectional checker (internal to `passes::layout`). | `passes::layout` only. | Internal subroutine artefact; not exported across pass boundaries. |
| `TypeObligationLog` | HM equality, expected checking, coercion, and finite-choice stages inside `passes::layout`. | Diagnostics until layout/recovery facts are emitted. | Internal diagnostic evidence only. |

**`LayoutFacts.backend_shape` field (2026-05-12 extension per Lock 1 union clause + Lock 10 cost-model auto-detect + Lock 15 fusion discipline + Lock 16 admissibility allowlist + SOTA-BEAT SK-V3 research)**. Per Lock 1: tape and direct-to-struct are one union; a SIMD mask stream is a transient producer; if structural offsets are retained, the structural projection IS the tape (no second sidecar). The five `BackendShape` variants below are the five ways the substrate may project for a given rule — `EagerTape` / `OffsetTape` / `EventTape` retain a queryable document, `SinkOnly` does not, `CollapsedStage` fuses mask-state and emission for AVX-512-class hardware. The cost model picks per-rule; no BBNF directive carries the choice.

```rust
pub enum BackendShape {
    /// Default. Alt { Dispatch } lowers reading source[pos] (eager byte position).
    /// Selected for rules whose body or transitive uses include @error(recover),
    /// @host fn decoded-at-parse, @layout scope, or whose first-set has overlap
    /// (the latter forces Alt { Speculative }, not Alt { Dispatch }).
    EagerTape,
    /// Retained offset tape: Alt { Dispatch } reads source[offsets[cursor]] and
    /// advances a typed event cursor. Selected for rules with byte-finite
    /// disjoint first-sets and lazy scalar spans.
    OffsetTape,
    /// Retained event tape: cursor indexes compact event cells with stored
    /// payload classes or recovery/layout side facts.
    EventTape,
    /// Direct-to-struct sink: parser emits typed fields and retains no
    /// queryable document identity. Selected only when the API shape does not
    /// require path/value traversal after parse. This is the SOTA direct
    /// emission shape; a retained-tape view walk is only a correctness proof.
    SinkOnly,
    /// AVX-512-class collapsed-stage backend (asmjson-class FSM with
    /// mask-held parser state; Lock 16 admissibility under collapsed-stage
    /// dispatch). Selected when target features and grammar facts admit a
    /// strict single-pass mask/state walk. Per-grammar opt-in via CPUID/cost
    /// model, not directive.
    CollapsedStage,
}
```

Cost-model derivation algorithm at `passes::recognizers::derive_backend_shape(grammar_ir, rule_id) -> BackendShape` (no new directives; all inputs are existing Grammar IR facts):
1. If transitive uses include any `ErrorDirective` ⇒ `EagerTape`
2. Else if rule body contains `Call { kind: Host }` decoded-at-parse ⇒ `EagerTape`
3. Else if rule body contains `LayoutDirective` ⇒ `EagerTape`
4. Else if rule's `Alt` first-set has overlap (per existing `passes::recognizers` first-set computation) ⇒ `EagerTape` (lowers `Alt` as `Speculative`, not `Dispatch`)
5. Else if the public output mode is direct-only and no post-parse path/value traversal is required ⇒ `SinkOnly`
6. Else if target features admit AND rule is a hub with ≥ 4 byte-disjoint arms ⇒ `CollapsedStage`
7. Else if payload/recovery/layout side facts must be retained per cursor ⇒ `EventTape`
8. Else ⇒ `OffsetTape`

Per-shape lowering output. Each `BackendShape` value resolves to a concrete artefact triple the codegen template emits. Four of the five shapes stay inside LLVM's optimiser, lowering to a Rust recursive-descent body that may call grammar-neutral SIMD primitives via FFI shims; the fifth bifurcates to hand-written NASM that owns its own dispatch addresses. The artefact-triple table:

| Shape | Lowering output |
|---|---|
| `EagerTape` | (rust_recursive_descent_body — eager `source[pos]` reads, `Alt { Speculative }` dispatch; optional primitive FFI shim calls when scan-only sub-rules admit a Layer-1 primitive per Lock 16) |
| `OffsetTape` | (rust_recursive_descent_body — `EventCursor` over retained structural offsets, `Alt { Dispatch }` against `event.byte()`; optional primitive FFI shim calls) |
| `EventTape` | (rust_recursive_descent_body — `EventCursor` over compact event cells carrying payload/recovery/layout side facts; optional primitive FFI shim calls) |
| `SinkOnly` | (rust_recursive_descent_body — direct typed-field writes during parse, no retained queryable document, no post-parse generic view walk; optional primitive FFI shim calls) |
| `CollapsedStage` | (rust_caller_shim — `parse_value` entry point that prepares the input buffer per the Lock 16 `EOB_PAD_CLAMP` discipline and invokes the kernel; asm_kernel_file — per-grammar hand-authored file at `skinny/crates/bbnf-simd/src/x86_64/{grammar}_collapsed.asm`; data_section — codegen-emitted `.data` section co-located in the same file, carrying the 256-byte classifier LUT, the state-transition LUT, and the accept/reject decision table) |

The bifurcation is load-bearing for LLVM compatibility. Recursive-descent Rust compiles to an implicit automaton through LLVM's optimiser — the call-stack-as-parse-state lowering fuses with force-inlined hot leaves under Lock 15's `lto = "fat"` + `codegen-units = 1` + ~20 KiB hot-function ceiling, and yyjson's reference C body demonstrates the same shape stays in i-cache. Codegen-emitted *explicit* Rust automatons do not survive this lowering: LLVM cannot fold an indirect-dispatch state walk back into PC-as-state form, and the overhead asmjson eliminates via `jmp [r10 + state*8]` reappears as branch-misprediction taxa in any LLVM-emitted equivalent. The lone exception — `CollapsedStage` — therefore consumes hand-written NASM where direct control over generated-code addresses is available (asmjson's PC-as-state pattern; Lock 16's `FSM_DISPATCH_THREADED` primitive in `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`). All four other shapes stay in LLVM's territory and consume Layer-1 primitives from the same `ext/x86/bbnf.asm` vocabulary only at scan-shaped inner loops where the primitive's grammar-neutral signature (`BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `BULK_EMIT_COMPRESSED`, `EOB_PAD_CLAMP`, `FRAME_PUSH_BOUNDED`, `FRAME_POP_BOUNDED`) admits a direct FFI binding. The Rust per-shape lowerer surface now exists in the skinny prototype at `skinny/crates/codegen/src/lower/rust.rs`; its `SinkOnly` path lowers Backend IR into a grammar-neutral `SinkOnlyProgram` consumed by the JSON runtime renderer. The two-layer reusable vocabulary — Layer 0 vendored from dav1d at `skinny/crates/bbnf-simd/ext/x86/x86inc.asm` (1,978 LOC, BSD-2), Layer 1 grammar-neutral macros at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` — is the dav1d / asmjson factoring elaborated at `restart/skinny/audit/SOTA-BEAT-DESIGN.md` §5.2; Lock 1 governs the substrate union that admits all five shapes, Lock 14 governs the zero-grammar-overfitting discipline that keeps `bbnf.asm` grammar-neutral, Lock 15 governs the i-cache residency budget that bounds the recursive-descent shapes, and Lock 16 governs the admissibility allowlist that bounds the primitive vocabulary. The same-wave-consumer rule at `docs/precepts/instructions/LESSONS-LEARNED.md:17-26` constrains admission: a `CollapsedStage` lowering target lands only when a per-grammar kernel author is in flight (no substrate-without-consumer); a primitive lands in `bbnf.asm` only when at least one shape consumes it through codegen at the same wave.

### 7.4 SK-V5 Implementation Status

The §7.3 surfaces — the `BackendShape` enum, the `derive_backend_shape` algorithm, and the `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` field — are now present in the skinny prototype. `ir/src/lib.rs` owns `BackendShape`; `passes/src/lib.rs` populates `LayoutFacts.backend_shape` through `passes::recognizers::derive_backend_shape_with_diagnostics`; `codegen/src/lower/` dispatches per shape. The current close blocker is no longer symbol absence; it is measured cost selection and runtime materialization quality across retained parse and generated `SinkOnly`.

The codegen text-emission step is split. Retained parser/view scaffolding still uses the historical JSON template surface, but the direct `SinkOnly` entry is no longer a decorative pass-through. `codegen/src/lower/sink_only.rs` walks `BackendIr` into `SinkOnlyProgram`, `DirectBuild` carries a field/source roster, and `codegen/src/lib.rs` refuses direct emission if the backend lacks `DirectBuild`. The former `json_templates/sink_direct.rs` splice is removed. Generated direct string lowering now passes raw string spans plus `needs_unescape` to `JsonSink::*_source`; the default hooks preserve the old allocation behavior, and the measured no-allocation decoded visitor route is rejected until a fused decode+sink primitive exists. The BIR construction itself remains non-decorative — `extract::single_plan` walks the grammar honestly, projects `materialize_rule` per rule, and emits a `BackendIr` whose recognizers, rules, shape facts, and direct field rosters drive the direct renderer tests.

`passes::compile` at `passes/src/lib.rs:24-36` is grammar-blind in two places that the §7.3 contract requires it not to be. The shape-mining call at `passes/src/lib.rs:28` is the literal `shapes::shapes_for_json()` regardless of input grammar; the recognizer-mining call at `passes/src/lib.rs:29` is `recognizers::nominate_json(&_grammar)` whose `_grammar` parameter is unused (`passes/src/lib.rs:232-238` returns a single hardcoded `SimdScan` over the JSON structural alphabet). The downstream `materialization_for_rule` helper at `passes/src/lib.rs:423-434` matches on the literal rule-name strings `"object"`, `"array"`, `"pair"`, `"string"`, `"number"`, `"bool"`, `"null"`. Each is a `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` Lock 14 violation that `cargo xtask lint-no-hardcoded-grammars` would surface if run against `passes`; the lint is catalogued at `restart/locks/14-LOCKS.md:60` and the diagnostic row sits at §7.5 of this document.

The remaining remediation is not another directive or BIR variant. H.W4 must make `derive_backend_shape` a measured cost decision across retained and direct workloads, remove the remaining grammar-specific mining from generic passes during Lock 14 cleanup, and close the measured rows named in `skinny/RESULTS.md`: retained parse G rows, `N-direct` rows, fused decoded-string delivery, exact float/string/Unicode materialization, event-stream consumption, and full-matrix incorporation of the SK-V5 item-56 Canada structural-scan floor fix. Item 56 is a useful general lesson for V1: structural scan throughput is a pair of grammar-neutral operations — classify structural/terminator bytes from a supplied alphabet, then bulk-emit set-bit positions into the active projection — not a JSON-only special case and not a reason to reintroduce a sidecar substrate.

The pre-existing `ShapeFacts` at `ir/src/lib.rs:436-467` is not the spec's `BackendShape` selector and remains untouched by SK-V5. It is a typed-view shape catalogue — a `Vec<Shape>` whose `Shape::{Struct, Enum}` variants carry named Rust types (`JsonRoot { value: JsonValue<'i> }`, etc.) consumed by the `view.rs` direct-builder emission and by the `Value` API. The spec's `LayoutFacts.backend_shape: HashMap<RuleId, BackendShape>` is a per-rule lowering-mode selector that names a generation strategy for `Alt { mode: Dispatch }`. Same surface noun, distinct concerns; both side tables remain, side by side, after Wave 1.

### 7.5 Diagnostic Vocabulary

The diagnostic codes the codebase commits to are catalogued here. PASS-1
contributes the lookbehind-width clause (`restart/audit/pass-1-substrate/PASS-1.md:84-121`),
PASS-2 contributes the codegen and lifetime clauses
(`restart/audit/pass-2-codegen/PASS-2.md` §8), PASS-3 contributes the
runtime, host, layout, and pointer clauses
(`restart/audit/pass-3-runtime/PASS-3.md:352-366`), and Lock 14 contributes
the metadata/onboarding clauses (`restart/locks/14-LOCKS.md:60`). The
table below is the consolidated catalogue; MASTER-PLAN §24 cookbook table
references this catalogue rather than re-enumerating codes.

Phase 8.4 retires the numeric alias system. The catalogue carries
human-readable codes only; the prior numeric aliases (`BBNF-LIFE001`,
`BBNF-LIFE002`, `BBNF-VISIT002`, `BBNF-LAYOUT002`, `BBNF-OPT001`,
`BBNF-OPT002`, `BBNF-PATH001`, `BBNF-PATH002`, `BBNF-GRAMMAR001`,
`BBNF-CG001`) and pure-numeric codes (`BBNF-LIFE003` through `BBNF-SEM040`)
fold into mnemonic names. CLI, LSP, and cookbook surfaces consume the
human-readable form; numeric aliases were LLM-trained-distribution
artefacts that double-tracked an 11-row catalogue for no compression
gain.

| Code | Site | Meaning |
|---|---|---|
| `BBNF-LIFETIME-ESCAPE` | `bbnf` parse API. | Borrow lifetime exceeds source lifetime; use `parse_owned` or extend the source borrow. |
| `BBNF-ARENA-MISMATCH` | `parse_in`. | Caller-provided arena lifetime does not match the parse arena. |
| `BBNF-LOOKBEHIND-WIDTH` | Grammar IR `Lookbehind`; lookbehind width analysis. | Lookbehind `\|<` width is unbounded; constrain the predicate to a finite width or move the assertion into a regex literal. Also rejects unbounded lookbehind reaching Grammar IR before Backend IR. |
| `BBNF-LIFETIME-CONSTRUCTOR` | Generated owned/borrowed constructors. | Emitted constructor violates the lifetime surface contract. |
| `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY` | `runtime/visitor`. | Direct field mutation rejected; mutations route through the read-write visitor entry. |
| `BBNF-VISITOR-NO-MATCHING-KINDS` | Visitor declaration check. | Visitor declares no kinds matching the grammar; warning only. |
| `BBNF-VISITOR-RECOVERY-SKIP` | Visitor recovery. | Recovery nodes silently skipped by the visitor; warning. |
| `BBNF-LAYOUT-CONFLICT` | BIR `LayoutScope`. | Conflicting layout policy. |
| `BBNF-LAYOUT-UNCLOSED` | BIR `LayoutScope` (`Pop`). | Unclosed layout scope reaches the BIR boundary. |
| `BBNF-LAYOUT-UNUSED` | `@layout` lowering. | `@layout` directive is unused by the generated formatter; warning. |
| `BBNF-PRATT-NOT-APPLIED` | `passes::recognizers`. | Pratt detection ran but rejected the rule; cost model declined. |
| `BBNF-SIMD-NOT-SELECTED` | `passes::recognizers`. | SIMD detection ran but rejected the rule; cost, unsupported Unicode semantics, or missing exact/prefilter verifier contract rejected the SIMD path. |
| `BBNF-COST-PLAN-NOT-CANONICAL` | `cost-model` / `codegen::verify`. | A measured alternate materialization, dispatch, primitive, or capacity plan dominates the current canonical plan; generated output must switch plans or record a rejected-with-evidence reason. |
| `BBNF-UTF8-INVALID-AT-PARSE` | `runtime` byte entrypoint / `bbnf-simd` validation. | Invalid UTF-8 reached a public view or string accessor instead of failing at parse/scan boundary. |
| `BBNF-UNICODE-NONCHAR-CODEPOINT` | `parse-that/unicode`. | Unicode escape decoding rejected a Unicode scalar value solely because it is a noncharacter; RFC 8259 JSON accepts such scalar values. |
| `BBNF-FORCE-INLINE-MISSED` | `codegen::verify`. | A rule mined as hot by `LayoutFacts.hot_call_graph` did not receive the required generated inline attribute or failed the post-LTO hot-leaf gate. |
| `BBNF-ICACHE-BUDGET-EXCEEDED` | `codegen::verify` / bench profile. | The fused hot parse driver exceeds the configured hot-function size budget after LTO. |
| `BBNF-METADATA-MISSING-GRAMMAR` | `pipeline::workspace`. | Grammar source declared but no `[workspace.metadata.bbnf.grammars.<name>]` block; Lock 14 requires both surfaces. |
| `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE` | Lock 14 lint. | A generic crate hardcodes a grammar name; `cargo xtask lint-no-hardcoded-grammars` enforces. |
| `BBNF-PATH-UNKNOWN-SEGMENT` | `path` macro. | Path segment does not match the grammar schema. |
| `BBNF-PATH-GRAMMAR-MISMATCH` | `path` macro. | Path expression refers to a different grammar than the inferred root. |
| `BBNF-PATH-UNKNOWN-TERMINAL` | `path` macro. | Path terminal type unknown to the macro; regenerate to refresh the schema. |
| `BBNF-HOST-SIGNATURE-MISMATCH` | `passes::layout` host signature unification. | Host function body cannot satisfy the inferred signature. |
| `BBNF-CHAIN-STEP` | `passes::layout` chain composition. | Chain step does not accept the previous step's output type. |
| `BBNF-HOST-WASM-PRIMITIVE-MISSING` | WASM lowerer. | Host chain cannot lower to WASM; primitive missing in WASM ABI. |
| `BBNF-SUBSUMPTION-EDGE` | `passes::layout` coercion check. | A chain, annotation, host call, or generated-shape projection needs a coercion, but no registered bounded coercion rule exists at that checking edge. |
| `BBNF-GENERIC-CYCLE` | `passes::layout` generic instantiation. | Generic rule monomorphisation would produce an unbounded `(RuleId, TypeArgs)` instance set; add a return annotation, break the recursive type argument, or route through a concrete rule. |
| `BBNF-LOCAL-EQUALITY-ANNOTATION` | `passes::layout` GADT branch-local-equality check. | A match-arm refinement annotation (`Pattern @ where T = U`) is missing or ill-typed; OutsideIn(X)-style implication constraints could not solve the wanted equality from the givens. |
| `BBNF-RECOVERY*` | Error pass. | `@error` directive recovery codes; emitted by `RecoveryFacts` and routed through `ErrorRecover` and LSP diagnostics. |
| `BBNF-CODEGEN-IMPORT-DENY` | Lowerer import-deny check. | Lowerer imports Grammar IR; only the BIR producer pass may consume Grammar IR. |
| `BBNF-CODEGEN-LOC-BUDGET` | Generated LOC budget. | Generated LOC exceeds the per-grammar or aggregate +2 percent budget. |
| `BBNF-CODEGEN-REGEN-EQUALITY` | Regen equality. | BIR snapshot changed without committed generated output. |
| `BBNF-CODEGEN-TEMPLATE-METADATA` | Runtime template metadata. | Template lacks path, visitor, or diagnostic metadata. |
| `BBNF-BIR-LOOKBEHIND-GUARD` | BIR validation. | Unbounded lookbehind reached BIR despite Grammar IR rejection (last-line guard). |
| `BBNF-BACKEND-SHAPE-INCONSISTENT` | `passes::recognizers::derive_backend_shape` (Lock 10 + Lock 15). | Cost-model output cannot resolve a coherent `LayoutFacts.backend_shape` for the rule (e.g., transitive uses simultaneously include `@error(recover)` AND target-feature `avx512vbmi2` admits `CollapsedStage` — the recovery path forces `EagerTape`, blocking the optimisation). Emitted with the conflicting factors named; cookbook entry advises whether to relax the directive or accept the fallback shape. |
| `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` | `passes::recognizers::derive_backend_shape` / `codegen::verify`. | Cost model would select `CollapsedStage`, but the target lacks a green Layer-1 primitive vocabulary, a committed per-grammar `.asm` author, target silicon, or a grammar-specific parity harness. Recovery is automatic fallback to `OffsetTape`; the warning records the missing grammar × ISA pair. |
| `BBNF-FORCE-INLINE-MISSED` | Lock 15 verification at `cargo asm` post-build. | A rule mined as hot-path in `LayoutFacts.hot_call_graph` is generated without `#[inline(always)]` in the emitted source; either the codegen template branch is wrong or the cost-model threshold is mis-tuned. |
| `BBNF-ICACHE-BUDGET-EXCEEDED` | Lock 15 verification at post-LTO `cargo asm` size pass. | The fused hot function (e.g., `parse_value` after LTO) exceeds the per-grammar i-cache budget (default ~20 KiB per yyjson reference). Either reduce hot-path size via per-rule extraction or accept the budget-overrun warning with measurement justification. |
| `BBNF-UTF8-INVALID-AT-PARSE` | Scan stage UTF-8 validation per Lock 9 + the corpora-correctness gap finding 2026-05-12. | Source bytes fail UTF-8 validation at scan time (via `simdutf8::basic::from_utf8`). Parse rejects at scan boundary; view layer never observes invalid bytes. Replaces the prior view-time `from_utf8().expect()` panic path. |
| `BBNF-UNICODE-NONCHAR-CODEPOINT` | String-decode `\uXXXX` resolution. | Non-character codepoint (`U+FDD0..U+FDEF`, `U+nFFFE`, `U+nFFFF` for `n` in 0..=0x10) decoded in a JSON string. Per RFC 8259, these are valid; the prior `char::from_u32` rejection at `parse-that-regex/src/lib.rs:352` was over-strict. Warning, not error; admit by default. |

The verbatim diagnostic strings for each code live with the producer:
`restart/audit/pass-2-codegen/PASS-2.md:533-538` for the codegen and BIR
codes; `restart/audit/pass-3-runtime/PASS-3.md:452-472` for the runtime,
host, layout, path, and visitor codes. The catalogue here binds
identifiers and producer sites; downstream cookbooks reference identifiers
and let consumers inspect the producer for the verbatim string.

### 7.6 Backend Trait

Lock 5 commits to per-backend lowerers as the contract boundary
(`restart/locks/14-LOCKS.md:42`). PASS-1 §2 names the per-backend lowering
obligations table (`restart/audit/pass-1-substrate/PASS-1.md:61-71`). The
formal Rust trait that enforces this boundary is the `Backend` trait. V1
ships `RustBackend: Backend` only; V2 and later add `WasmBackend: Backend`
and `TsBackend: Backend` without re-architecting BIR or codegen.

The trait surface is:

```rust
pub trait Backend {
    type Output;
    type Error;

    fn lower(
        &self,
        bir: &BackendIR,
        ctx: &LowerContext,
    ) -> Result<Self::Output, Self::Error>;

    fn emit_artefacts(
        &self,
        grammar: &GrammarMeta,
        schemas: &SchemaSet,
    ) -> Result<ArtefactSet, Self::Error>;
}
```

The two-method surface is deliberate. The four artefacts (runtime template, value API, visitor, path schema) are co-emitted from a single `(grammar, schemas)` input; per-method dispatch was contrivance because no V1 or V2 caller emits one artefact without the others. `SchemaSet` bundles the value, visitor, and path schemas as one struct; `ArtefactSet` bundles the typed file trees for the four artefacts. The four artefact files remain distinct on disk under `runtime/src/grammars/<g>/`.

Backend trait obligations:

| Method | Input | V1 RustBackend output | V2 WasmBackend output | V2 TsBackend output |
|---|---|---|---|---|
| `lower` | `&BackendIR`, `&LowerContext` | `RustSource` (committed `.rs` artefact tree under `crates/runtime/src/grammars/<g>/`) | `WasmRustSource` (wasm32 lowering of the same `BackendIR`) | `TsSource` (committed `.ts` artefact tree) |
| `emit_artefacts` | `&GrammarMeta`, `&SchemaSet` | `ArtefactSet` bundling `runtime/src/grammars/<g>/{generated.rs, parser.rs, host.rs, view.rs, value.rs, visitor.rs}` + typed `Value` enum + `Visitor` trait + `VisitTypes` bitflag + `<g>.path-schema.toml` + typed `path!` glue | `ArtefactSet` wasm32-pinned mirror plus exported ABI shell | `ArtefactSet` TS package mirror with TS `Value` namespace + d.ts + visitor interface + `<g>.path-schema.toml` + TS `compilePath` glue |

Backend trait invariants:

| Invariant | Gate |
|---|---|
| `Backend::Output` is a typed source artefact, never raw bytes that bypass the committed-source contract. | Lock 6 `xtask` regen equality check; raw byte outputs reject in CI. |
| Lowerers walk `BackendIR` only. Grammar IR is forbidden inside a `Backend` impl. | `BBNF-CODEGEN-IMPORT-DENY` lint at `crates/codegen/src/lower/`. |
| Every grammar in §12.1 lowers through every active `Backend`. V1 has one active `Backend` (`RustBackend`); V2 adds two more without grammar-side changes. | Per-grammar matrix at §12.1 expands columns when a new `Backend` impl lands. |
| The trait is generic-crate code; no grammar names appear inside any `Backend` impl. | Lock 14 generic-crate audit; `rg -nE 'JsonParser|CssL4Parser|BbnfBootstrap|GoogleSheetsParser' crates/codegen/src/` returns zero. |

The `LowerContext` type carries: target triple (or wasm32-equivalent),
generated-code budget cursor, grammar metadata reference, a `&SideTables`
reference whose definition lives at §7.3 (one struct over `LayoutFacts`,
`ShapeFacts`, `RecognizerFacts`, `CostFacts`, `RecoveryFacts`,
`BridgeJustification`), and lint-mode toggles. The `ArtefactSet` type
carries typed file trees with their committed paths and budget metadata,
not raw strings; the four artefact families (runtime template, typed
value API, visitor, path schema) are routed through one struct so the
trait surface stays clean while per-artefact emission policy lives inside
the `Backend` impl body.

V2 deferral note: when V2 admits `WasmBackend` and `TsBackend`, the BIR
alphabet does not change (PASS-1 §2 owns the alphabet). The new impls
implement the trait above; the per-grammar matrix at §12.1 grows columns
for the new lowering targets; the publish gates at MASTER-PLAN J.W3 and
Lock 11 grow rows for the new published artefacts. The trait pre-existence
is the load-bearing piece that makes that V2 expansion mechanical rather
than architectural.

## 8. BBNF Language Surface

The BBNF surface supports the extensions settled in README and PASS-1:
lookbehind, block-bodied `@host fn`, multi-function chaining, generics,
`@error(recover = ...)`, and `@layout` (`restart/README.md:121-182`,
`restart/audit/pass-1-substrate/PASS-1.md` §6). It does not contain
rewrite-mode or grammar-level Unicode class algebra.

### 8.1 Core Grammar Sketch

```ebnf
Grammar       ::= (Directive | RuleDecl)*
Directive     ::= ImportDecl | HostFn | LayoutDecl | ErrorDecl | PrettyDecl | TokenDecl

ImportDecl    ::= "@import" "{" Ident ("," Ident)* "}" "from" StringLit ";"
HostFn        ::= "@host" "fn" Ident GenericParams? "(" ParamList? ")"
                  "->" Type HostAttrs? Block
RuleDecl      ::= "rule"? Ident GenericParams? RuleParams? ReturnType?
                  "=" Expr MapTail? ";"
LayoutDecl    ::= "@layout" Ident LayoutBody
ErrorDecl     ::= "@error" Ident ErrorBody
PrettyDecl    ::= "@pretty" Ident PrettyStrategy+ ";"
PrettyStrategy ::= "compact" | "group" | "indent" | "hardbreak" | "sep" "(" StringLit ")" | "block"
TokenDecl     ::= "@token" Ident ";"

GenericParams ::= "<" Ident ("," Ident)* ">"
RuleParams    ::= "(" ParamList? ")"
ParamList     ::= Param ("," Param)*
Param         ::= Ident ":" Type
ReturnType    ::= "->" Type

Expr          ::= Alt
Alt           ::= Seq ("|" Seq)*
Seq           ::= Prefix*
Prefix        ::= Lookbehind | Lookahead | Suffix
Lookbehind    ::= Expr "|<" Expr | Expr "|<!" Expr
Lookahead     ::= "&" Suffix | "!" Suffix
Suffix        ::= Primary Quantifier?
Quantifier    ::= "?" | "*" | "+" | "{" Number ("," Number?)? "}"
Primary       ::= Literal | Regex | Ref | Group | HostCall | LambdaExpr
Group         ::= "(" Expr ")"
Ref           ::= Ident TypeArgs?
TypeArgs      ::= "<" Type ("," Type)* ">"
HostCall      ::= "@" Ident "(" HostArgs? ")"
LambdaExpr    ::= "|" ParamList? "|" (Expr | Block)
MapTail       ::= "->" ChainExpr
ChainExpr     ::= Ident ("->" Ident)*
Regex         ::= "/" RegexProgram "/"
Type          ::= Ident TypeArgs? | TupleType | RecordType | BorrowType | FnType
FnType        ::= "fn" "(" (Type ("," Type)*)? ")" "->" Type
```

`RegexProgram` is parsed by `parse-that-regex` (the regex sub-crate of
`parse-that`). Unicode classes may exist inside that regex syntax, but BBNF
itself does not expose a set algebra surface.

This sketch is a synthesis copy of PASS-1 §6. The rule-level chain form is
`Expr -> f1 -> f2`; a generic rule therefore chains as
`Object<V> = Expr -> f1 -> f2;`, with `Object<V>` parsed through `Ref` and
`GenericParams` / `TypeArgs`. Method-chain syntax such as `a.f().g()` is legal
only inside the block body of `HostFn`. Bodyless host declarations have no
production. Recovery is owned by `ErrorDecl`: `recover = ...` lives inside
`ErrorBody`, and standalone recovery directives have no production.

V1 directive canon: the six-directive `Directive` production above is the
complete V1 surface; `RuleDecl` is a grammar member, not a directive. `@import` carries cross-file grammar composition
(`grammar/bbnf/bbnf.bbnf:4-5`); `@host fn` carries typed host primitives
with block-bodied implementations; `@error(recover = ...)` carries
recovery vocabulary; `@layout` carries layout policies (with `@ws`
folded into `@layout(ws = ...)`); `@pretty` carries pretty-printing
strategy with the verbatim vocabulary `compact`, `group`, `indent`,
`hardbreak`, `sep(...)`, `block` preserved across the 30+ extant grammar
sites (`grammar/json/json.bbnf:18-20`); `@token` carries atomic-token
markers binding to the BIR scanner (`grammar/css/pretty.bbnf:17-19`).
`@debug` is a host primitive, not a directive. The retired catchall
`Annotation = "@" Ident AnnotationBody?` does not appear in the V1
production; the `directive-canon` lint at §13.1 rejects every
`@directive` outside this set, including the explicitly retired
`@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, `@ws`,
standalone `@recover`.

V1 function-value surface: `Type` admits `FnType = "fn" "(" ... ")" "->"
Type`, making function types first-class. The `Primary` site replaces
the prior `Closure` non-terminal with `LambdaExpr = "|" ParamList? "|"
(Expr | Block)`; lambda captures by `&'i Tape<'i>` reference only; the
`Fn` / `FnMut` / `FnOnce` discrimination Rust exposes is collapsed at
the BBNF surface; capture-by-move is forbidden in V1. The transducer
apotheosis follows: `@host fn map<I, O>(f: fn(I) -> O, xs: [I]) -> [O]
{ ... }` is well-typed without a `@transducer` directive because `f`
carries a function type and `map` carries a higher-rank scheme.

Input-normalization deletions are explicit and named. Every surface listed below
is forbidden from the BBNF grammar and from any IR variant; the named substrate
is the only place the corresponding capability may live.

| Surface | Status at BBNF level | Routed substrate | Closing gate |
|---|---|---|---|
| Rewrite-mode (transducer/rewrite ladders). | Deleted; not parsed, not lowered, not represented in either IR. | None; rewrite-mode does not survive as a feature. | `rg "rewrite-mode\|RewriteMode\|@rewrite"` returns zero outside this deletion table. |
| Unicode class set algebra (`[[:alpha:]]&&[^a-z]`, `\p{L}--\p{Cyrillic}`). | Deleted from BBNF grammar surface. | `parse-that-regex` HIR may model class algebra inside a regex literal; BBNF never exposes it as productions. | Architecture §8.1 has no `CharClass`/`ClassExpr` production; `parse-that-regex` carries the algebra under HIR. |
| Grammar-level `(?<= ...)` lookbehind syntax. | Deleted in favor of `|<` and `|<!`. | `parse-that-regex` retains regex-internal `(?<= ...)`; BBNF parser rejects it outside regex literals. | PASS-1 spec uses `|<` only; lookbehind diagnostic carries `BBNF-LOOKBEHIND-WIDTH`. |
| Standalone `@recover` directive. | Folded into `@error(recover = ...)`; not a separate top-level form. | `error` crate keeps the recovery descriptor; grammar parser rejects bare `@recover`. | PASS-3 amendment; no production for `Recover ::= ...` outside `@error` body. |
| Per-grammar declaration crates. | Not default; allowed only via fenced metadata escape valve. | Workspace `[workspace.metadata.bbnf.grammars.<name>]` plus the §5.6 review form. | Lock 14 lint passes; the §5.6 fence supplies reason/scope/deletion path. |

### 8.2 Type System

The V1 type system has a rank-1 Hindley-Milner principal-scheme core, an
expected-type check/synth interface, bounded coercion obligations, and finite
CSP choices for grammar-derived implementation alternatives. README sets HM,
bidirectional typing, CSP use, explicit annotations, generic rules,
subtyping/coercion, and lookbehind types as in-scope
(`restart/README.md:258-268`). PASS-1 adds host/chaining/generic constraints
to the type contract (`restart/audit/pass-1-substrate/PASS-1.md:24-42`).

Type rules:

| Rule | Contract |
|---|---|
| HM equality constraints | Fresh type variables, scheme instantiation, generalization, and first-order unification produce internal `TypeFacts`; failures preserve source-span, expected-from, actual-from, and solver-stage metadata in `TypeObligationLog`. |
| Expected-type checking | Nodes synthesize unless an annotation, directive, host signature, branch context, or chain step supplies an expected type; annotations check inferred types rather than bypassing them. |
| Bounded coercion | Numeric widening, lifetime-owned escalation, generated-record shape narrowing, and host-improvement rules produce named obligations at explicit checking edges; each obligation lowers to an explicit coercion or fails before Backend IR. |
| Finite-choice CSP | CSP solves bounded non-HM choices: host overload selection, layout representation, materialization mode, recognizer eligibility, recovery strategy, backend erasure, and extraction legality. |
| Generic rules | Rule schemes are generalized at definition, instantiated at each `Ref`/call site, recorded with source spans, and admitted to codegen only after finite `(RuleId, TypeArgs)` monomorphisation evidence exists. |
| Host functions are typed. | Block-bodied `@host fn` definitions and generic primitives share the same checker; host overloads with determining arguments are represented as explicit improvement constraints before finite CSP selection. |
| Chains compose left-to-right. | The left expression synthesizes a value type; each step checks against the previous step's output as its first expected argument; the chain fails at the first mismatch. |
| Lookbehind must be bounded. | Unbounded lookbehind is rejected before Backend IR. |
| Layout and error directives are typed side effects. | They produce `LayoutFacts` and `RecoveryFacts`, not ad hoc codegen flags. |

V1 generic rules are parametric HM schemes. The V1 type system composes
five named mechanisms: HM-equality (Algorithm-W; Damas-Milner 1982;
Pierce 2002 ch.22) + Pierce-Turner local check/synth (the bidirectional
expected-type interface above) + DK13 higher-rank algorithmic completeness
(Dunfield-Krishnaswami 2013; ordered existential contexts, principality
tracking, decidability, soundness, completeness, explicit annotation
rules for non-principal programs) + finite CSP for non-HM choices +
GADT branch-local-equality refinement (Phase 8.3.1 V1 fold; OutsideIn(X)
implication constraints discharged at `passes::types` and propagated to
`LayoutFacts`). HM-equality, Algorithm-W, and first-order unification are
one algorithm — Damas-Milner principal-scheme inference with first-order
unifier — presented as one named mechanism rather than three. CHR-style
improvement is a constraint-emission helper inside `csp-solver` (Phase
8.3.1 V1 fold; not a separate type-system layer); the helper closes
host-overload ambiguity at the bridge boundary by materialising the
finite improvement constraints the CSP solver consumes. Higher-rank
polymorphism is therefore a V1 surface, not a future amendment. The
user mandate's "inference stronger than Rust if possible" is honoured by
DK13's principality tracking, which admits annotation-elidable
polymorphism that Rust requires the programmer to write out.

GADT branch-local-equality refinements are V1 user-facing surface:
match-arm patterns admit refinement annotations (`Pattern @ where T = U ->
Block` per §8.1 grammar production), and OutsideIn(X)-style implication
constraints solved at `passes::types` propagate the local equalities
through to `LayoutFacts`. The CSP solver carries `Implication { givens,
wanted }` constraints as the substrate for branch-local equality; the
`BBNF-LOCAL-EQUALITY-ANNOTATION` diagnostic emits when a match-arm
refinement annotation is missing or ill-typed (annotation rules per the
PASS-1 §6b diagnostic ledger). Row polymorphism remains internal
(record-narrowing collapse in `passes::layout`); the user-facing row-poly
surface defers to a later type-system research gate, not to V1.

Function values + types are first-class V1 surface. The `Type`
non-terminal admits `fn(T) -> U` (PASS-1 §2 grammar amendment); function
parameters in `@host fn` accept function-typed arguments (the transducer
apotheosis without a `@transducer` directive). Closure capture is by
`&'i Tape<'i>` reference only; capture-by-move is forbidden in V1; the
`Fn` / `FnMut` / `FnOnce` discrimination Rust exposes is collapsed at the
BBNF surface in V1 — the lifetime-bounded reference closure is the only
V1 form.

Schema-mining miner: the type system runs telemetry-driven schema
inference. Most user grammar rules emit `LayoutFacts` and `ShapeFacts`
without explicit annotations; the miner consumes parsed corpus telemetry,
proposes candidate shapes, runs them through the HM/CSP/DK13 solver
chain, and rejects candidates that fail principality or finite-CSP
legality. The user's mandate "type algebra + telemetry to generate
semantic schemas without explicit annotations in most cases" lands here.

Record narrowing in V1 is finite generated-shape coercion only: source and
target shapes must both be known at compile time. The internal
row-polymorphism collapse is a `passes::layout` subroutine, never a public
artefact.

### 8.3 Host Functions

Host functions decompose through:

1. Generic primitives in `host`.
2. Workspace metadata.
3. Block-bodied `@host fn` definitions.

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
| Host chain closure | Previous host result and explicit args only. | Output of segment N unifies with input of segment N+1. | Lowered to `Seq` of `CallHost`. |
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
| `payload` | Scalar payload slots, normalized strings, and auxiliary arenas. |
| `view` | Borrowed read views. |
| `trace` | Debug/replay event hooks. |

Tape invariants:

| Invariant | Gate |
|---|---|
| Append-only after committed checkpoints. | Builder tests. |
| Rollback is bounded and does not clone OpenFrame stacks. | Perf tests and code review. |
| Tokens borrow source slices where possible, and payload policy declares when normalized strings or parsed scalars live in the payload arena. | Slice-borrow API tests plus materialisation-cost artefact. |
| Direct views can point into tape. | `DocumentView` tests. |
| Every public node has one `(TapeId, node id, payload class)` identity. | Runtime identity smoke for document root, `ValueRef`, path/select, visitor, and debug trace. |

### 9.2 Direct-To-Struct Union

Direct builders do not bypass the substrate event stream. Retained direct
views are projections over sealed tape; `SinkOnly` direct outputs are
projections over the same accepted event stream with no sealed tape. Both
lower from `TapeEmit` / `DirectBuild` scheduling and share validation, spans,
source slices, diagnostics, node kind, and payload policy. Direct scalar fields
are caches over declared payload slots or generated sink fields, never a
second authoritative tree.

The SK-V3 direct workload adds one binding SOTA caveat: a direct typed view over
retained tape proves correctness, not throughput. For public APIs that do not
require path/value traversal after parse, `LayoutFacts.backend_shape` must
select `SinkOnly` and lower `DirectBuild` to typed-field writes during parsing.
The first skinny sink-only digest parser closes the retained-view penalty but
still misses sonic-rs direct on 16 of 17 rows after duplicate UTF-8 validation,
scanner-owned integer classification, context-specific scalar hooks, and BIR
lowered generated `SinkOnly` landed; direct SOTA therefore also depends on
exact float/string/Unicode materialization primitives inside the sink.
Generated direct string lowering now reaches the sink as `(raw, needs_decode)`
source hooks; the generic no-allocation decoded visitor, the sink-local exact
decoded-stats helper, and the quote-source streaming hasher were measured and
rejected, so the remaining string close is a field-layout or same-loop
decode+sink primitive.
The retained view-walk digest remains a parity oracle and regression check; it
is not a SOTA-class direct-to-struct close.

The owning shape splits by materialization. Retained APIs are document-first:
a generated root owns or is paired with a sealed `Tape<'input>` snapshot, and
`ValueRef`/typed projections borrow that tape. A root over a borrowed
parser-state tape is not the committed runtime shape. `SinkOnly` APIs retain
no document identity after parse; only the typed output remains.

The runtime materialization model is:

```text
byte input
  -> mask stream
  -> typed event cursor
  -> { OffsetTape | EventTape | SinkOnly | CollapsedStage }
  -> DocumentView / direct typed output
```

The mask stream is a transient producer. It may be scanned by SIMD or scalar
kernels, but it is not a retained substrate. The typed event cursor is the
shared read/write abstraction: it walks offsets, event cells, or collapsed
state transitions and emits either a sealed tape or direct typed fields. Retained
document identity is `(TapeId, cursor, event_kind_or_payload_class)`. `SinkOnly`
has no document identity because no queryable document remains after parse.
SK-V5 redress item 50 rejects parse-time retained projection side tables as the
implementation of this cursor: dense and sparse aux columns improved view
probes but regressed the parse plane. Event consumption must reduce source
rediscovery without writing another retained column during parse.

Runtime SOTA gates must publish a token-economy artifact alongside throughput:
token count, logical tape bytes, allocated tape bytes, tape bytes per input
byte for both, payload bytes, pair-token count, open/close token counts,
scalar-token count, and skip-count/skip-class counts. If structural scan and
zero-arena gates pass while parse Mbps misses the competitor envelope,
close-token emission, pair-token emission, skip patching, allocation capacity,
and tape sealing are sanctioned perturbation candidates. JSON skinny adopted
close-token elision and private-Vec semantic sealing after before/after bench
rows; pair-token fusion measured as token-count-positive but throughput-negative
and is not canonical. A 12-byte skipless-token perturbation reduced logical
tape bytes but produced mixed throughput (twitter regression, citm gain, canada
noise) and is also not canonical. These remain Lock 1 substrate choices and
require before/after bench rows rather than a second tree or hidden side
substrate.

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
are emitted under `runtime/src/grammars/<name>` (`restart/audit/pass-2-codegen/PASS-2.md` §7).

## 10. Codegen And Lowerers

Code generation is Backend-IR-only. Lock 5 forbids source-emitting per backend
and forbids emitters walking grammar directly (`restart/locks/14-LOCKS.md:42`).
PASS-2 defines the `BackendLowerer` contract and final lowerer ownership
(`restart/audit/pass-2-codegen/PASS-2.md:80-96`).

Lowerers:

| Lowerer | V1 contract | Trait impl |
|---|---|---|
| Rust | Primary production lowerer. Emits runtime template, tape/direct builder, host chain calls, visitors, value projections. | `RustBackend: Backend` per §7.6. |
| SIMD | Pattern lowerer fed by recognizer facts and validated `SimdScan` BIR; exact scans need scalar parity, and prefilters need a verifier route before tape emission. | Co-impl inside `RustBackend` (cfg-gated through `bbnf-simd`). |
| VM | Executable interpreter for debug, replay, and golden equality. | Internal evaluator over `BackendIR`; not a public `Backend` impl (replay-only). |
| WASM | Deferred post-V1; lands as `WasmBackend: Backend` in V2 alongside Lock 11 publication carry. | V2. |
| TS | Deferred post-V1; lands as `TsBackend: Backend` in V2 alongside the principled TS-native parse+runtime fork. | V2. |

Generated source is committed. Lock 6 rejects a proc-macro facade and requires
`xtask`-style committed source generation (`restart/locks/14-LOCKS.md:44`).

### 10.1 Rewrite-Budget Categories And Thresholds

The egraph + cost-model bridge (§7.3 `EGraphFacts`, `BridgeJustification`,
`CostFacts`) rewrites Backend IR plans within a per-category saturation
budget. Lock 4's per-domain orthogonality (`restart/locks/14-LOCKS.md:40`)
demands that each rewrite category run inside its own budget pool with
its own legality-vs-cost discipline. The post-Phase-8.4 fold lands three
categories — `legality-rewrites` and `normalization-rewrites` are
LOAD-BEARING for V1 meta-grammar correctness; `cost-driven-rewrites` is
ASPIRATIONAL for V1 SOTA throughput at the H tranche body. The prior
fourth category, `simplification-rewrites`, folds into `codegen::verify`
at F.W3 (one-pass dead-mark elision belongs alongside regen-equality, not
as an e-graph budget pool).

| Category | Purpose | Budget mode | Threshold (V1) | V1 classification | Owner |
|---|---|---|---|---|---|
| `legality-rewrites` | Mandatory canonicalisations that no plan may skip (e.g., empty-Seq normalisation, nullable-body Repeat rejection, dispatch-key disjointness). | Saturate to fixpoint; failure aborts compile. | unbounded steps; aborts on cycle detection (`debug_assert!` cycle hash). | LOAD-BEARING (correctness). | `passes::normalize`. |
| `normalization-rewrites` | Optional canonicalisations that simplify lower-time choices (e.g., literal-ordering, alt-flattening, redundant-predicate elision). | Saturate to fixpoint with step budget. | ≤ 1024 steps per `Rule`; halts on first plateau. | LOAD-BEARING (correctness-adjacent). | `egraph::rewrite` driven by `passes::normalize`. |
| `cost-driven-rewrites` | Cost-model-led plan selection (e.g., Pratt vs scalar, SIMD vs scalar, dispatch-tree vs jump-table). | Bounded e-graph saturation gated by `CostModel::should_continue`. | ≤ 256 e-class merges per `Rule`; per-grammar override via `[workspace.metadata.bbnf.grammars.<g>.rewrite_budget]`. | ASPIRATIONAL (throughput-bound; H tranche body). | `egraph` + `cost-model::frontier`. |

Post-extraction local simplifications (dead-`SpanMark` removal, `TapeEmit`
coalescing, `DebugMark` elision under non-debug profile) run inside
`codegen::verify` as a single pass over the extracted plan, with no
fixpoint and no e-graph need; F.W3 absorbs the simplification step
alongside regen-equality.

The thresholds bind to the e-graph saturation budgets owned by the
`egraph` crate; per-grammar overrides flow through workspace metadata so
extreme grammars (CSS L4 colour-function chain; Sheets formula Pratt
spine) admit larger budgets without bloating the default. Threshold
violations emit `BBNF-PRATT-NOT-APPLIED` and `BBNF-SIMD-NOT-SELECTED`
where applicable; the diagnostic identifies which budget pool exhausted
and which `CostFacts` row the rewrite stalled on.

## 11. Performance Targets

SOTA gates are explicit restart requirements. README names the target family:
twitter <= 380 us, canada <= 2.8 ms, citm <= 750 us, CSS bootstrap <= 3 ms,
animate <= 1.6 ms, and simdjson on-demand 56000 Mbps-class x86 targets
(`restart/README.md:322-340`). Lock 8 lists competitor anchors: simdjson OD
56000 Mbps, sonic-rs M1 twitter 436 us, and lightning-css Bootstrap 4.16 ms
(`restart/locks/14-LOCKS.md:48`). SOTA.md records the supporting competitor
benchmarks (`restart/corpora/SOTA.md:50-89`, `restart/corpora/SOTA.md:130-136`).

Gate owners:

| Gate | Owner |
|---|---|
| JSON twitter/citm/canada | `bbnf-bench`, `cost-model`, `runtime`, `bbnf-simd`. |
| CSS bootstrap/animate | `bbnf-bench`, `runtime`, `passes::recognizers`, `codegen::rust`. |
| simdjson-class throughput | `bbnf-simd`, `runtime::tape`, `codegen::simd`. |
| Generated LOC budget | `cost-model::loc_budget`, `codegen::verify`. |
| Parallel-substrate clone absence | `runtime`, `codegen`, `bbnf-bench`. |

PASS-2 sets a generated LOC budget baseline and allows a +2 percent ceiling
for emitted runtime source (`restart/audit/pass-2-codegen/PASS-2.md` §6).
All benchmark rows record parse mode, validation mode, source ownership mode,
materialisation mode, string ownership mode, scalar-cache policy where direct
or tape cursor rows expose one, competitor flags, and whether regex/SIMD paths
ran as exact scans, prefilters, VM, lazy DFA, or full DFA. `parse(&str)` rows
record Rust prevalidation; byte/file entry-point rows record bbnf validation
before any `ValueRef` is exposed.

Exact gate rows:

| Row | Competitor baseline | Restart target | Required metadata |
|---|---|---|---|
| `json/twitter` | sonic-rs 436us, simd-json 424us on M1 Pro. | <= 380us on M1 Pro. | CPU model, OS, compiler flags, input hash, parse mode, competitor version, bbnf commit, warmup, sample policy. |
| `json/citm` | sonic-rs 854us, simd-json 831us on M1 Pro. | <= 750us on M1 Pro. | CPU model, OS, compiler flags, input hash, selector mode, competitor version, bbnf commit, warmup, sample policy. |
| `json/canada` | sonic-rs 3.144ms, simd-json 3.226ms on M1 Pro. | <= 2.8ms on M1 Pro. | CPU model, OS, compiler flags, input hash, array scan profile, competitor version, bbnf commit, warmup, sample policy. |
| `css/bootstrap` | lightning-css 4.16ms on M1 Pro. | <= 3.0ms on M1 Pro. | CSS fixture hash, layout mode, visitor mode, competitor version, bbnf commit, warmup, sample policy. |
| `css/animate` | lightning-css 1.97ms on M1 Pro. | <= 1.6ms on M1 Pro. | CSS fixture hash, layout mode, visitor mode, competitor version, bbnf commit, warmup, sample policy. |
| `simd/structural_scan` | simdjson On-Demand ~56000 Mbps on x86 AVX2; ~40000 Mbps on M-series NEON. | >= 40000 Mbps on M-series, >= 56000 Mbps on x86 AVX2; scalar parity hash matches. | ISA, CPU flags, kernel, scalar parity hash, competitor version, bbnf commit, warmup, sample policy. |

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

### 12.1 YAML Onboarding Walkthrough

| Step | Author input | Generated output | Gate |
|---|---|---|---|
| Grammar | `grammars/yaml.bbnf` carries yaml rules, any block-bodied `@host fn` definitions, and any `@error(recover = ...)` policy needed by the grammar. | Grammar IR and BIR snapshots for yaml. | `cargo xtask bbnf check yaml` accepts the grammar and rejects standalone recovery directives. |
| Metadata | One `[workspace.metadata.bbnf.grammars.yaml]` block names `source = "grammars/yaml.bbnf"`, `output_dir = "crates/runtime/src/grammars/yaml"`, `recognizers = "auto"`, `pratt = "auto"`, `simd = "auto"`, `wasm = false`, and `generated_loc_budget = 1.02`. | Template parameters for runtime files, path schema, visitor metadata, host route, diagnostics, and budget sidecars. | Metadata validation rejects manual Rust registry, manual path registry, manual host shim, and declaration-crate onboarding. |
| Runtime generation | No handwritten Rust input. | `runtime/src/grammars/yaml/{generated.rs,parser.rs,host.rs,view.rs,value.rs,visitor.rs}`, `yaml.path-schema.toml`, diagnostic metadata, and the yaml bench manifest are emitted from grammar + metadata. | `cargo xtask bbnf build yaml` plus `git diff -- crates ':!crates/runtime/src/grammars/yaml'` shows zero generic-crate Rust changes. |
| Benchmark gate | No `fixtures/yaml/` input during onboarding. | Provisional yaml budget row reports `generated_loc <= 4,000`, regen wall, and bench-manifest metadata; parity fixtures are a later J gate. | `cargo test -p bbnf --test future_grammar_yaml` and `cargo test -p bbnf-language-server yaml_metadata` pass before parity fixtures are admitted. |

### 12.2 Per-Grammar Authority Table

Lock 14 mandates per-X tables for "all nine seed grammars" claims. The
authoritative 10-row × 9-column matrix below covers the nine extant grammars
plus the `yaml` onboarding probe. It is fed by PASS-2 §7 runtime emission table,
PASS-2 §6 LOC budget table, and PASS-3 §6a feeder table
(`restart/audit/pass-3-runtime/PASS-3.md:333-344`); architecture is the
authoritative consumer, the PASS surfaces remain the producer-side reference.
Every "all extant grammars" or "all nine seed grammars" claim elsewhere in this
document resolves against this table.

| Grammar | Typed root | `ValueRef` borrow shape | Runtime files emitted | Visitor + `VisitTypes` | Path schema | `path!` macro typing | Regex engine | Fixture manifest | Host route | Generated LOC (current → max) | Declaration-crate status |
|---|---|---|---|---|---|---|---|---|---|---:|---|
| `bbnf` | `Bbnf` | `BbnfRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs`, layout, error, `PrattSpine` LUT | `BbnfVisitor`, `BbnfVisitTypes` | `bbnf.path-schema.toml` | `path!` typed against `bbnf.path-schema.toml`; `pointer!` retires per Lock 7 + naming-canon lint | `parse-that-regex` (sub-crate of `parse-that`) | `fixtures/bbnf/manifest.toml` | self-host primitives plus regen utilities; metadata + `@host fn` blocks in `bbnf.bbnf` | 21,503 → 21,933 | none (default; §5.6 fence empty) |
| `bnf` | `Bnf` | `BnfRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, layout, error | `BnfVisitor`, `BnfVisitTypes` | `bnf.path-schema.toml` | `path!` typed against `bnf.path-schema.toml` | `parse-that-regex` | `fixtures/bnf/manifest.toml` | none (pure recogniser; metadata-only host stanza) | 3,290 → 3,356 | none |
| `csv` | `Csv` | `CsvRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, layout, error, `SimdScan` for delimiter alphabet | `CsvVisitor`, `CsvVisitTypes` | `csv.path-schema.toml` | `path!` typed against `csv.path-schema.toml` | `parse-that-regex` | `fixtures/csv/manifest.toml` | metadata + escape host fns from `host::primitives` | 1,693 → 1,727 | none |
| `css_l4` | `CssL4` | `CssL4Root` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs`, `layout.rs`, error, `SimdScan` for structural alphabet | `CssL4Visitor`, `CssL4VisitTypes` | `css_l4.path-schema.toml` | `path!` typed against `css_l4.path-schema.toml` | `parse-that-regex` | `fixtures/css/manifest.toml` | colour-function host primitives plus length conversion via `host::primitives`; metadata + `@host fn` blocks | 107,138 → 109,281 | none |
| `css_pretty` | `CssPretty` | `CssPrettyRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, `layout.rs`, error | `CssPrettyVisitor`, `CssPrettyVisitTypes` | `css_pretty.path-schema.toml` | `path!` typed against `css_pretty.path-schema.toml` | `parse-that-regex` | shares `fixtures/css/` corpus | metadata + format host fns from `host::primitives` | 9,021 → 9,201 | none |
| `ebnf` | `Ebnf` | `EbnfRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, layout, error | `EbnfVisitor`, `EbnfVisitTypes` | `ebnf.path-schema.toml` | `path!` typed against `ebnf.path-schema.toml` | `parse-that-regex` | `fixtures/ebnf/manifest.toml` | none (metadata-only host stanza) | 7,646 → 7,799 | none |
| `google_sheets` | `GoogleSheets` | `GoogleSheetsRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs`, layout, error, `PrattSpine` for operator precedence | `GoogleSheetsVisitor`, `GoogleSheetsVisitTypes` | `google_sheets.path-schema.toml` | `path!` typed against `google_sheets.path-schema.toml` | `parse-that-regex` | `fixtures/sheets/manifest.toml` | range/date/array-literal host primitives plus formula host chains | 14,088 → 14,370 | none |
| `json` | `Json` | `JsonRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, layout, error, `SimdScan` for structural alphabet (twitter/citm/canada hot path) | `JsonVisitor`, `JsonVisitTypes` | `json.path-schema.toml` | `path!` typed against `json.path-schema.toml` (the canonical SOTA-anchor case) | `parse-that-regex` | `fixtures/json/manifest.toml` | metadata + numeric/string host fns from `host::primitives` | 3,500 → 3,570 | none |
| `math` | `Math` | `MathRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, layout, error, `PrattSpine` for operator precedence | `MathVisitor`, `MathVisitTypes` | `math.path-schema.toml` | `path!` typed against `math.path-schema.toml` | `parse-that-regex` | `fixtures/math/manifest.toml` | metadata + numeric host fns from `host::primitives` (Pratt-eligible operator chain only) | 871 → 888 | none |
| `yaml` (onboarding probe) | `Yaml` | `YamlRoot` owns sealed `Tape<'i>` | `generated.rs`, `parser.rs`, `host.rs` (if metadata declares host route), layout, error; Pratt/SIMD auto-detected from grammar shape | `YamlVisitor`, `YamlVisitTypes` | `yaml.path-schema.toml` | `path!` typed against `yaml.path-schema.toml` (parity-phase only) | `parse-that-regex` | parity-phase `fixtures/yaml/manifest.toml` (post-onboarding gate, never an onboarding surface) | decomposed via `host::primitives` plus block-bodied `@host fn` chain in the metadata block per `restart/README.md:155`; no Rust per-grammar code emerges from onboarding | 0 → ≤ 4,000 (provisional; SYNTHESIS Wave-2 owner) | none (Lock 14 onboarding admits exactly two surfaces: `yaml.bbnf` plus `[workspace.metadata.bbnf.grammars.yaml]`; declaration crate is forbidden at onboarding) |

Column semantics:

| Column | Definition |
|---|---|
| Typed root | The generated direct-to-struct type returned by `parse(&'i str)` per PASS-3 §2. |
| `ValueRef` borrow shape | The generated root/document owns a sealed `Tape<'i>` snapshot; `ValueRef<'doc, 'i, K>` is the untyped tape-cursor view borrowing that document tape. It backs `path!`, `select!`, visitors, and the debugger per Lock 1 and PASS-3 §4. The legacy `pointer!` macro retires under the naming-canon lint; the canonical macro is `path!`. |
| Runtime files emitted | Template-emitted files under `runtime/src/grammars/<name>/`; every cell is generated or data-only, hand-written runtime files are forbidden by Lock 14. |
| Visitor + `VisitTypes` | The generated `Visitor` trait and its bitflag-pruned visit-type set per PASS-3 §3. |
| Path schema | The generated path-schema sidecar consumed by `path!` / `select!` typing per PASS-3 §3. |
| `path!` macro typing | The compile-time path-AST typing surface backed by `path-core`. Every grammar's `path!` invocation types against the matching path-schema sidecar; rejects mismatches with `BBNF-PATH-UNKNOWN-SEGMENT` / `BBNF-PATH-GRAMMAR-MISMATCH` per the §7.5 catalogue. |
| Regex engine | Every grammar lowers `Regex` BIR variants through `parse-that-regex` (the regex sub-crate of `parse-that`); the regex-automata oracle role retires per V1-FOLD-CANDIDATES Tier 3 #23. |
| Fixture manifest | The corpus manifest under `crates/test-fixtures/corpus/`; `yaml` carries a parity-phase manifest only. |
| Host route | The host-function decomposition source: `@host fn` blocks in the grammar, generic primitives in `host::primitives`, or workspace-metadata directives. Declaration crates are not part of the default host route. |
| Generated LOC | PASS-2 §6 baselines and +2% ceiling per grammar; LOC excludes `parser.rs` macros and `host.rs` shells per Lock 13's generated-file exemption. |
| Declaration-crate status | `none` (default; §5.6 fence empty) for every grammar in the seed set. Any future entry must populate the eight-field §5.6 review form. |

V1 trajectory carry: TS and WASM lowering columns are absent from the
matrix above. They land in V2 alongside `WasmBackend: Backend` and
`TsBackend: Backend` (§7.6) plus the Lock 11 V2 publication rows
(`restart/locks/14-LOCKS.md:54`). The V1 `RustBackend` is the sole
active `Backend` impl; the per-grammar matrix grows columns mechanically
when a new `Backend` impl lands.

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

### 13.1 Lint Manifest

Lint categories are an architectural contract, not a tooling preference.
Each lint rejects a specific class of corpus drift; together they form
the structural fence around the generic-crate vs grammar-named code
boundary, the directive canon, the regex-engine canon, and the
per-grammar metadata fence. Each lint binds to either a `cargo clippy`
extension or a `cargo xtask lint` step.

| Lint category | What it rejects | Bound to | Diagnostic code |
|---|---|---|---|
| `directive-canon` | Any `@directive` outside the six V1 directives (`@import`, `@host fn`, `@error`, `@layout`, `@pretty`, `@token`). Retired directives `@pratt`, `@simd`, `@transducer`, `@rewrite`, `@unicode`, standalone `@recover`, and `@ws` (folded into `@layout(ws = ...)`) are explicit reject targets. | `cargo xtask lint --directive-canon` plus `grammar/parse/directives` syntax check. | `BBNF-DIRECTIVE-RETIRED`. |
| `naming-canon` | `pointer!` macro mentions in greenfield text or new code (the macro is `path!`); `bbnf-path` / `bbnf-path-ts` crate-name references (the canonical names are `path` and `path-ts`); `bbnf-regex` references (the canonical name is `parse-that-regex`). | `cargo xtask lint --naming-canon`. | `BBNF-NAMING-DRIFT`. |
| `regex-engine-canon` | `regex-automata` import in any V1 active reference (the regex engine is `parse-that-regex`); the corpus removes the regex-automata oracle role per V1-FOLD-CANDIDATES Tier 3 #23. | `cargo xtask lint --regex-engine-canon` plus `Cargo.toml` deny-import gate. | `BBNF-REGEX-ENGINE-DRIFT`. |
| `per-grammar-fence-canon` | Lock 14 violations: grammar names in generic-crate source, grammar-named modules outside `runtime/src/grammars/<g>/`, per-grammar match arms in `bbnf-ir`/`passes`/`codegen`/`runtime`/`host`/`path`/`path-core`/`egraph`/`csp-solver`/`parse-that`/`parse-that-regex`/`bbnf-simd`/`analysis`/`lsp`. | `cargo xtask lint-grammar-generalization` (existing) plus `cargo xtask lint --fence-canon`. | `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`. |

The lint manifest is part of the architectural contract; lints retire
only by explicit amendment. Adding a lint requires extending this table
and the matching `cargo xtask lint` subcommand; removing one requires
the same.

### 13.2 Cookbook Page Contract

Every cookbook page consumed by the J.W2 publication gate (see MASTER-PLAN
§24 friction ledger) follows a uniform four-field contract. The contract
exists so cookbook pages compose into a single doc-set rather than seven
varieties; J.W2 rejects pages that omit any field.

| Field | Required content |
|---|---|
| Audience + mental model | The named user type (library consumer, grammar author, LSP integrator, parity engineer) plus the one-paragraph mental model the page commits to. The audience field admits exactly the user types enumerated in MASTER-PLAN §24; the mental model field paraphrases no other doc. |
| Minimum running example | A copy-pasteable Rust snippet (or grammar fragment for grammar-author audiences) that compiles against the V1 `bbnf` crate facade and exercises the surface the page documents. The example anchors against a fixture under `crates/test-fixtures/corpus/`; no inline fixtures. |
| Diagnostic codes table | Every diagnostic code the documented surface can emit, listed verbatim from the §7.5 catalogue; no ad-hoc rewording. The table cross-references the producer site so the consumer can inspect the verbatim string. |
| Close-gate command | The `cargo xtask` invocation (or `cargo test -p <crate>` invocation) that proves the example still compiles and emits the documented diagnostics on cookbook regen. Pages without a close-gate command fail J.W2. |

The contract is consumed (not authored) by every page under
`docs/cookbook/`; J.W2's regen gate verifies field presence by
template-matching the page front-matter. The contract retires only by
amendment to this section.

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
6. Rewrite-mode out; Unicode class algebra below BBNF in `parse-that-regex`.
7. Generic host and runtime systems; no per-grammar declaration crates by
   default.
8. SOTA, generated LOC, tree-shape, and future-grammar tests as hard gates.

This is the architecture consumed by `restart/MIGRATION.md` and
`restart/MASTER-PLAN.md`.
