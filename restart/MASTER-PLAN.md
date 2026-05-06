# Restart Master Plan

This document is the Phase 2 master plan for the greenfield restart. It
synthesizes the architecture and migration contracts into a tranche sequence.
It keeps the tranche set at stub level; full per-tranche drafting is out of
scope for this phase.

## 0. Executive Summary

The restart is a greenfield workspace with inherited evidence. The README names
the new anchor and explicitly routes planning through the post-interrogation
answers, locks, precepts, and synthesis pass outputs (`restart/README.md:3`).
The legacy BA-BD tranche set remains a source of inheritance, but the
inheritance index says the new plan is A-J (`restart/inheritance/INDEX.md:29-40`).

The central product move is grammar-derived infrastructure. A grammar is added
by a `.bbnf` file plus `[workspace.metadata.bbnf.grammars.<name>]`; no Rust
crate, match arm, parser registry, or per-grammar declaration crate is added by
default (`restart/README.md:11-25`). Current hardcoded parser and path
registries are migration targets, not patterns to preserve
(`restart/corpora/CENSUS.md:103-122`).

The central runtime move is tape unioned with direct-to-struct. Tape is the
substrate name and implementation family; ParseStream is not a replacement
term. Lock 1 sets tape plus direct-to-struct and rejects parallel substrates
and OpenFrame ladders (`restart/locks/14-LOCKS.md:34`). PASS-3 resolves stale
ParseStream mentions against the README and locks
(`restart/audit/pass-3-runtime/PASS-3.md:14-23`).

The central compiler move is two IRs plus side tables. Grammar IR is semantic;
Backend IR is executable and is the only lowerer input. README requires two
IRs (`restart/README.md:104-118`), and Lock 5 rejects emitters walking grammar
directly (`restart/locks/14-LOCKS.md:42`). PASS-2 supplies the final 23-variant
Backend IR contract (`restart/audit/pass-2-codegen/PASS-2.md:52-76`).

The central language move is a constrained BBNF extension set. Lookbehind,
`@host fn`, multi-function chaining, generics, `@error`, and `@layout` are in.
Rewrite-mode is out. Unicode class algebra is deferred to `parse-that/regex`,
not exposed as grammar-level syntax (`restart/README.md:121-182`,
`restart/audit/pass-1-substrate/PASS-1.md:84-121`).

## 1. Synthesis Verdict Ledger

| Concern | PASS verdict | Master-plan action |
|---|---|---|
| Tape/direct substrate | KEEP/REINVENT around tape plus direct views. | Tranche B owns runtime substrate and generated view shell. |
| ParseStream rename | DISCARD. | No tranche may introduce ParseStream as runtime term. |
| Grammar IR | REINVENT as semantic IR. | Tranche C owns Grammar IR and validation gates. |
| Backend IR | KEEP concept, REINVENT exact contract around 23 variants. | Tranche E owns BIR, VM, and lowerer boundary. |
| Optimized IR | DISCARD as third IR; keep side tables. | Tranche C/E own side-table producers and consumers. |
| CSP/egraph bridge | KEEP as bridged crates. | Tranche C owns bridge facts and extraction. |
| Cost model | KEEP. | Tranche C/H/J own scoring, SOTA profiles, and budgets. |
| Lookbehind | KEEP. | Tranche D owns parser/type/lowering gates. |
| Rewrite-mode | DISCARD. | Tranche D proves parser rejection. |
| Unicode class algebra | DEFER below BBNF. | Tranche D routes regex Unicode to `parse-that/regex`. |
| Host functions/chains/generics | KEEP. | Tranche D owns typing and runtime dispatch. |
| Error/layout directives | KEEP. | Tranche D/I own grammar facts, recovery, LSP. |
| Per-grammar declaration crates | DISCARD by default. | Tranche A/D define rare escape valve only. |
| Runtime template | REINVENT. | Tranche F emits `runtime/src/grammars/<name>`. |
| Path/visitor | KEEP concept, split implementation. | Tranche G owns `path`, `path-core`, visitor mutation on the V1 Rust line; `path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. |
| Analysis/LSP | REINVENT/consolidate. | Tranche I owns `bbnf-language-server`. |

PASS-1 gives the substrate/language verdicts (`restart/audit/pass-1-substrate/PASS-1.md:5-20`),
PASS-2 gives the codegen/runtime verdicts (`restart/audit/pass-2-codegen/PASS-2.md:5-17`),
and PASS-3 gives the user runtime and ecosystem verdicts
(`restart/audit/pass-3-runtime/PASS-3.md:27-38`).

## 2. Final Workspace

The final workspace is the 24-crate set specified in `restart/ARCHITECTURE.md`
§1-§5, and the current-crate disposition is in `restart/MIGRATION.md` §3-§15.
The README is the source of truth for the crate names and the internal prefix
rule (`restart/README.md:29-60`).

| Layer | Crates |
|---|---|
| User entrypoints | `bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`. |
| Compiler substrate | `error`, `source`, `grammar`, `ir`, `passes`, `pipeline`, `vm`. |
| Backend/runtime | `codegen`, `runtime`, `host`, `cost-model`. |
| Path | `path`, `path-core` (V1 Rust line); `path-ts` defers post-V1 per Lock 7/11 amendments and the V2 `TsBackend: Backend` per ARCH §7.5. |
| Sister crates | `egraph`, `egraph-derive`, `csp-solver`, `parse-that`, `simd-scan`. |
| Dev/test | `test-fixtures`. |

The old workspace crates map to this set according to `restart/MIGRATION.md`.
`ser` and `gorgeous` are archive-only before implementation starts, per Lock 12
(`restart/locks/14-LOCKS.md:56`).

## 3. IR And BBNF Contract Summary

`restart/ARCHITECTURE.md` §7 is the IR contract. `restart/ARCHITECTURE.md` §8
is the BBNF language contract. The master plan treats those sections as
inputs, not work to reopen inside tranche drafting.

| Area | Governing architecture section | Owning tranche | Verification |
|---|---|---|---|
| Grammar IR variants | `restart/ARCHITECTURE.md` §7.1 | C/D | Grammar-to-IR tests. |
| Backend IR variants | `restart/ARCHITECTURE.md` §7.2 | E/F/H | BIR validation and VM replay. |
| Side tables | `restart/ARCHITECTURE.md` §7.3 | C/E | Pass output tests. |
| Lookbehind | `restart/ARCHITECTURE.md` §8 | D/E/F | Bounded lookbehind parser/type/lower tests. |
| Host functions and chains | `restart/ARCHITECTURE.md` §8.3-§8.4 | D/F | Host type and runtime dispatch tests. |
| Generics | `restart/ARCHITECTURE.md` §8.2 | D | Generic rule type tests. |
| Error/layout directives | `restart/ARCHITECTURE.md` §8 | D/I | Recovery and layout facts. |
| Rewrite-mode rejection | `restart/ARCHITECTURE.md` §8 | D | Negative parser fixture. |
| Unicode regex routing | `restart/ARCHITECTURE.md` §8 | D/H | `parse-that/regex` tests. |

## 4. Hard Architectural Gates

| Gate | Command family | Owner tranche |
|---|---|---|
| Future grammar add | Add `yaml.bbnf` plus workspace metadata; generated runtime is derivative output. | A/G/F |
| No grammar-name dispatch | `rg` for parser type names, strategy tables, and grammar-name registries. | A through J |
| Backend IR only | Codegen tests and `rg` proving lowerers do not walk Grammar IR. | E/F/H |
| Tape/direct substrate | Runtime tests, no OpenFrame clone stack, no ParseStream runtime concept. | B/F/H |
| BBNF extension set | Parser accepts settled features and rejects rewrite-mode syntax. | D |
| Unicode routing | Regex Unicode lives under `parse-that/regex`, not Grammar IR set algebra. | D |
| Generated equality | Regenerate committed output and compare byte-for-byte. | F through J |
| Generated LOC budget | Enforce PASS-2 +2 percent budget. | F/H/J |
| Tree shape | 4-10 children and no handwritten file over 500 LOC. | A through J |
| SOTA | JSON/CSS/SIMD gates. | H/J |

Lock 13 defines the tree-shape and LOC ceiling (`restart/locks/14-LOCKS.md:58`).
Lock 8 defines SOTA competitor anchors (`restart/locks/14-LOCKS.md:48`).
PASS-2 defines generated LOC budget tracking (`restart/audit/pass-2-codegen/PASS-2.md` §6).

Exact SOTA close rows. Each row binds a competitor baseline, a bbnf target, and
the platform that produces both. The baseline numbers are the corpus anchors
in `restart/corpora/SOTA.md:50-89` and `restart/corpora/SOTA.md:130-136`.

| Row | Competitor baseline | bbnf target | Platform | Owner |
|---|---|---|---|---|
| `json/twitter` | sonic-rs 436us; simd-json 424us. | <= 380us. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W3, J.W1. |
| `json/citm` | sonic-rs 854us; simd-json 831us. | <= 750us. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W3, J.W1. |
| `json/canada` | sonic-rs 3.144ms; simd-json 3.226ms (`restart/corpora/SOTA.md:56`). | <= 2.8ms. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W3, J.W1. |
| `css/bootstrap` | lightning-css 4.16ms. | <= 3.0ms. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W4, J.W1. |
| `css/animate` | lightning-css 1.97ms. | <= 1.6ms. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W4, J.W1. |
| `simd/structural_scan` | simdjson On-Demand ~7 GB/s on x86 AVX2; ~5 GB/s on M-series NEON. | >= 5 GB/s on M-series, >= 7 GB/s on x86 AVX2; scalar parity hash matches. | M1 Pro macOS NEON and x86_64 AVX2 build host. | H.W1/H.W3, J.W1. |

The SOTA close gates measure the Rust line only at H.W1/H.W3/H.W4 per Lock 8
amendment (`restart/locks/14-LOCKS.md:48`). WASM SOTA measurements defer
post-V1 alongside the V2 `WasmBackend: Backend` impl per `restart/ARCHITECTURE.md`
§7.5; no measurement-pending WASM anchor lands in V1.

Benchmark reproducibility schema. Every benchmark row at H.W3, H.W4, and J.W1
must serialise the following metadata; rows missing any field fail the gate.

| Field | Source |
|---|---|
| CPU model and microarchitecture. | `sysctl -n machdep.cpu.brand_string` on macOS, `lscpu` on Linux. |
| OS name and kernel version. | `uname -a`. |
| Compiler flags and target. | `RUSTFLAGS`, `target-cpu`, `--release`, profile name. |
| Input hash. | `sha256sum` of the corpus file. |
| Competitor crate name and version. | `Cargo.lock` for sonic-rs/simd-json/lightning-css; record path. |
| bbnf commit. | `git rev-parse HEAD`. |
| Warmup policy. | sample count, warmup iterations, criterion configuration. |
| Sample policy. | confidence interval, outlier rejection, statistical method. |

## 5. Tranche Set

The tranche set is A-J. Each tranche has a consumer gate before close. The
precepts require wave boundaries by dependency and same-wave consumer tests
(`docs/precepts/instructions/LESSONS-LEARNED.md:1-34`), and orchestration
limits parallel work to disjoint paths and clear ownership
(`docs/precepts/instructions/ORCHESTRATION.md:1-46`).

| Tranche | Title | Stub waves | Primary close gate |
|---|---:|---:|---|
| A | Workspace Genesis | 5 | New crate graph builds, archive complete, metadata schema validates. |
| B | Runtime Substrate | 5 | Tape/direct `DocumentView` works for one generated grammar shell. |
| C | IR And Optimization Core | 6 | Grammar IR, side tables, CSP/egraph/cost bridge produce Backend IR facts. |
| D | BBNF Extension Surface | 6 | Settled extensions parse/typecheck; rewrite-mode rejected; function values + lambdas + match + tuple lower at D.W5. |
| E | Backend IR And VM | 5 | 23-variant Backend IR validates and VM replays all variants. |
| F | Rust Lowerer And Runtime Template | 6 | Rust lowerer emits equal generated runtime for seed grammars. |
| G | Path, Value, Visitor | 5 | `path!`, `select!`, visitor mutation, and future grammar gate pass. |
| H | Pratt, SIMD | 5 | Auto-detected Pratt/SIMD pass early SOTA gates on the Rust line. WASM defers post-V1 via `WasmBackend: Backend` per `restart/ARCHITECTURE.md` §7.5. |
| I | Recovery, Incremental, LSP | 5 | LSP incremental parse and recovery diagnostics work on seed grammars. |
| J | Parity, Docs, Publication Close | 6 | Cross-backend parity, SOTA, docs, and publication readiness pass. |

The counts are planning stubs. Full wave docs are not part of Phase 2.

### 5.1 Calendar And Carry Matrix

Calendar is dependency order, not a wall-clock promise. Dates belong to the
implementation dispatch after capacity is known.

| Tranche | Calendar slot | Carry FROM | Carry TO | Layer ownership |
|---|---:|---|---|---|
| A | 1 | README crate table, BA archive discipline, locks 12-14. | B, C, D, F, G, I, J. | Workspace, metadata, source/error/grammar skeleton. |
| B | 2 | Lock 1, PASS-3 runtime, restart perf sketch. | F, G, H, I. | Tape/direct runtime and document API. |
| C | 3 | PASS-1 IR/type/bridge, current `ir`, CSP/egraph crates. | D, E, F, H, I. | Grammar IR, side tables, optimization facts. |
| D | 4 | README extension decisions, PASS-1 BBNF spec. | E, F, I. | BBNF parser, typing, host, layout/error facts. |
| E | 5 | PASS-2 BIR, BC ABI inheritance. | F, H, I, J. | Backend IR, VM, lowerer boundary. |
| F | 6 | PASS-2 lowerers/templates, Lock 6. | G, H, I, J. | Rust lowerer and generated runtime. |
| G | 7 | README path API, Lock 7, PASS-3 visitor/path. | I, J. | Value, path, visitor, future grammar proof. |
| H | 8 | Lock 10, PASS-2 SIMD, SOTA corpus. | J. | Pratt, SIMD, early perf on Rust line. WASM lower-and-bench programme awaits V2 `WasmBackend: Backend` impl per ARCH §7.5. |
| I | 9 | README incremental, PASS-3 LSP/recovery, current analysis/lsp. | J. | Recovery, incremental parsing, LSP/debug. |
| J | 10 | BD parity/publication inheritance, all prior gates. | Close. | Parity, docs, publication, archive audit. |

### 5.2 Tranche Outputs

| Tranche | Required outputs | Forbidden output |
|---|---|---|
| A | New workspace, metadata validator, archive commits, lint gates. | Production code depending on old strategy registry. |
| B | Tape/direct runtime and facade parse API. | ParseStream runtime rename or parallel substrate. |
| C | Grammar IR and side-table producers. | Third optimized IR tree. |
| D | Settled extension parser/type support. | Rewrite-mode syntax or BBNF Unicode set algebra. |
| E | Backend IR, VM, lowerer trait. | Lowerer that imports Grammar IR as emitter input. |
| F | Rust generated runtime for seed grammars. | Proc-macro codegen facade or unbudgeted generated churn. |
| G | Path/value/visitor APIs and future grammar gate. | Hardcoded path registry by grammar name. |
| H | Auto Pratt/SIMD on the Rust line. | `@pratt` or `@simd` grammar directives; WASM V1 (defers post-V1 alongside V2 `WasmBackend: Backend`). |
| I | Recovery/incremental/LSP. | LSP-only parser semantics. |
| J | Parity, SOTA, docs, publish dry run. | New architecture decisions without routed amendment. |

### 5.3 YAML Grammar Trajectory: A->F->J

This trajectory is a receiving-gate proof for one grammar, not a special yaml
implementation path.

| Tranche | YAML state | Gate that closes the handoff |
|---|---|---|
| A | `grammars/yaml.bbnf` plus one `[workspace.metadata.bbnf.grammars.yaml]` block enters the workspace. | Metadata validator accepts the two surfaces and rejects Rust registry, path registry, host shim, fixture-only admission, and declaration-crate onboarding. |
| B | Tape/direct substrate can host a generated yaml root once F emits it. | Tape identity and direct-root tests stay grammar-neutral; no `OpenFrame` or runtime `ParseStream` concept enters the yaml route. |
| C | Grammar IR and side tables represent yaml rules, layout, recognizer facts, and recovery facts without grammar-name dispatch. | `cargo xtask bbnf bir yaml --check` emits a stable yaml BIR snapshot from grammar + metadata only. |
| D | The BBNF extension parser proves yaml uses only settled syntax: lookbehind, generics, block-bodied `@host fn`, chains, `@error(recover = ...)`, and `@layout`. | Negative fixtures reject rewrite syntax, grammar Unicode algebra, bodyless host forms, and standalone recovery directives. |
| E | Backend IR lowers yaml through the same BIR alphabet as seed grammars. | Import-deny and BIR-snapshot gates pass with yaml included in the smoke cohort. |
| F | Runtime generation emits yaml files, path schema, diagnostics, visitor metadata, host route, and budget sidecars. | `cargo xtask bbnf build yaml`; generated LOC reports `yaml <= 4,000`; generic crates show no handwritten yaml changes. |
| G | `path!` and `select!` validate against the generated yaml path schema. | `path-core` schema dump and Rust macro smoke pass for yaml without hardcoded grammar registries. |
| H | yaml exercises Pratt and SIMD recognizer facts on the Rust line; auto-detection runs grammar-neutral. | Pratt/SIMD coverage report includes yaml when its grammar shape activates a recognizer. WASM lowering of yaml host primitives defers post-V1 alongside the V2 `WasmBackend: Backend` impl per ARCH §7.5; the WASM ABI matrix lands in V2, not V1. |
| I | Recovery and LSP consume the same yaml diagnostics as batch parse. | `DocumentSnapshot` and `ReparsePlan` tests show yaml fallback accounting and CLI/LSP diagnostic parity. |
| J | Public docs and publication dry runs include yaml as future-grammar proof, not a seed-grammar budget member. | J.W2 docs example runs; J.W3 publish dry-run keeps stable crates public and incubation-failing sister crates as path-deps; J.W5 close report records the two-surface proof. |

## 6. Tranche A - Workspace Genesis

Goal: create the greenfield crate graph and remove production ties to stale
workspace shape.

Inheritance:

| Source | Use |
|---|---|
| BA archive ceremony and preflight gates. | Archive `ser`/`gorgeous`; establish close checklist. |
| README crate table. | Create the 24-crate workspace (`restart/README.md:29-60`). |
| MODULES archive calls. | Move archive-only crates out of production (`restart/corpora/MODULES.md:165-212`). |
| Lock 14. | Enforce grammar generalization (`restart/locks/14-LOCKS.md:60`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| A.W0 | Branch/tag preflight, archive `ser` and `gorgeous`, remove them from workspace. | `cargo metadata` succeeds without archive crates; `git rev-parse pre-restart-2026-05-04` resolves; greenfield branch exists. |
| A.W1 | Create the V1 crate skeletons with Lock 13 tree shape; bind crate package names per Architecture §1 (unprefixed internal crates: `path`, `path-core`, `test-fixtures`, `passes`, `simd-scan`, `egraph`, `csp-solver`, `parse-that-regex`; user-facing crates retain `bbnf-` prefix). `path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. | `cargo check --workspace` reaches expected stubs; `rg "bbnf-path\|bbnf-test-fixtures"` returns zero hits in workspace member declarations; `path-ts` does not appear as a V1 workspace member. |
| A.W2 | Replace root metadata schema. | Metadata validation accepts current nine grammars. |
| A.W3 | Add source/error/grammar minimal APIs. | `grammar` can parse a seed `.bbnf` into AST. |
| A.W4 | Add generalization and tree-shape lint gates; consume the lint manifest contract at `restart/ARCHITECTURE.md` §13.1 (lint name, pattern set, allowlist syntax, exit semantics, owning tranche — landed Phase 7.1). | No hardcoded grammar dispatch in generic crates; `cargo xtask lint-grammar-generalization` consumes the contract, never authors it. |

Hard close:

```sh
cargo metadata --no-deps
cargo check --workspace
cargo xtask lint-tree
cargo xtask lint-grammar-generalization
```

No tranche after A may rely on the old root strategy table.

## 7. Tranche B - Runtime Substrate

Goal: implement tape plus direct-to-struct as one runtime substrate family.

Inheritance:

| Source | Use |
|---|---|
| Lock 1. | Tape/direct substrate and no OpenFrame ladders (`restart/locks/14-LOCKS.md:34`). |
| PASS-3 runtime architecture. | `DocumentView`, `parse`, `parse_in`, `parse_owned`, tape/direct model (`restart/audit/pass-3-runtime/PASS-3.md:42-135`). |
| Restart sketch perf evidence. | Remove OpenFrame clone pressure (`restart/corpora/RESTART-SKETCH.md:154-184`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| B.W0 | `runtime/src/tape` tokens, spans, append builder. | Tape builder unit tests. |
| B.W1 | Bounded checkpoints and rollback. | Speculative branch test without OpenFrame clone. |
| B.W2 | `DocumentView`, `OwnedDocument`, `NodeView`, `TokenView`. | `bbnf` facade exposes parse API stubs. |
| B.W3 | Direct builder shell and tape identity hooks. | Direct view borrows spans from tape. |
| B.W4 | Seed generated grammar shell. | One grammar parses through tape/direct shell. |

Hard close:

```sh
cargo test -p runtime tape
cargo test -p bbnf parse_api
rg "OpenFrame|Vec<OpenFrame>|ParseStream" crates/runtime/src crates/codegen/src
```

Any `ParseStream` hit must be macro parser code using `syn`, not runtime
substrate language.

## 8. Tranche C - IR And Optimization Core

Goal: create Grammar IR, side tables, and the optimization bridge that feeds
Backend IR extraction.

Inheritance:

| Source | Use |
|---|---|
| README two-IR decision. | Grammar IR and Backend IR stay distinct (`restart/README.md:104-118`). |
| PASS-1 IR and type commitments. | Grammar IR variants, type inference, CSP/egraph bridge (`restart/audit/pass-1-substrate/PASS-1.md:24-42`). |
| Lock 4. | Bridge crates compose by output piping, not fused hypergraph (`restart/locks/14-LOCKS.md:40`). |
| Current `ir` corpus. | Mine useful facts and split large files (`restart/corpora/MODULES.md:264-505`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| C.W0 | Grammar IR enum, IDs, spans, validation. | Seed grammar lowers AST to Grammar IR. |
| C.W1 | HM principal-scheme core plus expected-type checking inside `passes::layout`, with first-order equality unification before bounded coercion and finite CSP choices, **DK13 algorithmic completeness for higher-rank polymorphism per Lock 4 amendment**, and **branch-local equality (GADT substrate) hidden behind `BBNF-LOCAL-EQUALITY-ANNOTATION`** until V2 surface admission; `LayoutFacts` is public, while `TypeFacts` and `TypeObligationLog` remain internal. | Host-free seed grammar proves principal schemes under DK13; type-obligation snapshot separates equality, expected, coercion, finite-choice, and instantiation stages; downstream passes read `LayoutFacts`, never `TypeFacts`; the GADT machinery composes through CSP without surface leakage. |
| C.W2 | ShapeFacts, value-shape mining, **schema-mining miner (telemetry-driven schema inference)** as a sibling miner to `ShapeFacts`, and **internal row polymorphism for record-narrowing collapse** (no surface; the type alphabet remains `Type ::= ... \| RecordType` per PASS-1 §6). | Direct-builder shell contract consumes ShapeFacts in a C fixture and records B integration gaps; the schema miner emits inferred record schemas without explicit annotations for the seed grammars; row polymorphism collapses convergent record narrowings without surface widening. |
| C.W3 | RecognizerFacts and Pratt/SIMD candidate mining. | Facts feed E-owned BIR snapshots, not placeholder hints. |
| C.W4 | CSP/egraph bridge tables: stable ID maps, monotone fact exchange, rewrite guard API, rewrite budget policy (rewrite-budget categories with node/iteration ceilings landed at `restart/ARCHITECTURE.md` §10.1 per Phase 7.1; the fail-closed posture and representative-stability protocol route to C.W4 implementation rather than authoring at architecture level), representative-stability test, **CHR-improvement layer for host overloads**, **`Backend` trait surface obligations cross-referenced from `restart/ARCHITECTURE.md` §7.5 so the V1 `RustBackend` and V2 `WasmBackend` / `TsBackend` consume identical bridge facts**, and bridge-justification records. | Egraph and CSP exchange facts through bridge API; extraction never reads a stale e-node representative; the rewrite-budget policy is consumed (not authored); CHR-improvement closes host-overload ambiguity at the bridge boundary; the bridge feeds the same alphabet to every active `Backend` impl. |
| C.W5 | `CostFacts`, `CostDecision` evidence, objective profiles, Pareto/frontier extraction, solver-backed composition skeleton, and bridge-justified legality. | Backend IR builder receives selected alternatives plus evidence for rejected and dominated candidates. |

Hard close:

```sh
cargo test -p ir grammar_ir
cargo test -p passes types shapes recognizers bridge
cargo test -p passes type_obligations principal_core bridge_representative_stability
cargo test -p cost-model facts frontier solve
```

No C work may introduce a third optimized IR tree.

## 9. Tranche D - BBNF Extension Surface

Goal: implement the settled BBNF surface and reject stale extension proposals.

Inheritance:

| Source | Use |
|---|---|
| README extension set. | Lookbehind, host functions, chains, generics, error/layout in; rewrite out; Unicode below BBNF (`restart/README.md:121-182`). |
| PASS-1 formal grammar. | Extension parsing and semantics (`restart/audit/pass-1-substrate/PASS-1.md:84-121`). |
| ffuzzy. | Mine chaining/transducer questions only where accepted; do not carry rewrite-mode as BBNF syntax. |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| D.W0 | Lookbehind parser, bounds checker, Grammar IR node. | Bounded positive/negative lookbehind tests. |
| D.W1 | Generic rules, annotations, scheme instantiation, finite monomorphisation-set evidence, generic-cycle diagnostics, and **the V1 type alphabet `Type ::= Ident GenericArgs? \| TupleType \| RecordType \| BorrowType \| FnType` per Lock 10 amendment** (function types `fn(T) -> U` first-class in the `Type` non-terminal). | Generic seed grammar typechecks; generated instance-set report is finite; function types unify under DK13 without higher-rank surface leakage. |
| D.W2 | Block-bodied `@host fn` definitions and host primitive registry. | Host call compiles without declaration crate. |
| D.W3 | Multi-function chaining type/runtime contract, including left-to-right expected-argument obligations, bounded coercion-site fixtures, and first-mismatch diagnostics. | Chain result feeds a later parser expression; a negative chain fixture fails at the first mismatching step with `BBNF-HOST002` or `BBNF-SUBSUMPTION-EDGE`. |
| D.W4 | `@error`, `@layout`, regex Unicode routing, rewrite rejection. | Rewrite syntax fails; regex Unicode stays in `parse-that-regex`. |
| D.W5 | Function values + lambda literals (`\|x\| body`) + closure capture by `&'i` reference + match expression + tuple expression/pattern lowering, including function-typed parameters in `@host fn` (the transducer apotheosis without `@directive`) and the closure environment frame at the BIR boundary. | Function-typed `@host fn` parameter compiles; closure environment lowers through the same BIR alphabet as block-bodied `@host fn`; match/tuple typecheck under DK13; the V1 BBNF surface admits `Type ::= ... \| FnType` per Lock 10 amendment without DK13 surface leakage. |

Hard close:

```sh
cargo test -p grammar extensions
cargo test -p passes host_generics lookbehind
cargo test -p passes type_obligations principal_core chain_expected_flow monomorphisation_set
cargo test -p parse-that regex_unicode
```

No declaration crate is introduced unless the rare escape-valve gate is used.

## 10. Tranche E - Backend IR And VM

Goal: build the executable Backend IR and replayable VM before production
lowering.

Inheritance:

| Source | Use |
|---|---|
| PASS-2 BIR table. | 23 variants (`restart/audit/pass-2-codegen/PASS-2.md:52-76`). |
| Lock 5. | Lowerers consume BIR, not Grammar IR (`restart/locks/14-LOCKS.md:42`). |
| BC backend ABI inheritance. | Parity and typed boundary discipline. |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| E.W0 | Backend IR enum, IDs, validation. | All variants construct and validate. |
| E.W1 | Grammar IR + side tables to BIR builder. | Seed grammar produces BIR. |
| E.W2 | VM interpreter for core control flow. | VM parses seed grammar through BIR. |
| E.W3 | VM support for tape/direct, host, path, recovery, debug marks. | VM replays all BIR variants. |
| E.W4 | Lowerer trait and boundary tests. | Codegen cannot import Grammar IR emitter logic. |

Hard close:

```sh
cargo test -p ir backend_ir
cargo test -p vm replay_all_backend_ir_variants
cargo test -p codegen backend_lowerer_boundary
```

## 11. Tranche F - Rust Lowerer And Runtime Template

Goal: emit committed Rust runtime source from Backend IR and prove regeneration
equality.

Inheritance:

| Source | Use |
|---|---|
| PASS-2 runtime template. | Output under `runtime/src/grammars/<name>` (`restart/audit/pass-2-codegen/PASS-2.md` §7). |
| PASS-2 lowerer contract. | `BackendLowerer` methods and Rust V1 scope (`restart/audit/pass-2-codegen/PASS-2.md:80-96`). |
| Lock 6. | Committed source generation, no proc-macro facade (`restart/locks/14-LOCKS.md:44`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| F.W0 | Rust lowerer skeleton for control flow and literals. | Generated seed grammar compiles. |
| F.W1 | Tape/direct emit and builder integration. | Runtime parse returns `DocumentView`. |
| F.W2 | Host calls/chains, layout, `@error(recover = ...)`. | Extension seed grammar compiles and runs. |
| F.W3 | Generated module template and headers. | Regenerated output is equal. |
| F.W4 | Generated LOC budget tooling. | Budget report under +2 percent. |
| F.W5 | Current nine grammar regeneration. | Nine seed grammars build through new template. |

Hard close:

```sh
cargo xtask bbnf build --all
git diff --exit-code crates/runtime/src/grammars
cargo xtask generated-loc-budget --max-growth 1.02
cargo test -p runtime generated_grammars
```

## 12. Tranche G - Path, Value, Visitor

Goal: deliver user-facing value navigation and mutation on top of tape/direct
documents.

Inheritance:

| Source | Use |
|---|---|
| README path API. | `path!`, `select!`, visitor mutation (`restart/README.md:272-318`). |
| Lock 7. | `path`, `path-core` split on the V1 Rust line (`restart/locks/14-LOCKS.md:46`); `path-ts` defers post-V1. |
| PASS-3 path and visitor commitments. | Typed path diagnostics and mutation API (`restart/audit/pass-3-runtime/PASS-3.md:82-95`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| G.W0 | `path-core` AST/parser/evaluator. | Runtime document query test passes. |
| G.W1 | Rust `path!` and `select!`. | Compile-time path diagnostics work. |
| G.W2 | `ValueRef`, `ValueOwned`, shape-backed projection. | Value API reads seed grammar. |
| G.W3 | Read-write visitor mutation. | Mutation updates document through visitor only. |
| G.W4 | Future grammar test on the V1 Rust line: yaml enters through grammar source plus metadata; generated runtime is derivative; `path-core` schema dump round-trips. The TS schema (`path-ts`) defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. | yaml enters through grammar source plus metadata; generated runtime is derivative; `path-core` schema round-trip succeeds. |

Hard close:

```sh
cargo test -p path-core
cargo test -p path
cargo test -p runtime visitor
cargo test -p test-fixtures future_grammar_yaml
```

## 13. Tranche H - Pratt, SIMD

Goal: activate performance recognizers on the Rust line without adding
syntax directives.

Inheritance:

| Source | Use |
|---|---|
| Lock 10. | Pratt and SIMD are auto-detected (`restart/locks/14-LOCKS.md:52`). |
| Lock 5. | V1 ships `RustBackend: Backend` only via `restart/ARCHITECTURE.md` §7.5; WASM defers post-V1 as `WasmBackend: Backend`. |
| Lock 8. | V1 SOTA close gates measure the Rust line only; WASM SOTA defers post-V1 (`restart/locks/14-LOCKS.md:48`). |
| PASS-2 detector and SIMD coverage. | Detection thresholds and scalar/NEON/AVX2/AVX512 coverage (`restart/audit/pass-2-codegen/PASS-2.md` §3). |
| SOTA corpus. | JSON/CSS competitor baselines (`restart/corpora/SOTA.md:50-89`, `restart/corpora/SOTA.md:130-136`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| H.W0 | Pratt recognizer facts and BIR `PrattSpine`. | Expression grammar uses auto-detected Pratt. |
| H.W1 | SIMD recognizer facts and `SimdScan` integration, with `Exact` structural scans, `Prefilter` candidate scans, scalar offset-vector parity, verifier-before-tape emission, and `parse-that-regex` internal cross-engine parity (NFA vs lazy DFA vs full DFA vs VM) for regex fixtures. | Literal and regex scans route through scanner kernels only when exactness and cost evidence win; missing verifier routes fall back to scalar or regex verifier-first. |
| H.W2 | AVX2/NEON/scalar dispatch gates. | Platform-specific tests pass on supporting hardware; tests on non-supporting hardware are skipped with a CI-readable skip-marker recording the missing capability (for example `cpu_feature: avx2_unsupported`). |
| H.W3 | Early JSON SOTA gates. | `json/twitter` <= 480us, `json/citm` <= 950us, `json/canada` <= 3.5ms on M1 Pro with metadata; final J.W1 thresholds at 380us / 750us / 2.8ms. |
| H.W4 | Early CSS SOTA gates. | `css/bootstrap` <= 3.8ms, `css/animate` <= 1.9ms on M1 Pro with metadata; final J.W1 thresholds at 3.0ms / 1.6ms. |

The wave count drops from six to five with the WASM defer. The prior H.W3
(WASM V1 via wasm32 Rust binding) and its WASM host-primitive ABI matrix
route to the V2 `WasmBackend: Backend` programme per ARCH §7.5 and §7.5
table; no measurement-pending WASM anchor lands in V1.

Hard close:

```sh
cargo test -p passes recognizers
cargo test -p simd-scan
cargo test -p simd-scan exact_prefilter_parity
cargo test -p parse-that-regex cross_engine_parity
cargo bench -p bbnf-bench --bench sota_json
cargo bench -p bbnf-bench --bench sota_css
```

## 14. Tranche I - Recovery, Incremental, LSP

Goal: ship diagnostics and editor behavior over the same pipeline and runtime
contracts.

Inheritance:

| Source | Use |
|---|---|
| README incremental rule. | Batch incremental is opt-in; LSP is always-on (`restart/README.md:344-348`). |
| PASS-3 recovery/LSP contract. | `DocumentSnapshot`, `ReparsePlan`, diagnostics (`restart/audit/pass-3-runtime/PASS-3.md:137-158`). |
| Current `analysis` and `lsp`. | Mine diagnostics and protocol behavior. |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| I.W0 | RecoveryFacts and diagnostic codes. | Error directive fixtures produce stable diagnostics. |
| I.W1 | Incremental source snapshots, snapshot-scoped `TapeId`, old-to-new reuse maps, query invalidation keys, and reparse plans. | Edited seed grammar reparses changed region or reports a named fallback reason, reuse-map absence, invalidated queries, and silent LSP behavior in the `incremental/edit_anchor` ledger. |
| I.W2 | LSP diagnostics and semantic index. | Editor fixture sees grammar/type errors. |
| I.W3 | Debug/replay and playground hooks. | VM trace displayed through server/debug API. |
| I.W4 | CLI and LSP parity for diagnostics. | Same input yields same diagnostic codes. |

Hard close:

```sh
cargo test -p error recovery
cargo test -p bbnf-language-server incremental diagnostics
cargo test -p vm debug_replay
```

## 15. Tranche J - Parity, Docs, Publication Close

Goal: close the restart with parity, SOTA, documentation, package readiness,
and archive discipline.

Inheritance:

| Source | Use |
|---|---|
| BD parity and fixture discipline. | Cross-backend parity and fixture package. |
| README close posture. | Tranche synthesis and documentation ready state (`restart/README.md:400-446`). |
| Style precepts. | Concrete docs without filler (`docs/precepts/instructions/STYLE.md:1-55`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| J.W0 | Cross-backend parity matrix for Rust, VM, WASM V1. | Parity matrix passes for seed grammars. |
| J.W1 | Final SOTA gate and benchmark report. | JSON/CSS/SIMD targets met; misses require amendment before close. |
| J.W2 | Public docs redo. | Docs build and examples run. |
| J.W3 | Package readiness for public crates: confirm publication-name plan, validate `[workspace.package]` defaults, dry-run `cargo publish` for every public crate, and verify path-dep incubation does not leak to `crates.io`. Two gates apply per Lock 11 (`restart/locks/14-LOCKS.md:54`): (i) the **stable surface** (`bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `parse-that-regex`) publishes unconditionally at J.W3; (ii) the **incubation-cleared sister crates** (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`) publish at J.W3 only after the 2-tranche stability gate clears — API has not changed across two consecutive prior tranche closes, downstream consumers compile against a frozen-version dry-run, and no breaking change is queued. Crates that fail the stability gate carry their dry-run results in the J.W3 report and remain path-deps until the next J cycle. `path-ts` defers post-V1 alongside the principled TS-native parse+runtime fork; the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5 publishes `path-ts` in V2. | `cargo xtask publish --dry-run` passes for the stable surface (`bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `parse-that-regex`) plus every incubation-cleared sister crate (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`); incubation-failing sister crates remain path-deps with the failure recorded; `path-ts` is not in V1 publish scope; private crates are unpublished. |
| J.W4 | Archive and migration audit. | No stale crates/docs in production workspace. |
| J.W5 | Restart close report. | All locks, gates, and routed punch-list items recorded. |

Hard close:

```sh
cargo test --workspace
cargo bench -p bbnf-bench --bench sota_json
cargo bench -p bbnf-bench --bench sota_css
cargo xtask parity --all
cargo xtask docs --check
cargo xtask publish --dry-run
```

## 16. Workspace And Cargo Schema Handoff

`restart/ARCHITECTURE.md` §5 owns the full workspace and metadata schema. The
implementation tranches consume it as follows:

| Schema area | Owner tranche | Downstream consumer |
|---|---|---|
| `[workspace].members` 24-crate set | A | All tranches. |
| `[workspace.package]` publication defaults | A/J | Publish dry run. |
| `[workspace.dependencies]` path dependencies | A | Crate graph checks. |
| `[workspace.metadata.bbnf]` roots/profile | A/F/J | Pipeline and generated output. |
| `[workspace.metadata.bbnf.recognizers]` | A/C/H | Recognizer mining and cost model. |
| `[workspace.metadata.bbnf.host_fns]` | A/D/F | Host registry typing/runtime. |
| `[workspace.metadata.bbnf.grammars.<name>]` | A/F/G/J | Future grammar and generated runtime. |
| Runtime mode/options | B/F | Tape/direct generation. |
| Optimization profile | C/H/J | Cost and SOTA gates. |
| Codegen targets | E/F/H/J | Rust/WASM output. |
| Fixture roots | A/G/J | Test fixture and parity matrix. |

No tranche may reintroduce the current `bbnf-strategy` table or a production
manifest table. `restart/MIGRATION.md` §4 and §19 define the deletion and grep
gates.

## 17. Commit Chain Disposition

Implementation uses the greenfield commit chain described in
`restart/MIGRATION.md` §15 and §18. The key mechanics are:

| Step | Tranche | Required commit shape |
|---|---|---|
| Tag pre-restart state | A.W0 | History marker, no source edits. |
| Create greenfield branch | A.W0 | Branch marker, no source edits. |
| Archive `ser`/`gorgeous` | A.W0 | Body-bearing archive commit. |
| Create crate skeleton | A.W1 | Body-bearing workspace commit with metadata evidence. |
| Move kept code | A-J as needed | Narrow `git mv` commits where possible. |
| Replace conflicting code | A-J as needed | Body names the retired architecture and new gate. |
| Land generated runtime | F | Body includes regen equality and LOC budget evidence. |
| Close SOTA/parity | J | Body includes benchmark metadata and routed remainder. |

The commit chain is not an excuse to squash unrelated work. The local commit
discipline requires inspecting staged and dirty state, staging only the
intended slice, and using bodies for broad/history-relevant work.

## 18. Migration Timeline

`restart/MIGRATION.md` §17 is the file-movement sequence. This master timeline
binds migration state to tranche exits.

| Exit | Migration state |
|---|---|
| A close | Archive-only crates out of workspace; metadata schema active; skeletons build. |
| B close | Tape/direct runtime exists and old OpenFrame/ParseStream runtime concepts are blocked. |
| C close | Current `ir` useful facts are mined into new IR/pass/cost boundaries. |
| D close | Settled BBNF extensions replace stale extension proposals. |
| E close | Backend IR and VM exist; old grammar-walking backend cannot be used. |
| F close | Current generated grammar/runtime layout is replaced by template output. |
| G close | Current path crate duplication and hardcoded registries are gone. |
| H close | Recognizer/SIMD paths are real consumers of C/E/F work on the Rust line; WASM deferred post-V1. |
| I close | `analysis` and `lsp` are consolidated. |
| J close | Migration audit proves no stale production paths remain. |

## 19. Archive Disposition

Archive work has two classes: production artifacts removed from the workspace,
and legacy planning material retained as inheritance.

| Class | Action | Owner |
|---|---|---|
| Production crates `ser`, `gorgeous` | Move out of workspace before A close. | A.W0. |
| Legacy tranche docs BA-BD and earlier archive material | Keep as inheritance/reference, not active plan. | A/J audit. |
| Current `restart/` prompt/locks/corpora/inheritance inputs | Read-only; never modified by implementation tranches. | All tranches. |
| Generated old runtime output | Replaced by F-generated runtime and archived through Git history. | F/J. |

The user constrained this synthesis to avoid editing restart inputs, crates,
docs, or archive directories outside the three output files.

## 20. Generated LOC Trajectory

Generated source is a tracked product, not incidental output.

| Stage | Generated code state | Budget action |
|---|---|---|
| Before F | Old generated code exists only as migration input. | Record baseline. |
| F.W0-F.W2 | New lowerer emits partial seed grammar output. | Budget report can be advisory; xtask wall under 30s on M1 baseline machine. |
| F.W3 | Template headers and equality land. | Budget gate becomes required; per-grammar runtime LOC must be in PASS-2 baseline +/- 2 percent; xtask wall under 60s. |
| F.W4 | Generated LOC budget tooling. | `cargo xtask generated-loc-budget` returns under +2 percent across the seed set; wall under 60s. |
| F.W5 | Current nine seed grammars regenerate. | +2 percent ceiling enforced for every grammar; wall under 90s. |
| H.W3 | Early JSON SOTA gates run with SIMD-target output. | SIMD-attributed LOC reported by target (AVX2/NEON/scalar); JSON SOTA delta numeric; wall under 120s. |
| H.W4 | Early CSS SOTA gates. | CSS visitor/path additions reported separately under PASS-3 generated visitor LOC budget; wall under 120s. |
| J | Final release artifacts regenerate from clean checkout. | Budget and equality are release gates; wall under 180s end-to-end including parity matrix. |

PASS-2 is the authority for the +2 percent generated LOC ceiling
(`restart/audit/pass-2-codegen/PASS-2.md` §6).

Per-grammar generated LOC baseline. The firm per-grammar baselines live in
`restart/ARCHITECTURE.md` §12.2 (`Generated LOC (current → max)` column);
this table mirrors them so all "nine seed grammars" claims close without
chasing PASS-2 or ARCHITECTURE. A.W2 verifies the firm numerics against the
live W3 branch and reports drift; the numbers themselves are recorded, not
deferred.

| Grammar | Current baseline LOC (ARCHITECTURE §12.2) | F.W5 ceiling | Tranche owner |
|---|---:|---|---|
| `bbnf` | 21,503. | F.W5 baseline +2 percent (21,933). | F. |
| `bnf` | 3,290. | F.W5 baseline +2 percent (3,356). | F. |
| `csv` | 1,693. | F.W5 baseline +2 percent (1,727). | F. |
| `css_l4` | 107,138. | F.W5 baseline +2 percent (109,281). | F. |
| `css_pretty` | 9,021. | F.W5 baseline +2 percent (9,201). | F. |
| `ebnf` | 7,646. | F.W5 baseline +2 percent (7,799). | F. |
| `google_sheets` | 14,088. | F.W5 baseline +2 percent (14,370). | F. |
| `json` | 3,500. | F.W5 baseline +2 percent (3,570); SIMD additions at H attribute by target. | F/H. |
| `math` | 871. | F.W5 baseline +2 percent (888). | F. |
| `yaml` (probe) | 0 (not in seed budget; reported separately under future-grammar metadata until admitted). | provisional ceiling ≤ 4,000 (SYNTHESIS Wave-2 owner); yaml never closes a tranche on a seed-grammar gate before admission. | A/G/F. |

Budget enforcement rows:

| Scope | Gate |
|---|---|
| F lowerer/runtime template | per-grammar generated runtime budget plus equality check. |
| H SIMD/WASM additions | target-attributed generated LOC with benchmark justification. |
| J release close | clean-checkout regeneration, total <= PASS-2 +2 percent unless amended. |
| Handwritten support | Lock 13: no file over 500 LOC and 4-10 children per source directory. |

## 21. Lock Ownership

| Lock | Owner tranche | Close proof |
|---|---|---|
| 1 Tape/direct substrate | B/F/H | Runtime identity tests, payload projection tests, no OpenFrame clone stack. |
| 2 Layout lowering term | D/F | Layout facts lower through BIR. |
| 3 Cursor-parse/byte-skip | B/H | Empty-path elision and scanner tests. |
| 4 CSP/egraph bridge | C | Bridge tests, no fused hypergraph, representative-stability test, rewrite-budget test, bridge-justification round-trip. |
| 5 Backend IR lowerers | E/F/H | Codegen BIR-only tests. |
| 6 Committed codegen | F/J | Regeneration equality. |
| 7 Path split | G | `path`, `path-core` tests on V1 Rust line; `path-ts` defers post-V1 per Lock 7/11 amendments. |
| 8 SOTA gates | H/J | Bench report with baselines, validation mode, source ownership mode, materialisation mode, objective profile, and complete host hardware metadata. |
| 9 Slice-borrow API | B/G | `parse`, `parse_in`, `parse_owned` tests. |
| 10 Pratt/SIMD auto | C/H | Recognizer facts, no directives, scalar parity for exact scans, verifier route for prefilters. |
| 11 Path-dep incubation | A/J | Sister crates remain generic and publishable. |
| 12 Archive `ser`/`gorgeous` | A/J | Archive paths outside workspace. |
| 13 Tree discipline | A through J | `lint-tree`, `lint-loc`. |
| 14 Grammar generalization | A/D/F/G/J | Future grammar test and no hardcoded dispatch. |

Lock 13 verification rows. Each row is the executable evidence the named owner
must produce when closing its tranche; ARCHITECTURE.md §13 owns the exception
ledger.

| Surface | Child-count gate | LOC gate | Exception rationale | Enforcing command |
|---|---|---|---|---|
| Handwritten crate `src/` directories | 4-10 immediate children. | No handwritten file over 500 LOC. | Migration may temporarily raise the child count with a tranche-local rationale; lint allowlist names the exception. | `cargo xtask lint-tree` and `cargo xtask lint-loc --handwritten-max 500`. |
| Generated grammar/runtime files (`runtime/src/grammars/<name>/generated.rs`). | Not bound by 4-10. | LOC excepted; budget rows in §20 govern. | Lock 6 requires committed generation; LOC ceiling is the +2 percent budget. | `cargo xtask generated-loc-budget --max-growth 1.02`. |
| Generated data tables (Pratt, SIMD, scanner). | Not bound. | Excepted with header metadata. | Data products carry source metadata and stable headers, not LOC ceilings. | `cargo xtask lint-generated-headers`. |
| Proc-macro roots (`*-derive`, `path/src/macro_impl`). | 4-10. | 500 LOC handwritten ceiling. | None; proc-macro roots follow the handwritten rule. | `cargo xtask lint-tree` and `cargo xtask lint-loc`. |
| SIMD intrinsic files (`simd-scan/src/avx2.rs`, `neon.rs`). | 4-10 in parent dir. | 500 LOC ceiling unless the file is single-target intrinsics. | Single-target intrinsic files carry a tranche-local LOC exception. | `cargo xtask lint-loc --simd-exception`. |

The lock table in README marks all 14 locks as restart inputs
(`restart/README.md:377-397`).

## 22. Documentation Plan

Documentation is rebuilt after architecture stabilizes, not patched around the
old tree.

| Area | Owner tranche | Output |
|---|---|---|
| Language spec | D/J | BBNF syntax, type system, host functions, layout/error directives. |
| Compiler architecture | C/E/F/J | IRs, side tables, pipeline, lowerers, generation. |
| Runtime/user guide | B/G/I/J | Parse APIs, document views, values, visitors, paths, diagnostics. |
| Performance guide | H/J | Recognizers, SIMD, Pratt, SOTA methodology. |
| Contributor guide | A/J | Workspace shape, tree discipline, future grammar process, commit discipline. |
| Migration notes | J | What was archived, replaced, retained, and why. |

Docs must cite concrete source facts with path:line references; README requires
that discipline (`restart/README.md:452`). Style follows the local precepts
(`docs/precepts/instructions/STYLE.md:1-55`).

## 23. Risk Register

| Risk | Mitigation |
|---|---|
| Old grammar registries reappear in new crates. | Lock 14 lint from A onward; future grammar test in G; `cargo xtask lint-no-hardcoded-grammars` enforced as a close gate at A.W4, G.W4, and J.W4 with `rg "PRODUCTION_MANIFEST_TABLE\|GrammarAuditTag\|bbnf-strategy"` returning zero outside generated data and corpus citations. |
| Direct-to-struct bypasses tape. | B/F tests prove direct views share spans/events with tape. |
| Backend lowerer imports Grammar IR for convenience. | E/F boundary tests and crate dependency checks. |
| `@host fn` becomes a hidden declaration-crate requirement. | D host tests prove generic primitives and metadata path first. |
| Rewrite-mode returns through ffuzzy inheritance. | D parser rejection test and no IR variant. |
| Unicode class algebra leaks into BBNF. | D routes Unicode to `parse-that/regex`; Grammar IR keeps regex opaque. |
| Generated LOC grows without review. | F/J budget report with PASS-2 +2 percent ceiling. |
| LSP incremental parser diverges from batch parser. | I CLI/LSP diagnostic parity tests. |
| SOTA gates are measured on unclear hardware. | H/J benchmark metadata records CPU, OS, build flags, input hashes. |
| Legacy archive becomes active code again. | A/J workspace membership checks. |

## 24. Carry And Friction Ledger

This is the single carry-truth ledger. Synthesis-sourced rows and
migration-implementation-sourced rows live here together; the `Source` column
distinguishes them. `restart/MIGRATION.md` §20 cross-references this ledger
rather than duplicating receivers.

| Item | Receiver | Blocker | Gate | Source |
|---|---|---|---|---|
| Declaration-crate escape valve | A/D | Review form missing reason, scope, owner, or deletion path. | Metadata validator rejects `allow_declaration_crate = true` without the eight-field review form (template at `restart/ARCHITECTURE.md` §5.6 lines 738-770 — landed Phase 7.1). A.W4 consumes the template; D consumers reference it when the rare escape valve fires. | synthesis + migration |
| Layout lowering | D/F | `@layout` remains parser metadata and does not lower through `LayoutFacts` and BIR. | LayoutFacts test plus BIR `LayoutPush`/`LayoutPop` replay. | synthesis |
| Cursor skip | B/H | Runtime cannot prove empty-path and byte-skip behavior. | `__EAGER_EMPTY_PATH` and `CursorDecision::Skip` fixtures. | synthesis + migration |
| PASS-3 consumers | F/G/I | Generated runtime omits path, visitor, diagnostics, or host metadata. | `path-core`, visitor, and language-server consumer smokes. | synthesis |
| SOTA metadata | H/J | Bench numbers lack machine/input/build metadata. | Benchmark report schema rejects incomplete rows; benchmark host hardware profiles cite SOTA baselines and record machine metadata. | synthesis + migration |
| Cost evidence | C/F/H/J | Selected-only cost evidence loses rejected, dominated, profile, target, or objective-mode provenance. | `cost-model` evidence report lists selected, rejected, dominated, objective mode, target, profile, and extraction method. | synthesis |
| Regex cross-engine parity | D/H/J | `parse-that-regex` engines (NFA, lazy DFA, full DFA, VM) drift without grammar-owned delta or parity evidence. | `parse-that-regex` fixtures compare grammar HIR/verifier integration across the internal engine matrix for Unicode class algebra, no-capture DFA, lazy/full DFA, and prefilter candidates. The parity is internal to the crate; no third-party oracle is cited. | synthesis |
| Runtime materialisation metadata | B/F/J | Direct/tape materialisation rows hide payload, string-normalisation, scalar-cache, or repeated-access cost. | Generated materialisation report includes token width, payload class, string-normalization policy, numeric policy, direct-field count, repeated-access class, and source ownership mode per node kind. | synthesis |
| yaml onboarding | A/F/G/J | Future grammar requires any manual Rust registry/path/host edit. | yaml source + workspace metadata plus generated runtime only. | synthesis |
| Archive closure | A/J | `ser` or `gorgeous` remains in production workspace; archive destination must be outside production workspace. | Workspace membership check and migration audit; `archive/<crate>/` placement verified. | synthesis + migration |
| TS production | V2 amendment | TS lowering defers post-V1; the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5 owns the `path-ts` schema, the LSP TS bridge, and the typed TS path/value/visitor surface. V1 carries no TS production row. | V2 amendment opens when the V2 `TsBackend: Backend` lands; V1 J.W3 publishes the Rust line only. | synthesis |
| BD parity | F/J | BD-equivalent parity matrix not run for Rust/VM backends on the V1 line. | `cargo xtask parity --all` matrix passes for nine seed grammars on Rust/VM. WASM/TS parity defers post-V1 alongside V2 `WasmBackend: Backend` + `TsBackend: Backend` per ARCH §7.5. | synthesis |
| PASS-1 reconciliation | C/D | Grammar IR, side tables, or BBNF surface drift between PASS-1 and synthesis. | Architecture §7 schema matches PASS-1 §2 enum, and Architecture §8.1 matches PASS-1 §6 on block-bodied `@host fn`, infix lookbehind, and rule-level `->` chains; reconciliation noted in close report. | synthesis |
| PASS-3 API docs | G/I/J | Public docs for `path!`, `select!`, visitor, language-server omit committed string diagnostics. | PASS-3 carry ledger column closes; cookbook pages list every diagnostic code. | synthesis |
| Publication readiness | A/J | Crate package names, README, license, dependencies fail dry-run publish. | `cargo xtask publish --dry-run` clean for every public crate. | synthesis |
| Fixture handoff | A/G/J | `test-fixtures` lacks parity fixtures or duplicate fixtures live in old crates. | Fixture audit at J.W4; per-grammar fixture manifest column in §12.2 table. | synthesis |
| `path-ts` schema | V2 amendment | `path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. V1 carries no `path-ts` schema row; V1 ships `path` + `path-core` (Rust) at J.W3. | V2 amendment opens with `TsBackend: Backend`; the V2 schema must round-trip identical `path-core` AST as `path` does. | synthesis |
| WASM ABI | V2 amendment | WASM defers post-V1 alongside the V2 `WasmBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. V1 carries no WASM ABI row; the WASM lower-and-bench programme awaits V2. | V2 amendment opens with `WasmBackend: Backend`; the V2 record carries exported function names, host-call shape, marshalling rule, primitive coverage, and scalar/SIMD parity. | synthesis + migration |
| Generated header fields | F | Generated header omits grammar, metadata, or Backend IR hashes. | `cargo xtask lint-generated-headers` rejects missing fields; F.W3 template gate. | migration |
| `path-ts` package publication timing | V2 amendment | `path-ts` defers post-V1; V1 J.W3 publishes Rust-line only per Lock 11 amendment. | V2 amendment owns `path-ts` publication when the V2 `TsBackend: Backend` impl per ARCH §7.5 lands; V2 J.W3 dry-run records `path-ts` only after V2 parity matrix passes. | migration |
| PASS-2 BIR snapshots | E/F | BIR snapshots live outside `ir::backend_ir` or fail to feed codegen import-deny tests. | Snapshots committed under `ir::backend_ir`; `BBNF-GEN001` import-deny gate consumes them at every codegen close. | migration |

Cookbook and migration friction rows. Every row binds a target user, a mental
model, a confusion point, the artefact that resolves it, and the diagnostic the
user sees when they get it wrong. Each cookbook page consumes the page contract
template (audience + mental model, minimum running example, diagnostic codes
table, close-gate command) anchored at `restart/ARCHITECTURE.md` §13.2
so J.W2 produces uniform pages, not seven varieties.

| Friction | Target user | Mental model | Confusion point | Artefact | Diagnostic |
|---|---|---|---|---|---|
| `path!` and `select!` | Library consumer building queries against generated documents. | A path expression is checked against the grammar's path schema at compile time; canonical Rust spelling uses an explicit grammar prefix such as `path!(Json => "/...")` and `select!(Json => "...")`; the bracket form `path!(Json, ["a", "b", 0])` is equivalent. The macros land at G.W1 per Phase 7.1 grammar amendments (PASS-1 §6, ARCH §8). | "Why does my path not compile when the JSON looks fine?" | Cookbook page `cookbook/path-dsl.md` plus `path-core` schema dump. | `BBNF-PATH-UNKNOWN-SEGMENT` and `BBNF-PATH-GRAMMAR-MISMATCH`. |
| Lifetime constructors | Library consumer choosing between `parse`, `parse_in`, `parse_owned`. | `parse` borrows; `parse_in` borrows into a caller arena; `parse_owned` allocates a self-contained document. | "Why does my borrow live longer than the source?" | Cookbook page `cookbook/parse-lifetimes.md` plus `runtime` API doc. | `BBNF-LIFETIME-ESCAPE` and `BBNF-ARENA-MISMATCH`. |
| Visitor mutation | Library consumer mutating documents through the visitor. | Mutation goes through the read-write visitor only; direct field writes are forbidden. | "Why does the borrow checker reject my edit?" | Cookbook page `cookbook/visitor-mutation.md` plus PASS-3 visitor contract. | `BBNF-VISITOR-MUTATION-OUTSIDE-ENTRY`. |
| Layout errors | Grammar author writing layout-bearing rules. | `@layout` lowers through `LayoutFacts` and BIR `LayoutPush`/`LayoutPop`; conflicts are typed errors. | "Why does the layout not nest the way I expected?" | Cookbook page `cookbook/layout.md`. | `BBNF-LAYOUT-CONFLICT` and `BBNF-LAYOUT-UNCLOSED`. |
| Pratt/SIMD decisions | Grammar author wondering why a recognizer was or was not applied. | Pratt and SIMD are auto-detected from grammar shape; metadata can disable but not force. | "Why did Pratt not apply to my expression rule?" | Cookbook page `cookbook/recognizers.md` plus `cargo xtask explain-recognizer`. | `BBNF-PRATT-NOT-APPLIED` and `BBNF-SIMD-NOT-SELECTED`. |
| Crate split migration | Migrating from old workspace shape. | Old `bbnf-path*` and `core` are split into unprefixed crates. | "Where did `bbnf-path` go?" | Cookbook page `cookbook/migration-crate-split.md` plus MIGRATION.md §3.1. | None; this is documentation friction, not a runtime diagnostic. |
| Adding yaml | Grammar author adding a new grammar. | Two surfaces only: `grammars/yaml.bbnf` plus `[workspace.metadata.bbnf.grammars.yaml]`; generated runtime/path/visitor/host outputs and the bench manifest are derivatives. | "Where do I register yaml in Rust?" | Cookbook page `cookbook/add-grammar.md` plus Architecture §12.1 walkthrough and future-grammar test. | `BBNF-METADATA-MISSING-GRAMMAR` and `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`. |
| yaml syntax error | Grammar author editing a new yaml grammar under LSP. | The grammar remains admitted while a sample edit is malformed; typed recovery is carried by `DocumentSnapshot`, `TapeId` reuse maps, and recovery facts over the same tape/direct identity. | "Why did the LSP keep a typed `YamlRoot` when indentation is broken?" | Recovery cookbook plus `DocumentSnapshot` trace and `incremental/edit_anchor` ledger. | `BBNF-RECOVERY001` plus debug-only fallback reason when anchors fail. |
| `format()` public method | Library consumer pretty-printing a parsed document. | Every generated runtime exposes `format()` on `DocumentView` and on `OwnedDocument`; dispatch is metadata-driven, reading `@layout` + `@pretty` directives produced by the grammar; the call site authors no formatting policy. | "How do I customise the output? Where does the indent setting live?" | Cookbook page `cookbook/format.md` plus the per-grammar `@pretty` strategy vocabulary (`compact`, `group`, `indent`, `hardbreak`, `sep`, `block`) authored at grammar-source time. | None; formatting is total over admitted documents. Authoring a `@pretty` strategy that disagrees with `@layout` raises `BBNF-LAYOUT-CONFLICT` at grammar-compile time, never at `format()` call time. |

## 25. Implementation Order

The implementation order is:

1. Commit Phase 2 synthesis outputs.
2. Start tranche A from a clean worktree.
3. Archive `ser` and `gorgeous`.
4. Create the 24-crate skeleton and metadata validator.
5. Close A with generalization and tree-shape gates.
6. Build B and C before any production lowerer work.
7. Build D before F needs extension-aware generation.
8. Build E before F lowerers.
9. Build F before G future-grammar proof.
10. Build H after recognizer facts and runtime template exist.
11. Build I after recovery facts and runtime views exist.
12. Build J only when parity, SOTA, docs, and publication checks have real
    artifacts to verify.

No implementation tranche starts by editing PASS outputs, prompt contracts,
locks, corpora, or inheritance docs. Those documents are inputs.

## 26. Master Close

The restart is ready for tranche drafting when:

1. `restart/ARCHITECTURE.md` defines the workspace, DAG, APIs, IRs, BBNF
   surface, Cargo schema, runtime, lowerers, and gates.
2. `restart/MIGRATION.md` assigns current crates and file families to concrete
   fates.
3. This master plan sets A-J tranche stubs with owner gates and conflict
   resolutions.
4. All three files cite governing sources and preserve the settled authority:
   tape stays tape, rewrite-mode stays out, Unicode algebra stays under regex,
   and declaration crates are not default.

After this point, the next work is detailed tranche drafting and implementation
from a clean branch.
