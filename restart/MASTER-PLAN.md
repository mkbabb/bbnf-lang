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
| Path/visitor | KEEP concept, split implementation. | Tranche G owns `path`, `path-core`, `path-ts`, visitor mutation. |
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
| Path | `path`, `path-core`, `path-ts`. |
| Sister crates | `egraph`, `egraph-derive`, `csp-solver`, `parse-that`, `simd-scan`. |
| Dev/test | `test-fixtures`. |

The old workspace crates map to this set according to `restart/MIGRATION.md`.
`ser` and `gorgeous` are archive-only before implementation starts, per Lock 10
(`restart/locks/14-LOCKS.md:54-56`).

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
| Future grammar add | Add `yaml.bbnf` plus metadata; no Rust source edits except generated runtime. | A/G/F |
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
PASS-2 defines generated LOC budget tracking (`restart/audit/pass-2-codegen/PASS-2.md:293-310`).

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
| D | BBNF Extension Surface | 5 | Settled extensions parse/typecheck; rewrite-mode rejected. |
| E | Backend IR And VM | 5 | 23-variant Backend IR validates and VM replays all variants. |
| F | Rust Lowerer And Runtime Template | 6 | Rust lowerer emits equal generated runtime for seed grammars. |
| G | Path, Value, Visitor | 5 | `pointer!`, `select!`, visitor mutation, and future grammar gate pass. |
| H | Pratt, SIMD, WASM | 6 | Auto-detected Pratt/SIMD and WASM V1 pass early SOTA gates. |
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
| H | 8 | Lock 10, PASS-2 SIMD/WASM, SOTA corpus. | J. | Pratt, SIMD, WASM V1, early perf. |
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
| H | Auto Pratt/SIMD and WASM V1. | `@pratt` or `@simd` grammar directives. |
| I | Recovery/incremental/LSP. | LSP-only parser semantics. |
| J | Parity, SOTA, docs, publish dry run. | New architecture decisions without routed amendment. |

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
| A.W0 | Branch/tag preflight, archive `ser` and `gorgeous`, remove them from workspace. | `cargo metadata` succeeds without archive crates. |
| A.W1 | Create 24 crate skeletons with Lock 13 tree shape. | `cargo check --workspace` reaches expected stubs. |
| A.W2 | Replace root metadata schema. | Metadata validation accepts current nine grammars. |
| A.W3 | Add source/error/grammar minimal APIs. | `grammar` can parse a seed `.bbnf` into AST. |
| A.W4 | Add generalization and tree-shape lint gates. | No hardcoded grammar dispatch in generic crates. |

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
| B.W3 | Direct builder side-by-side with tape. | Direct view borrows spans from tape. |
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
| C.W1 | TypeFacts and HM/bidirectional core. | Host-free seed grammar typechecks. |
| C.W2 | ShapeFacts and value-shape mining. | Direct builder consumes ShapeFacts. |
| C.W3 | RecognizerFacts and Pratt/SIMD candidate mining. | Facts feed placeholder BIR hints. |
| C.W4 | CSP/egraph bridge tables. | Egraph and CSP exchange facts through bridge API. |
| C.W5 | CostFacts and extraction skeleton. | Backend IR builder receives selected alternatives. |

Hard close:

```sh
cargo test -p ir grammar_ir
cargo test -p passes types shapes recognizers bridge
cargo test -p cost-model facts
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
| D.W1 | Generic rules and annotations. | Generic seed grammar typechecks. |
| D.W2 | `@host fn` declarations and host primitive registry. | Host call compiles without declaration crate. |
| D.W3 | Multi-function chaining type/runtime contract. | Chain result feeds a later parser expression. |
| D.W4 | `@error`, `@layout`, regex Unicode routing, rewrite rejection. | Rewrite syntax fails; regex Unicode stays in `parse-that/regex`. |

Hard close:

```sh
cargo test -p grammar extensions
cargo test -p passes host_generics lookbehind
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
| PASS-2 runtime template. | Output under `runtime/src/grammars/<name>` (`restart/audit/pass-2-codegen/PASS-2.md:98-116`). |
| PASS-2 lowerer contract. | `BackendLowerer` methods and Rust V1 scope (`restart/audit/pass-2-codegen/PASS-2.md:80-96`). |
| Lock 6. | Committed source generation, no proc-macro facade (`restart/locks/14-LOCKS.md:44`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| F.W0 | Rust lowerer skeleton for control flow and literals. | Generated seed grammar compiles. |
| F.W1 | Tape/direct emit and builder integration. | Runtime parse returns `DocumentView`. |
| F.W2 | Host calls/chains, layout, error recover. | Extension seed grammar compiles and runs. |
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
| README path API. | `pointer!`, `select!`, visitor mutation (`restart/README.md:272-318`). |
| Lock 7. | `path`, `path-core`, `path-ts` split (`restart/locks/14-LOCKS.md:46`). |
| PASS-3 path and visitor commitments. | Typed path diagnostics and mutation API (`restart/audit/pass-3-runtime/PASS-3.md:82-95`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| G.W0 | `path-core` AST/parser/evaluator. | Runtime document query test passes. |
| G.W1 | Rust `pointer!` and `select!`. | Compile-time path diagnostics work. |
| G.W2 | `ValueRef`, `ValueOwned`, shape-backed projection. | Value API reads seed grammar. |
| G.W3 | Read-write visitor mutation. | Mutation updates document through visitor only. |
| G.W4 | `path-ts` schema and future grammar test. | Add `yaml.bbnf` plus metadata without Rust edits. |

Hard close:

```sh
cargo test -p path-core
cargo test -p path
cargo test -p runtime visitor
cargo test -p test-fixtures future_grammar_yaml
```

## 13. Tranche H - Pratt, SIMD, WASM

Goal: activate performance recognizers and the WASM V1 path without adding
syntax directives.

Inheritance:

| Source | Use |
|---|---|
| Lock 10. | Pratt and SIMD are auto-detected (`restart/locks/14-LOCKS.md:52`). |
| PASS-2 detector and SIMD coverage. | Detection thresholds and scalar/NEON/AVX2/AVX512/WASM coverage (`restart/audit/pass-2-codegen/PASS-2.md:118-135`). |
| SOTA corpus. | JSON/CSS competitor baselines (`restart/corpora/SOTA.md:50-89`, `restart/corpora/SOTA.md:130-136`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| H.W0 | Pratt recognizer facts and BIR `PrattSpine`. | Expression grammar uses auto-detected Pratt. |
| H.W1 | SIMD recognizer facts and `SimdScan` integration. | Literal/regex scans route through scanner kernels. |
| H.W2 | AVX2/NEON/scalar dispatch gates. | Platform-specific tests or skipped metadata. |
| H.W3 | WASM V1 via wasm32 Rust binding. | WASM package parses seed grammar. |
| H.W4 | Early JSON SOTA gates. | twitter/citm/canada progress report. |
| H.W5 | Early CSS SOTA gates. | bootstrap/animate progress report. |

Hard close:

```sh
cargo test -p passes recognizers
cargo test -p simd-scan
cargo test -p codegen wasm
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
| I.W1 | Incremental source snapshots and reparse plans. | Edited seed grammar reparses changed region. |
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
| J.W1 | Final SOTA gate and benchmark report. | JSON/CSS/SIMD targets met or formally routed. |
| J.W2 | Public docs redo. | Docs build and examples run. |
| J.W3 | Package readiness for public crates. | Dry-run publish metadata passes. |
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
| H close | Recognizer/SIMD/WASM paths are real consumers of C/E/F work. |
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
| F.W0-F.W2 | New lowerer emits partial seed grammar output. | Budget report can be advisory. |
| F.W3 | Template headers and equality land. | Budget gate becomes required. |
| F.W5 | Current nine seed grammars regenerate. | +2 percent ceiling enforced. |
| H | SIMD/WASM generation may add target-specific output. | Attribute budget by target. |
| J | Final release artifacts regenerate from clean checkout. | Budget and equality are release gates. |

PASS-2 is the authority for the +2 percent generated LOC ceiling
(`restart/audit/pass-2-codegen/PASS-2.md:293-310`).

## 21. Lock Ownership

| Lock | Owner tranche | Close proof |
|---|---|---|
| 1 Tape/direct substrate | B/F/H | Runtime tests, no OpenFrame clone stack. |
| 2 Layout lowering term | D/F | Layout facts lower through BIR. |
| 3 Cursor-parse/byte-skip | B/H | Empty-path elision and scanner tests. |
| 4 CSP/egraph bridge | C | Bridge tests, no fused hypergraph. |
| 5 Backend IR lowerers | E/F/H | Codegen BIR-only tests. |
| 6 Committed codegen | F/J | Regeneration equality. |
| 7 Path split | G | `path`, `path-core`, `path-ts` tests. |
| 8 SOTA gates | H/J | Bench report with baselines. |
| 9 Slice-borrow API | B/G | `parse`, `parse_in`, `parse_owned` tests. |
| 10 Pratt/SIMD auto | C/H | Recognizer facts, no directives. |
| 11 Path-dep incubation | A/J | Sister crates remain generic and publishable. |
| 12 Archive `ser`/`gorgeous` | A/J | Archive paths outside workspace. |
| 13 Tree discipline | A through J | `lint-tree`, `lint-loc`. |
| 14 Grammar generalization | A/D/F/G/J | Future grammar test and no hardcoded dispatch. |

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
| Old grammar registries reappear in new crates. | Lock 14 lint from A onward; future grammar test in G. |
| Direct-to-struct bypasses tape. | B/F tests prove direct views share spans/events with tape. |
| Backend lowerer imports Grammar IR for convenience. | E/F boundary tests and crate dependency checks. |
| `@host fn` becomes a hidden declaration-crate requirement. | D host tests prove generic primitives and metadata path first. |
| Rewrite-mode returns through ffuzzy inheritance. | D parser rejection test and no IR variant. |
| Unicode class algebra leaks into BBNF. | D routes Unicode to `parse-that/regex`; Grammar IR keeps regex opaque. |
| Generated LOC grows without review. | F/J budget report with PASS-2 +2 percent ceiling. |
| LSP incremental parser diverges from batch parser. | I CLI/LSP diagnostic parity tests. |
| SOTA gates are measured on unclear hardware. | H/J benchmark metadata records CPU, OS, build flags, input hashes. |
| Legacy archive becomes active code again. | A/J workspace membership checks. |

## 24. Implementation Order

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

## 25. Master Close

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
