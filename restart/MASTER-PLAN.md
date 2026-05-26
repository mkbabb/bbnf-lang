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
and OpenFrame ladders (`restart/locks/LOCKS.md:48`). PASS-3 resolves stale
ParseStream mentions against the README and locks
(`restart/audit/pass-3-runtime/PASS-3.md:14-23`).

**SK-V6 fold-back (2026-05-15).** The H/J SOTA close now routes through
same-plane schema v3 and the SK-V6 asmjson/DAV1D synthesis. The target remains
to beat sonic-rs, simdjson, yyjson, and strict asmjson rows, but only rows with
matching strictness, output shape, ownership, hardware, feature mask, corpus,
and freshness can close SOTA. The mandatory implementation handoff is
`restart/skinny/tranches/sk-v6/SPEC.md`.

The central compiler move is two IRs plus side tables. Grammar IR is semantic;
Backend IR is executable and is the only lowerer input. README requires two
IRs (`restart/README.md:104-118`), and Lock 5 rejects emitters walking grammar
directly (`restart/locks/LOCKS.md:113`). PASS-2 supplies the Backend IR
payload contract (`restart/audit/pass-2-codegen/PASS-2.md:52-76`); the
post-Phase-8.4 fold lands the 20-variant alphabet (19 semantic variants plus
`Return`) per ARCH §7.2.

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
| Backend IR | KEEP concept, REINVENT exact contract around the 20-variant shape (19 semantic variants plus `Return`). | Tranche E owns BIR, VM, and lowerer boundary. |
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
| Sister crates | `egraph`, `egraph-derive`, `csp-solver`, `parse-that`, `bbnf-simd`. |
| Dev/test | `test-fixtures`. |

The old workspace crates map to this set according to `restart/MIGRATION.md`.
`ser` and `gorgeous` are archive-only before implementation starts, per Lock 12
(`restart/locks/LOCKS.md:199`).

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

Lock 13 defines the tree-shape and LOC ceiling (`restart/locks/LOCKS.md:207`).
Lock 8 defines SOTA competitor anchors (`restart/locks/LOCKS.md:119`).
PASS-2 defines generated LOC budget tracking (`restart/audit/pass-2-codegen/PASS-2.md` §6).

Exact SOTA close rows. Each row binds a competitor baseline, a bbnf target, and
the platform that produces both. The baseline numbers are the corpus anchors
in `restart/corpora/SOTA.md:50-89` and `restart/corpora/SOTA.md:130-136`.

| Row | Competitor baseline | bbnf target | Platform | Owner |
|---|---|---|---|---|
| `json/twitter` | sonic-rs 436us; simd-json 424us; M5 Max DOM-class anchors yyjson 3687 MiB/s / simdjson 2923 / sonic-rs Value-DOM 2438 (`restart/corpora/SOTA.md` §2.4). | <= 380us on M1 Pro; close to sonic-rs Value-DOM (2438 MiB/s) on M5 Max DOM-class. | M1 Pro macOS / M5 Max macOS, native Rust release with `target-cpu=native`. | H.W1/H.W2, J.W1. |
| `json/citm` | sonic-rs 854us; simd-json 831us. | <= 750us on M1 Pro; ≥ yyjson 2498 MiB/s on M5 Max (skinny v3 already at 3571). | M1 Pro / M5 Max macOS, native Rust release with `target-cpu=native`. | H.W1/H.W2, J.W1. |
| `json/canada` | sonic-rs 3.144ms; simd-json 3.226ms (`restart/corpora/SOTA.md:56`). | <= 2.8ms on M1 Pro; ≥ yyjson 1550 MiB/s on M5 Max (skinny v3 already at 1675). | M1 Pro / M5 Max macOS, native Rust release with `target-cpu=native`. | H.W1/H.W3, J.W1. |
| `json/twitter` + `json/random` + `json/unicode_mixed` + `json/unicode_basic` plus the nine SK-V5-regressed parse rows | sonic-rs / simdjson per-corpus M5 Max anchors in `skinny/RESULTS.md` and sidecar profiles. | `parse_value_at` attributed at PC level under `runtime/parse-attribution`; outcome-G eliminated or each remaining G row carries a falsified REDRESS candidate. | M5 Max macOS arm64 NEON. | SK-V6 Wave 1 re-profile first; H.W1/H.W2/H.W3 only after fresh attribution. |
| `json/direct_to_struct` / `semantic_full_digest_stressor` (expanded workload gate) | sonic-rs semantic digest sidecar rows in the same `skinny/RESULTS.md` run. | no `N-direct` rows for the stressor unless each miss carries a falsified REDRESS route. The generated SinkOnly direct path is correctness-green, BIR-lowered, and now preserves raw string spans to the sink boundary, but the digest stressor still passes only four rows; REDRESS 66-69 reject source-hook folding, parser-owned decoded scratch, byte-output unescape, and semantic string facts as the close route for this workload. | M5 Max macOS arm64 NEON. | H.W4, J.W1. |
| `json/real_typed_struct` (representative DirectBuild gate) | sonic-rs serde struct and serde_json struct oracles over the same owned Rust output types. | Track 1 generated `SinkOnly` and Track 2 independent direct parser each produce owned typed structs before checksum; no parse-time checksum-only sink; no bench-private Track 1 parser; broad `serde_json::Value` output is allowed only for fields proven null-only in the checked fixture. REDRESS 70 rejects the first hand-authored typed sink as a close and adds the schema-source rule: the host/API output type must feed generated `DirectBuild` field facts before this row can prove grammar-general direct output. Scout close requires Track 1 within `sonic-rs * 1.10` on one generated typed fixture and no worse than `sonic-rs * 1.25` on the other before folding into the full gate. | M5 Max macOS arm64 NEON. | SK-V6 Wave 3 / H.W4. |
| `json/collapsed_stage_x86_strict` | asmjson strict/permissive rows split on the same x86_64 host; sonic-rs/simdjson/yyjson sidecars in the same run. | `CollapsedStage` may beat asmjson only on a strict same-plane row with generated grammar tables, admitted Layer 1 primitives, and a per-grammar wrapper. Permissive asmjson rows are flaw probes and cannot close the strict V1 gate. | Zen 4 / AVX-512-class x86_64 after arm64 matrix close. | Optional SK-V6 Wave 7 / H.W5 successor. |
| `css/bootstrap` | lightning-css 4.16ms. | <= 3.0ms. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W6, J.W1. |
| `css/animate` | lightning-css 1.97ms. | <= 1.6ms. | M1 Pro macOS, native Rust release with `target-cpu=native`. | H.W6, J.W1. |
| `simd/structural_scan` | simdjson On-Demand ~56000 Mbps on x86 AVX2; ~40000 Mbps on M-series NEON. | >= 40000 Mbps on M-series, >= 56000 Mbps on x86 AVX2; scalar parity hash matches. | M1 Pro macOS NEON and x86_64 AVX2 build host. | H.W2/H.W5, J.W1. |

The SOTA close gates measure the Rust line only at H.W1/H.W2/H.W4/H.W5/H.W6
per Lock 8 amendment (`restart/locks/LOCKS.md:119`). WASM SOTA measurements
defer post-V1 alongside the V2 `WasmBackend: Backend` impl per
`restart/ARCHITECTURE.md` §7.5; no measurement-pending WASM anchor lands in V1.

SOTA-parity is the meta-grammar correctness floor: a bbnf-generated parser
that lands within the competitor envelope (e.g., `json/twitter` ≤ 480us at
H.W2 against sonic-rs 436us) demonstrates V1 correctness regardless of
SOTA-beat status. SOTA-beat is the audacious target the user mandate
anchors against (`json/twitter` ≤ 380us at J.W1, surpassing sonic-rs);
the H tranche body owns the cost-driven rewrites + SIMD-recogniser tuning
that closes the parity-to-beat delta. If H.W1/H.W2/H.W3 measurements land
at parity but not beat, J.W1 close gates record the parity-not-beat outcome
and route SOTA-beat work to the H tranche body for further iteration; V1
correctness does not gate on SOTA-beat.

Benchmark reproducibility schema. Every benchmark row at H.W1/H.W2/H.W4 and J.W1
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
| E | Backend IR And VM | 5 | 20-variant Backend IR validates and VM replays all variants (post-Phase-8.4 fold per ARCH §7.2). |
| F | Rust Lowerer And Runtime Template | 6 | Rust lowerer emits equal generated runtime for seed grammars. |
| G | Path, Value, Visitor | 5 | `path!`, `select!`, visitor mutation, and future grammar gate pass. |
| H | Pratt, SIMD, typed-event codegen, full-SOTA receivers | 10 current rows + V1.1 receiver waves | Auto-detected Pratt/SIMD pass, typed-event/direct row-plane accounting, CSS parity, JSON 51-row strict matrix, decision-engine fold, union material-differential/block, zero-orphan primitive discipline, and no-demotion gates on the Rust line. WASM defers post-V1 via `WasmBackend: Backend` per `restart/ARCHITECTURE.md` §7.5. |
| I | Recovery, Incremental, LSP | 5 | LSP incremental parse and recovery diagnostics work on seed grammars. |
| J | Parity, Docs, Publication Close | 6 | Rust/VM parity, SOTA, docs, and publication readiness pass; WASM/TS parity defers to V2 backends. |

The counts are planning stubs. Full wave docs are not part of Phase 2.

Pass Omega V2 SK-V14 AUDIT-ZERO reconciliation note (per MP-3B-V1-D01):
the A-J table remains a planning census, but §13 already has ten concrete H
rows (`H.W0`, `H.W1`, `H.W2`, `H.W2.5`, `H.W3`, `H.W4`, `H.W4.LOCK14`, `H.W5`,
`H.W6`, `H.W7`). The current concrete MASTER census is 59 rows: A5 + B5 + C6
+ D6 + E5 + F6 + G5 + H10 + I5 + J6. Under SK-V14 audit-zero baseline
(`restart/skinny/tranches/sk-v14/SYNTHESIS.md:54`-`60`,`:191`-`198`) every
prior CSS L4 + JSON admit row in `skinny/RESULTS.md` reverts to
AUDIT-FALSIFIED; the 59 stub waves remain pending; the SK-V14 SPEC W0..W11
plan (see §13.3 below) executes FIRST per the indefatigability clause and
SPEC §16 sequencing. The V3 wave-classification census (0 landed / 6 refuted
/ 59 pending / 14 new) is the post-CRUD ledger; what survives audit are the
architectural pillars (W5 bbnf-regex extraction LOAD-BEARING, W6 e-graph
active cost extraction LOAD-BEARING, W7 CSP cascade LOAD-BEARING, bbnf-simd
52-file primitive surface, OffsetFlags + Tape generic substrate, and
generated JSON parse_direct + real-typed parsers — labeled
`pillars-LOAD-BEARING`, distinct from `landed-as-row-admit`). Scoped skinny
landings are NOT V1/root/campaign close unless the row text says so.
Evidence: `restart/MASTER-PLAN.md:524`-`535`,
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:80`-`84`,
`restart/skinny/tranches/sk-v14/SYNTHESIS.md:178`-`187`.

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
| H | Auto Pratt/SIMD on the Rust line. | `@pratt` or `@simd` grammar directives; WASM lower-and-bench work defers post-V1 alongside V2 `WasmBackend: Backend`. |
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
| Lock 14. | Enforce grammar generalization (`restart/locks/LOCKS.md:220`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| A.W0 | Branch/tag preflight, archive `ser` and `gorgeous`, remove them from workspace. | `cargo metadata` succeeds without archive crates; `git rev-parse pre-restart-2026-05-04` resolves; greenfield branch exists. |
| A.W1 | Create the V1 crate skeletons with Lock 13 tree shape; bind crate package names per Architecture §1 (unprefixed internal crates: `path`, `path-core`, `test-fixtures`, `passes`, `bbnf-simd`, `egraph`, `csp-solver`, `parse-that-regex`; user-facing crates retain `bbnf-` prefix). `path-ts` defers post-V1 alongside the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5. | `cargo check --workspace` reaches expected stubs; `rg "bbnf-path\|bbnf-test-fixtures"` returns zero hits in workspace member declarations; `path-ts` does not appear as a V1 workspace member. |
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

Pattern H per-tranche census (per MP-3B-V1-D03 + LAC-1E-15 + LOCKS.md Lock 14
amendment at `restart/locks/LOCKS.md:402`-`435`). Every tranche must cite the
current Pattern H per-grammar hand-written runtime file count via committed
transcript of `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs'
| wc -l` (the `-mindepth 2` form is mandatory; `-maxdepth 2` is REJECTED by
Lock 14 amendment). SK-V14 baseline at HEAD `85a043224`: **67** (V13 64 + 3
css_pretty co-derivation). Any +N over the prior tranche must trace to
grammar-roster change OR PRUNE-4 sub-wave count update (9 sub-waves not 8 per
S-P0 §2.3). The W6 PRUNE-4 envelope binds at ≤2.0k LOC C-1 part-B aggregate
across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted), ≤90
min per sub-wave (W6.0..W6.8), aggregate ≤810 min, per SK-V14 SPEC §13 W6
authority at `restart/skinny/tranches/sk-v14/SPEC.md:243`.

R4 `cargo xtask regen-{grammar}` round-trip discipline (per MP-3B-V1-D05 +
LAC-1E-13 + LOCKS.md Lock 6 amendment at `restart/locks/LOCKS.md:185`-`198`).
Any file carrying `// @generated by skinny bbnf-codegen` must (a) trace to a
rostered xtask emission, (b) emit byte-equivalent output when regenerated
from grammar source + workspace metadata, (c) reject hand-patching per
`[clean-regen-discipline]`. The CSS L4 fake `@generated` header on
hand-written templates is the pattern S-P0 §1 identifies as the dominant
recurrence vector; SK-V14 W2 lands `cargo xtask regen-css` round-trip clean
as the first instance of the regen-{grammar} family for the skinny runtime
tree only. Pass Omega V3 W2R assigns root `crates/core/src/runtime/css_l4/`
collapse to W6.0 after W5D-DELETE closes over W5C-GEN's provider-free generator body. The
family extends to JSON / Sheets / BBNF / EBNF / BNF / CSV / Math at later
waves; A consumers inherit the R4 gate so future grammar onboarding inherits
the round-trip check.

## 7. Tranche B - Runtime Substrate

Goal: implement tape plus direct-to-struct as one runtime substrate family.

Inheritance:

| Source | Use |
|---|---|
| Lock 1. | Tape/direct substrate and no OpenFrame ladders (`restart/locks/LOCKS.md:48`). |
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
| Lock 4. | Bridge crates compose by output piping, not fused hypergraph (`restart/locks/LOCKS.md:111`). |
| Current `ir` corpus. | Mine useful facts and split large files (`restart/corpora/MODULES.md:264-505`). |

Stub waves:

| Wave | Scope | Consumer gate |
|---|---|---|
| C.W0 | Grammar IR enum, IDs, spans, validation. | Seed grammar lowers AST to Grammar IR. |
| C.W1 | HM principal-scheme core plus expected-type checking inside `passes::layout`, with first-order equality unification before bounded coercion and finite CSP choices, **DK13 algorithmic completeness for higher-rank polymorphism per Lock 4 amendment**, and **GADT V1 user-facing branch-local-equality refinements** (`Pattern @ where T = U` per PASS-1 §6 grammar amendment) solved through OutsideIn(X)-style implication constraints in `passes::types`, with `BBNF-LOCAL-EQUALITY-ANNOTATION` emitted on missing or ill-typed refinement; `LayoutFacts` is public, while `TypeFacts` and `TypeObligationLog` remain internal. | Host-free seed grammar proves principal schemes under DK13; GADT-using grammar proves match-arm refinements discharge wanted equalities under OutsideIn(X); type-obligation snapshot separates equality, expected, coercion, finite-choice, local-equality, and instantiation stages; downstream passes read `LayoutFacts`, never `TypeFacts`; the GADT surface composes through CSP `Implication` constraints. |
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

W7 PRUNE-5 SCAFFOLD-to-LOAD-BEARING wiring discipline (per MP-3B-V1-D08).
SK-V14 W8 per-grammar policy + W9 same-substrate union are SCAFFOLD-ONLY at
SK-V14 baseline: zero runtime consumers in `passes`/`codegen`/`runtime`/`ir`;
only `bbnf-bench/src/{bin/gate.rs, lock14_baseline.rs, report.rs}` reference
them. C.W4 (CSP/egraph bridge) + C.W5 (CostFacts/Pareto) consume the SK-V14
W7 PRUNE-5 wiring per `restart/skinny/tranches/sk-v14/SPEC.md:779`-`839` to
elevate W8/W9 from SCAFFOLD to LOAD-BEARING via named pre-wave hot-leaf row
+ Lock-1 triad per shape; sequencing C-1 → C-4 binds per S-P0 §2.2. Decision
engine fold per SK-V13 G2 + T-P2 2D requires active cost evidence + CSP
feasibility + egraph language + guarded rewrites + P1-P8 retirement; W8/W9
SCAFFOLD wiring is the falsifiability gate. Consumer: bounded resolver
reports + JSON/CSS equality rows + samply trace hot-leaf shift.

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
| D.W3 | Multi-function chaining type/runtime contract, including left-to-right expected-argument obligations, bounded coercion-site fixtures, and first-mismatch diagnostics. | Chain result feeds a later parser expression; a negative chain fixture fails at the first mismatching step with `BBNF-CHAIN-STEP` or `BBNF-SUBSUMPTION-EDGE`. |
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
| PASS-2 BIR table. | 20-variant shape (19 semantic variants plus `Return`) per ARCH §7.2 and PASS-2 payload-refiner mapping. |
| Lock 5. | Lowerers consume BIR, not Grammar IR (`restart/locks/LOCKS.md:113`). |
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
| Lock 6. | Committed source generation, no proc-macro facade (`restart/locks/LOCKS.md:115`). |

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

Pattern H per-tranche census (per MP-3B-V1-D03; mirrors Tranche A discipline).
At F close, `find crates/core/src/runtime -mindepth 2 -type f -name '*.rs' |
wc -l` must trend monotonically downward through W6 PRUNE-4 9 sub-waves
(SK-V14 baseline = 67; F close target is determined by the W5A/W5B.0..W5B.4/W5C-GEN/W5D-DELETE
PRUNE-3 + W6 PRUNE-4 collapse plan per SK-V14 SPEC §13 W5A/W5B.0..W5B.4/W5C-GEN/W5D-DELETE/W6
at `restart/skinny/tranches/sk-v14/SPEC.md:242`-`244`). The combined
W5A/W5B-FRONTEND aggregate/W5C-GEN/W5D-DELETE + W6 envelope binds at W5A closed 921 LOC,
W5B-FRONTEND aggregate ≤1.0k LOC across W5B.0..W5B.4, W5C-GEN ≤1.0k LOC, W5D-DELETE ≤400 LOC
(C-1 part-A) + ≤2.0k LOC (W6 C-1 part-B aggregate across 9 sub-waves; avg ~220
LOC/grammar; generated output uncounted) per SK-V14 SPEC §13 authority. F.W5
nine-seed regeneration consumes the R4
round-trip discipline (per MP-3B-V1-D05): every emitted file is byte-equal
to its regenerated form from grammar source + workspace metadata; hand-patches
return REVISE per `[clean-regen-discipline]`. R4 family covers
JSON / CSS / Sheets / BBNF / EBNF / BNF / CSV / Math.

## 12. Tranche G - Path, Value, Visitor

Goal: deliver user-facing value navigation and mutation on top of tape/direct
documents.

Inheritance:

| Source | Use |
|---|---|
| README path API. | `path!`, `select!`, visitor mutation (`restart/README.md:272-318`). |
| Lock 7. | `path`, `path-core` split on the V1 Rust line (`restart/locks/LOCKS.md:117`); `path-ts` defers post-V1. |
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

## 13. Tranche H - Pratt, SIMD, typed-event codegen

The SK-V5 implementation packet at `restart/skinny/tranches/sk-v5/SPEC.md` declares seven numbered waves (0–7) that execute the H-tranche scope below. The two numbering schemes align as follows: SK-V5 Wave 0 has no prior H-tranche entry (it is new strictness/diagnostic infrastructure); SK-V5 Wave 1 covers H.W1 plus H.W2.5 substrate authoring; SK-V5 Wave 2 covers H.W2 (number lever) and the `SinkOnly` portion of H.W4; SK-V5 Wave 3 covers H.W3 (parse-that string/Unicode closure) and the residual UTF-8 work folded out of H.W4; SK-V5 Wave 4 is the Lock 14 remediation row added below (not previously wave-tagged); SK-V5 Wave 5 covers H.W5 (primitive bodies); SK-V5 Wave 6 covers H.W6 (CSS SOTA gates / strict matrix); SK-V5 Wave 7 is the x86 `CollapsedStage` successor (optional, grammar-keyed). The reading order for execution is `restart/skinny/tranches/sk-v5/SYNTHESIS.md` → `restart/skinny/tranches/sk-v5/SPEC.md` → `restart/skinny/tranches/sk-v5/HANDOFF.md`; the SK-V3 packet remains the historical attribution path that the SK-V5 packet supersedes for wave routing.

Goal: activate performance recognizers + typed-event codegen template + per-target SIMD/ASM primitive layer (`bbnf-simd`) on the Rust line; close the expanded skinny SOTA-BEAT gate against sonic-rs, simd-json, yyjson, simdjson C++, and asmjson reference planes. The primary close is **arm64 Apple Silicon**. x86_64 AVX-512 closes as a secondary hardware gate with strict/permissive comparator rows separated.

Pass Omega V1.1 status ledger:

| Wave | V1.1 status | Receiver |
|---|---|---|
| H.W0 | LANDED-SCOPED: preflight, Plan D capacity, and escape-mask correctness prerequisite landed; no throughput row admitted by this alone. | Lock 15/16 evidence and S-P3 primitive guards. |
| H.W1 | PARTIAL: Rust-state substrate/backend-shape derivation landed (`603308b3`), throughput recovery pending. | H/J benchmark rows and S-P3 row movement. |
| H.W2 | PARTIAL: consumed primitive subset admitted; new primitives require scalar parity, checkasm, same-wave consumer, and measured row movement. | Lock 16 manifest and primitive ledger. |
| H.W2.5 | PARTIAL: primitive vocabulary exists; contract-only macros stay non-admitting until consumed or deleted/demoted/blocked. Per MP-3B-V1-D11 + S-P3 V3 §3C, any wave admitting any dispatch-envelope-internal primitive ships F-V2-P1ABC-RERECORD as Stage-0 of the same wave (cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites); under SK-V14, Stage-0 binds UNCONDITIONALLY to W10 (parse_only distinct path) per SPEC §13 entry-gate inheritance at `restart/skinny/tranches/sk-v14/SPEC.md:982`-`1000`. | Primitive state machine and zero-orphan gate; SK-V14 W10 R8 parse_only distinct path exit gate; consumer manifest (must-bind per SPEC §1): P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13. |
| H.W3 | SPLIT: number materialization landed; UTF-8/string fusion is refuted as a close route. | Regex/HIR facts and string/Unicode row gates. |
| H.W4 | PARTIAL: SinkOnly/direct/typed row-plane accounting remains open under SK-V13. | JSON 51-row strict sonic matrix. |
| H.W4.LOCK14 | PARTIAL: GrammarConfig legality is evidence, not fleet-wide grammar-neutral Lock 14 closure until CSS plus both Sheets and BBNF-self fail-closed or generated-role witnesses pass; with only one negative control, label the result `scoped non-JSON witness`. Per MP-3B-V1-D06 + LAC-1E-14, a formal 5th `FactStream` substrate category lands alongside OffsetTape/EventTape/SinkOnly/CollapsedStage (CSS L4 declaration-values admitted as same-plane fact-output per Lock 1 substrate manifest at `restart/locks/LOCKS.md:100`-`116`); the 5-shape `BackendShape` canon stays unchanged. Per MP-3B-V1-D09, Lock 14 v+1 generic-crate forward invariant binds at LOCKS.md `:402`-`435`. Per MP-3B-V1-D08, W7 PRUNE-5 SCAFFOLD-to-LOAD-BEARING wiring per `restart/skinny/tranches/sk-v14/SPEC.md:779`-`839` consumes here. | Generated provider/config/sink repair plus both Sheets and BBNF-self witnesses; Lock 14 baseline gate; Lock 1 v+1 substrate manifest; SK-V14 W7 PRUNE-5 LOAD-BEARING wiring. |
| H.W5 | LANDED-SCOPED: consumed arm64/generic set landed; x86 successor optional/background; no-orphan rule mandatory. | Lock 16 and SK-V13 G4. |
| H.W6 | REPLACED: SK-V6 strict-matrix-before-CSS text is superseded by the SK-V13 full-SOTA receiver map. | SK-V13 G1-G7 and J.W1. |
| H.W7 | PENDING: Pratt recognizer facts and BIR `PrattSpine` still depend on C/E fact and BIR closure. | C.W3/C.W4/E.W0/E.W1/H. |

The H tranche post-2026-05-12 is no longer "aspirational" per Lock 8's earlier framing. It is a concrete engineering target with empirical attribution (`restart/skinny/tranches/shared/SOTA-BEAT-DESIGN.md` §1) and three measurable gates: (a) twitter Mbps wall-clock, (b) hot-leaf count vs comparator, (c) cycle-per-byte vs simdjson floor.

Inheritance:

| Source | Use |
|---|---|
| Lock 10. | Pratt and SIMD are auto-detected (`restart/locks/LOCKS.md:164`); the cost model selects per-grammar `backend_shape ∈ {EagerTape, OffsetTape, EventTape, SinkOnly, CollapsedStage}`. |
| Lock 15. | Build-profile discipline (`lto=true codegen-units=1 panic="abort" debug=true`) — co-load-bearing with the codegen template inversion. |
| Lock 16. | SIMD/ASM admissibility allowlist (§4 below carries the verbatim allowlist). |
| Lock 5. | V1 ships `RustBackend: Backend` only via `restart/ARCHITECTURE.md` §7.5; WASM defers post-V1 as `WasmBackend: Backend`. |
| Lock 8. | V1 SOTA close gates measure the Rust line only; WASM SOTA defers post-V1 (`restart/locks/LOCKS.md:119`). |
| PASS-2 detector and SIMD coverage. | Detection thresholds and scalar/NEON/AVX2/AVX512 coverage (`restart/audit/pass-2-codegen/PASS-2.md` §3). |
| SOTA corpus + comparative profile baseline. | JSON/CSS competitor baselines (`restart/corpora/SOTA.md:50-89`, `restart/corpora/SOTA.md:130-136`); six-agent comparative-profile cohort outputs at `skinny/profile/{sonic-rs-v2,simdjson-v2}/PROFILE-REPORT.md` (post-2026-05-12). |
| SOTA-BEAT design. | `restart/skinny/tranches/sk-v6/DISPATCH-PROMPT.md` + `restart/skinny/tranches/sk-v6/SYNTHESIS.md` + `restart/skinny/tranches/sk-v6/SPEC.md` + `restart/skinny/tranches/sk-v6/research/` — SK-V6 same-plane recovery over the SK-V5 measured substrate history. The deleted SK-V3/SK-V4 packets are no longer dispatch authority. |

Waves (host-arch primary; arm64 Apple Silicon first, then x86_64). SK-V6 now
binds the wave routing: SK-V5 landed the Rust state, then measurements refuted
the Wave 3 UTF-8-fusion close. The expanded SOTA-BEAT corpus from
`restart/skinny/BENCH.md` §3 is the binding gate, not the historical triad.
No new kernel prescription is canonical until SK-V6 Wave 1 re-profiles the
generated Track 1 baseline.

SK-V6 addendum: after the 2026-05-15 asmjson/DAV1D pass, H.W6 requires schema
v3 same-plane comparator metadata before any SOTA claim, and H.W2.5 requires
DAV1D-grade checkasm hardening (forced feature masks, register-clobber checks,
stack canaries, cycle counters, scalar oracle normalization, and same-wave
consumers). The direct close route is generated `DirectFieldFacts`, not a
benchmark-private sink.

| Wave | Scope | Consumer gate |
|---|---|---|
| H.W0 (preflight + Plan D capacity + escape_mask_64 fix) | Lock 15 enforcement (`[profile.release] lto=fat codegen-units=1 panic="abort" debug=true`); Plan D `Vec::with_capacity(256)` + geometric grow adopted as production default per Wave 2 Agent 6 evidence (deletes sampled and sparse-flag capacity helpers); `escape_mask_64` NEON correctness bug fix per Wave 2 Agent 5 evidence (xorshift adversarial repro `0xCAFEF00DBAADF00D`); `bbnf-simd` crate scaffold (per-target submodule layout per `SOTA-BEAT-DESIGN.md` §3.1); CPUID dispatch at parser construction. | `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity` zero divergences; Plan D matches the cross-corpus throughput table at SK-V3 packet §4 (+4.8% random, +10.2% github_events, 23–64% capacity reclamation); release-build invocation confirms `-C lto=fat` + `codegen-units=1`. |
| H.W1 (typed event cursor over tape projection — load-bearing) | **LANDED for Rust-state substrate in SK-V5 Wave 1 (`603308b3`) and still open for throughput recovery.** Cost model in `passes::recognizers` derives `LayoutFacts.backend_shape[rule_id]` from existing Grammar IR facts; `BackendShape`, `LayoutFacts.backend_shape`, `derive_backend_shape`, and `codegen/src/lower/` exist. Parse-time retained projection aux side tables (REDRESS 50), byte-class whitespace cursor (51), and parser-local structural-mask cursor (53) are rejected. H.W1 must make structural projection the single parse substrate; no new BIR variant and no new BBNF directive. | SK-V6 Wave 1 re-profiles the generated Track 1 baseline before selecting any new H.W1 intervention. A later implementation wave must either reduce named retained parse G rows or record falsified candidates in REDRESS. |
| H.W2 (bbnf-simd kernel contract — host-aarch64 first; pathology-class fix kernels) | **PARTIALLY LANDED.** AArch64 classifier work, `bulk_emit_positions_64`, `BYTE_CLASS_FROM_*`, bitmap helpers, and `EOB_PAD_CLAMP` have scalar references, checkasm parity, and hot consumers where admitted. Class A `match_tiny_plain_string` wiring remains invalidated. The older claim that the NEON UTF-8 codepoint pipeline is the corrected parse-G fix is superseded by SK-V6: REDRESS 50-55 show the Wave 3 family did not close the current generated-baseline rows. | Canada structural scan is green in the full matrix at 69075 Mbps. New H.W2 primitives require SK-V6 Wave 1 attribution, same-wave consumer, and same-row Mbps lift; otherwise they are rejected. |
| H.W2.5 (primitive vocabulary authoring + checkasm gate — `ext/x86/bbnf.asm`) | Author grammar-neutral primitive macros that compose the shared SIMD/ASM vocabulary at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`. Admission is consumed-only: `BYTE_CLASS_FROM_TABLE_64`, `BYTE_CLASS_FROM_EQ_SET_64`, `BITMAP_PREFIX_XOR_64`, `BITMAP_NEXT_SET_BIT`, `EOB_PAD_CLAMP`, the AArch64 structural+terminator classifier, and `BULK_EMIT_POSITIONS_64` have scalar references, checkasm parity, and same-wave hot consumers. `BULK_EMIT_COMPRESSED`, `FSM_DISPATCH_THREADED`, `FRAME_PUSH_BOUNDED`, and `FRAME_POP_BOUNDED` remain contract-level until structural-tape compressed sink, bracket-stack, or per-grammar CollapsedStage consumers land in the same change. Layer 0 vendored macros stay read-only in `ext/x86/x86inc.asm` (dav1d, BSD-2). Admission to either consumer path is gated through the FFmpeg-discipline harness at `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --release --test checkasm_parity`. | All admitted primitives pass `primitive-checkasm` with zero divergence on the active host; no primitive lands without a wave-bound consumer. The prior "all nine bodies" reading is superseded by the no-orphan rule. |
| H.W3 (parse-that primitive closure — string, Unicode, number) | **NUMBER LANDED; UTF-8 FUSION REFUTED AS CLOSE.** Eisel-Lemire and integer materialization are vendored and consumed; `numbers` direct closes. The SK-V5 Wave 3 string/UTF-8 family is not canonical as a close route after REDRESS 50-55. `parse-that/string` / `unicode` remain the ownership boundary for exact string and Unicode semantics, but no new kernel lands without fresh attribution and same-row lift. | `numbers` passes. `unicode_*`, `distinct_values`, `y_string_unicode`, and other string-bound direct/retained rows remain SK-V6 Wave 1 profile targets before implementation. |
| H.W4 (workload gates + direct-to-struct `SinkOnly` closure + 5-shape backend_shape per-rule selection) | **PARTIALLY LANDED.** The generated `SinkOnly` path is correctness-green, lowerer-authored from BIR, preserves raw string spans to `JsonSink::*_source`, and keeps retained view-walk digest as parity oracle. Direct receiver/source-shape redress lifts four stressor rows (`citm_catalog`, `apache_builds`, `github_events`, `instruments`) but overall gate remains `N-direct / NoGo`. Generic decoded visitor, sink-local exact-stats helper, quote-source streaming hasher, source-hook folding, parser-owned decoded scratch, byte-output unescape, semantic string facts, and the first hand-authored JSON typed sink are rejected as closes. REDRESS 72 admits a generated-retained-only cap-16 probe and rejects global/direct/Track 2 widening, so H.W4 must carry per-shape cost facts instead of a single string threshold. | SK-V6 Wave 3 splits the plane: `semantic_full_digest_stressor` remains visible as a guard, while `real_typed_struct` becomes the representative DirectBuild closure row only after the host/API output schema is lowered into generated field facts rather than supplied by a benchmark-private hand sink. |
| H.W4.LOCK14 (Lock 14 remediation — generic-crate audit closure; SK-V5 Wave 4) | **PARTIALLY LANDED.** `simd-scan`, `generated_eventcursor.rs`, and the `eventcursor` feature/cfg path are purged. The `bbnf-simd/src/lib.rs` god-module split and remaining JSON classifier parameterization are still pending. | SK-V6 Wave 5 performs the durability split only after parse-G and direct N-direct are reduced; it is not a SOTA-beat prerequisite. |
| H.W5 (x86_64 AVX-512 primitive path — strict additions consumed by retained and direct shapes) | **LANDED FOR CONSUMED ARM64/GENERIC SET; X86 SUCCESSOR OPTIONAL.** Consumed primitive admission is green for the active host set. No-orphan macro bodies stay blocked until same-wave consumers exist. `CollapsedStage` remains a separate per-grammar `.asm` authoring wave. | `primitive-checkasm` must pass for admitted primitives; x86 `CollapsedStage` requires Zen 4 silicon, NASM author, strict comparator plane, and per-grammar consumer. |
| H.W6 | **SK-V13 full-SOTA receiver map.** G1 full CSS L4 parity, G2 decision-engine fold, G3 union variant or architectural block, G4 zero aarch64 production orphans, G5 all 51 JSON rows above strict sonic-rs or architecturally blocked, G6 Totality V1.1/G-Omega before W0, and G7 no-demotion. | `restart/skinny/tranches/sk-v13/SYNTHESIS.md` G1-G7 governs S-P3 and J.W1; every miss needs row admission, measured rejection, or architectural-block proof. |
| H.W7 | Pratt recognizer facts and BIR `PrattSpine`. | Expression grammar uses auto-detected Pratt. |

### 13.2 Pass Omega V1.1 Receiver Waves

| New wave | Allocation | Receiver |
|---|---|---|
| MP.NW0 | G-Omega and Totality V1.1 ratification before any SK-V13 W0/source/RESULTS/REDRESS wave. | G-Omega packet; CRUD-2/CRUD-4; S-P3 pre-W0 refusal gate. |
| MP.NW1 | Current-state authority and row-plane telemetry fold for SK-V12 CSS admission and JSON parse/direct/typed planes. | BENCH, HANDOFF, MASTER current-state ledger. |
| MP.NW2 | CSS stylesheet root and selector framework under strict lightningcss parity. | SK-V13 G1 CSS feature rows. |
| MP.NW3 | CSS declaration-values expansion: declarations, `var()`, `calc()`, colors, custom-property/value facts. | SK-V13 G1 feature rows. |
| MP.NW4 | CSS visual/rule expansion: gradients, transforms, filters, easing, at-rules, nesting. | SK-V13 G1 feature matrix. |
| MP.NW5 | JSON 51-row strict sonic matrix: 17 corpora x parse_only/direct_to_struct/real_typed_struct. Per MP-3B-V1-D11 + S-P3 V3 §3C carry-forward, any wave admitting any dispatch-envelope-internal primitive ships F-V2-P1ABC-RERECORD as Stage-0 of the same wave: cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites. Under SK-V14, Stage-0 binds UNCONDITIONALLY to W10 (parse_only distinct path) per SPEC §13 entry-gate inheritance chain at `restart/skinny/tranches/sk-v14/SPEC.md:982`-`1000`. Consumer manifest (must-bind per SPEC §1): P2-A C6 + P2-C C-P2C-3 + C-P2C-8 + P2-E Gap 1/3/4/5 + P2-F C6/C7/C10/C12/C13. | SK-V13 G5 and J.W1; SK-V14 W10 R8 parse_only distinct path. |
| MP.NW6 | Lock 14 generated provider/config/sink/fact/flag/schema repair with CSS plus both Sheets and BBNF-self fail-closed or generated-role witnesses for fleet-wide Lock 14 closure; with only one negative control, label the result `scoped non-JSON witness`, not fleet-wide or grammar-neutral closure. Per MP-3B-V1-D06, a formal 5th `FactStream` substrate category lands alongside OffsetTape/EventTape/SinkOnly/CollapsedStage (CSS L4 declaration-values fact-stream admitted same-plane fact-output; comparator provenance mandatory; telemetry gate-consumed); the 5-shape `BackendShape` canon (EagerTape/OffsetTape/EventTape/SinkOnly/CollapsedStage) STAYS UNCHANGED — FactStream is a substrate-target classification, NOT a 6th BackendShape variant. Per MP-3B-V1-D09 + LAC-1E-08 + LAC-1E-15, Lock 14 v+1 generic-crate forward invariant binds: generic crates carry ZERO `match grammar { Json => ..., CssL4 => ... }` arms; ZERO grammar-named modules; ZERO grammar-specific types in public APIs; ZERO per-grammar feature flags; ZERO hand-written per-grammar runtime files (post-W6); per-grammar runtime is emitted from ONE grammar-agnostic generator template consuming grammar source + workspace metadata; `xtask gate-json` rejects any commit that introduces grammar-specific code in a generic crate. | Lock 14, generated registry, non-JSON witnesses; Lock 1 v+1 substrate manifest at `restart/locks/LOCKS.md:100`-`116` (FactStream 5th substrate); Lock 14 baseline gate at `restart/locks/LOCKS.md:402`-`435`. |
| MP.NW7 | Regex/HIR fact extraction import boundary through `parse-that-regex` or equivalent facts. | D/H regex fact consumer and generated parser/resolver row. |
| MP.NW8 | Decision-engine replacement: bbnf-regex extraction, egraph language, guarded rewrites, active cost, CSP feasibility, P1-P8 retirement/fail-closed compatibility. Per MP-3B-V1-D08, W7 PRUNE-5 wires the SK-V14 W8 per-grammar policy + W9 same-substrate union from SCAFFOLD to LOAD-BEARING (zero runtime consumers in `passes`/`codegen`/`runtime`/`ir` at SK-V14 baseline; only `bbnf-bench/src/{bin/gate.rs, lock14_baseline.rs, report.rs}` reference them) per `restart/skinny/tranches/sk-v14/SPEC.md:779`-`839`. Sequencing C-1 → C-4 binds per S-P0 §2.2. Per MP-3B-V1-D11 + S-P3 V3 §3C, Stage-0 binding F-V2-P1ABC-RERECORD applies to any wave admitting any dispatch-envelope-internal primitive (cargo build + interactive samply record + cfg_attr flip at `generated.rs:33-237` 8 sites). | SK-V13 G2, C.W4/C.W5, backend-shape rows; SK-V14 W7 PRUNE-5; bounded resolver reports + JSON/CSS equality rows + samply trace hot-leaf shift. |
| MP.NW9 | AArch64 ASCII run-skip production split and zero-orphan disposition. | SK-V13 G4, CSS scan-block consumer or measured rejection. |
| MP.NW10 | Fresh union-substrate variant or architectural block with material differential beyond REDRESS 96/97/98. | SK-V13 G3, substrate/runtime/codegen/bench wave. |
| MP.NW11 | Both Sheets and BBNF-self fail-closed or generated-role witnesses for generated role facts and future grammar onboarding; one negative control is `scoped non-JSON witness` evidence only, not fleet-wide Lock 14 or grammar-neutral closure. | Lock 14 and future grammar onboarding gates. |
| MP.NW12 | Rolling SOTA delta and no-demotion close gate for every JSON row/plane and every CSS feature. | BENCH rolling table, HANDOFF close gate, MASTER J.W1/J.W5. |

Cost/risk summary: MP.NW0 250-700 doc LOC high process; MP.NW1 180-420
doc/report LOC high gate; MP.NW2 350-500 high parity; MP.NW3 600-840
medium-high; MP.NW4 700-950 medium; MP.NW5 350-900 high; MP.NW6 700-2000 cap
2600 high; MP.NW7 300-700 high; MP.NW8 900-2200 cap 2800 high; MP.NW9 120-220
narrow high if generalized; MP.NW10 700-1600 cap 2000 high; MP.NW11 250-800
medium-high; MP.NW12 150-350 high anti-paper-close. These costs are receiver
allocations for G-Omega-ratified planning, not implementation authorization.

The typed event cursor at H.W1 remains the load-bearing architectural change
for the current arm64 expanded-gate miss, but SK-V6 withdraws the old claim
that H.W3's NEON UTF-8 codepoint pipeline is the parse-G close. REDRESS 50-55
show that the SK-V5 Wave 3 family did not lift the generated Track 1 baseline,
so SK-V6 re-profiles before prescribing another kernel. H.W2.5 lifts the SIMD
vocabulary at `ext/x86/bbnf.asm` so H.W5 and any per-grammar `CollapsedStage`
authoring wave share one macro layer. The x86_64 route past simdjson, yyjson,
and asmjson uses the same cost model but different kernels; collapsed-stage is
selected by grammar facts and CPUID when it beats retained tape under strict
correctness. The prior WASM-binding wave routes to V2 per Lock 8.

Per-grammar `CollapsedStage` `.asm` authoring waves are grammar-keyed, not numbered into the H letter tranche. Each grammar whose `LayoutFacts.backend_shape` derivation admits `CollapsedStage` for at least one rule (per ARCHITECTURE.md §7.3 priority 6 — target features admit AND rule is a hub with ≥ 4 byte-disjoint arms) acquires one wave per `(grammar × ISA)` pair, where the ISA axis is the subset of `{avx2, avx512, neon}` the host machine admits and the cost model selects. The grammar's classifier `.data` table (256-byte input-byte → class) and state-transition `.data` table (9-state-class current × class → next, sized `9 × |class_set|` for asmjson-shape DPDAs and grammar-derived for others) are codegen-emitted from Grammar IR facts; the wrapper `.asm` — roughly 150 LOC per `(grammar × ISA)` pair — is hand-written by composing the Layer-1 macros from `skinny/crates/bbnf-simd/ext/x86/bbnf.asm` (the spine being `FSM_DISPATCH_THREADED` + `FRAME_PUSH_BOUNDED` / `FRAME_POP_BOUNDED` + the chosen byte-classifier primitive) around the codegen-emitted tables. Admission is gated by the `BBNF-COLLAPSEDSTAGE-NOT-VIABLE` diagnostic at the cost-model output: absence of an author for a `(grammar × ISA)` pair the cost model would otherwise select falls the grammar silently back to `OffsetTape` (the next-priority shape per the §7.3 derivation tree), with the diagnostic surfacing the missing pair so the audit log records the deferral. These authoring waves are tracked in a separate per-grammar log alongside the grammar's `[workspace.metadata.bbnf.grammars.<name>]` block per Lock 14, not in the H letter tranche, because their cadence is grammar-arrival-driven rather than fleet-architecture-driven. The same-wave-consumer rule at `docs/precepts/instructions/LESSONS-LEARNED.md:17-26` binds each wave: a kernel author lands the `.asm` only with the codegen-emitted `.data` tables and the parity test for the grammar's `parse_value` entry point in the same wave; no `.asm` lands ahead of its consumer.

### §13.1 Admissible SIMD primitives (Lock 16 allowlist, verbatim)

Audit-overlay 4-column SK-V14 schema binding (per MP-3B-V1-D07 + LAC-1E-16 +
LOCKS.md Lock 8 amendment at `restart/locks/LOCKS.md:213`-`233`). Every
gate-consumed RESULTS row must carry FOUR audit-overlay columns required by
SK-V14 SYNTHESIS §2 + SPEC §0.4 (per `restart/skinny/tranches/sk-v14/SPEC.md:135`-`138`):

| audit-overlay column | value domain | semantics |
|---|---|---|
| `track2_entry_point` | symbol path | the entry-point function the comparator binds against; AUDIT-FALSIFIED if the row's "comparator" routes through a different code path than the strict comparator. |
| `comparator_plane` | enum { parse_only, direct, typed } | comparator-plane provenance; rows must declare which plane the comparator implements. |
| `per_iter_equality` | boolean + oracle name | a per-iteration equality oracle runs INSIDE the timing region; AUDIT-FALSIFIED if equality verifies outside the timed loop. |
| `audit_overlay_verdict` | enum { AUDIT-FALSIFIED, AUDIT-SUSTAINED, AUDIT-PENDING } | gate-enforced verdict per row; `xtask gate-json` REJECTS rows missing any of the four columns or carrying AUDIT-FALSIFIED without an architectural-block. |

Falsifiability gate companion: an admitted row missing any of these columns
is no admit at all; `xtask gate-json` consumes the schema and rejects
incomplete rows per Lock 8 v+1 amendment.

Each row carries citation + architecture + replaces. Hand-tuned undocumented intrinsic loops without an architectural name are forbidden as magic per Lock 16.

**arm64 NEON** (`bbnf-simd/aarch64/`):

| Primitive | Intrinsic | Citation | Replaces / enables | asmjson doesn't use |
|---|---|---|---|---|
| 4-table 64-byte classify | `vqtbl4q_u8` | Lemire 2019 "Arbitrary byte-to-byte maps using ARM NEON" | sonic-rs's 1-table `vqtbl1q_u8`; saves ~16 c/64B per intrinsics agent quantification; binds the NEON body of the `BYTE_CLASS_FROM_TABLE_64` Layer-1 macro at `ext/aarch64/bbnf.asm` | arm64-only; asmjson is AVX-512 only |
| Interleaved-vector movemask | `vld4q_u8` + `vshrn_n_u16` + `vsriq_n_u8` + `vzip1q_u8` | validark.dev/posts/interleaved-vectors-on-arm/ (Validark 2024) | sonic-rs's AND-OR tree; 4× faster bitmap synthesis; binds the aarch64 body of the `BITMAP_NEXT_SET_BIT` Layer-1 macro (the bit-reverse + clz analog of x86 `tzcnt`) | arm64-only |
| Quad-load 64 bytes | `vld1q_u8_x4` | Arm A64 ISA | 4× separate `vld1q_u8`; frees 2 M-series load-ports | arm64-only |
| Branchless mask select | `vbslq_u8` | Arm A64 ISA | conditional emit/branch in `string_block.rs` | arm64-only |
| Byte popcount | `vcntq_u8` + `vaddvq_u8` | Arm A64 ISA | scalar `count_ones()`; saves GPR round-trip | arm64-only |
| **LD4-interleaved 4-channel classifier** (NEW 2026-05-12; Wave 1 NEON research) | `vld4q_u8` + per-channel `vqtbl1q_u8` + `vshrn`/`vsri`/`zip1` | validark.dev/posts/interleaved-vectors-on-arm/; simdjson PR #2333 | parallel-channel byte classification; ~10% drop in simdjson stage1 c/B on Apple arm64 (M5 Max-specific lever) | AVX-512 only; lever unreachable for asmjson |
| **NEON ternary bitwise BCAX/EOR3** (NEW 2026-05-12; ARMv8.2-A SHA3 extension) | `vbcaxq_u8` (Bit-Clear-And-XOR), `veor3q_u8` (3-way XOR) | Arm Architecture Reference Manual ARMv8.2-A SHA3 | equivalent to AVX-512 `vpternlogq` on arm64; collapses `bic + eor` 2-op into 1-op; ~12–18% inner-loop op-count reduction; on every M1+ and Neoverse-V1/V2 | sonic-rs does NOT use; arm64-only |
| **NEON set-membership (svmatch_u8 emulation)** (NEW 2026-05-12) | `vceqq_u8` + `vorrq_u8` reduction tree | Lemire 2026 "The fastest way to match characters on ARM processors" | portable equivalent of SVE2 `svmatch_u8` against 16-byte alphabet; same source ships M5 Max NEON and dispatches to native MATCH on SVE2 hosts (Graviton4); binds the NEON body of the `BYTE_CLASS_FROM_EQ_SET_64` Layer-1 macro | arm64-only |

**x86_64 AVX-512 VBMI2** (`bbnf-simd/x86_64/avx512_vbmi2/`; Ice Lake+ / Zen 4+). Per Wave 1 Agent 1 + Agent 3 evidence, asmjson's actual instruction footprint is minimal — only `vpcmpeqb`, `kmovq`, `vpcmpub`, `korq`, `vmovdqu8`, `tzcnt`; **zero** `vpternlogq`, `vpclmulqdq`, `vpcompressb`, `vgf2p8affineqb`, `vpermb`, `vpermt2b`, `vpmadd52`, `vpopcntb`. Esoterica below are the route to outclass asmjson with strict architectural additions on top:

| Primitive | Intrinsic | Citation | Replaces / enables | asmjson uses? |
|---|---|---|---|---|
| One-shot structural-offset emission | `_mm512_mask_compressstoreu_epi8` | felixcloutier VPCOMPRESSB; Lemire 2022 "Parsing JSON faster with AVX-512"; simdjson `icelake/simd.h:157` **explicitly leaves this unused for portability** | tzcnt + blsr scalar loop (~25 c/64B saved); binds the AVX-512 VBMI2 body of the `BULK_EMIT_COMPRESSED` Layer-1 macro | No — uses tzcnt-driven seek |
| 3-mask boolean fusion | `_mm512_ternarylogic_epi64` | WikiChip AVX-512F; Sneller "Branchless Code With AVX-512" | (in-string ∧ ¬escaped) ∧ structural in 1 µop | No — uses separate `vpand`/`vpor` |
| 128-byte byte-shuffle classify | `vpermi2b` | WikiChip AVX-512_VBMI | 2× `vpshufb` lane-restricted lookups; supports alphabet >7-byte first-set; binds the AVX-512 VBMI body of the `BYTE_CLASS_FROM_TABLE_64` Layer-1 macro for arbitrary 64-byte LUTs | No — limited to 16-byte alphabet |
| Cross-window quote-state carry | `_mm512_alignr_epi8` | felixcloutier | explicit prev-bit propagation | Used (shift-register pattern) |
| **k-mask arithmetic family** (NEW 2026-05-12) | `_kandn_mask64`, `_kxor_mask64`, `_kxnor_mask64`, `_kshiftrq`, `_ktestq` | Travis Downs kreg-facts blog (2019/2020); AVX-512F base | keep classifier masks in k0..k7 across state transitions; spill only on EOB | No — uses only `korq` + `kmovq`; ~4 store+load eliminated per chunk |
| **VPCLMULQDQ at 512-bit lane** (NEW 2026-05-12; Ice Lake+, Zen 3+) | `_mm512_clmulepi64_epi128` | WikiChip VPCLMULQDQ; BranchFree.org "Quote pairs with PCLMULQDQ" (2019); Linux kernel CRC-32C 45–60 GB/s with this primitive | simdjson prefix-XOR string-bitmap primitive at 4× width vs 128-bit `_mm_clmulepi64_si128`; binds the AVX-512 body of the `BITMAP_PREFIX_XOR_64` Layer-1 macro (the NEON body falls back to a scalar carry chain per `SOTA-BEAT-DESIGN.md` §5.2) | No — uses cmp+branch on backslash per byte; no prefix-XOR primitive |
| **AVX-IFMA `vpmadd52luq`/`vpmadd52huq`** (NEW 2026-05-12; Sapphire Rapids+, Zen 4+) | `_mm512_madd52lo_epu64`, `_mm512_madd52hi_epu64` | WikiChip AVX-512_IFMA; Lemire 2024 Sapphire Rapids vs Zen 4 JSON | Eisel-Lemire mantissa multiplication for `parse_number`; mantissa-mul stays in vector lanes, returns f64 directly; ~3× on canada/mesh/marine_ik/numbers | No — dispatches number tokens to a Rust `JsonWriter` vtable (no number parse in asm) |
| **AVX-512 VNNI `vpdpbusd`** (NEW 2026-05-12; Cascade Lake+, Zen 4+) | `_mm512_dpbusd_epi32` | Lemire 2023 "Parsing integers quickly with AVX-512" | byte×byte→i32 dot product, 4 bytes per int32 lane; 16-digit chunk → 4 lanes of `(d3*1000 + d2*100 + d1*10 + d0)` via one dot product | No — no number parse in asm |
| **AVX-512 BITALG `vpshufbitqmb` + `vpopcntb`** (NEW 2026-05-12; Ice Lake+, Zen 4+) | `_mm512_bitshuffle_epi64_mask`, `_mm512_popcnt_epi8` | WikiChip AVX-512_BITALG | bit-gather 8 selected bits per 64-bit lane into k-mask (inverse of `vpcompressb`); per-byte popcount; per-state classification becomes data, not code | No — uses per-byte cmp + branch trees |
| **AVX-512 GFNI** (NEW 2026-05-12; Ice Lake+, Zen 4+) | `_mm512_gf2p8affine_epi64_epi8` | Wojciech Mula 2018-2024; Intel GFNI Technology Guide 2018 | arbitrary 8-bit affine transformation in 1 µop; 2× over `vpshufb`; single-op character classification for any grammar's structural-byte classify; binds the AVX-512 GFNI body of the `BYTE_CLASS_FROM_TABLE_64` and `BYTE_CLASS_FROM_EQ_SET_64` Layer-1 macros when the class set is encodable as an 8×8 GF(2) affine matrix (the asmjson-class structural set `{}[],:` falls in the affine-encodable subset) | No — uses per-state `vpcmpeqb` against literal byte sets |

**x86_64 AVX-2 + BMI2** (`bbnf-simd/x86_64/avx2/`; Haswell+ / Zen 1+):

| Primitive | Intrinsic | Citation | Replaces / enables |
|---|---|---|---|
| Bits-to-indexes | `_pext_u64` | Mula branchfree.org "Bits to indexes in BMI2 and AVX-512" (2018) | tzcnt + blsr loop on non-VBMI2 hosts; Zen 1/2 PEXT slow, gate via CPUID |
| String-bitmap prefix-XOR | `_mm_clmulepi64_si128` (CLMUL) | simdjson original; sonic-rs `src/util/arch/x86_64.rs` | baseline simdjson primitive; adopt rather than reinvent; binds the AVX-2 fallback body of the `BITMAP_PREFIX_XOR_64` Layer-1 macro on hosts that lack VPCLMULQDQ-512 |
| 32-byte byte-shuffle classify | `_mm256_shuffle_epi8` | simdjson haswell; sonic-rs | baseline AVX-2 classifier; binds the AVX-2 body of the `BYTE_CLASS_FROM_TABLE_64` Layer-1 macro (two `vpshufb` passes against a 32-byte half-LUT with high-nibble fold) |

**Portable scalar** (`bbnf-simd/scalar/`):

| Primitive | Mechanism | Citation | Replaces / enables |
|---|---|---|---|
| 8-byte SWAR classify | `word.wrapping_sub(0x2020202020202020) >> 7` for whitespace; `word ^ 0x2222222222222222` for quote | asmjson SWAR fallback (atomicincrement/asmjson `doc/paper.md:226-234`); ~7 GB/s on Zen 4 | non-SIMD hosts; correctness floor for portability |

**Handwritten `asm!` admissibility**: admitted only when the equivalent intrinsic is absent from `core::arch::*`. Current admitted list:

- arm64 `ldp` / `stp` pair-load and pair-store (yyjson `repeat16` lineage; no `core::arch::aarch64` intrinsic).
- arm64 `stnp` non-temporal pair-store (no intrinsic).
- arm64 `PRFM PLDL1KEEP` / `PRFM PLDL2STRM` tuned prefetch (`core::intrinsics::prefetch_*` does not expose PLDL2STRM).
- x86_64 / arm64 asmjson-style `r10`-direct-threading FSM entry (collapsed-stage backend H.W5; `asm!` is the only way to bind state to PC). Lifts into the `FSM_DISPATCH_THREADED` Layer-1 macro at `skinny/crates/bbnf-simd/ext/x86/bbnf.asm`, authored at H.W2.5; per-grammar `CollapsedStage` `.asm` sources consume this macro plus codegen-emitted `.data` tables rather than inlining the threading discipline.

New admissible-list entries require documented measurement justification at the patch site + a citation to a published architecture.

Hard close:

```sh
cargo test -p passes recognizers
cargo test -p bbnf-simd
cargo test -p bbnf-simd exact_prefilter_parity
cargo test -p parse-that-regex cross_engine_parity
cargo bench -p bbnf-bench --bench sota_json
cargo bench -p bbnf-bench --bench sota_css
```

### §13.3 SK-V14 W0..W11 Receiver Block (per MP-3B-V1-D02 + MP-NW-SK14-W0..W11-INHERIT)

The SK-V14 SPEC §3-§14 12-wave W0..W11 plan is admitted as a MASTER-plan
receiver block sibling to §13.2 (Pass Omega V1.1 Receiver Waves). SK-V14
W0..W11 execute under S-P3 dispatch BEFORE any MP.NW* MASTER wave per the
indefatigability clause + SPEC §16 sequencing. The MP.NW1..MP.NW12 receiver
waves admit only what the SK-V14 W11 close residuals route forward per the
indefatigability clause (`restart/skinny/tranches/sk-v14/SYNTHESIS.md:39`-`50`).

The 12-wave manifest is authoritative at
`restart/skinny/tranches/sk-v14/SPEC.md:237`-`248`; this section absorbs it
as a MASTER receiver-block reference (verbatim absorption would risk drift
as SK-V14 SPEC updates; per CH1 receiver routing the SPEC remains the
canonical source of truth and this section refers by path).

Pass Omega V4 W4R amends the W4/W5 boundary after REDRESS-184: W2's
skinny-side `regen-css` gate admitted at `45568e669`, W3 production corpus
staging admitted at `b0a864f0b`, and W4 reruns as CSS L4 admit-ledger prune
only. CSS provider/template deletion moves to W5, where the deletion lands in
the same replacement slice as the grammar-agnostic provider path and
`regen_css.rs` migration. Pass Omega V5 W5R refines that receiver into W5A
source-consuming generator capability before the now-superseded W5B
provider/template deletion gate.
Pass Omega V6 W5BR supersedes the W5B deletion gate after REDRESS-210: W5A's
request boundary admitted at `286233fa2`, W5B-GEN now owns the provider-free
runtime generator body, and W5C-DELETE deletes provider/template residue and
closes Lock 14 only after W5B-GEN admits. W6 remains the Pass Omega V3 W2R
structure with W6.0 owning CSS L4 root-runtime collapse after W5C-DELETE.

Pass Omega V6 W5BR amends the W5 boundary after REDRESS-210: W5A remains
closed as source-consuming request-boundary work; W5B-GEN builds the
provider-free runtime generator body without deleting providers/templates; and
W5C-DELETE removes the old CSS provider/template mesh plus closes the Lock 14
baseline only after W5B-GEN is load-bearing. The V5 ≤1.4k combined W5A/W5B
budget is superseded for this repair: W5A closed at 921 LOC, W5B-GEN carries
≤1.0k C-1 part-A, and W5C-DELETE carries ≤400 C-1 part-A.

Pass Omega V7 W5B-GENR supersedes the V6 W5B-GEN gate after REDRESS-211:
W5A remains admitted as source-consuming request-boundary work; W5B-FRONTEND
now owns generic BBNF grammar-source frontend/import/IR closure with CSS L4 as
the strict positive witness; W5C-GEN owns the provider-free runtime generator
body consuming W5A request facts plus W5B-FRONTEND IR; and W5D-DELETE owns
provider/template deletion plus Lock 14 baseline close only after W5C-GEN is
load-bearing. The V7 C-1 part-A envelope is W5A closed at 921 LOC,
W5B-FRONTEND ≤1.0k, W5C-GEN ≤1.0k, and W5D-DELETE ≤400. W5B-FRONTEND and
W5C-GEN must first add explicit `lock14_baseline.rs` owner-path and parent-diff
subject routing before source redress. Compatibility syntax such as `@ws` is a
frontend-lowering witness into canonical IR, not a new public directive.

Pass Omega V8 W5B-FRONTENDR supersedes the V7 one-shot W5B-FRONTEND cap after
REDRESS-212: W5B-FRONTEND is an aggregate PRUNE-3B sequence with W5B.0
LOCK14-GATE, W5B.1 IMPORT-CLOSURE, W5B.2 LAYOUT-DISCARD, W5B.3
PRETTY-SPAN-PROJECTION, and W5B.4 REQUEST-CONSUMER. Each W5B.N sub-wave carries
HARD CAP 30 min, commit-safe evidence at 27 min, and halt at 30 min; aggregate
W5B-FRONTEND cap is <=150 min. W5B-FRONTEND closes only after W5B.0 through
W5B.4 all admit. W5C-GEN remains blocked until aggregate W5B-FRONTEND close,
W5D-DELETE remains blocked until W5C-GEN close, and W6/W7/W8/W9/W10 remain
blocked by the PRUNE chain. The W5B aggregate preserves the V7 <=1.0k C-1
part-A LOC envelope unless a later CHALLENGE authorizes narrower partitions.

| Wave | SPEC section | Scope | Entry gate | LOC budget (source + tests + gates + docs; generated output uncounted) | Time cap |
|---|---|---|---|---|---|
| W0 | §3 | Baseline Profile + Telemetry Lock (SK-V14-open) | Dispatchable only after G-Omega | 0 production behavior LOC; ≤250 report/gate/test/doc LOC | ≤90 min |
| W1 | §4 | Comparator Rebind + Per-Iter Equality + PRUNE-1 (R1 + R2 + R3 PRUNE-1) | Conditional on W0 close | ≤1.08k C-2 source/test + ≤500 C-5 part-A revert; total ≤1.58k | ≤90 min |
| W2 | §5 | regen-css xtask (R4 — first instance of regen-{grammar} family; skinny runtime tree only after W2R) | Conditional on W1 close | ≤2.0k C-3 part-A source/test; generated output named separately | ≤90 min |
| W3 | §6 | Production CSS Corpora (R5; ~960 KB) | Conditional on W2 close | ≤200 corpora-staging LOC; corpora files are bytes-only | ≤90 min |
| W4 | §7 | PRUNE-2 — CSS L4 admit-ledger prune: restore rolling delta to 0/24 CSS L4 admitted and add 24 row-keyed REDRESS entries; no provider/template deletion until W5D-DELETE authorizes deletion. | Conditional on W2 + W3 close | ≤500 docs/ledger | ≤90 min |
| W5A | §8 | PRUNE-3A — grammar-neutral source-consuming runtime generator contract: pass grammar source + workspace metadata into codegen, make required V1 grammar-source constructs parseable for runtime generation without grammar-id branches, migrate `regen-css` to the new path, prove CSS L4 plus JSON/Sheets/BBNF-self non-JSON gates; no provider/template deletion. | Conditional on W4 ledger close | ≤1.0k C-1 part-A source/test LOC | ≤90 min |
| W5B-FRONTEND | §8B | PRUNE-3B aggregate — W5B.0 LOCK14-GATE; W5B.1 IMPORT-CLOSURE; W5B.2 LAYOUT-DISCARD; W5B.3 PRETTY-SPAN-PROJECTION; W5B.4 REQUEST-CONSUMER. Lower CSS L4 compatibility constructs into canonical IR; no provider/template deletion. | Conditional on W5A close + REDRESS-212 / Pass Omega V8 CRUD; W5B-FRONTEND closes only after W5B.0..W5B.4 all admit | aggregate ≤1.0k C-1 part-A source/test LOC; redress/report edits and reject-only `skinny/REDRESS.md` count | ≤30 min per W5B.N; aggregate ≤150 min |
| W5C-GEN | §8C | PRUNE-3C — provider-free runtime generator body: replace the live `RuntimeProvider`/`render_runtime_profile` production path with one grammar-neutral generator body consuming W5A request facts plus W5B-FRONTEND IR; no provider/template deletion. | Conditional on W5B-FRONTEND close | ≤1.0k C-1 part-A source/test LOC | ≤90 min |
| W5D-DELETE | §8D | PRUNE-3D — provider/template deletion and Lock 14 baseline close: delete old provider/template residue only after W5C-GEN is load-bearing; retire provider dispatch residue; run `regen-css`, companions, JSON, and non-JSON witnesses. | Conditional on W5C-GEN close | ≤400 C-1 part-A source/test LOC | ≤90 min |
| W6 | §9 | PRUNE-4 — 9 sub-waves: W6.0 CSS L4 root-runtime collapse, then remaining per-grammar runtime collapses (R3 PRUNE-4; C-1 part-B) | Conditional on W5D-DELETE close | ≤2.0k C-1 part-B aggregate across 9 sub-waves (avg ~220 LOC/grammar; generated output uncounted) | ≤90 min per sub-wave (W6.0..W6.8); aggregate ≤810 min |
| W7 | §10 | PRUNE-5 — wire W8 policy + W9 union from SCAFFOLD to LOAD-BEARING (R3 PRUNE-5; C-4) | Conditional on W6 close (C-1 MUST precede C-4 per S-P0 §2.2) | ≤1.4k C-4 source/test LOC | ≤90 min |
| W8 | §11 | CSS L4 Re-Admit (R6; grammar-derived pipeline + production corpora + work-equivalent comparator) | Conditional on W7 close | ≤650 source/test LOC | ≤90 min |
| W9 | §12 | JSON Direct + Typed Re-Admit (R7; under rebound R1 comparators) | Conditional on W1 close locally, but globally blocked until PRUNE-1..PRUNE-5 close | ≤450 source/test LOC | ≤90 min |
| W10 | §13 | JSON parse_only Distinct Path + Re-Admit (R8) | Conditional on W1 + W9 close locally, but globally blocked until PRUNE-1..PRUNE-5 close; F-V2-P1ABC-RERECORD Stage-0 binds UNCONDITIONALLY per MP-3B-V1-D11 + S-P3 V3 §3C | ≤650 source/test LOC | ≤90 min |
| W11 | §14 | Close And Alpha Feedback | Conditional on W0-W10 dispositions | 0 source LOC; docs/RESULTS/REDRESS/HANDOFF/SPEC reconciliation only | ≤90 min |

Total envelope across all candidates (per SPEC §2 after V8 W5B-FRONTENDR): C-1 is W5A
closed 921 LOC + W5B-FRONTEND aggregate ≤1.0k across W5B.0..W5B.4 + W5C-GEN ≤1.0k + W5D-DELETE ≤400 + W6 ≤2.0k; C-2
600-1.08k; C-3 1.2k-2.0k; C-4 800-1.4k; C-5 250-500. Aggregate
~5.65k-8.90k. Any wave exceeding its envelope by >20% escalates per
`[generated-size-budget]`. R10 indefatigability clause: SK-V14 brackets
SK-V15 automatically if any goal unmet
(`restart/skinny/tranches/sk-v14/SPEC.md:44`-`50`,`:396`-`407`).

### §13.4 New Waves From T-P3 V4 LOCK (14 NEW; per MP-NW-01..14)

The 14 NEW waves added under T-P3 V4 LOCK consist of (a) the 12 V3-carried
MP.NW0..MP.NW12 receiver waves preserved in §13.2 above, plus (b) two new
G-Omega waves admitted by T-P3 V1 reconciliation per
`restart/audit/totality/p3/3B-master-plan-reconciliation.md:80`-`84`. Each
wave carries a same-wave consumer column. The 14 NEW waves are enumerated
below with their wave-id, allocation, and same-wave consumer.

| New wave | MASTER carrier | Allocation | Same-wave consumer |
|---|---|---|---|
| MP-NW-01 | §13.2 MP.NW0 | G-Omega and Totality V1.1 ratification before SK-V13/SK-V14 W0/source/RESULTS/REDRESS wave | G-Omega packet; CRUD-2/CRUD-4; S-P3 pre-W0 refusal gate |
| MP-NW-02 | §13.2 MP.NW1 | Current-state authority and row-plane telemetry fold | BENCH, HANDOFF, MASTER current-state ledger |
| MP-NW-03 | §13.2 MP.NW2 | CSS stylesheet root + selector framework under strict lightningcss parity | SK-V13 G1 CSS feature rows; SK-V14 W8 |
| MP-NW-04 | §13.2 MP.NW3 | CSS declaration-values expansion (declarations, var(), calc(), colors, custom-property/value facts) | SK-V13 G1 feature rows; SK-V14 W8 |
| MP-NW-05 | §13.2 MP.NW4 | CSS visual/rule expansion (gradients, transforms, filters, easing, at-rules, nesting) | SK-V13 G1 feature matrix; SK-V14 W8 |
| MP-NW-06 | §13.2 MP.NW5 | JSON 51-row strict sonic matrix (17 corpora × parse_only/direct_to_struct/real_typed_struct); Stage-0 F-V2-P1ABC-RERECORD binds per MP-3B-V1-D11 | SK-V13 G5 and J.W1; SK-V14 W10 R8 parse_only distinct path |
| MP-NW-07 | §13.2 MP.NW6 | Lock 14 generated provider/config/sink/fact/flag/schema repair; FactStream 5th substrate category per MP-3B-V1-D06; Lock 14 v+1 generic-crate forward invariant per MP-3B-V1-D09 | Lock 14 baseline gate; Lock 1 v+1 substrate manifest; SK-V14 W5A/W5B-FRONTEND/W5C-GEN/W5D-DELETE/W6 |
| MP-NW-08 | §13.2 MP.NW7 | Regex/HIR fact extraction import boundary through `parse-that-regex` or equivalent | D/H regex fact consumer and generated parser/resolver row |
| MP-NW-09 | §13.2 MP.NW8 | Decision-engine replacement: bbnf-regex extraction + egraph language + guarded rewrites + active cost + CSP feasibility + P1-P8 retirement; W7 PRUNE-5 SCAFFOLD-to-LOAD-BEARING per MP-3B-V1-D08 | SK-V13 G2; C.W4/C.W5; SK-V14 W7 PRUNE-5; backend-shape rows; bounded resolver reports |
| MP-NW-10 | §13.2 MP.NW9 | AArch64 ASCII run-skip production split and zero-orphan disposition | SK-V13 G4; CSS scan-block consumer or measured rejection |
| MP-NW-11 | §13.2 MP.NW10 | Fresh union-substrate variant or architectural block with material differential beyond REDRESS 96/97/98 (Lock 1 v+1 triad: changed data movement + changed consumer shape + measured row outcome) | SK-V13 G3; substrate/runtime/codegen/bench wave; refusal entry at §24 per MP-3B-V1-D10 |
| MP-NW-12 | §13.2 MP.NW11 | Both Sheets and BBNF-self fail-closed or generated-role witnesses for generated role facts and future grammar onboarding | Lock 14 and future grammar onboarding gates |
| MP-NW-13 | §13.2 MP.NW12 | Rolling SOTA delta and no-demotion close gate for every JSON row/plane and every CSS feature | BENCH rolling table; HANDOFF close gate; MASTER J.W1/J.W5 |
| MP-NW-14 | NEW (this section) — MP-NW-SK14-F-V2-P1ABC-RERECORD-STAGE-0 | Stage-0 binding obligation: F-V2-P1ABC-RERECORD attaches UNCONDITIONALLY to SK-V14 W10 per §13 entry-gate inheritance chain; any reorder/replacement of W10 inherits the Stage-0 obligation; consumer manifest must-bind per SPEC §1 | SK-V14 W10 R8 parse_only distinct-path exit gate; consumer manifest verified at W10 exit gate per `restart/skinny/tranches/sk-v14/SPEC.md:982`-`1000` |

The MP-NW-SK14-W0..W11-INHERIT wave (admits the SK-V14 SPEC verbatim as a
12-wave receiver block per §13.3 above) is carried as receiver structure,
not enumerated as a separate MP-NW-* row; the §13.3 table IS its carrier.

The MP-NW-SK14-SKELETON-DELETE-REFUTED wave is recorded as a
PERMANENT-PRE-BLOCK refusal at §24 carry/friction ledger (per MP-3B-V1-D10);
the refusal entry IS its consumer per CH6 anti-paper-close discipline.

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
| J.W0 | Rust/VM parity matrix for the V1 line; WASM/TS parity defers to V2 backend impls. | Rust/VM parity matrix passes for seed grammars; V2 carries cross-backend parity after `WasmBackend` / `TsBackend` registration. |
| J.W1 | Final SOTA gate and benchmark report. | JSON/CSS/SIMD targets met under SK-V13 G1-G7: full CSS parity, all 51 JSON rows above strict sonic-rs or architecturally blocked, decision-engine fold, union variant/block, zero aarch64 production orphans, Totality V1.1/G-Omega pre-W0, and no demotion. Misses require architectural-block proof or amendment before close. |
| J.W2 | Public docs redo. | Docs build and examples run. |
| J.W3 | Package readiness for public crates: confirm publication-name plan, validate `[workspace.package]` defaults, dry-run `cargo publish` for every public crate, and verify path-dep incubation does not leak to `crates.io`. Two gates apply per Lock 11 (`restart/locks/LOCKS.md:190`): (i) the **stable surface** (`bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `parse-that-regex`) publishes unconditionally at J.W3; (ii) the **incubation-cleared sister crates** (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`) publish at J.W3 only after the 2-tranche stability gate clears — API has not changed across two consecutive prior tranche closes, downstream consumers compile against a frozen-version dry-run, and no breaking change is queued. Crates that fail the stability gate carry their dry-run results in the J.W3 report and remain path-deps until the next J cycle. `path-ts` defers post-V1 alongside the principled TS-native parse+runtime fork; the V2 `TsBackend: Backend` impl per `restart/ARCHITECTURE.md` §7.5 publishes `path-ts` in V2. | `cargo xtask publish --dry-run` passes for the stable surface (`bbnf`, `bbnf-cli`, `bbnf-language-server`, `bbnf-bench`, `path`, `path-core`, `parse-that-regex`) plus every incubation-cleared sister crate (`egraph`, `egraph-derive`, `csp-solver`, `parse-that`); incubation-failing sister crates remain path-deps with the failure recorded; `path-ts` is not in V1 publish scope; private crates are unpublished. |
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

CH7 Overfit-Prune lens binding (per MP-3B-V1-D04 + LAC-1E-12 + LOCKS.md
preface clause at `restart/locks/LOCKS.md:44`-`69`). Every plan and every
redress at every CHALLENGE phase runs CH1-CH7 (not CH1-CH6). CH7 is a
first-class lens with the same blocking authority as CH1 (Correctness). A
CH7 REJECT triggers (a) immediate plan revise OR (b) redress revert with a
REDRESS entry for implementation artefacts. CH7 cannot be carried as
"acknowledged but not blocking". The CH7 lens scans for: fabricated
baselines; cited-but-absent surface text; circular evidence; cosmetic
re-author replacing the artefacts that should contain it; meta-CH7
collision pattern. CH7 is recorded as a clause in the LOCKS.md preface
(NOT Lock 17) — the 16-lock count is preserved per Lock manifest at
`restart/locks/LOCKS.md:71` `## Gestalt — sixteen locks`.

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

6-class cost-neutrality taxonomy (per FOLD-3D-013 + S-P3 V3 §3C extension of
the T-P1 5-class taxonomy with a 6th class). Every ACCEPT/MODIFY disposition
that touches a research-layer artefact classifies its fold into one of six
classes; un-classified folds default to admission cost and return REVISE.
The six classes are: (1) cite-rebind; (2) cite-cosmetic; (3) REJECT-label-
refinement; (4) anti-paper-close-paragraph-insertion; (5) anchor-refresh; (6)
mirror-refresh (NEW at V3). The discipline binds to CHALLENGE V{N}
fold-packet authoring and wave-triumvirate dispatch admission gates. Every
cite-bearing micro-fold institutionalises pre/post-grep on downstream mirrors
per NEW-CH2-V3-02 orphan-cell propagation guard.

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
| H.W1/H.W2 | Early JSON SOTA gates run with SIMD-target output (typed event cursor + Class B `\uXXXX` NEON decode; Class A `match_tiny_plain_string` wiring withdrawn per §13 H.W2). | SIMD-attributed LOC reported by target (AVX2/NEON/scalar); JSON SOTA delta numeric; wall under 120s. |
| H.W5 | x86_64 AVX-512 esoterica path; collapsed-stage backend. | AVX-512 SIMD-attributed LOC reported alongside arm64 NEON; strict/permissive comparator planes split. |
| H.W6 | Early CSS SOTA gates. | CSS visitor/path additions reported separately under PASS-3 generated visitor LOC budget; wall under 120s. |
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
| 15 Build-profile / i-cache discipline | F/H/J | Release profile, force-inline, hot-function budget, and post-LTO profile evidence. |
| 16 SIMD/ASM admissibility | H/J | Primitive manifest, scalar reference, strict checkasm, same-wave consumer, and zero-orphan disposition. |

Lock 13 verification rows. Each row is the executable evidence the named owner
must produce when closing its tranche; ARCHITECTURE.md §13 owns the exception
ledger.

| Surface | Child-count gate | LOC gate | Exception rationale | Enforcing command |
|---|---|---|---|---|
| Handwritten crate `src/` directories | 4-10 immediate children. | No handwritten file over 500 LOC. | Migration may temporarily raise the child count with a tranche-local rationale; lint allowlist names the exception. | `cargo xtask lint-tree` and `cargo xtask lint-loc --handwritten-max 500`. |
| Generated grammar/runtime files (`runtime/src/grammars/<name>/generated.rs`). | Not bound by 4-10. | LOC excepted; budget rows in §20 govern. | Lock 6 requires committed generation; LOC ceiling is the +2 percent budget. | `cargo xtask generated-loc-budget --max-growth 1.02`. |
| Generated data tables (Pratt, SIMD, scanner). | Not bound. | Excepted with header metadata. | Data products carry source metadata and stable headers, not LOC ceilings. | `cargo xtask lint-generated-headers`. |
| Proc-macro roots (`*-derive`, `path/src/macro_impl`). | 4-10. | 500 LOC handwritten ceiling. | None; proc-macro roots follow the handwritten rule. | `cargo xtask lint-tree` and `cargo xtask lint-loc`. |
| SIMD intrinsic files (`bbnf-simd/src/avx2.rs`, `neon.rs`). | 4-10 in parent dir. | 500 LOC ceiling unless the file is single-target intrinsics. | Single-target intrinsic files carry a tranche-local LOC exception. | `cargo xtask lint-loc --simd-exception`. |

The lock table in README marks all 16 locks as restart inputs
(`restart/README.md:377-397`). Locks 15 + 16 (build-profile discipline + SIMD/ASM
admissibility allowlist) land 2026-05-12 after the V9.2 lazy-tape refutation
and the six-agent comparative-profile cohort.

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

Per MP-3B-V1-D04, every documentation surface authored under the J tranche
runs CH1-CH7 (the CH7 Overfit-Prune lens binding is the same clause carried
in §17 above and in LOCKS.md preface at `restart/locks/LOCKS.md:44`-`69`).
Documentation drift that introduces fabricated baselines, cited-but-absent
surface text, circular evidence, or cosmetic re-author replacing the
artefacts that should contain it returns CH7 REJECT and triggers revise.

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
| Omega V1.1 MASTER reconciliation | CRUD-2 / G-Omega | §H scoped landings, refuted routes, and SK-V13 full-SOTA receivers are not visible in current MASTER. | Apply the Ω-D accepted diff only after Pass Omega convergence and G-Omega; preserve landed-scoped/partial/refuted/pending labels. | omega |
| Rolling SOTA delta | H/J/BENCH/HANDOFF | Close can paper over row demotion or one-row CSS admission. | `restart/skinny/ROLLING-SOTA-DELTA.md` or equivalent BENCH-owned table carries every JSON row/plane and CSS feature; regressions fail G7 unless architectural-block/user re-pin is recorded. | omega + skinny |
| G-Omega before SK-V13 W0 | HANDOFF / S-P3 | Implementation waves can start before Totality V1.1 ratifies skinny lessons. | HANDOFF and S-P3 SPEC refuse Wave 0, source edit waves, and RESULTS/REDRESS-writing waves until G-Omega closes. | omega + skinny |
| Declaration-crate escape valve | A/D | Review form missing reason, scope, owner, or deletion path. | Metadata validator rejects `allow_declaration_crate = true` without the eight-field review form (template at `restart/ARCHITECTURE.md` §5.6 lines 738-770 — landed Phase 7.1). A.W4 consumes the template; D consumers reference it when the rare escape valve fires. | synthesis + migration |
| Layout lowering | D/F | `@layout` remains parser metadata and does not lower through `LayoutFacts` and BIR. | LayoutFacts test plus BIR `LayoutScope` (`Push`/`Pop`) replay. | synthesis |
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
| PASS-2 BIR snapshots | E/F | BIR snapshots live outside `ir::backend_ir` or fail to feed codegen import-deny tests. | Snapshots committed under `ir::backend_ir`; `BBNF-CODEGEN-IMPORT-DENY` import-deny gate consumes them at every codegen close. | migration |
| Cross-host metadata carrier | A or J body | Cargo.toml's `[workspace.metadata.bbnf]` block is V1 Rust-line; the schema content is host-agnostic but the carrier is Rust-specific. Tranche-body work promotes the schema content to a language-neutral sidecar (e.g., `bbnf.toml`) so future TS / WASM onboarding does not re-invent the carrier. The schema fields lock at V1; the carrier-promotion work happens at A.W4 (sidecar emission alongside the metadata validator) or J body (publication-readiness sidecar finalisation). | A or J body close gate verifies sidecar round-trip equality with the Cargo.toml block; cross-host consumers (`tower-lsp` / wasm-pack / npm scripts) read the same schema content from the sidecar. | synthesis |
| SKELETON triple DELETE REFUSED (per MP-3B-V1-D10 + T-P2 LAC-2F-V5-02 ELEVATION + MP-NW-SK14-SKELETON-DELETE-REFUTED) | Refusal entry itself + Lock 1 v+1 substrate-union amendment at `restart/locks/LOCKS.md:137`-`158` + REDRESS watch-list | The SKELETON triple DELETE (parallel substrate, parallel scanner sidecar, parallel cost-shape) was elevated by T-P2 V3 §3Z LOCK as doubly inadmissible per `restart/audit/totality/p3/T-P3-DISPATCH-CONTEXT.md:26`. Lock 1 v+1 substrate-union amendment at LOCKS.md `:137` generalises REDRESS 96/97/98 PERMANENT-PRE-BLOCK to ALL transient classifier-state primitives. Any future cycle attempting to revive the SKELETON triple DELETE must cite THIS refusal row and provide fresh material differential evidence per `[abrogate-before-patch]`: (a) changed data movement, (b) changed consumer shape, (c) measured row outcome — and pass CHALLENGE re-acceptance + Lock 1 v+1 amendment + new REDRESS entry. The refusal IS the consumer per CH6 anti-paper-close discipline; this row carries forward forever. | omega (T-P3 V4 LOCK; MP-3B-V1-D10) |

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
| Layout errors | Grammar author writing layout-bearing rules. | `@layout` lowers through `LayoutFacts` and BIR `LayoutScope` (`Push`/`Pop`); conflicts are typed errors. | "Why does the layout not nest the way I expected?" | Cookbook page `cookbook/layout.md`. | `BBNF-LAYOUT-CONFLICT` and `BBNF-LAYOUT-UNCLOSED`. |
| Pratt/SIMD decisions | Grammar author wondering why a recognizer was or was not applied. | Pratt and SIMD are auto-detected from grammar shape; metadata can disable but not force. | "Why did Pratt not apply to my expression rule?" | Cookbook page `cookbook/recognizers.md` plus `cargo xtask explain-recognizer`. | `BBNF-PRATT-NOT-APPLIED` and `BBNF-SIMD-NOT-SELECTED`. |
| Crate split migration | Migrating from old workspace shape. | Old `bbnf-path*` and `core` are split into unprefixed crates. | "Where did `bbnf-path` go?" | Cookbook page `cookbook/migration-crate-split.md` plus MIGRATION.md §3.1. | None; this is documentation friction, not a runtime diagnostic. |
| Adding yaml | Grammar author adding a new grammar. | Two surfaces only: `grammars/yaml.bbnf` plus `[workspace.metadata.bbnf.grammars.yaml]`; generated runtime/path/visitor/host outputs and the bench manifest are derivatives. | "Where do I register yaml in Rust?" | Cookbook page `cookbook/add-grammar.md` plus Architecture §12.1 walkthrough and future-grammar test. | `BBNF-METADATA-MISSING-GRAMMAR` and `BBNF-GRAMMAR-NAME-IN-GENERIC-CRATE`. |
| yaml syntax error | Grammar author editing a new yaml grammar under LSP. | The grammar remains admitted while a sample edit is malformed; typed recovery is carried by `DocumentSnapshot`, `TapeId` reuse maps, and recovery facts over the same tape/direct identity. | "Why did the LSP keep a typed `YamlRoot` when indentation is broken?" | Recovery cookbook plus `DocumentSnapshot` trace and `incremental/edit_anchor` ledger. | `BBNF-RECOVERY*` plus debug-only fallback reason when anchors fail. |
| `format()` public method | Library consumer pretty-printing a parsed document. | Every generated runtime exposes `format()` on `DocumentView` and on `OwnedDocument`; dispatch is metadata-driven, reading `@layout` + `@pretty` directives produced by the grammar; the call site authors no formatting policy. | "How do I customise the output? Where does the indent setting live?" | Cookbook page `cookbook/format.md` plus the per-grammar `@pretty` strategy vocabulary (`compact`, `group`, `indent`, `hardbreak`, `sep`, `block`) authored at grammar-source time. | None; formatting is total over admitted documents. Authoring a `@pretty` strategy that disagrees with `@layout` raises `BBNF-LAYOUT-CONFLICT` at grammar-compile time, never at `format()` call time. |

## 25. Implementation Order

The implementation order is:

1. Commit Phase 2 synthesis outputs.
2. Complete Pass Omega convergence and G-Omega before applying any V1.1
   MASTER/LOCKS/HANDOFF/MIGRATION/SKINNY corpus amendments or dispatching
   SK-V13 W0.
3. Start tranche A from a clean worktree.
4. Archive `ser` and `gorgeous`.
5. Create the 24-crate skeleton and metadata validator.
6. Close A with generalization and tree-shape gates.
7. Build B and C before any production lowerer work.
8. Build D before F needs extension-aware generation.
9. Build E before F lowerers.
10. Build F before G future-grammar proof.
11. Build H after recognizer facts and runtime template exist.
12. Build I after recovery facts and runtime views exist.
13. Build J only when parity, SOTA, docs, and publication checks have real
    artifacts to verify.

No implementation tranche starts by editing PASS outputs, prompt contracts,
locks, corpora, inheritance docs, `skinny/RESULTS.md`, or `skinny/REDRESS.md`.
Those documents are inputs. SK-V13 source/generated/gate/result waves remain
blocked until Pass Omega convergence, CRUD authorization, and G-Omega.

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

## 27. Phase 8.4 Simplification Fold Ledger (trio)

Phase 8.4 absorbs the V8 simplification candidates routed to the MASTER-PLAN trio (`restart/ARCHITECTURE.md`, `restart/MASTER-PLAN.md`, `restart/MIGRATION.md`) per `restart/audit/hardening/HARDENING-CONSOLIDATED-V8.md` §3 and `restart/audit/hardening/HARDENING-MASTER-PLAN-V8.md` §7. Classification follows the verify-then-patch discipline — full-author surfaces a fresh paragraph, patch-delta amends an extant clause in place, verify-only-stub records that the amendment lands elsewhere with the trio's cross-references intact. Phase 8.3.1 user adjudications already absorbed Q1 (GADT V1 surface), Q2 (CHR V1 fold inside csp-solver), and Q3 (function composition library DELETE); the V8 ledger entries that originally routed those items to V2 are RETIRED at this fold.

| V8 # | Tier | Surface | Classification | Disposition |
|---|---|---|---|---|
| α1 | architectural cardinality | `ARCH §7.5:1083-1113` Backend trait 5 methods → 2 (`lower` + `emit_artefacts`); `ArtefactSet` typed file tree replaces the four `*Output` types | patch-delta | trait code block + obligations table collapse; per-grammar artefact files remain distinct on disk; PASS-2 fold owns the codegen-side text update |
| α2 | architectural cardinality | `ARCH §8.2:1278-1289` type-system stack 7 → 5 mechanisms; HM-equality (Algorithm-W) + Pierce-Turner local check/synth + DK13 algorithmic completeness + finite CSP + GADT-refinement (post-Phase-8.3.1 V1 fold); CHR-improvement (post-Phase-8.3.1 V1 fold) integrates as constraint-emission helper inside csp-solver, not a separate type-system layer | patch-delta | §8.2 prose paragraph collapses three names of one algorithm; CHR text moves to constraint-emission helper phrasing |
| α3 | architectural cardinality | `ARCH §7.2:894-984` BIR alphabet 22 → 19; three semantically-redundant pair collapses — `(LayoutPush, LayoutPop) → LayoutScope { kind }`, `(DispatchAlt, SpeculativeAlt) → Alt { mode }`, `(CallHost, HostChain) → CallHost` (chain expresses as `Seq` of `CallHost`) | patch-delta | variant table + payload table + example table collapse; PASS-2 fold carries the per-variant prose |
| α5 | architectural cardinality | `ARCH §10.1:1444-1449` rewrite-budget 4 → 3 categories; fold `simplification-rewrites` into `codegen::verify` (no e-graph need; one-pass dead-mark elision belongs alongside regen-equality at F.W3); add LOAD-BEARING vs ASPIRATIONAL labels per Lens K | patch-delta | §10.1 budget table loses the fourth row; following paragraph reframes thresholds as three-category contract |
| β1 | diagnostic vocabulary | `ARCH §7.4:1032-1063` retire numeric alias system (`BBNF-LIFE001`, `BBNF-LIFE002`, `BBNF-VISIT002`, `BBNF-LAYOUT002`, `BBNF-OPT001`, `BBNF-OPT002`, `BBNF-PATH001`, `BBNF-PATH002`, `BBNF-GRAMMAR001`, `BBNF-CG001`); pure-numeric codes (`BBNF-LIFE003`, `BBNF-LIFE009`, `BBNF-VISIT001`, `BBNF-VISIT003`, `BBNF-LAYOUT001`, `BBNF-PATH003`, `BBNF-HOST001`, `BBNF-HOST002`, `BBNF-HOST003`, `BBNF-GEN014`, `BBNF-CODEGEN021`, `BBNF-CODEGEN033`, `BBNF-SEM040`) get human-readable replacements | patch-delta | catalogue table edits; cookbook references at §24 already use human-readable forms; §10.1 paragraph that emits OPT001/002 cites human-readable codes |
| γ10 | host-language leverage | `ARCH §5:739-745` Cargo.toml workspace metadata cross-host carrier; V1 carrier remains Cargo.toml; sidecar promotion documented in MASTER-PLAN §24 carry routed to tranche A or J body, not V2 amendment | patch-delta | one-line note at §5; new MASTER-PLAN §24 carry row routes the cross-host metadata-carrier work to tranche-body |
| δ8 | tranche-body routing | `MASTER-PLAN §4:131-141` SOTA gates; SOTA-parity is correctness floor (V1 close at J.W1); SOTA-beat is audacious aspirational at tranche-H body | patch-delta | one-sentence cite below the SOTA-row table at §4 distinguishes parity-floor from beat-aspiration; H tranche §13 wave routing (SK-V3 re-routed) binds H.W1 (typed event cursor) / H.W2 (Class A/B NEON) early gates and J.W1 final gates |
| δ9 | RETIRED Phase 8.3.1 | function composition library V2 amendment | verify-only-stub | Phase 8.3.1 Q3 user adjudication DELETED the library entirely; V1 function-value surface absorbs every composition use case via inline closure expression; trio carries no V2 row |
| δ10 | RETIRED Phase 8.3.1 | CHR-improvement layer V2 amendment | verify-only-stub | Phase 8.3.1 Q2 user adjudication FOLDED CHR-improvement into V1 csp-solver as constraint-emission helper; trio carries no V2 row; α2 above amends §8.2 to reflect the V1 fold |
| ε5 | hygiene | `MASTER-PLAN §24:769-791` carry ledger sweep; no V2 amendment row remains for V1-folded items; ASPIRATIONAL items route to tranche bodies (H body for SOTA-beat, D body for DK13 rank-N + schema-miner telemetry, I body for DAP/LSP/incremental + reuse-map cookbook) | patch-delta | §24 surveys for stale "V2 amendment" rows and re-routes; the three surviving V2-amendment rows (TS production, `path-ts` schema, WASM ABI, `path-ts` package publication timing) all bind to live V2 backends per Lock 5 + Lock 11, not retired V1-folds |

Items not routed to the trio at this fold:

- α4 (Grammar-IR `Map` + `HostCall` merge), α6 (three-path generic-cycle validation), α7 (`BackendLowerer` 8-method clarification), β2 + β3 (`BBNF-OPT` cookbook-only + OpenFrame rename), γ1-γ9 (host-leverage reframings), δ1-δ2 (DK13 rank-N + schema-miner telemetry), δ5-δ7 (DAP/LSP/incremental body), ε1-ε4 (PASS-2 ε hygiene rows) — owned by PASS-1 / PASS-2 / PASS-3 sister fold agents.
- β3 (OpenFrame rename) and α7 (BackendLowerer clarification) and V8-P11 (parse API V2 cross-host note) and V8-P9 (ARCH §5 sidecar note) and V8-P7 (LowerContext side-table SIMPLIFY) — the trio touches §5 (γ10 and §24 rows) and §11 (gate-owner row) where their text intersects MASTER-PLAN coherence; ARCH-internal text amendments live with the SYNTHESIS amendment commit.

The patch-delta amendments compose without cross-target conflict: α1 (Backend trait shape) + α3 (BIR alphabet) + α5 (rewrite-budget) + β1 (diagnostic vocabulary) edits land in disjoint sections of `restart/ARCHITECTURE.md`; α2 (type-system stack) edits §8.2 only; γ10 + ε5 edit §24 carry rows; δ8 edits §4 of MASTER-PLAN.
