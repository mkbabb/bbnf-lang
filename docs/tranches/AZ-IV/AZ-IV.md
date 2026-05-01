# AZ-IV - Canonical Activation

AZ-IV completes the AZ-III carry burn-down by making the checked-in tree, regenerated tree, runtime parser surface, optimization substrate, sibling topology, and benchmark evidence describe one system. It is also lossless with respect to BA and BB: old BA/BB file layouts may be retired, but their functional requirements must either land inside AZ-IV with evidence or be retired by an explicit artefact-backed decision.

## Thesis

The system already chose the right architecture: grammar-derived Rust struct graphs are the materialized parse form, `cargo xtask regen` is the canonical Rust generation path, and CSP/egraph/shape/Pratt/regex/SIMD/view support exist to make that path general and fast. AZ-IV closes the remaining gap by consuming or deleting every existing substrate. No derive revival, tape facade, generated tape view layer, parent-pointer sidecar, or compatibility shim is allowed. BA's typed path/query product is implemented through the existing runtime/document/type-inference surface, not through a second parser or shadow path system.

## Invariants

1. **One parse path**: generated StructDirect parsers are the Rust runtime path. Tape, bootstrap parser, derive-generated parser, and DTA walker fallbacks stay retired.
2. **Grammar generality**: parser binding, shape dispatch, regex payloads, typed projections, and backend strategy selection derive from grammar/manifest facts, not literal parser names or JSON-family assumptions.
3. **Substrate with consumer**: rewrites, ruler, CSP decisions, regex HIR/egraph facts, shape dictionaries, structural scan, Pratt metadata, and views either change generated/runtime behavior in the same wave or are deleted/retired.
4. **Semantic parity is type-inference driven**: `TypeDesc`, `StructRegistry`, obligations, grammar facts, and generated projection tables define parity. Hand-coded normalizers, rule-name dispatch tables, host-shim duplicates, and synthetic payload defaults are supplementary diagnostics, not parity proof.
5. **Semantic parity is current**: JSON uses sonic-rs as oracle, CSS uses lightningcss, Sheets uses the full parity corpus, BBNF self-host uses generated BbnfBootstrap, and TS is executable rather than string-checked.
6. **Direct struct projection must perform**: parse-only speed is not enough. StructDirect document/value/path projection rows must beat or match same-harness competitor rows where a competitor exists, especially sonic-rs JSON value/path access.
7. **No legacy code**: stale DTA/walker/tape wording, dead functions, compat exports, no-op emitter hooks, and fallback-to-JSON surfaces are wave-owned deletion/refactor targets.
8. **Evidence closes gates**: no gate closes on API existence, grep-only runtime claims, disabled tests, or "consumer later" scaffolding.

## Carry Ledger

| Carry | Source | AZ-IV destination | Close condition |
|---|---|---|---|
| Strict regen drift | AZ-III C1 | W0 - Truth And Canonical Regen | `cargo xtask regen --check` green live for all manifest grammars; parity run against regenerated tempdir output |
| Egraph `Map` stripping | AZ-III C2 | W0 - Truth And Canonical Regen | extraction preserves typed `Map { fn_id }` when payload semantics require it |
| Sheets parity gap | AZ-III C3 | W1 - Runtime Surface And Semantic Parity | full Sheets parity surface green, including string, array, range, and prefix cases |
| TS discriminated union | AZ-III C4 | W1 - Runtime Surface And Semantic Parity | TS backend emits, typechecks, and executes representative grammars |
| Tailwind regex timeout | AZ-III C5 | W2 - Optimization Substrate Activation | emitted scanner path resolves timeout class without per-call map overhead |
| Watchdog bench rows | AZ-III C6 | W3 - Measurement And Close | fat-LTO and bench-iter matrices have no unresolved watchdog rows |
| WASM/derive residue | second hardening | W0 - Truth And Canonical Regen | no production `bbnf_derive` or deleted `crates/derive` references in active workspace/wasm/sibling paths |
| BA typed path/query product | BA | W1 - Runtime Surface And Semantic Parity | existing `runtime::path`, `path!`, and per-document `*PathQuery` surfaces become type-inference driven, zero-allocation, and benchmarked against sonic-rs/simdjson where comparable |
| BA host-binding isomorphism | BA.W2 | W1 - Runtime Surface And Semantic Parity | TS path/parser binding executes from generated output; every missing Python/host path binding has an explicit no-surface decision or lands with isomorphic signatures/errors |
| BB rewrite/ruler program | BB | W2 - Optimization Substrate Activation | Ruler enumeration, egraph residue, VM oracle, ranker/tiering, schema/provenance, grammar rewrite dirs, and CI/review ledgers are either production-wired or explicitly retired with evidence |
| Rewrite/ruler substrate unconsumed | second hardening | W2 - Optimization Substrate Activation | every non-empty loaded ruleset proves load, search/apply, extraction, writeback, generated diff, and oracle/bench evidence |
| Full substrate denominator | third hardening | W2 - Optimization Substrate Activation, W3 - Measurement And Close | every mined fact, sidecar, rule, template, shape, scan, Pratt, view, regex, CSP, and egraph decision has generated/runtime evidence or is deleted |
| Post-AU and sonic-rs performance floor | AU through AZ-III | W3 - Measurement And Close | fat-LTO `post-AZ-IV.json` beats post-AU/post-AZ floors row-by-row and same-harness JSON projection rows are parity-or-better against sonic-rs |

## Wave Table

| Wave | Agents | Closes on evidence | Status |
|---|---:|---|---|
| W0 - Truth And Canonical Regen | 5 parallel | workspace/doc truth, strict regen, manifest strategy, `Map` preservation, metadata gates | planned |
| W1 - Runtime Surface And Semantic Parity | 5 parallel | typed path/query product, type-inference projection parity, CSS/Sheets/JSON/BBNF/TS parity, shape generality | planned |
| W2 - Optimization Substrate Activation | 5 parallel | full rewrite/ruler program, CSP/regex authority, shape_dict/SIMD, Pratt/view, legacy deletion | planned |
| W3 - Measurement And Close | 3 parallel | post-AU/post-AZ/sonic-rs performance floors, substrate denominator ledger, workspace gates, close docs | planned |

## Critical Files And Ownership

| Surface | Owner wave | Primary paths |
|---|---|---|
| Active plan truth | W0, W3 | `docs/GESTALT.md`, `docs/codegen-paths.md`, `docs/tranches/REMAINING-TRAJECTORY.md`, `docs/tranches/BA/**`, `docs/tranches/BB/**`, `docs/tranches/AZ-IV/**` |
| Regen and manifest binding | W0 | `xtask/src/regen.rs`, `Cargo.toml`, `crates/ir/src/registry/strategy.rs`, `crates/core/src/grammar/generated/**` |
| Egraph payload preservation | W0, W2 | `crates/ir/src/egraph/**`, `crates/egraph/src/**`, `crates/ir/src/rewrites/**` |
| Runtime path, view, projection | W1 | `crates/core/src/runtime/path.rs`, `crates/core/src/runtime/view.rs`, `crates/core/src/runtime/*/document.rs`, `crates/core/src/runtime/css_l4/**`, `crates/core/src/backend/rust/view/**`, `crates/core/benches/json/value.rs` |
| Grammar parity | W1 | `grammar/**`, `crates/core/tests/*parity*.rs`, `crates/core/tests/backend_ts.rs`, `crates/core/tests/pipeline_compile_request.rs` |
| CSP/regex/shape/Pratt/SIMD | W2 | `crates/ir/src/passes/csp_strategy/**`, `crates/core/src/generate/regex/**`, `crates/core/src/backend/rust/emitter/shapes/**`, `crates/simd-scan/**` |
| Benchmark and profiling | W3 | `crates/core/benches/**`, `scripts/*bench*`, `scripts/profile-bench-headless.sh`, `docs/benchmarks/AZ-IV/**`, `docs/benchmarks/post-AU.json`, `docs/benchmarks/post-AZ-*.json` |
| Sibling topology | W0 | `wasm/**`, `/Users/mkbabb/Programming/parse-that/**`, `/Users/mkbabb/Programming/pprint/**`, `.cargo/config.toml`, package locks |

## Orchestration Rules

1. Max six agents per wave. Older ten-agent text is historical and non-normative.
2. Parallel writers use named sibling worktrees with distinct `CARGO_TARGET_DIR`; no parallel implementation or docs-writing agent writes in main.
3. The orchestrator records `git status --short`, staged paths, base commit, worktree list, and target dirs before dispatch.
4. Empty/null/no-evidence return is a failed dispatch: redispatch verbatim once with the same worktree pointer; a second empty/no-evidence return triggers the triumvirate.
5. Triumvirate means research, plan augment/synthesis, and redress. It is mandatory for scope reveal that invalidates file bounds, hard gates, or substrate-with-consumer wiring.
6. Read-only agents do not commit at hard cap. Write-authorized agents commit at 0.9N only when the staged slice is clean and owned.
7. Before every commit, use the local `commit-discipline` skill: inspect dirty/staged state, preserve unrelated staged work, stage only intended paths, review `git diff --cached`, and stop if the slice cannot be isolated.
8. Broad, generated, deletion, benchmark, profiling, gate/status, and cross-repo commits require bodies naming why, what landed, evidence, and routed remainder. No AI/tool authorship.
9. Profiling agents may share one prepared absolute target only after preparation; no two cargo invocations run concurrently against the same target dir.

## Hard Gates

1. `cargo xtask regen --check` passes live for all manifest grammars and is archived under `docs/benchmarks/AZ-IV/`.
2. Parser strategy binding is manifest/registry driven; a synthetic grammar rename/addition test fails if a new literal parser-name arm is required.
3. Regenerated tempdir outputs run the parity matrix; checked-in freshness and runtime parity cannot be proven by separate stale artefacts.
4. JSON, CSS, Sheets, BBNF, and TS parity gates are current, green, regenerated-output based, and type-inference driven. CSS/Sheets parity cannot close on early-return payload gaps, hand normalizer equivalence, rule-name projection, or synthetic default payloads.
5. Egraph extraction preserves semantic wrappers such as `Map { fn_id }`; a named test fails if extraction strips typed payloads.
6. BA path/query requirements close inside the existing runtime surface: `path!`, `Path`, `PathSegment`, and every active `*PathQuery` trait are type-inference checked, zero-allocation on traversal, externally benchmarked, and host-binding status is explicit.
7. Every non-empty loaded rewrite/ruler ruleset traverses the full production chain: RON load, egraph search/apply, VM-residue oracle where egraph is silent, rank/tier, extraction, writeback, generated Rust diff, oracle proof, and benchmark/parity non-regression.
8. CSP decisions that select a regex/layout/dispatch engine are reflected at emitted consumers or the dead decision surface is deleted. Sidecars may carry payloads after CSP selection; they may not choose strategy.
9. Every active shape, miner, Pratt, view, structural scan, regex HIR, CSP, and egraph fact is in the denominator ledger and has generated/runtime consumer evidence or deletion proof.
10. Legacy audit closes: `emit_dfa_inline_body`, DTA walker/tape wording, old color compatibility, fallback-to-JSON substrate path, discarded Rust per-rule compile work, derive/bootstrap residue, duplicated host shims, stale package locks, and sidecar authority are deleted, renamed, or justified by current consumers.
11. Fat-LTO `post-AZ-IV.json` carries row-by-row post-AU floor, post-AZ same-profile deltas, AZ-III bench-iter deltas, status, and pass/fail. No row may be watchdog-routed.
12. Same-harness JSON direct struct projection rows close parity-or-better against sonic-rs (`bbnf_value_* <= sonic_value_*` on time, or equivalent throughput ratio). Parse-only rows cannot satisfy projection performance.

## Cross-Tranche Debt

AZ-IV absorbs BA and BB functionally while rejecting their stale or contradictory mechanisms. BA's typed path/query requirements land through the existing runtime/document/type-inference surface. BB's rewrite/ruler requirements land through the existing `crates/ir/src/rewrites`, `crates/egraph/src/ruler`, `xtask`, and egraph pipeline. BC.W5/W6 debug/minimise tooling is not opened unless W3 proves a close blocker that needs it; if so, it enters a named scope-reveal ledger before implementation. If any BA/BB item cannot land inside AZ-IV without changing the thesis, `FINAL.md` must name the exact successor destination and cite the artefact that proves why it cannot be absorbed.

## Brittleness Window

No tranche-wide brittleness window is declared. A wave may declare a local brittleness window only in its wave spec, with suspended gates, restoration wave, and reason. AZ-IV cannot close while any brittleness window is open.
