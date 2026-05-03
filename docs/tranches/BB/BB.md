# Tranche BB — Egraph Rule Inference + Ruler + VM Oracle + Ranker

**Status**: planned. Opens after BA close.
**Base**: master (post-BA close commit).
**Letter discipline**: un-subsumed at master `40092b28`. The previous BB tranche (subsumption banner pointing at AZ-IV + recycled-BA) is archived at `docs/tranches/BB/historical/subsumed/`. The recycled-BA plan that holds the rule-discovery scope is preserved at `docs/tranches/BA/historical/recycled-rule-discovery/BA-rule-discovery.md` and is the predecessor reading. BB inherits its scope verbatim.

## Thesis

BB closes the loop on `feedback_pluggable-components`, `feedback_csp-always-optimize`, and `feedback_general-infra-crates` by letting the e-graph *discover* grammar-level rewrite rules rather than only apply a fixed set. Ruler-style CVC enumeration over `IrNode` produces candidate pairs `(L, R)`; the e-graph itself is the fast-path equivalence check; the surviving VM interpreter serves as the non-circular ground-truth oracle on the *residue* — candidates the e-graph cannot decide — and nothing more. An automatic ranker scores every surviving candidate; a tiered review pipeline auto-accepts the trivial class, fast-tracks the structural class, and reserves full human review for the novel class only. Rules live outside `crates/core`: fleet-wide rules in `crates/ir/src/rewrites/`, grammar-specific rules colocated with each grammar under `grammar/<name>/rewrites/*.ron` via a standardised schema `cargo xtask regen` scans at IR-pipeline time and compiles into that grammar's cost-config.

## Active Contradictions / Architectural Defect

The substrate exists but the *discovery* mechanism does not:

1. **`crates/ir/src/rewrites/` does not exist** at BA close. AZ-IV.W4 deleted the unconsumed `RuleSet` field from `pipeline.rs::CompileOptions::rewrites` and the unconsumed `egraph::ruler::*` skeleton; BA.W0 confirmed the deletion. BB recreates the rewrites tree clean — schema, registry, ranker, tiering, miner — without inheriting the `eprintln`-sink anti-pattern.
2. **Tranche H rules are hand-coded.** `factor`, `merge_regex_alts`, `inline_acyclic` were hand-written. BB does not hand-code; BB *discovers*. Every rule that persists was produced by enumeration, survived oracle validation, and cleared the ranker tiering. Tranche H rediscovery ≥ 80% is the soundness gate.
3. **The e-graph is the fast path; the VM is the residue oracle.** An e-graph that already contains both `L` and `R` in the same class proves their equivalence without any external call. The VM runs only when the e-graph is silent — `L` and `R` belong to different classes under the current rewrite set. Empirically (per Ruler / Enumo), >90% of candidate pairs are captured or redundant once even a small seed ruleset is in place; the VM workload is sized to the residue, not total enumeration.
4. **No backward compatibility, no warm benchmarks, no stub.** Per the user's voice precept and `feedback_no_workarounds`: every BB substrate is consumed in the same wave it lands. The substrate-audit test is the gate.
5. **Storage is grammar-colocated and extensible.** Adding a grammar `foo` with three custom rewrites requires creating `grammar/foo/rewrites/{r1,r2,r3}.ron`. That is the whole delta. No `crates/core` edit. No regen-emitter edit. No hand-authored registry.

## Invariants

(BB-scoped; AZ-IV + BA invariants persist + are extended.)

1. **Discovery, not authorship.** Every persisting rule was enumerated, oracle-validated, and ranker-tiered. Hand-coded rules in production code are forbidden after BB close; existing Tranche H rules are absorbed into the discovered set or retired.
2. **E-graph fast path; VM residue oracle.** The e-graph is the proof substrate; the VM runs only on residue. The VM stays narrow: ~1800 LOC at BA close; BB neither grows it nor revives the deleted token-dispatch walker.
3. **Storage is grammar-colocated.** Fleet-wide rules in `crates/ir/src/rewrites/`; grammar-specific rules in `grammar/<name>/rewrites/*.ron`. `crates/core` never accumulates a hand-curated rule list.
4. **Ranking + tiering is first-class.** Class 1 (trivial / algebraic / rediscovered) auto-accepts with audit log only. Class 2 (structural resemblance to hand-coded patterns) fast-tracks. Class 3 (novel) is the only class that consumes human review time.
5. **Rule admission chain is end-to-end.** A rule is not admitted until: schema validation → live registry → e-graph search/apply → extraction chooses it → `write_back_optimized` changes `GrammarIR` → expanded Rust hot path changes → fixture and bench/proof move. Per-rule report shows `search > 0`, `apply/work > 0`, extraction selected the new form, and generated code changed in a parser hot path.
6. **No regression on `post-BA.json` close matrix.** Any BB-accepted rule that regresses the close matrix reverts the rule batch.
7. **Substrate-audit GREEN.** Every new BB substrate (rewrites/, ruler/, ranker, schema) passes the permanent test at every wave close.

## BA Dependency (hard opening gate)

BB opens after BA close. The opening contract:

1. **Direct-projection codegen GREEN.** BA closes the value-API direct-projection thesis; BB consumes the typed StructRegistry output for IR-rewrite candidate enumeration.
2. **`crates/ir/src/rewrites/` does not exist** at BA close (BA.W0 deleted it; BB.W0 recreates it clean — no `eprintln`-sink anti-pattern inherited).
3. **`StructRegistry` populated for JSON / CSS L4 / Sheets / BBNF.** Verified at BA close per `docs/tranches/BA/audit/W2-emit-document.txt`. The enumerator and oracle consume the same registry.
4. **Tape path fully deleted.** Verified at BA close. Direct-to-struct substrate stable; the enumerator targets the struct tree.
5. **TypedPath<G, T> + `path!` macro live + lazy parse on 4 grammars** — BA hardens these into the canonical value-API path; BB consumes them for path-rewrite enumeration.
6. **Workspace nextest 100% pass** at BA close.
7. **`cargo xtask regen --check` green 9/9** at BA close.
8. **Permanent `crates/ir/tests/substrate_audit.rs` test passing** — every BB substrate (rewrites/, ruler/, ranker, schema) must pass it at every wave close.

If any of these is not true at BA close, BB does not open. The carry routes back to BA per the non-routable-carries discipline (a non-routable item that survives close is a process failure; the response is triumvirate review of BA thesis, not a successor letter).

## Carry Ledger — BA Routed Items

| Carry | Source | BB destination | Close condition |
|---|---|---|---|
| F4 Tailwind regex_scan timeout | BA.W2 routing decision | W3 (grammar-specific rule discovery) | path-shape rewrite candidate cleared by oracle; CSS L4 close matrix shows the regex_scan row at-or-above |
| `merge_path_seed` rules as seed bag | BA.W0 decision | W1 (Ruler CVC enumerator) | the W3.0 path-shape rewrites are consumed as enumeration seeds; OR the BA.W0 deletion stands and BB.W1 enumerates fresh |

## Non-Routable Carries

The 18 items below cannot route to a successor letter. BB closes inside these or BB does not close.

| # | Item | Owner wave | Closure proof |
|---|---|---|---|
| 1 | `crates/ir/src/rewrites/` recreated clean | W0 | module exists; schema validates against base RON files; no `eprintln`-sink anti-pattern; every substrate has a wave-bound consumer |
| 2 | Ruler-style CVC enumerator | W1 | `crates/egraph/src/ruler/enumerate.rs` exists; produces candidate pairs `(L, R)` over `IrNode` alphabet up to bounded size; named test fixture |
| 3 | E-graph residue oracle wrapper | W2 | `crates/egraph/src/ruler/residue.rs`; e-graph-first equivalence; routes residue to VM oracle |
| 4 | VM oracle on residue | W2 | `crates/egraph/src/ruler/oracle.rs`; per-candidate budget; fixture-corpus byte-compare; no walker resurrection |
| 5 | Automatic ranker | W3 | `crates/ir/src/rewrites/rank.rs`; signals: match frequency, cost delta, generality, similarity-to-known, novelty, tree size |
| 6 | Class-1/2/3 tiering | W3 | `crates/ir/src/rewrites/tiering.rs`; classifier with target ≥ 90% Class 1 + 2 |
| 7 | RON rule-file schema validator | W4 | `crates/ir/src/rewrites/schema.rs`; rejects malformed RON with file/line diagnostics |
| 8 | Grammar-colocated `grammar/<name>/rewrites/*.ron` | W4 | `cargo xtask regen` discovers and compiles per-grammar rule files without per-grammar code edits; named test |
| 9 | Tranche H rediscovery ≥ 80% | W1 + W2 | `crates/egraph/tests/tranche_h_rediscovery.rs` GREEN; named rule-name list shows ≥ 80% match |
| 10 | Retained-rule corpus hit-rate ≥ 0.1 per parse | W3 | `docs/benchmarks/post-BB-W3-hit-rate.json`; samply attribution proving firings register on parse hot path |
| 11 | ≥ 5 accepted rules per primary grammar | W3 | per-grammar rule count in `docs/benchmarks/post-BB.json`; JSON / CSS L4 / Sheets / BBNF each ≥ 5 |
| 12 | Generated `.rs` shrinks ≥ 10 LOC for one grammar | W4 | per-grammar LOC delta in `docs/tranches/BB/audit/W4-loc-delta.txt`; at least one grammar shows the shrink |
| 13 | Throughput gain on `post-BA.json` close matrix | W5 | `docs/benchmarks/post-BB.json` shows ≥ 1 grammar with measurable throughput gain |
| 14 | No regression on `post-BA.json` close matrix | W5 | every row in `post-BB.json` matches or beats `post-BA.json` |
| 15 | E-graph node-count ceiling | W1, W2, W3 | per-wave size bound; fail-fast if `EGraphSolver::node_count()` exceeds declared ceiling; alphabet narrows on overflow |
| 16 | Class 1 auto-accept audit log | W4 | `docs/rules/audit-log.ndjson` has signed entry per Class-1 rule |
| 17 | Class 3 reviewer-signed rationale | W5 | every Class-3 rule has `docs/rules/<rule-id>.md` with rationale |
| 18 | Substrate-audit GREEN at every wave close | W0..W6 | `crates/ir/tests/substrate_audit.rs` passes after each wave |

## Wave Table

| Wave | Agents | Closes on evidence | Status |
|---|---:|---|---|
| W0 - Substrate Preflight | 5 parallel | regen drift cleared; cost extractor live; `crates/ir/src/rewrites/` recreated clean; schema validator; base RON rules land; substrate_audit GREEN | planned |
| W1 - Ruler CVC Enumerator | 5 parallel | `crates/egraph/src/ruler/enumerate.rs` produces candidate pairs over `IrNode` alphabet; bounded size; e-graph residue wrapper begins; Tranche H rediscovery starts | planned |
| W2 - VM Oracle On Residue | 5 parallel | `crates/egraph/src/ruler/oracle.rs` runs candidate `L, R` over fixture corpus; byte-compare tape output; per-candidate budget; Tranche H rediscovery ≥ 80% green | planned |
| W3 - Ranker + Tiering | 5 parallel | automatic ranker scores every surviving candidate; Class-1/2/3 classifier; ≥ 5 accepted rules per primary grammar; corpus hit-rate ≥ 0.1 | planned |
| W4 - Grammar-Colocated Rewrite Dirs | 5 parallel | `grammar/<name>/rewrites/*.ron` schema; `cargo xtask regen` integration; base rules land; generated `.rs` shrinks ≥ 10 LOC for one grammar | planned |
| W5 - Review Ledger + CI | 5 parallel | `docs/rules/audit-log.ndjson` signed; Class-3 rationale per rule; throughput gain on `post-BA.json` matrix; no regression | planned |
| W6 - Measurement And Close | 3 parallel | `post-BB.json` per SPEC.md; substrate-audit GREEN; samply 7-artefact contract per perf claim; FINAL.md | planned |

## Critical Files And Ownership

| Surface | Owner wave | Primary paths |
|---|---|---|
| Substrate preflight + cost extractor + schema | W0 | `crates/ir/src/rewrites/{mod,schema,base/*.ron}.rs`, `crates/ir/tests/substrate_audit.rs` |
| Ruler CVC enumerator | W1 | `crates/egraph/src/ruler/enumerate.rs`, `crates/egraph/src/ruler/residue.rs` |
| VM oracle | W2 | `crates/egraph/src/ruler/oracle.rs`, `crates/egraph/tests/oracle_*.rs`, `crates/egraph/tests/tranche_h_rediscovery.rs` |
| Ranker + tiering | W3 | `crates/ir/src/rewrites/{rank,tiering}.rs`, `crates/ir/tests/rank_*.rs` |
| Grammar-colocated rewrites | W4 | `grammar/<name>/rewrites/*.ron`, `xtask/src/regen.rs` (modify-carve), `crates/core/src/rewrites/mod.rs` (IR-pipeline scan + compile) |
| Review ledger + CI | W5 | `docs/rules/audit-log.ndjson`, `docs/rules/<rule-id>.md`, `docs/rules/runs/<run-id>.md` |
| Benchmark + profiling | W6 | `crates/core/benches/**`, `docs/benchmarks/post-BB.json`, `.profiles/samply/post-BB/**`, `docs/tranches/BB/FINAL.md` |

## Orchestration Rules

(Same as BA: max six agents per wave; parallel writers use named sibling worktrees; orchestrator records `git status` before dispatch; empty-return = redispatch verbatim once then triumvirate; HARD CAP 30 min default; triumvirate auto-triggers per ORCHESTRATION.md §Triumvirate Auto-Triggers; sub-agent prompts ≤ 700 words.)

## Hard Gates

1. `cargo xtask regen --check` 9/9 green at every wave close.
2. `cargo nextest run --workspace --cargo-profile ax-iter` 100% pass at every wave close.
3. **Tranche H rediscovery ≥ 80%**: `crates/egraph/tests/tranche_h_rediscovery.rs` GREEN; named rule list shows ≥ 80% of `factor`, `merge_regex_alts`, `inline_acyclic` (and any other Tranche H hand-coded rules) rediscovered by enumeration.
4. **Retained-rule corpus hit-rate ≥ 0.1 per parse**: `docs/benchmarks/post-BB-W3-hit-rate.json` records firings per parse averaged across the 4 primary grammars; rules below the floor retire per the e-graph cost model.
5. **≥ 5 accepted rules per primary grammar**: JSON / CSS L4 / Sheets / BBNF each have ≥ 5 accepted rules at BB close.
6. **Generated `.rs` shrinks ≥ 10 LOC for one grammar**: `docs/tranches/BB/audit/W4-loc-delta.txt` records the delta; at least one grammar shows the shrink.
7. **Throughput gain on `post-BA.json` close matrix**: `docs/benchmarks/post-BB.json` shows ≥ 1 grammar with measurable throughput gain.
8. **No regression on `post-BA.json` close matrix**: every row in `post-BB.json` matches or beats `post-BA.json`.
9. **Class 1 auto-accept audit log**: `docs/rules/audit-log.ndjson` has a signed entry for every Class-1 rule.
10. **Class 3 rationale per rule**: every Class-3 rule has `docs/rules/<rule-id>.md` with reviewer-signed rationale.
11. **Class distribution**: > 90% of accepted rules classify as Class 1 or Class 2 across all shipped waves.
12. **`crates/ir/src/rewrites/` compiles standalone**; schema validator rejects malformed RON with file/line diagnostics.
13. **`cargo xtask regen` discovers and compiles `grammar/<name>/rewrites/*.ron`** for every grammar without per-grammar code edits.
14. **Permanent `crates/ir/tests/substrate_audit.rs` test passes** — every BB substrate has a production caller.
15. **Oracle rejection rate ≤ 50% per wave**; higher rejection indicates alphabet drift; the alphabet narrows before the next wave.
16. **E-graph node-count ceiling per wave**; crossing reverts the enumeration alphabet at wave close.
17. **Samply 7-artefact contract per perf-citing Hard Gate**.
18. `cargo fmt --all -- --check`, focused `cargo clippy --profile ax-iter`, and `git diff --check` pass at every wave close.

## Deletion Bias

BB deletes before adding. Forbidden patterns:

- no hand-coded rule landing in production after BB.W0 (Tranche H rules are absorbed or retired)
- no `eprintln`-sink anti-pattern in `crates/ir/src/rewrites/` (per `feedback_no_workarounds`)
- no `RuleSet` re-resurrection from AZ-IV-deleted `pipeline.rs::CompileOptions::rewrites`
- no walker resurrection (the AX.W0b-deleted token-dispatch walker stays deleted)
- no per-grammar registry hand-curation (the discovery is the source)
- no rule that fails the e-graph cost model auto-accepting (Class 3 review is the gate)
- no `*_v2` modules, no compatibility feature flags

## Cross-Tranche Debt

- **BB opens after BA close**.
- **BC opens after BB close** as the cleanup pass.
- **BD+ reserved** for TS/WASM re-engineering.

If a non-routable item cannot land inside BB without changing the BB thesis, the response is a triumvirate review of the thesis — not a new tranche letter.

## Brittleness Window

No tranche-wide brittleness window. A wave may declare a local brittleness window only in its wave spec.
