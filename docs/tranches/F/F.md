# Tranche F — Optimiser Pipeline

## Gestalt

Tranche F lands the per-domain orthogonal optimisation pipeline per Lock 4. CSP type/layout inference, e-graph rewriting, pattern miners, shape analysis, and cost model compose by output-piping — no unified hypergraph. Each lives in its own crate (egraph + csp-solver path-deps until stable per Lock 11; tranche I handles publication). The cost-model output-piping lands per Pass B Agent B.5 §6 — the cost model stays in egraph per `feedback_kiss-perf-bias`, but its output pipes to the codegen substrate to drive Pratt + SIMD auto-detection per Lock 10. No `@pratt`/`@simd` directives — the optimiser mines grammar shape (left-recursive operator chains → Pratt) and leaf-pattern shape (charclass / keyword set / regex → SIMD scanner) and emits accordingly.

Per Lock 10's verification mandate: `rg '@pratt\|@simd' grammar/` returns 0; the optimiser's auto-detection runs over every grammar in workspace metadata. The `pratt_eligibility = "auto" | "force" | "skip"` and `simd_eligibility = "auto" | "force" | "skip"` per-grammar metadata knobs escape misclassification.

Same-wave consumer for each optimiser tier ensures Era V's failure mode does not repeat. The cost model emits to the codegen IR; the e-graph rewrite rules emit to the codegen IR; the CSP solver's layout decisions emit to bbnf-ir's `LayoutRegistry`. Every substrate landing in F has a same-wave consumer in F itself or in the per-grammar declaration crates from E.

## Hard gates

| Gate | Wave | Verification |
|---|---|---|
| Per-domain orthogonal verification | F.W2 | `cargo tree -p bbnf-passes` shows egraph + csp-solver as orthogonal sub-deps; no fused-hypergraph crate exists |
| Cost-model output-piping | F.W3 | egraph's cost-model output consumed by `bbnf-codegen-ir::ir::lower::cost_decisions`; per-grammar regen reflects cost-driven decisions |
| Pratt auto-detection | F.W4 | grammar with left-recursive operator chain (math.bbnf) emits Pratt-eligible rules; grammar without (json.bbnf) does not; `rg '@pratt' grammar/` returns 0 |
| SIMD auto-detection | F.W4 | grammar with charclass / keyword leaves emits SIMD scanner; grammar with small leaves does not (cost-model gate); `rg '@simd' grammar/` returns 0 |
| `pratt_eligibility` / `simd_eligibility` knobs honoured | F.W4 | per-grammar metadata knobs override auto-detection; integration test confirms |
| Pattern miners + recogniser ensemble integrated | F.W2 | every recogniser in `bbnf-passes/src/recognizers/` runs over every grammar; output-pipes to cost model |
| E-graph rewrite rules consume codegen IR | F.W3 | rewrite rules emit through `LayoutSink` per Lock 5; per-grammar regen reflects rewrite-driven changes |
| First SOTA-anchored perf gate | F.W6 | sonic-rs M1 Pro twitter ≤ 436 µs on JSON declaration crate's `parse` smoke; per Lock 8 |

## Wave summary table

| Wave | Name | Agents | Closes-on |
|---|---|---:|---|
| F.W0 — Optimiser pipeline scaffold | bbnf-passes/src/{recognizers, csp_strategy, egraph, rewrites}/ wired; output channels named | 2 parallel | scaffold compiles; output channels stub |
| F.W1 — CSP type/layout inference integration | csp-solver consumes Layout from bbnf-ir; emits resolved Layout back to LayoutRegistry | 2 parallel | CSP solver round-trips one grammar's Layout; per-grammar regen unchanged |
| F.W2 — Per-domain orthogonal verification + pattern miners | dependency-DAG audit; pattern miners (operator_chain, pattern_alphabet, etc.) integrate per-grammar | 3 parallel | DAG audit passes; pattern miners produce per-grammar fact tables |
| F.W3 — Cost model output-piping + e-graph rewrite rules | cost-model output pipes to codegen IR; rewrite rules emit through LayoutSink | 2 parallel | per-grammar regen reflects cost-driven + rewrite-driven decisions |
| F.W4 — Pratt + SIMD auto-detection | optimiser auto-detects Pratt eligibility (left-recursive operator chain) + SIMD eligibility (charclass / keyword leaves); per-grammar emit reflects | 3 parallel (per-grammar batches) | math.bbnf emits Pratt; json.bbnf emits SIMD scanner; metadata knobs override |
| F.W5 — Per-grammar perf benches | per-grammar `benches/parse.rs` against bbnf-bench harness; baseline numbers captured | 4 parallel (per-grammar batches) | benches run; baselines captured |
| F.W6 — First SOTA gate (sonic-rs twitter) | JSON declaration crate's parse smoke ≤ 436 µs on M1 Pro; per Lock 8 | 1 | gate passes; SOTA-anchored evidence at `audit/restart/perf-2026-XX-XX-json.md` |

## Carry-tags FROM

| Carry | Source tranche | Gate |
|---|---|---|
| `bbnf-passes` (every transformation pass) | C | C.W4 |
| 22-variant codegen IR + Emitter trait | D | D.W2 |
| 9 per-grammar declaration crates with template-emitted runtimes | E | E.W3 |
| Sister-crate path-deps (egraph + csp-solver) | A | A.W2 |
| `bbnf-vm` ready for VM-driven testing | C | C.W3 |

## Carry-tags TO

| Carry | Receiving tranche | Gate |
|---|---|---|
| Optimiser pipeline integrated per-grammar | G (slice-borrow API consumes optimised emit), H (TS+WASM consume same optimisation) | (continuous) |
| First SOTA-anchored perf gate | J (cross-backend parity + final perf gates) | J.W2, J.W4 |
| Pratt + SIMD auto-detection proven | I (publication-prep includes API freeze for these decisions) | I.W1 |
| Cost-model + rewrite rule integration | I (egraph publication includes cost-model contract) | I.W2 |

## 14-lock honoured cell map

| Lock | Status | Wave |
|---|---|---|
| 1 — Tape dead | honoured | (continuous from E) |
| 2 — Layout canon | honoured | (continuous from C) |
| 3 — Cursor + byte-skip | honoured | (continuous from E) |
| 4 — Per-domain orthogonal | substantively-honoured | F.W2 (DAG verification) |
| 5 — IR + per-backend | honoured | (continuous from D) |
| 6 — xtask source emit | honoured | (continuous) |
| 7 — `crates/path/` consolidated | honoured | (continuous from C) |
| 8 — Surpass SOTA | partial | F.W6 (first gate; full set at J) |
| 9 — Slice-borrow primary | n/a | (deferred to G) |
| 10 — Pratt + SIMD auto-detected | substantively-honoured | F.W4 |
| 11 — Path-deps for sister crates | honoured | (continuous; tranche I handles publication) |
| 12 — ser + gorgeous archive | honoured | (continuous) |
| 13 — No god directories | honoured | (continuous from C) |
| 14 — Full grammar generalisation | honoured | (continuous from E) |
| `feedback_csp-always-optimize` | honoured | F.W1 |
| `feedback_pluggable-components` | honoured | F.W3 (cost model + rewrite rules pluggable) |

## Risks + mitigations

| Risk | Mitigation |
|---|---|
| Optimiser pipeline (egraph + csp-solver) output-piping fuses by accident | F.W2 dependency-DAG audit: `cargo tree -p bbnf-passes` confirms orthogonal sub-deps; per-domain test isolation; per master plan §13 R10 |
| Pratt auto-detection misfires on a grammar shape, breaking parse | F.W4 per-grammar Pratt-eligibility inspection: emit Pratt-eligible rules to log; user confirms per grammar; metadata knob escapes; per §13 R11 |
| SIMD auto-detection misfires on small leaves, blowing dispatch overhead | F.W4 cost-model gate: SIMD only emits when expected dispatch overhead < SIMD payoff; metadata knob escapes; per §13 R12 |
| Cost-model output integration breaks existing optimiser tests | F.W3 staged: cost model emits stub first; integration tests pass; substantive integration follows |
| First SOTA gate (sonic-rs twitter) fails | F.W6 instrumentation: samply profile of JSON parse; bottleneck identification; if hardware-bounded, document the gap; if substrate-bounded, redress via triumvirate |
| Pattern miner false-positives produce wrong recogniser output | F.W2 per-grammar regression: pre-tranche grammar's recogniser output captured; post-tranche output cross-checked; divergence triggers triumvirate |

## Build/iter time gate

| Concern | Budget | Verification |
|---|---|---|
| Per-grammar optimiser pass time | ≤ 10s per grammar | F.W4 |
| `cargo bench -p <g> --bench parse` | ≤ 60s per grammar | F.W5 |
| Generated-LOC budget | F.exit: 143,250 LOC (-2.5K vs. E.exit) | per master plan §12.2 |

## Voice locks

Per master plan §14.

## Closing posture

Tranche F closes with the optimiser pipeline operational. Per-domain orthogonal optimisation honours per Lock 4; Pratt + SIMD auto-detect per Lock 10; first SOTA-anchored perf gate fires per Lock 8. Tranche G's slice-borrow API consumes the optimised emit; tranche H's TS+WASM emitters consume the same optimisation; tranche I's publication-prep includes the optimiser contract.

The greenfield mandate carries: no `@pratt` / `@simd` directives (optimiser auto-detects); no fused-hypergraph (per-domain orthogonal); cost model is pluggable (not hardcoded heuristics).
