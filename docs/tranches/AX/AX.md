# Tranche AX — The RD Reckoning

AX is the reckoning tranche. AW-V closed with 0/17 parse entries exceeding post-AU (JSON twitter 486 MB/s = 24.7% of baseline; CSS/Sheets/BBNF at 3-7% of baseline). Shape-emitter substrate landed for all grammars but only JSON's `parse()` routes through it at runtime; CSS/Sheets/BBNF still delegate to `__dta_walker_inline::run`. The `has_w4_classified` gate at `crates/core/src/backend/rust/emitter/grammar.rs:718` over-restricts JSON's visitor-path. AW-V's thesis — "auto-derive the sonic-rs-class inner loop from any BBNF grammar" — was demonstrated exactly once, on JSON, at W3 close (commit `c1e86ab3`), and lost by W6.

AX augments the 400-commit work, not forks it. No hand-tuned per-grammar prototypes. No new grammar directives. The shape emitter IS the generalization mechanism; fix routing, delete interpreter, deploy lever portfolio, CI-gate parity against external comparators. When AX closes: one codegen path reached by all four grammars, no interpreter, structural-parity CI against sonic-rs + lightningcss + simdjson OnDemand + serde_json + cssparser, document-parallel fork as amortisation multiplier, e-graph grammar rewriting subsumes classification, subsystem ledger zero.

## Architectural thesis

Six propositions:

1. **The regression must be repaired before the interpreter deletes.** V's 0/17 gate miss is a routing + gate failure that W0a closes before W0b deletion.
2. **The interpreter is architectural debt.** Total reclaim ~78,500 LOC (DTA machinery + PSI + `simd-scan::emit/*` + 7 dead `GrammarProfile` slots + `bbnf-tape-codegen` + deprecated csp_strategy aliases + Lever 4 + ~3,900 LOC DTA test suites + ~57,481 LOC generated.rs walker output).
3. **The tape's access API shapes the ceiling more than the tape's storage layout does.** SoA primary + AoS sidecar hybrid per R4 §5.
4. **Novel levers compound only when they share a substrate AND a demonstrable floor.** V's substrate-first-consumer-later anti-pattern must not recur.
5. **Parallelism is an amortisation multiplier over single-thread exceed, not a single-thread lever.**
6. **Parity IS the generality claim.** No hand-tuned per-grammar prototypes. Structural-parity harnesses assert byte-identical-OR-field-equivalent output against external comparators.

**What AX does NOT do** (routed to AY): incremental re-parse, structural-default recovery, decision-log replay tooling, parse-step debugger, Cranelift JIT.

**Pratt clarification.** Sheets 6 MB/s is not a Pratt-mechanism cost. Master already collapses the 6-rung operator tower into ONE `ShuntingYard` state at DTA lift time. Sheets is walker-routed because `has_shape_dispatcher_entrypoint` rejects Seq-rooted `formula`. **W0a is the Pratt lever.** G8 OperatorChain rewrite in W11 is detector-LOC cleanup, not throughput.

## Invariants

1. **One codegen path** — no hybrid, no fallback, no `dta_run_cold` cold-path replay.
2. **Tape Value API is monomorphised at the user's target type**, not a tagged-union runtime Value.
3. **All unsafe is concentrated in kernels**; `unreachable_unchecked` at proven-dead dispatchers is the one emitter-inserted `unsafe` primitive.
4. **No new grammar directives.** Ever. All per-rule decisions derive from existing IR facts, miner outputs, or `ParseOptions` runtime flags. No `@hint`, `@parallel`, `@tune`, `@cost`, `@utf8`, `@lazy`, `@input_size`. The grammar author's surface is the BBNF syntax existing grammars already use.
5. **Parity harnesses are binary: pass or fail.** No tolerances.
6. **Document-parallel fork is opt-in via `ParseOptions::parallel_threshold`**, default single-thread.
7. **Wire-contract end-to-end tests per IR-derived emitter output.** Every `pub const` the emitter produces carries a wire-contract test asserting the full pipeline.
8. **No legacy code, no shims, no forward hooks for AY.**
9. **Gate predicates frozen after introducing wave.** No downstream wave widens a classification/admission predicate without explicit re-plan-with-more-agents.
10. **Mid-wave bench-checkpoint.** Every wave runs the 19-entry matrix at mid-wave AND close. Regression ≥ 5% triggers re-plan.
11. **No per-grammar hand-tuned prototypes.** Generality is proved by the shape emitter beating external comparators or matching post-AU self-baselines.
12. **Wave discipline carries from AW.** No scope creep mid-wave; scope-reveal triggers re-plan.
13. **Ledger-only wave = re-plan trigger.** If wave-close cannot cite runtime evidence (samply self-time ≥ 1%, `nm` symbol absence, wire-contract end-to-end) for every substrate landing, the wave reopens.
14. **Gate-predicate symmetry.** Every predicate disabling emission carries a per-grammar wire-contract test asserting the predicate's output at every wave close.
15. **Small-input amortisation documented at plan time.** Sheets parse entries (505 B – 1.8 KB) cannot meet post-AU on SIMD-amortising levers; their path is W0a routing.
16. **Predicate-widening requires re-bench of downstream gates.** A wave widening a classification predicate runs the full bench matrix at commit time.
17. **"Architectural transposition complete; throughput in next wave" is not a closeable wave.**
18. **No stubs, no shims, no placeholder surfaces.** Every Value API variant ships field-complete on day one of its wave. No `todo!()` arms, no `_` variants placeholdered for later, no `#[allow(dead_code)]` on struct fields awaiting a populator.
19. **Per-wave spec documents.** Each wave carries its own `docs/tranches/AX/waves/W<N>.md` spec. The AX.md parent is the index; the spec documents are the orchestrator's dispatch inputs.

## AX operational posture

1. **Bench-checkpoint mid-wave.** `cargo bench` at mid + close per wave. Saved to `docs/benchmarks/post-AX-W<N>-{mid,close}.json`. Regression ≥ 5% triggers re-plan.
2. **Wire-contract compile-gates on every wave.** Every gate predicate that disables emission carries a test asserting the gate's outcome for every grammar at every wave close. File: `crates/core/tests/gate_predicate_wire_contract.rs` (new in W0a).
3. **Ledger review at wave handoff.** Wave N+1 cannot open until Wave N's ledger is reviewed against the gate predicates every downstream wave depends on.
4. **Frozen-contract rule for gate predicates.** Once W0a lands, no wave widens `has_w4_classified`, `has_full_shape_coverage`, `has_shape_dispatcher_entrypoint`, or any analogous predicate without explicit re-plan.

## Wave summary

Eighteen waves. Each row links to its spec document. Block A is correctness + API (W0a–W3); Block B is optimizations (W4–W14); W15 is FINAL.

| Wave | Spec | Headline | Opens after |
|------|------|----------|-------------|
| **W0a** | [waves/W0a.md](waves/W0a.md) | Gate repair + non-Alt-rooted `parse()` routing + `gate_predicate_wire_contract.rs` | AW-V.W6 |
| **W0b** | [waves/W0b.md](waves/W0b.md) | Interpreter deletion + substrate-without-consumer purge + crate renames | W0a |
| **W0c** | [waves/W0c.md](waves/W0c.md) | AW-V.md rewrite in RD language | W0b |
| **W1** | [waves/W1.md](waves/W1.md) | Value API + hybrid tape + named_types BINDINGS widening (L8) | W0c |
| **W2** | [waves/W2.md](waves/W2.md) | Parity harnesses CI-gated (5 comparators) | W1 |
| **W3** | [waves/W3.md](waves/W3.md) | Subsystem closures + W0b investigation-queue resolution | W2 |
| **W4** | [waves/W4.md](waves/W4.md) | JSON SIMD levers + L1/L2 miner inheritance + scanner generalization | W3 |
| **W5** | [waves/W5.md](waves/W5.md) | CSS SIMD levers + ShapeRef at shape-emit (L3 relocation) | W4 |
| **W6** | [waves/W6.md](waves/W6.md) | Per-pattern `last_byte_set` narrowing + CTNS emission | W5 |
| **W7** | [waves/W7.md](waves/W7.md) | Gradient / LazyValue / on-demand materialization | W6 |
| **W8** | [waves/W8.md](waves/W8.md) | Speculative parsing + shape-transition Markov predictor | W7 |
| **W9** | [waves/W9.md](waves/W9.md) | Document-parallel fork (heuristic `fork_cut_byte`) | W8 |
| **W10** | [waves/W10.md](waves/W10.md) | E-graph universal rewrites G1-G4 + `is_fixed_shape` cost bias (L5) | W9 |
| **W11** | [waves/W11.md](waves/W11.md) | E-graph per-shape rewrites G5-G9 + rewrite fuzz | W10 |
| **W12** | [waves/W12.md](waves/W12.md) | Shape-dispatch detector retirement (~1,676 → ~150 LOC) | W11 |
| **W13** | [waves/W13.md](waves/W13.md) | CPU autotune + PMC + cost-grid sweep | W12 |
| **W14** | [waves/W14.md](waves/W14.md) | Multi-visitor pairs + multi-key SIMD compare | W13 |
| **W15** | [waves/W15.md](waves/W15.md) | FINAL + bench matrix + AY handoff | W14 |

## AX → AY handoff contract

Seven conditions must verify clean before AY opens. Any miss is an AX-close blocker.

1. `grep -rE 'dispatch_one|try_branch|advance_or_pop_with|dta_run|DtaTable|DtaState|FrameStack' crates/` returns zero.
2. `nm target/release/deps/{json,css_l4,google_sheets,bbnf}_monolithic-*` shows zero DTA symbols.
3. Every grammar's `parse()` routes through shape dispatcher; no walker fallback.
4. All 17 bench entries ≥ post-AU on single-thread.
5. `has_w4_classified` and analog gate predicates deleted (not merely narrowed).
6. All 9 e-graph rewrites active; 12 detector files deleted.
7. Zero `#[ignore]` in workspace.

## Defensible floor

Per `docs/tranches/AW/audit/lever-efficacy.md` priors (6 HIGH / 10 MEDIUM / 12 LOW / 6 NOVEL across 37 items; P(all land) ≈ 5-12%), AX's defensible floor is 5 items recovering post-AU on JSON + tailwind:

1. W0a gate repair + routing
2. W0a `gate_predicate_wire_contract.rs`
3. W2 parity harnesses
4. W9 document-parallel fork
5. W0c doc rewrite + W0b crate renames

This floor historically recovers ≥ post-AU on JSON (5 entries) + tailwind single-thread + parallel multipliers. CSS bootstrap + 3 Sheets + 3 BBNF entries have no HIGH-confidence historical lever; they rely on W0a routing + the LOW/NOVEL tail (BoundedRegex, CTNS, ShapeRef, LazyValue).

## Post-tranche review candidates

Decision at W15 close, not mid-wave:
- vpaddq_u8 (W4), vdotq_s32 (W4), PMULL verify (W4), Bloom+GADT (W5), ShapeRef consumer (W5).

Each ledger-only in ≥ 2 prior tranches. Every item lands in its scheduled wave; each wave's close ledger records the bench delta. Retirement at tranche close only — no mid-wave scope-cut.

## Indefatigability

When AX closes:
- One codegen path reached by ALL FOUR grammars.
- No interpreter. No gates masquerading as admission predicates. No prototypes per grammar.
- Workspace-local crates are `tape`, `simd-scan`, `json-prototype`, `jit` — no `bbnf-` prefix.
- First-class Value API with structural parity vs sonic-rs + lightningcss.
- SoA primary tape with AoS sidecar.
- Every viable novel lever deployed or rejected with rationale.
- Structural-parity CI gates on five external comparators.
- Document-parallel fork as amortisation multiplier on ≥ 1 MB inputs.
- E-graph grammar rewriting subsumes shape classification; detectors deleted.
- Subsystem ledger zero. Mid-wave benches run. Frozen gate predicates. AY.md drafted.
