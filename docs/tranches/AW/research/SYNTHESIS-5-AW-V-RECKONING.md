# AW-V Reckoning — Six-Agent Research Wave Synthesis (2026-04-18)

Six worktree-isolated sub-agents dispatched per `docs/instructions/RESEARCH.md`. Deliverables verbatim at `docs/tranches/AW/research/aw5-r{1..6}-*.md`. This document is peer-review, archaeology, composition-map, and an honest recommendation for AW-V.W3+.

## 0. The thing that resolves the architectural question

**The W2.1 prototype is recursive descent. It beats sonic-rs 5/5 on JSON.**

- post-AW-IV master (`docs/benchmarks/post-AW-IV.json`): JSON twitter 288 MB/s, CSS bootstrap 15 MB/s, Sheets parse_simple 6 MB/s — **6.8–30× regressions vs post-AU RD baseline**.
- post-AW-V-W2 prototype (`docs/benchmarks/post-AW-V-W2-prototype.json`, worktree): JSON twitter 2577 MB/s (1.31× post-AU, 0.89× sonic-rs). **`nm` clean on `dispatch_one | try_branch | advance_or_pop_with | __dta_walker_inline | DtaState | FrameStack`**.
- Prototype source: `crates/bbnf-json-prototype/src/` — 2,246 LOC of recursive descent with inline SIMD kernels + monomorphic visitor. No interpreter. No tagged-union state machine.

**The user's challenge re-stated is its own answer: "The prototype is RD after all???" — yes, and it wins.**

`aw3-r6-path-b-rip-dta.md` §7 predicted this in AW-III: *"Path A's W5.6 is admitting Path B; it keeps DTA branding for continuity."* The prediction is now artefact-backed.

## 1. Per-agent headline findings

| Agent | Thesis | Load-bearing finding |
|---|---|---|
| R1 — Stage-1 SIMD pre-pass | Chronically counterproductive, not deferred | **The pre-pass LANDED in AW-III.W5.d (commit `91df0809`)**. Twitter regressed 192→170 MB/s. W2.1 (no pre-pass) then hit 2577 MB/s (15× post-W5.d). The "chronic deferral" framing is wrong; it has repeatedly been tried and repeatedly lost. |
| R2 — Shape-emitter generality | Mechanism sound; IR signal richness sufficient | **10/15 hand-tuning decisions are IR-derivable, 3 profile-derivable, 2 backend-layout.** The only gap is the canada scalar-SIMD-crossover (`2edb612b`), mitigated by a 3-line hybrid kernel. Splicing vs hand-written: zero LLVM-IR difference given `lto=fat codegen-units=1 debug=true`. |
| R3 — Compiled automaton | bbnf-regex works because states are uniform | **State counts: JSON 51, CSS L4 83 (NOT 800), BBNF bootstrap 496.** bbnf-regex DFA bodies are ~20–40 bytes of uniform `match b` machine code per state; DTA arms range 40–400 LOC across 14 non-uniform variants. The 154 KB walker pathology is non-uniform-body-size, not LLVM inline budget. W4.d's codegen-specialised walker DID ship — `nm` confirms `dispatch_one` absent; `try_branch` survives at the cross-crate helper boundary. |
| R4 — SIMD levers | 3 strikes, 2 novel kernels, 1 dead code | **STRIKE**: `push_compound_fused_v32` is a self-aliased load/store tautology (LLVM MemorySSA elides); "17-digit NEON lever" does not exist. **VERIFY**: PMULL path may already fire on M4 (shift-XOR dead code). **ADD**: `vpaddq_u8` movemask cascade (3–4× faster than current `vaddv_u8`), `vdotq_s32` canada fraction packadd (1.5×), TBL-4 kinded bitmap (eliminates walker byte-load). |
| R5 — Departure A: rip DTA | Evidence-backed Path B | **Delete manifest: −7,610 LOC** (`dta_walker/` 3,875 + `emitter/dta.rs` 935 + `driver.rs` interpreter ~2,800). Rename `dta_walker/` → `shapes/`. AX consumers X0, X3–X10 survive with `dta_run_cold` (or rework to RD-checkpoint, −3,300 more LOC). |
| R6 — Departure B: e-graph grammar rewriting | Subsumes classification via extraction | 8 algebraic rewrites (Alt flatten, Seq flatten, KwWs fusion, PHF dispatch, Ref-to-leaf inline with leaf-predicate guardrail, PhfLoop, ClassifyByteLoop, OperatorChain). E-graph substrate ready today (`crates/egraph/`). 779 LOC of detectors collapses to 150 LOC of tag-reading via extraction. |

## 2. Archaeology trail per proposal

| Proposal | First attempt | Deletion / disablement | Failure mode named | Guardrail for re-introduction |
|---|---|---|---|---|
| Stage-1 SIMD pre-pass (R1) | `7198c974` (AO.0.4-0.6) v1 | `2f7c1bd4` (AQ.5) ~1500 LOC delete; AU.2.7 per-site helpers; `91df0809` (AW-III.W5.d) re-landed driver-consumed; `54eaa735` reverted Regex-bound because dense-alphabet grammars collapse the bound | "Pre-scan overhead costs ~15-25% without WS elision" (`4417f8a7`); "alphabet-disjoint precondition is grammar-IR data the current pass doesn't surface" (`54eaa735`) | R1 §4 grammar-property criterion: pre-pass only when `Regex("[^S]*")` ∧ S ⊊ single_bytes ∧ ConsumeToNextStructural lifter exists. Per-compound opt-in via `GrammarProfile.prefer_inline_in_loop`. |
| Compile-DTA-to-native (R3) | AW-III.W4.b `9581ea09` scaffold | Never deleted; carried through AW-IV W1.4-aggro (`96a955cf`) + W2.1 (`b96be94c`, `a62057b4`) | Code-size budget; `advance_or_pop_with` / ShuntingYard reducer bodies too big to splice per-arm (`FINAL-IV.md:147-153`) | R3 §8 recommendation: do not invest in general automaton compiler. The W4 specialised walker succeeded structurally and missed numerically; shape-emitter is the same mechanism with deduplication. |
| Rip DTA / fn-per-rule (R5) | AW-III.R6 proposed; rejected in favor of "keep DTA with W5.6 codegen" | Never implemented until AW-V.W2.1 prototype | "Un-deleting fn-per-rule requires re-bootstrapping; Path A's only appeal was continuity" (R6 §9) | R5 §5 bootstrap recipe: keep `dta_walker/` for BBNF's generated.rs one commit bridge, then delete. AW-I.W4ζ recipe (`87f65214` + `49656fd4`) is the template. |
| E-graph grammar rewrites (R6-depart) | `inline.rs` + `normalize.rs` + `prefix.rs` + `structural.rs` | `bfa50f25` (2026-04-08) deleted ~1200 LOC: "normalizer's cross-rule cascading architecturally cannot be expressed in one-pass saturation" | Unbounded inline cascade | R6-depart §3 G5 leaf-predicate: inline only when body extracts to Literal/Regex/Epsilon/KwWs (terminal). Combined with 64-iter / 100K-node caps already in `CostConfig`. |
| Lever 4 v32 fused store (struck by R4) | AW-V.W1.3 `1cf69a69` | Live on master; R4 names as tautology | Self-aliased `vld1q_u8 → vst1q_u8` elided by LLVM MemorySSA; W1.3 ledger doc-comment acknowledges (`b3cf555e`) | Replace with paired-column `stp q0, q1` over (span_lo, span_hi). Forfeits nothing; gains 4 scalar stores → 2 paired stores. |
| "17-digit NEON lever" (struck by R4) | Projection in AW-V.md:684,692 | Never shipped | Overfit artefact; no implementation in-tree | Strike from ledger. Replace with `vdotq_s32` on fraction path. |

## 3. Composition map — what stacks, what subsumes

| Proposal | Subsumed by | Composes with | Status in recommended plan |
|---|---|---|---|
| R5 — Rip DTA, emit fn-per-rule | — | R2 (shape emitter = fn-per-rule with shape templates) | **PRIMARY**. Rename `dta_walker/` → `shapes/`, delete interpreter. |
| R2 — Shape-emitter generality | R5 (shape-emitter IS the rip-DTA consumer) | R1 per-compound dispatch, R4 SIMD kernels | **PRIMARY**. Ship all 11 detectors in W3.2; Number hybrid kernel; `#[cold]` on bottom-80% rules. |
| R1 — Stage-1 pre-pass (per-compound) | R2 (emit-side splice selects inline-vs-indexed per rule) | R4 PMULL, R4 TBL-4 | **W4 component**. Ship as `prefer_inline_in_loop` per-rule bitmap. Per-pattern alphabet narrowing (`BoundedRegex` lifter) required for CSS/Sheets. |
| R4 — SIMD primitives | — | R2 (spliced via `bbnf-simd-scan::emit` fragments) | **W1 strikes + W3/W4 adds**. Strike Lever 4 + 17-digit; add PMULL verify + `vpaddq_u8` movemask + `vdotq_s32` packadd + TBL-4 kinded bitmap (W4). |
| R3 — Compiled-automaton | R5 (shape-emitter is the deduplicated form of fully-inlined DTA) | — | **Closure**. The architectural question resolves: DTA arms are non-uniform; automata compile when states are uniform; shape-emitter re-introduces uniformity via body-shape equivalence classes. |
| R6 — E-graph grammar rewriting | — | R2 (extraction replaces detectors); R5 (extraction feeds fn-per-rule emitter) | **AX or AW-VI**. Don't adopt in AW-V. Requires substrate + 8 rewrite files + regression harness against current detectors; too much scope during the rip-DTA landing. After W5 closes, land as an IR pass that deletes the detectors. |

The primary stack for AW-V.W3-W6: **R5 + R2 + R1 + R4**. R3 is a closure statement (no action). R6 defers.

## 4. The honest AW-V restate

### What's PRIMARY path (the 5-item critical path to throughput)

1. **Rename + rescope.** `docs/tranches/AW/AW-V.md` §"Compile DTA into hot-path code — not abrogate" is factually incorrect. Replace with: *"Emit fn-per-rule over shape-selected body templates. The DTA substrate was IR-mining scaffolding; the scaffold comes down as the consumer emerges."* No "DTA compilation" branding. The prototype is RD. The shape-emitter is RD. Call it what it is.

2. **Strike the architectural dead ends before W3.2 opens.**
   - Lever 4 `push_compound_fused_v32` (R4 §5): self-aliased tautology. Replace with paired-column `stp` over (span_lo, span_hi). W1.3 ledger corrected post-hoc.
   - "17-digit NEON lever" projection (R4 §4): overfit. Strike from AW-V.md:684,692. Replace with `vdotq_s32` on canada fraction path.
   - `dta_run_cold` as hot-path replay surface (R5 §4-7): vestigial. Either retain as cold-only (−500 LOC preserved) OR rework to RD-checkpoint (−3,300 more LOC). Decision pending user input.

3. **W3.2 ships all 11 shape detectors + all 11 emitter modules in one wave.** Per R2 §4, the 4 novel shapes (ArgList/Flat/Wrap/HRegex) are trivially IR-derivable from existing miners. W3.1's stub-and-defer violates no-deferrals. Per R2 §6, the Number-shape emitter MUST ship the scalar-SIMD hybrid kernel (scalar first 4 digits, then SIMD fallthrough) — not gated on workload profiling.

4. **W4 = CSS + Sheets + BBNF shape coverage + per-compound kernel strategy.** Per R1 §5.2, add `GrammarProfile.prefer_inline_in_loop: &[RuleId]` bitmap + `GrammarProfile.pattern_alphabets: &[PatternAlphabet]`. Emit `BoundedRegex` lifter for dense-alphabet CSS/Sheets rules. Per R4 §3, verify PMULL fires on M4; port `vpaddq_u8` cascade verbatim from sonic-simd; add `vdotq_s32` canada fraction refinement.

5. **W5 deletes the DTA interpreter.** −7,610 LOC of `dta_walker/`, `emitter/dta.rs`, `driver.rs::dispatch_one/try_branch/advance_or_pop_with`. Bootstrap recipe (R5 §5) is the AW-I.W4ζ pattern, proven. W1 `bbnf-tape-codegen` substrate survives; only the interpreter runtime deletes.

### What DEFERS to AW-VI / AX

- TBL-4 kinded bitmap (R4 §6 Design B). Strong CSS payoff; ships after shape-emitter generalises.
- E-graph grammar rewriting (R6-depart). Clean path to deleting shape detectors; too much scope inside AW-V.
- Document-parallel fork (already AW-V deferred per AW-V.md). Amortization multiplier only, not exceed-sonic lever.
- SME/SME2 (R4 §8 risk): unreachable from stable Rust until 2026-Q3ish nightly intrinsics land. Defer.
- AX consumers (X8 gradient, X9 speculative, X10 user multi-visitor): all compose cleanly with shape-emitter substrate per R5 §4.

### What NEVER ships

Explicit dead-ends from the archaeology trail:

- A universal driver-consumed stage-1 SIMD pre-pass for all grammars (R1 §3 — tried 6 times, regressed every time).
- Unbounded `inline_acyclic` without the leaf-predicate guardrail (R6-depart §8 — the 2026-04-08 failure).
- Per-arm splice of `advance_or_pop_with` + `ShuntingYard` reducer (R3 §4 mitigation #2 — 50M-line bodies, LLVM rejects).
- "Keep DTA interpreter on cold path and also ship shape-emitter hot path" branding (R5 §8 — fork surface, violates one-codegen-path invariant).

## 5. Critical do-not list for AW-V W3+ execution

This is the anti-scope statement. Every item is a known dead-end from this synthesis.

1. **Do NOT re-brand as "compile DTA into hot-path code".** The prototype is RD. The shape-emitter is RD with IR-selected templates. Call it fn-per-rule.
2. **Do NOT stub shapes across waves.** If all 11 detectors can't ship in W3.2, the wave is misdefined. No-deferrals invariant.
3. **Do NOT ship a universal stage-1 SIMD pre-pass.** Per-compound opt-in via `GrammarProfile.prefer_inline_in_loop` bitmap. JSON: all-inline; CSS/Sheets: indexed where pattern alphabets narrow.
4. **Do NOT keep Lever 4 `push_compound_fused_v32`.** Architectural self-alias. Strike; ship paired `stp` replacement.
5. **Do NOT project "17-digit NEON" or any numeric lever without `cargo asm` proof.** The projection ledger rots; strike now.
6. **Do NOT bundle multiple generated.rs emitters.** The `dta_walker/` and `shapes/` co-residence is a fork surface. Bridge via bootstrap recipe, then delete.
7. **Do NOT splice `advance_or_pop_with`'s 400-LOC body per arm.** R3 §4 mitigation #2 is unworkable. The helpers go via rip-DTA (R5), not per-arm inline.
8. **Do NOT add new e-graph rewrite rules in this tranche.** R6-depart is a clean proposal but its scope is 8 rewrite files + cost model + regression harness. Lands AW-VI / AX, not here.

## 6. Honest close

The six-agent wave resolved the architectural question but did not surface novel performance levers beyond what was already scoped. What it did is:

- **Validate R6's §7 prediction** with W2.1 empirical evidence: Path B is the honest formulation; the shape-emitter W3+ plan IS Path B.
- **Correct three factual errors** in prior tranche artefacts: CSS L4 state count (83, not 800), stage-1 SIMD status (landed, regressed — not deferred), `push_compound_fused_v32` semantics (self-aliased, dead).
- **Strike two projections** as overfit: Lever 4, "17-digit NEON".
- **Surface two novel kernels** worth shipping in W4: TBL-4 kinded bitmap, `vdotq_s32` packadd.
- **Resolve the DTA interpreter fate**: retire after W5 per the bootstrap recipe; AX survives with `dta_run_cold` (optional) or RD-checkpoint rework (cleaner).

The 1000-commit anti-pattern is real and present. The avoidance is: commit to fn-per-rule, commit to strike the two bad levers, commit to W5 interpreter deletion, ship W3.2 with all 11 shapes or stop and re-plan. No more "compile the DTA" narrative. No more universal stage-1 proposals. The prototype proved the path; generalise it.

The R5 recommended rewrite of `docs/tranches/AW/AW-V.md` §Invariants.3 + §Wave-schedule + §Delete-manifest should land as the first commit of the revised W3 open. Not a new tranche letter — this is a scope *re-statement* within AW-V, not a scope pivot; the wave schedule, the hard gates, and the critical files all survive. What deletes is the continuity branding.

---

**Verification artefact citations**:
- `docs/benchmarks/post-AW-IV.json` (master regression evidence)
- `docs/benchmarks/post-AW-V-W2-prototype.json` (RD validation)
- `docs/tranches/AW/AW-V-W2-close.md` (nm + samply attribution for W2.1)
- Commits: `91df0809` (W5.d pre-pass landing), `54eaa735` (W5.d repair), `bfa50f25` (e-graph rewrite deletion), `2edb612b` (scalar int scan win), `2f7c1bd4` (AQ stage-1 delete), `4417f8a7` (AP pre-pass disable with WS reason), `9581ea09` / `0802c6ce` / `96a955cf` / `b96be94c` / `a62057b4` / `465a9f2c` (W4-W2.1 codegen-specialised walker landings), `1cf69a69` (Lever 4 v32 landing, to be reverted), `b3cf555e` (W1.3 pathology doc-comment), `87f65214` / `49656fd4` (AW-I.W4ζ bootstrap recipe precedent).
