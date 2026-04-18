# V-to-X Learnings — Audit #4 (Angle: AX plan vs V's actual close state)

## 1. Angle headline

**AX.md is drafted as if V delivered its "projected best-case" table, but V closed at 0/17 entries exceeding post-AU, with JSON twitter at 486 MB/s (vs AX.W2's CSS-normalize≥1500 gate assumption) and non-JSON grammars never demonstrating shape-dispatcher routing. The plan inherits risk it has not yet priced in: (a) W0 deletes an interpreter that is still the *only* path by which 12 of 17 bench entries reach `parse()`; (b) W3+ per-grammar gates are anchored on a JSON-only prototype; (c) W2's 12-lever cluster repeats the scope-widening failure mode that crashed V.W4 → V.W5; (d) the "substrate-with-consumer" invariant is written but the wave topology violates it at W1/W2. The plan needs to be re-sequenced around one fact: V's single diagnosed regression (`has_w4_classified` false-positive on JSON's `pair`/`value`) is load-bearing on every downstream gate.**

## 2. AX.W0 sequencing — split into W0a / W0b / W0c

V evidence: at V.W6 close, `crates/core/src/backend/rust/emitter/grammar.rs:718` (`!has_w4_classified(ir)`) gates `parse_with_visitor` off for JSON. V.W6 admits 12/17 entries (CSS/Sheets/BBNF) reach `parse()` via the walker, and only JSON's tape-path improved (+61-73% via W5.2's per-Ref dispatcher substrate reaching parse() indirectly). Deleting the walker BEFORE fixing `has_w4_classified` fails on master immediately — not because the architecture is wrong, but because the routing gap `FINAL-V.md` §"Why the throughput missed" §2 names as bounded has not yet closed.

**Proposal: split W0 into three strictly serial sub-waves.**

- **W0a — Gate repair + visitor path re-activation.** (New sub-wave, one agent.) Narrow `has_w4_classified` at `crates/core/src/backend/rust/emitter/grammar.rs:718` and `crates/core/src/backend/rust/emitter/shapes/dispatcher.rs:836` to *trait-predicate test* — "does any W4 classification in this grammar emit a visitor method outside the W3 trait set?" Re-admit `parse_with_visitor` emission for JSON. Verify `json_monolithic_value` bench compiles AND matches prototype within ±2%. Additionally: emit `parse()` entry-shape dispatch for non-Alt-rooted grammars (CSS OW-wrapped, Sheets Seq, BBNF Repeat) per `FINAL-V.md` §"Why the throughput missed" §2. **Hard gate: every grammar parses via shape dispatcher, not walker fallback.** This is V's diagnosed residual, not an AX novelty.
- **W0b — Interpreter deletion.** Current AX.W0 content, unchanged. `grep` returns zero hits; `nm` shows zero DTA symbols.
- **W0c — AW-V doc + Lever 4 cleanup.** Current AX.0.2 content.

Rationale: AX's "one codegen path" invariant is violated by master today — deleting the walker before W0a closes is deleting the only path 12/17 entries actually run on. W0b's `cargo test --workspace` green gate is impossible without W0a.

## 3. Per-grammar gates for AX.W3+

V evidence: the prototype-parity gate (W3.2 "±5% of W2.1 prototype bench") only exists for JSON. `bbnf-json-prototype` (2,246 LOC) is the sole hand-tuned reference. CSS/Sheets/BBNF have no comparator analog; V's CSS bootstrap at 14 MB/s vs AX.W2's 1500 MB/s gate is a 100× gap, not a tuning gap.

**Proposal: per-grammar twin-reference gates.**

| Grammar | Reference | Gate form |
|---|---|---|
| JSON | `bbnf-json-prototype` | Emitter-within-±5% of prototype; prototype-beats-sonic absolute floor |
| CSS | **NEW**: `bbnf-css-prototype` crate, hand-tuned on `normalize`/`bootstrap` | Emitter-within-±10% of prototype; prototype vs lightningcss ratio ≥ 0.85× |
| Sheets | Self-parity with post-AU (at 95/128/121 MB/s) | Emitter ≥ post-AU on all three parse entries |
| BBNF | Self-parity with post-AU (at 394 MB/s bbnf_self) | Emitter ≥ post-AU on bbnf_self |

The CSS prototype is new scope — ~1,500-2,000 LOC — but it is the operational definition of "generalises beyond JSON." Without it, AX.W3+ gates are unenforceable beyond "not worse than V." Build the CSS prototype as a new wave **between W0 and W1** (W0.5), authored in a sibling worktree, establishing the CSS exceed baseline before ANY emitter work targets CSS. Sheets and BBNF do not merit prototypes (too small; post-AU is already achievable via W0a's routing fix per FINAL-V §3).

## 4. W2 lever cluster risk analysis

V evidence: V.W4 landed "6 detectors + 6 emitters" in one wave. W4-fix-rest's detector widening (commits `569c17e4` / `ce2fd9f6`) caused JSON's `pair` and `value` to classify as Flat/Wrap. This was not noticed until W6 because no wave-close wire-contract test asserted "JSON's visitor bench compiles and matches prototype." The scope-widening cost was not priced at W4 close.

AX.W2 has 12 sub-items across 5 parallel agents. **Highest risk: AX.2.5 `BoundedRegex` lifter + `ConsumeToNextStructural` emission.** R1 §3 names the precondition as "per-pattern alphabet-narrowed stage-1 pre-pass with IR-lifter." V never shipped CTNS emission in any wave; the shape emitter has no emission pattern for it. AX.2.5 asks 5 parallel agents to ship 12 items, one of which is a novel emission pattern that touches string/number/scalar/hregex shapes simultaneously.

**Proposal: phase W2 into W2a / W2b / W2c.**

- **W2a — JSON-only levers** (5 items): paired `stp` (replaces reverted Lever 4), `unreachable_unchecked`, scan-fusion, AltReorder, `vpaddq_u8`/`vdotq_s32`. Hard gate: JSON twitter ≥ W2.1 prototype. No scope leakage.
- **W2b — CSS-targeted levers** (4 items): TBL-4 kinded bitmap, kind-separated stage-1 streams, SIMD-speculative Alt-branch for Unordered, ShapeRef consumer + bloom dedup. Hard gate: CSS normalize exceeds the CSS prototype from W0.5.
- **W2c — `BoundedRegex` lifter + CTNS emission** (3 items): the novel emission pattern. Own wave. Hard gate: CSS bootstrap ≥ CSS-prototype ± 5%. If CTNS does not move CSS bootstrap after W2c lands, retire per the "levers that don't move a gate after substrate deployment retire with rationale" invariant.

Rationale: V's substrate-then-consumer anti-pattern hit hardest on novel emission. Don't repeat it across 12 levers × 5 agents in one wave.

## 5. Substrate-with-consumer audit of every AX wave

V evidence: `FINAL-V.md` §Invariant-2 marks this invariant "◐ (substrate landed; consumer engagement incomplete)." Lever 4 shipped at W1.3, no consumer reached it at W6. `push_compound_fused_v32` is the anti-pattern. The invariant is the rule AX claims to uphold.

| Wave | Substrate | Consumer | Same wave? | Status |
|---|---|---|---|---|
| W0a (new) | — | Gate repair | Yes | OK |
| W0b | Deletion | Compile fail if W0a miss | Yes | OK |
| W0.5 (new) | CSS prototype | Parity reference | Yes | OK |
| W1 | `Value` + AoS sidecar | `ValueVisitor<T>` monomorphised compile | Partial | **Risk: hybrid tape's packed_cache gated behind "first random-access read" — consumer unclear at wave-close. Needs an explicit consumer (e.g. Twitter's lazy-field-extraction bench) in the same wave.** |
| W2a/b/c | SIMD + dispatch levers | W1 monomorphised path consumes | Yes | OK if wire-contract tests per lever |
| W3 | `LazyRef` tape kind | `*LazyValue` wrappers | Yes | OK |
| W4 | Markov predictor | Speculation rollback | Yes | OK but confidence threshold > 0.6 unchecked |
| W5 | Fork split-point | Merge | Yes | OK |
| W6α | 4 universal rewrites | Extraction tag consumer | Yes | OK |
| W6β | 5 per-shape rewrites + detector retirement | `classify_shape(egraph, root)` | Yes | **Risk: retirement deletes 779 LOC in same wave as 5 new rewrites. A divergence means rollback of both.** Split W6β into W6β (rewrites) → W6γ (detector retirement). |
| W7 | Five-variant codegen | `cpu_variant::Auto` | Yes | OK |
| W8 | JIT emitter | Cache + invalidate | Yes | OK |
| W9 | `#[emit_paired_with]` macro | Per-grammar example visitors | Yes | OK |
| W10 | Parity harnesses | CI gate | Yes | OK |

**Recommendation: add wire-contract end-to-end test per wave.** Invariant 7 states this; codify per-wave as gate item: "every `pub const` the wave's substrate introduces is consumed by an end-to-end test invoking the runtime consumer." V missed this at W4 → W5 → W6.

## 6. Parity harness hardening (W10)

V evidence: parity harnesses exist (sonic-rs 5/5, lightningcss 4/4). Zero-divergence achieved at V.W5.2. AX.W10 expands coverage to ≥200 fixtures AND re-asserts zero-divergence.

Risk: AX.W4 (speculative) and AX.W6 (e-graph) are the waves most likely to introduce silent divergence. A speculation mispredict re-walked correctly produces an identical tape; a mispredict with bad rollback produces a divergent tape without test failure until a parity fixture exercises the specific bad predicate.

**Proposal: pre-parity regression protection for W4 and W6.**

- **W4.4 pre-parity fuzz.** Speculate + rollback fuzz on corpus subset (≥ 50 inputs per grammar); every speculation attempt's rollback path compared tape-identical to non-speculative path. Fails the wave if any non-identical rollback observed.
- **W6β.2 rewrite fuzz.** Every rewrite rule fuzzed against 100 grammars sampled from `grammar/**.bbnf` — every rewrite's pre/post-rewrite tape MUST be identical on all corpus fixtures for every sampled grammar. Fails the wave if any rewrite produces tape divergence.

**Binary parity remains the AX.W10 gate.** Per-variant tolerances introduce ambiguity on "did we generalize or did we tolerate divergence." V already demonstrated zero-divergence is achievable; AX should not lower that bar.

## 7. Risk budget + process changes — AX operational posture

V evidence: V.W4's detector widening broke a gate in W5 revealed in W6. Three wave boundaries absorbed a regression introduced in one commit. Process failures name:

- Wave N's gate predicate (`has_w4_classified`) was tested only at Wave N close, not in any downstream wave.
- Wave N's bench was `cargo test -p bbnf --release`; the `cargo bench -p bbnf --bench json_monolithic_value` failed to compile but wasn't exercised in W5.
- Wave N's wire-contract tests asserted emitted code shape, not emitted code's bench compiles.

**Proposed "AX operational posture" section for AX.md:**

1. **Bench-checkpoint mid-wave.** Every wave runs the 19-entry bench matrix at mid-wave + close, not just close. Regression triggers re-plan per README.md wave-verification-ledger.
2. **Wire-contract compile-gates on every wave.** A gate predicate that disables emission (`has_w4_classified` analog) carries a wire-contract test asserting the gate's outcome for every grammar at every wave close. Every wave's test-manifest lists the per-grammar predicate outcomes as assertions.
3. **Ledger review at each wave handoff.** Wave N+1 cannot open until Wave N's ledger is reviewed against the gate predicates every downstream wave depends on. Ledger review is a serial agent, not an implicit orchestrator action.
4. **Frozen-contract rule for gate predicates.** Once W0a lands the narrowed `has_w4_classified` (and analogous predicates), no subsequent wave widens them without explicit re-plan. V's W4 widened silently; AX freezes after W0a.

## 8. AX → AY handoff contract

V evidence: `FINAL-V.md` §"Carry-forward into AW-VI / AX" lists 7 items. AY.md opens with "AX closes first" invariant. Handoff risk: AX's residual debt silently inherits.

**Specific AX artefacts AY must verify clean before opening:**

1. `grep -rE 'dispatch_one|try_branch|advance_or_pop_with|dta_run|DtaTable|DtaState|FrameStack' crates/` returns zero (AY.md §Invariant 1).
2. `nm target/release/deps/{json,css_l4,google_sheets,bbnf}_monolithic-*` shows zero DTA symbols. (Not just grep.)
3. Every grammar's `parse()` routes through shape dispatcher; no walker fallback for any classified rule. (W0a's residual.)
4. All 17 bench entries ≥ post-AU on single-thread. (AX.W12 gate.) Any entry below post-AU is inherited debt AY cannot absorb because AY's ≤5% regression gate assumes a hot baseline.
5. `has_w4_classified` and any analog gate predicates deleted (not just narrowed) — if a predicate exists in AX at close, it's still a fork surface AY can re-introduce.
6. All 9 e-graph rewrites active; detector files deleted (not `#[cfg(deprecated)]`-gated).
7. Zero `#[ignore]` in workspace.

Any miss is an AX-close blocker, not an AY-open negotiation.

## 9. Concrete edits to AX.md (≤ 20)

Line-level changes to `docs/tranches/AX/AX.md`:

1. **§Wave-schedule table, W0 row**: split into W0a, W0b, W0c per §2 above.
2. **§Wave-schedule, insert W0.5**: "CSS hand-prototype (`bbnf-css-prototype` crate)" between W0 and W1.
3. **§Wave-schedule, W2 row**: split into W2a, W2b, W2c per §4.
4. **§Wave-schedule, W6β row**: split into W6β (rewrites) and W6γ (detector retirement).
5. **§Scope item 1 (W0)**: rename "Interpreter exorcism + AW-V cleanup" to "Gate repair + interpreter exorcism + AW-V cleanup"; add "narrow `has_w4_classified` + emit `parse()` entry-shape dispatch for non-Alt-rooted grammars" as first sub-item.
6. **§Invariants, add 9**: "Gate predicates frozen after W0a. No downstream wave widens a classification/admission predicate without explicit re-plan."
7. **§Invariants, add 10**: "Mid-wave bench-checkpoint. Every wave runs the 19-entry matrix at mid-wave + close."
8. **§Invariants, add 11**: "Per-grammar twin-reference gates. JSON vs `bbnf-json-prototype`; CSS vs `bbnf-css-prototype` (W0.5); Sheets/BBNF vs post-AU per-entry."
9. **§Phase 0, AX.0.1 delete-manifest**: add preamble row "GATE: Before any file in this manifest deletes, `has_w4_classified` must be narrowed per W0a and `json_monolithic_value` bench must match prototype within ±2%."
10. **§Phase 2, AX.2.5 BoundedRegex**: add explicit sub-gate "if W2c close does not move CSS bootstrap ≥ 1.25× W2b close, retire lever per no-stub-levers invariant."
11. **§Phase 6, W6β**: move detector-retirement line from W6β hard gate to new W6γ hard gate.
12. **§Hard gates summary, renumber**: W0→W0a/W0b/W0c; W2→W2a/W2b/W2c; W6β→W6β/W6γ.
13. **§Phase 12, W12**: add explicit post-AX bench-matrix line "every single-thread parse entry ≥ post-AU **AND** JSON entries ≥ sonic-rs × 1.07×" — matches AY's handoff invariant §8.
14. **Insert §"AX operational posture"** after §Invariants per §7 above (4 numbered process items).
15. **§Scope item 5 (W4 speculative)**: add "W4.4 pre-parity fuzz ≥ 50 inputs per grammar" per §6.
16. **§Scope item 7 (W6)**: add "W6β.2 rewrite fuzz against 100 sampled grammars" per §6.
17. **§Architectural thesis, proposition 5**: change "Parity is the generality claim" to "Parity is the generality claim, refined per-grammar — JSON + CSS twin-reference prototypes establish absolute exceed floors; Sheets/BBNF use post-AU as self-parity floor."
18. **§Critical files, add row**: `crates/bbnf-css-prototype/` (new workspace member, W0.5).
19. **§Critical files, add row**: `crates/core/tests/gate_predicate_wire_contract.rs` (new; wire-contract freezing gate predicates).
20. **§Indefatigability**: add bullet "Mid-wave benches run. Frozen gate predicates. Twin-reference prototypes. One codegen path reached by all grammars, not just JSON."
