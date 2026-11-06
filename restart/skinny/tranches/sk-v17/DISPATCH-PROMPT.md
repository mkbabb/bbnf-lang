# SK-V17 DISPATCH-PROMPT — Per-Wave Triumvirate Dispatch Contract

Date: 2026-05-29. Cycle: S-P3 V3 (folds the V2 CHALLENGE disposition: W2 exit-gate maintain budget → -2.0% median band vs W1 typed-tape baseline, reconciling SPEC↔P3-C §2.2/§3; all V1 dispositions carried forward). Master HEAD `f87ee713a`.

This is the orchestrator's per-wave dispatch contract for the SK-V17 SPEC
(`restart/skinny/tranches/sk-v17/SPEC.md`). Each wave is dispatched individually as a
triumvirate (research → plan → redress, distinct commits) per
`restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`, with the six-lens CHALLENGE
interposed for first-of-class interventions. No wave dispatches until its prior wave
closes and its SPEC entry gate holds. Only W0 is authorized now; W1–W5 are conditional.

## Universal dispatch invariants (every wave carries these verbatim)

- **Benched tree only.** Owner paths are `skinny/crates/` (+ `skinny/xtask/`,
  `skinny/RESULTS.md`, `skinny/REDRESS.md`). Editing `crates/core/` / `crates/ir/` is
  wrong-tree dishonesty and is REJECTed. Tape/layout gates are verified by grepping
  `skinny/crates/`, never `crates/core/`.
- **aarch64 only.** Apple M5 Max. No x86, no AVX-512, no SVE.
- **Lock 1 + Lock 14.** One substrate (the existing `Tape`/`ValueRef`/`TapeBuilder`);
  grammar-neutral generic crates (CSS routing derived from `.bbnf`/`BackendRule`, never
  hand-curated). Per SPEC §1.
- **Scalar-ref + checkasm + same-wave consumer per primitive.** Every SIMD primitive
  lands its scalar twin + checkasm differential before wiring, and its hot-path consumer
  in the same commit. No orphan kernel.
- **N≥50 cold median.** Every CSS measurement is N≥50 cold per-parse median via
  `css_canon_bench.rs` (code-asserted `:250`); the same-run re-baselined lightningcss
  full-CSSOM is the only strict bar; cssparser token-scan is a flaw probe only.
- **Equality before speed.** No CSS >SOTA admission unless `css_typed_summary_equal=true`
  (EXACT 8-field) is re-proven on the touched path first.
- **No paper close.** A wave closes on a measured bench row, never on "wired"/
  "integrated"/"future consumer". Every miss → REDRESS evidence or a routed residual.
- **No deferrals; PRUNE before rebuild.** W1 deletes the fact-stream + W5C array before
  routing CSS into the tape; it does not add a parallel path.
- **Hard cap (dispatch-hard-cap).** Each dispatch carries "HARD CAP: N min. At 0.9N
  commit, at N halt" (research 20, plan 15, redress 30; CHALLENGE 90; ≤90 min wave wall
  per SPEC §2). An overrun surfaces to the user as an extension decision.
- **Revert is commit-sliced**, not hand-edited (per each wave's SPEC revert protocol).

## Per-wave dispatch lines

### W0 — Baseline + Telemetry Lock + lightningcss CSSOM re-baseline (AUTHORIZED NOW)

Entry: `skinny/RESULTS.md` is the SK-V16 close baseline; the W0 plan names the
`SK-V17-open` capture method + no-behavior-change proof. No CHALLENGE required (0 behavior
LOC). Owner: `bbnf-bench/`, `xtask/src/`, `RESULTS.md`. Exit gate (SPEC §3): `SK-V17-open`
captured; per-corpus lightningcss full-CSSOM median emitted N≥50 cold for all four
benched corpora; `gate-json` rejects `css_sample_count<50` / non-median / non-cold /
non-full-cssom / phantom-`normalize` / single-tuple-broadcast (proven by fixture); JSON
51/51 + CSS guard rows within ±1.0%; no behavior LOC. Cap ≤90 min. Downstream: W0
rejection blocks W1–W5.

### W1 — PRUNE: retire fact-stream + W5C array → tape activation (CHALLENGE required)

Conditional on W0 close. First-of-class, substrate-touching → CHALLENGE interposed.
Candidates L2+L7+L3 (minimal cursor). Entry: tape-activation seam-flip site
(`regen_css.rs:45-153`) + W5C deletion (`codegen/src/lib.rs:336`) + `BackendRule`-derived
routing + O(1) checkpoint/truncate named; CHALLENGE accepts no-second-substrate +
not-a-renamed-REDRESS-50-55/60-72. Exit gate (SPEC §4, MEASURABLE): `tape_activated=true`
(grep `TapeBuilder|ValueRef|PayloadArena` over `grammars/css_l4_*/` non-zero +
PayloadArena counters); `w5c_profile_array_retired=true` (grep W5C over `skinny/crates/`
ZERO); `css_typed_summary_equal=true` EXACT 8-field; JSON 51/51 ±1.0%; NO speed admission;
**NO dangling `emit_fact_stream` round-trip assertion** (the seven `runtime/src/lib.rs`
test fns + the seven `grammars/css_l4_*/parser.rs:6` live entries + the three
`codegen/src/lib.rs` `.contains("emit_fact_stream")` asserts are migrated to the tape plane
or deleted in this commit — same-wave-consumer non-negotiable). Owner LOC ≤450 + named
generated CSS output. Cap ≤90 min. **Post-W1 obligation:** run the
typed-tape re-profile (N≥50) that gates W4. Downstream: W1 rejection blocks W2–W5.

### W2 — Layout-driven lazy projection generator (CHALLENGE required)

Conditional on W0/W1 close. First-of-class codegen generator → CHALLENGE interposed.
Candidates L3 (full rider) + L8 + L4. Entry: the `BackendRule`-walk recipe + four
generated artefacts (`document/value/view/visitor`) + eager-tree population-parity
baseline named; CHALLENGE accepts L8-flag-as-branch-tag (not catalogue) + L1/L4-index-is-
offsets. Exit gate (SPEC §5, MEASURABLE): `lazy_view_generated=true`; **JSON
`value_from_ref` rider re-emits byte-equal THROUGH the new generator (diff empty); a
CSS-only generator that leaves JSON untouched FAILS (R-CH2-1/CH2)**;
`css_rich_ast_preserved=true` (value-plane population parity); `projection_generality_
exercise ∈ {json, css_l4}`; per-corpus typed-median N≥50 emitted, **no worse than -2.0%
median vs the W1 typed-tape baseline** (`track1_typed@W2(c) ≥ -2.0%` vs W1, P3-C §2.2/§3 —
the bench-falsifiable band; cross-bar deferred to W3); equality re-proven; JSON 51/51
±1.0%; Lock 14 + CSS non-JSON proof pass.
Owner LOC ≤450 (≤650 only with an accepted pre-redress fit proof per SPEC §5 — a ≤5-LOC
per-artefact LOC accounting attributing the over-450 lines to a named intrinsic cause) +
named generated artefacts. Cap ≤90 min. Downstream: W2 rejection blocks W3.

### W3 — NEON structural index, re-profiled (CHALLENGE required) — THE >SOTA WAVE

Conditional on W0/W1/W2 close. First-of-class primitives → CHALLENGE interposed.
Candidates L1 (eq-set classifier) + L5 (comment mask, net-new) + L6 (bracket mask,
net-new). Entry: the RE-PROFILE (N≥50, benched tape path) naming the surviving scan leaf
as top-N AFTER the alloc floor fell — if no scan leaf survives, NO kernel lands (no orphan
kernel) and the >SOTA gate is evaluated on the W2 plane; scalar-ref + checkasm per
primitive present; CHALLENGE accepts index-is-offsets + L6-scalar-balance-default (CTZ
consumer-only/parity-gated, not default) + eq-set-fan-not-lo6 + no-udot/i8mm. Exit gate
(SPEC §6, MEASURABLE — THE >SOTA GATE): **≥1 regular corpus (animate OR bootstrap) crosses
`delta_vs_lightningcss > 1.0×` at N≥50 cold median, full-cssom plane, rich-ast intact,
equality re-proven;** tailwind benched cold N≥50 ADMIT-if->1.0×-else-REPORT-honest-residual;
material median reported; `native_simd_status ∈ {parity-pass, checkasm-pass}`;
`simd_non_json_exercise=css_l4`; JSON 51/51 ±1.0%; index IS the tape. Owner LOC ≤450 +
named generated SIMD. Cap ≤90 min. Downstream: a regular-corpus cross meets the tranche
criterion; no cross → honest residual + PASS-ALPHA §8 WARN.

### W4 — Commit-by-construction Alt-mode (DOUBLY CONDITIONAL)

Conditional on W1 close AND the post-W1 re-profile (N≥50) surfacing the
recognition-control loop / a speculative-rollback leaf as top-N self-time. **If the
re-profile does NOT surface a rollback leaf, W4 does NOT dispatch; L9 is recorded
not-needed (NOT a failure).** Candidate L9 riding the SK-V16-banked O(1) `offsets.len()`
checkpoint/truncate. Entry: the re-profile evidence + the Alt shape + byte-identical-tape
parity proof named. Exit gate (SPEC §7, MEASURABLE): byte-identical tape with/without the
Alt-mode pass; a measured **≥ +5%** N≥50 lift on the gated corpus vs the W3 plane (below
+5% disposes L9 as not-warranted, measurably, not a failure); any W3-crossed regular
corpus stays crossed; JSON 51/51 ±1.0%; equality held. Owner LOC ≤300. Cap ≤90 min.
Downstream: feeds W5 close.

### W5 — Close, clean regen, Lock-14 audit, Alpha feedback

Conditional on W0–W4 dispositions. Entry: every prior wave admitted/rejected/routed with
REDRESS/RESULTS/HANDOFF present. Exit gate (SPEC §8, MEASURABLE): `dirty_generated_state=
clean` (`cargo xtask regen --check` 9/9 exit 0); no CSS/JSON policy in generic crates
(Lock-14 audit + CSS non-JSON proof); every wave admitted/rejected/routed; JSON 51/51 GO;
rich-ast intact; equality re-proven; tranche criterion recorded TRUE or honest residual
escalated PASS-ALPHA §8 WARN (NOT paper-closed); no accepted source change lacks profile/
threshold/REDRESS-id/Lock-14-proof/same-wave-consumer. Owner LOC 0 default (≤150 named
Lock-14 cleanup). Cap ≤90 min. Downstream: on close, Pass Alpha dispatches SK-V17→SK-V18.

## Convergence / escalation

- Each wave returns ADMIT / REJECT / ROUTED, recorded in REDRESS + RESULTS + HANDOFF.
- The tranche succeeds when ≥1 regular corpus (animate OR bootstrap) crosses the
  lightningcss full-CSSOM bar at N≥50 median, with preserve-rich-ast intact, EXACT
  cssparser equality re-proven, and JSON 51/51 held.
- If NO regular corpus crosses after the four-lever stack (W1–W4), the tranche records
  the honest residual and escalates to the user with a `WARN` verdict naming the surviving
  hot leaf and the per-corpus deltas — NOT a paper-close, NOT a corpus-average claim.
- tailwind landing short is acceptable and recorded; it is NOT tranche-blocking provided a
  regular corpus crosses.
