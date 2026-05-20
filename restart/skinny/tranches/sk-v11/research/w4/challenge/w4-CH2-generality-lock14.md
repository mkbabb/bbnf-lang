# SK-V11 W4 CH2: Generality / Lock 14

Date: 2026-05-20.
Scope: W4 mandatory CHALLENGE, CH2 generality / Lock 14 lens.
Output: this file.
Disposition: ACCEPT.

## Read Set

- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R1-generated-dispatch-lowering.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R2-json-generated-runtime.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R3-direct-oracles.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R4-gate-report-consumption.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R5-row-floors.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R6-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `restart/skinny/tranches/sk-v11/HANDOFF.md`
- `skinny/REDRESS.md`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

## CH2 Question

Can W4 proceed as a JSON-local generated direct/container-tail slice without
claiming grammar generalization, leaking JSON policy into generic code, relying
on REDRESS 113 as non-JSON proof, or introducing a directive/BIR/substrate route?

## Verdict

ACCEPT.

W4 may proceed to redress as the plan's JSON-local D1
`container_tail_next` slice for `random/direct_to_struct/main`. This acceptance
does not close the non-JSON generalization axis and does not authorize generic
code behavior changes. It accepts the SPEC Section 8 owner-path correction that
adds `skinny/crates/codegen/src/sink_direct.rs`, because that file is the source
renderer for the generated direct sink parser. W4 remains bounded to generated
JSON direct output plus independent JSON direct Track 2 and gate/report
consumption.

If implementation needs a semantic edit in generic lowering, BIR, directives,
shared runtime, or grammar-neutral code, W4 returns REVISE before source work or
records REDRESS rejection. The live REDRESS 113 block remains carried forward.

## Checks

| Check | Assessment | Evidence |
|---|---|---|
| Owner-path SPEC correction for `sink_direct.rs` | ACCEPT | The plan identifies the omission and says the generated direct parser is emitted by `skinny/crates/codegen/src/sink_direct.rs`, so `lower/sink_only.rs` plus `json_templates/generated.rs` cannot change the direct sink path (`w4-plan-container-tail-direct.md:28-41`). Current SPEC Section 8 now names `sink_direct.rs` in W4 owner paths (`SPEC.md:494-506`). R2 confirms `emit_with_layout` appends `sink_direct::render(sink_only)` and that `sink_direct.rs` emits the direct entry, value dispatch, container rules, number rules, and utility rules (`w4-R2-json-generated-runtime.md:51-77`). Older R1/R6 owner-gap text is therefore a pre-correction hazard, not a current blocker. |
| REDRESS 113 non-JSON block | ACCEPT | REDRESS 113 records W2 as `BLOCKED` because W1b admitted no generated non-JSON baseline and W2 may not create the first measurable non-JSON row (`skinny/REDRESS.md:3340-3355`). SPEC Section 8 carries that block directly into W4: REDRESS 113 is not generic-edit proof (`SPEC.md:508-511`). The W4 plan repeats that W2's non-JSON axis remains blocked and cannot be used as a generic-code proof (`w4-plan-container-tail-direct.md:36-38`, `76-89`). |
| Generic-code risk | ACCEPT, bounded | SPEC still requires same-wave CSS L4 / Sheets / BBNF-self proof for generic/codegen/runtime-outside-JSON behavior changes (`SPEC.md:177-178`, `230-245`). W4 can pass CH2 only because the planned source work is JSON-local: `sink_direct.rs` emits `JsonSink` direct generated code, regenerated into `runtime/src/grammars/json/generated.rs`, while `lower/sink_only.rs` is conditional and limited to renderer metadata/tests with no semantic edit (`w4-plan-container-tail-direct.md:45-64`, `91-110`). A semantic change to `ir`, `passes`, generic `sink_only` lowering, `json_provider` generality, or shared runtime policy would exceed this acceptance. |
| JSON policy leakage | ACCEPT | R2 states the direct parser is a cursor-over-bytes sink path and does not use `ParserState`, the offset tape, or the structural scanner (`w4-R2-json-generated-runtime.md:86-113`). It also says direct byte control is local to `sink_direct.rs` and the appended runtime section (`w4-R2-json-generated-runtime.md:125-146`). JSON punctuation may remain in generated per-grammar code and JSON-specific direct Track 2; it must not appear as policy in generic crates. |
| New directive/BIR/substrate risk | ACCEPT | The plan's selected D1 helper is scalar, JSON-local generated code with no directive, BIR variant, backend shape, class lane, sidecar, retained cursor, object carry, or parse-only substrate (`w4-plan-container-tail-direct.md:20-26`). R2 says no generic JSON policy/directive/BIR change is needed; existing BIR already carries the required dispatch facts (`w4-R2-json-generated-runtime.md:187-208`). SPEC Section 8 requires CHALLENGE proof of no directive/BIR/substrate change and blocks object/key/value carry, retained cursor, class lane, sidecar, and JSON policy in generic crates (`SPEC.md:508-529`). |
| Track 2 independence under CH2 | ACCEPT | The plan mirrors D1 in `direct_struct.rs` through a local hand helper and forbids calls to generated code or generated helper functions (`w4-plan-container-tail-direct.md:104-110`). R3 maps the row-moving direct Track 2 oracle to `direct_struct::hand::sink_digest`, independent from generated Track 1, with serde/sonic as digest-plane comparators (`w4-R3-direct-oracles.md:111-124`, `184-188`). |
| Gate/report leakage and producer-only telemetry | ACCEPT with required consumption | W4 can reuse existing JSON direct-contract fields and must not add new telemetry fields (`w4-R4-gate-report-consumption.md:68-120`). The W4 branch must admit only selected rows, require both Track 1 and Track 2 to clear the selected W4 floor, mark `wave_id=SK-V11-W4`, `same_wave_consumer_class=gate_json_direct_contract`, a concrete REDRESS id, and digest-plane comparator evidence, then validate the same facts in `report.rs` (`w4-R4-gate-report-consumption.md:74-115`). |
| JSON-local proceed without grammar generalization | ACCEPT | W4 may proceed only as direct-plane closure/fixpoint work while carrying the blocked non-JSON axis (`skinny/REDRESS.md:3352-3355`, `3379-3380`). The plan selects exactly one JSON direct row, `random/direct_to_struct/main`, with a 7878 Mbps floor on both tracks and same-output digest equality across generated Track 1, independent Track 2, serde_json, and sonic-rs (`w4-plan-container-tail-direct.md:66-89`). This is not a grammar-generalization claim. |

## Required Redress Boundaries

- Treat `sink_direct.rs` as accepted W4 owner surface only for the generated JSON
  direct renderer. Do not infer broad codegen or grammar-neutral authority from
  that path.
- Keep `lower/sink_only.rs` semantic behavior unchanged unless CHALLENGE is
  reopened with non-JSON proof authority. Metadata/test-only edits remain
  conditional.
- Regenerate `runtime/src/grammars/json/generated.rs`; do not hand patch
  generated output.
- Keep JSON punctuation, separator, close-byte, and object/array role policy in
  generated JSON-local code or JSON-specific hand Track 2 only.
- Add no directive, BIR variant, `BackendShape`, public substrate API,
  parser-owned sidecar, retained cursor, class lane, object key/value carry, or
  hidden byte/class mask.
- Consume W4 in `gate-json`/`report.rs` with existing fields and fail closed on
  missing W4 provenance, below-floor rows, coupled Track 2, wrong comparator
  plane, direct guard regression, or producer-only telemetry.
- Carry REDRESS 113 forward into W8/W9 unless a later Alpha/Pass-Omega contract
  creates generated non-JSON baseline authority.

## Failure Conditions

Return REVISE before redress if implementation needs generic-code semantic
changes, a broader owner path, new telemetry fields, or a non-JSON/generalized
claim not covered by the W4 plan.

Return REJECT/REDRESS if the source slice lands but `random/direct_to_struct`
misses either Track 1 or Track 2 floor, output parity fails, Track 2 couples to
generated helpers, guard rows regress, or W4 provenance is not consumed by the
gate.

DISPOSITION: ACCEPT
