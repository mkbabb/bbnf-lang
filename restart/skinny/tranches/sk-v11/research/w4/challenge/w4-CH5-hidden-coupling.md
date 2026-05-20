# SK-V11 W4 CH5 Hidden Coupling

Date: 2026-05-20.

Scope: CH5 review of the W4 `container_tail_next` plan, focused on hidden
Track 1 / Track 2 coupling, generated helper leakage, identical bug risk,
report/gate provenance coupling, and stale floor-table mismatch.

Output: this file only.

Disposition: **REVISE before redress**.

## Verdict

The planned D1 shape is directionally admissible, but not ready for redress.
Generated Track 1 may factor a JSON-local container-tail helper in
`skinny/crates/codegen/src/sink_direct.rs` and regenerate
`skinny/crates/runtime/src/grammars/json/generated.rs`; Track 2 may mirror the
shape only as a local `direct_struct.rs` hand-parser helper. That split is not
hidden coupling by itself.

The current plan still under-specifies the fail-closed tests that make that
split trustworthy. In particular, W4 must not rely on the existing generic
direct-contract validator or the stale `sk_v10_direct_floor` table. `random`
has a false-accept band today: the stale report floor is 7734 Mbps, while the
SK-V11 Section 0.4 W4 floor is 7878 Mbps. A W4 row with both tracks in that
band could pass stale validation while missing the actual W4 floor.

Redress is not authorized until the plan requires the negative tests below and
uses one W4 selected-row floor authority shared by producer and validator.

## Materials Read

- `restart/skinny/tranches/sk-v11/research/w4/w4-plan-container-tail-direct.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R1-generated-dispatch-lowering.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R2-json-generated-runtime.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R3-direct-oracles.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R4-gate-report-consumption.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R5-row-floors.md`
- `restart/skinny/tranches/sk-v11/research/w4/w4-R6-preblocked-ledger.md`
- `restart/skinny/tranches/sk-v11/SPEC.md`
- `skinny/crates/codegen/src/sink_direct.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

## Findings

### 1. Hidden shared code

REVISE.

The plan correctly says Track 2 owns its own local helper and must not call
generated Track 1 or generated helper code. That is the right boundary:
generated Track 1 is `runtime::generated_json::parse_direct` through the emitted
SinkOnly parser, while direct Track 2 is `direct_struct::hand::sink_digest`.

The redress plan must make the boundary testable. A shared new helper in
`parse_that_regex`, generic runtime, `json_templates/generated.rs`, or another
common parser module would be hidden coupling unless CHALLENGE reopens the
route. Existing shared low-level string, number, and whitespace helpers are
baseline facts; W4 must not add a new shared container-tail parser primitive.

### 2. Generated helper leakage

REVISE.

The generated helper must remain private generated JSON direct code. It should
be emitted by `sink_direct.rs`, appear only in regenerated JSON output, and not
be exported, re-exported, imported by `direct_struct.rs`, moved into generic
runtime, or used by `track2/json.rs`.

The plan's "local helper" wording is good, but CH5 needs a source-level
negative test or equivalent assertion so this does not become a caller-level
claim with a shared implementation underneath.

### 3. Identical bug risk

REVISE.

Track 1 and Track 2 intentionally implement the same container-tail state
machine. That creates a same-bug risk: both parsers can agree on invalid JSON if
they share an off-by-one, trailing-comma, close-byte, or whitespace-after-comma
mistake. Existing valid-fixture parity is not enough, and serde/sonic parity in
the direct digest path is shape parity for valid rows, not a malformed-input
rejection proof.

The W4 tests must include malformed micro-fixtures and assert generated Track 1,
hand Track 2, serde_json, and sonic-rs all reject. Agreement between Track 1 and
Track 2 alone is insufficient.

### 4. Report/gate provenance coupling

REVISE.

W4 should reuse existing fields, but not the existing generic direct-contract
meaning. A W4 admission must be specifically consumed as `SK-V11-W4` /
`REDRESS-115` with `same_wave_consumer_class =
gate_json_direct_contract`, strict measured-row validation, digest output,
independent Track 2 status, same-run native comparator evidence, and a
W4-specific source delta such as `direct-dispatch-byteset`.

The report validator must reject old W2/W10 direct-contract provenance even
when the Mbps values clear the W4 floor. `track2_independence_status =
independent_verified` must not self-attest if the rest of the W4 provenance is
missing, stale, gate-only, or points at the wrong comparator/source row.

### 5. Stale floor-table mismatch

REVISE, blocking.

`report.rs` currently validates direct movement through `sk_v10_direct_floor`,
whose W4 candidate values do not match SK-V11 Section 0.4. For the selected
plan row:

| Row | SK-V11 W4 floor | Current `sk_v10_direct_floor` |
|---|---:|---:|
| `random/direct_to_struct` | 7878 | 7734 |

This is a hidden gate/provenance coupling defect. Producer and validator must
use one W4 selected-row floor helper, or an admitted row can depend on which
side of the gate reads which table. The helper must be W4-specific and selected
row scoped; unselected W4 candidates must remain W0-clamped even when their
fresh numbers happen to clear a floor.

## Required Negative Tests

These tests are required before W4 source redress:

1. **Generated helper leakage:** fail if direct Track 2 calls
   `runtime::generated_json`, generated SinkOnly helpers,
   `container_tail_next_direct`, or any generated Track 1 container-tail symbol.
   Also assert the generated helper is not public API or re-exported.

2. **No new shared parser helper:** fail if the W4 container-tail helper lands
   in `parse_that_regex`, generic runtime, `track2/json.rs`, or another shared
   parser module rather than separately in generated Track 1 and local hand
   Track 2 code.

3. **Malformed container tails:** table-test generated Track 1, hand Track 2,
   serde_json, and sonic-rs rejection for at least trailing comma in object and
   array, missing comma, missing colon, wrong close byte, comma followed by
   close after whitespace, empty value after comma, and extra root value.

4. **W4 false-accept floor band:** gate and report both reject
   `random/direct_to_struct` when Track 1 and Track 2 are above the stale 7734
   Mbps floor but below the SK-V11 W4 7878 Mbps floor.

5. **Unselected candidate clamp:** an unselected W4 candidate from `canada`,
   `mesh`, `update_center`, `github_events`, or `twitter` remains
   `N-direct / NO-GO` even if synthetic Track 1 and Track 2 values clear its
   floor.

6. **Old direct-contract provenance:** report validation rejects a W4 candidate
   row carrying `SK-V10-W2`, `SK-V10-W10`, `REDRESS-101`, `REDRESS-109`,
   `direct-reclaimed`, or `direct-residual` provenance, even with passing Mbps.

7. **Gate-only or stale W4 provenance:** report validation rejects
   `gate_only`, missing `REDRESS-115`, stale `SK-V9-open`, non-`SK-V11-W4`
   wave id, wrong `sk_v9_open_delta`, deferred validation, non-digest output,
   and non-`independent_verified` Track 2 status.

8. **Comparator/source mismatch:** report validation rejects W4 rows whose
   native comparator evidence is not same-run direct `sonic_rs_direct_to_struct`
   / `serde_json_direct_to_struct` on the digest plane for the selected row.

9. **Guard regression:** if W4 claims Section 0.5 guard coverage, gate/report
   tests fail on direct guard floor misses for `citm_catalog`, `apache_builds`,
   `marine_ik`, or `unicode_basic`.

## Required Plan Revision

Before redress, revise the W4 plan to:

- replace optional "if already covered" language with the required negative
  tests above;
- add one selected-row W4 floor helper consumed by both `gate.rs` and
  `report.rs`;
- make W4 validation reject stale W2/W10 direct-contract provenance;
- keep Track 1 and Track 2 helpers separate in source and generated output;
- keep the malformed-input rejection proof separate from valid-row digest
  parity.

DISPOSITION: REVISE before redress.
