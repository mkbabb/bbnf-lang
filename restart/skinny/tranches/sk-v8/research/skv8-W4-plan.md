# SK-V8 W4 Plan - Direct Guard Triage

Date: 2026-05-18.

Status: W4 plan challenged and rejected/routed after V1 measurement.

## Scope

W4 selects three `N-direct` rows whose generated Track 1 lane already clears
the same-run sonic strict direct floor and whose failure is confined to the
independent hand Track 2 lane:

| Row | Track 1 floor | Track 1 current | Track 2 floor | Track 2 current | Required move |
|---|---:|---:|---:|---:|---|
| `apache_builds/direct_to_struct` | 8048 Mbps | 8306 Mbps | 8048 Mbps | 7796 Mbps | Track 2 +3.2% |
| `numbers/direct_to_struct` | 7230 Mbps | 9773 Mbps | 7230 Mbps | 6966 Mbps | Track 2 +3.8% |
| `random/direct_to_struct` | 7401 Mbps | 7751 Mbps | 7401 Mbps | 6952 Mbps | Track 2 +6.5% |

The floor is `sonic-rs strict Mbps / 1.10`, from the existing direct guard.

## Implementation Plan

Owner path:

- `skinny/crates/bbnf-bench/src/direct_struct.rs`

Implement only inside the hand Track 2 direct parser:

1. Add parent-folding helpers for object and array scalar values.
2. In object and array parsing, dispatch scalar child values directly into the
   parent digest when the value is string, number, bool, or null.
3. Preserve recursive `JsonDirectDigest` construction for nested object/array
   children.
4. Keep generated Track 1, runtime, codegen, BIR, directives, substrate, and
   generic crates unchanged.

This is not Track 2 coupling. Track 2 still does not call generated SinkOnly,
generated typed helpers, generated Track 1, or a shared benchmark-private
parser. It keeps its own cursor, whitespace, string, number, literal, object,
array, and error logic.

Source/test LOC cap:

- <=300 source/test LOC.

## Falsifiability Gates

Target row gates:

- `apache_builds/direct_to_struct`: Track 1 >= 8048 Mbps and Track 2 >= 8048
  Mbps.
- `numbers/direct_to_struct`: Track 1 >= 7230 Mbps and Track 2 >= 7230 Mbps.
- `random/direct_to_struct`: Track 1 >= 7401 Mbps and Track 2 >= 7401 Mbps.

Guard gates:

- Existing direct GO rows maintain GO:
  `citm_catalog/direct_to_struct`, `marine_ik/direct_to_struct`, and
  `unicode_basic/direct_to_struct`.
- All non-target direct rows are no worse than -2.0% Track 1 and Track 2 versus
  `SK-V8-open`.
- Current `real_typed_struct` GO rows maintain GO.
- Direct correctness parity remains green across Track 1, Track 2, serde_json,
  and sonic-rs.
- Lock 14 remains green because no generic code changes are planned.

Verification commands:

```text
cargo test -p bbnf-bench direct_struct -- --nocapture
cargo xtask check-json
cargo xtask check-conformance
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo bench -p bbnf-bench --bench json_parity -- 'json/(apache_builds|numbers|random)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
cargo bench -p bbnf-bench --bench json_parity -- 'json/(citm_catalog|marine_ik|unicode_basic)/(track1_direct_to_struct|track2_direct_to_struct|sonic_rs_direct_to_struct|serde_json_direct_to_struct)$'
cargo xtask gate-json --advisory --check-results
```

If the checked report gate refuses a row-table refresh because of pre-existing
W0 Criterion run-id drift, W4 must record that explicitly and must not claim a
`skinny/RESULTS.md` row-table admission unless the checked gate accepts the
updated report.

## Revert And Redress

If any selected row misses its floor, any existing direct GO row loses GO, any
non-target direct lane regresses beyond -2.0%, or Track 2 independence is
compromised, revert the source patch and record the failed candidate in
`skinny/REDRESS.md`.

Residual rows after W4 remain routed by output-contract class:

- String/materializer misses stay under REDRESS 54, 55, 66-69, and 72.
- Numeric-array dispatch beyond parent scalar folding routes to a later
  direct-workload control path only with fresh hot-leaf evidence.
- Digest-as-product remains rejected; typed product proof is owned by
  `real_typed_struct`, not W4 direct digest.

## V1 Fold And Disposition

W4 V1 hardening returned REVISE because the original plan under-proved SPEC
Section 7 full-table maintain, lacked a W4-aware checked report path, and had
no Lock 14 W4 parent-diff allowance for `direct_struct.rs`.

A local source attempt implemented the planned hand Track 2 scalar-parent fold
and saved the rejected patch at
`/tmp/skv8-wave4-track2-scalar-fold-rejected.patch`. Correctness passed, but
native Criterion falsified the selected three-row gate: `apache_builds` cleared
sonic/1.10, while `random` remained below sonic/1.10 and `numbers` Track 2
regressed by +6.3287% time. The source patch was reverted, `skinny/RESULTS.md`
remains unchanged, and W4 closes only as a rejected/routed direct guard triage
after V2/V3 hardening accepts this disposition.
