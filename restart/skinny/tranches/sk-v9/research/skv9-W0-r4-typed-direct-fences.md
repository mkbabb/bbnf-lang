# SK-V9 W0 R4: Typed/Direct Row Admission Fences

Date: 2026-05-18.
Role: W0 research R4.
Scope: typed/direct row admission fences for the SK-V9-open telemetry-lock.
Output: this research schema only. No source, generated output, benchmark row,
`skinny/RESULTS.md`, or `skinny/REDRESS.md` change is dispatched here.

## Contract Summary

W0 is a recovery telemetry-lock, not a behavior wave. It may update run identity,
report labels, replay metadata, manifest validation, and diagnostic fences. It
must not move parser/scanner/SIMD/codegen behavior, throughput cells,
Apache/CITM measured row admission, direct product claims, or strict admission
from deferred/view-boundary rows
(`restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md:40-49`).

R4 binds the typed/direct admission fences that W0 must make explicit:

1. A pure W0 refresh starts from the current 38 main rows. It does not add
   `apache_builds/real_typed_struct` or `citm_catalog/real_typed_struct` as
   measured rows.
2. Apache/CITM remain `source_product_only` until a later measured typed-row
   admission wave supplies fresh run-id/metadata evidence, generated Track 1
   DirectBuild, independent serde/oracle proof, sonic typed parity, and rendered
   `A / GO` rows.
3. `canada/real_typed_struct` remains rejected until a fresh full-fixture
   DirectBuild-vs-serde checksum proof exists. Length-only, digest-only,
   coordinate-count-only, or schema-presence-only proofs are invalid.
4. `direct_to_struct` rows with `Output plane=digest` remain guard/control-path
   evidence. They cannot be relabeled as typed product proof without a direct
   output contract or control-path tranche accepted outside W0.
5. Existing rows remain valid W0 rows: four measured `real_typed_struct A / GO`
   rows stay typed direct product-plane rows, and the three current
   `direct_to_struct A / GO` rows stay digest-plane GO rows without becoming
   typed product proof.

## Inputs Read

- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-A-results-extraction.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-D-validated-invalidated.md`
- `restart/skinny/tranches/sk-v9/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/V1/CH3.md`
- `skinny/REDRESS.md` entries 91, 92, and 93
- `skinny/crates/bbnf-bench/src/real_typed_struct.rs`
- `skinny/crates/bbnf-bench/src/gate.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/report.rs`

## Current Authority

SK-V9 opens from the W0-rendered SK-V8 report authority. The current main-table
state is 17 `parse_only` rows, 17 `direct_to_struct` rows, and 4 measured
`real_typed_struct A / GO` rows. The SK-V9 cycle close condition already binds
R4's fences: SK-V8 source/product parity cannot count as measured Apache/CITM
row-table progress, direct digest routes cannot be product proof, current GO
rows must maintain, and REDRESS 91-93 remain route boundaries
(`restart/skinny/tranches/sk-v9/SYNTHESIS.md:81-95`).

Typed row-table candidates are explicitly not W0 rows:

| Row | Current status | W0 R4 state |
|---|---|---|
| `apache_builds/real_typed_struct` | Source/product parity admitted by REDRESS 91; absent as measured `RESULTS.md` row | `source_product_only_forbid_w0_measured_row` |
| `citm_catalog/real_typed_struct` | Source/product parity admitted by REDRESS 91; absent as measured `RESULTS.md` row | `source_product_only_forbid_w0_measured_row` |
| `canada/real_typed_struct` | Rejected by REDRESS 91 on full-fixture DirectBuild-vs-serde checksum mismatch | `rejected_checksum_forbid_shortcut` |

The current direct surface remains digest-plane:

| Direct group | Rows | W0 R4 state |
|---|---|---|
| Current direct GO | `citm_catalog/direct_to_struct`, `marine_ik/direct_to_struct`, `unicode_basic/direct_to_struct` | `digest_guard_go_not_product_proof` |
| W4 selected misses | `apache_builds/direct_to_struct`, `numbers/direct_to_struct`, `random/direct_to_struct` | `digest_guard_no_go_scalar_parent_rejected` |
| Other direct misses | Remaining 11 `N-direct / NO-GO` rows | `digest_guard_no_go_contract_required` |

## Current Code Surface

`real_typed_struct.rs` already carries source/product support for six typed
fixtures: `twitter`, `apache_builds`, `citm_catalog`, `update_center`, `mesh`,
and `marine_ik` (`skinny/crates/bbnf-bench/src/real_typed_struct.rs:10-17`,
`skinny/crates/bbnf-bench/src/real_typed_struct.rs:182-191`). That support is
not the same thing as row-table admission. The typed proof helper checks
generated Track 1, Track 2, serde_json, and sonic-rs checksums
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:310-323`), and the full
Apache/CITM fixture parity test exists as source/product evidence only
(`skinny/crates/bbnf-bench/src/real_typed_struct.rs:609-618`).

`bin/gate.rs` already gives W0 the key fence: real-typed metadata expectations
are derived from the measured SK-V8-open baseline, not from the source fixture
map (`skinny/crates/bbnf-bench/src/bin/gate.rs:1075-1117`). The typed metadata
specs are required only when `real_typed_expected` is true
(`skinny/crates/bbnf-bench/src/bin/gate.rs:1298-1340`). R4 preserves that
baseline-driven policy.

`report.rs` validates the W0 report against the exact SK-V8-open baseline row
set and rejects missing, duplicate, unknown, outcome-moved, verdict-moved, or
throughput-moved rows (`skinny/crates/bbnf-bench/src/report.rs:496-532`). The
baseline contains four measured real-typed rows and no Apache/CITM/Canada
real-typed rows (`skinny/crates/bbnf-bench/src/report.rs:678-684`,
`skinny/crates/bbnf-bench/src/report.rs:755-775`,
`skinny/crates/bbnf-bench/src/report.rs:813-818`). It also names all direct
rows, including the current digest-plane GO rows and W4 residual misses
(`skinny/crates/bbnf-bench/src/report.rs:692-725`,
`skinny/crates/bbnf-bench/src/report.rs:778-888`).

`gate.rs` strict admission evidence requires strict row/comparator state,
`parse_utf8=measured-row`, matching row/comparator planes, fresh native strict
comparator evidence, and `measured_validation_path=measured-row`
(`skinny/crates/bbnf-bench/src/gate.rs:58-70`,
`skinny/crates/bbnf-bench/src/gate.rs:136-176`). Current W0 rows are deliberately
`Strictness=deferred`, `parse_utf8=view-boundary`, and
`measured_validation_path=view-boundary`
(`skinny/crates/bbnf-bench/src/report.rs:1096-1120`), so W0 cannot create strict
SOTA admission from the refreshed manifest.

## R4 Fence Schema

R4 should be represented as gate-consumed admission-state fields or equivalent
derived predicates. The schema below is a research shape, not a new source patch.

| Field | Allowed values | Required binding |
|---|---|---|
| `admission_scope` | `current_w0_measured`, `source_product_only`, `rejected_checksum`, `unsupported_absent`, `digest_guard` | Separates source support, rejected typed attempts, unsupported fixtures, and measured rows. |
| `main_table_permission` | `render_current_baseline`, `forbid_in_w0`, `future_candidate_only` | W0 main table starts from the 38 baseline rows; Apache/CITM/Canada real-typed rows use `forbid_in_w0`. |
| `product_proof_kind` | `typed_direct_product`, `digest_guard_only`, `none_absent`, `rejected` | Direct digest rows must never emit `typed_direct_product`. |
| `typed_checksum_contract` | `track1_track2_serde_sonic_full_fixture`, `not_applicable`, `missing`, `failed` | Canada requires full-fixture parity before any future typed route can reopen. |
| `metadata_basis` | `sk_v8_open_baseline`, `future_measured_admission`, `redress_absence`, `unsupported_fixture` | Prevents source-only typed fixtures from becoming W0 metadata requirements. |
| `strict_admission_state` | `deferred_view_boundary_non_strict`, `measured_row_required`, `not_applicable_guard` | W0 rows stay non-strict unless a later accepted wave changes measured validation. |
| `redress_boundary` | `none`, `REDRESS-91`, `REDRESS-93` | Every blocked shortcut carries the route boundary that explains rejection. |

### Row-State Mapping

| Row family | `admission_scope` | `main_table_permission` | `product_proof_kind` | `metadata_basis` | `redress_boundary` |
|---|---|---|---|---|---|
| `twitter/real_typed_struct` | `current_w0_measured` | `render_current_baseline` | `typed_direct_product` | `sk_v8_open_baseline` | `none` |
| `update_center/real_typed_struct` | `current_w0_measured` | `render_current_baseline` | `typed_direct_product` | `sk_v8_open_baseline` | `none` |
| `mesh/real_typed_struct` | `current_w0_measured` | `render_current_baseline` | `typed_direct_product` | `sk_v8_open_baseline` | `none` |
| `marine_ik/real_typed_struct` | `current_w0_measured` | `render_current_baseline` | `typed_direct_product` | `sk_v8_open_baseline` | `none` |
| `apache_builds/real_typed_struct` | `source_product_only` | `forbid_in_w0` | `none_absent` | `future_measured_admission` | `REDRESS-91` |
| `citm_catalog/real_typed_struct` | `source_product_only` | `forbid_in_w0` | `none_absent` | `future_measured_admission` | `REDRESS-91` |
| `canada/real_typed_struct` | `rejected_checksum` | `forbid_in_w0` | `rejected` | `redress_absence` | `REDRESS-91` |
| All `direct_to_struct` rows | `digest_guard` | `render_current_baseline` | `digest_guard_only` | `sk_v8_open_baseline` | `REDRESS-93` only when scalar-parent/direct-product claims are made |

## W0 Reject Predicates

The W0 gate should fail closed on these R4 predicates. If a pure W0 artifact only
renders absence ledger text outside the main row table, these predicates still
apply to every row or absence entry.

| Predicate | Reject when | Purpose |
|---|---|---|
| `r4_apache_citm_measured_row_in_w0` | `json/apache_builds/real_typed_struct/main` or `json/citm_catalog/real_typed_struct/main` is rendered as a measured W0 main row without the later typed admission gate. | Prevents counting six measured real-typed rows from source/product evidence. |
| `r4_source_fixture_metadata_inflation` | W0 requires Apache/CITM real-typed Criterion metadata merely because `fixture_for_name` supports them. | Preserves measured-baseline metadata expectations. |
| `r4_canada_typed_shortcut` | Canada is admitted or made fixture-supported using length, digest, field count, coordinate count, schema presence, or partial-fixture parity instead of full fixture checksum parity across generated Track 1, Track 2/serde, serde_json, and sonic-rs. | Preserves REDRESS 91's checksum failure. |
| `r4_direct_digest_product_claim` | A row with `workload=direct_to_struct` and `Output plane=digest` emits `product_proof_kind=typed_direct_product` or claims product-plane admission. | Prevents digest guard evidence from becoming product proof. |
| `r4_scalar_parent_alias` | A direct row claims movement through scalar-parent folding, parent-digest folding, or a renamed equivalent without a V9-aware checked gate, full-table maintain proof, and independent Track 2 digest-arithmetic backstop. | Preserves REDRESS 93. |
| `r4_strict_from_deferred_view_boundary` | A current W0 typed/direct row is used as strict admission while strictness is deferred or measured validation remains view-boundary. | Keeps W0 telemetry non-strict. |
| `r4_existing_row_invalidation` | Current measured typed rows or current direct rows are removed, relabeled, or demoted by the fence despite unchanged W0 evidence. | Keeps existing rows valid while adding fences. |

## Falsifiability Gates

- `r4_w0_main_row_count`: pure W0 emits exactly the current 38 main rows. Extra
  Apache/CITM/Canada `real_typed_struct` main rows reject. Missing existing
  typed/direct baseline rows reject.
- `r4_apache_citm_absence_is_explicit`: Apache/CITM may appear only as absence
  ledger or candidate text with `admission_scope=source_product_only`,
  `main_table_permission=forbid_in_w0`, and `metadata_basis=future_measured_admission`.
- `r4_canada_absence_is_explicit`: Canada typed state must be
  `admission_scope=rejected_checksum`, `typed_checksum_contract=failed`, and
  `redress_boundary=REDRESS-91` until fresh full-fixture checksum proof exists.
- `r4_direct_plane_guard`: every `direct_to_struct` row keeps
  `product_proof_kind=digest_guard_only` while `Output plane=digest`.
- `r4_existing_typed_go_maintain`: current measured typed rows remain valid W0
  rows: `twitter`, `update_center`, `mesh`, and `marine_ik`.
- `r4_existing_direct_go_maintain`: current digest-plane direct GO rows remain
  valid W0 rows: `citm_catalog`, `marine_ik`, and `unicode_basic`.
- `r4_strict_boundary`: W0 fences do not convert deferred/view-boundary rows into
  strict SOTA admission. Strict admission requires a later measured-row validation
  path accepted by the strict comparator gate.

## Future Admission Boundaries

Apache/CITM measured admission belongs to the Alpha-E typed row-table candidate,
not to W0. The future wave must satisfy the existing gate:

- row ids `json/apache_builds/real_typed_struct/main` and
  `json/citm_catalog/real_typed_struct/main`;
- full same-run metadata, sample count 100, matching input hash and bytes;
- checksum parity across generated Track 1, serde_json Track 2/oracle, and
  sonic-rs typed;
- selected comparator `sonic_rs_strict` on the `typed direct` plane;
- generated Track 1 at least `sonic_rs_real_typed_struct / 1.10`;
- current real-typed GO rows maintain their accepted floors
  (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:94-115`).

Direct product proof belongs to a later direct output contract or control-path
tranche, not to W0. The future wave must satisfy the Alpha-E direct gates:

- no digest row relabeled as typed product proof;
- control-path rows get stable workload identity distinct from `real_typed_struct`;
- selected rows meet Track 1 and Track 2 floors without scalar-parent folding;
- current direct GO rows maintain;
- any product-output route checksum-matches serde_json and sonic-rs on the
  `typed direct` plane; digest-only evidence cannot satisfy the product gate
  (`restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md:278-302`).

## Verification Hooks For A Later W0 Patch

Focused tests should be enough; W0 R4 does not need broad behavior coverage if
it does not touch parser/scanner/SIMD/codegen behavior.

- `w0_forbids_source_only_apache_citm_real_typed_rows`
- `w0_keeps_apache_citm_real_typed_metadata_optional`
- `w0_forbids_canada_length_or_digest_typed_shortcut`
- `w0_direct_digest_rows_are_not_product_proof`
- `w0_existing_real_typed_go_rows_stay_valid`
- `w0_existing_direct_go_rows_stay_valid`
- `w0_rejects_strict_claim_from_deferred_view_boundary_typed_or_direct_row`

Smoke commands for a later W0 implementation slice:

```sh
(cd skinny && cargo test -p bbnf-bench report::tests gate::tests)
(cd skinny && cargo xtask check-real-typed)
(cd skinny && cargo xtask gate-json --advisory --check-results)
```

Expected local caveat: the current P1 hardening records that
`cargo xtask gate-json --advisory --check-results` fails on existing cache
coherence (`twitter SIMD metadata invalid: SIMD metadata is from a different
capture`). That reinforces W0 recovery; it is not permission to weaken row
admission fences.

## Disposition

R4 is safe to fold into the SK-V9 W0 research packet as a telemetry-lock schema.
It forbids Apache/CITM measured row admission in W0, keeps Canada typed routed
behind full-fixture checksum proof, prevents direct digest product laundering,
and preserves the existing measured typed/direct rows as valid W0 baseline rows.
