# SK-V9 SPEC - Recovery Wave Plan

Date: 2026-05-18.

Status: post-G-Alpha skinny packet. G-Alpha is closed by user instruction, but
S-P1 V1 did not converge. This SPEC therefore opens SK-V9 with one executable
wave: W0 telemetry-lock recovery. Behavior waves are listed as conditional
placeholders only until W0 closes and a fresh post-W0 S-P1 rerun converges.

Authority:

- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/p1/`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-r1-gate-report-baseline.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-r2-criterion-metadata.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-r3-diagnostic-fences.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-r4-typed-direct-fences.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-r5-lock14-redress.md`
- `restart/skinny/tranches/sk-v9/research/skv9-W0-r6-spec-dispatch-shape.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`

Dispatch lock:

- G-Alpha is closed.
- S-P1 V1 is an opening gap ledger, not a completed profile.
- W0 is the only currently dispatchable wave.
- W1+ behavior waves require `G-W0-TELEMETRY-LOCK`,
  `G-S-P1-RERUN-CONVERGED`, and a fresh S-P2/S-P3 revision before dispatch.

## Section 0 - Close Condition And Goalset

### Section 0.1 - SK-V9 Close Condition

SK-V9 closes only when all of these are true:

1. W0 produces and consumes a coherent `SK-V9-open` telemetry manifest.
2. W0 proves no parser, scanner, SIMD, runtime, codegen, generated-output, typed
   product, direct product, or strict-admission behavior moved.
3. A fresh post-W0 S-P1 rerun converges under hardening and uses SK-V9-open
   evidence rather than absent, stale, or historical rows.
4. Any later behavior candidate is selected by revised S-P2/S-P3 evidence and
   closes by measured row gates or REDRESS.
5. Strict admission remains strict-vs-strict on matching output planes only.
6. Apache/CITM measured typed rows, Canada typed, retained structural
   projection, and direct product routes either admit by their named gates or
   remain explicitly blocked.
7. `skinny/RESULTS.md`, `skinny/REDRESS.md`, `SPEC.md`, `DISPATCH-PROMPT.md`,
   and `HANDOFF.md` agree at close.

### Section 0.2 - Candidate Status

| Candidate | Initial SK-V9 status | Release gate |
|---|---|---|
| SK-V9-open telemetry/gate refresh | W0 dispatchable | `G-W0-TELEMETRY-LOCK` |
| Fresh S-P1 profile rerun | Interlock after W0, not a behavior wave | `G-S-P1-RERUN-CONVERGED` |
| Apache/CITM measured typed row admission | Blocked placeholder | W0 plus S-P1 rerun plus revised S-P2/S-P3 typed plan |
| Retained tape plus structural projection | Blocked placeholder | W0 plus S-P1 rerun plus Lock 14 and proof-first challenge |
| Direct output/control-path proof | Blocked placeholder | W0 plus S-P1 rerun plus direct contract plan |
| SK-V9 close/pass-alpha feedback | Blocked placeholder | All admitted/rejected waves reconciled |

### Section 0.3 - Opening Baseline

The opening benchmark authority remains the current W0-rendered 38-row JSON
report. W0 may relabel and consume that authority as `SK-V9-open`; it may not
add rows, move throughput cells as behavior evidence, upgrade outcomes, upgrade
verdicts, or convert deferred/view-boundary evidence into strict admission.

Opening row families:

| Family | Current row count | W0 posture |
|---|---:|---|
| `parse_only` | 17 | Baseline rows only; no strict SOTA admission. |
| `direct_to_struct` | 17 | Digest guard rows only; direct digest is not typed product proof. |
| `real_typed_struct` | 4 | Existing measured typed rows maintain; no Apache/CITM/Canada measured additions in W0. |

### Section 0.4 - Required Telemetry

W0 must make these fields gate-consumed or fail-closed with an explicit absence
reason:

```text
row_id
grammar_id
domain
corpus
workload
outcome_id
verdict
strictness
output_plane
track1_mbps
track2_mbps
comparator_id
comparator_plane
comparator_strictness
comparator_freshness
measured_validation_path
profile_artifact
sample_cost
sample_count
build_flags
host_triple
feature_mask
costfacts_rule_id
costfacts_chosen_shape
costfacts_rejected_alternative_ids
redress_entry
wave_id
run_id
sidecar_freshness
sk_v9_open_delta
substrate_surface
structural_projection_status
substrate_cardinality
same_wave_consumer_class
track2_independence_status
diagnostic_nonproducer_status
```

Producer-only telemetry rejects. W0 closes only if `gate-json` consumes the
manifest in the same wave.

## Section 1 - Non-Negotiables

- No new directive.
- No new BIR variant.
- No new `BackendShape` variant.
- No `UnionTape`.
- No new substrate surface.
- No public substrate API.
- No parser-owned structural cursor or parser-owned fact slot.
- No parallel or sidecar substrate.
- No JSON policy in generic crates.
- No strict admission except strict-vs-strict on a matching output plane.
- No stale, permissive, lossy, absent, historical, sidecar-only, or
  view-boundary evidence as strict admission.
- No Apache/CITM measured `real_typed_struct` rows in W0.
- No Canada typed shortcut through length, digest, schema, coordinate count, or
  partial-fixture evidence.
- No direct digest row relabeled as typed product proof.
- No structural-scan-only, masking probe, PMU, or Criterion slope artifact used
  as a producer for Track 1, Track 2, typed product, direct product, or strict
  admission.
- No behavior source change without a same-wave consumer and measured gate.
- Research, plan, challenge when required, redress, and close remain distinct.

## Section 2 - Wave Manifest

| Wave | Section | Name | Dispatch status | Owner budget | Hard cap |
|---|---|---|---|---|---:|
| W0 | Section 3 | SK-V9-open Telemetry-Lock Recovery | Dispatchable now | Telemetry/report/gate docs and focused gate code only | <=90 min |
| Interlock | Section 4 | Fresh S-P1 Rerun | Conditional after W0 | Profile artifacts and S-P1 research/hardening docs | <=90 min |
| W1 | Section 5 | Revised S-P2/S-P3 Candidate Release | Blocked placeholder | Research/plan docs only until release | <=90 min |
| W2 | Section 6 | Typed Row Admission Candidate | Blocked placeholder | Exact typed owner paths only after release | <=90 min |
| W3 | Section 7 | Tape Plus Structural-Projection Candidate | Blocked placeholder | Exact retained tape owner paths only after release | <=90 min |
| W4 | Section 8 | Direct Contract Candidate | Blocked placeholder | Exact direct/output-contract owner paths only after release | <=90 min |
| W5 | Section 9 | Close And Alpha Feedback | Blocked placeholder | Docs/results/redress reconciliation | <=90 min |

Behavior placeholders are not dispatch authority. Any W1+ behavior attempt
before `G-BEHAVIOR-RELEASE` returns REVISE without source edits.

## Section 3 - W0 Telemetry-Lock Recovery

Objective: make the opening telemetry self-consistent as `SK-V9-open`, consume
it with `gate-json`, and freeze all behavior surfaces.

Owner paths:

| Path | Allowed W0 use |
|---|---|
| `skinny/crates/bbnf-bench/src/bin/gate.rs` | Run identity, manifest production, same-wave consumer validation. |
| `skinny/crates/bbnf-bench/src/report.rs` | Report labels, required telemetry checks, baseline manifest validation. |
| `skinny/crates/bbnf-bench/src/gate.rs` | Strict-admission and comparator refusal checks if required. |
| `skinny/crates/bbnf-bench/src/metadata.rs` | Criterion metadata validation if required. |
| `skinny/crates/bbnf-bench/src/lock14_baseline.rs` | Lock 14 telemetry classification if required. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs` | Metadata emission assertions only, if required. |
| `skinny/crates/bbnf-bench/benches/simd_scan.rs` | SIMD diagnostic metadata assertions only, if required. |
| `skinny/xtask/src/main.rs` | Existing `RESULTS.md` marker checker strings only; no behavior or generic policy. |
| `skinny/RESULTS.md` | Generated W0 report update only after `gate-json --update-results`. |
| `skinny/REDRESS.md` | W0 rejection/admission boundary only if redress changes or rejects a route. |
| `restart/skinny/tranches/sk-v9/` | W0 plan, handoff, close artifacts. |

Freeze paths include parser, scanner, SIMD behavior, runtime, IR, passes,
codegen, grammar inputs, fixtures, generated parser output, generated typed
output, direct structs, real typed source/product logic, and any non-JSON
grammar behavior.

Entry gate:

1. W0 research cohort archived.
2. W0 plan artifact names exact owner paths, revert slice, same-wave consumer,
   and pre-blocked routes.
3. Dirty worktree is inspected and intended slice is staged separately.
4. Fresh Criterion capture is produced with `RUSTFLAGS="-C target-cpu=native"`.

Exit gate `G-W0-TELEMETRY-LOCK` passes only if:

1. `skinny/RESULTS.md` contains `SK-V9-open` and one
   `sk-v9-open:criterion-fnv64-<16 hex>` run id.
2. `gate-json` consumes the manifest and rejects stale/mixed metadata.
3. The manifest has exactly the current 38 main row identities.
4. W0 does not add Apache/CITM/Canada measured typed rows.
5. W0 does not move behavior owner paths.
6. Structural scan, masking probes, PMU, and cycles-per-byte remain diagnostic
   non-producers.
7. Current direct digest rows remain digest guards and are not product proof.
8. `cargo test -p bbnf-bench --lib --bins`, `cargo xtask check-json`,
   `cargo xtask check-real-typed`, and `cargo xtask check-conformance` pass.
9. `cargo xtask gate-json --advisory --check-results` passes after update.
10. `cargo xtask lint-loc` is run and any pre-existing budget debt is recorded.

Revert protocol: if any behavior path changes, if metadata validation is
weakened to pass stale evidence, or if W0 cannot consume the SK-V9-open manifest,
revert the W0 source/report slice and record REDRESS. Do not close W0 by prose.

## Section 4 - Fresh S-P1 Rerun Interlock

After W0, rerun S-P1 against SK-V9-open evidence. The rerun must not use absent
telemetry, stale SK-V4/SK-V8 fused evidence, source-eligible-only typed rows, or
sidecar-historical-only comparators as behavior ancestors.

`G-S-P1-RERUN-CONVERGED` passes only if a hardening consolidation records
convergence and names fresh evidence for any behavior candidate. Otherwise W1+
remains blocked.

## Section 5 - Revised S-P2/S-P3 Candidate Release

W1 is a planning release gate, not behavior redress. It may release W2+ only
after W0 and S-P1 rerun convergence. It must produce revised S-P2/S-P3 artifacts
that bind exact candidate rows, owner paths, proof obligations, comparator
floors, no-regression gates, and REDRESS pre-blocks.

`G-BEHAVIOR-RELEASE` passes only when `G-W0-TELEMETRY-LOCK`,
`G-S-P1-RERUN-CONVERGED`, and revised S-P2/S-P3 candidate gates all pass.

## Section 6 - Typed Row Admission Placeholder

Apache/CITM typed row-table admission remains blocked. A future wave must prove
generated Track 1 direct typed output, independent Track 2 or oracle proof,
same-run strict comparator evidence, current typed GO row maintain, and rendered
row-table admission. W0 source/product parity is insufficient.

Canada typed remains rejected until full-fixture checksum parity is proven across
generated Track 1, Track 2 or oracle, serde_json, and sonic-rs. Length, digest,
schema, field-count, or coordinate-count shortcuts reject.

## Section 7 - Tape Plus Structural-Projection Placeholder

The structural-projection route is blocked by REDRESS 92 and the substrate
ceiling. A future wave must prove a single retained substrate identity with no
new directive, BIR variant, `BackendShape`, `UnionTape`, public substrate API,
sidecar, parser-owned cursor, or `tape_vs_tape` production consumer.

W3 lead candidate, if released by revised S-P2/S-P3, remains the tape plus
structural-projection union only after proof-first challenge accepts its owner
paths, source budget, revert slice, same-wave consumer, and non-JSON/general
surface posture.

## Section 8 - Direct Contract Placeholder

Direct routes remain digest guards until a future wave provides a direct output
contract or control-path contract with generated Track 1 plus independent Track
2/oracle evidence. Digest-only rows cannot be product proof, and scalar-parent
folding remains blocked by REDRESS 93.

## Section 9 - Close And Alpha Feedback

Close is a reconciliation wave only after W0, S-P1 rerun, revised S-P2/S-P3, and
all admitted/rejected behavior waves are recorded. It must reconcile RESULTS,
REDRESS, SPEC, DISPATCH-PROMPT, HANDOFF, and any V10 alpha inputs without hiding
residual risk.
