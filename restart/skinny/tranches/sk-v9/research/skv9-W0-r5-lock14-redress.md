# SK-V9 W0 R5: Lock 14 And REDRESS Pre-Block Audit

Date: 2026-05-18.
Pass: SK-V9 W0 research.
Agent: R5.
Scope: Lock 14 owner-path audit and REDRESS pre-block schema for the recovery
W0 telemetry-lock wave.
Output: this file only.
Status: research schema; no implementation dispatch.

## Read Authority

- `restart/skinny/tranches/sk-v9/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v9/HANDOFF.md`
- `restart/skinny/tranches/sk-v9/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-C-redress-digest.md`
- `restart/skinny/tranches/sk-v9/research/alpha/alpha-E-candidate-shortlist.md`
- `skinny/REDRESS.md` entries 91, 92, and 93
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`

Current HEAD while researching: `00c184136ed8371c1f076b6b750e2bec313e7803`
(`docs(sk-v9-p1-hardening): record V1 recovery disposition`).

## Controlling Finding

W0 is a recovery telemetry-lock wave, not a behavior wave. It may update run
identity, report labels, manifest validation, replay metadata, diagnostic
fences, and gate-consumed telemetry. It must not move parser, scanner, SIMD,
codegen, generated-output, product behavior, throughput cells, Apache/CITM
measured row admission, direct product claims, or strict admission from
deferred/view-boundary rows.

This follows the SK-V9 handoff next move: W0 must run with `gate-json` as the
same-wave consumer and no parser/scanner/SIMD/codegen behavior movement
(`HANDOFF.md:68-80`). P1 hardening made the same constraint explicit:
W0 produces and consumes a SK-V9-open report/gate manifest, behavior frozen,
then S-P1 reruns before behavior waves become eligible
(`HARDENING-S-P1-V1-CONSOLIDATED.md:38-70`).

R5 disposition: W0 source planning is safe only under exact telemetry owner
paths and only with fail-closed REDRESS boundaries carried forward unchanged.

## W0 Source Owner Schema

Default W0 source-owner set is the current Lock 14 `bench_gate_schema`
telemetry-only set:

| Owner path | W0 class | Allowed W0 use |
|---|---|---|
| `skinny/crates/bbnf-bench/src/metadata.rs` | `bench_gate_schema` | Metadata field shape, run identity, required telemetry validation. |
| `skinny/crates/bbnf-bench/src/report.rs` | `bench_gate_schema` | Rendered report schema and fail-closed required-field checks. |
| `skinny/crates/bbnf-bench/src/gate.rs` | `bench_gate_schema` | Gate validators, negative fixtures, consumer checks. |
| `skinny/crates/bbnf-bench/src/bin/gate.rs` | `bench_gate_schema` | `gate-json` production/consumption path only. |
| `skinny/crates/bbnf-bench/src/lock14_baseline.rs` | `bench_gate_schema` | Lock 14 allowlist/frozen-root validation. |
| `skinny/crates/bbnf-bench/benches/json_parity.rs` | `bench_gate_schema` | Telemetry/gate benchmark metadata assertions only. |
| `skinny/crates/bbnf-bench/benches/simd_scan.rs` | `bench_gate_schema` | Diagnostic metadata assertions only, not scan behavior. |

These paths are the only current allowlist entries with
`w0_mutability = "telemetry_only"` in `lock14_baseline.rs:272-312`.
Everything else in the allowlist is `read_only`, including grammar inputs,
fixture inputs, generated JSON output, generated typed output, per-grammar
provider/template material, generic surfaces, host/API schema facts, and
`real_typed_struct.rs` (`lock14_baseline.rs:13-325`).

`skinny/xtask/src/main.rs` appears in Alpha-E as a possible W0 owner, but it is
not in the current Lock 14 allowlist. R5 recommendation: avoid touching it in
W0 unless a later W0 plan first names the exact CLI-only need, adds/validates a
Lock 14 telemetry classification in the same gate slice, and proves no behavior
surface changes. The current default should rely on existing `xtask` delegation
to `bbnf-bench` gate code.

## Lock 14 Audit Requirements

The W0 implementation plan must keep these executable Lock 14 constraints:

1. Allowlist entries remain one of the existing classes and either `read_only`
   or `telemetry_only`; unsupported classes or mutability fail validation
   (`lock14_baseline.rs:349-378`, `lock14_baseline.rs:533-552`).
2. Paths naming `UnionTape` or `directive` fail validation
   (`lock14_baseline.rs:371-372`).
3. Frozen roots stay clean before and after W0. The frozen set includes
   `grammars`, `test_data`, test fixtures, runtime, IR, passes, codegen,
   grammar, `bbnf`, SIMD sources/build/ext, parse-that-regex, direct Track 2,
   real typed structs/generated output, track2, parity, scan, materialization,
   and `xtask/src/real_typed_schema.rs` (`lock14_baseline.rs:381-402`).
4. Current parent-diff exceptions are only SK-V8 W2 typed owner paths and SK-V8
   W5 provider-boundary paths (`lock14_baseline.rs:405-414`,
   `lock14_baseline.rs:461-489`). There is no SK-V9 W0 exception for frozen
   parser, tape, SIMD, codegen, generated, direct, or typed source paths.
5. `BackendShape` remains exactly the five expected variants and no `UnionTape`
   text appears in the IR surface (`lock14_baseline.rs:553-576`).

W0 may strengthen the gate, but it may not create a new directive, BIR variant,
`BackendShape`, `UnionTape`, public substrate API, sidecar substrate,
parser-owned fact slot, or generic JSON policy. The SK-V9 Lock 14 gate requires
public API, grammar branch, primitive/table, role/fact, template/provider, and
non-JSON scans for any candidate touching generic CostFacts, codegen, runtime,
SIMD, tape, parser-template, report, or gate surfaces (`SYNTHESIS.md:122-144`).

## REDRESS 91-93 Pre-Blocks

| REDRESS | W0-safe reading | W0 pre-block |
|---|---|---|
| 91 | Apache/CITM `real_typed_struct` are source/product parity only and are not measured rows in the W0 manifest (`REDRESS.md:2622-2659`). Canada typed is rejected on full-fixture checksum mismatch (`REDRESS.md:2637-2640`). | W0 cannot add Apache/CITM measured rows, claim six measured typed GO rows, weaken run-id/metadata checks, or admit Canada via length-only/digest-only evidence. |
| 92 | W3 structural projection was rejected before source because scanner structural positions and retained tape events are not isomorphic, and retained `ValueRef` depends on the current event stream (`REDRESS.md:2663-2690`). | W0 cannot edit SIMD scan, JSON scan, tape layout, generated parser/view/value, codegen templates, parity/materialization, or row reporting to implement structural projection. No sidecar, parser-owned cursor/fact, `tape_vs_tape`, `UnionTape`, `BackendShape`, BIR, directive, or public substrate API. |
| 93 | W4 scalar-parent fold in `direct_struct.rs` was falsified by Criterion, and no Lock 14 parent-diff allowance was admitted (`REDRESS.md:2694-2729`). | W0 cannot touch `direct_struct.rs`, add direct digest arithmetic, reopen scalar-parent folding under a new name, or treat digest rows as product proof. |

## Prior Cluster Pre-Blocks

The Alpha-C cluster ledger remains binding for W0. A W0 source plan must reject
any telemetry change that smuggles one of these routes into report/gate source
or later behavior eligibility without fresh measured evidence, exact owner
paths, same-wave production consumer, no-regression gate, REDRESS citation, and
challenge acceptance (`alpha-C-redress-digest.md:215-234`).

| Cluster | W0 treatment |
|---|---|
| REDRESS 16/17/18/25 | No pair-token fusion, dispatch churn, structural-index typed prepass, separator elision, or generic SWAR whitespace through W0 labels. |
| REDRESS 28+33 and 72/83 | No tiny-string NEON/TBL wiring, global cap-16 policy, or generated-retained `StringBlock16` wrapper. |
| REDRESS 50-55 | No aux/projection side tables, EventCursor, byte-class/structural cursors, sink-local decoded stats, or quote-source streaming hash. |
| REDRESS 60-72 | No retained/direct string-materialization route, direct source-hook folding, parser-owned decoded scratch, hand typed sink, or hidden directive. |
| REDRESS 73 | No generated-retained helper-shape transfer to hand Track 2 or direct control paths. |
| REDRESS 80 | No zero-fallback Eisel-Lemire widening or raw `parse::<f64>()` shortcut. |
| REDRESS 82 | No single-quartet Unicode escape classifier retry. |
| REDRESS 84 and 65 | No object key/value-byte carry or object-pair control compaction retry by helper rename. |
| REDRESS 88/89/90 | No PMULL prefix-XOR default body, CTZ/bulk production consumer, or B6 canary as performance proof. |
| REDRESS 36-38 and 85-86 plus W5 | No generic JSON policy in generic crates; W5 provider cleanup does not authorize behavior or broaden the allowlist. |
| SC-6-L1-R1 / substrate ceiling | No Lock 1 amendment, `UnionTape`, new `BackendShape`, BIR variant, directive, public substrate API, or sidecar cardinality inside SK-V9 W0. |
| Strictness and telemetry | No lossy/permissive/sidecar-only strict admission, CostFacts-only row claim, telemetry-only producer, or `tape_vs_tape` production consumer. |

## W0 Falsifiability Gates

W0 R5 recommends these gates before any W0 source changes close:

1. `w0_exact_owner_paths`: source diff is limited to the telemetry owner set
   above, or the plan explicitly rejects/splits before source edits.
2. `w0_lock14_frozen_clean`: `lock14_baseline::validate` passes, frozen roots
   have no dirty status or parent diff, and `BackendShape` remains five
   variants with no `UnionTape`.
3. `w0_gate_json_same_wave_consumer`: every new or relabeled telemetry field is
   parsed and rejected/accepted by `gate-json` in the same wave. Producer-only
   report fields reject.
4. `w0_required_fields_consumed`: SK-V9 telemetry columns from
   `SYNTHESIS.md:242-296` have either a gate consumer or an explicit
   fail-closed absence reason.
5. `w0_no_behavior_drift`: generated output and frozen behavior roots remain
   byte-identical; throughput cells do not move unless a separate accepted
   measured-row tranche owns them.
6. `w0_redress_preblock_scan`: the W0 patch and report wording contain no
   reopened REDRESS 91, 92, 93, or cluster route under renamed vocabulary.
7. `w0_no_new_surface`: no new directive, BIR variant, substrate/API,
   `BackendShape`, `UnionTape`, parser-owned fact slot, sidecar producer, or
   generic JSON policy.
8. `w0_diagnostic_nonproducer`: structural-scan-only, masking probes,
   cycles-per-byte, PMU, and Criterion slope artifacts are explicitly
   diagnostic non-producers. They cannot populate Track 1, Track 2, strict
   admission, product proof, Apache/CITM measured-row evidence, retained cursor
   state, or parser-owned fact slots (`HARDENING-S-P1-V1-CONSOLIDATED.md:59-65`).

## Verification Command Schema

Minimum commands for a W0 plan that edits telemetry owner paths:

```bash
cd skinny
cargo test -p bbnf-bench lock14_baseline -- --nocapture
cargo test -p bbnf-bench --lib --bins
cargo xtask check-json
cargo xtask check-real-typed
cargo xtask check-conformance
cargo xtask gate-json --advisory --check-results
```

Repository-root checks:

```bash
git diff --check
git diff --exit-code -- skinny/crates/runtime/src/grammars/json \
  skinny/crates/bbnf-bench/src/generated_real_typed.rs \
  skinny/crates/bbnf-bench/src/direct_struct.rs \
  skinny/crates/ir/src \
  skinny/crates/passes/src \
  skinny/crates/codegen/src \
  skinny/crates/bbnf-simd/src \
  skinny/crates/parse-that-regex/src \
  skinny/grammars \
  skinny/test_data
```

If `cargo xtask gate-json --advisory --check-results` fails only on the
pre-existing SK-V8 cache coherence issue noted by P1 hardening, W0 may record
that as the recovery target, but it may not close until the SK-V9-open manifest
is coherently produced and consumed or the plan is split with the failure
recorded.

## Rollback And REDRESS Protocol

If a W0 source slice touches any non-owner path, weakens a validator to pass
stale/mixed evidence, moves a row, edits behavior roots, or reopens a pre-block,
the wave must fail closed:

1. Revert the W0 source slice as a unit before close.
2. Preserve any attempted patch path in the wave redress artifact if source was
   attempted.
3. Do not edit `skinny/RESULTS.md` for partial telemetry.
4. Add a REDRESS entry only if W0 implementation/redress actually rejects or
   routes a source attempt; otherwise keep this research as pre-block evidence.
5. Keep behavior S-P2/P3 waves blocked until W0 plus a fresh S-P1 rerun
   converge (`HANDOFF.md:68-80`; `HARDENING-S-P1-V1-CONSOLIDATED.md:85-90`).

## Close Recommendation

Dispatch W0 only as a gate/report telemetry-lock with exact owner paths:
`metadata.rs`, `report.rs`, `gate.rs`, `bin/gate.rs`, `lock14_baseline.rs`, and
focused bench metadata tests. Do not include parser, tape, SIMD, codegen,
generated output, direct Track 2, real typed schema/source, fixtures, or
`RESULTS.md` row movement in the source slice. Any need outside those owner
paths is a split-before-dispatch condition, not a W0 expansion.
