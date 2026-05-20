# SK-V12 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V3.
Date: 2026-05-20.
Scope: bind the SK-V12 telemetry schema, non-JSON companion report, and fail-closed gate rules before any SK-V12 wave dispatch.
Output: this file.
Pass Alpha goalset: SK-V12 must admit one generated non-JSON direct or typed parser baseline, then one measured grammar-generalized intervention on that same row at least `ceil(baseline_mbps * 1.01)`, while preserving the 4 direct and 7 typed JSON guard rows; `parse_only`, W3 substrate, and JSON direct residual movement remain diagnostic or pre-blocked (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:50`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:57`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:70`).
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 - Synthesis

P3-D is a telemetry and gate-binding artifact, not implementation authority.
S-P3 is read-only against `skinny/` source, and later wave redress owns any
report, gate, bench, codegen, runtime, or `RESULTS.md` source changes
(`restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` §1-§2). The schema choice
therefore must be conservative: SK-V12 inherits the schema-v3 discipline and
10-outcome enum from SK-V11, carries the SK-V8 required-telemetry precedent,
and adds only semantic fields that a same-wave gate consumes.

The live JSON result surface is already split between a rendered 26-column
main table and a gate-consumed telemetry manifest. The main table header names
`Corpus`, `Workload`, `Outcome`, `Verdict`, strictness/validation fields,
Track 1/Track 2 Mbps, strict/permissive comparator Mbps, delta columns, `Hot
leaf`, and `Signal` (`skinny/RESULTS.md:3`). The manifest starts at
`skinny/RESULTS.md:47` and carries row id, grammar, domain, wave, run id,
validation, profile artifact, sample cost/count, build/host/feature metadata,
CostFacts, REDRESS, baseline delta, substrate, structural projection,
cardinality, consumer, Track 2 status, diagnostic nonproducer status, and
structured comparator evidence (`skinny/RESULTS.md:49`). The current seed
surface remains overall `N-direct / NoGo`, Track 1 is generated JSON parse, and
Track 2 is the independent hand-coded parser that never calls generated Track 1
(`skinny/RESULTS.md:143`, `skinny/RESULTS.md:144`,
`skinny/RESULTS.md:145`).

SK-V12's first material target is not another JSON row. The opening synthesis
requires a generated non-JSON baseline with generated Track 1, independent
Track 2 or oracle, strict output equality, W1 thresholded same-run throughput,
input provenance, run/build/host/sample telemetry, and gate/report consumption
before any JSON-only micro-wave (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:38`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:41`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:43`). It then requires a
same-row intervention delta consumed by the gate
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:50`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:52`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:54`). The generated non-JSON
telemetry therefore enters a companion report by default unless a wave updates
`skinny/RESULTS.md` and every existing RESULTS consumer in the same slice
(`restart/skinny/tranches/sk-v12/HANDOFF.md:89`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:91`). This keeps JSON `gate-json`
stable while making the non-JSON evidence executable.

REDRESS 111 is the accepted precedent for that companion lane: it admitted only
a non-JSON evidence report, consumed by `bbnf-bench --bin gate` via a
non-JSON-report argument, without relaxing JSON schema-v3, updating
`skinny/RESULTS.md`, creating a generated baseline, or moving a parser row
(`skinny/REDRESS.md:3284`, `skinny/REDRESS.md:3285`,
`skinny/REDRESS.md:3286`). REDRESS 112 and 113 then rejected the generated
baseline/intervention route because generated CSS L4 Track 1 did not exist and
the threshold was not measurable (`skinny/REDRESS.md:3315`,
`skinny/REDRESS.md:3323`, `skinny/REDRESS.md:3349`). SK-V12's schema must use
that lesson directly: companion evidence is admissible only when it is
generated, benchmarked, same-plane, independently checked, and consumed by the
same wave's gate.

## §2 - Deliverable

### §2.1 Inherited schema and outcome enum

The SK-V12 schema inherits the SK-V11 schema-v3 identifier set:

```text
row_id grammar_id domain corpus workload outcome_id verdict strictness
parse_utf8 escape_complete flaw_probe output_plane track1_mbps track2_mbps
comparator_id comparator_plane comparator_strictness comparator_freshness
sidecar_freshness comparator_value_mbps comparator_source_artifact
measured_validation_path profile_artifact sample_cost sample_count build_flags
host_triple feature_mask costfacts_rule_id costfacts_chosen_shape
costfacts_rejected_alternative_ids redress_entry wave_id run_id
sk_v9_open_delta substrate_surface structural_projection_status
substrate_cardinality same_wave_consumer_class track2_independence_status
diagnostic_nonproducer_status
```

Source authority: `restart/skinny/tranches/sk-v11/SPEC.md:94` through
`restart/skinny/tranches/sk-v11/SPEC.md:107`. The SK-V8 precedent remains
binding for rendering flexibility: required fields may appear as columns,
gate-consumed manifest entries, or a gate-consumed JSON payload, but every
emitted field must be consumed by the gate in the same wave
(`restart/skinny/tranches/sk-v8/SPEC.md:103`,
`restart/skinny/tranches/sk-v8/SPEC.md:105`,
`restart/skinny/tranches/sk-v8/SPEC.md:142`).

The inherited 10-identifier outcome enum is unchanged:

```text
A C G I J K L M N-direct S
```

No SK-V12 wave may add an outcome variant. `S`, `L`, and `N-direct` are
non-admission outcomes; `S` is diagnostic/substrate-guard and cannot close a
SOTA claim (`restart/skinny/tranches/sk-v11/SPEC.md:83`,
`restart/skinny/tranches/sk-v11/SPEC.md:89`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`).

### §2.2 SK-V12 non-JSON companion report fields

Generated non-JSON rows may enter `skinny/RESULTS.md` only if the same wave
updates every RESULTS consumer, including `gate-json`, report rendering,
negative fixtures, close-doc checks, and any manifest parser. Otherwise the
wave must write a companion report under the wave artifact directory and invoke
a same-wave companion gate. The report schema id is:

```text
sk-v12-nonjson-generated-v1
```

Required report fields:

| Field | Required shape | Gate binding |
|---|---|---|
| `schema_id` | `sk-v12-nonjson-generated-v1` | Reject unknown schema ids and SK-V11 placeholder schemas for generated-baseline admission. |
| `row_id` | `{grammar_id}/{corpus_or_workload}/{workload}/main` | Stable join key for baseline and intervention rows; reject duplicates. |
| `grammar_id` | `css_l4`, `sheets`, or `bbnf_self` unless SPEC names another generated grammar | Reject `json` for the generated non-JSON close axis. |
| `domain` | `non_json_generated` plus a grammar domain label | Reject generic `json_bench` for non-JSON rows. |
| `corpus_or_workload` | Fixture/workload id with source path | Must resolve to a fixture and provenance record. |
| `workload` | `direct_to_struct`, `real_typed_struct`, or a SPEC-named generated typed/direct workload | Reject `parse_only` as a SOTA admission plane. |
| `workload_class` | `baseline` or `intervention` | Intervention rows must name a valid baseline row. |
| `output_plane` | `typed_direct`, `direct_sink`, or SPEC-named product plane | Comparator/oracle plane must match. |
| `outcome_id` / `verdict` | inherited enum plus `GO`/`NO-GO` | Reject outcomes outside the inherited enum. |
| `generated_track1_source_path` | Generated runtime/parser source path | Must be generated or per-grammar runtime code, not stale hand witness code. |
| `generated_runtime_path` | Runtime module path loaded by the benchmark | Must build and match the selected grammar id. |
| `generated_input_provenance` | Fixture/source path plus generator command or checksum | Reject unknown or JSON-derived fixture provenance. |
| `track1_mbps` | measured Mbps; W1 baseline gate requires >= 1 Mbps | Required for baseline and intervention rows. |
| `track1_artifact` | Criterion or equivalent benchmark artifact path | Must match the row/run id. |
| `track2_or_oracle_source_path` | Independent Track 2 or oracle source | Must not call generated Track 1, generated SinkOnly helpers, generated runtime internals, or `runtime::generated_json::parse`. |
| `track2_independence_status` | `independent_verified` or fail-closed reason | Reject coupled, shared-source, or self-attested-only independence. |
| `track2_or_oracle_mbps` | measured Mbps; W1/W2 admission requires >= 1 Mbps | Required for admitting baseline and intervention rows; `n/a` is allowed only for non-admitting support reports. |
| `strict_output_equality` | `pass` or structured failure | Baseline/intervention cannot admit without `pass`. |
| `oracle_status` | same-plane, strict, independent, freshness marker | Reject comparator/oracle plane mismatch and stale or permissive anchors. |
| `baseline_row_id` | `none` for baseline; row id for intervention | Intervention rows must reference an admitted baseline from the same SK-V12 bracket. |
| `baseline_mbps` | measured W1 baseline Mbps for intervention | Required to compute the delta floor. |
| `threshold_mbps` | `ceil(baseline_mbps * 1.01)` or stricter SPEC floor | Reject unmeasurable thresholds. |
| `run_id` | stable SK-V12 run id | All Track 1, Track 2/oracle, guard, and comparator values consumed together must be same-run or explicitly fail. |
| `host_triple` / `feature_mask` / `build_flags` | host/build/ISA provenance | Required for aarch64/SIMD and strict comparator claims. |
| `sample_count` / `sample_cost` | positive sample count and c/B, ns/B, or equivalent tuple; W1 baseline gate requires sample count >= 30 | Reject zero/missing sample evidence. |
| `benchmark_artifact_path` | concrete artifact path | Must resolve under the wave capture root or named external artifact root. |
| `json_guard_state` | `not_refreshed` with no-touch proof, or a table of refreshed guard rows/floors/results | If JSON reports are refreshed, all 4 direct and 7 typed guards must carry measured maintain/lift/demotion status; otherwise the wave must prove no JSON-producing path was touched and `skinny/RESULTS.md` stayed unchanged. |
| `wave_id` / `redress_entry` | wave id and `none`, `pending`, or `REDRESS-<id>` | Failed waves must record REDRESS evidence. |
| `same_wave_consumer_class` | `companion_gate_generated_baseline`, `companion_gate_generated_intervention`, or SPEC-named consumer | Reject producer-only reports. |
| `gate_status` | `pass`, `fail`, or structured blocked status | Must be written by the gate, not by the benchmark alone. |

The same-wave consumer for companion reports is the companion gate invoked by
the wave redress packet, not the markdown artifact itself. A legal SK-V12 wave
therefore consumes the report through a command equivalent to:

```text
bbnf-bench --bin gate -- --skv12-non-json-report <report.json>
```

P3-F may choose the exact command spelling, but it must be a checked executable
gate and must run in the same wave that emits the companion report. If a wave
instead chooses to render non-JSON rows into `skinny/RESULTS.md`, the same-wave
consumer is `gate-json --check-results` plus any updated report/manifest
validator; partial RESULTS rendering without every consumer updated is
producer-only telemetry and rejects.

### §2.3 `gate-json` and companion-gate rejection rules

`gate-json` remains the JSON `RESULTS.md` and manifest validator. It must fail
closed on:

- missing required schema-v3 identifiers or folded cells that cannot be
  reconstructed by the validator;
- duplicate or unknown row ids;
- unsupported outcome identifiers or attempts to add a new enum variant;
- stale, non-uniform, missing, or invalid run ids;
- stale strict anchors, strict plane mismatch, wrong comparator id, or
  permissive/lossy comparators used for strict admission;
- deferred validation, `view-boundary` validation, or `parse_utf8` evidence
  used as measured-row strict admission;
- `parse_only` SOTA claims;
- direct digest evidence used as typed proof;
- Track 2 coupling or independence self-attestation without source/provenance
  separation;
- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate
  reopen claims;
- generic-crate JSON policy leakage;
- producer-only telemetry: any emitted field not consumed by `gate-json` in the
  same wave.

Source authority: SK-V11 required-telemetry rejection rules
(`restart/skinny/tranches/sk-v11/SPEC.md:114`,
`restart/skinny/tranches/sk-v11/SPEC.md:117`) plus SK-V12 telemetry binding
(`restart/skinny/tranches/sk-v12/SYNTHESIS.md:222`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:224`).

The SK-V12 companion gate must fail closed on all `gate-json` rules that apply
to its semantic fields, plus these non-JSON-specific rules:

- missing `schema_id`, `grammar_id`, generated Track 1 source path, generated
  runtime path, generated input provenance, independent Track 2/oracle path,
  strict output equality, finite Track 1 Mbps, run id, host/build/sample
  metadata, output plane, wave id, redress entry, same-wave consumer class, or
  gate status;
- stale placeholder evidence used as generated-baseline proof;
- `schema_id=sk-v11-w1a-nonjson-v1` used as SK-V12 admission evidence;
- hand-only parser or stale witness module claimed as generated Track 1;
- W1/W2 admitting row with `track2_or_oracle_mbps=n/a` or below 1 Mbps;
- generated Track 1 and Track 2/oracle sharing source, helper calls, generated
  runtime internals, generated SinkOnly helpers, or benchmark digest shortcuts;
- baseline row missing when an intervention row is claimed;
- intervention threshold missing, stale, or below `ceil(baseline_mbps * 1.01)`
  unless SPEC sets a stricter floor;
- baseline/intervention row ids from different grammar ids, output planes,
  run ids, or host/build captures;
- `json` grammar id used for the generated non-JSON close axis;
- JSON guard refresh without all guard rows carrying maintain/lift/demotion
  disposition, or `json_guard_state=not_refreshed` without no-touch proof;
- JSON direct residual movement before the generated non-JSON priority is
  satisfied or explicitly blocked by measurement;
- parse-only, W3, sidecar substrate, or direct residual route claims embedded
  in a non-JSON report;
- JSON policy in generic crates or runtime outside generated per-grammar
  modules.

## §3 - Falsifiability binding

SK-V12 telemetry closes only when a measurable gate consumes the fields it
requires. For the generated baseline row, the companion gate or updated
`gate-json` consumer must prove:

1. one selected generated non-JSON baseline row exists for `css_l4`, `sheets`,
   or `bbnf_self`;
2. generated Track 1 builds and benchmarks on the selected direct or typed
   plane;
3. independent Track 2 or oracle is same-plane, strict, and source-independent;
4. strict output equality passes;
5. Track 1 Mbps and Track 2/oracle Mbps are finite and tied to one run id,
   host, feature mask, build flags, sample count, and benchmark artifact;
6. the report is consumed by the same-wave gate;
7. JSON guards remain unchanged or carry measured maintain/lift/demotion state
   if refreshed.

For the intervention row, the gate must additionally prove:

1. `baseline_row_id` points to the admitted baseline row in the same SK-V12
   bracket;
2. `threshold_mbps >= ceil(baseline_mbps * 1.01)` unless P3-C/P3-F set a
   stricter floor;
3. Track 1 on the intervention row meets or exceeds the threshold on the same
   output plane with strict equality still passing;
4. Track 2/oracle independence remains valid and same-plane;
5. the selected primitive or runtime change names a same-wave consumer, not an
   orphan report field.

For JSON guard rows, the schema binds the existing surface rather than moving
it. The seed surface is 4 `direct_to_struct A / GO`, 13
`direct_to_struct N-direct / NO-GO`, 7 `real_typed_struct A / GO`, and
diagnostic parse-only rows (`restart/skinny/tranches/sk-v12/HANDOFF.md:41`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:43`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:44`,
`restart/skinny/tranches/sk-v12/HANDOFF.md:45`). Any refreshed JSON report must
carry the direct and typed guard floors from S-P3 and cannot silently demote an
admitted row (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:57`,
`restart/skinny/tranches/sk-v12/SYNTHESIS.md:149`).

An unmeasurable threshold, missing baseline Mbps, missing run id, or companion
report not consumed by a gate is a REVISE before redress, or a REDRESS reject if
discovered during redress.

## §4 - Pre-blocked routes

The schema must keep these routes closed rather than give them a new telemetry
name:

- W3 union/event/class-column/streaming-cursor/class-lane/sidecar substrate,
  including `UnionTape`, retained structural vectors, parser-owned projections,
  and W4-through-W3 cascade-lock (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:70`).
- `parse_only` SOTA admission or close credit; parse-only rows are diagnostic
  only (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:62`).
- JSON direct residual row movement without fresh material evidence beyond
  REDRESS 114-119 and without satisfying the non-JSON priority first
  (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:65`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:181`).
- W0-clamped or docs-only direct admission for rows that were numerically above
  a floor but lack behavior provenance (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:114`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:115`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:116`).
- Direct digest evidence as typed proof or as a grammar-generalization proof.
- Producer-only fields, stale report lanes, stale placeholder rows, or
  companion reports without an executable same-wave gate.
- JSON policy in generic crates or runtime outside generated per-grammar
  modules (`restart/skinny/tranches/sk-v12/SYNTHESIS.md:73`,
  `restart/skinny/tranches/sk-v12/SYNTHESIS.md:222`).
- New directive, BIR variant, `BackendShape`, public substrate API,
  parser-owned sidecar/fact slot, second retained substrate, or x86 target
  (`restart/skinny/tranches/sk-v12/HANDOFF.md:131`).

## §5 - Sources

- `restart/prompts/ORCHESTRATOR.md` for S-P3 dispatch, CHALLENGE, convergence,
  no-substrate, Lock 14, scalar/checkasm, same-wave consumer, and no-deferral
  rules.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md` for P3-D scope,
  frontmatter, required SK-V8 schema carry-forward, `gate-json` rejection
  binding, and S-P3 read-only source boundary.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md` for the SK-V12 close condition,
  generated non-JSON baseline/intervention priority, JSON guard surface,
  parse-only/W3/direct residual pre-blocks, telemetry binding, outcome enum,
  and companion/report fail-closed rules.
- `restart/skinny/tranches/sk-v12/HANDOFF.md` for the active next move,
  goalset, telemetry binding, companion report versus `RESULTS.md` placement,
  and refusal conditions.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
  for the accepted SK-V12-open profile authority and current result surface.
- `restart/skinny/tranches/sk-v12/research/p2/hardening/HARDENING-S-P2-CONVERGED.md`
  for the accepted S-P2 candidate pool and S-P3 load-bearing facts.
- `restart/skinny/tranches/sk-v12/research/p2/p2a-sota-teardown.md` through
  `p2f-grammar-neutral.md` for candidate scalar-reference, checkasm/parity,
  same-wave consumer, grammar-neutral, substrate, and pre-block boundaries.
- `restart/skinny/tranches/sk-v8/SPEC.md` §0.4 for required telemetry
  rendering flexibility and same-wave gate consumption.
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md` for the
  earlier telemetry-schema binding shape and failure-state vocabulary.
- `restart/skinny/tranches/sk-v11/SPEC.md` §0.2-§0.3 for the 10-outcome enum,
  schema-v3 required identifier set, and non-JSON companion-report placement
  rule.
- `skinny/RESULTS.md` for the live JSON main table, SK-V9 W0 telemetry
  manifest, current overall `N-direct / NoGo` seed, and Track 2 independence
  note.
- `skinny/REDRESS.md` through REDRESS 120, especially REDRESS 111-113 for the
  non-JSON companion lane and generated-baseline rejection, and REDRESS 119-120
  for direct residual fixpoint and SK-V12 routing.
