# SK-V13 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V13.
Date: 2026-05-21.
Scope: bind the SK-V13 telemetry schema, required row universe, and gate-json rejection rules before any SK-V13 wave dispatch.
Output: this file.
Pass Alpha goalset: G1-G7 require full CSS L4 parity or architectural blocks, every 17 JSON corpora x 3 planes above strict sonic-rs or architectural blocks, decision-engine fold, at least one union variant admitted or blocked, zero aarch64 orphans, G-Omega before W0, and no silent demotion.
Candidate pool: research/p2/ post-CHALLENGE survivors.

## §1 — Synthesis (concrete; cites P1 row, P2 candidate, REDRESS entry, or goalset line)

P3-D carries the SK-V8 telemetry discipline forward and makes it stricter for
the SK-V13 full-SOTA pin. SK-V8 `SPEC.md` §0.4 made telemetry a gate-consumed
contract: profile artifact, run id, host/build/feature metadata, comparator
plane/strictness/freshness, CostFacts, REDRESS, substrate/cardinality, same-wave
consumer, and Track 2 independence are not optional report prose. `gate-json`
must consume them and reject missing or malformed evidence in the same wave.

SK-V13 expands the required row universe. `skinny/RESULTS.md` currently renders
the legacy schema-v3 JSON table plus the SK-V12 CSS admission and a manifest,
but P1-F records only 41 checked JSON rows and 10 missing typed product
surfaces. The SK-V13 schema must require all 51 JSON rows: 17 corpora x
`parse_only`, `direct_to_struct`, and `real_typed_struct`. A missing typed
surface is a row state, not a missing row and not a close.

The user pin and SK-V13 SYNTHESIS revoke every shortcut that previously let
`parse_only` remain diagnostic-only or direct residual rows close by REDRESS-119
history. JSON admission is now same-plane strict only: Track 1 must beat
`sonic-rs strict Mbps + 1` for the same corpus, same output plane, same host,
same run relation, and strict equality semantics.

CSS is no longer a one-row tranche. The admitted SK-V12 row
`css_l4/declaration_values/direct_to_struct/main` stays admitted, but the
scoping matrix still has 23 non-OUT_OF_SCOPE features to admit or
architecturally block. Every CSS parity row must carry same-plane
lightningcss strict comparator evidence, an independent cssparser oracle or
hand-checked golden table, strict equality artifacts, fixture/corpus
provenance, and generated LOC budget telemetry.

S-P1 and S-P2 also constrain what the schema may accept. S-P1 evidence is
`profile_signal_not_gate_admission`; CSS profiling is timer/fact-sink dominated;
structural SIMD scan is a scanner micro-result only; direct JSON envelopes are
not primitive admissions. S-P2 converged with CSS rows marked
`CSS-ROW-SCOPE-CONDITIONAL`, with SIMD/ASM requiring scalar reference,
strict checkasm/parity, same-wave consumer, row movement, and zero-orphan
evidence.

G-Omega is a hard pre-W0 gate. S-P1/S-P2/S-P3 planning may proceed, but any
implementation wave, generated runtime edit, gate/report edit, `RESULTS.md`
refresh, or `REDRESS.md` append must remain blocked until G-Omega closes and
the SK-V13 SPEC admits W0 dispatch.

## §2 — Deliverable (the shortlist / sequence / gate set / schema / ledger / SPEC section)

### §2.1 Schema Layers

The SK-V13 gate must consume four layers as one schema. Printing a field in a
report without a gate consumer is producer-only telemetry and rejects the wave.

1. **Rendered legacy RESULTS surface.** Preserve the existing schema-v3 rendered
   columns unless a later wave explicitly migrates them:
   `Corpus`, `Workload`, `Outcome`, `Verdict`, `Strictness`, `parse_utf8`,
   `escape_complete`, `flaw_probe`, `Output plane`, `Track 1 Mbps`,
   `Track 2 Mbps`, comparator Mbps columns, deltas, `Hot leaf`, and `Signal`.
   This is the inherited SK-V8 "24-column" shorthand plus the live
   schema-v3/RESULTS header; SK-V13 extends it rather than renaming it.
2. **SK-V8 §0.4 manifest fields.** Carry forward `row_id`, `grammar_id`,
   `domain`, `comparator_id`, `comparator_plane`, `comparator_strictness`,
   `comparator_freshness`, `measured_validation_path`, `Profile artifact`,
   sample cost, `Sample count`, `Build flags`, `Host triple`, `Feature mask`,
   CostFacts fields, `Redress entry`, `Wave id`, `Run id`,
   `Sidecar freshness`, opening-delta field, `substrate_surface`,
   `structural_projection_status`, `substrate_cardinality`,
   `same_wave_consumer_class`, and `track2_independence_status`.
3. **SK-V13 common provenance.** Add `schema_version=sk-v13-telemetry-v1`,
   `source_commit`, `criterion_root_or_report_root`, `artifact_sha256`,
   `producer_id`, `consumer_gate`, `evidence_timestamp_utc`,
   `pass_alpha_goal_id`, `g_alpha_status`, `g_omega_status`, and
   `totality_surface_version`.
4. **Domain extension blocks.** JSON, CSS, SIMD/checkasm, union, decision-engine,
   rolling-delta, and generated-size blocks are required when their row family
   is present or touched by a wave.

### §2.2 Required Row Universe

JSON required rows are generated from this product:

```text
corpora =
  twitter, citm_catalog, canada, apache_builds, github_events,
  update_center, mesh, random, gsoc-2018, marine_ik, instruments,
  numbers, unicode_mixed, unicode_escapes, unicode_basic,
  distinct_values, y_string_unicode

workloads =
  parse_only, direct_to_struct, real_typed_struct
```

Required row id shape:

```text
json/{corpus}/{workload}/main
```

Every one of the 51 JSON rows must exist in the manifest. If a generated typed
product surface is absent, the row must still exist with
`row_state=missing_product_surface`, `Outcome=NO-ADMIT`,
`Track 1 Mbps=n/a`, `sonic_rs_strict_mbps=n/a`, and an actionable
`block_or_reopen_reason`. That state is open unless an
architectural-level intrinsic-block proof is recorded and user-pinned.

CSS required rows are generated from the SK-V13 scoping parity matrix. The gate
must count the non-OUT_OF_SCOPE matrix rows at W0, preserve the SK-V12 admitted
declaration-values row, and require every remaining matrix row to become
`ADMITTED-PARITY` or `ARCHITECTURAL-BLOCK`. A matrix-count drift rejects unless
there is an explicit user re-pin.

### §2.3 Common Required Fields

| Field | Required shape | Gate binding |
|---|---|---|
| `schema_version` | `sk-v13-telemetry-v1` | Reject mixed schema versions inside one report. |
| `row_id` | stable row id | Reject duplicates, unknown rows, and missing JSON/CSS matrix rows. |
| `grammar_id` | `json`, `css_l4`, `sheets`, `bbnf_self`, or `user:<slug>` | Telemetry key only; behavior may not branch on this in generic crates. |
| `domain` | `json_bench`, `css_l4_parity`, `decision_engine`, `simd_primitive`, `union_substrate` | Selects required domain block. |
| `row_state` | `open`, `admitted`, `no_go`, `missing_product_surface`, `architectural_block`, `measured_reject` | Close accepts only `admitted` or `architectural_block`. |
| `Outcome` / `Verdict` | existing enum plus explicit non-admission state | `A/GO` only after strict equality and SOTA margin. |
| `Wave id` | `SK-V13-open`, `W0`, `Wn`, or subwave id | Determines gate rules and stale-wave rejection. |
| `Run id` | stable fingerprint over inputs, Criterion/report roots, comparator artifacts, and host | Reject row when run id moves without refreshed telemetry. |
| `source_commit` | git SHA or checked tree identity | Reject admission from unpinned source provenance. |
| `Host triple` / `host` | target triple plus machine family | Required for same-run comparator and SIMD claims. |
| `Build flags` | profile, target CPU, rustflags, feature flags | Required for reproducibility and feature-mask validation. |
| `Feature mask` | architecture and enabled SIMD/ASM features | Required for checkasm and comparator eligibility. |
| `Profile artifact` | path plus artifact hash or explicit non-profile reason | Reject placeholders and stale inherited hot leaves. |
| `Sample cost` / `Sample count` | c/B or equivalent tuple; positive count | Reject zero or mixed-run samples. |
| `Redress entry` | `none`, `REDRESS-<id>`, or `pending:<wave>` | Failed/rejected row movement must route to REDRESS. |
| `consumer_gate` | named gate command/report reader | Reject producer-only telemetry. |
| `g_alpha_status` | closed bracket id or user pin id | Required for all SK-V13 rows. |
| `g_omega_status` | `closed:<id>` before W0 implementation, otherwise `blocked-pre-w0` | Reject implementation-wave telemetry before G-Omega closes. |

### §2.4 JSON Required Fields

| Field | Required shape | Gate binding |
|---|---|---|
| `corpus` | one of the 17 JSON corpora | Must match fixture path and row id. |
| `workload` | `parse_only`, `direct_to_struct`, `real_typed_struct` | All three required for every corpus. |
| `Strictness` | `strict` for admission; otherwise explicit non-admission | `deferred`, `lossy`, or `permissive` cannot admit. |
| `parse_utf8` | `measured-row`, `scan-boundary`, `view-boundary`, `post-parse`, `n/a` | Admission requires validation in the measured row or accepted scan-boundary equivalent. |
| `escape_complete` | `yes`, `no`, `n/a` | JSON string rows require `yes` for admission. |
| `Output plane` | `DOM`, `borrowed_view_over_offset_tape`, `digest`, `typed direct`, or normalized equivalent | Must equal SOTA comparator plane for admission. |
| `Track 1 Mbps` | positive Mbps unless row is open missing-surface | Must exceed strict SOTA by more than 1 Mbps for admission. |
| `Track 2 Mbps` | positive Mbps or explicit independent oracle state | Must remain independent or be explicitly untouched. |
| `sonic_rs_strict_mbps` | same-plane strict same-run anchor | Required for every JSON row before admission. |
| `sonic_rs_lossy_mbps` | flaw probe only | Never counts as SOTA anchor. |
| `serde_json_mbps` | strict baseline where runnable | Required as JSON baseline telemetry. |
| `comparator_evidence[]` | comparator id, plane, strictness, freshness, Mbps, artifact path, artifact hash | Reject stale, absent, historical, or mixed-plane anchor claims. |
| `strict_equality_artifact` | path/hash/status for output equality | Required for admission. |
| `delta_vs_sota_mbps` | `Track1 - sonic_rs_strict` | Must be `> 1.0` for admission. |
| `delta_vs_prior_tranche_mbps` | signed delta from SK-V12/SK-V13-open | Negative movement fails G7 unless architecturally blocked and user-pinned. |
| `material_differential` | required for REDRESS-119/120 reopen rows | Reject direct residual reopen without fresh differential. |
| `typed_surface_status` | `present`, `missing_product_surface`, `generated_this_wave`, `architectural_block` | Ten missing typed rows remain open until generated/admitted or blocked. |

### §2.5 CSS Required Fields

| Field | Required shape | Gate binding |
|---|---|---|
| `css_feature_id` | stable id from SK-V13 parity matrix | Reject CSS rows not tied to the matrix. |
| `css_feature_status` | `ADMITTED-PARITY`, `OPEN`, `PARTIAL`, `OUT_OF_SCOPE`, `ARCHITECTURAL-BLOCK` | `PARTIAL` cannot close. `OUT_OF_SCOPE` requires user re-pin or matrix authority. |
| `fixture_id` / `corpus_id` | W1b fixture or named real CSS corpus with hash | Reject unpinned fixture. |
| `Output plane` | CSS fact-stream plane, e.g. `css_l4_declaration_value_fact_stream` | Must match Track 1, lightningcss, and oracle planes. |
| `lightningcss_version` | pinned version plus build/source id | Required for every CSS parity admission. |
| `lightningcss_mbps` | same-plane strict Mbps | Admission requires `Track 1 Mbps > lightningcss_mbps + 1`. |
| `lightningcss_artifact` | facts/equality artifact path and hash | Reject report-only Mbps. |
| `cssparser_oracle_status` | version, parser path, artifact, hash | Required unless a golden oracle is declared. |
| `golden_oracle_status` | table id, author/check status, artifact, hash | Required when cssparser does not cover the production. |
| `strict_equality` | `track1=cssparser=lightningcss` or declared golden equivalent | Required for admission. |
| `feature_coverage_match` | accept/reject variant matrix vs lightningcss | Required; missing variant table rejects. |
| `generated_loc` / `generated_module_bytes` | generated source LOC and bytes | Required for generated LOC budget. |
| `grammar_checksum` / `input_checksum` | SHA/fingerprint fields | Required for fixture and grammar provenance. |
| `json_guard_state` | refreshed guard run or no-touch proof | Required when generic code can affect JSON rows. |

### §2.6 SIMD, Checkasm, And Primitive Evidence

| Field | Required shape | Gate binding |
|---|---|---|
| `primitive_id` | stable primitive or candidate id | Required for every SIMD/ASM claim. |
| `primitive_owner_path` | source/test path family | Must be in owner paths authorized by the wave. |
| `scalar_reference_status` | `pass:<path>` plus coverage note | Missing scalar reference rejects. |
| `checkasm_suite` | test path, strict flag, case count, artifact hash | Admission requires strict mode, not advisory-only. |
| `checkasm_case_count` | positive integer; window primitives target 150+ unless wave justifies narrower | Reject underspecified parity matrices. |
| `feature_mask_required` | e.g. `neon`, `pmull`, `cssc`, `dotprod` | Must be satisfied by host feature mask or fallback declared. |
| `same_wave_consumer_path` | production caller path | Missing consumer is an orphan and rejects. |
| `row_movement_evidence` | target row ids and Mbps deltas | Support-only primitives reject. |
| `orphan_status` | `none`, `wired`, `deleted`, `demoted_with_redress` | Close requires zero aarch64 orphans. |

### §2.7 Union Material Differential

| Field | Required shape | Gate binding |
|---|---|---|
| `union_variant_id` | `union-c1`, `union-c2`, `union-c3`, or wave-specific id | Required for union attempts. |
| `prior_redress_ids` | includes relevant `REDRESS-96`, `REDRESS-97`, `REDRESS-98`, and SIMD adjacencies where applicable | Missing historical citations reject. |
| `material_differential` | concrete difference from prior class-column, streaming-cursor, or class-lane failures | Reject renamed old routes. |
| `substrate_cardinality` | `one` | `two`, `unknown`, parser sidecar, or `UnionTape` rejects. |
| `structural_projection_status` | `retained_as_tape`, `transient_only`, or explicit non-admission | Admission requires legal single substrate. |
| `sidecar_status` | `none` or `forbidden_absent` | Sidecar event vectors, aux density tables, retained cursors reject. |
| `same_wave_consumer_path` | generated parser/sink row consumer | `tape_vs_tape`, Track 2, comparator-only, or gate-only is not a consumer. |

### §2.8 Decision-Engine Facts

| Field | Required shape | Gate binding |
|---|---|---|
| `resolver_wave_id` | bbnf-regex, egraph, cost, CSP, cascade-deletion, or combined wave id | Required when decision-engine paths are touched. |
| `bbnf_regex_status` | API/tests/callsite migration status | Required before regex-dependent e-graph/CSP evidence. |
| `egraph_language_status` | language impl, rewrite set, saturation bounds, artifact hash | Reject unbounded or unconsumed e-graph reports. |
| `cost_function_status` | active `egg::CostFunction` or equivalent, cost inputs, stale-cost percent | Reject passive ledger as active optimizer evidence. |
| `csp_status` | solver id, variables/constraints, solve time, UNSAT/SAT artifact | Reject CSP solve time over wave threshold unless REDRESS routes. |
| `cascade_status` | `active`, `fail_closed`, `deleted`, `gated_retired` | After resolver land, silent fallback to P1-P8 rejects. |
| `resolver_output_piping` | regex facts -> e-graph -> cost -> CSP -> codegen | Fused hidden solver violates Lock 4 and rejects. |
| `decision_json_guard_state` | JSON equality/throughput guard after resolver edits | Required for generic decision-engine changes. |

### §2.9 Rolling SOTA Delta

Every Pass Alpha bracket and SK-V13 close candidate must consume:

```text
restart/skinny/ROLLING-SOTA-DELTA.md
```

Required columns are exactly:

```text
row | plane | T1_current | T1_sota | margin | tranche_admitted
```

The gate must cover all 51 JSON rows plus every non-OUT_OF_SCOPE CSS parity
feature. Negative `margin` remains open unless `tranche_admitted` names an
architectural-block proof. A row that moves backward from the prior tranche
fails G7 unless a user re-pin explicitly accepts the architectural block.

### §2.10 Generated LOC Budget

Generated or resolver-heavy rows must emit:

| Field | Required shape |
|---|---|
| `loc_budget_id` | wave/SPEC budget id |
| `generated_loc` | generated runtime/module LOC |
| `runtime_loc_delta` | runtime implementation LOC |
| `codegen_loc_delta` | codegen/template LOC |
| `gate_test_loc_delta` | report/gate/test LOC |
| `generated_module_bytes` | bytes of generated module |
| `budget_status` | `pass`, `over_budget_revise`, `over_budget_reject` |

CSS rows use the scoping envelopes as planning ceilings. Decision-engine waves
must additionally record e-graph/CSP compile-time overhead. A generated-size
field that is emitted but not checked by the gate is producer-only telemetry.

### §2.11 Gate-JSON Rejection Rules

`gate-json` must reject before computing admission when any rule below trips:

1. **Missing row universe:** fewer than 51 JSON rows, missing CSS matrix rows,
   missing rolling delta rows, duplicate `row_id`, or unknown `row_id`.
2. **Missing required field:** any required common/domain field is absent after
   the wave that introduces that row family.
3. **Stale run id:** row run id does not match the artifact fingerprint, prior
   run id is reused after inputs change, or comparator/report roots come from a
   different run without explicit non-admission sidecar status.
4. **Mixed output plane:** Track 1, Track 2, comparator, lightningcss,
   cssparser, golden oracle, or rolling-delta planes differ for an admission
   claim.
5. **Permissive anchor:** lossy sonic-rs, permissive RapidJSON, historical
   sidecars, stale C++ rows, or different-plane DOM rows are used as SOTA.
6. **Deferred validation admission:** `Strictness=deferred`,
   `parse_utf8=view-boundary`, validation outside measured row, or missing
   strict equality artifact is used for `A/GO`.
7. **CSS oracle gap:** CSS row lacks lightningcss strict evidence, lacks
   cssparser/golden independent oracle, lacks feature-coverage matrix, or uses
   report-only Mbps.
8. **Producer-only telemetry:** a report emits a field that no same-wave gate
   parses and rejects, including companion CSS reports, checkasm JSON,
   CostFacts, rolling delta, generated LOC, or union material differential.
9. **SIMD orphan:** primitive lacks scalar reference, strict checkasm/parity,
   same-wave production consumer, row movement, or zero-orphan disposition.
10. **Union stale route:** union attempt lacks prior REDRESS citations, lacks
    material differential, retains a sidecar, introduces `UnionTape`, or has
    substrate cardinality other than one.
11. **Decision-engine paper close:** resolver wave emits passive facts only,
    leaves P1-P8 cascade as silent fallback, omits solve/cost evidence, or
    violates output-piping.
12. **Generated LOC opacity:** generated LOC/module bytes or budget status are
    missing on generated rows.
13. **G-Omega/G-Alpha gap:** implementation-wave telemetry appears before
    `g_omega_status=closed:<id>` or without the SK-V13 Pass Alpha goal id.
14. **Rolling demotion:** rolling delta margin regresses or an admitted row is
    silently demoted without architectural-block proof and user re-pin.

## §3 — Falsifiability binding (named corpus rows + Mbps thresholds)

JSON threshold: for every `json/{corpus}/{workload}/main` row,
`Track 1 Mbps > sonic_rs_strict_mbps + 1.0` on the same output plane and strict
same-run comparator relation. `parse_only` rows use strict sonic-rs
`parse_only`; direct rows use strict digest/direct plane; typed rows use strict
typed-direct plane. Missing typed surfaces are open rows, not skipped rows.

CSS threshold: for every non-OUT_OF_SCOPE CSS matrix row,
`Track 1 Mbps > lightningcss_mbps + 1.0` on the same CSS fact-stream plane,
with strict equality against lightningcss and cssparser or a hand-checked golden
oracle. The already admitted declaration-values row must not demote.

Rolling delta threshold: every JSON row/plane and CSS feature has a
`margin = T1_current - T1_sota`. `margin > 1.0` is required for admission unless
the row carries architectural-block proof. Any negative movement from the prior
tranche fails G7.

SIMD/checkasm threshold: primitive microbench speedup alone is never enough.
The primitive's same-wave consumer must move a named JSON/CSS row or preserve a
named admitted row while the wave records a measured reject/block. Checkasm must
run strict and consume scalar-reference parity evidence.

Union threshold: a union attempt must move a named retained-parse/direct/CSS row
or produce architectural-block evidence. It must prove one substrate,
same-wave consumer, no sidecar, material differential from REDRESS 96/97/98,
and strict row evidence.

Decision-engine threshold: resolver waves must emit active facts that are
consumed by codegen/gate in the same wave: bbnf-regex facts, e-graph candidates,
CostFunction extraction, CSP assignment, and P1-P8 cascade fail-closed/deletion.
Support-only extraction is not an admission.

## §4 — Pre-blocked routes (REDRESS entries each wave must NOT re-open)

The schema must prevent these routes from re-entering as telemetry relabels:

- REDRESS 28+33 tiny-string / NEON string routes without grammar policy and row
  movement.
- REDRESS 50-55 parser side tables, parser-owned cursors, aux columns,
  decoded-string sink hooks, and source-method shortcuts.
- REDRESS 60-72 direct materialization/source-hook/hash attempts without fresh
  material differential and strict row evidence.
- REDRESS 80 one-row `canada` mantissa widening.
- REDRESS 82-84 single-quartet unicode proof-only and object-pair dispatch
  compaction replays.
- REDRESS 88/89 PMULL/CSSC/CTZ/bulk routes as default hot-body or support-only
  implementations.
- REDRESS 96/97/98 class-column, streaming-cursor, class-lane, retained
  structural sidecar, or parser-owned union variants.
- REDRESS 119/120 as close authority for direct residuals; they are history
  and require fresh SK-V13 material differential.
- REDRESS 126 as permission to keep or add orphans; it is demotion evidence,
  not production admission.
- Any use of lossy/permissive comparators, stale sidecars, mixed planes,
  Track 2, `tape_vs_tape`, or gate-only telemetry as a production consumer.
- Any W0 dispatch before G-Omega closes.

## §5 — Sources (every upstream artefact cited)

- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`
- `restart/prompts/ORCHESTRATOR.md` §3, §3W, §3Z
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v13/HANDOFF.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/profile-provenance-v3.md`
- `restart/skinny/tranches/sk-v13/research/p1/support/mode3-harness-provenance.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V4-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v13/research/p1/hardening/HARDENING-S-P1-V5-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2a-sota-teardown.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2b-dav1d-process.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2c-arch-esoterica.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2d-substrate-tape.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2e-parse-that-gaps.md`
- `restart/skinny/tranches/sk-v13/research/p2/p2f-grammar-neutral.md`
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V3-CONSOLIDATED.md`
- `restart/skinny/tranches/sk-v13/research/p2/hardening/HARDENING-S-P2-V4-CONVERGED.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-css-parity-gap.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-decision-engine.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`
- `restart/skinny/tranches/sk-v8/SPEC.md` §0.4
- `restart/skinny/tranches/sk-v8/research/p3/p3d-telemetry-schema.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
