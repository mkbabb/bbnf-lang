# SK-V8 P3-D: Telemetry Schema Binding

Pass: S-P3 Synthesis-Plan. Cycle: V3 citation fold.
Date: 2026-05-18.
Scope: bind the SK-V8 telemetry schema, gate rejection rules, and W3 substrate-adjudication fields before any SK-V8 wave dispatch.
Output: this file.
Pass Alpha goalset: SK-V8 must create `SK-V8-open`, populate required telemetry on all 38 current main rows, make `gate-json` reject missing telemetry, bind CostFacts before behavior admission, preserve typed GO rows, keep direct digest rows as guard rows, and pass Lock 14/Lock 15 at close (`SPEC Sections 0.1-0.5, 2, and 3-11`, `HANDOFF Sections 2, 3a, and 4-10`).
Candidate pool: S-P2 substrate-ceiling survivors after V7 consolidation; V7 accepted 6/6 and authorizes S-P3, not any implementation wave (`S-P2 V7 consolidated verdict and preserved boundaries`).
Traceability note: inline citations use stable section labels. Concrete local paths are retained in Sources; when a table row names a RESULTS row or REDRESS id, that row/id is the resolving anchor.


## §1 - Synthesis

P3-D is a gate-binding plan, not a source plan. S-P3 is read-only against `skinny/` source, and implementation belongs only to later wave redress phases (`PASS-3 Synthesis-Plan role and gate sections`). This artifact therefore names schema requirements, allowed values, row bindings, fixtures, and failure states only.

The current authority is schema-v3 `RESULTS schema-v3 current main rows and Track 2 authority`: 38 main rows, current overall `N-direct / NoGo`, same-run sonic strict/lossy provenance, sidecar C++ values only where populated, and missing hot-leaf attribution (`RESULTS schema-v3 current main rows and Track 2 authority`, `Alpha-A results extraction`). The live rendered header has 26 columns, even though the P3 prompt shorthand says "24-column schema"; this plan preserves the live 26-column header and treats the prompt wording as shorthand for the schema-v3 surface (`RESULTS schema-v3 current main rows and Track 2 authority`, `PASS-3 Synthesis-Plan role and gate sections`).

The schema has two jobs before behavior waves can dispatch. First, W0 must make every current row observably tied to a run, profile artifact, host/build metadata, comparator plane, strictness, sidecar freshness, and `SK-V8-open` delta (`SPEC Sections 0.1-0.5, 2, and 3-11`). Second, W1 must make CostFacts chosen/rejected evidence consumed by `gate-json --with-cost-facts`, without changing parser behavior or generated JSON output unless a later plan explicitly owns that behavior (`SPEC Sections 0.1-0.5, 2, and 3-11`).

The W3 tape plus structural-projection union needs extra observability but no new directive, BIR variant, or substrate. S-P2 preserves the union as a lead hypothesis only: Tier A is structural-class cursor migration inside the single retained `Tape`; Tier B owns string-boundary, quote/backslash/parity, CostFacts-template, and non-JSON production migration (`SYNTHESIS opening state and S-P2/W3 finding sections`). W3 remains blocked until W0/W1 close and a fresh plan names owner paths, same-wave production consumer, revert protocol, thresholds, measured-path strict validation, and challenge acceptance (`HANDOFF Sections 2, 3a, and 4-10`, `SPEC Sections 0.1-0.5, 2, and 3-11`).

## §2 - Deliverable

### §2.1 Rendered schema-v3 surface

Keep the current rendered `RESULTS schema-v3 current main rows and Track 2 authority` columns unchanged for `SK-V8-open`:

```text
Corpus
Workload
Outcome
Verdict
Strictness
parse_utf8
escape_complete
flaw_probe
Output plane
Track 1 Mbps
Track 2 Mbps
sonic-rs strict Mbps
sonic-rs lossy Mbps
simdjson DOM Mbps
simdjson On Demand Mbps
yyjson default Mbps
asmjson SWAR Mbps
asmjson AVX-512 Mbps
RapidJSON default Mbps
serde_json Mbps
Delta vs SK-V6
Delta vs sonic-strict
Delta vs simdjson DOM
Delta vs yyjson
Hot leaf
Signal
```

W0 may add the SK-V8 fields below as rendered columns, an adjacent checked manifest, or a gate-consumed JSON section. The non-negotiable is consumption: every emitted field must be read and rejected by `gate-json` in the same wave, not merely printed (`SPEC Sections 0.1-0.5, 2, and 3-11`, `PASS-3 Synthesis-Plan role and gate sections`).

### §2.2 Required fields and allowed values

| Field | Required by | Allowed values / shape | Gate binding |
|---|---|---|---|
| `row_id` | W0 addition | `{grammar_id}/{corpus}/{workload}/{track_set}`; stable string | Reject duplicates or missing row ids; row id is the join key for profile, sidecar, CostFacts, and `SK-V8-open` delta. |
| `grammar_id` | SPEC 0.4 | `json`; future `css_l4`, `sheets`, `bbnf_self`, `user:<slug>` | Generic telemetry model key; no generic crate may branch on this value for behavior. |
| `domain` | SPEC 0.4 | `json_bench`; future grammar-domain ids | Required for comparator grouping and non-JSON absence-of-anchor reasons. |
| `Corpus` | existing | Current 17 JSON fixtures: `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, `y_string_unicode` | Must match known fixture manifest and profile artifact path. |
| `Workload` | existing | Current main: `parse_only`, `direct_to_struct`, `real_typed_struct`; routed residual only: `tape_vs_tape`; non-main probe: `simd_structural_scan` | `tape_vs_tape` is not default W0/W1 scope and cannot satisfy W3 production-consumer proof (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| `Outcome` | existing / SPEC 0.3 | `A`, `C`, `G`, `K`, `L`, `N-direct`; optional post-W0 amendment `S` | Reject any other outcome after W0; `K` and `S` never support strict SOTA admission (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| `Verdict` | existing | `GO`, `NO-GO` | Must be computed by gate, not hand-written. `S`/`K` substrate-guard rows remain `NO-GO` for SOTA admission. |
| `Strictness` | existing | `strict`, `deferred`, `permissive`, `lossy`, `n/a` | Strict admission requires `strict`; current rows are `deferred` (`RESULTS schema-v3 current main rows and Track 2 authority`). |
| `parse_utf8` | existing | `scan-boundary`, `view-boundary`, `post-parse`, `none`, `n/a` | Strict admission requires measured-row validation, not `view-boundary` or post-parse. |
| `escape_complete` | existing | `yes`, `no`, `n/a` | Strict admission requires `yes` and measured-row validation. |
| `flaw_probe` | existing | Free text, but must include `none` or a named flaw-probe reason | Lossy/permissive rows may appear only as flaw probes. |
| `row_output_plane` / `Output plane` | normalized W0 addition over existing prose | `borrowed_view_offset_tape`, `dom_value_tree`, `digest`, `typed_direct`, `offset_index`, `structural_index`, `sink_only_transient`, `n/a` | Required comparator-plane equality key. Current parse rows are borrowed view over offset tape vs DOM; direct rows are digest; real typed rows are typed direct (`RESULTS schema-v3 current main rows and Track 2 authority`). |
| `Track 1 Mbps`, `Track 2 Mbps` | existing | Positive numeric Mbps or `n/a` only when workload has no track | W0 telemetry-only rows must remain within +/-1.0 percent versus `SK-V8-open` (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| Comparator Mbps fields | existing | Positive numeric Mbps or `n/a` | Populated values require comparator metadata row and sidecar/native freshness. |
| `comparator_id` | SPEC 0.4 | `sonic_rs_strict`, `sonic_rs_lossy`, `serde_json`, `simdjson_dom`, `simdjson_on_demand`, `yyjson_default`, `yyjson_minify`, `rapidjson_default`, `asmjson_swar`, `asmjson_avx512`, `simdjson_stage1_structural`, `sonic_lazy_skeleton`, `none` | Required for every comparator cell used in a delta or admission claim. |
| `comparator_plane` | SPEC 0.4 | `dom_value_tree`, `digest`, `typed_direct`, `offset_index`, `structural_index`, `lazy_iterator`, `sax_dpda`, `none` | Strict admission rejects when this does not equal `row_output_plane` (`SPEC Sections 0.1-0.5, 2, and 3-11`, `SC-5 parse_only demotion and tape_vs_tape limits`). |
| `comparator_strictness` | SPEC 0.4 | `strict`, `permissive`, `lossy`, `unknown`, `n/a` | Strict admission requires `strict`; sonic lossy and permissive rows remain flaw probes (`Alpha-B comparator matrix`). |
| `Sidecar freshness` | SPEC 0.4 | `same-run`, `same-run-native`, `sidecar-same-run`, `stale:<reason>`, `absent:<reason>`, `historical:<id>`, `n/a` | Strict admission allows same-run native strict anchors or same-run sidecars only; missing sidecars require explicit absence reason (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| `Profile artifact` | SPEC 0.4 | Existing path under a W0 profile directory, plus symbol/profile format | Reject placeholder `unprofiled`; W0 must name profile command and artifacts (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| `Hot leaf` | existing | Non-placeholder symbol path plus percentage/sample count, or `not-hot:<reason>` for probe rows | Reject `unprofiled in W0b; no kernel prescription from this row` after W0. |
| `Cycles per byte` | SPEC 0.4 | Positive numeric `c/B` or explicit equivalent sample-cost tuple | Required on every current main row after W0. |
| `Sample count` | SPEC 0.4 | Positive integer and sampler type | Reject zero, missing, or mixed-run counts without run id split. |
| `Build flags` | SPEC 0.4 | Release profile, target-cpu, LTO/codegen-units, relevant env | Required for same-run comparability. |
| `Host triple` | SPEC 0.4 | Rust target triple plus machine family | Required for SIMD/ASM and sidecar eligibility. |
| `Feature mask` | SPEC 0.4 | Host feature set, for example `aarch64:neon,pmull,cssc?`; x86 equivalents as present | Required for primitive and comparator provenance. |
| `Wave id` | SPEC 0.4 | `SK-V8-open`, `W0`, `W1`, `W2`, `W3`, `W4`, `W5`, `W6`, or rejected subwave id | Determines which gate rules apply. |
| `Run id` | SPEC 0.4 | Stable unique id for each bench/profile/gate run | Comparator and bbnf rows used together must share a same-run relation or disclose sidecar status. |
| `SK-V8-open delta` | SPEC 0.4 | `baseline` on opening row; then signed percent versus the row's `SK-V8-open`; `new-row:<reason>` for newly added rows | Reject plain `n/a` after W0 for rows with an opening predecessor. |
| `CostFacts rule id` | SPEC 0.4 / W1 | `costfacts:<grammar_id>:<rule_id>` or `none:<reason>` before W1 | Required after W1 for materialized JSON rule rows (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| `CostFacts chosen shape` | SPEC 0.4 / W1 | Existing five shapes only: `OffsetTape`, `EventTape`, `EagerTape`, `SinkOnly`, `CollapsedStage`; `none:<reason>` before W1 | No `UnionTape` or new `BackendShape` value. The union is a representation replacement, not a sixth shape (`SYNTHESIS opening state and S-P2/W3 finding sections`). |
| `CostFacts rejected alternative ids` | SPEC 0.4 / W1 | Non-empty list after W1, or `none:<reason>` before W1 | Gate rejects missing rejected-alternative evidence after W1. |
| `Redress entry` | SPEC 0.4 | `none`, `REDRESS-<id>`, or `pending-rejection:<wave>` | Any failed wave must record REDRESS evidence; no silent retreat. |
| `measured_validation_path` | P3-D addition | `measured-row`, `view-boundary`, `post-parse`, `comparator-only`, `absent` | Strict admission and W3 union admission require `measured-row`. |
| `substrate_surface` | P3-D addition | `retained_offset_tape`, `retained_union_tape`, `transient_structural_scan`, `sink_only_transient`, `typed_direct_projection`, `sidecar_forbidden`, `n/a` | W3 Tier A can admit only `retained_union_tape` on touched retained parser rows. |
| `structural_projection_status` | P3-D addition | `discarded_after_capacity`, `retained_as_tape`, `retained_sidecar_forbidden`, `transient_only`, `collapsed_no_retained_vec`, `n/a` | Current baseline is `discarded_after_capacity`; W3 pass requires `retained_as_tape`, not sidecar. |
| `substrate_cardinality` | P3-D addition | `one`, `two_forbidden`, `zero_or_inert`, `unknown` | W3 rejects `two_forbidden` or `unknown`; SC-6's discriminant is cardinality (`SC-6 Lock 1/14 and one-substrate constraints`). |
| `same_wave_consumer_class` | P3-D addition | `gate_only`, `generated_json_retained_parser`, `retained_view_value_ref`, `direct_sink`, `typed_direct`, `track2_oracle`, `none`, `n/a` | W3 Tier A production proof requires `generated_json_retained_parser`; `tape_vs_tape`, `track2_oracle`, and `gate_only` do not count. |
| `track2_independence_status` | P3-D addition | `independent_verified`, `independent_untouched`, `coupled_forbidden`, `n/a` | Track 2 must not call generated Track 1 or new tape internals unless the plan explicitly owns that proof (`RESULTS schema-v3 current main rows and Track 2 authority`, `SC-3 Tier A owner/cost table and one-Tape constraints`). |

These P3-D additions are telemetry fields only. They introduce no BBNF directive, no BIR variant, no public substrate type, and no sixth `BackendShape`.

### §2.3 Row and gate bindings

| Gate | Binding |
|---|---|
| W0 `SK-V8-open` | All 38 current main rows must have row ids, run ids, profile artifacts, hot leaves, host/build/feature metadata, cycles-per-byte or sample-cost equivalent, sidecar freshness, comparator metadata, and `SK-V8-open delta=baseline`. Parser/scanner/SIMD/asm/codegen behavior changes reject W0 (`SPEC Sections 0.1-0.5, 2, and 3-11`, `HANDOFF Sections 2, 3a, and 4-10`). |
| W0 malformed manifest | `gate-json` must reject at least one intentionally malformed sidecar manifest and must reject stale/sidecar-only comparator evidence as strict admission (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| W1 CostFacts | After W1, `gate-json --with-cost-facts` rejects missing `CostFacts rule id`, chosen shape, rejected alternatives, evidence source, REDRESS reference, or wave id (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| W2 typed product | New typed rows require explicit host/API schema facts, generated Track 1, structurally independent Track 2/oracle, full-table maintain, and no digest-stressor promotion (`SPEC Sections 0.1-0.5, 2, and 3-11`, `Alpha-D validated/invalidated rows`). |
| W3 union | Selected parse rows must prove strict validation, comparator evidence, structural cursor work, and admitted tape facts occurred in the measured row. The schema rejects sidecar, view-boundary, post-parse, comparator-only, old offset append, and `tape_vs_tape`-as-consumer proofs (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| W4 direct guard | Direct rows stay guard rows unless a W4 plan names rows and output-plane proof. Digest rows do not become product-plane proof (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| W5 Lock 14 | Generic-crate edits must pass public API, grammar branch, primitive/table, role/fact boundary, template/provider, and non-JSON proof scans (`SPEC Sections 0.1-0.5, 2, and 3-11`). |
| W6 close | `RESULTS schema-v3 current main rows and Track 2 authority`, `REDRESS entries named in this sentence or table row`, `SPEC Sections 0.1-0.5, 2, and 3-11`, and `HANDOFF Sections 2, 3a, and 4-10` agree; no row remains with missing required telemetry or unrouted failure (`SPEC Sections 0.1-0.5, 2, and 3-11`). |

Telemetry implementation slices must fit the user-imposed 90-minute implementation-wave hard cap. If W0 schema lock, W1 CostFacts binding, or any future `tape_vs_tape` augmentation cannot fit that cap with tests and one gate refresh, P3-B/P3-F must split the work or route it as a rejected/blocked plan; do not hide overflow inside a broader behavior wave.

### §2.4 Strict admission refusal rules

`gate-json` rejects strict admission before computing `A`/`G`/`GO` when any predicate fails:

1. `row_output_plane != comparator_plane`.
2. `Strictness != strict`.
3. `comparator_strictness != strict`.
4. `Sidecar freshness` is not `same-run` / `sidecar-same-run`, and the comparator is not a same-run native strict anchor.
5. `measured_validation_path != measured-row`.
6. `parse_utf8=view-boundary`, `Strictness=deferred`, stale sidecar, sidecar-only C++ evidence, historical SK-V6 evidence, or plane mismatch is being used as admission evidence instead of guard telemetry.
7. Outcome is `K` or `S`.

These rules preserve the SC-5 adjudication: current `parse_only` rows are substrate-guard non-admission telemetry, not strict SOTA scoreboard rows, even when their sonic-strict delta is positive (`SC-5 parse_only demotion and tape_vs_tape limits`).

### §2.5 W3 union adjudication fields

For any W3 plan that touches the tape plus structural-projection union, the W0/W1 schema must already be able to answer these questions per selected row:

| Question | Required observable |
|---|---|
| Did the structural projection replace the offset tape rather than run beside it? | `substrate_surface=retained_union_tape`, `structural_projection_status=retained_as_tape`, `substrate_cardinality=one`. |
| Did the old scalar offset producer disappear? | Wave artifact must bind to a row whose `Hot leaf` no longer assigns the selected structural rediscovery leaf as an unowned residual; `structural_projection_status` cannot be `discarded_after_capacity`. |
| Did the measured row consume the new substrate? | `same_wave_consumer_class=generated_json_retained_parser` for Tier A; retained view/`ValueRef` is touched/proven with its own row, not substituted for parser consumption. |
| Did validation occur in the measured row? | `measured_validation_path=measured-row`, `parse_utf8=scan-boundary` or an accepted equivalent, and `escape_complete=yes`. |
| Did Track 2 stay independent? | `track2_independence_status=independent_verified` or `independent_untouched`, plus no Track 2 source coupling hidden in the wave. |
| Did generic code remain grammar-neutral? | Lock 14 scans pass; structural classes and fact ids are opaque generated ordinals, not JSON roles (`SC-3 Tier A owner/cost table and one-Tape constraints`, `SC-6 Lock 1/14 and one-substrate constraints`). |

Failure to answer any row-level question is a W3 plan failure, not a reason to relax the schema. `tape_vs_tape` may be added later as W0/W1 gate-binding telemetry only with named owner files, tests, LOC, and rerun budget; it is never a W3 production same-wave consumer (`SPEC Sections 0.1-0.5, 2, and 3-11`, `HANDOFF Sections 2, 3a, and 4-10`).

### §2.6 Failure states

| Failure state | Gate response |
|---|---|
| `missing_required_field` | Reject after W0 if any current main row lacks a required SK-V8 field, profile artifact, run id, or `SK-V8-open` delta. |
| `unsupported_outcome` | Reject any outcome outside the allowed enum after W0 unless REDRESS and SPEC deliberately amend the enum. |
| `strict_plane_mismatch` | Refuse strict admission when `row_output_plane` and `comparator_plane` differ. Keep the row as guard telemetry only. |
| `deferred_validation_admission` | Refuse strict admission when `Strictness=deferred`, `parse_utf8=view-boundary`, or validation occurred outside the measured row. |
| `stale_or_sidecar_only_strict_claim` | Refuse strict admission when a C++ sidecar, historical row, or stale manifest supplies the decisive comparator evidence. |
| `producer_only_telemetry` | Reject a wave that emits profile, sidecar, CostFacts, substrate, or freshness fields without gate consumption in the same wave. |
| `w0_behavior_drift` | Reject W0 if parser, scanner, SIMD, asm, codegen behavior, product-plane behavior, or throughput moves beyond the +/-1.0 percent telemetry budget. |
| `w1_costfacts_missing` | Reject W1 if materialized JSON rule rows lack CostFacts chosen shape, rejected alternatives, evidence source, wave id, or REDRESS binding. |
| `w3_side_substrate` | Reject W3 if the structural projection is retained beside the old offset append path, if cardinality is not one, or if a parser-owned cursor/fact table survives. |
| `w3_telemetry_consumer_substitution` | Reject W3 if `tape_vs_tape`, `simd_structural_scan`, Track 2, retained view, or comparator-only evidence is substituted for the generated retained parser production consumer. |
| `lock14_generic_leak` | Reject any generic-crate edit that branches on JSON/corpus/role names or interprets structural-class ordinals/fact ids outside generated grammar modules. |
| `implementation_cap_overflow` | Reject or split any telemetry implementation plan that cannot land, verify, refresh the gate, and update results within the user-imposed 90-minute implementation-wave cap. |

## §3 - Falsifiability Binding

W0 fixture expectation: all 38 current main rows are present and telemetry-bound. That is 17 `parse_only`, 17 `direct_to_struct`, and 4 `real_typed_struct` rows (`Alpha-A results extraction`, `RESULTS schema-v3 current main rows and Track 2 authority`).

W0 fixture list: `twitter`, `citm_catalog`, `canada`, `apache_builds`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `marine_ik`, `instruments`, `numbers`, `unicode_mixed`, `unicode_escapes`, `unicode_basic`, `distinct_values`, and `y_string_unicode`. Every row must bind to a `SK-V8-open` run id, profile artifact, sample count, c/B or equivalent sample cost, host triple, build flags, feature mask, and normalized output/comparator plane.

Residual rows that must not remain unprofiled after W0: `twitter parse_only`, `update_center parse_only`, `apache_builds parse_only`, `github_events parse_only`, `unicode_escapes parse_only`, `y_string_unicode parse_only`, `citm_catalog parse_only`, and `instruments parse_only`, matching the Alpha-E telemetry candidate (`Alpha-E candidate shortlist and pre-blocks`).

Sidecar expectations: populated simdjson, yyjson, RapidJSON, and asmjson cells need manifest coverage for corpus identity, binary identity, hardware, build flags, run date/run id, comparator plane, comparator strictness, and freshness. Missing sidecar cells use `absent:<reason>` and cannot be promoted by prose. Alpha-B identifies missing or sparse sidecar coverage by comparator (`Alpha-B comparator matrix`).

Throughput expectation for W0/W1 schema work: every current throughput cell remains within +/-1.0 percent of `SK-V8-open` unless the later plan explicitly names a stricter no-throughput-change gate. A larger move means the wave was not telemetry-only and must be rejected or split (`SPEC Sections 0.1-0.5, 2, and 3-11`).

W3 fixture expectation: any Tier A union plan starts from post-W0 data and names selected parse rows plus guards. SC-3's owner/cost table names structural-heavy diagnostic rows `twitter`, `apache_builds`, `gsoc-2018`, `distinct_values`, and `y_string_unicode`, and number-heavy guards `canada`, `mesh`, and `numbers`; thresholds copied from S-P2 must be recomputed from post-W0 same-run strict rows, not stale sidecars (`SC-3 Tier A owner/cost table and one-Tape constraints`).

## §4 - Pre-Blocked Routes

The schema must prevent old routes from re-entering through relabeling:

- Do not relabel stale sidecars as same-run strict anchors; REDRESS 77 and 78 made strict/lossy provenance explicit (`REDRESS entries named in this sentence or table row`).
- Do not turn `K` or optional `S` substrate-guard rows into SOTA admission rows; SC-5 requires demotion plus full residual visibility (`SC-5 parse_only demotion and tape_vs_tape limits`).
- Do not treat `tape_vs_tape`, `simd_structural_scan`, sidecar manifests, or comparator-only rows as W3 production consumers (`SC-5 parse_only demotion and tape_vs_tape limits`, `SPEC Sections 0.1-0.5, 2, and 3-11`).
- Do not introduce `UnionTape`, `BackendShape::Union`, a new BIR variant, a new BBNF directive, a public substrate API, parser-owned cursor/facts, or a parallel substrate (`S-P2 V7 consolidated verdict and preserved boundaries`).
- Do not reopen REDRESS 28+33, 50-55, 60-72, 80, 82-84, 88, 89, or historical blocked routes without fresh W0 evidence, same-wave consumer, no-regression gate, REDRESS citation, and challenge acceptance (`HANDOFF Sections 2, 3a, and 4-10`, `Alpha-C REDRESS digest`).

## §5 - Sources

- `restart/prompts/ORCHESTRATOR.md`, `restart/prompts/ORCHESTRATOR.md`, `restart/prompts/ORCHESTRATOR.md`.
- `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`, `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md`.
- `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`, `restart/prompts/pass-contracts/SKINNY-TRIUMVIRATE.md`.
- `restart/skinny/tranches/sk-v8/SYNTHESIS.md`, `restart/skinny/tranches/sk-v8/SPEC.md`, `restart/skinny/tranches/sk-v8/SPEC.md`, `restart/skinny/tranches/sk-v8/SPEC.md`.
- `restart/skinny/tranches/sk-v8/HANDOFF.md`, `restart/skinny/tranches/sk-v8/HANDOFF.md`.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-1-offset-tape-teardown.md`.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-2-two-stage-sota.md` for Tier A/Tier B measured-path, same-wave consumer, scalar/checkasm, and strict-plane requirements.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-3-union-substrate-design.md`.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-4-string-plane-gap.md`.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-5-k-classification-adjudication.md`.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`, `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/SC-6-lock1-amendment-generalisation.md`.
- `restart/skinny/tranches/sk-v8/research/p2-substrate-ceiling/hardening/HARDENING-S-P2-V7-CONSOLIDATED.md`.
- `restart/skinny/tranches/sk-v8/research/alpha/alpha-A-results-extraction.md`, `restart/skinny/tranches/sk-v8/research/alpha/alpha-B-competitor-deltas.md`, `restart/skinny/tranches/sk-v8/research/alpha/alpha-C-redress-digest.md`, `restart/skinny/tranches/sk-v8/research/alpha/alpha-D-validated-invalidated.md`, `restart/skinny/tranches/sk-v8/research/alpha/alpha-E-candidate-shortlist.md`.
- `skinny/RESULTS.md`, `skinny/RESULTS.md`, `skinny/REDRESS.md`, `skinny/REDRESS.md`.

## Self-Verdict

ACCEPT.

Confidence: 92%.

Reason: the plan preserves the live schema-v3 surface, makes the SK-V8 required fields gate-consumed, supplies allowed values and failure states for strictness/output-plane/comparator/substrate classification, keeps `tape_vs_tape` as telemetry only, and gives W3 enough observable substrate/cardinality fields to adjudicate the tape plus structural-projection union without a directive, BIR variant, public substrate type, or automatic implementation dispatch.
