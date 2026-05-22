# SK-V13 W9 Plan - Same-Substrate Union Material Differential

Cycle: W9 Plan. Scope: SPEC Section 13.

## Selected Intervention

Select C1: `union-c1-per-rule-same-tape`.

W9 will attempt a codegen-private, grammar-local same-substrate projection
that uses the existing tape or generated fact stream as the only retained
substrate. The material differential against REDRESS 96/97/98 is compile-time
generated projection, not a runtime sidecar:

- Not REDRESS 96: no retained class column and no move-consumed structural
  index vector.
- Not REDRESS 97: no streaming cursor, second cursor type, or parser-owned
  structural list.
- Not REDRESS 98: no class-lane-only proof and no docs-only gate retirement.

The planned C1 surface is deliberately narrow:

1. Add generated-private projection metadata/helpers to the selected grammar
   config/template surface.
2. Consume those helpers from one production parser/fact-stream path in the
   same wave.
3. Micro-prove reference vs candidate projection cost and equality.
4. Measure the named production row.
5. Admit only if the row moves toward SOTA without guard regression; otherwise
   close as the measured architectural block
   `JSON-CSS-W9-SAME-SUBSTRATE-UNION-CONSUMED-BUT-NO-ROW-MOVEMENT`.

No W9 source may add public `UnionTape`, public substrate API, `BackendShape`,
BIR variant, directive, retained `StructuralIndex`, class column, aux table,
sidecar event vector, second scan, parser-owned cursor/list, or generic JSON
policy.

## Owner Paths

Primary SPEC owner paths:

- `skinny/crates/runtime/src/tape/`
- `skinny/crates/runtime/src/grammars/json/`
- `skinny/crates/runtime/src/grammars/css_l4_*`
- `skinny/crates/codegen/src/`
- `skinny/crates/passes/src/`
- `skinny/crates/bbnf-bench/`

Unselected:

- `skinny/crates/bbnf-simd/` is read-only in W9. Selecting or touching it
  would promote W9 to the C3 SIMD-first gate with same-wave zero-orphan,
  scalar-reference, strict checkasm, and production consumer obligations.

Selected implementation owner paths:

- `skinny/crates/runtime/src/grammars/json/config.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/codegen/src/json_templates/config.rs`
- `skinny/crates/codegen/src/json_templates/generated.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/config.rs`
- `skinny/crates/runtime/src/grammars/css_l4_declaration_values_extended/generated.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/config.rs`
- `skinny/crates/codegen/src/css_l4_declaration_values_extended_templates/generated.rs`
- `skinny/crates/bbnf-bench/src/report.rs`
- `skinny/crates/bbnf-bench/src/bin/gate.rs`
- `skinny/crates/bbnf-bench/src/lock14_baseline.rs`
- `skinny/xtask/src/main.rs`
- `restart/skinny/tranches/sk-v13/research/w9/`
- `skinny/REDRESS.md`

`skinny/RESULTS.md` and `restart/skinny/ROLLING-SOTA-DELTA.md` are touched
only if W9 admits a row or changes a row's recorded disposition. A measured
block with unchanged Mbps leaves both files unchanged.

## Implementation Shape

W9 uses a reference/candidate pair so the attempt is falsifiable rather than a
scaffold:

1. JSON generated config defines a private projection descriptor for structural
   offsets in the existing tape. The runtime generated parser consumes that
   descriptor from the structural emit path. If no legal hot-path consumer can
   read the descriptor without adding sidecars or public API, redress records
   that as the C1 intrinsic block.
2. CSS declaration-values-extended generated config defines an equivalent
   private projection descriptor for generated fact-stream token classes. The
   live generated scanner/fact-stream path consumes it; static captured rows
   are not eligible as W9 consumers.
3. The candidate must not change public output semantics. Strict equality for
   JSON and CSS rows is mandatory before measurement evidence is accepted.
4. A retained fact artifact records the reference/candidate micro-prove:
   variant id, row id, reference hash, candidate hash, equality status,
   reference ns/event or ns/byte, candidate ns/event or ns/byte, speedup ratio,
   and whether production row Mbps moved.
5. If redress proves that C1 cannot be consumed by a row without a forbidden
   public substrate, sidecar, class vector, second scan, or output-semantic
   change, the code slice is reverted and the report records an architectural
   block rather than landing support-only code.

Preferred named consumers:

- JSON: `json/twitter/parse_only/main` if the retained parser path can consume
  the same-tape projection without public API.
- CSS: `css_l4/declaration_values_extended/direct_to_struct/main` if the fact
  stream can consume the projection while preserving strict equality.

The plan intentionally names both candidate rows. Redress may admit or block
on either row, but at least one named production row must be measured.

## Falsifiability Gate

Primary gate: `G-W9-SAME-SUBSTRATE-UNION`.

Pass conditions:

1. Report schema is `sk-v13-same-substrate-union-v1`.
2. `wave_id = SK-V13-W9`.
3. `consumer_gate = G-W9-SAME-SUBSTRATE-UNION`.
4. `union_variant_id = union-c1-per-rule-same-tape`.
5. `material_differential_status = accepted`.
6. REDRESS citations include `96`, `97`, and `98`.
7. `substrate_cardinality = one`.
8. `public_union_tape_status = absent`.
9. `public_substrate_api_status = absent`.
10. `backend_shape_expansion_status = absent`.
11. `bir_directive_expansion_status = absent`.
12. `class_column_status = absent`.
13. `retained_structural_index_status = absent`.
14. `sidecar_vector_status = absent`.
15. `second_scan_status = absent`.
16. `parser_owned_cursor_status = absent`.
17. `json_consumer_row_id` and/or `css_consumer_row_id` names a production row.
18. JSON strict equality status passes for any JSON row touched.
19. CSS strict equality status passes for any CSS row touched.
20. Full JSON/CSS guard maintain status passes.
21. Lock 14 owner-path and generic-scan status passes.
22. `row_move_toward_sota_status` is `pass`, `admitted`, or
    `measured_architectural_block`.
23. If status is `pass` or `admitted`, report includes before/after Mbps,
    same-plane SOTA threshold, strict comparator proof, and no guard demotion.
24. If status is `measured_architectural_block`, `block_id =
    JSON-CSS-W9-SAME-SUBSTRATE-UNION-CONSUMED-BUT-NO-ROW-MOVEMENT`.
25. W5/W6/W7/W8 companion reports remain gate-consumed in the final advisory
    run.

Reject states:

- Support-only projection metadata.
- Microbench-only or checkasm-only claim.
- A row consumer postponed to W11/W12/W14.
- Public `UnionTape`, public substrate API, new `BackendShape`, new BIR
  variant, new directive, retained `StructuralIndex`, sidecar vector, class
  column, parser cursor/list, aux table, or second scan.
- C3/SIMD file touch without same-wave zero-orphan/checkasm/scalar-reference
  closure.
- JSON/CSS guard regression.
- `RESULTS.md` or rolling delta edit without measured row movement.

## Report Shape

Report schema: `sk-v13-same-substrate-union-v1`.

Required fields:

- Provenance: `schema_version`, `wave_id`, `run_id`, `source_commit`,
  `host_triple`, `build_flags`, `feature_mask`, `consumer_gate`,
  `g_omega_status`.
- Variant: `union_variant_id`, `material_differential`,
  `material_differential_status`, `prior_redress_citations`.
- Substrate proof: `substrate_cardinality`, `public_union_tape_status`,
  `public_substrate_api_status`, `backend_shape_expansion_status`,
  `bir_directive_expansion_status`, `class_column_status`,
  `retained_structural_index_status`, `sidecar_vector_status`,
  `second_scan_status`, `parser_owned_cursor_status`.
- Consumers: `json_consumer_row_id`, `json_consumer_path`,
  `css_consumer_row_id`, `css_consumer_path`, `same_wave_consumer_class`.
- Micro-prove: `reference_projection_hash`, `candidate_projection_hash`,
  `projection_equality_status`, `reference_ns_per_unit`,
  `candidate_ns_per_unit`, `projection_speedup_ratio`.
- Measurements: `json_row_mbps_before`, `json_row_mbps_after`,
  `css_row_mbps_before`, `css_row_mbps_after`,
  `row_move_toward_sota_status`.
- Guards: `json_strict_equality_status`, `css_strict_equality_status`,
  `json_guard_state`, `css_guard_state`.
- Lock 14: `lock14_status`, `lock14_owner_path_status`,
  `lock14_generic_scan_status`.
- SIMD: `simd_route_status`, `orphan_count_after`, `checkasm_status`,
  `scalar_reference_status`.
- Evidence: `union_fact_artifact_path`, `union_fact_artifact_sha256`.
- Disposition: `affected_row_ids`, `block_id`, `redress_entry`.

Gate flag: `--skv13-same-substrate-union-report`.

Gate print:

`G-W9-SAME-SUBSTRATE-UNION <row_move_toward_sota_status> <path>`.

## Preblocked Routes

Binding preblocks:

- REDRESS 50/51/53: side tables, byte-class cursors, parser-local structural
  cursor/list.
- REDRESS 88/89: old PMULL/CTZ default bodies. C3 not selected.
- REDRESS 92: scanner/tape non-isomorphism. W9 must prove output equality
  before any scanner position is treated as a tape event.
- REDRESS 96/97/98: class-column, streaming-cursor, and class-lane union
  replays.
- REDRESS 126: microbench-only ASM split and orphan demotion. W9 does not
  touch `bbnf-simd`.

Material differential:

- Generated-private C1 projection.
- Existing tape or fact stream remains the only substrate.
- Same-wave row consumer is measured.
- Gate/report rejects support-only closure.

## Revert Protocol

If W9 fails after source edits:

1. Revert runtime JSON/CSS generated/config edits, codegen templates,
   report/gate/xtask/Lock14 edits, retained W9 artifacts, and any RESULTS or
   rolling delta edits as one slice.
2. Save the rejected patch at `/tmp/skv13-waveW9-rejected.patch`.
3. Append REDRESS with failed variant id, row measurements, guard state,
   material differential, and rejected patch path.
4. Retain only research, plan, and CHALLENGE artifacts.

If W9 records the measured architectural block without retained behavior
source, keep only the report/gate/Lock14 evidence needed for machine
consumption and do not update RESULTS/rolling delta.

## Validation Commands

Minimum redress commands:

- Targeted runtime/codegen tests for the selected C1 projection.
- `cargo test -p runtime json -- --nocapture`
- `cargo test -p runtime css_l4_declaration_values_extended_emit_fact_stream -- --nocapture`
- `cargo test -p codegen css_l4_declaration_values_extended_generated_runtime_reproducible -- --nocapture`
- Named JSON/CSS row strict equality tests selected by redress.
- `cargo test -p bbnf-bench skv13_same_substrate_union_report -- --nocapture`
- `cargo test -p bbnf-bench --bin gate skv13_same_substrate_union_report -- --nocapture`
- `cargo test -p xtask gate_json_passthrough_accepts_skv13_same_substrate_union_report_flag -- --nocapture`
- `cargo test -p bbnf-bench lock14_baseline -- --nocapture`
- `RUSTFLAGS="-C target-cpu=native" cargo xtask gate-json --check-results
  --advisory
  --skv13-decision-regex-report
  ../restart/skinny/tranches/sk-v13/research/w5/skv13-W5-decision-regex.json
  --skv13-decision-active-cost-report
  ../restart/skinny/tranches/sk-v13/research/w6/skv13-W6-decision-active-cost.json
  --skv13-decision-csp-cascade-report
  ../restart/skinny/tranches/sk-v13/research/w7/skv13-W7-decision-csp-cascade.json
  --skv13-per-grammar-policy-report
  ../restart/skinny/tranches/sk-v13/research/w8/skv13-W8-per-grammar-policy.json
  --skv13-same-substrate-union-report
  ../restart/skinny/tranches/sk-v13/research/w9/skv13-W9-same-substrate-union.json`

## CHALLENGE Questions

1. Is C1 sufficiently material against REDRESS 96/97/98, or does the planned
   projection still replay a class/structural side substrate under another
   name?
2. Can the named JSON/CSS consumer truly read the projection in production
   without public API, output drift, or a sidecar?
3. Does the measured-block route have enough intrinsic evidence if no row
   moves?
4. Are full JSON/CSS guards and Lock 14 scans strong enough to catch silent
   demotion or generic JSON leakage?
5. Does excluding `bbnf-simd` from W9 correctly route C3 to W12 without
   deferring a selected W9 obligation?
