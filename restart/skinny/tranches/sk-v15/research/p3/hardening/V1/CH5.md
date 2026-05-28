# CH5 Hidden Coupling - SK-V15 S-P3 V1

Verdict: REVISE

Scope: evaluate the S-P3 V1 packet from commit 4fe37c042 for hidden
coupling across W0-W9: parallel substrates, retained sidecars, renamed
scanners, Track 1 == Track 2 dishonesty, parser-owned structural
projections, aux density tables, sidecar event vectors, and broadcast
admission.

## Findings

| ID | Disposition | Wave or surface | Finding | Evidence | Required fold |
|---|---|---|---|---|---|
| CH5-S-P3-V1-F1 | REVISE | SPEC Section 13, W7 | The packet mostly blocks sidecars, but the final SPEC does not explicitly carry the full CH5 forbidden set into executable exit/pre-block language. PASS-3 CH5 asks whether SPEC exit gates forbid parser-owned structural projection, retained cursor, aux density table, and sidecar event vector. P3-C and P3-E do forbid these classes, but SPEC Section 13 only names retained sidecar tables, cursor streams, class columns, public UnionTape, and second tape. W7 also names EventTape lowerers without an explicit statement that EventTape must remain an existing BackendShape lowering into the accepted substrate, not a retained sidecar event vector. | PASS-3 CH5 requirement: `restart/prompts/skinny/PASS-3-SYNTHESIS-PLAN.md:134-138`; P3-C rejects retained structural/cursor/class streams, parser-owned sidecars, density tables, second tapes, and related routes at `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:215-220`; P3-E blocks aux projection tables, parser-owned structural cursors, class columns, retained indexes, streaming cursor state, public UnionTape, and sidecars at `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:48-60`; SPEC pre-block is narrower at `restart/skinny/tranches/sk-v15/SPEC.md:390-403`; W7 names EventTape at `restart/skinny/tranches/sk-v15/SPEC.md:327-343`. | Fold the full CH5 forbidden vocabulary into SPEC Section 1, the global gates, Section 13, and W7: no parser-owned structural projection, retained cursor/list, aux density/projection table, sidecar event vector, parallel source pass, second tape, public UnionTape, or new substrate/API. State that EventTape is only one of the existing five BackendShape lowerers and must not materialize a sidecar event vector or retained parser-owned event stream. |
| CH5-S-P3-V1-F2 | REVISE | P3-B W0 telemetry lock | P3-B W0 uses a non-canonical anti-broadcast field list that drifts from P3-D and the SPEC. The drift is CH5-relevant because hidden coupling detection depends on exact gate-consumed fields, not near synonyms. P3-B names sample_count, row_claim_scope, comparator_workload_id, producer_path, generator_source_id, semantic_output_kind, and strictness_source, while P3-D/SPEC require measurement_origin, value_plane, css_comparator_workload, generator_source, lock14_scan_scope, lock16_status, checkasm_or_parity_status, gate_exclusion_report, and broadcast_group_id. | P3-B W0 field list: `restart/skinny/tranches/sk-v15/research/p3/p3b-wave-sequencing.md:45-48`; canonical SK-V15 fields in SYNTHESIS: `restart/skinny/tranches/sk-v15/SYNTHESIS.md:121-127`; P3-D field meanings and rejection rules: `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:21-50`; SPEC canonical field list: `restart/skinny/tranches/sk-v15/SPEC.md:94-116`. | Replace P3-B's W0 field list with the exact ten canonical SK-V15 fields or make P3-B reference P3-D/SPEC by name. Add a rejection rule that aliases are not substitutes unless the schema is deliberately bumped and the gate consumes the mapping. |
| CH5-S-P3-V1-F3 | ACCEPT | W0, W1, W5 | The packet does not introduce broadcast admission. The current CSS W8R shape is treated as diagnostic/non-admission, W1 demotes or collapses it, and W5 may admit CSS only on typed output with distinct measurements or one explicit aggregate diagnostic row. | Synthesis anti-broadcast close condition and addendum: `restart/skinny/tranches/sk-v15/SYNTHESIS.md:38-49`, `restart/skinny/tranches/sk-v15/SYNTHESIS.md:98-110`; P3-C W0/W1/W5 gates: `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:33-73`, `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:133-155`; P3-D hidden one-to-N rejection: `restart/skinny/tranches/sk-v15/research/p3/p3d-telemetry-schema.md:52-74`; SPEC W0/W1/W5: `restart/skinny/tranches/sk-v15/SPEC.md:168-213`, `restart/skinny/tranches/sk-v15/SPEC.md:281-305`. | None beyond F1/F2. Preserve the diagnostic-only W8R rule and distinct measurement_row_id requirement. |
| CH5-S-P3-V1-F4 | ACCEPT | W2, W3, W4, W6, W8 | No wave affirmatively introduces a renamed scanner, Track 1 == Track 2 path, retained sidecar substrate, aux density table, or FNV production arbiter. The packet keeps Lock 14/16 scans fail-closed, blocks generic JSON/CSS mode leaks, requires Pattern H regeneration proof, makes Decision Engine facts grammar-neutral, and quarantines FNV to bench/xtask evidence. | ORCHESTRATOR CH5 and convergence rules: `restart/prompts/ORCHESTRATOR.md:74-88`, `restart/prompts/ORCHESTRATOR.md:104-123`; P3-C primitive-specific rejects: `restart/skinny/tranches/sk-v15/research/p3/p3c-falsifiability-gates.md:215-220`; P3-E global pre-blocks: `restart/skinny/tranches/sk-v15/research/p3/p3e-preblocked-ledger.md:46-60`; SPEC W2-W4/W6/W8 gates: `restart/skinny/tranches/sk-v15/SPEC.md:215-279`, `restart/skinny/tranches/sk-v15/SPEC.md:307-325`, `restart/skinny/tranches/sk-v15/SPEC.md:348-367`. | None beyond F1/F2. Keep same-wave consumer and gate-consumed report requirements. |

## Required Folds

1. Fold the full CH5 forbidden set into the final SPEC and dispatch language:
   parser-owned structural projection, retained cursor/list, aux density or
   projection table, sidecar event vector, parallel source pass, second tape,
   public UnionTape, retained sidecar table, class column, whitespace bitmap,
   and new substrate/API.

2. Counter-bind W7 EventTape language. W7 may implement only the existing
   BackendShape lowerer path, with output written into the accepted runtime
   substrate or a gate-consumed rejected alternative. It must not create a
   sidecar event vector, retained parser-owned event stream, sixth
   BackendShape, public substrate API, or alternate document projection.

3. Normalize P3-B W0 telemetry to the exact P3-D/SPEC field names:
   `measurement_row_id`, `measurement_origin`, `value_plane`,
   `css_comparator_workload`, `generator_source`, `lock14_scan_scope`,
   `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`,
   and `broadcast_group_id`. Any alias field must be gate-consumed through an
   explicit schema bump; otherwise it is producer-only telemetry and rejects.

4. Preserve the existing ACCEPTed anti-broadcast and Track 1/Track 2 folds:
   CSS W8R rows remain diagnostic/non-admission until typed same-workload
   retiming, N admits require N distinct measurement rows unless the row is
   one explicit aggregate, and FNV closed-enum evidence remains bench-only.
