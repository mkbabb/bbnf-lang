# SK-V15 S-P3 V1 CH7 OVERFIT-PRUNE / GATE-EXCLUSION

Pass: S-P3 Synthesis-Plan. Cycle: V1. Lens: CH7.
Date: 2026-05-28.
Input packet: commit `4fe37c042`.

## Verdict

REVISE.

The packet carries the right anti-overfit intent from S-P0 and PASS-IMPL V1,
but the executable contract still leaves four overfit routes open: canonical
telemetry fields can be aliased away in P3-B, CSS W8R broadcast numbers appear
as typed-admission floors in candidate/gate text, gate-exclusion language is
not uniformly load-bearing in SPEC and DISPATCH, and W7 EventTape/lowerer
wording can be read as permission to materialize a sidecar event vector rather
than an existing BackendShape lowering.

## Findings

| ID | Disposition | Surface | Finding | Required fold |
|---|---|---|---|---|
| CH7-V1-01 | REVISE | P3-B W0 telemetry | P3-B names `sample_count`, `row_claim_scope`, `comparator_workload_id`, `producer_path`, `generator_source_id`, `semantic_output_kind`, and `strictness_source`, while P3-D and SPEC name the required SK-V15 fields as `measurement_row_id`, `measurement_origin`, `value_plane`, `css_comparator_workload`, `generator_source`, `lock14_scan_scope`, `lock16_status`, `checkasm_or_parity_status`, `gate_exclusion_report`, and `broadcast_group_id`. This alias gap is an overfit vector because a gate can appear to consume telemetry while missing the fields that detect hidden broadcast and self-exempting scans. | Replace the P3-B field list with the exact P3-D/SPEC names. Add a rule that aliases reject unless the schema is deliberately bumped and the gate consumes the mapping. |
| CH7-V1-02 | REVISE | P3-A / P3-C CSS thresholds | CSS candidate and W5 gate text still quote W8R broadcast values as live floors. P3-D says those numbers are one repeated diagnostic full-parse run, and SPEC says CSS admits require typed output and same-workload comparator semantics. Reusing the W8R numbers as typed-output floors risks laundering the broadcast evidence into a new row label. | Move W8R numbers to diagnostic-negative fixtures only. W5A/W5B must capture fresh same-run cssparser typed-value comparator data after Track 1 emits typed CSS output, then bind any CSS admit to that same-plane measurement. |
| CH7-V1-03 | REVISE | SPEC / DISPATCH gate-exclusion | SPEC states self-exempting gates reject, but the final per-wave envelopes do not consistently require gate output to print included roots, excluded roots, exclusion reasons, and scan of the exclusion list itself. DISPATCH similarly says gates consume reports but does not carry the canonical table. | Promote the Lock 14/16 exclusion report table into SPEC and DISPATCH. Every W2/W3/W4/W6/W7/W8 gate that depends on generic cleanliness, generated provenance, lowerer proof, or primitive admission must fail if exclusions are missing or producer-only. |
| CH7-V1-04 | REVISE | W7 EventTape / BackendShape lowerers | W7 asks for all five lowerers, including EventTape, but does not counter-bind EventTape as an existing BackendShape lowering over the accepted substrate. Without that sentence, a redress plan could close by creating a retained sidecar event vector or parser-owned event stream under the EventTape name. | Add explicit EventTape language to P3-C, SPEC, and DISPATCH: EventTape is a BackendShape lowering only; it may not create a sixth shape, public substrate API, sidecar event vector, retained parser-owned stream, or alternate document projection. |
| CH7-V1-05 | ACCEPT WITH FOLD | S-P2 boundary | The packet correctly excludes x86, PMULL hot-body promotion, CSSC bulk-consumer promotion, numeric/digit rescue, retained structural/cursor/class streams, schema-shaped builders, harness hashes, and stale CSS broadcast proof. | Preserve this exclusion list when reindexing to W0-W11 and normalizing REDRESS clusters. |

## Required Folds

1. Normalize P3-B W0 telemetry to the exact P3-D/SPEC field names and make
   alias-only telemetry a reject.
2. Remove W8R broadcast metrics from live CSS typed-admission thresholds; keep
   them only as diagnostic negative fixtures until fresh typed-output
   cssparser comparator rows exist.
3. Promote gate-exclusion reporting into SPEC and DISPATCH as a table with
   included roots, excluded roots, exclusion reasons, and gate consumption.
4. Counter-bind EventTape in every W7/lowerer surface so it cannot become a
   retained sidecar event vector or new substrate.
5. Carry the S-P2 rejected-route set unchanged through the W0-W11 reindex.

## Close Condition For V2

CH7 can accept only when the packet has one canonical telemetry vocabulary,
fresh typed-output CSS comparator gates, visible exclusion reporting in the
dispatch surfaces, and explicit EventTape anti-sidecar language. Directional
warnings are not enough; the final wave contract must make the overfit routes
unexecutable.
