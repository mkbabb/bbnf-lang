# SK-V12 W4 Orphan Disposition

Wave: SK-V12 W4.
Redress entry: REDRESS-126.
Status: ROUTE-PRODUCTION-SPLIT.

## Selected Candidate

```text
selected_candidate=a64_ascii_set_run_skip
selected_candidate_orphan_accounting=separate_from_five_row_orphan_set
selected_candidate_microbench=pass
selected_candidate_microbench_artifact=restart/skinny/tranches/sk-v12/research/w4/w4-delimiter-find-microbench.json
selected_candidate_final_action=route_production_split_before_wiring
```

The selected candidate is not one of the five SK-V12 W4 orphan rows. The
caller-level parity/microbench test proves the existing
`byte_class_from_eq_set_64` primitive can accelerate
`find_ascii_set_member64`, but PLAN-V4 forbids production CSS wiring in this
default branch after a pass. Production consumption is routed to a separate
planned split.

## Orphan Rows

| orphan_name | evidence_status | consumer_path / no-production-consumer | lock16_status | redress_entry | source_grep_evidence | test_or_checkasm_evidence | redress_adjacency | material_differential | selected_by_w4 | final_disposition |
|---|---|---|---|---|---|---|---|---|---|---|
| `bitmap_prefix_xor_64` | `production_reachable_scalar_delegate` | `skinny/crates/runtime/src/grammars/json/scan.rs:203,239 -> bbnf_simd::prefix_xor_64 -> prim::bitmap_prefix_xor_64` | `scalar_delegate_no_new_admission` | REDRESS-126 | `rg prefix_xor_64 skinny/crates/runtime skinny/crates/bbnf-simd/src` shows JSON scan consumer and aarch64 delegate body. | Existing `checkasm_bitmap_prefix_xor_64.rs` covers scalar parity; not rerun because W4 ships no production admission for this primitive. | REDRESS 88 | Historical PMULL/prefix-XOR route remains distinct from W4 delimiter member-find; no new EOR3/PMULL body is admitted. | no | `inventory_demoted_with_evidence` |
| `bitmap_next_set_bit` | `no_non_test_consumer_found` | `no-production-consumer` | `no_new_admission` | REDRESS-126 | `rg bitmap_next_set_bit skinny/crates/runtime skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/tests` shows dispatch/tests but no runtime consumer. | Existing `checkasm_bitmap_next_set_bit.rs` covers scalar parity; not rerun because W4 does not select CTZ production wiring. | REDRESS 89 | Historical CTZ/bulk route remains distinct from W4 delimiter member-find; no CSSC CTZ production body is admitted. | no | `inventory_demoted_with_evidence` |
| `bulk_emit_positions_64` | `production_reachable_scalar_delegate` | `skinny/crates/bbnf-simd/src/lib.rs:209 -> compact_mask -> prim::bulk_emit_positions_64`; JSON scan calls `compact_mask` at `scan.rs:267`. | `scalar_delegate_no_new_admission` | REDRESS-126 | `rg compact_mask skinny/crates/runtime skinny/crates/bbnf-simd/src` shows JSON scan consumer and scalar-delegate aarch64 body. | Existing `checkasm_bulk_emit_positions_64.rs` covers scalar parity; not rerun because W4 does not select bulk emit production wiring. | REDRESS 89 | Historical bulk-emitter route remains distinct from W4 delimiter member-find; no production rewire is admitted. | no | `inventory_demoted_with_evidence` |
| `byte_context` | `support_test_only` | `no-production-consumer` | `support_primitive_no_new_admission` | REDRESS-126 | `rg byte_context skinny/crates/runtime skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/tests` shows only module/test usage. | Existing `aarch64_primitives.rs::byte_context_shifts_across_chunk_boundaries` smoke-tests the shifts; not rerun because W4 does not select this primitive. | n/a | W4 does not attempt a byte-context neighbor-lane route; it selects delimiter member-find. | no | `inventory_demoted_with_evidence` |
| `cache_hints` | `support_test_only` | `no-production-consumer` | `support_primitive_no_new_admission` | REDRESS-126 | `rg cache_hints skinny/crates/runtime skinny/crates/bbnf-simd/src skinny/crates/bbnf-simd/tests` shows only module/test usage. | Existing `aarch64_primitives.rs::streaming_pair_store_writes_two_words` smoke-tests the store hint; not rerun because W4 does not select this primitive. | n/a | W4 does not attempt a prefetch/store-hint route; it selects delimiter member-find. | no | `inventory_demoted_with_evidence` |

## Final Fields

```text
selected_candidate=a64_ascii_set_run_skip
selected_candidate_orphan_accounting=separate_from_five_row_orphan_set
orphan_count=0
json_guard_state=pass:no-touch-proof-empty
css_gate_state=route-production-split:no-production-css-wiring
```
