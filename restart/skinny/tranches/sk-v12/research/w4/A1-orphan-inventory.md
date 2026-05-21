# SK-V12 W4 A1 - AArch64 Orphan Inventory

Scope: SPEC Section 9 / USER PIN D5 inventory for the five carried aarch64
orphans. This is read-only research; no behavior source change is proposed
here.

Sources read:

- `restart/skinny/tranches/sk-v12/SPEC.md` Section 9.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md` D5.
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md`.
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`.
- Current `bbnf-simd` aarch64/scalar modules and checkasm tests.
- Live consumer grep across `skinny/crates`.

## Inventory

| Primitive | Current files / symbols | Production consumer status | Scalar / checkasm status | Inventory demotion without behavior source change? | Blocker if not demoted |
|---|---|---|---|---|---|
| `bitmap_prefix_xor_64` | `src/aarch64/bitmap_prefix_xor_64.rs::bitmap_prefix_xor_64_neon`; `src/scalar/bitmap_prefix_xor_64.rs::bitmap_prefix_xor_64_scalar`; dispatch field in `src/dispatch.rs`; public `prim::bitmap_prefix_xor_64`; wrapper `prefix_xor_64`. | Consumed by JSON scan through `runtime/src/grammars/json/scan.rs` -> `prefix_xor_64(real_quotes_fast, in_string)`. The current aarch64 body is not a NEON/ASM body; it delegates to the scalar reference. | Scalar reference exists. Dedicated `tests/checkasm_bitmap_prefix_xor_64.rs` covers fixed cases plus 4096 xorshift masks against scalar. CHECKASM-REPORT names it as a checked primitive, but current host implementation is scalar-on-arm64. | Yes. The live aarch64 file is a no-op scalar delegate, and REDRESS 88 rejected PMULL as the default hot body. Demoting the carried orphan to "scalar-delegate consumed; no admitted aarch64 kernel" changes no runtime behavior. | Any new PMULL/EOR3/prefix route must cite REDRESS 88, micro-prove a narrow same-row consumer, keep scalar/checkasm parity, and wire that consumer same-wave. |
| `bitmap_next_set_bit` | `src/aarch64/bitmap_next_set_bit.rs::bitmap_next_set_bit_neon`; `src/scalar/bitmap_next_set_bit.rs::bitmap_next_set_bit_scalar`; dispatch field; public `prim::bitmap_next_set_bit`. | No current non-test production call to `prim::bitmap_next_set_bit` was found. CHECKASM-REPORT's older "compact_mask structural projection emit" description is stale for the live tree: `compact_mask` calls `bulk_emit_positions_64`, not this primitive. The aarch64 body delegates to scalar. | Scalar reference exists. Dedicated `tests/checkasm_bitmap_next_set_bit.rs` covers boundary cases and 4096 random masks for every cursor 0..=64. | Yes. It is an unused scalar-delegate wrapper with no active aarch64 body. Inventory demotion is behavior-neutral. | A CSSC/CTZ admission must cite REDRESS 89, prove a real caller path, and avoid the rejected CTZ bulk-consumer shape unless materially different. |
| `bulk_emit_positions_64` | `src/aarch64/bulk_emit_positions_64.rs::bulk_emit_positions_64_neon`; `src/scalar/bulk_emit_positions_64.rs::bulk_emit_positions_64_scalar`; dispatch field; public unsafe `prim::bulk_emit_positions_64`; wrapper `compact_mask`. | Production-consumed. `bbnf_simd::compact_mask` calls `prim::bulk_emit_positions_64`; `scan_dispatch` and JSON scan both call `compact_mask`. However, on aarch64 the selected body delegates directly to the scalar implementation. | Scalar reference exists. Dedicated `tests/checkasm_bulk_emit_positions_64.rs` covers sentinel-preserving writes, boundary masks, and 4096 random masks. CHECKASM-REPORT / REDRESS history shows prior bulk emission admission/rejection cycles, but the live aarch64 file is scalar-delegating. | Yes, if recorded precisely as "production-consumed scalar delegate, no orphan aarch64 kernel body." No behavior source change is needed because removing the orphan claim does not alter the active code path. | Any real aarch64 emission body must reopen the REDRESS 89 adjacency with a material differential, strict checkasm, and same-wave CSS or JSON-guard consumer measurement. |
| `byte_context` | `src/aarch64/byte_context.rs::shift_right_one` and `shift_left_one` using `vextq_u8`; exported by `src/aarch64/mod.rs`. | No production consumer found. Only current code refs are docs and `tests/aarch64_primitives.rs::byte_context_shifts_across_chunk_boundaries`. | No scalar module and no dedicated `checkasm_byte_context` test. There is a smoke-style aarch64 primitive test for one triple. CHECKASM-REPORT lists this as a cheap future checkasm target, not admitted coverage. | Yes. It is support inventory, not a shipped behavior primitive. SPEC Section 9 explicitly allows inventory demotion with evidence for non-selected orphans when no behavior source change is needed. | To admit instead, W4 needs a scalar triple-window oracle, checkasm sweeps over previous/current/next chunks, and a same-wave chunk-spanning CSS string/comment/layout or JSON guard consumer. |
| `cache_hints` | `src/aarch64/cache_hints.rs::prefetch_read`, `prefetch_write_stream`, `store_pair_streaming_u64`, `streaming_pair_store_hint`; exported by `src/aarch64/mod.rs`. | No production consumer found. Only current code ref outside docs is `tests/aarch64_primitives.rs::streaming_pair_store_writes_two_words`. | No scalar module and no checkasm harness. The store-pair smoke test proves one side-effectful store, not semantic parser parity. P2 research treats PRFM/STNP as inventory unless a concrete output/tape/fact-stream writer consumes it. | Yes. With no production caller, inventory demotion is behavior-neutral. It should not be called Lock 16-admitted, because hints/stores still need a semantic no-op oracle or side-effect bounds before production use. | Admission requires a named writer consumer, strict equality, side-effect bounds for stores/hints, a same-host microbench showing row movement, and JSON/CSS guard measurement. |

## A1 Disposition

The five-row close path can defensibly use inventory demotion for all five
without behavior source changes, but the demotions are not identical:

- `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, and
  `bulk_emit_positions_64` have scalar references and checkasm coverage; their
  live aarch64 bodies are scalar delegates rather than admitted kernels.
- `byte_context` and `cache_hints` are true support-only aarch64 modules:
  neither has a production consumer, a scalar reference, or dedicated checkasm.

W4 should therefore not spend redress budget "admitting" any of these by
documentation alone. Either select one as the primary ASM-gen candidate with
fresh micro-proof, scalar/parity, and same-wave consumer, or inventory-demote
the non-selected rows explicitly in `orphan-disposition.md`.
