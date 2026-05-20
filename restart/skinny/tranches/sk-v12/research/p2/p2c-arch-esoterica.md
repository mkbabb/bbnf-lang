# SK-V12 P2-C: Host-Arch ASM/SIMD Esoterica

Pass: S-P2 Research. Cycle: V3.
Date: 2026-05-20.
Scope: aarch64/Apple Silicon only; inventory ARMv9.2-A/NEON/ASM candidates against the pin-era S-P1 hot leaves. x86 is out of scope.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; container_dispatch; unicode_escape_hex_decode; number_digit_span; simd_movemask; string_escape_decode; output_digest_hash; ascii_whitespace_skip; typed_direct_projection; serde_json_oracle_read_parse.
Lock surface: both - Lock 1 for transient masks versus retained substrate, Lock 14 for grammar-neutral generic crates; Lock 16 is the SIMD/ASM admission gate.

## §1 - Findings (concrete; file:line on bbnf claims, citation on external claims)

1. The live P1 authority for P2-C is the pin-aware SK-V12 S-P1 convergence file, not pre-pin prose. It records the Apple Silicon host/toolchain and native build flags (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:27-48`), names the ten accepted hot-family antecedents (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60-64`), and explicitly says JSON-only profile telemetry can nominate S-P2 families but does not prove CSS L4, Sheets, or BBNF-self behavior (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:73-76`).

2. The top self-time families are split by Track 1/Track 2 so oracle work is not reused as generated parser evidence: parse Track 1 names bounded_plain_string_scan, container_dispatch, number_digit_span, simd_movemask, and unicode_escape_hex_decode; direct Track 1 is output_digest_hash; direct Track 2 carries string_escape_decode; typed Track 1 carries typed_direct_projection (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:32-45`). CSS L4 remains absent from the hot-leaf ledger because no generated CSS runtime or lightningcss same-plane comparator exists yet (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:47-50`).

3. USER PIN D3 and D4 reopen categories, not historical implementations. D3 rescinds the category-level union/substrate preblocks while preserving REDRESS 96/97/98 as measured-rejected variants and requiring a prior-REDRESS citation, material differential, scalar/parity/checkasm, same-wave consumer, and CHALLENGE pass for any new implementation (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39-56`). D4 does the same for REDRESS 88 PMULL prefix-XOR, REDRESS 89 CSSC CTZ bulk consumer, and REDRESS 90 canary hardening-as-row-movement (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:58-69`).

4. USER PIN D5 turns the aarch64 orphan set into a close criterion. The five carried orphans are `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, `bulk_emit_positions_64`, `byte_context`, and `cache_hints`; each is wave-eligible only if a same-commit consumer wires it, and the campaign target is zero orphan kernels by SK-V12 close (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:71-78`). The handoff repeats the same zero-orphan ADMIT/FIXPOINT condition (`restart/skinny/tranches/sk-v12/HANDOFF.md:77-96`).

5. The `escape_mask_64` correctness bug is a hard prerequisite before any new SIMD admission. The pin requires verification and resolution before new SIMD admission (`restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:97-106`), the handoff makes W2 the correctness gate and says SIMD remains blocked on failure (`restart/skinny/tranches/sk-v12/HANDOFF.md:125-140`), and CHECKASM records the falsifier: xorshift seed `0xCAFEF00DBAADF00D`, iter 0, 128-byte JSON-pool buffer, with a state-handoff mismatch between `escape_mask_64`'s `new_carry` and `scan_json_tail`'s `escaped` argument (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102-126`).

6. Lock 1 allows transient SIMD masks but forbids parallel retained substrates: if structural offsets are retained, the projection is the tape (`restart/locks/LOCKS.md:52`). Lock 14 forbids grammar-name branches and grammar-specific code in generic crates including `bbnf-simd`, `parse-that`, runtime, codegen, and path crates (`restart/locks/LOCKS.md:78`). Lock 16 allows aarch64 TBL, movemask, loads/shifts, UDOT/SDOT, LD4-interleaved classification, SHA3 ternary bitwise, set-membership, and cache hints, but requires scalar parity and corpus parity for every primitive (`restart/locks/LOCKS.md:87-112`).

7. The local aarch64 inventory is mixed: several useful kernels exist, but several are scalar delegates or support-only. `classify_tbl4` builds low-6 tables and uses `vqtbl4q_u8` plus movemask over four stripes (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:7-43`, `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:47-65`). `byte_class_from_eq_set_64_neon` has a real NEON fan-out body (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:31-72`), while `byte_class_from_table_64_neon`, `bitmap_prefix_xor_64_neon`, `bitmap_next_set_bit_neon`, `bulk_emit_positions_64_neon`, and `eob_pad_clamp_neon` delegate to scalar (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/eob_pad_clamp.rs:1-6`).

8. TBL/TBX is the strongest row-relevant Apple Silicon surface because it maps directly to byte-set classification, bounded string scanning, whitespace/layout skipping, and hex-nibble decode. In-tree TBL appears in `classify_tbl4` and `unescape_uxxxx` (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:29-43`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:81-120`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:123-166`). The official Arm Neon Intrinsics Reference maps `vqtbl4q_u8` to A64 `TBL` and `vqtbx4q_u8` to A64 `TBX` [ACLE-TBL-TBX].

9. UDOT/DotProd is present only as a narrow four-digit proof. `parse_4_digits` validates bytes and dispatches to a `target_feature = "dotprod"` body when available; that body emits inline `udot` (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4-49`). The current parser's digit-run scanner is still SWAR/scalar over 8/4/2-byte blocks and scalar tails (`skinny/crates/parse-that-regex/src/number/mod.rs:105-162`, `skinny/crates/parse-that-regex/src/number/mod.rs:164-223`). Arm ACLE maps `vdotq_u32` to A64 `UDOT` and defines the dotprod feature macro [ACLE-UDOT].

10. Wide-shift and movemask support exists, but it is a support row unless consumed by a scanner. The movemask uses `vshrn_n_u16`, `vsri_n_u8`, and `vzip1q_u8` (`skinny/crates/bbnf-simd/src/aarch64/movemask.rs:3-25`); byte-context uses `vextq_u8` for cross-chunk one-byte context (`skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:1-11`). Arm ACLE maps these to `SHRN`, `SRI`, `ZIP1`, and `EXT` families [ACLE-SHIFT-EXT].

11. LD4/interleaves are not implemented locally. `quad_load` is `vld1q_u8_x4`, a contiguous four-vector load, not structured `LD4` deinterleave (`skinny/crates/bbnf-simd/src/aarch64/quad_load.rs:1-6`). The official Arm Neon Intrinsics Reference maps `vld4q_u8` to `LD4 {Vt.16B - Vt4.16B},[Xn]` [ACLE-LD4]. A legal LD4 route needs a scalar deinterleave oracle and a real existing interleaved stream; manufacturing a second stream violates Lock 1.

12. PMULL and CSSC CTZ are category-unblocked but historically blocked as default production routes. REDRESS 88 rejected PMULL as the default `bitmap_prefix_xor_64` hot body after parse regressions despite correctness and asm proof (`skinny/REDRESS.md:2510-2540`). REDRESS 89 rejected the CTZ/bulk consumer after correctness and asm proof because refreshed rows regressed (`skinny/REDRESS.md:2542-2585`). Under D4, new consumers may dispatch only if they are materially different from those defaults.

13. SHA3 `EOR3`/`BCAX`, CSSC extrema, CNT/ADDV popcount/reduction, PRFM/STNP cache hints, and BF16 matrix instructions are inventory until a P1 hot leaf and same-wave consumer exist. No local aarch64 SHA3 body exists in the module tree (`skinny/crates/bbnf-simd/src/aarch64/mod.rs:1-32`). `cache_hints` contains inline `prfm` and `stnp` (`skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:1-33`) but the coverage audit classifies it as orphan support with no same-wave consumer (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:58-61`).

## §2 - Candidate primitives and inventory disposition

PIN-V1 split: only C1, C3, C4, C5, and C6 are selectable S-P3
candidates from the pin S-P1 evidence. C2 and C7-C12 remain in this
artifact because D4/D5 require the arch inventory and orphan disposition,
but they are inventory/support unless a later CSS-local profile and
same-wave consumer make them measurable.

| Candidate | PIN-V1 disposition | Reason |
|---|---|---|
| C1 `a64_tbl_tbx_byte_class_mask64` | Selectable candidate | P1 antecedents name byte classification, movemask, layout skip, and bounded string scan; scalar references/checkasm exist. |
| C2 `a64_ld4_interleaved_classifier64x4` | Inventory/drop for current S-P2 | No P1 hot leaf proves a real interleaved stream, no scalar deinterleave oracle exists, and manufacturing the stream would violate Lock 1. |
| C3 `a64_udot_digit_run_span` | Selectable candidate | P1 names `number_digit_span`; UDOT helper exists but needs full digit-run scalar/checkasm and caller proof. |
| C4 `a64_wide_string_special_scan64` | Selectable candidate | P1 names bounded string/string escape/movemask leaves; prior string proof means caller movement, not primitive-only speed, is binding. |
| C5 `a64_hex_quartet_decode_x4` | Selectable candidate | P1 names unicode hex/string escape leaves; x4 path exists but needs strict scalar x4 parity and CSS/generated consumer. |
| C6 `a64_ascii_set_run_skip` | Selectable candidate | P1 names whitespace/container/movemask; CSS layout/trivia skipper is a same-wave consumer candidate. |
| C7 `a64_pmull_prefix_xor_narrow_consumer` | Support-only until W2 + named caller | REDRESS 88 material differential and `escape_mask_64` resolution are prerequisites; no default body admission. |
| C8 `a64_cssc_ctz_mask_emit_narrow_consumer` | Support-only until named caller | REDRESS 89 blocks global bulk/next-bit replacement; only local first-bit support inside a measured caller is eligible. |
| C9 `a64_sha3_ternary_mask_fold` | Inventory/drop for current S-P2 | No P1 hot leaf names a concrete hot three-input formula and no local SHA3 body exists. |
| C10 `a64_wide_shift_movemask_context_support` | Support inventory | `byte_context` is an orphan; eligible only as same-wave support under C1/C4/C6-style scanners. |
| C11 `a64_prfm_stnp_output_stream_hint` | Inventory/drop for current S-P2 | Cache hints have no parser primitive semantics, no scalar/checkasm route, and no P1 writer hot leaf sufficient for admission. |
| C12 `a64_utf8_ascii_fast_block` | Support inventory | Eligible only under a generated string scanner that requires UTF-8 validation and preserves grammar-owned policy. |

### C1. `a64_tbl_tbx_byte_class_mask64`

- Shape: classify 16-byte or 64-byte windows from a grammar-supplied byte set/table using `TBL` low-6 lookup, equality recheck, and optional `TBX` default-preserve behavior; return transient masks only.
- Scalar-ref status: `byte_class_from_eq_set_64_scalar` and `byte_class_from_table_64_scalar` are executable specs (`skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20-38`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1-10`). `classify_tbl4` and eq-set have NEON bodies; table64 currently delegates to scalar.
- Arch: aarch64 NEON `TBL`/`TBX`; Apple Silicon only.
- P1 antecedent: bounded_plain_string_scan; container_dispatch; simd_movemask; ascii_whitespace_skip.
- Micro-proof need: strict checkasm over offsets, tails, low-6 collisions, high-bit bytes, empty/full sets, CSS/Sheets/BBNF byte sets, and corpus parity. Existing eq-set/table tests cover much of the shape (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:156-216`, `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13-49`) but a real table64 NEON body would need refreshed parity.
- Same-wave consumer: generated byte-set caller such as CSS L4 layout/trivia skip, delimiter dispatch, or bounded string special scan. Standalone classifier lift is proof-only.

### C2. `a64_ld4_interleaved_classifier64x4`

- Shape: if a canonical stream is already naturally four-way interleaved, use `LD4` to deinterleave four channels, classify each channel, and return four transient masks.
- Scalar-ref status: absent today. `quad_load` proves only contiguous `vld1q_u8_x4`, not LD4 deinterleave (`skinny/crates/bbnf-simd/src/aarch64/quad_load.rs:1-6`; smoke test at `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:71-82`).
- Arch: aarch64 structured load `LD4` plus NEON classify; Apple Silicon only.
- P1 antecedent: container_dispatch and simd_movemask are adjacent, but no S-P1 hot leaf proves a real interleaved source stream.
- Micro-proof need: scalar deinterleave+classify oracle, channel-order checks, alignment and tail coverage, and a proof that no sidecar/intermediate retained stream is introduced.
- Same-wave consumer: non-selectable until a generated parser or scanner already reads a canonical interleaved stream in the same loop. If a wave first creates the interleaving just to feed LD4, reject under Lock 1.

### C3. `a64_udot_digit_run_span`

- Shape: validate and accumulate fixed-size ASCII digit groups with UDOT weights, returning consumed count, partial value, and status while sign, exponent, radix, suffix/unit, overflow, and materialization policy remain caller-owned.
- Scalar-ref status: current `parse_4_digits` has a scalar fallback and UDOT body (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4-49`). The larger digit-run oracle is parse-that-regex's scanner and accumulation path (`skinny/crates/parse-that-regex/src/number/mod.rs:31-162`).
- Arch: aarch64 DotProd `UDOT`; feature-gated by dotprod.
- P1 antecedent: number_digit_span; typed_direct_projection.
- Micro-proof need: exhaustive valid/invalid digit groups, non-digit offset sweeps, unaligned loads, tails, long runs, prefix accumulation/truncation, and JSON/CSS/Sheets/BBNF numeric slices. Current `aarch64_primitives` coverage is only a smoke test (`skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:167-184`).
- Same-wave consumer: parse-that Layer-1 digit-run primitive or generated number/literal consumer that replaces a real hot digit loop in the same wave. It must not reopen the JSON numeric slot route without fresh material evidence.

### C4. `a64_wide_string_special_scan64`

- Shape: widen the current 16-byte string-special scanner to a 64-byte four-stripe result for terminator, escape, control, and non-ASCII masks; may use compare fan-out, TBL byte classes, movemask, `EXT` byte context, and first-interesting extraction.
- Scalar-ref status: 16-byte scalar and NEON bodies exist (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30-72`), with parity in `checkasm_parity` (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:617-640`). A 64-byte scalar oracle does not exist.
- Arch: aarch64 NEON compare/TBL/wide-shift/movemask.
- P1 antecedent: bounded_plain_string_scan; string_escape_decode; simd_movemask.
- Micro-proof need: new 64-byte scalar oracle, all special-byte positions, multi-hit priority, all alignments/tails, non-ASCII boundaries, cross-block escape context, and caller microbench. REDRESS 106 proved the old full-string proof was correctness-green but aggregate-slower (`skinny/REDRESS.md:3150-3170`).
- Same-wave consumer: generated string/literal scanner that consumes the 64-byte result immediately and measures row impact. Reusing the previous full-string proof or adding an unused primitive is not admissible.

### C5. `a64_hex_quartet_decode_x4`

- Shape: decode four fixed-width hex quartets with TBL nibble lookup, returning code units plus validity; surrogate joining and escape policy remain grammar/caller-owned.
- Scalar-ref status: x1 scalar oracle exists (`unescape_uxxxx_scalar`), and x1/x4 NEON bodies exist (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:33-166`). x4 parity is smoke-only (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58-68`).
- Arch: aarch64 NEON `TBL`; optional `TBX` fallback if a generated hex policy benefits.
- P1 antecedent: unicode_escape_hex_decode; string_escape_decode.
- Micro-proof need: scalar x4 oracle that invokes x1 semantics lane-by-lane, invalid nibble in every position, mixed valid/invalid quartets, surrogate pair/nonpair policy handoff, alignment, boundary, and tails. Prior proof-only/reuse boundaries remain active (REDRESS 107/108 at `skinny/REDRESS.md:3172-3222`).
- Same-wave consumer: generated escape/segment decoder or parse-that primitive consuming x4 output in the same wave. JSON `\uXXXX` production reuse alone is insufficient; CSS fixed/variable hex policy must stay generated.

### C6. `a64_ascii_set_run_skip`

- Shape: skip a run of grammar-supplied ASCII layout/trivia bytes by classifying blocks and extracting the first nonmember; comments and broader trivia policy remain generated caller logic.
- Scalar-ref status: parse-that-regex currently has JSON-shaped `skip_ascii_whitespace` and `skip_ascii_spaces` (`skinny/crates/parse-that-regex/src/lib.rs:112-147`); generic byte-set scalar refs exist in `bbnf-simd`.
- Arch: aarch64 NEON TBL/TBX or eq-set fan-out plus movemask; optional CTZ only as a local first-set-bit support detail.
- P1 antecedent: ascii_whitespace_skip; container_dispatch; simd_movemask.
- Micro-proof need: generic byte-set-run oracle over empty/full sets, first-nonmember at every offset, high-bit bytes, all alignments/tails, CSS/Sheets/BBNF layouts, and corpus parity.
- Same-wave consumer: generated CSS L4 layout/trivia skipper, Sheets formula whitespace skipper, or BBNF-self layout skipper. A JSON-only whitespace speedup is guard context, not SK-V12 admission.

### C7. `a64_pmull_prefix_xor_narrow_consumer`

- Shape: use PMULL/PMULL2 to compute a prefix-XOR/string-region mask only inside a newly named narrow consumer, not as the default `bitmap_prefix_xor_64` body.
- Scalar-ref status: scalar prefix-XOR is executable and current aarch64 delegates to it (`skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1-14`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`).
- Arch: aarch64 NEON polynomial multiply `PMULL`/`PMULL2`; feature-gated where needed.
- P1 antecedent: simd_movemask; bounded_plain_string_scan; string_escape_decode.
- Micro-proof need: bit-exact scalar parity over all carry-in states, random masks, long backslash runs, `escape_mask_64` boundary cases, and caller-level microbench after W2 correctness is green.
- Same-wave consumer: a specific generated string/escape scanner or CSS/string row that consumes the PMULL result in the same wave. Material differential from REDRESS 88: not the production-default `bitmap_prefix_xor_64` body and not a parse-only JSON default path; the consumer must be narrower, feature-gated, and row-measured.

### C8. `a64_cssc_ctz_mask_emit_narrow_consumer`

- Shape: use CSSC `CTZ` for first-set-bit or next-set-bit extraction inside a local mask-emission loop, without replacing the whole default bulk emit path.
- Scalar-ref status: scalar `bitmap_next_set_bit_scalar` and `bulk_emit_positions_64_scalar` are executable refs; aarch64 delegates to them today (`skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1-13`, `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1-13`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`).
- Arch: aarch64 CSSC `CTZ`; compiler lowering or explicit asm only if equivalent intrinsic/lowering is unavailable and justified.
- P1 antecedent: simd_movemask; container_dispatch; ascii_whitespace_skip.
- Micro-proof need: existing boundary/random checkasm is a start (`skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:5-29`, `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs:29-60`); a new CTZ route needs asm visibility, all cursor states 0..64, zero/full masks, guard canary, and caller microbench.
- Same-wave consumer: one generated scanner that consumes emitted positions or first-set-bit immediately. Material differential from REDRESS 89: no global default replacement of `bitmap_next_set_bit`/`bulk_emit_positions_64`, no canary-as-row-movement claim, and no retained cursor/side table.

### C9. `a64_sha3_ternary_mask_fold`

- Shape: replace a proven hot three-input byte-mask boolean expression with `EOR3` or `BCAX`; examples are quote/escape/control fusion or caller-owned mask blending, but only if the exact formula is named.
- Scalar-ref status: absent today. Scalar reference would be an explicit boolean formula over three byte vectors or packed masks; no local aarch64 SHA3 module exists.
- Arch: aarch64 SHA3 extension `EOR3`/`BCAX`.
- P1 antecedent: none sufficient today. bounded/string/movemask leaves are adjacent but do not name a hot three-input formula.
- Micro-proof need: feature gate, exhaustive small-domain formula proof, randomized vector parity, and caller-level parity. No corpus/bench claim without a real consumer.
- Same-wave consumer: non-selectable until a generated scanner/source line names the exact three-input expression and consumes it in the same wave.

### C10. `a64_wide_shift_movemask_context_support`

- Shape: support primitive for cross-chunk context and mask packing: `EXT` for previous/current/next byte context, `SHRN`/`SRI`/`ZIP1` for movemask, `CNT`/`ADDV` for reductions.
- Scalar-ref status: no standalone scalar ref because this is an implementation detail; any consuming candidate must define its own scalar oracle. Existing `byte_context` has no dedicated scalar/checkasm and is one of the five orphans (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:53-56`).
- Arch: aarch64 NEON wide shifts/reductions.
- P1 antecedent: simd_movemask; bounded_plain_string_scan; ascii_whitespace_skip.
- Micro-proof need: boundary parity over adjacent chunks, all alignments, first/last byte handoffs, and consumer-specific scalar oracle. Cannot ship as standalone support.
- Same-wave consumer: C1/C4/C6-style scanner. Zero-orphan rule requires either same-wave consumption or inventory demotion/removal.

### C11. `a64_prfm_stnp_output_stream_hint`

- Shape: prefetch or non-temporal pair-store hints for output-plane writes, digest/oracle buffers, or tape-like streams.
- Scalar-ref status: no-op/no-hint path must be the reference; `cache_hints` has asm bodies but no scalar/checkasm or consumer (`skinny/crates/bbnf-simd/src/aarch64/cache_hints.rs:1-33`).
- Arch: aarch64 `PRFM` and `STNP` asm; Apple Silicon only.
- P1 antecedent: output_digest_hash, but P1 marks this as generated Track 1 direct output work, not a parser primitive.
- Micro-proof need: identical-output fallback, cache-counter or same-host microbench if available, and no JSON-only digest proof. Previous SK-V11 hardening demoted cache hints to inventory-only absent fresh behavior evidence.
- Same-wave consumer: output-plane writer or generated CSS fact-stream sink, not a parser scanner. If no consumer is wired, demote/remove to satisfy zero-orphan close.

### C12. `a64_utf8_ascii_fast_block`

- Shape: classify ASCII/non-ASCII over a 16-byte block and fall back to scalar UTF-8 validation only when needed.
- Scalar-ref status: `validate_block_scalar` exists; NEON body only fast-paths all-ASCII then stores and calls scalar for non-ASCII (`skinny/crates/bbnf-simd/src/aarch64/utf8/validate_block.rs:76-112`).
- Arch: aarch64 NEON compare plus movemask.
- P1 antecedent: bounded_plain_string_scan; string_escape_decode.
- Micro-proof need: existing UTF-8 block parity covers representative cases (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:11-56`); production expansion would need full boundary/corpus proof and caller microbench.
- Same-wave consumer: generated string scanner that requires UTF-8 validation. It must not shift UTF-8 policy out of generated grammar/view boundaries.

## §3 - Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

- C1 `a64_tbl_tbx_byte_class_mask64`: grammar-neutral when byte sets/tables are generated from grammar metadata. CSS delimiters/layout bytes, Sheets operators/separators, and BBNF metasyntax fit. JSON structural alphabets may not be hardcoded into `bbnf-simd`.
- C2 `a64_ld4_interleaved_classifier64x4`: grammar-neutral only if a canonical interleaved stream already exists independent of the optimization. Creating an interleaved side stream is not neutral and violates Lock 1.
- C3 `a64_udot_digit_run_span`: grammar-neutral for ASCII decimal digit runs; JSON number policy, CSS units, Sheets exponent forms, and BBNF numeric syntax remain generated caller policy.
- C4 `a64_wide_string_special_scan64`: grammar-neutral if terminator, escape byte, control limit, and UTF-8 mode are parameters. It becomes JSON-only if fixed to quote/backslash/control<0x20.
- C5 `a64_hex_quartet_decode_x4`: neutral only as fixed-width hex decode. JSON `\uXXXX` surrogate rules, CSS variable-width escapes, Sheets doubled quotes, and BBNF literal/regex policy stay outside the primitive.
- C6 `a64_ascii_set_run_skip`: grammar-neutral for grammar-supplied byte sets. CSS comments/layout and BBNF comments require generated policy around the byte-set primitive.
- C7 `a64_pmull_prefix_xor_narrow_consumer`: neutral only as a bitmask prefix operation over caller-supplied masks. It must not encode JSON string/escape semantics.
- C8 `a64_cssc_ctz_mask_emit_narrow_consumer`: neutral as first/next-set-bit extraction over transient masks. It is not neutral if it persists a structural-position vector or cursor list.
- C9 `a64_sha3_ternary_mask_fold`: neutral as boolean algebra over caller masks, but currently ineligible because no hot three-input formula is named.
- C10 `a64_wide_shift_movemask_context_support`: neutral only as implementation detail under a neutral scanner. Support-only modules must be consumed or demoted for zero-orphan close.
- C11 `a64_prfm_stnp_output_stream_hint`: not a parser grammar primitive. It can be grammar-neutral only as an optional host/output sink hint with no semantic visibility.
- C12 `a64_utf8_ascii_fast_block`: neutral if validation mode is caller-selected. It cannot move UTF-8 acceptance/rejection policy into a generic scanner.

## §4 - Risks (REDRESS entries any candidate must NOT re-open)

1. Do not admit any new SIMD/ASM route until W2 verifies and resolves `escape_mask_64`. The bug is correctness, not performance; a throughput microbench cannot waive it (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102-126`, `restart/skinny/tranches/sk-v12/HANDOFF.md:125-140`).

2. REDRESS 88/89/90 are category-unblocked but historically binding. The material differentials required by USER PIN D4 are:

   | Historical route | Blocked implementation | Required differential for any new C7/C8 route |
   |---|---|---|
   | REDRESS 88 PMULL | default PMULL body for `bitmap_prefix_xor_64`, production parse rows regressed (`skinny/REDRESS.md:2510-2540`) | narrow consumer, not default body; scalar parity + `escape_mask_64` boundary proof; feature-gated fallback; same-wave generated CSS/non-JSON or guard-row consumer with measured non-regression |
   | REDRESS 89 CSSC CTZ | global next-bit/bulk-emitter replacement, rows regressed (`skinny/REDRESS.md:2542-2585`) | local first/next-bit support inside one consumer; no retained cursor/side table; no global dispatch rewrite; same-wave row measurement |
   | REDRESS 90 canary | canary hardening was admitted only as integrity hardening, not bitmap body admission (`skinny/REDRESS.md:2587-2604`) | canary remains checkasm integrity only; it cannot be claimed as row movement or primitive admission |

3. REDRESS 96/97/98 are category-unblocked by D3 but still define the material differentials for any Lock 1-adjacent arch route:

   | Historical route | Blocked implementation | Required differential for any new route |
   |---|---|---|
   | REDRESS 96 | co-indexed class column plus move-consumed structural index; every must-improve row failed (`skinny/REDRESS.md:2795-2848`) | no retained class column; transient masks consumed in the same loop; one tape remains the substrate |
   | REDRESS 97 | allocation-free streaming structural cursor; every must-improve row failed (`skinny/REDRESS.md:2850-2906`) | no parser-owned cursor/list; no second scan stream; same-wave consumer must be source-visible and measured |
   | REDRESS 98 | class-lane-only proof retired as paper-close (`skinny/REDRESS.md:2910-2950`) | no source-free proof-only route; producer and consumer must both land in the same wave |

4. Do not repackage string/escape proof-only work as production. REDRESS 106 rejected the full string primitive micro-proof after a 0.774x aggregate caller result (`skinny/REDRESS.md:3150-3170`); REDRESS 107 admitted x4 escape decode only as proof, and REDRESS 108 rejected production reuse without a new source delta (`skinny/REDRESS.md:3172-3222`).

5. Do not use JSON direct residuals as the SK-V12 close target. REDRESS 119/120 close direct residuals as guard/routed evidence, while the pin close target is generated CSS L4 strictly greater than lightningcss on the same plane (`skinny/REDRESS.md:3495-3553`, `restart/skinny/tranches/sk-v12/SYNTHESIS.md:35-70`).

6. Do not leave production aarch64 support orphans. C7/C8/C10/C11 are exactly the danger zone: if they are not consumed in a same commit, they must be explicitly inventory-demoted or removed before close (`restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:34-61`).

7. Do not use LD4, PRFM/STNP, SHA3 ternary, CSSC extrema, BF16, or other ARMv9.2-A esoterica as paper candidates. Without a P1 antecedent, scalar reference, micro-proof, and same-wave consumer, they are inventory only.

## §5 - Sources (every external citation - comparator source, ISA manual, prior tranche)

- `restart/prompts/skinny/PASS-2-RESEARCH.md:48-85`, `:95-123`, `:237-251`: S-P2/P2-C contract, output schema, and candidate admissibility gates.
- `restart/prompts/ORCHESTRATOR.md:74-122`: CH1-CH6 challenge lenses and §3Z convergence.
- `restart/skinny/tranches/sk-v12/USER-PIN-W1-CSS-L4-SOTA.md:39-120`: D3/D4 category unblocks, D5 zero-orphan, x86 out of scope, Lock 16/escape-mask prerequisite, and REDRESS disposition table.
- `restart/skinny/tranches/sk-v12/HANDOFF.md:71-180`: close criteria, zero-orphan surface, W2/W4 seed split, telemetry binding, and refusal conditions.
- `restart/skinny/tranches/sk-v12/SYNTHESIS.md:35-95`, `:170-226`, `:260-276`: ADMIT/FIXPOINT requirements, union/ASM-gen material differential, telemetry binding, and W4 ARMv9.2 route.
- `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:27-84`: pin S-P1 authority and hot-family antecedents.
- `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:1-120`: capture root, host/toolchain, replay surface, and hot-leaf table authority.
- `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md`, `p1b-samply-mode-2.md`, `p1c-samply-mode-3.md`, `p1d-pmu-cycles.md`, `p1e-hot-leaf-attribution.md`, `p1f-results-delta.md`: six P1 artifacts consumed for hot-leaf, PMU, Mode III absence, and RESULTS-delta boundaries.
- `restart/skinny/tranches/sk-v12/research/skv12-aarch64-simd-coverage-audit.md:10-199`: active aarch64 primitive table, orphan list, Lock 16 compliance check, and recommendation matrix.
- `skinny/RESULTS.md:5-45`, `:143-146`: live JSON result rows and unchanged `N-direct / NoGo` outcome.
- `skinny/REDRESS.md:2508-2604`: REDRESS 88/89/90 PMULL, CSSC CTZ, and canary hardening history.
- `skinny/REDRESS.md:2795-2950`: REDRESS 96/97/98 union substrate measured failures and retirement.
- `skinny/REDRESS.md:3150-3222`: REDRESS 106/107/108 string and escape proof boundaries.
- `skinny/REDRESS.md:3282-3355`, `:3495-3553`: non-JSON baseline blocker, direct residual fixpoint, and SK-V11 close routing.
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:50-126`: deterministic checkasm harness and `escape_mask_64` falsifier.
- `skinny/crates/bbnf-simd/src/aarch64/*.rs`, `skinny/crates/bbnf-simd/src/scalar/*.rs`, and `skinny/crates/bbnf-simd/tests/checkasm_*.rs`: local aarch64/scalar/test inventory cited inline above.
- `skinny/crates/parse-that-regex/src/lib.rs:112-147`, `:302-459`, `:461-585`; `skinny/crates/parse-that-regex/src/number/mod.rs:31-223`: current whitespace, string, Unicode escape, and number scalar surfaces.
- [ACLE-TBL-TBX] Arm Neon Intrinsics Reference, AdvSIMD table lookup section: `vqtbl4q_u8` maps to A64 `TBL`; `vqtbx4q_u8` maps to A64 `TBX`. https://arm-software.github.io/acle/neon_intrinsics/advsimd.html
- [ACLE-UDOT] Arm Neon Intrinsics Reference, AdvSIMD dot-product section: `vdotq_u32` maps to A64 `UDOT`; Arm C Language Extensions defines `__ARM_FEATURE_DOTPROD`. https://arm-software.github.io/acle/neon_intrinsics/advsimd.html and https://arm-software.github.io/acle/main/acle.html
- [ACLE-SHIFT-EXT] Arm Neon Intrinsics Reference for shift/extract/reduction sections: `vextq_u8`, `vshrn_n_u16`, `vsriq_n_u8`, `vaddvq_u8`, and `vcntq_u8`. https://arm-software.github.io/acle/neon_intrinsics/advsimd.html
- [ACLE-LD4] Arm Neon Intrinsics Reference, structured-load section: `vld4q_u8` maps to `LD4 {Vt.16B - Vt4.16B},[Xn]`. https://arm-software.github.io/acle/neon_intrinsics/advsimd.html
- [ACLE-PMULL-CSSC-SHA3] Arm Neon Intrinsics Reference and Arm C Language Extensions: polynomial multiply entries `vmull_p64`/`vmull_high_p64`, CSSC feature macro `__ARM_FEATURE_CSSC`, SHA3 feature macro `__ARM_FEATURE_SHA3`, and SHA3 ternary bitwise intrinsics `EOR3`/`BCAX`. https://arm-software.github.io/acle/neon_intrinsics/advsimd.html and https://arm-software.github.io/acle/main/acle.html
