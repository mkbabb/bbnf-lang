# SK-V12 P2-C: Host-Arch ASM/SIMD Esoterica

Pass: S-P2 Research. Cycle: V12.
Date: 2026-05-20.
Scope: AArch64/Apple Silicon instruction inventory for SK-V12 accepted P1 hot leaves; x86 is context only.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; container_dispatch; unicode_escape_hex_decode; number_digit_span; simd_movemask; string_escape_decode; output_digest_hash; ascii_whitespace_skip; typed_direct_projection; serde_json_oracle_read_parse.
Lock surface: Lock 1 + Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. S-P2 is a read-only research pass against source, and this artifact must name candidates without selecting waves or landing code. PASS-2 defines P2-C as the host-arch/ASM/SIMD lane and requires candidate shape, scalar-ref status, grammar-neutrality, risks, and primary sources (`restart/prompts/skinny/PASS-2-RESEARCH.md:15`, `restart/prompts/skinny/PASS-2-RESEARCH.md:21`, `restart/prompts/skinny/PASS-2-RESEARCH.md:55`).

2. The live SK-V12 authority is still `N-direct / NoGo`: direct has only JSON product rows and typed has accepted rows, but generated non-JSON direct/typed baseline remains first priority before behavior waves (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`, `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:44`, `restart/skinny/tranches/sk-v12/HANDOFF.md:55`). JSON-only telemetry may nominate primitive families but does not prove CSS/Sheets/BBNF-self generality (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:60`).

3. Accepted P1 antecedents for P2-C are the ten hot families listed in the converged P1 file; the same list also states that samply artifacts are retained as artifact-only while xctrace XML self-time is authority (`restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:47`, `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:52`). P1E maps those families to local source anchors, including string scan, escape decode, unicode hex decode, number span, whitespace skip, container dispatch, movemask, and digest hashing (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:205`).

4. Lock 1 allows transient SIMD mask streams but forbids retained parallel substrates/sidecars; Lock 14 requires grammar-general generic crates; Lock 16 already admits generic aarch64 ideas for UDOT/SDOT, LD4+TBL classifier shape, and SHA3 ternary bitwise instructions, with scalar parity/corpus parity required for SIMD primitives (`restart/locks/LOCKS.md:52`, `restart/locks/LOCKS.md:78`, `restart/locks/LOCKS.md:92`, `restart/locks/LOCKS.md:112`).

5. Local aarch64 code already contains TBL/TBX-adjacent classifier material and TBL string/hex proofs: `classify_tbl4` builds a low6 table and uses `vqtbl4q_u8` for chunk/block classification (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:7`, `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:22`, `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:47`); `match_tiny_plain_string` uses a TBL probe but active tiny-string routes remain REDRESS-bound (`skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:63`, `skinny/REDRESS.md:324`, `skinny/REDRESS.md:1973`); `unescape_uxxxx` has scalar, x1, and x4 NEON TBL decode bodies (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125`).

6. Local movemask and wide-shift support is real but support-level: the shared movemask uses `vshrn_n_u16`, `vsri_n_u8`, `vzip1q_u8`, and scalar pack (`skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`); string boundary context uses `vextq_u8` (`skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:3`); `string_block` currently has a 16-byte scalar reference and a NEON compare fan-out (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`, `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:57`).

7. Local UDOT/DotProd support exists only as a narrow four-digit primitive: `parse_4_digits` falls back to scalar unless `dotprod` is enabled, and the inline-asm body uses `udot` (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5`, `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:25`). ACLE defines `__ARM_FEATURE_DOTPROD` for dot-product data-manipulation instructions and maps `vdotq_u32` to A64 `UDOT` (E2, E3).

8. Local bitmap/mask primitives are scalar delegates on aarch64 today: table classification, next set bit, prefix xor, and bulk emit all call scalar bodies (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_next_set_bit.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`). Existing checkasm tests already define scalar parity expectations for these shapes (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13`, `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:5`, `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs:5`, `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs:5`).

9. PMULL and CSSC CTZ are inventory items, not open default routes. REDRESS 88 rejects PMULL as the default `bitmap_prefix_xor_64` hot body after row regression; REDRESS 89 rejects CSSC CTZ bulk consumer/canary fold despite correctness and asm proof (`skinny/REDRESS.md:2510`, `skinny/REDRESS.md:2542`). ACLE maps `vmull_p64` to `PMULL`, and ACLE defines `__ARM_FEATURE_CSSC` as including CTZ (E4, E2).

10. Structured LD4 is not currently implemented in local aarch64 modules. `quad_load` uses `vld1q_u8_x4`, which loads four contiguous vectors, not interleaved `LD4` (`skinny/crates/bbnf-simd/src/aarch64/quad_load.rs:3`). ACLE maps `vld4q_u8` to `LD4 {Vt.16B - Vt4.16B},[Xn]`, so any LD4 path needs a fresh scalar deinterleave oracle and same-pass consumer proof (E5).

11. SHA3 ternary bitwise instructions are inventory/support only until a P1 hot leaf exposes a real three-input boolean fold. ACLE gates SHA3 intrinsics with `__ARM_FEATURE_SHA3` and maps `veor3q_u8` to `EOR3`; `vbcaxq_u8` maps to `BCAX` (E2, E6). No local aarch64 SHA3/EOR3/BCAX primitive body exists in the audited modules (`skinny/crates/bbnf-simd/src/aarch64/mod.rs:1`).

12. Secondary x86 is context only for SK-V12. The handoff explicitly refuses using x86 as the implementation target or as an excuse to defer Apple Silicon/aarch64 work (`restart/skinny/tranches/sk-v12/HANDOFF.md:116`, `restart/skinny/tranches/sk-v12/HANDOFF.md:128`).

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Candidate count: 8. These are candidate primitives only; no wave is selected here.

### C1. `a64_tbl_byte_class_mask64`

- Shape: grammar-supplied byte-set or 64-entry low6 table over a 64-byte block, using A64 TBL/TBX-family lookup plus equality recheck where needed, returning transient masks for caller-local scan/dispatch.
- Scalar-ref status: current scalar refs exist for eq-set and table classification (`byte_class_from_eq_set_64_scalar`, `byte_class_from_table_64_scalar`); current aarch64 eq-set has a body, table path is still a scalar delegate (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:26`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1`).
- Checkasm expectation: extend existing checkasm density/alignment/corpus coverage to prove table-body parity, high-bit behavior, nonmember preservation, and no retained sidecar (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:156`, `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13`).
- Arch/instruction: AArch64 TBL/TBX (`vqtbl4q_u8`, optionally `vqtbx4q_u8`) per ACLE (E1).
- P1 antecedent: bounded_plain_string_scan; ascii_whitespace_skip; container_dispatch; simd_movemask.

### C2. `a64_ld4_interleaved_classifier64x4`

- Shape: for a proven existing interleaved byte stream, use `vld4q_u8`/LD4 to deinterleave four channels, classify each channel with TBL/table/equality logic, and return four transient channel masks.
- Scalar-ref status: no local LD4-specific scalar oracle exists; `quad_load` covers `vld1q_u8_x4`, not LD4 (`skinny/crates/bbnf-simd/src/aarch64/quad_load.rs:3`, `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:71`).
- Checkasm expectation: add scalar `ld4_deinterleave_classify` oracle with alignment/tail/channel-order tests, then compare four masks against the scalar byte-table/equality refs. Must also prove the consumer reads one canonical stream and does not create sidecar storage.
- Arch/instruction: AArch64 structured load LD4 (`vld4q_u8`) plus TBL/TBX as needed (E5, E1).
- P1 antecedent: container_dispatch; ascii_whitespace_skip; bounded_plain_string_scan; simd_movemask.

### C3. `a64_udot_digit_span`

- Shape: decode/validate fixed-size digit groups inside longer numeric spans using byte subtract/validation plus UDOT weighted accumulation; return consumed count and partial value/status, leaving overflow and grammar policy scalar/caller-owned.
- Scalar-ref status: local `parse_4_digits` has scalar fallback and UDOT body; number-span scalar anchors live in parse-that-regex (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5`, `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:25`, `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:205`).
- Checkasm expectation: expand beyond current primitive tests to cover all non-digit offsets, mixed valid/invalid groups, tails, overflow boundaries, signed/unsigned policy separation, and canary/register guard parity (`skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:168`, `skinny/crates/bbnf-simd/tests/checkasm_common.rs:50`).
- Arch/instruction: AArch64 DotProd UDOT, gated by `__ARM_FEATURE_DOTPROD` (E2, E3).
- P1 antecedent: number_digit_span; typed_direct_projection.

### C4. `a64_wide_string_special_scan64`

- Shape: widen the 16-byte string-special block into a 64-byte transient scan returning masks or first offset for quote, backslash, control byte, and non-ASCII; use compare fan-out, TBL byte-class support, movemask, and wide-shift boundary handling.
- Scalar-ref status: current scalar and NEON string block are 16-byte only (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`, `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:57`).
- Checkasm expectation: add 64-byte scalar oracle; exhaustively cover every special byte position, multi-hit priority, tails, misalignment, cross-block escape context, and corpus parity. Caller benchmark must report c/B and row Mbps deltas because REDRESS 61 rejected prior long-string production without row movement (`skinny/REDRESS.md:1382`).
- Arch/instruction: AArch64 NEON compare, TBL, CNT/ADDV or movemask, EXT/SHRN/SRI/ZIP1 wide-shift support (E1, E7, E8, E9).
- P1 antecedent: bounded_plain_string_scan; string_escape_decode; simd_movemask.

### C5. `a64_hex_quartet_decode_x4`

- Shape: decode four `\uXXXX`/hex quartets with TBL nibble lookup, return four code units plus validity mask; surrogate joining and grammar-specific escape policy remain scalar/caller-owned.
- Scalar-ref status: local scalar `unescape_uxxxx_scalar` and x1/x4 NEON bodies exist, but the current x4 test is smoke-level, not a full scalar-ref checkasm (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:125`, `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:59`).
- Checkasm expectation: add x4 scalar oracle covering invalid hex in each nibble, mixed valid/invalid quartets, surrogate pairs/nonpairs, alignment, and tails; require same-wave source delta before production use because prior proof-only/reuse routes were rejected (`skinny/REDRESS.md:2287`, `skinny/REDRESS.md:3174`, `skinny/REDRESS.md:3436`).
- Arch/instruction: AArch64 TBL/TBX (E1).
- P1 antecedent: unicode_escape_hex_decode; string_escape_decode.

### C6. `a64_ascii_set_run_skip`

- Shape: skip runs of grammar-supplied ASCII layout/trivia bytes using 64-byte table/equality classification plus first-nonmember mask extraction; comments and grammar policy remain caller-owned.
- Scalar-ref status: current whitespace skip anchors are scalar parse-that-regex functions, and generic 64-byte table/eq-set scalar refs exist (`restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:205`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs:20`, `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs:1`).
- Checkasm expectation: add generic byte-set run scalar oracle covering empty/full sets, high-bit bytes, all first-nonmember offsets, tails, and corpus parity across JSON/CSS/Sheets/BBNF-self fixtures. CTZ may be an implementation detail only if it does not reopen REDRESS 89.
- Arch/instruction: AArch64 TBL/TBX, movemask, optional CSSC CTZ only as narrow scalar-equivalent support (E1, E2, E9).
- P1 antecedent: ascii_whitespace_skip; container_dispatch; simd_movemask.

### C7. `a64_mask_emit_next_support`

- Shape: support primitive for transient masks: next-set-bit and bulk position emission inside the same scan loop, with optional CSSC CTZ for next-bit extraction. It must not be a standalone retained structural cursor, side table, or default prefix-xor rewrite.
- Scalar-ref status: scalar refs and checkasm already exist for next set bit, bulk emit positions, and prefix xor; current aarch64 bodies delegate to scalar (`skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs:1`, `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs:1`, `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs:1`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1`).
- Checkasm expectation: preserve existing boundary/random parity; add asm expectation only for a narrow consumer-owned CTZ path. PMULL prefix-xor default route is blocked and must remain outside this candidate (`skinny/REDRESS.md:2510`, `skinny/REDRESS.md:2542`).
- Arch/instruction: AArch64 CSSC CTZ support where available; PMULL inventory is REDRESS-blocked for default prefix xor (E2, E4).
- P1 antecedent: simd_movemask; container_dispatch.

### C8. `a64_sha3_ternary_bool_fold`

- Shape: support primitive for three-input boolean folds over byte masks, using EOR3/BCAX when a measured hot leaf actually combines three masks in the same expression; examples include quote/escape/control fusion or digest-plane bit mixing, but only after scalar formula and consumer are named.
- Scalar-ref status: no local aarch64 SHA3/EOR3/BCAX body exists; scalar ref would be an explicit boolean formula over three `uint8x16_t`-sized byte lanes or packed masks (`skinny/crates/bbnf-simd/src/aarch64/mod.rs:1`).
- Checkasm expectation: exhaustive small-domain formula tests plus randomized vector tests, feature-gated with `__ARM_FEATURE_SHA3`, and corpus parity only through a same-wave consumer. No standalone telemetry-only admission.
- Arch/instruction: AArch64 SHA3 ternary logical EOR3/BCAX (E2, E6).
- P1 antecedent: simd_movemask; container_dispatch; output_digest_hash; string_escape_decode.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

- C1 `a64_tbl_byte_class_mask64`: grammar-neutral if the lookup table or byte set is generated from grammar byte classes and the mask is transient. Generalises to CSS layout bytes, Sheets separators/operators, and BBNF-self punctuation. JSON-only tiny-string dispatch wiring remains blocked.
- C2 `a64_ld4_interleaved_classifier64x4`: grammar-neutral only if a real canonical byte stream is already interleaved in memory or a same-loop consumer naturally touches four channels. It is not neutral if it creates a second scan stream or retained deinterleaved sidecar.
- C3 `a64_udot_digit_span`: grammar-neutral for decimal digit-run decode where the grammar's numeric token accepts ASCII digits. JSON number policy, CSS numeric units, Sheets numeric literals, and BBNF-self numeric tokens can share the primitive while keeping token policy scalar.
- C4 `a64_wide_string_special_scan64`: grammar-neutral for quoted/plain-string front ends that need quote/backslash/control/non-ASCII boundaries. Candidate must expose only byte-class masks or first offsets, not JSON-specific escape state.
- C5 `a64_hex_quartet_decode_x4`: grammar-neutral for any grammar with fixed-width ASCII hex escape/quartet decode. JSON `\uXXXX` semantics, CSS escapes, or other escape policies must remain caller-owned.
- C6 `a64_ascii_set_run_skip`: grammar-neutral when the skipped set is generated from grammar layout/trivia bytes. It must not assume JSON whitespace only.
- C7 `a64_mask_emit_next_support`: grammar-neutral only as an internal support primitive over a transient mask produced by the active scanner. It must not persist positions or introduce a structural sidecar.
- C8 `a64_sha3_ternary_bool_fold`: grammar-neutral as a boolean algebra helper over caller-supplied masks. It is not admissible as a grammar-specific hash/digest shortcut.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- REDRESS 28/33/72: TBL tiny-string and cap16 work proved correctness but active tiny-string dispatch/regression boundaries remain; C1/C4/C5 must not reuse tiny-string wiring as a shortcut (`skinny/REDRESS.md:324`, `skinny/REDRESS.md:1973`).
- REDRESS 50/51/53: side tables, byte-class whitespace/event cursors, and parser-local structural-mask cursors are rejected; C1/C2/C6/C7 must keep masks transient and same-loop (`skinny/REDRESS.md:715`, `skinny/REDRESS.md:742`, `skinny/REDRESS.md:784`).
- REDRESS 61/106/107/108: string/escape microproofs without row movement or real source delta are insufficient; C4/C5 need scalar parity, source delta, and row-level evidence before any production admission (`skinny/REDRESS.md:1382`, `skinny/REDRESS.md:3152`, `skinny/REDRESS.md:3174`, `skinny/REDRESS.md:3200`).
- REDRESS 64/82/83/84: retained unicode/string/object-pair metadata routes are rejected; C4/C5 must not retain run validators, StringBlock16 probes, or object-pair compaction (`skinny/REDRESS.md:1584`, `skinny/REDRESS.md:2287`, `skinny/REDRESS.md:2320`, `skinny/REDRESS.md:2360`).
- REDRESS 88/89/90: PMULL default prefix-xor and CSSC CTZ bulk consumer/canary fold are rejected; C7 can only use CTZ as a narrow scalar-equivalent support detail, and PMULL remains REDRESS-blocked unless a future distinct narrow consumer proves itself (`skinny/REDRESS.md:2510`, `skinny/REDRESS.md:2542`, `skinny/REDRESS.md:2589`).
- REDRESS 111/112/119/120: SK-V12 still lacks a generated non-JSON baseline and direct residual JSON rows are exhausted; every candidate needs a CSS/Sheets/BBNF-self generalizable consumer path, not another JSON-only proof (`skinny/REDRESS.md:3284`, `skinny/REDRESS.md:3313`, `skinny/REDRESS.md:3497`, `skinny/REDRESS.md:3531`).
- Handoff refusal surface: do not use source edits in S-P2, W3/parse-only/JSON residual reopen, telemetry-only primitive proofs, generic-crate JSON policy, or x86 implementation target (`restart/skinny/tranches/sk-v12/HANDOFF.md:116`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

Local tranche and repo sources:

- PASS-2 contract: `restart/prompts/skinny/PASS-2-RESEARCH.md:15`, `restart/prompts/skinny/PASS-2-RESEARCH.md:21`, `restart/prompts/skinny/PASS-2-RESEARCH.md:55`.
- SK-V12 handoff: `restart/skinny/tranches/sk-v12/HANDOFF.md:46`, `restart/skinny/tranches/sk-v12/HANDOFF.md:55`, `restart/skinny/tranches/sk-v12/HANDOFF.md:116`, `restart/skinny/tranches/sk-v12/HANDOFF.md:133`.
- SK-V12 P1 artifacts read-set: `restart/skinny/tranches/sk-v12/research/p1/p1a-samply-mode-1.md:112`, `restart/skinny/tranches/sk-v12/research/p1/p1b-samply-mode-2.md:143`, `restart/skinny/tranches/sk-v12/research/p1/p1c-samply-mode-3.md:214`, `restart/skinny/tranches/sk-v12/research/p1/p1d-pmu-cycles.md:90`, `restart/skinny/tranches/sk-v12/research/p1/p1e-hot-leaf-attribution.md:205`, `restart/skinny/tranches/sk-v12/research/p1/p1f-results-delta.md:77`, `restart/skinny/tranches/sk-v12/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41`.
- Capture/replay authority: `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:11`, `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-capture-manifest.md:47`, `restart/skinny/tranches/sk-v12/research/p1/skv12-p1-replay.tsv:1`.
- Current result/redress/lock authority: `skinny/RESULTS.md:1`, `skinny/REDRESS.md:324`, `skinny/REDRESS.md:715`, `skinny/REDRESS.md:1382`, `skinny/REDRESS.md:2510`, `skinny/REDRESS.md:3284`, `restart/locks/LOCKS.md:52`, `restart/locks/LOCKS.md:78`, `restart/locks/LOCKS.md:92`, `restart/locks/LOCKS.md:112`.
- Local aarch64 SIMD modules/tests: `skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:7`, `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:63`, `skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`, `skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`, `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5`, `skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:26`, `skinny/crates/bbnf-simd/src/aarch64/quad_load.rs:3`, `skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:23`, `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:156`, `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13`, `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs:5`, `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs:5`, `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs:5`, `skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:59`.
- Prior tranche context only: `restart/skinny/tranches/sk-v11/research/p2/p2c-arch-esoterica.md:1`.

External primary Arm ISA/ACLE sources:

- E1. Arm Neon Intrinsics Reference, table lookup intrinsics: `vqtbl4q_u8` maps to A64 `TBL` and `vqtbx4q_u8` maps to A64 `TBX` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 15815-15821 and 16139-16147).
- E2. Arm C Language Extensions: `__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_CSSC`, and `__ARM_FEATURE_SHA3` feature macros and selection/dependency text (https://arm-software.github.io/acle/main/acle.html, lines 1694-1697, 1754-1758, 1815-1817, 1911-1913, 1945).
- E3. Arm Neon Intrinsics Reference, dot product intrinsics: `vdotq_u32` maps to A64 `UDOT` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 18087-18092).
- E4. Arm Neon Intrinsics Reference, polynomial multiply: `vmull_p64` maps to `PMULL` and `vmull_high_p64` maps to `PMULL2` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 16597-16605). Arm A64 CTZ instruction page: https://developer.arm.com/documentation/ddi0602/2025-12/Base-Instructions/CTZ--Count-trailing-zeros- .
- E5. Arm Neon Intrinsics Reference, structured loads: `vld4q_u8` maps to `LD4 {Vt.16B - Vt4.16B},[Xn]` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 11909-11916).
- E6. Arm Neon Intrinsics Reference, SHA3 ternary logical: `veor3q_u8` maps to `EOR3`; `vbcaxq_u8` maps to `BCAX` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 18256-18263 and 18322-18329).
- E7. Arm Neon Intrinsics Reference, EXT: `vextq_u8` maps to A64 `EXT` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 10104-10115).
- E8. Arm Neon Intrinsics Reference, wide shift/narrow support: `vshrn_n_u16` maps to `SHRN`; `vsriq_n_u8` maps to `SRI` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 5943-5946 and 6365-6370).
- E9. Arm Neon Intrinsics Reference, reductions/bit count: `vaddvq_u8` maps to `ADDV`; `vcntq_u8` maps to `CNT` (https://arm-software.github.io/acle/neon_intrinsics/advsimd.html, lines 3885-3886 and 8720-8726).
