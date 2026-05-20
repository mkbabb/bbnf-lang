# SK-V12 aarch64 SIMD Coverage Audit

**Date:** 2026-05-20  
**Scope:** aarch64 NEON/SIMD primitives in `skinny/crates/bbnf-simd` on target M5 Max (ARMv9.2-A)  
**Authority:** CHECKASM-REPORT.md, REDRESS.md (entries 28, 33, 88, 89, 90), p2c-arch-esoterica.md (SK-V11/V12)  
**Inventory source:** direct file enumeration + consumer grep; Lock 16 compliance check per CHECKASM-REPORT.md §SK-V5 Wave 5 table

---

## §1 Active Primitives Table

| Primitive name | File path | Scalar ref | Checkasm | Consumer wired | Row movement | REDRESS/notes |
|---|---|---|---|---|---|---|
| `byte_class_from_eq_set_64` | `aarch64/byte_class_from_eq_set_64.rs` | ✓ `byte_class_from_eq_set_64_scalar` | ✓ `checkasm_byte_class_from_eq_set_64.rs` | YES: `dispatch.rs:68` | None | Lock 16 admits generic aarch64 ideas; C1 candidate in P2-C |
| `byte_class_from_table_64` | `aarch64/byte_class_from_table_64.rs` | ✓ `byte_class_from_table_64_scalar` | ✓ `checkasm_byte_class_from_table_64.rs` | DELEGATED: calls scalar | None | Wrapper; scalar ref in `lib.rs:114` consume path; C1 candidate |
| `bitmap_prefix_xor_64` | `aarch64/bitmap_prefix_xor_64.rs` | ✓ `bitmap_prefix_xor_64_scalar` | ✓ `checkasm_bitmap_prefix_xor_64.rs` | DELEGATED: calls scalar | None | Wrapper; active in `lib.rs:171`; REDRESS 88 blocks PMULL default |
| `bitmap_next_set_bit` | `aarch64/bitmap_next_set_bit.rs` | ✓ `bitmap_next_set_bit_scalar` | ✓ `checkasm_bitmap_next_set_bit.rs` | DELEGATED: calls scalar | None | Wrapper; used in `compact_mask` `lib.rs:245`; REDRESS 89 blocks CTZ consumer |
| `bulk_emit_positions_64` | `aarch64/bulk_emit_positions_64.rs` | ✓ `bulk_emit_positions_64_scalar` | ✓ `checkasm_bulk_emit_positions_64.rs` | DELEGATED: calls scalar | None | Wrapper; unsafe in `compact_mask` `lib.rs:250`; support only (C7) |
| `eob_pad_clamp` | `aarch64/eob_pad_clamp.rs` | ✓ `eob_pad_clamp_scalar` | ✓ `checkasm_eob_pad_clamp.rs` | DELEGATED: calls scalar | None | Wrapper; dispatch entry `dispatch.rs:73`; Lock 16 admits generic |
| `classify_tbl4` | `aarch64/classify_tbl4.rs` | ✓ `classify_chunk` scalar + parity tests | ✓ `checkasm_parity.rs` | YES: `dispatch.rs:24-32` (NeonTbl4 backend) | NO row movement (parity-only) | C1 candidate; vqtbl4q_u8 low6 table; grammar-neutral |
| `byte_context` | `aarch64/byte_context.rs` | No scalar | No dedicated checkasm | NO: support only | None | `vextq_u8` shift primitives; used in string boundary context (C4 support) |
| `cache_hints` | `aarch64/cache_hints.rs` | No scalar | No checkasm | NO | None | PRFM/STNP inventory only; SK-V11 hardening demoted unless fresh P1 evidence |
| `digit_mac` | `aarch64/digit_mac.rs` | ✓ scalar fallback in `parse_4_digits`; parse-that-regex digit oracle | ✓ `aarch64_primitives.rs:168` (smoke) | YES: conditional (dotprod feature) | NO row movement today | C3 candidate; UDOT x1 only; needs x4 oracle + checkasm + same-wave consumer |
| `eob_pad_clamp` | (see above) | | | | | |
| `match_tiny_plain_string` | `aarch64/match_tiny_plain_string.rs` | ✓ `match_tiny_plain_string_scalar` | ✓ (via checkasm_parity.rs indirect) | NO: wired in dispatch but active path REDRESS-blocked | BLOCKED | REDRESS 324/1973: active tiny-string routes remain blocked; Lock 14 grammar-neutral but C1 wiring rejected |
| `movemask` | `aarch64/movemask.rs` | No dedicated scalar ref; pattern-based movemask in `byte_class_from_eq_set_64.rs:79-87` | Covered under `checkasm_byte_class_from_eq_set_64.rs` | YES: consumed by all classify/string paths | Support only | vshrn_n_u16<4> + vzip1q_u8 idiom; no standalone row movement (C7 support) |
| `quad_load` | `aarch64/quad_load.rs` | No scalar (vld1q_u8_x4 is load primitive) | ✓ `aarch64_primitives.rs:71` (smoke) | NO: not currently called | None | vld1q_u8_x4 wrapper; LD4 not implemented (I1 inventory, not candidate) |
| `string_block` | `aarch64/string_block.rs` | ✓ `scan_string_special_block_scalar` | ✓ `checkasm_parity.rs:617-640` (16-byte only) | YES: consume paths in parse-that-regex (16-byte blocks) | NO row movement | C4 candidate; widened form needs new 64-byte oracle + caller parity; current 16-byte at ceiling |
| `unescape_uxxxx` | `aarch64/unescape_uxxxx.rs` | ✓ `unescape_uxxxx_scalar`; x1 and x4 bodies | ✓ x1 in `checkasm_parity.rs`; x4 is smoke-only in `checkasm_utf8_block.rs:59` | YES: x1 in `parse-that-regex:402` (parse_unicode_escape_4digit_neon); x4 is proof-only | NO row movement for x4 | C5 candidate; x1 wired, x4 proof-gated; REDRESS 2287/3174/3436 reject proof-only reuse; needs strict x4 checkasm + same-wave consumer |
| `utf8/validate_block` | `aarch64/utf8/validate_block.rs` | ✓ `validate_block_scalar` (16-byte) | ✓ `checkasm_utf8_block.rs` (16-byte parity) | YES: consumed in checkasm tests; no production wiring in parse-that-regex | PARITY only | Support for UTF-8 validation; scalar reference solid; no row movement |

---

## §2 Orphan Primitives

**Count: 5 orphans**

### 1. `bitmap_prefix_xor_64` (aarch64 wrapper)
- **Planned consumer:** bitmap carry handoff for JSON string region escape scanning (`lib.rs:171` prefix_xor_64 call)
- **Blocker:** REDRESS 88 rejects PMULL as the default `bitmap_prefix_xor_64` hot body after parse benchmark regressions (`skinny/REDRESS.md:2510`). Current aarch64 implementation delegates to scalar reference by design.
- **Removable per clean-regen-discipline:** YES. The wrapper is a no-op delegate; actual PMULL body blocked by REDRESS 88; scalar path is authoritative.

### 2. `bitmap_next_set_bit` (aarch64 wrapper)
- **Planned consumer:** next set bit extraction for `compact_mask` position emission (`lib.rs:245` call)
- **Blocker:** REDRESS 89 rejects CSSC CTZ bulk consumer despite correctness and asm proof (`skinny/REDRESS.md:2542`). Current aarch64 implementation delegates to scalar by design; no CTZ body wired.
- **Removable per clean-regen-discipline:** YES. Wrapper is a no-op delegate; CTZ blocked by REDRESS 89; scalar reference authoritative.

### 3. `bulk_emit_positions_64` (aarch64 wrapper)
- **Planned consumer:** bulk position emission into Vec<u32> inside `compact_mask` loop (`lib.rs:250` unsafe call)
- **Blocker:** No aarch64 SIMD optimization currently implemented; delegates to scalar. REDRESS 89 scope overlaps (bulk emit is part of the next-bit/compress pipeline) but no dedicated REDRESS entry.
- **Removable per clean-regen-discipline:** YES. Support primitive (C7) with no standalone row movement; scalar path is stable; wrapper is non-functional.

### 4. `byte_context` (shift helpers)
- **Planned consumer:** cross-chunk string boundary context via `vextq_u8` for widened string-special (C4 candidate)
- **Blocker:** No current production consumer wired. Candidate C4 `wide_string_special_scan64` is not yet admitted (P2-C research only; same-wave consumer gate unresolved).
- **Removable per clean-regen-discipline:** YES. Support inventory only (marked "support-only inside bounded string/whitespace candidate" in SK-V11 P2-C §2); can be extracted when C4 lands with a real consumer.

### 5. `cache_hints` (PRFM/STNP)
- **Planned consumer:** output_digest_hash or output-plane writeback streaming hints
- **Blocker:** SK-V11 hardening flagged V1 trace to output_digest_hash as over-broad and demoted PRFM/STNP to inventory-only. No fresh P1 behavior evidence ties hint placement to a row-moving consumer (`restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md:12,64`).
- **Removable per clean-regen-discipline:** YES. Explicitly inventory-only per SK-V11 hardening; no same-wave consumer proved; can be removed or moved to optional module.

---

## §3 ARMv9.2-A Untapped Surface

### NEON TBL/TBX (vqtbl4q_u8, vqtbx4q_u8)
- **In-tree status:** TBL is actively used in `classify_tbl4` (low-6 table classification, 4×16-byte stripes) and `unescape_uxxxx` (hex nibble LUT). TBX is not currently used.
- **Untapped:** Full TBX fallback path (graceful out-of-range handling without re-equality-check) and LD4 deinterleave (I1 inventory, no current interleaved stream proved).
- **Hot leaf:** bounded_plain_string_scan, unicode_escape_hex_decode, container_dispatch (C1 byte-class generalization)
- **Lock 16:** Admits generic aarch64 TBL ideas with scalar parity/corpus parity required (`restart/locks/LOCKS.md:92`)
- **SK-V12 wave:** C1 in W2 selected-baseline if same-wave consumer names a grammar-neutral byte-set caller (whitespace, delimiter dispatch, or string-special scan)

### PMULL / PMULL2 (vmull_p64)
- **In-tree status:** No aarch64 NEON body; scalar `bitmap_prefix_xor_64_scalar` is authoritative
- **Hot leaf:** bitmap carry handoff for string region escape scanning
- **Lock 16:** REDRESS 88 explicitly rejects PMULL as the default `bitmap_prefix_xor_64` hot body after parse benchmark regressions (`skinny/REDRESS.md:2510`)
- **SK-V12 wave:** BLOCKED by REDRESS 88 with no reopen path visible. Cannot be drafted for W2.

### SHA3 EOR3 / BCAX (veor3q_u8, vbcaxq_u8)
- **In-tree status:** No aarch64 body; no local three-input boolean expression identified
- **Untapped:** Inventory-only per SK-V11 P2-C §2 ("no V3 same-wave consumer"). No P1 hot leaf names a three-input mask fold (quote/escape/control fusion or digest-plane bit mixing without a scalar formula).
- **Hot leaf:** None sufficient for SK-V12 candidate eligibility
- **Lock 16:** Allows SHA3 ternary bitwise (`restart/locks/LOCKS.md:94`) but requires measured consumer
- **SK-V12 wave:** Non-selectable. Cannot be drafted for W2 without a fresh profile naming the exact three-input expression and the same wave supplying scalar oracle + checkasm + consumer.

### CSSC CTZ (ctz, clz, cnt, popcount)
- **In-tree status:** No aarch64 NEON body; scalar references use `trailing_zeros` (via `ctz` or compiler lowering). Current `bitmap_next_set_bit_neon` delegates to scalar.
- **Hot leaf:** next-set-bit extraction for mask emit (C7 support)
- **Lock 16:** REDRESS 89 explicitly rejects CSSC CTZ bulk consumer/canary fold despite correctness and asm proof (`skinny/REDRESS.md:2542`)
- **SK-V12 wave:** BLOCKED by REDRESS 89. CTZ can only be used as a narrow scalar-equivalent support detail if a consumer proves itself outside the default production path. Cannot be drafted for W2 as a bulk emit optimization.

### UDOT / SDOT (vdotq_u32, vsdotq_s32)
- **In-tree status:** UDOT used in `digit_mac.rs:parse_4_digits_dotprod` under `target_feature = "dotprod"`. SDOT not used.
- **Hot leaf:** number_digit_span (C3 candidate); only 4-digit helper visible; number-span scalar oracle in parse-that-regex
- **Lock 16:** Admits dot-product primitives with scalar parity and checkasm required (`restart/locks/LOCKS.md:92`)
- **SK-V12 wave:** C3 in W2 if same-wave number consumer replaces a real digit-run hot loop. Current `parse_4_digits` is micro-proof only; needs x4 oracle, broader coverage (all non-digit offsets, mixed valid/invalid), and caller-level row gate.

### CSSC (general: CTZ, CNT, ABS, SMAX, SMIN, UMAX, UMIN)
- **In-tree status:** Only CTZ/CNT inventory; no ABS/SMAX/SMIN/UMAX/UMIN bodies.
- **Untapped:** Compare-and-branch reduction (ABS, SMIN/SMAX) not yet exploited; all-zero/all-ones detection (UMAX, UMIN) not profiled on hot leaf
- **Hot leaf:** None named for ABS/extrema in P1
- **SK-V12 wave:** Cannot be drafted for W2; requires P1 attribution

### BFDOT / BFMMLA (bf16 matrix multiply-accumulate)
- **In-tree status:** Not present; no BF16 codepath in any aarch64 module
- **Untapped:** Inventory only; JSON/CSS/Sheets/BBNF parsers have no BF16 numeric tokens
- **SK-V12 wave:** Out of scope; not applicable to text parsing

### SVE2 (scalable vectors)
- **In-tree status:** Not present; all primitives use fixed-width NEON (128-bit)
- **Untapped:** Inventory only; Lock 16 does not mention SVE2; Apple Silicon M5 Max supports SVE2 but target tranche is NEON-only
- **SK-V12 wave:** Out of scope per tranche handoff ("aarch64 Apple Silicon instruction inventory" is NEON/SIMD-only, not SVE)

---

## §4 Lock 16 Compliance Check

**Spec §2.2 requirement:** Every primitive must have (a) scalar reference, (b) parity/checkasm, (c) same-wave consumer.

### Compliant primitives (3 total):
1. **`byte_class_from_eq_set_64`**: ✓ scalar `byte_class_from_eq_set_64_scalar`, ✓ checkasm coverage, ✓ consumed via `dispatch.rs:68` in `select_primitive_kernels` for generic scan dispatch
2. **`classify_tbl4`**: ✓ scalar parity tests (checkasm_parity.rs), ✓ checkasm (parity.rs), ✓ consumed via `dispatch.rs:24-32` in SelectedBackend::NeonTbl4
3. **`unescape_uxxxx` (x1 only)**: ✓ scalar `unescape_uxxxx_scalar`, ✓ checkasm (parity.rs), ✓ consumed in `parse-that-regex:402` (parse_unicode_escape_4digit_neon)

### Non-compliant primitives (6 violations):
1. **`byte_class_from_table_64` (aarch64 wrapper)**: ✓ scalar, ✓ checkasm, BUT consumed via delegate (calls scalar in aarch64 body); no actual NEON body; this is a compliance exception per dispatch structure (Lock 16 footnote permits wrappers that delegate with fallback)
2. **`bitmap_prefix_xor_64` (aarch64 wrapper)**: ✓ scalar, ✓ checkasm, BUT consumed via delegate; wrapper is no-op by design per REDRESS 88 (PMULL blocked)
3. **`bitmap_next_set_bit` (aarch64 wrapper)**: ✓ scalar, ✓ checkasm, BUT consumed via delegate; wrapper is no-op by design per REDRESS 89 (CTZ blocked)
4. **`bulk_emit_positions_64` (aarch64 wrapper)**: ✓ scalar, ✓ checkasm, BUT consumed via delegate; support primitive (C7) with no row movement target
5. **`eob_pad_clamp` (aarch64 wrapper)**: ✓ scalar, ✓ checkasm, BUT consumed via delegate; wrapper delegates to scalar; technically compliant if wrappers are exempt, but no actual NEON body
6. **`unescape_uxxxx` (x4 form)**: ✓ scalar (`unescape_uxxxx_scalar` repeated 4× in proof), ✓ smoke checkasm (checkasm_utf8_block.rs:59), BUT **✗ NO same-wave consumer** (proof-only; REDRESS 2287/3174/3436 reject reuse without new source delta); **violates §2.2**

### `digit_mac` (parse_4_digits)
- ✓ scalar fallback, ✓ smoke checkasm (aarch64_primitives.rs:168), BUT **✗ no same-wave consumer** proven to replace a real hot loop (x1 only, narrowly used in four-digit micro-proof); **violates §2.2** as production primitive
- Status: Candidate C3 (admission-gated on full x4 oracle + checkasm + caller row movement)

### `match_tiny_plain_string`
- ✓ scalar `match_tiny_plain_string_scalar`, ✓ parity tests, BUT **✗ active wiring is REDRESS-blocked** (REDRESS 324/1973); dispatch path exists but hot consumer path is not taken
- Status: C1 candidate (grammar-neutral byte-class lookup) with active path blocked; wiring unresolved until REDRESS blocks lifted

---

## §5 Recommendation Matrix (W2 Selected-Baseline Intervention)

### Context
SK-V12 W2 "selected-baseline intervention" per Sheets profile targets grammar-neutral primitives with:
- Scalar reference + parity/checkasm ✓
- Same-wave consumer in generated code (NOT telemetry-only)
- Row movement on at least one non-JSON profile (CSS/Sheets/BBNF-self OR JSON-only if justified)
- ≤430 LOC budget (typical constraint for W2 kernel admission)

### Candidate 1: `a64_tbl_byte_class_mask64` (C1)
- **File path:** `aarch64/byte_class_from_eq_set_64.rs` (87 lines; existing parity harness complete)
- **Scalar-ref status:** ✓ `byte_class_from_eq_set_64_scalar` (38 lines) + `byte_class_from_table_64_scalar` (10 lines); both executable specs
- **Checkasm:** ✓ Existing `checkasm_byte_class_from_eq_set_64.rs` (19 KB, 156 lines directive coverage); alignment/density/corpus sweep complete
- **Grammar-neutrality:** ✓ Lookup table generated from Grammar IR byte classes (JSON structural, CSS delimiters, Sheets operators, BBNF metasyntax)
- **Consumer wiring:** Requires generated byte-set caller (whitespace skip, delimiter dispatch, or string-special scan). JSON structural dispatch via `classify_tbl4` is wired but not a row-moving consumer alone.
- **Same-wave consumer:** Whitespace skip (`C6` variant) or bounded string-special scan (`C4` variant) must consume in W2 simultaneously
- **Lock 14 grammar-neutrality:** PASS ✓ (byte set is grammar-neutral; avoid JSON-only structural role)
- **LOC budget:** 87 existing + ~150 for generalized 64-byte table/eq-set wrapper = ~237 LOC ✓
- **Hot-leaf attribution:** bounded_plain_string_scan, container_dispatch, ascii_whitespace_skip (P1 hot leaves directly addressed)
- **Recommendation:** **#1 candidate** if W2 plan includes a grammar-neutral whitespace-skip or layout-byte consumer. TBL coverage is solid, scalar ref is strong, checkasm is complete. Risk: consumer wiring must be same-wave (not W3).

### Candidate 2: `a64_udot_digit_span` (C3)
- **File path:** `aarch64/digit_mac.rs` (71 lines); `parse-that-regex/number/mod.rs` (digit oracle)
- **Scalar-ref status:** ✓ `parse_4_digits` scalar fallback (lines 6-23); number-span oracle in parse-that-regex (200+ lines); SWAR 8/4/2-byte blocks proven
- **Checkasm:** Partial ✓ Smoke test in `aarch64_primitives.rs:168`; **missing:** exhaustive invalid-digit, mixed valid/invalid, tail, overflow coverage
- **Grammar-neutrality:** ✓ ASCII decimal digit decode (JSON, CSS, Sheets, BBNF number tokens all share digit syntax)
- **Consumer wiring:** Number-span consumer in generated direct/typed literal decoder or parse-that Layer-1 primitive must replace a real hot digit loop. Current x1 micro-proof is insufficient.
- **Same-wave consumer:** Requires new `pt_digit_run_span` primitive or direct number materializer consuming x1 or x4 UDOT result; grammar sign/exponent/overflow policy stays scalar
- **Lock 14 grammar-neutrality:** PASS ✓ (digit decode is generic; number policy stays caller-owned)
- **LOC budget:** 71 existing + ~80 for x4 oracle + scalar parity + full checkasm = ~151 LOC ✓
- **Hot-leaf attribution:** number_digit_span, typed_direct_projection (P1 hot leaves; high profile measurement)
- **Recommendation:** **#2 candidate if Sheets numeric profile shows >1% delta on digit-heavy rows.** Scalar fallback is stable, UDOT proof is correct (inline asm validated), but **blocker:** missing x4 oracle, strict checkasm (invalid cases, mixed lanes), and caller row gate. W2 should defer unless caller layer (parse-that or generated literal decoder) names the same-wave consumer.

### Candidate 3: `a64_wide_string_special_scan64` (C4)
- **File path:** `aarch64/string_block.rs` (72 lines; 16-byte version existing)
- **Scalar-ref status:** Partial ✓ `scan_string_special_block_scalar` (16-byte; 24 lines); **missing:** 64-byte oracle
- **Checkasm:** Partial ✓ 16-byte parity in `checkasm_parity.rs:617-640`; **missing:** 64-byte oracle + alignment/boundary/tail sweep
- **Grammar-neutrality:** ✓ Terminator/escape/control/non-ASCII are grammar parameters (not JSON-hardcoded); CSS may differ on control-limit; Sheets non-ASCII handling varies
- **Consumer wiring:** Requires bounded string scanner in generated direct/typed or parse-that Layer-1; narrow consumer (not full string materializer; REDRESS 106 rejected broad proof)
- **Same-wave consumer:** Real string/literal scanner must replace existing hot span in same wave with strict row equality; measurement required (REDRESS 61 rejected proof-only; REDRESS 106 showed 0.774× prior attempt)
- **Lock 14 grammar-neutrality:** PASS ✓ (byte class and limit are parameters; avoid JSON escape state fusion)
- **LOC budget:** 72 existing + ~200 for 64-byte oracle + checkasm + movemask/EXT boundary handling = ~272 LOC ✓
- **Hot-leaf attribution:** bounded_plain_string_scan (highest P1 rank)
- **Recommendation:** **#3 candidate, conditional on row movement proof.** TBL byte-class support (C1) must land first as the classification substrate. 64-byte oracle is straightforward (four 16-byte scalar blocks), but **blocker:** needs new consumer proof showing real row delta on at least CSS or Sheets (not JSON-only). REDRESS 61/106 firewall any broad string rewrite; narrow consumer gate is strict.

---

## Summary

- **Compliant Lock 16 primitives:** 3 (byte_class_from_eq_set_64, classify_tbl4, unescape_uxxxx x1)
- **Non-compliant or blocked:** 6 (wrappers delegating to scalar; x4 forms proof-only; candidates C3/C4/C6 gate-blocked on same-wave consumer + row movement)
- **Orphan count:** 5 (bitmap_prefix_xor_64, bitmap_next_set_bit, bulk_emit_positions_64, byte_context, cache_hints)
- **ARMv9.2-A untapped:** PMULL (REDRESS 88), CSSC CTZ (REDRESS 89), SHA3 EOR3/BCAX (no 3-input fold expression), SVE2 (NEON-only scope), BFDOT (no BF16 tokens)
- **W2 top-3 candidates ranked:** (1) C1 TBL byte-class + whitespace consumer, (2) C3 UDOT digit-span if Sheets shows >1% delta, (3) C4 widened string-special if narrow consumer + row gate proven

**Honestly: none of the three candidates is risk-free for W2 without a same-wave consumer wired and row measurement complete. C1 (TBL) has the strongest parity foundation but requires caller coordination. C3 (UDOT) needs strict x4 checkasm. C4 (string-special) reopens REDRESS 106 boundary cautiously.**

