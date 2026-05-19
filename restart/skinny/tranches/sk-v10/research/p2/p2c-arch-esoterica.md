# SK-V10 P2-C: Host-Arch ASM/SIMD Esoterica

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: host-architecture instruction inventory keyed to SK-V10 P1 hot leaves, with aarch64 primary and x86 secondary.
Output: this file.
P1 hot-leaf antecedents: `string_tiny_scan`, `string_full_scan`, `string_escape` / `unicode_escape_hex`, `number_digit_scan` / `number_scan`, `whitespace_skip`, `simd_movemask`, `direct_struct` digest fold, and direct/typed maintain rows from P1-E.
Lock surface: both Lock 1 and Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. The current host is aarch64 Apple M5 Max, so AArch64 is the primary inventory target, not an x86 backport exercise. P1-E records `aarch64-apple-darwin;arch=aarch64;cpu=Apple M5 Max` and names the product-plane hot leaves: string tiny/full scan, escape/hex decode, number scan, whitespace, movemask, array/object walk, and digest fold (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:1`, `:40`-`:47`).
2. P1 hot leaves are product-plane leaves. Parse-only structural-scan speed is a masking signal and cannot reopen W3. P1-C says isolated SIMD scan is faster than scalar on every row, but any future kernel must micro-prove the primitive and the product-plane caller, not only the primitive (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:88`-`:130`).
3. The existing AArch64 inventory already contains relevant primitives: TBL4 classification (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:22`, `:31`), movemask packing via `vshrn`/`vsri` (`skinny/crates/bbnf-simd/src/aarch64/movemask.rs:4`-`:10`), 16-byte string special scanning (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:31`, `:57`-`:69`), TBL unicode quartet decode (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:40`, `:74`, `:125`), and UDOT digit MAC (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5`, `:27`, `:40`).
4. AArch64 PMULL and CSSC CTZ are known instruction routes, but production rewires are blocked. REDRESS 88 rejected PMULL as the default `bitmap_prefix_xor_64` body after JSON parse regressions despite checkasm and asm proof (`skinny/REDRESS.md:2510`-`:2540`). REDRESS 89 rejected the CSSC/`ctz` bulk consumer after six parse maintain rows dropped by more than 2% (`skinny/REDRESS.md:2544`-`:2585`).
5. AArch64 TBL/TBX remain viable only when they target an existing hot caller and do not reintroduce the rejected wrappers. The single-quartet unicode classifier route was correctness/checkasm green but row-failed (`skinny/REDRESS.md:2285`-`:2315`); the StringBlock16 tiny probe row-failed and must not be reopened as the same wrapper (`skinny/REDRESS.md:2318`-`:2354`).
6. The W3 union/event substrate is retired, not merely pending. REDRESS 96/97 made the structural-index/class-column thesis measurable and missed every W3 must-improve and W10b maintain row; REDRESS 98 forbids forcing, amending, or splitting the same union-substrate hypothesis without a new Alpha/S-P3 contract (`skinny/REDRESS.md:2795`-`:2848`, `:2850`-`:2906`, `:2908`-`:2946`).
7. X86 support is broad in the source tree but mostly scaffold or secondary to the host. The tree has AVX2, AVX-512 BW, VBMI2, GFNI, VPCLMUL, IFMA, VNNI, BITALG, and kmask modules; most advanced bodies remain `unimplemented!()` except the AVX-512 BW `BYTE_CLASS_FROM_EQ_SET_64` asm path (`skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm:2`-`:18`; `skinny/crates/bbnf-simd/src/x86_64/avx512_vbmi2/classify.rs:35`-`:43`; `skinny/crates/bbnf-simd/src/x86_64/avx512_gfni/classify_affine.rs:45`-`:57`; `skinny/crates/bbnf-simd/src/x86_64/avx512_vpclmul/prefix_xor.rs:36`-`:50`).
8. ISA reference points: Arm's A64 ISA reference is the primary source for A64 instruction semantics; Arm's public ISA overview lists PMULL/PMULL2 as AArch64 cryptographic polynomial multiply instructions. Intel's Intrinsics Guide and Intel compiler intrinsic docs are the primary public references for x86 AVX/AVX-512, GFNI, VPCLMULQDQ, VBMI2, and PCLMULQDQ instruction families.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

### AArch64 inventory first

| Candidate | Shape | Arch route | P1 antecedent | Scalar-ref status | Checkasm status | Same-wave consumer note | REDRESS disposition |
|---|---|---|---|---|---|---|---|
| `BYTE_CLASS_FROM_TABLE_64` / TBL4 classify maintain | 64-byte byte-class mask from grammar-owned table; returns structural / terminator / escape / control masks without JSON policy in `bbnf-simd`. | NEON `TBL` via `vqtbl4q_u8`, movemask via `vshrn`/`vsri`. | `simd_movemask` on `gsoc-2018`, secondary on `twitter`, `github_events`, `update_center`. | Existing scalar table classifier anchors `prim::byte_class_from_table_64`. | Existing checkasm table parity is recorded in `CHECKASM-REPORT.md`; any new table variant needs its own cell. | Only admissible if consumed by current direct/typed string caller or scanner parity gate; not a retained union substrate producer. | REDRESS 96-98 block substrate consumption; table classification itself is maintain-only unless product caller micro-proof wins. |
| `STRING_FIRST_SPECIAL_16_INLINE` | Return first index of quote / escape / control / non-ASCII in one 16-byte block, without the rejected generated-retained wrapper overhead. | NEON compares plus movemask; wide shifts are already used in movemask. A lower-overhead hand asm form may use `rbit`/`clz` or scalar `ctz` after mask materialization. | `string_full_scan`, `string_tiny_scan`, `unicode_mixed`, `unicode_escapes`, typed `twitter` / `apache_builds` / `update_center`. | Existing scalar reference is `scan_string_special_block_scalar`. | Missing as a standalone admitted candidate for this exact inline first-special surface; must add checkasm over all masks and alignments. | Same-wave consumer must be `match_string_at_quote_trusted_utf8` or generated typed/direct `skip_plain_string_end`, not W3. | The old `StringBlock16` wrapper is blocked by REDRESS 83; this candidate survives only if micro-proof shows lower call-site overhead before wiring. |
| `UNICODE_ESCAPE_X4_VALIDATE_DECODE` | Decode or validate four `\uXXXX` quartets in a 16-byte batch, returning codepoints or failure mask without eager decoded scratch. | NEON `TBL` (`vqtbl1q_u8`) nibble decode plus wide shifts/OR for pack; TBX is optional for fallback-preserve forms but not required. | `string_escape` / `unicode_escape_hex` on `unicode_escapes`, `unicode_mixed`, `y_string_unicode`. | Existing scalar `unescape_uxxxx_scalar`, `join_surrogates`, and x4 NEON body exist. | Single-quartet checkasm was green historically; x4 production surface needs dedicated checkasm with invalid hex, BMP, surrogate, and alignment cases. | Same-wave consumer must be `validate_unicode_escape_run`, `decode_unicode_escape`, or `unescape_string` while preserving lazy output. | Single-quartet production route is blocked by REDRESS 82; x4 validate/decode is only a candidate if it avoids per-quartet materializer cost and eager scratch. |
| `DIGIT_RUN_DOTPROD_4_OR_8` | Consume 4 or 8 ASCII digits into integer accumulator with prevalidated digit mask, or return failure. | AArch64 DotProd `UDOT`; current code uses inline asm `udot` for 4 digits. | `number_digit_scan` / `number_scan` on `canada`, `mesh`, `numbers`, `marine_ik`, typed `mesh`, typed `marine_ik`. | Existing `parse_4_digits` scalar loop. For 8+ digits, scalar reference must include overflow and exact materialization boundary. | No dedicated product-plane checkasm for digit-run materialization found; must add. | Same-wave consumer must be `parse-that-regex::number::scan_digit_run` / `match_number_span_from_first` or typed direct numeric materializer. | Canada typed shortcut remains blocked; numeric primitive may not be used to admit Canada typed without full-fixture typed proof. |
| `ASCII_WHITESPACE_RUN_16_OR_32` | Count or skip consecutive grammar whitespace over a bounded block; return first non-whitespace index. | NEON compare fan, TBL class table, or pairwise mask with wide shift extraction. | `whitespace_skip` on `citm_catalog`, `random`, `mesh`, `marine_ik`, typed `citm_catalog`. | Current scalar `skip_ascii_whitespace` is the reference. | Missing for a standalone primitive. | Same-wave consumer must be the current parse-that whitespace caller and preserve admitted `citm_catalog` direct/typed rows. | Prior generic SWAR whitespace and cursor/sidecar variants were rejected; this must be a leaf replacement only, no sidecar. |
| `MOVEMASK_FASTPATH_16` | Reduce a NEON comparison vector to a 16-bit lane mask with fewer scalar stores and less overhead than current `movemask_u8x16`. | Wide shifts (`vshrn`, `vsri`) are already in tree; candidate is micro-asm or refined intrinsic sequence. | `simd_movemask` visible on `gsoc-2018`, secondary direct rows. | Existing scalar mask pack can be a small loop over lane high bits. | Existing movemask smoke/parity exists indirectly; a direct checkasm cell is needed for all 0/FF and arbitrary high-bit patterns. | Same-wave consumer must be `classify_tbl4`, `string_block`, or UTF-8 block caller with a row-level micro-proof. | Not blocked by itself, but W3 consumption is blocked. |
| `PMULL_PREFIX_XOR_NARROW` | Prefix-XOR over quote/backslash masks using polynomial multiply, but only for a proven string-heavy caller and never as default hot body. | AArch64 PMULL/PMULL2. | `string_full_scan`, escape mask propagation; possible `unicode_mixed` / `unicode_escapes`. | Existing scalar `bitmap_prefix_xor_64_scalar` and `escape_mask_64`. | Existing checkasm passed in rejected W10; new narrow route still needs checkasm and row-specific proof. | Same-wave consumer must be a string-region caller whose microbench wins; do not route through global `bitmap_prefix_xor_64` dispatch. | REDRESS 88 blocks default PMULL production path. Candidate is marked REDRESS-blocked unless reframed as narrow, caller-proven, non-default. |
| `CSSC_CTZ_NEXT_SET_BIT` | Next set bit from mask and cursor using CSSC `ctz`; optional bulk emitter use. | AArch64 FEAT_CSSC `CTZ`. | `unicode_basic` trailing_zeros, structural emit loops, movemask consumers. | Existing scalar `bitmap_next_set_bit_scalar`. | Existing W10b checkasm passed; new consumer still needs checkasm and no-diff maintain proof. | Same-wave consumer may not be global `bulk_emit_positions_64` unless it proves no maintain regression. | REDRESS 89 blocks CTZ bulk consumer as production path. Candidate is REDRESS-blocked for default bulk emit. |

### X86 second

| Candidate | Shape | Arch route | P1 antecedent | Scalar-ref status | Checkasm status | Same-wave consumer note | REDRESS disposition |
|---|---|---|---|---|---|---|---|
| `BYTE_CLASS_FROM_EQ_SET_64_AVX512BW` | 64-byte set membership mask for up to eight bytes. | AVX-512 BW `vpbroadcastb` + `vpcmpeqb` + `korq` + `kmovq`; asm body exists. | Generic byte-class, string/structural classification. | Existing scalar reference in `scalar/byte_class_from_eq_set_64.rs`. | Existing dedicated checkasm path. | Secondary host route; same-wave consumer must be grammar-neutral scanner or product caller. | Not a SK-V10 row mover on Apple host. |
| `AVX2_CLASSIFY_32` | 32-byte structural or byte-class classifier. | AVX2 `vpshufb`/compare/movemask. | Same as AArch64 table classify. | Scalar ref in module. | Body scaffold is `unimplemented!()`, so no production checkasm. | Use only for future x86 host wave. | Secondary; avoid JSON-hardcoded classifiers from REDRESS 36. |
| `AVX512_VBMI2_OR_BITALG_CLASSIFY_64` | 64-byte classify to k-mask, optionally multi-class in one pass. | VBMI2/BITALG `vpshufbitqmb`, k-mask residency. | `simd_movemask`, string/structural classification. | Scalar classify refs exist. | Bodies are scaffolded; no production checkasm. | Future x86-specific same-wave scanner/caller only. | Blocked from substrate route by REDRESS 96-98. |
| `AVX512_VBMI2_COMPRESS_POSITIONS` | Emit structural positions by compressing indices under k-mask. | VBMI2 `vpcompressd` / `vpcompressb`. | Structural emit / `compact_mask`; not primary P1 product leaf except movemask. | Scalar `bulk_emit_positions_64_scalar`. | Scaffold only. | Must be scanner-local and same-wave consumed; not a retained structural vector. | W3 blocks retained sidecar/vector consumption. |
| `AVX512_KMASK_FUSE_ESCAPE` | Keep quote/backslash/control/string masks in k-registers for mask algebra. | AVX-512 F k-mask `kandnq`, `korq`, `kxorq`, `kortestq`, shifts. | String full scan / escape propagation. | Existing scalar mask algebra in `escape_mask_64`. | Scaffold only. | Future x86 string scanner only; no row movement on aarch64. | Not blocked if caller-local; blocked if used to recreate W3. |
| `VPCLMUL_PREFIX_XOR` / `AVX2_PCLMUL_PREFIX_XOR` | Prefix-XOR string body mask by carry-less multiply. | PCLMULQDQ / VPCLMULQDQ. | String-region mask propagation. | Scalar prefix-xor refs exist. | Scaffold only. | Future x86 narrow string caller only. | Mirrors AArch64 PMULL risk; do not default-wire without row proof. |
| `AVX512_VNNI_DIGIT_MAC` | 8/16 digit block dot product into integer accumulator. | VNNI `vpdpbusd`. | `number_digit_scan` / typed numeric rows. | Scalar `parse_8_digits_scalar` exists. | Scaffold only. | Future x86 numeric materializer. | Canada typed shortcut remains blocked. |
| `AVX_IFMA_MANTISSA` | Eisel-Lemire mantissa multiply lanes. | AVX-IFMA `vpmadd52luq`. | Number materialization. | Scalar mantissa helpers exist in number path, but IFMA module is scaffold. | No production checkasm. | Only for x86 numeric materialization with exact float parity. | REDRESS 80 rejected mantissa-widen route; blocked unless fresh P1 names mantissa overflow/ambiguous returns. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Verdict |
|---|---|
| Byte-class/table classify | Grammar-neutral if alphabet/table is grammar-owned data. Generalizes to CSS delimiter sets, Sheets token separators, and BBNF-self punctuation. |
| String first-special block | Grammar-neutral when terminator, escape, control limit, and non-ASCII policy are parameters. JSON-only if it bakes quote/backslash/control semantics in `bbnf-simd`. |
| Unicode escape x4 validate/decode | Per-grammar template surface. Generalizes to CSS escapes only if the grammar supplies width/radix/escape-policy parameters; JSON surrogate policy must stay outside generic SIMD. |
| Digit-run DotProd / VNNI | Grammar-neutral numeric materialization primitive for fixed ASCII digit runs. Float syntax policy, exponent rules, and overflow disposition stay in parse-that or grammar code. |
| Whitespace run skip | Grammar-neutral byte-set run primitive if the whitespace set is supplied by grammar. |
| Movemask fastpath | Grammar-neutral reduction primitive. |
| PMULL / VPCLMUL prefix-XOR | Grammar-neutral bitmap prefix primitive; consumer must be grammar-owned string-region logic. |
| CSSC CTZ next-set-bit / x86 TZCNT/BMI emit | Grammar-neutral bitmap iteration primitive; production bulk consumers are REDRESS-blocked unless same-wave proof reopens narrowly. |
| AVX-512 classify/compress/k-mask families | Grammar-neutral in primitive form, JSON-overfit if wired as a JSON structural sidecar or class-column substrate. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- REDRESS 28 + 33: Class A NEON tiny-string wiring is not the parse-G fix; active 16-byte tiny-string dispatch regressed and the active parser remains on the 8-byte scalar tiny recognizer.
- REDRESS 50-55 and 66-69: direct scratch, eager decode, side tables, semantic facts, and fused materializer routes remain blocked unless a fresh material differential proves otherwise. P1-C's eager decode table is slower on every row.
- REDRESS 80: mantissa-widen is blocked; IFMA or wider Eisel-Lemire lanes need fresh mantissa-specific evidence, not just number hot-leaf evidence.
- REDRESS 82: single-quartet unicode classifier production route is blocked even though correctness/checkasm passed.
- REDRESS 83: generated-retained `StringBlock16` tiny wrapper is blocked; a future string-special primitive must prove lower call-site overhead before generated parser wiring.
- REDRESS 88: PMULL prefix-XOR as the default hot body is blocked after parse regressions.
- REDRESS 89: CSSC CTZ / bitmap-next-bit bulk consumer is blocked after maintain-row regressions.
- REDRESS 96-98: W3 union/class-column/streaming-cursor substrate is retired. No candidate may add a sidecar producer, retained class lane, second scan, parser-owned structural projection, or renamed union substrate.
- Alpha / SK-V10 pre-block: PMULL/CTZ production rewires as default hot paths, Canada typed shortcut, parse-only SOTA close, and any substrate/kernel intervention without micro-prove-first evidence are rejected defaults.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/bbnf-simd/src/aarch64/*`
- `skinny/crates/bbnf-simd/src/x86_64/*`
- `skinny/crates/bbnf-simd/src/scalar/*`
- `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- `skinny/crates/bbnf-simd/CONCRETIZATION-REPORT.md`
- Arm Developer, A64 Instruction Set Architecture: https://developer.arm.com/Architectures/A64%20Instruction%20Set%20Architecture
- Arm, ARMv8-A A64 ISA Overview PDF: https://developer.arm.com/-/media/Files/pdf/graphics-and-multimedia/ARMv8_InstructionSetOverview.pdf
- Intel Intrinsics Guide: https://www.intel.com/content/www/us/en/docs/intrinsics-guide/index.html
- Intel compiler docs for carry-less multiplication intrinsics / PCLMULQDQ: https://www.intel.com/content/www/us/en/docs/cpp-compiler/developer-guide-reference/2021-9/intrinsics-for-car-less-mult-and-adv-encrypt-std.html
