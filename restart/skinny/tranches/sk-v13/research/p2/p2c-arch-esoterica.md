# SK-V13 P2-C: Host AArch64 ASM/SIMD Esoterica

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-21.
Scope: host aarch64 / Apple Silicon instruction inventory keyed to S-P1 hot leaves; x86 is background only.
Output: this file.
P1 hot-leaf antecedents: CSS `scan_block` byte-by-byte delimiter advance; JSON structural scan SIMD/scalar probes; `parse_that_regex::unescape_string`; `read_hex_unit_scalar`; generated JSON direct envelopes; JSON number/direct rows; `string_block` / tiny-string sidecars.
Lock surface: both — Lock 1 for structural/union candidates, Lock 14 for grammar-neutral primitive policy, Lock 16 for SIMD/checkasm discipline.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. The best near-term aarch64 candidate is not a new exotic instruction; it is production consumption of the already micro-proven CSS delimiter member-find route. The CSS generated scanner still walks bytes one at a time in `scan_block`: `{`, `;`, and `}` are matched at `skinny/crates/runtime/src/grammars/css_l4_declaration_values/generated.rs:38-58`, with the default arm `self.pos += 1`. SK-V12 W4's `a64_ascii_set_run_skip` microbench reported `4.718279341x` speedup for `find_ascii_set_member64` over the `{`, `}`, `;` delimiter set, but production wiring was deferred in `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md:45-76`. S-P1 CSS profiling is timer/fact-sink dominated rather than parser-leaf dominated (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:82-87`), so this route is a prior micro-proof plus exact caller contract, not a P1 parser-hot-leaf proof.

2. Structural SIMD scan is real and fast, but structural retention is REDRESS-heavy. P1-C measures structural SIMD faster than scalar on all 17 corpora, with `mesh` 5.04x, `canada` 5.01x, and `numbers` 4.96x ratios (`restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md:63-83`, `:115-117`). The current JSON scanner consumes a TBL classifier plus scalar delegates for prefix/emit: `scan.rs:214-240` loads `classify_tbl4`, calls `escape_mask_64`, `prefix_xor_64`, and later `compact_mask` at `scan.rs:267`. The aarch64 bodies for `bitmap_prefix_xor_64`, `bitmap_next_set_bit`, and `bulk_emit_positions_64` are scalar delegates (`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor_64.rs:1-4`, `bitmap_next_set_bit.rs:1-4`, `bulk_emit_positions_64.rs:1-4`) despite being selected in the primitive table (`skinny/crates/bbnf-simd/src/dispatch.rs:63-74`).

3. PMULL and CSSC CTZ are admissible only as a new SIMD-first union route, not as default hot-body substitutions. Arm's ACLE defines `__ARM_FEATURE_CSSC` for common short sequence compression instructions including `CTZ` (`https://arm-software.github.io/acle/main/acle.html`, CSSC section) and documents PMULL/PMULL2 via `vmull_p64` / `vmull_high_p64` (`https://arm-software.github.io/acle/neon_intrinsics/advsimd.html`, polynomial multiply). REDRESS 88 rejected PMULL prefix-XOR as a default hot body, and REDRESS 89 rejected CTZ bulk consumer shape (`skinny/REDRESS.md:2510-2540`, `:2542-2601`). USER PIN D3/D4 unblocks the categories, but only with material differential and same-wave consumer; the viable differential is Union-C: PMULL matrix + CTZ rank-order emission that deletes scalar consume-structural in the measured consumer, not a local replacement of existing scalar delegates.

4. UDOT/DotProd is present in-tree but proof-only. `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:5-22` has a scalar `parse_4_digits`; `:25-49` has a `dotprod` target-feature body using `udot`. Arm ACLE says dot-product instructions operate on 8-bit sub-elements and are available when `__ARM_FEATURE_DOTPROD` is defined (`https://arm-software.github.io/acle/main/acle.html`, Dot Product extension and availability sections), and the Neon Intrinsics Reference maps `vdotq_u32` to `UDOT Vd.4S,Vn.16B,Vm.16B` (`https://arm-software.github.io/acle/neon_intrinsics/advsimd.html`, dot product table). S-P1 direct envelopes show number/container-heavy rows (`canada`, `numbers`, `mesh`) but no primitive numeric leaf yet (`restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md:72-86`), so UDOT needs numeric-density measurement and a same-wave numeric consumer before it can be more than measured-reject evidence.

5. TBL is already the right table-lookup primitive for current byte classes; TBX is a refinement, not a primary route. `classify_tbl4` uses `vqtbl4q_u8` (`skinny/crates/bbnf-simd/src/aarch64/classify_tbl4.rs:16-32`, `:47-65`), and `unescape_uxxxx` uses `vqtbl1q_u8` for hex nibble decode (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:74-120`, `:123-166`). Arm documents `vqtbl4q_u8` as TBL over four 16-byte tables and `vqtbx4q_u8` as TBX preserving an accumulator for out-of-range indices (`https://arm-software.github.io/acle/neon_intrinsics/advsimd.html`, table lookup and extended table lookup). Because current low-6 classification and fixed-width hex decode have scalar refs and parity shape, TBX is useful only if CSS escaped identifiers or non-fixed CSS escapes need graceful fallback in the same wave.

6. The string-special route is a wide-shift / context propagation candidate, not a standalone orphan. `string_block` currently scans 16-byte windows for terminator/escape/control/non-ASCII masks (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:30-72`) and parse-that consumes it in JSON string paths (`skinny/crates/parse-that-regex/src/lib.rs:472-478`, `:551-557`). The existing `byte_context` module is only `vextq_u8` one-byte neighbor propagation (`skinny/crates/bbnf-simd/src/aarch64/byte_context.rs:1-10`) and is demoted inventory unless wired into a 64-byte string-special consumer. Arm's Neon Intrinsics Reference maps `vextq_u8` to `EXT Vd.16B,Vn.16B,Vm.16B,#n` and fixed shifts like `vshlq_n_u16` to SHL (`https://arm-software.github.io/acle/neon_intrinsics/advsimd.html`, EXT and shift sections).

7. EOR3/SHA3 is `NOT-S-P3-ELIGIBLE` inventory for V2 research. Arm ACLE exposes `__ARM_FEATURE_SHA3` for SHA3 instruction availability and the Neon reference maps `veor3q_u8` to `EOR3 Vd.16B,Vn.16B,Vm.16B,Va.16B` (`https://arm-software.github.io/acle/main/acle.html`, SHA3 extension; `https://arm-software.github.io/acle/neon_intrinsics/advsimd.html`, EOR3 table). No S-P1 hot leaf identifies a three-input boolean expression after the V2/V4 folds: P1-E records CSS top leaves as timer/fact-sink and direct unicode as `unescape_string` rather than quote/escape/control fusion (`restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md:82-87`, `:105-118`). EOR3 should not be shortlisted unless S-P2/P3 can name a measured string-mask consumer.

8. x86 remains background only. The tree contains x86 AVX2/AVX-512/GFNI/VBMI2/VPCLMUL modules, but SK-V13 implementation scope is aarch64/Apple Silicon. Their only S-P2-C value is conceptual: GFNI/VBMI2/VPCLMUL show what single-op classify, compress, and carry can look like, but no SK-V13 wave should implement or benchmark x86.

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Rows stamped `NOT-S-P3-ELIGIBLE` are retained as inventory or close-hygiene
facts only. They are not S-P3 shortlist authority unless a later research fold
adds a named S-P1 hot expression plus scalar reference, checkasm/parity, and a
same-wave row consumer.

| Candidate | Shape | Arch / feature | Scalar reference | Checkasm / parity expectation | Same-wave consumer | P1 antecedent | S-P3 disposition |
|---|---|---|---|---|---|---|---|
| C-P2C-1 `ascii_set_member64_css_delimiter` | 64-byte window, generated ASCII delimiter set, return first member offset / end. Backed by `byte_class_from_eq_set_64` mask plus first-set extraction. | AArch64 NEON TBL/eq-set; no x86 scope. | Existing W4 scalar member-find / byte-set oracle; SK-V12 microbench artifact. | Existing `checkasm_ascii_set_member_find_64` plus CSS delimiter, tail 0..63, duplicate, high-bit, no-hit, early-hit cases. | CSS `scan_block` delimiter search in generated declaration-values runtime; strict equality vs lightningcss and cssparser/golden oracle in same wave. | CSS `scan_block` byte loop at `generated.rs:38-58`; W4 microbench 4.72x. | Highest-priority ASM production candidate; no second deferral permitted. |
| C-P2C-2 `pmull_cssc_structural_union_emit64` | Build structural-position matrix from 64-byte mask, compute prefix/string state with PMULL-compatible bit algebra, extract positions by CTZ/rank order, emit class+position tuple directly. | AArch64 PMULL + CSSC CTZ; fallback scalar. | Existing scalar `prefix_xor_64`, `bitmap_next_set_bit_scalar`, `bulk_emit_positions_64_scalar`, and current scalar consume path. | New checkasm matrix: all densities, quotes/backslashes, carry in/out, aligned/unaligned windows, escaped quote cases, structural-set permutations; must retain existing checkasm for prefix/next/bulk. | Union-C generated structural consumer or JSON direct structural projection in same wave; must show row movement and guard floors. | Structural SIMD/scalar 1.49-5.04x; JSON scan currently calls scalar delegates through `scan.rs:203-267`. | High-risk only if S-P3 wants the required fresh union attempt; must cite REDRESS 88/89/96/97/98. |
| C-P2C-3 `udot_digit_span_x4` | Validate/decode four independent 4-digit lanes or digit-run heads; output value(s) and validity mask for numeric token consumer. | AArch64 DotProd (`udot` / `vdotq_u32`) behind `target_feature=dotprod`. | `parse_4_digits` scalar in `digit_mac.rs:5-22` plus overflow/invalid token oracle. | Expand smoke tests into strict checkasm: invalid lanes, signs, decimal point, exponent edge, overflow, mixed-valid lanes, tail lengths. | JSON numeric direct/parse consumer or CSS number token consumer; only if numeric-density and row measurement move a JSON/CSS row. | Direct envelopes for `canada`, `numbers`, `mesh`; P1-D direct c/B and mode-III counters. | Medium/high risk; likely measured-reject unless S-P2-E proves numeric leaf density. |
| C-P2C-4 `tbl_tbx_escape_decode_batch` | Batch hex escape/classification using TBL; TBX variant only when out-of-range bytes need accumulator-preserving fallback. | AArch64 TBL/TBX. | `unescape_uxxxx_scalar` and grammar-specific escape oracle. | Existing unicode escape parity plus fixed-width x4/xN, invalid nibbles, surrogate pairs, CSS variable-length escape terminator and whitespace rules if CSS consumes it. | JSON `parse_that_regex::unescape_string` or CSS escaped identifier/value parser in same wave. | `unicode_escapes` direct rank-1 `unescape_string` 46.7%; `y_string_unicode` parse rank-1 `read_hex_unit_scalar`. | Selectable for JSON fixed-width `\uXXXX`; CSS needs grammar-policy proof first. |
| C-P2C-5 `string_special_64_context` | Four 16-byte string-special blocks as one 64-byte oracle; use `vextq_u8` for cross-chunk neighbor context and fixed shifts/OR for mask assembly. | AArch64 NEON EXT + shifts; no SVE. | 64-byte scalar oracle built from current 16-byte scalar block and byte-context scalar. | Alignment, tails, cross-chunk quote/escape/control, long backslash runs, non-ASCII, grammar quote/escape policy cases. | JSON string scanner or CSS string/escaped-ident scanner in same wave; row movement required. | `distinct_values` tiny-string parse sidecar, unicode/string direct gaps, existing parse-that string-block consumers. | Conditional; good support for C-P2C-4 or Union-C, not a standalone support landing. |
| C-P2C-6 `eor3_string_mask_fusion` | Fuse three same-width masks, e.g. quote ^ escape ^ control, with one EOR3 instruction. | AArch64 SHA3/EOR3 (`__ARM_FEATURE_SHA3`). | Scalar `a ^ b ^ c` over u8x16/u64 masks. | Exhaustive three-mask truth table for 16 lanes plus corpus parity for consumer. | Only a measured string mask consumer. | No current S-P1 three-input hot expression. | `NOT-S-P3-ELIGIBLE` in V2. Inventory only; do not shortlist without new hot-expression evidence. |
| C-P2C-7 `byte_context_orphan_resolution` | Either wire `byte_context` into C-P2C-5 or delete/demote it with REDRESS evidence; do not retain as support-only. | AArch64 EXT. | Current tests / scalar neighbor context. | If wired, include C-P2C-5 checkasm; if deleted/demoted, REDRESS inventory proof. | C-P2C-5 consumer or none. | REDRESS-126 orphan inventory; no P1 production caller. | `NOT-S-P3-ELIGIBLE` as a standalone wave. Close hygiene only; may be folded into C-P2C-5 consumer or deletion/demotion evidence. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Grammar-neutral verdict | Notes |
|---|---|---|
| C-P2C-1 | Generalisable byte-set primitive. | The primitive knows only `set: &[u8]` and a byte window. CSS supplies `{`, `}`, `;`; JSON could supply structural bytes; Sheets/BBNF-self could supply delimiters. Comments, recovery, and semantic roles stay in generated grammar code. |
| C-P2C-2 | Generalisable only if structural set and emitted tuple schema are grammar parameters. | Lock 1 is preserved if the emitted projection is the tape/union itself, not a parallel sidecar. JSON/CSS/Sheets must each provide structural byte policy and tuple fields through GrammarConfig/cost model. |
| C-P2C-3 | Generalisable decimal digit primitive. | JSON numbers, CSS numeric tokens, Sheets numeric literals, and BBNF numeric grammar sites can consume it, but sign/decimal/exponent policy is grammar-owned. |
| C-P2C-4 | Partially generalisable. | Fixed-width JSON `\uXXXX` is not the same as CSS variable-length escapes. The TBL hex-nibble core is neutral; the escape-language wrapper is per grammar. |
| C-P2C-5 | Generalisable if quote, escape, control-limit, and non-ASCII policy are parameters. | Current JSON constants must move through GrammarConfig before non-JSON consumption. CSS comments and identifiers are caller policy, not SIMD policy. |
| C-P2C-6 | Generalisable bit-mask algebra, but currently unsupported by evidence. | Three-input boolean fusion is neutral; absent a named hot expression, it is not selectable. |
| C-P2C-7 | Hygiene only. | `byte_context` is neutral as an EXT helper but becomes admissible only through a grammar-neutral string/context consumer. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

- REDRESS 88: PMULL prefix-XOR as a default production hot body regressed JSON rows. C-P2C-2 must distinguish itself by deleting scalar structural consumption through a SIMD-first union consumer; a local PMULL replacement for `bitmap_prefix_xor_64` is rejected.
- REDRESS 89: CSSC CTZ / bulk emit failed as a standalone bulk consumer. C-P2C-2 must prove CTZ is rank-order extraction inside a measured union consumer; C-P2C-7 must not revive `bitmap_next_set_bit` or `bulk_emit_positions_64` as support-only.
- REDRESS 96/97/98: union substrate variants regressed or failed CHALLENGE. Any Union-C route must cite these and name the material differential: SIMD-first direct tuple writeback, not class-column, streaming cursor, or class-lane-only substrate.
- REDRESS 82-84 and SK-V10 unicode proof-only limits: C-P2C-4 must not land a single-quartet unicode classifier without a row-moving consumer. JSON fixed-width escape and CSS variable-length escape must not be conflated.
- REDRESS 60-72: no sidecar event vector, parser-owned cursor, or decoded-string stats sink. C-P2C-2 and C-P2C-5 must preserve Lock 1 and generated grammar ownership.
- REDRESS 119/120: direct-row fixpoints are history under the addendum but still require material differential for every reopen. Generated direct-envelope profiles are not proof that a SIMD primitive will move a row.
- REDRESS 126: five aarch64 orphans were demoted with evidence. SK-V13 may wire or delete/demote, but cannot add or retain another orphan. `cache_hints` has no P1 hot-leaf antecedent and should remain non-selectable unless a later P1 rerun names a store/prefetch leaf.

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- Arm C Language Extensions 2026Q1, feature macros and availability: `https://arm-software.github.io/acle/main/acle.html` (`__ARM_FEATURE_DOTPROD`, `__ARM_FEATURE_CSSC`, `__ARM_FEATURE_SHA3`, `__ARM_FEATURE_CLZ`).
- Arm Neon Intrinsics Reference 2026Q1: `https://arm-software.github.io/acle/neon_intrinsics/advsimd.html` (`vqtbl4q_u8`, `vqtbx4q_u8`, `vdotq_u32`, `vmull_p64`, `vextq_u8`, `vshlq_n_u16`, `veor3q_u8`).
- Arm A64 / Armv8 instruction overview PDF supplied by orchestrator: `https://developer.arm.com/-/media/Files/pdf/graphics-and-multimedia/ARMv8_InstructionSetOverview.pdf`.
- Arm Compiler for Embedded Reference Guide 6.20 supplied by orchestrator: `https://documentation-service.arm.com/static/641d6f0fac798355e7426b54`.
- FFmpeg checkasm source: `https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html`; checkasm records function versions/perf state and reports OK/FAILED, matching the scalar-reference + candidate + benchmark discipline bbnf carries in Lock 16.

V2 citation tightening for S-P3 gate text:

- CSSC CTZ: cite the Arm ACLE `__ARM_FEATURE_CSSC` feature macro and the CTZ
  instruction availability. Gate text must also state the scalar fallback
  (`trailing_zeros` or `rbit` + `clz`) because there is no portable NEON
  intrinsic name equivalent to `vctzq`.
- PMULL: cite Neon polynomial multiply entries `vmull_p64` and
  `vmull_high_p64`; any prefix/string-state body must retain the scalar
  prefix-XOR oracle and REDRESS 88 differential.
- UDOT: cite `__ARM_FEATURE_DOTPROD` and Neon `vdotq_u32`
  (`UDOT Vd.4S,Vn.16B,Vm.16B`); admission still needs strict digit-lane
  checkasm and numeric-row consumer.
- TBL/TBX: cite Neon `vqtbl4q_u8` and `vqtbx4q_u8`; TBX is a fallback
  refinement only when the grammar's escape/classification policy needs
  accumulator-preserving out-of-range handling.
- EXT: cite Neon `vextq_u8`; admissible only through a 64-byte string/context
  consumer or deletion/demotion proof, not as `byte_context` inventory.
- EOR3: cite `__ARM_FEATURE_SHA3` and Neon `veor3q_u8`; V2 marks it
  `NOT-S-P3-ELIGIBLE` until a named three-input hot expression exists.
- `restart/prompts/skinny/PASS-2-RESEARCH.md`.
- `restart/locks/LOCKS.md` Lock 1, Lock 14, Lock 16.
- `restart/skinny/tranches/sk-v13/HANDOFF.md`.
- `restart/skinny/tranches/sk-v13/SYNTHESIS.md`.
- `restart/skinny/USER-PIN-ADDENDUM-2026-05-21-FULL-SOTA.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-simd-asm-union.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-value-api-union.md`.
- `restart/skinny/tranches/sk-v13/scoping/sk-v13-scoping-profile-truth.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1a-samply-mode-1.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1b-samply-mode-2.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1c-samply-mode-3.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1d-pmu-cycles.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1e-hot-leaf-attribution.md`.
- `restart/skinny/tranches/sk-v13/research/p1/p1f-results-delta.md`.
- `restart/skinny/tranches/sk-v13/research/p1/support/evidence-ledger-v3.md`.
- `skinny/RESULTS.md`.
- `skinny/REDRESS.md`.
