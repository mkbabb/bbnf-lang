# SK-V10 P2-B: DAV1D/FFmpeg ASM Process

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: map the dav1d/FFmpeg hand-written-ASM checkasm discipline onto bbnf-simd and produce the primitive-admission process S-P3 must gate against.
Output: this file.
P1 hot-leaf antecedents: `string_tiny_scan`, `string_full_scan`, `string_escape`, `unicode_escape_hex`, `number_digit_scan`, `whitespace_skip`, `array_walk`, `object_walk`, `simd_movemask`, `alloc`.
Lock surface: both Lock 1 and Lock 14.

## V1 Challenge Fold

`p2g-candidate-ledger.md` is the post-CHALLENGE eligibility authority for S-P3.
The primitive names in this file are process evidence and alias vocabulary until
that ledger marks a canonical candidate `row-gated` or `proof-only`.
`mask_next_and_emit_positions_64` is demoted to REDRESS-blocked inventory for
SK-V10 because its current antecedent is trailing-zero visibility rather than an
accepted product-plane hot-leaf class. X86-only or future-host process examples
are inventory-only on the Apple aarch64 host.

P1 anchor repairs: the direct row routes in finding 3 also use
`p1e-hot-leaf-attribution.md:55` (`canada` digit scan), `:58`
(`update_center` tiny string), `:59` (`mesh` digit/array), and `:64`
(`numbers` digit scan), plus the existing anchors for `twitter`,
`github_events`, `gsoc-2018`, `instruments`, `unicode_mixed`, and
`unicode_escapes`. `whitespace_skip_mask_64` is grounded in the class-map at
`p1e-hot-leaf-attribution.md:44` and the `mesh` direct profile at
`p1b-samply-mode-2.md:85`.

Process citation repair: DAV1D checkasm process claims are background unless a
wave plan carries pinned source anchors. The pinned dav1d 1.5.1 source
`3060ebf8dd26952579373084984daf70a54f5368/tests/checkasm/checkasm.c` lists DSP
families at lines 69-95, CPU flag families at lines 98-128, CPU flag iteration
at lines 814-868, and buffer overwrite checks at lines 1110-1128. FFmpeg
checkasm process claims remain anchored to the 8.0 doxygen `checkasm.c` and
`checkasm.h` sources cited below.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

1. SK-V10 is authorized for S-P2 research but not source implementation. The
   handoff says S-P1 is closed, W3 union substrate is retired, the primary JSON
   frontier is `direct_to_struct`, typed product-plane generalization is bounded,
   and parse-only SOTA is retired
   (`restart/skinny/tranches/sk-v10/HANDOFF.md:5`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:6`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:7`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:8`). It also refuses any wave that
   reopens W3, treats parse-only rows as SOTA admissions, scopes a substrate or
   kernel wave without same-host micro-proof, edits source before S-P3, or leaks
   JSON-only policy into generic crates
   (`restart/skinny/tranches/sk-v10/HANDOFF.md:75`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:77`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:78`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:80`,
   `restart/skinny/tranches/sk-v10/HANDOFF.md:82`).

2. The current close target is not "make the scanner faster." The synthesis
   records 17 parse-only rows as planning evidence only, 14 direct-to-struct
   NO-GO rows as the primary JSON frontier, and 6 real typed rows as the
   product-plane SOTA surface
   (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:50`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:52`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:53`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:54`). It explicitly requires
   unicode/string kernels to stay on existing call sites and pass isolated
   same-host micro-benchmark proof before S-P3 scoping
   (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:97`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:145`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:149`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:150`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:152`).

3. P1 hot leaves are row-specific, not a single substrate diagnosis. P1-E maps
   `string_tiny_scan`, `string_full_scan`, `string_escape` /
   `unicode_escape_hex`, `number_digit_scan`, `whitespace_skip`, `array_walk` /
   `object_walk`, `simd_movemask`, and `alloc` to rows
   (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:29`,
   `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:38`).
   The direct-row table then routes `twitter`, `github_events`,
   `update_center`, and `instruments` to tiny-string work; `unicode_mixed` and
   `unicode_escapes` to full string/escape work; `canada`, `mesh`, and
   `numbers` to digit scanning; and `gsoc-2018` to `simd_movemask` plus string
   split
   (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:53`,
   `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:57`,
   `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:63`,
   `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:65`,
   `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:66`,
   `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:61`).

4. P1-C already proves why primitive-only wins are insufficient. The SIMD
   structural scan beats scalar on every row, but P1-C states that this cannot
   reopen W3 because SK-V9 W3 regressed must-improve and maintain rows; any
   future candidate must micro-prove both the primitive and the product-plane
   caller that consumes it
   (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:111`,
   `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:112`,
   `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:115`).
   Eager decoded-value materialization is also slower on every row and is
   barred as the route for direct-plane interventions
   (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:77`,
   `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:79`).

5. bbnf-simd already has the right checkasm skeleton. `checkasm_common`
   provides deterministic xorshift generation and stack-canary guarding
   (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:3`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:50`). Its AArch64 path can
   assert callee-saved register preservation
   (`skinny/crates/bbnf-simd/tests/checkasm_common.rs:83`,
   `skinny/crates/bbnf-simd/tests/checkasm_common.rs:107`). The general
   parity test compares dispatched candidate output to scalar output, verifies
   the candidate did not mutate its input, sweeps alignment, supports deliberate
   bug injection, and makes corpus parity strict
   (`skinny/crates/bbnf-simd/tests/checkasm_parity.rs:13`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:16`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:20`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:200`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:209`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:233`,
   `skinny/crates/bbnf-simd/tests/checkasm_parity.rs:345`).

6. bbnf-simd's latest ASM-facing primitive shows the desired scalar-oracle
   shape. `byte_class_from_eq_set_64` declares the scalar reference as the
   executable specification and requires every active backend to agree bit for
   bit
   (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:14`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:16`).
   Its test has signal trapping, stack guarding, alignment sweep, set-size
   sweep, adversarial seeds, corpus parity, and edge cases
   (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:90`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:129`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:189`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:224`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:261`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:300`,
   `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:376`).
   The x86 wrapper feature-gates the AVX-512 BW FFI by `target_arch` and
   `target_feature`, and otherwise falls back to scalar
   (`skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.rs:16`,
   `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.rs:37`,
   `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.rs:50`).

7. The current primitive dispatch surface is small and explicit:
   `byte_class_from_table_64`, `bitmap_prefix_xor_64`, `bitmap_next_set_bit`,
   `bulk_emit_positions_64`, and `eob_pad_clamp`
   (`skinny/crates/bbnf-simd/src/dispatch.rs:49`,
   `skinny/crates/bbnf-simd/src/dispatch.rs:51`,
   `skinny/crates/bbnf-simd/src/dispatch.rs:55`). On AArch64 these dispatch to
   NEON implementations; elsewhere they fall back to scalar
   (`skinny/crates/bbnf-simd/src/dispatch.rs:63`,
   `skinny/crates/bbnf-simd/src/dispatch.rs:66`,
   `skinny/crates/bbnf-simd/src/dispatch.rs:77`).

8. The dav1d/FFmpeg lesson is process, not a permission slip for ASM. FFmpeg's
   checkasm keeps per-function failure accounting, signal handling, and
   benchmark gating; failed functions are marked not-ok and counted, and fatal
   signals are converted into checkasm failures rather than silent crashes
   (FFmpeg `checkasm.c`, lines 1093-1140 in the 8.0 doxygen source). FFmpeg's
   `checkasm.h` declares buffer comparison helpers and padded overwrite checks
   for candidate-vs-reference validation (FFmpeg `checkasm.h`, trunk source
   around lines 448-483). dav1d runs a checkasm matrix over per-DSP families and
   CPU flags; the pinned dav1d 1.5.1 source registers DSP families and x86 /
   ARM feature families before iterating CPU flags
   (`3060ebf8dd26952579373084984daf70a54f5368/tests/checkasm/checkasm.c`,
   lines 69-128 and 814-868). The 2025 dav1d
   checkasm migration rationale also names more robust runtime scaling, outlier
   rejection, variance reporting, and maintainability as goals (VideoLAN dav1d
   MR !1812).

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

The admission process S-P3 should enforce before any SIMD/ASM primitive enters a
wave:

1. Name the P1 antecedent and target corpus rows. A primitive with no P1 hot
   leaf is rejected as speculative.
2. Write or identify the scalar oracle first under `bbnf-simd::scalar`. The
   scalar body is the executable specification, not a fallback after the SIMD
   body exists.
3. Add a checkasm differential harness before integration: deterministic random
   cases, boundary cases, alignment windows, signal guard where applicable,
   stack canary, ABI/callee-saved checks for AArch64 ASM, corpus-window parity,
   and strict failure in CI.
4. Gate by feature and target at the wrapper boundary. Unsupported hosts must
   execute the scalar oracle, not a partially compiled kernel.
5. Name the same-wave consumer and micro-benchmark the primitive and caller
   together on the target host before product integration.
6. Reject if the consumer is a sidecar scan, union substrate, decoded scratch
   materializer, parser-owned class table, or parse-only SOTA claim.

| Candidate | Shape | Scalar-ref status | Arch | P1 antecedent | Admission verdict |
|---|---|---|---|---|---|
| `tiny_plain_string_first_special_64` | Given a source window, caller-owned delimiter/escape byte sets, and per-grammar stop-set table, return first stop offset plus mask for up to 64 bytes; consumer must be an existing direct/typed string scanner. | Existing scalar anchors cover tiny-string membership/first-match in the SK-V3 checkasm compile anchors; `p2g-candidate-ledger.md` marks this proof-only until the exact scalar oracle, checkasm target, row gate, and caller are named. | AArch64 NEON first; x86 routes are inventory-only for SK-V10. | `string_tiny_scan` on `twitter`, `github_events`, `update_center`, `instruments`, `distinct_values`; P1-E lines 53, 57, 58, 63, 68. | Conditionally admissible. Admit only with scalar oracle + checkasm + same-wave direct/typed string caller. Reject if framed as a parser sidecar or W3 consumer. |
| `string_full_scan_escape_control_64` | For a 64-byte block, produce quote, backslash, control, and optional non-ASCII masks without materializing decoded output. | Partial scalar ingredients exist in table/equality-class and prefix-mask primitives; exact multi-mask scalar oracle must be written first. | AArch64 NEON or x86 AVX2/AVX-512. | `string_full_scan`, `string_escape` on `unicode_mixed`, `unicode_escapes`, `y_string_unicode`; P1-E lines 65-69. | Conditionally admissible. It must reduce existing string/unescape caller work and must not route through eager decoded scratch, which P1-C rejects. |
| `unicode_escape_hex4_decode` | Decode exactly four ASCII hex bytes to a 16-bit code unit and validity mask; optional paired-surrogate helper remains scalar unless checkasm proves a vector shape. | Existing scalar anchor for `unescape_uxxxx_scalar` is compile-checked, but S-P3 must bind it to a bbnf-simd scalar module and exact differential harness. | AArch64 NEON table/nibble path; x86 SSSE3/AVX2 nibble table if host-relevant. | `unicode_escape_hex` on `unicode_escapes`, `unicode_mixed`; P1-E lines 42, 66. | Conditionally admissible for existing unescape call sites only. Reject any decoded-string arena or scratch route under REDRESS 66-69. |
| `number_digit_run_classify_64` | Return digit mask, first non-digit offset, and optional decimal/exponent class masks for a 64-byte number scan window. | Scalar oracle would be simple byte-class + state sketch, but no current same-wave consumer is proven. | AArch64 NEON; x86 AVX2/AVX-512. | `number_digit_scan` on `canada`, `mesh`, `numbers`, `marine_ik`; P1-E lines 55, 59, 64, 62. | Research-only / not admitted by P2-B. The hot leaf is real, but Canada typed is pre-blocked and numeric rows need caller proof; reject if proposed from isolated scan speed alone. |
| `whitespace_skip_mask_64` | Produce ASCII whitespace mask and next non-whitespace offset for grammar-provided whitespace set. | Scalar oracle trivial; checkasm feasible. | AArch64 NEON; x86 AVX2/AVX-512. | `whitespace_skip` on `citm_catalog`, `random`, `mesh`, `marine_ik`, `instruments`; P1-E lines 54, 60, 62, 63. | Maintain-only unless paired with a same-wave direct/typed caller. Reject as a standalone primitive because `citm_catalog` is already a maintain row and W10-style regressions are barred. |
| `structural_movemask_64` / `byte_class_from_eq_set_64` extension | Produce structural membership masks for grammar-provided byte sets; current x86 AVX-512 BW equality-set shape already exists. | Strong. `byte_class_from_eq_set_64_scalar` is the executable spec and current tests enforce bit parity. | Existing x86 AVX-512 BW wrapper; AArch64 equivalent only if scalar/checkasm/consumer exists. | `simd_movemask` on `gsoc-2018`, secondary on `twitter`, `github_events`, `update_center`; P1-E lines 46, 61. | Reject as a product wave unless the consumer is typed-root/direct-output work, not W3. Current P1-C says structural scan speed alone cannot reopen W3. |
| `mask_next_and_emit_positions_64` | Fuse next-set-bit and bulk position emission for dense masks while preserving canonical tape offset writes. | Existing scalar oracles for `bitmap_next_set_bit_scalar` and `bulk_emit_positions_64_scalar` exist, but the fused shape is not S-P3 eligible. | AArch64 NEON/CSSC CTZ if available; x86 BMI2/AVX2. | Demoted to REDRESS-blocked inventory: no accepted SK-V10 P1 hot-leaf class plus live direct/typed consumer currently justifies it. | Reject for SK-V10. REDRESS already pre-blocks next-bit bulk consumer and sidecar routes. |
| `allocation_elision_string_materializer` | Not a SIMD primitive; changes direct field materialization semantics or output contract. | Scalar reference would be product-level, not bbnf-simd. | N/A. | `alloc` on `y_string_unicode`; P1-E lines 47, 84-88. | Reject from P2-B. P1-C shows eager decode is slower, and REDRESS 66-69 exhaust direct source-hook/scratch families under the current digest workload. Route to P2-D/P2-E if the output contract changes. |
| `array_object_walk_dispatch_hint` | Not an ASM primitive; a control-path or generated-code change for object/array walking. | Product-level scalar/control oracle required outside bbnf-simd. | N/A. | `array_walk` / `object_walk` on numeric and mixed rows; P1-E lines 45, 62. | Reject from bbnf-simd admission. It may be S-P3 direct-output work, but it is not a dav1d-style leaf kernel unless reduced to a grammar-neutral byte/mask primitive with a same-wave consumer. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

| Candidate | Grammar-neutral verdict |
|---|---|
| `tiny_plain_string_first_special_64` | Generalisable if the stop set is grammar-provided bytes and the primitive returns masks/offsets only. JSON-only if it bakes quote/backslash policy or string validity rules into bbnf-simd. |
| `string_full_scan_escape_control_64` | Generalisable as byte-class masks for quote, escape, control, and non-ASCII classes. CSS strings, Sheets literals, and BBNF-self token text can all consume the same mask vocabulary with grammar-owned policy. |
| `unicode_escape_hex4_decode` | Generalisable as fixed-width hex nybble decode plus validity. JSON's `\u` policy, CSS escape length policy, and Sheets string policy must remain outside the primitive. |
| `number_digit_run_classify_64` | Generalisable as digit/sign/dot/exponent classification only. JSON number grammar, CSS numeric dimensions, and Sheets numeric coercion must remain grammar-owned. |
| `whitespace_skip_mask_64` | Generalisable if whitespace membership is a caller-provided byte set. JSON-only if it hardcodes exactly JSON whitespace. |
| `structural_movemask_64` / `byte_class_from_eq_set_64` extension | Already grammar-neutral in shape: byte-set membership over a 64-byte block. The consumer decides whether bytes are JSON structural bytes, CSS delimiters, Sheet separators, or BBNF punctuation. |
| `mask_next_and_emit_positions_64` | Generalisable as bitmap-to-offset emission. It violates Lock 1 only if used to create a parallel structural projection or sidecar producer. |
| `allocation_elision_string_materializer` | JSON/product-specific under the current shape; not a bbnf-simd grammar-neutral primitive. |
| `array_object_walk_dispatch_hint` | JSON/product-specific unless reframed as a generic byte/mask/control primitive. |

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

1. Do not reopen W3 union substrate or a renamed class-column/event substrate.
   SK-V10 synthesis records W3 as falsified and REDRESS 98 retired it
   (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:69`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:71`). The handoff makes this a
   refusal condition (`restart/skinny/tranches/sk-v10/HANDOFF.md:75`).

2. Do not use parse-only rows or isolated structural-scan wins as SOTA
   admission. Parse-only is planning evidence only, and P1-C explicitly says the
   isolated SIMD scanner is not enough to reopen W3
   (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:52`,
   `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:111`).

3. Do not revive eager decoded scratch, parser-owned decoded materialization, or
   direct string source-hook folding. P1-C shows eager decode is slower on every
   row (`restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md:77`),
   and P1-E specifically blocks using the `y_string_unicode` allocator leaf as
   authorization for decoded scratch
   (`restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md:84`).

4. Do not treat a feature-gated SIMD/ASM body as admitted when scalar fallback
   is the only path on the host. The current dispatch model explicitly falls
   back to scalar outside its target feature surface
   (`skinny/crates/bbnf-simd/src/dispatch.rs:77`,
   `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.rs:50`).

5. Do not admit an ASM primitive without a same-wave consumer. REDRESS records
   same-wave runtime consumers as mandatory for admitted primitives
   (`skinny/REDRESS.md:1243`), while SK-V10 synthesis requires micro-proof,
   host flags, target input slices, expected consumer, and failure threshold
   before source redress (`restart/skinny/tranches/sk-v10/SYNTHESIS.md:145`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:149`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:152`,
   `restart/skinny/tranches/sk-v10/SYNTHESIS.md:153`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

- `restart/prompts/skinny/PASS-2-RESEARCH.md`
- `restart/skinny/tranches/sk-v10/HANDOFF.md`
- `restart/skinny/tranches/sk-v10/SYNTHESIS.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1a-samply-mode-1.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1c-samply-mode-3.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1d-pmu-cycles.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1e-hot-leaf-attribution.md`
- `restart/skinny/tranches/sk-v10/research/p1/p1f-results-delta.md`
- `restart/skinny/tranches/sk-v10/research/p1/hardening/HARDENING-S-P1-V1-CONSOLIDATED.md`
- `skinny/RESULTS.md`
- `skinny/REDRESS.md`
- `skinny/crates/bbnf-simd/tests/checkasm_common.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_parity.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs`
- `skinny/crates/bbnf-simd/tests/checkasm_eob_pad_clamp.rs`
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/src/scalar/byte_class_from_table_64.rs`
- `skinny/crates/bbnf-simd/src/scalar/bitmap_next_set_bit.rs`
- `skinny/crates/bbnf-simd/src/scalar/bitmap_prefix_xor_64.rs`
- `skinny/crates/bbnf-simd/src/scalar/bulk_emit_positions_64.rs`
- `skinny/crates/bbnf-simd/src/scalar/eob_pad_clamp.rs`
- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.rs`
- `skinny/crates/bbnf-simd/src/x86_64/byte_class_from_eq_set_64.asm`
- FFmpeg checkasm source:
  <https://ffmpeg.org/doxygen/8.0/checkasm_8c_source.html>
- FFmpeg checkasm header source:
  <https://ffmpeg.org/doxygen/trunk/checkasm_8h_source.html>
- dav1d checkasm main source:
  <https://raw.githubusercontent.com/videolan/dav1d/master/tests/checkasm/checkasm.c>
- dav1d checkasm migration MR:
  <https://code.videolan.org/videolan/dav1d/-/merge_requests/1812>
