# SK-V10 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: enumerate the SIMD/string/number/regex primitive gaps demanded by the accepted SK-V10 S-P1 hot leaves.
Output: this file.
P1 hot-leaf antecedents: `string_tiny_scan`, `string_full_scan`, `string_escape`, `unicode_escape_hex`, `number_digit_scan`, `number_scan`, `whitespace_skip`, `alloc`, and the REDRESS-routed `simd_movemask`/structural leaf.
Lock surface: both Lock 1 and Lock 14.

## §1 - Findings

`parse-that-regex` is no longer a pure scalar crate. It already calls aarch64 `bbnf-simd` helpers for trusted string block scans, UTF-8 block validation, and batched `\uXXXX` decode inside private control paths (`skinny/crates/parse-that-regex/src/lib.rs:386`, `:461`, `:547`, `:775`). The gap is therefore not "add SIMD somewhere"; the gap is a grammar-neutral Layer-1 vocabulary that generated direct/typed code can consume without cloning private JSON helper loops.

The S-P1 product-plane hot leaves are concrete. Direct rows are dominated by generated tiny/plain string loops, full string/escape decode, number scans, whitespace, and one allocator leaf (`restart/skinny/tranches/sk-v10/research/p1/p1b-samply-mode-2.md:79-95`; `p1e-hot-leaf-attribution.md:40-47`). Typed rows preserve six `A / GO` rows, but their hot leaves still include typed direct string skips and number scans (`p1b-samply-mode-2.md:101-106`). `skinny/RESULTS.md` makes the target frontier binding: 14 `direct_to_struct` rows are `N-direct / NO-GO`, while all parse-only rows are diagnostic `S / NO-GO` and cannot be used as SOTA admissions.

The generated/runtime duplication is visible. JSON generated direct has `match_tiny_plain_string_with_cap::<8>` and retained has `::<16>` (`skinny/crates/runtime/src/grammars/json/generated.rs:159-185`). Typed direct generated code emits separate `tiny_plain_string_end` and `skip_plain_string_end` loops with 32/96-byte caps (`skinny/crates/codegen/src/typed_direct.rs:634-660`; generated instance at `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1344-1370`). These loops do not consume a shared `parse-that` primitive even though their semantics are grammar-neutral: find the first terminator, escape, or control byte under a bounded cap.

The number path has a similar shape. `match_number_span_from_first` and private `scan_digit_run` fuse JSON numeric grammar, digit-run validation, mantissa accumulation, and decimal exponent accounting (`skinny/crates/parse-that-regex/src/number/mod.rs:38-106`). That is correct for parse semantics, but S-P1 names `number_digit_scan` and `number_scan` as independent hot leaves; the primitive vocabulary does not expose a lower-level digit-run/classify/MAC operation for direct or typed consumers.

`skip_ascii_whitespace` is public but scalar/SWAR and policy-shaped around JSON whitespace (`skinny/crates/parse-that-regex/src/lib.rs:113-147`). S-P1 names it on direct and typed rows, but the route is a maintain/secondary candidate because `citm_catalog` is already admitted and S-P1 labels it a maintain row (`p1e-hot-leaf-attribution.md:54`).

The allocator leaf on `y_string_unicode` is not a SIMD primitive by itself. Direct `DirectParser::string` returns `Cow<'a, str>` and calls `unescape_string` when the span needs decode (`skinny/crates/bbnf-bench/src/direct_struct.rs:541-560`); `unescape_string` allocates `String::with_capacity` for escaped content (`parse-that-regex/src/lib.rs:718-728`). P1-C proves eager decoded-value materialization is slower on every probed row, so any candidate here must be a consumer-owned output-plane operation, not a parser-owned scratch or retained sidecar (`p1c-samply-mode-3.md:77-87`).

Lock 1 and REDRESS 98 block structural-projection routes. `simd_movemask` appears in profiles, but REDRESS 96 and 97 falsified union/class structural consumption, and REDRESS 98 retires `G-W3-UNION-SUBSTRATE`. P2-E therefore does not nominate a parse-that structural cursor primitive for SK-V10.

## §2 - Candidate Primitives

| Candidate | Shape | Scalar reference sketch | Layer placement | Arch | P1 antecedent | Same-wave consumer |
|---|---|---|---|---|---|---|
| `bounded_plain_string_end` | `fn bounded_plain_string_end(input: &[u8], quote: usize, cap: usize, terminator: u8, escape: u8, control_limit: u8) -> PlainEnd { end: Option<usize>, reason: EndReason }` over the existing bytes, with `cap` supplied by codegen. | Loop from `quote + 1` to `min(quote + 1 + cap, len)`, return `Some(i + 1)` on terminator, `NeedsFull` on escape/control, `NoHit` at cap/end. This is the scalar form of generated JSON `match_tiny_plain_string_with_cap` and typed `skip_plain_string_end`. | Layer 1 grammar-neutral string token primitive. It may consume Layer 0 table/movemask bodies, but its public contract is byte-set/cap based. | Primary aarch64 NEON via `string_block::scan_string_special_block`; x86 AVX2/AVX-512 later via byte-class masks. | `string_tiny_scan` on `twitter`, `github_events`, `instruments`, `update_center`, `distinct_values`, typed `twitter`/`apache_builds`/`update_center`. | Generated JSON direct parser and typed direct generator in the same wave; retained parse is not a consumer for this candidate. |
| `plain_string_special_span` | `fn plain_string_special_span(input, start, mode, classes) -> PlainStringScan` exported from parse-that with flags for terminator, escape, control, non-ASCII, and UTF-8 validation status. | Existing scalar fallback in `skip_string_plain`/`skip_string_plain_trusted`: 8-byte SWAR masks, then byte loop; preserve exact error offsets. | Layer 1 string scanner. Layer 0 is the block classifier (`StringSpecialBlock`) and UTF-8 block validator. | Already partly aarch64-backed; x86 version can lower to AVX2 `pshufb`/AVX-512 byte-class once admitted. | `string_full_scan` on `unicode_mixed`, `unicode_escapes`, Track 2 `unicode_basic`. | Direct string parse paths that currently call `match_string_at_quote_trusted_utf8`; must micro-prove at direct rows, not parse-only rows. |
| `escape_run_decode_x4` | `fn decode_escape_run_x4(input, slash) -> EscapeRun { scalars, bytes_consumed }`, validating four contiguous `\uXXXX` units and surrogate joins without writing to a `String`. | Scalar loop over up to four escapes: check `\u`, call `read_hex_unit_scalar`, validate surrogate pairing, emit Unicode scalar values to a fixed stack array. | Layer 1 codec primitive. Layer 0 is nibble decode/table lookup (`unescape_uxxxx`) only. | aarch64 NEON TBL exists privately; x86 can use SSSE3/AVX2 shuffle or AVX-512 VBMI2 later. | `string_escape` / `unicode_escape_hex` on `unicode_escapes`, `unicode_mixed`, `y_string_unicode`. | Only a direct/typed escaped-string consumer that folds or writes decoded scalars in the same loop; no standalone parse-only quartet admission. |
| `string_segments_fold` | `fn fold_decoded_string_segments(raw, consumer) -> Result<FoldStats>` where the consumer receives borrowed plain segments and decoded scalar chunks; no retained decoded scratch. | Walk `raw`; copy no plain bytes; for each escape call scalar decode and invoke `consumer.segment(..)` / `consumer.char(..)`. The scalar oracle must include the current `unescape_string` outputs byte-for-byte. | Layer 1 output-plane bridge, not Layer 0 SIMD. It belongs in parse-that only if the consumer trait is grammar-neutral; otherwise it stays a generated direct template helper. | Arch-neutral first; optional `escape_run_decode_x4` on aarch64/x86 only after scalar parity. | `alloc` on `y_string_unicode`; `string_escape` on unicode direct losses. | A direct product consumer such as digest fold or typed owned field writer; must be same-wave and row-bound. |
| `digit_run_span_64` | `fn digit_run_span_64(input, offset, limit) -> DigitRun { end, digits, overflow }` that classifies a run and optionally accumulates up to 19 decimal digits. | Byte loop with checked `value = value * 10 + digit`; return at first non-digit or overflow. For fixed blocks, mirror current private 8/4/2 digit helpers before scalar tail. | Layer 1 numeric token primitive. Layer 0 may provide digit-class masks or digit-MAC bodies. | aarch64 can use UDOT/dotprod for 4/8 digit MAC; x86 VNNI/AVX2 later. | `number_digit_scan` / `number_scan` on `canada`, `mesh`, `numbers`, `marine_ik`, typed `mesh`, typed `marine_ik`. | Generated direct numeric array/object paths and typed numeric Vec/product rows. Canada typed remains REDRESS-blocked until full-fixture parity exists. |
| `number_span_parts` | Split `match_number_span_from_first` into grammar-neutral parts: sign/integer/fraction/exponent scan plus a `NumberSpan` assembler. | Existing `match_number_span_from_first` is the oracle; refactor only after tests assert identical `NumberSpan` for all current number fixtures. | Layer 1 parse-that API, not SIMD by itself. | Arch-neutral; consumes `digit_run_span_64` when available. | `number_scan` self-time on `mesh` and typed numeric rows. | Direct/typed codegen that can skip or materialize numbers without re-entering JSON-specific parser control. |
| `ascii_class_skip` | `fn ascii_class_skip(input, offset, table: &[bool; 256]) -> usize` or a compact class-table equivalent for whitespace/layout classes. | Current `skip_ascii_whitespace` byte loop is oracle for JSON whitespace; generalized scalar loops until `!table[byte]`. | Layer 1 grammar-neutral byte-class skip; Layer 0 byte-class-from-table. | aarch64 NEON table/movemask; x86 AVX2/AVX-512 byte-class. | `whitespace_skip` on `citm_catalog`, `random`, `mesh`, `marine_ik`, typed `citm_catalog`. | Lower priority: direct rows only if S-P3 needs a maintain-safe whitespace slice; CSS/Sheets layout makes this useful for Lock 14. |

Non-candidate: `structural_cursor_from_movemask`. S-P1 shows `simd_movemask` in `gsoc-2018` and some string-heavy profiles, but REDRESS 96/97/98 pre-block retained structural cursor/substrate routes. A future primitive may still classify bytes transiently inside an existing string/number caller, but P2-E does not nominate a parse-that structural producer.

## §3 - Grammar-Neutrality

`bounded_plain_string_end` is grammar-neutral when expressed as `(terminator, escape, control_limit, cap)`. JSON supplies `"`/`\`/0x20; CSS strings, Sheets strings, and BBNF-self literals can supply their own delimiter/escape/control policy. Risk: hard-coding JSON quote/backslash in `parse-that` or `bbnf-simd` would violate Lock 14.

`plain_string_special_span` is grammar-neutral if `StringMode` stays a policy enum and the byte classes are supplied by the caller. It generalizes to CSS escapes and BBNF literals only if escape validation remains outside the block classifier.

`escape_run_decode_x4` is grammar-neutral only as a hex-quartet decoder plus surrogate join. The JSON-only part is the `\u` introducer and escape policy; that must stay in the grammar/template caller. CSS escape decoding has different width/termination rules, so S-P3 should mark this as "shared hex primitive, per-grammar escape parser."

`string_segments_fold` has the highest grammar-neutrality risk. A generic segment iterator is neutral; a `JsonDigestSink` callback, JSON key/value distinction, or sink-local decoded stats helper is not. If the target is typed product output, the schema/host contract must be the same-wave consumer.

`digit_run_span_64` and `number_span_parts` are neutral at the decimal-token level. JSON, Sheets, CSS numeric tokens, and BBNF-self all need decimal digit spans, but exponent/sign/fraction acceptance differs by grammar. The primitive must expose parts rather than baking JSON number grammar into generic SIMD code.

`ascii_class_skip` is the strongest Lock 14 candidate: layout/whitespace classes are grammar-supplied byte sets. JSON whitespace is just one table.

## §4 - Risks

- REDRESS 28 + 33: Class A retained tiny-string NEON wiring is invalidated. `bounded_plain_string_end` may be shortlisted only for direct/typed same-wave consumers, not retained parse closure.
- REDRESS 50-55: retained projection, whitespace cursor, parser-local structural cursor, decoded-string stats, and quote-source fused materializer routes are rejected. None of the candidates may add sidecar producers, parser-owned cursors, or sink-local stats as proof.
- REDRESS 60-72: retained parse/control experiments and direct escaped-string materialization families are pre-blocked. `string_segments_fold` must not repeat direct source-hook folding, parser-owned decoded scratch, byte-output unescape, semantic facts, or hand-authored JSON typed sinks.
- REDRESS 80: Canada mantissa widening is rejected because fallback evidence was zero. Numeric candidates must target measured digit-run/direct-array hot leaves, not table-only f64 widening.
- REDRESS 82: single-quartet Unicode classifier is rejected. `escape_run_decode_x4` must be framed as a batched run plus output-plane consumer, not another per-quartet helper.
- REDRESS 83: retained StringBlock16 tiny probe is rejected. Existing `string_block` may serve direct/typed callers only after micro-proof; retained parse rows cannot be the same-wave consumer.
- REDRESS 88/89: PMULL prefix-XOR and CSSC CTZ bodies are not hot bodies without consumers. P2-E does not nominate next-bit/prefix structural primitives.
- REDRESS 96/97/98: W3 union substrate and streaming cursor are retired. Any candidate producing retained structural offsets/classes for parse closure is rejected before S-P3.

## §5 - Sources

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
- `restart/locks/LOCKS.md`
- `restart/ARCHITECTURE.md`
- `skinny/crates/parse-that-regex/src/lib.rs`
- `skinny/crates/parse-that-regex/src/number/mod.rs`
- `skinny/crates/parse-that-regex/src/number/integer.rs`
- `skinny/crates/parse-that-regex/src/number/eisel_lemire/algorithm.rs`
- `skinny/crates/parse-that-regex/src/integration/simd_scan_hook.rs`
- `skinny/crates/bbnf-simd/src/aarch64/string_block.rs`
- `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs`
- `skinny/crates/bbnf-simd/src/lib.rs`
- `skinny/crates/bbnf-simd/src/dispatch.rs`
- `skinny/crates/runtime/src/grammars/json/generated.rs`
- `skinny/crates/bbnf-bench/src/direct_struct.rs`
- `skinny/crates/bbnf-bench/src/generated_real_typed.rs`
- `skinny/crates/codegen/src/typed_direct.rs`
