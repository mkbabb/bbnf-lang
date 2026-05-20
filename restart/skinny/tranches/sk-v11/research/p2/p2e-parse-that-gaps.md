# SK-V11 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V2.
Date: 2026-05-19.
Scope: parse-that primitive gaps tied to direct/typed/non-JSON product consumers.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; string_escape_decode; unicode_escape_hex_decode; number_digit_span; ascii_whitespace_skip.
Support/oracle-only leaves: container_dispatch; simd_movemask; output_digest_hash.
Lock surface: Lock 1 + Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

S-P1 accepts the direct hot-leaf vocabulary as `bounded_plain_string_scan`, `string_escape_decode`, `unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, and `output_digest_hash` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:98-119`). P2-E V2 keeps only the parse-that candidate gaps with a concrete scalar reference and a same-wave product consumer: `ascii_whitespace_skip`, `bounded_plain_string_scan`, `number_digit_span`, and escaped-string/hex decode from `string_escape_decode` plus `unicode_escape_hex_decode`.

The retained parse call sites in generated JSON are no longer admission surfaces. They are guards and micro-proof harnesses only: a candidate may use retained generated parse rows to prove semantics and non-regression, but S-P3 admission must come from a generated direct, typed, or non-JSON product-plane consumer with strict output parity. This folds the V1 CH3 warning that parse-only movement, retained structural class lanes, W3 cursors, and substrate sidecars do not close SK-V11 (`restart/skinny/tranches/sk-v11/research/p2/hardening/V1/CH3.md:82-93`).

Lock 1 matters because parse-that helpers must return only the scalar result needed by the caller or transient visitor events. They must not retain whitespace cursors, string segment side tables, decoded scratch, structural masks, or parser-owned projections (`restart/locks/LOCKS.md:52`). Lock 14 matters because the generic crates may expose byte-set, digit-run, bounded special-byte, and hex/escape segment kernels, but not JSON-specific grammar policy (`restart/locks/LOCKS.md:78`).

V2 candidate-pool hygiene:

| Hot leaf | V2 disposition | Reason |
| --- | --- | --- |
| `ascii_whitespace_skip` | Retained candidate: `pt_byte_set_run_skip`. | The scalar reference exists as JSON whitespace/space-only loops, but the generic primitive must be byte-set run skip, not comment-aware layout trivia (`skinny/crates/parse-that-regex/src/lib.rs:112-147`). |
| `bounded_plain_string_scan` | Retained candidate: `pt_bounded_plain_string_end`. | Local scalar loops exist in generated/direct/typed code; the first admitted parse-that shape is scalar factoring with full parser fallback (`skinny/crates/runtime/src/grammars/json/generated.rs:161-185`, `skinny/crates/bbnf-bench/src/direct_struct.rs:564-576`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811-1835`). |
| `number_digit_span` | Retained candidate: `pt_digit_run_span_accumulate`. | parse-that has the scalar digit scanner and fixed-width helpers, but the public JSON number matcher is too policy-heavy for direct/typed reuse (`skinny/crates/parse-that-regex/src/number/mod.rs:37-223`). |
| `string_escape_decode` + `unicode_escape_hex_decode` | Retained candidate: `pt_escaped_string_segments`. | Existing materializers and quartet decoders are semantic references, not a production admission. V2 admits only a new segment-stream caller with scalar oracle and product consumer (`skinny/crates/parse-that-regex/src/lib.rs:302-344`, `skinny/crates/parse-that-regex/src/lib.rs:718-809`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:39-166`). |
| `container_dispatch` | Non-candidate for P2-E. Support/oracle only. | Generated grammar control/FIRST-set dispatch belongs to generated code, not a parse-that primitive. REDRESS only admits narrow same-loop container carries and blocks object/key/value-byte compaction (`skinny/REDRESS.md:1492-1685`, `skinny/REDRESS.md:2360-2397`). |
| `simd_movemask` | Non-candidate for P2-E. Support-only for other primitives. | bbnf-simd owns Layer-0 mask vocabulary; P2-E may depend on a strict parity gate for a same-loop byte/string consumer, but must not add a standalone parser primitive. |
| `output_digest_hash` | Non-candidate for P2-E. Benchmark oracle only. | Digest/hash is an output-plane verifier or product host sink. Moving it into parse-that or bbnf-simd would leak benchmark policy into generic parser semantics. |

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

Each retained candidate must carry the same CH4 tuple before S-P3 shortlist: scalar-reference status, strict checkasm/parity expectation or product parity when scalar-only, micro-prove-first status, same-wave product consumer, feature/fallback behavior, output-plane declaration, and a reject boundary. A candidate that cannot name a same-wave direct, typed, or generated non-JSON consumer stays in research or proof-only status.

Common commands for candidate packets:

- Scalar/product parity: `cargo test -p parse-that-regex`.
- bbnf-simd strict parity when any AArch64 body is routed: `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --tests`.
- Product row gate: the current `bbnf-bench` row-bench command filtered to the named direct/typed/non-JSON packet. The packet must record the exact command it uses rather than relying on prose.

Strict no-regression means no semantic mismatch, no output-plane mismatch, no sidecar allocation, and no named guard row losing more than `0.5%` median throughput or cycles/byte on the same host. Minimum useful movement means at least one named target row improves by `>= 1.0%` median throughput or cycles/byte, or the candidate is rejected or kept proof-only. When an optional SIMD body is routed, it must also pass strict checkasm on the same host before product rows count.

| Candidate | Scalar reference | Output plane | Same-wave product consumer | Row/proof set | Strict parity/checkasm | Fallback | Reject boundary |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `pt_byte_set_run_skip` | New scalar oracle: loop from `offset` while `ByteSet::contains(input[i])`; JSON references are `skip_ascii_whitespace` and `skip_ascii_space` (`skinny/crates/parse-that-regex/src/lib.rs:112-147`). | Cursor offset only. No whitespace bitmap, cursor sidecar, class column, structural mask, or retained trivia facts. | Direct JSON whitespace sites and a generated non-JSON layout consumer: Sheets `?w` formulas or CSS/BBNF layout byte-set skip with comments handled by generated grammar policy (`grammar/google-sheets/google-sheets.bbnf:103-161`, `grammar/css/l4/color.bbnf:18-27`, `grammar/bbnf/bbnf.bbnf:17-18`). | Target rows: `twitter`, `random`, `distinct_values`; W0 planning guards: `instruments`; non-JSON target: generated Sheets formulas first, CSS declaration values second. Retained JSON parse rows are guards only. | Scalar parity over empty runs, all offsets, tails, all four JSON whitespace bytes, space-only sets, and non-JSON byte sets. If bbnf-simd eq-set/table blocks are used, run `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_byte_class_from_eq_set_64` and `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_byte_class_from_table_64`. | Default to scalar parse-that oracle. Feature-gate any AArch64 body behind existing bbnf-simd dispatch and fall back to scalar on missing feature, strict divergence, or unsupported set shape. | Reject if no direct/typed/non-JSON row gains `>= 1.0%`, if any named guard row regresses beyond `0.5%`, or if implementation handles CSS/BBNF comments inside the generic byte-set primitive. |
| `pt_bounded_plain_string_end` | Scalar oracle lifted from local capped plain-string loops in generated JSON, direct_struct, and typed generated code (`skinny/crates/runtime/src/grammars/json/generated.rs:161-185`, `skinny/crates/bbnf-bench/src/direct_struct.rs:564-576`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811-1835`). | Borrowed span end offset only. No decoded string, retained string block, string-side table, byte-output materializer, or semantic string-field facts. | Direct JSON string/key fast paths, typed generated string skip, and a generated non-JSON string/literal consumer: CSS dual-quoted strings, Sheets doubled-quote strings, or BBNF literal/regex spans (`grammar/css/l4/tokens.bbnf:7-9`, `grammar/google-sheets/google-sheets.bbnf:8-12`, `grammar/bbnf/bbnf.bbnf:11-15`). | Target rows: `twitter`, `github_events`, `update_center`, `random`, `distinct_values`, `gsoc-2018`; guard rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`; retained generated JSON string/key parse sites are micro-proof guards only. | Scalar parity against local loops and existing parse-that string tests: `cargo test -p parse-that-regex`. Future AArch64 block body must add strict parity for every quote offset, cap, alignment, escape/control byte, and tail: `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --tests`. | Start scalar-only. On miss, fall back to the full parse-that string matcher. Do not route current retained/tiny NEON parser wiring; any future AArch64 body is optional and scalar-fallback guarded. | Reject if scalar factoring does not produce `>= 1.0%` on at least one target string row with strict no-regression on Unicode guards, or if the packet widens into a 64-byte retained scan, StringBlock16 retained wrapper, tiny NEON parser route, or primitive-parity-only production claim. |
| `pt_digit_run_span_accumulate` | Scalar oracle derived from private parse-that `scan_digit_run` and fixed-width accumulation helpers; full `match_number_span_from_first` remains the JSON-policy layer (`skinny/crates/parse-that-regex/src/number/mod.rs:37-223`). | `DigitRun { end, digit_count, mantissa_prefix, overflow_or_truncated }` only. No f64 fallback change, no widened mantissa policy, no number-side table, no output digest update. | Direct JSON numeric rows, typed numeric field guards, and generated non-JSON numeric consumers: CSS dimensions/percentages, Sheets numeric formulas, or BBNF numeric literals (`grammar/css/l4/value-unit.bbnf:8-16`, `grammar/css/l4/value-unit.bbnf:62-72`, `grammar/google-sheets/google-sheets.bbnf:6-7`, `grammar/bbnf/expressions.bbnf:6-14`). | Target rows: `canada`, `mesh`, W0 planning row `numbers`; typed guards: `mesh`, `marine_ik`; non-JSON target: CSS dimensions first, Sheets numeric formulas second. Retained JSON number parser rows are guards only. | Scalar parity over digit lengths 0 through long runs, non-digit boundaries, offsets, alignments, and truncation/overflow. If the AArch64 4-digit/dotprod helper is used, add a candidate-specific strict parity harness beyond the existing smoke tests and run `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test aarch64_primitives`. | Full number grammar and conversion remain the fallback. AArch64 digit chunks are optional and feature-gated; scalar oracle owns semantics when dotprod is absent or strict parity fails. | Reject if no target direct/typed/non-JSON numeric row gains `>= 1.0%`, if any typed guard or conversion output changes, or if the packet changes f64 fallback, leading-zero/sign/exponent policy, or mantissa widening. |
| `pt_escaped_string_segments` | New scalar segment-stream oracle, using `unescape_string`, `decode_unicode_escape`, and bbnf-simd scalar quartet decode as semantic references (`skinny/crates/parse-that-regex/src/lib.rs:302-344`, `skinny/crates/parse-that-regex/src/lib.rs:718-809`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:39-47`). | Visitor stream of raw spans, simple escapes, and decoded scalar values. No parser-owned decoded scratch, decoded stats sink, output hash, byte-output materializer, or retained semantic string facts. | Must name a new direct/typed/non-JSON source delta beyond the already-consuming `unescape_string` path: typed decoded string fields, CSS escaped strings or hex color decode, or BBNF literal/regex escape decoding (`grammar/css/l4/tokens.bbnf:7-9`, `grammar/css/l4/color.bbnf:187-190`, `grammar/bbnf/bbnf.bbnf:11-15`). Direct JSON Unicode rows are guards unless paired with that new product consumer. | Target/guard rows: `unicode_escapes`, `unicode_mixed`, `y_string_unicode`; non-JSON target: CSS escaped strings or hex-color decode first, BBNF literals second. Current x4 caller coverage is proof-only smoke unless a new caller delta lands. | Scalar segment parity for simple escapes, valid/invalid Unicode escapes, surrogate policy delegated to generated caller, dense runs, and boundary splits. Current x4 coverage is fixed-valid smoke only; production needs a scalar x4 oracle plus strict valid/invalid/mixed/alignment/surrogate cases, e.g. `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_utf8_block` and `BBNF_SIMD_STRICT=1 cargo test -p bbnf-simd --test checkasm_parity`. | Default to scalar segment visitor or existing `unescape_string` materialization when the caller needs an owned string. AArch64 quartet/x4 decode is optional support and cannot be routed without strict parity and a new product consumer. | Reject if the only consumer is the existing `unescape_string` path, if no direct/typed/non-JSON consumer gains `>= 1.0%`, if JSON surrogate policy enters parse-that/bbnf-simd generic APIs, or if the packet adds decoded scratch/stats/hash side channels. |

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

### Candidate 1: `pt_byte_set_run_skip`

Shape: `fn skip_byte_set_run(input: &[u8], offset: usize, set: ByteSet) -> usize`, where `ByteSet` is grammar metadata and the result is the first offset not in the set. JSON whitespace is one generated instantiation, not the primitive name or generic policy.

Layer placement: Layer 1 in parse-that-regex, backed by Layer-0 bbnf-simd byte classification only when the same-wave consumer proves it. bbnf-simd already exposes `byte_class_from_eq_set_64` and `byte_class_from_table_64` (`skinny/crates/bbnf-simd/src/lib.rs:234-272`).

Grammar-neutral boundary: CSS and BBNF comments remain generated layout policy, not generic byte-set skip. Sheets `?w` is the cleanest first non-JSON byte-set consumer (`grammar/google-sheets/google-sheets.bbnf:103-161`).

### Candidate 2: `pt_bounded_plain_string_end`

Shape: `fn bounded_plain_string_end(input: &[u8], quote_offset: usize, cap: usize, escape: u8, control_limit: u8) -> Option<usize>`, returning the closing-quote end offset only when the body is plain, the close is within `cap`, and no escape or control byte appears before the close.

Layer placement: Layer 1 in parse-that-regex. It may call the full parse-that string matcher on miss. Layer 0 may later provide a block scan, but V2 admission starts scalar-only because previous retained/tiny NEON parser routes are REDRESS-preblocked.

Grammar-neutral boundary: quote byte, escape byte, control cutoff, and cap are parameters. JSON, CSS, Sheets, and BBNF literals can share the bounded special-byte scan; JSON UTF-8 or surrogate policy cannot move into the generic helper.

### Candidate 3: `pt_digit_run_span_accumulate`

Shape: `fn digit_run_span_accumulate(input: &[u8], offset: usize, max_accum_digits: u8) -> DigitRun`, where `DigitRun` contains `end`, `digit_count`, `mantissa_prefix`, and `overflow_or_truncated`. Grammar-specific sign, decimal, exponent, suffix/unit, leading-dot, and fallback policy stay in generated parsers or parse-that number policy.

Layer placement: Layer 1 in parse-that-regex. A later Layer-0 bbnf-simd helper may accelerate digit masks or fixed-width multiply-add, but the primitive is not a replacement for full number grammar.

Grammar-neutral boundary: CSS dimensions and Sheets numbers demonstrate that the shared primitive is digit-run and bounded accumulation only; JSON-strict number span cannot become the generic API.

### Candidate 4: `pt_escaped_string_segments`

Shape: `fn escaped_string_segments(input: &[u8], body: Range<usize>, table: EscapeTable, visitor: impl FnMut(EscapedSegment)) -> Result<()>`, where segments are raw spans, simple escapes, or decoded scalar values. The API does not allocate decoded scratch, retain side tables, hash, or choose a grammar's output encoding.

Layer placement: Layer 1 in parse-that-regex. Layer 0 remains raw hex/quartet decode support in bbnf-simd. The x4 Unicode route is proof-only until it has a scalar x4 oracle, strict invalid/alignment coverage, and a new source delta beyond `unescape_string`.

Grammar-neutral boundary: the neutral primitive is escape segment plus hex-nibble/hex-run decode. JSON `\uXXXX` prefix, surrogate joining, CSS variable-width escapes, Sheets doubled quotes, and BBNF literal/regex policy belong to generated grammar or host code.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

Do not reopen W3/substrate repairs. REDRESS 50, 51, 53, 92, 96, 97, and 98 block parser-owned side tables, whitespace cursors, structural-mask cursors, class columns, move-consumed structural indexes, and union-substrate repairs (`skinny/REDRESS.md:715-768`, `skinny/REDRESS.md:784-813`, `skinny/REDRESS.md:2663-2690`, `skinny/REDRESS.md:2797-2906`). P2-E candidates may return scalar offsets or transient visitor events only.

Do not repackage rejected string materialization routes. REDRESS 54, 55, 60, 61, 62, 64, 67, 68, 69, 72, 82, 83, 106, 107, and 108 block decoded stats sinks, fused quote-source materializers, retained trusted string boundary collapse, retained long/wide string scans, delayed-wide scan, retained Unicode escape validators, parser-owned decoded scratch, byte-output materialization, semantic string-field facts, broad cap widening, single-quartet materializer, generated retained StringBlock16 probes, primitive-parity-only full string production, x4 proof-to-production promotion, and reuse of the already-consuming `unescape_string` caller (`skinny/REDRESS.md:815-870`, `skinny/REDRESS.md:1346-1488`, `skinny/REDRESS.md:1584-1635`, `skinny/REDRESS.md:1736-1886`, `skinny/REDRESS.md:1996-2004`, `skinny/REDRESS.md:2287-2356`, `skinny/REDRESS.md:3152-3222`).

Do not reopen numeric fallback work. REDRESS 80 blocks mantissa-widen/f64-fallback routes, so `pt_digit_run_span_accumulate` must prove digit-span value without changing conversion semantics (`skinny/REDRESS.md:2217-2248`).

Do not turn support/oracle leaves into parse-that primitives. `container_dispatch` is generated grammar control, `simd_movemask` is Layer-0 support for same-loop consumers, and `output_digest_hash` is benchmark/output oracle or per-product host behavior. None may enter the P2-E primitive pool without fresh P1 evidence and a new direct/typed/non-JSON product packet.

Checkasm parity must be strict before any AArch64 body is product-routed. The current checkasm report records open strict-mode NEON divergence in classifier parity while the harness defines scalar comparison, alignment, and corpus gates (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41-52`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102-126`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:172-198`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

No external sources were used. Local sources:

- S-P2 hardening V1 consolidated: `restart/skinny/tranches/sk-v11/research/p2/hardening/HARDENING-S-P2-V1-CONSOLIDATED.md`
- S-P2 hardening V1 CH2, CH3, CH4, CH6: `restart/skinny/tranches/sk-v11/research/p2/hardening/V1/CH2.md`, `restart/skinny/tranches/sk-v11/research/p2/hardening/V1/CH3.md`, `restart/skinny/tranches/sk-v11/research/p2/hardening/V1/CH4.md`, `restart/skinny/tranches/sk-v11/research/p2/hardening/V1/CH6.md`
- P2-F grammar-neutral abstraction: `restart/skinny/tranches/sk-v11/research/p2/p2f-grammar-neutral.md`
- P1 hot-leaf attribution: `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`
- P1 hardening convergence: `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- REDRESS ledger: `skinny/REDRESS.md`
- Locks: `restart/locks/LOCKS.md`
- parse-that-regex string/number/unicode code and tests: `skinny/crates/parse-that-regex/src/lib.rs`, `skinny/crates/parse-that-regex/src/number/mod.rs`, `skinny/crates/parse-that-regex/src/unicode/utf8_block.rs`
- bbnf-simd primitive code and checkasm tests: `skinny/crates/bbnf-simd/src/lib.rs`, `skinny/crates/bbnf-simd/src/dispatch.rs`, `skinny/crates/bbnf-simd/src/aarch64/*.rs`, `skinny/crates/bbnf-simd/tests/*.rs`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- Generated JSON, direct_struct, Track 2, and typed generated consumers: `skinny/crates/runtime/src/grammars/json/generated.rs`, `skinny/crates/runtime/src/grammars/json/scan.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, `skinny/crates/bbnf-bench/src/direct_struct.rs`, `skinny/crates/bbnf-bench/src/track2/json.rs`
- Non-JSON grammar proof surfaces: `grammar/css/l4/tokens.bbnf`, `grammar/css/l4/color.bbnf`, `grammar/css/l4/value-unit.bbnf`, `grammar/google-sheets/google-sheets.bbnf`, `grammar/bbnf/bbnf.bbnf`, `grammar/bbnf/expressions.bbnf`
