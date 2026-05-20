# SK-V11 P2-E: Parse-That Primitive Gaps

Pass: S-P2 Research. Cycle: V1.
Date: 2026-05-19.
Scope: parse-that and bbnf-simd primitive gaps tied to direct residual hot leaves.
Output: this file.
P1 hot-leaf antecedents: bounded_plain_string_scan; string_escape_decode; unicode_escape_hex_decode; number_digit_span; ascii_whitespace_skip; container_dispatch; simd_movemask; output_digest_hash.
Lock surface: Lock 1 + Lock 14.

## §1 — Findings (concrete; file:line on bbnf claims, citation on external claims)

S-P1 accepts the direct hot-leaf vocabulary as `bounded_plain_string_scan`, `string_escape_decode`, `unicode_escape_hex_decode`, `number_digit_span`, `ascii_whitespace_skip`, `container_dispatch`, `simd_movemask`, and `output_digest_hash` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:98-119`). The same P1 note ties those leaves to direct residual rows including `twitter`, `canada`, `github_events`, `update_center`, `mesh`, `random`, `gsoc-2018`, `unicode_escapes`, `distinct_values`, and `y_string_unicode` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:153-164`), plus W0-clamped rows `instruments`, `numbers`, and `unicode_mixed` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:186-188`). The converged hardening note keeps that surface in scope while warning that diagnostics and JSON-only telemetry do not close SK-V11 without a non-JSON intervention (`restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md:41-55`).

Lock 1 matters because a primitive must not become a retained side substrate: if structural offsets are retained, they are the tape projection, not a second parser surface (`restart/locks/LOCKS.md:52`). Lock 14 matters because any parse-that or bbnf-simd primitive must be grammar-neutral; JSON-named policy belongs in the generated JSON grammar, not in the generic crates (`restart/locks/LOCKS.md:78`).

Hot-leaf coverage:

| P1 hot leaf | Exposed today | Gap verdict |
| --- | --- | --- |
| `bounded_plain_string_scan` | Generated JSON has local tiny/capped string loops (`skinny/crates/runtime/src/grammars/json/generated.rs:161-185`, `skinny/crates/runtime/src/grammars/json/generated.rs:610-641`); direct_struct has a local `tiny_plain_string` (`skinny/crates/bbnf-bench/src/direct_struct.rs:564-576`); typed generated code has local plain-string skip helpers (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811-1835`). parse-that exposes full quoted-string matchers (`skinny/crates/parse-that-regex/src/lib.rs:157-281`) and keeps the trusted plain skip private (`skinny/crates/parse-that-regex/src/lib.rs:547-574`). bbnf-simd exposes raw string blocks and tiny-match modules (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:5-72`, `skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:37-96`). | Missing Layer-1 parse-that primitive: bounded plain-string end with a scalar oracle. AArch64 raw pieces exist but the retained/tiny NEON routes are REDRESS-preblocked, so this candidate starts scalar-only. |
| `string_escape_decode` | parse-that exposes `unescape_string` and `decode_unicode_escape` (`skinny/crates/parse-that-regex/src/lib.rs:302-344`, `skinny/crates/parse-that-regex/src/lib.rs:718-809`), while validation helpers remain private (`skinny/crates/parse-that-regex/src/lib.rs:283-294`, `skinny/crates/parse-that-regex/src/lib.rs:346-382`). | Primitive exists for materialization, but not as a public segment/run primitive that direct, typed, CSS, or BBNF-self consumers can share without adding decoded scratch or a stats sink. |
| `unicode_escape_hex_decode` | bbnf-simd has AArch64 scalar and NEON `uXXXX` decoders (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:39-47`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:72-166`); parse-that calls the x4 path inside `unescape_string` (`skinny/crates/parse-that-regex/src/lib.rs:777-780`) but keeps scalar hex helpers private (`skinny/crates/parse-that-regex/src/lib.rs:945-966`). | Raw decode exists; the gap is a grammar-neutral escaped-segment API and public scalar oracle, not another JSON-only Unicode validator. |
| `number_digit_span` | parse-that exposes `match_number_span_from_first` and materializers (`skinny/crates/parse-that-regex/src/number/mod.rs:37-103`, `skinny/crates/parse-that-regex/src/number/mod.rs:225-272`) but keeps `scan_digit_run` private (`skinny/crates/parse-that-regex/src/number/mod.rs:105-162`). bbnf-simd only has a 4-digit AArch64 dotprod helper (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4-23`). | Missing shared digit-run/span-and-accumulate primitive. Existing public number parsing is too coarse for direct/typed hot loops; existing SIMD is too narrow and not integrated. |
| `ascii_whitespace_skip` | parse-that exposes a JSON whitespace skipper (`skinny/crates/parse-that-regex/src/lib.rs:112-125`) and has a private spaces-only helper (`skinny/crates/parse-that-regex/src/lib.rs:127-147`). bbnf-simd exposes byte-class masks over tables and equality sets (`skinny/crates/bbnf-simd/src/lib.rs:234-272`). | Missing grammar-neutral byte-set run skip. Layer-0 classification exists; Layer-1 cursor advance over a byte set does not. |
| `container_dispatch` | Generated JSON owns dispatch and container next logic (`skinny/crates/runtime/src/grammars/json/generated.rs:37-57`, `skinny/crates/runtime/src/grammars/json/generated.rs:310-338`), and Track 2 has its own container next logic over the runtime tape (`skinny/crates/bbnf-bench/src/track2/json.rs:270-300`). | No parse-that/bbnf-simd primitive gap should be opened in P2-E. This is generated control flow; REDRESS only admits narrow in-loop carries and blocks broader object/value-byte control compaction. |
| `simd_movemask` | bbnf-simd exposes classifier and bitmap primitives (`skinny/crates/bbnf-simd/src/lib.rs:169-223`, `skinny/crates/bbnf-simd/src/dispatch.rs:49-87`), and generated JSON scan consumes the structural scanner (`skinny/crates/runtime/src/grammars/json/scan.rs:200-275`). Some AArch64 bitmap bodies still delegate to scalar (`skinny/crates/bbnf-simd/src/aarch64/bitmap_prefix_xor.rs:1-4`, `skinny/crates/bbnf-simd/src/aarch64/bulk_emit_positions_64.rs:1-4`). | Primitive vocabulary mostly exists. The gap is not a new P2-E primitive; product use would reopen W3/substrate routing unless another P2 owns a same-loop consumer. |
| `output_digest_hash` | direct_struct owns benchmark digest/hash mixing locally (`skinny/crates/bbnf-bench/src/direct_struct.rs:123-128`, `skinny/crates/bbnf-bench/src/direct_struct.rs:717-742`). | Not a parse-that/bbnf-simd primitive candidate. Moving benchmark output hashing into generic crates would violate grammar-neutrality and would not create a typed/non-JSON parser primitive. |

## §2 — Candidate primitives (each: shape + scalar-ref status + arch + P1 antecedent)

### Candidate 1: `pt_byte_set_run_skip`

Shape: `fn skip_byte_set_run(input: &[u8], offset: usize, set: ByteSet) -> usize`, where `ByteSet` is grammar metadata and the result is the first offset not in the set. JSON whitespace is one instantiation, not the primitive name or policy.

Scalar reference sketch: loop from `offset` while `set.contains(input[i])`, then return `i`. The existing scalar JSON skipper is the closest reference (`skinny/crates/parse-that-regex/src/lib.rs:112-125`); a new scalar oracle should be generic over the byte set.

Layer placement: Layer 1 in parse-that-regex, backed by Layer-0 bbnf-simd byte classification when profitable. bbnf-simd already exposes `byte_class_from_eq_set_64` and `byte_class_from_table_64` (`skinny/crates/bbnf-simd/src/lib.rs:234-272`).

AArch64 status: partial. `byte_class_from_eq_set_64_neon` exists for equality-set masks (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_eq_set_64.rs:1-72`), while table classification currently delegates to the scalar implementation (`skinny/crates/bbnf-simd/src/aarch64/byte_class_from_table_64.rs:1-4`). This candidate can start scalar and enable the equality-set path only after parity and row proof.

Checkasm parity expectation: strict scalar parity over offsets, alignments, tails, all four JSON whitespace bytes, and non-JSON byte sets. The existing equality-set and table harnesses provide the model (`skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_eq_set_64.rs:8-17`, `skinny/crates/bbnf-simd/tests/checkasm_byte_class_from_table_64.rs:13-49`).

Same-wave consumer: generated direct whitespace call sites such as `skip_ws` (`skinny/crates/runtime/src/grammars/json/generated.rs:238-242`) and Track 2 cursor code, with direct residual rows `twitter`, `random`, `distinct_values`, and W0-clamped `instruments` as row-level checks (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:153-164`, `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:186-188`).

Micro-proof target: standalone span benchmark plus same-wave direct-row gate showing the generic Layer-1 call does not regress short whitespace spans and improves at least one residual whitespace-heavy row. It must not create a whitespace bitmap, cursor, class column, or sidecar.

Direct/typed/non-JSON consumer candidate: direct JSON whitespace, typed guard rows that touch whitespace/string boundaries, CSS Level 4 whitespace/trivia, and BBNF-self layout trivia.

P1 antecedent: `ascii_whitespace_skip` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:110`).

### Candidate 2: `pt_bounded_plain_string_end`

Shape: `fn bounded_plain_string_end(input: &[u8], quote_offset: usize, cap: usize, escape: u8, control_limit: u8) -> Option<usize>`, returning the closing-quote end offset only when the string body is plain, the closing quote is within `cap`, and no escape or control byte appears before the close.

Scalar reference sketch: lift the existing generated/direct local cap loops into one scalar oracle. Current local references are in generated JSON (`skinny/crates/runtime/src/grammars/json/generated.rs:161-185`), direct_struct (`skinny/crates/bbnf-bench/src/direct_struct.rs:564-576`), and typed generated code (`skinny/crates/bbnf-bench/src/generated_real_typed.rs:1811-1835`).

Layer placement: Layer 1 in parse-that-regex. It should call full parse-that string matching only on miss; Layer 0 may later provide a block scan, but not in the first proof.

AArch64 status: disabled by default for the candidate. bbnf-simd has `scan_string_special_block` (`skinny/crates/bbnf-simd/src/aarch64/string_block.rs:56-72`) and a tiny plain matcher (`skinny/crates/bbnf-simd/src/aarch64/match_tiny_plain_string.rs:79-96`), but REDRESS blocks the current retained/tiny NEON routes. The admitted first shape is scalar Layer 1 with a future optional AArch64 body only after checkasm and row proof.

Checkasm parity expectation: future AArch64 body must match the scalar oracle over every quote offset, cap, alignment, escape/control byte, and tail length. Until a native body exists, unit tests should assert parity against the local generated loops and existing parse-that string tests (`skinny/crates/parse-that-regex/src/lib.rs:993-1020`, `skinny/crates/parse-that-regex/src/lib.rs:1116-1127`).

Same-wave consumer: generated string/key parse paths (`skinny/crates/runtime/src/grammars/json/generated.rs:90-116`, `skinny/crates/runtime/src/grammars/json/generated.rs:142-155`, `skinny/crates/runtime/src/grammars/json/generated.rs:610-641`), Track 2 local tiny string handling (`skinny/crates/bbnf-bench/src/track2/json.rs:313-325`), and direct_struct (`skinny/crates/bbnf-bench/src/direct_struct.rs:541-562`).

Micro-proof target: replace local scalar tiny loops with the shared scalar primitive in the same wave and prove no regression on string residual rows, especially `twitter`, `github_events`, `update_center`, `random`, `distinct_values`, and `gsoc-2018` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:153-164`).

Direct/typed/non-JSON consumer candidate: direct JSON string/key rows, typed generated string skip, CSS strings, Sheets quoted strings, and BBNF string literals.

P1 antecedent: `bounded_plain_string_scan` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:106`).

### Candidate 3: `pt_digit_run_span_accumulate`

Shape: `fn digit_run_span_accumulate(input: &[u8], offset: usize, max_accum_digits: u8) -> DigitRun`, where `DigitRun` contains `end`, `digit_count`, `mantissa_prefix`, and `overflow_or_truncated`. Grammar-specific sign, decimal, exponent, and fallback policy stay in parse-that number parsing.

Scalar reference sketch: derive the oracle from the private parse-that digit scanner and its fixed-width helpers (`skinny/crates/parse-that-regex/src/number/mod.rs:105-162`, `skinny/crates/parse-that-regex/src/number/mod.rs:164-223`), then keep `match_number_span_from_first` as the public number-policy layer (`skinny/crates/parse-that-regex/src/number/mod.rs:37-103`).

Layer placement: Layer 1 in parse-that-regex. A later Layer-0 bbnf-simd helper may accelerate digit masks or fixed-width multiply-add, but the primitive is not a replacement for full number grammar.

AArch64 status: partial. bbnf-simd only exposes `parse_4_digits`, with dotprod when available (`skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:4-23`, `skinny/crates/bbnf-simd/src/aarch64/digit_mac.rs:25-49`). There is no admitted 8/16 digit-run kernel or public integration into parse-that number scanning.

Checkasm parity expectation: scalar oracle versus any AArch64 body over digit and non-digit boundaries, offsets, alignments, digit lengths 0 through long runs, and overflow/truncation cases. Existing AArch64 primitive tests cover only the narrow 4-digit helper (`skinny/crates/bbnf-simd/tests/aarch64_primitives.rs:167-184`), so this candidate needs its own parity harness.

Same-wave consumer: generated JSON number direct paths (`skinny/crates/runtime/src/grammars/json/generated.rs:650-685`), direct residual rows `canada`, `mesh`, and W0-clamped `numbers` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:153-164`, `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:186-188`), plus typed numeric guard rows such as `mesh` and `marine_ik` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:199-204`).

Micro-proof target: show a measurable win in the digit-run leaf without changing f64 fallback behavior or widening mantissa policy. REDRESS 80 blocks that route; this candidate only moves digit-run/span work.

Direct/typed/non-JSON consumer candidate: direct JSON numeric arrays/objects, typed numeric fields, CSS numeric tokens, Sheets formula numeric literals, and BBNF numeric literals.

P1 antecedent: `number_digit_span` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:109`).

### Candidate 4: `pt_escaped_string_segments`

Shape: `fn escaped_string_segments(input: &[u8], body: Range<usize>, table: EscapeTable, visitor: impl FnMut(EscapedSegment)) -> Result<()>`, where segments are raw bytes, simple escapes, or decoded scalar values. The API does not allocate decoded scratch, does not retain a side table, and does not hash inside parse-that.

Scalar reference sketch: use parse-that `unescape_string` and `decode_unicode_escape` as semantic references (`skinny/crates/parse-that-regex/src/lib.rs:302-344`, `skinny/crates/parse-that-regex/src/lib.rs:718-809`), but expose a new scalar segment oracle because current hex and validation pieces are private (`skinny/crates/parse-that-regex/src/lib.rs:346-382`, `skinny/crates/parse-that-regex/src/lib.rs:945-966`).

Layer placement: Layer 1 in parse-that-regex. Layer 0 remains the raw Unicode quartet decoder in bbnf-simd, not a JSON string materializer.

AArch64 status: raw bodies exist but are not sufficient evidence. bbnf-simd exposes scalar and NEON Unicode quartet decode (`skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:39-47`, `skinny/crates/bbnf-simd/src/aarch64/unescape_uxxxx.rs:72-166`), and parse-that already calls the x4 helper inside materialization (`skinny/crates/parse-that-regex/src/lib.rs:777-780`). Prior product attempts around retained Unicode escape validation and single-quartet classification were rejected, so this candidate starts as a scalar segment API with AArch64 gated behind a new proof.

Checkasm parity expectation: scalar segment stream versus any AArch64 decode path for simple escapes, valid and invalid Unicode escapes, surrogate pairs, dense runs, and boundary splits. Existing tests cover dense Unicode run and bad-escape cases (`skinny/crates/parse-that-regex/src/lib.rs:1031-1080`, `skinny/crates/parse-that-regex/src/lib.rs:1153-1171`) and x4 checkasm smoke coverage (`skinny/crates/bbnf-simd/tests/checkasm_utf8_block.rs:58-68`), but not a public segment stream.

Same-wave consumer: only admissible if paired with a same-wave direct or typed consumer that needs decoded segments without adding a decoded scratch buffer or digest sink. Candidate JSON rows are `unicode_escapes`, `unicode_mixed`, and `y_string_unicode` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:153-164`, `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:186-188`).

Micro-proof target: prove standalone parity and row improvement against `unescape_string` on Unicode-heavy strings, then show a same-wave consumer in direct or typed code. The proof must explicitly rule out decoded stats, parser-owned scratch, byte-output materialization, and semantic string-field facts as hidden carriers.

Direct/typed/non-JSON consumer candidate: typed decoded string fields, CSS escape decoding, BBNF string literal decoding, and direct JSON Unicode rows only if the consumer is segment-based rather than benchmark-hash-specific.

P1 antecedent: `string_escape_decode` and `unicode_escape_hex_decode` (`restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md:107-108`).

Not candidates for P2-E:

- `container_dispatch`: generated grammar control, not a parse-that primitive; admitted work must stay as same-loop generated control and respect the narrow ContainerNext route.
- `simd_movemask`: bbnf-simd already owns the Layer-0 vocabulary; product use without a same-loop consumer risks reopening W3.
- `output_digest_hash`: benchmark output plane, not grammar-neutral parser vocabulary.

## §3 — Grammar-neutrality (each candidate: JSON-only or CSS/Sheets/BBNF-self generalisable)

`pt_byte_set_run_skip` is generalisable when the byte set is grammar metadata. JSON whitespace, CSS trivia, Sheets formula whitespace, and BBNF layout all use the same shape; only the set changes.

`pt_bounded_plain_string_end` is generalisable when quote byte, escape byte, control cutoff, and cap are parameters. JSON keys/strings, CSS strings, Sheets quoted strings, and BBNF literals can share the primitive. A JSON-only cap or UTF-8 policy would violate Lock 14.

`pt_digit_run_span_accumulate` is generalisable as a digit-run primitive. JSON number policy, CSS numeric-token policy, Sheets formula numbers, and BBNF numeric literals can own their surrounding grammar rules while sharing digit run and bounded accumulation.

`pt_escaped_string_segments` is only generalisable if `EscapeTable` is data and parse-that does not hard-code JSON surrogate or escape policy into bbnf-simd. If the first proof cannot show a CSS or BBNF-self consumer shape, the candidate should be downgraded to JSON-only and held out of the generic crates.

The no-candidate leaves are intentionally not generalised here. `container_dispatch` belongs to generated grammar code; `simd_movemask` is already Layer 0; `output_digest_hash` is benchmark/output behavior.

## §4 — Risks (REDRESS entries any candidate must NOT re-open)

Do not reopen W3/substrate repairs. REDRESS 50, 51, 53, 92, 96, 97, and 98 block parser-owned side tables, whitespace cursors, structural-mask cursors, class columns, move-consumed structural indexes, and union-substrate repairs (`skinny/REDRESS.md:715-768`, `skinny/REDRESS.md:784-813`, `skinny/REDRESS.md:2663-2690`, `skinny/REDRESS.md:2797-2906`). Lock 1 reinforces that structural projection cannot become a second substrate (`restart/locks/LOCKS.md:52`).

Do not repackage rejected string materialization routes. REDRESS 54, 55, 60, 61, 62, 64, 67, 68, 69, 72, 82, and 83 block decoded stats sinks, fused quote-source materializers, retained trusted string boundary collapse, retained long-string scan, delayed-wide scan, retained Unicode escape validators, parser-owned decoded scratch, byte-output materialization, semantic string-field facts, broad cap-widening, single-quartet Unicode classification, and generated retained StringBlock16 probes (`skinny/REDRESS.md:815-870`, `skinny/REDRESS.md:1346-1488`, `skinny/REDRESS.md:1584-1635`, `skinny/REDRESS.md:1736-1886`, `skinny/REDRESS.md:1996-2004`, `skinny/REDRESS.md:2287-2356`).

Do not reopen numeric fallback work. REDRESS 80 blocks the mantissa-widen/f64-fallback route, so `pt_digit_run_span_accumulate` must prove digit-span value without changing conversion semantics (`skinny/REDRESS.md:2217-2248`).

Do not reopen bitmap body-fill routes. REDRESS 88 and 89 reject the PMULL prefix-xor and CSSC CTZ default hot bodies, so `simd_movemask` work is not a P2-E candidate unless another pass supplies a consumer and new proof (`skinny/REDRESS.md:2510-2585`).

Do not broaden container control. REDRESS 63 admits only a narrow ContainerNext/next-byte carry, while later object/key/value-byte compactions are blocked; P2-E should not turn container dispatch into a generic parse-that primitive (`skinny/REDRESS.md:1492-1580`, `skinny/REDRESS.md:1639-1685`, `skinny/REDRESS.md:2360-2397`).

Checkasm parity must be strict before any AArch64 body is product-routed. The current checkasm report still records open strict-mode NEON divergence in classifier parity, while the harness defines scalar comparison, alignment, and corpus gates (`skinny/crates/bbnf-simd/CHECKASM-REPORT.md:41-52`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:102-126`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md:172-198`).

## §5 — Sources (every external citation — comparator source, ISA manual, prior tranche)

No external sources were used. Local sources:

- S-P2 instructions: `restart/prompts/skinny/PASS-2-RESEARCH.md`
- SK-V11 handoff: `restart/skinny/tranches/sk-v11/HANDOFF.md`
- P1 hot-leaf attribution: `restart/skinny/tranches/sk-v11/research/p1/p1e-hot-leaf-attribution.md`
- P1 results delta: `restart/skinny/tranches/sk-v11/research/p1/p1f-results-delta.md`
- P1 hardening convergence: `restart/skinny/tranches/sk-v11/research/p1/hardening/HARDENING-S-P1-CONVERGED.md`
- Skinny result ledger: `skinny/RESULTS.md`
- REDRESS ledger: `skinny/REDRESS.md`
- Locks: `restart/locks/LOCKS.md`
- parse-that-regex string/number/unicode code and tests: `skinny/crates/parse-that-regex/src/lib.rs`, `skinny/crates/parse-that-regex/src/number/mod.rs`, `skinny/crates/parse-that-regex/src/unicode/utf8_block.rs`
- bbnf-simd primitive code and checkasm tests: `skinny/crates/bbnf-simd/src/lib.rs`, `skinny/crates/bbnf-simd/src/dispatch.rs`, `skinny/crates/bbnf-simd/src/aarch64/*.rs`, `skinny/crates/bbnf-simd/tests/*.rs`, `skinny/crates/bbnf-simd/CHECKASM-REPORT.md`
- Generated JSON, direct_struct, and Track 2 consumers: `skinny/crates/runtime/src/grammars/json/generated.rs`, `skinny/crates/runtime/src/grammars/json/scan.rs`, `skinny/crates/bbnf-bench/src/generated_real_typed.rs`, `skinny/crates/bbnf-bench/src/direct_struct.rs`, `skinny/crates/bbnf-bench/src/track2/json.rs`
